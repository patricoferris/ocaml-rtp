type version = Zero | One | Two | Three [@@deriving sexp_of]

let version_of_cstruct buff =
  match Cstruct.get_uint8 buff 0 lsr 6 with
  | 0 -> Ok Zero
  | 1 -> Ok One
  | 2 -> Ok Two
  | 3 -> Ok Three
  | _ -> Error (`Msg "Unknown version")

let version_to_int = function Zero -> 0 | One -> 1 | Two -> 2 | Three -> 3

type extension = { profile : Cstruct.uint8; extension : Cstruct.t }

let extension_equal (a : extension) (b : extension) =
  a.profile = b.profile && Cstruct.equal a.extension b.extension

let pp_extenstion ppf { profile; extension } =
  Format.fprintf ppf "profile(%i){ %a }" profile Cstruct.hexdump_pp extension

type header = {
  version : version;
  padding : bool;
  ext : bool;
  csrc_count : int;
  marker : bool;
  payload_type : Cstruct.uint8;
  sequence_number : Cstruct.uint16;
  timestamp : Cstruct.uint32;
  sync_src : Cstruct.uint32;
  csrc_srcs : Cstruct.uint32 list;
  extension : extension option;
}

let header_equal (a : header) (b : header) =
  a.version = b.version && a.padding = b.padding && a.ext = b.ext
  && a.csrc_count = b.csrc_count
  && a.marker = b.marker
  && a.payload_type = b.payload_type
  && a.sequence_number = b.sequence_number
  && a.timestamp = b.timestamp && a.sync_src = b.sync_src
  && a.csrc_srcs = b.csrc_srcs
  && Option.equal extension_equal a.extension b.extension

type t = { header : header; payload : Cstruct.t }

let get_bool off bit buff = Cstruct.get_uint8 buff off land (1 lsl bit) > 0

let of_cstruct buff =
  let version = version_of_cstruct buff |> Result.get_ok in
  let padding = get_bool 0 5 buff in
  let ext = get_bool 0 4 buff in
  let csrc_count = Cstruct.get_uint8 buff 0 land 0x0F in
  let marker = get_bool 1 7 buff in
  let payload_type = Cstruct.get_uint8 buff 1 land 0x7F in
  let sequence_number = Cstruct.BE.get_uint16 buff 2 in
  let timestamp = Cstruct.BE.get_uint32 buff 4 in
  let sync_src = Cstruct.BE.get_uint32 buff 8 in
  let rec collect_sources sources = function
    | n when n <= 0 -> List.rev sources
    | n ->
        collect_sources
          (Cstruct.BE.get_uint32 buff (12 + (4 * (csrc_count - n))) :: sources)
          (n - 1)
  in
  let csrc_srcs = collect_sources [] csrc_count in
  match ext with
  | true ->
      let ext_off = 12 + (4 * csrc_count) in
      let profile = Cstruct.BE.get_uint16 buff ext_off in
      let ext_length = Cstruct.BE.get_uint16 buff (ext_off + 2) in
      let extension = Cstruct.sub buff (ext_off + 4) (4 * ext_length) in
      let payload_off = ext_off + 4 + (4 * ext_length) in
      let payload =
        Cstruct.sub buff payload_off (Cstruct.length buff - payload_off)
      in
      {
        header =
          {
            version;
            padding;
            ext;
            csrc_count;
            marker;
            payload_type;
            sequence_number;
            timestamp;
            sync_src;
            csrc_srcs;
            extension = Some { profile; extension };
          };
        payload;
      }
  | false ->
      let payload_off = 12 + (4 * csrc_count) in
      {
        header =
          {
            version;
            padding;
            ext;
            csrc_count;
            marker;
            payload_type;
            sequence_number;
            timestamp;
            sync_src;
            csrc_srcs;
            extension = None;
          };
        payload = Cstruct.sub buff payload_off (Cstruct.length buff - payload_off);
      }

let header_into_cstruct (h : header) buff =
  let version = version_to_int h.version in
  let padding = Bool.to_int h.padding in
  let ext = Bool.to_int h.ext in
  let first_byte =
    (version lsl 6) lor (padding lsl 5) lor (ext lsl 4) lor h.csrc_count
  in
  let marker = Bool.to_int h.marker in
  let second_byte = (marker lsl 7) lor h.payload_type in
  Cstruct.set_uint8 buff 0 first_byte;
  Cstruct.set_uint8 buff 1 second_byte;
  Cstruct.BE.set_uint16 buff 2 h.sequence_number;
  Cstruct.BE.set_uint32 buff 4 h.timestamp;
  Cstruct.BE.set_uint32 buff 8 h.sync_src;
  List.iteri
    (fun i c -> Cstruct.BE.set_uint32 buff (12 + (4 * i)) c)
    h.csrc_srcs

let to_cstruct (t : t) =
  let ( <+> ) = Cstruct.append in
  let size = 12 + (t.header.csrc_count * 4) in
  let buff = Cstruct.create size in
  header_into_cstruct t.header buff;
  match t.header.extension with
  | Some { extension; profile } ->
      let ext_header =
        let ext_buff = Cstruct.create 4 in
        Cstruct.BE.set_uint16 ext_buff 0 profile;
        Cstruct.(BE.set_uint16 ext_buff 2 (Cstruct.length extension / 4));
        ext_buff
      in
      buff <+> ext_header <+> extension <+> t.payload
  | None -> buff <+> t.payload

let pp ppf (t : t) = Cstruct.hexdump_pp ppf (to_cstruct t)

let equal (a : t) (b : t) =
  Cstruct.equal a.payload b.payload && header_equal a.header b.header
