(* 
  ~~~ H264: A summary as it pertains to RTP ~~~

  The H264 video codec can be used in lots of different
  scenarios. The specification distinguishes between the 
  Video Coding Layer (VCL) and the Network Abstraction
  Layer (NAL). The VCL is not here, this is purely a network thing.

  The NAL encoder takes slices from the output of VCL
  and turns them into units (NALU) which are transmitted
  over the network.

  A NAL unit contains a one-byte header and a payload. The header
  contains the kind of unit amongst other things.
*)

type header = Cstruct.t (* 1 byte *)

let header_of_cstruct buff = buff

[%%cenum
type typ =
  | SPS_NAL [@id 7]
  | PPS_NAL [@id 8]
  | AUD_NAL [@id 9]
  | ESEQ_NAL [@id 10]
  | ESTREAM_NAL [@id 11]
  | FILLER_NAL [@id 12]
  | STAP_A [@id 24]
  | STAP_B [@id 25]
  | MTAP16 [@id 26]
  | MTAP24 [@id 27]
  | FU_A [@id 28]
  | FU_B [@id 29]
[@@uint8_t]]

let fzb header =
  let mask = 0x80 in
  Cstruct.get_uint8 header 0 land mask

let nri header =
  let mask = 0x60 in
  Cstruct.get_uint8 header 0 land mask

let typ header =
  let mask = 0x1F in
  let i = Cstruct.get_uint8 header 0 land mask in
  (int_to_typ i, i)

(* Some of the checks from H264 ITU... *)
let checks header =
  let _, i = typ header in
  let n = nri header in
  assert (fzb header = 0);
  assert ((not (i = 5)) || n <> 0);
  assert ((not (i = 6 || i = 9 || i = 10 || i = 11 || i = 12)) || n = 0)

type t = { header : header; payload : Cstruct.t }

let of_cstruct buff =
  let header = Cstruct.sub buff 0 1 in
  match typ header with
  | None, i when not (i >= 1 && i <= 24) ->
      Error (`Msg Fmt.(str "Unknown packet type %i" i))
  | _ ->
      let payload = Cstruct.sub buff 1 (Cstruct.length buff - 1) in
      Ok { header; payload }

let to_cstruct { header; payload } = Cstruct.append header payload

let equal a b =
  Cstruct.equal a.header b.header && Cstruct.equal a.payload b.payload

let pp ppf { header; payload } =
  Fmt.pf ppf "nri: %i, typ: %i, payload: %a" (nri header)
    (typ header |> snd)
    Cstruct.hexdump_pp payload
