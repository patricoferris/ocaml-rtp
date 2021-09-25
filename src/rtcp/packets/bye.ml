type t = {
  header : Header.t;
  ssrcs : Cstruct.uint32 list;
  reason : string option;
}

let packet_type = 203

let equal a b = a = b

let of_cstruct buff =
  let header = Header.of_cstruct buff in
  let len = header.count in
  let rec collect_ssrcs ssrcs = function
    | 0 -> List.rev ssrcs
    | n -> collect_ssrcs (Cstruct.BE.get_uint32 buff (len - n) :: ssrcs) (n - 1)
  in
  let ssrcs = collect_ssrcs [] len in
  let reason_off = 4 + (len * 4) in
  match Cstruct.len buff - reason_off <= 0 with
  | false ->
      let reason_length = Cstruct.get_uint8 buff reason_off in
      let reason =
        Cstruct.to_string ~off:(reason_off + 1) ~len:reason_length buff
      in
      { header; ssrcs; reason = Some reason }
  | true -> { header; ssrcs; reason = None }

let to_cstruct t =
  let ( <+> ) = Cstruct.append in
  let header = Header.to_cstruct t.header in
  let count = List.length t.ssrcs in
  let ssrcs = Cstruct.create count in
  List.iteri (fun i s -> Cstruct.BE.set_uint32 ssrcs (4 * i) s) t.ssrcs;
  match t.reason with
  | None -> Cstruct.append header ssrcs
  | Some s -> (
      let reason = Cstruct.of_string s in
      let length = Cstruct.length reason in
      (* header + ssrcs + length + reason *)
      let total_length = 4 + (4 * count) + 1 + length in
      let reason =
        let buff = Cstruct.create 1 in
        Cstruct.set_uint8 buff 0 length;
        Cstruct.append buff reason
      in
      match Util.get_padding total_length with
      | n when n = 0 -> header <+> ssrcs <+> reason
      | n -> header <+> ssrcs <+> reason <+> Cstruct.create n)
