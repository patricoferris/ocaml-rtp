type t = { header : Header.t; ssrc : Cstruct.uint32; reports : Report.t list }

let of_cstruct buff =
  let header = Header.of_cstruct buff in
  let buff = Cstruct.(sub buff 4 (length buff - 4)) in
  let ssrc = Cstruct.BE.get_uint32 buff 0 in
  let report_offset = 4 in
  let length = Cstruct.len buff - report_offset in
  let rec collect_reports acc = function
    | 0 -> List.rev acc
    | n ->
        collect_reports
          (Report.of_cstruct
             (Cstruct.sub buff (report_offset + (length - n)) Report.length)
           :: acc)
          (n - 4)
  in
  let reports = collect_reports [] length in
  { header; ssrc; reports }

let to_cstruct t =
  let ( <+> ) = Cstruct.append in
  let buff = Cstruct.create 4 in
  Cstruct.BE.set_uint32 buff 0 t.ssrc;
  let reports =
    List.fold_left Cstruct.append Cstruct.empty
      (List.map Report.to_cstruct t.reports)
  in
  Header.to_cstruct t.header <+> buff <+> reports

let packet_type = 201

let equal = Stdlib.( = )
