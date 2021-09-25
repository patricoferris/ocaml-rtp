type t = {
  header : Header.t;
  ssrc : Cstruct.uint32;
  ntp : Cstruct.uint64;
  rtp_timestamp : Cstruct.uint32;
  packet_count : Cstruct.uint32;
  octect_count : Cstruct.uint32;
  reports : Report.t list;
}

let of_cstruct buff =
  let header = Header.of_cstruct buff in
  let buff = Cstruct.(sub buff 4 (length buff - 4)) in
  let ssrc = Cstruct.BE.get_uint32 buff 0 in
  let ntp = Cstruct.BE.get_uint64 buff 4 in
  let rtp_timestamp = Cstruct.BE.get_uint32 buff 12 in
  let packet_count = Cstruct.BE.get_uint32 buff 16 in
  let octect_count = Cstruct.BE.get_uint32 buff 20 in
  let report_offset = 24 in
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
  { header; ssrc; ntp; rtp_timestamp; packet_count; octect_count; reports }

let to_cstruct t =
  let ( <+> ) = Cstruct.append in
  let buff = Cstruct.create 24 in
  Cstruct.BE.set_uint32 buff 0 t.ssrc;
  Cstruct.BE.set_uint64 buff 4 t.ntp;
  Cstruct.BE.set_uint32 buff 12 t.rtp_timestamp;
  Cstruct.BE.set_uint32 buff 16 t.packet_count;
  Cstruct.BE.set_uint32 buff 20 t.octect_count;
  let reports =
    List.fold_left Cstruct.append Cstruct.empty
      (List.map Report.to_cstruct t.reports)
  in
  Header.to_cstruct t.header <+> buff <+> reports

let packet_type = 200

let equal = Stdlib.( = )
