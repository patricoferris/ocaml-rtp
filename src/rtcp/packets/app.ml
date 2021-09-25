type t = {
  header : Header.t;
  ssrc : Cstruct.uint32;
  name : string;
  payload : Cstruct.t;
}

let subtype t = t.header.count

let packet_type = 204

let of_cstruct buff =
  let header = Header.of_cstruct buff in
  let ssrc = Cstruct.BE.get_uint32 buff 4 in
  let name = Cstruct.to_string ~off:8 ~len:4 buff in
  let payload = Cstruct.sub buff 12 (Cstruct.length buff - 12) in
  { header; ssrc; name; payload }

let to_cstruct t =
  let ( <+> ) = Cstruct.append in
  let header = Header.to_cstruct t.header in
  let buff = Cstruct.create 4 in
  Cstruct.BE.set_uint32 buff 4 t.ssrc;
  header <+> buff <+> Cstruct.of_string t.name <+> t.payload

let equal a b =
  Header.equal a.header b.header
  && a.ssrc = b.ssrc && String.equal a.name b.name
  && Cstruct.equal a.payload b.payload
