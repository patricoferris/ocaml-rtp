type t = {
  version : Rtp.Packet.version;
  padding : bool;
  count : int;
  packet_type : Cstruct.uint8;
  length : Cstruct.uint16;
}

let of_cstruct buff =
  let version = Rtp.Packet.version_of_cstruct buff |> Result.get_ok in
  let padding = Rtp.Packet.get_bool 0 5 buff in
  let count = Cstruct.get_uint8 buff 0 land 0x1F in
  let packet_type = Cstruct.get_uint8 buff 1 in
  let length = Cstruct.BE.get_uint16 buff 2 in
  { version; padding; count; packet_type; length }

let to_cstruct h =
  let buff = Cstruct.create 4 in
  let version = Rtp.Packet.version_to_int h.version in
  let padding = Bool.to_int h.padding in
  let first_byte = (version lsl 6) lor (padding lsl 5) lor h.count in
  Cstruct.set_uint8 buff 0 first_byte;
  Cstruct.set_uint8 buff 1 h.packet_type;
  Cstruct.BE.set_uint16 buff 2 h.length;
  buff

let equal a b = a = b
