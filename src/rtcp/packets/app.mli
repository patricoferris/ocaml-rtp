type t = {
  header : Header.t;
  ssrc : Cstruct.uint32;
  (* ASCII name *)
  name : string;
  payload : Cstruct.t;
}

val subtype : t -> int

include S.Packet with type t := t
