type t = {
  header : Header.t;
  ssrcs : Cstruct.uint32 list;
  reason : string option;
}

include S.Packet with type t := t
