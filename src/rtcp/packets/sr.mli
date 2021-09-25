(** {1 Sender Report Packet}

    The SR Packet consists of three sections and possibly a fourth
    profile-specifc extension. *)

type t = {
  header : Header.t;
  ssrc : Cstruct.uint32;
  ntp : Cstruct.uint64;
  rtp_timestamp : Cstruct.uint32;
  packet_count : Cstruct.uint32;
  octect_count : Cstruct.uint32;
  reports : Report.t list;
}

include S.Packet with type t := t
