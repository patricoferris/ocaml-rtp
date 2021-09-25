(** {1 Report} *)

type t = {
  ssrc : Cstruct.uint32;
  fraction_lost : Cstruct.uint8;
  packets_lost : Cstruct.uint32;
  higest_sequence_num_received : Cstruct.uint32;
  interarrival_jitter : Cstruct.uint32;
  last_sr : Cstruct.uint32;
  dlsr : Cstruct.uint32;
}
(** The type for reports found in the sender and receiver packets.

    - [ssrc]: the source identifier to which this report pertains
    - [fraction_lost]: fraction of packets lost from [ssrc] since last SR or RR
      (decimal point on left edge)
    - [packets_lost]: total cumulative RTP packets lost at [ssrc] since
      reception began
    - [interarrival_jitter]: estimate of statistical variance of the RTP data
      packet interarrival time measured in timestamp units
    - [last_sr]: middle 32 bits out of the 64 in the NTP timestamp
    - [dslr]: delay expressed in units of [1/65536] between receiving the last
      SR packet and sending the RR block. *)

val of_cstruct : Cstruct.t -> t

val to_cstruct : t -> Cstruct.t

val length : int
(** The length in bytes of a single report block *)
