type t = {
  ssrc : Cstruct.uint32;
  fraction_lost : Cstruct.uint8;
  packets_lost : Cstruct.uint32;
  higest_sequence_num_received : Cstruct.uint32;
  interarrival_jitter : Cstruct.uint32;
  last_sr : Cstruct.uint32;
  dlsr : Cstruct.uint32;
}

let of_cstruct buff =
  let ssrc = Cstruct.BE.get_uint32 buff 0 in
  let fraction_lost = Cstruct.get_uint8 buff 4 in
  let packets_lost =
    Int32.logand (Cstruct.BE.get_uint32 buff 4) 0x00_FF_FF_FFl
  in
  let higest_sequence_num_received = Cstruct.BE.get_uint32 buff 8 in
  let interarrival_jitter = Cstruct.BE.get_uint32 buff 12 in
  let last_sr = Cstruct.BE.get_uint32 buff 16 in
  let dlsr = Cstruct.BE.get_uint32 buff 20 in
  {
    ssrc;
    fraction_lost;
    packets_lost;
    higest_sequence_num_received;
    interarrival_jitter;
    last_sr;
    dlsr;
  }

let to_cstruct t =
  let buff = Cstruct.create 24 in
  Cstruct.BE.set_uint32 buff 0 t.ssrc;
  Cstruct.BE.set_uint32 buff 4 t.packets_lost;
  Cstruct.set_uint8 buff 4 t.fraction_lost;
  Cstruct.BE.set_uint32 buff 8 t.higest_sequence_num_received;
  Cstruct.BE.set_uint32 buff 12 t.interarrival_jitter;
  Cstruct.BE.set_uint32 buff 16 t.last_sr;
  Cstruct.BE.set_uint32 buff 20 t.dlsr;
  buff

let length = 24
