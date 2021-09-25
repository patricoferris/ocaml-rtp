open Util

let rtp_packet = Alcotest.testable Rtp.Packet.pp Rtp.Packet.equal

let extension =
  Alcotest.testable Rtp.Packet.pp_extenstion Rtp.Packet.extension_equal

let test_packet_1 () =
  let raw_packet = list_to_cstruct [ 
    0x90; 0xe0; 0x69; 0x8f; 0xd9; 0xc2; 0x93; 0xda; 0x1c; 0x64; 0x27; 0x82; 0x00; 0x01; 0x00; 0x01; 0xFF; 0xFF; 0xFF; 0xFF; 0x98; 0x36; 0xbe; 0x88; 0x9e
  ] in 
  let expect =
    Rtp.Packet.
      {
        header =
          {
            version = Two;
            padding = false;
            ext = true;
            marker = true;
            payload_type = 96;
            sequence_number = 27023;
            timestamp = Int32.of_int 3653407706;
            sync_src = 476325762l;
            csrc_count = 0;
            csrc_srcs = [];
            extension = Some { profile = 1; extension = list_to_cstruct [ 0xFF; 0xFF; 0xFF; 0xFF ] }
          };
        payload = Cstruct.(sub raw_packet 20 (len raw_packet - 20));
      }
  in
  let rtp = Rtp.Packet.of_cstruct raw_packet in
  let cstruct_of_exptect = Rtp.Packet.to_cstruct expect in
  Alcotest.(check cstruct) "same cstruct" raw_packet cstruct_of_exptect;
  Alcotest.(check bool) "extensions" expect.header.ext rtp.header.ext;
  Alcotest.(check int32) "sync src" expect.header.sync_src rtp.header.sync_src;
  Alcotest.(check int32) "timestamp" expect.header.timestamp rtp.header.timestamp;
  Alcotest.(check (option extension)) "extension" expect.header.extension rtp.header.extension;
  Alcotest.check cstruct "same payload" expect.payload rtp.payload;
  Alcotest.check rtp_packet "same packet" expect rtp
  [@@ocamlformat "disable"]

let test_packet_multiple_extensions () = 
  let packet = list_to_cstruct [
    0x90; 0xe0; 0x69; 0x8f; 0xd9; 0xc2; 0x93; 0xda; 0x1c; 0x64;
		0x27; 0x82; 0xBE; 0xDE; 0x00; 0x03; 0x10; 0xAA; 0x21; 0xBB;
		0xBB; 0x33; 0xCC; 0xCC; 0xCC; 0xCC; 0x00; 0x00;
		(* Payload *)
		0x98; 0x36; 0xbe; 0x88; 0x9e;
  ] in 
  let rtp = Rtp.Packet.of_cstruct packet in 
    assert (rtp.header.version = Rtp.Packet.Two)
    [@@ocamlformat "disable"]

let tests =
  [
    ("packet_1", `Quick, test_packet_1);
    ("packet_2", `Quick, test_packet_multiple_extensions);
  ]
