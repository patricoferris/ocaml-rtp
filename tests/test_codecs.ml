open Rtp
open Util

let h264 = Alcotest.testable H264.pp H264.equal

let test_codec_result f c buff expected () =
  let actual = f buff in
  Alcotest.(check (result c msg)) "same value" expected actual

let codecs =
  [
    ( Rtp.H264.of_cstruct,
      h264,
      list_to_cstruct [ 0x90; 0x90; 0x90 ],
      Ok
        H264.
          {
            header = header_of_cstruct @@ list_to_cstruct [ 0x90 ];
            payload = list_to_cstruct [ 0x90; 0x90 ];
          } );
  ]

let tests =
  List.mapi
    (fun i (f, c, b, e) ->
      (Fmt.str "packet_%i" i, `Quick, test_codec_result f c b e))
    codecs
