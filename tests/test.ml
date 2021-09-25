let () =
  Alcotest.run "rtp"
    [ ("rtp", Test_rtp.tests); ("rtp_codecs", Test_codecs.tests) ]
