let get_padding len =
  let rem = len mod 4 in
  if rem = 0 then 0 else 4 - rem
