let rec user_input = (prompt, cb) => {
  switch (LNoise.linenoise(prompt)) {
  | None => ()
  | Some(v) => {
    cb(v);
    user_input(prompt, cb)
  }}
}

let repl = (cb) => {
  LNoise.history_load(~filename="history.txt") |> ignore;
  LNoise.history_set(~max_length=100) |> ignore;

  (from_user => {
    if (from_user == "quit") {
      exit(0);
    };
  LNoise.history_add(from_user) |> ignore;
  LNoise.history_save(~filename="history.txt") |> ignore;
    from_user |> cb
  }) |> user_input("user> ")
}
