let parse language =
  match language with
  | Config.Lama -> Lama.parse_stmts
  | Config.Racket -> Racket.parse_stmts
