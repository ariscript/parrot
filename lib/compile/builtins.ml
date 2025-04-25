open Exprs

let inject_builtins (e : 'a expr) : tag expr =
  let bname id = BName (id, false, ()) in
  ELet
    ( [ ( bname "print",
          ELambda
            ([BName ("p", false, ())], EApp (EId ("print", ()), [EId ("p", ())], Native, ()), ()),
          () );
        ( BName ("equal", false, ()),
          ELambda
            ( [BName ("e1", false, ()); BName ("e2", false, ())],
              EApp (EId ("equal", ()), [EId ("e1", ()); EId ("e2", ())], Native, ()),
              () ),
          () );
        (BName ("input", false, ()), ELambda ([], EApp (EId ("input", ()), [], Native, ()), ()), ())
      ],
      untagE e,
      () )
  |> tag
;;
