structure TestEquiv =
struct
  open Equiv
  open Syntax

  datatype result = PASS | FAIL

  fun sk (Ktype : kind) = "T"
    | sk (Karrow(k1,k2)) = sk(k1) ^ " -> " ^ sk(k2)

  fun sc (Cvar(i) : con) = "Var(" ^ Int.toString(i) ^ ")"
    | sc (Clam(k,c)) = "lam(" ^ sk(k) ^ "." ^ sc(c) ^ ")"
    | sc (Capp(c1,c2)) = sc(c1) ^ "(" ^ sc(c2) ^ ")"
    | sc (Carrow(c1,c2)) = sc(c1) ^ " -> " ^ sc(c2)
    | sc (Cforall(k,c)) = "FA(" ^ sk(k) ^ "." ^ sc(c) ^ ")"

  (* prints them backwards for notation used in notes *)
  fun sctx L = "[" ^ (String.concatWith ", " (List.rev (map sk L))) ^ "]"

  fun pass n = (print("Passed test " ^ Int.toString(n) ^ "\n"); n+1)

  fun fail j n ctx c1 c2 k = (print("Failed test " ^ Int.toString(n) ^ ":\n" ^
                                sctx(ctx) ^ " |/- " ^ sc(c1) ^ " <=> " ^ sc(c2) ^
                                " : " ^ sk(k) ^ "\n"); n+1)

  val fail1 = fail "<=>"
  val fail2 = fail "<->"

  val fail3 = fn exp =>
                  let
                    val n' = fail "<->"
                    val _ = print(" but expected kind" ^ sk(exp) ^ "\n")
                  in
                    n'
                  end

  fun runEquivTests L = (print("\n\n----Running equiv Tests ----\n");
      foldl (fn ((ctx,c1,c2,k,res),n) => (equiv ctx c1 c2 k; if res = PASS
                                                             then pass n
                                                             else fail1 n ctx c1 c2 k)
                                         handle TypeError => if res = FAIL
                                                             then pass n
                                                             else fail1 n ctx c1 c2 k)
            0 L)


  exception Wrong of kind

  fun runEquivStrTests L = (print("\n\n----Running equivStr Tests----\n");
      foldl (fn ((ctx,c1,c2,k,res),n) =>
                (let val k' = equivStr ctx c1 c2 in
                   if (k' = k) andalso res = PASS
                   then pass n
                   else fail3 k' n ctx c1 c2 k end)
                handle TypeError => if res = FAIL
                                    then pass n
                                    else fail2 n ctx c1 c2 k)
            0 L)

  (* test = ([c1, c2,...] c c' k *)
  (* where [c1,c2,...] is the context *)
  (* where c and c' are the constructors to be checked for equivalence *)
  (* where k is the kind to check for equivalence at *)
  val equivTests = [([Ktype], Cvar 0, Cvar 0, Ktype, PASS),
                    ([Ktype], Capp(Clam(Ktype, Cvar(0)),Cvar(0)), Cvar(0), Ktype, PASS),
                    ([Ktype], Capp(Clam(Ktype, Cvar(1)),Cvar(0)), Cvar(0), Ktype, PASS),
                    ([Karrow(Ktype,Ktype)], Capp(Clam(Ktype, Cvar(1)),Cvar(0)),
                     Cvar(0), Karrow(Ktype, Ktype), PASS),
                    ([], Clam(Ktype, Cforall(Ktype, Cvar(0))), Clam(Ktype, Cforall(Ktype, Cvar(0))),
                     Karrow(Ktype, Ktype), PASS)]

  val equivStrTests = [([Ktype],Cvar 0, Cvar 0, Ktype, PASS),
                      ([Ktype, Karrow(Ktype,Ktype)], Cvar 0, Cvar 0, Ktype, PASS),
                      ([Ktype, Karrow(Ktype,Ktype)], Cvar 1, Cvar 1, Karrow(Ktype,Ktype), PASS),
                      ([Ktype, Karrow(Ktype,Ktype)], Cvar 0, Cvar 1, Karrow(Ktype,Ktype), FAIL)]

  val _ = runEquivTests equivTests
  val _ = runEquivStrTests equivStrTests

end
