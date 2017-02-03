structure TestEquiv =
struct
  open Equiv
  open Syntax

  fun sk (Ktype : kind) = "T"
    | sk (Karrow(k1,k2)) = sk(k1) ^ " -> " ^ sk(k2)

  fun sc (Cvar(i) : con) = "Var(" ^ Int.toString(i) ^ ")"
    | sc (Clam(k,c)) = "lam(" ^ sk(k) ^ "." ^ sc(c) ^ ")"
    | sc (Capp(c1,c2)) = sc(c1) ^ "(" ^ sc(c2) ^ ")"
    | sc (Carrow(c1,c2)) = sc(c1) ^ " -> " ^ sc(c2)
    | sc (Cforall(k,c)) = "FA(" ^ sk(k) ^ "." ^ sc(c) ^ ")"

  (* prints them backwards for notation used in notes *)
  fun sctx L = "[" ^ (String.concatWith ", " (List.rev (map sk L))) ^ "]"

  fun runEquivTests L = (print("\n\n----Running equiv Tests ----\n");
      foldl (fn ((ctx,c1,c2,k),n) => (equiv ctx c1 c2 k;
                                      print("Passed test " ^ Int.toString(n) ^ "\n");n+1)
                                     handle TypeError =>
                                            (print("Failed test " ^ Int.toString(n) ^ ":\n" ^
                                                   sctx(ctx) ^ " |/- " ^ sc(c1) ^ " <=> " ^ sc(c2) ^
                                                   " : " ^ sk(k) ^ "\n"); n+1))
            0 L)


  exception Wrong of kind

  fun runEquivStrTests L = (print("\n\n----Running equivStr Tests----\n");
      foldl (fn ((ctx,c1,c2,k),n) =>
                (let val k' = equivStr ctx c1 c2 in
                   if (k' = k)
                   then (print("Passed test " ^ Int.toString(n) ^ "\n"); n+1)
                   else raise Wrong(k') end)
                handle TypeError => (print("Failed test " ^ Int.toString(n) ^
                                           " due to TypeError:\n" ^
                                           sctx(ctx) ^ " |- " ^ sc(c1) ^ " <-> " ^ sc(c2) ^ " : "
                                           ^ sk(k)); n+1)
                     | Wrong k' => (print("Failed test " ^ Int.toString(n) ^
                                          " due to incorrect result:\n" ^
                                          sctx(ctx) ^ " |- " ^ sc(c1) ^ " <-> " ^ sc(c2) ^ " : "
                                          ^ sk(k') ^ " but wanted " ^ sk(k)); n+1))
            0 L)

  (* test = ([c1, c2,...] c c' k *)
  (* where [c1,c2,...] is the context *)
  (* where c and c' are the constructors to be checked for equivalence *)
  (* where k is the kind to check for equivalence at *)
  val equivTests = [([Ktype], Cvar 0, Cvar 0, Ktype),
                    ([Ktype], Capp(Clam(Ktype, Cvar(0)),Cvar(0)), Cvar(0), Ktype),
                    ([Ktype], Capp(Clam(Ktype, Cvar(1)),Cvar(0)), Cvar(0), Ktype),
                    ([Karrow(Ktype,Ktype)], Capp(Clam(Ktype, Cvar(1)),Cvar(0)),
                     Cvar(0), Karrow(Ktype, Ktype)),
                    ([], Clam(Ktype, Cforall(Ktype, Cvar(0))), Clam(Ktype, Cforall(Ktype, Cvar(0))),
                     Karrow(Ktype, Ktype))]

  val equivStrTests = [([Ktype],Cvar 0, Cvar 0, Ktype)]

  val _ = runEquivTests equivTests
  val _ = runEquivStrTests equivStrTests

end
