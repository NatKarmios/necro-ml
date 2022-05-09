(** This file was automatically generated using necroml
	See https://gitlab.inria.fr/skeletons/necro-ml/ for more informations *)

(** The unspecified types *)
module type TYPES = sig
  
end

(** The interpretation monad *)
module type MONAD = sig
  type 'a t
  val ret: 'a -> 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val branch: (unit -> 'a t) list -> 'a t
  val fail: string -> 'a t
  val apply: ('a -> 'b t) -> 'a -> 'b t
  val extract: 'a t -> 'a
end

(** All types, and the unspecified terms *)
module type UNSPEC = sig
  module M: MONAD
  include TYPES

  type rel =
  | Pos of nat
  | Neg of nat
  and nat =
  | S of nat
  | O
  and boolean =
  | True
  | False
  and rat = rel * nat
  and rel' = nat * nat
end

(** A default instantiation *)
module Unspec (M: MONAD) (T: TYPES) = struct
  exception NotImplemented of string
  include T
  module M = M

  type rel =
  | Pos of nat
  | Neg of nat
  and nat =
  | S of nat
  | O
  and boolean =
  | True
  | False
  and rat = rel * nat
  and rel' = nat * nat
end

(** The module type for interpreters *)
module type INTERPRETER = sig
  module M: MONAD

  type rel =
  | Pos of nat
  | Neg of nat
  and nat =
  | S of nat
  | O
  and boolean =
  | True
  | False
  and rat = rel * nat
  and rel' = nat * nat

  val abs: rel -> nat M.t
  val add: nat -> (nat -> nat M.t) M.t
  val addq: rat -> (rat -> rat M.t) M.t
  val addz: rel -> (rel -> rel M.t) M.t
  val div: nat -> (nat -> nat M.t) M.t
  val divq: rat -> (rat -> rat M.t) M.t
  val divz: rel -> (rel -> rel M.t) M.t
  val eq: nat -> (nat -> boolean M.t) M.t
  val eqq: rat -> (rat -> boolean M.t) M.t
  val eqz: rel -> (rel -> boolean M.t) M.t
  val even: nat -> boolean M.t
  val evenz: rel -> boolean M.t
  val gcd: nat -> (nat -> nat M.t) M.t
  val ge: nat -> (nat -> boolean M.t) M.t
  val geq: rat -> (rat -> boolean M.t) M.t
  val getInt: rat -> rel M.t
  val gez: rel -> (rel -> boolean M.t) M.t
  val gt: nat -> (nat -> boolean M.t) M.t
  val gtq: rat -> (rat -> boolean M.t) M.t
  val gtz: rel -> (rel -> boolean M.t) M.t
  val inv: rat -> rat M.t
  val isInt: rat -> boolean M.t
  val le: nat -> (nat -> boolean M.t) M.t
  val leq: rat -> (rat -> boolean M.t) M.t
  val lez: rel -> (rel -> boolean M.t) M.t
  val lt: nat -> (nat -> boolean M.t) M.t
  val ltq: rat -> (rat -> boolean M.t) M.t
  val ltz: rel -> (rel -> boolean M.t) M.t
  val max: nat -> (nat -> nat M.t) M.t
  val maxq: rat -> (rat -> rat M.t) M.t
  val maxz: rel -> (rel -> rel M.t) M.t
  val min: nat -> (nat -> nat M.t) M.t
  val minq: rat -> (rat -> rat M.t) M.t
  val minz: rel -> (rel -> rel M.t) M.t
  val mod': nat -> (nat -> nat M.t) M.t
  val mul: nat -> (nat -> nat M.t) M.t
  val mulq: rat -> (rat -> rat M.t) M.t
  val mulz: rel -> (rel -> rel M.t) M.t
  val notb: boolean -> boolean M.t
  val odd: nat -> boolean M.t
  val oddz: rel -> boolean M.t
  val opp: rel -> rel M.t
  val oppq: rat -> rat M.t
  val pow: nat -> (nat -> nat M.t) M.t
  val predz: rel -> rel M.t
  val reduce: rat -> rat M.t
  val rel'_to_rel: rel' -> rel M.t
  val rel_to_rel': rel -> rel' M.t
  val sgn: rel -> (rel -> rel M.t) M.t
  val sub: nat -> (nat -> nat M.t) M.t
  val subq: rat -> (rat -> rat M.t) M.t
  val subz: rel -> (rel -> rel M.t) M.t
  val succz: rel -> rel M.t
end

(** Module defining the specified terms *)
module MakeInterpreter (F: UNSPEC) = struct
  include F

  let ( let* ) = M.bind

  let apply1 = M.apply
  let apply2 f arg1 arg2 =
    let* _tmp = apply1 f arg1 in
    apply1 _tmp arg2

  let rec abs n =
    M.branch [
      (function () ->
        begin match n with
        | Neg n -> M.ret n
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | Pos n -> M.ret n
        | _ -> M.fail ""
        end)]
  and add m =
    M.ret (function n ->
      begin match n with
      | O -> M.ret m
      | S n ->
          let* s = apply2 add m n in
          M.ret (S s)
      end)
  and addq =
    function (n1, d1) ->
    M.ret (function (n2, d2) ->
      let* pn1d2 = apply2 mulz n1 (Pos d2) in
      let* pn2d1 = apply2 mulz n2 (Pos d1) in
      let* pd1d2 = apply2 mul d1 d2 in
      let* num = apply2 addz pn1d2 pn2d1 in
      apply1 reduce (num, pd1d2))
  and addz m =
    M.ret (function n ->
      M.branch [
        (function () ->
          begin match (m, n) with
          | (Pos m, Pos n) ->
              let* s = apply2 add m n in
              M.ret (Pos s)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Neg m, Neg n) ->
              let* s = apply2 add m n in
              M.branch [
                (function () ->
                  let* _tmp = apply2 eq O s in
                  begin match _tmp with
                  | True -> M.ret (Pos O)
                  | _ -> M.fail ""
                  end) ;
                (function () ->
                  let* _tmp = apply2 eq O s in
                  begin match _tmp with
                  | False -> M.ret (Neg s)
                  | _ -> M.fail ""
                  end)]
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Pos m, Neg n) ->
              M.branch [
                (function () ->
                  let* _tmp = apply2 ge m n in
                  begin match _tmp with
                  | True ->
                      let* s = apply2 sub m n in
                      M.ret (Pos s)
                  | _ -> M.fail ""
                  end) ;
                (function () ->
                  let* _tmp = apply2 lt m n in
                  begin match _tmp with
                  | True ->
                      let* s = apply2 sub n m in
                      M.ret (Neg s)
                  | _ -> M.fail ""
                  end)]
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Neg m, Pos n) -> apply2 addz (Pos n) (Neg m)
          | _ -> M.fail ""
          end)])
  and div m =
    M.ret (function n ->
      begin match n with
      | S n' ->
          let* cmp = apply2 le m n' in
          begin match cmp with
          | True -> M.ret O
          | False ->
              let* m' = apply2 sub m n in
              let* d = apply2 div m' n in
              M.ret (S d)
          end
      | _ -> M.fail ""
      end)
  and divq a =
    M.ret (function b ->
      let* b' = apply1 inv b in
      apply2 mulq a b')
  and divz m =
    M.ret (function n ->
      M.branch [
        (function () ->
          begin match (m, n) with
          | (Pos m, Pos n) ->
              let* d = apply2 div m n in
              M.ret (Pos d)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Pos m, Neg n) ->
              let* d = apply2 div m n in
              apply1 opp (Pos d)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Neg m, _) ->
              let* d = apply2 divz (Pos m) n in
              apply1 opp d
          | _ -> M.fail ""
          end)])
  and eq m =
    M.ret (function n ->
      begin match (m, n) with
      | (O, O) -> M.ret True
      | (S m, S n) -> apply2 eq m n
      | _ -> M.ret False
      end)
  and eqq q1 =
    M.ret (function q2 ->
      let ((n1, d1), (n2, d2)) = (q1, q2) in
      let* pn1d2 = apply2 mulz n1 (Pos d2) in
      let* pn2d1 = apply2 mulz n2 (Pos d1) in
      apply2 eqz pn1d2 pn2d1)
  and eqz m =
    M.ret (function n ->
      M.branch [
        (function () ->
          begin match (m, n) with
          | (Pos m, Pos n) -> apply2 eq m n
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Neg m, Neg n) -> apply2 eq m n
          | _ -> M.fail ""
          end) ;
        (function () ->
          let* (m, n) =
            M.branch [
              (function () ->
                begin match (m, n) with
                | (Pos m, Neg n) -> M.ret (m, n)
                | _ -> M.fail ""
                end) ;
              (function () ->
                begin match (m, n) with
                | (Neg m, Pos n) -> M.ret (m, n)
                | _ -> M.fail ""
                end)]
          in
          M.branch [
            (function () ->
              begin match m with
              | O ->
                  begin match n with
                  | O -> M.ret True
                  | _ -> M.fail ""
                  end
              | _ -> M.fail ""
              end) ;
            (function () ->
              begin match m with
              | S _ -> M.ret False
              | _ -> M.fail ""
              end) ;
            (function () ->
              begin match n with
              | S _ -> M.ret False
              | _ -> M.fail ""
              end)])])
  and even n =
    M.branch [
      (function () ->
        begin match n with
        | O -> M.ret True
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | S O -> M.ret False
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | S S n -> apply1 even n
        | _ -> M.fail ""
        end)]
  and evenz n =
    let* n = apply1 abs n in
    apply1 even n
  and gcd a =
    M.ret (function b ->
      M.branch [
        (function () ->
          begin match a with
          | O -> M.ret b
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match b with
          | O -> M.ret a
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match b with
          | S _ ->
              let* m = apply2 mod' b a in
              apply2 gcd m a
          | _ -> M.fail ""
          end)])
  and ge m =
    M.ret (function n ->
      apply2 le n m)
  and geq a =
    M.ret (function b ->
      apply2 leq b a)
  and getInt q =
    let* (nom, den) = apply1 reduce q in
    begin match den with
    | S O -> M.ret nom
    | _ -> M.fail ""
    end
  and gez m =
    M.ret (function n ->
      apply2 lez n m)
  and gt m =
    M.ret (function n ->
      apply2 lt n m)
  and gtq a =
    M.ret (function b ->
      apply2 ltq b a)
  and gtz m =
    M.ret (function n ->
      apply2 ltz n m)
  and inv =
    function (n, d) ->
    let* _tmp = apply2 eqz (Pos O) n in
    begin match _tmp with
    | False ->
        let* n' = apply1 abs n in
        let* d' = apply2 sgn n (Pos d) in
        apply1 reduce (d', n')
    | _ -> M.fail ""
    end
  and isInt q =
    let* (_, den) = apply1 reduce q in
    apply2 eq (S O) den
  and le m =
    M.ret (function n ->
      begin match (m, n) with
      | (O, _) -> M.ret True
      | (S m, S n) -> apply2 le m n
      | _ -> M.ret False
      end)
  and leq =
    function (n1, d1) ->
    M.ret (function (n2, d2) ->
      let* pn1d2 = apply2 mulz n1 (Pos d2) in
      let* pn2d1 = apply2 mulz n2 (Pos d1) in
      apply2 lez pn1d2 pn2d1)
  and lez m =
    M.ret (function n ->
      M.branch [
        (function () ->
          begin match (m, n) with
          | (Pos m, Pos n) -> apply2 le m n
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Neg m, Neg n) -> apply2 le n m
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Neg _, Pos _) -> M.ret True
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Pos m, Neg n) ->
              M.branch [
                (function () ->
                  let* _tmp = apply2 eq O m in
                  begin match _tmp with
                  | True ->
                      let* _tmp = apply2 eq O n in
                      begin match _tmp with
                      | True -> M.ret True
                      | _ -> M.fail ""
                      end
                  | _ -> M.fail ""
                  end) ;
                (function () ->
                  let* _tmp = apply2 eq O m in
                  begin match _tmp with
                  | False -> M.ret False
                  | _ -> M.fail ""
                  end) ;
                (function () ->
                  let* _tmp = apply2 eq O n in
                  begin match _tmp with
                  | False -> M.ret False
                  | _ -> M.fail ""
                  end)]
          | _ -> M.fail ""
          end)])
  and lt m =
    M.ret (function n ->
      apply2 le (S m) n)
  and ltq =
    function (n1, d1) ->
    M.ret (function (n2, d2) ->
      let* pn1d2 = apply2 mulz n1 (Pos d2) in
      let* pn2d1 = apply2 mulz n2 (Pos d1) in
      apply2 ltz pn1d2 pn2d1)
  and ltz m =
    M.ret (function n ->
      let* m = apply1 succz m in
      apply2 lez m n)
  and max m =
    M.ret (function n ->
      M.branch [
        (function () ->
          begin match m with
          | O -> M.ret n
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match n with
          | O -> M.ret m
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match m with
          | S m ->
              begin match n with
              | S n ->
                  let* v = apply2 max m n in
                  M.ret (S v)
              | _ -> M.fail ""
              end
          | _ -> M.fail ""
          end)])
  and maxq a =
    M.ret (function b ->
      M.branch [
        (function () ->
          let* _tmp = apply2 leq a b in
          begin match _tmp with
          | True -> M.ret b
          | _ -> M.fail ""
          end) ;
        (function () ->
          let* _tmp = apply2 leq a b in
          begin match _tmp with
          | False -> M.ret a
          | _ -> M.fail ""
          end)])
  and maxz m =
    M.ret (function n ->
      M.branch [
        (function () ->
          begin match (m, n) with
          | (Pos m, Pos n) ->
              let* v = apply2 max m n in
              M.ret (Pos v)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Neg m, Neg n) ->
              let* v = apply2 min m n in
              apply1 opp (Pos v)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Neg _, Pos _) -> M.ret n
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Pos _, Neg _) -> M.ret m
          | _ -> M.fail ""
          end)])
  and min m =
    M.ret (function n ->
      let* v = apply2 max m n in
      let* s = apply2 add m n in
      apply2 sub s v)
  and minq a =
    M.ret (function b ->
      M.branch [
        (function () ->
          let* _tmp = apply2 leq a b in
          begin match _tmp with
          | True -> M.ret a
          | _ -> M.fail ""
          end) ;
        (function () ->
          let* _tmp = apply2 leq a b in
          begin match _tmp with
          | False -> M.ret b
          | _ -> M.fail ""
          end)])
  and minz m =
    M.ret (function n ->
      let* v = apply2 maxz m n in
      let* s = apply2 addz m n in
      apply2 subz s v)
  and mod' m =
    M.ret (function n ->
      let* cmp = apply2 lt m n in
      begin match cmp with
      | True -> M.ret m
      | False ->
          let* s = apply2 sub m n in
          apply2 mod' s n
      end)
  and mul m =
    M.ret (function n ->
      begin match n with
      | O -> M.ret O
      | S n ->
          let* p = apply2 mul m n in
          apply2 add p m
      end)
  and mulq =
    function (n1, d1) ->
    M.ret (function (n2, d2) ->
      let* n = apply2 mulz n1 n2 in
      let* d = apply2 mul d1 d2 in
      apply1 reduce (n, d))
  and mulz m =
    M.ret (function n ->
      M.branch [
        (function () ->
          begin match (m, n) with
          | (Pos m, Pos n) ->
              let* p = apply2 mul m n in
              M.ret (Pos p)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Pos m, Neg n) ->
              let* p = apply2 mul m n in
              apply1 opp (Pos p)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match (m, n) with
          | (Neg m, _) ->
              let* p = apply2 mulz (Pos m) n in
              apply1 opp p
          | _ -> M.fail ""
          end)])
  and notb b =
    begin match b with
    | True -> M.ret False
    | False -> M.ret True
    end
  and odd n =
    let* b = apply1 even n in
    apply1 notb b
  and oddz n =
    let* b = apply1 evenz n in
    apply1 notb b
  and opp n =
    M.branch [
      (function () ->
        begin match n with
        | Pos S n -> M.ret (Neg (S n))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | Pos O -> M.ret (Pos O)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | Neg n -> M.ret (Pos n)
        | _ -> M.fail ""
        end)]
  and oppq =
    function (n, d) ->
    let* n' = apply1 opp n in
    apply1 reduce (n', d)
  and pow m =
    M.ret (function n ->
      begin match n with
      | O -> M.ret (S O)
      | S n ->
          let* p = apply2 pow m n in
          apply2 mul m p
      end)
  and predz n =
    M.branch [
      (function () ->
        begin match n with
        | Neg n -> M.ret (Neg (S n))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | Pos O -> M.ret (Neg (S O))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | Pos S n -> M.ret (Pos n)
        | _ -> M.fail ""
        end)]
  and reduce q =
    let (n, d) = q in
    let* n' = apply1 abs n in
    let* g = apply2 gcd n' d in
    let* new_n = apply2 divz n (Pos g) in
    let* new_d = apply2 div d g in
    M.ret (new_n, new_d)
  and rel'_to_rel z =
    let (m, n) = z in
    apply2 subz (Pos m) (Pos n)
  and rel_to_rel' z =
    M.branch [
      (function () ->
        begin match z with
        | Pos n -> M.ret (n, O)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match z with
        | Neg n -> M.ret (O, n)
        | _ -> M.fail ""
        end)]
  and sgn m =
    M.ret (function n ->
      M.branch [
        (function () ->
          begin match m with
          | Pos O -> M.ret (Pos O)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match m with
          | Neg O -> M.ret (Pos O)
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match m with
          | Pos S _ -> M.ret n
          | _ -> M.fail ""
          end) ;
        (function () ->
          begin match m with
          | Neg S _ -> apply1 opp n
          | _ -> M.fail ""
          end)])
  and sub m =
    M.ret (function n ->
      begin match (n, m) with
      | (O, m) -> M.ret m
      | (S n, S m) -> apply2 sub m n
      | _ -> M.branch []
      end)
  and subq a =
    M.ret (function b ->
      let* mb = apply1 oppq b in
      apply2 addq a mb)
  and subz m =
    M.ret (function n ->
      let* n = apply1 opp n in
      apply2 addz m n)
  and succz n =
    M.branch [
      (function () ->
        begin match n with
        | Pos n -> M.ret (Pos (S n))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | Neg O -> M.ret (Pos (S O))
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | Neg S O -> M.ret (Pos O)
        | _ -> M.fail ""
        end) ;
      (function () ->
        begin match n with
        | Neg S S n -> M.ret (Neg (S n))
        | _ -> M.fail ""
        end)]
end