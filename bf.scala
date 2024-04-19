// Brainfuck interpreter

sealed trait Command
object Plus extends Command
object Minus extends Command
object Left extends Command
object Right extends Command
object Input extends Command
object Output extends Command
final class Loop[Tuple] extends Command

type Plus = Plus.type
type Minus = Minus.type
type Left = Left.type
type Right = Right.type
type Input = Input.type
type Output = Output.type

sealed trait Nat
object Zero extends Nat
type Zero = Zero.type
final class Succ[N <: Nat] extends Nat

sealed trait HList
object HNil extends HList
type HNil = HNil.type
final class +:[H, T <: HList] extends HList

trait Tape[L <: HList, C <: Nat, R <: HList]
type Stream = HList

trait Eval[
    Prog <: HList,
    T <: Tape[?, ?, ?],
    I <: Stream,
    O <: Stream,
    RT <: Tape[?, ?, ?],
    RI <: Stream,
    RO <: Stream
]

object Eval {
  given empty[T <: Tape[?, ?, ?], I <: Stream, O <: Stream]
      : Eval[HNil, T, I, O, T, I, O] =
    ???
  given plus[
      P <: HList,
      L <: HList,
      C <: Nat,
      R <: HList,
      I <: Stream,
      O <: Stream,
      RT <: Tape[?, ?, ?],
      RI <: Stream,
      RO <: Stream
  ](using
      Eval[P, Tape[L, Succ[C], R], I, O, RT, RI, RO]
  ): Eval[Plus +: P, Tape[L, C, R], I, O, RT, RI, RO] = ???

  given minus[
      P <: HList,
      L <: HList,
      C <: Nat,
      R <: HList,
      I <: Stream,
      O <: Stream,
      RT <: Tape[?, ?, ?],
      RI <: Stream,
      RO <: Stream
  ](using
      Eval[P, Tape[L, C, R], I, O, RT, RI, RO]
  ): Eval[Minus +: P, Tape[L, Succ[C], R], I, O, RT, RI, RO] = ???

  given leftEmpty[
      P <: HList,
      C <: Nat,
      R <: HList,
      I <: Stream,
      O <: Stream,
      RT <: Tape[?, ?, ?],
      RI <: Stream,
      RO <: Stream
  ](using
      Eval[P, Tape[HNil, Zero, C +: R], I, O, RT, RI, RO]
  ): Eval[Left +: P, Tape[HNil, C, R], I, O, RT, RI, RO] = ???

  given left[
      P <: HList,
      L <: HList,
      N <: Nat,
      C <: Nat,
      R <: HList,
      I <: Stream,
      O <: Stream,
      RT <: Tape[?, ?, ?],
      RI <: Stream,
      RO <: Stream
  ](using
      Eval[P, Tape[L, N, C +: R], I, O, RT, RI, RO]
  ): Eval[Left +: P, Tape[N +: L, C, R], I, O, RT, RI, RO] = ???

  given rightEmpty[
      P <: HList,
      L <: HList,
      C <: Nat,
      I <: Stream,
      O <: Stream,
      RT <: Tape[?, ?, ?],
      RI <: Stream,
      RO <: Stream
  ](using
      Eval[P, Tape[C +: L, Zero, HNil], I, O, RT, RI, RO]
  ): Eval[Right +: P, Tape[L, C, HNil], I, O, RT, RI, RO] = ???

  given right[
      P <: HList,
      L <: HList,
      C <: Nat,
      N <: Nat,
      R <: HList,
      I <: Stream,
      O <: Stream,
      RT <: Tape[?, ?, ?],
      RI <: Stream,
      RO <: Stream
  ](using
      Eval[P, Tape[C +: L, N, R], I, O, RT, RI, RO]
  ): Eval[Right +: P, Tape[L, C, N +: R], I, O, RT, RI, RO] = ???

  given loopEnd[
      P <: HList,
      B <: HList,
      T <: Tape[?, ?, ?],
      I <: Stream,
      O <: Stream,
      BRT <: Tape[?, Zero, ?],
      BRI <: Stream,
      BRO <: Stream,
      PRT <: Tape[?, ?, ?],
      PRI <: Stream,
      PRO <: Stream
  ](using
      Eval[B, T, I, O, BRT, BRI, BRO],
      Eval[P, BRT, BRI, BRO, PRT, PRI, PRO]
  ): Eval[Loop[B] +: P, T, I, O, PRT, PRI, PRO] = ???

  given loopRepeat[
      P <: HList,
      B <: HList,
      T <: Tape[?, ?, ?],
      I <: Stream,
      O <: Stream,
      N <: Nat,
      BRT <: Tape[?, Succ[N], ?],
      BRI <: Stream,
      BRO <: Stream,
      PRT <: Tape[?, ?, ?],
      PRI <: Stream,
      PRO <: Stream
  ](using
      Eval[B, T, I, O, BRT, BRI, BRO],
      Eval[Loop[B] +: P, BRT, BRI, BRO, PRT, PRI, PRO]
  ): Eval[Loop[B] +: P, T, I, O, PRT, PRI, PRO] = ???
}

def foo =
  summon[Eval[
    Plus +: Plus +: Right +: Plus +: Loop[Left +: HNil] +: HNil,
    Tape[HNil, Zero, HNil],
    HNil,
    HNil,
    ?,
    ?,
    ?
  ]]
