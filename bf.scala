// Brainfuck interpreter

// Open this in Scastie or some other editor, scroll to the bottom,
// and hover over `foo` to see output.

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
] {
  type Stdout = RO
  type OutTape = RT
}

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

  given input[
      P <: HList,
      L <: HList,
      C <: Nat,
      R <: HList,
      IC <: Nat,
      I <: Stream,
      O <: Stream,
      RT <: Tape[?, ?, ?],
      RI <: Stream,
      RO <: Stream
  ](using
      Eval[P, Tape[L, IC, R], I, O, RT, RI, RO]
  ): Eval[Input +: P, Tape[L, C, R], IC +: I, O, RT, RI, RO] = ???

  given output[
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
      Eval[P, Tape[L, C, R], I, C +: O, RT, RI, RO]
  ): Eval[Output +: P, Tape[L, C, R], I, O, RT, RI, RO] = ???

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

/////////////// Begin presentation stuff /////////////////////

// Using compiletime ops and match types may be "cheating," but it's
// just for making the output nicer when hovering over `foo` below.
// None of this is required for evaluation, just implicits

import scala.compiletime.ops.int.{+, S}

type NatToInt[N] <: Int = N match {
  case Zero    => 0
  case Succ[n] => 1 + NatToInt[n]
}
// Using Tuple rather than HList so that output is formatted nicely
type NatsToInts[L] <: Tuple = L match {
  case HNil   => EmptyTuple
  case n +: t => NatToInt[n] *: NatsToInts[t]
}
type TapeToInts[T <: Tape[?, ?, ?]] = T match {
  case Tape[l, c, r] => (NatsToInts[l], NatToInt[c], NatsToInts[r])
}

type IntToNat[I <: Int] <: Nat = I match {
  case 0    => Zero
  case S[n] => Succ[IntToNat[n]]
}
type TupleToHList[T] <: HList = T match {
  case EmptyTuple => HNil
  case h *: t     => h +: TupleToHList[t]
}

def eval[P <: HList, I <: Stream](using
    e: Eval[P, Tape[HNil, Zero, HNil], I, HNil, ?, ?, ?]
): (NatsToInts[e.Stdout], TapeToInts[e.OutTape]) = ???

// Hover over foo to see the results
// Output should be ((3, 0, 2, 1), ((0, 4), 5, (3, 2, 1)))
// (3, 0, 2, 1) is the output stream (reversed)
// ((0, 4), 5, (3, 2, 1)) is the tape. The (0, 4) is reversed. 5 is the current position
def foo =
  eval[
    TupleToHList[
      (
          Plus,
          Plus,
          Right,
          Plus,
          Output,
          Loop[Left +: Output +: HNil],
          Input,
          Output,
          Left,
          Left,
          Left,
          Input,
          Right,
          Right,
          Input
      )
    ],
    TupleToHList[Tuple.Map[(3, 4, 5), IntToNat]]
  ]
