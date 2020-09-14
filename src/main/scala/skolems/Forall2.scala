/*
 * Copyright 2019 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package skolems

import scala.annotation.unchecked.uncheckedVariance

private[skolems] sealed trait Parent2 {
  private[skolems] type Apply[A,B]
}

trait Forall2[+F[_,_]] extends Parent2 { outer =>
  private[skolems] type Apply[A,B] = F[A,B] @uncheckedVariance
  def apply[A,B]: F[A,B]
}

object Forall2 {
  // cannot be referenced outside of Forall2
  private[Forall2] type τ
  private[Forall2] type τ2

  /**
   * This function is intended to be invoked explicitly, the implicit
   * modifiers are simply because the compiler can infer this in many
   * cases. For example:
   *
   * implicit def eitherMonad[A]: Monad[Either[A, ?]] = ???
   *
   * implicitly[∀[α => Monad[Either[α, ?]]]]
   *
   * The above will work.
   */
  def apply[F[_,_]](ft: F[τ,τ2]): Forall2[F] =
    new Forall2[F] {
      def apply[A,B] = ft.asInstanceOf[F[A,B]]
    }

  /**
   * This is the implicit version of apply, but restructured and encoded
   * such that the F is unconstrained in in arity or fixity.
   */
  implicit def materialize[T <: Parent2](implicit ft: T#Apply[τ,τ2]): T =
    apply(ft).asInstanceOf[T]

  def raise[F[_,_], B](f: Forall2[λ[(α,α2) => F[α,α2] => B]]): ∃∃[F] => B =
    ef => f[ef.A,ef.A2](ef.value)

  def lower[F[_,_], B](f: ∃∃[F] => B): Forall2[λ[(α,α2) => F[α,α2] => B]] =
    Forall2[λ[(α,α2) => F[α,α2] => B]](fa => f(∃∃[F](fa)))

  def lowerA[F[_,_], B, C]: PartiallyAppliedLowerA[F, B, C] =
    new PartiallyAppliedLowerA[F, B, C]

  final class PartiallyAppliedLowerA[F[_,_], B, C] {
    def apply[A,A2](f: ∃∃[F] => B): F[A,A2] => B =
      lower[F, B](f)[A,A2]
  }

  /**
   * Utilities to implicitly materialize let-bound polymorphic contexts.
   */
  /*object Implicits {
    implicit def materializeUnification[P <: Parent2, A, E](
        implicit P: P,
        ev: P#Apply[E] <:< A): A =
      ev(P.asInstanceOf[Forall2[P.Apply]][E])
  }*/
}
