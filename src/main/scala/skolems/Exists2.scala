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

trait Exists2[+F[_,_]] {
  type A
  type A2
  val value: F[A,A2]
}

object Exists2 {

  /**
   * A forgetful constructor which packs a concrete value into an existential.
   * This is mostly useful for explicitly assisting the compiler in sorting all
   * of this out.
   */
  def apply[F[_,_]]: PartiallyApplied[F] =
    new PartiallyApplied[F]

  final class PartiallyApplied[F[_,_]] {
    def apply[A0,A20](fa: F[A0,A20]): Exists2[F] =
      new Exists2[F] {
        type A = A0
        type A2 = A20
        val value = fa
      }
  }

  def unapply[F[_,_]](ef: Exists2[F]): Some[F[A,A2] forSome { type A; type A2 }] =
    Some(ef.value)

  /**
   * Tricksy overload for when you want everything to "just work(tm)".
   * The implicit modifiers are to allow the compiler to materialize
   * things through implicit search when relevant; don't be afraid to
   * call this function explicitly.
   */
  implicit def apply[F[_,_], A, A2](implicit fa: F[A,A2]): Exists2[F] =
    apply[F](fa)

  // non-implicit parameter version designed to provide nicer syntax
  implicit def coerce[F[_,_], A, A2](F: F[A,A2]): Exists2[F] = apply(F)

  def raise[F[_,_], B](f: Exists2[λ[(α,α2) => F[α,α2] => B]]): ∀∀[F] => B =
    af => f.value(af[f.A,f.A2])

  // This cast is required because Scala's type inference will not allow
  // the parameter from the ∀ invocation (which is unreferenceable) to
  // flow "outward" and form the constrained input type of the function.
  // So because we don't have non-local inference in Scala, we need to
  // play tricks with erasure and cast from F[Any].
  def lower[F[_,_], B](f: ∀∀[F] => B): Exists2[λ[(α,α2) => F[α,α2] => B]] =
    Exists2[λ[(α,α2) => F[α,α2] => B]]((fa: F[Any,Any]) => f(∀∀[F](fa.asInstanceOf)))

  def lowerE[F[_,_], B](f: ∀∀[F] => B): (F[A,A2] => B) forSome { type A; type A2 } =
    lower[F, B](f).value

  /**
   * Utilities to implicitly materialize native `forSome` contexts.
   */
  /*object Implicits {
    implicit def materialize[F[_]](implicit F: Exists[F]): F[A] forSome { type A } = F()
  }*/
}
