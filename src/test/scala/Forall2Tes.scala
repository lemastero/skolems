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

import skolems.∀∀

class Forall2Tes {
  trait Profunctor[=>:[_, _]] {
    def dimap[S,T,A,B](sa: S => A, bt: B => T): A =>: B => S =>: T
  }

  type Optics[P[_, _],S,T,A,B] = P[A,B] => P[S,T]
  type S = Int
  type T = Int
  type A = Int
  type B = Int
  type Iso1[P[_,_]] = Profunctor[P] => Optics[P,S,T,A,B]
  type Iso2[P[_,_]] = ∀∀[Iso1]

  //type Iso3[S,T,A,B] = ∀∀[λ[P => Profunctor[P] => Optics[P,S,T,A,B]]]
}


