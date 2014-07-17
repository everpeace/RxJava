/**
 * Copyright 2014 Netflix, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package rx.lang.scala.scalaz

import scala.language.higherKinds
import scalaz._
import rx.lang.scala.Observable
import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalaCheckBinding._
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration.Duration

/**
 * This object provides implicits for tests only.
 */
object ImplicitsForTest {

  // Equality based on sequenceEqual() method.
  implicit def observableEqual[A](implicit eqA: Equal[A]) = new Equal[Observable[A]]{
    def equal(a1: Observable[A], a2: Observable[A]) = {
      val p = Promise[Boolean]
      val sub = a1.sequenceEqual(a2).firstOrElse(false).subscribe(v => p.success(v))
      try {
        Await.result(p.future, Duration.Inf)
      } finally {
        sub.unsubscribe()
      }
    }
  }

  implicit def observableTEqual[F[_], A](implicit F0: Equal[F[Observable[A]]]):Equal[ObservableT[F,A]]
  = F0.contramap((_: ObservableT[F, A]).run)

  implicit def observableShow[A](implicit showA: Show[A], showLA:Show[List[A]])
  = Show.shows[Observable[A]](ob =>
      "Observable" + Show.showContravariant.contramap[List[A],Observable[A]](showLA)(_.toBlocking.toList).show(ob))

  implicit def observableTShow[F[_], A](implicit show: Show[F[Observable[A]]])
  = Show.shows[ObservableT[F, A]](obT =>
      "ObservableT(" + Show.showContravariant.contramap[F[Observable[A]], ObservableT[F,A]](show)(_.run).show(obT) + ")")

  implicit def observableArbitrary[A](implicit a: Arbitrary[A], array: Arbitrary[Array[A]]): Arbitrary[Observable[A]]
  = Functor[Arbitrary].map(array)(Observable.items(_:_*))

  implicit def observableTArbitrary[F[_], A](implicit A: Arbitrary[F[Observable[A]]]): Arbitrary[ObservableT[F, A]]
  = Functor[Arbitrary].map(A)(ObservableT(_))

}
