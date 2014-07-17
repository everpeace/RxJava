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

import org.scalacheck.Prop._

import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalazProperties._
import rx.lang.scala.Observable
import org.specs2.scalaz.Spec
import org.specs2.runner.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class ObservableSpec extends Spec{
  import rx.lang.scala.scalaz._
  import ImplicitsForTest._

  "Observable" should {
    "satisfies Equal laws (TEST USE ONLY: following tests depends on this equality.)" in {
      checkAll(equal.laws[Observable[Int]])
    }

    "satisfies Monoid laws" in {
      checkAll(monoid.laws[Observable[Int]])
    }

    "satisfies Monad laws" in {
      checkAll(monad.laws[Observable])
    }

    "satisfies MonadPlus laws" in {
      checkAll(monadPlus.strongLaws[Observable])
    }

    "satisfies IsEmpty laws" in {
      checkAll(isEmpty.laws[Observable])
    }

    "satisfies Traverse laws" in {
      checkAll(traverse.laws[Observable])
    }

    "be applied to Zip operators." in {
      forAll { (ob:Observable[Int], f: Int => Int) =>
        (ob <*|*> (_ map f)) === (ob zip (ob map f))
      }
    }
  }
}
