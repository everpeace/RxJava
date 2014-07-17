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

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.scalaz.Spec

import scalaz._
import Scalaz._
import scalaz.scalacheck.ScalazProperties._

@RunWith(classOf[JUnitRunner])
class ObservableTSpec extends Spec {

  import rx.lang.scala.scalaz._
  import ImplicitsForTest._

  type ObservableTId[A] = ObservableT[Id, A]
  type ObservableTList[A] = ObservableT[List, A]
  type ObservableTOption[A] = ObservableT[Option, A]

  "ObservableT" should {
    "satisfies Equal laws (TEST USE ONLY: following tests depends on this equality.)" in {
      checkAll(equal.laws[ObservableTId[Int]])
      checkAll(equal.laws[ObservableTList[Int]])
      checkAll(equal.laws[ObservableTOption[Int]])
    }

    "satisfies Monoid laws" in {
      checkAll(monoid.laws[ObservableTId[Int]])
      checkAll(monoid.laws[ObservableTList[Int]])
      checkAll(monoid.laws[ObservableTOption[Int]])
    }

    "satisfies MonadPlus laws" in {
      checkAll(monadPlus.laws[ObservableTId])
      checkAll(monadPlus.laws[ObservableTList])
      checkAll(monadPlus.laws[ObservableTOption])
    }
  }
}
