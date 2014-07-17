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
package rx.lang.scala.scalaz.examples


import scala.language.higherKinds

import org.specs2.matcher.AnyMatchers
import org.specs2.scalaz.{ScalazMatchers, Spec}
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import rx.lang.scala.Observable
import rx.lang.scala.Observable.items

/**
 * This demonstrates how you apply Scalaz's operators to Observables.
 */
@RunWith(classOf[JUnitRunner])
class RxScalazDemo extends Spec with AnyMatchers with ScalazMatchers{

  import scalaz._
  import Scalaz._
  import rx.lang.scala.scalaz._
  import ImplicitsForTest._

  "Observable" should {
    "be applied to Monoid operators" in {
      (items(1, 2) |+| items(3, 4)) must equal(items(1, 2, 3, 4))

      (items(1, 2) ⊹   items(3, 4)) must equal(items(1, 2, 3, 4))

      mzero[Observable[Int]] must equal(Observable.empty)
    }

    "be applied to Functor operators" in {
      (items(1, 2) ∘ {_ + 1}) must equal(items(2, 3))

      (items(1, 2) >| 5) must equal(items(5, 5))

      (items(1, 2) as 4) must equal(items(4, 4))

      items(1, 2).fpair must equal(items((1, 1), (2, 2)))

      items(1, 2).fproduct {_ + 1} must equal(items((1, 2), (2, 3)))

      items(1, 2).strengthL("x") must equal(items(("x", 1), ("x", 2)))

      items(1, 2).strengthR("x") must equal(items((1, "x"), (2, "x")))

      Functor[Observable].lift {(_: Int) + 1}(items(1, 2)) must equal(items(2, 3))
    }

    "be applied to Applicative operators" in {
      1.point[Observable] must equal(items(1))

      1.η[Observable] must equal(items(1))

      (items(1, 2) |@| items(3, 4)) {_ + _} must equal(items(4, 5, 5, 6))

      (items(1) <*> {(_: Int) + 1}.η[Observable]) must equal(items(2))

      items(1) <*> {items(2) <*> {(_: Int) + (_: Int)}.curried.η[Observable]} must equal(items(3))

      items(1) <* items(2) must equal(items(1))

      items(1) *> items(2) must equal(items(2))

      Apply[Observable].ap(items(2)) {{(_: Int) + 3}.η[Observable]} must equal(items(5))

      Apply[Observable].lift2 {(_: Int) * (_: Int)}(items(1, 2), items(3, 4)) must equal(items(3, 4, 6, 8))
    }

    "be applied to Monad and MonadPlus operators" in {
      (items(3) >>= {(i: Int) => items(i + 1)}) must equal(items(4))

      (items(3) >> items(2)) must equal(items(2))

      items(items(1, 2), items(3, 4)).μ must equal(items(1, 2, 3, 4))

      (items(1, 2) <+> items(3, 4)) must equal(items(1, 2, 3, 4))

      PlusEmpty[Observable].empty[Int] must equal(Observable.empty)
    }
    "be applied to Traverse operators" in {
      items(1, 2, 3).foldMap {_.toString} must equal("123")

      items(1, 2, 3).foldLeftM(0)((acc, v) => (acc + v).some) must equal(6.some)

      items(1, 2, 3).suml must equal(6)

      items(1, 2, 3).∀(_ > 0) must equal(true)

      items(1, 2, 3).∃(_ > 2) must equal(true)

      items(1, 2, 3).traverse(x => (x + 1).some) must equal(items(2, 3, 4).some)

      items(1.some, 2.some).sequence must equal(items(1, 2).some)
    }
  }

  "ObservableT[List,_]" should {

    type ObservableTList[A] = ObservableT[List, A]

    // simple contructor
    // ObservableT(items(a1,a2,...)::Nil)
    def itemsT[A](a: A*) = ObservableT.items[List, A](a: _*)

    // We can lift List monad to Observable by using ObservableT.
    // liftT(1,2) => ObservableT(items(1)::items(2)::Nil)
    def liftT[A](as: A*) = List(as: _*).liftM[ObservableT]

    "be used as a composit Monoid of List and Observable" in {
      val ans = ObservableT(items(1, 4) :: items(1, 5) :: items(2, 4) :: items(2, 5) :: Nil)

      (liftT(1, 2) |+| liftT(4, 5)) must equal(ans)

      (liftT(1, 2) ⊹ liftT(4, 5)) must equal(ans)

      mzero[ObservableT[List, Int]] must equal(ObservableT.empty[List, Int])
    }

    "be used as  a composit Functor of List and Observable" in {
      liftT(1, 2) ∘ {
        _ + 1
      } must equal(liftT(2, 3))

      (liftT(1, 2) >| 5) must equal(liftT(5, 5))

      (liftT(1, 2) as 4) must equal(liftT(4, 4))

      liftT(1, 2).fpair must equal(liftT((1, 1), (2, 2)))

      liftT(1, 2).fproduct {
        _ + 1
      } must equal(liftT((1, 2), (2, 3)))

      liftT(1, 2).strengthL("x") must equal(liftT(("x", 1), ("x", 2)))

      liftT(1, 2).strengthR("x") must equal(liftT((1, "x"), (2, "x")))

      Functor[ObservableTList].lift {
        (_: Int) + 1
      }(liftT(1, 2)) must equal(liftT(2, 3))
    }

    "be used as a composit Applicative functor of List and Observable" in {
      1.point[ObservableTList] must equal(liftT(1))

      1.η[ObservableTList] must equal(liftT(1))

      (liftT(1, 2) |@| liftT(3, 4)) {
        _ + _
      } must equal(liftT(4, 5, 5, 6))

      (liftT(1) <*> {
        (_: Int) + 1
      }.η[ObservableTList]) must equal(liftT(2))

      liftT(1) <*> {
        liftT(2) <*> {
          (_: Int) + (_: Int)
        }.curried.η[ObservableTList]
      } must equal(liftT(3))

      liftT(1, 2) <* liftT(3, 4) must equal(liftT(1, 1, 2, 2))

      liftT(1, 2) *> liftT(3, 4) must equal(liftT(3, 4, 3, 4))

      Apply[ObservableTList].ap(liftT(2)) {
        {
          (_: Int) + 3
        }.η[ObservableTList]
      } must equal(liftT(5))

      Apply[ObservableTList].lift2 {
        (_: Int) * (_: Int)
      }(liftT(1, 2), liftT(3, 4)) must equal(liftT(3, 4, 6, 8))
    }

    "be used as a composit Monad and MonadPlus functor of List and Observable" in {
      (liftT(1, 2) >>= { i =>
        itemsT(i + 1, i + 2)
      }) must equal(ObservableT(items(2, 3) :: items(3, 4) :: Nil))

      // List[Observable[_]] can be treated as composit Monad.
      (for {
        i <- liftT(1, 2)
        k <- List(i + 1, i + 2).liftM[ObservableT] // we can lift List to List[Observable[_]]
      } yield k) must equal(ObservableT(items(2) :: items(3) :: items(3) :: items(4) :: Nil))

      (liftT(4, 3) >> liftT(2, 1)) must equal(liftT(2, 1, 2, 1))

      liftT(liftT(1, 2), liftT(3, 4)).μ must equal(liftT(1, 2, 3, 4))

      (liftT(1, 2) <+> liftT(3, 4)) must equal(ObservableT(items(1, 3) :: items(1, 4) :: items(2, 3) :: items(2, 4) :: Nil))

      PlusEmpty[ObservableTList].empty[Int] must equal(ObservableT.empty[List, Int])
    }
  }

  "More practical examples" should {
    "be showed with ObservableT[Option,_]" in {
      val ob1 = items(1, 2, 3, 4)
      val map = Map(1 -> 10, 2 -> 20, 3 -> 30, 4 -> 40)
      (for {
        i <- ObservableT(Option(ob1))      //Option[Observable[Int]]
        j <- map.get(i).liftM[ObservableT] //Option can be lift to Option[Observable[Int]]
      } yield (i, j+1)) must equal(ObservableT(Option(items((1,11),(2,21),(3,31),(4,41)))))
    }
  }
}
