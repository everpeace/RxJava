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

import rx.lang.scala.Observable

import scala.language.higherKinds
import scalaz._

/**
 * Monad Transformer for Observable
 */
final case class ObservableT[F[_], A](run: F[Observable[A]]){
  self =>

  def concat(b: => ObservableT[F, A])(implicit F: Bind[F]) : ObservableT[F, A] = new ObservableT(F.bind(run){ ob1 =>
    F.map(b.run){ ob2 =>
      observableInstances.plus(ob1, ob2)
    }
  })

  def map[B](f: A => B)(implicit F: Functor[F]): ObservableT[F, B] = ObservableT[F, B](F.map(run)(_ map f))

  // TODO: flatMap shouldn't block. How could we do that?
  // we have to use blocking operation to extract ObservableT[F,B] from Observable[ObservableT[F,B]]
  def flatMap[B](f: A => ObservableT[F, B])(implicit F: Monad[F]): ObservableT[F, B] = ObservableT(
    F.bind(run){ ob =>
      // blocking !!
      observableInstances.foldLeft[ObservableT[F, B], ObservableT[F, B]](ob.map(f), ObservableT.empty)(_ concat _).run
    })
}


object ObservableT {
  def empty[F[_], A](implicit F: Applicative[F]): ObservableT[F, A] = new ObservableT(F.point(Observable.empty))

  def items[F[_], A](a : A*)(implicit F: Applicative[F]):ObservableT[F,A] = ObservableT(F.point(Observable.items(a:_*)))

  def fromObservable[F[_], A](ob: F[Observable[A]]) = ObservableT(ob)
}



//
// Prioritized Implicits for type class instances
//
sealed trait ObservableTInstance0 {
  implicit def observableTFunctor[F[_]](implicit F0: Functor[F]): Functor[({type λ[α] = ObservableT[F, α]})#λ] = new ObservableTFunctor[F] {
    implicit def F: Functor[F] = F0
  }

  implicit def observableTSemigroup[F[_], A](implicit F0: Bind[F]): Semigroup[ObservableT[F,A]] = new ObservableTSemigroup[F,A] {
    implicit def F: Bind[F] = F0
  }
}


private[scalaz] trait ObservableTInstances extends ObservableTInstance0 {
  implicit def observableTMonoid[F[_], A](implicit F0: Monad[F]): Monoid[ObservableT[F,A]] = new ObservableTMonoid[F,A] {
    implicit def F: Monad[F] = F0
  }

  implicit def observableTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[({type λ[α] = ObservableT[F, α]})#λ] = new ObservableTMonadPlus[F] {
    implicit def F: Monad[F] = F0
  }

  implicit val observableTHoist: Hoist[ObservableT] = new ObservableTHoist {}
}



//
// Implementation of type classes
//
private trait ObservableTFunctor[F[_]] extends Functor[({type λ[α] = ObservableT[F, α]})#λ] {
  implicit def F: Functor[F]
  override def map[A, B](fa: ObservableT[F, A])(f: A => B): ObservableT[F, B] = fa map f
}


private trait ObservableTSemigroup[F[_], A] extends Semigroup[ObservableT[F,A]] {
  implicit def F: Bind[F]
  override def append(f1: ObservableT[F, A], f2: => ObservableT[F, A]): ObservableT[F, A] = f1 concat f2
}


private trait ObservableTMonoid[F[_], A] extends Monoid[ObservableT[F,A]] with ObservableTSemigroup[F,A] {
  implicit def F: Monad[F]
  override def zero = ObservableT.empty
}


private trait ObservableTMonadPlus[F[_]] extends MonadPlus[({type λ[α] = ObservableT[F, α]})#λ] with ObservableTFunctor[F] {
  implicit def F: Monad[F]

  override def point[A](a: => A): ObservableT[F, A] = ObservableT.items(a)

  override def plus[A](a: ObservableT[F, A], b: => ObservableT[F, A]): ObservableT[F, A]
  = a concat b

  override def bind[A, B](fa: ObservableT[F, A])(f: (A) => ObservableT[F, B]): ObservableT[F, B]
  = fa flatMap f

  override def empty[A]: ObservableT[F, A] = ObservableT.empty
}


private trait ObservableTHoist extends Hoist[ObservableT] {
  override implicit def apply[G[_]](implicit G: Monad[G]): Monad[({type λ[α] = ObservableT[G, α]})#λ] = observableTMonadPlus[G]

  override def hoist[M[_], N[_]](f: ~>[M, N])(implicit M: Monad[M]): ~>[({type λ[x] = ObservableT[M, x]})#λ, ({type λ[x] = ObservableT[N, x]})#λ] = new (({type λ[x] = ObservableT[M, x]})#λ ~> ({type λ[x] = ObservableT[N, x]})#λ){
    override def apply[A](fa: ObservableT[M, A]): ObservableT[N, A] = ObservableT(f(fa.run))
  }

  override def liftM[G[_], A](ga: G[A])(implicit M: Monad[G]): ObservableT[G, A] = ObservableT(M.map(ga)(a => Observable.items(a)))

}