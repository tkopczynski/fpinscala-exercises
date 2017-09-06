package chapter3

/**
  * Created by Tomasz Kopczynski.
  */
object Chapter3 {

  def sum(integers: List[Int]): Int = {
    List.foldLeft(integers, 0)(_ + _)
  }

  def product(integers: List[Int]): Int = {
    List.foldLeft(integers, 1)(_ * _)
  }

  def addOne(integers: List[Int]): List[Int] = {
    List.foldLeft(integers, Nil: List[Int])((l, e) => List.append(l, e+1))
  }

  def listToString(doubles: List[Double]): List[String] =
    List.foldLeft(doubles, Nil:List[String])((l, e) => List.append(l, e.toString))

  def addLists(l1: List[Int], l2: List[Int]): List[Int] = {
      l1 match {
        case Nil => Nil
        case Cons(head, tail) => {
          l2 match {
            case Nil => Nil
            case Cons(head2, tail2) => Cons(head + head2, addLists(tail, tail2))
          }
        }
      }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(left, right) => maximum(left).max(maximum(right))
    }
  }

  def maximumFold(t: Tree[Int]): Int = Tree.fold(t)(l => l.value)(b => maximumFold(b.left).max(maximumFold(b.right)))

//  def main(args: Array[String]): Unit = {
//    println(Tree.map(Branch(Leaf(1), Leaf(2)))(_ + 1))
//    println(Tree.depth(Branch(Leaf(1), Leaf(2))))
//    println(maximum(Branch(Leaf(1), Leaf(2))))
//    println(Tree.size(Branch(Leaf(1), Leaf(2))))
//    println(List.hasSubsequence(List(1,2,3,4), List(1,3)))
//    println(List.zipWith(List(1,2,3), List(4,5,6))(_ + _))
//    println(addLists(List(1,2,3), List(4,5,6)))
//    println(List.flatMap(List(1,2,3))(i => List(i,i)))
//    println(List.filter(List(1,2,3,4))(i => i % 2 == 0))
//    println(List.filterFlat(List(1,2,3,4))(i => i % 2 == 0))
//    println(List.map(List(1,2,3))(_ + 1))
//    println(listToString(List(1.0:Double, 2.0:Double, 3.5: Double)))
//    println(addOne(List(1,2,3)))
//    println(List.concat(List(List(1,2,3,4),List(5,6,7,8,9))))
//  }
}
