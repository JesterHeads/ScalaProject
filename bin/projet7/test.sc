package projet7
import jeuDeLaVie._
object test {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val testList=List("   "," X ","XXX")            //> testList  : List[String] = List("   ", " X ", XXX)
  val n=chainesToGrille(testList)                 //> n  : projet7.jeuDeLaVie.Grille = List((1,1), (2,0), (2,1), (2,2))
  n.max                                           //> res0: (Int, Int) = (2,2)
}