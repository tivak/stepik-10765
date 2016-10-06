//  https://stepik.org/lesson/%D0%AD%D0%B9%D0%BB%D0%B5%D1%80%D0%BE%D0%B2%D1%8B-%D0%B3%D1%80%D0%B0%D1%84%D1%8B-10765/step/12
//  Найдите эйлеров цикл в графе.
//
//  Формат входных данных:
//  В первой строке указаны два числа разделенных пробелом: vv (число вершин) и ee (число ребер).
//  В следующих ee строках указаны пары вершин, соединенных ребром. Выполняются ограничения: 2≤v≤1000,0≤e≤10002≤v≤1000,0≤e≤1000 .
//
//  Формат выходных данных:
//  Одно слово: NONE, если в графе нет эйлерова цикла, или список вершин в порядке обхода эйлерова цикла, если он есть.

import scala.collection.Iterator
import scala.io.StdIn

object Main {
  def main(args: Array[String]) {
    // put your code here
    val firstLine = StdIn.readLine()

    val graph = Graph()
    val eNum = graph.initFirst(firstLine)

    graph.initEdges((1 to eNum).map(i => StdIn.readLine()).toList)

    val cicle = graph.getCicle()
    if (cicle == null || cicle.isEmpty)
      println("NONE")
    else
      println(cicle.map(x => x + 1).mkString(" "))
  }
}

case class Graph() {
  private var vNum: Int = 0
  private var edgesArr: Array[List[Int]] = _
  private var edges: Map[Int, Edge] = _

  def initFirst(string: String) = {
    val list = string.split(" ")
    vNum = list(0).toInt
    edgesArr = (0 until vNum).map(x => List[Int]()).toArray
    list(1).toInt
  }

  def initEdges(list: List[String]) = {
    edges = list.indices.map(i => {
      val eArr = list(i).split(" ")

      val v1 = eArr(0).toInt - 1
      val v2 = eArr(1).toInt - 1

      i -> Edge(v1, v2)
    }).toMap

    edges.foreach(edge => {
      edgesArr(edge._2.vertex1) = edgesArr(edge._2.vertex1) :+ edge._2.vertex2
      edgesArr(edge._2.vertex2) = edgesArr(edge._2.vertex2) :+ edge._2.vertex1
    })
  }

  def getCicle() : List[Int] = {
    def nextV(v: Int, vList: List[Int], eList: Map[Int, Edge]): List[Int] = {
      if (eList.isEmpty)
        vList
      else {
        val iterator = eList.filter(x => x._2.vertex1 == v || x._2.vertex2 == v).iterator
        nextI(v, iterator, vList :+ v, eList)
      }
    }

    def nextI(v: Int, iterator: Iterator[(Int, Edge)], vList: List[Int], eList: Map[Int, Edge]): List[Int] = {
      if (iterator.hasNext) {
        val eNext = iterator.next
        val vNext = if (eNext._2.vertex1 == v) eNext._2.vertex2 else eNext._2.vertex1
        val res: List[Int] = nextV(vNext, vList, eList - eNext._1)
        if (res != null) res else nextI(v, iterator, vList, eList)
      } else
        null
    }

    if (!check())
      null
    else {
      nextV(0, List[Int](), edges)
    }
  }

  private def check(): Boolean = {
    def nextV(v: Int, vList: Set[Int]): Set[Int] = {
      if (!vList.contains(v))
        nextVI(edgesArr(v).iterator, vList + v)
      else
        vList
    }

    def nextVI(iterator: Iterator[Int], vList: Set[Int]): Set[Int] = {
      if (iterator.hasNext) {
        val vNextList: Set[Int] = nextV(iterator.next, vList)
        nextVI(iterator, vNextList)
      }
      else
        vList
    }

    if (!edgesArr.map(x => x.size).forall(x => x % 2 == 0))
      false
    else
      nextV(0, Set[Int]()).size == vNum
  }

  case class Edge(vertex1: Int, vertex2: Int)
}
