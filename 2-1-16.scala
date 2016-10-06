//https://stepik.org/lesson/Деревья-9934/step/16
//На вход программе подаётся описание простого связного графа. Первая строка содержит два числа: число вершин V≤10000V≤10000 и число рёбер E≤30000E≤30000 графа. Следующие EE строк содержат номера пар вершин, соединенных рёбрами. Вершины имеют номера от 00 до V−1V−1. Выведите список из VV чисел — расстояний от вершины 0 до соответствующих вершин графа.

import scala.collection.{Map, SortedMap}
import scala.collection.mutable.Queue
import scala.io.StdIn

object Main {
  def main(args: Array[String]) {
    // put your code here
    val firstLine = StdIn.readLine()

    val graph = Graph()
    val eNum = graph.initFirst(firstLine)

    graph.initEdges((1 to eNum).map(i => StdIn.readLine()).toList)

    val distance = graph.getDistance()
    println(distance.mkString(" "))
  }
}

case class Graph() {
  private var vNum: Int = 0
//  private var eNum: Int = 0
//  private var edges: Set[Edge] = Set()
//  private var edgesMap: Map[Int, Set[Int]] = _
  private var edgesArr: Array[Set[Int]] = _

  def initFirst(string: String) = {
    val list = string.split(" ")
    vNum = list(0).toInt
//    eNum =
//    eNum
//    edgesArr = new Array[Set[Int]](vNum)
    edgesArr = (0 until vNum).map(x => Set[Int]()).toArray
    list(1).toInt
  }

  def initEdges(list: List[String]) = {
    list.foreach(str => {
      val eArr = str.split(" ")

      val v1 = eArr(0).toInt
      val v2 = eArr(1).toInt

//      edges += Edge(v1, v2)

      edgesArr(v1) += v2
      edgesArr(v2) += v1
    })

//    edgesMap = (0 to vNum).map(vertex => vertex ->
//      edges.filter(e => e.vertex1 == vertex || e.vertex2 == vertex).map(e => if (e.vertex1 == vertex) e.vertex2 else e.vertex1)
//    ).toMap
  }

  def getDistance() : Array[Int] = {
    val vDistance:Array[Int] = (0 until vNum).map(x => -1).toArray

    var queue: Queue[Int] = Queue()
    queue += 0
    queue += -1

    var distance = 0

    while (queue.nonEmpty) {
      val vertex = queue.dequeue

      if (vertex == -1) {
        if (queue.nonEmpty) {
          distance += 1
          queue += -1
        }
      }
      else if (vDistance(vertex) == -1) {
        vDistance(vertex) = distance

//        for (nVertex <- edges.filter(e => e.vertex1 == vertex || e.vertex2 == vertex)
//          .map(e => if (e.vertex1 == vertex) e.vertex2 else e.vertex1)) {
//        for (nVertex <- edgesMap(vertex)) {
        for (nVertex <- edgesArr(vertex)) {
          if (vDistance(nVertex) == -1) {
            queue += nVertex
          }
        }
      }
    }

    vDistance
  }

  case class Edge(vertex1: Int, vertex2: Int)
}
