//https://stepik.org/lesson/Деревья-9934/step/9
//Найти количество компонент связности неориентированного графа при помощи поиска в глубину.
//
//Формат входных данных:
//На вход подаётся описание графа. В первой строке указаны два натуральных числа, разделенные пробелом: число вершин v≤1000v≤1000 и число рёбер e≤1000e≤1000. В следующих ee строках содержатся описания рёбер. Каждое ребро задаётся разделённой пробелом парой номеров вершин, которые это ребро соединяет. Считается, что вершины графа пронумерованы числами от 1 до vv.
//
//Формат выходных данных:
//
//Одно число — количество компонент связности графа.

import scala.io.StdIn

object Main {
  def main(args: Array[String]) {
    // put your code here
    val firstLine = StdIn.readLine()

    val graph = Graph()
    val eNum = graph.initFirst(firstLine)

    graph.initEdges((1 to eNum).map(i => StdIn.readLine()).toList)

    println(graph.getNum)
  }
}

case class Graph() {
  private var vNum: Int = 0
  private var eNum: Int = 0
  private var edges: Set[Edge] = Set()

  def initFirst(string: String) = {
    val list = string.split(" ")
    vNum = list(0).toInt
    eNum = list(1).toInt
    eNum
  }

  def initEdges(list: List[String]) = {
    list.foreach(str => {
      val eArr = str.split(" ")

      edges += Edge(eArr(0).toInt, eArr(1).toInt)
    })
  }

  def getNum = {
    var vertices: Set[Int] = (1 to vNum).toSet

    def checkV(vertix: Int): Unit = {
      vertices -= vertix
      for (nVertix <- edges.filter(e => e.vertex1 == vertix || e.vertex2 == vertix)
        .map(e => if (e.vertex1 == vertix) e.vertex2 else e.vertex1)) {
        if (vertices.contains(nVertix)) {
          checkV(nVertix)
        }
      }
    }

    var cnt: Int = 0
    while (vertices.nonEmpty) {
      cnt += 1
      checkV(vertices.head)
    }
    cnt
  }

  case class Edge(vertex1: Int, vertex2: Int)
}
