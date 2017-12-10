import scala.collection.mutable.HashMap

object Solution {
    var uset: Array[Int] = Array.empty
    var rank: Array[Int] = Array.empty
    var indexMap: HashMap[String, Int] = HashMap.empty

    def find(x: Int): Int = {
        if (uset(x) != x) {
            uset(x) = find(uset(x))
        }
        return uset(x)
    }

    def union(x: Int, y: Int) = {
        val rx = find(x)
        val ry = find(y)
        if (rx != ry) {
            if (rank(rx) <= rank(ry)) {
                uset(ry) = rx
                rank(rx) += rank(ry)
            } else {
                uset(rx) = ry
                rank(ry) += rank(rx)
            }
        }
    }

    def areSentencesSimilarTwo(words1: Array[String], words2: Array[String], pairs: Array[Array[String]]): Boolean = {
        var idx: Int = 0

        uset = Array.range(0, 4000)
        rank = Array.fill(4000)(0)
        indexMap = HashMap.empty

        if (words1.length != words2.length) {
            return false
        }

        def getIdx(wd: String): Int = {
            indexMap.get(wd) match {
                case None => {
                    indexMap += wd -> idx
                    idx += 1
                    return idx - 1
                }
                case Some(idx) => idx
            }
        }

        // build disjoint set
        pairs.foreach(pair => {
            val idxa = getIdx(pair(0))
            val idxb = getIdx(pair(1))
            union(idxa, idxb)
        })

        words1.zip(words2).forall { case (a, b) => find(getIdx(a)) == find(getIdx(b)) }
    }

    def test(): Boolean = {
        val words1 = Array("great", "acting", "skills")
        val words2 = Array("fine", "drama", "talent")
        val pairs = Array(
            Array("great", "good"),
            Array("fine", "good"),
            Array("acting","drama"),
            Array("skills","talent")
        )
        areSentencesSimilarTwo(words1, words2, pairs)
    }
}
