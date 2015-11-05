package wordnet

import scala.io.Source
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

/**
 * CodeMapping.scala
 * Construct CodeMapping from NAICS classification definiton
 * @author Chen Meng
 */
object CodeMapping {

    /**
     * Node subclass representing simple Trie structure
     */
	class Node() {
        val links = new HashMap[String,Node]()
    }

	/**
     * form a set of code and its descriptions
     * used as a filter for codeMapping
     * @param src: source path of vocabs in .txt form
     * @return HashSet of code and its descriptions
     */
	def processCode(src: String): Node = {
		var fileIter = Source.fromFile(src).getLines
		var sortedCode = new Node()
        while (fileIter.hasNext) {
            insert(sortedCode, fileIter.next.split("[ \t\n\f\r:,.;]+")(0))
        }
		return sortedCode
	}

    /**
     * insert a string into the given Node
     * @param x: the node need for insertion
     * @param s: String needed to be insert
     * @return insert string s to its node
     */
    def insert(x: Node, s: String): Unit = {
    	if (s.toInt < 100) {
    		x.links.put(s, new Node())
    	} else {
            for (ele <- x.links.keySet){
                if (s.regionMatches(0, ele, 0, ele.length)){
                    var nextNode = x.links.get(ele).get
                    if (s.length - ele.length == 1) nextNode.links.put(s, new Node())
                    else insert(nextNode, s)
                } else if (ele == "31" && (s.regionMatches(0, "32", 0, 2) || s.regionMatches(0, "33", 0, 2))) {
                    insert(x.links.get("31").get, s)
                } else if (ele == "44" && s.regionMatches(0, "45", 0, 2)) {
                    insert(x.links.get("44").get, s)
                } else if (ele == "48" && s.regionMatches(0, "49", 0, 2)) {
                    insert(x.links.get("48").get, s)
                }
            }
        }
    }

    /**
     * print Node recursively, used for testing
     * @param x: the node need for printing
     */
    def printNode(x: Node): Unit = {
        println(x.links.keySet.toString)
    	for(n <- x.links.keySet){
            print(n + " --> ")
    		printNode(x.links.get(n).get)
    	}
    }

    /**
     * shortcut for constructing CodeMapping
     * @return complete codeMapping from given source file
     */
    def construct : Node = { return processCode("src/2-digit.txt")}

}




