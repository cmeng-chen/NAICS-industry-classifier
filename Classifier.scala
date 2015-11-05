package classifier

import company.Company
import wordnet._
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.immutable.List
import scala.util.parsing.json
import scala.io.Source
import scala.io.BufferedSource
import scala.io.StdIn.readLine
import java.io._

/**
 * Classifier.scala
 * running algorithm to determine the classification of the given company queue
 * output the result into an .txt file
 * @author Jack Ye, Chen Meng
 */
object Classifier {

	final val CONTROL = -1

	/**
	 * read json file containing company information
	 * classify given company using wordNet and codeMapping structure
	 * write result to output destination
	 * @param src: company json source file
	 * @param dst: destination file written
	 * @param wn: wordNet
	 * @param map: codeMapping
	 * @param current: current identified industry id to carry further
	 */
	def classify(src: String, dst: String, wn: HashMap[String, HashSet[String]], map: CodeMapping.Node, current: String) : Unit = {
		val file = new File(dst)
		val bw = new BufferedWriter(new FileWriter(file))
		val json_Cstring = Source.fromFile(src).getLines.mkString
		val cmap = json.JSON.parseFull(json_Cstring).get.asInstanceOf[List[scala.collection.immutable.HashMap.HashTrieMap[String, Object]]]
		for (company <- cmap) {
			var c = mapToComapny(company)
			println("start classifying: " + c.name)
			// println("unique id: " + c.uniqueID)
			// println(c.wordList.toString)
			var level = map.links
			var rank : HashMap[String, Double] = HashMap()
			for (id <- level.keySet) {
				var ws = wn.get(id).get
				// println("current wordNet:" + id + " --> " + ws.toString)
				var count = 0
				for (w <- c.wordList) {
					if (containsPart(ws, w)) count += 1
				} 
				// println("rank: " + id + " --- " + count + " in " + ws.size)
				var p : Double = 0
				if (current == "" && ws.size < 100) p = (count + CONTROL).toDouble / ((ws.size + CONTROL) * 5).toDouble
				else if (current == "" && ws.size < 600) p = (count + CONTROL).toDouble / ((ws.size + CONTROL) * 1.2).toDouble
				else p = (count + CONTROL).toDouble / (ws.size + CONTROL).toDouble
				// println("portion is: " + p)
				rank.put(id, p)
			}
			// println(rank.toString)
			var maxID = ""
			var maxPortion : Double = -100
			for (r <- rank.keySet) {
				var portion = rank.get(r).get
				// println("checking id: " + r)
				// println("checking portion: " + portion)
				if (portion > maxPortion) {
					maxID = r
					maxPortion = portion
					// println("maxID: " + maxID)
					// println("maxPortion: " + maxPortion)
				}
			}
			var curr = ""
			if (maxPortion != -100) {
				// println("recursion to next: " + maxID)
				curr  = classifyFurther(c, wn, map.links.get(maxID).get, maxID)
			} else {
				curr = current
			}
			bw.write(c.uniqueID)
			bw.write("\t")
			bw.write(curr)
			bw.write("\n")
		}
		bw.close()
	}

	/**
	 * recursively classify given company using wordNet and codeMapping structure
	 * @param c: given company
	 * @param wn: wordNet
	 * @param map: codeMapping
	 * @param current: current identified id to carry on
	 * @return calculated classification id of given company
	 */
	def classifyFurther(c: Company, wn: HashMap[String, HashSet[String]], map: CodeMapping.Node, current: String) : String = {
		var level = map.links
		var rank : HashMap[String, Double] = HashMap()
		for (id <- level.keySet) {
			var ws = wn.get(id).get
			var count = 0
			for (w <- c.wordList) {
				if (containsPart(ws, w)) count += 1
			} 
			var p : Double = 0
			if (current == "" && ws.size < 100) p = (count + CONTROL).toDouble / ((ws.size + CONTROL) * 5).toDouble
			else if (current == "" && ws.size < 600) p = (count + CONTROL).toDouble / ((ws.size + CONTROL) * 1.2).toDouble
			else p = (count + CONTROL).toDouble / (ws.size + CONTROL).toDouble
			rank.put(id, p)
		}
		var maxID = ""
		var maxPortion : Double = -100
		for (r <- rank.keySet) {
			var portion = rank.get(r).get
			if (portion > maxPortion) {
				maxID = r
				maxPortion = portion
			}
		}
		if (maxPortion == -100) {
			return current
		}
		else {
			return classifyFurther(c, wn, map.links.get(maxID).get, maxID)
			}
	}

	/**
	 * check whether an element in the set contains a part of s
	 * abandoned after testing for correctness for classification
	 * @param s: given string
	 * @return true if an element in the set contains a part of s
	 */
	def containsPart(set: HashSet[String], s: String) : Boolean = {
		return set.contains(s)
	}

	/**
     * make a company from the given HashMap(immutable)
     * @param des: given source file path
     * @return a company object with information obtained from json object
     */
	def mapToComapny(des: scala.collection.immutable.HashMap[String, Object]) : Company = {
		val web = des.get("website").get.asInstanceOf[List[String]]
		var website = new HashSet[String]()
		for (w <- web) {
			website += w
		}
		val description = des.get("description").get.asInstanceOf[String]
		val address = des.get("address").get.asInstanceOf[String]
		val unique_id = des.get("unique_id").get.asInstanceOf[String]
		val name = des.get("name").get.asInstanceOf[String]
		// println("name: " + name)
		val c = new Company(name, address, description, website, unique_id)
		return c
	}

	/**
	 * main method for the whole program
	 * run classification algorithm on given queue of companies and output to given destination
	 * @param args: stdIn string array
	 */
	def main(args: Array[String]): Unit = {
		println("*************************************")
		println("****** Now Starting Classifier ******")
		println("**** Author: Jack Ye, Chen Meng ****")
		println("*************************************")
		println("Constructing wordNet.........")
		val wordNet = WordNetProcessor.construct
		println("Constructing CodeMapping.........")
		val codeMapping = CodeMapping.construct
		println("Starting Classification.........")
		classify("src/challenge_set.json", "result.txt", wordNet, codeMapping, "")
		println("Classification Result is available at result.txt.........")

	}






}