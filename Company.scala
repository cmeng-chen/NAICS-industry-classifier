package company

import scala.collection.Iterator
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.io.Source
import java.net._

/**
 * WordNetProcessor.scala
 * company class, used to form company queue with given data
 * @author Jack Ye
 */
class Company(n: String, a: String, d: String, u: HashSet[String], id: String) {

	var name : String = n
	var address : String = a
	var description : String = d
	var url : HashSet[String] = u
	var uniqueID : String = id
	val filter = processVocabs("src/wordListFilter.txt")
	val selector = processVocabs("src/companyPageTraits.txt")
	var wordList : Queue[String] = generatewordList(description, url)

	final val MAX_RECURSION = 2
	final val MAX_REPEAT_DES = 3
	final val MAX_REPEAT_NAME = 10

	/**
     * form a set of vocabs, used to form filter and selector
     * used as a filter for wordNet
     * @param src: source path of vocabs in .txt form
     * @return Queue of common vocabs
     */
	def processVocabs(src: String): HashSet[String] = {
		var vocabs: HashSet[String] = HashSet()
		var fileIter = Source.fromFile(src).getLines
		var arr = fileIter.next.split("[ \t\n\f\r:,.;]+")
		for (s <- arr if s != "") vocabs.add(s)
		// println(vocabs.toString);
		return vocabs
	}

	/**
     * get html page infomation from given url, with timeout setting
     * prevent server or network malfunction
     * copied from https://argcv.com/articles/3510.c
     * @param url: page destination address
     * @param timeout: specified timeout
     * @return complete string of webpage
     */
	def fromUrlWithTimeout(url: String, timeout: Int = 8000): String = {
		import java.net.URL
		import scala.io.Source
		val conn = (new URL(url)).openConnection()
		conn.setConnectTimeout(timeout)
		conn.setReadTimeout(timeout)
		val stream = conn.getInputStream()
		val src = (scala.util.control.Exception.catching(classOf[Throwable]) opt Source.fromInputStream(stream).mkString) match {
			case Some(s: String) => s
			case _ => ""
		}
		stream.close()
		src
	}

	/**
     * generate a given wordList using given description and url
     * @param description: given description of company
     * @param urls: given set of website address of company
     * @return a wordList contianing keywords of the company
     */
	def generatewordList(description: String, urls: HashSet[String]) : Queue[String] = {
		println("generating wordlist for: " + name)
		var wordList : Queue[String] = Queue()
		generateByDescription(wordList, description)
		var duplicated : HashSet[String] = HashSet()
		for (url <- urls) generateByURL(wordList, url, url, MAX_RECURSION, duplicated)
		return wordList
	}

	/**
     * update a given wordList using given description
     * @param ws: wordList of the associated company
     * @param description: given description of company
     */
	def generateByDescription(ws: Queue[String], description: String) : Unit = {
		// println("generating by description")
		var arr = description.split("[ \t\n\f\r:,.;&/\"\'-]+")
		// arr.foreach(x => print(x + "--- "))
		// println()
		for (s <- arr) {
			// println("checking " + s)
			// println(filter.toString)
			if (s != "" && !filter.contains(s.toLowerCase)) {
				// println(s + " is added")
				for (i <- 0 until MAX_REPEAT_DES) ws += s.toLowerCase
			}
		}
		var arr2 = name.split("[ \t\n\f\r:,.;&/\"\']+")
		for (s <- arr2) {
			// println("checking " + s)
			// println(filter.toString)
			if (s != "" && !filter.contains(s.toLowerCase)) {
				// println(s + " is added")
				for (i <- 0 until MAX_REPEAT_NAME) ws += s.toLowerCase
			}
		}


	}

	/**
     * update a given wordList using given url
     * particularly, look into the "home", "about", "product" pages
     * @param ws: wordList of the associated company
     * @param url: website page address of given company
     * @param origin: original piece of copy of url, used for recursion
     * @param depth: control of number of recursions to evaluate subpages
     * @param duplicated: set of already seen websites, used to prevent multiple identical links
     */
	def generateByURL(ws: Queue[String], url: String, origin: String, depth: Int, duplicated: HashSet[String]) : Unit = {
		try {
			// println("generating by url")
			if (depth == 0) return
			// println("connecting to " + url)
			var iter = fromUrlWithTimeout(url).split("\n").iterator
			var nextLine = ""
			while (iter.hasNext) {
				nextLine = iter.next
				// println("current line: " + nextLine)
				for (s <- nextLine.split("[ >]+")) {
					if (s == "<p") {
						analyzePTag(ws, nextLine, iter)
					}
					if (s == "<a") {
						var newURL = analyzeATag(nextLine, origin)
						if (newURL != "" && !duplicated.contains(newURL)) {
							duplicated += newURL
							generateByURL(ws, newURL, url, depth - 1, duplicated)
						}
					}
				}
			}
		} catch {
			case e: Exception => println("URL Section Excpetion")
		}
	}

	
	/**
     * evaluate a given <p> tag line, read all information
     * @param ws: wordList of the associated company
     * @param pTagLine: a line of html code with a <p> or </p> tag
     * @param iter: iterator of complete html page
     */
	def analyzePTag(ws: Queue[String], pTagLine: String, iter: Iterator[String]) {
		// println("current line for ptag analysis: " + pTagLine)
		var arr = pTagLine.split("[^a-zA-Z/]+")
		// arr.foreach(x => print(x + "--- "))
		// println()
		var start = arr.length
		var end = arr.length
		for (i <- 0 until arr.length) {
			// println("matching: " + arr(i))
			if (arr(i).equals("p"))start = i
			if (arr(i).equals("/p")) end = i
		}
		// println("length: " + arr.length)
		// println("start: " + start)
		// println("end: " + end)

		if (start < arr.length && end < arr.length) {
			for (i <- start + 1 until end if !filter.contains(arr(i).toLowerCase)) ws += arr(i).toLowerCase
		}
		else if (start < arr.length) {
			// println("need recursion")
			for (i <- start + 1 until arr.length if !filter.contains(arr(i).toLowerCase)) {
				ws += arr(i).toLowerCase
			}
			analyzePTag(ws, iter.next, iter)
		}
		else if (end < arr.length) {
			for (i <- 0 until end if !filter.contains(arr(i).toLowerCase)) ws += arr(i).toLowerCase
		}
	}

	/**
	 * evaluate a given <a> tag line, get subpage address
     * @param aTagLine: a line of html code with a <a> or </a> tag
     * @param origin: original website, used to compare hyperlink and locallink
     * @return subpage address
     */
	def analyzeATag(aTagLine: String, origin: String) : String = {
		var arr = aTagLine.split("[ \t\n\f\r\"\'<>]+")
		// println("current line for a tag analysis" + aTagLine)
		// arr.foreach(x => print(x + "--- "))
		// println()
		var hasDepth = false
		for (i <- arr if selector.contains(i.toLowerCase)) hasDepth = true
		if (hasDepth) {
			for (e <- arr) {
				// println("currently evaluating " + e)
				if (e.regionMatches(e.length-5, ".html", 0, 5) || e.regionMatches(e.length-4, ".php", 0, 4)) {
					if (e.regionMatches(0, origin, 0, origin.length)) return e
					else return origin + "/" + e
				}
			}
		}
		return ""
	}

	/**
	 * company info expression, used for test purpose
     * @return company expression
     */
	override def toString : String = {
		return "Company: " + name + "\n" + 
							address + "\n" +   
							url.toString + "\n"
	}
}










