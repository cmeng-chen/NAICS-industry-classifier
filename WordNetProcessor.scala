package wordnet

import scala.io.Source
import scala.io.BufferedSource
import scala.io.StdIn.readLine
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

/**
 * WordNetProcessor.scala
 * Construct WordNet from NAICS classification definiton
 * @author Jack Ye
 */
object WordNetProcessor {

/*******************************************************
************* constructor for wordNet ******************
********************************************************/
	
	final val MAX_SET_SIZE = 100
	final val MIN_SET_SIZE = 40
	/**
     * form a set of common vocabs
     * used as a filter for wordNet
     * @param src: source path of vocabs in .txt form
     * @return HashSet of common vocabs
     */
	def processCommonVocab(src: String): HashSet[String] = {
		var commonVocabs: HashSet[String] = HashSet()
		var fileIter = Source.fromFile(src).getLines
		var arr = fileIter.next.split("[ \t\n\f\r:,.;]+")
		for (s <- arr if s != "") commonVocabs.add(s)
		return commonVocabs
	}

	/**
     * check if there're any brackets in the array
     * if so, mutate the given array to delete bracketed words
     * @param arr: array for check
     */
	def checkBracket(arr: Array[String]): Unit = {
		var i = 0

		//words in the form "(word)": just set the word to empty
		for (i <- 0 until arr.length) {
			if (arr(i) != "" && arr(i).charAt(0) == '(' && arr(i).charAt(arr(i).length - 1) == ')') {
				arr(i) = ""
			}
		}

		//words in the form "(word", set everything later to empty until a new ')' reached
		var leftB : HashSet[Int] = HashSet()
		for (i <- 0 until arr.length) {
			if (arr(i) != "" && arr(i).charAt(0) == '(') {
				leftB.add(i)
			}
		}
		for (i <- leftB) {
			var k = i
			while (k < arr.length && (arr(k) == "" || arr(k).charAt(arr(k).length - 1) != ')')) {
					arr(k) = ""
					k = k + 1
			}
			if (k < arr.length) arr(k) = ""
		}

		//words in the form "word)", set everything before to empty until a new '(' reached
		var rightB = -1;
		for (i <- arr.length -1 to 0 by -1) {
			if (arr(i) != "" && arr(i).charAt(arr(i).length - 1) == ')') {
				rightB = i;
			}
		}
		for (i <- rightB to 0 by -1 if rightB >= 0) {
			arr(i) = ""
		}
	}

	/**
     * check if there're any pattern of "sector"/subsector"/"industry"/"industry group" in the array
     * if patterns occurs and the following is a number, not current ID, clear the whole array
     * @param arr: array for check
     * @param currentID: current input id for wordNet
     */
	def checkSubsector(arr: Array[String], currentID: String): Unit = {
		if (currentID != "") {
			var i = 0
			var hasSub = false
			for (i <- 0 until arr.length) {
				if (arr(i).toLowerCase.equals("subsector") || arr(i).toLowerCase.equals("industry")) {
					if (i <= arr.length - 2 && arr(i+1).matches("\\d+") && arr(i+1) != currentID) {
						hasSub = true
					}
					else if (i <= arr.length - 3 && arr(i+1).toLowerCase.equals("group") && arr(i+2).matches("\\d+") && arr(i+1) != currentID) {
						hasSub = true
					}
				}
				else if (!(i == 0 && arr.length > 2 && arr(i+1).equals("Sector") && arr(0) == ""))  {
					if (arr(i).toLowerCase.equals("sector") && i <= arr.length - 2 && arr(i+1).matches("\\d+") && arr(i+1) != currentID) {
						hasSub = true
					}
				}
			}
			if (hasSub) {
				for (i <- 0 until arr.length) arr(i) = ""
			}
		}
	}

	/**
     * form a wordnet map
     * @param src: source path of NAICS classification definitons in .txt form
     * @param commonVocabs: HashSet of common vocabs as filter
     * @return HashMap of wordNet, key:classification id, value: collection of words
     */
	def processWordNet(src: String, commonVocabs: HashSet[String]): HashMap[String, HashSet[String]] = {
		val wordNet : HashMap[String, HashSet[String]] = HashMap()
		var definitions = Source.fromFile(src).getLines
		var arr : Array[String] = Array[String]()
		var currentID : String = ""
		var currentSet : HashSet[String] = HashSet()
		var cr = false
		var i = 0
		while (definitions.hasNext) {

			//update arr to be the next line and check brackets
			arr = definitions.next.split("[ \t\n\f\r:,.;/\"\']+")
			checkBracket(arr)
			checkSubsector(arr, currentID)

			for (i <- 0 until arr.length) {
				// println("examing " + arr(i))
				//if the first word is "Sector", the next must be a two digit id
				if (arr.length > 2 && i == 0 && arr(1).equals("Sector")) {
					// println("sector start id")
					cr = false
					if (currentID != "") wordNet.put(currentID, currentSet)
					var temp = arr(2).split("--")
					currentID = temp(0)
					if (temp.length > 1) arr(2) = temp(1)
					else arr(2) = ""
					currentSet = new HashSet[String]()
				}

				//if the first word is a number, it is a 3-6 digit id
				else if (i == 0 && arr(i).matches("\\d+")) {
					if (arr(i).toInt > 10) {
						// println("classification start id")
						cr = false
						if (currentID != "") wordNet.put(currentID, currentSet)
						currentID = arr(0)
						currentSet = new HashSet[String]()
					}
				}

				//if cross reference appears, jump to next id
				else if (i == 0 && arr(i).toLowerCase.equals("cross-references")) {
					// println("ignore content follow")
					cr = true
				}
				else if (arr.length > 1 && i == 0 && arr(1).toLowerCase.equals("conversely")) {
					cr = true
				}

				//for other vocabs, if not in commonVocabs, add to wordNet
				else if (!(cr || arr(i) == "" || commonVocabs.contains(arr(i).toLowerCase()))) {
					// println("added word " + arr(i) + " to " + currentID)
					if (!arr(i).matches("\\d+")) currentSet.add(arr(i).toLowerCase)
				}
			}
		}
		wordNet.put(currentID, currentSet)

		//improvements to wordNet
		addDetailedDescription(wordNet)
		mannualInjection(wordNet)
		convertCombinedID(wordNet)
		improveDuplicatedID(wordNet)
		improveSmallSet(wordNet)
		return wordNet
	}


/*******************************************************
********* improvement functions for wordNet ************
********************************************************/
	
	/**
     * add detailed description obtained from index definition excel file
     * @param wn: HashMap of wordNet id-wordSet pairs
     */
	def addDetailedDescription(wn: HashMap[String, HashSet[String]]) : Unit = {
		var definitions = Source.fromFile("src/index_detail.txt").getLines
		while (definitions.hasNext) {
			var arr = definitions.next.split("[\\s\"\',();.-]+")
			var set = wn.get(arr(0)).get
			for (i <- 1 until arr.length) {
				// println(arr(i))
				set += arr(i).toLowerCase
			}
		}
	}

	
	/**
     * for combined id in format "xx-xx", convert it to the prior one
     * @param wn: HashMap of wordNet id-wordSet pairs
     */
	def convertCombinedID(wn: HashMap[String, HashSet[String]]) : Unit = {
		var x = ""
		for (x <- wn.keySet) {
			if (x.length == 5 && x.charAt(2) == '-') {	
				wn.put(x.split("-")(0), wn.get(x).get)
				wn.remove(x)
			}
		}
	}

	/**
     * when two different id has the same definition, replace the shorter id's word set
     * @param wn: HashMap of wordNet id-wordSet pairs
     */
	def improveDuplicatedID(wn: HashMap[String, HashSet[String]]) : Unit = {
    	var x = ""
		var y = ""
		for (x <- wn.keySet) {
			for (y <- wn.get(x).get) {
				if (y.matches("\\d+") && x.regionMatches(0, y, 0, x.length)) {
					wn.put(x, wn.get(y).get)
				}
			}
		}
	}



	/**
     * when a set lacks vocabs, improve the set by appending its subsets
     * @param wn: HashMap of wordNet id-wordSet pairs
     */
	def improveSmallSet(wn: HashMap[String, HashSet[String]]) : Unit = {
		var x = ""
		var x2 = ""
		var y : HashSet[String] = HashSet()
		var total = 0
    	for (x <- wn.keySet) {
			y = wn.get(x).get
			if (y.size <= MIN_SET_SIZE || x.toInt < 100) {
				for (x2 <- wn.keySet if x.regionMatches(0, x2, 0, x.length)) {
					if (y.size <= MAX_SET_SIZE || x.toInt < 100) y = y.union(wn.get(x2).get) 
				}
				wn.put(x, y)
			}
		}
	}


	/**
     * mannually inject words to wordset, used for test purpose
     * @param wn: HashMap of wordNet id-wordSet pairs
     */
	def mannualInjection(wn: HashMap[String, HashSet[String]]) : Unit = {
		for (x <- wn.keySet) {
			if (x=="81" || x=="811" || x=="8111" || x=="81111" || x=="811111") {
				var newVocab = HashSet[String]("auto", "tech", "collision", "collisions", "repair", "repairing", "driving")
				var y = (wn.get(x).get).union(newVocab)
				wn.put(x, y)
			}
			if (x=="72" || x=="721110" || x=="721" || x=="7211" || x=="72111") {
				var newVocab = HashSet[String]("hotel", "motel", "inn", "inns")
				var y = (wn.get(x).get).union(newVocab)
				wn.put(x, y)
			}
			if (x=="61" || x=="611" || x=="6116" || x=="61162" || x=="611620") {
				var newVocab = HashSet[String]("sail", "sailing")
				var y = (wn.get(x).get).union(newVocab)
				wn.put(x, y)
			}
			if (x=="72" || x=="722" || x=="7225" || x=="72251" || x=="722511") {
				var newVocab = HashSet[String]("shop", "soda", "steak", "cheesesteak", "cheese", "burger", "beef", "cheezy")
				var y = (wn.get(x).get).union(newVocab)
				wn.put(x, y)
			}
		}
	}

/*******************************************************
**** shortcut for constructing complete wordNet ********
********************************************************/

	def construct : HashMap[String, HashSet[String]] = {
		return processWordNet("src/2012_Definition_File.txt", processCommonVocab("src/commonVocab.txt"))
	}

/*******************************************************
************* test functions ***************************
********************************************************/
	
	/**
     * test for checkBracket function
     */
    def testCheckBracket() : Unit = {
		println("Start checkBracket test")
		val arr1 = Array("a", "(b)", "c", "d")
		val arr2 = Array("a", "(b", "c)", "d", "e")
		val arr3 = Array("a", "b", "c)", "d", "(e")
		val arr4 = Array("a", "(b", "(c)", "d)", "e")
		val arr5 = Array("a", "(b", "(c)", "d)", "(e", "f)", "g")
		arr1.foreach(print)
		println("\n")
		checkBracket(arr1)
		arr1.foreach(print)
		println("\n")
		arr2.foreach(print)
		println("\n")
		checkBracket(arr2)
		arr2.foreach(print)
		println("\n")
		arr3.foreach(print)
		println("\n")
		checkBracket(arr3)
		arr3.foreach(print)
		println("\n")
		arr4.foreach(print)
		println("\n")
		checkBracket(arr4)
		arr4.foreach(print)
		println("\n")
		arr5.foreach(print)
		println("\n")
		checkBracket(arr5)
		arr5.foreach(print)
		println("\n")
    }

    /**
     * test processWordNet function
     */
    def testProcessWordNet() : Unit = {
    	println("Start test processWordNet")
    	val cv = processCommonVocab("src/commonVocab.txt")
		println("---start reading try.txt")
		val try1 = processWordNet("src/try.txt", cv)
		try1.foreach(println)
		println("IN TOTAL CLASSIFICATION NUMBER: " + try1.size)
		val try2 = processWordNet("src/try2.txt", cv)
		println("---start reading try2.txt")
		try2.foreach(println)
		println("IN TOTAL CLASSIFICATION NUMBER: " + try2.size)
		val try3 = processWordNet("src/try3.txt", cv)
		try3.foreach(println)
		println("IN TOTAL CLASSIFICATION NUMBER: " + try3.size)
		val wn = processWordNet("src/2012_Definition_File.txt", cv)
		println("---start reading 2012_Definition_File.txt")
		wn.keySet.foreach(println)
		println("IN TOTAL CLASSIFICATION NUMBER: " + wn.size)
    }

    /**
     * test if some sets in wordNet contain numbers and collect info
     */
    def testNumberInSet() : Unit = {
    	println("Start test number in set")
    	val cv = processCommonVocab("src/commonVocab.txt")
		val wn = processWordNet("src/2012_Definition_File.txt", cv)
    	var x = ""
		var y = ""
		var count = 0
		var stotal = 0
		var total = 0
		for (x <- wn.keySet) {
			for (y <- wn.get(x).get) {
				if (y.matches("\\d+")) {
					print(y + " ")
					count = count + 1
				}
			}
			if (count > 1) {
				print(" *****")
				stotal = stotal + 1
			}
			if (count > 0) {
				total = total + 1
				println(" --- ID - " + x)
			}
			count = 0
		}
		println("IDs with two or more numbers in set: " + stotal)
		println("IDs with number in set: " + total)
    }

    /**
     * test if some sets in wordNet contain too small amount of words
     * @param n: word amount requirement for each set
     */
    def testSmallSet(n: Int): Unit = {
    	println("Start test small set")
    	val cv = processCommonVocab("src/commonVocab.txt")
		val wn = processWordNet("src/2012_Definition_File.txt", cv)
    	var x = ""
		var y = 0
		var total = 0
    	for (x <- wn.keySet) {
			print("ID - " + x + " size: ")
			y = wn.get(x).get.size
			print(y)
			if (y <= n) {
				total = total + 1
				print(" *****")
			}
			print("\n")
		}
		println("Too small set: " + total)
    }

    /**
     * test the complete wordNet content in an infinite loop
     * enter "exit" to exit the loop
     */
    def checkWordNetLoop() : Unit = {
    	val cv = processCommonVocab("src/commonVocab.txt")
		val wn = processWordNet("src/2012_Definition_File.txt", cv)
		var s = ""
		while (s != "exit") {
			s = readLine
			if (wn.contains(s)) {
				wn.get(s).get.foreach(x => print(x + " "))
				println()
			} else if (!s.equals("exit")){
				println("no such ID")
			}
		}
    }

	/**
     * main method of WordNetProcesor
     * used for testing purpose
     * uncomment to run the test
     * @param args: stdin from user input
     */
	def main(args: Array[String]): Unit = {
		// testCheckBracket
		// testProcessWordNet
		// testNumberInSet
		// testSmallSet(MIN_SET_SIZE)
		checkWordNetLoop
		// construct.foreach(println)
	}
}




