package company

import company.Company
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet
import scala.io.Source

object CompanyTest {

	// def testAnalyzeATag() {
	// 	var a = new Company("11111","2","3",new HashSet[String](), "5")
	// 	println(a.analyzeATag("<a href='about.php'>About Us</a>", "http://www.iwdesigners.com"))
	// }

	// def testAnalyzePTag() {
	// 	println("start test analyzePTag -- Case 1")
	// 	var a = new Company("11111","2","3",new HashSet[String](), "5")
	// 	var set = new Queue[String]()
	// 	var arr = Array("</div>", "<p>QVC Clothing and Accessory Personality</p>")
	// 	a.analyzePTag(set, "<p>Online Store for Award Winning Children&#039;s Book Series</p>", arr.iterator)
	// 	set.foreach(println)

	// 	println("start test analyzePTag -- Case 2")
	// 	arr = Array("QVC Clothing and Accessory Personality</p>", "</div>")
	// 	set = new Queue[String]()
	// 	a.analyzePTag(set, "<p>Online Store for Award Winning Children&#039;s Book Series", arr.iterator)
	// 	set.foreach(println)
	// }

	// def testGenerateByURL() {
	// 	var a = new Company("11111","2","3",new HashSet[String](), "5")
	// 	var set = new Queue[String]()
	// 	a.generateByURL(set, "http://www.iwdesigners.com", "http://www.iwdesigners.com", 2)
	// 	set.foreach(println)
	// }

	// def testFilter() {
	// 	var a = new Company("11111","2","3",new HashSet[String](), "5")
	// 	println(a.filter.toString)
	// }

	/**
     * main method of WordNetProcesor
     * used for testing purpose
     * uncomment to run the test
     * @param args: stdin from user input
     */
	def main(args: Array[String]): Unit = {
		// testFilter
	}
}


