// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types._

// This
// (https://stackoverflow.com/questions/40015416/spark-unable-to-load-native-hadoop-library-for-your-platform)
// actually does seems to work, to eliminate the missing hadoop message.
// 'WARN NativeCodeLoader: Unable to load native-hadoop library for your platform... using builtin-java classes where applicable'
// AW not sure if the 'hadoop missing warning' matters though.

object Main {

	type Embedding = (String, List[Double])
	type ParsedReview = (Integer, String, Double)

	org.apache.log4j.Logger getLogger "org"  setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark =  SparkSession.builder
		.appName ("Sentiment")
		.master  ("local[5]")
		.getOrCreate

	spark.conf.set("spark.executor.memory", "4g")

  import spark.implicits._

	val reviewSchema = StructType(Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)))

	// Read file and merge the text and summary into a single text column

	def loadReviews (path: String): Dataset[ParsedReview] =
		spark
			.read
			.schema (reviewSchema)
			.json (path)
			.rdd
			.zipWithUniqueId
			.map[(Integer,String,Double)] { case (row,id) =>
          (id.toInt, s"${row getString 2} ${row getString 0}", row getDouble 1) }
			.toDS
			.withColumnRenamed ("_1", "id" )
			.withColumnRenamed ("_2", "text")
			.withColumnRenamed ("_3", "overall")
			.as[ParsedReview]

  // Load the GLoVe embeddings file

  def loadGlove (path: String): Dataset[Embedding] =
		spark
			.read
			.text (path)
      .map  { _ getString 0 split " " }
      .map  (r => (r.head, r.tail.toList.map (_.toDouble))) // yuck!
			.withColumnRenamed ("_1", "word" )
			.withColumnRenamed ("_2", "vec")
			.as[Embedding]

  def main(args: Array[String]) = {

    val DATA_PATH = "/data/"

    val glove  = loadGlove (s"${DATA_PATH}/glove.6B.50d.txt")
    val reviews = loadReviews (s"${DATA_PATH}/reviews_small_1.json")

    // replace the following with the project code
    glove.show
    reviews.show

    // Train the sentiment perceptron here (this is *one* possible workflow, and it is slow ...)
    //
    //   - First clean the data
    //      - Use the tokenizer to turn records with reviews into records with
    //      lists of words
		//         documentation: https://spark.apache.org/docs/latest/ml-features.html#tokenizer
    //         output type: a collection of (Integer, Seq[String], Double)
    //  - Second translate the reviews to embeddings
    //      - Flatten the list to contain single words
    //         output type: a collection (Integer, String, Double) but much
    //         longer than input
    //      - Join the glove vectors with the triples
    //         output type: a collection (Integer, String, Double,
    //         Array[Double])
    //      - Drop the word column, we don't need it anymore
    //         output type: a collection (Integer, Double, Array[Double])
    //      - Add a column of 1s
    //         output type: a collection (Integer, Double, Array[Double], Integer)
    //      - Reduce By Key (using the first or two first columns as Key), summing the last column
    //         output type: a collection (Integer, Double, Array[Double], Integer)
    //         (just much shorter this time)
    //      - In each row divide the Array (vector) by the count (the last column)
    //         output type: a collection (Integer, Double, Array[Double])
    //         This is the input for the classifier training
    //  - Train the perceptron:
    //      - translated the ratings from 1..5 to 1..3 (use map)
    //      - make sure tha columns are named "id", "label", "features"
    //      - follow the MultilayerPerceptronClassifier tutorial.
    //      - Remember that the first layer needs to be #50 (for vectors of size
    //      50), and the last needs to be #3.
    //  - Validate the perceptron
    //      - Either implement your own validation loop  or use
	  //        org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
    //
    // Any suggestions of improvement to the above guide are welcomed by
    // teachers.
    //
    // This is an open programming exercise, you do not need to follow the above
    // guide to complete it.

		spark.stop
  }

}
