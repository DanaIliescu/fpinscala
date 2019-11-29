// Advanced Programming. Andrzej Wasowski. IT University
//
// Group number: 23
// AUTHOR1: Dana-Maria Iliescu, dail@itu.dk
// AUTHOR2: Sara Qirko, saqi@itu.dk
// AUTHOR3: Petr Sindelar, pesi@itu.dk


// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)
import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.ml.feature.{LabeledPoint, Tokenizer}
import org.apache.spark.ml.{Model, linalg}
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.ml.tuning.{CrossValidator, CrossValidatorModel, ParamGridBuilder}
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.mllib.util.MLUtils
import org.apache.spark.sql.catalyst.encoders.RowEncoder
import org.apache.spark.sql.{DataFrame, Dataset, Row, SparkSession}
import org.apache.spark.sql.types._
import org.apache.spark.sql.functions.rand

import scala.collection.mutable
import scala.concurrent.Future

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

	val reviewSchema = StructType(
    Array(
			StructField ("reviewText", StringType, nullable=false),
			StructField ("overall",    DoubleType, nullable=false),
			StructField ("summary",    StringType, nullable=false)
    )
  )

	//  Read file and merge the text and summary into a single text column

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

  //  Load the GLoVe embeddings file

  def loadGlove (path: String): Dataset[Embedding] =
		spark
			.read
			.text (path)
      .map  { _ getString 0 split " " }
      .map  (r => (r.head, r.tail.toList.map (_.toDouble))) // yuck!
			.withColumnRenamed ("_1", "word" )
			.withColumnRenamed ("_2", "vec")
			.as[Embedding]

	def tokenize(reviews: Dataset[ParsedReview]): Dataset[Row] = {
		val tokenizer = new Tokenizer().setInputCol("text").setOutputCol("wordSequence")

		val tokenizeSchema = StructType(
			Array(
				StructField ("id", IntegerType, nullable = false),
				StructField ("wordSequence", DataTypes.createArrayType(StringType), nullable = false),
				StructField ("rating", DoubleType, nullable = false)
			)
		)

		val tokenizeEncoder = RowEncoder(tokenizeSchema)

		val tokens = tokenizer.transform(reviews)
  		.select($"id", $"wordSequence", $"overall")
			.map{ //  Translate the ratings from 1..5 to 0..2 (negative/neutral/positive) 
				case Row(id: Int, wordSequence: Seq[String], rating: Double) =>
					Row(id, wordSequence,     
						if (rating < 3) 0.0
						else if (rating == 3) 1.0
						else 2.0
					)
				}(tokenizeEncoder)

		tokens
	}

  def flattenTokens(tokenizedReviews: Dataset[Row]): Dataset[ParsedReview] = {
    tokenizedReviews.flatMap{ 
      case Row(id: Int, wordSequence: Seq[String], rating: Double) =>
        wordSequence.map(x => (id, x.toLowerCase(), rating))
    }.as[ParsedReview]
  }

  val averageVectorSchema = StructType(
    Array(
      StructField("id", IntegerType, nullable = false),
      StructField("attributes", DataTypes.createArrayType(DoubleType), nullable = false)
    )
  )

  val averageVectorEncoder = RowEncoder(averageVectorSchema)

  def averageVectors(aggregated: DataFrame): Dataset[Row] = {
    //  Get number of words for every unique key
    val wordCount = aggregated.groupByKey(r => r.getAs[Int](0)).count

    val vectorSum = aggregated
      .drop("_2", "word", "_3")   //  Drop the columns that we don't need anymore
                                  //    output type: a collection (Integer, Array[Double])
      .groupByKey(r => r.getAs[Int]("_1"))
      .reduceGroups((a1, a2) => Row(a1.getInt(0),     
        (a1.getSeq(1), a2.getSeq(1))  
        .zipped.map((a1: Double, a2: Double) => a1+a2)))  //  Reduce By Key (using the first column as key), summing the last column
                                                          //    output type: a collection (Integer, Array[Double])
        .map{
            case (_, r: Row) => 
              Row(r.getInt(0), r.getAs[mutable.WrappedArray[Double]](1).flatMap((x: Double) => Seq(x)))
          }(averageVectorEncoder)

    val vectorCount = vectorSum.join(wordCount, vectorSum("id") === wordCount("value")).drop("value")

    //  In each row divide the Array (vector) by the count (the last column)
    //    output type: a collection (Integer, Array[Double])
    //    This is the input for the classifier training
    val averagedVectors = vectorCount.map{
      case Row(id: Int, attr: Seq[Double], num: Long) =>
        Row(id, attr.map(
          (x: Double) => x / num
        ))
    }(averageVectorEncoder)

    averagedVectors
  }

  def constructDataFrame(averagedVectors: Dataset[Row], tokenizedReviews: Dataset[Row]): DataFrame = {
    val dataFrame = averagedVectors.join(tokenizedReviews, Seq("id"))
      .drop("wordSequence", "id") // Drop the unnecessary columns
      .as[(Array[Double], Double)]
      .map{ //  MultilayerPerceptronClassifier expects Vector features
        case (attributes: Array[Double], rating: Double) =>
          (Vectors.dense(attributes), rating)
      }
      .withColumnRenamed ("_1", "features") //  Make sure that columns are named "features", "label"
      .withColumnRenamed ("_2", "label")

    dataFrame
  }

  def trainModel(df: DataFrame): Unit = {
    //  Remember that the first layer needs to be #50 (for vectors of size 50), and the last needs to be #3.
    val layers = Array[Int](200, 140, 70, 3)

    val trainer = new MultilayerPerceptronClassifier()
      .setLayers(layers)
      .setMaxIter(100)
      .setBlockSize(256)

    val sparkSession = df.sparkSession
    val schema = df.schema
    val splits = MLUtils.kFold(df.toDF.rdd, 10, 2L)
    
    splits.zipWithIndex.map {
      case ((training, validation), splitIndex) => {
        val trainingDataset = sparkSession.createDataFrame(training, schema).cache()
        val validationDataset = sparkSession.createDataFrame(validation, schema).cache()
        val model = trainer.fit(trainingDataset) // Train the model
        val result = model.transform(validationDataset) // Validate
        val predictionAndLabels = result.select("prediction", "label")
        val evaluator = new MulticlassClassificationEvaluator().setMetricName("accuracy")
        println(s"Accuracy for set $splitIndex:")
        println(s"Test set accuracy = ${evaluator.evaluate(predictionAndLabels)}")
      }
    }
  }

  def main(args: Array[String]) = {
    val DATA_PATH = "/data/"

    val glove  = loadGlove (s"${DATA_PATH}/glove.6B/glove.6B.200d.txt")
    val reviews = loadReviews (s"${DATA_PATH}/reviews_Amazon_Instant_Video_5.json")

    glove.show
    reviews.show

    // Train the sentiment perceptron here (this is *one* possible workflow, and it is slow ...)
    //
    //   - First clean the data
    //      - Use the tokenizer to turn records with reviews into records with lists of words
		//         documentation: https://spark.apache.org/docs/latest/ml-features.html#tokenizer
    //         output type: a collection of (Integer, Seq[String], Double)
    val tokenized = tokenize(reviews)
    //  - Second translate the reviews to embeddings
    //      - Flatten the list to contain single words
    //         output type: a collection (Integer, String, Double) but much longer than input
    val flattened = flattenTokens(tokenized)
    //      - Join the glove vectors with the triples
    //         output type: a collection (Integer, String, Double, String, Array[Double])
		val aggregated = flattened.join(glove, flattened("_2") === glove("word"))
    //      - Calculate the average vector
    //         output type: a collection (Integer, Array[Double])
    val averagedVectors = averageVectors(aggregated)
    //  - Train the perceptron:
    //      - follow the MultilayerPerceptronClassifier tutorial.
    val dataFrame = constructDataFrame(averagedVectors, tokenized)
    trainModel(dataFrame)
    // Any suggestions of improvement to the above guide are welcomed by
    // teachers.
    //
    // This is an open programming exercise, you do not need to follow the above
    // guide to complete it.

     spark.stop
  }

}


// ANSWERS

// 1. What data sets have you managed to run (including size)?
// We have used glove.6B.50d.txt, glove.6B.100d.txt, glove.6B.200d.txt, glove.6B.300d.txt 
// and reviews_Amazon_Instant_Video_5.json (37,126 reviews). However, in the middle of the execution
// while using glove.6B.300d.txt an error occurred.


// 2. What accuracy have you obtained? 
// For glove.6B.50d.txt and reviews_Amazon_Instant_Video_5.json the average accuracy is 0.79675652307048761.
// For glove.6B.100d.txt and reviews_Amazon_Instant_Video_5.json the average accuracy is 0.80606171541106065.
// For glove.6B.200d.txt and reviews_Amazon_Instant_Video_5.json the average accuracy is 0.81257247332193166.

// With how many iterations? 
// 100 iterations.

// And with what layer configuration? 
// For glove.6B.50d.txt and reviews_Amazon_Instant_Video_5.json: val layers = Array[Int](50, 32, 18, 3).
// For glove.6B.50d.txt and reviews_Amazon_Instant_Video_5.json: val layers = Array[Int](100, 60, 30, 3).
// For glove.6B.50d.txt and reviews_Amazon_Instant_Video_5.json: val layers = Array[Int](200, 140, 70, 3).
// For glove.6B.50d.txt and reviews_Amazon_Instant_Video_5.json: val layers = Array[Int](300, 200, 100, 3).

// Show the variance of accuracy observed in cross validation.
// For glove.6B.50d.txt and reviews_Amazon_Instant_Video_5.json:
// Accuracy for set 0:
// Test set accuracy = 0.7924321360021935
// Accuracy for set 1:
// Test set accuracy = 0.7915300546448087
// Accuracy for set 2:
// Test set accuracy = 0.8136830719307734
// Accuracy for set 3:
// Test set accuracy = 0.7982362373062534
// Accuracy for set 4:
// Test set accuracy = 0.7992
// Accuracy for set 5:
// Test set accuracy = 0.7835978835978836
// Accuracy for set 6:
// Test set accuracy = 0.8014203769461896
// Accuracy for set 7:
// Test set accuracy = 0.8018766756032172
// Accuracy for set 8:
// Test set accuracy = 0.7899182561307901
// Accuracy for set 9:
// Test set accuracy = 0.7956705385427666

// For glove.6B.100d.txt and reviews_Amazon_Instant_Video_5.json:
// Accuracy for set 0:
// Test set accuracy = 0.8064162325198794
// Accuracy for set 1:
// Test set accuracy = 0.7991803278688525
// Accuracy for set 2:
// Test set accuracy = 0.8174689021092483
// Accuracy for set 3:
// Test set accuracy = 0.8025120256547301
// Accuracy for set 4:
// Test set accuracy = 0.8077333333333333
// Accuracy for set 5:
// Test set accuracy = 0.7968253968253968
// Accuracy for set 6:
// Test set accuracy = 0.8128926522807975
// Accuracy for set 7:
// Test set accuracy = 0.810455764075067
// Accuracy for set 8:
// Test set accuracy = 0.8035422343324251
// Accuracy for set 9:
// Test set accuracy = 0.8035902851108765

// For glove.6B.200d.txt and reviews_Amazon_Instant_Video_5.json:
// Accuracy for set 0:
// Test set accuracy = 0.8116259939676447
// Accuracy for set 1:
// Test set accuracy = 0.809016393442623
// Accuracy for set 2:
// Test set accuracy = 0.8269334775554353
// Accuracy for set 3:
// Test set accuracy = 0.8115980758952431
// Accuracy for set 4:
// Test set accuracy = 0.8125333333333333
// Accuracy for set 5:
// Test set accuracy = 0.7978835978835979
// Accuracy for set 6:
// Test set accuracy = 0.8137121005189839
// Accuracy for set 7:
// Test set accuracy = 0.8144772117962467
// Accuracy for set 8:
// Test set accuracy = 0.8106267029972752
// Accuracy for set 9:
// Test set accuracy = 0.8173178458289335

// For glove.6B.300d.txt and reviews_Amazon_Instant_Video_5.json:
// Accuracy for set 0:
// Test set accuracy = 0.818206745270085
// Accuracy for set 1:
// Test set accuracy = 0.8098360655737705
// Accuracy for set 2:
// Test set accuracy = 0.8274743104380746
// Accuracy for set 3:
// Test set accuracy = 0.8129342597541421
// Accuracy for set 4:
// Test set accuracy = 0.8125333333333333
// After set 4 at some point we got this error
// 19/10/29 03:33:15 WARN Executor: Issue communicating with driver in heartbeater
// org.apache.spark.rpc.RpcTimeoutException: Futures timed out after [10000 milliseconds]. This timeout is controlled by spark.executor.heartbeatInterval
// 19/10/29 03:33:16 ERROR TaskSchedulerImpl: Lost executor driver on localhost: Executor heartbeat timed out after 908428 ms


// 3. What degree of parallelization was obtained? (for instance contrast the wall clock time with the CPU time). 
// Note we are not asking you to optimize parallelization, just to report what you obtained.
// We have only measured the wall clock time with the following results:
// For glove.6B.50d.txt and reviews_Amazon_Instant_Video_5.json: [success] Total time: 4893 s, completed Oct 28, 2019 7:36:12 PM (aproximately 1 hour)
// For glove.6B.100d.txt and reviews_Amazon_Instant_Video_5.json: [success] Total time: 7382 s, completed Oct 28, 2019 9:44:13 PM (aproximately 2 hours)
// For glove.6B.200d.txt and reviews_Amazon_Instant_Video_5.json: [success] Total time: 8395 s, completed Oct 28, 2019 10:00:25 PM (aproximately 2.2 hours)
// For glove.6B.300d.txt and reviews_Amazon_Instant_Video_5.json: failed to finish executing after approximately 5 hours.


// 4. What extensions (if any) you have implemented? Have you tried another classifier? 
// Another network configuration? Running on a highly parallel cluster of machines, etc. ...
// We haven't implemented any extensions.