package program

import scala.util.{ Random, Success, Try }

object DataSource {
  case class UserId(userId: Int) extends AnyVal
  case class Rec(recId: String, score: Float)
  case class UserRec(userId: UserId, recs: List[Rec])

  lazy val users = (1 until 10).map(UserId).toList

  lazy val recs = ('a' to 'z').map(c => Rec(c.toString, Random.nextFloat))

  lazy val recommendations = users.map {
    user => {
      if(user.userId % 2 == 0) UserRec(user, List.empty)
      else UserRec(user, recs.toList)
    }
  }


  def algorithm1(userId: Int): UserRec =
    recommendations.find(u => u.userId.userId == userId).get

  def algorithm2(userId: Int): UserRec =
    recommendations
      .find(u => u.userId.userId == userId)
      .get
      .copy(recs = recs.filter(r => r.recId > "h").toList)

  lazy val algorithms = Map (
    "algo1" -> algorithm1 _,
    "algo2" -> algorithm2 _
  )

  val algoDefault = "algo1"

}


/**
  * This is an exercise to explore advantages of moving from imperative design to FP design
  *
  * We are going to define program example which is going to take several arguments pased through command line
  * and evaluate each command line in order to execute diferent branch of the program.
  *
  * Story 1: As an user i want to get recommendations from an specific algorith, but if there are no recommendations for this algorith
  * or i forgot to specify what algorithm should be use i would like to have default recommendations from the best algorithm the system has.
  *
  * Story 2: As an user i want to get a message if recommendation's algorithm i requested is wrong.
  *
  * Story 3: As an user i want to be able to be retrieve with a limited number of recommendations
  *
  */
object AppImperative {

  import DataSource._

  def getCommandLineArgs(args: Array[String]): (String, String, String) = {

    if (args.length < 1) {
      println("At least userId must be provided")
      sys.exit(1)
    }

    val userIdArg = args(0)
    var recommenderId = algoDefault
    if (args.length > 1) {
      recommenderId = args(1)
    }
    var limitArg = ""
    if (args.length > 2) {
      limitArg = args(2)
    }
    (userIdArg, recommenderId, limitArg)
  }


  def convertArgs(userIdArg: String, limitArg: String): (Int, Int) = {
    var userId: Int = -1
    var limit: Int = 10

    if (userIdArg != null && userIdArg.nonEmpty) {
      Try(userIdArg.toInt) match {
        case Success(number) => userId = number
        case _ => {
          println("UserId must be an Int value")
          sys.exit(1)
        }
      }
    } else {
      println("UserId must be provided")
      sys.exit(1)
    }

    if (limitArg != null && limitArg.nonEmpty) {
      Try(limitArg.toInt) match {
        case Success(number) => limit = number
        case _ => {
          println("Limit must be an Int value")
          sys.exit(1)
        }
      }
    }
    (userId, limit)
  }

  private def getRecommendations(request: CreateRequest): Unit = {
    val userId = request.userId
    val recommenderId = request.recommenderId
    val limit = request.limit
    if (users.exists(_.userId == userId)) {
      var algoId: String = algoDefault
      if (recommenderId != null && recommenderId.nonEmpty) {
        if (algorithms.keys.exists(_ == recommenderId)) {
          algoId = recommenderId
        }
      }
      var result = algorithms.get(algoId).get(userId)
      if (result.recs.isEmpty) {
        result = algorithms.get(algoDefault).get(userId)
      }
      if (result.recs.isEmpty) {
        println(s"No recommendations found for userId $userId")
      } else {
        val filteredResult = result.copy(recs = recs.slice(0, limit).toList)
        println(s"\nRecommnedations for userId $userId...")
        println(s"Algorithm $algoId")
        println(s"Recs: ${filteredResult.recs}")
      }
      sys.exit(0)
    } else {
      println(s"No user found with userId $userId")
      sys.exit(1)
    }
  }


  def program(args: Array[String]): Unit = {

    val request = createRequest(args)

    getRecommendations(request)

  }

  case class CreateRequest(recommenderId: String, userId: Int, limit: Int)

  private def createRequest(args: Array[String]): CreateRequest = {
    val getCommandLineArgsResult: (String, String, String) = getCommandLineArgs(args)
    val userIdArg: String = getCommandLineArgsResult._1
    var recommenderId: String = getCommandLineArgsResult._2
    var limitArg: String = getCommandLineArgsResult._3

    var (userId: Int, limit: Int) = convertArgs(userIdArg, limitArg)
    CreateRequest(recommenderId, userId, limit)
  }
}


object ToScalaFP extends App {
  import AppImperative._

  program(args)
}


