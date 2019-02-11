package program

import scala.util.Random

object DataSource {

  sealed trait AppError extends Throwable {
    def message: String
  }
  case object UnknownError extends AppError {
    override def message: String = s"Unexpected Error"
  }
  case class UserNotFound(userId: UserId) extends AppError {
    override def message: String = s"User not found for id $userId"
  }
  case object UserNotProvided extends AppError {
    override def message: String = s"User id must be provided"
  }
  case class RecommendationsNotFound(userId: UserId, algo: String) extends AppError {
    override def message: String = s"Recommendations not found for $userId with algorithm '$algo'"
  }

  case class UserId(userId: Int) extends AnyVal
  case class Rec(recId: String, score: Float)
  case class UserRec(userId: UserId, recs: List[Rec])

  lazy val users = (1 until 10).map(UserId).toList

  lazy val recs = ('a' to 'z').map(c => Rec(c.toString, Random.nextFloat))

  lazy val recommendations = users.map {
    user => {
      if(user.userId % 2 == 0) {
        UserRec(user, List.empty)
      } else {
        UserRec(user, recs.toList)
      }
    }
  }


  def emptyRecs(user: Int): UserRec = {
    UserRec(UserId(user), List.empty)
  }


  case class Algorithm(name: String, run: UserId => Option[UserRec])

  val algo1 = Algorithm("algo1", userId => recommendations.find(u => u.userId == userId))

  val algo2 = Algorithm("algo2", userId => recommendations
    .find(u => u.userId == userId)
    .map(_.copy(recs = recs.filter(r => r.recId > "h").toList)))

  val algo3 = Algorithm("algo3" ,_ => None)

  lazy val algorithms = Map (
    "algo1" -> algo1,
    "algo2" -> algo2,
    "algo3" -> algo3
  )

  val algoDefault = Some("algo1")

  val limitDefault = 10

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


  def getUser(userId: Option[Int]): Option[Int] =
    userId.filter(user => users.exists(_.userId == user))

  def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
    recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))


  def program(userId: Option[Int],
              recommenderId: Option[String] = None,
              limit: Option[Int] = None): Unit = {


    val user = getUser(user)

    val algorithm = getAlgorithm(recommenderId)

    val result = algorithm.flatMap(_.run(user)).orElse(Some(emptyRecs(user.get)))

    val limitFilter = limit.orElse(Some(limitDefault))

    userId match {
      case Some(user) => {
        if (users.exists(_.userId == user)) {
          var algoId: String = algoDefault.get
          recommenderId match {
            case Some(recId) => {
              if (recId != null && recId.nonEmpty) {
                if (algorithms.keys.exists(_ == recId)) {
                  algoId = recId
                }
              }
            }
            case None => ()
          }
          var result = algorithms.get(algoId).flatMap(_.run(UserId(user))).getOrElse(emptyRecs(user))
          if (result.recs.isEmpty) {
            result = algorithms.get(algoDefault.get).flatMap(_.run(UserId(user))).getOrElse(emptyRecs(user))
          }
          if (result.recs.isEmpty) {
            println(s"No recommendations found for userId $userId")
          } else {
            val amount = limit match {
              case Some(l) => l
              case None => limitDefault
            }
            val filteredResult = result.copy(recs = recs.slice(0, amount).toList)
            println(s"\nRecommnedations for userId $user...")
            println(s"Algorithm $algoId")
            println(s"Recs: ${filteredResult.recs}")
          }
          sys.exit(0)
        } else {
          println(s"No user found with userId $userId")
          sys.exit(1)
        }

      }
      case None => {
        println("UserId must be provided")
        sys.exit(1)
      }
    }

  }

}


object ToScalaFP extends App {
  import AppImperative._

  program(Some(1), Some("algo2"), Some(5))
}


