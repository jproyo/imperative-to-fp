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


  def algorithm1(userId: UserId): Either[AppError, UserRec] =
    recommendations
      .find(u => u.userId == userId)
      .toRight(RecommendationsNotFound(userId, "algo1"))

  def algorithm2(userId: UserId): Either[AppError, UserRec] =
    recommendations
      .find(u => u.userId == userId)
      .map(_.copy(recs = recs.filter(r => r.recId > "h").toList))
      .toRight(RecommendationsNotFound(userId, "algo2"))

  def algorithm3(userId: UserId): Either[AppError, UserRec] =
    None.toRight(RecommendationsNotFound(userId, "algo3"))

  type Algo = UserId => Either[AppError, UserRec]

  lazy val algorithms = Map (
    "algo1" -> algorithm1 _,
    "algo2" -> algorithm2 _
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


  implicit class FilterRecs(recs: UserRec){
    def filter(limit: Int): UserRec =
      recs.copy(recs = recs.recs.slice(0, limit))
  }

  def program(userId: Option[Int],
              recommenderId: Option[String] = None,
              limit: Option[Int] = None): Unit = {

    val result = getRecommendations(userId, recommenderId, limit)

    printResult(userId, result)

  }

  private def printResult(userId: Option[Int], result: Either[AppError, (String, UserRec)]): Unit = {
    result.fold(error => println(error.message), recs => {
      println(s"\nRecommnedations for userId ${recs._2.userId}....")
      println(s"Algorithm ${recs._1}")
      println(s"Recs: ${recs._2.recs}")
    })
  }

  private def getRecommendations(userId: Option[Int],
                                 recommenderId: Option[String],
                                 limit: Option[Int]): Either[AppError, (String, UserRec)] = {
    for {
      userData            <- getUser(userId)
      (recId, algorithm)  <- getAlgorithm(recommenderId)
      recs                <- algorithm(userData)
      limit               <- limit.orElse(Some(limitDefault)).toRight(UnknownError)
      filteredRecs         = recs.filter(limit)
    } yield (recId, filteredRecs)
  }

  private def getAlgorithm(recommenderId: Option[String]): Either[AppError, (String, Algo)] = {
    (for {
      recorDef  <- recommenderId.orElse(algoDefault)
      recId     <- algorithms.keys.find(_ == recorDef).orElse(algoDefault)
      algorithm <- algorithms.get(recId)
    } yield (recId, algorithm)).toRight(UnknownError)
  }

  private def getUser(userId: Option[Int]): Either[AppError, UserId] = {
    for {
      userParam <- userId.toRight(UserNotProvided)
      userData  <- users.find(_.userId == userParam).toRight(UserNotFound(UserId(userParam)))
    } yield userData
  }

}

object ToScalaFP extends App {
  import AppImperative._

  program(Some(1), Some("algo1"), None)
  println("------------------------------\n")

  program(Some(2), Some("algo2"), Some(5))
  println("------------------------------\n")

  program(Some(3), Some("algo5"), Some(15))
  println("------------------------------\n")

  program(Some(14), Some("algo2"), Some(15))
  println("------------------------------\n")

  program(None, Some("algo3"), Some(15))
  println("------------------------------\n")

  program(Some(1), Some("algo3"), Some(15))
  println("------------------------------\n")
}


