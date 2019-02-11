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

  case class Result(algorithm: Algorithm, recs: UserRec)

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

object algebras {

  import DataSource._


  trait UserRepo[F[_]] {
    def getUser(userId: Option[Int]): F[UserId]
  }


  object UserRepo {
    def apply[F[_]](implicit UserR: UserRepo[F]): UserRepo[F] = UserR
  }

  def getUser[F[_]: UserRepo](userId: Option[Int]): F[UserId] = UserRepo[F].getUser(userId)


  trait Filter[F[_]] {
    def filter(userRec: UserRec, limit: Int): F[UserRec]
  }

  object Filter {
    def apply[F[_]](implicit Fil: Filter[F]): Filter[F] = Fil
  }

  trait AlgorithmRepo[F[_]] {
    def getAlgorithm(recommenderId: Option[String]): F[Algorithm]
    def execute(algo: Algorithm, userId: UserId): F[UserRec]
  }

  object AlgorithmRepo {
    def apply[F[_]](implicit Algo: AlgorithmRepo[F]): AlgorithmRepo[F] = Algo
  }


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



  def program(userId: Option[Int],
              recommenderId: Option[String] = None,
              limit: Option[Int] = None): Unit = {

    val result: Option[Result] = getRecommendations(userId, recommenderId, limit)

    printResults(userId, result)

  }

  def getRecommendations(userId: Option[Int], recommenderId: Option[String], limit: Option[Int]): Option[Result] = {
    val result = for {
      user           <- getUser(userId)
      algorithm      <- getAlgorithm(recommenderId)
      result         <- executeAlgorithm(user, algorithm)
      limitFilter     = limit.getOrElse(limitDefault)
      resultFiltered <- filterResults(result, limitFilter)
    } yield Result(algorithm, resultFiltered)
    result
  }


  def printResults(userId: Option[Int], result: Option[Result]): Unit = {
    result.fold(println(s"No recommendations found for userId $userId"))(algoRes => {
      println(s"\nRecommnedations for userId ${algoRes.recs.userId}...")
      println(s"Algorithm ${algoRes.algorithm.name}")
      println(s"Recs: ${algoRes.recs.recs}")
    })
  }

  private def getUser(userId: Option[Int]): Option[UserId] =
  userId.filter(user => users.exists(_.userId == user)).map(UserId)

  private def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
  recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))

  private def executeAlgorithm(user: UserId, algorithm: Algorithm): Option[UserRec] =
    algorithm.run(user)

  private def filterResults(result: UserRec, limitFilter: Int): Option[UserRec] =
    Some(result.copy(recs = recs.slice(0, limitFilter).toList))


}


object ToScalaFP extends App {
  import AppImperative._

  program(Some(1), Some("algo2"), Some(5))
}


