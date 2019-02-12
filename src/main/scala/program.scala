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
  case class AlgorithmNotFound(recId: String) extends AppError {
    override def message: String = s"Algorithm not found for id $recId"
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

  trait Program[F[_]] {

    def flatMap[A, B](fa: F[A], afb: A => F[B]): F[B]

    def map[A, B](fa: F[A], ab: A => B): F[B]

    def fold[A, B, C](fa: F[A], first: B => C, second: A => C): C

  }
  object Program {
    def apply[F[_]](implicit Prog: Program[F]): Program[F] = Prog
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit Prog: Program[F]): F[B] = Prog.map(fa, f)
    def flatMap[B](afb: A => F[B])(implicit Prog: Program[F]): F[B] = Prog.flatMap(fa, afb)
    def fold[B, C](first: B => C, second: A => C)(implicit Prog: Program[F]): C = Prog.fold(fa, first, second)
  }



  trait UserRepo[F[_]] {
    def getUser(userId: Option[Int]): F[UserId]
  }

  object UserRepo {
    def apply[F[_]](implicit F: UserRepo[F]): UserRepo[F] = F
  }

  def getUser[F[_]: UserRepo](userId: Option[Int]): F[UserId] = UserRepo[F].getUser(userId)


  trait Filter[F[_]] {
    def filter(userRec: UserRec, limit: Int): F[UserRec]
  }

  object Filter {
    def apply[F[_]](implicit Fil: Filter[F]): Filter[F] = Fil
  }

  def filter[F[_]: Filter](userRec: UserRec, limit: Int): F[UserRec] = Filter[F].filter(userRec, limit)

  trait AlgorithmRepo[F[_]] {
    def getAlgorithm(recommenderId: Option[String]): F[Algorithm]
    def execute(algo: Algorithm, userId: UserId): F[UserRec]
  }

  object AlgorithmRepo {
    def apply[F[_]](implicit Algo: AlgorithmRepo[F]): AlgorithmRepo[F] = Algo
  }

  def getAlgorithm[F[_]: AlgorithmRepo](recommenderId: Option[String]): F[Algorithm] = AlgorithmRepo[F].getAlgorithm(recommenderId)
  def execute[F[_]: AlgorithmRepo](algo: Algorithm, userId: UserId): F[UserRec] = AlgorithmRepo[F].execute(algo, userId)


}

object interpreter {

  import DataSource._
  import algebras._

  implicit object UserRepoOption extends UserRepo[Option] {
    override def getUser(userId: Option[Int]): Option[UserId] =
      userId.filter(user => users.exists(_.userId == user)).map(UserId)
  }

  implicit object AlgorithmRepoOption extends AlgorithmRepo[Option]{
    override def getAlgorithm(recommenderId: Option[String]): Option[Algorithm] =
      recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))

    override def execute(algo: Algorithm, userId: UserId): Option[UserRec] = algo.run(userId)
  }

  implicit object FilterOption extends Filter[Option] {
    override def filter(userRec: UserRec, limit: Int): Option[UserRec] =
      Some(userRec.copy(recs = recs.slice(0, limit).toList))
  }

  implicit object ProgramOption extends Program[Option] {
    override def flatMap[A, B](fa: Option[A], afb: A => Option[B]): Option[B] = fa.flatMap(afb)

    override def map[A, B](fa: Option[A], ab: A => B): Option[B] = fa.map(ab)

    override def fold[A, B, C](fa: Option[A], first: B => C, second: A => C): C =
      fa.fold(first(UnknownError.asInstanceOf[B]))(second(_))

  }

  implicit object UserRepoEither extends UserRepo[Either[AppError, ?]] {
    override def getUser(userId: Option[Int]): Either[AppError, UserId] = {
      for {
        userParam <- userId.map(UserId).toRight(UserNotProvided)
        userDb    <- users.find(_ == userParam).toRight(UserNotFound(userParam))
      } yield userDb
    }
  }

  implicit object AlgorithmRepoEither extends AlgorithmRepo[Either[AppError, ?]]{
    override def getAlgorithm(recommenderId: Option[String]): Either[AppError, Algorithm] =
      recommenderId.orElse(algoDefault).flatMap(algorithms.get(_))
        .toRight(AlgorithmNotFound(recommenderId.getOrElse(algoDefault.get)))

    override def execute(algo: Algorithm, userId: UserId): Either[AppError, UserRec] =
      algo.run(userId).toRight(RecommendationsNotFound(userId, algo.name))
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
  import algebras._



  def program(userId: Option[Int],
              recommenderId: Option[String] = None,
              limit: Option[Int] = None): Unit = {

    import interpreter._

    val resultEither = getRecommendations[Either[AppError, UserRec]](userId, recommenderId, limit)

    printResults(userId, resultEither)

    val resultOption = getRecommendations[Option](userId, recommenderId, limit)

    printResults(userId, resultOption)
  }

  def getRecommendations[F[_]: UserRepo: AlgorithmRepo: Filter: Program](userId: Option[Int],
                                                                recommenderId: Option[String],
                                                                limit: Option[Int]): F[Result] = {
    for {
      user           <- getUser(userId)
      algorithm      <- getAlgorithm(recommenderId)
      result         <- execute(algorithm, user)
      limitFilter     = limit.getOrElse(limitDefault)
      resultFiltered <- filter(result, limitFilter)
    } yield Result(algorithm, resultFiltered)

  }


  def printResults[F[_]: Program](userId: Option[Int], result: F[Result]): Unit = {
    result.fold[AppError, Unit](error => println(s"Error ${error.message}"), algoRes => {
      println(s"\nRecommnedations for userId ${algoRes.recs.userId}...")
      println(s"Algorithm ${algoRes.algorithm.name}")
      println(s"Recs: ${algoRes.recs.recs}")
    })
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


