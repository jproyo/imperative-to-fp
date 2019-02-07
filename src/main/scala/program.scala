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
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }
  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)
    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.flatMap(fa, afb)
    def fold[B, C](first: B => C, second: A => C)(implicit F: Program[F]): C = F.fold(fa, first, second)
  }

  trait UserRepo[F[_]] {
    def getUser(userId: Option[Int]): F[UserId]
  }

  object UserRepo {
    def apply[F[_]](implicit F: UserRepo[F]): UserRepo[F] = F
  }

  def getUser[F[_]: UserRepo](userId: Option[Int]): F[UserId] = UserRepo[F].getUser(userId)

  trait Limiter[F[_]] {
    def limit(limit: Option[Int]): F[Int]
  }

  object Limiter {
    def apply[F[_]](implicit F: Limiter[F]): Limiter[F] = F
  }

  def limiter[F[_]: Limiter](limit: Option[Int]): F[Int] = Limiter[F].limit(limit)


  trait AlgorithmRepo[F[_]] {
    def getAlgorithm(recommenderId: Option[String]): F[(String, Algorithm)]
    def execute(algo: Algorithm, userId: UserId): F[UserRec]
  }

  object AlgorithmRepo {
    def apply[F[_]](implicit F: AlgorithmRepo[F]): AlgorithmRepo[F] = F
  }

  def getAlgorithm[F[_]: AlgorithmRepo](recommenderId: Option[String]): F[(String, Algorithm)] = AlgorithmRepo[F].getAlgorithm(recommenderId)
  def execute[F[_]: AlgorithmRepo](algorithm: Algorithm, userId: UserId): F[UserRec] = AlgorithmRepo[F].execute(algorithm, userId)



}

object interpreter {

  import DataSource._
  import algebras._


  implicit object ProgramOption extends Program[Option] {
    override def flatMap[A, B](fa: Option[A], afb: A => Option[B]): Option[B] = fa.flatMap(afb)

    override def map[A, B](fa: Option[A], ab: A => B): Option[B] = fa.map(ab)

    override def fold[A, B, C](fa: Option[A], first: B => C, second: A => C): C = fa match {
      case Some(x) => second(x)
      case None => first(UnknownError.asInstanceOf[B])
    }

  }

  implicit object UserRepoOption extends UserRepo[Option] {
    override def getUser(userId: Option[Int]): Option[UserId] = {
      for {
        userParam <- userId
        userData  <- users.find(_.userId == userParam)
      } yield userData
    }
  }

  implicit object AlgorithmRepoOption extends AlgorithmRepo[Option] {
    override def getAlgorithm(recommenderId: Option[String]): Option[(String, Algorithm)] = {
      (for {
        recorDef  <- recommenderId.orElse(algoDefault)
        recId     <- algorithms.keys.find(_ == recorDef).orElse(algoDefault)
        algorithm <- algorithms.get(recId)
      } yield (recId, algorithm))
    }

    override def execute(algo: Algorithm, userId: UserId): Option[UserRec] = algo.run(userId)

  }

  implicit object LimiterOption extends Limiter[Option] {
    override def limit(limit: Option[Int]): Option[Int] = limit.orElse(Some(limitDefault))
  }


  implicit object ProgramEither extends Program[Either[AppError, ?]] {
    override def flatMap[A, B](fa: Either[AppError, A], afb: A => Either[AppError, B]): Either[AppError, B] =
      fa.flatMap(afb)

    override def map[A, B](fa: Either[AppError, A], ab: A => B): Either[AppError, B] = fa.map(ab)

    override def fold[A, B, C](fa: Either[AppError, A], first: B => C, second: A => C): C = fa match {
      case Left(value) => first(value.asInstanceOf[B])
      case Right(value) => second(value)
    }

  }

  implicit object UserRepoEither extends UserRepo[Either[AppError, ?]] {
    override def getUser(userId: Option[Int]): Either[AppError, UserId] = {
      for {
        userParam <- userId.toRight(UserNotProvided)
        userData  <- users.find(_.userId == userParam).toRight(UserNotFound(UserId(userParam)))
      } yield userData
    }
  }

  implicit object AlgoRepoEither extends AlgorithmRepo[Either[AppError, ?]] {
    override def getAlgorithm(recommenderId: Option[String]): Either[AppError, (String, Algorithm)] = {
      (for {
        recorDef  <- recommenderId.orElse(algoDefault)
        recId     <- algorithms.keys.find(_ == recorDef).orElse(algoDefault)
        algorithm <- algorithms.get(recId)
      } yield (recId, algorithm)).toRight(UnknownError)
    }

    override def execute(algo: Algorithm, userId: UserId): Either[AppError, UserRec] =
      algo.run(userId).toRight(RecommendationsNotFound(userId, algo.name))
  }

  implicit object LimiterEither extends Limiter[Either[AppError, ?]] {
    override def limit(limit: Option[Int]): Either[AppError, Int] =
      limit.orElse(Some(limitDefault)).toRight(UnknownError)
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


  implicit class FilterRecs(recs: UserRec){
    def filter(limit: Int): UserRec =
      recs.copy(recs = recs.recs.slice(0, limit))
  }



  def program(userId: Option[Int],
              recommenderId: Option[String] = None,
              limit: Option[Int] = None): Unit = {


    import interpreter._

    val resultEither = getRecommendations[Either[AppError, ?]](userId, recommenderId, limit)

    printResult[Either[AppError, ?]](userId, resultEither)

    val resultOption = getRecommendations[Option](userId, recommenderId, limit)

    printResult[Option](userId, resultOption)

  }

  def getRecommendations[F[_]: UserRepo: AlgorithmRepo: Limiter: Program](userId: Option[Int],
                                                                          recommenderId: Option[String],
                                                                          limit: Option[Int]): F[(String, UserRec)] = {
    for {
      userData            <- getUser(userId)
      (recId, algorithm)  <- getAlgorithm(recommenderId)
      recs                <- execute(algorithm, userData)
      limit               <- limiter(limit)
      filteredRecs         = recs.filter(limit)
    } yield (recId, filteredRecs)
  }

  private def printResult[F[_]: Program](userId: Option[Int], result: F[(String, UserRec)]): Unit = {
    result.fold[AppError, Unit](e => println(s"Error: ${e.message}"), recs => {
      println("\nRecommendations")
      println("--------------------------------------")
      println(s"UserId: ${recs._2.userId}")
      println(s"Algorithm: ${recs._1}")
      println(s"Recs: ${recs._2.recs}")
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


