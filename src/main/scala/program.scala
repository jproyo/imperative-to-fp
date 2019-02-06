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
      if(user.userId % 2 == 0) {
        UserRec(user, List.empty)
      } else {
        UserRec(user, recs.toList)
      }
    }
  }


  def algorithm1(userId: Int): Option[UserRec] =
    recommendations.find(u => u.userId.userId == userId)

  def algorithm2(userId: Int): Option[UserRec] =
    recommendations
      .find(u => u.userId.userId == userId)
      .map(_.copy(recs = recs.filter(r => r.recId > "h").toList))

  lazy val algorithms = Map (
    "algo1" -> algorithm1 _,
    "algo2" -> algorithm2 _
  )

  val algoDefault = "algo1"

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


    val result = for {
      userParam    <- userId
      userData     <- users.find(_.userId == userParam)
      recId        <- recommenderId.orElse(Some(algoDefault))
      limit        <- limit.orElse(Some(limitDefault))
      algorithm    <- algorithms.get(recId)
      recs         <- algorithm(userData.userId)
      filteredRecs  = recs.filter(limit)
    } yield filteredRecs

    result.map { recs =>
      println(s"\nRecommnedations for userId ${recs.userId}....")
      println(s"Algorithm $recommenderId")
      println(s"Recs: ${recs.recs}")
    }.getOrElse{
      println(s"No recommendations found for user $userId")
    }

  }
}

object ToScalaFP extends App {
  import AppImperative._

  program(Some(2), Some("algo5"), Some(5))
}


