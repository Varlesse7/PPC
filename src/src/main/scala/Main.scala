package upmc.akka.ppc

import akka.actor.{Props,  Actor,  ActorRef,  ActorSystem}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

object Concert extends App {
  import Provider._
  import DataBaseActor._
  import Conductor._
  import PlayerActor._
  println("starting Mozart's game")

  val system = ActorSystem("ConcertSimulation")
  val db = system.actorOf(Props[DataBaseActor])
  val player = system.actorOf(Props[PlayerActor])
  val prov = system.actorOf(Props(new Provider(db)))
  val conductor = system.actorOf(Props(new Conductor(player, prov)))

  prov ! InitCond(conductor)

  conductor ! "StartGame"

 }
