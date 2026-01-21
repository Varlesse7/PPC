package upmc.akka.ppc

import akka.actor._
import scala.util.{Success, Failure}

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

object Musicien  {
     case class StillAlive(id: Int)
     case class Chef(tab_viv: List[Int])
     case class Vote(id: Int)
     case class ResElection(res: Int)
     case class TabAlive(res: List[Int])
     case class Start()
}

class Musicien (val id:Int, val terminaux:List[Terminal]) extends Actor {
     import Musicien._
     import Election._
     import Provider._
     import Vivarium._

     // Les differents acteurs du systeme
     val db = context.actorOf(Props[DataBaseActor])
     val player = context.actorOf(Props[PlayerActor])
     val prov = context.actorOf(Props(new Provider(db)))
     val conductor = context.actorOf(Props(new Conductor(player, prov)))

     prov ! InitCond(conductor)

     var tab_viv = List[Int](-1,-1,-1,-1)

     val vivarium = context.actorOf(Props(new Vivarium(tab_viv)), name = "vivarium")
     val election = context.actorOf(Props[Election], name = "election")

     var chef = false
     
     
     def receive = {
          
          // Comportement Musicien
          case "StartGame" => {
               conductor ! "StartGame"
               println(tab_viv)
          }

          // Séparer encore plus le tout 
          case Start => {
               vivarium ! NewBorn (id) 

               for (i <- 0 to 3) {
                    val host = terminaux(i).ip
                    val port = terminaux(i).port
                    val path = s"akka.tcp://MozartSystem$i@127.0.0.1:$port/user/Musicien$i"

                    context.actorSelection(path).resolveOne(3.seconds).onComplete {
                         case Success(ref) => ref ! NewBorn(id)
                         case Failure(ex)  => 
                    }
               }
          }

          case NewBorn(id) => {
               vivarium ! NewBorn(id)
          }

          case SynchrAlive(res) => {
               tab_viv = res

               context.system.scheduler.scheduleOnce ( 500.milliseconds ) { 
                    for (i <- 0 to 3) {
                         if (tab_viv(i) >= 0 ){
                              val host = terminaux(i).ip
                              val port = terminaux(i).port
                              val path = s"akka.tcp://MozartSystem$i@127.0.0.1:$port/user/Musicien$i"

                              context.actorSelection(path).resolveOne(3.seconds).onComplete {
                                   case Success(ref) => ref ! StillAlive(id)
                                   case Failure(ex)  => println(s"Alive : Impossible de resoudre $path : $ex")
                              }
                         }
                    } 
               }

               if (!(tab_viv.contains(1))){
                    election ! NewChef(tab_viv)
               }
          }

          // Comportement Chef
          case Chef(tab_viv) =>{
               chef = true
               for (i <- 0 to 3) {
                    if (tab_viv(i) >= 0) {
                         val host = terminaux(i).ip
                         val port = terminaux(i).port
                         val path = s"akka.tcp://MozartSystem$i@127.0.0.1:$port/user/Musicien$i"

                         context.actorSelection(path).resolveOne(3.seconds).onComplete {
                              case Success(ref) => ref ! "StartGame"
                              case Failure(ex)  =>
                         }
                    }
               }
               
               context.system.scheduler.scheduleOnce ( 1800.milliseconds ) { 
                    self ! Chef(tab_viv)
               }
          }

          // Partie Vivarium
          case StillAlive(id) => {
               vivarium ! StillAlive(id)

               context.system.scheduler.scheduleOnce ( 500.milliseconds ) { 
                    for (i <- 0 to 3) {
                         if (tab_viv(i) >= 0 ){
                              val host = terminaux(i).ip
                              val port = terminaux(i).port
                              val path = s"akka.tcp://MozartSystem$i@127.0.0.1:$port/user/Musicien$i"

                              context.actorSelection(path).resolveOne(3.seconds).onComplete {
                                   case Success(ref) => ref ! StillAlive(id)
                                   case Failure(ex)  => println(s"Alive : Impossible de resoudre $path : $ex")
                              }
                         }
                    } 
               }
          }

          // Election
          case Vote (id) => {
               for (i <- 0 to 3 ){
                    if (tab_viv(i) == 0){ // Première fois que j'utilise l'acteur remote
                         val host = terminaux(i).ip
                         val port = terminaux(i).port
                         val path = s"akka.tcp://MozartSystem$i@127.0.0.1:$port/user/Musicien$i"

                         context.actorSelection(path).resolveOne(3.seconds).onComplete {
                              case Success(ref) => ref ! OtherVote(id)
                              case Failure(ex)  => println(s"Impossible de resoudre $path : $ex")
                         }
                    }
               }
          }
          
          case OtherVote(id) => {
               election ! OtherVote(id)
          }

          case ResElection (res) => {
               tab_viv = tab_viv.updated(res, 1)

               if (tab_viv(id) == 1) {
                    vivarium ! ResTab(tab_viv)
                    self ! Chef(tab_viv)
               }
          }

          //
     
          
     }
}
