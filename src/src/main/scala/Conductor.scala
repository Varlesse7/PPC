package upmc.akka.ppc

import akka.actor.{Props,  Actor,  ActorRef,  ActorSystem}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import ExecutionContext.Implicits.global

object Conductor {
    case class Play(meas: DataBaseActor.ObjetMusical)
}


class Conductor (player: ActorRef,prov: ActorRef) extends Actor {
    import Provider._
    import PlayerActor._

    var rand = new scala.util.Random

    def receive = {
        case "StartGame" => {
            var de1 = rand.nextInt(6)
            var de2 = rand.nextInt(6)
            prov ! GetMeasure(de1+de2)
        }
        case SendMeasure(meas) => {
            player ! Play(meas)
            context.system.scheduler.scheduleOnce ( 1800.milliseconds ) { 
                self ! "StartGame"
            }
        }
    }
}
