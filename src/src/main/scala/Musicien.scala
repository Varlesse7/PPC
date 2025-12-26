package upmc.akka.leader

import akka.actor._

object Musicien (val id: Int, val terminaux: List[Terminal]) {
     case class Alive(id: Int)
     case class Chef(tab_viv: List(Int))
}

class Musicien (val id:Int, val terminaux:List[Terminal]) extends Actor {

     // Les differents acteurs du systeme
     val conductor = context.actorOf(Props[Conductor], name = "conductor")
     val vivarium = context.actorOf(Props[Vivarium], name = "vivarium")
     val election = context.actorOf(Props[Election], name = "election")

     val chef = false

     
     def receive = {
          
          // Comportement Musicien
          case "StartGame" => {
               conductor ! "StartGame"
          }
          
          // Comportement Chef
          case Chef(tab_viv) =>{
               chef = true
               for (i <- 0 to 3) {
                    if tab_viv(i) == 0 
                    then terminaux[i].port ! "StartGame"
               }
          }

          // Partie Vivarium
          case Alive(id) => {
               vivarium ! Alive(id)
          }
          case TabAlive(res) => {
               tab_viv = res
               // Partie Election
               if !(tab_viv.contains(1))
               then election ! NewChef(tab_viv)
          }
          case Vote (id) => {
               if tab_viv(i) == 0 
               then terminaux[i].port ! OtherVote(id)
          }
          case OtherVote(id) => {
               election ! OtherVote(id)
          }
          case ResElection (res) => {
               tab_viv(res) = 1
          }

          //

          
          // Envoie au autres que je suis vivant
          for (i <- 0 to 3) {
               if tab_viv(i) == 0 
               then terminaux[i].port ! Alive(id)
          } 

     
          if chef 
          then context.system.scheduler.scheduleOnce (1800 milliseconds) (Chef(tab_viv))
          
     }
}
