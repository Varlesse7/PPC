
package upmc.akka.leader

import akka.actor._

object Election () {
    case class NewChef(tab_viv: List(Int))
}

class Election () extends Actor {
    val possible_chef = List()
    var rand = new scala.util.Random
    val resultat = List(0,0,0,0)


    def receive = {
        case NewChef(tab_viv) => {
            for (i <- 0 to 3) {
                if tab_viv(i) == 0 
                then possible_chef + i
            }
            self ! MyVote
        }
        case MyVote =>{
            var vote = rand.nextInt(possible_chef.lenght)
            sender ! Vote(possible_chef(vote))
        }
        case OtherVote(id) => {
            resultat(id) = resultat(id) + 1
        }

        if resultat.sum == possible_chef.lenght 
        then ResElection(resultat.zipWithIndex.maxBy(_._1)._2)
    }
}