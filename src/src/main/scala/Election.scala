package upmc.akka.ppc

import akka.actor._

object Election {
    case class NewChef(tab_viv: List[Int])
    case class OtherVote(id: Int)
}

class Election () extends Actor {
    import Musicien._
    import Election._

    var possible_chef = List[Int]()
    var rand = new scala.util.Random
    var resultat = List(0,0,0,0)


    def receive = {
        case NewChef(tab_viv) => {
            for (i <- 0 to 3) {
                if (tab_viv(i) >= 0) {
                    println(s"\n\nExec : $i")
                    possible_chef = possible_chef :+ i 
                }
            }

            println(s"Possible Chef : $possible_chef\n\n")

            if (possible_chef.length > 1){
                var vote = rand.nextInt(possible_chef.length)
                sender ! Vote(possible_chef(vote))
            } else{
                sender ! Vote(possible_chef(0))
            }
        }
        
        case OtherVote(id) => {
            resultat = resultat.updated(id, resultat(id)+ 1)

            if (resultat.sum == possible_chef.length){
                possible_chef = List[Int]()
                sender ! ResElection(resultat.zipWithIndex.maxBy(_._1)._2)
                resultat = List(0,0,0,0)
            }
        }

    }

}