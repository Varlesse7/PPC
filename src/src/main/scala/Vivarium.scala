package upmc.akka.leader

import akka.actor._

object Vivarium (val tab_viv: List(Int)) {
     case class Alive(id: Int)
}

class Vivarium (val tab_viv: List(Int)) extends Actor {

    val tab_cmpt = List(0,0,0,0)

    def receive = {
        case Alive (id) => {
            for (i <- 0 to 3) {
                if i == id 
                then tab_cmpt(i) = 0
                else tab_cmpt(i) = tab_cmpt(i)+1
            }
        }

        for (i <- 0 to 3) {
            if tab_cmpt(i) >= 10 
            then {
                tab_viv(i) = -1
                sender ! TabAlive(tab_viv)
            }
        }
    }
}