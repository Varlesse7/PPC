package upmc.akka.ppc

import akka.actor._


class Vivarium (var tab_viv: List[Int]) extends Actor {

    import Musicien._

    var tab_cmpt = List(0,0,0,0)

    def receive = {
        case Alive (id, role) => {
            
            if (role == -1) {
                tab_viv = tab_viv.updated(id, 0)
            }
            for (i <- 0 to 3) {
                if (i == id){
                    tab_cmpt = tab_cmpt.updated(id, 0)
                } else {
                    tab_cmpt = tab_cmpt.updated(id, tab_cmpt(id) + 1)
                }
            }


            for (i <- 0 to 3) {
                if ((tab_cmpt(i) >= 10) && (tab_viv(i) >= 0)){
                    tab_viv = tab_viv.updated(i, -1)
                }
            }
            sender ! TabAlive(tab_viv)
        }

    }
}