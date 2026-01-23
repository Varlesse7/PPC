package upmc.akka.ppc

import akka.actor._

object Vivarium {
    case class ResTab(res: List[Int])
    case class SynchrNewAlive(res: List[Int])
    case class SynchrAlive(res: List[Int])
    case class NewBorn(id: Int)
}

class Vivarium (var tab_viv: List[Int]) extends Actor {
    import Vivarium._
    import Musicien._

    var tab_cmpt = List(0,0,0,0)

    def receive = {
        
        case ResTab(res) => {
            tab_viv = res
        }

        case NewBorn(id) => {
            if (tab_viv(id) == -1){
                tab_viv = tab_viv.updated(id, 0)

                sender ! SynchrNewAlive(tab_viv)
            }
        }

        case StillAlive (id, role) => {

            if (tab_viv(id) == -1){
                tab_viv = tab_viv.updated(id, role)
                sender ! SynchrAlive(tab_viv)
            }

            for (i <- 0 to 3) {
                if (tab_viv(i) >= 0){
                    if (i == id){
                        tab_cmpt = tab_cmpt.updated(id, 0)
                    } else {
                        tab_cmpt = tab_cmpt.updated(id, tab_cmpt(id) + 1)
                    }
                }
            }



            for (i <- 0 to 3) {
                if ((tab_cmpt(i) >= 10)){
                    tab_viv = tab_viv.updated(i, -1)
                    tab_cmpt = tab_cmpt.updated(i, 0)
                }
            }
        }
    }
}