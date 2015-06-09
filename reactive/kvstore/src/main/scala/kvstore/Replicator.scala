package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)

  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import context.dispatcher

  var acks = Map.empty[Long, (ActorRef, Replicate)]
  var pending = Vector.empty[Snapshot]
  var seqCounter = 0L
  def nextSeq = {
    val ret = seqCounter
    seqCounter = seqCounter + 1
    ret
  }

  val timeout = Duration.create(50, MILLISECONDS)

  context.system.scheduler.schedule(timeout, timeout) {
    acks foreach {
      case (seq, (sender, Replicate(key, valueOption, id))) => replica ! Snapshot(key, valueOption, seq)
    }
  }

  def receive: Receive = {
    case Replicate(key, valOpt, id) => {
      val nxtSeq = nextSeq
      acks = acks + (nxtSeq -> (sender, Replicate(key, valOpt, id)))
      replica ! Snapshot(key, valOpt, nxtSeq)
    }

    case SnapshotAck(key, nxtSeq) => {
      (acks get nxtSeq) foreach {
        case (sender, Replicate(key, valOpt, id)) => {
          sender ! Replicated(key, id)
          acks = acks - nxtSeq
        }
      }
    }
  }
}