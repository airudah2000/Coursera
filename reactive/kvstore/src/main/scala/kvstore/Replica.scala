package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor, ActorLogging }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor with ActorLogging {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher
  import context.system

  var kv = Map.empty[String, String]
  var secondaries = Map.empty[ActorRef, ActorRef]
  var replicators = Set.empty[ActorRef]
  var replicatorAcks = Map.empty[Long, (ActorRef, Set[ActorRef])]
  val persistence = system.actorOf(persistenceProps)
  var persistAcks = Map.empty[Long, (ActorRef, Persist)]

  override val supervisorStrategy = OneForOneStrategy() {
    case _: Exception => Restart
  }

  val timeout = Duration.create(50, MILLISECONDS)
  system.scheduler.schedule(timeout, timeout) {
    persistAcks.values foreach {
      case (client, msg) => persistence ! msg
    }
  }

  def setFailTimer(id: Long) {
    system.scheduler.scheduleOnce(Duration.create(1, SECONDS)) {
      persistAcks get id foreach {
        case (client, msg) => {
          client ! OperationFailed(id)
          persistAcks = persistAcks - id
        }
      }

      replicatorAcks get id foreach {
        case (client, actors) => {
          client ! OperationFailed(id)
          replicatorAcks = replicatorAcks - id
        }
      }
    }
  }

  def persist(key: String, valueOption: Option[String], id: Long) = {
    val persistMsg = Persist(key, valueOption, id)
    persistAcks = persistAcks + (id -> (sender, persistMsg))
    persistence ! persistMsg
  }

  def replicate (client: ActorRef, key: String, valueOption: Option[String], id: Long) = {
    if (! replicators.isEmpty) {
      replicatorAcks = replicatorAcks + (id -> (client, replicators))
      replicators foreach {
        rplctr => rplctr ! Replicate(key, valueOption, id)
      }
    }
  }

  def replicatorAcknowledged(id: Long, actor: ActorRef) = {
    replicatorAcks get(id) foreach {
      case (client, actors) => {
        val remainder = actors - actor
        if (remainder.isEmpty) {
          replicatorAcks = replicatorAcks - id
        } else {
          replicatorAcks += (id -> (client, remainder))
        }

        if (remainder.isEmpty && (persistAcks get id).isEmpty) {
          client ! OperationAck(id)
        }
      }
    }
  }

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }


  val leader: Receive = {
    case Insert(key, value, id) => {
      setFailTimer(id)
      persist(key, Some(value), id)
      replicate(sender, key, Some(value), id)
    }

    case Remove(key, id) => {
      setFailTimer(id)
      persist(key, None, id)
      replicate(sender, key, None, id)
    }

    case Persisted(key, id) => {
      log.debug("Primary persisted " + id)

      val ack = persistAcks get id
      persistAcks = persistAcks - id

      ack.foreach {
        case (client, Persist(key, valueOption, id)) => {
          valueOption match {
            case Some(value) => kv = kv + (key -> value)
            case None => kv = kv - key
          }
        }

          if ((replicatorAcks get id).isEmpty) client ! OperationAck(id)
      }
    }

    case Replicated(key, id) => replicatorAcknowledged(id, sender)

    case Get(key, id) => sender ! GetResult(key, kv get key, id)

    case Replicas(replicas) => {
      val newSecondaries = replicas - self -- secondaries.keySet
      val removedSecondaries = secondaries.keySet -- replicas
      val removedReplicators = removedSecondaries.map{sec => (secondaries get sec).get}

      removedSecondaries.foreach {secondary => secondaries -= secondary}

      removedReplicators.foreach {
        replicator => {
          system.stop(replicator)
          replicatorAcks.keySet.foreach {
            id => replicatorAcknowledged(id, replicator)
          }
        }
      }

      newSecondaries.foreach {
        secondary => {
          val replicator = system.actorOf(Replicator.props(secondary))
          secondaries += (secondary -> replicator)

          kv.keys map {key => replicator ! Replicate(key, kv get key, 0L)}
        }
      }
      replicators = secondaries.values.toSet
    }
  }

  var expectedSeq = 0L

  var pendingChanges = Map.empty[String, (Long, Option[String])]

  val replica: Receive = {

    case Get(key, id) => {
      val effectiveValue = pendingChanges get key match {
        case Some((seq, valueOption)) => valueOption
        case _ => kv.get(key)
      }
      sender ! GetResult(key, effectiveValue, id)
    }

    case Snapshot(key, valueOption, seq) => {
      if (seq < expectedSeq) {
        sender ! SnapshotAck(key, seq)
      }

      if (seq == expectedSeq) {
        pendingChanges += (key -> (seq, valueOption))
        persist(key, valueOption, seq)
      }
    }

    case Persisted(key, seq) => {
      val ack = persistAcks get seq
      persistAcks = persistAcks - seq
      ack.foreach {
        case (client, Persist(key, valueOption, id)) => {
          valueOption match {
            case Some(value) => kv += (key -> value)
            case None => kv -= key
          }

          expectedSeq = Math.max(expectedSeq, seq + 1)

          pendingChanges get(key) foreach {
            case (pendingSeq, pendingChange) if (pendingSeq <= seq) => pendingChanges = pendingChanges - key
          }

          client ! SnapshotAck(key, seq)
        }
      }
    }
  }

  arbiter ! Join
}