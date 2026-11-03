package demos

import scala.collection.mutable.ListBuffer
import org.slf4j.LoggerFactory
import utils.ImplicitConversions.anyToRunnable

object DemoActors {

  import org.apache.pekko.actor.{Actor, ActorLogging, ActorSystem, Props}

  import scala.collection.mutable

  trait Listener {
    def onDone(): Unit
  }

  class B {
    val listeners = ListBuffer[Listener]()

    def addListener(l: Listener): Unit = listeners += l

    def doWork() = {
      Thread.sleep(10000)
      listeners.foreach(_.onDone())
    }
  }

  class A extends Actor with ActorLogging with Listener {
    private var done = true

    override def receive: Receive = {
      case _ =>
        log.debug("Received")
        if (done) {
          log.debug("Processing message")
          done = false
          val b = new B
          b.addListener(this)
          new Thread(() => b.doWork()).start()
        } else {
          log.debug("Busy processing. Ignoring message")
        }
    }

    override def onDone(): Unit = done = true
  }

  def run(): Unit = {
    val log = LoggerFactory.getLogger(getClass)
    val actorSystem = ActorSystem.create("asd")
    val a = actorSystem.actorOf(Props(new A))
    (1 to 10).foreach { i =>
      log.debug("Sending message")
      a ! "hello"
      log.debug("Sleeping in while")
      Thread.sleep(2000)
    }
  }
}
