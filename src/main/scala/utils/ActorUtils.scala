package utils

import java.util.concurrent.{Executors, TimeUnit}

import org.apache.pekko.actor.{ActorRef, ActorSystem, Cancellable}
import messages.Message

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

object ActorUtils {
  implicit val executionContext: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())

  def broadcast(message: Message)(implicit system: ActorSystem): Unit = system.eventStream.publish(message)

  def schedule(delayMS: Long,
    intervalMS: Long,
    task: Runnable)(implicit system: ActorSystem): Cancellable = {
    val delay = FiniteDuration(delayMS, TimeUnit.MILLISECONDS)
    val duration = FiniteDuration(intervalMS, TimeUnit.MILLISECONDS)

    system.scheduler.scheduleWithFixedDelay(delay, duration)(task)
  }

  def scheduleOnce(delayMS: Long,
    task: Runnable)(implicit system: ActorSystem): Cancellable = {
    val delay = FiniteDuration(delayMS, TimeUnit.MILLISECONDS)
    system.scheduler.scheduleOnce(delay, task)
  }

  def subscribe(a: ActorRef, c: Class[?])(implicit sys: ActorSystem): Unit = {
    sys.eventStream.subscribe(a, c)
  }
}
