package scanimation

import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import akka.testkit.TestKit
import akka.util.Timeout

import scala.concurrent.duration._

class AkkaSpec extends TestKit(ActorSystem("test")) with Spec {
  implicit val to: Timeout = Timeout.durationToTimeout(30.seconds)
  implicit val m: Materializer = ActorMaterializer()

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }
}