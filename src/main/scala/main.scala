import akka.actor.ActorSystem
import akka.stream.Materializer
import marshaller.PremiereReader

import java.nio.file.Paths
import scala.concurrent.ExecutionContext
import io.circe.generic.auto._
import io.circe.syntax._


object main {
  implicit val system = ActorSystem("prem-test")
  implicit val mat = Materializer.matFromSystem
  implicit val ec:ExecutionContext = system.dispatcher

  def main(args: Array[String]): Unit = {
    import models.UUIDEncoder._
    val reader = new PremiereReader()
    reader
      .readFromFile(Paths.get("20210630_enlightenment_technologies_ultra.xml"))
      //.readFromFile(Paths.get("KP-47797.xml"))
//      .map(_.map(assembledProject=>{
//        val json = assembledProject.asJson
//        println(json.toString())
//      }))
      .onComplete({
        _ =>
          system.terminate()
      })
  }
}
