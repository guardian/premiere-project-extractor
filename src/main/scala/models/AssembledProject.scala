package models

import java.util.UUID
import io.circe.generic.auto._
import UUIDEncoder._
import io.circe.{Encoder, Json}

sealed trait AssembledProjectItem {
  val version:String
  val classId:UUID
  val name:String
}

case class GenericProjectItem(version:String, classId:UUID, children:Seq[AssembledProjectItem], name:String) extends AssembledProjectItem
case class AssembledClip(version:String, classId:UUID, name:String, clips:Seq[VideoMediaSource]) extends AssembledProjectItem

case class AssembledProject(version:String,classId:UUID,rootProjectItem:AssembledProjectItem) {
  def dumpToScreen = {
    def dumpContents(level:Int, item:AssembledProjectItem):Unit = {
      val indent = "  " * level
      item match {  //only certain types of sub-object have their own sub-object
        case GenericProjectItem(_, _, children, _)=>
          println(s"$indent${item.classId} ${item.version} ${item.name}")
          children.foreach(child=>dumpContents(level+1, child))
        case AssembledClip(version, classId, name, clips)=>
          println(s"$indent${classId} ${version} ${name} Sources: ${clips.map(_.mediaRef)} [duration: ${clips.map(_.originalDuration)}]")
        case _=>
          println(s"$indent${item.classId} ${item.version} ${item.name}")
      }
    }

    println(s"Project version $version classID $classId\n")
    println("-----------------------------------------\n")
    dumpContents(0, rootProjectItem)
  }
}

//object AssembledProject {
//  implicit val encodeAssembledProject:Encoder[AssembledProject] = new Encoder[AssembledProject] {
//    override def apply(a: AssembledProject): Json = Json.obj(
//      "version"->Json.fromString(a.version),
//      "classId"->Json.fromString(a.classId.toString),
//      "rootProjectItem"->a.rootProjectItem match {
//        case i@GenericProjectItem(_,_,_,_)=>Encoder[GenericProjectItem].apply(i)
//        case i@AssembledClip(_)=>Encoder[AssembledClip].apply(i)
//      }
//    )
//  }
//}