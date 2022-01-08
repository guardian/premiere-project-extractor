package models

import java.util.UUID

case class MasterClip(classId:UUID, objectUid:UUID, version:String, name:Option[String], changeVersion:Option[Int], clips:Seq[IntegerReference]) extends ProjectItem
