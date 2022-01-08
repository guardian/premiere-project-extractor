package models

import java.util.UUID

case class BinProjectItem(classId:UUID, objectUid:UUID, version:String, name:Option[String], items:Seq[ItemReference]) extends ProjectItem