package models

import java.util.UUID

case class ClipProjectItem(classId:UUID, objectUid:UUID, version:String, name:Option[String], masterClipId:Option[UUID]) extends ProjectItem
