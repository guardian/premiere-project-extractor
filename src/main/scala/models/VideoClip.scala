package models

import java.util.UUID

case class VideoClip(classId:UUID, objectUid:Int, version:String, source:Option[IntegerReference], clipId:Option[UUID], inUse:Option[Boolean]) extends ProjectItemIntIndexed
