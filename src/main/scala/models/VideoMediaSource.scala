package models

import java.util.UUID

case class VideoMediaSource(classId:UUID, objectUid:Int, version:String, mediaRef:Option[UUID], originalDuration:Option[Long]) extends ProjectItemIntIndexed
