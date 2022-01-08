package models

import java.util.UUID

trait ProjectItem {
  val classId: UUID
  val objectUid: UUID
  val version: String
  val name:Option[String]
}

trait ProjectItemIntIndexed {
  val classId: UUID
  val objectUid: Int
  val version: String
}