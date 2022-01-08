package models

import java.util.UUID

case class Project(objectId:String,
                   classId:UUID,
                   version:String,
                   rootProjectItem:Option[UUID],
                   nextSequenceID:Option[String],
                   contents:Map[UUID, ProjectItem],
                   integerIndexedContents:Map[Int, ProjectItemIntIndexed]
                  )
