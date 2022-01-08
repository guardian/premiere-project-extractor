package models

import java.util.UUID

case class ItemReference (index:Option[Int],to:UUID)
case class IntegerReference(index:Option[Int], to:Int)