package models

import io.circe.Decoder.Result
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

import java.util.UUID
import scala.util.{Failure, Success, Try}

object UUIDEncoder {
  implicit val uuidEncoder:Encoder[UUID] = new Encoder[UUID] {
    final def apply(a: UUID): Json = Json.fromString(a.toString)
  }

  implicit val uuidDecoder:Decoder[UUID] = new Decoder[UUID] {
    final def apply(c: HCursor): Result[UUID] = Try {
      UUID.fromString(c.value.toString())
    } match {
      case Success(uuid)=>Right(uuid)
      case Failure(err)=>Left(DecodingFailure(err.getMessage, c.history))
    }
  }
}
