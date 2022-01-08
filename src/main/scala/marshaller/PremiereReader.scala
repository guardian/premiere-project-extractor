package marshaller
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.alpakka.xml.{EndElement, ParseEvent, StartElement, TextEvent}
import akka.stream.alpakka.xml.scaladsl.XmlParsing
import akka.stream.scaladsl.{FileIO, Keep, Sink}
import models.{AssembledClip, AssembledProject, AssembledProjectItem, BinProjectItem, ClipProjectItem, GenericProjectItem, IntegerReference, ItemReference, MasterClip, Project, ProjectItem, VideoClip, VideoMediaSource}

import java.nio.file.Path
import java.util.UUID
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

class PremiereReader (implicit actorSystem: ActorSystem, mat:Materializer)
{
  case class ParserState(nodeTree:mutable.Stack[String],
                         project:Option[Project],
                         currentBin:Option[BinProjectItem],
                         currentClip:Option[ClipProjectItem],
                         currentMasterClip:Option[MasterClip],
                         currentVideoClip:Option[VideoClip],
                         currentMediaSource:Option[VideoMediaSource],
                         stringBuffer: StringBuilder
                        )
  object ParserState {
    def empty = new ParserState(mutable.Stack.empty, None, None, None, None, None, None, new StringBuilder)
  }

  implicit class ElementImprovements(el:StartElement) {
    def uuidAttribute(attrName:String) = {
      for {
        attrString <- el.findAttribute(attrName)
        uuid <- Try { UUID.fromString(attrString.value)}.toOption
      } yield uuid
    }

    def stringAttribute(attrName:String) = el.findAttribute(attrName).map(_.value)

    def intAttribute(attrName:String) = stringAttribute(attrName)
      .flatMap(str=>Try { str.toInt }.toOption)
  }

  protected def handleParseEvent(ev:ParseEvent, state:ParserState):ParserState = ev match {
    case el: StartElement=>
      val updatedNodeTree = state.nodeTree.push(el.localName)
      el.localName match {
        case "Project"=>
          el.uuidAttribute("ClassID") match {
            case Some(clsId)=>
              val newProject = Project(el.stringAttribute("ObjectID").getOrElse(""), clsId, el.stringAttribute("Version").getOrElse(""), None,None, Map(), Map())
              state.copy(nodeTree = updatedNodeTree, project = Some(newProject))
            case None=>
              state
          }
        case "Item"=>
          (state.currentBin, el.uuidAttribute("ObjectURef")) match {
            case (Some(bin), Some(uref))=>
              val updatedBin = bin.copy(items = bin.items :+ ItemReference(el.intAttribute("Index"), uref))
              state.copy(nodeTree = updatedNodeTree, currentBin = Some(updatedBin))
            case _=>
              state
          }
        case "RootProjectItem"=>
          (state.project, el.uuidAttribute("ObjectURef"), el.uuidAttribute("ObjectUID"),el.uuidAttribute("ClassID")) match {
            case (Some(project), Some(rootItemRef), _, _)=>
              val newProject = project.copy(rootProjectItem=Some(rootItemRef))
              state.copy(nodeTree = updatedNodeTree, project = Some(newProject))
            case (Some(project), _, Some(uid), Some(clsId))=>
              val newItem = BinProjectItem(clsId, uid, el.stringAttribute("Version").getOrElse(""), None, Seq())
              state.copy(nodeTree = updatedNodeTree,currentBin = Some(newItem))
            case _=>
              state
          }
        case "BinProjectItem"=>
          (el.uuidAttribute("ObjectUID"), el.uuidAttribute("ClassID")) match {
            case (Some(uid), Some(clsid))=>
              val newBin = BinProjectItem(clsid, uid, el.stringAttribute("Version").getOrElse(""), None, Seq())
              state.copy(nodeTree = updatedNodeTree,currentBin = Some(newBin))
            case _=>
              throw new ReaderException("BinProjectItem without ObjectUID / ClassID")
          }
        case "ClipProjectItem"=>
          (el.uuidAttribute("ObjectUID"), el.uuidAttribute("ClassID")) match {
            case (Some(uid), Some(clsid))=>
              val newClip = ClipProjectItem(clsid, uid, el.stringAttribute("Version").getOrElse(""), None, None)
              state.copy(nodeTree = updatedNodeTree,currentClip = Some(newClip))
            case _=>
              throw new ReaderException("ClipProjectItem without ObjectUID / ClassID")
          }
        case "MasterClip"=>
          (el.uuidAttribute("ObjectURef"), state.currentClip, el.uuidAttribute("ObjectUID"), el.uuidAttribute("ClassID")) match {
            case (Some(uref), Some(clip), _, _) =>
              state.copy(
                nodeTree = updatedNodeTree,
                currentClip = Some(clip.copy(
                  masterClipId = Some(uref)
                ))
              )
            case (_, _, Some(uid), Some(clsid)) =>
              val newMC = MasterClip(clsid, uid, el.stringAttribute("Version").getOrElse(""), None, None, Seq())
              state.copy(nodeTree = updatedNodeTree, currentMasterClip = Some(newMC))
            case _ =>
              val params = (el.uuidAttribute("ObjectURef"), state.currentClip, el.uuidAttribute("ObjectUID"), el.uuidAttribute("ClassID"))
              throw new ReaderException(s"MasterClip without ObjectURef / ObjectUID / ClassID: $params")
          }
        case "Clip"=>
          (state.currentMasterClip, el.intAttribute("ObjectRef")) match {
            case (Some(mc), Some(objectRef))=>
              state.copy(
                nodeTree = updatedNodeTree,
                currentMasterClip = Some(
                  mc.copy(clips = mc.clips :+ IntegerReference(el.intAttribute("Index"), objectRef))
                )
              )
            case _=>
              state
          }
        case "Source"=>
          (state.currentVideoClip, el.intAttribute("ObjectRef")) match {
            case (Some(clip), Some(ref))=>
              state.copy(
                nodeTree = updatedNodeTree,
                currentVideoClip = Some(
                  clip.copy(source = Some(IntegerReference(None, ref)))
                )
              )
            case _=>
              state
          }
        case "VideoClip"|"AudioClip"=>
          (el.uuidAttribute("ClassID"), el.intAttribute("ObjectID")) match {
            case (Some(clsid), Some(oid))=>
              val newVC = VideoClip(clsid, oid, el.stringAttribute("Version").getOrElse(""), None, None, None)
              state.copy(
                nodeTree = updatedNodeTree,
                currentVideoClip = Some(newVC)
              )
            case _=>
              state
          }
        case "VideoMediaSource"|"AudioMediaSource"=>
          (el.uuidAttribute("ClassID"), el.intAttribute("ObjectID")) match {
            case (Some(clsid), Some(oid))=>
              state.copy(
                nodeTree = updatedNodeTree,
                currentMediaSource = Some(VideoMediaSource(clsid, oid, el.stringAttribute("Version").getOrElse(""), None, None))
              )
            case _=>
              state
          }
        case _=>
          state.copy(nodeTree = updatedNodeTree)
      }
    case el: EndElement=>
      el.localName match {
        case "Name"=>
          (state.currentBin, state.currentClip, state.currentMasterClip) match {
            case (Some(bin), _, _)=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                currentBin = Some(
                  bin.copy(name = Some(state.stringBuffer.toString().strip())
                  )
                ),
                stringBuffer = state.stringBuffer.empty
              )
            case (_, Some(clip), _)=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                currentClip = Some(
                  clip.copy(name = Some(state.stringBuffer.toString().strip()))
                ),
                stringBuffer = state.stringBuffer.empty
              )
            case (_, _, Some(masterClip))=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                currentMasterClip = Some(
                  masterClip.copy(name = Some(state.stringBuffer.toString().strip()))
                ),
                stringBuffer = state.stringBuffer.empty
              )
            case _=>
              state.copy(nodeTree = state.nodeTree.tail, stringBuffer = state.stringBuffer.empty)
          }
        case "Media"=>
          (state.currentMediaSource, Try{UUID.fromString(state.stringBuffer.toString().strip())}.toOption) match {
            case (Some(source), Some(uid))=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                currentMediaSource = Some(
                  source.copy(mediaRef = Some(uid))
                )
              )
            case _=>
              state
          }
        case "OriginalDuration"=>
          (state.currentMediaSource, Try{state.stringBuffer.toString().strip().toLong}.toOption) match {
            case (Some(source), Some(longval))=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                currentMediaSource = Some(
                  source.copy(originalDuration = Some(longval))
                )
              )
            case _=>
              state
          }
        case "ClipID"=>
          (state.currentVideoClip, Try { UUID.fromString(state.stringBuffer.toString().strip())}.toOption) match {
            case (Some(clip), Some(newId))=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                currentVideoClip = Some(clip.copy(clipId = Some(newId))),
                stringBuffer = state.stringBuffer.empty
              )
            case _=>
              state
          }
        case "InUse"=>
          state.currentVideoClip match {
            case Some(clip)=>
              val stringValue=state.stringBuffer.toString().strip()
              val boolValue = if(stringValue=="false"){
                false
              } else {
                true
              }
              state.copy(
                nodeTree = state.nodeTree.tail,
                currentVideoClip = Some(clip.copy(inUse = Some(boolValue))),
                stringBuffer = state.stringBuffer.empty
              )
            case _=>
              state
          }
        case "NextSequenceID"=>
          val parentNodeName = state.nodeTree.toList(1)
          state.project match {
            case Some(project)=>
              if(parentNodeName=="Project") {
                state.copy(
                  nodeTree = state.nodeTree.tail,
                  project = Some(
                    project.copy(
                      nextSequenceID = Some(state.stringBuffer.toString().strip())
                    )
                  ),
                  stringBuffer = state.stringBuffer.empty
                )
              } else {
                state
              }
            case _=>
              throw new ReaderException("Got a NextSequenceID outside a project")
          }
        case "RootProjectItem"|"BinProjectItem"=>
          (state.project, state.currentBin) match {
            case (Some(project), Some(currentBin))=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                project = Some(
                  project.copy(
                    contents = project.contents + (currentBin.objectUid->currentBin)
                  )
                ),
                currentBin=None
              )
            case _=>
              state
          }
        case "MasterClip"=>
          (state.project, state.currentMasterClip)match {
            case (Some(project), Some(masterClip))=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                project = Some(
                  project.copy(
                    contents = project.contents + (masterClip.objectUid->masterClip)
                  )
                ),
                currentMasterClip = None
              )
            case _=>
              state
          }

        case "ClipProjectItem"=>
          (state.project, state.currentClip) match {
            case (Some(project), Some(currentClip))=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                project = Some(
                  project.copy(
                    contents = project.contents + (currentClip.objectUid->currentClip)
                  )
                ),
              )
            case _=>
              state
          }
        case "VideoClip"|"AudioClip"=>
          (state.project, state.currentVideoClip) match {
            case (Some(project), Some(currentClip))=>
              state.copy(
                nodeTree = state.nodeTree.tail,
                project = Some(
                  project.copy(
                    integerIndexedContents = project.integerIndexedContents + (currentClip.objectUid->currentClip)
                  )
                )
              )
            case _=>
              state
          }
        case "VideoMediaSource"|"AudioMediaSource"=>
          (state.project, state.currentMediaSource) match {
            case (Some(project), Some(currentSource))=>
              println(s"Saving $currentSource")
              state.copy(
                nodeTree = state.nodeTree.tail,
                project = Some(
                  project.copy(
                    integerIndexedContents = project.integerIndexedContents + (currentSource.objectUid->currentSource)
                  )
                ),
                currentMediaSource = None
              )
            case _=>
              state
          }
        case _=>
          state.copy(nodeTree = state.nodeTree.tail, stringBuffer = state.stringBuffer.empty)
      }
    case text:TextEvent =>
      state.copy(stringBuffer = state.stringBuffer.append(text.text))
    case _=>
      state
  }

  def readFromFile(path:Path) = {
    implicit val ec:ExecutionContext = actorSystem.dispatcher

    val completionfuture = FileIO.fromPath(path)
      .via(XmlParsing.parser.async)
      .toMat(Sink.fold(ParserState.empty)((state, ev)=>handleParseEvent(ev, state)))(Keep.right)
      .run()
      .map(_.project.flatMap(assemble))

    completionfuture.onComplete({
        case Success(assembledProject)=>
          println(s"Final parser state is ${assembledProject.map(_.dumpToScreen)}")
        case Failure(err)=>
          println(s"Extraction failed: ${err}")
      })
    completionfuture
  }

  def assemble(from:Project):Option[AssembledProject] = {
    from
      .rootProjectItem
      .flatMap(itemId=>from.contents.get(itemId))
      .flatMap(itemSource=>assembleGenericProjectItem(from, itemSource))
      .map(assembledRootItem=>
        AssembledProject(
          from.version,
          from.classId,
          assembledRootItem
        )
      )
  }

  def assembleGenericProjectItem(p:Project, source:ProjectItem):Option[AssembledProjectItem] = source match {
    case b: BinProjectItem=>
      val assembledChildItems = b.items
        .flatMap(ref=>{
          p.contents.get(ref.to).map(item=>assembleGenericProjectItem(p, item))
        })
        .collect({ case Some(item)=>item })

      Some(GenericProjectItem(b.version, b.classId, assembledChildItems, b.name.getOrElse("")))
    case c: ClipProjectItem=>
      c.masterClipId.flatMap(uid=>p.contents.get(uid)) match {
        case Some(mc:MasterClip)=>
          val videoClips = mc.clips  //FIXME should sort with the 'index' parameter
            .map(ref=>p.integerIndexedContents.get(ref.to))
            .collect({case Some(clip:VideoClip)=>clip})

          val mediaSources = videoClips
            .map(clip=>clip.source.flatMap(s=>p.integerIndexedContents.get(s.to)))
            .collect({case Some(source:VideoMediaSource)=>source})

          println(s"${c.name}: clips $videoClips mediaSources $mediaSources")
          Some(AssembledClip(c.version, c.classId, c.name.getOrElse(""), mediaSources))
        case other@_=>
          println(s"master clip ${c.masterClipId} was a ${other}")
          None
      }
    case _=>
      None
  }

}
