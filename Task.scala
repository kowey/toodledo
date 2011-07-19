sealed abstract class Status
case object NextAction extends Status
case object Waiting    extends Status
case object Someday    extends Status

case class Task ( txt:String
                , status:Status
                , contexts:List[String] 
                , projects:List[String]
                , tags:List[String]
                , dueDate:Option[String]
                , messageIds:List[String]
                ) 
{
  def isDead = (contexts contains "done") || (contexts contains "removed")

  object toodledo {
    val title   = txt
    val stat    = status match {
                    case NextAction => "Next Action"
                    case Waiting    => "Waiting"
                    case Someday    => "Someday"
                  }
    val context = contexts.headOption getOrElse ""
    val project = projects.headOption getOrElse ""
    val tag     = tags mkString ","
    val duedate = dueDate getOrElse ""
    val notes = (contexts drop 1 map ("OtherContext: " + _)) ++
                (projects drop 1 map ("OtherProject: " + _))

    def toXml(id:Int, msgCache:Map[String,String]) = {
        val messages = messageIds map (msgCache get _)
        val msgStrs  = for ((i,m) <- messageIds zip messages)
                           yield ("\n === message " + i + " ===\n" + (m getOrElse ""))
        val fullNotes = (notes ++ msgStrs) mkString "\n"

<item>
 <id>{id}</id>
 <parent>0</parent>
 <title>{title}</title>
 <tag>{tag}</tag>
 <context>{context}</context>
 <goal>{project}</goal>
 <status>{stat}</status>
 <duedate>{duedate}</duedate>
 <note>{fullNotes}</note>
</item>
    }
  }
}


object Task {
    def fromHsGtd(x:String) = {
        def isWaiting(w:String)   = w == ":WF"
        def isTag(w:String)       = w startsWith "::"
        def isMessageId(w:String) = (w startsWith ":<") && (w endsWith ">")
        val projectLike           = List(isMessageId(_), isTag(_), isWaiting(_))
        def isProject(w:String)   = (w startsWith ":") && (! (projectLike exists (f => f(w))))

        val somedayCtxs = List("zzz_someday", "someday")
        def isContext(w:String) = w startsWith "@"
        def isDueDate(w:String) = w startsWith "!"
        val special     = projectLike ++ List(isContext(_), isProject(_), isDueDate(_))
        def isSpecial(w:String) = special exists (f => f(w))

        val words = (x split "\\s").toList

        val descr = words filterNot (isSpecial(_)) mkString " "
        val rawCtxs = for (w <- words if isContext(w))   yield w replaceAll ("^@","")
        val ctxs    = rawCtxs diff somedayCtxs
        val stat  = if      (words exists (isWaiting(_)))             Waiting
                    else if (rawCtxs exists (somedayCtxs contains _)) Someday
                    else                                              NextAction
        val projs = for (w <- words if isProject(w))   yield w replaceAll ("^:","")
        val tags  = for (w <- words if isTag(w))       yield w replaceAll ("^::","")
        val msgs  = for (w <- words if isMessageId(w)) yield w replaceAll ("^:<","") replaceAll (">$","")
        val date  = words filter (isDueDate(_)) match {
                      case Nil     => None
                      case List(d) => Some(d replaceAll ("^!",""))
                      case _       => sys.error("Don't know how to deal with multi-date tasks " + x)
                    }

        Task(descr, stat, ctxs, projs, tags, date, msgs)
    }

   def toToodleDo(tasks:List[Task], cache:Map[String,String]) =
<xml>
    <title>HsGTD to ToodleDo export</title>
    <link>http://darcsden.com/kowey/toodledo</link>
    <toodledoversion>6</toodledoversion>
    <description/>
{tasks.zipWithIndex map {ti => ti._1.toodledo.toXml(ti._2,cache)}}
</xml>

}

// vim: set ts=4 sw=4 et:
