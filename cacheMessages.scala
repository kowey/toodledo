import scala.sys.process._
import scala.xml._
import java.lang.String._

object MessageCache {
    def read(x:Node) = x match {
       case <cache>{xs @ _*}</cache> => xs flatMap (readOne(_))
       case _                        => sys.error("Not a message cache")
    }

    def readOne(x:Node) = x match {
        case <message><id>{i}</id><txt>{t}</txt></message> => Some (i.text, t.text)
        case _  => None
    }

}

object cacheMessages {
    import scala._

    def main(args:Array[String]) = {
        if (args.length != 1) {
            Console.err.println("Need: hsgtd todo file")
            sys.exit(1)
        }

        val f=args(0)
        val xs=scala.io.Source.fromFile(f).getLines.toList
        val tasks=xs map Task.fromHsGtd filterNot (_.isDead)
        val msgs=tasks flatMap (_.messageIds)
        
        def notmuch(i:String) = Process.apply("notmuch", List("show", "--format=mbox", "id:" + i))
        val simplify = "formail -k -X From: -X Date -X Subject"

//m replaceAll ()
        val results = msgs map (notmuch(_) #| "ripmime-pipe" !!)
        val out=
<cache>
{
        for ((i,t) <- msgs zip results) yield <message><id>{i}</id><txt>{t}</txt></message>
}
</cache>
        XML.save(f + ".msg-cache", out, "UTF-8")
    }
}

// vim: set ts=4 sw=4 et:
