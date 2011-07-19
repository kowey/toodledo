object hsgtdToToodleDo {
    import scala._
    import scala.xml._
    
    def readCacheFile(x:String) = MessageCache.read(XML.loadFile(x)).toMap

    def main(args:Array[String]) = {
        if (args.length < 1) {
            Console.err.println("Need: hsgtd todo file")
            sys.exit(1)
        }
        val f  = args(0)
        val cf = if (args.length < 2) None else Some(args(1))
        val cache = cf map readCacheFile getOrElse Nil.toMap

        val xs=scala.io.Source.fromFile(f).getLines.toList
        val tasks=xs map Task.fromHsGtd filterNot (_.isDead)
        Console.println(Task.toToodleDo(tasks, cache))
    }
}

// vim: set ts=4 sw=4 et:
