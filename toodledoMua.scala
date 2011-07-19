import scala._
import scala.xml._

import java.io.Console._
import java.io.File
import java.util.List._
import java.util.Scanner._

import org.loststone.toodledo._
import org.loststone.toodledo.ToodledoApi._
import org.loststone.toodledo.ToodledoApiImpl._
import org.loststone.toodledo.data._
import org.loststone.toodledo.exception._
import org.loststone.toodledo.util._
import scalaj.collection.Imports._
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

case class Client(email:String, pw:String, token:AuthToken, cached:Boolean) {
    val tdApi = Client.tdApi

    def mkTodo(task:Task):Todo = {
        var t = new Todo
        val convert = task.toodledo
        t.setTitle(convert.title)
        //t.setContext(convert.title)
        t
    }
        
    def perhapsRetry[A](f:Client => A):A = {
        if (cached) {
            val client2 = Client.connect(email, pw) getOrElse sys.exit(1)
            f(client2)
        } else { 
            Console.out.println("Retry with new token failed")
            sys.exit(1)
        }
    }

    def withRetry[A](f:Client => A):A = {
        try {
			Option(f(this)) match {
                case None    => perhapsRetry(_.withRetry(f))
                case Some(x) => x
            }
		} catch {
            case e => e.printStackTrace
            sys.exit(1)
		}
    }

    def withRetryInt[A](f:Client => Int):Int = {
        try {
            val ret = f(this)
			if (f(this) == -1)
               perhapsRetry(_.withRetryInt(f))
            else
               ret
		} catch {
            case e => e.printStackTrace
            sys.exit(1)
		}
    }

	def getTodos:Seq[Todo]       = {
        Console.err.println("Fetching todos")
        withRetry(c => tdApi.getTodosList(c.token)).asScala
    }

    def getContexts:Seq[Context] = {
        Console.err.println("Fetching contexts")
        withRetry(c => tdApi.getContexts(c.token)).asScala
    }

	def getGoals:Seq[Goal]       = {
        Console.err.println("Fetching goals")
        withRetry(c => tdApi.getGoals(c.token)).asScala
    }

	def addContext(n:String):Context = {
        Console.err.println("Adding context: " + n)
        var x = new Context
        x setName n 
        val i = withRetryInt(c => tdApi.addContext(c.token,x))
        x setId i
        x
    }

    // TODO: refactor
	def addGoal(n:String):Goal = {
        Console.err.println("Adding goal: " + n)
        var x = new Goal
        x setName n 
        val i = withRetryInt(c => tdApi.addGoal(c.token,x))
        x setId i
        x
    }

    // TODO: refactor
	def addTodo(cache:ToodledoCache, t:Task, xnote:Seq[String]):Todo = {
        Console.err.println("Adding task: " + t.txt)
        var x = new Todo
        val tt = t.toodledo
        val ctx  = (cache.contexts find (_.getName == tt.context))
        val goal = (cache.goals    find (_.getName == tt.project))
        val note = tt.notes ++ xnote
        val stat = t.status match {
            case NextAction => Status.NEXT_ACTION 
            case Waiting    => Status.WAITING
            case Someday    => Status.SOMEDAY
        }
        x setTitle   java.net.URLEncoder.encode(tt.title, "UTF8")
        x setStatus  stat
        ctx  map (x setContext _.getId)
        goal map (x setGoal    _.getId)
        if (tt.tag != "") (x setTag tt.tag)
        x setNote    java.net.URLEncoder.encode(note mkString "\n", "UTF8")
        val i = withRetryInt(c => tdApi.addTodo(c.token, x))
        x setId i
        x
    }
}

object Client {
    val tdApi:ToodledoApi = new ToodledoApiImpl
    val tokenCacheFile = new File(System.getProperty("user.home"),".toodledotoken")

    def readTokenCache(password:String):Option[AuthToken] = {
        if (tokenCacheFile.exists) {
            val t = scala.io.Source.fromFile(tokenCacheFile).mkString
            val now = new DateTime
            val tparser = ISODateTimeFormat.dateTimeParser
            (t split "\\s").toList match {
                case List(u,t,d) if (now isBefore tparser.parseDateTime(d)) => Some(new AuthToken(password, u, t))
                case _ => None
            }
        }
        else 
            None
    }
    
    def writeTokenCache(userid:String, t:AuthToken) {
        val p = new java.io.PrintWriter(tokenCacheFile)
        val fields = List(userid, t.getToken, t.getDate)
        p.println(fields mkString " ")
        p.flush()
    }

    def createCached(email:String, pw:String, t:AuthToken) = {
        Console.err.println("Creating client using cached token " + t.getToken)
        Client(email, pw, t, true)
    }


	def connect(email:String, pw:String):Option[Client] =
		try {
            Console.err.println("Seeking new token for " + email)
			val userid = tdApi.getUserId(email, pw)
			val token = tdApi.initialize(userid, pw)
            writeTokenCache(userid, token)
            Some(Client(email, pw, token, false))
		} catch {
            case _:ToodledoApiException => sys.error("Could not connect to http://www.toodledo.com")
            case _:IncorrectUserPasswordException => {
                Console.err.println("Wrong user name or password")
                None
            }
		    case _:MissingPasswordException => {
                Console.err.println("No password supplied")
                None
            }
		}
}

object QueuedTask {
    val queueDir = new File(System.getProperty("user.home"),".toodledoq")
    val fs = queueDir.listFiles

    def readQueue = {
        fs map readTask
    }

    def readTask(f:File) = {
        val xs=scala.io.Source.fromFile(f).getLines.toList
        val task=Task.fromHsGtd(xs.head)
        val body=xs.tail
        (task, body)
    }
    
    def removeEntries = {
        fs map (_.delete)
    }
}

case class ToodledoCache(contexts:Seq[Context], goals:Seq[Goal])

object ToodledoCache {
    val cacheFile = new File(System.getProperty("user.home"),".toodledocache")


    def readCache = {
        if (cacheFile.exists) {
            val n = XML.loadFile(cacheFile)
            val cs = (n \\ "contexts" \\ "context") flatMap (readContext(_))
            val gs = (n \\ "goals"    \\ "goal")    flatMap (readGoal(_))
            Some(ToodledoCache(cs, gs))
        }
        else
            None
    }

    def writeCache(cache:ToodledoCache) = {
        val out =
<cache>
    <contexts>
    { cache.contexts map (x => <context><id>{x.getId}</id><name>{x.getName}</name></context>) }
    </contexts>
    <goals>
    { cache.goals map (x => <goal><id>{x.getId}</id><name>{x.getName}</name></goal>) }
    </goals>
</cache>
        XML.save(cacheFile.toString, out)
    }

    // TODO: kill boilerplate
    def readContext(x:Node) = x match {
        case <context><id>{i}</id><name>{t}</name></context> => {
            val c = new Context()
            c.setId(i.text.toInt)
            c.setName(t.text)
            Some(c)
        }
        case _  => None
    }

    def readGoal(x:Node) = x match {
        case <goal><id>{i}</id><name>{t}</name></goal> => {
            val g = new Goal()
            g.setId(i.text.toInt)
            g.setName(t.text)
            Some(g)
        }
        case _  => None
    }
}


object toodledoMua {
    def main(args:Array[String]) = {
        if (args.length != 2) {
            Console.err.println("Need: name password")
            sys.exit(1)
        }
        val email = args(0)
        val pw    = args(1)

        val client = Client.readTokenCache(pw) match {
            case Some(t) => Client.createCached(email, pw, t)
            case None    => Client.connect(email, pw) getOrElse sys.exit(1)
        }

        def fetchCache = {
            val ctxs  = client.getContexts
            val goals = client.getGoals
            val cache = ToodledoCache(ctxs, goals)
            ToodledoCache.writeCache(cache)
            cache
        }

        def readCache = ToodledoCache.readCache match {
            case Some(x) => x
            case None    => fetchCache
        }
       
        def checkCache(cs:Seq[String], gs:Seq[String])(cache:ToodledoCache) = {
            val missingC = cs filterNot (c => cache.contexts exists (_.getName == c))
            val missingG = gs filterNot (g => cache.goals    exists (_.getName == g))
            (missingC, missingG)
        }
 
        def updateCache(cs:Seq[String], gs:Seq[String]) = {
            val cache1 = readCache
            val (missingC1, missingG1) = checkCache(cs,gs)(cache1)
            if (missingC1.isEmpty && missingG1.isEmpty) cache1
            else {
                val cache2 = fetchCache
                val (missingC2, missingG2) = checkCache(cs,gs)(cache2)
                if (missingC1.isEmpty && missingG2.isEmpty) cache2
                else {
                    val newCs = missingC2 map (client addContext _)
                    val newGs = missingG2 map (client addGoal _)
                    ToodledoCache(cache2.contexts ++ newCs, cache2.goals ++ newGs)
                }
            }
        }
    
        val queue = QueuedTask.readQueue
        val cs = queue flatMap (_._1.contexts take 1)
        val gs = queue flatMap (_._1.projects take 1)
        val cache = updateCache(cs, gs)
        for ((t,b) <- queue) yield client.addTodo(cache, t, b)
        QueuedTask.removeEntries
    }
}
// vim: set ts=4 sw=4 et:
