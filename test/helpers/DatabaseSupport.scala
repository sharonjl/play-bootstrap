package helpers

import java.sql.{Connection, DriverManager}

import anorm._
import org.scalatest.BeforeAndAfterEach
import org.scalatestplus.play.PlaySpec

/**
 * Created by sharon on 14-12-30.
 */
trait DatabaseSupport extends BeforeAndAfterEach {
  base: PlaySpec with TestConfiguration =>

  val dbConfigName: String

  val driver = config.getString("db.default.driver").get
  val url = config.getString("db.default.url").get
  val username = config.getString("db.default.user").get
  val password = config.getString("db.default.password").get

  val dbName = this.toString().toLowerCase
  val postgresDbUrl = url.replace("TEST_DBNAME", "postgres")
  val testDbUrl = url.replace("TEST_DBNAME", dbName)

  /**
   * Execute a block of code, providing a JDBC connection. The connection and all created statements are
   * automatically released.
   *
   * @param block Code block to execute.
   */
  def withConnection[A](block: Connection => A): A = withConnection()(block)

  /**
   * Execute a block of code, providing a JDBC connection. The connection and all created statements are
   * automatically released.
   *
   * @param dbUrl Database url to point to
   * @param block
   * @tparam A
   * @return
   */
  def withConnection[A](dbUrl: String = testDbUrl)(block: Connection => A): A = {
    val connection = DriverManager.getConnection(dbUrl, username, password)
    try {
      block(connection)
    } finally {
      connection.close()
    }
  }


  /**
   * Execute a block of code, in the scope of a JDBC transaction.
   * The connection and all created statements are automatically released.
   * The transaction is automatically committed, unless an exception occurs.
   *
   * @param block Code block to execute.
   */
  def withTransaction[A](block: Connection => A): A = {
    withConnection { connection =>
      try {
        connection.setAutoCommit(false)
        val r = block(connection)
        connection.commit()
        r
      } catch {
        case e: Throwable => connection.rollback(); throw e
      }
    }
  }

  def dbSetUp() = {
    withConnection(postgresDbUrl) {
      implicit connection =>
        SQL(s"DROP DATABASE IF EXISTS $dbName; CREATE DATABASE $dbName").execute()
    }

    withTransaction {
      implicit connection =>
        val evolution_seq = play.api.db.evolutions.Evolutions.applicationEvolutions(new java.io.File("."), this.getClass.getClassLoader, dbConfigName)
        evolution_seq.reverse.flatMap(e => EvolutionSqlScript(e.sql_up).statements).foreach {
          sql =>
            SQL(sql).execute()
        }
    }
  }

  def dbTearDown() = {
    withTransaction {
      implicit connection =>
        val evolution_seq = play.api.db.evolutions.Evolutions.applicationEvolutions(new java.io.File("."), this.getClass.getClassLoader, dbConfigName)
        evolution_seq.reverse.flatMap(e => EvolutionSqlScript(e.sql_down).statements).foreach {
          sql => SQL(sql).execute()
        }
    }
  }

  override protected def beforeEach(): Unit = {
    dbSetUp()
    super.beforeEach()
  }

  override protected def afterEach(): Unit = {
    super.afterEach()
    dbTearDown()
  }

  /*
   * The following class was taken from the Play framework however they listed theirs as private.
   * @href https://github.com/playframework/playframework/blob/master/framework/src/play-jdbc/src/main/scala/play/api/db/evolutions/Evolutions.scala
   */
  private case class EvolutionSqlScript(sql: String) {
    def statements: Seq[String] = {
      // Regex matches on semicolons that neither precede nor follow other semicolons
      sql.split("(?<!;);(?!;)").map(_.trim.replace(";;", ";")).filter(_ != "")
    }
  }

}
