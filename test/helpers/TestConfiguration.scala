package helpers

import java.io.File

import com.typesafe.config.ConfigFactory
import play.api.Configuration

/**
 * Reads test configuration file
 */
trait TestConfiguration {

  lazy val config = Configuration(ConfigFactory.parseFile(new File("conf/application.test.conf")).resolve())
}
