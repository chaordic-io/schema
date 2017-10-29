package io.chaordic.db

import java.util.Properties

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import org.flywaydb.core.Flyway

object TestDataSource {
  val props = new Properties()
  props.load(this.getClass.getResourceAsStream("/db.properties"))
  private val config = new HikariConfig(props)
  val dataSource = new HikariDataSource(config)


  DatabaseMigrations()

}

object DatabaseMigrations{

  def apply(): Unit = {
    val flyway = new Flyway()
    // Point it to the database
    flyway.setDataSource(TestDataSource.dataSource)
    flyway.setLocations("/db/migrations")
    // Start the migration
    flyway.migrate()
    println("migrated")
  }

}