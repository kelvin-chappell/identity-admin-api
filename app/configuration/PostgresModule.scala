package configuration

import javax.sql.DataSource

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import scalikejdbc.{ConnectionPool, DataSourceConnectionPool}
import Config.Postgres
import play.api.{Configuration, Environment}
import play.api.inject.Module

class PostgresModule extends Module {
  override def bindings(environment: Environment, configuration: Configuration) = {
    val dataSource: DataSource = {
      val config = new HikariConfig()
      config.setConnectionTestQuery("SELECT 1")
      config.setJdbcUrl(Postgres.jdbcUrl)
      config.setUsername(Postgres.username)
      config.setPassword(Postgres.password)
      config.setMaximumPoolSize(Postgres.poolSize)
      new HikariDataSource(config)
    }
    ConnectionPool.singleton(new DataSourceConnectionPool(dataSource))
    Nil
  }
}
