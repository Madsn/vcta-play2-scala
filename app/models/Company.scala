package models

import java.util.{Date}

import play.api.db._
import play.api.Play.current

import anorm._
import anorm.SqlParser._

import scala.language.postfixOps

case class Company(id: Pk[Long] = NotAssigned, name: String)

/**
 * Helper for pagination.
 */
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

object Company {
    
  /**
   * Parse a Company from a ResultSet
   */
  val simple = {
    get[Pk[Long]]("company.id") ~
    get[String]("company.name") map {
      case id~name => Company(id, name)
    }
  }
  
  val parseComp = Company.simple ~ (Company.simple ?) map {
    case company => company
  }
  
  /*
   * Retrieve a Company from ID
   */
  def findById(id: Long): Option[Company] = {
    DB.withConnection { implicit connection =>
      SQL("select * from company where id = {id}").on('id -> id).as(Company.simple.singleOpt)
    }
  }
  
  def list(page: Int = 0, pageSize: Int = 10, orderBy: Int = 1, filter: String = "%"): Page[Company] = {
    val offset = pageSize * page
    DB.withConnection { implicit connection =>
      val companies = SQL(
          """
          select * from company
          where company.name like {filter}
          order by {orderBy} nulls last limit {pageSize} offset {offset}
          """
          ).on(
          'pageSize -> pageSize,
          'offset -> offset,
          'filter -> filter,
          'orderBy -> orderBy
          ).as(Company.parseComp *)
    
          val totalRows = SQL(
          """
          select count(*) from Company
          where company.name like {filter}
          """
          ).on(
          'filter -> filter
          ).as(scalar[Long].single)
              
          Page(companies, page, offset, totalRows)
    }
  }
  
}

