package scanimation

import java.util.UUID

import com.github.simplyscala.{MongoEmbedDatabase, MongodProps}
import com.typesafe.scalalogging.LazyLogging
import scanimation.format._
import scanimation.mongo._
import org.mongodb.scala.bson.Document
import org.mongodb.scala.{MongoClient, Observable, SingleObservable}

class MongoSpec extends Spec with MongoEmbedDatabase with LazyLogging {
  private val recompile = 2
  private var process: MongodProps = _
  private var client: MongoClient = _

  case class Person(name: String, age: Int)

  implicit val personFormat: MF[Person] = format2(Person)
  val john42 = Person("John", 42)
  val jeremy34 = Person("Jeremy", 34)
  val mary42 = Person("Mary", 42)
  val data: List[Person] = john42 :: jeremy34 :: Nil

  "mongo" can {
    "write and read person" in {
      check(john42)
    }

    "write and read lists and options" in {
      case class Collections(strings: List[String], people: List[Person], intOpt: Option[Int], personOpt: Option[Person])
      implicit val collectionsFormat: MF[Collections] = format4(Collections)
      check(Collections(
        strings = "foo" :: "bar" :: "baz" :: Nil,
        people = data,
        intOpt = Some(42),
        personOpt = Some(Person("Mary", 75))
      ))
      check(Collections(
        strings = Nil,
        people = Nil,
        intOpt = None,
        personOpt = None
      ))
    }

    "search for person by equals" in {
      search(
        data = data,
        query = $(Person)(_.name $eq "John")
      ) shouldBe (john42 :: Nil)

      search(
        data = john42 :: mary42 :: jeremy34 :: Nil,
        query = $(Person)(_.age $eq 42),
        sort = $(Person)(_.name $asc, _.age $asc)
      ) shouldBe (john42 :: mary42 :: Nil)

      search(
        data = data,
        query = $(Person)(_.name $neq "John")
      ) shouldBe (jeremy34 :: Nil)
    }

    "search for person by various filters" in {
      search(
        data = data,
        query = $(Person)(_.age $gt 42)
      ) shouldBe Nil

      search(
        data = data,
        query = $(Person)(_.age $gt 34)
      ) shouldBe (john42 :: Nil)

      search(
        data = data,
        query = $(Person)(_.age $gt 10),
        sort = $(Person)(_.age $asc)
      ) shouldBe (jeremy34 :: john42 :: Nil)

      search(
        data = data,
        query = $(Person)(_.age $lt 34)
      ) shouldBe Nil

      search(
        data = data,
        query = $(Person)(_.age $lt 42)
      ) shouldBe (jeremy34 :: Nil)

      search(
        data = data,
        query = $(Person)(_.age $lt 50),
        sort = $(Person)(_.age $desc)
      ) shouldBe data

      search(
        data = data,
        query = $(Person)(_.age $in List(10, 34)),
        sort = $(Person)(_.age $desc)
      ) shouldBe (jeremy34 :: Nil)

      search(
        data = data,
        query = $(Person)(_.age $in List(42, 34)),
        sort = $(Person)(_.age $desc)
      ) shouldBe data
    }

    "search person holder by embedded field" in {
      case class PersonHolder(person: Person)
      implicit val personHolderFormat: MF[PersonHolder] = format1(PersonHolder)
      search(
        data = PersonHolder(john42) :: Nil,
        query = $(PersonHolder)(_.person.name $eq "John")
      ) shouldBe (PersonHolder(john42) :: Nil)
    }

    "search using typed case class" in {
      val typed = $$(Person)
      search(
        data = data,
        query = typed(_.name $eq "John")
      ) shouldBe (john42 :: Nil)
    }

    "search for list elements" in {
      case class PeopleHolder(people: List[Person])
      implicit val peopleHolderFormat: MF[PeopleHolder] = format1(PeopleHolder)
      search(
        data = PeopleHolder(john42 :: jeremy34 :: Nil) :: PeopleHolder(mary42 :: jeremy34 :: Nil) :: Nil,
        query = $(PeopleHolder)(_.people.anyElement.name $eq "John")
      ) shouldBe (PeopleHolder(john42 :: jeremy34 :: Nil) :: Nil)

      search(
        data = PeopleHolder(john42 :: jeremy34 :: Nil) :: PeopleHolder(mary42 :: jeremy34 :: Nil) :: Nil,
        query = $(PeopleHolder)(_.people.someElement.name $eq "John")
      ) shouldBe (PeopleHolder(john42 :: jeremy34 :: Nil) :: Nil)
    }
  }

  def search[A <: AnyRef](data: List[A], query: Document, sort: Document = Document())(implicit format: MF[A]): List[A] = {
    val collectionName = UUID.randomUUID().toString
    val db = client.getDatabase("test")
    db.createCollection(collectionName).await
    val collection = db.getCollection(collectionName)
    collection.insertMany(data.map(a => a.toBson)).await
    collection.find(query).sort(sort).await.map(a => a.toScala[A])
  }

  def check[A <: AnyRef](a: A)(implicit format: MF[A]): Unit = {
    val collectionName = UUID.randomUUID().toString
    val db = client.getDatabase("test")
    db.createCollection(collectionName).await
    val collection = db.getCollection(collectionName)
    collection.insertOne(a.toBson).await
    collection.find().await.head.toScala[A] shouldBe a
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    logger.info("starting test mongo instance")
    process = eventually(mongoStart())
    client = MongoClient("mongodb://127.0.0.1:12345/")
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    logger.info("stopping test mongo instance")
    Option(process).foreach(p => mongoStop(p))
    Option(client).foreach(c => c.close())
    logger.info("stopped test mongo instance")
  }

  implicit class SingleObservableOps[A](obs: SingleObservable[A]) {
    def await: A = obs.toFuture().futureValue
  }

  implicit class ObservableOps[A](obs: Observable[A]) {
    def await: List[A] = obs.toFuture().futureValue.toList
  }

}