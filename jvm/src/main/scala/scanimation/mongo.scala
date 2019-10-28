package scanimation

import akka.actor.Scheduler
import scanimation.common._
import scanimation.format._
import scanimation.util.actors.SystemStatus
import scanimation.util.akkautil._
import org.mongodb.scala.bson._
import org.mongodb.scala.{Document, MongoCollection, MongoDatabase}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object mongo {

  type MongoFormat[A] = AbstractFormat[A, BsonValue]
  type MF[A] = MongoFormat[A]

  implicit class DocumentOps(val document: Document) extends AnyVal {
    /** Converts bson document to scala object */
    def toScala[A: MF](implicit format: MF[A]): A = format.read(Nil, document.toBsonDocument)._1
  }

  implicit class BsonValueOps(val bson: BsonValue) extends AnyVal {
    /** Converts bson value to scala object */
    def toScala[A: MF](implicit format: MF[A]): A = format.read(Nil, bson)._1
  }

  implicit class MongoAnyRefOps[A <: AnyRef](val any: A) extends AnyVal {
    /** Converts scala value to bson document */
    def toBson(implicit format: MF[A]): Document = Document(any.format().asDocument())
  }

  /** Provides the unit for mongo format */
  implicit val mongoType: FormatType[BsonValue] = new FormatType[BsonValue] {
    override def unit: BsonValue = new BsonDocument()
  }

  /** Reads and writes bsons */
  implicit val bsonValueFormat: MF[BsonValue] = new MF[BsonValue] {
    override def read(path: Path, formatted: BsonValue): (BsonValue, BsonValue) = {
      readPath(path, formatted).getOrElse(throw new IllegalArgumentException(s"missing field: ${path.stringify}")) -> formatted
    }

    override def append(path: Path, a: BsonValue, formatted: BsonValue): BsonValue = {
      writePath(path, formatted, a)
      formatted
    }
  }

  /** Reads and writes strings */
  implicit val stringFormat: MF[String] = bsonValueFormat.map(
    {
      case string: BsonString => string.getValue
      case other => throw new IllegalArgumentException(s"wrong string type: $other")
    },
    string => new BsonString(string)
  )

  /** Reads and writes booleans */
  implicit val booleanFormat: MF[Boolean] = bsonValueFormat.map(
    {
      case boolean: BsonBoolean => boolean.getValue
      case other => throw new IllegalArgumentException(s"wrong boolean type: $other")
    },
    ooolean => new BsonBoolean(ooolean)
  )

  /** Reads and writes ints */
  implicit val intFormat: MF[Int] = bsonValueFormat.map(
    {
      case int: BsonInt32 => int.getValue
      case other => throw new IllegalArgumentException(s"wrong int type: $other")
    },
    int => new BsonInt32(int)
  )

  /** Reads and writes longs */
  implicit val longFormat: MF[Long] = bsonValueFormat.map(
    {
      case long: BsonInt64 => long.getValue
      case other => throw new IllegalArgumentException(s"wrong long type: $other")
    },
    long => new BsonInt64(long)
  )

  /** Reads lists of A */
  implicit def implicitListFormat[A: MF]: MF[List[A]] = new MF[List[A]] {
    override def read(path: Path, formatted: BsonValue): (List[A], BsonValue) = {
      readPath(path, formatted) match {
        case None =>
          Nil -> formatted
        case Some(array: BsonArray) =>
          array.getValues.asScala.toList.map(element => element.toScala[A]) -> formatted
        case Some(other) =>
          throw new IllegalArgumentException(s"wrong list format: $other")
      }
    }

    override def append(path: Path, a: List[A], formatted: BsonValue): BsonValue = {
      a.zipWithIndex.foreach { case (element, index) =>
        element.format(path :+ ArrayPathSegment(index), formatted)
      }
      formatted
    }
  }

  /** Reads optional A */
  implicit def implicitOptionFormat[A: MF]: MF[Option[A]] = implicitListFormat[A].map(list => list.headOption, option => option.toList)

  /** Reads the bson value at given path */
  private def readPath(path: Path, bson: BsonValue): Option[BsonValue] = path match {
    case Nil =>
      Some(bson)

    case FieldPathSegment(field) :: tail =>
      Option(bson.asDocument().get(field)).flatMap(b => readPath(tail, b))

    case ArrayPathSegment(index) :: tail =>
      val array = bson.asArray()
      if (index >= 0 && index < array.size()) readPath(tail, array.get(index))
      else None
  }

  /** Writes the bson value at given path */
  private def writePath(path: Path, bson: BsonValue, value: BsonValue): Unit = path match {
    case Nil =>
      throw new IllegalArgumentException(s"cannot write value at empty path: $bson")

    case FieldPathSegment(field) :: Nil =>
      bson.asDocument().put(field, value)

    case FieldPathSegment(field) :: tail =>
      Option(bson.asDocument().get(field)) match {
        case Some(sub) =>
          writePath(tail, sub, value)
        case None =>
          val sub = tail.head match {
            case _: FieldPathSegment => new BsonDocument()
            case _: ArrayPathSegment => new BsonArray()
          }
          bson.asDocument().put(field, sub)
          writePath(tail, sub, value)
      }

    case ArrayPathSegment(index) :: Nil =>
      bson.asArray().add(value)

    case ArrayPathSegment(index) :: tail =>
      if (index >= 0 && index < bson.asArray().size()) {
        writePath(tail, bson.asArray().get(index), value)
      } else {
        val sub = tail.head match {
          case _: FieldPathSegment => new BsonDocument()
          case _: ArrayPathSegment => new BsonArray()
        }
        bson.asArray().add(sub)
        writePath(tail, sub, value)
      }
  }

  /** Converts operations into mongo document */
  implicit val mongoOperationType: OperationType[Document] = { operations =>
    val bson = new BsonDocument()
    operations.foreach {
      case BinaryOperation(path, Operations.EqualTo, value) =>
        writePrimitive(mergePath(path), bson, value)
      case BinaryOperation(path, operation, value) =>
        val operator = operation match {
          case Operations.NotEqualTo => "$ne"
          case Operations.GreaterThan => "$gt"
          case Operations.GreaterThanOrEqualTo => "$gte"
          case Operations.LessThan => "$lt"
          case Operations.LessThanOrEqualTo => "$lte"
          case Operations.In => "$in"
          case Operations.Exists => "$exists"
          case other => sys.error(s"unknown operation: $other")
        }
        writePrimitive(mergePath(path) :+ FieldPathSegment(operator), bson, value)
      case UnaryOperation(path, Operations.SortAsc) =>
        writePrimitive(mergePath(path), bson, 1)
      case UnaryOperation(path, Operations.SortDesc) =>
        writePrimitive(mergePath(path), bson, -1)
    }
    Document(bson)
  }

  /** Writes the scala primitive at given path */
  private def writePrimitive(path: Path, bson: BsonValue, value: Any): Unit = value match {
    case v: String =>
      stringFormat.append(path, v, bson)
    case v: Int =>
      intFormat.append(path, v, bson)
    case v: Long =>
      longFormat.append(path, v, bson)
    case v: Boolean =>
      booleanFormat.append(path, v, bson)
    case vs: List[_] =>
      vs.zipWithIndex.foreach { case (v, index) => writePrimitive(path :+ ArrayPathSegment(index), bson, v) }
  }

  /** Merges path list into one field */
  private def mergePath(path: Path): Path = {
    val total = path.foldLeft[List[List[PathSegment]]](Nil) {
      case (head :: tail, s: FieldPathSegment) =>
        (head :+ s) :: tail
      case (Nil, s: FieldPathSegment) =>
        (s :: Nil) :: Nil

      case (parts, ArrayPathSegment(-1)) =>
        parts

      case (parts, ArrayPathSegment(-2)) =>
        Nil :: (FieldPathSegment("$elemMatch") :: Nil) :: parts

      case (head :: tail, s: ArrayPathSegment) =>
        (head :+ s) :: tail
      case (Nil, s: ArrayPathSegment) =>
        (s :: Nil) :: Nil
    }
    val merged = total.map { subpath =>
      val string = subpath
        .flatMap {
          case FieldPathSegment(field) => Some(field)
          case ArrayPathSegment(-1) => None
          case ArrayPathSegment(index) => Some(index.toString)
        }
        .mkString(".")
      FieldPathSegment(string)
    }
    merged.reverse
  }

  implicit class MongoDatabaseOps(val db: MongoDatabase) extends AnyVal {
    /** Ensures that collection with given name exists and wraps it into typed collection */
    def ensureCollection[A <: AnyRef](tpe: CaseClassType[A], name: String)(implicit format: MF[A], ec: ExecutionContext, s: Scheduler): TypedMongoCollection[A] = {
      val collection = for {
        names <- retryFuture(() => db.listCollectionNames().toFuture, attempts = 5, delay = 5.seconds)
        _ <- if (names.contains(name)) {
          UnitFuture
        } else {
          db.createCollection(name).toFuture
        }
      } yield db.getCollection[Document](name)
      new TypedMongoCollection[A](collection, tpe)
    }
  }

  /** Types the collection using formatter */
  class TypedMongoCollection[A <: AnyRef](delegate: Future[MongoCollection[Document]], cct: CaseClassType[A])(implicit format: MF[A], ec: ExecutionContext) {
    type CCT = CaseClassType[A]
    private val empty: CCT => Document = cct => Document()

    /** Ensures that given indices exist */
    def ensureIndices(code: CCT => List[Document]): Future[Unit] = for {
      collection <- delegate
      indices = code.apply(cct)
      futures = indices.map(index => collection.createIndex(index).toFuture)
      _ <- Future.sequence(futures)
    } yield ()

    /** Counts document in collection that match given query */
    def countDocuments(query: CCT => Document = empty): Future[Long] = for {
      collection <- delegate
      count <- collection.countDocuments(query.apply(cct)).toFuture
    } yield count

    /** Finds documents that match given query */
    def find(query: CCT => Document = empty, sort: CCT => Document = empty, limit: Int = -1): Future[List[A]] = for {
      collection <- delegate
      sortDocument = sort.apply(cct)
      documents <- collection
        .find(query.apply(cct))
        .chainIf(limit >= 0)(s => s.limit(limit))
        .chainIf(sortDocument.nonEmpty)(s => s.sort(sortDocument))
        .toFuture
      entities = documents.map(d => d.toScala[A]).toList
    } yield entities

    /** Finds single document that match given query */
    def findOne(query: CCT => Document = empty, sort: CCT => Document = empty): Future[Option[A]] = for {
      documents <- find(query, sort = sort, limit = 1)
    } yield documents.headOption

    /** Inserts given entity into the collection */
    def insertOne(entity: A): Future[Unit] = for {
      collection <- delegate
      _ <- collection.insertOne(entity.toBson).toFuture
    } yield ()

    /** Replaces single document with given body */
    def replaceOne(query: CCT => Document, replacement: A): Future[Unit] = for {
      collection <- delegate
      _ <- collection.replaceOne(query.apply(cct), replacement.toBson).toFuture
    } yield ()

    /** Deletes multiple documents from the collection */
    def deleteMany(query: CCT => Document = empty): Future[Unit] = for {
      collection <- delegate
      _ <- collection.deleteMany(query.apply(cct)).toFuture
    } yield ()

    /** Deletes single document from the collection */
    def deleteOne(query: CCT => Document = empty): Future[Unit] = for {
      collection <- delegate
      _ <- collection.deleteOne(query.apply(cct)).toFuture
    } yield ()

    /** Tests if collection is healthy */
    def status(prefix: String): Future[SystemStatus] = for {
      collection <- delegate
      status <- findOne().transform {
        case Success(any) => Success(SystemStatus(s"$prefix.mongo.${collection.namespace.getCollectionName}", healthy = true))
        case Failure(up) => Success(SystemStatus(s"$prefix.mongo.${collection.namespace.getCollectionName}", healthy = false, error = Some(up.getMessage)))
      }
    } yield status

  }

}