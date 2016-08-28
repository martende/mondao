# mondao

**mondao** - Is macro based serialisator/deserealisator for mongo scala driver,
inspired by play framework Json converters and reflexive `salat` converter.

Supported types:
* `Option[_]`
* `Traversable[_]`
* `GenMapLike[String,_]` / `GenMapLike[_ <: Enumerator,_]`
* `Array[_]`
* case classes with other supported types
* `ObjectId`
* the other types can be extended by providing `mondao.Macros.Writes` / `mondao.Macros.Reads implicit`
* Tuple2 `(_,_)`

###Example:


    case class ParsedImage(
        url:String,
        oid:Option[ObjectId]
    )

    // creates mondao.Macros.Writes[ParsedImage] implicit converter

    implicit val w1 = mondao.Macros.writes[ParsedImage]
    implicit val r1 = mondao.Macros.reads[ParsedImage]

    val oid = new ObjectId()
    val o1 = scoin2.common.ParsedImage("url",Some(oid))

    // uses implicit Writes converter
    val d = mondao.Convert.toBson(o1)

    assert(d == BsonDocument("url" -> "url", "oid" -> oid))

    // uses implicit Writes converter

    val o2 = mondao.Convert.fromBson[ParsedImage](d).get
    assert(o1 == o2 )



to see more examples look at tests folder


### Limitations
* Does not support macro expansion for classes that declared in functions.
* Enumeration types are converted as their .toString conversions besides $minus that is converted to '-' symbol.

### Debugging
* To print expanded macro during compilation use the following macros:
    * `mondao.Macros.writesDebug`
    * `mondao.Macros.readsDebug`