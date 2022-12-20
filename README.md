# data-mapping

![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/Greenfossil/data-mapping/run-tests.yml?branch=master)
![](https://img.shields.io/github/license/Greenfossil/data-mapping)
![](https://img.shields.io/github/v/tag/Greenfossil/data-mapping)

Official Greenfossil Scala library to handle form submission.

## How to Build

This library uses sbt as its build tool. It requires at least Java 17 or later to build.

Follow the official guide on how to install [sbt](https://www.scala-sbt.org/download.html).

## Getting Started

### Import

To use data-mapping, import the following package:
```scala
import com.greenfossil.data.mapping.Mapping.*
import com.greenfossil.data.mapping.Mapping
```

### Types of mapping

Single field mappings can be declared like below:

```scala
val textMapping = Mapping("text", text)
val nonEmptyTextMapping = Mapping("text", nonEmptyText)
val boolMapping = Mapping("bool", boolean)
val intMapping = Mapping("int", number)
val localDateMapping = Mapping("date", localDateUsing("yyyy-MM-dd"))
```

Mappings can also contain multiple fields:

```scala
val tupleMapping = tuple(
  "text" -> text,
  "date" -> localDateUsing("yyyy-MM-dd"),
  "int" -> number,
  "bool" -> boolean
)
```

Mappings can be mapped to a case class:

```scala
case class User(firstname: String, lastname: String)
val userForm = mapping[User](
  "firstname" -> text,
  "lastname" -> text
)
```

Mappings can also be defined to have repeated values

```scala
val namesForm: Mapping[Seq[String]] = Mapping("name", seq(text))
val tuplesForm: Mapping[Seq[(String, String)]] = repeatedTuple(
  "firstname" -> text,
  "lastname" -> text
)
case class User(firstname: String, lastname: String)
val usersForm: Mapping[Seq[User]] = repeatedMapping[User](
  "firstname" -> text,
  "lastname" -> text
)
```

A field can sometime be optional: 

```scala
val optionalTextMapping: Mapping[Option[String]] = Mapping("optional", optional(text))
```

A field can have a default value. Default values are used when there is no corresponding data for the field from the request.

```scala
val numberMapping = Mapping("number", default(1))
val dateMapping = Mapping("date", default(localDateUsing("yyyy-MM-dd"), LocalDate.now))
```

A field can also be made to have a static value using ignored

```scala
case class User(id: Long, firstname: String, lastname: String)
val usersForm: Mapping[Seq[User]] = repeatedMapping[User](
  "id" -> ignored(1),
  "firstname" -> text,
  "lastname" -> text
)
```

### Form Constraints

Individual fields can have constraints. By default, nonEmptyText field requires a non empty strings while text field accepts empty strings. Other fields, such as number or localDate requires the submitted data to be of a certain format.

Text fields can be defined to require a certain length:

```scala
val textMapping = Mapping("text", text(minLength = 10, maxLength = 30, trim = true))
```

Similarly, number fields can have min or max value:

```scala
val numberMapping = Mapping("number", number(min = 10, max = 30))]
```

A field can also have ad-hoc constraints:

```scala
val dobMapping = Mapping(
  "dob", 
  localDate.verifying("Date should not be in the future", !_.isAfter(LocalDate.now))
)
```

Multiple fields can be verified as a whole:

```scala
val tupleMapping = tuple(
  "min" -> number,
  "max" -> number
).verifying("Min has to be smaller or equal to max", (min, max) => min <= max)
```

### Transformation

Fields can be transformed from one type to another. 

In the example below, a comma separated text is transformed to a sequence of String.

```scala
val form: Mapping[Seq[String]] = Mapping(
  "values", text.transform[Seq[String]](_.split(","), _.mkString(","))
)
```

### Handling filling a form with initial values

You can fill the form using the fill method:

```scala
val form = Mapping("name", nonEmptyText)

form.fill("John Doe")
```

### Handling binding

Data can be retrieved from the request using the bindFromRequest() method.

```scala
@Post("/submit")
def submitForm = Action{implicit request =>
  form.bindFromRequest().fold(
    errorForm => ???, // form errors can be obtained from errorForm.errors
    data => ??? // successfully bound data
  )
}
```

The form can also be bound manually using key-value pairs:

```scala
form.bind("firstname" -> "john", "lastname" -> "doe")
```

### Accessing field values and errors

A bound form can have individual field errors.

To access the individual fields, we can use the apply methods:

```scala
val form = tuple(
  "name" -> nonEmptyText.transform[String](_.capitalize, _.capitalize),
  "age" -> number
)
val boundForm = form.bind("name" -> "john")
val nameValue = boundForm[String]("name").value // Some("John")
val nameBindingValue = boundForm[String]("name").bindingValue // Some("john")
val ageError = boundForm[Int]("age").errors.map(_.message) // List("error.required")
```


## License

data-mapping is licensed under the Apache license version 2.
See [LICENSE](LICENSE.txt).
