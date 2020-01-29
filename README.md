![sttp-model](https://github.com/softwaremill/sttp-model/raw/master/banner.png)

[![Join the chat at https://gitter.im/softwaremill/sttp-model](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/softwaremill/sttp-model?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/softwaremill/sttp-model.svg?branch=master)](https://travis-ci.org/softwaremill/sttp-model)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.softwaremill.sttp.model/core_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.softwaremill.sttp.model/core_2.12)

sttp is a family of Scala HTTP-related projects, and currently includes:

* [sttp client](https://github.com/softwaremill/sttp): The Scala HTTP client you always wanted!
* [sttp tapir](https://github.com/softwaremill/tapir): Typed API descRiptions
* sttp model: this project. Simple Scala HTTP model. Used by sttp client & sttp tapir

## Quickstart with sbt

Add the following dependency:

```scala
"com.softwaremill.sttp.model" %% "core" % "1.0.0-RC6"
```

sttp model is available for Scala 2.11, 2.12, 2.13, Scala.JS and Scala Native.

## Project content

Available model classes include:

* `Uri`
* `Method`
* `StatusCode`
* `MediaType`
* `Header`
* `HeaderNames`
* `Cookie`
* `MultiQueryParams`
* `Part`

Most classes contain both serialisation & parsing functionality, following these conventions:

* `.toString` returns a representation of the model class in a format as in an HTTP request/response. For example,
  for an uri this will be `http://...`, for a header `[name]: [value]`, etc.
* constructors of the model classes are private; instances should be created through methods on the companion objects.
* `[SthCompanionObject].parse(serialized: String): Either[String, Sth]`: returns an error message or an instance of
  the model class
* `[SthCompanionObject].unsafeApply(values)`: creates an instance of the model class; validates the input values and in 
  case of an error, *throws an exception*. An error could be e.g. that the input values contain characters outside of
  the allowed range
* `[SthCompanionObject].safeApply(...): Either[String, Sth]`: same as above, but doesn't throw exceptions. Instead,
  returns an error message or the model class instance
* `[SthCompanionObject].notValidated(...): Sth`: creates the model type, without validation, and without throwing
  exceptions 

## Documentation

[Javadocs](https://www.javadoc.io/doc/com.softwaremill.sttp.model/core_2.12/latest/sttp/model/index.html)

The docs for sttp client contain documentation for the model classes: [overview](https://sttp.readthedocs.io/en/latest/model/model.html),
[uri interpolator](https://sttp.readthedocs.io/en/latest/model/uri.html).

## Contributing

If you have a question, or hit a problem, feel free to ask on our [gitter channel](https://gitter.im/softwaremill/sttp-model)!

Or, if you encounter a bug, something is unclear in the code or documentation, don’t hesitate and open an issue on GitHub.

### Building & testing the scala-native version

By default, sttp-native will **not** be included in the aggregate build of the root project. To include it, define the `STTP_NATIVE` environmental variable before running sbt, e.g.:

```
STTP_NATIVE=1 sbt
```

You might need to install some additional libraries, see the [scala native](http://www.scala-native.org/en/latest/user/setup.html) documentation site.

## Commercial Support

We offer commercial support for sttp and related technologies, as well as development services. [Contact us](https://softwaremill.com) to learn more about our offer!

## Copyright

Copyright (C) 2017-2019 SoftwareMill [https://softwaremill.com](https://softwaremill.com).
