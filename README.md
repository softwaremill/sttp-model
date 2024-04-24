![sttp-model](https://github.com/softwaremill/sttp-model/raw/master/banner.png)

[![Join the chat at https://gitter.im/softwaremill/sttp](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/softwaremill/sttp)
[![CI](https://github.com/softwaremill/sttp-model/workflows/CI/badge.svg)](https://github.com/softwaremill/sttp-model/actions?query=workflow%3ACI+branch%3Amaster)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.softwaremill.sttp.model/core_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.softwaremill.sttp.model/core_2.13)

[![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/softwaremill/sttp-model)

sttp is a family of Scala HTTP-related projects, and currently includes:

* [sttp client](https://github.com/softwaremill/sttp): The Scala HTTP client you always wanted!
* [sttp tapir](https://github.com/softwaremill/tapir): Typed API descRiptions
* sttp model: this project. Simple Scala HTTP model. Used by sttp client & sttp tapir

## Quickstart with sbt

Add the following dependency:

```scala
"com.softwaremill.sttp.model" %% "core" % "1.7.10"
```

sttp model is available for Scala 2.11, 2.12, 2.13, 3.0.0, Scala.JS and Scala Native.

## Project content

Available model classes include:

* `Uri`
* `Method`
* `StatusCode`
* `MediaType`
* `Header`
* `HeaderNames`
* `QueryParams`
* body fragments:  
   * `Part`
   * `ServerSentEvent`  
* header values:
   * `Accepts`
   * `Accept-Encoding`
   * `CacheDirective`
   * `Cookie`
   * `ETag`
   * `Range`
   * `ContentRange`
   * `WWWAuthenticateChallenge`

Most classes contain both serialisation & parsing functionality, following these conventions:

* `.toString` returns a representation of the model class in a format as in an HTTP request/response. For example,
  for an uri this will be `http://...`, for a header `[name]: [value]`, etc.
* `[SthCompanionObject].parse(serialized: String): Either[String, Sth]`: returns an error message, or an instance of
  the model class
* `[SthCompanionObject].unsafeApply(values)`: creates an instance of the model class; validates the input values and in 
  case of an error, *throws an exception*. An error could be e.g. that the input values contain characters outside
  the allowed range
* `[SthCompanionObject].safeApply(...): Either[String, Sth]`: same as above, but doesn't throw exceptions. Instead,
  returns an error message, or the model class instance
* `[SthCompanionObject].apply(...): Sth`: creates the model type, without validation, and without throwing
  exceptions 

## Documentation

[Javadocs](https://www.javadoc.io/doc/com.softwaremill.sttp.model/core_2.12/latest/sttp/model/index.html)

The docs for sttp client contain documentation for the model classes: [overview](https://sttp.softwaremill.com/en/latest/model/model.html),
[uri interpolator](https://sttp.softwaremill.com/en/latest/model/uri.html).

## Contributing

If you have a question, or hit a problem, feel free to ask on our [gitter channel](https://gitter.im/softwaremill/sttp-model)!

Or, if you encounter a bug, something is unclear in the code or documentation, don’t hesitate and open an issue on GitHub.

### Testing Scala.js

Scala.js tests require [chromedriver](https://chromedriver.chromium.org/). It is downloaded automatically
by the `test` task. Make sure to run it before running tests in any other way, e.g. using `testOnly`.

### Building & testing the scala-native version

By default, sttp-native will **not** be included in the aggregate build of the root project. To include it, define the `STTP_NATIVE` environmental variable before running sbt, e.g.:

```
STTP_NATIVE=1 sbt
```

You might need to install some additional libraries, see the [scala native](http://www.scala-native.org/en/latest/user/setup.html) documentation site.

## FAQ: Encoding & decoding URI components

See [sttp client `Uri` docs](https://sttp.softwaremill.com/en/latest/model/uri.html#faq-encoding-decoding-uri-components)

## Commercial Support

We offer commercial support for sttp and related technologies, as well as development services. [Contact us](https://softwaremill.com) to learn more about our offer!

## Copyright

Copyright (C) 2019-2021 SoftwareMill [https://softwaremill.com](https://softwaremill.com).
