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
"com.softwaremill.sttp.model" %% "core" % "2.0.0-RC1"
```

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
