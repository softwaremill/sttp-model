FROM gitpod/workspace-full
USER gitpod
RUN brew install coursier/formulas/coursier sbt scalaenv
RUN sudo env "PATH=$PATH" coursier bootstrap org.scalameta:scalafmt-cli_2.13:3.5.2 \
  -r sonatype:snapshots \
  -o /usr/local/bin/scalafmt --standalone --main org.scalafmt.cli.Cli
RUN scalaenv install scala-2.13.6 && scalaenv global scala-2.13.6
