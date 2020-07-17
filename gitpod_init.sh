 
 #!/bin/bash

METALS_DIR="$GITPOD_REPO_ROOT/.metals"
APPS_DIR="$METALS_DIR/apps"
METALS_VERSION="0.9.0"

mkdir -p $APPS_DIR

export PATH=$PATH:/usr/local/openjdk-8/bin:$APPS_DIR

echo "-Dsbt.coursier.home=$METALS_DIR/coursier" >> .jvmopts
echo "-Dcoursier.cache=$METALS_DIR/coursier" >> .jvmopts
echo "-Dsbt-dir=$METALS_DIR/sbt" >> .jvmopts
echo "-Dsbt-boot=$METALS_DIR/sbt/boot" >> .jvmopts
echo "-Divy=$METALS_DIR/.ivy2" >> .jvmopts

curl -Lo $APPS_DIR/cs https://git.io/coursier-cli-linux && chmod +x $APPS_DIR/cs

cs install --install-dir $APPS_DIR --only-prebuilt=true bloop
cs install --install-dir $APPS_DIR sbt

cs fetch org.scalameta:metals_2.12:$METALS_VERSION --cache=$METALS_DIR/coursier 
cs fetch org.scalameta:scalafmt-cli_2.12:2.4.2 --cache=$METALS_DIR/coursier 

sbt -Dbloop.export-jar-classifiers=sources bloopInstall
bloop compile --cascade rootProject

echo "export PATH=\$PATH:/usr/local/openjdk-8/bin:$APPS_DIR" >> ~/.bashrc