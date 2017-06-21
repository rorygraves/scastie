FROM openjdk:8u131-jdk-alpine

RUN \
  echo "Installing packages" &&\
  apk update &&\
  apk add ca-certificates &&\
  update-ca-certificates &&\
  apk add openssl &&\
  apk add --update bash &&\
  apk add --update ncurses &&\
  apk add --update nodejs &&\
  apk add --update curl &&\
  apk add --update graphviz &&\
# JRE fails to load fonts if there are no standard fonts in the image; DejaVu is a good choice,
# see https://github.com/docker-library/openjdk/issues/73#issuecomment-207816707
  apk add --update ttf-dejavu &&\
  apk add --update font-adobe-100dpi &&\
  apk add --update git

# These fonts are somewhat prettier
RUN git clone --depth 1 --branch release https://github.com/adobe-fonts/source-code-pro.git /usr/share/fonts/source-code-pro && \
  rm -rf /usr/share/fonts/source-code-pro/.git && \
  fc-cache -f -v /usr/share/fonts/source-code-pro

RUN apk update && apk add --no-cache fontconfig && \
  mkdir -p /usr/share && \
  cd /usr/share && \
  curl -L https://github.com/Overbryd/docker-phantomjs-alpine/releases/download/2.11/phantomjs-alpine-x86_64.tar.bz2 | tar xj &&\
  ln -s /usr/share/phantomjs/phantomjs /usr/bin/phantomjs

ENV LANG en_US.UTF-8

ENV COURSIER_CACHE /root/.coursier
ENV SBT_OPTS -Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled

RUN \
  echo "Installing sbt" &&\
  wget https://dl.bintray.com/sbt/native-packages/sbt/0.13.15/sbt-0.13.15.tgz -O /tmp/sbt-0.13.15.tgz && \
  mkdir -p /app/sbt && \
  tar -xzvf /tmp/sbt-0.13.15.tgz -C /app/sbt && \
  ln -s /app/sbt/sbt/bin/sbt /usr/local/bin/sbt

RUN \
  echo "Priming sbt" &&\
  mkdir /root/.sbt &&\
  mkdir /root/.sbt/0.13 &&\
  mkdir /root/.sbt/0.13/plugins &&\
  echo 'addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC6")' >> /root/.sbt/0.13/plugins/plugins.sbt  &&\
  echo 'addSbtPlugin("org.ensime" % "sbt-ensime" % "1.12.11")' >> /root/.sbt/0.13/plugins/plugins.sbt

RUN \
  echo "Priming 2.10 caches" &&\
  mkdir /root/scala-210 &&\
  mkdir /root/scala-210/project &&\
  echo 'sbt.version = 0.13.15' > /root/scala-210/project/build.properties &&\
  echo 'scalaVersion := "2.10.6"' > /root/scala-210/build.sbt &&\
  mkdir /root/scala-210/src &&\
  mkdir /root/scala-210/src/main &&\
  mkdir /root/scala-210/src/main/scala &&\
  echo 'object Main { def main(args: Array[String]): Unit = println("Hello, World!") }' > /root/scala-210/src/main/scala/Main.scala  &&\
  cd /root/scala-210; sbt run

RUN \
  echo "Priming 2.11 caches" &&\
  mkdir /root/scala-211 &&\
  mkdir /root/scala-211/project &&\
  echo 'sbt.version = 0.13.15' > /root/scala-211/project/build.properties &&\
  echo 'scalaVersion := "2.11.11"' > /root/scala-211/build.sbt &&\
  mkdir /root/scala-211/src &&\
  mkdir /root/scala-211/src/main &&\
  mkdir /root/scala-211/src/main/scala &&\
  echo 'object Main { def main(args: Array[String]): Unit = println("Hello, World!") }' > /root/scala-211/src/main/scala/Main.scala &&\
  cd /root/scala-211; sbt run

RUN \
  echo "Priming 2.12 caches" &&\
  mkdir /root/scala-212 &&\
  mkdir /root/scala-212/project &&\
  echo 'sbt.version = 0.13.15' > /root/scala-212/project/build.properties &&\
  echo 'scalaVersion := "2.12.2"' > /root/scala-212/build.sbt &&\
  mkdir /root/scala-212/src &&\
  mkdir /root/scala-212/src/main &&\
  mkdir /root/scala-212/src/main/scala &&\
  echo 'object Main { def main(args: Array[String]): Unit = println("Hello, World!") }' > /root/scala-212/src/main/scala/Main.scala &&\
  cd /root/scala-212; sbt run

RUN \
  echo "Priming 2.13 caches" &&\
  mkdir /root/scala-213 &&\
  mkdir /root/scala-213/project &&\
  echo 'sbt.version = 0.13.15' > /root/scala-213/project/build.properties &&\
  echo 'scalaVersion := "2.13.0-M1"' > /root/scala-213/build.sbt &&\
  mkdir /root/scala-213/src &&\
  mkdir /root/scala-213/src/main &&\
  mkdir /root/scala-213/src/main/scala &&\
  echo 'object Main { def main(args: Array[String]): Unit = println("Hello, World!") }' > /root/scala-213/src/main/scala/Main.scala &&\
  cd /root/scala-213; sbt run

RUN mkdir /root/dotty
RUN mkdir /root/dotty/project
RUN echo 'sbt.version = 0.13.15' > /root/dotty/project/build.properties
RUN echo 'addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.1.3")' > /root/dotty/project/plugins.sbt
RUN echo 'scalaVersion := "0.2.0-RC1"' > /root/dotty/build.sbt
RUN mkdir /root/dotty/src
RUN mkdir /root/dotty/src/main
RUN mkdir /root/dotty/src/main/scala
RUN echo 'object Main { def main(args: Array[String]): Unit = println("Hello, World!") }' > /root/dotty/src/main/scala/Main.scala
RUN cd /root/dotty; sbt run

RUN apk add --update procps
RUN addgroup -g 433 ensime
RUN adduser -h /home/ensime -G ensime -D -u 433 -s /bin/sh ensime
USER ensime

RUN mkdir -p "/home/ensime/workspace"
RUN echo "export COURSIER_CACHE=/home/ensime/.coursier" > /home/ensime/.profile
RUN echo "export SBT_OPTS=\"-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled\"" >> /home/ensime/.profile

USER root

RUN apk add sudo

# --symbolic-link
RUN  cp -R  /root/.sbt/ /home/ensime/.sbt/
RUN  cp -R  /root/.ivy2/ /home/ensime/.ivy2/
RUN  cp -R  /root/.coursier/ /home/ensime/.coursier/
RUN chown -R ensime:ensime /home/ensime/.ivy2/
RUN chown -R ensime:ensime /home/ensime/.sbt/
RUN chown -R ensime:ensime /home/ensime/.coursier/

USER root

# ensime should run as this user
ENV ENSIME_USER_GROUP ensime