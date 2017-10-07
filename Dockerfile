FROM "ubuntu:17.04"
MAINTAINER Jasper Van der Jeugt version: 0.1

RUN apt-get update
RUN apt-get install -y postgresql-server-dev-all
RUN apt-get install -y libgmp-dev
RUN apt-get install -y libnss3 libnss-lwres libnss-mdns ca-certificates netbase

ADD bin/fugacious-server /usr/bin/fugacious-server
ADD bin/fugacious-sendmail /usr/bin/fugacious-sendmail
ADD fugacious.yaml.prod /home/fugacious/fugacious.yaml
ADD assets /home/fugacious/assets

RUN useradd -ms /bin/bash -G sudo fugacious
RUN chown -R fugacious /home/fugacious

USER fugacious
ENV USER fugacious
WORKDIR /home/fugacious

EXPOSE 8000

CMD fugacious-server fugacious.yaml
