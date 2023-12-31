FROM ubuntu:18.04
MAINTAINER rick@owensmurray.com

RUN apt-get update && apt-get install -y libgmp10 libleveldb1v5 psmisc netbase curl

ADD /bin /bin

CMD [ "/bin/json-spec" ]

