FROM ubuntu:14.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update
RUN apt-get upgrade -y
RUN apt-get install haskell-platform emacs24-nox openssh-server git -y

RUN cabal update
RUN cabal install cabal-install

ENV PATH /.cabal/bin:$PATH
ENV LANG C.UTF-8

RUN mkdir /app
RUN git clone https://github.com/khanage/mongoweb /app/mongoweb
WORKDIR /app/mongoweb

RUN cabal install --dependencies-only --constraint="contravariant == 0.6"
RUN cabal build

COPY app.config /app/mongoweb/app.config

ENTRYPOINT ./dist/build/mongoweb/mongoweb

EXPOSE 8080
