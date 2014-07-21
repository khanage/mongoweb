FROM ubuntu:14.04

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update
RUN apt-get upgrade -y

RUN apt-get install haskell-platform emacs24-nox openssh-server git -y

RUN cabal update
RUN cabal install cabal-install

ENV PATH /.cabal/bin:$PATH

RUN echo $PATH
RUN cabal --version

EXPOSE 22 80