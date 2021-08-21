# Dockerfile for mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t learn-you-a-haskell .
#     docker run --rm -p 8888:8888 --name learn-you-a-haskell learn-you-a-haskell:latest jupyter lab --LabApp.token=''
#

FROM ghcr.io/jamesdbrock/ihaskell-notebook:master@sha256:c40ede11c9c0c82960e0db34f47385d96d197a1762821fcb21c61282ac69ce5c

USER root

RUN mkdir /home/$NB_USER/learn_you_a_haskell
COPY notebook/*.ipynb /home/$NB_USER/learn_you_a_haskell/
COPY notebook/img /home/$NB_USER/learn_you_a_haskell/img
RUN chown --recursive $NB_UID:users /home/$NB_USER/learn_you_a_haskell

USER $NB_UID

ENV JUPYTER_ENABLE_LAB=yes
