# Dockerfile for mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t learn-you-a-haskell .
#     docker run --rm -p 8888:8888 --name learn-you-a-haskell learn-you-a-haskell:latest jupyter lab --ServerApp.token=''
#

FROM ghcr.io/ihaskell/ihaskell-notebook:master@sha256:7d8b0f2cf2d78afaa3088a31cafcec948408ac6bf228a6b73e001233ef40decf

USER root

RUN mkdir /home/$NB_USER/learn_you_a_haskell
COPY notebook/*.ipynb /home/$NB_USER/learn_you_a_haskell/
COPY notebook/img /home/$NB_USER/learn_you_a_haskell/img
RUN chown --recursive $NB_UID:users /home/$NB_USER/learn_you_a_haskell

ARG EXAMPLES_PATH=/home/$NB_USER/ihaskell_examples
COPY notebook_extra/WidgetRevival.ipynb $EXAMPLES_PATH/
RUN chown $NB_UID:users $EXAMPLES_PATH/WidgetRevival.ipynb

USER $NB_UID

ENV JUPYTER_ENABLE_LAB=yes

