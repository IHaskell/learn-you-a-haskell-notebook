# Dockerfile for mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t learn-you-a-haskell .
#     docker run --rm -p 8888:8888 --name learn-you-a-haskell learn-you-a-haskell:latest jupyter lab --LabApp.token=''
#

FROM ghcr.io/jamesdbrock/ihaskell-notebook:master@sha256:23f44a47d53280b6d7c62eb5eb392865c12917047f117775dcec60022dff202f

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

