# Dockerfile for mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t learn-you-a-haskell .
#     docker run --rm -p 8888:8888 --name learn-you-a-haskell learn-you-a-haskell:latest jupyter lab --LabApp.token=''
#

FROM crosscompass/ihaskell-notebook:9dc7b1034d5f

USER root

RUN mkdir /home/$NB_USER/learn_you_a_haskell
COPY notebook/*.ipynb /home/$NB_USER/learn_you_a_haskell/
COPY notebook/img /home/$NB_USER/learn_you_a_haskell/img
RUN chown --recursive $NB_UID:users /home/$NB_USER/learn_you_a_haskell

USER $NB_UID

ENV JUPYTER_ENABLE_LAB=yes
