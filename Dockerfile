# Dockerfile for mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t learn-you-a-haskell .
#     docker run --rm -p 8888:8888 --name learn-you-a-haskell --env JUPYTER_TOKEN=x learn-you-a-haskell:latest
#

FROM crosscompass/ihaskell-notebook:62631e7176e8

USER root

RUN mkdir /home/$NB_USER/learn_you_a_haskell
COPY notebooks/*.ipynb /home/$NB_USER/learn_you_a_haskell/
COPY notebooks/img /home/$NB_USER/learn_you_a_haskell/img
RUN chown --recursive $NB_UID:users /home/$NB_USER/learn_you_a_haskell

USER $NB_UID

ENV JUPYTER_ENABLE_LAB=yes
