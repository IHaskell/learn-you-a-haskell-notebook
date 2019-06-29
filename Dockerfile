# Dockerfile for mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t learn-you-a-haskell .
#     docker run --rm -p 8888:8888 --name learn-you-a-haskell --env JUPYTER_TOKEN=x learn-you-a-haskell:latest
#

FROM crosscompass/ihaskell-notebook:2e52f44c6347

USER root

COPY notebook/*.ipynb /home/$NB_USER/work/
COPY notebook/img /home/$NB_USER/work/img
RUN chown --recursive $NB_UID:users work

USER $NB_UID

ENV JUPYTER_ENABLE_LAB=yes
