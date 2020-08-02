# Dockerfile for mybinder.org
#
# Test this Dockerfile:
#
#     docker build -t learn-you-a-haskell .
#     docker run --rm -p 8888:8888 --name learn-you-a-haskell --env JUPYTER_TOKEN=x learn-you-a-haskell:latest
#

FROM crosscompass/ihaskell-notebook:8b7eb58cec8d

USER root

RUN mkdir /home/$NB_USER/learning_haskell_programming
COPY notebooks/*.ipynb /home/$NB_USER/learning_haskell_programming/
COPY notebooks/images /home/$NB_USER/learning_haskell_programming/images
RUN chown --recursive $NB_UID:users /home/$NB_USER/learning_haskell_programming

USER $NB_UID

ENV JUPYTER_ENABLE_LAB=yes
