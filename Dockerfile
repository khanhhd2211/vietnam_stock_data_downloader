# Base image https://hub.docker.com/u/rocker/
FROM openanalytics/r-base

LABEL maintainer "Tobias Verbeke <tobias.verbeke@openanalytics.eu>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# copy necessary files
## app folder

RUN mkdir /root/app

COPY . /root/app
WORKDIR /root/app

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
# CMD ["R", "-e", "shiny::runApp('/app', launch.browser=FALSE, host = '0.0.0.0', port = 8080)"]
CMD ["R", "-e", "shiny::runApp('/root/app')"]