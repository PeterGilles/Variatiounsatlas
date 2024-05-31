FROM rocker/r-ver:4.3.2

RUN echo 'options(repos = c(REPO_NAME = "https://packagemanager.posit.co/cran/__linux__/jammy/2023-11-24"))' >> /root/.Rprofile

WORKDIR /home/app

COPY . .

RUN R -e "source('install_deps.R')"

RUN apt-get update && apt-get install -y \
    libudunits2-dev \
    libgdal-dev \
    libmagick++-dev \
    libproj-dev

EXPOSE 3838

CMD R -e 'shiny::runApp("/home/app", port = 3838, host = "0.0.0.0")'