FROM rocker/rstudio:4.3

RUN apt-get update && \
    apt-get -y --no-install-recommends install software-properties-common \
    python3-dev \
    python3-pip \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libxt6


RUN pip install synapseclient==4.3.1

COPY . /nfportalutils

# R components for installation or suggested usage
RUN R -e "install.packages(c('remotes', 'rmarkdown', 'DiagrammeR'), repos='http://cran.rstudio.com/')"
RUN R -e "remotes::install_local('nfportalutils', dependencies = c('Depends', 'Imports'))"
RUN R -e "remotes::install_version('reticulate', version = '1.39.0', repos ='http://cran.us.r-project.org')"

ENTRYPOINT [ "/bin/bash" ]

