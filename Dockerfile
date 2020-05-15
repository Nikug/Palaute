# Mostly from: https://juanitorduz.github.io/dockerize-a-shinyapp/

# BUILD: sudo docker build -t palaute .
# RUN: sudo docker run --rm -p 80:3838 palaute

FROM rocker/shiny:3.6.3

# Online tutorial told me to install these
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev

# Dependencies
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('dplyr', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('stm', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tm', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('SnowballC', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('syuzhet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggplot2', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('proxy', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('wesanderson', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('Rtsne', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('corpus', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('scales', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readr', repos='http://cran.rstudio.com/')"

# Copying files
COPY course-analysis.Rproj /srv/shiny-server
COPY app.R /srv/shiny-server/
COPY server.R /srv/shiny-server/
COPY ui.R /srv/shiny-server/

# Copying folders
COPY data /srv/shiny-server/data
COPY modules /srv/shiny-server/modules
COPY scripts /srv/shiny-server/scripts
COPY www /srv/shiny-server/www

# Select port (default shiny-server port)
EXPOSE 3838

# Allow permissions
RUN sudo chown -R shiny:shiny /srv/shiny-server

# Run app
CMD ["/usr/bin/shiny-server.sh"]
