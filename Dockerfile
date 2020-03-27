# get shiny serves plus tidyverse packages image
FROM rocker/shiny-verse:latest
RUN mkdir -p /var/lib/shiny-server/bookmarks/shiny
RUN chmod -R 755 /srv/shiny-server/

RUN sed -i 's/3838/3838 0.0.0.0/' /etc/shiny-server/shiny-server.conf


# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev\
    xtail \
    libssl-dev \
    libicu-dev \
    libudunits2-dev\
    libgdal-dev



#RUN R -e "remotes::install_github(c('rstudio/httpuv'))"
RUN R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinydashboard', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('andrewsali/shinycssloaders')"
RUN R -e "install.packages('shinydashboardPlus', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('tidyverse', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('readxl', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggthemes', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('rjson', repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('nik01010/dashboardthemes')"
RUN R -e "install.packages('RCzechia', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('leaflet', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('RJSONIO', repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('deSolve', repos='http://cran.rstudio.com/')"
	


# copy the app to the image
COPY app.R /srv/shiny-server/
COPY  google-analytics.html /srv/shiny-server/
#COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN chown -R shiny:shiny /srv/shiny-server

# select port
EXPOSE 8080

# allow permission


# run app
CMD ["/usr/bin/shiny-server.sh"]