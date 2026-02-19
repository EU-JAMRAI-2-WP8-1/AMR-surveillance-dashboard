# Base R Shiny image
FROM rocker/shiny:4.4.3

# install Linux R packages dependencies
RUN apt update
RUN apt install libcurl4-openssl-dev

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
## base
RUN R -e "install.packages(c('shinyWidgets', 'shinyjs'))"
## data parsing
RUN R -e "install.packages(c('rjson', 'openxlsx'))"
## map
## RUN R -e "install.packages(c())"
## plots and tables
RUN R -e "install.packages(c('plotly', 'DT'))"
## style
RUN R -e "install.packages(c('bslib', 'bsicons', 'thematic'))"
## email - uses system curl (no R packages needed)
##
## removed
## RUN R -e "install.packages(c('jqr', 'udunits2', 'dplyr', 'gapminder', 'rmarkdown', 'readxl', 'jsonlite', 'hash', 'countrycode', 'ggplot2', 'echarts4r', 'remotes', 'sf', 'geos', 'gdal', 'geojsonR', 'geojsonio', 'leaflet'))"

# Copy the Shiny app code
#COPY app.R /home/shiny-app/app.R                           ## mounted in docker compose
COPY www/data/* /srv/shiny-server/www/data/
COPY www/logos/* /srv/shiny-server/www/logos/
COPY www/html/* /srv/shiny-server/www/html/
COPY www/favicons/* /srv/shiny-server/www/favicons/
#COPY www/css/style.css /srv/shiny-server/www/css/style.css ## mounted in docker compose
#COPY www/js/script.js /srv/shiny-server/www/js/script.js   ## mounted in docker compose

# Expose the application port
EXPOSE 8180

# Run the R Shiny app ## -> command in docker-compose.yaml
#CMD Rscript /home/shiny-app/app.R # run from docker compose
