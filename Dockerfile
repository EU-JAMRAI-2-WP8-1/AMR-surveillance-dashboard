# Base R Shiny image
FROM rocker/shiny

# install Linux R packages dependencies
RUN apt update
RUN apt install libcurl4-openssl-dev

# Linux packages for geojsonio
#RUN apt install -y libjq-dev
#RUN apt install -y libudunits2-dev
#RUN apt install -y libgeos-dev
#RUN apt install -y gdal-bin
#RUN apt install -y proj-bin
#RUN apt install -y libgdal-dev
#RUN apt install -y libproj-dev

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
## base
RUN R -e "install.packages(c('jqr', 'udunits2', 'dplyr', 'gapminder', 'shinyWidgets', 'rmarkdown'))"
## data parsing
RUN R -e "install.packages(c('readxl', 'rjson', 'jsonlite', 'hash'))"
## map
##RUN R -e "install.packages(c())"
## plots and tables
RUN R -e "install.packages(c('countrycode', 'ggplot2', 'echarts4r', 'plotly', 'DT'))"
## style
RUN R -e "install.packages(c('bslib', 'bsicons', 'thematic'))"
## 
RUN R -e "install.packages(c('remotes', 'sf'))"
## removed
## RUN R -e "install.packages(c('remotes', 'sf', 'geos', 'gdal', 'geojsonR', 'geojsonio', 'leaflet'))"

# Copy the Shiny app code
#COPY app.R /home/shiny-app/app.R                             ## mounted in docker compose
COPY files/data/* /home/shiny-app/files/data/
COPY files/logos/* /srv/shiny-server/www/logos/
COPY files/html/* /srv/shiny-server/www/html/
#COPY files/css/style.css /srv/shiny-server/www/css/style.css ## mounted in docker compose
#COPY files/js/script.js /srv/shiny-server/www/js/script.js   ## mounted in docker compose

# Expose the application port
EXPOSE 8180

# Run the R Shiny app ## -> command in docker-compose.yaml
#CMD Rscript /home/shiny-app/app.R # run from docker compose
