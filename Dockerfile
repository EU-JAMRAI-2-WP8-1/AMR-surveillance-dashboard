# Base R Shiny image
FROM rocker/shiny

# install Linux R packages dependencies
RUN apt-get install libcurl4-openssl-dev
##RUN apt-get install -y libudunits2-dev

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
## base
RUN R -e "install.packages(c('dplyr', 'leaflet', 'gapminder', 'shinyWidgets', 'rmarkdown'))"
## data parsing
RUN R -e "install.packages(c('readxl'))"
## plots
RUN R -e "install.packages(c('countrycode', 'ggplot2', 'echarts4r', 'plotly', 'geojsonR'))"
## style
RUN R -e "install.packages(c('bslib', 'bsicons', 'thematic'))"
## 
RUN R -e "install.packages(c('remotes'))"

# Copy the Shiny app code
#COPY app.R /home/shiny-app/app.R
COPY files/data/* /home/shiny-app/files/data/
COPY files/logos/* /srv/shiny-server/www/logos/
#COPY files/css/style.css /srv/shiny-server/www/css/style.css

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
#CMD Rscript /home/shiny-app/app.R
