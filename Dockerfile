# Base R Shiny image
FROM rocker/shiny

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
RUN R -e "install.packages(c('dplyr', 'ggplot2', 'leaflet', 'gapminder', 'shinyWidgets', 'bslib', 'bsicons', 'thematic', 'rmarkdown'))"

# Copy the Shiny app code
#COPY app.R /home/shiny-app/app.R
COPY results.csv /home/shiny-app/results.csv
COPY files/logos/* /srv/shiny-server/www/logos/
#COPY files/css/style.css /srv/shiny-server/www/css/style.css

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
#CMD Rscript /home/shiny-app/app.R
