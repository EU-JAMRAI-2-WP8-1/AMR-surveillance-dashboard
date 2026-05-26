# Base R Shiny image matching renv.lock R version
FROM rocker/shiny:4.5.2

# Install Linux system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Make app directory
RUN mkdir /home/shiny-app

# Install renv and restore packages from lockfile into the system library.
# Copy renv.lock before app code so this layer is cached when only app code changes.
COPY renv.lock /home/shiny-app/renv.lock
RUN R -e "install.packages('renv', repos = 'https://cloud.r-project.org')" && \
    R -e "renv::restore(lockfile = '/home/shiny-app/renv.lock', library = .libPaths()[1])"

EXPOSE 8180
