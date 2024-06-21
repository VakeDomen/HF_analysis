# Base R Shiny image
FROM rocker/shiny:latest

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
RUN R -e "install.packages(c('dplyr','ggplot2','scales','lubridate','gridExtra','ggrepel','tidyverse','readr','stringr','tidyr','forcats','shinythemes','shinyWidgets','plotly','DT'))"

# Copy the Shiny app code
COPY visualization/app.R /home/shiny-app/app.R

RUN mkdir /home/data

COPY data/author_activity.csv /home/data/author_activity.csv
COPY data/data.csv /home/data/data.csv
COPY data/data_for_model_evolution.csv /home/data/data_for_model_evolution.csv

# Expose the application port
EXPOSE 6630

# Run the R Shiny app
CMD Rscript /home/shiny-app/app.R