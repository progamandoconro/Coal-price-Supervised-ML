FROM debian
RUN apt-get update && apt-get upgrade -y
RUN apt-get install r-base python-pip git nano -y 
RUN echo "install.packages(c('zoo',lubridate','randomForest',\
'e1071','neuralnet','caret','reticulate','keras','shiny',\
'shinydashboard','ROSE','dplyr','stringr','ggplot2'))" > libs.R && Rscript libs.R
RUN apt-get update && apt-get upgrade -y
RUN git clone https://github.com/progamandoconro/carbon-prediction
WORKDIR carbon-prediction
RUN echo "download.file\
('https://programandoconro.files.wordpress.com/2019/08/carbon_colombia.xlsx'\
,destfile='precio_carbon.xlsx')" > data.R && Rscript data.R  script.R
