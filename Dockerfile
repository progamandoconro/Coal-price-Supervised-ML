FROM debian
RUN apt-get update && apt-get upgrade -y
RUN apt-get install r-base python-pip git nano -y 
RUN echo "install.packages(c('zoo','lubridate','dplyr','stringr'))" > lib_mineria.R && Rscript lib_mineria.R
RUN echo "install.packages(c('RandomForest','e1071','neuralnet','caret','reticulate','keras'))" > lib_ML.R && Rscript lib_ML.R
RUN echo "install.packages (c('shiny','shinydashboard','ROSE','ggplot2'))" > lib_graf.R && Rscript lib_graf.R 
RUN apt-get update && apt-get upgrade -y
RUN git clone https://github.com/progamandoconro/carbon-prediction
WORKDIR carbon-prediction
RUN echo "download.file\
('https://programandoconro.files.wordpress.com/2019/08/carbon_colombia.xlsx'\
,destfile='precio_carbon.csv')" > data.R && Rscript data.R  script.R
