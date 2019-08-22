FROM debian
RUN apt-get update && apt-get upgrade -y
RUN apt-get install r-base python-pip git nano -y 
RUN echo "install.packages(c('zoo',lubridate','dplyr','stringr'))" > libs_mineria.R && Rscript libs_mineria 
RUN echo "install.packages(c('RandomForest','e1071','neuralnet','caret','reticulate','keras'))" > libs_ML.R && Rscript libs_ML
RUN echo "install.packages (c('shiny','shinydashboard','ROSE''ggplot2'))" > lib_grafs.R && Rscript lib_graf.R 
RUN git clone https://github.com/progamandoconro/carbon-prediction
WORKDIR carbon-prediction
RUN echo "download.file\
('https://programandoconro.files.wordpress.com/2019/08/carbon_colombia.xlsx'\
,destfile='precio_carbon.xlsx')" > data.R && Rscript data.R  script.R
