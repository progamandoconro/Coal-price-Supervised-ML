FROM debian
RUN apt-get update && apt-get upgrade -y
RUN apt-get install r-base python-pip git nano -y 
RUN echo "install.packages('keras') ; library (keras) ; install_keras()" > keras.R && Rscript keras.R
RUN /usr/bin/python2.7 -m pip install --upgrade --user virtualenv
RUN echo "install.packages(c('openxlsx','lubridate','dplyr','stringr','ROSE','reticulate'))" > libs.R && Rscript libs.R
RUN echo "install.packages(c('RandomForest','e1071','neuralnet','caret'))" > libs_ML.R && Rscript libs_ML.R
RUN git clone https://github.com/progamandoconro/carbon-prediction
WORKDIR carbon-prediction
RUN echo "download.file\
('https://programandoconro.files.wordpress.com/2019/08/carbon_colombia.xlsx'\
,destfile='precio_carbon.xlsx')" > data.R && Rscript data.R 
RUN Rscript script.R
