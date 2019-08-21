FROM debian

RUN apt-get update && apt-get upgrade -y
RUN apt-get install r-base python-pip git -y 

RUN echo "install.packages(c('zoo'))" > packages2_R.R && Rscript packages2_R.R
RUN echo "install.packages(c('lubridate'))" > packages3_R.R && Rscript packages3_R.R
RUN echo "install.packages(c('randomForest','e1071','neuralnet','caret'))" > packagesML.R && Rscript packagesML.R
RUN echo "install.packages(c('reticulate','keras'))" > packagesDL.R && Rscript packagesDL.R
RUN echo "install.packages(c('MASS','shiny','shinydashboard'))" > packages_compl.R && Rscript packages_compl.R
RUN echo "install.packages(c('ROSE','dplyr'))" > rose.R && Rscript rose.R
RUN echo "install.packages(c('ggplot2', 'plotly'))" > plots.R && Rscript plots.R
RUN apt-get install nano vim -y
RUN git clone https://github.com/progamandoconro/carbon-prediction
WORKDIR carbon-prediction
RUN Rscript R_script.R
