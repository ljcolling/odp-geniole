FROM lcolling/r-verse-base:4.0.0 


RUN install2.r --error \
    --deps TRUE \
    sjmisc \
    readxl \
    robustbase \
    here \
    ez

WORKDIR $HOME/rstudio