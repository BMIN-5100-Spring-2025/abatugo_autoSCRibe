FROM rocker/tidyverse:4.2.0

WORKDIR /

# documentation on install2.r: https://rocker-project.org/use/extending.html#install2.r
# installing packages
RUN install2.r  \
    Matrix \ 
    pak \ 
    MatrixModels \ 
    pbkrtest \ 
    gridExtra \ 
    DT \ 
    knitr \
    kableExtra \
    arsenal \
    readxl \
    rmarkdown \
    broom \
    tidyselect \
    gt \
    gtsummary \
    rstatix \
    pagedown \
    && rm -rf /tmp/downloaded_packages

COPY script/ /script

COPY header-footer.css /script/header-footer.css

CMD ["Rscript", "/script/main.R"]