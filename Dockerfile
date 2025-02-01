FROM rocker/r-ver:4.2

WORKDIR /

COPY r-requirements.txt /script/r-requirements.txt

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libpng-dev \
    libfreetype6-dev \
    libtiff5-dev \
    libjpeg-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libfontconfig1-dev \
    libv8-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*

# Install Matrix 1.6.0 or later (required for MatrixModels)
RUN Rscript -e "install.packages('Matrix', repos='http://cran.rstudio.com/')"

# Install 'pak' for faster package installation
RUN Rscript -e "install.packages('pak', repos='http://cran.rstudio.com/')"

# Install dependencies first
RUN Rscript -e "pak::pkg_install(c('MatrixModels', 'pbkrtest'))"


RUN Rscript -e "install.packages(c('ggplot2', 'dplyr', 'tidyverse', 'gridExtra'), repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('DT', 'knitr', 'kableExtra'), repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('arsenal', 'readxl', 'rmarkdown'), repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('broom', 'tidyselect', 'gt'), repos='http://cran.rstudio.com/')"
RUN Rscript -e "install.packages(c('gtsummary'))"
RUN Rscript -e "install.packages(c('rstatix'))"
RUN Rscript -e "install.packages(c('pagedown'))"

RUN apt-get update && apt-get install -y --no-install-recommends curl \
    && curl -L https://github.com/jgm/pandoc/releases/download/3.1.1/pandoc-3.1.1-linux-amd64.tar.gz \
    | tar xvzf - --strip-components=1 -C /usr/local \
    && rm -rf /var/lib/apt/lists/*

COPY script/ /script

COPY header-footer.css /script/header-footer.css

CMD ["Rscript", "/script/main.R"]