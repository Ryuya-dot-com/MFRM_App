FROM rocker/r-ver:4.3.2

ENV DEBIAN_FRONTEND=noninteractive \
    CRAN_REPO=https://packagemanager.posit.co/cran/latest \
    BRMS_BACKEND=rstan

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    g++ \
    gfortran \
    make \
    cmake \
    pkg-config \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    openjdk-17-jdk-headless \
    libfontconfig1-dev \
    libudunits2-dev \
    libglpk-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libwebp-dev \
    libsodium-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libgsl-dev \
  && rm -rf /var/lib/apt/lists/*

ENV JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
ENV PATH="${JAVA_HOME}/bin:${PATH}"

COPY backend/install.R /tmp/install.R
RUN Rscript /tmp/install.R && rm /tmp/install.R
RUN Rscript -e "options(repos = c(CRAN = Sys.getenv('CRAN_REPO', 'https://packagemanager.posit.co/cran/latest'))); if (!requireNamespace('plumber', quietly = TRUE)) install.packages('plumber', dependencies = TRUE)" 

WORKDIR /app
COPY . /app

EXPOSE 8000
CMD ["Rscript", "backend/run_api.R"]
