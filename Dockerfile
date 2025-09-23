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
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libglpk-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libgsl-dev \
  && rm -rf /var/lib/apt/lists/*

COPY backend/install.R /tmp/install.R
RUN Rscript /tmp/install.R && rm /tmp/install.R

WORKDIR /app
COPY . /app

EXPOSE 8000
CMD ["Rscript", "backend/run_api.R"]
