# Start from a minimal Debian base image
# FROM debian:stable-slim
FROM ubuntu:18.04

LABEL org.opencontainers.image.source="https://github.com/watdev-eu/integrated_models"
ENV DEBIAN_FRONTEND=noninteractive
ENV MODDIR=/modeller3/WATDEV/TOOLBOX

ARG SWAT_VERSION="v1.1.3"
ARG DSSAT_VERSION="v4.8.0.15"

# === Install dependencies ===
RUN set -eux; \
    apt-get update && apt-get install -y libgfortran4 git wget unzip rsync ca-certificates build-essential gfortran cmake vim sed; \
    rm -rf /var/lib/apt/lists/*

RUN set -eux; \
    # clone local repository (to keep the sym links operational) 
    git clone -b "janne-full-dump" https://github.com/watdev-eu/integrated_models ${MODDIR}; \
    # prepare bin output folder
    mkdir -p "${MODDIR}/bin"; 
    # initialise error file
    # echo "" > "${MODDIR}/bin/MODEL.ERR"; \
    # resolve name conflict 
    # sed -i -e 's/albedo/albedo_swat/g' "${MODDIR}/SWAT/albedo.f";  \ 
    # rm -Rf /tmp/swat;   

ENV PATH="/modeller3/WATDEV/TOOLBOX/bin/:${PATH}"

WORKDIR ${MODDIR}

#RUN make swat 

#RUN set -eux; \
#    mkdir -p "/model";

#WORKDIR /model

CMD ["/bin/bash"]
