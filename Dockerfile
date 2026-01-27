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
    git clone -b main https://github.com/watdev-eu/integrated_models ${MODDIR}; \
    # requires specific version compatible with swat
    git clone --depth 1 -b ${DSSAT_VERSION} https://github.com/DSSAT/dssat-csm-os.git ${MODDIR}/SourceCode_dssat-csm-os-master_v4.8; \
    # use forked fixed version (until PR lands)
    git clone --depth 1 -b ubuntu-compat https://github.com/pvgenuchten/SWAT-MODFLOW3 /tmp/swat; \
    mv /tmp/swat/src "${MODDIR}/SourceCodeSM_V3"; \
    # prepare bin output folder
    mkdir -p "${MODDIR}/bin"; \
    # initialise error file
    echo "" > "${MODDIR}/bin/MODEL.ERR"; \
    # resolve name conflict 
    sed -i -e 's/albedo/albedo_swat/g' "${MODDIR}/SWAT/albedo.f";  \ 
    rm -Rf /tmp/swat;   

WORKDIR ${MODDIR}

RUN make swat 

CMD ["/bin/bash"]
