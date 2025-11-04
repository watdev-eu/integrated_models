# Start from a minimal Debian base image
FROM debian:stable-slim

LABEL org.opencontainers.image.source="https://github.com/nfmsu/Integrated_models"
ENV DEBIAN_FRONTEND=noninteractive
ENV MODDIR=/modeller3/WATDEV/TOOLBOX

# === Install dependencies ===
RUN set -eux; \
    apt-get update && apt-get install -y --no-install-recommends \
        build-essential gfortran cmake git pkg-config wget unzip rsync ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# === Copy the current repository into container ===
COPY . ${MODDIR}/

# === Clone required external repositories ===
RUN set -eux; \
    rm -rf ${MODDIR}/SourceCode_dssat-csm-os-master_v4.8 ${MODDIR}/SWAT || true; \
    git clone --depth 1 https://github.com/DSSAT/dssat-csm-os.git ${MODDIR}/SourceCode_dssat-csm-os-master_v4.8; \
    git clone --depth 1 https://github.com/crazyzlj/SWAT.git ${MODDIR}/SWAT

# === Build DSSAT ===
RUN set -eux; \
    mkdir -p ${MODDIR}/SourceCode_dssat-csm-os-master_v4.8/build && cd ${MODDIR}/SourceCode_dssat-csm-os-master_v4.8/build; \
    cmake -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_INSTALL_PREFIX=/opt/dssat ..; \
    make -j$(nproc); \
    make install

# === Build SWAT ===
RUN set -eux; \
    mkdir -p ${MODDIR}/SWAT/build && cd ${MODDIR}/SWAT/build; \
    cmake -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_INSTALL_PREFIX=/opt/swat ..; \
    make -j$(nproc); \
    make install

# === Optionally build Integrated_models if it has CMakeLists.txt ===
RUN set -eux; \
    if [ -f "${MODDIR}/CMakeLists.txt" ]; then \
        mkdir -p ${MODDIR}/build && cd ${MODDIR}/build; \
        cmake -DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_INSTALL_PREFIX=/opt/integrated_models ..; \
        make -j$(nproc); \
        make install; \
    else \
        echo "No CMakeLists.txt found in ${MODDIR}, skipping Integrated_models build."; \
    fi

WORKDIR ${MODDIR}
CMD ["/bin/bash"]
