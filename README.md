# INTEGRATED SWAT & DSAT model

This repository contains code to integrate 3 models for agricultural decision support.

## Run the code on local machines (Linux)
 
To run the code, please copy the original [DSSAT code](https://github.com/DSSAT/dssat-csm-os) to your local machine and update the symbolic links in the repository to match the corresponding directories on your local system.

Similar can be done with the source codes of https://swat.tamu.edu/software/swat-modflow/.

The code still needs to be compiled on your local machine.
To compile it, use the command `make swat`. You can change the output directory of the binary file which is saved in Make file, and it will then copy the binary wherever you prefer.

In general, the original source codes should be maintained in a folder that only `SUDO` can modify, except for file copying.
Use only symbolic links to connect to those folders. 

## Docker

Run model from Github Container Registry.
Navigate a shell to the TxtInOut folder and run:

```
docker run -it -v $(pwd):/model ghcr.io/watdev-eu/integrated_models:v0.2 bash
```

Or build and run the container locally with:

```
docker build -t watdev/intmod
docker run -it -v $(pwd):/model watdev/intmod bash
```

On the container, run the model with:

```
cd /model
swat
```

## Notes

- clone the repo inside container (do not copy in files) else symlinks get broken
- many errors when running on recent ubunty with libgfortran5 -> downgraded to 18 libgfortran 4
- use zip file from https://swat.tamu.edu/software/swat-modflow/, because it includes many modules

File `interface.f90` has at line 2629 a path reference to a path where the DSSAT config parameters are located

## Project

This activity is part of the WATDEV project: Climate Smart WATer Management and Sustainable DEVelopment for Food and Agriculture in East Africa

The overarching objective of the project is to increase the sustainability of agricultural water management and resilience of agro-ecosystems to climate change in East Africa and Egypt. This will be achieved by two specific objectives: SO1. National Ministries and Research Institutions will be able to improve their knowledge and management on water in agriculture ; SO2. Farmers and local actors will implement innovative/sustainable solutions and skills on water management

The project has been funded by the European Union under [DeSiRa Capacity4dev](https://capacity4dev.europa.eu/projects/desira/info/watdev_en).
