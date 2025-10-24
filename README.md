# INTEGRATED SWAT & DSAT model

This repository contains code to integrate 3 models for agricultural decision support.

## Run the code on your local machines (Linux)
 
To run the code, please copy the original [DSSAT code](https://github.com/DSSAT/dssat-csm-os) to your local machine and update the symbolic links in the repository to match the corresponding directories on your local system.

The same should be done with the original [SWAT](https://github.com/swat-model/swatplus) and [MODFLOW](https://github.com/MODFLOW-ORG/modflow6) source codes.

The code still needs to be compiled on your local machine.
To compile it, use the command `make swat`. You can change the output directory of the binary file which is saved in Make file, and it will then copy the binary wherever you prefer.

In general, the original source codes should be maintained in a folder that only `SUDO` can modify, except for file copying.
Use only symbolic links to connect to those folders. 

## Project

This activity is part of the WATDEV project: Climate Smart WATer Management and Sustainable DEVelopment for Food and Agriculture in East Africa

The overarching objective of the project is to increase the sustainability of agricultural water management and resilience of agro-ecosystems to climate change in East Africa and Egypt. This will be achieved by two specific objectives: SO1. National Ministries and Research Institutions will be able to improve their knowledge and management on water in agriculture ; SO2. Farmers and local actors will implement innovative/sustainable solutions and skills on water management

The project has been funded by the European Union under [DeSiRa Capacity4dev](https://capacity4dev.europa.eu/projects/desira/info/watdev_en).