Placeholder folder for the model input content, this folder should be replaced by the model to be run as a mounted volume.

``` 
docker build -t watdev/integrated-model .
docker run -v $(pwd)/SIM253_RES:/modeller3/WATDEV/TOOLBOX/model -it watdev/integrated-model /bin/bash
```