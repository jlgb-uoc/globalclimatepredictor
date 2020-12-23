Due to the big size of the data (8599212 rows), the processing has been divided in 2 steps.

Debido al gran tamaño de los datos (8599212 filas), el procesado se ha dividido en varios pasos. En cada fichero se realizan calculos diferentes e independientes que intentan analizar los datos desde diferentes puntos de vista focalizando en algun tema por separado.


On the first step, we choose the data we're working on, based on the existence of non-informed temperatures.

_Cleaning.Rmd_ does first calculations and filtering of the required data, and writes an intermediate file _ReducedData.csv_ so we can have a reduced amount of data to work easily with R
We eliminate City, Country and Temperature uncertainty information because we're not using it in the model
We are transformating latitude and longitude to integer positive and negative values
We extract date information keeping only year and month

_GobalTrend.Rmd

