Debido al gran tamaño de los datos (8599212 filas), el procesado se ha dividido en varios pasos. En cada fichero se realizan calculos diferentes e independientes que intentan analizar los datos desde diferentes puntos de vista focalizando en algun tema por separado.

_Cleaning.Rmd_ realiza unos primeros calculos y un filtrado de los datos, escribiendo un fichero intermedio llamado _ReducedData.csv_, acelerando de esta manera la ejecucion de algunos de los otros ficheros que usan estos datos precalculados, ademas de eliminar informacion no necesaria.

_Solstice.Rmd_ se utiliza para encontrar la relacion entre el mes de año y la temperatura media.

_GobalTrend.Rmd_ realiza un estudio simple de la tendencia de la temperatura a nivel global y particularizando en la ciudad y pais de los que disponemos de mayor numero de datos

_Deviation.Rmd_ realiza calculos de desviacion tipica para encontrar los paises y con mayor y menor varianza de temperatura, elevacion y distancia al mar para ver como esos parametros influyen en los modelos aplicados

_GlobalClimatePredictor.Rmd_ realiza un analisis mas exhaustivo de los datos desde varios puntos de vista. Debido a la extension del dataset y el tamaño de los graficos, es necesaria una buena cantidad de memoria para poder ejecutar el fichero.





