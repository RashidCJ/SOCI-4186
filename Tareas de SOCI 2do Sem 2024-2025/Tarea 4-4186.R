# Cabecera -----------
# SOCI 4186 -- sección N                #
# Nombre:                               #
# Apellidos:                            #
# Tarea 4: Cargar y modificar datos     #

#En esta tarea harán lo siguiente
# 1. Guardar esta tarea, cambiando el nombre de Tarea3-4186.R
# a Apellido1-Apellido2-Tarea3-{1 o 2}.R 
# 2. Completar las próximas secciones, guardando la secuencia
# de comandos que usaron para cumplir con lo pedido. A modo de ejemplo
# dejo una parte ya hecha. Continúen desde ahí.

#Sección 0: Cargar librerías -----------
# Deben cargar las librerías adecuada para toda la tarea aquí. 
# A modo de ejemplo cargaré la de dslabs. Carguen tidyverse.
library(dslabs)
library()

# De una vez aprovechen seleccionar su directorio de trabajo aquí con código.

# Sección 1: Modificando datos con mutate -----------
## Carguen los datos de heights y murders, y modifíquenlos 
## añadiendo al menosuna nueva variable c/u.
## En el caso de heights, añadan una variable que sea la altura en metros.

data()

## En el caso de murders, añadan una variable que sea la tasa de asesinatos por cada 100,000 habitantes.
## Modificarán usando mutate y case_when los nombres de las regiones al español.
data()

# Sección 2: Resumiendo datos -----------
## Con summarise, resuman los datos de murders y heights por grupos. En específico
## quiero que resuman la tasa de asesinatos por región y la altura promedio por género.

# Sección 3: Exportando datos -----------
## Guardarán los datos de murders y heights en formato csv o csv2, con sus 
## iniciales y el nombre del archivo. Por ejemplo, si su nombre es Juan Pérez García,
## el archivo de murders se llamará JPG-murders.csv y el de heights JPG-heights.csv.
## También enviarán el archivo de este script.
