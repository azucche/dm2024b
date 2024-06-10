# Arbol elemental con libreria  rpart
# Debe tener instaladas las librerias  data.table  ,  rpart  y  rpart.plot

# cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

# Aqui se debe poner la carpeta de la materia de SU computadora local
setwd("/Users/ayelenzucchelli/Master/MineriaDatos") # Establezco el Working Directory

# cargo el dataset
dataset <- fread("./datasets/dataset_pequeno.csv")

dtrain <- dataset[foto_mes == 202107] # defino donde voy a entrenar
dapply <- dataset[foto_mes == 202109] # defino donde voy a aplicar el modelo

#-0.466153309355772, 758, 351, 6 -> 48.267
#-0.567332982774384, 595, 254, 6 -> 49.947

# genero el modelo,  aqui se construye el arbol
# quiero predecir clase_ternaria a partir de el resto de las variables
modelo <- rpart(
        formula = "clase_ternaria ~ .",
        data = dtrain, # los datos donde voy a entrenar
        xval = 0,
        cp = -1, # esto significa no limitar la complejidad de los splits
        minsplit = 2, # minima cantidad de registros para que se haga el split
        minbucket = 1, # minima cantidad de registros en una hoja
        maxdepth = 30  # profundidad maxima del arbol -> puede ser entre [2,30]
)

#1000, 500, 6
#1000, 500, 5
#1000, 500, 7 -> este es el tercer mejor
#1000, 500, 8
#1000, 600, 7
#1000, 400, 7
#1000, 450, 7 -> este es el cuarto mejor
#1000, 450, 6 -> este es el mejor
#1000, 450, 5 -> este es el segundo mejor
#900, 400, 6 
#900, 325, 6 
#500, 325, 6 IGUAL
#325, 150, 6 50507
#325, 175, 6 49877
#325, 200, 6 49007
#325, 160, 6 50807


# grafico el arbol
pdf('arbolito_051.pdf')
prp(modelo,
        extra = 101, digits = -5,
        branch = 1, type = 4, varlen = 0, faclen = 0
)
dev.off()

# aplico el modelo a los datos nuevos
prediccion <- predict(
        object = modelo,
        newdata = dapply,
        type = "prob"
)

# prediccion es una matriz con TRES columnas,
# llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
# cada columna es el vector de probabilidades

# agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[, prob_baja2 := prediccion[, "BAJA+2"]]

# solo le envio estimulo a los registros
#  con probabilidad de BAJA+2 mayor  a  1/40
dapply[, Predicted := as.numeric(prob_baja2 > 1 / 40)]

# genero el archivo para Kaggle
# primero creo la carpeta donde va el experimento
#dir.create("./exp/")
#dir.create("./exp/KA2001")

# solo los campos para Kaggle
fwrite(dapply[, list(numero_de_cliente, Predicted)],
        file = "./exp/KA2001/K101_51.csv",
        sep = ","
)

