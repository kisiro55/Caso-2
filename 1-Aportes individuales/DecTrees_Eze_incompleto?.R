## FUNCION PARA ENTRENAMIENTO

Entrenamiento <- function(DFTrain, DFTags, ConfLev, MinNum ){
        modelo <- C5.0(DFTrain,DFTags, C5.0Control(CF = ConfLev,
                                                   minCases = MinNum))
        return(model)
}

####
# Agrupar variables para simplificar 
#

##
# ESTANDARIZACION DE VARIABLE NUMERICAS
#

idx <- which(sapply(subscriptores.training, class) %in% c("numeric","integer"))

Sub_Train_EST1 <- subset(subscriptores.training,select(scale(subscriptores.training[,idx]))
                         
)SUB.TRAIN1 <- scale() 
