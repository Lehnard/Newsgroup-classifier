train_test <- function(X, y, train.part= 0.8) {
        
#########################################################################################                         
##                                                                                      #
##  Autores: Maria Calvo             Fecha: 22/05/2017                                  #
##           David Grinan                                                               #
##           Jordi Aceiton                                                              #
##                                                                                      #
##                                                                                      #  
##  train_test(X, y, train.part= 0.8)                                                   #
##                                                                                      #
##      Hace la particion de X e y en Xtrain, ytrain, Xtest, ytest segun                #
##      la proporcion indicada en "train.part". Posteriormente guarda                   #
##      los 4 sets en "./data/"                                                         #
##                                                                                      #
##      input:                                                                          #
##                                                                                      #
##              X -- Matriz documentos-terminos (tipo matrix)                           #
##                                                                                      #
##              y -- Vector de etiquetas con la pertenencia de los                      #
##                   documentos en las distintas clases.                                #
##                                                                                      #
##              train.part -- Proporcion de X que ira para Train.                       #
##                            El resto, 1-train.part va al Test.                        #
##                                                                                      #
##      output:                                                                         #
##                                                                                      #
##              sets -- Una lista que contiene las listas train y                       #
##                      test. La lista train contiene Xtrain e ytrain.                  #
##                      La test, Xtest e y test.                                        #
##                                                                                      #
#########################################################################################         
        
        cat("\nPartiendo X e y en train y test sets... ")
        
        # Control de entrada de argumento.
        if(train.part>=1 | train.part<=0.5) stop(" 0.5 <= train.part <= 1")
        
        docs <- 1:floor(nrow(X)*train.part)
        
        Xtrain <- X[docs, ]
        Xtest <- X[-docs, ]
        ytrain <- y[docs]
        ytest <- y[-docs]
        
        train <- list(X= Xtrain, y= ytrain)
        test <- list(X= Xtest, y= ytest)
        
        cat("ok!\n")
        
        sets <- list(train= train, test= test)
        
} 