predict.logist <- function(thetas, Xpred) {
        
#########################################################################################                         
##                                                                                      #
##  Autores: Maria Calvo             Fecha: 22/05/2017                                  #
##           David Grinan                                                               #
##           Jordi Aceiton                                                              #
##                                                                                      #
##                                                                                      # 
##      predict.logist(thetas, Xpred)                                                   #
##                                                                                      #
##              Pasandole un vector-matriz de thetas (theta0, theta1, ...) saca         #
##              la prediccion de classificacion one vs all - logistic regression        #        
##              de los ejemplos en Xpred.                                               #
##                                                                                      #
##      input:                                                                          #
##                                                                                      #
##              thetas -- Matriz n x m donde n es el numero de features de X i          #
##                        m es el numero de clases en one vs all.                       #        
##                                                                                      #
##              Xpred -- El data set para hacer predicciones del modelo.                #
##                                                                                      #
##      output:                                                                         #
##                                                                                      #
##              ypred -- La clasificacion en clases de los ejemplos en Xpred.           #
##                                                                                      #
##                                                                                      #
#########################################################################################        

        ## clasificadores para el set Xpred.
        # Anadimos el intercept term x0 = 1 para el metodo
        Xpred <-cbind(x0= 1, Xpred)
        
        n <- ncol(thetas)
        h_pred <- matrix(0, nrow= nrow(Xpred), ncol= n)
        for(i in 1:n) {
                
                z <- thetas[,i] %*% t(Xpred)
                
                # matriz de clasificadores, cada columna para cada logistic regresion.
                # funcion sigmoid para definir clasificadores. 
                h_pred[,i] <- 1/(1+exp(-z))    
        }
        
        # prediccion
        ypred <- apply(h_pred, MARGIN= 1, which.max)
        
        # Normalizacion de h_pred
        h_pred_norm <- (1/rowSums(h_pred)) * h_pred 
        
        results <- list(prediction= ypred, class_h= h_pred, norm_probs= h_pred_norm)
        
        return(results)
}

