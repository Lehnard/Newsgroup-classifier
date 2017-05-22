logist_ml <- function(Xtrain, ytrain, lambda= 0, maxit= 25) {
        
#########################################################################################                         
##                                                                                      #
##  Autores: Maria Calvo             Fecha: 22/05/2017                                  #
##           David Grinan                                                               #
##           Jordi Aceiton                                                              #
##                                                                                      #
##                                                                                      # 
##      logist_ml(Xtrain, ytrain, lambda= 0, maxit= 25)                                 #
##                                                                                      #
##              Usa el metodo "one vs all" para entrenar un modelo de                   #
##              clasificacion. Este consta de tantas regresiones logisticas             #
##              como grupos de clasificacion tenemos. Finalmente para cada              #
##              ejemplo de X se elige el clasificador h = sigmoid(theta'X_i)            #
##              con un valor mayor (probabilidad de pertenencia o no mayor).            #
##                                                                                      #
##              Para la optimizacion de la funcion de coste de cada                     #
##              regresion logistica se usa el método optim() que dispone R              #
##              análogo al fminunc Unconstrained Minimization de mathlab.               #
##                                                                                      #
##              Igualmente, se anaden las funciones necesarias para la regresion:       #
##                                                                                      #
##                         costFunctionReg(theta, X, y, lambda)                         #
##                         JcostFunctionReg(theta, X, y,lambda)                         #        
##                         gradientReg(theta,X,y,lambda)                                #
##                                                                                      #
##      input:                                                                          #        
##                                                                                      #
##              Xtrain -- El train data set para entrenar el modelo                     #
##                                                                                      #
##              ytrain -- El vector de etiquetas de pertenencia de cada i-ejemplo       #
##                        de Xtrain a los distintos grupos-conjuntos-clases.            #
##                                                                                      #
##              lambda -- parametro de regularizacion                                   #
##                                                                                      #
##              maxit -- parametro para el metodo optim() de R.                         #
##                                                                                      #
##                                                                                      #
##      output:                                                                         #
##                                                                                      #
##              thetas -- Matriz n x m donde las m-columnas son los parametros          #
##                        theta de cada clasificador newsgroup "m" vs all.              #
##                        n, el numero de filas es el numero de features de Xtrain      #          
##                                                                                      #
#########################################################################################        

        
        # Matrices train y test construïdas con las y1, y2, ..., yn 
        # donde n es el número de grupos y cada yk representa un vector
        # binario de 1s y 0s segun si el ejemplo pertenece al grupo k o al resto.
        # 1 pertenece al grupo k , 0 si no.
        n <- max(ytrain)
        yTrainMat <- matrix(0, nrow= length(ytrain), ncol= n)
        
        for(i in 1:n) {
                yTrainMat[,i] <- as.numeric(ytrain == i)
        }
        
        ## LOGISTIC REGRESSION
        ## para cada uno de los n casos de clasificacion  para cada pertenece 
        ## al k-group o no. "one vs. all"
        
        # Anadimos el intercept term x0 = 1 para el metodo
        Xtrain <-cbind(x0= 1, Xtrain)
        
        # Initialize useful objects.
        initial_theta <- rep(0, ncol(Xtrain))
        theta <- matrix(0, nrow= ncol(Xtrain), ncol= n)
        
        # Entrenamos el modelo  (adquirir theta optimo)
        for(i in 1:n) {
                
                cat("One vs all - logistic regression para newsgroup num:", i,"de", n,"...")
                
                # optimizacion
                optimRes <- optim(par= initial_theta,
                                  fn= JcostFunctionReg, 
                                  X = Xtrain, y = yTrainMat[,i], lambda= lambda,  # assign explicitly the argument values.
                                  gr= gradientReg, 
                                  method= "BFGS",
                                  control= list(maxit= maxit))
                
                # obtenemos los parámetros óptimos de la logistic regresion de cada grupo. 
                theta[,i] <- optimRes$par
                
                cat(" ok!\n")
        }
        
        logist_model <- list(weights= theta,
                             description= "Objeto modelo logistic regresion one vs all.",
                             optimizer= "optim {stats}",
                             par= "initial_theta",
                             fn= "JcostFunctionReg",
                             gr= "gradientReg",
                             method= "BFGS",
                             maxit= maxit,
                             lambda= lambda,
                             n.classes= n)
    
        return(logist_model)
}

###############################################################################        
## FUNCIONES PARA logist_classif
costFunctionReg <- function(theta, X, y, lambda) {
        
## =============================================================================        
##   costFunctionReg(theta, X, y, lambda)   
##
##      Compute cost and gradient for logistic regression with regularization
##
##   input:     X  a matrix containing {x0=1, x1, x2, ..., xn} features.
##              y  a vector containing {y} values
##              theta  the vector theta to compute J(theta) and grad(theta).  
##              lambda   the regularization parameter.
##
##   output:    a list which elements are: J and grad
##
##              "J"         The J(theta) value computed 
##                         
##              "grad"      The grad(theta) vector computed        
##                     
## =============================================================================            
## =============================================================================
## It also contains JcostFunctionReg and gradientReg to compute same as  
## costFunctionReg but needed to use optim R function (the equivalent fminunc 
## OCTAVE function)
## =============================================================================
        
        # Initialize some useful values
        m <- length(y)        # number of examples
        n <- length(theta)    # number of features + 1.
        
        # Sigmoid function
        sigmoid <- function(z) { 1/(1+exp(-z)) }
        
        # Variables needed to compute J and grad.
        z <- X %*% theta    # z(i) = theta0*x0(i) + theta1*x1(i) + ...
        h <- sigmoid(z)     # h(i) = h(X(i)) 
        
        # Cost functon J(theta) = j0 + regTerm
        j0 <- 1/m * sum( (-y*log(h)) - (1-y)*log(1-h) )
        jRegTerm <- lambda/(2*m) * sum(theta[2:n]^2)
        
        j <- j0 + jRegTerm
        
        # Gradient function grad = grad0 + gradRegTerm
        grad0 <- numeric()
        for (i in 1:n) {
                grad0[i] <- 1/m * sum( (h-y) * X[ ,i] )
        }
        
        gradRegTerm <- lambda/m * c(0, theta[2:n])   # gradRegTerm[1] = 0
        
        grad <- grad0 + gradRegTerm
        
        # Return values
        return_values <- list(J= j, Gradient= grad)
}

JcostFunctionReg <- function(theta, X, y,lambda) {
        
        # Initialize some useful values
        m <- length(y)        # number of examples
        n <- length(theta)    # number of features + 1.
        
        # Sigmoid function
        sigmoid <- function(z) { 1/(1+exp(-z)) }
        
        # Variables needed to compute J and grad.
        z <- X %*% theta
        h <- sigmoid(z)
        
        # Cost functon J(theta) = j0 + regTerm
        j0 <- 1/m * sum( (-y*log(h)) - (1-y)*log(1-h) )
        jRegTerm <- lambda/(2*m) * sum(theta[2:n]^2)
        
        j <- j0 + jRegTerm   
} 

gradientReg <- function(theta,X,y,lambda) {
        
        # Initialize some useful values
        m <- length(y)        # number of examples
        n <- length(theta)    # number of features + 1.        
        
        # Sigmoid function
        sigmoid <- function(z) { 1/(1+exp(-z)) }
        
        # Variables needed to compute J and grad.
        z <- X %*% theta
        h <- sigmoid(z)
        
        # Gradient function grad = grad0 + gradRegTerm
        grad0 <- numeric()
        for (i in 1:n) {
                grad0[i] <- 1/m * sum( (h-y) * X[ ,i] )
        }
        
        gradRegTerm <- lambda/m * c(0, theta[2:n])   # gradRegTerm[1] = 0
        
        grad <- grad0 + gradRegTerm
}

