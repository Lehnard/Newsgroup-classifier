X_y_dataset <- function(corpus, labels, tfidf= TRUE, with.normalize= FALSE,
                        word.length= c(2,15), freq.bounds= c(5, Inf),
                        bounds= c(1,Inf), with.sparse= TRUE, sparse= 0.95) {
 
#########################################################################################                         
##                                                                                      #
##  Autores: Maria Calvo             Fecha: 22/05/2017                                  #
##           David Grinan                                                               #
##           Jordi Aceiton                                                              #
##                                                                                      #
##                                                                                      #
##  X_y_dataset(corpus, labels, tfidf= TRUE, with.normalize= FALSE,                     #
##              word.length= c(2,15), freq.bounds= c(5, Inf),                           #
##              bounds= c(1,Inf), with.sparse= TRUE, sparse= 0.95)                      #
##                                                                                      #
##      Genera y guarda la matriz documentos-terminos (con tfidf si TRUE o              #
##      tf si FALSE) que sera X a partir de "corpus".                                   #
##                                                                                      #
##      Genera y guarda, el vector "y" de etiquetas numericas para assignar             #
##      grupo a cada documento de "X" a partir de labels. Asi, alt.atheism <-> 1,       #
##      comp.graphics <-> 2, etc.                                                       #
##                                                                                      #
##      devuelve una estructura tipo matriz, X, un vector y y los pesos idf.            #
##                                                                                      #
##      input:                                                                          #
##                                                                                      #
##              corpus -- estructura corpus de paquete "tm" del que se                  #
##                        generara estructura dtm (document-term-matrix)                #
##                        del paquete "tm" para luego hacer conversion                  #
##                        forzosa a matrix "X".                                         #
##                                                                                      #
##              labels -- Vector de etiquetas de pertenencia de los                     #
##                        documentos a las distintas classes (newsgroups).              #
##                                                                                      #
##              new -- TRUE para generar y guardar nueva X e y. FALSE                   #
##                     para cargar version guardada.                                    #
##                                                                                      #
##              tfidf -- X matriz tf o tf-idf (TRUE/FALSE).                             #
##                                                                                      #
##              with.normalize -- TRUE para normalizar matriz tf-idf "X".               #       
##                                                                                      #
##              word.length -- numeric(2), 1a comp.= min.letras, 2a comp.= max.letras.  #
##                                                                                      #
##              freq.bounds -- numeric(2), 1a comp.= min.freq, 2a comp.= max.freq.      #
##                                                                                      #
##              with.sparse -- con reduccion de terminos por sparse (TRUE               #
##                             FALSE)                                                   #
##                                                                                      #
##              sparse -- en caso with.sparse= TRUE, valor de sparse.                   #
##                                                                                      #
##      output:                                                                         #
##                                                                                      #
##              Xy -- lista con los resultados X, y e idf.                              #
##                                                                                      #
##                                                                                      #
#########################################################################################        

        library(tm)
        
        cat("\nGenerando matriz terminos-documentos X y vector y... ")  
        
        # Matriz de terminos-documentos creada con package tm. Filtro de longitud 
        # de palabras "word.length" y  filtrado por ocurrencias "freq.bounds".
        dtm <- DocumentTermMatrix(corpus, 
                             list(wordLengths= word.length, bounds= list(global= freq.bounds)))
        
        # Quita terminos con Sparse > sparse = num de docs donde no 
        # aparece termino/total docs.
        if(with.sparse) { dtm <- removeSparseTerms(dtm, sparse= sparse) }
        
        # Conversion de matriz document-term de paquete "tm" a matriz de "base". 
        dt <- as.matrix(dtm)
        
        
        # Crea matriz tf-idf
        idf <- numeric() # en caso que no se pide tfidf= TRUE, la funcion devuelve numeric(0).
        if(tfidf) {
                
                # vector de pesos "inverse-document-frecuency".
                idf <- log(nrow(dt)/(colSums(dt!=0))+1) 
                
                # Matriz tf-idf.
                dtidf <- t(t(dt)*idf)
                
                # aplica normalizacion.
                if(with.normalize) {
                        dtidf <- dtidf/sqrt(rowSums(dtidf^2))
                        
                        # Documentos vacíos aparecen NaN al dividir por 0.
                        dtidf[is.nan(dtidf)] <- 0  # asignamos 0 a los NaN. 
                }
        }
        
        # Generamos vector de clases, asignando etiqueta numerica a los grupos.
        names <- sort(unique(labels))
        
        y <- numeric(length(labels))
        for(i in 1:length(labels)){
                # obtenemos los valores numericos para y
                y[i] <- which(labels[i]==names)
        }
        
        # Asigna a X la dt o la dt con idf.
        if (tfidf) {X <- dtidf} else {X <- dt}
        
        # Generamos el objeto lista de retorno con X e y.
        Xy <- list(X= X, y= y, idf= idf)
        
        # Guarda.
        save(Xy, file= "./data/Xy.RData")
        
        cat("ok!\n")
        return(Xy)
}