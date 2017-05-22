create_DT_sample <- function(num_docs, seed= 1234, type= "uniform") {

#########################################################################################                         
##                                                                                      #
##  Autores: Maria Calvo             Fecha: 22/05/2017                                  #
##           David Grinan                                                               #
##           Jordi Aceiton                                                              #
##                                                                                      #
##                                                                                      #
##  create_DT_sample(num_docs, seed= 1234)                                              #
##                                                                                      #
##      Crea un subconjunto del dataset DT.RData (documentos-Content|File|Tag|id)       #
##      con "num_docs" documentos escogidos al azar de manera que cada grupo            #
##      de "Tag" se vea representado de la manera mas uniforme posible.                 #
##      Para ello repite "iter" sampleados de DT i se queda con el que                  #
##      tenga menor "desvRelativa = desv/med" que pasa a ser DT_sample. Este            #
##      se guarda en disco en "./data/DT_sample.RData".                                 #
##                                                                                      #
##                                                                                      #
##      input:                                                                          #
##                                                                                      #
##              num_docs -- El numero de documentos que tomara de DT                    #
##                          para hacer el subconjunto DT_sample                         #
##                                                                                      #
##                                                                                      #
##              seed -- seed= 1234 por defecto. Es la semilla para                      #
##                      el sampleado aleatorio.                                         #
##                                                                                      #
##              type -- puede ser "uniform" o "min_desv". El primer tipo                #
##                      hace un sampleado de la muestra tomando el mismo                #
##                      numero de documentos de cada grupo.                             #
##                      El segundo tipo hace "iter" sammpleados y se queda              #
##                      con el candidato con menor desviacion relativa                  #
##                      respecto de la media, es decir el mas equitativamente           #
##                      representado posible.                                           #
##                                                                                      #
##                                                                                      #
##      output:                                                                         #
##                                                                                      #
##              DT_sample -- Subconjunto aleatorio de "num_docs" documentos             #
##                           de DT de manera que contenga documentos de                 #
##                           cada grupo lo mas uniformemente representados              #
##                           posible.                                                   #
##                                                                                      #
#########################################################################################        
                
        library("data.table")
        
        cat("\nGenerando muestra aleatoria de documentos... ")   
                
        # Cargando el dataset DT con todos los documentos.
        load("./data/DT.RData")
        
        # Semilla para el sampleado aleatorio.
        set.seed(seed)         

# =============================================================================        
# SAMPLEADO POR DESVIACION RELATIVA MÍNIMA RESPECTO A LA MEDIA.         
# =============================================================================                
        if (type=="min_desv") { 
                
                ## PARAMETROS INTERNOS DE LA FUNCION
                desvRelativa <- -1      # Inicializacion de variable. Valor negativo entra al "for".
                intentos <- 10          # iteraciones que se samplea para escoger el mejor 
                                        # subconjunto aleatorio.
        
                ## Subset de documentos.
                # Buscamos muestra con un sampleado de casos en que el numero de textos tomados 
                # de cada newsgroup sea lo mas parecido posible.
                for(i in 1:intentos) {
                
                        # Tomamos candidato aleatorio(subconjunto de num_docs documentos) de DT. 
                        index <- sample(1:nrow(DT), num_docs, replace= F)
                        candidato <- DT[index, ]  
                
                        # "candidato" recoge cierto numero de documentos de cada newsgroup, que
                        # es aleatorio. Se busca candidato con la menor desviacion relativa, es
                        # decir, donde el numero de documentos en cada newsgroup es más 
                        # equitativo.
                        # funcionamiento: candidato[ ,Tag] es el vector Tag de candidato.
                        # Si hacemos table(candidato[,Tag]) numera cuantos elementos hay
                        # de cada tipo en Tag en un vector. Finalmente se usa sd i med.
                        desv <- sd(table(candidato[ ,Tag]))
                        med <- mean(table(candidato[ ,Tag]))
                
                        # Actualizacion a valores mas optimos.
                        if(desvRelativa > desv/med | desvRelativa < 0) {
                        
                                # Reasignamos la mejor desviacion relativa hasta el momento
                                desvRelativa <- desv/med
                        
                                # Mejor candidato hasta el momento asignado a muestra.
                                DT_sample <- candidato
                        }#if
                }#for

# =============================================================================        
# SAMPLEADO UNIFORME.         
# =============================================================================                         
        } else if (type=="uniform")  {
                
                ## PARAMETROS INTERNOS DE LA DUNCION
                newsgroups <- 20        # Numero de grupos 
                
                ## Subset de documentos.
                # Buscamos muestra con un sampleado de casos en que el numero de  
                # textos tomados de cada newsgroup sea el mismo.
                
                # Etiquetas a recorrer
                tags <- DT[,Tag]
                nombres <- levels(as.factor(tags))
                
                # Vector final de indices
                indices <- c()
                # Numero de documentos a tomar de cada tema
                cuantos <- floor(num_docs/newsgroups)
                
                # Hay temas con menos de 1000 docs. Entonces:
                if(cuantos > 997) { cuantos <- 997  }
                
                # Bucle por etiquetas
                for(i in 1:newsgroups) {
                        
                        # Seccion de DT tomada
                        secc <- DT[Tag==nombres[i]]
                        # Muestra aleatoria de ese tema
                        index <- sample(1:nrow(secc), cuantos)
                        # Traduccion de los indices para que se adecuen a DT
                        offset <- min(which(DT[,Tag]==nombres[i])) - 1
                        index <- index+offset
                        # Anadido al vector de indices
                        indices <- c(indices,index)
                }#for
                
                # crear la muestra
                indices <- sample(indices, length(indices), replace= FALSE)
                DT_sample <- DT[indices, ] 
                
        } else { stop("\ntype has to be \"uniform\" or \"min_desv\".")}

        # Guardamos objeto DT_sample a fichero DT_sample.RData
        save(DT_sample, file= "./data/DT_sample.RData")
        
        # Permite ver el numero de documentos por grupo.
        # table(DT_sample$Tag)
        
        cat(" ok!\n")
        return(DT_sample)      
}