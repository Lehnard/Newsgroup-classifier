process_texts <- function(texts_vector, stem= FALSE) {

#########################################################################################                         
##                                                                                      #
##  Autores: Maria Calvo             Fecha: 22/05/2017                                  #
##           David Grinan                                                               #
##           Jordi Aceiton                                                              #
##                                                                                      #
##                                                                                      #
##  process_texts(texts_vector)                                                         #
##                                                                                      #
##      Convierte un vector de textos (character vector de strings) en una              #
##      estructura tipo VCorpus del paquete "tm" y preprocesa su texto con:             #
##                                                                                      #
##              - textos a minusculas.                                                  #
##              - etiqueta direcciones de email y direcciones http o https.             #
##              - tratamiento para caracteres especiales.                               #  
##              - quita palabras de stopwords("SMART").                                 #        
##              - quita la puntuación.                                                  #
##              - quita numeros.                                                        #
##              - quita palabras de un diccionario propio (mywords.txt)                 #
##              - Lematiza (stem) si stem= TRUE.                                        #
##                                                                                      #
##      devuelve una estructura tipo VCorpus con el preprocesado de textos.             #
##                                                                                      #
##      input:                                                                          #
##                                                                                      #
##              texts_vector -- un vector de textos. Character vector con               #
##                              "strings" en cada componente. Sera el                   #
##                              argumento de VectorSource() del paquete                 #
##                              "tm".                                                   #
##                                                                                      #
##              stem -- stem= FALSE por defecto. Activa, o no, el stemizado             #             
##                      del paquete "tm".                                               #
##                                                                                      #
##      output:                                                                         #
##                                                                                      #
##              corpus -- El corpus generado con "texts_vector", stemizado              #
##                        o no (stem= TRUE/FALSE) con el preprocesado de                #
##                        texto especificado.                                           #
##                                                                                      #
##                                                                                      #
#########################################################################################          
        
        library("tm")
        
        cat("\nProcesando textos... ")
        
        # Declara "corpus", una estructura Corpus de tm package, con 
        # "texts_vector".
        corpus <- VCorpus(VectorSource(texts_vector))
        
        ## Procesado de texto
        
        # Textos a minusculas
        corpus <- tm_map(corpus, content_transformer(tolower))
        # Etiqueta direcciones de email con regex y direcciones http o https.
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[^\\s]+@[^\\s]+\\.[a-z]{2,4}', 
                         replacement = ' ')
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = '(http|https)://[^\\s]*', 
                         replacement = ' ',  perl= TRUE)
        
        # Tratamiento para caracteres especiales:
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[-]+', replacement= ' ')
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\u2014", replacement= ' ')
        corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\u2019", replacement= "\'")
        
        
        corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
        # Quita puntuación.  
        corpus <- tm_map(corpus, removePunctuation)
        # Quita numeros
        corpus <- tm_map(corpus, removeNumbers)
        
        # Quita palabras de nuestra lista "mywords"
        mywords <- readLines("./words/mywords.txt")
        corpus <- tm_map(corpus, removeWords, mywords)
        
        # Quita espacios
        corpus <- tm_map(corpus, stripWhitespace)
        
        
        cat("ok!\n")
        
        # Stemizado.
        if(stem) {
                cat("Proceso de lematizacion (stem)... ")
                corpus <- tm_map(corpus, stemDocument)
                cat("ok!\n")
        }
       
        return(corpus)
}