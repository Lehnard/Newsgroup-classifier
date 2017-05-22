##########################################################################################      
#
#  built_DT.R  Script.
#
#  Construye el dataset DT que contiene 4 variables para cada documento text que son
#  id (identificador del documento), file (nombre del documento), group (grupo de noticias
#  al que pertenece) y content (contenido de texto)
#
#  Autores:     Maria Calvo
#               David Grinan
#               Jordi Aceiton
#
#  Fecha:       22/05/2017
#
#########################################################################################


library(data.table)

# Guardamos los paths de cada uno de los archivos de texto.
files_path <- list.files("./texts_newsgroup", recursive= TRUE, full.names= TRUE)

# Guardamos el nombre de los archivos y grupos como "grupo/archivo" para luego romperlos
# por "/" y obtener el grupo i el nombre por separado de cada documento.
files <- list.files("./texts_newsgroup", recursive= TRUE, full.names= FALSE)

splitted <- strsplit(files, split= "/")

files_group <-  as.character(lapply(splitted, '[[', 1))
files_name <-  as.character(lapply(splitted, '[[', 2))
files_number <- length(files_path) 


# Cargamos los textos en una lista con la ayuda de lapply(). readLines() genera un 
# vector de caracteres donde cada componente es una linea del documento.
files_text <- lapply(files_path, readLines)

# Para separar parte metadatos del contenido usamos la primera linea en blanco como
# separador. 
meta_sep_line <- lapply(files_text, match, x= "")

for(i in 1:files_number) {
        
        files_text[[i]] <- files_text[[i]][-(1:meta_sep_line[[i]]) ]
}

# Juntamos todas las componentes del vector de caracteres de cada documento.
files_text <- lapply(files_text, paste, collapse= " ")

# Generamos DT como un datatable del paquete data.table que hereda de data.frame.
DT <- data.table(id= 1:files_number, 
                 file= files_name, 
                 group= files_group,
                 content= files_text)

# Opcion para guardar el DT generado.
if(FALSE) {save(DT, file="./data/DT.RData")}

# Limpieza de objetos intermedios.
rm(files, splitted, i, meta_sep_line, files_path, files_text, files_group, files_name, files_number, text)
