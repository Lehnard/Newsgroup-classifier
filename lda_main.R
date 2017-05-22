load("./data/DT_sample.RData")


library("data.table", quietly= TRUE)
library("stringr", quietly= TRUE)
library("tm", quietly= TRUE)
library("SnowballC", quietly= TRUE)
library("stringr", quietly = TRUE)
library("text2vec", quietly = TRUE)

# Source: VectorSource. Interprets each element of the vector x as a document.
corpus <- Corpus(VectorSource(DT_sample[,Content]))
# Matriz de Documentos(filas)-terminos(columnas) creada con package tm.

# Textos a minusculas
corpus <- tm_map(corpus, content_transformer(tolower))
# Etiqueta direcciones de email con regex y direcciones http o https.
corpus <- tm_map(corpus, content_transformer(gsub), pattern = '[^\\s]+@[^\\s]+\\.[a-z]{2,4}', 
                 replacement = ' labelemail ')
corpus <- tm_map(corpus, content_transformer(gsub), pattern = '(http|https)://[^\\s]*', 
                 replacement = ' labelhttp ')
# Con stopwords("SMART") quitamos palabras: don't, won't, they've,....
corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
# Quita puntuación.
corpus <- tm_map(corpus, removePunctuation)
# Quita numeros
corpus <- tm_map(corpus, removeNumbers)
# Quita palabras de nuestra lista "words"
words <- readLines("./words/mystopwords2.txt")
corpus <- tm_map(corpus, removeWords, words)
# Quita espacios
corpus <- tm_map(corpus, stripWhitespace)
# Hacer steming en todos los textos de corpus_muestra
corpus <- tm_map(corpus, stemDocument)

#Created LSA space as below
tdm <- TermDocumentMatrix(corpus)
tdm <- removeSparseTerms(tdm, sparse= 0.995)

dtm <- DocumentTermMatrix(corpus) 
dtm <- removeSparseTerms(dtm, sparse= 0.995)

#tokens = DT_sample$Content  %>% tolower %>% word_tokenizer
dt_corpus <-data.table(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
tokens = DT_sample$Content  %>% word_tokenizer

# turn off progressbar because it won't look nice in rmd
it = itoken(tokens, ids = DT_sample$id, progressbar = FALSE)

v = create_vocabulary(it) %>% prune_vocabulary(term_count_min = 10, 
                                               doc_proportion_max = 0.2)
vectorizer <- vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "lda_c")

if(FALSE) {
lda_model <-   LDA$new(n_topics = 250, 
                       vocabulary = v,
                       doc_topic_prior = 0.1, 
                       topic_word_prior = 0.01)

# document-topic matrix
doc_topic_distr <- lda_model$fit_transform(dtm, 
                                           n_iter = 1000, 
                                           convergence_tol = 0.01, 
                                           check_convergence_every_n = 10)

# word-topic matrix
word_topic_dist <- lda_model$get_word_vectors()

library(LDAvis)
library(servr)
lda_model$plot() #gráfico
}

#############################################
#############################################

# svm
source("./functionScripts/svm_classif.R")
source("./functionScripts/logist_classif.R")


lda_acc_test <- data.frame()

step <- 50
orig <- 300
fin <- 500
for(i in seq(orig, fin, by= step)) {
        
        cat("\nlda_svm_pred   topics= ", i,"...\n")
        
        lda_model <-   LDA$new(n_topics = i, 
                               vocabulary = v,
                               doc_topic_prior = 0.1, 
                               topic_word_prior = 0.01)
        
        # document-topic matrix
        doc_topic_distr <- lda_model$fit_transform(dtm, 
                                                   n_iter = 1000, 
                                                   convergence_tol = 0.01, 
                                                   check_convergence_every_n = 10)
        
        m <- as.DocumentTermMatrix(doc_topic_distr, weighting = weightTfIdf)
        doc_topic_distr_idf <- as.matrix(m)
        
        # particion Xtrain, Xtest, ytrain, ytest a partir de X e y.
        X <- doc_topic_distr_idf
        
        labels <- DT_sample$Tag
        
        # ordenamos alfabeticamente las categorias de newsgroup cuya posicion determina su 
        # valor numerico.
        names <- sort(unique(labels))
        
        y <- numeric(length(labels))
        for(j in 1:length(labels)){
                
                # obtenemos los valores numericos para y
                y[j] <- which(labels[j]==names)
        }
        
        partition <- floor(length(y)*0.8)
        
        # Creamos 2 subconjuntos ytrain, ytest
        ytrain <- y[1:partition]
        Xtrain <- X[1:partition, ]
        
        ytest <- y[-(1:partition)]
        Xtest <- X[-(1:partition), ]
        
        svm <- T
        if(svm) {
                lda_svm_pred <- svm_classif(Xtrain, ytrain, Xtest, ytest, kernel= "radial")
        
                # Accuracy test
                accu_svm_train <-  signif(sum(lda_svm_pred$train==ytrain)/length(ytrain), digits= 3) 
                accu_svm_test <-  signif(sum(lda_svm_pred$test==ytest)/length(ytest), digits= 3)
                cat("accu_svm_test= ", accu_svm_test, "\n")
        
                lda_acc_test[(i-(orig-step))/step, 1] <- i
                lda_acc_test[(i-(orig-step))/step, 2] <- accu_svm_train
                lda_acc_test[(i-(orig-step))/step, 3] <- accu_svm_test
        }
        
        logist <- F
        if(logist) {
                # parametros
                lambda <- 0    # parametro de regularizacion
                maxit <- 50    # numero maximo de iteraciones para el metodo optimizador.
        
                logist_pred <- logist_classif(Xtrain, ytrain, Xtest, ytest, lambda, maxit)
        
                # Accuracy test
                accu_logist_train <-  signif(sum(logist_pred$train ==ytrain)/length(ytrain), digits= 3)
                accu_logist_test <-  signif(sum(logist_pred$test ==ytest)/length(ytest), digits= 3)
        
                cat("accu_logist_train= ", accu_logist_train, "accu_logist_test= ",accu_logist_test, "\n")
                
                lda_acc_test[(i-(orig-step))/step, 1] <- i
                lda_acc_test[(i-(orig-step))/step, 2] <- accu_logist_train
                lda_acc_test[(i-(orig-step))/step, 3] <- accu_logist_test
        }
}
colnames(lda_acc_test) <- c("num.topics", "acc.train", "acc.test")

# m <- as.DocumentTermMatrix(doc_topic_distr, weighting = weightTfIdf)
# doc_topic_distr_idf <- as.matrix(m)
