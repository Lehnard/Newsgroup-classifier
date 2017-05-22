# Newsgroup-classifier
Maria Calvo, David Griñán, Jordi Aceiton 

## What is it?
This is a project to build a news classificator as an exercise to practice Machine learning classification problems.

## Data: 

**20_newsgroup**   

http://www.cs.cmu.edu/afs/cs/project/theo-20/www/data/news20.html

This data set is a collection of 20,000 messages, collected from 20 different netnews
newsgroups. One thousand messages from each of the twenty newsgroups were chosen at
random and partitioned by newsgroup name. 

The list of newsgroups from which the messages were chose is as follows:

alt.atheism   
talk.politics.guns   
talk.politics.mideast   
talk.politics.misc   
talk.religion.misc   
soc.religion.christian   
comp.sys.ibm.pc.hardware   
comp.graphics   
comp.os.mswindows.misc   
comp.sys.mac.hardware     
comp.windows.x   
rec.autos   
rec.motorcycles   
rec.sport.baseball    
rec.sport.hockey     
sci.crypt    
sci.electronics    
sci.space    
sci.med   
misc.forsale   

## Content

**build_DT.R**

Script to generate the dataset with document features: id, file, Tag and Content. 
                                
* id: document identificator.
* file: document file name.
* Tag: Should be called Group better. The news group where document belongs.
* Content: the document text content.

**classifica_main.R script**

The basic tool to:

* Generate (or load) the 1 < m < 19997 document dataset DT_sample from DT.
* Preprocess document text content.
* Create term-frecuency matrix weighted with inverse-document-frecuency (tf-idf).
* Split it into train and test set. 
* Apply training method to (Xtrain, ytrain):
    + SVM from R package e1071.
    + KSVM from R package kernlab.
    + Logistic regression "one vs all".

**lda_main.R**

The idea was to create a script to apply latent dirichlet allocation (lda) to make topic analysis. 
