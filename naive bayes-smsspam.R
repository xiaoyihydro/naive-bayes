sms_raw<-read.table("C:/Users/Elaine/Desktop/SMSSpam.txt",stringsAsFactors=FALSE,sep="\t",header = F,comment="",quote=NULL)
str(sms_raw)
names(sms_raw)<-c("type","text")
str(sms_raw)

#Change the 'type' cahracter vector into factor,��Ҷ˹Ҫ��Ŀ�����Ϊfactor#
sms_raw$type<-factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

#data preparation-cleaning and standardizing text data#
install.packages("tm")
library(tm)

#��ԭʼ�����еĶ���Ϣ����Ϊ�����������������Ͽ�
sms_corpus<-VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)

#To receive a summary of specific messages#
inspect(sms_corpus[1:2])

#To view the actual message text##To view one message#
as.character(sms_corpus[[1]])

#Use lapply()command to apply as.character() to a subset of corpus element
lapply(sms_corpus[1:2],as.character)


#Clean the text

sms_corpus_clean<-tm_map(sms_corpus,tolower)#�����д�ת��ΪСд��ĸ#
as.character(sms_corpus_clean[[1]])
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)#ȥ������#
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords()) #ȥ��ֹͣ�ʣ�to,and, but and or#
sms_corpus_clean<-tm_map(sms_corpus_clean,removePunctuation)#ȥ�����#
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)#ȥ���ո�#
sms_corpus_clean<-tm_map(sms_corpus_clean,PlainTextDocument)

#Data preparation-splitting text document into words#
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
sms_dtm

#Data preparation-creating training and test dataset
sms_dtm_train<-sms_dtm[1:4176,]
sms_dtm_test<-sms_dtm[4177:5568,]
sms_train_labels<-sms_raw[1:4176,]$type
sms_test_lables<-sms_raw[4177:5568,]$type
prop.table(table(sms_train_labels)) #�г���Ե����Ƶ��#
prop.table(table(sms_test_lables))

#Visualizing text data-word clouds#
#Word cloud is a way to visually depict the frequency at which words appear in text data#
library(worcloud)
wordcloud(sms_corpus_clean,min.freq=50,random.order = F)

#create a subset where the message type is spam#
spam<-subset(sms_raw,type=="spam")
ham<-subset(sms_raw,type=="ham")

wordcloud(spam$text,max.words=40,scale=c(3,0.5))
wordcloud(ham$text,max.words=40,scale = c(3,0.5))

#Data preparation-creating indicator features for frequent words##��С������Χ#
#less than 0.1% of records in the training data
findFreqTerms(sms_dtm_train,5) #elimate any words that appear in less than five SMS messages
sms_freq_words<-findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)

#limit the DTM to speific words
sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]

#ͨ�����������л�����û��ĳ�������ж��Ƿ����������ţ�
#��ô���Ǻ���ȻӦ��ʹ�õľ����Ǳ��ĳ������ĳ�������г����˻���û�г���
convert_counts<-function(x){
  x<-ifelse(x>0,"Yes","No")
}

#Convert the training and test matrics##��ÿһ�ж����������Ĵ���#
#MARGIN=1 is used for rows,MARGIN=2 is used for columns#
sms_train<-apply(sms_dtm_freq_train,MARGIN = 2,convert_counts)
sms_test<-apply(sms_dtm_freq_test,MARGIN = 2,convert_counts)

#Training a model on the data#
install.packages("e1071")
library(e1071)
sms_classifier<-naiveBayes(sms_train,sms_train_labels)

#Evaluating model performance#
sms_test_pred<-predict(sms_classifier,sms_test) #��ģ�ͽ���Ԥ��#

#To compare the predictions to the true values#
install.packages("gmodels")
library(gmodels)
CrossTable(sms_test_pred,sms_test_lables,
           prop.chisq = F,prop.c = F,
           dnn=c('predicted','actual'))