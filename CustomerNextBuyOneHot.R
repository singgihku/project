

library(neuralnet)
library(ggplot2)
library(nnet)
library(dplyr)
library(reshape2)
library(data.table)
library(caret)
library(e1071)
library(BBmisc)
library(purrr)

#set.seed(50)
rm(list = ls())

#=================FUNCTION ENCODE/DECODE

myonehot_encode_auto <- function(data, delimiter= "Q") {
  if(!(apply(data, 2, is.numeric)[[1]]) ){
    column <- colnames(dplyr::select_if(data, negate(is.numeric)))
    out <- list()
    for (i in 1:length(column)) {
      col_encode <- class.ind(as.factor(paste(column[i],data[,column[i]],sep=delimiter)))
      ifelse(i==1,all_encode <- col_encode,all_encode <- cbind(all_encode,col_encode))
    }
    numeric <- dplyr::select_if(data, is.numeric)
    encode <- cbind(numeric,all_encode)
    out$encode <- encode
    out$column <- colnames(encode)
    out$delimiter <- delimiter
    return(out)
  } else {
    return(data)
  }
}


myonehot_encode_basedon_model <- function(data,nnmodel, delimiter= "Q") {
  #--- Create temprary matrix
  n <- nrow(data)
  m <- length(nnmodel$model.list$variables)
  raw_test_temp <- matrix(0, n, m)
  colnames(raw_test_temp) <- nnmodel$model.list$variables
  #--- encode new data
  raw_test_predictor <- myonehot_encode_auto(data, delimiter= delimiter)
  #--- update temporary matrix with encoded new data
  coltemp <- colnames(raw_test_temp)
  coltest <- colnames(raw_test_predictor$encode)
  for (i in intersect( coltemp,  coltest)) {
    raw_test_temp[,i] <- raw_test_predictor$encode[,i]
  }
  return(raw_test_temp)
}

myonehot_decode <- function(data, delimiter= "Q") {
  enc_colnames <- colnames(data)
  prefix <- vector()
  for (k in 1:length(enc_colnames)) {
    prefix[k] <- explode(enc_colnames[k], sep = delimiter)[1]
  }
  uniq_prefix <- unique(prefix)
  
  for (l in 1 : length(uniq_prefix)){
    df <- as.data.frame(data) %>% select(starts_with(uniq_prefix[l]))
    decoded <- as.data.frame(colnames(df)[apply(df,1,which.max)])
    decoded <- as.data.frame(apply(decoded,1:2, function(x) {explode(x, sep = delimiter)[2]}))
    colnames(decoded) <- uniq_prefix[l]
    ifelse(l==1,all_decode <- decoded,all_decode <- cbind(all_decode,decoded))
  }
  return(all_decode)
}


#===========================START PROGRAM

wd <- "D:/RA-FILES/Wholesale and Broadband Revenue Assurance/03New Business/TCASH/05_Current_Alert/SERVER_TCASH_ANALYTIC/CustomerNextBuy/"
#wd <- "/home/rickygf/NBBRA/CustomerNextBuy/"
setwd(wd)
options(scipen = 999)

#yesterday_filedate <- paste("logs/tcash_revas_",format(Sys.Date()-1, "%Y%m%d"),".txt",sep="") 
#yesterday_filedate <- "/home/rickygf/NBBRA/RuleBaseAlert/logs/tcash_revas_20181011.txt"
yesterday_filedate <- "D:/RA-FILES/Wholesale and Broadband Revenue Assurance/03New Business/TCASH/05_Current_Alert/logs/tcash_revas_20181011.txt"

#yesterday <- Sys.Date()-1
yesterday <- "2018-10-11"

sDate <- yesterday
eDate <- yesterday
sDateTime <- paste(sDate,"00:00:00",sep=" ")
eDateTime <- paste(eDate,"23:59:59",sep=" ")


#====Load  data
df_file <- read.csv(yesterday_filedate,header=F,sep="|",colClasses=c("character","numeric",rep("character",4),rep("numeric",3),"character"))   
colnames(df_file) <- c("trx_date","trx_id","order_id","identity_id","identity_desc","identity_type","credit","debit","balance","trx_type_desc")
df_file$trx_date <- as.POSIXct(df_file$trx_date, format="%d%m%Y %H:%M:%S")
df_file <- df_file %>% 
  filter(trx_date >= as.POSIXct(sDateTime) & trx_date <= as.POSIXct(eDateTime)) %>% 
  arrange(trx_date)

#summary(df_file)

#===Data formatting


debit_df_file <- filter(df_file,debit != 0 & identity_type == "1000")
credit_df_file <- filter(df_file,credit != 0 & identity_type == "5000")
join_refnum <- merge(x = debit_df_file, y = credit_df_file, by.x = "order_id",by.y = "order_id")

#str(join_refnum)

trx <- join_refnum %>% 
  select(trx_date.x,identity_id.x,identity_id.y) %>% 
  #salah filter(between(msg.Date.Timestamp, as.Date("2018-10-04"),as.Date("2018-10-05"))) %>% #dari 2018-10-03 00:00:00 s/d 2018-10-03 23:59:59
  #filter(msg.Date.Timestamp >= as.POSIXct("2018-10-04 00:00:00") & msg.Date.Timestamp <= as.POSIXct("2018-10-04 23:59:59"))  %>%    
  #filter(grepl("^[[:digit:]]+$", msg.DebitShortCode)) %>% #ambil yang numeric saja
  #filter(!grepl("^[[:digit:]]+$", msg.CreditMSISDN)) %>%  #ambil yang mengandung alphabet saja
  arrange(identity_id.x,trx_date.x) %>% 
  distinct(trx_date.x,identity_id.x,identity_id.y)

trx$next_identity_id.y <- with(trx,ifelse(identity_id.x==lead(identity_id.x),lead(identity_id.y),NA))
trx <- trx[c("identity_id.x","identity_id.y","next_identity_id.y")]
trx <- filter(trx, identity_id.y != next_identity_id.y )
trx <- na.omit(trx)
colnames(trx) <- c("ACC_NO_DEBET","ACC_NO_CREDIT","NEXT_ACC_NO_CREDIT")

trx <- trx[sample(nrow(trx), 100), ]




#====One hot encoding

predictor <- myonehot_encode_auto(trx[c("ACC_NO_DEBET","ACC_NO_CREDIT")], delimiter= "Q")
target <- myonehot_encode_auto(trx[c("NEXT_ACC_NO_CREDIT")], delimiter= "Q")
pre_process_trx_df <- cbind(predictor$encode,target$encode)




#====Create formula
text_pred <- ""
text_pred <- paste(predictor$column, collapse=" + ")
text_labe <- ""
text_labe <- paste(target$column, collapse=" + ")
text_formula <- ""
text_formula <- paste(text_labe," ~ ",text_pred,sep="")
f <- as.formula(text_formula)


#======CREATE MODEL
#Rule of thumb number of hidden neurons
#Nh = Ns/(alpha*(Ni + No))
#Ni  = number of input neurons.
#No = number of output neurons.
#Ns = number of samples in training data set.
#?? = an arbitrary scaling factor usually 2-10.

layer_node <- c(4,2)
trx_net <- neuralnet(f, data = pre_process_trx_df,hidden = layer_node, act.fct = "tanh", linear.output = FALSE,stepmax = 1e+05, rep = 1)
plot(trx_net)
#trx_net$model.list$variables
#trx_net$model.list$response


#======Model accuracy check
trx_preds <- neuralnet::compute(trx_net, predictor$encode)
origi_vals <- max.col(target$encode)
pr.nn_2 <- max.col(trx_preds$net.result)
print(paste("Model Accuracy: ", round(mean(pr.nn_2==origi_vals)*100, 2), "%.", sep = ""))




#=====Test prediction multirow data frame

raw_test <- trx[1:100,1:2]
raw_test_label <- trx[1:100,3]


#----encode test data
#cl1 <-apply(raw_test[1],2, function(x){x<- paste(colnames(raw_test[1]),x,sep="Q")})
#cl2 <-apply(raw_test[2],2, function(x){x<- paste(colnames(raw_test[2]),x,sep="Q")})
#cl<- cbind(cl1,cl2) 
#n <- nrow(cl)
#m <- length(trx_net$model.list$variables)
#test_data <- matrix(0, n, m)
#colnames(test_data) <- trx_net$model.list$variables
#test_data[cbind(seq(nrow(cl)),match(cl[,1],colnames(test_data[,1:m])))] <- 1
#test_data[cbind(seq(nrow(cl)),match(cl[,2],colnames(test_data[,1:m])))] <- 1
#test_data <- as.data.frame(test_data)


#--- Encode new data
raw_test_encode <- myonehot_encode_basedon_model(raw_test,trx_net,delimiter="Q")
#----compute test
new_out2 <- neuralnet::compute(trx_net,raw_test_encode)


#----prediction result test
hasil_df2 <- as.data.frame(new_out2$net.result)
colnames(hasil_df2) <- trx_net$model.list$response

hasil_decode <- myonehot_decode(hasil_df2, delimiter= "Q") 
source_test <- trx[1:100,1:3]
table_predict <- cbind(source_test,hasil_decode)



#----Model accuracy check

print(paste("Model Accuracy: ", round(mean(table_predict[3]==table_predict[4])*100, 2), "%.", sep = ""))

#----Save model
#saveRDS(trx_net, paste(wd,"nextbuy_model.rds",sep=""))


#load model and test
#my_model <- readRDS(paste(wd,"nextbuy_model.rds",sep=""))
