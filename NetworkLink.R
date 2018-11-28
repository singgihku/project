

# Load package
library(networkD3)
library(data.table)
library(magrittr)
library(dplyr)

#rm(list=ls())
wd <- "D:/RA-FILES/Wholesale and Broadband Revenue Assurance/03New Business/TCASH/05_Current_Alert/SERVER_TCASH_ANALYTIC/NetworkLink/"
#wd <- "/home/rickygf/NBBRA/NetworkLink/"
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


debit_df_file <- filter(df_file,debit != 0 )
credit_df_file <- filter(df_file,credit != 0)
join_refnum <- merge(x = debit_df_file, y = credit_df_file, by.x = "order_id",by.y = "order_id")

#str(join_refnum)

data_load <- join_refnum[c("identity_id.x","identity_type.x", "identity_id.y","identity_type.y","debit.x")]
data_load <- na.omit(data_load)
colnames(data_load) <- c("source_name","source_group","target_name","target_group","amount")

str(data_load)

#=====choose unique related identity based on specific identity
#data_load[89,]

degree <- 1
identity_name <- c("202000000023120959")
list_acc <- c()
for (i in (1:degree)) {
  entity <- data_load %>%
    filter( source_name  %in% identity_name | target_name  %in% identity_name) #%>%
  identity_name <- unique(c(as.vector(entity$source_name),as.vector(entity$target_name)))
  list_acc <- c(list_acc,identity_name)
}
list_acc <- unique(list_acc)
str(list_acc)
#View(list_acc)
#=====Popolate transaction based on choosesn identity

data_net <- data_load %>%
  filter( source_name  %in% list_acc | target_name  %in% list_acc) %>% 
#filter( !(source_name  %in% c("sales_bang_tcash") | target_name  %in% c("sales_bang_tcash")))
filter( !(source_name  %in% c("201000000000013001") | target_name  %in% c("201000000000013001")))
data_net <- head(data_net,100)
summary(data_net)
#View(data_net)



#====Arrange Link -> source target value
library(dplyr)
link_uniq <- data_net %>%
  group_by(source_name, source_group,target_name,target_group) %>%
  summarize(COUNT = n(), AMOUNT = sum(amount))
head(link_uniq)
class(link_uniq)
link_uniq

#====Arrange Node -> name group size (index start from 0)

name_source <- link_uniq[c("source_name","source_group")]
colnames(name_source) <- c("name","group")
name_target <- link_uniq[c("target_name","target_group")]
colnames(name_target) <- c("name","group")
node_bind <- unique(rbind(name_source,name_target))
index <- as.data.frame(as.numeric(0:(nrow(node_bind)-1)))
colnames(index) <- "index"
head(node_bind)
head(index)
node_bind_index <- (cbind(index,node_bind))
node_bind_index

Network_Nodes <- as.data.frame(node_bind[c("name","group")])
Network_Nodes

#====JOIN Link Node to indexing Link
nrow(link_uniq)

link_uniq_dindex <-merge(x = link_uniq, y = node_bind_index, by.x = "source_name",by.y = "name")
head(link_uniq_dindex)
link_uniq_allindex <-merge(x = link_uniq_dindex, y = node_bind_index, by.x = "target_name",by.y = "name")
head(link_uniq_allindex)

Network_Links <- link_uniq_allindex[c("index.x","index.y","COUNT")]
colnames(Network_Links) <- c("source","target","value")
Network_Links

# Create graph with legend and varying node radius
forceNetwork(Links = Network_Links, Nodes = Network_Nodes, Source = "source",
             Target = "target", Value = "value", NodeID = "name",
             #Nodesize = "size",
             radiusCalculation = "Math.sqrt(d.nodesize)+6",
             Group = "group", opacity = 1, opacityNoHover = T,
             #width = 1000,
             #height = 700, 
             fontSize = 12,
             arrows = TRUE,
             linkDistance = 100,
             legend = TRUE,
             zoom = TRUE) %>% 
             saveNetwork(file = paste(wd,"forceNetworkLink.html",sep=""))





sankeyNetwork(Links = Network_Links, Nodes = Network_Nodes, Source = "source",
              Target = "target", Value = "value", NodeID = "name",
              units = "cnt_trx", fontSize = 12, nodeWidth = 30)%>% 
              saveNetwork(file = paste(wd,"sankeyNetworkLink.html",sep=""))




