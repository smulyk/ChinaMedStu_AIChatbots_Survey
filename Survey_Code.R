setwd("C:/Users/R/Desktop/survey")

#### library package ####

library(tableone)
library(readxl)
library(dplyr)
library(likert)
library(magrittr)
library(ggplot2)
library(reshape)
library(psych)
library(ggtree)
library(aplot)
library(ggthemes)


#### demographic ####

dat<-read_xlsx("./data_stu.xlsx","demo")
names(dat)
dput(names(m2))
allVars<-c("gender" , "age", "major", "highest_edu" ,"oversea_exp"  ,  "clinical_exp",  "research_exp"  ,
           "univer_region", "univer_level"   , "ever_use"  )
fvars<-c( "gender","major", "highest_edu" ,"oversea_exp"  ,  "clinical_exp",  "research_exp"  ,
          "univer_region", "univer_level", "ever_use"  )
Svytab1<-CreateTableOne(vars = allVars, 
                        strata ="ever_use",data =dat , factorVars = fvars)
table1<-print(Svytab1)



dat<-read_xlsx("./data_stu.xlsx","use_pattern")
names(dat)
dput(names(dat))
allVars<-c("most_often_use_AIchatbots","use_num1","often_use","exposue_way", "learn_way", "time_use", "freq_use" , 
           "use_for_main", "know_algorithm", "check_update", 
           "bring_chane", "china_vs_other")
fvars<-c( "most_often_use_AIchatbots", "use_num1","often_use","exposue_way", "learn_way", "time_use", "freq_use" , 
          "use_for_main", "know_algorithm", "check_update", 
          "bring_chane", "china_vs_other"  )
Svytab1<-CreateTableOne(vars = allVars,data =dat , factorVars = fvars,)
table1<-print(Svytab1)


d1<-read_xlsx("./data_stu.xlsx","718") %>%
  as.data.frame()




#### Likert Visualization for AI-chatbots use####

d1<-read_xlsx("./data_stu.xlsx") %>%
  as.data.frame()


### significance test
wilcox.test(writing_score~writing, data = d1)



df<-read_xlsx("./data_stu1.xlsx","study") ###clinical practice, research, eaducation



df<- df %>%
  mutate(negativeScore = case_when(
    response<3 ~ count,
    response==3 ~ count/2,
    response>3 ~ 0
  ))


df = df %>%
  group_by(question) %>%
  summarise(totalNegativeScore = sum(negativeScore)) %>%
  merge(df, by = "question")


df = df %>%
  group_by(question) %>%
  summarise(totalScore = sum(count)) %>%
  merge(df, by = "question") %>%
  mutate(PercentofTotal = count/totalScore) %>%
  mutate(LikertPercent = -totalNegativeScore/totalScore)

for (i in 1:nrow(df)) {
  df$startPercent[i] = ifelse(df$response[i]==1,
                              df$LikertPercent[i],
                              df$startPercent[i-1]+df$PercentofTotal[i-1])
}


df = df %>%
  mutate(endPercent = startPercent + PercentofTotal)



LikertChart = ggplot() +
  geom_linerange(data = df, aes(x = question, ymin = startPercent, ymax = endPercent,
                                color = as.factor(response)),size = 5,alpha = 0.9)+
  guides(color = guide_legend(override.aes = list(size = 1),keyheight = 1,keywidth = 1,title = NULL))+
  scale_color_manual(breaks = 5:1,
                     labels = rev(c("Very Negative","Negative", 
                                    "Neutral",
                                    "Positive","Very Positive")), 
                     values = c("tan3","tan1","grey85","steelblue1","steelblue3"))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c('Not use','Use','Medical knowledege',
                                                                     'Not use','Use','Research knowledge',
                                                                     'Not use','Use','Study assistance',
                                                                     'Not use','Use','Exam preparation'))+
  geom_text(aes(x = c(mean(c(1, 2)), mean(c(4, 5)),mean(c(7, 8)),mean(c(10, 11))),    
                y = 0.98,  
                label = c("**","**",'**',"ns")),  # add significance symbol according the results of  #line 52
            vjust = -0.5,    
            hjust = 0.5)


LikertChart


p1<-LikertChart+  
  geom_hline(aes(yintercept = 0),alpha = 0.5,size = 1)+
  scale_y_continuous(name = NULL, limits = c(-1,1),labels = scales::percent)+
  coord_flip()+
  theme_minimal()+
  labs(x="")+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10),
        legend.text = element_text(size=10),
        legend.title  = element_text(size=10),
        legend.position = "bottom"
  )


p1



###### Concerns on AI-chatbots use ######
df<-read_xlsx("./problem.xlsx","edu") 

df<- df %>%
  mutate(negativeScore = case_when(
    response<3 ~ count,
    response==3 ~ count/2,
    response>3 ~ 0
  ))


df = df %>%
  group_by(question) %>%
  summarise(totalNegativeScore = sum(negativeScore)) %>%
  merge(df, by = "question")

df = df %>%
  group_by(question) %>%
  summarise(totalScore = sum(count)) %>%
  merge(df, by = "question") %>%
  mutate(PercentofTotal = count/totalScore) %>%
  mutate(LikertPercent = -totalNegativeScore/totalScore)

for (i in 1:nrow(df)) {
  df$startPercent[i] = ifelse(df$response[i]==1,
                              df$LikertPercent[i],
                              df$startPercent[i-1]+df$PercentofTotal[i-1])
}


df = df %>%
  mutate(endPercent = startPercent + PercentofTotal)



LikertChart = ggplot() +
  geom_linerange(data = df, aes(x = question, ymin = startPercent, ymax = endPercent,color = as.factor(response)),size = 15,alpha = 0.9)+
  guides(color = guide_legend(override.aes = list(size = 10),keyheight =0.1,keywidth =0.1,title = NULL))+
  scale_color_manual(breaks = 5:1,
                     labels = rev(c("No need","Not Necessary", "Neutral","Necessary","Very Need")), 
                     values = c("tan3","tan1","grey85","steelblue1","steelblue3"))

LikertChart


LikertChart+  
  geom_hline(aes(yintercept = 0),alpha = 0.5,size = 1)+
  scale_y_continuous(name = NULL, limits = c(-1,1),labels = scales::percent)+
  scale_x_discrete(name = NULL, labels= c("fake","exam","self_study","education equality","critical thinking"))+
  coord_flip()+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10,face = "bold.italic"),
        axis.title = element_text(size=10,family = "serif"),
        axis.text = element_text(size=10,family ="serif" ),
        legend.text = element_text(size=10,family ="serif"),
        legend.title  = element_text(size=10,family ="serif"))


###combined likert
df<- df %>%
  mutate(negativeScore = case_when(
    response<3 ~ count,
    response==3 ~ count/2,
    response>3 ~ 0
  ))


df = df %>%
  group_by(question) %>%
  summarise(totalNegativeScore = sum(negativeScore)) %>%
  merge(df, by = "question")

df = df %>%
  group_by(question) %>%
  summarise(totalScore = sum(count)) %>%
  merge(df, by = "question") %>%
  mutate(PercentofTotal = count/totalScore) %>%
  mutate(LikertPercent = -totalNegativeScore/totalScore)

for (i in 1:nrow(df)) {
  df$startPercent[i] = ifelse(df$response[i]==1,
                              df$LikertPercent[i],
                              df$startPercent[i-1]+df$PercentofTotal[i-1])
}


df = df %>%
  mutate(endPercent = startPercent + PercentofTotal)

LikertChart = ggplot() +
  geom_linerange(data = df, aes(x = question, ymin = startPercent, ymax = endPercent,
                                color = as.factor(response)),size = 5,alpha = 0.9)+
  guides(color = guide_legend(override.aes = list(size = 1),keyheight = 1,keywidth = 1,title = NULL))+
  scale_color_manual(breaks = 5:1,
                     labels = rev(c("No need","Not Necessary", "Neutral","Necessary","Very Need")), 
                     values = c("tan3","tan1","grey85","steelblue1","steelblue3"))+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),labels = c('Responsibility','Fake/false information','Informed consent',
                                                                                    'Medical dispute',"Doctor's authority",'RESEARCH',
                                                                                    'Fake/false information','Academic fraud/misconduct','Authorship',
                                                                                    'Academic abuse','Creativity','gv',
                                                                                    'fake/false information','cheating','self-study ability',
                                                                                    'education equality','critical thinking'))
LikertChart


p1<-LikertChart+  
  geom_hline(aes(yintercept = 0),alpha = 0.5,size = 1)+
  scale_y_continuous(name = NULL, limits = c(-1,1),labels = scales::percent)+
  coord_flip()+
  theme_minimal()+
  labs(x="")+
  theme(panel.grid.minor.x = element_blank(),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size=10),
        axis.text = element_text(size=10 ),
        legend.text = element_text(size=10),
        legend.title  = element_text(size=10),
        legend.position = "bottom"
  )

p1

#### heatmap correlation ####


table1 <- read.table("c1.txt",header = T,sep="\t",row.names = 1,check.names = F)
table2 <- read.table("c2.txt",header = T,sep="\t",row.names = 1,check.names = F)


### make legends for Clinical practice, research and education

df2<-read_xlsx("data_stu1.xlsx","Sheet5")
df2$y<-factor(df2$y,levels = rev(df2$y))
p2<-ggplot(df2,aes(x=x,y=y))+
  geom_tile(aes(fill=group))+
  scale_x_continuous(expand = c(0,0))+
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y=element_text(face = "plain",colour = "black",size=11),
        legend.position = "left")+
  scale_fill_manual(values = c("#b08800","#e4ce00",
                               "#aec017"))+
  labs(fill= "Field")
p2


#calculate the correlation coefficient and p value
pp <- corr.test(table1,table2,method="pearson",adjust = "fdr")
cor <- pp$r 
pvalue <- pp$p

# make significance label 
display <- pvalue
l1 <- nrow(display);l2 <- ncol(display)
for(i in 1:l1){
  for(k in 1:l2){
    a <- as.numeric(display[i,k])
    if(a <= 0.001){
      a <- "***"
    }
    if( 0.001 < a && a <= 0.01){
      a <- "**"
    }
    if(0.01 < a && a < 0.05){
      a <- "*"
    }
    if(a >= 0.05){
      a <- ""
    }
    display[i,k] <- a
  }
}

heatmap <- melt(cor)
heatmap[,4] <- melt(pvalue)[,3]
heatmap[,5] <- melt(display)[,3]
names(heatmap) <- c("use pattern","response","cor","pvalue","display")

write.table (heatmap,file ="heatmap.xls", sep ="\t", row.names = F)  

### clustering and make the cluster tree
phr <- hclust(dist(cor)) %>% 
  ggtree(layout="rectangular", branch.length="none") 
phc <- hclust(dist(t(cor))) %>% ggtree() + layout_dendrogram()


### combine
pp <- ggplot(heatmap,aes(gene,sample,fill=cor)) + 
  geom_tile()+
  theme_minimal()+scale_fill_viridis_c()+
  geom_text(aes(label=display),size=5,color="white")+
  scale_fill_gradient2(low="#000081", high="#AF1A1F", mid="#FFFFFF")+ 
  scale_y_discrete(position="right")+xlab(NULL) + ylab(NULL)+
  theme(axis.text.x = element_text(angle = 0,hjust=0.8,vjust=0.6))+
  theme(axis.text.x=element_text(face = "plain",colour = "black",size=11))+ 
  theme(axis.text.y=element_text(face = "plain",colour = "black",size=11))+ 
  theme(legend.text=element_text(face="plain", colour = "black",size = 10))+
  labs(fill = "correlation")

p1<-pp %>% insert_top(phc, height=0.0) %>% insert_left(phr, width=0.0) %>%
  insert_left(p2,width = 0.05)
p1




