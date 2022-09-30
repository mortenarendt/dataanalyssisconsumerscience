library(tidyverse) 
library(caret)
# X <- rio::import('~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/Rådata_B1.xlsx' , skip = 1)
X2 <- rio::import('~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/Results_Ferm_rugbrød_B1.xlsx', dec = '.')
cata <- rio::import('~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/CATARyeBread.xlsx')
# remove timpstamp
cata <- cata %>% select(-Q1_1__Liking_Time_Stamp, -Q7___Time_Stamp,-Q2___Time_Stamp,-Q3___Time_Stamp,-Q4___Time_Stamp,-Q5___Time_Stamp,-Q6___Time_Stamp)

X2 <- X2 %>% 
  mutate(bread = ProductName %>% substr(1,7),
         rep = ProductName %>% substr(9,9)) 
X2[,9:30] <- X2[,9:30] %>% mutate_if(is.character,as.numeric)

g3 <- X2 %>% 
  gather(attrib,val,L_Braendt:ES_S_Br誅d) %>% 
  group_by(attrib, bread,CJ) %>% 
  dplyr::summarise(val = mean(val)) %>% 
  ggplot(data = ., aes(paste('B',substr(bread,7,7)),val, group = CJ, color = CJ)) + 
  geom_point() + geom_line() + facet_wrap(~attrib, nrow = 4) + 
  theme_bw() + theme(legend.position = 'bottom') + xlab('Bread') + ylab('Mean response')

g4 <- X2 %>% 
  gather(attrib,val,L_Braendt:ES_S_Br誅d) %>% 
  group_by(attrib, bread,CJ) %>% 
  dplyr::summarise(val = sd(val)) %>% 
  ggplot(data = ., aes(paste('B',substr(bread,7,7)),val, group = CJ, color = CJ)) + 
  geom_point() + geom_line() + facet_wrap(~attrib, nrow = 4) + 
  theme_bw() + theme(legend.position = 'bottom') + xlab('Bread') + ylab('SD response')

X2 <- X2 %>% 
  filter(CJ!='Boks 2')
PCAmdl <- prcomp(X2[,9:30], scale. = T)  
g1 <- ggbiplot::ggbiplot(PCAmdl, groups = X2$bread, ellipse = T)  + theme_bw()
g2 <- ggbiplot::ggbiplot(PCAmdl, groups = X2$CJ, ellipse = T)  + theme_bw()


fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)

xx <- cata %>% select(Q1_1__Liking,Q2__1__Dark:Q7__4__Burned)
plsmdl <- train(Q1_1__Liking ~ ., data = xx, 
                 method = "pls", 
                 trControl = fitControl,
                preProc  = c("center", "scale"),
                 verbose = FALSE)
scores <- plsmdl$finalModel$scores %>% as.data.frame()

  
library(pls)
CATAlik <- list()
CATAlik$CATA <- scale(as.matrix(xx[,-1]))
CATAlik$Liking <- scale(xx$Q1_1__Liking)
rownames(CATAlik$Liking) <- rownames(CATAlik$CATA) <- paste(cata$Sample_Name,cata$Panelist_Code)
catalik.pls <- plsr(Liking ~ CATA, ncomp = 2, data = CATAlik, validation = "LOO")
corrplot(catalik.pls, labels = cata$Sample_Name)
scores <- catalik.pls$scores %>% unclass %>% data.frame %>% cbind(cata)
loads <- catalik.pls$loadings %>% unclass() %>% data.frame() %>% 
  rownames_to_column('var') %>% 
  mutate(attrib = var %>% substr(8,30)) %>% 
  rbind(catalik.pls$Yloadings %>% unclass() %>% data.frame() %>% rownames_to_column('var') %>% mutate(attrib = var) )


gA <- ggplot(data = scores, aes(Comp.1,Comp.2, color = Sample_Name)) + 
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  geom_point() + stat_ellipse() + theme_bw() +
  geom_line(aes(group = Panelist_Code), color = 'grey70') + 
  theme(legend.position = 'bottom')

gB <- ggplot(data = loads, aes(Comp.1,Comp.2,label = attrib)) + 
  # geom_point() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + 
  geom_text() + theme_bw()

library(patchwork)
ggsave('~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/ryebread_CATAplsLiking.pdf', gA + gB, height = 7, width = 13)
ggsave('~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/ryebread_byjudge.pdf', g2)
ggsave('~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/ryebread_byproduct.pdf', g1)

ggsave('~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/ryebread_attrib_by_product_mean.pdf', g3, height = 7, width = 12)

ggsave('~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/ryebread_attrib_by_product_sd.pdf', g4, height = 7, width = 12)
