rm(list = ls()) 
library(tidyverse)
#Import CSV, find directory and then copy to command
# file.choose() #find "URL" link in the console

# X <- read.csv2('H:\\From_SCIENCE\\Documents\\Fundraising\\FermFoods\\Data\\Results_Ferm_rugbrød_B1.csv')
X <- read.csv2('~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/Results_Ferm_rugbrød_B1.csv')
# head(X)
# View(X)
# str(X)

# install.packages("tidyverse")
#: Table mean SD
library(ggplot2)

## Istedet for hver enkel, kan denne her benyttes: 
X[,7:28] <- X[,7:28] %>% 
  mutate_if(is.character, as.numeric)
X <- X %>% 
  mutate(Rep  = Rep %>% as.factor, 
         Product = Product %>% as.factor)


# X$Product<-as.factor(X$Product)
# X$L_Braendt<-as.numeric(X$L_Braendt)
# X$L_Malt<-as.numeric(X$L_Malt)
# X$L_Surdej<-as.numeric(X$L_Surdej)
# X$U_Mork<-as.numeric(X$U_Mork)
# X$T_S_Sej<-as.numeric(X$T_S_Sej)
# X$U_Kompakt<-as.numeric(X$U_Kompakt)
# X$S_Fersk<-as.numeric(X$S_Fersk)
# X$S_Solsikke<-as.numeric(X$S_Solsikke)
# X$S_Plantemix<-as.numeric(X$S_Plantemix)
# X$GS_Salt<-as.numeric(X$GS_Salt)
# X$GS_Sodt<-as.numeric(X$GS_Sodt)
# X$GS_Surt<-as.numeric(X$GS_Surt)
# X$GS_Bitter<-as.numeric(X$GS_Bitter)
# X$T_Klaeg<-as.numeric(X$T_Klaeg)
# X$T_Poppende<-as.numeric(X$T_Poppende)
# X$ES_Bitter<-as.numeric(X$ES_Bitter)
# X$ES_Halm<-as.numeric(X$ES_Halm)
# X$ES_Harsk<-as.numeric(X$ES_Harsk)
# X$MF_Coating<-as.numeric(X$MF_Coating)
# X$U_S_Mork<-as.numeric(X$U_S_Mork)
# X$ES_S_Bitter<-as.numeric(X$ES_S_Bitter)
# X$ES_S_Br.d<-as.numeric(X$ES_S_Br.d)
																				
ggplot(data = X, aes(Product, L_Braendt)) +
  geom_boxplot()
# aggregate(X$L_Braendt,
          # by = list(X$Product), mean)
# aggregate(X$L_Braendt,
          # by = list(X$Product), sd)

library(tidyverse)
tb <- X %>% 
  group_by(Product) %>% # specify which grouping vector to use
  dplyr::summarise(n = n(), # compute n
            mn = mean(L_Braendt), # compute mean
            s = sd(L_Braendt), # compute s
            q1 = quantile(L_Braendt,0.25), # compute lower 25% quartile
            q3 = quantile(L_Braendt,0.75)) # compute upper 75% quartile
tb  
# library(lme4)
# X$Rep<-as.factor(X$Rep)
# X$Product<-as.factor(X$Product)
# Husk fixed effects = factor, coviate age int, num, random effects after 1|, vekselvirkninger med* (husk ikke Tukey p? vekselvirkninger, men)
# LBraendt <-lm(data=X, L_Braendt~Product+Rep*Subject)
# anova(LBraendt)
# summary(LBraendt)
# 
# library(multcomp)
# X$Product<-as.factor(X$Product)
# LBraendtPair<-glht(LBraendt,linfct=mcp(Product="Tukey"))
# cld(L_BraendtPair)


## 
# Til sammenligning af de fire produkter benyttes descriptiv stats via f.eks. summarise, samt en lmer model med multcomp
# exempel L_Braendt
library(lme4)
library(lmerTest)
library(multcomp)
library(broom.mixed)
m <- lmer(data = X, L_Braendt ~ Product  + (1|Subject))
anova(m) 
# eller (via broom.mixed)
m %>% anova %>% tidy
# get letters
mm <- m %>% glht(linfct=mcp(Product="Tukey")) %>% cld()
mm
# combine in table
tb <- tb %>% cbind(mm$mcletters$Letters %>% as.data.frame() %>% rename(c('.' = 'Lettes')))

# Det laver vi til en function samlet funktion som tar en data.frame med y som response og Product og Subject som indput, og smider en data frame ud som resultat. 

getSTATs <- function(df){
  tb <- df %>% 
    group_by(Product) %>% # specify which grouping vector to use
    dplyr::summarise(n = n(), # compute n
                     mn = mean(y), # compute mean
                     s = sd(y), # compute s
                     q1 = quantile(y,0.25), # compute lower 25% quartile
                     q3 = quantile(y,0.75)) # compute upper 75% quartile
  m <- lmer(data = df, y ~ Product  + (1|Subject)) 
  pv_anova <- m %>% anova %>% tidy
  mm <- m %>% glht(linfct=mcp(Product="Tukey")) %>% cld()
  # combine in table
  tb <- tb %>% 
    ungroup %>% 
    cbind(mm$mcletters$Letters %>% as.data.frame() %>% rename(c('.' = 'Letters'))) %>% 
    mutate(pv = pv_anova$p.value) 
  return(tb)
  
}

tbwithall <- X %>% 
  gather(var,y,L_Braendt:ES_S_Br.d) %>% 
  group_by(var) %>% 
  do(getSTATs(df = .))


tb <- tbwithall %>% 
  mutate(res = paste(round(mn,1),'pm',round(s,1), Letters)) %>% 
  dplyr::select(var,Product,res,pv) %>% 
  group_by(var) %>% 
  spread(Product,res) 

# Export to Excel:-) 
rio::export(tb, file = '~/Dropbox/Backup/MyDocumentsOnC/Course and teaching/DataAnalysis_ConsumerScience/dataanalyssisconsumerscience/data/ryebread/descp_tabl.xlsx')




