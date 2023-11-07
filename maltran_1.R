

#### Required packages ####
Sys.setenv(TZ="UTC")
library(tidyverse)
library(emmeans)
library(lme4)

#### read data ####
setwd('D://Papiers/Maltrans/remaltrans/')
list.files()

data <- read.table('data_R.csv', sep=",", header=TRUE, dec=".",na.string=NA, 
                   quote = "\"'", encoding = "UTF-8")

colnames(data)
str(data)

### Basic manipulations #### 
##Creation des effectifs des especes/ enquete,village, point,poste
df_HBR <- data %>% 
  dplyr::group_by(month, village, house, hour, place, genus)%>% 
  dplyr::summarize(effectif = n()) %>% 
  tidyr::spread(genus,'effectif')

# NA =  0 et creer colum sum
df_HBR[,6:8][is.na(df_HBR[,6:8])] <- 0
df_HBR$total <- Matrix::rowSums(df_HBR[,6:8])

# convert 
df_HBR$month <- as.factor(df_HBR$month)
df_HBR$village <- as.factor(df_HBR$village)

### Model tous les genres ####
fit_hbr <- glmer.nb(Anopheles~ month+village+place + (1|house) , data = df_HBR, family='poisson') # 

deviance(fit_hbr)/df.residual(fit_hbr)
summary(fit_hbr) 

## LRT 
car::Anova(fit_hbr,type=3) 

## comparison avec RR
hbr_v <- emmeans(fit_hbr,pairwise~village, type="response", adjust = "none") 
hbr_v_contrast <- summary(hbr_v$contrasts, infer=TRUE)%>%as.data.frame()
hbr_v_emmeans <- summary(hbr_v$emmeans, infer=TRUE)%>%as.data.frame()

hbr_m <- emmeans(fit_hbr,pairwise~month, type="response", adjust = "none")  
hbr_m_contrast <- summary(hbr_m$contrasts, infer=TRUE)%>%as.data.frame()
hbr_m_emmeans <- summary(hbr_m$emmeans, infer=TRUE)%>%as.data.frame()

hbr_m_v <- emmeans(fit_hbr,pairwise~month|village, type="response", adjust = "none") ## 
hbr_m_v_contrast <- summary(hbr_m_v$contrasts, infer=TRUE)%>%as.data.frame()
hbr_m_v_emmeans <- summary(hbr_m_v$emmeans, infer=TRUE)%>%as.data.frame()

hbr_v_m <- emmeans(fit_hbr,pairwise~village|month, type="response", adjust = "none") ## 
hbr_v_m_contrast <- summary(hbr_v_m$contrasts, infer=TRUE)%>%as.data.frame()
hbr_v_m_emmeans <- summary(hbr_v_m$emmeans, infer=TRUE)%>%as.data.frame()

## Mean densities per person and day : 4 personnes * 4 nuits = 8 
df_HBR %>% 
  dplyr::group_by(village) %>% 
  dplyr::summarise(sum = sum(Anopheles)/8) %>% 
  as.data.frame()

df_HBR%>% 
  dplyr::group_by(village, month) %>% 
  dplyr::summarise(sum = sum(Anopheles)/8) %>% 
  as.data.frame()

df_ano <- df_HBR%>% 
  dplyr::group_by(village, month, house, place) %>% 
  dplyr::summarise(sum = sum(Anopheles)) %>% 
  as.data.frame()


df_ano$month <- factor(df_ano$month,levels = c("Mai","Juin","Juillet","Aout", 
                                               "Septembre", "Octobre","Novembre"))

#### graph hbr ####
graph_hbr <- df_ano%>%ggplot(aes(x=month,y=sum))+
  geom_boxplot(outlier.shape = NA)+
  stat_summary(fun.y = mean, geom = "point",shape=20,
               size=3,color="red", fill="red")+
  facet_wrap(~village) 

graph_hbr+
  theme_bw()+
  labs(x="Surveys",
       y = expression("Average of human biting rates ("*"  "*b.h^-1~.n^-1*")"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 11, color = "black"),
        axis.title.x = element_text(size = 12, color="black"),
        axis.title.y = element_text(size = 12, color="black"),
        strip.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size = 11, color = "black"),
        legend.title = element_text(size = 12),
        panel.grid = element_blank())+
  scale_x_discrete(labels=c("Mai"="May","Juin"="June","Juillet"="July","Aout"="August", 
                            "Septembre"="September", "Octobre"="October","Novembre"="November"))


#### Endo - exo #####
## comparison avec RR
hbr_end <- emmeans(fit_hbr,pairwise~place, type="response", adjust = "none") 
hbr_end_contrast <- summary(hbr_end$contrasts, infer=TRUE)%>%as.data.frame()
hbr_end_emmeans <- summary(hbr_end$emmeans, infer=TRUE)%>%as.data.frame()

hbr_v_end <- emmeans(fit_hbr,pairwise~village|place, type="response", adjust = "none") ## 
hbr_v_end_contrast <- summary(hbr_v_end$contrasts, infer=TRUE)%>%as.data.frame()
hbr_v_end_emmeans <- summary(hbr_v_end$emmeans, infer=TRUE)%>%as.data.frame()

hbr_end_v <- emmeans(fit_hbr,pairwise~place|village, type="response", adjust = "none") ## 
hbr_end_v_contrast <- summary(hbr_end_v$contrasts, infer=TRUE)%>%as.data.frame()
hbr_end_v_emmeans <- summary(hbr_end_v$emmeans, infer=TRUE)%>%as.data.frame()

hbr_end_vm <- emmeans(fit_hbr,pairwise~place|village+month, type="response", adjust = "none") ## 
hbr_end_vm_contrast <- summary(hbr_end_vm$contrasts, infer=TRUE)%>%as.data.frame()
hbr_end_vm_emmeans <- summary(hbr_end_vm$emmeans, infer=TRUE)%>%as.data.frame()


### comparaison tx endo - exo
total = 5717
end0 <- df_HBR %>%
  dplyr::group_by(village) %>% 
  dplyr::summarise(sum = sum(Anopheles)) %>% 
  as.data.frame()

end1 <- df_HBR %>%
  dplyr::group_by(village, place) %>% 
  dplyr::summarise(sum = sum(Anopheles)) %>% 
  as.data.frame()
#Kimi
prop.test(end1$sum[1:2],c(end0$sum[1],end0$sum[1]))

#Santi
prop.test(end1$sum[3:4],c(end0$sum[2],end0$sum[2]))

## exo - endo
prop.test(end1$sum[c(2,4)],end0$sum)
prop.test(end1$sum[c(1,3)],end0$sum)


### data preparation
df_ex <- df_HBR%>%
  dplyr::group_by(village,month)%>%
  dplyr::summarise(n=sum(Anopheles))

df_endo_exo <- df_HBR%>%
  dplyr::group_by(village,month,place)%>%
  dplyr::summarise(sum = sum(Anopheles),hb=mean(Anopheles)/4, mean = mean(Anopheles), 
                   sd=sd(Anopheles),se=sd(Anopheles)/sqrt(4))
df_endo_exo[,4:7][is.na(df_endo_exo[,4:8])] <- 0
n = c(390, 390, 158, 158,  57, 57,   7, 7, 292, 292, 934, 934, 628, 628, 
      825, 825, 306, 306, 142, 142,  13, 13, 547, 547, 687,687, 751, 751)
df_endo_exo$total <- n
df_endo_exo$p <- df_endo_exo$sum/df_endo_exo$total

df_endo_exo$month <- factor(df_endo_exo$month,levels = c("Mai","Juin","Juillet","Aout",
                                                         "Septembre", "Octobre","Novembre"))

endo <- ggplot(data=df_endo_exo,aes(x=month,y=sum))+
  geom_bar(aes(fill=place),stat="identity", position = 'dodge', width = 0.7)+
  facet_wrap(~village)+
  theme_bw()

endo+
  scale_fill_manual(labels = c("Indoor", "Outdoor"), 
                    values = c("#999999", "#E69F00")) +
  labs(x="Surveys",
       y=expression("Number of "~italic(Anopheles)~" species"))+
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 11, color = "black"),
        axis.title.x = element_text(size = 12, color="black"),
        axis.title.y = element_text(size = 12, color="black"),
        strip.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size = 11, color = "black"),
        legend.title = element_text(size = 12),
        panel.grid = element_blank())+
  guides(fill=guide_legend(title=" "))+
  scale_x_discrete(labels=c("Mai"="May","Juin"="June","Juillet"="July","Aout"="August", 
                            "Septembre"="September", "Octobre"="October","Novembre"="November"))


### cycle aggressiveness #####
## data preparation 
# Juin 
cycle_juin <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour,
                        subset(df_HBR,month=="Juin"),sum)
cycle_juin$hour <- as.factor(cycle_juin$hour)

cycle_juin$month <- c(rep("Juin",length(cycle_juin$hour)))
cycle_juin$Lsp <- c(rep("species",length(cycle_juin$hour)))


## Juillet 
cycle_juillet <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour,
                           subset(df_HBR,month=="Juillet"),sum)
cycle_juillet$hour <- as.factor(cycle_juillet$hour)

cycle_juillet$month <- c(rep("Juillet",length(cycle_juillet$hour)))
cycle_juillet$Lsp <- c(rep("species",length(cycle_juillet$hour)))

## Aout 
cycle_aout <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour,
                        subset(df_HBR,month=="Aout"),sum)
cycle_aout$hour <- as.factor(cycle_aout$hour)

cycle_aout$month <- c(rep("Aout",length(cycle_aout$hour)))
cycle_aout$Lsp <- c(rep("species",length(cycle_aout$hour)))

## Septembre 
cycle_septembre <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour,
                             subset(df_HBR,month=="Septembre"),sum)
cycle_septembre$hour <- as.factor(cycle_septembre$hour)

cycle_septembre$month <- c(rep("Septembre",length(cycle_septembre$hour)))
cycle_septembre$Lsp <- c(rep("species",length(cycle_septembre$hour)))

## Octobre 
cycle_octobre <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour,
                           subset(df_HBR,month=="Octobre"),sum)
cycle_octobre$hour <- as.factor(cycle_octobre$hour)

cycle_octobre$month <- c(rep("Octobre",length(cycle_octobre$hour)))
cycle_octobre$Lsp <- c(rep("species",length(cycle_octobre$hour)))

## Novembre 
cycle_novembre <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour,
                            subset(df_HBR,month=="Novembre"),sum)
cycle_novembre$hour <- as.factor(cycle_novembre$hour)

cycle_novembre$month <- c(rep("Novembre",length(cycle_novembre$hour)))
cycle_novembre$Lsp <- c(rep("species",length(cycle_novembre$hour)))

## rbind 
df_cycle <- rbind(cycle_juin,cycle_juillet,cycle_aout,cycle_septembre,
                 cycle_octobre, cycle_novembre)

# relevels 
df_cycle$month <- factor(df_cycle$month,levels = c("Juin","Juillet","Aout",
                                                 "Septembre", "Octobre","Novembre"))

df_cycle$hour <- factor(df_cycle$hour,levels = c("20", "21", "22", "23", "0", "1",
                                                 "2", "3", "4", "5"))

## plot 
plt1 <- ggplot(data = df_cycle,aes(x=hour,y=Anopheles,group=Lsp))+
  geom_line(col="red")+
  facet_wrap(~month)+ 
  theme_bw()

plt1+
  labs(x = "Heures de capture",
       y=expression("Nbre de piqûres/homme*heure ("*" "*p.h^-1~.h^-1*"))"))+
  theme(axis.text.x = element_text(hjust=1, size = 11, color = "black"),
        axis.title.x = element_text(size = 12, color="black"),
        axis.title.y = element_text(size = 12, color="black"),
        strip.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size = 11, color = "black"),
        legend.title = element_text(size = 12),
        panel.grid = element_blank())+
  guides(fill=guide_legend(title=" "))

## by indoor/outdoor
### cycle aggressiveness #####
## data preparation 
# Juin 
cycle_juin1 <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour+place,
                         subset(df_HBR,month=="Juin"),sum)
cycle_juin1$hour <- as.factor(cycle_juin1$hour)
cycle_juin1$place <- as.factor(cycle_juin1$place)

cycle_juin1$hbr_aedes <- cycle_juin1[,3]/(4*4)
cycle_juin1$hbr_anopheles <- cycle_juin1[,4]/(4*4)
cycle_juin1$hbr_mansonia <- cycle_juin1[,5]/(4*4)
cycle_juin1$month <- c(rep("Juin",length(cycle_juin1$hour)))
cycle_juin1$Lsp <- c(rep("species",length(cycle_juin1$hour)))


## Juillet 
cycle_juillet1 <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour+place,
                           subset(df_HBR,month=="Juillet"),sum)
cycle_juillet1$hour <- as.factor(cycle_juillet1$hour)
cycle_juillet1$place <- as.factor(cycle_juillet1$place)

cycle_juillet1$hbr_aedes <- cycle_juillet1[,3]/(4*4)
cycle_juillet1$hbr_anopheles <- cycle_juillet1[,4]/(4*4)
cycle_juillet1$hbr_mansonia <- cycle_juillet1[,5]/(4*4)
cycle_juillet1$month <- c(rep("Juillet",length(cycle_juillet1$hour)))
cycle_juillet1$Lsp <- c(rep("species",length(cycle_juillet1$hour)))

## Aout 
cycle_aout1 <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour+place,
                        subset(df_HBR,month=="Aout"),sum)
cycle_aout1$hour <- as.factor(cycle_aout1$hour)
cycle_aout1$place <- as.factor(cycle_aout1$place)

cycle_aout1$hbr_aedes <- cycle_aout1[,3]/(4*4)
cycle_aout1$hbr_anopheles <- cycle_aout1[,4]/(4*4)
cycle_aout1$hbr_mansonia <- cycle_aout1[,5]/(4*4)
cycle_aout1$month <- c(rep("Aout",length(cycle_aout1$hour)))
cycle_aout1$Lsp <- c(rep("species",length(cycle_aout1$hour)))

## Septembre 
cycle_septembre1 <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour+place,
                             subset(df_HBR,month=="Septembre"),sum)
cycle_septembre1$hour <- as.factor(cycle_septembre1$hour)
cycle_septembre1$place <- as.factor(cycle_septembre1$place)

cycle_septembre1$hbr_aedes <- cycle_septembre1[,3]/(4*4)
cycle_septembre1$hbr_anopheles <- cycle_septembre1[,4]/(4*4)
cycle_septembre1$hbr_mansonia <- cycle_septembre1[,5]/(4*4)
cycle_septembre1$month <- c(rep("Septembre",length(cycle_septembre1$hour)))
cycle_septembre1$Lsp <- c(rep("species",length(cycle_septembre1$hour)))

## Octobre 
cycle_octobre1 <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour+place,
                           subset(df_HBR,month=="Octobre"),sum)
cycle_octobre1$hour <- as.factor(cycle_octobre1$hour)
cycle_octobre1$place <- as.factor(cycle_octobre1$place)

cycle_octobre1$hbr_aedes <- cycle_octobre1[,3]/(4*4)
cycle_octobre1$hbr_anopheles <- cycle_octobre1[,4]/(4*4)
cycle_octobre1$hbr_mansonia <- cycle_octobre1[,5]/(4*4)
cycle_octobre1$month <- c(rep("Octobre",length(cycle_octobre1$hour)))
cycle_octobre1$Lsp <- c(rep("species",length(cycle_octobre1$hour)))

## Novembre 
cycle_novembre1 <- aggregate(cbind(Aedes,Anopheles,Mansonia)~hour+place,
                            subset(df_HBR,month=="Novembre"),sum)
cycle_novembre1$hour <- as.factor(cycle_novembre1$hour)
cycle_novembre1$place <- as.factor(cycle_novembre1$place)

cycle_novembre1$hbr_aedes <- cycle_novembre1[,3]/(4*4)
cycle_novembre1$hbr_anopheles <- cycle_novembre1[,4]/(4*4)
cycle_novembre1$hbr_mansonia <- cycle_novembre1[,5]/(4*4)
cycle_novembre1$month <- c(rep("Novembre",length(cycle_novembre1$hour)))
cycle_novembre1$Lsp <- c(rep("species",length(cycle_novembre1$hour)))

## rbind 
df_cycle1 <- rbind(cycle_juin1,cycle_juillet1,cycle_aout1,cycle_septembre1,
                  cycle_octobre1, cycle_novembre1)

# relevels 
df_cycle1$month <- factor(df_cycle1$month,levels = c("Juin","Juillet","Aout",
                                                     "Septembre", "Octobre","Novembre"))

df_cycle1$hour <- factor(df_cycle1$hour,levels = c("20", "21", "22", "23", "0", 
                                                   "1","2", "3", "4", "5"))

## plot 
plt2 <- ggplot(data = df_cycle1,aes(x=hour,y=hbr_anopheles,group=place,color=place))+
  geom_line(size=0.75)+
  facet_wrap(~month)+ 
  theme_bw()
plt2

plt2+
  labs(x = "Heures de capture",
       y=expression("Nbre de piqûres/homme*heure ("*" "*p.h^-1~.h^-1*"))"))+
  scale_color_manual(values = c('#F8766D', '#00BFC4'),
                     breaks = c('indoor','outdoor'),
                     labels=c('interieur', 'exterieur'),
                     name='Point de capture')+
  theme(axis.text.x = element_text(hjust=1, size = 11, color = "black"),
        axis.title.x = element_text(size = 12, color="black"),
        axis.title.y = element_text(size = 12, color="black"),
        strip.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size = 11, color = "black"),
        legend.title = element_text(size = 12),
        panel.grid = element_blank())
  
## English 
plt3 <- ggplot(data = df_cycle1,aes(x=hour,y=hbr_anopheles,group=place,color=place))+
  geom_line(size=0.75)+
  facet_wrap(~month, labeller = labeller(month=c("Juin"="June", "Juillet"="July",
                                                 "Aout"="August", "Septembre"="September",
                                                 "Octobre"="October","Novembre"="November")))+ 
  theme_bw()

###
plt3+
  labs(x = "Sampling time",
       y=expression("Human biting rate ("*" "*b.h^-1~.h^-1*"))"))+
  scale_color_manual(values = c('#F8766D', '#00BFC4'),
                     #breaks = c('indoor','outdoor'),
                     #labels=c('interieur', 'exterieur'),
                     name='Sampling \npoint')+
  theme(axis.text.x = element_text(hjust=1, size = 11, color = "black"),
        axis.title.x = element_text(size = 12, color="black"),
        axis.title.y = element_text(size = 12, color="black"),
        strip.text.x = element_text(size=12, color="black"),
        axis.text.y = element_text(size = 11, color = "black"),
        legend.title = element_text(size = 12),
        panel.grid = element_blank())


### Parous rate ####
## read parous data 
setwd('D://Papiers/Maltrans/remaltrans/')
list.files()

p_data <- read.table('Parous.csv', sep=",", header=TRUE, dec=".",na.string=NA, 
                     quote = "\"'", encoding = "UTF-8")

p_data$village <- as.factor(p_data$village)
p_data$month <- as.factor(p_data$month)
p_data$place <- as.factor(p_data$place)
p_data$house <- as.factor(p_data$house)

## Analysis parous rate
#by villqge
parit1 <- xtabs(~village+ParRate,data = p_data)

test1 <- prop.test(parit1[, 2],marginSums(parit1, "village"))

conf1 <- t(sapply(Map(prop.test, parit1[,2], marginSums(parit1, "village")), "[[","conf.int"))
test1_df1 <- as.data.frame(conf1)
colnames(test1_df1) <-  c('lower', 'upper')
test1_df <- add_column(test1_df1,prop =as.vector(test1$estimate),.before = "lower")
#rm(test1,conf1, test1_df1)

## by period
parit2 <- xtabs(~month+ParRate,data = p_data)

test2 <- prop.test(parit2[, 2],marginSums(parit2, "month"))

conf2 <- t(sapply(Map(prop.test, parit2[,2], marginSums(parit2, "month")), "[[","conf.int"))
test2_df1 <- as.data.frame(conf2)
colnames(test2_df1) <-  c('lower', 'upper')

test2_df <- add_column(test2_df1,prop =as.vector(test2$estimate),.before = "lower")


### test des difference entre la composition specifiques 
# by species
kimi1 <- c(168, 145, 41)
santi1 <- c(149, 153, 50)

k1 <- c(354, 354, 354)
s1 <- c(352, 352, 352)

prop.test(kimi1, k1)
prop.test(santi1, s1)

# by village 
vil <- c(354, 352)
tv <- c(706,706)
prop.test(vil, tv)

## Graph 
samp_t <- c(3188, 3777)
lab1 <- c('Kimidougou', 'Santidougou')
piepercent<- round(100*samp_t/sum(samp_t), 1)
pie(samp_t, lab1)
pie(samp_t, lab1, col=c('gray100', 'lightblue'))
#legend("topright", lab1, cex = 0.8)

lab2 <- c('Culicinae', 'Anophelinae')
esp_kimi <- c(748, 2440)
esp_kimi1 <- c(2440, 748)
pie(esp_kimi, lab2, col = c('azure3', 'coral'))

esp_ano <- c(748, 2404, 2, 14, 20, 0)

esp_santi <- c(581, 3196)
pie(esp_santi, lab2, col = c('azure3', 'coral'))

## Species of An gamb
lab_sp <- c('An. gambiae', 'An. coluzzii', 'An. arabiensis')
esp1_k <- c(168, 145, 41)
esp2_s <- c(149, 153, 50)
barplot(height = esp1_k, names= lab_sp, width = 0.15)


barplot(height = cbind(ag=c(168, 149), ac=c(145, 153), aa=c(41, 50)), 
        names.arg = lab_sp,beside = TRUE, width = c(1,1,1), ylim = c(0, 200), 
        col=c(col=c('gray100', 'lightblue')), horiz = F)
axis(1, at=c(2,5,8), labels = lab_sp)
text(c(2,5,8), par("usr")[3], labels = lab_sp, srt = 45, pos = 1.9, xpd = TRUE)


barplot(height = cbind(ag=c(168, 149), ac=c(145, 153), aa=c(41, 50)), 
        beside = TRUE, width = c(1,1,1), ylim = c(0, 200), col=c(col=c('gray100', 'lightblue')),
        legend.text=c('Kimidougou', 'Santidougou'), args.legend = list(x = "topright"))


barplot(height = cbind(ag=c(168, 149), ac=c(145, 153), aa=c(41, 50)), 
        beside = FALSE, width = c(1,1,1), 
        legend.text=c('Kimidougou', 'Santidougou'))


pie(rep(1, 12), col = gray.colors(12))


## Parite 
tota_par <- c(572, 823)
par <- c(306, 444)
prop.test(par, tota_par)

