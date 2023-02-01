             ############################################################
             ##               Anova de uma via.                        ##
             ##     Delineamento em blocos casualizados                ##
 #___________###########################################################___________#

#   rm(list = objects()) # limpar a memoria
#   rm(list = ls(all = TRUE)) # limpar a memoria
#   graphics.off() # limpar memoria de graficos

##___________________________________Conjunto de dados

## Fator de estudo as cutivares de mandioca 
cutivares <-as.factor(rep(c("Aciolina", "Pao", "Amazoninha"),each=4))
## Blocos
blocos <- factor(rep(1:4,3)) # repetir os numeros de 1 a 4 tres vezes
## Altura da planta em m
altura <- c(0.96, 0.97, 0.92, 0.97, 0.99, 1.05,
            0.9, 1.01, 0.93, 0.95, 0.83, 0.96)

## ___________________________________Colocondo o conjunto de dados no data.frame
dados<- data.frame(altura, cutivares, blocos)
summary(dados)

#Verificando em Dic
#___________________________________ Análise em dic
#___________________________________Modelo linear mais simples

analise1 <- lm(altura ~ cutivares)
analise1
anova(analise1)
summary(analise1)
modDIC<- aov(terms(altura~cutivares,
                       keep.order = TRUE), data = dados)
summary(modDIC)
#___________________________________verificando normalidade em forma de gráfico
library(hnp)
par(mfrow = c(2, 3)) # Aumentar a janela de visualização de plotes
HNP=hnp::hnp(analise1, paint.on=T, col="red" , las=1, pch=8)
plot(HNP,lty=c(2,3,2),  col=c(2,1,2,1))
plot(analise1)

par(mfrow = c(1, 1))
#___________________________________ Análise em DBC
#___________________________________ Modelo linear mais simples (lm)

analise2 <- lm(altura ~ blocos + cutivares)
analise2
anova(analise2)
#___________________________________ Comparando os modelos 
COMP_ANOVAS<- anova(analise1,analise2)
COMP_ANOVAS
summary(analise1)
summary(analise2)

#___________________________________ Blocos e cultivares foram significativos

#___________________________________Gráficando

A<- windowsFonts(A = windowsFont("Agency FB"))
par(mfrow = c(1, 2))
HNP=hnp::hnp(analise2, paint.on=T, col="red" ,
             las=1, pch=8,family ="A", size=0.4)
plot(HNP,lty=c(2,3,2),  col=c(2,1,2,1),  family ="A", size=0.4)
par(mfrow = c(1, 1))

par(mfrow = c(2, 3))
plot(analise2, family ="A" , size=0.4)
par(mfrow = c(1, 1))
#___________________________________ Análise em DBC
#___________________________________ Modelo linear mais simples (anova)
m01 <- aov(terms(altura~blocos+cutivares,
                 keep.order = TRUE), data = dados)

summary(m01)

#___________________________________Gráficando a distribuião dos dados com a função plot

plot(cutivares,altura, las= 1, data =dados, xlab = "cultivares",
     ylab = "altura m")

#___________________________________Gráficando a distribuião dos dados com a função boxplot
graf1 <- boxplot(altura ~ cutivares, data = dados,las= 1,xlab = "cultivares",
        ylab = "altura m")
#___________________________________Gráficando a distribuião dos dados com a função stripchart

A<- windowsFonts(A = windowsFont("Agency FB"))

graf2 <- stripchart(altura ~ cutivares, vertical = T,
           pch = 19, xlab = "cutivares",las=1,
           ylab= "altura m", method="overplot", col = 2,family ="A" )

#___________________________________Comparando os dois Gráficos boxplot vs. stripchart
x11()

par(mfrow = c(2, 1),mar = c(5, 5, 2, 2), mgp=c(3, 1, 0))
boxplot(altura ~ cutivares, data = dados,las= 1,xlab = "cultivares",
        ylab = "altura m")
A<- windowsFonts(A = windowsFont("Agency FB"))
stripchart(altura ~ cutivares, vertical = T,
           pch = 19, xlab = "cutivares",las=1,
           ylab= "altura m", method="overplot", col = 2,family ="A" )
par(mfrow = c(1, 1))
#####################_____________Tirar a média ____________#######################

 
Aciolina<-(dados[cutivares=="Aciolina",]); Aciolina<- mean(Aciolina$altura)
Pao<- (dados[cutivares=="Pao",]); Pao<- mean(Pao$altura)
Amazoninha<- (dados[cutivares=="Amazoninha",]);Amazoninha<- mean(Amazoninha$altura)

##_____________________________ Tirar desvio padrao
Aciolina<- (dados[cutivares=="Aciolina",]); Aciolina<- sd(Aciolina$altura)
Pao<-(dados[cutivares=="Pao",]); Pao<-sd(Pao$altura)
Amazoninha<- (dados[cutivares=="Amazoninha",]); Amazoninha<-sd(Amazoninha$altura)

cutivares <- c("Aciolina","Pão","Amazoninha")
media <- c(0.955,0.9875, 0.9175)
desvios <-c(0.03109126, 0.08164966,0.0591608)
resumo_1<-data.frame(cutivares, media,desvios)
resumo_1
##______________________________ Teste de média
TukeyHSD(aov(analise2), "cutivares")
TukeyHSD(m01)

#___________________________________Criando gráfico de barras e salvando dendro das pastas 


# As pastas devem ser criadas previamente

# Files------> New Folder------> Nomear pasta --Aulas_anova---> dendtro da pasta Aulas_anova---->
# ---> cri outra pasta --> graficos_1

png(file="Aulas_anova/graficos_1/saving_plot1.png",
     width=10, height=10, units="cm", res=300)
 
barplot(media,
        ylim = c(0,1.3),
        xlab="cultivares de mandioca",
        ylab="altura da planta (m)",
        main="Altura de plantas de mandioca",
        names.arg=cutivares,
        #col="blue",
        border="red",
        las=1, width = c(1, 1, 1), family ="A")
abline(h=0)
text(0.7, 0.999, "0.96",family ="A")
text(1.9, 1.029, "0.99",family ="A")
text(3.1, 0.96, "0.92",family ="A")

dev.off()
#_____________________________Inserir manualmente as letras do teste


 png(file="Aulas_anova/graficos_1/saving_plot2.png",
           width=10, height=10, units="cm", res=300)
 barplot(media,
        ylim = c(0,1.3),
        xlab="cultivares de mandioca",
        ylab="altura da planta (m)",
        main="Altura de plantas de mandioca",
        names.arg=cutivares,
        #col="blue",
        border="red",
        las=1, width = c(1, 1, 1), family ="A")
abline(h=0)
text(0.7, 0.999, "0.96 ab",family ="A")
text(1.9, 1.029, "0.99 a",family ="A")
text(3.1, 0.96, "0.92 b",family ="A")

dev.off()

#_______________________ colocando cor no grafico
barplot(media,
        ylim = c(0,1.3),
        xlab="cultivares de mandioca",
        ylab="altura da planta (m)",
        main="Altura de plantas de mandioca",
        names.arg=cutivares,
        col="blue",
        border="red",
        las=1)

#______________________Inserindo as médias e letras do teste de comparação

A<- windowsFonts(A = windowsFont("Agency FB")) # determinar a fonte do gráfico

stripchart(altura ~ cutivares, vertical = T,
           pch = 19, xlab = "cutivares",las=1,
           ylab= "altura (m)", method="overplot",
           col = 2,family ="A" , data = dados) # stack ou jitter
text(1.15, 0.967, "0.96 ab",family ="A")
segments(x0 = 1.04, y0 = 0.958, x1 = 1.22, y1 = 0.958, lwd=3,col="gray") 
text(2.12, 0.92, "0.92 b",family ="A")
segments(x0 = 2.05, y0 = 0.911, x1 = 2.20, y1 = 0.911, lwd=3,col="gray") 
text(2.9, 0.99, "0.99 a",family ="A")
segments(x0 = 2.8, y0 = 0.98, x1 = 2.99, y1 = 0.98, lwd=3,col="gray")

#___________________________________Criando gráfico com os valores Resumindos pelo padrão do R
resumo_1<-data.frame(cutivares, media,desvios)
resumo_1
library(tidyverse)

plot1<- ggplot(resumo_1)+
  geom_bar(aes(x=cutivares, y=media),
           stat = "identity",
           fill = "gray")+
  geom_errorbar(aes(x= cutivares,
                    ymin = media-desvios,
                    ymax = media+desvios),
                width = 0.2,
                color =1,
                size = 0.4)+
  labs(x = "Cutivares de mandioca",
       y = expression ('altura' ~~ (m ~ planta^{-1})), 
       title = " Altura de plantas de mandioca (teste de cultivares)") +
  scale_x_discrete(labels = c("Aciolina",
                              "Amazoninha",
                              "Pão 1"))+
  theme_bw()+
  annotate("text",1.28, 0.99,label="0.96 ab", family = "A")+
  annotate("text", 2.25, 0.95,label="0.92 b", family = "A")+
  annotate("text", 3.25, 1.02,label= "0.99 a", family = "A")+
  theme(text = element_text(family = "A"),
        axis.text=element_text(size=12),
        axis.title = element_text(color="black", size=12),
        axis.text.x = element_text(colour = "black", size=12),
        axis.text.y = element_text(colour = "black", size=12),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank())

plot1
## Salvando o gáfico
ggsave( filename="Aulas_anova/graficos_1/saving_plot3.png",
        plot = plot1,
        width = 10, height = 10, units = "cm", dpi = 300)

#_______________________-Resumido os valores observados pela função summarise do pacote tidyverse (dplyr:)

## Fator de estudo as cutivares de mandioca 
cutivares_m <-as.factor(rep(c("Aciolina", "Pao", "Amazoninha"),each=4))
## Blocos
blocos_4<- factor(rep(1:4,3)) # repetir os numeros de 1 a 4 tres vezes
## Altura da planta em m
altura_3 <- c(0.96, 0.97, 0.92, 0.97, 0.99, 1.05,
            0.9, 1.01, 0.93, 0.95, 0.83, 0.96)

## ___________________________________Colocondo o conjunto de dados no data.frame
dados2<- data.frame(altura_3, cutivares_m, blocos_4)

# exportar dados 
write.csv(dados2, file = "C:/Users/Dell/OneDrive/Documentos/Projetos_Git/Projeto_1_test/dados/dados_bdc_f1.csv")


library(tidyverse)

  dfr <- dados2 %>% 
  group_by(cutivares_m) %>% # Agrupar as cultivare em um sumario de média e desvios
  summarise(desvio = sd(altura_3),
            medias = mean(altura_3))%>% # médias e desvios
  mutate(high=medias+desvio,low=medias-desvio)
  dfr

  
A<- windowsFonts(A = windowsFont("Agency FB"))
plot2<- ggplot(data=dfr,mapping=aes(x=as.factor(cutivares_m),
                                    y=medias,color=cutivares_m))+
  geom_point(size=4)+
  geom_errorbar(aes(ymax=high,ymin=low),width=0.2)+
  labs(x = "Cutivares de mandioca",
       y = expression ('altura' ~~ (m ~ planta^{-1})), 
       title = " Altura de plantas de mandioca (teste de cultivares)") +
  scale_x_discrete(labels = c("Aciolina",
                              "Amazoninha",
                              "Pão 1"))+
  theme_bw()+
  annotate("text",1.2, 0.962,label="0.96 ab", family = "A")+
  annotate("segment",x=0.8, xend=1.5,y=0.955,yend=0.955,)+
  annotate("text", 2.15, 0.922,label="0.92 b", family = "A")+
  annotate("segment", x=1.8, xend=2.5, y=0.915,yend=0.915)+
  annotate("text", 3.15, 0.992,label= "0.99 a", family = "A")+
  annotate("segment", x=2.8, xend=3.5,y=0.985,yend=0.985)+
  theme(text = element_text(family = "A"),
        axis.text=element_text(size=12),
        axis.title = element_text(color="black", size=12),
        axis.text.x = element_text(colour = "black", size=12),
        axis.text.y = element_text(colour = "black", size=12),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        legend.direction="horizontal",
        legend.box = "horizontal",
        legend.text= element_text(size=rel(1.2)),
        legend.position="top")
plot2


ggsave( filename="Aulas_anova/graficos_1/saving_plot4.png",
        plot = plot2,
        width = 10, height = 10, units = "cm", dpi = 300)
################

resumo_1<-data.frame(cutivares, media,desvios)
library(tidyverse)
plot3<- ggplot(resumo_1)+
  geom_bar(aes(x=cutivares, y=media),
           stat = "identity",
           fill = "gray")+
  geom_errorbar(aes(x= cutivares,
                    ymin = media-desvios,
                    ymax = media+desvios),
                width = 0.2,
                color =1,
                size = 0.4)+
  theme_bw()

plot3

ggsave( filename="Aulas_anova/graficos_1/saving_plot5.png",
        plot = plot3,
        width = 10, height = 10, units = "cm", dpi = 300)


###########################################
library(tidyverse)

## Fator de estudo as cutivares de mandioca 
cutivares<- as.factor(rep(c("Aciolina", "Pao", "Amazoninha"),each=4))
## Blocos
blocos <- factor(rep(1:4,3))# repetir os numeros de 1 a 4 tres vezes
## Altura da planta em m
altura_t <- c(0.96, 0.97, 0.92, 0.97, 0.99, 1.05,
              0.9, 1.01, 0.93, 0.95, 0.83, 0.96)
## sumarizar os dados 
novo <- data.frame(cutivares,blocos,altura_t)

novo1 <- novo %>% 
  group_by(cutivares) %>%
  summarise(
    sd = sd(altura_t, na.rm = TRUE),
    altura = mean(altura_t)) %>% 
  mutate(low=altura-sd, high=altura+sd)

A<- windowsFonts(A = windowsFont("Agency FB"))
plot4<- ggplot(novo1, aes(cutivares, altura, color =cutivares)) +
  geom_jitter(position = position_jitter(0.1), color = "darkgray") + 
    geom_pointrange(aes(ymin = altura-sd, ymax = altura+sd),
                  data = novo1)+
  labs(x = "Cutivares de mandioca",
       y = expression ('altura' ~~ (m ~ planta^{-1})), 
       title = " Altura de plantas de mandioca (teste de cultivares)") +
  scale_x_discrete(labels = c("Aciolina",
                              "Amazoninha",
                              "Pão 1"))+
  theme_bw()+
  theme(text = element_text(family = "A"),
        axis.text=element_text(size=12),
        axis.title = element_text(color="black", size=12),
        axis.text.x = element_text(colour = "black", size=12),
        axis.text.y = element_text(colour = "black", size=12),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        legend.direction="horizontal",
        legend.box = "horizontal",
        legend.text= element_text(size=rel(1.2)),
        legend.position="top")
plot4


ggsave( filename="Aulas_anova/graficos_1/saving_plot6.png",
        plot = plot4,
        width = 10, height = 10, units = "cm", dpi = 300)






##
A<- windowsFonts(A = windowsFont("Agency FB"))
plot5<-ggplot(dados, aes(cutivares, altura, color = cutivares)) +
  geom_jitter(position = position_jitter(0.1)) + 
  geom_pointrange(aes(ymin = altura-sd, ymax = altura+sd),
                  data = novo1)+
  scale_color_manual(values = c("#29B00E","#00AFBB", "#E7B800"),
                     name="Cutivares",    
                     labels=c("Aciolina", "Amazoninha","Pão 1"))+
  labs(x = "Cutivares de mandioca",
       y = expression ('altura' ~~ (m ~ planta^{-1})), 
       title = " Altura de plantas de mandioca (teste de cultivares)",
       color= "Cutivares" ) +
  scale_x_discrete(labels = c("Aciolina",
                              "Amazoninha",
                              "Pão 1"))+
  theme_bw()+
  theme(text = element_text(family = "A"),
        axis.text=element_text(size=12),
        axis.title = element_text(color="black", size=12),
        axis.text.x = element_text(colour = "black", size=12),
        axis.text.y = element_text(colour = "black", size=12),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        legend.direction="horizontal",
        legend.box = "horizontal",
        legend.text= element_text(size=rel(1.2)),
        legend.position="top")
plot5
ggsave( filename="Aulas_anova/graficos_1/saving_plot7.png",
        plot = plot5,
        width = 10, height = 10, units = "cm", dpi = 300)

###########

A<- windowsFonts(A = windowsFont("Agency FB"))
plot6<- ggplot(dados, aes(cutivares, altura, color = cutivares)) +
  geom_jitter(position = position_jitter(0.15)) + 
  geom_pointrange(aes(ymin = altura-sd, ymax = altura+sd),
                  data = novo1)+
  labs(x = "Cutivares de mandioca",
       y = expression ('altura' ~~ (m ~ planta^{-1})), 
       title = " Altura de plantas de mandioca (teste de cultivares)",
       color= "Cutivares" ) +
  scale_x_discrete(labels = c("Aciolina","Amazoninha","Pão 1"))+
  scale_color_manual(values = c("#29B00E","#00AFBB", "#E7B800"),
                     name="Cutivares",    
                     labels=c("Aciolina", "Amazoninha","Pão 1"))+
  theme_bw()+
  theme(text = element_text(family = "A"),
        axis.text=element_text(size=12),
        axis.title = element_text(color="black", size=12),
        axis.text.x = element_text(colour = "black", size=12),
        axis.text.y = element_text(colour = "black", size=12),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        legend.direction="horizontal",
        legend.box = "horizontal",
        legend.text= element_text(size=rel(1.2)),
        legend.position="top")
plot6
ggsave( filename="Aulas_anova/graficos_1/saving_plot8.png",
        plot = plot6,
        width = 10, height = 10, units = "cm", dpi = 300)
###########################################
library(tidyverse)
## sumarizar os dados 
df.summary <- dados %>%
  group_by(cutivares) %>%
  summarise(
    sd = sd(altura, na.rm = TRUE),
    altura = mean(altura)
  )
df.summary

A<- windowsFonts(A = windowsFont("Agency FB"))

ggplot(df.summary, aes(cutivares, altura, color = cutivares)) +
  geom_col(data = df.summary, position = position_dodge(0.8), 
           width = 0.8, fill = "white") +
  geom_jitter(
    position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8)
  ) + 
  geom_errorbar(
    aes(ymin = altura-sd, ymax = altura+sd), data = df.summary, 
    width = 0.2, position = position_dodge(0.8)
  )+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#29B00E"),
                                         name="Cutivares",    
                                         labels=c("Aciolina", "Amazoninha","Pão 1")) +
  theme(legend.position = "top") +
  labs(x = "Cutivares de mandioca",
       y = expression ('altura' ~~ (m ~ planta^{-1})), 
       title = " Altura de plantas de mandioca (teste de cultivares)") +
  scale_x_discrete(labels = c("Aciolina","Amazoninha","Pão 1"))+
  theme_bw()+
  annotate("text",1.25, 0.993,label="0.96 ab", family = "A")+
  #annotate("segment",x=0.8, xend=1.5,y=0.955,yend=0.955,)+
  annotate("text", 2.3, 0.952,label="0.92 b", family = "A")+
  #annotate("segment", x=1.8, xend=2.5, y=0.915,yend=0.915)+
  annotate("text", 3.25, 1.029,label= "0.99 a", family = "A")+
  #annotate("segment", x=2.8, xend=3.5,y=0.985,yend=0.985)+
  theme(text = element_text(family = "A"),
        axis.text=element_text(size=12),
        axis.title = element_text(color="black", size=12),
        axis.text.x = element_text(colour = "black", size=12),
        axis.text.y = element_text(colour = "black", size=12),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        legend.direction="horizontal",
        legend.box = "horizontal",
        legend.text= element_text(size=rel(1.2)),
        legend.position="top")
