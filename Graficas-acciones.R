require(readxl)
require(ggplot2)
require(nortest)
getwd()
setwd("C://Users//lenovo//Documents//ARF veronica//graficas")
#library nortest
########################################read the database#########################################
basv<-read_excel("C://Users//lenovo//Documents//ARF veronica//RendimientosparaR.xlsx")
#ho:tienen normalidad
#shapiro wilk
shapiro.test(basv$`America movil`)$p.value
shapiro.test(basv$Bimbo)$p.value
shapiro.test(basv$Cemex)$p.value
shapiro.test(basv$Daimler)$p.value
shapiro.test(basv$Liverpool)$p.value
shapiro.test(basv$Banorte)$p.value
shapiro.test(basv$Facebook)$p.value
shapiro.test(basv$Amazon)$p.value
shapiro.test(basv$Netflix)$p.value
shapiro.test(basv$`American Ex`)$p.value
shapiro.test(basv$Pfizer)$p.value
shapiro.test(basv$`Walt disney`)$p.value
#anderson darling
ad.test(basv$`America movil`)$p.value #no tiene normalidad
ad.test(basv$Bimbo)$p.value #tiene normalidad
ad.test(basv$Cemex)$p.value #no tiene
ad.test(basv$Daimler)$p.value #no tiene
ad.test(basv$Liverpool)$p.value #tiene normalidad
ad.test(basv$Banorte)$p.value #no tiene
ad.test(basv$Facebook)$p.value #no tiene
ad.test(basv$Amazon)$p.value #no tiene
ad.test(basv$Netflix)$p.value #no tiene
ad.test(basv$`American Ex`)$p.value #no tiene
ad.test(basv$Pfizer)$p.value #no tiene
ad.test(basv$`Walt disney`)$p.value #no tiene
#cramer von mises
cvm.test(basv$`America movil`)$p.value
cvm.test(basv$Bimbo)$p.value
cvm.test(basv$Cemex)$p.value
cvm.test(basv$Daimler)$p.value
cvm.test(basv$Liverpool)$p.value
cvm.test(basv$Banorte)$p.value
cvm.test(basv$Facebook)$p.value
cvm.test(basv$Amazon)$p.value
cvm.test(basv$Netflix)$p.value
cvm.test(basv$`American Ex`)$p.value
cvm.test(basv$Pfizer)$p.value
cvm.test(basv$`Walt disney`)$p.value
#kolmogorov-smirnov
lillie.test(basv$`America movil`)$p.value
lillie.test(basv$Bimbo)$p.value
lillie.test(basv$Cemex)$p.value
lillie.test(basv$Daimler)$p.value
lillie.test(basv$Liverpool)$p.value
lillie.test(basv$Banorte)$p.value
lillie.test(basv$Facebook)$p.value
lillie.test(basv$Amazon)$p.value
lillie.test(basv$Netflix)$p.value
lillie.test(basv$`American Ex`)$p.value
lillie.test(basv$Pfizer)$p.value
lillie.test(basv$`Walt disney`)$p.value
#pearson
pearson.test(basv$`America movil`)$p.value
pearson.test(basv$Bimbo)$p.value
pearson.test(basv$Cemex)$p.value
pearson.test(basv$Daimler)$p.value
pearson.test(basv$Liverpool)$p.value
pearson.test(basv$Banorte)$p.value
pearson.test(basv$Facebook)$p.value
pearson.test(basv$Amazon)$p.value
pearson.test(basv$Netflix)$p.value
pearson.test(basv$`American Ex`)$p.value
pearson.test(basv$Pfizer)$p.value
pearson.test(basv$`Walt disney`)$p.value
#shapiro francia
sf.test(basv$`America movil`)$p.value
sf.test(basv$Bimbo)$p.value
sf.test(basv$Cemex)$p.value
sf.test(basv$Daimler)$p.value
sf.test(basv$Liverpool)$p.value
sf.test(basv$Banorte)$p.value
sf.test(basv$Facebook)$p.value
sf.test(basv$Amazon)$p.value
sf.test(basv$Netflix)$p.value
sf.test(basv$`American Ex`)$p.value
sf.test(basv$Pfizer)$p.value
sf.test(basv$`Walt disney`)$p.value
#grafica de las densidades
g1<-ggplot(basv,aes(basv$`America movil`))+geom_histogram()+geom_density(col="red")+
labs(title = "America movil", 
     caption = "Elaboracion propia con datos de Investing.com")+
xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g2<-ggplot(basv,aes(basv$Bimbo))+geom_histogram()+geom_density(col="red")+
  labs(title = "Bimbo", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g3<-ggplot(basv,aes(basv$Cemex))+geom_histogram()+geom_density(col="red")+
  labs(title = "Cemex", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g4<-ggplot(basv,aes(basv$Daimler))+geom_histogram()+geom_density(col="red")+
  labs(title = "Daimler", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g5<-ggplot(basv,aes(basv$Liverpool))+geom_histogram()+geom_density(col="red")+
  labs(title = "Liverpool", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g6<-ggplot(basv,aes(basv$Banorte))+geom_histogram()+geom_density(col="red")+
  labs(title = "Banorte", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g7<-ggplot(basv,aes(basv$Facebook))+geom_histogram()+geom_density(col="red")+
  labs(title ="Facebook", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g8<-ggplot(basv,aes(basv$Amazon))+geom_histogram()+geom_density(col="red")+
  labs(title = "Amazon", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g9<-ggplot(basv,aes(basv$Netflix))+geom_histogram()+geom_density(col="red")+
  labs(title = "Netflix", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g10<-ggplot(basv,aes(basv$`American Ex`))+geom_histogram()+geom_density(col="red")+
  labs(title = "American express", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g11<-ggplot(basv,aes(basv$Pfizer))+geom_histogram()+geom_density(col="red")+
  labs(title = "Pfizer", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
g12<-ggplot(basv,aes(basv$`Walt disney`))+geom_histogram()+geom_density(col="red")+
  labs(title = "Walt disney", 
       caption = "Elaboracion propia con datos de Investing.com")+
  xlab("Intervalos")+ylab("Densidad")+
  theme_bw()+theme(plot.caption = element_text(hjust = -.02))
ggsave(filename = "America movil.png",g1)
ggsave(filename = "Bimbo.png",g2)
ggsave(filename = "Cemex.png",g3)
ggsave(filename = "Daimler.png",g4)
ggsave(filename = "Liverpool.png",g5)
ggsave(filename = "Banorte.png",g6)
ggsave(filename = "Facebook.png",g7)
ggsave(filename = "Amazon.png",g8)
ggsave(filename = "Netflix.png",g9)
ggsave(filename = "American Express.png",g10)
ggsave(filename = "Pfizer.png",g11)
ggsave(filename = "Walt Disney.png",g12)