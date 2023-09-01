## Problema 3 - Desigualdad de ingresos
#install.packages("REAT")
#install.packages("wesanderson")
library(REAT)
library(ggtext)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(DescTools)
library(stringr)
library(gridExtra)

#Jalamos los datos
ingresos <- read_csv("https://datos.cdmx.gob.mx/dataset/52f97506-ef52-449a-b2c9-29b6197abaa6/resource/7f0d7073-6861-4d8c-9ca5-17ebe3ff2388/download/remuneraciones_da_qna_10_23.csv")
#Nos quedamos con las variables relevantes
ingresos <- ingresos %>% select(n_puesto,n_cabeza_sector,sueldo_tabular_bruto)

#Coeficiente de gini
DescTools::Gini(ingresos$sueldo_tabular_bruto)


#Gini plot
perfect <- ggplot() + geom_abline(slope=1,intercept=0) + 
  coord_cartesian(ylim = c(0,1),xlim=c(0,1))+
  theme_classic()+
  labs(title=str_wrap("1 In a perfectly equal society, X% of people accumulate X% of total wealth",60),
    x="Percentage of people",y="Percentage of wealth",
    subtitle = "People are sorted based on their wealth") +
    geom_point(aes(x=0.25,y=0.25),color="#0072B2",size=3)+
    annotate("text",x=0.5,y=0.25,label=str_wrap("The 25%  has 25% of total wealth",30),size=3)+
    geom_point(aes(x=0.75,y=0.75),color="#0072B2",size=3)+
    annotate("text",x=0.8,y=0.5,label=str_wrap("The top 25% has 25% of total wealth",30),size=3)
  


unequal <- ggplot() + geom_abline(slope=1,intercept=0) + 
  coord_cartesian(ylim = c(0,1),xlim=c(0,1))+
  theme_classic()+
  labs(title=str_wrap("2 As societies become more unequal, the lower % amass a smaller percentage of wealth",60),
       x="Percentage of people",y="Percentage of wealth") +
  geom_point(aes(x=0.25,y=0.1),color="#0072B2",size=3)+
  annotate("text",x=0.45,y=0.15,label=str_wrap("The 25th percentile now has only 10% of total wealth",30),size=3)+
  geom_point(aes(x=0.75,y=0.60),color="#0072B2",size=3)+
  annotate("text",x=0.8,y=0.5,label=str_wrap("The top 25th percentile now has 40% of total wealth",30),size=3)
  
x <- seq(0,1,0.0001)
lorenz.df <- data.frame(x=x,ymin=(0.2)*x + 0.8*x^2,ymax=x)
  
lorenz <- ggplot() + geom_abline(slope=1,intercept=0) + 
                    coord_cartesian(ylim = c(0,1),xlim=c(0,1))+
                    stat_function(fun=function(x)  (0.2)*x + 0.8*x^2,
                                  color="#0072B2",
                                  linewidth=1.5)+
                    theme_classic()+
                    geom_ribbon(data=lorenz.df,aes(x=x,ymin=ymin,ymax=ymax),
                                alpha=0.2,fill="navyblue")+
                    geom_ribbon(data=lorenz.df,aes(x=x,ymin=0,ymax=ymin),
                                alpha=0.2,fill="red")+
                    labs(title=str_wrap("3 Doing this for all points defines the  <span style='color:#0072B2;'>**Lorenz Curve**</span>",60),
                         subtitle="The futher the Lorenz Curve is from our equality line,\nless equal a society is",
                         x="Percentage of people",y="Percentage of wealth") +
                    scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+
                    theme(plot.title = element_markdown(lineheight = 1.1))

gini_graph <-  ggplot() + geom_abline(slope=1,intercept=0) + 
  coord_cartesian(ylim = c(0,1),xlim=c(0,1))+
  stat_function(fun=function(x)  (0.2)*x + 0.8*x^2,
                color="#0072B2",
                linewidth=1.5)+
  theme_classic()+
  geom_ribbon(data=lorenz.df,aes(x=x,ymin=ymin,ymax=ymax),
              alpha=0.2,fill="navyblue")+
  annotate("text",x=0.5,y=0.4,color="navyblue",label="A",size=7)+
  geom_ribbon(data=lorenz.df,aes(x=x,ymin=0,ymax=ymin),
              alpha=0.2,fill="red")+
  annotate("text",x=0.75,y=0.25,color="red",label="B",size=7)+
  annotate("text",x=0.3,y=0.75,label="Gini = A/(A+B)",size=4)+
  labs(title=stringr::str_wrap("4 The Gini Coefficient is the ratio between 
                      <span style='color:#000080;'>Area A</span> 
                      <br>and the total area under the equality line 
                      (<span style='color:#000080;'>A</span>+<span style='color:#FF0000;'>B</span>)",20),
       subtitle="The greater the Gini Coefficient, less equal a society",
       x="Percentage of people",y="Percentage of wealth") +
  scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0)) +
  theme(plot.title = element_markdown(lineheight = 1.1))




gini_viz <- arrangeGrob(perfect,unequal,lorenz,gini_graph,top="How to calculate the Gini Coefficient")
ggsave("02_Figures/gini_viz.pdf",gini_viz,width = 12.8,height = 8,units="in")


#Histograma
income.hist <- ggplot(ingresos)+
  geom_histogram(aes(x=sueldo_tabular_bruto),
                ,bins=50)+
  scale_x_continuous(labels=scales::dollar_format())+
  labs(x="Sueldo bruto ($ MXN)",y="Frecuencia",
       title="La distribución de los sueldos de la CDMX está sesgada a la derecha")+
  theme_classic()

ggsave("02_Figures/income_histogram.pdf")


#Calcule con qué porcentaje del ingreso se queda el: 0.1 % con mayores ingresos, el 1 % con mayores
# ingresos, el 5 % con mayores ingresos y el 10 % con mayores ingresos.
percentil <- c(0.999,0.99,0.95,0.9)
cuantiles <- quantile(ingresos$sueldo_tabular_bruto,percentil)

top_punto_1 <- ingresos %>% 
                select(sueldo_tabular_bruto) %>% 
                filter(sueldo_tabular_bruto>cuantiles[1])%>%
                sum()

top_1 <- ingresos %>% 
  select(sueldo_tabular_bruto) %>% 
  filter(sueldo_tabular_bruto>cuantiles[2])%>%
  sum()

top_5 <- ingresos %>% 
  select(sueldo_tabular_bruto) %>% 
  filter(sueldo_tabular_bruto>cuantiles[3])%>%
  sum()

top_10 <- ingresos %>% 
  select(sueldo_tabular_bruto) %>% 
  filter(sueldo_tabular_bruto>cuantiles[4])%>%
  sum()
  

total_ingresos <- sum(ingresos$sueldo_tabular_bruto)
top_5/total_ingresos
##Tablita: 
distribucion_ingreso <- data.frame(
  top_porcentaje = (1-percentil)*100,
  porcentaje_ingresos = c(
    top_punto_1/total_ingresos,
    top_1/total_ingresos,
    top_5/total_ingresos,
    top_10/total_ingresos
  )*100
)



