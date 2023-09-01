set.seed(189442)

#Normal
simula <- function(repeticiones){
  #Number of simulation
  n <- 10000
  #We simulate repetitions of size n
  simulaciones <- replicate(repeticiones,rnorm(n,-2,sd=3)) 
  #Sacamos el promedio (un promedio de n variables, obteniendo repeticiones promedios)
  medias <- colMeans(simulaciones)
  medias
}

#Distribución teórica según el TCL
x <- seq(-2.09, -1.91, 0.001)
df <- data.frame(x = x, y = dnorm(x, -2, sd=3/sqrt(10000)))

#Repetimos el experimento con un número distitno de iteraciones
repeticiones <- c(1,10,100,1000)
normal_plots <- list()
for(i in 1:4){
  #Obtenemos las medias
  medias <- simula(repeticiones[i])
  #Preparamos para graficar
  medias <- data.frame(medias)
  plot_title <- paste0("N= ", repeticiones[i])
  
  #Ploteamos y guardamos
  normal_plots[[i]] <- ggplot(medias)+
            geom_histogram(aes(x=medias,y =..density..)) +
            geom_line(data=df, aes(x=x,y=y), color="red")+
            labs(title=plot_title,x="Media",y="")+
            theme_classic()
    
}

#Presentamos las 4 repeticiones en un solo grid
g <- arrangeGrob(normal_plots[[1]],normal_plots[[2]],normal_plots[[3]],normal_plots[[4]],
             top="Simulación de medias para N(-2,3)")
ggsave(file="02_Figures/TCL_Normales.pdf",g)

#Repetimos lo mismo para las otras 3 distribuciones

#Para esto, ajustamos la media y sd de la distribución teórica

#Ji-Cuadrada 15
simula <- function(repeticiones){
  n <- 10000
  simulaciones <- replicate(repeticiones,rchisq(n,15)) 
  medias <- colMeans(simulaciones)
  medias
}

x <- seq(14.5,15.5,0.01)
df <- data.frame(x = x, y = dnorm(x, 15, sd=sqrt(30)/sqrt(10000)))

repeticiones <- c(1,10,100,1000)
chi_plots <- list()
for(i in 1:4){
  
  medias <- simula(repeticiones[i])
  medias <- data.frame(medias)
  plot_title <- paste0("N= ", repeticiones[i])
  
  chi_plots[[i]] <- ggplot(medias)+
    geom_histogram(aes(x=medias,y =..density..)) +
    geom_line(data=df, aes(x=x,y=y), color="red")+
    labs(title=plot_title,x="Media",y="")+
    theme_classic()
  
}

g <- arrangeGrob(chi_plots[[1]],chi_plots[[2]],chi_plots[[3]],chi_plots[[4]],
             top="Simulación de medias para Ji Cuadrada(15)")
ggsave(file="02_Figures/TCL_Ji.pdf",g)


#Exp(3)
simula <- function(repeticiones){
  
  n <- 10000
  simulaciones <- replicate(repeticiones,rexp(n,3)) 
  medias <- colMeans(simulaciones)
  medias
}

x <- seq(0.31,0.35,0.001)
df <- data.frame(x = x, y = dnorm(x, mean = (1/3), sd=(1/3)/sqrt(10000)))

repeticiones <- c(1,10,100,1000)
exp_plots <- list()
for(i in 1:4){
  
  medias <- simula(repeticiones[i])
  medias <- data.frame(medias)
  plot_title <- paste0("N= ", repeticiones[i])
  
  exp_plots[[i]] <- ggplot(medias)+
    geom_histogram(aes(x=medias,y =..density..)) +
    geom_line(data=df, aes(x=x,y=y), color="red")+
    labs(title=plot_title,x="Media",y="")+
    theme_classic()
  
}

g <- arrangeGrob(exp_plots[[1]],exp_plots[[2]],exp_plots[[3]],exp_plots[[4]],
             top="Simulación de medias para Exp(3)")
ggsave(file="02_Figures/TCL_Exp.pdf",g)


#Unif(-5,8)
simula <- function(repeticiones){

  n <- 10000
  simulaciones <- replicate(repeticiones,runif(n,-5,8)) 
  medias <- colMeans(simulaciones)
  medias
}

x <- seq(1.3,1.7,0.001)
df <- data.frame(x = x, y = dnorm(x, mean = 1.5, sd=(13/sqrt(12))/sqrt(10000)))

repeticiones <- c(1,10,100,1000)
unif_plots <- list()
for(i in 1:4){
  
  medias <- simula(repeticiones[i])
  medias <- data.frame(medias)
  plot_title <- paste0("N= ", repeticiones[i])
  
  unif_plots[[i]] <- ggplot(medias)+
    geom_histogram(aes(x=medias,y =..density..)) +
    geom_line(data=df, aes(x=x,y=y), color="red")+
    labs(title=plot_title,x="Media",y="")+
    theme_classic()
  
}

g <- arrangeGrob(unif_plots[[1]],unif_plots[[2]],unif_plots[[3]],unif_plots[[4]],
             top="Simulación de medias para Unif(-5,8)")
ggsave(file="02_Figures/TCL_Unif.pdf",g)