library(plotly)
library(ggplot2)
library(readxl)
library(tidyverse)
library(shiny)

definitiva <- read_excel("Tabla_definitivaV2.0.xlsx")

media.nacional <- NULL
for(i in 2002:2018){
  mediaaño <- mean(sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2), na.rm = T)
  media.nacional <- rbind(media.nacional, mediaaño) 
  
}

desv.nacional <- NULL
for(i in 2002:2018){
  sdaño <- sd(sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2), na.rm = T)
  desv.nacional <- rbind(desv.nacional, sdaño) 
  
}

años <- 2002:2018

datosnal <- data_frame(años, media.nacional, desv.nacional)
colnames(datosnal) <- c("year", "media.nacional", "sd.nacional")


muni <- "PUERTO LLERAS - META"
desviacion <- 1
tasa.var <- 0.3


mediaquinquenio1 <- apply(sapply(select (definitiva, contains(paste0(".tasa"))), as.numeric, 1)[,1:5], 1, mean, na.rm=T)

  media.municipal <- NULL 
  for(i in 6:17){
    mediaquinquenio <- apply(sapply(select (definitiva, contains(paste0(".tasa"))), as.numeric, 1)[,(i-4):i], 1, mean, na.rm=T)
    media.municipal <- cbind(media.municipal, mediaquinquenio)
  }


constante <- matrix(rep(mediaquinquenio1), ncol=5, nrow=1122)


media.movil <- cbind(constante,media.municipal)
  colnames(media.movil) <- años
  rownames(media.movil) <- definitiva$`MUNICIPIO - DEPARTAMENTO`
  


sdquinquenio1 <- apply(sapply(select (definitiva, contains(paste0(".tasa"))), as.numeric, 1)[,1:5], 1, sd, na.rm=T)


sd.municipal <- NULL
  for(i in 6:17){
    sdquinquenio <- apply(sapply(select (definitiva, contains(paste0(".tasa"))), as.numeric, 1)[,(i-4):i], 1, sd, na.rm=T)
    sd.municipal <- cbind(sd.municipal, sdquinquenio)
  }


constante2 <- matrix(rep(sdquinquenio1), ncol=5, nrow=1122)


sd.movil <- cbind(constante2,sd.municipal)
  rownames(sd.movil) <- definitiva$`MUNICIPIO - DEPARTAMENTO`
  colnames(sd.movil)  <- años
 



plotdata3 <- data.frame(x=datosnal$year, y=datosnal$media.nacional, 
                      mun= as.numeric(t(definitiva[definitiva$`MUNICIPIO - DEPARTAMENTO`== muni, 9:25])),
                      upper3 = datosnal$media.nacional+3*datosnal$sd.nacional, 
                      upper2 = datosnal$media.nacional+2*datosnal$sd.nacional, 
                      upper = datosnal$media.nacional+datosnal$sd.nacional,
                      lower= datosnal$media.nacional-datosnal$sd.nacional,
                      movil= media.movil[rownames(media.movil) == muni,],
                      sdmovil =sd.movil[rownames(sd.movil) == muni,]
  )
  colnames(plotdata3) <- c("Año", "Tasa", "Tasa_Municipio",  "más_3_desv.","más_2_desv.",   "más_1_desv.","menos_1_desv.", "Movil", "SDMovil")
  
 

 plotdata4 <- plotdata3 %>%
    mutate(tsa.cmbio= (Tasa_Municipio-lag(Tasa_Municipio, na.pad = TRUE))/(lag(Tasa_Municipio, na.pad = TRUE)), dfrncia=
             Tasa_Municipio-lag(Tasa_Municipio, na.pad = TRUE) )


  p <- ggplot(plotdata4) + geom_line(aes(y=Tasa, x=Año, colour = "blue"))  + 
    geom_line(aes(y=Tasa_Municipio, x=Año, colour = "red")) +
    geom_line(aes(y=Movil, x=Año, colour = "green")) +
    geom_ribbon(aes(ymin=más_2_desv., ymax=más_3_desv., x=Año, fill = "red"), alpha = 0.3)+
    geom_ribbon(aes(ymin=más_1_desv., ymax=más_2_desv., x=Año, fill = "orange"), alpha = 0.3)+
    geom_ribbon(aes(ymin=Tasa, ymax=más_1_desv., x=Año, fill = "yellow") ,alpha = 0.3)+
    geom_ribbon(aes(ymin=Movil, ymax=Movil + SDMovil*input$porcentaje2, x=Año, fill = "darkgreen") ,alpha = 0.3)+
    geom_ribbon(aes(ymin=menos_1_desv., ymax=Tasa, x=Año, fill = "lightgreen") ,alpha = 0.3)+scale_fill_identity() + theme(legend.position="none")+
   
     geom_point(data=subset(plotdata4, Tasa_Municipio>Movil+SDMovil*desviacion), aes(y=Tasa_Municipio,x=Año)) +
   
    geom_text(data=subset(plotdata4, Tasa_Municipio>Movil+SDMovil*desviacion) & tsa.cmbio>(tasa.var/100), aes(y=Tasa_Municipio,x=Año, label=paste0(round(tsa.cmbio,2)*100, "%"),hjust = 0, nudge_x = 0.5))+
    ggtitle(paste0(muni, " : ", "Picos con más de ", desviacion, " desv. est. arriba de media móvil municipal")) +
     geom_hline(yintercept = mean(plotdata4$Tasa_Municipio, na.rm=TRUE))
  
  # nrow(subset(plotdata4, Tasa_Municipio>Movil+SDMovil*desviacion))
  
  p <- ggplotly(p)
  p
  
  
  
  nivel_violencia <- NULL
  for(i in 2002:2018){
    picosaño <- filter(definitiva, sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2) < datosnal$sd.nacional[datosnal$year ==i] + datosnal$media.nacional[datosnal$year ==i] & sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2) > datosnal$media.nacional[datosnal$year ==i]) %>% mutate(Año = i) %>%  select("CÓDIGO MUNICIPIO", "MUNICIPIO", "DEPARTAMENTO", "MUNICIPIO - DEPARTAMENTO",Año, "Tasa" = paste0(i,".tasa"), "Conteo" = paste0(i,".conteo"),"Fuente" = paste0(i,".fuente"))
    violencialeve <- rbind(violencialeve, picosaño) 
    
  }
  
  violenciamedia <- NULL
  for(i in 2002:2018){
    picosaño <- filter(definitiva, sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2) < 2*datosnal$sd.nacional[datosnal$year ==i] + datosnal$media.nacional[datosnal$year ==i]  & sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2) > datosnal$media.nacional[datosnal$year ==i]+ datosnal$sd.nacional[datosnal$year ==i]) %>% mutate(Año = i) %>%  select("CÓDIGO MUNICIPIO", "MUNICIPIO", "DEPARTAMENTO", "MUNICIPIO - DEPARTAMENTO",Año, "Tasa" = paste0(i,".tasa"), "Conteo" = paste0(i,".conteo"),"Fuente" = paste0(i,".fuente"))
    violenciamedia <- rbind(violenciamedia, picosaño) 
    
  }
  
  violenciaalta <- NULL
  for(i in 2002:2018){
    picosaño <- filter(definitiva, sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2) < 3*datosnal$sd.nacional[datosnal$year ==i] + datosnal$media.nacional[datosnal$year ==i]  & sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2) > datosnal$media.nacional[datosnal$year ==i]+ 2*datosnal$sd.nacional[datosnal$year ==i]) %>% mutate(Año = i) %>%  select("CÓDIGO MUNICIPIO", "MUNICIPIO", "DEPARTAMENTO", "MUNICIPIO - DEPARTAMENTO",Año, "Tasa" = paste0(i,".tasa"), "Conteo" = paste0(i,".conteo"),"Fuente" = paste0(i,".fuente"))
    violenciaalta <- rbind(violenciaalta, picosaño) 
    
  }
  
  violenciaaltisima <- NULL
  for(i in 2002:2018){
    picosaño <- filter(definitiva, sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2) < 10*datosnal$sd.nacional[datosnal$year ==i] + datosnal$media.nacional[datosnal$year ==i]  & sapply(select (definitiva, contains (paste0(i,".tasa"))), as.numeric, 2) > datosnal$media.nacional[datosnal$year ==i]+ 3*datosnal$sd.nacional[datosnal$year ==i]) %>% mutate(Año = i) %>%  select("CÓDIGO MUNICIPIO", "MUNICIPIO", "DEPARTAMENTO", "MUNICIPIO - DEPARTAMENTO",Año, "Tasa" = paste0(i,".tasa"), "Conteo" = paste0(i,".conteo"),"Fuente" = paste0(i,".fuente"))
    violenciaaltisima <- rbind(violenciaaltisima, picosaño) 
    
  }
  
  violencialeve$CODIGO_MUNICIPIO <- violencialeve$`CÓDIGO MUNICIPIO`
  violenciamedia$CODIGO_MUNICIPIO <- violenciamedia$`CÓDIGO MUNICIPIO`
  violenciaalta$CODIGO_MUNICIPIO <- violenciaalta$`CÓDIGO MUNICIPIO`
  violenciaaltisima$CODIGO_MUNICIPIO <- violenciaaltisima$`CÓDIGO MUNICIPIO`
  
  
  conteopicos4 <- function( varc){
   violenciaaltisima%>% group_by_(.dots = list("DEPARTAMENTO", "CODIGO_MUNICIPIO", varc)) %>% 
      summarise(Total = n()) %>% rename_(.dots=list("MUNICIPIO"=varc)) %>% 
      select( MUNICIPIO, DEPARTAMENTO, Total)
  }
  
  conteo4 <- conteopicos4("MUNICIPIO")
  
  conteopicos3 <- function( varc){
    violenciaalta%>% group_by_(.dots = list("DEPARTAMENTO", "CODIGO_MUNICIPIO", varc)) %>% 
      summarise(Total = n()) %>% rename_(.dots=list("MUNICIPIO"=varc)) %>% 
      select( MUNICIPIO, DEPARTAMENTO, Total)
  }
  
  conteo3 <- conteopicos3("MUNICIPIO")
 
  
  conteopicos1 <- function( varc){
    violencialeve %>% group_by_(.dots = list("DEPARTAMENTO", "CODIGO_MUNICIPIO", varc)) %>% 
      summarise(Total = n()) %>% rename_(.dots=list("MUNICIPIO"=varc)) %>% 
      select( MUNICIPIO, DEPARTAMENTO, Total)
  }
  
  conteo <- conteopicos1("MUNICIPIO")
  conteopicos2 <- function( varc){
    violenciamedia %>% group_by_(.dots = list("DEPARTAMENTO", "CODIGO_MUNICIPIO", varc)) %>% 
      summarise(Total = n()) %>% rename_(.dots=list("MUNICIPIO"=varc)) %>% 
      select( MUNICIPIO, DEPARTAMENTO, Total)
  }
  
  conteo2 <- conteopicos2("MUNICIPIO")
  
  conteo2 <- conteo2[conteo2$Total >= 3,]
  
  i<- 81794
for(i in violenciaaltisima$`CÓDIGO MUNICIPIO`){
assign(paste0("consecutive",i), split(violenciaaltisima[violenciaaltisima$`CÓDIGO MUNICIPIO`==i,]$Año, cumsum(c(1, diff(violenciaaltisima[violenciaaltisima$`CÓDIGO MUNICIPIO`==i,]$Año) != 1))))
  }

  consecutive54250[lengths(consecutive54250)==max(lengths(consecutive54250))][1]

paste("(",paste(as.character(unlist(split(violenciaaltisima[violenciaaltisima$`CÓDIGO MUNICIPIO`==i,]$Año, cumsum(c(1, diff(violenciaaltisima[violenciaaltisima$`CÓDIGO MUNICIPIO`==i,]$Año) != 1))))), collapse = ", "),")")
  for(i in violenciaaltisima$`CÓDIGO MUNICIPIO`){
for(k in 1:length(eval(as.name(paste0("consecutive",i))))){
  if(length(eval(as.name(paste0("consecutive",i)))) <k) {
    break
  }
if(length(eval(as.name(paste0("consecutive",i)))[[k]])==1){
  assign(paste0("consecutive",i),eval(as.name(paste0("consecutive",i)))[-k])
}
  
}
  }
    
  
  for(i in levels(as.factor(violenciaaltisima$`CÓDIGO MUNICIPIO`))){
  if(length(eval(as.name(paste0("consecutive",as.character(i)))))==0){
    rm(list=c(paste0("consecutive",i)))
  }
  }
  
  
  listaconsecu <- NULL
  for(i in levels(as.factor(violenciaaltisima$`CÓDIGO MUNICIPIO`))){
    if(length(eval(as.name(paste0("consecutive",as.character(i)))))==0){
      rm(list=c(paste0("consecutive",i)))
    }else{
      listaconsecu <-c(listaconsecu, violenciaaltisima$`MUNICIPIO - DEPARTAMENTO`[violenciaaltisima$`CÓDIGO MUNICIPIO`==i])
    }
  }
  
  
  