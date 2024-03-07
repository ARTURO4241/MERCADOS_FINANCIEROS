#FUNCION
DECICIONES_BOLLINGER <- function(ultimo_valor_real, media, arriba, abajo){
  if (ultimo_valor_real > media){
    if (ultimo_valor_real<arriba){
      print("VENDE")
      return (-0.5)
    } else {
      print("VENDE TODO")
      return (-1)
    }
  } else {
    if (ultimo_valor_real < media){
      if (media>abajo){
        print('COMPRA')
        return (0.5)
      } else {
        print('COMPRA TODO')
        return (1)
      }
    } else {
      print ('MANTENER')
      return (0)
    }
  }
}  

DECICIONES_MACD <- function(valor,media){
  if (valor>0){
    if (media>valor){
      print("COMPRA ESPECULATIVA")
      return (0.5)
    } else {
      print ("COMPRA FUERTE")
      return (1)
    }
  }else{
    if (0>valor){
      if (media>valor){
        print('VENTA FUERTE')
        return (-1)
      } else {
        print('VENTA ESPECULATIVA')
        return (-0.5)
      }
    } else {
      print("ESPERA UN POCO, ES MOMENTO DE CAMBIOS")
      return (0)
    }
  }
}

DECICIONES_STOCH <- function(valor){
  if (valor>0.8){
      print ("VENTA")
      return (-0.5)
    }else{
    if (0.2>valor){
      print('COMPRA')
      return (0.5)
    } else {
      print('EL INSTRUMENTO OPERA EN RANGO')
      return (0)
    }
  }
}

DECICIONES <- function(valor){
  prom=mean(valor)
  if (prom<0.3){
    if (prom>-0.3){
      return ("ESPERAR")
    }else{
      if (prom>-0.6){
        return("VENTA ESPECULATIVA")
      } else {
        return("VENTA FUERTE")
      }
    }
  }else{
      if (prom<0.6){
        return("COMPRA ESPECULATIVA")
      }else{
        return("COMPRA FUERTE")
      }
    }
  }

mode <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

source("https://raw.githubusercontent.com/OscarVDelatorreTorres/yahooFinance/main/datosMultiplesYahooFinance.R")
# Ejemplo para descargar los históricos diarios de grupo Alfa, Microsoft en EEUU, Micrososft en México y el índice S&P/BMV IPC, desde el 1 de enrdo de 2023 a la fecha actual:
tickerV=c("ALFAA.MX","MSFT","MSFT.MX","^MXX")
deD="2023-01-01"
hastaD=Sys.Date()
per="D"

Datos=historico_multiples_precios(tickers=tickerV,de=deD,hasta=hastaD,periodicidad=per)

#EJERCICIO 1
#Para cada uno de los elementos en tickerV se extrae el cierre y se crea la banda de Bollinger
df1 <- as.data.frame(Datos[1])
df1_columna <- df1$ALFAA.MX.Close
bbands1 <-   BBands(df1_columna, n=20,sd=2)

df2 <- as.data.frame(Datos[2])
df2_columna <- df2$MSFT.Close
bbands2 <-   BBands(df2_columna, n=20,sd=2)

df3 <- as.data.frame(Datos[3])
df3_columna <- df3$MSFT.MX.Close
bbands3 <-   BBands(df3_columna, n=20,sd=2)

df4 <- as.data.frame(Datos[4])
df4_columna <- df4$MXX.Close
bbands4 <-   BBands(df4_columna, n=20,sd=2)

#EJERCICIO 2

print("************************* RECOMENDACION CON BOLLINGER ***************************")

RESUMEN1 <- as.data.frame(bbands1)
ultimo_valor_real_1=df1_columna[length(df1_columna)]
banda_inf1=RESUMEN1$dn
banda_sup1=RESUMEN1$up
media1=RESUMEN1$mavg
print("LA DECICION PARA ALFAA.MX ES: ")
decicion1b=DECICIONES_BOLLINGER(ultimo_valor_real_1[length(ultimo_valor_real_1)],media1[length(media1)],banda_sup1[length(banda_sup1)],banda_inf1[length(banda_inf1)])

RESUMEN2 <- as.data.frame(bbands2)
ultimo_valor_real_2=df2_columna[length(df2_columna)]
banda_inf2=RESUMEN2$dn
banda_sup2=RESUMEN2$up
media2=RESUMEN2$mavg
print("LA DECICION PARA MSFT ES: ")
decicion2b=DECICIONES_BOLLINGER(ultimo_valor_real_2[length(ultimo_valor_real_2)],media2[length(media2)],banda_sup2[length(banda_sup2)],banda_inf2[length(banda_inf2)])

RESUMEN3 <- as.data.frame(bbands3)
ultimo_valor_real_3=df3_columna[length(df3_columna)]
banda_inf3=RESUMEN3$dn
banda_sup3=RESUMEN3$up
media3=RESUMEN3$mavg
print("LA DECICION PARA MSFT.MX ES: ")
decicion3b=DECICIONES_BOLLINGER(ultimo_valor_real_3[length(ultimo_valor_real_3)],media3[length(media3)],banda_sup3[length(banda_sup3)],banda_inf3[length(banda_inf3)])

RESUMEN4 <- as.data.frame(bbands4)
ultimo_valor_real_4=df4_columna[length(df4_columna)]
banda_inf4=RESUMEN2$dn
banda_sup4=RESUMEN4$up
media4=RESUMEN4$mavg
print("LA DECICION PARA MXX ES: ")
decicion4b=DECICIONES_BOLLINGER(ultimo_valor_real_4[length(ultimo_valor_real_4)],media4[length(media4)],banda_sup4[length(banda_sup4)],banda_inf4[length(banda_inf4)])

print("************************* MACD ***************************")

print("LA DECICION PARA ALFAA.MX ES: ")
MACD_1<- as.data.frame(MACD(df1_columna))
banda_macd1=MACD_1$macd
banda_signal1=MACD_1$signal
decicion1m=DECICIONES_MACD(banda_macd1[length(banda_macd1)],banda_signal1[length(banda_signal1)])

print("LA DECICION PARA MSFT ES: ")
MACD_2<- as.data.frame(MACD(df2_columna))
banda_macd2=MACD_2$macd
banda_signal2=MACD_2$signal
decicion2m=DECICIONES_MACD(banda_macd2[length(banda_macd2)],banda_signal2[length(banda_signal2)])

print("LA DECICION PARA MSFT.MX ES: ")
MACD_3<- as.data.frame(MACD(df3_columna))
banda_macd3=MACD_3$macd
banda_signal3=MACD_3$signal
decicion3m=DECICIONES_MACD(banda_macd3[length(banda_macd3)],banda_signal3[length(banda_signal3)])

print("LA DECICION PARA MXX ES: ")
MACD_4<- as.data.frame(MACD(df4_columna))
banda_macd4=MACD_4$macd
banda_signal4=MACD_4$signal
decicion4m=DECICIONES_MACD(banda_macd4[length(banda_macd4)],banda_signal4[length(banda_signal4)])

print("************************* OSCILADOR ESTOCASTICO ***************************")

print("LA DECICION PARA ALFAA.MX ES: ")
OE_1<- as.data.frame(stoch(df1_columna,nFastK = 7,nFastD = 3,nSlowD = 3,bounded = TRUE))
banda_valor1=OE_1$fastK
decicion1s=DECICIONES_STOCH(banda_valor1[length(banda_valor1)])

print("LA DECICION PARA MSFT ES: ")
OE_2<- as.data.frame(stoch(df2_columna,nFastK = 7,nFastD = 3,nSlowD = 3,bounded = TRUE))
banda_valor2=OE_2$fastK
decicion2s=DECICIONES_STOCH(banda_valor2[length(banda_valor2)])

print("LA DECICION PARA MSFT.MX ES: ")
OE_3<- as.data.frame(stoch(df3_columna,nFastK = 7,nFastD = 3,nSlowD = 3,bounded = TRUE))
banda_valor3=OE_3$fastK
decicion3s=DECICIONES_STOCH(banda_valor3[length(banda_valor3)])

print("LA DECICION PARA MXX ES: ")
OE_4<- as.data.frame(stoch(df4_columna,nFastK = 7,nFastD = 3,nSlowD = 3,bounded = TRUE))
banda_valor4=OE_4$fastK
decicion4s=DECICIONES_STOCH(banda_valor4[length(banda_valor4)])

print("************************* DECICIONES ***************************")
#La decision será en base al promedio booleano 1 si hay que comprar, -1 vender y 0 mantener

ac1 <- c(decicion1b,decicion1m,decicion1s)
ac2 <- c(decicion2b,decicion2m,decicion2s)
ac3 <- c(decicion3b,decicion3m,decicion3s)
ac4 <- c(decicion4b,decicion4m,decicion4s)

print(paste("LA DECISIÓN DE ALFAA.MX ES: ",DECICIONES(mode(ac1))))
print(paste("LA DECISIÓN DE MSFT ES: ",DECICIONES(mode(ac2))))
print(paste("LA DECISIÓN DE MSFT.MX ES: ",DECICIONES(mode(ac3))))
print(paste("LA DECISIÓN DE MXX ES: ",DECICIONES(mode(ac4))))

print("************************* GRAFICA DEL DOLAR ***************************")
tickerV=c("MXN=X")
deD="2023-01-01"
hastaD=Sys.Date()
per="D"
Datos2=historico_multiples_precios(tickers=tickerV,de=deD,hasta=hastaD,periodicidad=per)
df <- as.data.frame(Datos2[1])
ggplot(df,aes(x=MXNX.Date,y=MXNX.Close))+geom_line()+geom_point()

print("************************* GRAFICA BASE 100 ***************************")
a=df$MXNX.Close*100/df$MXNX.Close[1]
b=df$MXNX.Date
DF=data.frame(FECHAS=b,VALORES=a)
ggplot(DF,aes(x=FECHAS,y=VALORES))+geom_line()+geom_point()