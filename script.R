regresion<- function(direccion){

df3<-read.csv(direccion,sep=",")

df3$xx<-df3[,1]*df3[,1]
df3$xy<-df3[,1]*df3[,2]


x_sum<-sum(df3[,1])
y_sum<-sum(df3[,2])
xx_sum<-sum(df3$xx)
xy_sum<-sum(df3$xy)
b1= (nrow(df3)*(xy_sum)-(x_sum*y_sum)) / (nrow(df3)*xx_sum-(x_sum*x_sum))
b0<- (y_sum-(b1*x_sum)) / nrow(df3)
x_med=x_sum/nrow(df3)
y_med=y_sum/nrow(df3)

df3$"x-X"<-df3[,1]-x_med
df3$"y-Y"<-df3[,2]-y_med
df3$"(x-X)^2"<-df3$`x-X`*df3$`x-X`
df3$"(y-Y)^2"<-df3$`y-Y`*df3$`y-Y`
df3$"(x-X)*(y-Y)"<-df3$`x-X`*df3$`y-Y`

sum_x_x_2<- sum(df3$`(x-X)^2`)
sum_y_Y_2<- sum(df3$`(y-Y)^2`)
sum_x_X_y_Y<-sum(df3$`(x-X)*(y-Y)`)

r<-(sum_x_X_y_Y)/(sqrt(sum_x_x_2)*sqrt(sum_y_Y_2))
r_<-cbind(r)

r2<-array(data=c(r_*r_))
r_2<-cbind(r2)

residuo<-array(data=c(df3[2]-(b1*df3[,1]+b0)))


library(ggplot2)

grafico<-ggplot(df3, aes(x = df3[,1], y = df3[,2])) +
  geom_point() + 
  ggtitle("Grafico No.1:  Chance.of.Admit - CGPA") +
  geom_line(data=df3,aes(x = df3[,1], y = b1*df3[,1]+b0),color="red",size=1) +
  theme_classic()

beta0<- array(data=c(b0))
beta1<- array(data=c(b1))
estimadores<-cbind(beta0,beta1)

my_list<-list(Estimadores=estimadores,r2=r_2,r=r_,Residuos=residuo,Grafico=grafico)
return(my_list)
}