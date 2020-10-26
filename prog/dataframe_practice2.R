#Packages------------------------------------------------------
library(tidyverse)
library(gdxrrw)

#vector----------------------------------------------------------
a <- c("apple","orange","grape","apple","orange")
b <- c("ringo","mikan","budou","ringo","mikan")
c <- c(10,12,3,8,15)
d <- c(T,T,T,F,T)


#make data frame-------------------------------------------------
df <- data.frame(name=a,japanese=b,weight=c,damage=d)
View(df)

#dataframe handling----------------------------------------------------
#data input--------------------------------------------------------
df_r <- read.csv("../data/rice.csv")
df_o <- read.csv("../data/onion.csv")

df_r <- read_csv("../data/rice.csv")
df_o <- read_csv("../data/onion.csv")


#data check----------------------------------------------------------
View(df_r)
View(df_o)

str(df_r)
summary(df_r)
attributes(df_r)


#filter select pipe---------------------------------------------------
filter(df_r,region=="a"|region=="b")%>%
  select("region","name","2003","2004","2005")%>%
  View()


#gather--------------------------------------------------------------
df_r <- gather(df_r,-c('region','name'), key='year', value='weight')
df_o <- gather(df_o,-c('region','name'), key='year', value='weight')

filter(df_r,region=="a")%>%
View()


#rbind----------------------------------------------------------------
df_b <- rbind(df_r,df_o)
filter(df_b)  %>%
  View()


#mutate------------------------------------------------------------------
df_b <- mutate(df_b, name=recode(name,"akitakomachi"="rice",
                            "koshihikari"="rice",
                            "kirara"="rice",
                            "kogane"="onion",
                            "kohaku"="onion"
                            ))

df_r <- mutate(df_r,species="rice")
df_o <- mutate(df_o,species="onion")


#dataframe spread---------------------------------------------------------
df_r <- spread(df_r, key = species, value = weight)
df_o <- spread(df_o, key = species, value = weight)

filter(df_r,region=="a")%>%
View()


#merge--------------------------------------------------------------------
df_a <- merge(df_r[,c(1,3,4)],df_o[,c(1,3,4)],all=T)
filter(df_a,region=="f")
View()


#datafrae Na handle----------------------------------------------------------
View(na.omit(df_a))

df_a[is.na(df_a)]<-0

df_a <- gather(df_a,-c('region','year'), key='species', value='weight')
View(filter(df_a,region=="f"))


#join-----------------------------------------------------------------------
full_join(df_a,df_b,by=c("species" = "name","region","year"))%>%
  filter(year=="2005")%>%
  View()

df_b[,2]<-as.character(df_b[,2])

full_join(df_a,df_b,by=c("species" = "name","region","year"))%>%
  filter(year=="2005")%>%
  View()


#if loop----------------------------------------------------------------------
s<-0
for (i in 1:nrow(df_a)){
  if(df_a[i,3] == "rice"){
    s <- s + df_a[i,4]*10
  } else {
    s <- s + df_a[i,4]*20
  }
}
s


#read gdx file-------------------------------------------------------------
df_gdx_gdp<- rgdx.param("../data/r_practice.gdx","GDP")

df_gdx_gdp$Y<- as.character(df_gdx_gdp$Y)
df_gdx_gdp$Y<- as.numeric(df_gdx_gdp$Y)









