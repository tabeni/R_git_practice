#Packages------------------------------------------------------
library(tidyverse)
library(gdxrrw)


#data_import---------------------------------------------------
pin<-"../data/r_practice.gdx"    

df_gdp<- rgdx.param(pin,"GDP")
df_pop<- rgdx.param(pin,"POP")
df_tpe<- rgdx.param(pin,"TPE")

df<-merge(df_gdp,df_pop,all=T)%>%
  merge(df_tpe,all=T)
df$Y = as.character(df$Y)
df$Y = as.numeric(df$Y)
df$R = as.character(df$R)
df$SC = as.character(df$SC)


#Q1---------------------------------------------------------
df_GDP_dif<-select(df,"Y","R","SC","GDP") %>%
spread(key = SC, value = GDP) 

mutate(df_GDP_dif, GDP_dif = df_GDP_dif$sc2 - df_GDP_dif$sc1)%>%
select("Y","R","GDP_dif")%>%
filter(Y != 2010) %>%
spread(key = Y, value = GDP_dif) %>%  
wgdx.reshape(gdxName="../output/Q1.gdx",symName="GDP_dif",symDim=2,tName="year")


#Q2------------------------------------------------------------
mutate(df, GDPcap= df$GDP/df$POP )%>%
select("Y","R","SC","GDPcap")%>%
  spread(key = Y, value = GDPcap) %>%  
  wgdx.reshape(gdxName="../output/Q2.gdx",symName="GDPcap",symDim=3,tName="year")


#Q3----------------------------------------------------------
select(df,"Y","R","SC","POP")%>%
  group_by(R,SC)%>%
  mutate(POP_dif=(POP-lag(POP))*10/lag(POP))%>%
  na.omit()%>%
  select("Y","R","SC","POP_dif")%>%
  mutate( Y = recode(Y,"2020"="2010 to 2020",
                           "2030"="2020 to 2030",
                           "2040"="2030 to 2040",
                           "2050"="2040 to 2050"
  )) %>% 
  spread(key="Y",value="POP_dif") %>%
  wgdx.reshape(gdxName="../output/Q3.gdx",symName="POP_dif",symDim=3,tName="year")


#Q4----------------------------------------------------------
full_join(filter(df,Y==2050,SC=="sc1"),filter(df,Y==2050,SC=="sc2"),by=c("Y","R"))%>%
select("R","GDP.x","GDP.y","POP.x","POP.y","TPE.x","TPE.y")%>%
rename(Region = R, 
       GDP_baseline = GDP.x, 
       Population_baseline = POP.x, 
       primary_energy_consumption_baseline = TPE.x, 
       GDP_high_growth = GDP.y, 
       Population_high_growth = POP.y, 
       primary_energy_consumption_high_growth = TPE.y)%>%
write_csv(path="../output/Q4.csv")    


#omake-------------------------------------------------
library(gt)

full_join(filter(df,Y==2050,SC=="sc1"),filter(df,Y==2050,SC=="sc2"),by=c("Y","R"))%>%
select("R","GDP.x","GDP.y","POP.x","POP.y","TPE.x","TPE.y")%>%
  gt()%>%
tab_spanner(label = "baseline",
            columns = vars(GDP.x,POP.x,TPE.x)
            )%>%
tab_spanner(label = "high_growth",
            columns = vars(GDP.y,POP.y,TPE.y)
            )%>%
tab_header(
    title = "r_practice.gdx",
    subtitle = md("2050")
  )  












  