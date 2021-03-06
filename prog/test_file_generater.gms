*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*
*Title: test_file_generater.gms
*Object: make gdx file for R practice
*Programmer: Osamu NISHIURA
*Date: Jun. 29th 2020
*Last update: Jun. 29th 2020
*NOTE:This data has no mean
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*

set
Y year/
2010
2020
2030
2040
2050
/
R region/
a
b
c
/
SC scenairo/
sc1
sc2
/
;

parameter
GDP(Y,R,SC)                 GDP
POP(Y,R,SC)             Population
TPE(Y,R,SC)             Total primary energy use
gr(R,SC)                growth
;


*data arrangement*-------------------------------------------------------------*

GDP(Y,'a',SC)=100 ;
POP(Y,'a',SC)=90 ;
TPE(Y,'a',SC)=40 ;

GDP(Y,'b',SC)=80 ;
POP(Y,'b',SC)=180 ;
TPE(Y,'b',SC)=90 ;

GDP(Y,'c',SC)=50 ;
POP(Y,'c',SC)=200 ;
TPE(Y,'c',SC)=80 ;

gr('a','sc1')=1;
gr('b','sc1')=3;
gr('c','sc1')=1;

gr('a','sc2')=2;
gr('b','sc2')=6;
gr('c','sc2')=4;


GDP(Y,R,SC)=GDP(Y,R,SC)+GDP(Y,R,SC)*gr(R,SC)*log(ord(Y))*0.5;
POP(Y,R,SC)=POP(Y,R,SC)+POP(Y,R,SC)*gr(R,SC)*log(ord(Y))*0.2;
TPE(Y,R,SC)=TPE(Y,R,SC)+TPE(Y,R,SC)*gr(R,SC)*log(ord(Y))*0.6;

*data unloading*-------------------------------------------------------------*

execute_unload 'r_practice.gdx'
GDP,POP,TPE;

