 
################################################################################
## Variante 8: MIT COUNTRY PLOTS #optimiert für 50x75cm Druck
setwd("C:/Users/chris/coding/short_history_R")
library(RColorBrewer)
library(sp) 
load("Polygon Plot/Country Plots/world_countries.rda")

  jj=paste("No_M",round(runif(1,min=2.501,max=1000.499)))
  #png(paste("polygons",jj,"_1200dpi.png",sep=""),width=6000,height=4000,res=1200)
  #postscript(paste("polygons",jj,".eps",sep=""),width=6000,height=4000)#,res=1200)
  #pdf(paste("poly_coun",jj,".pdf",sep=""),width=29.5,height=19.7) #width/height in inch 1:2.54 = 75cm/50cm
  jpeg(paste("polygons_",jj,".jpg",sep=""),width=7500,height=5000,quality=100,res=400)
    y<-18
    x<-27
    layout(matrix(c(1:(x*y)), nrow=y, ncol=x, byrow = F))
    
    #op<-par(no.readonly=T)
    par(mar=c(0,0,0,0))
    
    ##Preparation for colors
    col1<-brewer.pal(9,"Reds")[3:8]
    col2<-brewer.pal(9,"Greens")[3:8]
    col3<-brewer.pal(9,"YlOrBr")[2]#"yellow"
    col_mat1<-col_mat2<-col_mat3<-matrix(rep(NA,(x*y)), nrow=y, ncol=x, byrow = F)
    zufalls_vect<-rep(NA,y)
      for(i in 1:y){zufalls_vect[i]<-sum(round(runif(x-1,min=0,max=1)))}
    #ausnahme_vect<- x*0:(y-1)+zufalls_vect+1
     ausnahme_vect<- 0:(y-1)+zufalls_vect*y

    cht_land_nu<-sample(c(12, 13,16,19,21,39,47,52,53,55,64,71,73,83,90,92,94,101,155,176,183,186,187,207),size=20)     #132,184,217  
    
    for(i in 1:y){
      for(j in 1:x){
          if(j<=zufalls_vect[i]){
          col_mat1[i,j]<-col1[max(6 - j %/% (zufalls_vect[i]%/%5),1)]
          }else{if(j==(zufalls_vect[i]+1)){
          col_mat1[i,j]<-col3
          }else{    
          col_mat1[i,j]<-col2[max(6 - (x-j) %/% ((x-zufalls_vect[i])%/%5),1)]
          }}
    }}          
    count<-0
    
    for(i in 1:(x*y)){

      if(col_mat1[i]==col3){
      count<-count+1
      CHT<-cht_land_nu[count]
          if(CHT==64){plot(world_countries[slot(world_countries, "data")$SP_ID[64],],xlim=c(-5,2),ylim=c(35,45),col= col3,border=NA)}else{
          if(CHT==71){plot(world_countries[slot(world_countries, "data")$SP_ID[71],],xlim=c(-1,6),ylim=c(41,52),col= col3,border=NA)} else{
          if(CHT==73){plot(world_countries[slot(world_countries, "data")$SP_ID[73],],xlim=c(-5,2),ylim=c(50,62),col= col3,border=NA)} else{
          if(CHT==155){plot(world_countries[slot(world_countries, "data")$SP_ID[155],],xlim=c(13.5,14),ylim=c(58,72),col= col3,border=NA)} else{
          if(CHT==13){plot(world_countries[slot(world_countries, "data")$SP_ID[13],],xlim=c(110,165),ylim=c(-35,-20),col= col3,border=NA)} else{
          plot(world_countries[slot(world_countries, "data")$SP_ID[CHT],],col= col3,border=NA )}}}}}
        
        }else{
        plot(1, type="n", axes=F, xlab="", ylab="",ylim=c(0,10),xlim=c(0,10))
        zufall_form<-round(runif(1,min=0,max=1))
        xx<-runif(100,min=0,max=1)
        yy=3+min(which(xx<0.2 ))

        winkel<-360/yy     #VARIANTEN: (pi*2)/yy vergl auch 360/yy oder pi/yy
        startx<-runif(1,min=-winkel/2,max=winkel/2)
        lang<-runif(yy,min=1,max=5)
        grad<-startx+winkel*(1:yy)
  
        cosx<-cos(grad)
        sinx<-sin(grad)
        
        if(zufall_form==0){             
          rect(0,0,10,10,border=NA,col=col_mat1[i])
          polygon(5+cosx*lang,5+sinx*lang,,border=NA,col="white")
        }else{
          rect(0,0,10,10,border=NA,col="white")
          polygon(5+cosx*lang,5+sinx*lang,,border=NA,col=col_mat1[i])
        }
      }
    }
    #par(op)
  dev.off()  

