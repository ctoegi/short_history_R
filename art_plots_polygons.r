
setwd("C:\\Dokumente und Einstellungen\\toc\\Eigene Dateien\\R\\Art_Plots")
library(RColorBrewer)

##Variante 1 (kann durch die winkelfunktion geändert werden)

#postscript("polygons.eps",width=4000,height=4000)


#for(j in 1){ #um mehrere Plots zu erzeugen)
 
  j=paste("An-No-CI",round(runif(1,min=2.501,max=100.499)))
  png(paste("polygons",j,".png",sep=""),width=6000,height=6000,res=1200)
    x<-20
    layout(matrix(c(1:x^2), nrow=x, ncol=x, byrow = TRUE))
    par(mar=c(0,0,0,0))
    
    ##Preparation for 3:
    col1<-brewer.pal(9,"Reds")[3:8]
    col2<-brewer.pal(9,"Greens")[3:8]
    col_mat1<-col_mat2<-matrix(rep(NA,x^2), nrow=x, ncol=x)
    zufalls_mat1<-zufalls_mat2<-matrix(rep(NA,x^2), nrow=x, ncol=x)
    zufalls_mat1<-matrix(runif(x^2,min=0,max=1), nrow=x, ncol=x)
    zufalls_mat2<-matrix(runif(x^2,min=0,max=1), nrow=x, ncol=x)
    prob_mat_col<-matrix(rep(1:x/x-1/x/2,x),nrow=x, ncol=x)
    #hier 4 oder 5: einfach mat_CI2 und mat_CI1 ersetzen
    mat_CI2<-matrix(rep(1:x/x-1/x/2,x)*length(col1),nrow=x, ncol=x,byrow=F)#byrow=F
    mat_CI1<-matrix(rep(x:1/x-1/x/2,x)*length(col2),nrow=x, ncol=x,byrow=F)#byrow=F
    col_1or2<-zufalls_mat1>prob_mat_col
    ##Variante3
    #for(i in 1:x){
    #  for(j in 1:x){ 
    #    if(col_1or2[i,j]==TRUE){
    #    col_mat1[i,j]<-col1[ceiling(mat_CI1[i,j]*zufalls_mat2[i,j])]
    #    }else{
   #     col_mat1[i,j]<-col2[ceiling(mat_CI2[i,j]*zufalls_mat2[i,j])]
   #     }
   # }}
    ##Variante4
    col_1or2<-zufalls_mat1>prob_mat_col
    for(i in 1:x){
      for(j in 1:x){ 
        if(col_1or2[i,j]==TRUE){
        col_mat1[i,j]<-col1[ceiling(mat_CI1[i,j])]
        }else{
        col_mat1[i,j]<-col2[ceiling(mat_CI2[i,j])]
        }
    }}
    for(i in 1:x^2){
      xx<-runif(100,min=0,max=1)
      yy=3+min(which(xx<0.2 ))
      #yy<-1+ ceiling(i/10) + i%%10


      winkel<-360/yy     #VARIANTEN: (pi*2)/yy vergl auch 360/yy oder pi/yy
      startx<-runif(1,min=-winkel/2,max=winkel/2)
      lang<-runif(yy,min=1,max=5)
      grad<-startx+winkel*(1:yy)

      cosx<-cos(grad)
      sinx<-sin(grad)
      #V1: Rainbow
      #color1<-rainbow(10)[round(runif(1,min=0.501,max=10.499))]
      #V2: One color scheme (Blues, Reds)
      color1<-brewer.pal(9,"Blues")[round(runif(1,min=2.501,max=8.499))]      
      plot(1, type="n", axes=F, xlab="", ylab="",ylim=c(0,10),xlim=c(0,10))
      
      #polygon(5+cosx*lang,5+sinx*lang,,border=NA,col=color1)
      polygon(5+cosx*lang,5+sinx*lang,,border=NA,col=col_mat1[i])
    }
  dev.off()
#}


################################################################################
## Variante 6 (neuer Code) #optimiert für 50x75cm Druck
##inkl Variante 7 (mit Weiß auf Farben)

  jj=paste("No_M",round(runif(1,min=2.501,max=1000.499)))
  #png(paste("polygons",jj,"_1200dpi.png",sep=""),width=6000,height=4000,res=1200)
  #postscript(paste("polygons",jj,".eps",sep=""),width=6000,height=4000)#,res=1200)
  pdf(paste("polygons_new",jj,".pdf",sep=""),width=29.5,height=19.7) #width/height in inch 1:2.54 = 75cm/50cm
    y<-18
    x<-27
    layout(matrix(c(1:(x*y)), nrow=y, ncol=x, byrow = F))
    
    #op<-par(no.readonly=T)
    par(mar=c(0,0,0,0))
    
    ##Preparation for colors
    col1<-brewer.pal(9,"Reds")[3:8]
    col2<-brewer.pal(9,"Greens")[3:8]
    col3<-brewer.pal(9,"YlOrBr")[2]#"yellow"
    col_mat1<-col_mat2<-col_mat3<-matrix(rep(NA,(x*y)), nrow=y, ncol=x, byrow = TRUE)
    zufalls_vect<-rep(NA,y)
    for(i in 1:y){zufalls_vect[i]<-sum(round(runif(x-1,min=0,max=1)))}
    
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
    for(i in 1:(x*y)){
      xx<-runif(100,min=0,max=1)
      yy=3+min(which(xx<0.2 ))
      #yy<-1+ ceiling(i/10) + i%%10

      winkel<-360/yy     #VARIANTEN: (pi*2)/yy vergl auch 360/yy oder pi/yy
      startx<-runif(1,min=-winkel/2,max=winkel/2)
      lang<-runif(yy,min=1,max=5)
      grad<-startx+winkel*(1:yy)

      cosx<-cos(grad)
      sinx<-sin(grad)
      ##Variante 6
      #plot(1, type="n", axes=F, xlab="", ylab="",ylim=c(0,10),xlim=c(0,10))
      #polygon(5+cosx*lang,5+sinx*lang,,border=NA,col=col_mat1[i])
      ##Variante 7
      #plot(1, type="n", axes=F, xlab="", ylab="",ylim=c(0,10),xlim=c(0,10))
      #rect(0,0,10,10,col=col_mat1[i],border=NA)
      #polygon(5+cosx*lang,5+sinx*lang,,border=NA,col="white")
      ##Variante 8+9
      zufall_form<-round(runif(1,min=0,max=1))
      plot(1, type="n", axes=F, xlab="", ylab="",ylim=c(0,10),xlim=c(0,10))
      ##=Variante 8 
      if(zufall_form==0){
      ##=Variante 9
      #if(i%%2==0){
        rect(0,0,10,10,border=NA,col=col_mat1[i])
        polygon(5+cosx*lang,5+sinx*lang,,border=NA,col="white")
      }else{
        rect(0,0,10,10,border=NA,col="white")
        polygon(5+cosx*lang,5+sinx*lang,,border=NA,col=col_mat1[i])
      }
    }
    
    #par(op)
  dev.off()
  

################################################################################
## Variante 8: MIT COUNTRY PLOTS #optimiert für 50x75cm Druck
library(sp) 
load("C:\\Dokumente und Einstellungen\\toc\\Eigene Dateien\\R\\Art_Plots\\Country Plots\\world_countries.rda")

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

################################################################################

## Variante 10: REDS ONLY (wie Julia Herwig) #optimiert für 20/30 Druck

  jj=paste("Reds",round(runif(1,min=2.501,max=1000.499)))
  jpeg(paste("polygons_",jj,".jpg",sep=""),width=7500,height=5000,quality=100,res=400)
  #pdf(paste("polygons_",jj,".pdf",sep=""),width=29.53/2.5,height=19.7/2.5) #width/height in inch 1:2.54 = 75cm/50cm
    y<-18
    x<-27
    col1<-brewer.pal(9,"Reds")[8]  #[7][8]  #ist die background colour
    col2<-"white" #ist die Fill colour
    
    layout(matrix(c(1:(x*y)), nrow=y, ncol=x, byrow = F))
    par(mar=c(0,0,0,0))
    par(bg=col1)     #um weiße Ränder zu vermeiden
    
    for(i in 1:(x*y)){

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
        

        rect(0,0,10,10,border=NA,col=col1)
        polygon(5+cosx*lang,5+sinx*lang,,border=NA,col=col2)

    }
  dev.off()  


  ## Variante 11: BLACK AND WHITE ONLY  (or other colours defined in col1 und col2) #optimiert für 20/30 Druck

  jj=paste("Black_a_White",round(runif(1,min=2.501,max=1000.499)))
  jpeg(paste("polygons_",jj,".jpg",sep=""),width=7500,height=5000,quality=100,res=400)
 # pdf(paste("polygons_",jj,".pdf",sep=""),width=29.53/2.5,height=19.7/2.5) #width/height in inch 1:2.54 = 75cm/50cm
    y<-18
    x<-27
    #col1<-"black"  #[7][8]  #ist die background colour
    col1<-brewer.pal(9,"Blues")[9]
    col2<-"white" #ist die Fill colour
    
    layout(matrix(c(1:(x*y)), nrow=y, ncol=x, byrow = F))
    par(mar=c(0,0,0,0))
    par(bg=col1)     #um weiße Ränder zu vermeiden
    
    for(i in 1:(x*y)){

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
        

        rect(0,0,10,10,border=NA,col=col1)
        polygon(5+cosx*lang,5+sinx*lang,,border=NA,col=col2)

    }
  dev.off()

  
  
  
  
  
  
  
  
  
  
  
  
  
  
#########################TEST##################################################  
  
  pdf("test.pdf",width=29.5,height=19.7)
  y<-2
  x<-2
  layout(matrix(c(1:(x*y)), nrow=y, ncol=x, byrow = F))
  #par(mar=c(0,0,0,0))
  plot(1:10,1:10)
    plot(1:10,1:10)
      plot(1:10,1:10)
        plot(1:10,1:10)
  dev.off()
  