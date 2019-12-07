library(sp)
load("C:\\Dokumente und Einstellungen\\toc\\Eigene Dateien\\R\\Art_Plots\\Country Plots\\TM_WORLD_BORDERS_SIMPL-0.2.RData")
load("C:\\Dokumente und Einstellungen\\toc\\Eigene Dateien\\R\\Art_Plots\\Country Plots\\TM_WORLD_BORDERS_SIMPL-0.2_Mollweide.RData")
load("C:\\Dokumente und Einstellungen\\toc\\Eigene Dateien\\R\\Art_Plots\\Country Plots\\world_countries.rda")

plot(world_countries,col=c("red","green","yellow"))

#Wie einzelne Länder? (Siehe Code für Steiermark-Plots in Arbeit)

##Idea: Reproduce Slight 16 from Tavoni for the 4 different regions for 2003 and 2030, plot the regions with a Mollweide Plot and put it together with the Puzzle from Barbara.





AF<-world_countries
test<-slot(world_countries, "data")$SP_ID  == "AF"
AF<- AF[test,]

plot(AF,col=c("red"))


cht_land<-c(12, 13,16,19,21,39,47,52,53,55,64,71,73,83,90,92,94,101,132,155,176,183,184,186,187,207,217)
for (CHT in cht_land){
  png(paste("country",CHT,".png",sep=""),width=2000,height=2000,res=500)
    if(CHT==64){plot(world_countries[slot(world_countries, "data")$SP_ID[64],],xlim=c(-5,2),ylim=c(35,45),col="red")}else{
    if(CHT==71){plot(world_countries[slot(world_countries, "data")$SP_ID[71],],xlim=c(-1,6),ylim=c(41,52),col="red")} else{
    if(CHT==73){plot(world_countries[slot(world_countries, "data")$SP_ID[73],],xlim=c(-5,2),ylim=c(50,62),col="red")} else{
    if(CHT==155){plot(world_countries[slot(world_countries, "data")$SP_ID[155],],xlim=c(13.5,14),ylim=c(58,72),col="red")} else{
    if(CHT==13){plot(world_countries[slot(world_countries, "data")$SP_ID[13],],xlim=c(110,165),ylim=c(-35,-20),col="red")} else{
    plot(world_countries[slot(world_countries, "data")$SP_ID[CHT],],col="red" )}}}}}
  dev.off()
}


plot(world_countries[slot(world_countries, "data")$SP_ID[13],],xlim=c(110,165),ylim=c(-35,-20),col="red")


###

cht_land_nu<-sample(c(132, 12, 13,16,19,21,39,52,53,55,64,71,73,83,90,92,94,101,155,176,183,186,187,207),size=20)     #132,184,217 ,47=Serbien und Montenegro
names(cht_land_nu)<-world_countries@data[cht_land_nu,"names"]


CHT<-cht_land_nu[1]
CHT<-47
plot(world_countries[slot(world_countries, "data")$SP_ID[CHT],],col="red" )

bb<-bbox(world_countries[slot(world_countries, "data")$SP_ID[CHT],])
bbA<-bbox(world_countries[slot(world_countries, "data")$SP_ID[12],])
world_countries[slot(world_countries, "data")$SP_ID[12],][1]@bbox