
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(maptools) # a package for dealing with shapefiles/spatial data
#library(survey) # a package for analyzing clustered survey data
library(ggplot2) # a for making pretty plots
#library(RColorBrewer) # a package that has some pretty colors for the maps
#library(plyr) # a useful package for data manipulation
library(shiny)
library(reshape)
library(rgdal)



data_sets <- c("UNRA1","UNRA2","UNRA3","UNRA4","UNRA5")

mydata <- read.csv("ww/UNRA1.csv", header = FALSE)
question<-as.character(mydata[1,1])
question1<-as.character(mydata[c(17),c(1)])
u1qns<-c(question,question1)
categqn1<-c(as.character(paste(factor(mydata[c(2),c(5)]))),as.character(paste(factor(mydata[c(2),c(10)]))),as.character(paste(factor(mydata[c(2),c(41)]))),as.character(paste(factor(mydata[c(2),c(43)]))),as.character(paste(factor(mydata[c(2),c(45)]))),as.character(paste(factor(mydata[c(2),c(105)]))),as.character(paste(factor(mydata[c(2),c(117)]))))


################################################################################################################################################################################################
################################UNRA1..QUESTION 1......
region<-mydata[2:15,2:9]

soutthtt<-factor(region[c(2),c(4)])
southern<-as.numeric(paste(factor( region[c(5,7,9,11,13),c(4)])))
var2<-as.character(paste(factor( region[c(5,7,9,11,13),c(1)])))
south<-data.frame(var2,southern)

centtt<-factor(region[c(2),c(5)])
central<-as.numeric(paste(factor( region[c(5,7,9,11,13),c(5)])))
centr<-data.frame(var2,central)

eastt<-factor(region[c(2),c(6)])
eastern<-as.numeric(paste(factor( region[c(5,7,9,11,13),c(6)])))
east<-data.frame(var2,eastern)

northtt<-factor(region[c(2),c(7)])
northern<-as.numeric(paste(factor( region[c(5,7,9,11,13),c(7)])))
north<-data.frame(var2,northern)

westtt<-factor(region[c(2),c(8)])
western<-as.numeric(paste(factor( region[c(5,7,9,11,13),c(8)])))
west<-data.frame(var2,western)




hkmap = readOGR("../UNRASYS/ww/data_2/Uganda_sub_region2006.shp", layer="Uganda_sub_region2006")


a<-as.data.frame(hkmap)
thismap = hkmap
ListName = data.frame(Id=row.names(a), Code=thismap$SUB_REGION, Name=thismap$AREA)
#ListName$Code = gsub('CENTRAL.', '', as.character(ListName$Code))
RegionHK = c("WEST NILE","WESTERN")
RegionKL = c("ELGON","KARAMOJA","EAST CENTRAL","TESO")
RegionHC = c("ACHOLI")
RegionHZ = c("CENTRAL 2","LANGO")
ListName$Region = "Southern"
ListName$Region[ListName$Code %in% RegionHZ] = "Central"
ListName$Region[ListName$Code %in% RegionHK] = "Western"
ListName$Region[ListName$Code %in% RegionKL] = "Eastern" 
ListName$Region[ListName$Code %in% RegionHC] = "Northern"
ListName = ListName[order(ListName$Region),]
ListName$NewID = seq(1,dim(ListName)[1])
ListName[,c("NewID","Region","Code","Name")]
hkmapdf = fortify(hkmap)



value.v.dis<-c(as.numeric(paste(factor(region[c(5),c(4)]))),as.numeric(paste(factor(region[c(5),c(5)]))),as.numeric(paste(factor(region[c(5),c(6)]))),as.numeric(paste(factor(region[c(5),c(7)]))),as.numeric(paste(factor(region[c(5),c(8)]))))
value.v.sat<-c(as.numeric(paste(factor(region[c(13),c(4)]))),as.numeric(paste(factor(region[c(13),c(5)]))),as.numeric(paste(factor(region[c(13),c(6)]))),as.numeric(paste(factor(region[c(13),c(7)]))),as.numeric(paste(factor(region[c(13),c(8)]))))
value.dis<-c(as.numeric(paste(factor(region[c(7),c(4)]))),as.numeric(paste(factor(region[c(7),c(5)]))),as.numeric(paste(factor(region[c(7),c(6)]))),as.numeric(paste(factor(region[c(7),c(7)]))),as.numeric(paste(factor(region[c(7),c(8)]))))
value.nei<-c(as.numeric(paste(factor(region[c(9),c(4)]))),as.numeric(paste(factor(region[c(9),c(5)]))),as.numeric(paste(factor(region[c(9),c(6)]))),as.numeric(paste(factor(region[c(9),c(7)]))),as.numeric(paste(factor(region[c(9),c(8)]))))
value.sat<-c(as.numeric(paste(factor(region[c(11),c(4)]))),as.numeric(paste(factor(region[c(11),c(5)]))),as.numeric(paste(factor(region[c(11),c(6)]))),as.numeric(paste(factor(region[c(11),c(7)]))),as.numeric(paste(factor(region[c(11),c(8)]))))

#regionnames<-c(mydata[c(3),c(5,6,7,8,9)])
Region<-c(as.character(paste(factor(mydata[c(3),c(5)]))),as.character(paste(factor(mydata[c(3),c(6)]))),as.character(paste(factor(mydata[c(3),c(7)]))),as.character(paste(factor(mydata[c(3),c(8)]))),as.character(paste(factor(mydata[c(3),c(9)]))))
#try manual data input
mapdata<-data.frame(Region,value.v.dis,value.v.sat,value.dis,value.nei,value.sat)

thismap2 = ListName
ListName2 = data.frame(Id=row.names(a), Region=thismap2$Region, Name=thismap2$Name)
RegionHK = c("Western")
RegionKL = c("Eastern")
RegionHC = c("Northern")
RegionHZ = c("Central")
RegionHO = c("Southern")

#value v disa
ListName2$value[ListName2$Region %in% RegionHO] =mapdata[mapdata$Region=="Southern",c(2)]
ListName2$value[ListName2$Region %in% RegionHZ] =mapdata[mapdata$Region=="Central",c(2)] 
ListName2$value[ListName2$Region %in% RegionHK] =mapdata[mapdata$Region=="Western",c(2)]
ListName2$value[ListName2$Region %in% RegionKL] =mapdata[mapdata$Region=="Eastern",c(2)] 
ListName2$value[ListName2$Region %in% RegionHC] =mapdata[mapdata$Region=="Northern",c(2)] 
ListName2 = ListName2[order(ListName2$value),]
#value v sat
ListName2$value2[ListName2$Region %in% RegionHO] =mapdata[mapdata$Region=="Southern",c(3)]
ListName2$value2[ListName2$Region %in% RegionHZ] =mapdata[mapdata$Region=="Central",c(3)] 
ListName2$value2[ListName2$Region %in% RegionHK] =mapdata[mapdata$Region=="Western",c(3)]
ListName2$value2[ListName2$Region %in% RegionKL] =mapdata[mapdata$Region=="Eastern",c(3)] 
ListName2$value2[ListName2$Region %in% RegionHC] =mapdata[mapdata$Region=="Northern",c(3)] 
ListName2 = ListName2[order(ListName2$value2),]

#value dis
ListName2$value3[ListName2$Region %in% RegionHO] =mapdata[mapdata$Region=="Southern",c(4)]
ListName2$value3[ListName2$Region %in% RegionHZ] =mapdata[mapdata$Region=="Central",c(4)] 
ListName2$value3[ListName2$Region %in% RegionHK] =mapdata[mapdata$Region=="Western",c(4)]
ListName2$value3[ListName2$Region %in% RegionKL] =mapdata[mapdata$Region=="Eastern",c(4)] 
ListName2$value3[ListName2$Region %in% RegionHC] =mapdata[mapdata$Region=="Northern",c(4)] 
ListName2 = ListName2[order(ListName2$value3),]

#value nei
ListName2$value4[ListName2$Region %in% RegionHO] =mapdata[mapdata$Region=="Southern",c(5)]
ListName2$value4[ListName2$Region %in% RegionHZ] =mapdata[mapdata$Region=="Central",c(5)] 
ListName2$value4[ListName2$Region %in% RegionHK] =mapdata[mapdata$Region=="Western",c(5)]
ListName2$value4[ListName2$Region %in% RegionKL] =mapdata[mapdata$Region=="Eastern",c(5)] 
ListName2$value4[ListName2$Region %in% RegionHC] =mapdata[mapdata$Region=="Northern",c(5)] 
ListName2 = ListName2[order(ListName2$value4),]

#value sat
ListName2$value5[ListName2$Region %in% RegionHO] =mapdata[mapdata$Region=="Southern",c(6)]
ListName2$value5[ListName2$Region %in% RegionHZ] =mapdata[mapdata$Region=="Central",c(6)] 
ListName2$value5[ListName2$Region %in% RegionHK] =mapdata[mapdata$Region=="Western",c(6)]
ListName2$value5[ListName2$Region %in% RegionKL] =mapdata[mapdata$Region=="Eastern",c(6)] 
ListName2$value5[ListName2$Region %in% RegionHC] =mapdata[mapdata$Region=="Northern",c(6)] 
ListName2 = ListName2[order(ListName2$value5),]
ListName2$NewID = seq(1,dim(ListName2)[1])
ListName2[,c("NewID","Region","Name","value","value2","value3","value4","value5")]

hkmapdfs = fortify(hkmap) # ggplot2::fortify converts "sp" object to data.frame
hkmapdfs = merge(hkmapdf,ListName2, by.x="id", by.y="Id")
answerz<-factor(region[c(5),c(1)])
answer1<-factor(region[c(13),c(1)])
answer2<-factor(region[c(7),c(1)])
answer3<-factor(region[c(9),c(1)])
answer4<-factor(region[c(11),c(1)])




################################districts
districts<-mydata[c(2:15),c(2,10:40)]

discs<-c(as.character(paste(factor(districts[c(2),c(2)]))),as.character(paste(factor(districts[c(2),c(3)]))),as.character(paste(factor(districts[c(2),c(4)]))), as.character(paste(factor(districts[c(2),c(5)]))), as.character(paste(factor(districts[c(2),c(6)]))), as.character(paste(factor(districts[c(2),c(7)]))), as.character(paste(factor(districts[c(2),c(8)]))), as.character(paste(factor(districts[c(2),c(9)]))),as.character(paste(factor(districts[c(2),c(10)]))),as.character(paste(factor(districts[c(2),c(11)]))),as.character(paste(factor(districts[c(2),c(12)]))), as.character(paste(factor(districts[c(2),c(13)]))),as.character(paste(factor(districts[c(2),c(14)]))),as.character(paste(factor(districts[c(2),c(15)]))),as.character(paste(factor(districts[c(2),c(16)]))),
         as.character(paste(factor(districts[c(2),c(17)]))),as.character(paste(factor(districts[c(2),c(18)]))),as.character(paste(factor(districts[c(2),c(19)]))),as.character(paste(factor(districts[c(2),c(20)]))),as.character(paste(factor(districts[c(2),c(21)]))),as.character(paste(factor(districts[c(2),c(22)]))),as.character(paste(factor(districts[c(2),c(23)]))),as.character(paste(factor(districts[c(2),c(24)]))),as.character(paste(factor(districts[c(2),c(25)]))),as.character(paste(factor(districts[c(2),c(26)]))),as.character(paste(factor(districts[c(2),c(27)]))),as.character(paste(factor(districts[c(2),c(28)]))),as.character(paste(factor(districts[c(2),c(29)]))),as.character(paste(factor(districts[c(2),c(30)]))),as.character(paste(factor(districts[c(2),c(31)]))),
         as.character(paste(factor(districts[c(2),c(32)]))),as.character(paste(factor(districts[c(2),c(33)]))),as.character(paste(factor(districts[c(2),c(34)]))),as.character(paste(factor(districts[c(2),c(35)]))),as.character(paste(factor(districts[c(2),c(36)]))),as.character(paste(factor(districts[c(2),c(37)]))),as.character(paste(factor(districts[c(2),c(38)]))),as.character(paste(factor(districts[c(2),c(39)]))),as.character(paste(factor(districts[c(2),c(40)]))),as.character(paste(factor(districts[c(2),c(41)]))),as.character(paste(factor(districts[c(2),c(42)]))),as.character(paste(factor(districts[c(2),c(43)]))),as.character(paste(factor(districts[c(2),c(44)]))),as.character(paste(factor(districts[c(2),c(45)]))))


very.distt<-c(as.character(paste(factor(districts[c(5),c(1)]))))
very.dis<-c(as.numeric(paste(factor(districts[c(5),c(2)]))),as.numeric(paste(factor(districts[c(5),c(3)]))),as.numeric(paste(factor(districts[c(5),c(4)]))), as.numeric(paste(factor(districts[c(5),c(5)]))), as.numeric(paste(factor(districts[c(5),c(6)]))), as.numeric(paste(factor(districts[c(5),c(7)]))), as.numeric(paste(factor(districts[c(5),c(8)]))), as.numeric(paste(factor(districts[c(5),c(9)]))),as.numeric(paste(factor(districts[c(5),c(10)]))),as.numeric(paste(factor(districts[c(5),c(11)]))),as.numeric(paste(factor(districts[c(5),c(12)]))), as.numeric(paste(factor(districts[c(5),c(13)]))),as.numeric(paste(factor(districts[c(5),c(14)]))),as.numeric(paste(factor(districts[c(5),c(15)]))),as.numeric(paste(factor(districts[c(5),c(16)]))),
            as.numeric(paste(factor(districts[c(5),c(17)]))),as.numeric(paste(factor(districts[c(5),c(18)]))),as.numeric(paste(factor(districts[c(5),c(19)]))),as.numeric(paste(factor(districts[c(5),c(20)]))),as.numeric(paste(factor(districts[c(5),c(21)]))),as.numeric(paste(factor(districts[c(5),c(22)]))),as.numeric(paste(factor(districts[c(5),c(23)]))),as.numeric(paste(factor(districts[c(5),c(24)]))),as.numeric(paste(factor(districts[c(5),c(25)]))),as.numeric(paste(factor(districts[c(5),c(26)]))),as.numeric(paste(factor(districts[c(5),c(27)]))),as.numeric(paste(factor(districts[c(5),c(28)]))),as.numeric(paste(factor(districts[c(5),c(29)]))),as.numeric(paste(factor(districts[c(5),c(30)]))),as.numeric(paste(factor(districts[c(5),c(31)]))),
            as.numeric(paste(factor(districts[c(5),c(32)]))),as.numeric(paste(factor(districts[c(5),c(33)]))),as.numeric(paste(factor(districts[c(5),c(34)]))),as.numeric(paste(factor(districts[c(5),c(35)]))),as.numeric(paste(factor(districts[c(5),c(36)]))),as.numeric(paste(factor(districts[c(5),c(37)]))),as.numeric(paste(factor(districts[c(5),c(38)]))),as.numeric(paste(factor(districts[c(5),c(39)]))),as.numeric(paste(factor(districts[c(5),c(40)]))),as.numeric(paste(factor(districts[c(5),c(41)]))),as.numeric(paste(factor(districts[c(5),c(42)]))),as.numeric(paste(factor(districts[c(5),c(43)]))),as.numeric(paste(factor(districts[c(5),c(44)]))),as.numeric(paste(factor(districts[c(5),c(45)]))))
df1<-data.frame(discs,very.dis)


distt<-c(as.character(paste(factor(districts[c(7),c(1)]))))
dis<-c(as.numeric(paste(factor(districts[c(7),c(2)]))),as.numeric(paste(factor(districts[c(7),c(3)]))),as.numeric(paste(factor(districts[c(7),c(4)]))), as.numeric(paste(factor(districts[c(7),c(5)]))), as.numeric(paste(factor(districts[c(7),c(6)]))), as.numeric(paste(factor(districts[c(7),c(7)]))), as.numeric(paste(factor(districts[c(7),c(8)]))), as.numeric(paste(factor(districts[c(7),c(9)]))),as.numeric(paste(factor(districts[c(7),c(10)]))),as.numeric(paste(factor(districts[c(7),c(11)]))),as.numeric(paste(factor(districts[c(7),c(12)]))), as.numeric(paste(factor(districts[c(7),c(13)]))),as.numeric(paste(factor(districts[c(7),c(14)]))),as.numeric(paste(factor(districts[c(7),c(15)]))),as.numeric(paste(factor(districts[c(7),c(16)]))),
       as.numeric(paste(factor(districts[c(7),c(17)]))),as.numeric(paste(factor(districts[c(7),c(18)]))),as.numeric(paste(factor(districts[c(7),c(19)]))),as.numeric(paste(factor(districts[c(7),c(20)]))),as.numeric(paste(factor(districts[c(7),c(21)]))),as.numeric(paste(factor(districts[c(7),c(22)]))),as.numeric(paste(factor(districts[c(7),c(23)]))),as.numeric(paste(factor(districts[c(7),c(24)]))),as.numeric(paste(factor(districts[c(7),c(25)]))),as.numeric(paste(factor(districts[c(7),c(26)]))),as.numeric(paste(factor(districts[c(7),c(27)]))),as.numeric(paste(factor(districts[c(7),c(28)]))),as.numeric(paste(factor(districts[c(7),c(29)]))),as.numeric(paste(factor(districts[c(7),c(30)]))),as.numeric(paste(factor(districts[c(7),c(31)]))),
       as.numeric(paste(factor(districts[c(7),c(32)]))),as.numeric(paste(factor(districts[c(7),c(33)]))),as.numeric(paste(factor(districts[c(7),c(34)]))),as.numeric(paste(factor(districts[c(7),c(35)]))),as.numeric(paste(factor(districts[c(7),c(36)]))),as.numeric(paste(factor(districts[c(7),c(37)]))),as.numeric(paste(factor(districts[c(7),c(38)]))),as.numeric(paste(factor(districts[c(7),c(39)]))),as.numeric(paste(factor(districts[c(7),c(40)]))),as.numeric(paste(factor(districts[c(7),c(41)]))),as.numeric(paste(factor(districts[c(7),c(42)]))),as.numeric(paste(factor(districts[c(7),c(43)]))),as.numeric(paste(factor(districts[c(7),c(44)]))),as.numeric(paste(factor(districts[c(7),c(45)]))))
df2<-data.frame(discs,dis)

neiSaNorDistt<-c(as.character(paste(factor(districts[c(9),c(1)]))))
neiSatNorDis <-c(as.numeric(paste(factor(districts[c(9),c(2)]))),as.numeric(paste(factor(districts[c(9),c(3)]))),as.numeric(paste(factor(districts[c(9),c(4)]))), as.numeric(paste(factor(districts[c(9),c(5)]))), as.numeric(paste(factor(districts[c(9),c(6)]))), as.numeric(paste(factor(districts[c(9),c(7)]))), as.numeric(paste(factor(districts[c(9),c(8)]))), as.numeric(paste(factor(districts[c(9),c(9)]))),as.numeric(paste(factor(districts[c(9),c(10)]))),as.numeric(paste(factor(districts[c(9),c(11)]))),as.numeric(paste(factor(districts[c(9),c(12)]))), as.numeric(paste(factor(districts[c(9),c(13)]))),as.numeric(paste(factor(districts[c(9),c(14)]))),as.numeric(paste(factor(districts[c(9),c(15)]))),as.numeric(paste(factor(districts[c(9),c(16)]))),
                 as.numeric(paste(factor(districts[c(9),c(17)]))),as.numeric(paste(factor(districts[c(9),c(18)]))),as.numeric(paste(factor(districts[c(9),c(19)]))),as.numeric(paste(factor(districts[c(9),c(20)]))),as.numeric(paste(factor(districts[c(9),c(21)]))),as.numeric(paste(factor(districts[c(9),c(22)]))),as.numeric(paste(factor(districts[c(9),c(23)]))),as.numeric(paste(factor(districts[c(9),c(24)]))),as.numeric(paste(factor(districts[c(9),c(25)]))),as.numeric(paste(factor(districts[c(9),c(26)]))),as.numeric(paste(factor(districts[c(9),c(27)]))),as.numeric(paste(factor(districts[c(9),c(28)]))),as.numeric(paste(factor(districts[c(9),c(29)]))),as.numeric(paste(factor(districts[c(9),c(30)]))),as.numeric(paste(factor(districts[c(9),c(31)]))),
                 as.numeric(paste(factor(districts[c(9),c(32)]))),as.numeric(paste(factor(districts[c(9),c(33)]))),as.numeric(paste(factor(districts[c(9),c(34)]))),as.numeric(paste(factor(districts[c(9),c(35)]))),as.numeric(paste(factor(districts[c(9),c(36)]))),as.numeric(paste(factor(districts[c(9),c(37)]))),as.numeric(paste(factor(districts[c(9),c(38)]))),as.numeric(paste(factor(districts[c(9),c(39)]))),as.numeric(paste(factor(districts[c(9),c(40)]))),as.numeric(paste(factor(districts[c(9),c(41)]))),as.numeric(paste(factor(districts[c(9),c(42)]))),as.numeric(paste(factor(districts[c(9),c(43)]))),as.numeric(paste(factor(districts[c(9),c(44)]))),as.numeric(paste(factor(districts[c(9),c(45)]))))
df3<-data.frame(discs,neiSatNorDis)

satisfiedtt<-c(as.character(paste(factor(districts[c(11),c(1)]))))
satisfied<-c(as.numeric(paste(factor(districts[c(11),c(2)]))),as.numeric(paste(factor(districts[c(11),c(3)]))),as.numeric(paste(factor(districts[c(11),c(4)]))), as.numeric(paste(factor(districts[c(11),c(5)]))), as.numeric(paste(factor(districts[c(11),c(6)]))), as.numeric(paste(factor(districts[c(11),c(7)]))), as.numeric(paste(factor(districts[c(11),c(8)]))), as.numeric(paste(factor(districts[c(11),c(9)]))),as.numeric(paste(factor(districts[c(11),c(10)]))),as.numeric(paste(factor(districts[c(11),c(11)]))),as.numeric(paste(factor(districts[c(11),c(12)]))), as.numeric(paste(factor(districts[c(11),c(13)]))),as.numeric(paste(factor(districts[c(11),c(14)]))),as.numeric(paste(factor(districts[c(11),c(15)]))),as.numeric(paste(factor(districts[c(11),c(16)]))),
             as.numeric(paste(factor(districts[c(11),c(17)]))),as.numeric(paste(factor(districts[c(11),c(18)]))),as.numeric(paste(factor(districts[c(11),c(19)]))),as.numeric(paste(factor(districts[c(11),c(20)]))),as.numeric(paste(factor(districts[c(11),c(21)]))),as.numeric(paste(factor(districts[c(11),c(22)]))),as.numeric(paste(factor(districts[c(11),c(23)]))),as.numeric(paste(factor(districts[c(11),c(24)]))),as.numeric(paste(factor(districts[c(11),c(25)]))),as.numeric(paste(factor(districts[c(11),c(26)]))),as.numeric(paste(factor(districts[c(11),c(27)]))),as.numeric(paste(factor(districts[c(11),c(28)]))),as.numeric(paste(factor(districts[c(11),c(29)]))),as.numeric(paste(factor(districts[c(11),c(30)]))),as.numeric(paste(factor(districts[c(11),c(31)]))),
             as.numeric(paste(factor(districts[c(11),c(32)]))),as.numeric(paste(factor(districts[c(11),c(33)]))),as.numeric(paste(factor(districts[c(11),c(34)]))),as.numeric(paste(factor(districts[c(11),c(35)]))),as.numeric(paste(factor(districts[c(11),c(36)]))),as.numeric(paste(factor(districts[c(11),c(37)]))),as.numeric(paste(factor(districts[c(11),c(38)]))),as.numeric(paste(factor(districts[c(11),c(39)]))),as.numeric(paste(factor(districts[c(11),c(40)]))),as.numeric(paste(factor(districts[c(11),c(41)]))),as.numeric(paste(factor(districts[c(11),c(42)]))),as.numeric(paste(factor(districts[c(11),c(43)]))),as.numeric(paste(factor(districts[c(11),c(44)]))),as.numeric(paste(factor(districts[c(11),c(45)]))))
df4<-data.frame(discs,satisfied)


verysattt<-c(as.character(paste(factor(districts[c(13),c(1)]))))
verySat<-c(as.numeric(paste(factor(districts[c(13),c(2)]))),as.numeric(paste(factor(districts[c(13),c(3)]))),as.numeric(paste(factor(districts[c(13),c(4)]))), as.numeric(paste(factor(districts[c(13),c(5)]))), as.numeric(paste(factor(districts[c(13),c(6)]))), as.numeric(paste(factor(districts[c(13),c(7)]))), as.numeric(paste(factor(districts[c(13),c(8)]))), as.numeric(paste(factor(districts[c(13),c(9)]))),as.numeric(paste(factor(districts[c(13),c(10)]))),as.numeric(paste(factor(districts[c(13),c(11)]))),as.numeric(paste(factor(districts[c(13),c(12)]))), as.numeric(paste(factor(districts[c(13),c(13)]))),as.numeric(paste(factor(districts[c(13),c(14)]))),as.numeric(paste(factor(districts[c(13),c(15)]))),as.numeric(paste(factor(districts[c(13),c(16)]))),
           as.numeric(paste(factor(districts[c(13),c(17)]))),as.numeric(paste(factor(districts[c(13),c(18)]))),as.numeric(paste(factor(districts[c(13),c(19)]))),as.numeric(paste(factor(districts[c(13),c(20)]))),as.numeric(paste(factor(districts[c(13),c(21)]))),as.numeric(paste(factor(districts[c(13),c(22)]))),as.numeric(paste(factor(districts[c(13),c(23)]))),as.numeric(paste(factor(districts[c(13),c(24)]))),as.numeric(paste(factor(districts[c(13),c(25)]))),as.numeric(paste(factor(districts[c(13),c(26)]))),as.numeric(paste(factor(districts[c(13),c(27)]))),as.numeric(paste(factor(districts[c(13),c(28)]))),as.numeric(paste(factor(districts[c(13),c(29)]))),as.numeric(paste(factor(districts[c(13),c(30)]))),as.numeric(paste(factor(districts[c(13),c(31)]))),
           as.numeric(paste(factor(districts[c(13),c(32)]))),as.numeric(paste(factor(districts[c(13),c(33)]))),as.numeric(paste(factor(districts[c(13),c(34)]))),as.numeric(paste(factor(districts[c(13),c(35)]))),as.numeric(paste(factor(districts[c(13),c(36)]))),as.numeric(paste(factor(districts[c(13),c(37)]))),as.numeric(paste(factor(districts[c(13),c(38)]))),as.numeric(paste(factor(districts[c(13),c(39)]))),as.numeric(paste(factor(districts[c(13),c(40)]))),as.numeric(paste(factor(districts[c(13),c(41)]))),as.numeric(paste(factor(districts[c(13),c(42)]))),as.numeric(paste(factor(districts[c(13),c(43)]))),as.numeric(paste(factor(districts[c(13),c(44)]))),as.numeric(paste(factor(districts[c(13),c(45)]))))
df5<-data.frame(discs,verySat)


################################################################
#############ROAD types###################################################
Roadtype <- mydata[c(2:15),c(2,41:42)]

roadtypes<-c(as.character(paste(factor(Roadtype[c(2),c(2)]))),as.character(paste(factor(Roadtype[c(2),c(3)]))))


very.disttroads<-c(as.character(paste(factor(Roadtype [c(5),c(1)]))))
very.disroads<-c(as.numeric(paste(factor(Roadtype [c(5),c(2)]))),as.numeric(paste(factor(Roadtype [c(5),c(3)]))))

dfroads<-data.frame(roadtypes,very.disroads)

disttroads<-c(as.character(paste(factor(Roadtype [c(7),c(1)]))))
disroads<-c(as.numeric(paste(factor(Roadtype [c(7),c(2)]))),as.numeric(paste(factor(Roadtype [c(7),c(3)]))))
dfroads2<-data.frame(roadtypes,disroads)

neiSaNorDistt.roads<-c(as.character(paste(factor(Roadtype [c(9),c(1)]))))
neiSatNorDis.roads <-c(as.numeric(paste(factor(Roadtype [c(9),c(2)]))),as.numeric(paste(factor(Roadtype [c(9),c(3)]))))
dfroads3<-data.frame(roadtypes,neiSatNorDis.roads)

satisfiedttroads<-c(as.character(paste(factor(Roadtype [c(11),c(1)]))))
satisfiedroads<-c(as.numeric(paste(factor(Roadtype [c(11),c(2)]))),as.numeric(paste(factor(Roadtype [c(11),c(3)]))))
dfroads4<-data.frame(roadtypes,satisfiedroads)

verysatttroads<-c(as.character(paste(factor(Roadtype [c(13),c(1)]))))
verySatroads<-c(as.numeric(paste(factor(Roadtype [c(13),c(2)]))),as.numeric(paste(factor(Roadtype [c(13),c(3)]))))
dfroads5<-data.frame(roadtypes,verySatroads)          
################################################################
#############Respondent gender###################################################
Respongender <- mydata[c(2:15),c(2,43:44)]

gender<-c(as.character(paste(factor(Respongender[c(2),c(2)]))),as.character(paste(factor(Respongender[c(2),c(3)]))))


very.disttgender<-c(as.character(paste(factor(Respongender [c(5),c(1)]))))
very.disgender<-c(as.numeric(paste(factor(Respongender [c(5),c(2)]))),as.numeric(paste(factor(Respongender[c(5),c(3)]))))

dfrespon<-data.frame(gender,very.disgender)

disttgender<-c(as.character(paste(factor(Respongender [c(7),c(1)]))))
disgender<-c(as.numeric(paste(factor(Respongender [c(7),c(2)]))),as.numeric(paste(factor(Respongender [c(7),c(3)]))))
dfrespon2<-data.frame(gender,disgender)

neiSaNorDistt.gender<-c(as.character(paste(factor(Respongender [c(9),c(1)]))))
neiSatNorDis.gender <-c(as.numeric(paste(factor(Respongender [c(9),c(2)]))),as.numeric(paste(factor(Respongender [c(9),c(3)]))))
dfrespon3<-data.frame(gender,neiSatNorDis.gender)

satisfiedttGender<-c(as.character(paste(factor(Respongender [c(11),c(1)]))))
satisfiedgender<-c(as.numeric(paste(factor(Respongender [c(11),c(2)]))),as.numeric(paste(factor(Respongender [c(11),c(3)]))))
dfrespon4<-data.frame(gender,satisfiedgender)

verysatttgender<-c(as.character(paste(factor(Respongender [c(13),c(1)]))))
verySatGender<-c(as.numeric(paste(factor(Respongender [c(13),c(2)]))),as.numeric(paste(factor(Respongender [c(13),c(3)]))))
dfrespon5<-data.frame(gender,verySatGender)    

################################################################
###########      Road name    ###################################################
Roadname<-mydata[c(2:15),c(2,45:104)]

rdnames<-c(as.character(paste(factor(Roadname[c(2),c(2)]))),as.character(paste(factor(Roadname[c(2),c(3)]))),as.character(paste(factor(Roadname[c(2),c(4)]))), as.character(paste(factor(Roadname[c(2),c(5)]))), as.character(paste(factor(Roadname[c(2),c(6)]))), as.character(paste(factor(Roadname[c(2),c(7)]))), as.character(paste(factor(Roadname[c(2),c(8)]))), as.character(paste(factor(Roadname[c(2),c(9)]))),as.character(paste(factor(Roadname[c(2),c(10)]))),as.character(paste(factor(Roadname[c(2),c(11)]))),as.character(paste(factor(Roadname[c(2),c(12)]))), as.character(paste(factor(Roadname[c(2),c(13)]))),as.character(paste(factor(Roadname[c(2),c(14)]))),as.character(paste(factor(Roadname[c(2),c(15)]))),as.character(paste(factor(Roadname[c(2),c(16)]))),
           as.character(paste(factor(Roadname[c(2),c(17)]))),as.character(paste(factor(Roadname[c(2),c(18)]))),as.character(paste(factor(Roadname[c(2),c(19)]))),as.character(paste(factor(Roadname[c(2),c(20)]))),as.character(paste(factor(Roadname[c(2),c(21)]))),as.character(paste(factor(Roadname[c(2),c(22)]))),as.character(paste(factor(Roadname[c(2),c(23)]))),as.character(paste(factor(Roadname[c(2),c(24)]))),as.character(paste(factor(Roadname[c(2),c(25)]))),as.character(paste(factor(Roadname[c(2),c(26)]))),as.character(paste(factor(Roadname[c(2),c(27)]))),as.character(paste(factor(Roadname[c(2),c(28)]))),as.character(paste(factor(Roadname[c(2),c(29)]))),as.character(paste(factor(Roadname[c(2),c(30)]))),as.character(paste(factor(Roadname[c(2),c(31)]))),
           as.character(paste(factor(Roadname[c(2),c(32)]))),as.character(paste(factor(Roadname[c(2),c(33)]))),as.character(paste(factor(Roadname[c(2),c(34)]))),as.character(paste(factor(Roadname[c(2),c(35)]))),as.character(paste(factor(Roadname[c(2),c(36)]))),as.character(paste(factor(Roadname[c(2),c(37)]))),as.character(paste(factor(Roadname[c(2),c(38)]))),as.character(paste(factor(Roadname[c(2),c(39)]))),as.character(paste(factor(Roadname[c(2),c(40)]))),as.character(paste(factor(Roadname[c(2),c(41)]))),as.character(paste(factor(Roadname[c(2),c(42)]))),as.character(paste(factor(Roadname[c(2),c(43)]))),as.character(paste(factor(Roadname[c(2),c(44)]))),as.character(paste(factor(Roadname[c(2),c(45)])))
           ,as.character(paste(factor(Roadname[c(2),c(46)]))),as.character(paste(factor(Roadname[c(2),c(47)]))),as.character(paste(factor(Roadname[c(2),c(48)]))), as.character(paste(factor(Roadname[c(2),c(49)]))), as.character(paste(factor(Roadname[c(2),c(50)]))), as.character(paste(factor(Roadname[c(2),c(51)]))), as.character(paste(factor(Roadname[c(2),c(52)]))), as.character(paste(factor(Roadname[c(2),c(53)]))),as.character(paste(factor(Roadname[c(2),c(54)]))),as.character(paste(factor(Roadname[c(2),c(55)]))),as.character(paste(factor(Roadname[c(2),c(56)]))), as.character(paste(factor(Roadname[c(2),c(57)]))),
           as.character(paste(factor(Roadname[c(2),c(58)]))),as.character(paste(factor(Roadname[c(2),c(59)]))),as.character(paste(factor(Roadname[c(2),c(60)]))), as.character(paste(factor(Roadname[c(2),c(61)]))))


very.disttrdname<-c(as.character(paste(factor(Roadname[c(5),c(1)]))))
very.disname<-c(as.numeric(paste(factor(Roadname[c(5),c(2)]))),as.numeric(paste(factor(Roadname[c(5),c(3)]))),as.numeric(paste(factor(Roadname[c(5),c(4)]))), as.numeric(paste(factor(Roadname[c(5),c(5)]))), as.numeric(paste(factor(Roadname[c(5),c(6)]))), as.numeric(paste(factor(Roadname[c(5),c(7)]))), as.numeric(paste(factor(Roadname[c(5),c(8)]))), as.numeric(paste(factor(Roadname[c(5),c(9)]))),as.numeric(paste(factor(Roadname[c(5),c(10)]))),as.numeric(paste(factor(Roadname[c(5),c(11)]))),as.numeric(paste(factor(Roadname[c(5),c(12)]))), as.numeric(paste(factor(Roadname[c(5),c(13)]))),as.numeric(paste(factor(Roadname[c(5),c(14)]))),as.numeric(paste(factor(Roadname[c(5),c(15)]))),as.numeric(paste(factor(Roadname[c(5),c(16)]))),
                as.numeric(paste(factor(Roadname[c(5),c(17)]))),as.numeric(paste(factor(Roadname[c(5),c(18)]))),as.numeric(paste(factor(Roadname[c(5),c(19)]))),as.numeric(paste(factor(Roadname[c(5),c(20)]))),as.numeric(paste(factor(Roadname[c(5),c(21)]))),as.numeric(paste(factor(Roadname[c(5),c(22)]))),as.numeric(paste(factor(Roadname[c(5),c(23)]))),as.numeric(paste(factor(Roadname[c(5),c(24)]))),as.numeric(paste(factor(Roadname[c(5),c(25)]))),as.numeric(paste(factor(Roadname[c(5),c(26)]))),as.numeric(paste(factor(Roadname[c(5),c(27)]))),as.numeric(paste(factor(Roadname[c(5),c(28)]))),as.numeric(paste(factor(Roadname[c(5),c(29)]))),as.numeric(paste(factor(Roadname[c(5),c(30)]))),as.numeric(paste(factor(Roadname[c(5),c(31)]))),
                as.numeric(paste(factor(Roadname[c(5),c(32)]))),as.numeric(paste(factor(Roadname[c(5),c(33)]))),as.numeric(paste(factor(Roadname[c(5),c(34)]))),as.numeric(paste(factor(Roadname[c(5),c(35)]))),as.numeric(paste(factor(Roadname[c(5),c(36)]))),as.numeric(paste(factor(Roadname[c(5),c(37)]))),as.numeric(paste(factor(Roadname[c(5),c(38)]))),as.numeric(paste(factor(Roadname[c(5),c(39)]))),as.numeric(paste(factor(Roadname[c(5),c(40)]))),as.numeric(paste(factor(Roadname[c(5),c(41)]))),as.numeric(paste(factor(Roadname[c(5),c(42)]))),as.numeric(paste(factor(Roadname[c(5),c(43)]))),as.numeric(paste(factor(Roadname[c(5),c(44)]))),as.numeric(paste(factor(Roadname[c(5),c(45)])))
                ,as.numeric(paste(factor(Roadname[c(5),c(46)]))),as.numeric(paste(factor(Roadname[c(5),c(47)]))),as.numeric(paste(factor(Roadname[c(5),c(48)]))), as.numeric(paste(factor(Roadname[c(5),c(49)]))), as.numeric(paste(factor(Roadname[c(5),c(50)]))), as.numeric(paste(factor(Roadname[c(5),c(51)]))), as.numeric(paste(factor(Roadname[c(5),c(52)]))), as.numeric(paste(factor(Roadname[c(5),c(53)]))),as.numeric(paste(factor(Roadname[c(5),c(54)]))),as.numeric(paste(factor(Roadname[c(5),c(55)]))),as.numeric(paste(factor(Roadname[c(5),c(56)]))), as.numeric(paste(factor(Roadname[c(5),c(57)]))),
                as.numeric(paste(factor(Roadname[c(5),c(58)]))),as.numeric(paste(factor(Roadname[c(5),c(59)]))),as.numeric(paste(factor(Roadname[c(5),c(60)]))), as.numeric(paste(factor(Roadname[c(5),c(61)]))))

dfnames<-data.frame(rdnames,very.disname)          


distt.name<-c(as.character(paste(factor(Roadname[c(7),c(1)]))))
dis.name<-c(as.numeric(paste(factor(Roadname[c(7),c(2)]))),as.numeric(paste(factor(Roadname[c(7),c(3)]))),as.numeric(paste(factor(Roadname[c(7),c(4)]))), as.numeric(paste(factor(Roadname[c(7),c(5)]))), as.numeric(paste(factor(Roadname[c(7),c(6)]))), as.numeric(paste(factor(Roadname[c(7),c(7)]))), as.numeric(paste(factor(Roadname[c(7),c(8)]))), as.numeric(paste(factor(Roadname[c(7),c(9)]))),as.numeric(paste(factor(Roadname[c(7),c(10)]))),as.numeric(paste(factor(Roadname[c(7),c(11)]))),as.numeric(paste(factor(Roadname[c(7),c(12)]))), as.numeric(paste(factor(Roadname[c(7),c(13)]))),as.numeric(paste(factor(Roadname[c(7),c(14)]))),as.numeric(paste(factor(Roadname[c(7),c(15)]))),as.numeric(paste(factor(Roadname[c(7),c(16)]))),
            as.numeric(paste(factor(Roadname[c(7),c(17)]))),as.numeric(paste(factor(Roadname[c(7),c(18)]))),as.numeric(paste(factor(Roadname[c(7),c(19)]))),as.numeric(paste(factor(Roadname[c(7),c(20)]))),as.numeric(paste(factor(Roadname[c(7),c(21)]))),as.numeric(paste(factor(Roadname[c(7),c(22)]))),as.numeric(paste(factor(Roadname[c(7),c(23)]))),as.numeric(paste(factor(Roadname[c(7),c(24)]))),as.numeric(paste(factor(Roadname[c(7),c(25)]))),as.numeric(paste(factor(Roadname[c(7),c(26)]))),as.numeric(paste(factor(Roadname[c(7),c(27)]))),as.numeric(paste(factor(Roadname[c(7),c(28)]))),as.numeric(paste(factor(Roadname[c(7),c(29)]))),as.numeric(paste(factor(Roadname[c(7),c(30)]))),as.numeric(paste(factor(Roadname[c(7),c(31)]))),
            as.numeric(paste(factor(Roadname[c(7),c(32)]))),as.numeric(paste(factor(Roadname[c(7),c(33)]))),as.numeric(paste(factor(Roadname[c(7),c(34)]))),as.numeric(paste(factor(Roadname[c(7),c(35)]))),as.numeric(paste(factor(Roadname[c(7),c(36)]))),as.numeric(paste(factor(Roadname[c(7),c(37)]))),as.numeric(paste(factor(Roadname[c(7),c(38)]))),as.numeric(paste(factor(Roadname[c(7),c(39)]))),as.numeric(paste(factor(Roadname[c(7),c(40)]))),as.numeric(paste(factor(Roadname[c(7),c(41)]))),as.numeric(paste(factor(Roadname[c(7),c(42)]))),as.numeric(paste(factor(Roadname[c(7),c(43)]))),as.numeric(paste(factor(Roadname[c(7),c(44)]))),as.numeric(paste(factor(Roadname[c(7),c(45)])))
            ,as.numeric(paste(factor(Roadname[c(7),c(46)]))),as.numeric(paste(factor(Roadname[c(7),c(47)]))),as.numeric(paste(factor(Roadname[c(7),c(48)]))), as.numeric(paste(factor(Roadname[c(7),c(49)]))), as.numeric(paste(factor(Roadname[c(7),c(50)]))), as.numeric(paste(factor(Roadname[c(7),c(51)]))), as.numeric(paste(factor(Roadname[c(7),c(52)]))), as.numeric(paste(factor(Roadname[c(7),c(53)]))),as.numeric(paste(factor(Roadname[c(7),c(54)]))),as.numeric(paste(factor(Roadname[c(7),c(55)]))),as.numeric(paste(factor(Roadname[c(7),c(56)]))), as.numeric(paste(factor(Roadname[c(7),c(57)]))),
            as.numeric(paste(factor(Roadname[c(7),c(58)]))),as.numeric(paste(factor(Roadname[c(7),c(59)]))),as.numeric(paste(factor(Roadname[c(7),c(60)]))), as.numeric(paste(factor(Roadname[c(7),c(61)]))))

dfnames2<-data.frame(rdnames,dis.name)

neiSaNorDistt.name<-c(as.character(paste(factor(districts[c(9),c(1)]))))
neiSatNorDis.name<-c(as.numeric(paste(factor(Roadname[c(9),c(2)]))),as.numeric(paste(factor(Roadname[c(9),c(3)]))),as.numeric(paste(factor(Roadname[c(9),c(4)]))), as.numeric(paste(factor(Roadname[c(9),c(5)]))), as.numeric(paste(factor(Roadname[c(9),c(6)]))), as.numeric(paste(factor(Roadname[c(9),c(7)]))), as.numeric(paste(factor(Roadname[c(9),c(8)]))), as.numeric(paste(factor(Roadname[c(9),c(9)]))),as.numeric(paste(factor(Roadname[c(9),c(10)]))),as.numeric(paste(factor(Roadname[c(9),c(11)]))),as.numeric(paste(factor(Roadname[c(9),c(12)]))), as.numeric(paste(factor(Roadname[c(9),c(13)]))),as.numeric(paste(factor(Roadname[c(9),c(14)]))),as.numeric(paste(factor(Roadname[c(9),c(15)]))),as.numeric(paste(factor(Roadname[c(9),c(16)]))),
                     as.numeric(paste(factor(Roadname[c(9),c(17)]))),as.numeric(paste(factor(Roadname[c(9),c(18)]))),as.numeric(paste(factor(Roadname[c(9),c(19)]))),as.numeric(paste(factor(Roadname[c(9),c(20)]))),as.numeric(paste(factor(Roadname[c(9),c(21)]))),as.numeric(paste(factor(Roadname[c(9),c(22)]))),as.numeric(paste(factor(Roadname[c(9),c(23)]))),as.numeric(paste(factor(Roadname[c(9),c(24)]))),as.numeric(paste(factor(Roadname[c(9),c(25)]))),as.numeric(paste(factor(Roadname[c(9),c(26)]))),as.numeric(paste(factor(Roadname[c(9),c(27)]))),as.numeric(paste(factor(Roadname[c(9),c(28)]))),as.numeric(paste(factor(Roadname[c(9),c(29)]))),as.numeric(paste(factor(Roadname[c(9),c(30)]))),as.numeric(paste(factor(Roadname[c(9),c(31)]))),
                     as.numeric(paste(factor(Roadname[c(9),c(32)]))),as.numeric(paste(factor(Roadname[c(9),c(33)]))),as.numeric(paste(factor(Roadname[c(9),c(34)]))),as.numeric(paste(factor(Roadname[c(9),c(35)]))),as.numeric(paste(factor(Roadname[c(9),c(36)]))),as.numeric(paste(factor(Roadname[c(9),c(37)]))),as.numeric(paste(factor(Roadname[c(9),c(38)]))),as.numeric(paste(factor(Roadname[c(9),c(39)]))),as.numeric(paste(factor(Roadname[c(9),c(40)]))),as.numeric(paste(factor(Roadname[c(9),c(41)]))),as.numeric(paste(factor(Roadname[c(9),c(42)]))),as.numeric(paste(factor(Roadname[c(9),c(43)]))),as.numeric(paste(factor(Roadname[c(9),c(44)]))),as.numeric(paste(factor(Roadname[c(9),c(45)])))
                     ,as.numeric(paste(factor(Roadname[c(9),c(46)]))),as.numeric(paste(factor(Roadname[c(9),c(47)]))),as.numeric(paste(factor(Roadname[c(9),c(48)]))), as.numeric(paste(factor(Roadname[c(9),c(49)]))), as.numeric(paste(factor(Roadname[c(9),c(50)]))), as.numeric(paste(factor(Roadname[c(9),c(51)]))), as.numeric(paste(factor(Roadname[c(9),c(52)]))), as.numeric(paste(factor(Roadname[c(9),c(53)]))),as.numeric(paste(factor(Roadname[c(9),c(54)]))),as.numeric(paste(factor(Roadname[c(9),c(55)]))),as.numeric(paste(factor(Roadname[c(9),c(56)]))), as.numeric(paste(factor(Roadname[c(9),c(57)]))),
                     as.numeric(paste(factor(Roadname[c(9),c(58)]))),as.numeric(paste(factor(Roadname[c(9),c(59)]))),as.numeric(paste(factor(Roadname[c(9),c(60)]))), as.numeric(paste(factor(Roadname[c(9),c(61)]))))


dfnames3<-data.frame(rdnames,neiSatNorDis.name)

satisfiedtt.names<-c(as.character(paste(factor(districts[c(11),c(1)]))))

satisfied.names<-c(as.numeric(paste(factor(Roadname[c(11),c(2)]))),as.numeric(paste(factor(Roadname[c(11),c(3)]))),as.numeric(paste(factor(Roadname[c(11),c(4)]))), as.numeric(paste(factor(Roadname[c(11),c(5)]))), as.numeric(paste(factor(Roadname[c(11),c(6)]))), as.numeric(paste(factor(Roadname[c(11),c(7)]))), as.numeric(paste(factor(Roadname[c(11),c(8)]))), as.numeric(paste(factor(Roadname[c(11),c(9)]))),as.numeric(paste(factor(Roadname[c(11),c(10)]))),as.numeric(paste(factor(Roadname[c(11),c(11)]))),as.numeric(paste(factor(Roadname[c(11),c(12)]))), as.numeric(paste(factor(Roadname[c(11),c(13)]))),as.numeric(paste(factor(Roadname[c(11),c(14)]))),as.numeric(paste(factor(Roadname[c(11),c(15)]))),as.numeric(paste(factor(Roadname[c(11),c(16)]))),
                   as.numeric(paste(factor(Roadname[c(11),c(17)]))),as.numeric(paste(factor(Roadname[c(11),c(18)]))),as.numeric(paste(factor(Roadname[c(11),c(19)]))),as.numeric(paste(factor(Roadname[c(11),c(20)]))),as.numeric(paste(factor(Roadname[c(11),c(21)]))),as.numeric(paste(factor(Roadname[c(11),c(22)]))),as.numeric(paste(factor(Roadname[c(11),c(23)]))),as.numeric(paste(factor(Roadname[c(11),c(24)]))),as.numeric(paste(factor(Roadname[c(11),c(25)]))),as.numeric(paste(factor(Roadname[c(11),c(26)]))),as.numeric(paste(factor(Roadname[c(11),c(27)]))),as.numeric(paste(factor(Roadname[c(11),c(28)]))),as.numeric(paste(factor(Roadname[c(11),c(29)]))),as.numeric(paste(factor(Roadname[c(11),c(30)]))),as.numeric(paste(factor(Roadname[c(11),c(31)]))),
                   as.numeric(paste(factor(Roadname[c(11),c(32)]))),as.numeric(paste(factor(Roadname[c(11),c(33)]))),as.numeric(paste(factor(Roadname[c(11),c(34)]))),as.numeric(paste(factor(Roadname[c(11),c(35)]))),as.numeric(paste(factor(Roadname[c(11),c(36)]))),as.numeric(paste(factor(Roadname[c(11),c(37)]))),as.numeric(paste(factor(Roadname[c(11),c(38)]))),as.numeric(paste(factor(Roadname[c(11),c(39)]))),as.numeric(paste(factor(Roadname[c(11),c(40)]))),as.numeric(paste(factor(Roadname[c(11),c(41)]))),as.numeric(paste(factor(Roadname[c(11),c(42)]))),as.numeric(paste(factor(Roadname[c(11),c(43)]))),as.numeric(paste(factor(Roadname[c(11),c(44)]))),as.numeric(paste(factor(Roadname[c(11),c(45)])))
                   ,as.numeric(paste(factor(Roadname[c(11),c(46)]))),as.numeric(paste(factor(Roadname[c(11),c(47)]))),as.numeric(paste(factor(Roadname[c(11),c(48)]))), as.numeric(paste(factor(Roadname[c(11),c(49)]))), as.numeric(paste(factor(Roadname[c(11),c(50)]))), as.numeric(paste(factor(Roadname[c(11),c(51)]))), as.numeric(paste(factor(Roadname[c(11),c(52)]))), as.numeric(paste(factor(Roadname[c(11),c(53)]))),as.numeric(paste(factor(Roadname[c(11),c(54)]))),as.numeric(paste(factor(Roadname[c(11),c(55)]))),as.numeric(paste(factor(Roadname[c(11),c(56)]))), as.numeric(paste(factor(Roadname[c(11),c(57)]))),
                   as.numeric(paste(factor(Roadname[c(11),c(58)]))),as.numeric(paste(factor(Roadname[c(11),c(59)]))),as.numeric(paste(factor(Roadname[c(11),c(60)]))), as.numeric(paste(factor(Roadname[c(11),c(61)]))))


dfnames4<-data.frame(rdnames,satisfied.names)


verysatt.names<-c(as.character(paste(factor(districts[c(13),c(1)]))))

verySat.names<-c(as.numeric(paste(factor(Roadname[c(13),c(2)]))),as.numeric(paste(factor(Roadname[c(13),c(3)]))),as.numeric(paste(factor(Roadname[c(13),c(4)]))), as.numeric(paste(factor(Roadname[c(13),c(5)]))), as.numeric(paste(factor(Roadname[c(13),c(6)]))), as.numeric(paste(factor(Roadname[c(13),c(7)]))), as.numeric(paste(factor(Roadname[c(13),c(8)]))), as.numeric(paste(factor(Roadname[c(13),c(9)]))),as.numeric(paste(factor(Roadname[c(13),c(10)]))),as.numeric(paste(factor(Roadname[c(13),c(11)]))),as.numeric(paste(factor(Roadname[c(13),c(12)]))), as.numeric(paste(factor(Roadname[c(13),c(13)]))),as.numeric(paste(factor(Roadname[c(13),c(14)]))),as.numeric(paste(factor(Roadname[c(13),c(15)]))),as.numeric(paste(factor(Roadname[c(13),c(16)]))),
                 as.numeric(paste(factor(Roadname[c(13),c(17)]))),as.numeric(paste(factor(Roadname[c(13),c(18)]))),as.numeric(paste(factor(Roadname[c(13),c(19)]))),as.numeric(paste(factor(Roadname[c(13),c(20)]))),as.numeric(paste(factor(Roadname[c(13),c(21)]))),as.numeric(paste(factor(Roadname[c(13),c(22)]))),as.numeric(paste(factor(Roadname[c(13),c(23)]))),as.numeric(paste(factor(Roadname[c(13),c(24)]))),as.numeric(paste(factor(Roadname[c(13),c(25)]))),as.numeric(paste(factor(Roadname[c(13),c(26)]))),as.numeric(paste(factor(Roadname[c(13),c(27)]))),as.numeric(paste(factor(Roadname[c(13),c(28)]))),as.numeric(paste(factor(Roadname[c(13),c(29)]))),as.numeric(paste(factor(Roadname[c(13),c(30)]))),as.numeric(paste(factor(Roadname[c(13),c(31)]))),
                 as.numeric(paste(factor(Roadname[c(13),c(32)]))),as.numeric(paste(factor(Roadname[c(13),c(33)]))),as.numeric(paste(factor(Roadname[c(13),c(34)]))),as.numeric(paste(factor(Roadname[c(13),c(35)]))),as.numeric(paste(factor(Roadname[c(13),c(36)]))),as.numeric(paste(factor(Roadname[c(13),c(37)]))),as.numeric(paste(factor(Roadname[c(13),c(38)]))),as.numeric(paste(factor(Roadname[c(13),c(39)]))),as.numeric(paste(factor(Roadname[c(13),c(40)]))),as.numeric(paste(factor(Roadname[c(13),c(41)]))),as.numeric(paste(factor(Roadname[c(13),c(42)]))),as.numeric(paste(factor(Roadname[c(13),c(43)]))),as.numeric(paste(factor(Roadname[c(13),c(44)]))),as.numeric(paste(factor(Roadname[c(13),c(45)])))
                 ,as.numeric(paste(factor(Roadname[c(13),c(46)]))),as.numeric(paste(factor(Roadname[c(13),c(47)]))),as.numeric(paste(factor(Roadname[c(13),c(48)]))), as.numeric(paste(factor(Roadname[c(13),c(49)]))), as.numeric(paste(factor(Roadname[c(13),c(50)]))), as.numeric(paste(factor(Roadname[c(13),c(51)]))), as.numeric(paste(factor(Roadname[c(13),c(52)]))), as.numeric(paste(factor(Roadname[c(13),c(53)]))),as.numeric(paste(factor(Roadname[c(13),c(54)]))),as.numeric(paste(factor(Roadname[c(13),c(55)]))),as.numeric(paste(factor(Roadname[c(13),c(56)]))), as.numeric(paste(factor(Roadname[c(13),c(57)]))),
                 as.numeric(paste(factor(Roadname[c(13),c(58)]))),as.numeric(paste(factor(Roadname[c(13),c(59)]))),as.numeric(paste(factor(Roadname[c(13),c(60)]))), as.numeric(paste(factor(Roadname[c(13),c(61)]))))

dfnames5<-data.frame(rdnames,verySat.names)

################################################################
###########      Respondent Type    ###################################################


Respontype<-mydata[c(2:15),c(2,105:116)]

restype<-c(as.character(paste(factor(Respontype[c(2),c(2)]))),as.character(paste(factor(Respontype[c(2),c(3)]))),as.character(paste(factor(Respontype[c(2),c(4)]))), as.character(paste(factor(Respontype[c(2),c(5)]))), as.character(paste(factor(Respontype[c(2),c(6)]))), as.character(paste(factor(Respontype[c(2),c(7)]))), as.character(paste(factor(Respontype[c(2),c(8)]))), as.character(paste(factor(Respontype[c(2),c(9)]))),as.character(paste(factor(Respontype[c(2),c(10)]))),as.character(paste(factor(Respontype[c(2),c(11)]))),as.character(paste(factor(Respontype[c(2),c(12)]))), as.character(paste(factor(Respontype[c(2),c(13)]))))

very.disttRrestype<-c(as.character(paste(factor(Respontype[c(5),c(1)]))))
very.disRestype<-c(as.numeric(paste(factor(Respontype[c(5),c(2)]))),as.numeric(paste(factor(Respontype[c(5),c(3)]))),as.numeric(paste(factor(Respontype[c(5),c(4)]))), as.numeric(paste(factor(Respontype[c(5),c(5)]))), as.numeric(paste(factor(Respontype[c(5),c(6)]))), as.numeric(paste(factor(Respontype[c(5),c(7)]))), as.numeric(paste(factor(Respontype[c(5),c(8)]))), as.numeric(paste(factor(Respontype[c(5),c(9)]))),as.numeric(paste(factor(Respontype[c(5),c(10)]))),as.numeric(paste(factor(Respontype[c(5),c(11)]))),as.numeric(paste(factor(Respontype[c(5),c(12)]))), as.numeric(paste(factor(Respontype[c(5),c(13)]))))
dfRestypes<-data.frame(restype,very.disRestype)          

distt.restype<-c(as.character(paste(factor(Respontype[c(7),c(1)]))))
disRestype<-c(as.numeric(paste(factor(Respontype[c(7),c(2)]))),as.numeric(paste(factor(Respontype[c(7),c(3)]))),as.numeric(paste(factor(Respontype[c(7),c(4)]))), as.numeric(paste(factor(Respontype[c(7),c(5)]))), as.numeric(paste(factor(Respontype[c(7),c(6)]))), as.numeric(paste(factor(Respontype[c(7),c(7)]))), as.numeric(paste(factor(Respontype[c(7),c(8)]))), as.numeric(paste(factor(Respontype[c(7),c(9)]))),as.numeric(paste(factor(Respontype[c(7),c(10)]))),as.numeric(paste(factor(Respontype[c(7),c(11)]))),as.numeric(paste(factor(Respontype[c(7),c(12)]))), as.numeric(paste(factor(Respontype[c(7),c(13)]))))
dfRestypes2<-data.frame(restype,disRestype)


neiSaNorDisttRestype<-c(as.character(paste(factor(Respontype[c(9),c(1)]))))

neiSatNorDisRestype<-c(as.numeric(paste(factor(Respontype[c(9),c(2)]))),as.numeric(paste(factor(Respontype[c(9),c(3)]))),as.numeric(paste(factor(Respontype[c(9),c(4)]))), as.numeric(paste(factor(Respontype[c(9),c(5)]))), as.numeric(paste(factor(Respontype[c(9),c(6)]))), as.numeric(paste(factor(Respontype[c(9),c(7)]))), as.numeric(paste(factor(Respontype[c(9),c(8)]))), as.numeric(paste(factor(Respontype[c(9),c(9)]))),as.numeric(paste(factor(Respontype[c(9),c(10)]))),as.numeric(paste(factor(Respontype[c(9),c(11)]))),as.numeric(paste(factor(Respontype[c(9),c(12)]))), as.numeric(paste(factor(Respontype[c(9),c(13)]))))
dfRestypes3<-data.frame(restype,neiSatNorDisRestype)


satisfiedttRestype<-c(as.character(paste(factor(Respontype[c(11),c(1)]))))

satisfiedRestype<-c(as.numeric(paste(factor(Respontype[c(11),c(2)]))),as.numeric(paste(factor(Respontype[c(11),c(3)]))),as.numeric(paste(factor(Respontype[c(11),c(4)]))), as.numeric(paste(factor(Respontype[c(11),c(5)]))), as.numeric(paste(factor(Respontype[c(11),c(6)]))), as.numeric(paste(factor(Respontype[c(11),c(7)]))), as.numeric(paste(factor(Respontype[c(11),c(8)]))), as.numeric(paste(factor(Respontype[c(11),c(9)]))),as.numeric(paste(factor(Respontype[c(11),c(10)]))),as.numeric(paste(factor(Respontype[c(11),c(11)]))),as.numeric(paste(factor(Respontype[c(11),c(12)]))), as.numeric(paste(factor(Respontype[c(11),c(13)]))))
dfRestypes4<-data.frame(restype,satisfiedRestype)

verysattRestype<-c(as.character(paste(factor(Respontype[c(13),c(1)]))))

verySatRestype<-c(as.numeric(paste(factor(Respontype[c(13),c(2)]))),as.numeric(paste(factor(Respontype[c(13),c(3)]))),as.numeric(paste(factor(Respontype[c(13),c(4)]))), as.numeric(paste(factor(Respontype[c(13),c(5)]))), as.numeric(paste(factor(Respontype[c(13),c(6)]))), as.numeric(paste(factor(Respontype[c(13),c(7)]))), as.numeric(paste(factor(Respontype[c(13),c(8)]))), as.numeric(paste(factor(Respontype[c(13),c(9)]))),as.numeric(paste(factor(Respontype[c(13),c(10)]))),as.numeric(paste(factor(Respontype[c(13),c(11)]))),as.numeric(paste(factor(Respontype[c(13),c(12)]))), as.numeric(paste(factor(Respontype[c(13),c(13)]))))
dfRestypes5<-data.frame(restype,verySatRestype)
################################################################
###########      Age bracket    ###################################################

Agebracket<-mydata[c(2:15),c(2,117:124)]

age<-c(as.character(paste(factor(Agebracket[c(2),c(2)]))),as.character(paste(factor(Agebracket[c(2),c(3)]))),as.character(paste(factor(Agebracket[c(2),c(4)]))), as.character(paste(factor(Agebracket[c(2),c(5)]))), as.character(paste(factor(Agebracket[c(2),c(6)]))), as.character(paste(factor(Agebracket[c(2),c(7)]))), as.character(paste(factor(Agebracket[c(2),c(8)]))), as.character(paste(factor(Agebracket[c(2),c(9)]))))

very.disttAge<-c(as.character(paste(factor(Agebracket[c(5),c(1)]))))
very.disAge<-c(as.numeric(paste(factor(Agebracket[c(5),c(2)]))),as.numeric(paste(factor(Agebracket[c(5),c(3)]))),as.numeric(paste(factor(Agebracket[c(5),c(4)]))), as.numeric(paste(factor(Agebracket[c(5),c(5)]))), as.numeric(paste(factor(Agebracket[c(5),c(6)]))), as.numeric(paste(factor(Agebracket[c(5),c(7)]))), as.numeric(paste(factor(Agebracket[c(5),c(8)]))), as.numeric(paste(factor(Agebracket[c(5),c(9)]))))

dfAge<-data.frame(age,very.disAge)          

distt.Age<-c(as.character(paste(factor(Agebracket[c(7),c(1)]))))
disAge<-c(as.numeric(paste(factor(Agebracket[c(7),c(2)]))),as.numeric(paste(factor(Agebracket[c(7),c(3)]))),as.numeric(paste(factor(Agebracket[c(7),c(4)]))), as.numeric(paste(factor(Agebracket[c(7),c(5)]))), as.numeric(paste(factor(Agebracket[c(7),c(6)]))), as.numeric(paste(factor(Agebracket[c(7),c(7)]))), as.numeric(paste(factor(Agebracket[c(7),c(8)]))), as.numeric(paste(factor(Agebracket[c(7),c(9)]))))
dfAge2<-data.frame(age,disAge)


neiSaNorDistt.Age<-c(as.character(paste(factor(Agebracket[c(9),c(1)]))))

neiSatNorDisAge<-c(as.numeric(paste(factor(Agebracket[c(9),c(2)]))),as.numeric(paste(factor(Agebracket[c(9),c(3)]))),as.numeric(paste(factor(Agebracket[c(9),c(4)]))), as.numeric(paste(factor(Agebracket[c(9),c(5)]))), as.numeric(paste(factor(Agebracket[c(9),c(6)]))), as.numeric(paste(factor(Agebracket[c(9),c(7)]))), as.numeric(paste(factor(Agebracket[c(9),c(8)]))), as.numeric(paste(factor(Agebracket[c(9),c(9)]))))
dfAge3<-data.frame(age,neiSatNorDisAge)         

satisfiedttAge<-c(as.character(paste(factor(Agebracket[c(11),c(1)]))))

satisfiedAge<-c(as.numeric(paste(factor(Agebracket[c(11),c(2)]))),as.numeric(paste(factor(Agebracket[c(11),c(3)]))),as.numeric(paste(factor(Agebracket[c(11),c(4)]))), as.numeric(paste(factor(Agebracket[c(11),c(5)]))), as.numeric(paste(factor(Agebracket[c(11),c(6)]))), as.numeric(paste(factor(Agebracket[c(11),c(7)]))), as.numeric(paste(factor(Agebracket[c(11),c(8)]))), as.numeric(paste(factor(Agebracket[c(11),c(9)]))))
dfAge4<-data.frame(age,satisfiedAge)

verysatt.Age<-c(as.character(paste(factor(Agebracket[c(13),c(1)]))))

verySatAge<-c(as.numeric(paste(factor(Agebracket[c(13),c(2)]))),as.numeric(paste(factor(Agebracket[c(13),c(3)]))),as.numeric(paste(factor(Agebracket[c(13),c(4)]))), as.numeric(paste(factor(Agebracket[c(13),c(5)]))), as.numeric(paste(factor(Agebracket[c(13),c(6)]))), as.numeric(paste(factor(Agebracket[c(13),c(7)]))), as.numeric(paste(factor(Agebracket[c(13),c(8)]))), as.numeric(paste(factor(Agebracket[c(13),c(9)]))))
dfAge5<-data.frame(age,verySatAge)



################################################################################################################################################################################################
################################UNRA1..QUESTION 2......

regionq2<-mydata[18:75,2:9]

soutthttq2<-factor(regionq2[c(2),c(4)])
southernq2<-as.numeric(paste(factor( regionq2[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(4)])))
var2q2<-as.character(paste(factor( regionq2[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(1)])))
southq2<-data.frame(var2q2,southernq2)

centttq2<-factor(regionq2[c(2),c(5)])
centralq2<-as.numeric(paste(factor( regionq2[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(5)])))
centrq2<-data.frame(var2q2,centralq2)

easttq2<-factor(regionq2[c(2),c(6)])
easternq2<-as.numeric(paste(factor( regionq2[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(6)])))
eastq2<-data.frame(var2q2,easternq2)

northttq2<-factor(regionq2[c(2),c(7)])
northernq2<-as.numeric(paste(factor( regionq2[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(7)])))
northq2<-data.frame(var2q2,northernq2)

westttq2<-factor(regionq2[c(2),c(8)])
westernq2<-as.numeric(paste(factor( regionq2[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(8)])))
westq2<-data.frame(var2q2,westernq2)

###########################################################################district2 qn2###################################################
districtQn2<-mydata[18:75,c(2,10:40)]


discsQn2<-c(as.character(paste(factor(districtQn2[c(2),c(2)]))),as.character(paste(factor(districtQn2[c(2),c(3)]))),as.character(paste(factor(districtQn2[c(2),c(4)]))), as.character(paste(factor(districtQn2[c(2),c(5)]))), as.character(paste(factor(districtQn2[c(2),c(6)]))), as.character(paste(factor(districtQn2[c(2),c(7)]))), as.character(paste(factor(districtQn2[c(2),c(8)]))), as.character(paste(factor(districtQn2[c(2),c(9)]))),as.character(paste(factor(districtQn2[c(2),c(10)]))),as.character(paste(factor(districtQn2[c(2),c(11)]))),as.character(paste(factor(districtQn2[c(2),c(12)]))), as.character(paste(factor(districtQn2[c(2),c(13)]))),as.character(paste(factor(districtQn2[c(2),c(14)]))),as.character(paste(factor(districtQn2[c(2),c(15)]))),as.character(paste(factor(districtQn2[c(2),c(16)]))),
            as.character(paste(factor(districtQn2[c(2),c(17)]))),as.character(paste(factor(districtQn2[c(2),c(18)]))),as.character(paste(factor(districtQn2[c(2),c(19)]))),as.character(paste(factor(districtQn2[c(2),c(20)]))),as.character(paste(factor(districtQn2[c(2),c(21)]))),as.character(paste(factor(districtQn2[c(2),c(22)]))),as.character(paste(factor(districtQn2[c(2),c(23)]))),as.character(paste(factor(districtQn2[c(2),c(24)]))),as.character(paste(factor(districtQn2[c(2),c(25)]))),as.character(paste(factor(districtQn2[c(2),c(26)]))),as.character(paste(factor(districtQn2[c(2),c(27)]))),as.character(paste(factor(districtQn2[c(2),c(28)]))),as.character(paste(factor(districtQn2[c(2),c(29)]))),as.character(paste(factor(districtQn2[c(2),c(30)]))),as.character(paste(factor(districtQn2[c(2),c(31)]))),
            as.character(paste(factor(districtQn2[c(2),c(32)]))),as.character(paste(factor(districtQn2[c(2),c(33)]))),as.character(paste(factor(districtQn2[c(2),c(34)]))),as.character(paste(factor(districtQn2[c(2),c(35)]))),as.character(paste(factor(districtQn2[c(2),c(36)]))),as.character(paste(factor(districtQn2[c(2),c(37)]))),as.character(paste(factor(districtQn2[c(2),c(38)]))),as.character(paste(factor(districtQn2[c(2),c(39)]))),as.character(paste(factor(districtQn2[c(2),c(40)]))),as.character(paste(factor(districtQn2[c(2),c(41)]))),as.character(paste(factor(districtQn2[c(2),c(42)]))),as.character(paste(factor(districtQn2[c(2),c(43)]))),as.character(paste(factor(districtQn2[c(2),c(44)]))),as.character(paste(factor(districtQn2[c(2),c(45)]))))

roads.are.tarmtt<-c(as.character(paste(factor(districtQn2[c(5),c(1)]))))
roads.tarm<-c(as.numeric(paste(factor(districtQn2[c(5),c(2)]))),as.numeric(paste(factor(districtQn2[c(5),c(3)]))),as.numeric(paste(factor(districtQn2[c(5),c(4)]))), as.numeric(paste(factor(districtQn2[c(5),c(5)]))), as.numeric(paste(factor(districtQn2[c(5),c(6)]))), as.numeric(paste(factor(districtQn2[c(5),c(7)]))), as.numeric(paste(factor(districtQn2[c(5),c(8)]))), as.numeric(paste(factor(districtQn2[c(5),c(9)]))),as.numeric(paste(factor(districtQn2[c(5),c(10)]))),as.numeric(paste(factor(districtQn2[c(5),c(11)]))),as.numeric(paste(factor(districtQn2[c(5),c(12)]))), as.numeric(paste(factor(districtQn2[c(5),c(13)]))),as.numeric(paste(factor(districtQn2[c(5),c(14)]))),as.numeric(paste(factor(districtQn2[c(5),c(15)]))),as.numeric(paste(factor(districtQn2[c(5),c(16)]))),
              as.numeric(paste(factor(districtQn2[c(5),c(17)]))),as.numeric(paste(factor(districtQn2[c(5),c(18)]))),as.numeric(paste(factor(districtQn2[c(5),c(19)]))),as.numeric(paste(factor(districtQn2[c(5),c(20)]))),as.numeric(paste(factor(districtQn2[c(5),c(21)]))),as.numeric(paste(factor(districtQn2[c(5),c(22)]))),as.numeric(paste(factor(districtQn2[c(5),c(23)]))),as.numeric(paste(factor(districtQn2[c(5),c(24)]))),as.numeric(paste(factor(districtQn2[c(5),c(25)]))),as.numeric(paste(factor(districtQn2[c(5),c(26)]))),as.numeric(paste(factor(districtQn2[c(5),c(27)]))),as.numeric(paste(factor(districtQn2[c(5),c(28)]))),as.numeric(paste(factor(districtQn2[c(5),c(29)]))),as.numeric(paste(factor(districtQn2[c(5),c(30)]))),as.numeric(paste(factor(districtQn2[c(5),c(31)]))),
              as.numeric(paste(factor(districtQn2[c(5),c(32)]))),as.numeric(paste(factor(districtQn2[c(5),c(33)]))),as.numeric(paste(factor(districtQn2[c(5),c(34)]))),as.numeric(paste(factor(districtQn2[c(5),c(35)]))),as.numeric(paste(factor(districtQn2[c(5),c(36)]))),as.numeric(paste(factor(districtQn2[c(5),c(37)]))),as.numeric(paste(factor(districtQn2[c(5),c(38)]))),as.numeric(paste(factor(districtQn2[c(5),c(39)]))),as.numeric(paste(factor(districtQn2[c(5),c(40)]))),as.numeric(paste(factor(districtQn2[c(5),c(41)]))),as.numeric(paste(factor(districtQn2[c(5),c(42)]))),as.numeric(paste(factor(districtQn2[c(5),c(43)]))),as.numeric(paste(factor(districtQn2[c(5),c(44)]))),as.numeric(paste(factor(districtQn2[c(5),c(45)]))))
rdstar<-data.frame(discsQn2,roads.tarm)

morerds.contt<-c(as.character(paste(factor(districtQn2[c(7),c(1)]))))
moroads.cons<-c(as.numeric(paste(factor(districtQn2[c(7),c(2)]))),as.numeric(paste(factor(districtQn2[c(7),c(3)]))),as.numeric(paste(factor(districtQn2[c(7),c(4)]))), as.numeric(paste(factor(districtQn2[c(7),c(5)]))), as.numeric(paste(factor(districtQn2[c(7),c(6)]))), as.numeric(paste(factor(districtQn2[c(7),c(7)]))), as.numeric(paste(factor(districtQn2[c(7),c(8)]))), as.numeric(paste(factor(districtQn2[c(7),c(9)]))),as.numeric(paste(factor(districtQn2[c(7),c(10)]))),as.numeric(paste(factor(districtQn2[c(7),c(11)]))),as.numeric(paste(factor(districtQn2[c(7),c(12)]))), as.numeric(paste(factor(districtQn2[c(7),c(13)]))),as.numeric(paste(factor(districtQn2[c(7),c(14)]))),as.numeric(paste(factor(districtQn2[c(7),c(15)]))),as.numeric(paste(factor(districtQn2[c(7),c(16)]))),
                as.numeric(paste(factor(districtQn2[c(7),c(17)]))),as.numeric(paste(factor(districtQn2[c(7),c(18)]))),as.numeric(paste(factor(districtQn2[c(7),c(19)]))),as.numeric(paste(factor(districtQn2[c(7),c(20)]))),as.numeric(paste(factor(districtQn2[c(7),c(21)]))),as.numeric(paste(factor(districtQn2[c(7),c(22)]))),as.numeric(paste(factor(districtQn2[c(7),c(23)]))),as.numeric(paste(factor(districtQn2[c(7),c(24)]))),as.numeric(paste(factor(districtQn2[c(7),c(25)]))),as.numeric(paste(factor(districtQn2[c(7),c(26)]))),as.numeric(paste(factor(districtQn2[c(7),c(27)]))),as.numeric(paste(factor(districtQn2[c(7),c(28)]))),as.numeric(paste(factor(districtQn2[c(7),c(29)]))),as.numeric(paste(factor(districtQn2[c(7),c(30)]))),as.numeric(paste(factor(districtQn2[c(7),c(31)]))),
                as.numeric(paste(factor(districtQn2[c(7),c(32)]))),as.numeric(paste(factor(districtQn2[c(7),c(33)]))),as.numeric(paste(factor(districtQn2[c(7),c(34)]))),as.numeric(paste(factor(districtQn2[c(7),c(35)]))),as.numeric(paste(factor(districtQn2[c(7),c(36)]))),as.numeric(paste(factor(districtQn2[c(7),c(37)]))),as.numeric(paste(factor(districtQn2[c(7),c(38)]))),as.numeric(paste(factor(districtQn2[c(7),c(39)]))),as.numeric(paste(factor(districtQn2[c(7),c(40)]))),as.numeric(paste(factor(districtQn2[c(7),c(41)]))),as.numeric(paste(factor(districtQn2[c(7),c(42)]))),as.numeric(paste(factor(districtQn2[c(7),c(43)]))),as.numeric(paste(factor(districtQn2[c(7),c(44)]))),as.numeric(paste(factor(districtQn2[c(7),c(45)]))))
mo.rds.con<-data.frame(discsQn2,moroads.cons)

no.filled.potholestt<-c(as.character(paste(factor(districtQn2[c(9),c(1)]))))
no.filledpot<-c(as.numeric(paste(factor(districtQn2[c(9),c(2)]))),as.numeric(paste(factor(districtQn2[c(9),c(3)]))),as.numeric(paste(factor(districtQn2[c(9),c(4)]))), as.numeric(paste(factor(districtQn2[c(9),c(5)]))), as.numeric(paste(factor(districtQn2[c(9),c(6)]))), as.numeric(paste(factor(districtQn2[c(9),c(7)]))), as.numeric(paste(factor(districtQn2[c(9),c(8)]))), as.numeric(paste(factor(districtQn2[c(9),c(9)]))),as.numeric(paste(factor(districtQn2[c(9),c(10)]))),as.numeric(paste(factor(districtQn2[c(9),c(11)]))),as.numeric(paste(factor(districtQn2[c(9),c(12)]))), as.numeric(paste(factor(districtQn2[c(9),c(13)]))),as.numeric(paste(factor(districtQn2[c(9),c(14)]))),as.numeric(paste(factor(districtQn2[c(9),c(15)]))),as.numeric(paste(factor(districtQn2[c(9),c(16)]))),
                as.numeric(paste(factor(districtQn2[c(9),c(17)]))),as.numeric(paste(factor(districtQn2[c(9),c(18)]))),as.numeric(paste(factor(districtQn2[c(9),c(19)]))),as.numeric(paste(factor(districtQn2[c(9),c(20)]))),as.numeric(paste(factor(districtQn2[c(9),c(21)]))),as.numeric(paste(factor(districtQn2[c(9),c(22)]))),as.numeric(paste(factor(districtQn2[c(9),c(23)]))),as.numeric(paste(factor(districtQn2[c(9),c(24)]))),as.numeric(paste(factor(districtQn2[c(9),c(25)]))),as.numeric(paste(factor(districtQn2[c(9),c(26)]))),as.numeric(paste(factor(districtQn2[c(9),c(27)]))),as.numeric(paste(factor(districtQn2[c(9),c(28)]))),as.numeric(paste(factor(districtQn2[c(9),c(29)]))),as.numeric(paste(factor(districtQn2[c(9),c(30)]))),as.numeric(paste(factor(districtQn2[c(9),c(31)]))),
                as.numeric(paste(factor(districtQn2[c(9),c(32)]))),as.numeric(paste(factor(districtQn2[c(9),c(33)]))),as.numeric(paste(factor(districtQn2[c(9),c(34)]))),as.numeric(paste(factor(districtQn2[c(9),c(35)]))),as.numeric(paste(factor(districtQn2[c(9),c(36)]))),as.numeric(paste(factor(districtQn2[c(9),c(37)]))),as.numeric(paste(factor(districtQn2[c(9),c(38)]))),as.numeric(paste(factor(districtQn2[c(9),c(39)]))),as.numeric(paste(factor(districtQn2[c(9),c(40)]))),as.numeric(paste(factor(districtQn2[c(9),c(41)]))),as.numeric(paste(factor(districtQn2[c(9),c(42)]))),as.numeric(paste(factor(districtQn2[c(9),c(43)]))),as.numeric(paste(factor(districtQn2[c(9),c(44)]))),as.numeric(paste(factor(districtQn2[c(9),c(45)]))))
no.filledpots<-data.frame(discsQn2,no.filledpot)


imprd.rd.sigtt<-c(as.character(paste(factor(districtQn2[c(11),c(1)]))))
imprvd.rd.sig<-c(as.numeric(paste(factor(districtQn2[c(11),c(2)]))),as.numeric(paste(factor(districtQn2[c(11),c(3)]))),as.numeric(paste(factor(districtQn2[c(11),c(4)]))), as.numeric(paste(factor(districtQn2[c(11),c(5)]))), as.numeric(paste(factor(districtQn2[c(11),c(6)]))), as.numeric(paste(factor(districtQn2[c(11),c(7)]))), as.numeric(paste(factor(districtQn2[c(11),c(8)]))), as.numeric(paste(factor(districtQn2[c(11),c(9)]))),as.numeric(paste(factor(districtQn2[c(11),c(10)]))),as.numeric(paste(factor(districtQn2[c(11),c(11)]))),as.numeric(paste(factor(districtQn2[c(11),c(12)]))), as.numeric(paste(factor(districtQn2[c(11),c(13)]))),as.numeric(paste(factor(districtQn2[c(11),c(14)]))),as.numeric(paste(factor(districtQn2[c(11),c(15)]))),as.numeric(paste(factor(districtQn2[c(11),c(16)]))),
                 as.numeric(paste(factor(districtQn2[c(11),c(17)]))),as.numeric(paste(factor(districtQn2[c(11),c(18)]))),as.numeric(paste(factor(districtQn2[c(11),c(19)]))),as.numeric(paste(factor(districtQn2[c(11),c(20)]))),as.numeric(paste(factor(districtQn2[c(11),c(21)]))),as.numeric(paste(factor(districtQn2[c(11),c(22)]))),as.numeric(paste(factor(districtQn2[c(11),c(23)]))),as.numeric(paste(factor(districtQn2[c(11),c(24)]))),as.numeric(paste(factor(districtQn2[c(11),c(25)]))),as.numeric(paste(factor(districtQn2[c(11),c(26)]))),as.numeric(paste(factor(districtQn2[c(11),c(27)]))),as.numeric(paste(factor(districtQn2[c(11),c(28)]))),as.numeric(paste(factor(districtQn2[c(11),c(29)]))),as.numeric(paste(factor(districtQn2[c(11),c(30)]))),as.numeric(paste(factor(districtQn2[c(11),c(31)]))),
                 as.numeric(paste(factor(districtQn2[c(11),c(32)]))),as.numeric(paste(factor(districtQn2[c(11),c(33)]))),as.numeric(paste(factor(districtQn2[c(11),c(34)]))),as.numeric(paste(factor(districtQn2[c(11),c(35)]))),as.numeric(paste(factor(districtQn2[c(11),c(36)]))),as.numeric(paste(factor(districtQn2[c(11),c(37)]))),as.numeric(paste(factor(districtQn2[c(11),c(38)]))),as.numeric(paste(factor(districtQn2[c(11),c(39)]))),as.numeric(paste(factor(districtQn2[c(11),c(40)]))),as.numeric(paste(factor(districtQn2[c(11),c(41)]))),as.numeric(paste(factor(districtQn2[c(11),c(42)]))),as.numeric(paste(factor(districtQn2[c(11),c(43)]))),as.numeric(paste(factor(districtQn2[c(11),c(44)]))),as.numeric(paste(factor(districtQn2[c(11),c(45)]))))
imprd.rd.sigs<-data.frame(discsQn2,imprvd.rd.sig)


strict.usage.bodatt<-c(as.character(paste(factor(districtQn2[c(13),c(1)]))))
strict.rd.usage.boda<-c(as.numeric(paste(factor(districtQn2[c(13),c(2)]))),as.numeric(paste(factor(districtQn2[c(13),c(3)]))),as.numeric(paste(factor(districtQn2[c(13),c(4)]))), as.numeric(paste(factor(districtQn2[c(13),c(5)]))), as.numeric(paste(factor(districtQn2[c(13),c(6)]))), as.numeric(paste(factor(districtQn2[c(13),c(7)]))), as.numeric(paste(factor(districtQn2[c(13),c(8)]))), as.numeric(paste(factor(districtQn2[c(13),c(9)]))),as.numeric(paste(factor(districtQn2[c(13),c(10)]))),as.numeric(paste(factor(districtQn2[c(13),c(11)]))),as.numeric(paste(factor(districtQn2[c(13),c(12)]))), as.numeric(paste(factor(districtQn2[c(13),c(13)]))),as.numeric(paste(factor(districtQn2[c(13),c(14)]))),as.numeric(paste(factor(districtQn2[c(13),c(15)]))),as.numeric(paste(factor(districtQn2[c(13),c(16)]))),
                        as.numeric(paste(factor(districtQn2[c(13),c(17)]))),as.numeric(paste(factor(districtQn2[c(13),c(18)]))),as.numeric(paste(factor(districtQn2[c(13),c(19)]))),as.numeric(paste(factor(districtQn2[c(13),c(20)]))),as.numeric(paste(factor(districtQn2[c(13),c(21)]))),as.numeric(paste(factor(districtQn2[c(13),c(22)]))),as.numeric(paste(factor(districtQn2[c(13),c(23)]))),as.numeric(paste(factor(districtQn2[c(13),c(24)]))),as.numeric(paste(factor(districtQn2[c(13),c(25)]))),as.numeric(paste(factor(districtQn2[c(13),c(26)]))),as.numeric(paste(factor(districtQn2[c(13),c(27)]))),as.numeric(paste(factor(districtQn2[c(13),c(28)]))),as.numeric(paste(factor(districtQn2[c(13),c(29)]))),as.numeric(paste(factor(districtQn2[c(13),c(30)]))),as.numeric(paste(factor(districtQn2[c(13),c(31)]))),
                        as.numeric(paste(factor(districtQn2[c(13),c(32)]))),as.numeric(paste(factor(districtQn2[c(13),c(33)]))),as.numeric(paste(factor(districtQn2[c(13),c(34)]))),as.numeric(paste(factor(districtQn2[c(13),c(35)]))),as.numeric(paste(factor(districtQn2[c(13),c(36)]))),as.numeric(paste(factor(districtQn2[c(13),c(37)]))),as.numeric(paste(factor(districtQn2[c(13),c(38)]))),as.numeric(paste(factor(districtQn2[c(13),c(39)]))),as.numeric(paste(factor(districtQn2[c(13),c(40)]))),as.numeric(paste(factor(districtQn2[c(13),c(41)]))),as.numeric(paste(factor(districtQn2[c(13),c(42)]))),as.numeric(paste(factor(districtQn2[c(13),c(43)]))),as.numeric(paste(factor(districtQn2[c(13),c(44)]))),as.numeric(paste(factor(districtQn2[c(13),c(45)]))))
stric.usage.bb<-data.frame(discsQn2,strict.rd.usage.boda)
############# ROAD TYPE QN2####



RoadtypeQn2<- mydata[c(18:75),c(2,41:42)]

rdtypesqn2<-c(as.character(paste(factor(RoadtypeQn2[c(2),c(2)]))),as.character(paste(factor(RoadtypeQn2[c(2),c(3)]))))


roads.are.tarmtt2<-c(as.character(paste(factor(RoadtypeQn2[c(5),c(1)]))))
roads.tarm2<-c(as.numeric(paste(factor(RoadtypeQn2[c(5),c(2)]))),as.numeric(paste(factor(RoadtypeQn2[c(5),c(3)]))))

rdstar2<-data.frame(rdtypesqn2,roads.tarm2)


morerds.contt2<-c(as.character(paste(factor(RoadtypeQn2[c(7),c(1)]))))
moroads.cons2<-c(as.numeric(paste(factor(RoadtypeQn2[c(7),c(2)]))),as.numeric(paste(factor(RoadtypeQn2[c(7),c(3)]))))

mo.rds.con2<-data.frame(rdtypesqn2,moroads.cons2)


no.filled.potholestt2<-c(as.character(paste(factor(RoadtypeQn2[c(9),c(1)]))))
no.filledpot2<-c(as.numeric(paste(factor(RoadtypeQn2[c(9),c(2)]))),as.numeric(paste(factor(RoadtypeQn2[c(9),c(3)]))))

no.filledpots2<-data.frame(rdtypesqn2,no.filledpot2)


imprd.rd.sigtt2<-c(as.character(paste(factor(RoadtypeQn2[c(11),c(1)]))))
imprvd.rd.sig2<-c(as.numeric(paste(factor(RoadtypeQn2[c(11),c(2)]))),as.numeric(paste(factor(RoadtypeQn2[c(11),c(3)]))))

imprd.rd.sigs2<-data.frame(rdtypesqn2,imprvd.rd.sig2)


strict.usage.bodatt2<-c(as.character(paste(factor(RoadtypeQn2[c(13),c(1)]))))
strict.rd.usage.boda2<-c(as.numeric(paste(factor(RoadtypeQn2[c(13),c(2)]))),as.numeric(paste(factor(RoadtypeQn2[c(13),c(3)]))))

stric.usage.bb2<-data.frame(rdtypesqn2,strict.rd.usage.boda2)



############################################ RESPONDENT GENDER Qn2##############################
RespongenderQn2<- mydata[c(18:75),c(2,43:44)]
genderQn2<-c(as.character(paste(factor( RespongenderQn2[c(2),c(2)]))),as.character(paste(factor( RespongenderQn2[c(2),c(3)]))))

roads.are.tarmtt3<-c(as.character(paste(factor( RespongenderQn2[c(5),c(1)]))))
roads.tarm3<-c(as.numeric(paste(factor( RespongenderQn2[c(5),c(2)]))),as.numeric(paste(factor( RespongenderQn2[c(5),c(3)]))))
rdstar3<-data.frame( genderQn2,roads.tarm3)

morerds.contt3<-c(as.character(paste(factor( RespongenderQn2[c(7),c(1)]))))
moroads.cons3<-c(as.numeric(paste(factor( RespongenderQn2[c(7),c(2)]))),as.numeric(paste(factor( RespongenderQn2[c(7),c(3)]))))
mo.rds.con3<-data.frame( genderQn2,moroads.cons3)

no.filled.potholestt3<-c(as.character(paste(factor( RespongenderQn2[c(9),c(1)]))))
no.filledpot3<-c(as.numeric(paste(factor( RespongenderQn2[c(9),c(2)]))),as.numeric(paste(factor( RespongenderQn2[c(9),c(3)]))))
no.filledpots3<-data.frame( genderQn2,no.filledpot3)

imprd.rd.sigtt3<-c(as.character(paste(factor( RespongenderQn2[c(11),c(1)]))))
imprvd.rd.sig3<-c(as.numeric(paste(factor( RespongenderQn2[c(11),c(2)]))),as.numeric(paste(factor( RespongenderQn2[c(11),c(3)]))))
imprd.rd.sigs3<-data.frame( genderQn2,imprvd.rd.sig3)

strict.usage.bodatt3<-c(as.character(paste(factor( RespongenderQn2[c(13),c(1)]))))
strict.rd.usage.boda3<-c(as.numeric(paste(factor( RespongenderQn2[c(13),c(2)]))),as.numeric(paste(factor( RespongenderQn2[c(13),c(3)]))))
stric.usage.bb3<-data.frame( genderQn2,strict.rd.usage.boda3)

########################################ROAD NAME Qn2##########################################################################

RoadnmQn2<-mydata[18:75,c(2,45:104)]

rdnamesQn2<-c(as.character(paste(factor(RoadnmQn2[c(2),c(2)]))),as.character(paste(factor(RoadnmQn2[c(2),c(3)]))),as.character(paste(factor(RoadnmQn2[c(2),c(4)]))), as.character(paste(factor(RoadnmQn2[c(2),c(5)]))), as.character(paste(factor(RoadnmQn2[c(2),c(6)]))), as.character(paste(factor(RoadnmQn2[c(2),c(7)]))), as.character(paste(factor(RoadnmQn2[c(2),c(8)]))), as.character(paste(factor(RoadnmQn2[c(2),c(9)]))),as.character(paste(factor(RoadnmQn2[c(2),c(10)]))),as.character(paste(factor(RoadnmQn2[c(2),c(11)]))),as.character(paste(factor(RoadnmQn2[c(2),c(12)]))), as.character(paste(factor(RoadnmQn2[c(2),c(13)]))),as.character(paste(factor(RoadnmQn2[c(2),c(14)]))),as.character(paste(factor(RoadnmQn2[c(2),c(15)]))),as.character(paste(factor(RoadnmQn2[c(2),c(16)]))),
              as.character(paste(factor(RoadnmQn2[c(2),c(17)]))),as.character(paste(factor(RoadnmQn2[c(2),c(18)]))),as.character(paste(factor(RoadnmQn2[c(2),c(19)]))),as.character(paste(factor(RoadnmQn2[c(2),c(20)]))),as.character(paste(factor(RoadnmQn2[c(2),c(21)]))),as.character(paste(factor(RoadnmQn2[c(2),c(22)]))),as.character(paste(factor(RoadnmQn2[c(2),c(23)]))),as.character(paste(factor(RoadnmQn2[c(2),c(24)]))),as.character(paste(factor(RoadnmQn2[c(2),c(25)]))),as.character(paste(factor(RoadnmQn2[c(2),c(26)]))),as.character(paste(factor(RoadnmQn2[c(2),c(27)]))),as.character(paste(factor(RoadnmQn2[c(2),c(28)]))),as.character(paste(factor(RoadnmQn2[c(2),c(29)]))),as.character(paste(factor(RoadnmQn2[c(2),c(30)]))),as.character(paste(factor(RoadnmQn2[c(2),c(31)]))),
              as.character(paste(factor(RoadnmQn2[c(2),c(32)]))),as.character(paste(factor(RoadnmQn2[c(2),c(33)]))),as.character(paste(factor(RoadnmQn2[c(2),c(34)]))),as.character(paste(factor(RoadnmQn2[c(2),c(35)]))),as.character(paste(factor(RoadnmQn2[c(2),c(36)]))),as.character(paste(factor(RoadnmQn2[c(2),c(37)]))),as.character(paste(factor(RoadnmQn2[c(2),c(38)]))),as.character(paste(factor(RoadnmQn2[c(2),c(39)]))),as.character(paste(factor(RoadnmQn2[c(2),c(40)]))),as.character(paste(factor(RoadnmQn2[c(2),c(41)]))),as.character(paste(factor(RoadnmQn2[c(2),c(42)]))),as.character(paste(factor(RoadnmQn2[c(2),c(43)]))),as.character(paste(factor(RoadnmQn2[c(2),c(44)]))),as.character(paste(factor(RoadnmQn2[c(2),c(45)]))),as.character(paste(factor(RoadnmQn2[c(2),c(46)]))),as.character(paste(factor(RoadnmQn2[c(2),c(47)]))),as.character(paste(factor(RoadnmQn2[c(2),c(48)]))), as.character(paste(factor(RoadnmQn2[c(2),c(49)]))), as.character(paste(factor(RoadnmQn2[c(2),c(50)]))), as.character(paste(factor(RoadnmQn2[c(2),c(51)]))), as.character(paste(factor(RoadnmQn2[c(2),c(52)]))), as.character(paste(factor(RoadnmQn2[c(2),c(53)]))),as.character(paste(factor(RoadnmQn2[c(2),c(54)]))),as.character(paste(factor(RoadnmQn2[c(2),c(55)]))),as.character(paste(factor(RoadnmQn2[c(2),c(56)]))), as.character(paste(factor(RoadnmQn2[c(2),c(57)]))),
              as.character(paste(factor(RoadnmQn2[c(2),c(58)]))),as.character(paste(factor(RoadnmQn2[c(2),c(59)]))),as.character(paste(factor(RoadnmQn2[c(2),c(60)]))), as.character(paste(factor(RoadnmQn2[c(2),c(61)]))))

roads.are.tarmtt4<-c(as.character(paste(factor(RoadnmQn2[c(5),c(1)]))))

roads.tarm4<-c(as.numeric(paste(factor(RoadnmQn2[c(5),c(2)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(3)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(4)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(5)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(6)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(7)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(8)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(9)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(10)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(11)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(12)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(13)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(14)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(15)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(16)]))),
               as.numeric(paste(factor(RoadnmQn2[c(5),c(17)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(18)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(19)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(20)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(21)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(22)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(23)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(24)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(25)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(26)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(27)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(28)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(29)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(30)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(31)]))),
               as.numeric(paste(factor(RoadnmQn2[c(5),c(32)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(33)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(34)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(35)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(36)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(37)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(38)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(39)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(40)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(41)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(42)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(43)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(44)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(45)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(46)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(47)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(48)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(49)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(50)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(51)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(52)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(53)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(54)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(55)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(56)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(57)]))),
               as.numeric(paste(factor(RoadnmQn2[c(5),c(58)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(59)]))),as.numeric(paste(factor(RoadnmQn2[c(5),c(60)]))), as.numeric(paste(factor(RoadnmQn2[c(5),c(61)]))))
rdstar4<-data.frame(rdnamesQn2,roads.tarm4)

morerds.contt4<-c(as.character(paste(factor(RoadnmQn2[c(7),c(1)]))))
moroads.cons4<-c(as.numeric(paste(factor(RoadnmQn2[c(7),c(2)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(3)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(4)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(5)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(6)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(7)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(8)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(9)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(10)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(11)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(12)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(13)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(14)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(15)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(16)]))),
                 as.numeric(paste(factor(RoadnmQn2[c(7),c(17)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(18)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(19)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(20)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(21)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(22)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(23)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(24)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(25)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(26)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(27)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(28)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(29)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(30)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(31)]))),
                 as.numeric(paste(factor(RoadnmQn2[c(7),c(32)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(33)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(34)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(35)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(36)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(37)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(38)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(39)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(40)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(41)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(42)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(43)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(44)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(45)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(46)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(47)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(48)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(49)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(50)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(51)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(52)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(53)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(54)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(55)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(56)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(57)]))),
                 as.numeric(paste(factor(RoadnmQn2[c(7),c(58)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(59)]))),as.numeric(paste(factor(RoadnmQn2[c(7),c(60)]))), as.numeric(paste(factor(RoadnmQn2[c(7),c(61)]))))
mo.rds.con4<-data.frame(rdnamesQn2,moroads.cons4)

no.filled.potholestt4<-c(as.character(paste(factor(RoadnmQn2[c(9),c(1)]))))
no.filledpot4<-c(as.numeric(paste(factor(RoadnmQn2[c(9),c(2)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(3)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(4)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(5)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(6)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(7)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(8)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(9)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(10)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(11)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(12)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(13)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(14)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(15)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(16)]))),
                 as.numeric(paste(factor(RoadnmQn2[c(9),c(17)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(18)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(19)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(20)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(21)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(22)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(23)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(24)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(25)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(26)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(27)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(28)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(29)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(30)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(31)]))),
                 as.numeric(paste(factor(RoadnmQn2[c(9),c(32)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(33)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(34)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(35)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(36)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(37)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(38)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(39)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(40)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(41)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(42)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(43)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(44)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(45)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(46)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(47)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(48)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(49)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(50)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(51)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(52)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(53)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(54)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(55)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(56)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(57)]))),
                 as.numeric(paste(factor(RoadnmQn2[c(9),c(58)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(59)]))),as.numeric(paste(factor(RoadnmQn2[c(9),c(60)]))), as.numeric(paste(factor(RoadnmQn2[c(9),c(61)]))))
no.filledpots4<-data.frame(rdnamesQn2,no.filledpot4)


imprd.rd.sigtt4<-c(as.character(paste(factor(RoadnmQn2[c(11),c(1)]))))
imprvd.rd.sig4<-c(as.numeric(paste(factor(RoadnmQn2[c(11),c(2)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(3)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(4)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(5)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(6)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(7)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(8)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(9)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(10)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(11)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(12)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(13)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(14)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(15)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(16)]))),
                  as.numeric(paste(factor(RoadnmQn2[c(11),c(17)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(18)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(19)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(20)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(21)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(22)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(23)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(24)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(25)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(26)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(27)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(28)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(29)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(30)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(31)]))),
                  as.numeric(paste(factor(RoadnmQn2[c(11),c(32)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(33)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(34)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(35)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(36)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(37)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(38)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(39)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(40)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(41)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(42)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(43)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(44)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(45)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(46)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(47)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(48)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(49)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(50)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(51)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(52)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(53)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(54)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(55)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(56)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(57)]))),
                  as.numeric(paste(factor(RoadnmQn2[c(11),c(58)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(59)]))),as.numeric(paste(factor(RoadnmQn2[c(11),c(60)]))), as.numeric(paste(factor(RoadnmQn2[c(11),c(61)]))))
imprd.rd.sigs4<-data.frame(rdnamesQn2,imprvd.rd.sig4)


strict.usage.bodatt4<-c(as.character(paste(factor(RoadnmQn2[c(13),c(1)]))))
strict.rd.usage.boda4<-c(as.numeric(paste(factor(RoadnmQn2[c(13),c(2)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(3)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(4)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(5)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(6)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(7)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(8)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(9)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(10)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(11)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(12)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(13)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(14)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(15)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(16)]))),
                         as.numeric(paste(factor(RoadnmQn2[c(13),c(17)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(18)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(19)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(20)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(21)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(22)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(23)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(24)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(25)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(26)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(27)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(28)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(29)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(30)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(31)]))),
                         as.numeric(paste(factor(RoadnmQn2[c(13),c(32)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(33)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(34)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(35)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(36)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(37)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(38)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(39)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(40)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(41)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(42)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(43)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(44)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(45)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(46)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(47)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(48)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(49)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(50)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(51)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(52)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(53)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(54)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(55)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(56)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(57)]))),
                         as.numeric(paste(factor(RoadnmQn2[c(13),c(58)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(59)]))),as.numeric(paste(factor(RoadnmQn2[c(13),c(60)]))), as.numeric(paste(factor(RoadnmQn2[c(13),c(61)]))))
stric.usage.bb4<-data.frame(rdnamesQn2,strict.rd.usage.boda4)

############################################respondent type qn2##################
RespontypeQn2<-mydata[18:75,c(2,105:110)]
restypeQn2<-c(as.character(paste(factor(RespontypeQn2[c(2),c(2)]))),as.character(paste(factor(RespontypeQn2[c(2),c(3)]))),as.character(paste(factor(RespontypeQn2[c(2),c(4)]))), as.character(paste(factor(RespontypeQn2[c(2),c(5)]))), as.character(paste(factor(RespontypeQn2[c(2),c(6)]))), as.character(paste(factor(RespontypeQn2[c(2),c(7)]))), as.character(paste(factor(RespontypeQn2[c(2),c(8)]))), as.character(paste(factor(RespontypeQn2[c(2),c(9)]))),as.character(paste(factor(RespontypeQn2[c(2),c(10)]))),as.character(paste(factor(RespontypeQn2[c(2),c(11)]))))

roads.are.tarmtt5<-c(as.character(paste(factor(RespontypeQn2[c(5),c(1)]))))
roads.tarm5<-c(as.numeric(paste(factor(RespontypeQn2[c(5),c(2)]))),as.numeric(paste(factor(RespontypeQn2[c(5),c(3)]))),as.numeric(paste(factor(RespontypeQn2[c(5),c(4)]))), as.numeric(paste(factor(RespontypeQn2[c(5),c(5)]))), as.numeric(paste(factor(RespontypeQn2[c(5),c(6)]))), as.numeric(paste(factor(RespontypeQn2[c(5),c(7)]))), as.numeric(paste(factor(RespontypeQn2[c(5),c(8)]))), as.numeric(paste(factor(RespontypeQn2[c(5),c(9)]))),as.numeric(paste(factor(RespontypeQn2[c(5),c(10)]))),as.numeric(paste(factor(RespontypeQn2[c(5),c(11)]))))
rdstar5<-data.frame( restypeQn2,roads.tarm5)

morerds.contt5<-c(as.character(paste(factor(RespontypeQn2[c(7),c(1)]))))
moroads.cons5<-c(as.numeric(paste(factor(RespontypeQn2[c(7),c(2)]))),as.numeric(paste(factor(RespontypeQn2[c(7),c(3)]))),as.numeric(paste(factor(RespontypeQn2[c(7),c(4)]))), as.numeric(paste(factor(RespontypeQn2[c(7),c(5)]))), as.numeric(paste(factor(RespontypeQn2[c(7),c(6)]))), as.numeric(paste(factor(RespontypeQn2[c(7),c(7)]))), as.numeric(paste(factor(RespontypeQn2[c(7),c(8)]))), as.numeric(paste(factor(RespontypeQn2[c(7),c(9)]))),as.numeric(paste(factor(RespontypeQn2[c(7),c(10)]))),as.numeric(paste(factor(RespontypeQn2[c(7),c(11)]))))
mo.rds.con5<-data.frame( restypeQn2,moroads.cons5)

no.filled.potholestt5<-c(as.character(paste(factor(RespontypeQn2[c(9),c(1)]))))
no.filledpot5<-c(as.numeric(paste(factor(RespontypeQn2[c(9),c(2)]))),as.numeric(paste(factor(RespontypeQn2[c(9),c(3)]))),as.numeric(paste(factor(RespontypeQn2[c(9),c(4)]))), as.numeric(paste(factor(RespontypeQn2[c(9),c(5)]))), as.numeric(paste(factor(RespontypeQn2[c(9),c(6)]))), as.numeric(paste(factor(RespontypeQn2[c(9),c(7)]))), as.numeric(paste(factor(RespontypeQn2[c(9),c(8)]))), as.numeric(paste(factor(RespontypeQn2[c(9),c(9)]))),as.numeric(paste(factor(RespontypeQn2[c(9),c(10)]))),as.numeric(paste(factor(RespontypeQn2[c(9),c(11)]))))
no.filledpots5<-data.frame( restypeQn2,no.filledpot5)

imprd.rd.sigtt5<-c(as.character(paste(factor(RespontypeQn2[c(11),c(1)]))))
imprvd.rd.sig5<-c(as.numeric(paste(factor(RespontypeQn2[c(11),c(2)]))),as.numeric(paste(factor(RespontypeQn2[c(11),c(3)]))),as.numeric(paste(factor(RespontypeQn2[c(11),c(4)]))), as.numeric(paste(factor(RespontypeQn2[c(11),c(5)]))), as.numeric(paste(factor(RespontypeQn2[c(11),c(6)]))), as.numeric(paste(factor(RespontypeQn2[c(11),c(7)]))), as.numeric(paste(factor(RespontypeQn2[c(11),c(8)]))), as.numeric(paste(factor(RespontypeQn2[c(11),c(9)]))),as.numeric(paste(factor(RespontypeQn2[c(11),c(10)]))),as.numeric(paste(factor(RespontypeQn2[c(11),c(11)]))))
imprd.rd.sigs5<-data.frame( restypeQn2,imprvd.rd.sig5)

strict.usage.bodatt5<-c(as.character(paste(factor(RespontypeQn2[c(13),c(1)]))))
strict.rd.usage.boda5<-c(as.numeric(paste(factor(RespontypeQn2[c(13),c(2)]))),as.numeric(paste(factor(RespontypeQn2[c(13),c(3)]))),as.numeric(paste(factor(RespontypeQn2[c(13),c(4)]))), as.numeric(paste(factor(RespontypeQn2[c(13),c(5)]))), as.numeric(paste(factor(RespontypeQn2[c(13),c(6)]))), as.numeric(paste(factor(RespontypeQn2[c(13),c(7)]))), as.numeric(paste(factor(RespontypeQn2[c(13),c(8)]))), as.numeric(paste(factor(RespontypeQn2[c(13),c(9)]))),as.numeric(paste(factor(RespontypeQn2[c(13),c(10)]))),as.numeric(paste(factor(RespontypeQn2[c(13),c(11)]))))
stric.usage.bb5<-data.frame( restypeQn2,strict.rd.usage.boda5)

##############################################age bracket qn2#################

AgebracketQn2<-mydata[18:75,c(2,111:118)]
ageQn2<-c(as.character(paste(factor(AgebracketQn2[c(2),c(2)]))),as.character(paste(factor(AgebracketQn2[c(2),c(3)]))),as.character(paste(factor(AgebracketQn2[c(2),c(4)]))), as.character(paste(factor(AgebracketQn2[c(2),c(5)]))), as.character(paste(factor(AgebracketQn2[c(2),c(6)]))), as.character(paste(factor(AgebracketQn2[c(2),c(7)]))), as.character(paste(factor(AgebracketQn2[c(2),c(8)]))), as.character(paste(factor(AgebracketQn2[c(2),c(9)]))))

roads.are.tarmtt6<-c(as.character(paste(factor(AgebracketQn2[c(5),c(1)]))))
roads.tarm6<-c(as.numeric(paste(factor(AgebracketQn2[c(5),c(2)]))),as.numeric(paste(factor(AgebracketQn2[c(5),c(3)]))),as.numeric(paste(factor(AgebracketQn2[c(5),c(4)]))), as.numeric(paste(factor(AgebracketQn2[c(5),c(5)]))), as.numeric(paste(factor(AgebracketQn2[c(5),c(6)]))), as.numeric(paste(factor(AgebracketQn2[c(5),c(7)]))), as.numeric(paste(factor(AgebracketQn2[c(5),c(8)]))), as.numeric(paste(factor(AgebracketQn2[c(5),c(9)]))))
rdstar6<-data.frame( ageQn2,roads.tarm6)

morerds.contt6<-c(as.character(paste(factor(AgebracketQn2[c(7),c(1)]))))
moroads.cons6<-c(as.numeric(paste(factor(AgebracketQn2[c(7),c(2)]))),as.numeric(paste(factor(AgebracketQn2[c(7),c(3)]))),as.numeric(paste(factor(AgebracketQn2[c(7),c(4)]))), as.numeric(paste(factor(AgebracketQn2[c(7),c(5)]))), as.numeric(paste(factor(AgebracketQn2[c(7),c(6)]))), as.numeric(paste(factor(AgebracketQn2[c(7),c(7)]))), as.numeric(paste(factor(AgebracketQn2[c(7),c(8)]))), as.numeric(paste(factor(AgebracketQn2[c(7),c(9)]))))
mo.rds.con6<-data.frame( ageQn2,moroads.cons6)

no.filled.potholestt6<-c(as.character(paste(factor(AgebracketQn2[c(9),c(1)]))))
no.filledpot6<-c(as.numeric(paste(factor(AgebracketQn2[c(9),c(2)]))),as.numeric(paste(factor(AgebracketQn2[c(9),c(3)]))),as.numeric(paste(factor(AgebracketQn2[c(9),c(4)]))), as.numeric(paste(factor(AgebracketQn2[c(9),c(5)]))), as.numeric(paste(factor(AgebracketQn2[c(9),c(6)]))), as.numeric(paste(factor(AgebracketQn2[c(9),c(7)]))), as.numeric(paste(factor(AgebracketQn2[c(9),c(8)]))), as.numeric(paste(factor(AgebracketQn2[c(9),c(9)]))))
no.filledpots6<-data.frame( ageQn2,no.filledpot6)

imprd.rd.sigtt6<-c(as.character(paste(factor(AgebracketQn2[c(11),c(1)]))))
imprvd.rd.sig6<-c(as.numeric(paste(factor(AgebracketQn2[c(11),c(2)]))),as.numeric(paste(factor(AgebracketQn2[c(11),c(3)]))),as.numeric(paste(factor(AgebracketQn2[c(11),c(4)]))), as.numeric(paste(factor(AgebracketQn2[c(11),c(5)]))), as.numeric(paste(factor(AgebracketQn2[c(11),c(6)]))), as.numeric(paste(factor(AgebracketQn2[c(11),c(7)]))), as.numeric(paste(factor(AgebracketQn2[c(11),c(8)]))), as.numeric(paste(factor(AgebracketQn2[c(11),c(9)]))))
imprd.rd.sigs6<-data.frame( ageQn2,imprvd.rd.sig6)

strict.usage.bodatt6<-c(as.character(paste(factor(AgebracketQn2[c(13),c(1)]))))
strict.rd.usage.boda6<-c(as.numeric(paste(factor(AgebracketQn2[c(13),c(2)]))),as.numeric(paste(factor(AgebracketQn2[c(13),c(3)]))),as.numeric(paste(factor(AgebracketQn2[c(13),c(4)]))), as.numeric(paste(factor(AgebracketQn2[c(13),c(5)]))), as.numeric(paste(factor(AgebracketQn2[c(13),c(6)]))), as.numeric(paste(factor(AgebracketQn2[c(13),c(7)]))), as.numeric(paste(factor(AgebracketQn2[c(13),c(8)]))), as.numeric(paste(factor(AgebracketQn2[c(13),c(9)]))))
stric.usage.bb6<-data.frame( ageQn2,strict.rd.usage.boda6)



################################################################################################################################################################################################
################################
mydata1 <- read.csv("ww/UNRA2.csv", header = FALSE)
question3<-as.character(mydata1[1,1])
#############################regions
regiondt2q2<-mydata1[1:44,1:14]

cats<-c(as.character(paste(factor(regiondt2q2[c(5),c(1)]))),as.character(paste(factor(regiondt2q2[c(10),c(1)]))),as.character(paste(factor(regiondt2q2[c(15),c(1)]))),as.character(paste(factor(regiondt2q2[c(20),c(1)]))),as.character(paste(factor(regiondt2q2[c(25),c(1)]))),as.character(paste(factor(regiondt2q2[c(30),c(1)]))),as.character(paste(factor(regiondt2q2[c(35),c(1)]))),as.character(paste(factor(regiondt2q2[c(40),c(1)]))))

#southern
regiondt2q2soutt<-as.character(paste(factor(regiondt2q2[c(3),c(5)])))
Very.dissatisfiedd2<-c(as.numeric(paste(factor(regiondt2q2[c(5),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(10),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(15),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(20),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(25),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(30),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(35),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(40),c(5)]))))
Dissatisfiedd2<-c(as.numeric(paste(factor(regiondt2q2[c(6),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(11),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(16),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(21),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(26),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(31),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(36),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(41),c(5)]))))
Neithernord2<-c(as.numeric(paste(factor(regiondt2q2[c(7),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(12),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(17),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(22),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(27),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(32),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(37),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(42),c(5)]))))
Satisfiedd2<-c(as.numeric(paste(factor(regiondt2q2[c(8),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(13),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(18),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(23),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(28),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(33),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(43),c(5)]))))
Very.satisfiedd2<-c(as.numeric(paste(factor(regiondt2q2[c(9),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(14),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(19),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(24),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(29),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(34),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(5)]))),as.numeric(paste(factor(regiondt2q2[c(44),c(5)]))))

to_plot <- data.frame(cats,Very.dissatisfiedd2,Dissatisfiedd2,Neithernord2,Satisfiedd2,Very.satisfiedd2)
melted<-melt(to_plot, id="cats")


##Central
regiondt2q2centtt<-as.character(paste(factor(regiondt2q2[c(3),c(7)])))
Very.dissatisfiedd2c<-c(as.numeric(paste(factor(regiondt2q2[c(5),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(10),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(15),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(20),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(25),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(30),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(35),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(40),c(7)]))))
Dissatisfiedd2c<-c(as.numeric(paste(factor(regiondt2q2[c(6),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(11),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(16),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(21),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(26),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(31),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(36),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(41),c(7)]))))
Neithernord2c<-c(as.numeric(paste(factor(regiondt2q2[c(7),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(12),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(17),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(22),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(27),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(32),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(37),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(42),c(7)]))))
Satisfiedd2c<-c(as.numeric(paste(factor(regiondt2q2[c(8),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(13),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(18),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(23),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(28),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(33),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(43),c(7)]))))
Very.satisfiedd2c<-c(as.numeric(paste(factor(regiondt2q2[c(9),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(14),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(19),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(24),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(29),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(34),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(7)]))),as.numeric(paste(factor(regiondt2q2[c(44),c(7)]))))

to_plotc <- data.frame(cats,Very.dissatisfiedd2c,Dissatisfiedd2c,Neithernord2c,Satisfiedd2c,Very.satisfiedd2c)
meltedc<-melt(to_plotc, id="cats")

##East
regiondt2q2easttt<-as.character(paste(factor(regiondt2q2[c(3),c(9)])))
Very.dissatisfiedd2e<-c(as.numeric(paste(factor(regiondt2q2[c(5),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(10),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(15),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(20),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(25),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(30),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(35),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(40),c(9)]))))
Dissatisfiedd2e<-c(as.numeric(paste(factor(regiondt2q2[c(6),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(11),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(16),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(21),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(26),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(31),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(36),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(41),c(9)]))))
Neithernord2e<-c(as.numeric(paste(factor(regiondt2q2[c(7),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(12),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(17),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(22),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(27),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(32),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(37),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(42),c(9)]))))
Satisfiedd2e<-c(as.numeric(paste(factor(regiondt2q2[c(8),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(13),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(18),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(23),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(28),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(33),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(43),c(9)]))))
Very.satisfiedd2e<-c(as.numeric(paste(factor(regiondt2q2[c(9),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(14),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(19),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(24),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(29),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(34),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(9)]))),as.numeric(paste(factor(regiondt2q2[c(44),c(9)]))))

to_plote <- data.frame(cats,Very.dissatisfiedd2e,Dissatisfiedd2e,Neithernord2e,Satisfiedd2e,Very.satisfiedd2e)
meltede<-melt(to_plote, id="cats")

##Northern
regiondt2q2northtt<-as.character(paste(factor(regiondt2q2[c(3),c(11)])))
Very.dissatisfiedd2n<-c(as.numeric(paste(factor(regiondt2q2[c(5),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(10),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(15),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(20),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(25),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(30),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(35),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(40),c(11)]))))
Dissatisfiedd2n<-c(as.numeric(paste(factor(regiondt2q2[c(6),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(11),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(16),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(21),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(26),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(31),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(36),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(41),c(11)]))))
Neithernord2n<-c(as.numeric(paste(factor(regiondt2q2[c(7),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(12),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(17),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(22),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(27),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(32),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(37),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(42),c(11)]))))
Satisfiedd2n<-c(as.numeric(paste(factor(regiondt2q2[c(8),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(13),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(18),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(23),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(28),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(33),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(43),c(11)]))))
Very.satisfiedd2n<-c(as.numeric(paste(factor(regiondt2q2[c(9),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(14),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(19),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(24),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(29),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(34),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(11)]))),as.numeric(paste(factor(regiondt2q2[c(44),c(11)]))))

to_plotn <- data.frame(cats,Very.dissatisfiedd2n,Dissatisfiedd2n,Neithernord2n,Satisfiedd2n,Very.satisfiedd2n)
meltedn<-melt(to_plotn, id="cats")


##western
regiondt2q2westtt<-as.character(paste(factor(regiondt2q2[c(3),c(13)])))
Very.dissatisfiedd2w<-c(as.numeric(paste(factor(regiondt2q2[c(5),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(10),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(15),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(20),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(25),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(30),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(35),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(40),c(13)]))))
Dissatisfiedd2w<-c(as.numeric(paste(factor(regiondt2q2[c(6),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(11),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(16),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(21),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(26),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(31),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(36),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(41),c(13)]))))
Neithernord2w<-c(as.numeric(paste(factor(regiondt2q2[c(7),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(12),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(17),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(22),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(27),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(32),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(37),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(42),c(13)]))))
Satisfiedd2w<-c(as.numeric(paste(factor(regiondt2q2[c(8),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(13),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(18),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(23),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(28),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(33),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(43),c(13)]))))
Very.satisfiedd2w<-c(as.numeric(paste(factor(regiondt2q2[c(9),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(14),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(19),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(24),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(29),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(34),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(38),c(13)]))),as.numeric(paste(factor(regiondt2q2[c(44),c(13)]))))

to_plotw <- data.frame(cats,Very.dissatisfiedd2w,Dissatisfiedd2w,Neithernord2w,Satisfiedd2w,Very.satisfiedd2w)
meltedw<-melt(to_plotw, id="cats")



#############################districts
districtdt2q2<-mydata1[1:44,c(1,2,15:76)]

#southern
districtdt2q2souttm<-as.character(paste(factor(districtdt2q2[c(3),c(5)])))
Very.dissatisfiedd2m<-c(as.numeric(paste(factor(districtdt2q2[c(5),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(10),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(15),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(20),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(25),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(30),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(35),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(40),c(5)]))))
Dissatisfiedd2m<-c(as.numeric(paste(factor(districtdt2q2[c(6),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(11),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(16),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(21),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(26),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(31),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(36),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(41),c(5)]))))
Neithernord2m<-c(as.numeric(paste(factor(districtdt2q2[c(7),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(12),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(17),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(22),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(27),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(32),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(37),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(42),c(5)]))))
Satisfiedd2m<-c(as.numeric(paste(factor(districtdt2q2[c(8),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(13),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(18),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(23),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(28),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(33),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(38),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(43),c(5)]))))
Very.satisfiedd2m<-c(as.numeric(paste(factor(districtdt2q2[c(9),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(14),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(19),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(24),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(29),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(34),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(38),c(5)]))),as.numeric(paste(factor(districtdt2q2[c(44),c(5)]))))

to_plotm <- data.frame(cats,Very.dissatisfiedd2m,Dissatisfiedd2m,Neithernord2m,Satisfiedd2m,Very.satisfiedd2m)
meltedm<-melt(to_plotm, id="cats")

##Central
districtdt2q2centtts<-as.character(paste(factor(districtdt2q2[c(3),c(7)])))
Very.dissatisfiedd2cs<-c(as.numeric(paste(factor(districtdt2q2[c(5),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(10),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(15),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(20),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(25),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(30),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(35),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(40),c(7)]))))
Dissatisfiedd2cs<-c(as.numeric(paste(factor(districtdt2q2[c(6),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(11),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(16),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(21),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(26),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(31),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(36),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(41),c(7)]))))
Neithernord2cs<-c(as.numeric(paste(factor(districtdt2q2[c(7),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(12),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(17),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(22),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(27),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(32),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(37),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(42),c(7)]))))
Satisfiedd2cs<-c(as.numeric(paste(factor(districtdt2q2[c(8),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(13),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(18),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(23),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(28),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(33),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(38),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(43),c(7)]))))
Very.satisfiedd2cs<-c(as.numeric(paste(factor(districtdt2q2[c(9),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(14),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(19),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(24),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(29),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(34),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(38),c(7)]))),as.numeric(paste(factor(districtdt2q2[c(44),c(7)]))))

to_plotcs <- data.frame(cats,Very.dissatisfiedd2cs,Dissatisfiedd2cs,Neithernord2cs,Satisfiedd2cs,Very.satisfiedd2cs)
meltedcs<-melt(to_plotcs, id="cats")

#############################road types
roadtpt2q2<-mydata1[1:44,c(1,2,77:79)]

#unpaved
paved2q2souttp<-as.character(paste(factor(roadtpt2q2[c(3),c(5)])))
Very.dissatisfiedd2p<-c(as.numeric(paste(factor(roadtpt2q2[c(5),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(10),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(15),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(20),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(25),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(30),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(35),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(40),c(5)]))))
Dissatisfiedd2p<-c(as.numeric(paste(factor(roadtpt2q2[c(6),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(11),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(16),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(21),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(26),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(31),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(36),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(41),c(5)]))))
Neithernord2p<-c(as.numeric(paste(factor(roadtpt2q2[c(7),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(12),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(17),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(22),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(27),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(32),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(37),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(42),c(5)]))))
Satisfiedd2p<-c(as.numeric(paste(factor(roadtpt2q2[c(8),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(13),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(18),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(23),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(28),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(33),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(38),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(43),c(5)]))))
Very.satisfiedd2p<-c(as.numeric(paste(factor(roadtpt2q2[c(9),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(14),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(19),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(24),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(29),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(34),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(38),c(5)]))),as.numeric(paste(factor(roadtpt2q2[c(44),c(5)]))))

to_plotp <- data.frame(cats,Very.dissatisfiedd2p,Dissatisfiedd2p,Neithernord2p,Satisfiedd2p,Very.satisfiedd2p)
meltedp<-melt(to_plotp, id="cats")

##paved
unpavedqcentttu<-as.character(paste(factor(roadtpt2q2[c(3),c(3)])))
Very.dissatisfiedd2cu<-c(as.numeric(paste(factor(roadtpt2q2[c(5),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(10),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(15),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(20),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(25),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(30),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(35),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(40),c(3)]))))
Dissatisfiedd2cu<-c(as.numeric(paste(factor(roadtpt2q2[c(6),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(11),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(16),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(21),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(26),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(31),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(36),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(41),c(3)]))))
Neithernord2cu<-c(as.numeric(paste(factor(roadtpt2q2[c(7),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(12),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(17),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(22),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(27),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(32),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(37),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(42),c(3)]))))
Satisfiedd2cu<-c(as.numeric(paste(factor(roadtpt2q2[c(8),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(13),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(18),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(23),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(28),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(33),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(38),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(43),c(3)]))))
Very.satisfiedd2cu<-c(as.numeric(paste(factor(roadtpt2q2[c(9),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(14),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(19),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(24),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(29),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(34),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(38),c(3)]))),as.numeric(paste(factor(roadtpt2q2[c(44),c(3)]))))

to_plotcu <- data.frame(cats,Very.dissatisfiedd2cu,Dissatisfiedd2cu,Neithernord2cu,Satisfiedd2cu,Very.satisfiedd2cu)
meltedcu<-melt(to_plotcu, id="cats")

#############################respondent types
restpt2q2<-mydata1[1:44,c(1,2,81:84)]

#female
female2q2souttfe<-as.character(paste(factor(restpt2q2[c(3),c(5)])))
Very.dissatisfiedd2fe<-c(as.numeric(paste(factor(restpt2q2[c(5),c(5)]))),as.numeric(paste(factor(restpt2q2[c(10),c(5)]))),as.numeric(paste(factor(restpt2q2[c(15),c(5)]))),as.numeric(paste(factor(restpt2q2[c(20),c(5)]))),as.numeric(paste(factor(restpt2q2[c(25),c(5)]))),as.numeric(paste(factor(restpt2q2[c(30),c(5)]))),as.numeric(paste(factor(restpt2q2[c(35),c(5)]))),as.numeric(paste(factor(restpt2q2[c(40),c(5)]))))
Dissatisfiedd2fe<-c(as.numeric(paste(factor(restpt2q2[c(6),c(5)]))),as.numeric(paste(factor(restpt2q2[c(11),c(5)]))),as.numeric(paste(factor(restpt2q2[c(16),c(5)]))),as.numeric(paste(factor(restpt2q2[c(21),c(5)]))),as.numeric(paste(factor(restpt2q2[c(26),c(5)]))),as.numeric(paste(factor(restpt2q2[c(31),c(5)]))),as.numeric(paste(factor(restpt2q2[c(36),c(5)]))),as.numeric(paste(factor(restpt2q2[c(41),c(5)]))))
Neithernord2fe<-c(as.numeric(paste(factor(restpt2q2[c(7),c(5)]))),as.numeric(paste(factor(restpt2q2[c(12),c(5)]))),as.numeric(paste(factor(restpt2q2[c(17),c(5)]))),as.numeric(paste(factor(restpt2q2[c(22),c(5)]))),as.numeric(paste(factor(restpt2q2[c(27),c(5)]))),as.numeric(paste(factor(restpt2q2[c(32),c(5)]))),as.numeric(paste(factor(restpt2q2[c(37),c(5)]))),as.numeric(paste(factor(restpt2q2[c(42),c(5)]))))
Satisfiedd2fe<-c(as.numeric(paste(factor(restpt2q2[c(8),c(5)]))),as.numeric(paste(factor(restpt2q2[c(13),c(5)]))),as.numeric(paste(factor(restpt2q2[c(18),c(5)]))),as.numeric(paste(factor(restpt2q2[c(23),c(5)]))),as.numeric(paste(factor(restpt2q2[c(28),c(5)]))),as.numeric(paste(factor(restpt2q2[c(33),c(5)]))),as.numeric(paste(factor(restpt2q2[c(38),c(5)]))),as.numeric(paste(factor(restpt2q2[c(43),c(5)]))))
Very.satisfiedd2fe<-c(as.numeric(paste(factor(restpt2q2[c(9),c(5)]))),as.numeric(paste(factor(restpt2q2[c(14),c(5)]))),as.numeric(paste(factor(restpt2q2[c(19),c(5)]))),as.numeric(paste(factor(restpt2q2[c(24),c(5)]))),as.numeric(paste(factor(restpt2q2[c(29),c(5)]))),as.numeric(paste(factor(restpt2q2[c(34),c(5)]))),as.numeric(paste(factor(restpt2q2[c(38),c(5)]))),as.numeric(paste(factor(restpt2q2[c(44),c(5)]))))

to_plotfe <- data.frame(cats,Very.dissatisfiedd2fe,Dissatisfiedd2fe,Neithernord2fe,Satisfiedd2fe,Very.satisfiedd2fe)
meltedfe<-melt(to_plotfe, id="cats")

##male
maleqcentttma<-as.character(paste(factor(restpt2q2[c(3),c(3)])))
Very.dissatisfiedd2cma<-c(as.numeric(paste(factor(restpt2q2[c(5),c(3)]))),as.numeric(paste(factor(restpt2q2[c(10),c(3)]))),as.numeric(paste(factor(restpt2q2[c(15),c(3)]))),as.numeric(paste(factor(restpt2q2[c(20),c(3)]))),as.numeric(paste(factor(restpt2q2[c(25),c(3)]))),as.numeric(paste(factor(restpt2q2[c(30),c(3)]))),as.numeric(paste(factor(restpt2q2[c(35),c(3)]))),as.numeric(paste(factor(restpt2q2[c(40),c(3)]))))
Dissatisfiedd2cma<-c(as.numeric(paste(factor(restpt2q2[c(6),c(3)]))),as.numeric(paste(factor(restpt2q2[c(11),c(3)]))),as.numeric(paste(factor(restpt2q2[c(16),c(3)]))),as.numeric(paste(factor(restpt2q2[c(21),c(3)]))),as.numeric(paste(factor(restpt2q2[c(26),c(3)]))),as.numeric(paste(factor(restpt2q2[c(31),c(3)]))),as.numeric(paste(factor(restpt2q2[c(36),c(3)]))),as.numeric(paste(factor(restpt2q2[c(41),c(3)]))))
Neithernord2cma<-c(as.numeric(paste(factor(restpt2q2[c(7),c(3)]))),as.numeric(paste(factor(restpt2q2[c(12),c(3)]))),as.numeric(paste(factor(restpt2q2[c(17),c(3)]))),as.numeric(paste(factor(restpt2q2[c(22),c(3)]))),as.numeric(paste(factor(restpt2q2[c(27),c(3)]))),as.numeric(paste(factor(restpt2q2[c(32),c(3)]))),as.numeric(paste(factor(restpt2q2[c(37),c(3)]))),as.numeric(paste(factor(restpt2q2[c(42),c(3)]))))
Satisfiedd2cma<-c(as.numeric(paste(factor(restpt2q2[c(8),c(3)]))),as.numeric(paste(factor(restpt2q2[c(13),c(3)]))),as.numeric(paste(factor(restpt2q2[c(18),c(3)]))),as.numeric(paste(factor(restpt2q2[c(23),c(3)]))),as.numeric(paste(factor(restpt2q2[c(28),c(3)]))),as.numeric(paste(factor(restpt2q2[c(33),c(3)]))),as.numeric(paste(factor(restpt2q2[c(38),c(3)]))),as.numeric(paste(factor(restpt2q2[c(43),c(3)]))))
Very.satisfiedd2cma<-c(as.numeric(paste(factor(restpt2q2[c(9),c(3)]))),as.numeric(paste(factor(restpt2q2[c(14),c(3)]))),as.numeric(paste(factor(restpt2q2[c(19),c(3)]))),as.numeric(paste(factor(restpt2q2[c(24),c(3)]))),as.numeric(paste(factor(restpt2q2[c(29),c(3)]))),as.numeric(paste(factor(restpt2q2[c(34),c(3)]))),as.numeric(paste(factor(restpt2q2[c(38),c(3)]))),as.numeric(paste(factor(restpt2q2[c(44),c(3)]))))

to_plotcma <- data.frame(cats,Very.dissatisfiedd2cma,Dissatisfiedd2cma,Neithernord2cma,Satisfiedd2cma,Very.satisfiedd2cma)
meltedma<-melt(to_plotcma, id="cats")




#############################road name
roadnamext2q2<-mydata1[1:44,c(1,2,85:226)]

#female
roadn2q2souttrd<-as.character(paste(factor(roadnamext2q2[c(3),c(5)])))
Very.dissatisfiedd2rd<-c(as.numeric(paste(factor(roadnamext2q2[c(5),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(10),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(15),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(20),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(25),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(30),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(35),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(40),c(5)]))))
Dissatisfiedd2rd<-c(as.numeric(paste(factor(roadnamext2q2[c(6),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(11),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(16),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(21),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(26),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(31),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(36),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(41),c(5)]))))
Neithernord2rd<-c(as.numeric(paste(factor(roadnamext2q2[c(7),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(12),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(17),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(22),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(27),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(32),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(37),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(42),c(5)]))))
Satisfiedd2rd<-c(as.numeric(paste(factor(roadnamext2q2[c(8),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(13),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(18),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(23),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(28),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(33),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(38),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(43),c(5)]))))
Very.satisfiedd2rd<-c(as.numeric(paste(factor(roadnamext2q2[c(9),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(14),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(19),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(24),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(29),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(34),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(38),c(5)]))),as.numeric(paste(factor(roadnamext2q2[c(44),c(5)]))))

to_plotrd <- data.frame(cats,Very.dissatisfiedd2rd,Dissatisfiedd2rd,Neithernord2rd,Satisfiedd2rd,Very.satisfiedd2rd)
meltedrd<-melt(to_plotrd, id="cats")

##male
roadnqcentttrd1<-as.character(paste(factor(roadnamext2q2[c(3),c(3)])))
Very.dissatisfiedd2crd1<-c(as.numeric(paste(factor(roadnamext2q2[c(5),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(10),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(15),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(20),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(25),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(30),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(35),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(40),c(3)]))))
Dissatisfiedd2crd1<-c(as.numeric(paste(factor(roadnamext2q2[c(6),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(11),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(16),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(21),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(26),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(31),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(36),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(41),c(3)]))))
Neithernord2crd1<-c(as.numeric(paste(factor(roadnamext2q2[c(7),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(12),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(17),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(22),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(27),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(32),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(37),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(42),c(3)]))))
Satisfiedd2crd1<-c(as.numeric(paste(factor(roadnamext2q2[c(8),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(13),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(18),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(23),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(28),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(33),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(38),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(43),c(3)]))))
Very.satisfiedd2crd1<-c(as.numeric(paste(factor(roadnamext2q2[c(9),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(14),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(19),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(24),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(29),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(34),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(38),c(3)]))),as.numeric(paste(factor(roadnamext2q2[c(44),c(3)]))))

to_plotcrd1 <- data.frame(cats,Very.dissatisfiedd2crd1,Dissatisfiedd2crd1,Neithernord2crd1,Satisfiedd2crd1,Very.satisfiedd2crd1)
meltedcrd1<-melt(to_plotcrd1, id="cats")

########################################repondent type
respdtxt2q2<-mydata1[1:44,c(1,2,205:216)]

#one
respduttrdt<-as.character(paste(factor(respdtxt2q2[c(3),c(5)])))
Very.dissatisfiedd2rdt<-c(as.numeric(paste(factor(respdtxt2q2[c(5),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(10),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(15),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(20),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(25),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(30),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(35),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(40),c(5)]))))
Dissatisfiedd2rdt<-c(as.numeric(paste(factor(respdtxt2q2[c(6),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(11),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(16),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(21),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(26),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(31),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(36),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(41),c(5)]))))
Neithernord2rdt<-c(as.numeric(paste(factor(respdtxt2q2[c(7),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(12),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(17),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(22),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(27),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(32),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(37),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(42),c(5)]))))
Satisfiedd2rdt<-c(as.numeric(paste(factor(respdtxt2q2[c(8),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(13),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(18),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(23),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(28),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(33),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(38),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(43),c(5)]))))
Very.satisfiedd2rdt<-c(as.numeric(paste(factor(respdtxt2q2[c(9),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(14),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(19),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(24),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(29),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(34),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(38),c(5)]))),as.numeric(paste(factor(respdtxt2q2[c(44),c(5)]))))

to_plotrdt <- data.frame(cats,Very.dissatisfiedd2rdt,Dissatisfiedd2rdt,Neithernord2rdt,Satisfiedd2rdt,Very.satisfiedd2rdt)
meltedrdt<-melt(to_plotrdt, id="cats")

##two
respduttrdt1<-as.character(paste(factor(respdtxt2q2[c(3),c(3)])))
Very.dissatisfiedd2crdt1<-c(as.numeric(paste(factor(respdtxt2q2[c(5),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(10),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(15),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(20),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(25),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(30),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(35),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(40),c(3)]))))
Dissatisfiedd2crdt1<-c(as.numeric(paste(factor(respdtxt2q2[c(6),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(11),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(16),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(21),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(26),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(31),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(36),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(41),c(3)]))))
Neithernord2crdt1<-c(as.numeric(paste(factor(respdtxt2q2[c(7),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(12),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(17),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(22),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(27),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(32),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(37),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(42),c(3)]))))
Satisfiedd2crdt1<-c(as.numeric(paste(factor(respdtxt2q2[c(8),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(13),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(18),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(23),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(28),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(33),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(38),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(43),c(3)]))))
Very.satisfiedd2crdt1<-c(as.numeric(paste(factor(respdtxt2q2[c(9),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(14),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(19),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(24),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(29),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(34),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(38),c(3)]))),as.numeric(paste(factor(respdtxt2q2[c(44),c(3)]))))

to_plotcrdt1 <- data.frame(cats,Very.dissatisfiedd2crdt1,Dissatisfiedd2crdt1,Neithernord2crdt1,Satisfiedd2crdt1,Very.satisfiedd2crdt1)
meltedcrdt1<-melt(to_plotcrdt1, id="cats")


########################################age bracket
agebtxt2q2<-mydata1[1:44,c(1,2,217:231)]

#one
ageuttrdag<-as.character(paste(factor(agebtxt2q2[c(3),c(5)])))
Very.dissatisfiedd2rdag<-c(as.numeric(paste(factor(agebtxt2q2[c(5),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(10),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(15),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(20),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(25),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(30),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(35),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(40),c(5)]))))
Dissatisfiedd2rdag<-c(as.numeric(paste(factor(agebtxt2q2[c(6),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(11),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(16),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(21),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(26),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(31),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(36),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(41),c(5)]))))
Neithernord2rdag<-c(as.numeric(paste(factor(agebtxt2q2[c(7),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(12),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(17),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(22),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(27),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(32),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(37),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(42),c(5)]))))
Satisfiedd2rdag<-c(as.numeric(paste(factor(agebtxt2q2[c(8),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(13),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(18),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(23),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(28),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(33),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(38),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(43),c(5)]))))
Very.satisfiedd2rdag<-c(as.numeric(paste(factor(agebtxt2q2[c(9),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(14),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(19),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(24),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(29),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(34),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(38),c(5)]))),as.numeric(paste(factor(agebtxt2q2[c(44),c(5)]))))

to_plotrdag <- data.frame(cats,Very.dissatisfiedd2rdag,Dissatisfiedd2rdag,Neithernord2rdag,Satisfiedd2rdag,Very.satisfiedd2rdag)
meltedrdag<-melt(to_plotrdag, id="cats")

##two
agettrdag1<-as.character(paste(factor(agebtxt2q2[c(3),c(3)])))
Very.dissatisfiedd2crdag1<-c(as.numeric(paste(factor(agebtxt2q2[c(5),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(10),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(15),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(20),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(25),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(30),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(35),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(40),c(3)]))))
Dissatisfiedd2crdag1<-c(as.numeric(paste(factor(agebtxt2q2[c(6),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(11),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(16),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(21),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(26),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(31),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(36),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(41),c(3)]))))
Neithernord2crdag1<-c(as.numeric(paste(factor(agebtxt2q2[c(7),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(12),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(17),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(22),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(27),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(32),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(37),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(42),c(3)]))))
Satisfiedd2crdag1<-c(as.numeric(paste(factor(agebtxt2q2[c(8),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(13),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(18),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(23),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(28),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(33),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(38),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(43),c(3)]))))
Very.satisfiedd2crdag1<-c(as.numeric(paste(factor(agebtxt2q2[c(9),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(14),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(19),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(24),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(29),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(34),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(38),c(3)]))),as.numeric(paste(factor(agebtxt2q2[c(44),c(3)]))))

to_plotcrdag1 <- data.frame(cats,Very.dissatisfiedd2crdag1,Dissatisfiedd2crdag1,Neithernord2crdag1,Satisfiedd2crdag1,Very.satisfiedd2crdag1)
meltedcrdag1<-melt(to_plotcrdag1, id="cats")


################################################################################################################################################################################################
################################
mydata2 <- read.csv("ww/UNRA3.csv", header = FALSE)
question4<-as.character(mydata2[1,1])
question5<-as.character(mydata2[c(17),c(1)])
u1qnsd3<-c(question4,question5)

################################################################################################################################################################################################
################################UNRA3..QUESTION 4......region
regiond3<-mydata2[2:15,2:9]

soutthttd3<-factor(regiond3[c(2),c(4)])
southernd3<-as.numeric(paste(factor( regiond3[c(5,7,9,11,13),c(4)])))
var2d3<-as.character(paste(factor( regiond3[c(5,7,9,11,13),c(1)])))
southd3<-data.frame(var2d3,southernd3)

centttd3<-factor(regiond3[c(2),c(5)])
centrald3<-as.numeric(paste(factor( regiond3[c(5,7,9,11,13),c(5)])))
centrd3<-data.frame(var2d3,centrald3)

easttd3<-factor(regiond3[c(2),c(6)])
easternd3<-as.numeric(paste(factor( regiond3[c(5,7,9,11,13),c(6)])))
eastd3<-data.frame(var2d3,easternd3)

northttd3<-factor(regiond3[c(2),c(7)])
northernd3<-as.numeric(paste(factor( regiond3[c(5,7,9,11,13),c(7)])))
northd3<-data.frame(var2d3,northernd3)

westttd3<-factor(regiond3[c(2),c(8)])
westernd3<-as.numeric(paste(factor( regiond3[c(5,7,9,11,13),c(8)])))
westd3<-data.frame(var2d3,westernd3)


################################districts
districtsd3<-mydata2[c(2:15),c(2,10:40)]

discsd3<-c(as.character(paste(factor(districtsd3[c(2),c(2)]))),as.character(paste(factor(districtsd3[c(2),c(3)]))),as.character(paste(factor(districtsd3[c(2),c(4)]))), as.character(paste(factor(districtsd3[c(2),c(5)]))), as.character(paste(factor(districtsd3[c(2),c(6)]))), as.character(paste(factor(districtsd3[c(2),c(7)]))), as.character(paste(factor(districtsd3[c(2),c(8)]))), as.character(paste(factor(districtsd3[c(2),c(9)]))),as.character(paste(factor(districtsd3[c(2),c(10)]))),as.character(paste(factor(districtsd3[c(2),c(11)]))),as.character(paste(factor(districtsd3[c(2),c(12)]))), as.character(paste(factor(districtsd3[c(2),c(13)]))),as.character(paste(factor(districtsd3[c(2),c(14)]))),as.character(paste(factor(districtsd3[c(2),c(15)]))),as.character(paste(factor(districtsd3[c(2),c(16)]))),
           as.character(paste(factor(districtsd3[c(2),c(17)]))),as.character(paste(factor(districtsd3[c(2),c(18)]))),as.character(paste(factor(districtsd3[c(2),c(19)]))),as.character(paste(factor(districtsd3[c(2),c(20)]))),as.character(paste(factor(districtsd3[c(2),c(21)]))),as.character(paste(factor(districtsd3[c(2),c(22)]))),as.character(paste(factor(districtsd3[c(2),c(23)]))),as.character(paste(factor(districtsd3[c(2),c(24)]))),as.character(paste(factor(districtsd3[c(2),c(25)]))),as.character(paste(factor(districtsd3[c(2),c(26)]))),as.character(paste(factor(districtsd3[c(2),c(27)]))),as.character(paste(factor(districtsd3[c(2),c(28)]))),as.character(paste(factor(districtsd3[c(2),c(29)]))),as.character(paste(factor(districtsd3[c(2),c(30)]))),as.character(paste(factor(districtsd3[c(2),c(31)]))),
           as.character(paste(factor(districtsd3[c(2),c(32)]))),as.character(paste(factor(districtsd3[c(2),c(33)]))),as.character(paste(factor(districtsd3[c(2),c(34)]))),as.character(paste(factor(districtsd3[c(2),c(35)]))),as.character(paste(factor(districtsd3[c(2),c(36)]))),as.character(paste(factor(districtsd3[c(2),c(37)]))),as.character(paste(factor(districtsd3[c(2),c(38)]))),as.character(paste(factor(districtsd3[c(2),c(39)]))),as.character(paste(factor(districtsd3[c(2),c(40)]))),as.character(paste(factor(districtsd3[c(2),c(41)]))),as.character(paste(factor(districtsd3[c(2),c(42)]))),as.character(paste(factor(districtsd3[c(2),c(43)]))),as.character(paste(factor(districtsd3[c(2),c(44)]))),as.character(paste(factor(districtsd3[c(2),c(45)]))))


very.disttd3<-c(as.character(paste(factor(districtsd3[c(5),c(1)]))))
very.disd3<-c(as.numeric(paste(factor(districtsd3[c(5),c(2)]))),as.numeric(paste(factor(districtsd3[c(5),c(3)]))),as.numeric(paste(factor(districtsd3[c(5),c(4)]))), as.numeric(paste(factor(districtsd3[c(5),c(5)]))), as.numeric(paste(factor(districtsd3[c(5),c(6)]))), as.numeric(paste(factor(districtsd3[c(5),c(7)]))), as.numeric(paste(factor(districtsd3[c(5),c(8)]))), as.numeric(paste(factor(districtsd3[c(5),c(9)]))),as.numeric(paste(factor(districtsd3[c(5),c(10)]))),as.numeric(paste(factor(districtsd3[c(5),c(11)]))),as.numeric(paste(factor(districtsd3[c(5),c(12)]))), as.numeric(paste(factor(districtsd3[c(5),c(13)]))),as.numeric(paste(factor(districtsd3[c(5),c(14)]))),as.numeric(paste(factor(districtsd3[c(5),c(15)]))),as.numeric(paste(factor(districtsd3[c(5),c(16)]))),
              as.numeric(paste(factor(districtsd3[c(5),c(17)]))),as.numeric(paste(factor(districtsd3[c(5),c(18)]))),as.numeric(paste(factor(districtsd3[c(5),c(19)]))),as.numeric(paste(factor(districtsd3[c(5),c(20)]))),as.numeric(paste(factor(districtsd3[c(5),c(21)]))),as.numeric(paste(factor(districtsd3[c(5),c(22)]))),as.numeric(paste(factor(districtsd3[c(5),c(23)]))),as.numeric(paste(factor(districtsd3[c(5),c(24)]))),as.numeric(paste(factor(districtsd3[c(5),c(25)]))),as.numeric(paste(factor(districtsd3[c(5),c(26)]))),as.numeric(paste(factor(districtsd3[c(5),c(27)]))),as.numeric(paste(factor(districtsd3[c(5),c(28)]))),as.numeric(paste(factor(districtsd3[c(5),c(29)]))),as.numeric(paste(factor(districtsd3[c(5),c(30)]))),as.numeric(paste(factor(districtsd3[c(5),c(31)]))),
              as.numeric(paste(factor(districtsd3[c(5),c(32)]))),as.numeric(paste(factor(districtsd3[c(5),c(33)]))),as.numeric(paste(factor(districtsd3[c(5),c(34)]))),as.numeric(paste(factor(districtsd3[c(5),c(35)]))),as.numeric(paste(factor(districtsd3[c(5),c(36)]))),as.numeric(paste(factor(districtsd3[c(5),c(37)]))),as.numeric(paste(factor(districtsd3[c(5),c(38)]))),as.numeric(paste(factor(districtsd3[c(5),c(39)]))),as.numeric(paste(factor(districtsd3[c(5),c(40)]))),as.numeric(paste(factor(districtsd3[c(5),c(41)]))),as.numeric(paste(factor(districtsd3[c(5),c(42)]))),as.numeric(paste(factor(districtsd3[c(5),c(43)]))),as.numeric(paste(factor(districtsd3[c(5),c(44)]))),as.numeric(paste(factor(districtsd3[c(5),c(45)]))))
df1d3<-data.frame(discsd3,very.disd3)


disttd3<-c(as.character(paste(factor(districtsd3[c(7),c(1)]))))
disd3<-c(as.numeric(paste(factor(districtsd3[c(7),c(2)]))),as.numeric(paste(factor(districtsd3[c(7),c(3)]))),as.numeric(paste(factor(districtsd3[c(7),c(4)]))), as.numeric(paste(factor(districtsd3[c(7),c(5)]))), as.numeric(paste(factor(districtsd3[c(7),c(6)]))), as.numeric(paste(factor(districtsd3[c(7),c(7)]))), as.numeric(paste(factor(districtsd3[c(7),c(8)]))), as.numeric(paste(factor(districtsd3[c(7),c(9)]))),as.numeric(paste(factor(districtsd3[c(7),c(10)]))),as.numeric(paste(factor(districtsd3[c(7),c(11)]))),as.numeric(paste(factor(districtsd3[c(7),c(12)]))), as.numeric(paste(factor(districtsd3[c(7),c(13)]))),as.numeric(paste(factor(districtsd3[c(7),c(14)]))),as.numeric(paste(factor(districtsd3[c(7),c(15)]))),as.numeric(paste(factor(districtsd3[c(7),c(16)]))),
         as.numeric(paste(factor(districtsd3[c(7),c(17)]))),as.numeric(paste(factor(districtsd3[c(7),c(18)]))),as.numeric(paste(factor(districtsd3[c(7),c(19)]))),as.numeric(paste(factor(districtsd3[c(7),c(20)]))),as.numeric(paste(factor(districtsd3[c(7),c(21)]))),as.numeric(paste(factor(districtsd3[c(7),c(22)]))),as.numeric(paste(factor(districtsd3[c(7),c(23)]))),as.numeric(paste(factor(districtsd3[c(7),c(24)]))),as.numeric(paste(factor(districtsd3[c(7),c(25)]))),as.numeric(paste(factor(districtsd3[c(7),c(26)]))),as.numeric(paste(factor(districtsd3[c(7),c(27)]))),as.numeric(paste(factor(districtsd3[c(7),c(28)]))),as.numeric(paste(factor(districtsd3[c(7),c(29)]))),as.numeric(paste(factor(districtsd3[c(7),c(30)]))),as.numeric(paste(factor(districtsd3[c(7),c(31)]))),
         as.numeric(paste(factor(districtsd3[c(7),c(32)]))),as.numeric(paste(factor(districtsd3[c(7),c(33)]))),as.numeric(paste(factor(districtsd3[c(7),c(34)]))),as.numeric(paste(factor(districtsd3[c(7),c(35)]))),as.numeric(paste(factor(districtsd3[c(7),c(36)]))),as.numeric(paste(factor(districtsd3[c(7),c(37)]))),as.numeric(paste(factor(districtsd3[c(7),c(38)]))),as.numeric(paste(factor(districtsd3[c(7),c(39)]))),as.numeric(paste(factor(districtsd3[c(7),c(40)]))),as.numeric(paste(factor(districtsd3[c(7),c(41)]))),as.numeric(paste(factor(districtsd3[c(7),c(42)]))),as.numeric(paste(factor(districtsd3[c(7),c(43)]))),as.numeric(paste(factor(districtsd3[c(7),c(44)]))),as.numeric(paste(factor(districtsd3[c(7),c(45)]))))
df2d3<-data.frame(discsd3,disd3)

neiSaNorDisttd3<-c(as.character(paste(factor(districtsd3[c(9),c(1)]))))
neiSatNorDisd3 <-c(as.numeric(paste(factor(districtsd3[c(9),c(2)]))),as.numeric(paste(factor(districtsd3[c(9),c(3)]))),as.numeric(paste(factor(districtsd3[c(9),c(4)]))), as.numeric(paste(factor(districtsd3[c(9),c(5)]))), as.numeric(paste(factor(districtsd3[c(9),c(6)]))), as.numeric(paste(factor(districtsd3[c(9),c(7)]))), as.numeric(paste(factor(districtsd3[c(9),c(8)]))), as.numeric(paste(factor(districtsd3[c(9),c(9)]))),as.numeric(paste(factor(districtsd3[c(9),c(10)]))),as.numeric(paste(factor(districtsd3[c(9),c(11)]))),as.numeric(paste(factor(districtsd3[c(9),c(12)]))), as.numeric(paste(factor(districtsd3[c(9),c(13)]))),as.numeric(paste(factor(districtsd3[c(9),c(14)]))),as.numeric(paste(factor(districtsd3[c(9),c(15)]))),as.numeric(paste(factor(districtsd3[c(9),c(16)]))),
                   as.numeric(paste(factor(districtsd3[c(9),c(17)]))),as.numeric(paste(factor(districtsd3[c(9),c(18)]))),as.numeric(paste(factor(districtsd3[c(9),c(19)]))),as.numeric(paste(factor(districtsd3[c(9),c(20)]))),as.numeric(paste(factor(districtsd3[c(9),c(21)]))),as.numeric(paste(factor(districtsd3[c(9),c(22)]))),as.numeric(paste(factor(districtsd3[c(9),c(23)]))),as.numeric(paste(factor(districtsd3[c(9),c(24)]))),as.numeric(paste(factor(districtsd3[c(9),c(25)]))),as.numeric(paste(factor(districtsd3[c(9),c(26)]))),as.numeric(paste(factor(districtsd3[c(9),c(27)]))),as.numeric(paste(factor(districtsd3[c(9),c(28)]))),as.numeric(paste(factor(districtsd3[c(9),c(29)]))),as.numeric(paste(factor(districtsd3[c(9),c(30)]))),as.numeric(paste(factor(districtsd3[c(9),c(31)]))),
                   as.numeric(paste(factor(districtsd3[c(9),c(32)]))),as.numeric(paste(factor(districtsd3[c(9),c(33)]))),as.numeric(paste(factor(districtsd3[c(9),c(34)]))),as.numeric(paste(factor(districtsd3[c(9),c(35)]))),as.numeric(paste(factor(districtsd3[c(9),c(36)]))),as.numeric(paste(factor(districtsd3[c(9),c(37)]))),as.numeric(paste(factor(districtsd3[c(9),c(38)]))),as.numeric(paste(factor(districtsd3[c(9),c(39)]))),as.numeric(paste(factor(districtsd3[c(9),c(40)]))),as.numeric(paste(factor(districtsd3[c(9),c(41)]))),as.numeric(paste(factor(districtsd3[c(9),c(42)]))),as.numeric(paste(factor(districtsd3[c(9),c(43)]))),as.numeric(paste(factor(districtsd3[c(9),c(44)]))),as.numeric(paste(factor(districtsd3[c(9),c(45)]))))
df3d3<-data.frame(discsd3,neiSatNorDisd3)

satisfiedttd3<-c(as.character(paste(factor(districtsd3[c(11),c(1)]))))
satisfiedd3<-c(as.numeric(paste(factor(districtsd3[c(11),c(2)]))),as.numeric(paste(factor(districtsd3[c(11),c(3)]))),as.numeric(paste(factor(districtsd3[c(11),c(4)]))), as.numeric(paste(factor(districtsd3[c(11),c(5)]))), as.numeric(paste(factor(districtsd3[c(11),c(6)]))), as.numeric(paste(factor(districtsd3[c(11),c(7)]))), as.numeric(paste(factor(districtsd3[c(11),c(8)]))), as.numeric(paste(factor(districtsd3[c(11),c(9)]))),as.numeric(paste(factor(districtsd3[c(11),c(10)]))),as.numeric(paste(factor(districtsd3[c(11),c(11)]))),as.numeric(paste(factor(districtsd3[c(11),c(12)]))), as.numeric(paste(factor(districtsd3[c(11),c(13)]))),as.numeric(paste(factor(districtsd3[c(11),c(14)]))),as.numeric(paste(factor(districtsd3[c(11),c(15)]))),as.numeric(paste(factor(districtsd3[c(11),c(16)]))),
               as.numeric(paste(factor(districtsd3[c(11),c(17)]))),as.numeric(paste(factor(districtsd3[c(11),c(18)]))),as.numeric(paste(factor(districtsd3[c(11),c(19)]))),as.numeric(paste(factor(districtsd3[c(11),c(20)]))),as.numeric(paste(factor(districtsd3[c(11),c(21)]))),as.numeric(paste(factor(districtsd3[c(11),c(22)]))),as.numeric(paste(factor(districtsd3[c(11),c(23)]))),as.numeric(paste(factor(districtsd3[c(11),c(24)]))),as.numeric(paste(factor(districtsd3[c(11),c(25)]))),as.numeric(paste(factor(districtsd3[c(11),c(26)]))),as.numeric(paste(factor(districtsd3[c(11),c(27)]))),as.numeric(paste(factor(districtsd3[c(11),c(28)]))),as.numeric(paste(factor(districtsd3[c(11),c(29)]))),as.numeric(paste(factor(districtsd3[c(11),c(30)]))),as.numeric(paste(factor(districtsd3[c(11),c(31)]))),
               as.numeric(paste(factor(districtsd3[c(11),c(32)]))),as.numeric(paste(factor(districtsd3[c(11),c(33)]))),as.numeric(paste(factor(districtsd3[c(11),c(34)]))),as.numeric(paste(factor(districtsd3[c(11),c(35)]))),as.numeric(paste(factor(districtsd3[c(11),c(36)]))),as.numeric(paste(factor(districtsd3[c(11),c(37)]))),as.numeric(paste(factor(districtsd3[c(11),c(38)]))),as.numeric(paste(factor(districtsd3[c(11),c(39)]))),as.numeric(paste(factor(districtsd3[c(11),c(40)]))),as.numeric(paste(factor(districtsd3[c(11),c(41)]))),as.numeric(paste(factor(districtsd3[c(11),c(42)]))),as.numeric(paste(factor(districtsd3[c(11),c(43)]))),as.numeric(paste(factor(districtsd3[c(11),c(44)]))),as.numeric(paste(factor(districtsd3[c(11),c(45)]))))
df4d3<-data.frame(discsd3,satisfiedd3)


verysatttd3<-c(as.character(paste(factor(districtsd3[c(13),c(1)]))))
verySatd3<-c(as.numeric(paste(factor(districtsd3[c(13),c(2)]))),as.numeric(paste(factor(districtsd3[c(13),c(3)]))),as.numeric(paste(factor(districtsd3[c(13),c(4)]))), as.numeric(paste(factor(districtsd3[c(13),c(5)]))), as.numeric(paste(factor(districtsd3[c(13),c(6)]))), as.numeric(paste(factor(districtsd3[c(13),c(7)]))), as.numeric(paste(factor(districtsd3[c(13),c(8)]))), as.numeric(paste(factor(districtsd3[c(13),c(9)]))),as.numeric(paste(factor(districtsd3[c(13),c(10)]))),as.numeric(paste(factor(districtsd3[c(13),c(11)]))),as.numeric(paste(factor(districtsd3[c(13),c(12)]))), as.numeric(paste(factor(districtsd3[c(13),c(13)]))),as.numeric(paste(factor(districtsd3[c(13),c(14)]))),as.numeric(paste(factor(districtsd3[c(13),c(15)]))),as.numeric(paste(factor(districtsd3[c(13),c(16)]))),
             as.numeric(paste(factor(districtsd3[c(13),c(17)]))),as.numeric(paste(factor(districtsd3[c(13),c(18)]))),as.numeric(paste(factor(districtsd3[c(13),c(19)]))),as.numeric(paste(factor(districtsd3[c(13),c(20)]))),as.numeric(paste(factor(districtsd3[c(13),c(21)]))),as.numeric(paste(factor(districtsd3[c(13),c(22)]))),as.numeric(paste(factor(districtsd3[c(13),c(23)]))),as.numeric(paste(factor(districtsd3[c(13),c(24)]))),as.numeric(paste(factor(districtsd3[c(13),c(25)]))),as.numeric(paste(factor(districtsd3[c(13),c(26)]))),as.numeric(paste(factor(districtsd3[c(13),c(27)]))),as.numeric(paste(factor(districtsd3[c(13),c(28)]))),as.numeric(paste(factor(districtsd3[c(13),c(29)]))),as.numeric(paste(factor(districtsd3[c(13),c(30)]))),as.numeric(paste(factor(districtsd3[c(13),c(31)]))),
             as.numeric(paste(factor(districtsd3[c(13),c(32)]))),as.numeric(paste(factor(districtsd3[c(13),c(33)]))),as.numeric(paste(factor(districtsd3[c(13),c(34)]))),as.numeric(paste(factor(districtsd3[c(13),c(35)]))),as.numeric(paste(factor(districtsd3[c(13),c(36)]))),as.numeric(paste(factor(districtsd3[c(13),c(37)]))),as.numeric(paste(factor(districtsd3[c(13),c(38)]))),as.numeric(paste(factor(districtsd3[c(13),c(39)]))),as.numeric(paste(factor(districtsd3[c(13),c(40)]))),as.numeric(paste(factor(districtsd3[c(13),c(41)]))),as.numeric(paste(factor(districtsd3[c(13),c(42)]))),as.numeric(paste(factor(districtsd3[c(13),c(43)]))),as.numeric(paste(factor(districtsd3[c(13),c(44)]))),as.numeric(paste(factor(districtsd3[c(13),c(45)]))))
df5d3<-data.frame(discsd3,verySatd3)

################################################################
#############ROAD types###################################################
Roadtyped3 <- mydata2[c(2:15),c(2,41:42)]

Roadtyped3sd3<-c(as.character(paste(factor(Roadtyped3[c(2),c(2)]))),as.character(paste(factor(Roadtyped3[c(2),c(3)]))))


very.disttroadsd3<-c(as.character(paste(factor(Roadtyped3 [c(5),c(1)]))))
very.disroadsd3<-c(as.numeric(paste(factor(Roadtyped3 [c(5),c(2)]))),as.numeric(paste(factor(Roadtyped3 [c(5),c(3)]))))

dfroadsd3<-data.frame(Roadtyped3sd3,very.disroadsd3)

disttroadsd3<-c(as.character(paste(factor(Roadtyped3 [c(7),c(1)]))))
disroadsd3<-c(as.numeric(paste(factor(Roadtyped3 [c(7),c(2)]))),as.numeric(paste(factor(Roadtyped3 [c(7),c(3)]))))
dfroads2d3<-data.frame(Roadtyped3sd3,disroadsd3)

neiSaNorDistt.roadsd3<-c(as.character(paste(factor(Roadtyped3 [c(9),c(1)]))))
neiSatNorDis.roadsd3 <-c(as.numeric(paste(factor(Roadtyped3 [c(9),c(2)]))),as.numeric(paste(factor(Roadtyped3 [c(9),c(3)]))))
dfroads3d3<-data.frame(Roadtyped3sd3,neiSatNorDis.roadsd3)

satisfiedttroadsd3<-c(as.character(paste(factor(Roadtyped3 [c(11),c(1)]))))
satisfiedroadsd3<-c(as.numeric(paste(factor(Roadtyped3 [c(11),c(2)]))),as.numeric(paste(factor(Roadtyped3 [c(11),c(3)]))))
dfroads4d3<-data.frame(Roadtyped3sd3,satisfiedroadsd3)

verysatttroadsd3<-c(as.character(paste(factor(Roadtyped3 [c(13),c(1)]))))
verySatroadsd3<-c(as.numeric(paste(factor(Roadtyped3 [c(13),c(2)]))),as.numeric(paste(factor(Roadtyped3 [c(13),c(3)]))))
dfroads5d3<-data.frame(Roadtyped3sd3,verySatroadsd3)          
################################################################

#############Respondent gender###################################################
Respongenderd3 <- mydata2[c(2:15),c(2,43:44)]

genderd3<-c(as.character(paste(factor(Respongenderd3[c(2),c(2)]))),as.character(paste(factor(Respongenderd3[c(2),c(3)]))))


very.disttgenderd3<-c(as.character(paste(factor(Respongenderd3 [c(5),c(1)]))))
very.disgenderd3<-c(as.numeric(paste(factor(Respongenderd3 [c(5),c(2)]))),as.numeric(paste(factor(Respongenderd3[c(5),c(3)]))))

dfrespond3<-data.frame(genderd3,very.disgenderd3)

disttgenderd3<-c(as.character(paste(factor(Respongenderd3 [c(7),c(1)]))))
disgenderd3<-c(as.numeric(paste(factor(Respongenderd3 [c(7),c(2)]))),as.numeric(paste(factor(Respongenderd3 [c(7),c(3)]))))
dfrespon2d3<-data.frame(genderd3,disgenderd3)

neiSaNorDistt.genderd3<-c(as.character(paste(factor(Respongenderd3 [c(9),c(1)]))))
neiSatNorDis.genderd3 <-c(as.numeric(paste(factor(Respongenderd3 [c(9),c(2)]))),as.numeric(paste(factor(Respongenderd3 [c(9),c(3)]))))
dfrespon3d3<-data.frame(genderd3,neiSatNorDis.genderd3)

satisfiedttGenderd3<-c(as.character(paste(factor(Respongenderd3 [c(11),c(1)]))))
satisfiedgenderd3<-c(as.numeric(paste(factor(Respongenderd3 [c(11),c(2)]))),as.numeric(paste(factor(Respongenderd3 [c(11),c(3)]))))
dfrespon4d3<-data.frame(genderd3,satisfiedgenderd3)

verysatttgenderd3<-c(as.character(paste(factor(Respongenderd3 [c(13),c(1)]))))
verySatGenderd3<-c(as.numeric(paste(factor(Respongenderd3 [c(13),c(2)]))),as.numeric(paste(factor(Respongenderd3 [c(13),c(3)]))))
dfrespon5d3<-data.frame(genderd3,verySatGenderd3)  


################################################################
###########      Road name    ###################################################
Roadnamed3<-mydata2[c(2:15),c(2,45:104)]

rdnamesd3<-c(as.character(paste(factor(Roadnamed3[c(2),c(2)]))),as.character(paste(factor(Roadnamed3[c(2),c(3)]))),as.character(paste(factor(Roadnamed3[c(2),c(4)]))), as.character(paste(factor(Roadnamed3[c(2),c(5)]))), as.character(paste(factor(Roadnamed3[c(2),c(6)]))), as.character(paste(factor(Roadnamed3[c(2),c(7)]))), as.character(paste(factor(Roadnamed3[c(2),c(8)]))), as.character(paste(factor(Roadnamed3[c(2),c(9)]))),as.character(paste(factor(Roadnamed3[c(2),c(10)]))),as.character(paste(factor(Roadnamed3[c(2),c(11)]))),as.character(paste(factor(Roadnamed3[c(2),c(12)]))), as.character(paste(factor(Roadnamed3[c(2),c(13)]))),as.character(paste(factor(Roadnamed3[c(2),c(14)]))),as.character(paste(factor(Roadnamed3[c(2),c(15)]))),as.character(paste(factor(Roadnamed3[c(2),c(16)]))),
             as.character(paste(factor(Roadnamed3[c(2),c(17)]))),as.character(paste(factor(Roadnamed3[c(2),c(18)]))),as.character(paste(factor(Roadnamed3[c(2),c(19)]))),as.character(paste(factor(Roadnamed3[c(2),c(20)]))),as.character(paste(factor(Roadnamed3[c(2),c(21)]))),as.character(paste(factor(Roadnamed3[c(2),c(22)]))),as.character(paste(factor(Roadnamed3[c(2),c(23)]))),as.character(paste(factor(Roadnamed3[c(2),c(24)]))),as.character(paste(factor(Roadnamed3[c(2),c(25)]))),as.character(paste(factor(Roadnamed3[c(2),c(26)]))),as.character(paste(factor(Roadnamed3[c(2),c(27)]))),as.character(paste(factor(Roadnamed3[c(2),c(28)]))),as.character(paste(factor(Roadnamed3[c(2),c(29)]))),as.character(paste(factor(Roadnamed3[c(2),c(30)]))),as.character(paste(factor(Roadnamed3[c(2),c(31)]))),
             as.character(paste(factor(Roadnamed3[c(2),c(32)]))),as.character(paste(factor(Roadnamed3[c(2),c(33)]))),as.character(paste(factor(Roadnamed3[c(2),c(34)]))),as.character(paste(factor(Roadnamed3[c(2),c(35)]))),as.character(paste(factor(Roadnamed3[c(2),c(36)]))),as.character(paste(factor(Roadnamed3[c(2),c(37)]))),as.character(paste(factor(Roadnamed3[c(2),c(38)]))),as.character(paste(factor(Roadnamed3[c(2),c(39)]))),as.character(paste(factor(Roadnamed3[c(2),c(40)]))),as.character(paste(factor(Roadnamed3[c(2),c(41)]))),as.character(paste(factor(Roadnamed3[c(2),c(42)]))),as.character(paste(factor(Roadnamed3[c(2),c(43)]))),as.character(paste(factor(Roadnamed3[c(2),c(44)]))),as.character(paste(factor(Roadnamed3[c(2),c(45)])))
             ,as.character(paste(factor(Roadnamed3[c(2),c(46)]))),as.character(paste(factor(Roadnamed3[c(2),c(47)]))),as.character(paste(factor(Roadnamed3[c(2),c(48)]))), as.character(paste(factor(Roadnamed3[c(2),c(49)]))), as.character(paste(factor(Roadnamed3[c(2),c(50)]))), as.character(paste(factor(Roadnamed3[c(2),c(51)]))), as.character(paste(factor(Roadnamed3[c(2),c(52)]))), as.character(paste(factor(Roadnamed3[c(2),c(53)]))),as.character(paste(factor(Roadnamed3[c(2),c(54)]))),as.character(paste(factor(Roadnamed3[c(2),c(55)]))),as.character(paste(factor(Roadnamed3[c(2),c(56)]))), as.character(paste(factor(Roadnamed3[c(2),c(57)]))),
             as.character(paste(factor(Roadnamed3[c(2),c(58)]))),as.character(paste(factor(Roadnamed3[c(2),c(59)]))),as.character(paste(factor(Roadnamed3[c(2),c(60)]))), as.character(paste(factor(Roadnamed3[c(2),c(61)]))))


very.disttrdnamed3<-c(as.character(paste(factor(Roadnamed3[c(5),c(1)]))))
very.disnamed3<-c(as.numeric(paste(factor(Roadnamed3[c(5),c(2)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(3)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(4)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(5)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(6)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(7)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(8)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(9)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(10)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(11)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(12)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(13)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(14)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(15)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(16)]))),
                  as.numeric(paste(factor(Roadnamed3[c(5),c(17)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(18)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(19)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(20)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(21)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(22)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(23)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(24)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(25)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(26)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(27)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(28)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(29)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(30)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(31)]))),
                  as.numeric(paste(factor(Roadnamed3[c(5),c(32)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(33)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(34)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(35)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(36)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(37)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(38)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(39)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(40)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(41)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(42)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(43)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(44)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(45)])))
                  ,as.numeric(paste(factor(Roadnamed3[c(5),c(46)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(47)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(48)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(49)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(50)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(51)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(52)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(53)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(54)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(55)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(56)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(57)]))),
                  as.numeric(paste(factor(Roadnamed3[c(5),c(58)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(59)]))),as.numeric(paste(factor(Roadnamed3[c(5),c(60)]))), as.numeric(paste(factor(Roadnamed3[c(5),c(61)]))))

dfnamesd3<-data.frame(rdnamesd3,very.disnamed3)          


distt.named3<-c(as.character(paste(factor(Roadnamed3[c(7),c(1)]))))
dis.named3<-c(as.numeric(paste(factor(Roadnamed3[c(7),c(2)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(3)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(4)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(5)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(6)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(7)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(8)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(9)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(10)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(11)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(12)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(13)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(14)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(15)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(16)]))),
              as.numeric(paste(factor(Roadnamed3[c(7),c(17)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(18)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(19)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(20)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(21)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(22)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(23)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(24)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(25)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(26)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(27)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(28)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(29)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(30)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(31)]))),
              as.numeric(paste(factor(Roadnamed3[c(7),c(32)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(33)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(34)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(35)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(36)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(37)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(38)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(39)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(40)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(41)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(42)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(43)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(44)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(45)])))
              ,as.numeric(paste(factor(Roadnamed3[c(7),c(46)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(47)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(48)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(49)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(50)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(51)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(52)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(53)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(54)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(55)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(56)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(57)]))),
              as.numeric(paste(factor(Roadnamed3[c(7),c(58)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(59)]))),as.numeric(paste(factor(Roadnamed3[c(7),c(60)]))), as.numeric(paste(factor(Roadnamed3[c(7),c(61)]))))

dfnames2d3<-data.frame(rdnamesd3,dis.named3)

neiSaNorDistt.named3<-c(as.character(paste(factor(districts[c(9),c(1)]))))
neiSatNorDis.named3<-c(as.numeric(paste(factor(Roadnamed3[c(9),c(2)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(3)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(4)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(5)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(6)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(7)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(8)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(9)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(10)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(11)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(12)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(13)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(14)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(15)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(16)]))),
                       as.numeric(paste(factor(Roadnamed3[c(9),c(17)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(18)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(19)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(20)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(21)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(22)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(23)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(24)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(25)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(26)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(27)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(28)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(29)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(30)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(31)]))),
                       as.numeric(paste(factor(Roadnamed3[c(9),c(32)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(33)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(34)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(35)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(36)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(37)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(38)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(39)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(40)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(41)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(42)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(43)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(44)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(45)])))
                       ,as.numeric(paste(factor(Roadnamed3[c(9),c(46)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(47)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(48)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(49)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(50)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(51)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(52)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(53)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(54)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(55)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(56)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(57)]))),
                       as.numeric(paste(factor(Roadnamed3[c(9),c(58)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(59)]))),as.numeric(paste(factor(Roadnamed3[c(9),c(60)]))), as.numeric(paste(factor(Roadnamed3[c(9),c(61)]))))


dfnames3d3<-data.frame(rdnamesd3,neiSatNorDis.named3)

satisfiedtt.namesd3<-c(as.character(paste(factor(districts[c(11),c(1)]))))

satisfied.namesd3<-c(as.numeric(paste(factor(Roadnamed3[c(11),c(2)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(3)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(4)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(5)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(6)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(7)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(8)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(9)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(10)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(11)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(12)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(13)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(14)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(15)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(16)]))),
                     as.numeric(paste(factor(Roadnamed3[c(11),c(17)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(18)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(19)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(20)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(21)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(22)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(23)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(24)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(25)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(26)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(27)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(28)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(29)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(30)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(31)]))),
                     as.numeric(paste(factor(Roadnamed3[c(11),c(32)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(33)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(34)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(35)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(36)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(37)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(38)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(39)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(40)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(41)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(42)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(43)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(44)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(45)])))
                     ,as.numeric(paste(factor(Roadnamed3[c(11),c(46)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(47)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(48)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(49)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(50)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(51)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(52)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(53)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(54)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(55)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(56)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(57)]))),
                     as.numeric(paste(factor(Roadnamed3[c(11),c(58)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(59)]))),as.numeric(paste(factor(Roadnamed3[c(11),c(60)]))), as.numeric(paste(factor(Roadnamed3[c(11),c(61)]))))


dfnames4d3<-data.frame(rdnamesd3,satisfied.namesd3)


verysatt.namesd3<-c(as.character(paste(factor(districts[c(13),c(1)]))))

verySat.namesd3<-c(as.numeric(paste(factor(Roadnamed3[c(13),c(2)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(3)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(4)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(5)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(6)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(7)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(8)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(9)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(10)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(11)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(12)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(13)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(14)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(15)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(16)]))),
                   as.numeric(paste(factor(Roadnamed3[c(13),c(17)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(18)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(19)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(20)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(21)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(22)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(23)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(24)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(25)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(26)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(27)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(28)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(29)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(30)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(31)]))),
                   as.numeric(paste(factor(Roadnamed3[c(13),c(32)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(33)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(34)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(35)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(36)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(37)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(38)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(39)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(40)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(41)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(42)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(43)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(44)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(45)])))
                   ,as.numeric(paste(factor(Roadnamed3[c(13),c(46)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(47)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(48)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(49)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(50)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(51)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(52)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(53)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(54)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(55)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(56)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(57)]))),
                   as.numeric(paste(factor(Roadnamed3[c(13),c(58)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(59)]))),as.numeric(paste(factor(Roadnamed3[c(13),c(60)]))), as.numeric(paste(factor(Roadnamed3[c(13),c(61)]))))

dfnames5d3<-data.frame(rdnamesd3,verySat.namesd3)


################################################################
###########      Respondent Type    ###################################################


Respontyped3<-mydata2[c(2:15),c(2,105:110)]

restyped3<-c(as.character(paste(factor(Respontyped3[c(2),c(2)]))),as.character(paste(factor(Respontyped3[c(2),c(3)]))),as.character(paste(factor(Respontyped3[c(2),c(4)]))), as.character(paste(factor(Respontyped3[c(2),c(5)]))), as.character(paste(factor(Respontyped3[c(2),c(6)]))), as.character(paste(factor(Respontyped3[c(2),c(7)]))), as.character(paste(factor(Respontyped3[c(2),c(8)]))), as.character(paste(factor(Respontyped3[c(2),c(9)]))),as.character(paste(factor(Respontyped3[c(2),c(10)]))),as.character(paste(factor(Respontyped3[c(2),c(11)]))),as.character(paste(factor(Respontyped3[c(2),c(12)]))), as.character(paste(factor(Respontyped3[c(2),c(13)]))))

very.disttRrestyped3<-c(as.character(paste(factor(Respontyped3[c(5),c(1)]))))
very.disRestyped3<-c(as.numeric(paste(factor(Respontyped3[c(5),c(2)]))),as.numeric(paste(factor(Respontyped3[c(5),c(3)]))),as.numeric(paste(factor(Respontyped3[c(5),c(4)]))), as.numeric(paste(factor(Respontyped3[c(5),c(5)]))), as.numeric(paste(factor(Respontyped3[c(5),c(6)]))), as.numeric(paste(factor(Respontyped3[c(5),c(7)]))), as.numeric(paste(factor(Respontyped3[c(5),c(8)]))), as.numeric(paste(factor(Respontyped3[c(5),c(9)]))),as.numeric(paste(factor(Respontyped3[c(5),c(10)]))),as.numeric(paste(factor(Respontyped3[c(5),c(11)]))),as.numeric(paste(factor(Respontyped3[c(5),c(12)]))), as.numeric(paste(factor(Respontyped3[c(5),c(13)]))))
dfRestypesd3<-data.frame(restyped3,very.disRestyped3)          

distt.restyped3<-c(as.character(paste(factor(Respontyped3[c(7),c(1)]))))
disRestyped3<-c(as.numeric(paste(factor(Respontyped3[c(7),c(2)]))),as.numeric(paste(factor(Respontyped3[c(7),c(3)]))),as.numeric(paste(factor(Respontyped3[c(7),c(4)]))), as.numeric(paste(factor(Respontyped3[c(7),c(5)]))), as.numeric(paste(factor(Respontyped3[c(7),c(6)]))), as.numeric(paste(factor(Respontyped3[c(7),c(7)]))), as.numeric(paste(factor(Respontyped3[c(7),c(8)]))), as.numeric(paste(factor(Respontyped3[c(7),c(9)]))),as.numeric(paste(factor(Respontyped3[c(7),c(10)]))),as.numeric(paste(factor(Respontyped3[c(7),c(11)]))),as.numeric(paste(factor(Respontyped3[c(7),c(12)]))), as.numeric(paste(factor(Respontyped3[c(7),c(13)]))))
dfRestypes2d3<-data.frame(restyped3,disRestyped3)


neiSaNorDisttRestyped3<-c(as.character(paste(factor(Respontyped3[c(9),c(1)]))))

neiSatNorDisRestyped3<-c(as.numeric(paste(factor(Respontyped3[c(9),c(2)]))),as.numeric(paste(factor(Respontyped3[c(9),c(3)]))),as.numeric(paste(factor(Respontyped3[c(9),c(4)]))), as.numeric(paste(factor(Respontyped3[c(9),c(5)]))), as.numeric(paste(factor(Respontyped3[c(9),c(6)]))), as.numeric(paste(factor(Respontyped3[c(9),c(7)]))), as.numeric(paste(factor(Respontyped3[c(9),c(8)]))), as.numeric(paste(factor(Respontyped3[c(9),c(9)]))),as.numeric(paste(factor(Respontyped3[c(9),c(10)]))),as.numeric(paste(factor(Respontyped3[c(9),c(11)]))),as.numeric(paste(factor(Respontyped3[c(9),c(12)]))), as.numeric(paste(factor(Respontyped3[c(9),c(13)]))))
dfRestypes3d3<-data.frame(restyped3,neiSatNorDisRestyped3)


satisfiedttRestyped3<-c(as.character(paste(factor(Respontyped3[c(11),c(1)]))))

satisfiedRestyped3<-c(as.numeric(paste(factor(Respontyped3[c(11),c(2)]))),as.numeric(paste(factor(Respontyped3[c(11),c(3)]))),as.numeric(paste(factor(Respontyped3[c(11),c(4)]))), as.numeric(paste(factor(Respontyped3[c(11),c(5)]))), as.numeric(paste(factor(Respontyped3[c(11),c(6)]))), as.numeric(paste(factor(Respontyped3[c(11),c(7)]))), as.numeric(paste(factor(Respontyped3[c(11),c(8)]))), as.numeric(paste(factor(Respontyped3[c(11),c(9)]))),as.numeric(paste(factor(Respontyped3[c(11),c(10)]))),as.numeric(paste(factor(Respontyped3[c(11),c(11)]))),as.numeric(paste(factor(Respontyped3[c(11),c(12)]))), as.numeric(paste(factor(Respontyped3[c(11),c(13)]))))
dfRestypes4d3<-data.frame(restyped3,satisfiedRestyped3)

verysattRestyped3<-c(as.character(paste(factor(Respontyped3[c(13),c(1)]))))

verySatRestyped3<-c(as.numeric(paste(factor(Respontyped3[c(13),c(2)]))),as.numeric(paste(factor(Respontyped3[c(13),c(3)]))),as.numeric(paste(factor(Respontyped3[c(13),c(4)]))), as.numeric(paste(factor(Respontyped3[c(13),c(5)]))), as.numeric(paste(factor(Respontyped3[c(13),c(6)]))), as.numeric(paste(factor(Respontyped3[c(13),c(7)]))), as.numeric(paste(factor(Respontyped3[c(13),c(8)]))), as.numeric(paste(factor(Respontyped3[c(13),c(9)]))),as.numeric(paste(factor(Respontyped3[c(13),c(10)]))),as.numeric(paste(factor(Respontyped3[c(13),c(11)]))),as.numeric(paste(factor(Respontyped3[c(13),c(12)]))), as.numeric(paste(factor(Respontyped3[c(13),c(13)]))))
dfRestypes5d3<-data.frame(restyped3,verySatRestyped3)


################################################################
###########      Age bracket    ###################################################

Agebracketd3<-mydata2[c(2:15),c(2,111:118)]

aged3<-c(as.character(paste(factor(Agebracketd3[c(2),c(2)]))),as.character(paste(factor(Agebracketd3[c(2),c(3)]))),as.character(paste(factor(Agebracketd3[c(2),c(4)]))), as.character(paste(factor(Agebracketd3[c(2),c(5)]))), as.character(paste(factor(Agebracketd3[c(2),c(6)]))), as.character(paste(factor(Agebracketd3[c(2),c(7)]))), as.character(paste(factor(Agebracketd3[c(2),c(8)]))), as.character(paste(factor(Agebracketd3[c(2),c(9)]))))

very.disttAged3<-c(as.character(paste(factor(Agebracketd3[c(5),c(1)]))))
very.disAged3<-c(as.numeric(paste(factor(Agebracketd3[c(5),c(2)]))),as.numeric(paste(factor(Agebracketd3[c(5),c(3)]))),as.numeric(paste(factor(Agebracketd3[c(5),c(4)]))), as.numeric(paste(factor(Agebracketd3[c(5),c(5)]))), as.numeric(paste(factor(Agebracketd3[c(5),c(6)]))), as.numeric(paste(factor(Agebracketd3[c(5),c(7)]))), as.numeric(paste(factor(Agebracketd3[c(5),c(8)]))), as.numeric(paste(factor(Agebracketd3[c(5),c(9)]))))

dfAged3<-data.frame(aged3,very.disAged3)          

distt.Aged3<-c(as.character(paste(factor(Agebracketd3[c(7),c(1)]))))
disAged3<-c(as.numeric(paste(factor(Agebracketd3[c(7),c(2)]))),as.numeric(paste(factor(Agebracketd3[c(7),c(3)]))),as.numeric(paste(factor(Agebracketd3[c(7),c(4)]))), as.numeric(paste(factor(Agebracketd3[c(7),c(5)]))), as.numeric(paste(factor(Agebracketd3[c(7),c(6)]))), as.numeric(paste(factor(Agebracketd3[c(7),c(7)]))), as.numeric(paste(factor(Agebracketd3[c(7),c(8)]))), as.numeric(paste(factor(Agebracketd3[c(7),c(9)]))))
dfAge2d3<-data.frame(aged3,disAged3)


neiSaNorDistt.Aged3<-c(as.character(paste(factor(Agebracketd3[c(9),c(1)]))))

neiSatNorDisAged3<-c(as.numeric(paste(factor(Agebracketd3[c(9),c(2)]))),as.numeric(paste(factor(Agebracketd3[c(9),c(3)]))),as.numeric(paste(factor(Agebracketd3[c(9),c(4)]))), as.numeric(paste(factor(Agebracketd3[c(9),c(5)]))), as.numeric(paste(factor(Agebracketd3[c(9),c(6)]))), as.numeric(paste(factor(Agebracketd3[c(9),c(7)]))), as.numeric(paste(factor(Agebracketd3[c(9),c(8)]))), as.numeric(paste(factor(Agebracketd3[c(9),c(9)]))))
dfAge3d3<-data.frame(aged3,neiSatNorDisAged3)         

satisfiedttAged3<-c(as.character(paste(factor(Agebracketd3[c(11),c(1)]))))

satisfiedAged3<-c(as.numeric(paste(factor(Agebracketd3[c(11),c(2)]))),as.numeric(paste(factor(Agebracketd3[c(11),c(3)]))),as.numeric(paste(factor(Agebracketd3[c(11),c(4)]))), as.numeric(paste(factor(Agebracketd3[c(11),c(5)]))), as.numeric(paste(factor(Agebracketd3[c(11),c(6)]))), as.numeric(paste(factor(Agebracketd3[c(11),c(7)]))), as.numeric(paste(factor(Agebracketd3[c(11),c(8)]))), as.numeric(paste(factor(Agebracketd3[c(11),c(9)]))))
dfAge4d3<-data.frame(aged3,satisfiedAged3)

verysatt.Aged3<-c(as.character(paste(factor(Agebracketd3[c(13),c(1)]))))

verySatAged3<-c(as.numeric(paste(factor(Agebracketd3[c(13),c(2)]))),as.numeric(paste(factor(Agebracketd3[c(13),c(3)]))),as.numeric(paste(factor(Agebracketd3[c(13),c(4)]))), as.numeric(paste(factor(Agebracketd3[c(13),c(5)]))), as.numeric(paste(factor(Agebracketd3[c(13),c(6)]))), as.numeric(paste(factor(Agebracketd3[c(13),c(7)]))), as.numeric(paste(factor(Agebracketd3[c(13),c(8)]))), as.numeric(paste(factor(Agebracketd3[c(13),c(9)]))))
dfAge5d3<-data.frame(aged3,verySatAged3)

################################################################################################################################################################################################
################################UNRA3..QUESTION 2......

regionq2d3<-mydata2[18:91,2:9]

soutthttq2d3<-factor(regionq2[c(2),c(4)])
southernq2d3<-as.numeric(paste(factor( regionq2d3[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(4)])))
var2q2d3<-as.character(paste(factor( regionq2d3[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(1)])))
southq2d3<-data.frame(var2q2d3,southernq2d3)

centttq2d3<-factor(regionq2[c(2),c(5)])
centralq2d3<-as.numeric(paste(factor( regionq2d3[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(5)])))
centrq2d3<-data.frame(var2q2d3,centralq2d3)

easttq2d3<-factor(regionq2[c(2),c(6)])
easternq2d3<-as.numeric(paste(factor( regionq2d3[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(6)])))
eastq2d3<-data.frame(var2q2d3,easternq2d3)

northttq2d3<-factor(regionq2[c(2),c(7)])
northernq2d3<-as.numeric(paste(factor( regionq2d3[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(7)])))
northq2d3<-data.frame(var2q2d3,northernq2d3)

westttq2d3<-factor(regionq2[c(2),c(8)])
westernq2d3<-as.numeric(paste(factor( regionq2d3[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57),c(8)])))
westq2d3<-data.frame(var2q2d3,westernq2d3)



###########################################################################district2 qn2###################################################
districtQn2d3<-mydata2[18:91,c(2,10:40)]


discsQn2d3<-c(as.character(paste(factor(districtQn2d3[c(2),c(2)]))),as.character(paste(factor(districtQn2d3[c(2),c(3)]))),as.character(paste(factor(districtQn2d3[c(2),c(4)]))), as.character(paste(factor(districtQn2d3[c(2),c(5)]))), as.character(paste(factor(districtQn2d3[c(2),c(6)]))), as.character(paste(factor(districtQn2d3[c(2),c(7)]))), as.character(paste(factor(districtQn2d3[c(2),c(8)]))), as.character(paste(factor(districtQn2d3[c(2),c(9)]))),as.character(paste(factor(districtQn2d3[c(2),c(10)]))),as.character(paste(factor(districtQn2d3[c(2),c(11)]))),as.character(paste(factor(districtQn2d3[c(2),c(12)]))), as.character(paste(factor(districtQn2d3[c(2),c(13)]))),as.character(paste(factor(districtQn2d3[c(2),c(14)]))),as.character(paste(factor(districtQn2d3[c(2),c(15)]))),as.character(paste(factor(districtQn2d3[c(2),c(16)]))),
              as.character(paste(factor(districtQn2d3[c(2),c(17)]))),as.character(paste(factor(districtQn2d3[c(2),c(18)]))),as.character(paste(factor(districtQn2d3[c(2),c(19)]))),as.character(paste(factor(districtQn2d3[c(2),c(20)]))),as.character(paste(factor(districtQn2d3[c(2),c(21)]))),as.character(paste(factor(districtQn2d3[c(2),c(22)]))),as.character(paste(factor(districtQn2d3[c(2),c(23)]))),as.character(paste(factor(districtQn2d3[c(2),c(24)]))),as.character(paste(factor(districtQn2d3[c(2),c(25)]))),as.character(paste(factor(districtQn2d3[c(2),c(26)]))),as.character(paste(factor(districtQn2d3[c(2),c(27)]))),as.character(paste(factor(districtQn2d3[c(2),c(28)]))),as.character(paste(factor(districtQn2d3[c(2),c(29)]))),as.character(paste(factor(districtQn2d3[c(2),c(30)]))),as.character(paste(factor(districtQn2d3[c(2),c(31)]))),
              as.character(paste(factor(districtQn2d3[c(2),c(32)]))),as.character(paste(factor(districtQn2d3[c(2),c(33)]))),as.character(paste(factor(districtQn2d3[c(2),c(34)]))),as.character(paste(factor(districtQn2d3[c(2),c(35)]))),as.character(paste(factor(districtQn2d3[c(2),c(36)]))),as.character(paste(factor(districtQn2d3[c(2),c(37)]))),as.character(paste(factor(districtQn2d3[c(2),c(38)]))),as.character(paste(factor(districtQn2d3[c(2),c(39)]))),as.character(paste(factor(districtQn2d3[c(2),c(40)]))),as.character(paste(factor(districtQn2d3[c(2),c(41)]))),as.character(paste(factor(districtQn2d3[c(2),c(42)]))),as.character(paste(factor(districtQn2d3[c(2),c(43)]))),as.character(paste(factor(districtQn2d3[c(2),c(44)]))),as.character(paste(factor(districtQn2d3[c(2),c(45)]))))

roads.are.tarmttd3<-c(as.character(paste(factor(districtQn2d3[c(5),c(1)]))))
roads.tarmd3<-c(as.numeric(paste(factor(districtQn2d3[c(5),c(2)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(3)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(4)]))), as.numeric(paste(factor(districtQn2d3[c(5),c(5)]))), as.numeric(paste(factor(districtQn2d3[c(5),c(6)]))), as.numeric(paste(factor(districtQn2d3[c(5),c(7)]))), as.numeric(paste(factor(districtQn2d3[c(5),c(8)]))), as.numeric(paste(factor(districtQn2d3[c(5),c(9)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(10)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(11)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(12)]))), as.numeric(paste(factor(districtQn2d3[c(5),c(13)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(14)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(15)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(16)]))),
                as.numeric(paste(factor(districtQn2d3[c(5),c(17)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(18)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(19)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(20)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(21)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(22)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(23)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(24)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(25)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(26)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(27)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(28)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(29)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(30)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(31)]))),
                as.numeric(paste(factor(districtQn2d3[c(5),c(32)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(33)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(34)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(35)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(36)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(37)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(38)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(39)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(40)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(41)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(42)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(43)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(44)]))),as.numeric(paste(factor(districtQn2d3[c(5),c(45)]))))
rdstard3<-data.frame(discsQn2d3,roads.tarmd3)

morerds.conttd3<-c(as.character(paste(factor(districtQn2d3[c(7),c(1)]))))
moroads.consd3<-c(as.numeric(paste(factor(districtQn2d3[c(7),c(2)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(3)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(4)]))), as.numeric(paste(factor(districtQn2d3[c(7),c(5)]))), as.numeric(paste(factor(districtQn2d3[c(7),c(6)]))), as.numeric(paste(factor(districtQn2d3[c(7),c(7)]))), as.numeric(paste(factor(districtQn2d3[c(7),c(8)]))), as.numeric(paste(factor(districtQn2d3[c(7),c(9)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(10)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(11)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(12)]))), as.numeric(paste(factor(districtQn2d3[c(7),c(13)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(14)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(15)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(16)]))),
                  as.numeric(paste(factor(districtQn2d3[c(7),c(17)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(18)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(19)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(20)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(21)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(22)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(23)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(24)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(25)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(26)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(27)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(28)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(29)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(30)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(31)]))),
                  as.numeric(paste(factor(districtQn2d3[c(7),c(32)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(33)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(34)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(35)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(36)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(37)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(38)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(39)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(40)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(41)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(42)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(43)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(44)]))),as.numeric(paste(factor(districtQn2d3[c(7),c(45)]))))
mo.rds.cond3<-data.frame(discsQn2d3,moroads.consd3)

no.filled.potholesttd3<-c(as.character(paste(factor(districtQn2d3[c(9),c(1)]))))
no.filledpotd3<-c(as.numeric(paste(factor(districtQn2d3[c(9),c(2)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(3)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(4)]))), as.numeric(paste(factor(districtQn2d3[c(9),c(5)]))), as.numeric(paste(factor(districtQn2d3[c(9),c(6)]))), as.numeric(paste(factor(districtQn2d3[c(9),c(7)]))), as.numeric(paste(factor(districtQn2d3[c(9),c(8)]))), as.numeric(paste(factor(districtQn2d3[c(9),c(9)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(10)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(11)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(12)]))), as.numeric(paste(factor(districtQn2d3[c(9),c(13)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(14)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(15)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(16)]))),
                  as.numeric(paste(factor(districtQn2d3[c(9),c(17)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(18)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(19)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(20)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(21)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(22)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(23)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(24)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(25)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(26)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(27)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(28)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(29)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(30)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(31)]))),
                  as.numeric(paste(factor(districtQn2d3[c(9),c(32)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(33)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(34)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(35)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(36)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(37)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(38)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(39)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(40)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(41)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(42)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(43)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(44)]))),as.numeric(paste(factor(districtQn2d3[c(9),c(45)]))))
no.filledpotsd3<-data.frame(discsQn2d3,no.filledpotd3)


imprd.rd.sigttd3<-c(as.character(paste(factor(districtQn2d3[c(11),c(1)]))))
imprvd.rd.sigd3<-c(as.numeric(paste(factor(districtQn2d3[c(11),c(2)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(3)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(4)]))), as.numeric(paste(factor(districtQn2d3[c(11),c(5)]))), as.numeric(paste(factor(districtQn2d3[c(11),c(6)]))), as.numeric(paste(factor(districtQn2d3[c(11),c(7)]))), as.numeric(paste(factor(districtQn2d3[c(11),c(8)]))), as.numeric(paste(factor(districtQn2d3[c(11),c(9)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(10)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(11)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(12)]))), as.numeric(paste(factor(districtQn2d3[c(11),c(13)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(14)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(15)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(16)]))),
                   as.numeric(paste(factor(districtQn2d3[c(11),c(17)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(18)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(19)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(20)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(21)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(22)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(23)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(24)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(25)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(26)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(27)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(28)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(29)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(30)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(31)]))),
                   as.numeric(paste(factor(districtQn2d3[c(11),c(32)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(33)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(34)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(35)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(36)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(37)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(38)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(39)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(40)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(41)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(42)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(43)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(44)]))),as.numeric(paste(factor(districtQn2d3[c(11),c(45)]))))
imprd.rd.sigsd3<-data.frame(discsQn2d3,imprvd.rd.sigd3)


strict.usage.bodattd3<-c(as.character(paste(factor(districtQn2d3[c(13),c(1)]))))
strict.rd.usage.bodad3<-c(as.numeric(paste(factor(districtQn2d3[c(13),c(2)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(3)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(4)]))), as.numeric(paste(factor(districtQn2d3[c(13),c(5)]))), as.numeric(paste(factor(districtQn2d3[c(13),c(6)]))), as.numeric(paste(factor(districtQn2d3[c(13),c(7)]))), as.numeric(paste(factor(districtQn2d3[c(13),c(8)]))), as.numeric(paste(factor(districtQn2d3[c(13),c(9)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(10)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(11)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(12)]))), as.numeric(paste(factor(districtQn2d3[c(13),c(13)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(14)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(15)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(16)]))),
                          as.numeric(paste(factor(districtQn2d3[c(13),c(17)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(18)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(19)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(20)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(21)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(22)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(23)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(24)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(25)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(26)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(27)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(28)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(29)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(30)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(31)]))),
                          as.numeric(paste(factor(districtQn2d3[c(13),c(32)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(33)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(34)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(35)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(36)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(37)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(38)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(39)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(40)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(41)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(42)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(43)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(44)]))),as.numeric(paste(factor(districtQn2d3[c(13),c(45)]))))
stric.usage.bbd3<-data.frame(discsQn2d3,strict.rd.usage.bodad3)

############# ROAD TYPE QN2####



RoadtypeQn2d3<- mydata2[c(18:91),c(2,41:42)]

rdtypesqn2d3<-c(as.character(paste(factor(RoadtypeQn2d3[c(2),c(2)]))),as.character(paste(factor(RoadtypeQn2d3[c(2),c(3)]))))


roads.are.tarmtt2d3<-c(as.character(paste(factor(RoadtypeQn2d3[c(5),c(1)]))))
roads.tarm2d3<-c(as.numeric(paste(factor(RoadtypeQn2d3[c(5),c(2)]))),as.numeric(paste(factor(RoadtypeQn2d3[c(5),c(3)]))))

rdstar2d3<-data.frame(rdtypesqn2d3,roads.tarm2d3)


morerds.contt2d3<-c(as.character(paste(factor(RoadtypeQn2d3[c(7),c(1)]))))
moroads.cons2d3<-c(as.numeric(paste(factor(RoadtypeQn2d3[c(7),c(2)]))),as.numeric(paste(factor(RoadtypeQn2d3[c(7),c(3)]))))

mo.rds.con2d3<-data.frame(rdtypesqn2d3,moroads.cons2d3)


no.filled.potholestt2d3<-c(as.character(paste(factor(RoadtypeQn2d3[c(9),c(1)]))))
no.filledpot2d3<-c(as.numeric(paste(factor(RoadtypeQn2d3[c(9),c(2)]))),as.numeric(paste(factor(RoadtypeQn2d3[c(9),c(3)]))))

no.filledpots2d3<-data.frame(rdtypesqn2d3,no.filledpot2d3)


imprd.rd.sigtt2d3<-c(as.character(paste(factor(RoadtypeQn2d3[c(11),c(1)]))))
imprvd.rd.sig2d3<-c(as.numeric(paste(factor(RoadtypeQn2d3[c(11),c(2)]))),as.numeric(paste(factor(RoadtypeQn2d3[c(11),c(3)]))))

imprd.rd.sigs2d3<-data.frame(rdtypesqn2d3,imprvd.rd.sig2d3)


strict.usage.bodatt2d3<-c(as.character(paste(factor(RoadtypeQn2d3[c(13),c(1)]))))
strict.rd.usage.boda2d3<-c(as.numeric(paste(factor(RoadtypeQn2d3[c(13),c(2)]))),as.numeric(paste(factor(RoadtypeQn2d3[c(13),c(3)]))))

stric.usage.bb2d3<-data.frame(rdtypesqn2d3,strict.rd.usage.boda2d3)

############################################ RESPONDENT GENDER Qn2##############################
RespongenderQn2d3<- mydata2[c(18:91),c(2,43:44)]
genderQn2d3<-c(as.character(paste(factor( RespongenderQn2d3[c(2),c(2)]))),as.character(paste(factor( RespongenderQn2d3[c(2),c(3)]))))

roads.are.tarmtt3d3<-c(as.character(paste(factor( RespongenderQn2d3[c(5),c(1)]))))
roads.tarm3d3<-c(as.numeric(paste(factor( RespongenderQn2d3[c(5),c(2)]))),as.numeric(paste(factor( RespongenderQn2d3[c(5),c(3)]))))
rdstar3d3<-data.frame( genderQn2d3,roads.tarm3d3)

morerds.contt3d3<-c(as.character(paste(factor( RespongenderQn2d3[c(7),c(1)]))))
moroads.cons3d3<-c(as.numeric(paste(factor( RespongenderQn2d3[c(7),c(2)]))),as.numeric(paste(factor( RespongenderQn2d3[c(7),c(3)]))))
mo.rds.con3d3<-data.frame( genderQn2d3,moroads.cons3d3)

no.filled.potholestt3d3<-c(as.character(paste(factor( RespongenderQn2d3[c(9),c(1)]))))
no.filledpot3d3<-c(as.numeric(paste(factor( RespongenderQn2d3[c(9),c(2)]))),as.numeric(paste(factor( RespongenderQn2d3[c(9),c(3)]))))
no.filledpots3d3<-data.frame( genderQn2d3,no.filledpot3d3)

imprd.rd.sigtt3d3<-c(as.character(paste(factor( RespongenderQn2d3[c(11),c(1)]))))
imprvd.rd.sig3d3<-c(as.numeric(paste(factor( RespongenderQn2d3[c(11),c(2)]))),as.numeric(paste(factor( RespongenderQn2d3[c(11),c(3)]))))
imprd.rd.sigs3d3<-data.frame( genderQn2d3,imprvd.rd.sig3d3)

strict.usage.bodatt3d3<-c(as.character(paste(factor( RespongenderQn2d3[c(13),c(1)]))))
strict.rd.usage.boda3d3<-c(as.numeric(paste(factor( RespongenderQn2d3[c(13),c(2)]))),as.numeric(paste(factor( RespongenderQn2d3[c(13),c(3)]))))
stric.usage.bb3d3<-data.frame( genderQn2d3,strict.rd.usage.boda3d3)

########################################ROAD NAME Qn2##########################################################################

RoadnmQn2d3<-mydata2[18:91,c(2,45:104)]

rdnamesQn2d3<-c(as.character(paste(factor(RoadnmQn2d3[c(2),c(2)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(3)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(4)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(5)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(6)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(7)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(8)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(9)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(10)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(11)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(12)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(13)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(14)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(15)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(16)]))),
                as.character(paste(factor(RoadnmQn2d3[c(2),c(17)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(18)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(19)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(20)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(21)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(22)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(23)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(24)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(25)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(26)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(27)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(28)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(29)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(30)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(31)]))),
                as.character(paste(factor(RoadnmQn2d3[c(2),c(32)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(33)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(34)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(35)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(36)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(37)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(38)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(39)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(40)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(41)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(42)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(43)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(44)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(45)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(46)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(47)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(48)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(49)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(50)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(51)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(52)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(53)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(54)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(55)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(56)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(57)]))),
                as.character(paste(factor(RoadnmQn2d3[c(2),c(58)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(59)]))),as.character(paste(factor(RoadnmQn2d3[c(2),c(60)]))), as.character(paste(factor(RoadnmQn2d3[c(2),c(61)]))))

roads.are.tarmtt4d3<-c(as.character(paste(factor(RoadnmQn2d3[c(5),c(1)]))))

roads.tarm4d3<-c(as.numeric(paste(factor(RoadnmQn2d3[c(5),c(2)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(3)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(4)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(5)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(6)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(7)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(8)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(9)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(10)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(11)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(12)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(13)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(14)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(15)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(16)]))),
                 as.numeric(paste(factor(RoadnmQn2d3[c(5),c(17)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(18)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(19)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(20)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(21)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(22)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(23)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(24)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(25)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(26)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(27)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(28)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(29)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(30)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(31)]))),
                 as.numeric(paste(factor(RoadnmQn2d3[c(5),c(32)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(33)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(34)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(35)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(36)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(37)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(38)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(39)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(40)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(41)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(42)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(43)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(44)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(45)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(46)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(47)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(48)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(49)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(50)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(51)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(52)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(53)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(54)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(55)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(56)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(57)]))),
                 as.numeric(paste(factor(RoadnmQn2d3[c(5),c(58)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(59)]))),as.numeric(paste(factor(RoadnmQn2d3[c(5),c(60)]))), as.numeric(paste(factor(RoadnmQn2d3[c(5),c(61)]))))
rdstar4d3<-data.frame(rdnamesQn2d3,roads.tarm4d3)

morerds.contt4d3<-c(as.character(paste(factor(RoadnmQn2d3[c(7),c(1)]))))
moroads.cons4d3<-c(as.numeric(paste(factor(RoadnmQn2d3[c(7),c(2)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(3)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(4)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(5)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(6)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(7)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(8)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(9)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(10)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(11)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(12)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(13)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(14)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(15)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(16)]))),
                   as.numeric(paste(factor(RoadnmQn2d3[c(7),c(17)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(18)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(19)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(20)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(21)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(22)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(23)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(24)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(25)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(26)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(27)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(28)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(29)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(30)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(31)]))),
                   as.numeric(paste(factor(RoadnmQn2d3[c(7),c(32)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(33)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(34)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(35)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(36)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(37)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(38)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(39)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(40)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(41)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(42)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(43)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(44)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(45)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(46)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(47)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(48)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(49)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(50)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(51)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(52)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(53)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(54)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(55)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(56)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(57)]))),
                   as.numeric(paste(factor(RoadnmQn2d3[c(7),c(58)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(59)]))),as.numeric(paste(factor(RoadnmQn2d3[c(7),c(60)]))), as.numeric(paste(factor(RoadnmQn2d3[c(7),c(61)]))))
mo.rds.con4d3<-data.frame(rdnamesQn2d3,moroads.cons4d3)

no.filled.potholestt4d3<-c(as.character(paste(factor(RoadnmQn2d3[c(9),c(1)]))))
no.filledpot4d3<-c(as.numeric(paste(factor(RoadnmQn2d3[c(9),c(2)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(3)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(4)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(5)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(6)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(7)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(8)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(9)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(10)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(11)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(12)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(13)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(14)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(15)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(16)]))),
                   as.numeric(paste(factor(RoadnmQn2d3[c(9),c(17)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(18)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(19)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(20)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(21)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(22)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(23)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(24)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(25)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(26)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(27)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(28)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(29)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(30)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(31)]))),
                   as.numeric(paste(factor(RoadnmQn2d3[c(9),c(32)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(33)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(34)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(35)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(36)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(37)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(38)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(39)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(40)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(41)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(42)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(43)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(44)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(45)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(46)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(47)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(48)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(49)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(50)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(51)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(52)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(53)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(54)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(55)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(56)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(57)]))),
                   as.numeric(paste(factor(RoadnmQn2d3[c(9),c(58)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(59)]))),as.numeric(paste(factor(RoadnmQn2d3[c(9),c(60)]))), as.numeric(paste(factor(RoadnmQn2d3[c(9),c(61)]))))
no.filledpots4d3<-data.frame(rdnamesQn2d3,no.filledpot4d3)


imprd.rd.sigtt4d3<-c(as.character(paste(factor(RoadnmQn2d3[c(11),c(1)]))))
imprvd.rd.sig4d3<-c(as.numeric(paste(factor(RoadnmQn2d3[c(11),c(2)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(3)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(4)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(5)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(6)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(7)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(8)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(9)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(10)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(11)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(12)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(13)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(14)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(15)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(16)]))),
                    as.numeric(paste(factor(RoadnmQn2d3[c(11),c(17)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(18)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(19)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(20)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(21)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(22)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(23)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(24)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(25)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(26)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(27)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(28)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(29)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(30)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(31)]))),
                    as.numeric(paste(factor(RoadnmQn2d3[c(11),c(32)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(33)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(34)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(35)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(36)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(37)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(38)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(39)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(40)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(41)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(42)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(43)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(44)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(45)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(46)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(47)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(48)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(49)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(50)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(51)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(52)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(53)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(54)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(55)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(56)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(57)]))),
                    as.numeric(paste(factor(RoadnmQn2d3[c(11),c(58)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(59)]))),as.numeric(paste(factor(RoadnmQn2d3[c(11),c(60)]))), as.numeric(paste(factor(RoadnmQn2d3[c(11),c(61)]))))
imprd.rd.sigs4d3<-data.frame(rdnamesQn2d3,imprvd.rd.sig4d3)


strict.usage.bodatt4d3<-c(as.character(paste(factor(RoadnmQn2d3[c(13),c(1)]))))
strict.rd.usage.boda4d3<-c(as.numeric(paste(factor(RoadnmQn2d3[c(13),c(2)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(3)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(4)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(5)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(6)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(7)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(8)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(9)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(10)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(11)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(12)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(13)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(14)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(15)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(16)]))),
                           as.numeric(paste(factor(RoadnmQn2d3[c(13),c(17)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(18)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(19)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(20)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(21)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(22)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(23)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(24)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(25)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(26)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(27)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(28)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(29)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(30)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(31)]))),
                           as.numeric(paste(factor(RoadnmQn2d3[c(13),c(32)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(33)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(34)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(35)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(36)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(37)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(38)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(39)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(40)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(41)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(42)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(43)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(44)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(45)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(46)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(47)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(48)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(49)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(50)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(51)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(52)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(53)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(54)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(55)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(56)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(57)]))),
                           as.numeric(paste(factor(RoadnmQn2d3[c(13),c(58)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(59)]))),as.numeric(paste(factor(RoadnmQn2d3[c(13),c(60)]))), as.numeric(paste(factor(RoadnmQn2d3[c(13),c(61)]))))
stric.usage.bb4d3<-data.frame(rdnamesQn2d3,strict.rd.usage.boda4d3)

############################################respondent type qn2##################
RespontypeQn2d3<-mydata2[18:91,c(2,105:110)]
restypeQn2d3<-c(as.character(paste(factor(RespontypeQn2d3[c(2),c(2)]))),as.character(paste(factor(RespontypeQn2d3[c(2),c(3)]))),as.character(paste(factor(RespontypeQn2d3[c(2),c(4)]))), as.character(paste(factor(RespontypeQn2d3[c(2),c(5)]))), as.character(paste(factor(RespontypeQn2d3[c(2),c(6)]))), as.character(paste(factor(RespontypeQn2d3[c(2),c(7)]))), as.character(paste(factor(RespontypeQn2d3[c(2),c(8)]))), as.character(paste(factor(RespontypeQn2d3[c(2),c(9)]))),as.character(paste(factor(RespontypeQn2d3[c(2),c(10)]))),as.character(paste(factor(RespontypeQn2d3[c(2),c(11)]))))

roads.are.tarmtt5d3<-c(as.character(paste(factor(RespontypeQn2d3[c(5),c(1)]))))
roads.tarm5d3<-c(as.numeric(paste(factor(RespontypeQn2d3[c(5),c(2)]))),as.numeric(paste(factor(RespontypeQn2d3[c(5),c(3)]))),as.numeric(paste(factor(RespontypeQn2d3[c(5),c(4)]))), as.numeric(paste(factor(RespontypeQn2d3[c(5),c(5)]))), as.numeric(paste(factor(RespontypeQn2d3[c(5),c(6)]))), as.numeric(paste(factor(RespontypeQn2d3[c(5),c(7)]))), as.numeric(paste(factor(RespontypeQn2d3[c(5),c(8)]))), as.numeric(paste(factor(RespontypeQn2d3[c(5),c(9)]))),as.numeric(paste(factor(RespontypeQn2d3[c(5),c(10)]))),as.numeric(paste(factor(RespontypeQn2d3[c(5),c(11)]))))
rdstar5d3<-data.frame( restypeQn2d3,roads.tarm5d3)

morerds.contt5d3<-c(as.character(paste(factor(RespontypeQn2d3[c(7),c(1)]))))
moroads.cons5d3<-c(as.numeric(paste(factor(RespontypeQn2d3[c(7),c(2)]))),as.numeric(paste(factor(RespontypeQn2d3[c(7),c(3)]))),as.numeric(paste(factor(RespontypeQn2d3[c(7),c(4)]))), as.numeric(paste(factor(RespontypeQn2d3[c(7),c(5)]))), as.numeric(paste(factor(RespontypeQn2d3[c(7),c(6)]))), as.numeric(paste(factor(RespontypeQn2d3[c(7),c(7)]))), as.numeric(paste(factor(RespontypeQn2d3[c(7),c(8)]))), as.numeric(paste(factor(RespontypeQn2d3[c(7),c(9)]))),as.numeric(paste(factor(RespontypeQn2d3[c(7),c(10)]))),as.numeric(paste(factor(RespontypeQn2d3[c(7),c(11)]))))
mo.rds.con5d3<-data.frame( restypeQn2d3,moroads.cons5d3)

no.filled.potholestt5d3<-c(as.character(paste(factor(RespontypeQn2d3[c(9),c(1)]))))
no.filledpot5d3<-c(as.numeric(paste(factor(RespontypeQn2d3[c(9),c(2)]))),as.numeric(paste(factor(RespontypeQn2d3[c(9),c(3)]))),as.numeric(paste(factor(RespontypeQn2d3[c(9),c(4)]))), as.numeric(paste(factor(RespontypeQn2d3[c(9),c(5)]))), as.numeric(paste(factor(RespontypeQn2d3[c(9),c(6)]))), as.numeric(paste(factor(RespontypeQn2d3[c(9),c(7)]))), as.numeric(paste(factor(RespontypeQn2d3[c(9),c(8)]))), as.numeric(paste(factor(RespontypeQn2d3[c(9),c(9)]))),as.numeric(paste(factor(RespontypeQn2d3[c(9),c(10)]))),as.numeric(paste(factor(RespontypeQn2d3[c(9),c(11)]))))
no.filledpots5d3<-data.frame( restypeQn2d3,no.filledpot5d3)

imprd.rd.sigtt5d3<-c(as.character(paste(factor(RespontypeQn2d3[c(11),c(1)]))))
imprvd.rd.sig5d3<-c(as.numeric(paste(factor(RespontypeQn2d3[c(11),c(2)]))),as.numeric(paste(factor(RespontypeQn2d3[c(11),c(3)]))),as.numeric(paste(factor(RespontypeQn2d3[c(11),c(4)]))), as.numeric(paste(factor(RespontypeQn2d3[c(11),c(5)]))), as.numeric(paste(factor(RespontypeQn2d3[c(11),c(6)]))), as.numeric(paste(factor(RespontypeQn2d3[c(11),c(7)]))), as.numeric(paste(factor(RespontypeQn2d3[c(11),c(8)]))), as.numeric(paste(factor(RespontypeQn2d3[c(11),c(9)]))),as.numeric(paste(factor(RespontypeQn2d3[c(11),c(10)]))),as.numeric(paste(factor(RespontypeQn2d3[c(11),c(11)]))))
imprd.rd.sigs5d3<-data.frame( restypeQn2d3,imprvd.rd.sig5d3)

strict.usage.bodatt5d3<-c(as.character(paste(factor(RespontypeQn2d3[c(13),c(1)]))))
strict.rd.usage.boda5d3<-c(as.numeric(paste(factor(RespontypeQn2d3[c(13),c(2)]))),as.numeric(paste(factor(RespontypeQn2d3[c(13),c(3)]))),as.numeric(paste(factor(RespontypeQn2d3[c(13),c(4)]))), as.numeric(paste(factor(RespontypeQn2d3[c(13),c(5)]))), as.numeric(paste(factor(RespontypeQn2d3[c(13),c(6)]))), as.numeric(paste(factor(RespontypeQn2d3[c(13),c(7)]))), as.numeric(paste(factor(RespontypeQn2d3[c(13),c(8)]))), as.numeric(paste(factor(RespontypeQn2d3[c(13),c(9)]))),as.numeric(paste(factor(RespontypeQn2d3[c(13),c(10)]))),as.numeric(paste(factor(RespontypeQn2d3[c(13),c(11)]))))
stric.usage.bb5d3<-data.frame( restypeQn2d3,strict.rd.usage.boda5d3)

##############################################age bracket qn2#################

AgebracketQn2d3<-mydata2[18:91,c(2,111:118)]
ageQn2d3<-c(as.character(paste(factor(AgebracketQn2d3[c(2),c(2)]))),as.character(paste(factor(AgebracketQn2d3[c(2),c(3)]))),as.character(paste(factor(AgebracketQn2d3[c(2),c(4)]))), as.character(paste(factor(AgebracketQn2d3[c(2),c(5)]))), as.character(paste(factor(AgebracketQn2d3[c(2),c(6)]))), as.character(paste(factor(AgebracketQn2d3[c(2),c(7)]))), as.character(paste(factor(AgebracketQn2d3[c(2),c(8)]))), as.character(paste(factor(AgebracketQn2d3[c(2),c(9)]))))

roads.are.tarmtt6d3<-c(as.character(paste(factor(AgebracketQn2d3[c(5),c(1)]))))
roads.tarm6d3<-c(as.numeric(paste(factor(AgebracketQn2d3[c(5),c(2)]))),as.numeric(paste(factor(AgebracketQn2d3[c(5),c(3)]))),as.numeric(paste(factor(AgebracketQn2d3[c(5),c(4)]))), as.numeric(paste(factor(AgebracketQn2d3[c(5),c(5)]))), as.numeric(paste(factor(AgebracketQn2d3[c(5),c(6)]))), as.numeric(paste(factor(AgebracketQn2d3[c(5),c(7)]))), as.numeric(paste(factor(AgebracketQn2d3[c(5),c(8)]))), as.numeric(paste(factor(AgebracketQn2d3[c(5),c(9)]))))
rdstar6d3<-data.frame( ageQn2d3,roads.tarm6d3)

morerds.contt6d3<-c(as.character(paste(factor(AgebracketQn2d3[c(7),c(1)]))))
moroads.cons6d3<-c(as.numeric(paste(factor(AgebracketQn2d3[c(7),c(2)]))),as.numeric(paste(factor(AgebracketQn2d3[c(7),c(3)]))),as.numeric(paste(factor(AgebracketQn2d3[c(7),c(4)]))), as.numeric(paste(factor(AgebracketQn2d3[c(7),c(5)]))), as.numeric(paste(factor(AgebracketQn2d3[c(7),c(6)]))), as.numeric(paste(factor(AgebracketQn2d3[c(7),c(7)]))), as.numeric(paste(factor(AgebracketQn2d3[c(7),c(8)]))), as.numeric(paste(factor(AgebracketQn2d3[c(7),c(9)]))))
mo.rds.con6d3<-data.frame( ageQn2d3,moroads.cons6d3)

no.filled.potholestt6d3<-c(as.character(paste(factor(AgebracketQn2d3[c(9),c(1)]))))
no.filledpot6d3<-c(as.numeric(paste(factor(AgebracketQn2d3[c(9),c(2)]))),as.numeric(paste(factor(AgebracketQn2d3[c(9),c(3)]))),as.numeric(paste(factor(AgebracketQn2d3[c(9),c(4)]))), as.numeric(paste(factor(AgebracketQn2d3[c(9),c(5)]))), as.numeric(paste(factor(AgebracketQn2d3[c(9),c(6)]))), as.numeric(paste(factor(AgebracketQn2d3[c(9),c(7)]))), as.numeric(paste(factor(AgebracketQn2d3[c(9),c(8)]))), as.numeric(paste(factor(AgebracketQn2d3[c(9),c(9)]))))
no.filledpots6d3<-data.frame( ageQn2d3,no.filledpot6d3)

imprd.rd.sigtt6d3<-c(as.character(paste(factor(AgebracketQn2d3[c(11),c(1)]))))
imprvd.rd.sig6d3<-c(as.numeric(paste(factor(AgebracketQn2d3[c(11),c(2)]))),as.numeric(paste(factor(AgebracketQn2d3[c(11),c(3)]))),as.numeric(paste(factor(AgebracketQn2d3[c(11),c(4)]))), as.numeric(paste(factor(AgebracketQn2d3[c(11),c(5)]))), as.numeric(paste(factor(AgebracketQn2d3[c(11),c(6)]))), as.numeric(paste(factor(AgebracketQn2d3[c(11),c(7)]))), as.numeric(paste(factor(AgebracketQn2d3[c(11),c(8)]))), as.numeric(paste(factor(AgebracketQn2d3[c(11),c(9)]))))
imprd.rd.sigs6d3<-data.frame( ageQn2d3,imprvd.rd.sig6d3)

strict.usage.bodatt6d3<-c(as.character(paste(factor(AgebracketQn2d3[c(13),c(1)]))))
strict.rd.usage.boda6d3<-c(as.numeric(paste(factor(AgebracketQn2d3[c(13),c(2)]))),as.numeric(paste(factor(AgebracketQn2d3[c(13),c(3)]))),as.numeric(paste(factor(AgebracketQn2d3[c(13),c(4)]))), as.numeric(paste(factor(AgebracketQn2d3[c(13),c(5)]))), as.numeric(paste(factor(AgebracketQn2d3[c(13),c(6)]))), as.numeric(paste(factor(AgebracketQn2d3[c(13),c(7)]))), as.numeric(paste(factor(AgebracketQn2d3[c(13),c(8)]))), as.numeric(paste(factor(AgebracketQn2d3[c(13),c(9)]))))
stric.usage.bb6d3<-data.frame( ageQn2d3,strict.rd.usage.boda6d3)


################################################################################################################################################################################################
################################
mydata3 <- read.csv("ww/UNRA4.csv", header = FALSE)
question6<-as.character(mydata3[c(1),c(1)])

################################Regions
regiond4q<-mydata3[2:92,2:9]

soutthttd4q<-factor(regiond4q[c(2),c(4)])
southernd4q<-as.numeric(paste(factor( regiond4q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(4)])))
var2d4q<-as.character(paste(factor( regiond4q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(1)])))
southd4q<-data.frame(var2d4q,southernd4q)

centttd4q<-factor(regiond4q[c(2),c(5)])
centrald4q<-as.numeric(paste(factor( regiond4q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(5)])))
centrd4q<-data.frame(var2d4q,centrald4q)

easttd4q<-factor(regiond4q[c(2),c(6)])
easternd4q<-as.numeric(paste(factor( regiond4q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(6)])))
eastd4q<-data.frame(var2d4q,easternd4q)

northttd4q<-factor(regiond4q[c(2),c(7)])
northernd4q<-as.numeric(paste(factor( regiond4q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(7)])))
northd4q<-data.frame(var2d4q,northernd4q)

westttd4q<-factor(regiond4q[c(2),c(8)])
westernd4q<-as.numeric(paste(factor( regiond4q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(8)])))
westd4q<-data.frame(var2d4q,westernd4q)


################################DISTRICTS.
districtsd4<-mydata3[c(2:92),c(2,10:40)]

discsd4<-c(as.character(paste(factor(districtsd4[c(2),c(2)]))),as.character(paste(factor(districtsd4[c(2),c(3)]))),as.character(paste(factor(districtsd4[c(2),c(4)]))), as.character(paste(factor(districtsd4[c(2),c(5)]))), as.character(paste(factor(districtsd4[c(2),c(6)]))), as.character(paste(factor(districtsd4[c(2),c(7)]))), as.character(paste(factor(districtsd4[c(2),c(8)]))), as.character(paste(factor(districtsd4[c(2),c(9)]))),as.character(paste(factor(districtsd4[c(2),c(10)]))),as.character(paste(factor(districtsd4[c(2),c(11)]))),as.character(paste(factor(districtsd4[c(2),c(12)]))), as.character(paste(factor(districtsd4[c(2),c(13)]))),as.character(paste(factor(districtsd4[c(2),c(14)]))),as.character(paste(factor(districtsd4[c(2),c(15)]))),as.character(paste(factor(districtsd4[c(2),c(16)]))),
         as.character(paste(factor(districtsd4[c(2),c(17)]))),as.character(paste(factor(districtsd4[c(2),c(18)]))),as.character(paste(factor(districtsd4[c(2),c(19)]))),as.character(paste(factor(districtsd4[c(2),c(20)]))),as.character(paste(factor(districtsd4[c(2),c(21)]))),as.character(paste(factor(districtsd4[c(2),c(22)]))),as.character(paste(factor(districtsd4[c(2),c(23)]))),as.character(paste(factor(districtsd4[c(2),c(24)]))),as.character(paste(factor(districtsd4[c(2),c(25)]))),as.character(paste(factor(districtsd4[c(2),c(26)]))),as.character(paste(factor(districtsd4[c(2),c(27)]))),as.character(paste(factor(districtsd4[c(2),c(28)]))),as.character(paste(factor(districtsd4[c(2),c(29)]))),as.character(paste(factor(districtsd4[c(2),c(30)]))),as.character(paste(factor(districtsd4[c(2),c(31)]))),
         as.character(paste(factor(districtsd4[c(2),c(32)]))),as.character(paste(factor(districtsd4[c(2),c(33)]))),as.character(paste(factor(districtsd4[c(2),c(34)]))),as.character(paste(factor(districtsd4[c(2),c(35)]))),as.character(paste(factor(districtsd4[c(2),c(36)]))),as.character(paste(factor(districtsd4[c(2),c(37)]))),as.character(paste(factor(districtsd4[c(2),c(38)]))),as.character(paste(factor(districtsd4[c(2),c(39)]))),as.character(paste(factor(districtsd4[c(2),c(40)]))),as.character(paste(factor(districtsd4[c(2),c(41)]))),as.character(paste(factor(districtsd4[c(2),c(42)]))),as.character(paste(factor(districtsd4[c(2),c(43)]))),as.character(paste(factor(districtsd4[c(2),c(44)]))),as.character(paste(factor(districtsd4[c(2),c(45)]))))


a1tt<-c(as.character(paste(factor(districtsd4[c(5),c(1)]))))
a1<-c(as.numeric(paste(factor(districtsd4[c(5),c(2)]))),as.numeric(paste(factor(districtsd4[c(5),c(3)]))),as.numeric(paste(factor(districtsd4[c(5),c(4)]))), as.numeric(paste(factor(districtsd4[c(5),c(5)]))), as.numeric(paste(factor(districtsd4[c(5),c(6)]))), as.numeric(paste(factor(districtsd4[c(5),c(7)]))), as.numeric(paste(factor(districtsd4[c(5),c(8)]))), as.numeric(paste(factor(districtsd4[c(5),c(9)]))),as.numeric(paste(factor(districtsd4[c(5),c(10)]))),as.numeric(paste(factor(districtsd4[c(5),c(11)]))),as.numeric(paste(factor(districtsd4[c(5),c(12)]))), as.numeric(paste(factor(districtsd4[c(5),c(13)]))),as.numeric(paste(factor(districtsd4[c(5),c(14)]))),as.numeric(paste(factor(districtsd4[c(5),c(15)]))),as.numeric(paste(factor(districtsd4[c(5),c(16)]))),
      as.numeric(paste(factor(districtsd4[c(5),c(17)]))),as.numeric(paste(factor(districtsd4[c(5),c(18)]))),as.numeric(paste(factor(districtsd4[c(5),c(19)]))),as.numeric(paste(factor(districtsd4[c(5),c(20)]))),as.numeric(paste(factor(districtsd4[c(5),c(21)]))),as.numeric(paste(factor(districtsd4[c(5),c(22)]))),as.numeric(paste(factor(districtsd4[c(5),c(23)]))),as.numeric(paste(factor(districtsd4[c(5),c(24)]))),as.numeric(paste(factor(districtsd4[c(5),c(25)]))),as.numeric(paste(factor(districtsd4[c(5),c(26)]))),as.numeric(paste(factor(districtsd4[c(5),c(27)]))),as.numeric(paste(factor(districtsd4[c(5),c(28)]))),as.numeric(paste(factor(districtsd4[c(5),c(29)]))),as.numeric(paste(factor(districtsd4[c(5),c(30)]))),as.numeric(paste(factor(districtsd4[c(5),c(31)]))),
      as.numeric(paste(factor(districtsd4[c(5),c(32)]))),as.numeric(paste(factor(districtsd4[c(5),c(33)]))),as.numeric(paste(factor(districtsd4[c(5),c(34)]))),as.numeric(paste(factor(districtsd4[c(5),c(35)]))),as.numeric(paste(factor(districtsd4[c(5),c(36)]))),as.numeric(paste(factor(districtsd4[c(5),c(37)]))),as.numeric(paste(factor(districtsd4[c(5),c(38)]))),as.numeric(paste(factor(districtsd4[c(5),c(39)]))),as.numeric(paste(factor(districtsd4[c(5),c(40)]))),as.numeric(paste(factor(districtsd4[c(5),c(41)]))),as.numeric(paste(factor(districtsd4[c(5),c(42)]))),as.numeric(paste(factor(districtsd4[c(5),c(43)]))),as.numeric(paste(factor(districtsd4[c(5),c(44)]))),as.numeric(paste(factor(districtsd4[c(5),c(45)]))))
df1d4<-data.frame(discsd4,a1)


a2tt<-c(as.character(paste(factor(districtsd4[c(7),c(1)]))))
a2<-c(as.numeric(paste(factor(districtsd4[c(7),c(2)]))),as.numeric(paste(factor(districtsd4[c(7),c(3)]))),as.numeric(paste(factor(districtsd4[c(7),c(4)]))), as.numeric(paste(factor(districtsd4[c(7),c(5)]))), as.numeric(paste(factor(districtsd4[c(7),c(6)]))), as.numeric(paste(factor(districtsd4[c(7),c(7)]))), as.numeric(paste(factor(districtsd4[c(7),c(8)]))), as.numeric(paste(factor(districtsd4[c(7),c(9)]))),as.numeric(paste(factor(districtsd4[c(7),c(10)]))),as.numeric(paste(factor(districtsd4[c(7),c(11)]))),as.numeric(paste(factor(districtsd4[c(7),c(12)]))), as.numeric(paste(factor(districtsd4[c(7),c(13)]))),as.numeric(paste(factor(districtsd4[c(7),c(14)]))),as.numeric(paste(factor(districtsd4[c(7),c(15)]))),as.numeric(paste(factor(districtsd4[c(7),c(16)]))),
      as.numeric(paste(factor(districtsd4[c(7),c(17)]))),as.numeric(paste(factor(districtsd4[c(7),c(18)]))),as.numeric(paste(factor(districtsd4[c(7),c(19)]))),as.numeric(paste(factor(districtsd4[c(7),c(20)]))),as.numeric(paste(factor(districtsd4[c(7),c(21)]))),as.numeric(paste(factor(districtsd4[c(7),c(22)]))),as.numeric(paste(factor(districtsd4[c(7),c(23)]))),as.numeric(paste(factor(districtsd4[c(7),c(24)]))),as.numeric(paste(factor(districtsd4[c(7),c(25)]))),as.numeric(paste(factor(districtsd4[c(7),c(26)]))),as.numeric(paste(factor(districtsd4[c(7),c(27)]))),as.numeric(paste(factor(districtsd4[c(7),c(28)]))),as.numeric(paste(factor(districtsd4[c(7),c(29)]))),as.numeric(paste(factor(districtsd4[c(7),c(30)]))),as.numeric(paste(factor(districtsd4[c(7),c(31)]))),
      as.numeric(paste(factor(districtsd4[c(7),c(32)]))),as.numeric(paste(factor(districtsd4[c(7),c(33)]))),as.numeric(paste(factor(districtsd4[c(7),c(34)]))),as.numeric(paste(factor(districtsd4[c(7),c(35)]))),as.numeric(paste(factor(districtsd4[c(7),c(36)]))),as.numeric(paste(factor(districtsd4[c(7),c(37)]))),as.numeric(paste(factor(districtsd4[c(7),c(38)]))),as.numeric(paste(factor(districtsd4[c(7),c(39)]))),as.numeric(paste(factor(districtsd4[c(7),c(40)]))),as.numeric(paste(factor(districtsd4[c(7),c(41)]))),as.numeric(paste(factor(districtsd4[c(7),c(42)]))),as.numeric(paste(factor(districtsd4[c(7),c(43)]))),as.numeric(paste(factor(districtsd4[c(7),c(44)]))),as.numeric(paste(factor(districtsd4[c(7),c(45)]))))
df2d4<-data.frame(discsd4,a2)


a3tt<-c(as.character(paste(factor(districtsd4[c(9),c(1)]))))
a3 <-c(as.numeric(paste(factor(districtsd4[c(9),c(2)]))),as.numeric(paste(factor(districtsd4[c(9),c(3)]))),as.numeric(paste(factor(districtsd4[c(9),c(4)]))), as.numeric(paste(factor(districtsd4[c(9),c(5)]))), as.numeric(paste(factor(districtsd4[c(9),c(6)]))), as.numeric(paste(factor(districtsd4[c(9),c(7)]))), as.numeric(paste(factor(districtsd4[c(9),c(8)]))), as.numeric(paste(factor(districtsd4[c(9),c(9)]))),as.numeric(paste(factor(districtsd4[c(9),c(10)]))),as.numeric(paste(factor(districtsd4[c(9),c(11)]))),as.numeric(paste(factor(districtsd4[c(9),c(12)]))), as.numeric(paste(factor(districtsd4[c(9),c(13)]))),as.numeric(paste(factor(districtsd4[c(9),c(14)]))),as.numeric(paste(factor(districtsd4[c(9),c(15)]))),as.numeric(paste(factor(districtsd4[c(9),c(16)]))),
                 as.numeric(paste(factor(districtsd4[c(9),c(17)]))),as.numeric(paste(factor(districtsd4[c(9),c(18)]))),as.numeric(paste(factor(districtsd4[c(9),c(19)]))),as.numeric(paste(factor(districtsd4[c(9),c(20)]))),as.numeric(paste(factor(districtsd4[c(9),c(21)]))),as.numeric(paste(factor(districtsd4[c(9),c(22)]))),as.numeric(paste(factor(districtsd4[c(9),c(23)]))),as.numeric(paste(factor(districtsd4[c(9),c(24)]))),as.numeric(paste(factor(districtsd4[c(9),c(25)]))),as.numeric(paste(factor(districtsd4[c(9),c(26)]))),as.numeric(paste(factor(districtsd4[c(9),c(27)]))),as.numeric(paste(factor(districtsd4[c(9),c(28)]))),as.numeric(paste(factor(districtsd4[c(9),c(29)]))),as.numeric(paste(factor(districtsd4[c(9),c(30)]))),as.numeric(paste(factor(districtsd4[c(9),c(31)]))),
                 as.numeric(paste(factor(districtsd4[c(9),c(32)]))),as.numeric(paste(factor(districtsd4[c(9),c(33)]))),as.numeric(paste(factor(districtsd4[c(9),c(34)]))),as.numeric(paste(factor(districtsd4[c(9),c(35)]))),as.numeric(paste(factor(districtsd4[c(9),c(36)]))),as.numeric(paste(factor(districtsd4[c(9),c(37)]))),as.numeric(paste(factor(districtsd4[c(9),c(38)]))),as.numeric(paste(factor(districtsd4[c(9),c(39)]))),as.numeric(paste(factor(districtsd4[c(9),c(40)]))),as.numeric(paste(factor(districtsd4[c(9),c(41)]))),as.numeric(paste(factor(districtsd4[c(9),c(42)]))),as.numeric(paste(factor(districtsd4[c(9),c(43)]))),as.numeric(paste(factor(districtsd4[c(9),c(44)]))),as.numeric(paste(factor(districtsd4[c(9),c(45)]))))
df3d4<-data.frame(discsd4,a3)


a4tt<-c(as.character(paste(factor(districtsd4[c(11),c(1)]))))
a4<-c(as.numeric(paste(factor(districtsd4[c(11),c(2)]))),as.numeric(paste(factor(districtsd4[c(11),c(3)]))),as.numeric(paste(factor(districtsd4[c(11),c(4)]))), as.numeric(paste(factor(districtsd4[c(11),c(5)]))), as.numeric(paste(factor(districtsd4[c(11),c(6)]))), as.numeric(paste(factor(districtsd4[c(11),c(7)]))), as.numeric(paste(factor(districtsd4[c(11),c(8)]))), as.numeric(paste(factor(districtsd4[c(11),c(9)]))),as.numeric(paste(factor(districtsd4[c(11),c(10)]))),as.numeric(paste(factor(districtsd4[c(11),c(11)]))),as.numeric(paste(factor(districtsd4[c(11),c(12)]))), as.numeric(paste(factor(districtsd4[c(11),c(13)]))),as.numeric(paste(factor(districtsd4[c(11),c(14)]))),as.numeric(paste(factor(districtsd4[c(11),c(15)]))),as.numeric(paste(factor(districtsd4[c(11),c(16)]))),
             as.numeric(paste(factor(districtsd4[c(11),c(17)]))),as.numeric(paste(factor(districtsd4[c(11),c(18)]))),as.numeric(paste(factor(districtsd4[c(11),c(19)]))),as.numeric(paste(factor(districtsd4[c(11),c(20)]))),as.numeric(paste(factor(districtsd4[c(11),c(21)]))),as.numeric(paste(factor(districtsd4[c(11),c(22)]))),as.numeric(paste(factor(districtsd4[c(11),c(23)]))),as.numeric(paste(factor(districtsd4[c(11),c(24)]))),as.numeric(paste(factor(districtsd4[c(11),c(25)]))),as.numeric(paste(factor(districtsd4[c(11),c(26)]))),as.numeric(paste(factor(districtsd4[c(11),c(27)]))),as.numeric(paste(factor(districtsd4[c(11),c(28)]))),as.numeric(paste(factor(districtsd4[c(11),c(29)]))),as.numeric(paste(factor(districtsd4[c(11),c(30)]))),as.numeric(paste(factor(districtsd4[c(11),c(31)]))),
             as.numeric(paste(factor(districtsd4[c(11),c(32)]))),as.numeric(paste(factor(districtsd4[c(11),c(33)]))),as.numeric(paste(factor(districtsd4[c(11),c(34)]))),as.numeric(paste(factor(districtsd4[c(11),c(35)]))),as.numeric(paste(factor(districtsd4[c(11),c(36)]))),as.numeric(paste(factor(districtsd4[c(11),c(37)]))),as.numeric(paste(factor(districtsd4[c(11),c(38)]))),as.numeric(paste(factor(districtsd4[c(11),c(39)]))),as.numeric(paste(factor(districtsd4[c(11),c(40)]))),as.numeric(paste(factor(districtsd4[c(11),c(41)]))),as.numeric(paste(factor(districtsd4[c(11),c(42)]))),as.numeric(paste(factor(districtsd4[c(11),c(43)]))),as.numeric(paste(factor(districtsd4[c(11),c(44)]))),as.numeric(paste(factor(districtsd4[c(11),c(45)]))))
df4d4<-data.frame(discsd4,a4)

a5tt<-c(as.character(paste(factor(districtsd4[c(13),c(1)]))))
a5<-c(as.numeric(paste(factor(districtsd4[c(13),c(2)]))),as.numeric(paste(factor(districtsd4[c(13),c(3)]))),as.numeric(paste(factor(districtsd4[c(13),c(4)]))), as.numeric(paste(factor(districtsd4[c(13),c(5)]))), as.numeric(paste(factor(districtsd4[c(13),c(6)]))), as.numeric(paste(factor(districtsd4[c(13),c(7)]))), as.numeric(paste(factor(districtsd4[c(13),c(8)]))), as.numeric(paste(factor(districtsd4[c(13),c(9)]))),as.numeric(paste(factor(districtsd4[c(13),c(10)]))),as.numeric(paste(factor(districtsd4[c(13),c(11)]))),as.numeric(paste(factor(districtsd4[c(13),c(12)]))), as.numeric(paste(factor(districtsd4[c(13),c(13)]))),as.numeric(paste(factor(districtsd4[c(13),c(14)]))),as.numeric(paste(factor(districtsd4[c(13),c(15)]))),as.numeric(paste(factor(districtsd4[c(13),c(16)]))),
      as.numeric(paste(factor(districtsd4[c(13),c(17)]))),as.numeric(paste(factor(districtsd4[c(13),c(18)]))),as.numeric(paste(factor(districtsd4[c(13),c(19)]))),as.numeric(paste(factor(districtsd4[c(13),c(20)]))),as.numeric(paste(factor(districtsd4[c(13),c(21)]))),as.numeric(paste(factor(districtsd4[c(13),c(22)]))),as.numeric(paste(factor(districtsd4[c(13),c(23)]))),as.numeric(paste(factor(districtsd4[c(13),c(24)]))),as.numeric(paste(factor(districtsd4[c(13),c(25)]))),as.numeric(paste(factor(districtsd4[c(13),c(26)]))),as.numeric(paste(factor(districtsd4[c(13),c(27)]))),as.numeric(paste(factor(districtsd4[c(13),c(28)]))),as.numeric(paste(factor(districtsd4[c(13),c(29)]))),as.numeric(paste(factor(districtsd4[c(13),c(30)]))),as.numeric(paste(factor(districtsd4[c(13),c(31)]))),
      as.numeric(paste(factor(districtsd4[c(13),c(32)]))),as.numeric(paste(factor(districtsd4[c(13),c(33)]))),as.numeric(paste(factor(districtsd4[c(13),c(34)]))),as.numeric(paste(factor(districtsd4[c(13),c(35)]))),as.numeric(paste(factor(districtsd4[c(13),c(36)]))),as.numeric(paste(factor(districtsd4[c(13),c(37)]))),as.numeric(paste(factor(districtsd4[c(13),c(38)]))),as.numeric(paste(factor(districtsd4[c(13),c(39)]))),as.numeric(paste(factor(districtsd4[c(13),c(40)]))),as.numeric(paste(factor(districtsd4[c(13),c(41)]))),as.numeric(paste(factor(districtsd4[c(13),c(42)]))),as.numeric(paste(factor(districtsd4[c(13),c(43)]))),as.numeric(paste(factor(districtsd4[c(13),c(44)]))),as.numeric(paste(factor(districtsd4[c(13),c(45)]))))
df5d4<-data.frame(discsd4,a5)


################################road type.
roadtyped4<-mydata3[c(2:92),c(2,41:42)]

pavedttd4<-factor(roadtyped4[c(2),c(2)])
pavedd4<-as.numeric(paste(factor( roadtyped4[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(2)])))
var2roadtd4<-as.character(paste(factor( roadtyped4[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(1)])))
pavedroadtd4<-data.frame(var2roadtd4,pavedd4)


unpavedttd4<-factor(roadtyped4[c(2),c(3)])
unpavedd4<-as.numeric(paste(factor( roadtyped4[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(3)])))
unpavedroadtd4<-data.frame(var2roadtd4,unpavedd4)

################################respondent gender.
rsgenderd4<-mydata3[c(2:92),c(2,43:44)]

malettd4<-factor(rsgenderd4[c(2),c(2)])
maled4<-as.numeric(paste(factor( rsgenderd4[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(2)])))
var2rsgenderd4<-as.character(paste(factor( rsgenderd4[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(1)])))
maletd4<-data.frame(var2rsgenderd4,maled4)


femalettd4<-factor(rsgenderd4[c(2),c(3)])
femaled4<-as.numeric(paste(factor( rsgenderd4[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41,43,45,47,49,51,53,55,57,59,61,63,65,67,69,71,73,75,77,79,81,83,85,87,89,91),c(3)])))
femaletd4<-data.frame(var2rsgenderd4,femaled4)


################################.Road names

Roadnamed4<-mydata3[c(2:92),c(2,45:104)]

rdnamesd4<-c(as.character(paste(factor(Roadnamed4[c(2),c(2)]))),as.character(paste(factor(Roadnamed4[c(2),c(3)]))),as.character(paste(factor(Roadnamed4[c(2),c(4)]))), as.character(paste(factor(Roadnamed4[c(2),c(5)]))), as.character(paste(factor(Roadnamed4[c(2),c(6)]))), as.character(paste(factor(Roadnamed4[c(2),c(7)]))), as.character(paste(factor(Roadnamed4[c(2),c(8)]))), as.character(paste(factor(Roadnamed4[c(2),c(9)]))),as.character(paste(factor(Roadnamed4[c(2),c(10)]))),as.character(paste(factor(Roadnamed4[c(2),c(11)]))),as.character(paste(factor(Roadnamed4[c(2),c(12)]))), as.character(paste(factor(Roadnamed4[c(2),c(13)]))),as.character(paste(factor(Roadnamed4[c(2),c(14)]))),as.character(paste(factor(Roadnamed4[c(2),c(15)]))),as.character(paste(factor(Roadnamed4[c(2),c(16)]))),
             as.character(paste(factor(Roadnamed4[c(2),c(17)]))),as.character(paste(factor(Roadnamed4[c(2),c(18)]))),as.character(paste(factor(Roadnamed4[c(2),c(19)]))),as.character(paste(factor(Roadnamed4[c(2),c(20)]))),as.character(paste(factor(Roadnamed4[c(2),c(21)]))),as.character(paste(factor(Roadnamed4[c(2),c(22)]))),as.character(paste(factor(Roadnamed4[c(2),c(23)]))),as.character(paste(factor(Roadnamed4[c(2),c(24)]))),as.character(paste(factor(Roadnamed4[c(2),c(25)]))),as.character(paste(factor(Roadnamed4[c(2),c(26)]))),as.character(paste(factor(Roadnamed4[c(2),c(27)]))),as.character(paste(factor(Roadnamed4[c(2),c(28)]))),as.character(paste(factor(Roadnamed4[c(2),c(29)]))),as.character(paste(factor(Roadnamed4[c(2),c(30)]))),as.character(paste(factor(Roadnamed4[c(2),c(31)]))),
             as.character(paste(factor(Roadnamed4[c(2),c(32)]))),as.character(paste(factor(Roadnamed4[c(2),c(33)]))),as.character(paste(factor(Roadnamed4[c(2),c(34)]))),as.character(paste(factor(Roadnamed4[c(2),c(35)]))),as.character(paste(factor(Roadnamed4[c(2),c(36)]))),as.character(paste(factor(Roadnamed4[c(2),c(37)]))),as.character(paste(factor(Roadnamed4[c(2),c(38)]))),as.character(paste(factor(Roadnamed4[c(2),c(39)]))),as.character(paste(factor(Roadnamed4[c(2),c(40)]))),as.character(paste(factor(Roadnamed4[c(2),c(41)]))),as.character(paste(factor(Roadnamed4[c(2),c(42)]))),as.character(paste(factor(Roadnamed4[c(2),c(43)]))),as.character(paste(factor(Roadnamed4[c(2),c(44)]))),as.character(paste(factor(Roadnamed4[c(2),c(45)])))
             ,as.character(paste(factor(Roadnamed4[c(2),c(46)]))),as.character(paste(factor(Roadnamed4[c(2),c(47)]))),as.character(paste(factor(Roadnamed4[c(2),c(48)]))), as.character(paste(factor(Roadnamed4[c(2),c(49)]))), as.character(paste(factor(Roadnamed4[c(2),c(50)]))), as.character(paste(factor(Roadnamed4[c(2),c(51)]))), as.character(paste(factor(Roadnamed4[c(2),c(52)]))), as.character(paste(factor(Roadnamed4[c(2),c(53)]))),as.character(paste(factor(Roadnamed4[c(2),c(54)]))),as.character(paste(factor(Roadnamed4[c(2),c(55)]))),as.character(paste(factor(Roadnamed4[c(2),c(56)]))), as.character(paste(factor(Roadnamed4[c(2),c(57)]))),
             as.character(paste(factor(Roadnamed4[c(2),c(58)]))),as.character(paste(factor(Roadnamed4[c(2),c(59)]))),as.character(paste(factor(Roadnamed4[c(2),c(60)]))), as.character(paste(factor(Roadnamed4[c(2),c(61)]))))


attd4<-c(as.character(paste(factor(Roadnamed4[c(5),c(1)]))))
ad4<-c(as.numeric(paste(factor(Roadnamed4[c(5),c(2)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(3)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(4)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(5)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(6)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(7)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(8)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(9)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(10)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(11)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(12)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(13)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(14)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(15)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(16)]))),
       as.numeric(paste(factor(Roadnamed4[c(5),c(17)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(18)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(19)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(20)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(21)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(22)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(23)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(24)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(25)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(26)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(27)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(28)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(29)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(30)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(31)]))),
       as.numeric(paste(factor(Roadnamed4[c(5),c(32)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(33)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(34)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(35)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(36)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(37)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(38)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(39)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(40)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(41)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(42)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(43)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(44)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(45)])))
       ,as.numeric(paste(factor(Roadnamed4[c(5),c(46)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(47)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(48)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(49)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(50)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(51)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(52)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(53)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(54)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(55)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(56)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(57)]))),
       as.numeric(paste(factor(Roadnamed4[c(5),c(58)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(59)]))),as.numeric(paste(factor(Roadnamed4[c(5),c(60)]))), as.numeric(paste(factor(Roadnamed4[c(5),c(61)]))))

dfnamesd4<-data.frame(rdnamesd4,ad4)          


a1ttd4<-c(as.character(paste(factor(Roadnamed4[c(7),c(1)]))))
a1d4<-c(as.numeric(paste(factor(Roadnamed4[c(7),c(2)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(3)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(4)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(5)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(6)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(7)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(8)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(9)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(10)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(11)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(12)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(13)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(14)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(15)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(16)]))),
        as.numeric(paste(factor(Roadnamed4[c(7),c(17)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(18)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(19)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(20)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(21)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(22)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(23)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(24)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(25)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(26)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(27)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(28)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(29)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(30)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(31)]))),
        as.numeric(paste(factor(Roadnamed4[c(7),c(32)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(33)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(34)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(35)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(36)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(37)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(38)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(39)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(40)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(41)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(42)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(43)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(44)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(45)])))
        ,as.numeric(paste(factor(Roadnamed4[c(7),c(46)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(47)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(48)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(49)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(50)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(51)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(52)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(53)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(54)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(55)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(56)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(57)]))),
        as.numeric(paste(factor(Roadnamed4[c(7),c(58)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(59)]))),as.numeric(paste(factor(Roadnamed4[c(7),c(60)]))), as.numeric(paste(factor(Roadnamed4[c(7),c(61)]))))

dfnames2d4<-data.frame(rdnamesd4,a1d4)

a2ttd4<-c(as.character(paste(factor(Roadnamed4[c(9),c(1)]))))
a2d4<-c(as.numeric(paste(factor(Roadnamed4[c(9),c(2)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(3)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(4)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(5)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(6)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(7)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(8)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(9)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(10)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(11)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(12)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(13)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(14)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(15)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(16)]))),
        as.numeric(paste(factor(Roadnamed4[c(9),c(17)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(18)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(19)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(20)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(21)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(22)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(23)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(24)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(25)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(26)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(27)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(28)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(29)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(30)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(31)]))),
        as.numeric(paste(factor(Roadnamed4[c(9),c(32)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(33)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(34)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(35)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(36)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(37)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(38)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(39)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(40)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(41)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(42)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(43)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(44)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(45)])))
        ,as.numeric(paste(factor(Roadnamed4[c(9),c(46)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(47)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(48)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(49)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(50)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(51)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(52)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(53)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(54)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(55)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(56)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(57)]))),
        as.numeric(paste(factor(Roadnamed4[c(9),c(58)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(59)]))),as.numeric(paste(factor(Roadnamed4[c(9),c(60)]))), as.numeric(paste(factor(Roadnamed4[c(9),c(61)]))))


dfnames3d4<-data.frame(rdnamesd4,a2d4)

a3ttd4<-c(as.character(paste(factor(Roadnamed4[c(11),c(1)]))))

a3d4<-c(as.numeric(paste(factor(Roadnamed4[c(11),c(2)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(3)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(4)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(5)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(6)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(7)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(8)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(9)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(10)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(11)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(12)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(13)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(14)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(15)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(16)]))),
        as.numeric(paste(factor(Roadnamed4[c(11),c(17)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(18)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(19)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(20)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(21)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(22)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(23)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(24)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(25)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(26)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(27)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(28)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(29)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(30)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(31)]))),
        as.numeric(paste(factor(Roadnamed4[c(11),c(32)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(33)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(34)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(35)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(36)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(37)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(38)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(39)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(40)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(41)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(42)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(43)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(44)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(45)])))
        ,as.numeric(paste(factor(Roadnamed4[c(11),c(46)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(47)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(48)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(49)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(50)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(51)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(52)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(53)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(54)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(55)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(56)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(57)]))),
        as.numeric(paste(factor(Roadnamed4[c(11),c(58)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(59)]))),as.numeric(paste(factor(Roadnamed4[c(11),c(60)]))), as.numeric(paste(factor(Roadnamed4[c(11),c(61)]))))


dfnames4d4<-data.frame(rdnamesd4,a3d4)


a4ttd4<-c(as.character(paste(factor(Roadnamed4[c(13),c(1)]))))

a4d4<-c(as.numeric(paste(factor(Roadnamed4[c(13),c(2)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(3)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(4)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(5)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(6)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(7)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(8)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(9)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(10)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(11)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(12)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(13)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(14)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(15)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(16)]))),
        as.numeric(paste(factor(Roadnamed4[c(13),c(17)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(18)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(19)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(20)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(21)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(22)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(23)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(24)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(25)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(26)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(27)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(28)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(29)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(30)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(31)]))),
        as.numeric(paste(factor(Roadnamed4[c(13),c(32)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(33)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(34)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(35)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(36)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(37)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(38)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(39)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(40)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(41)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(42)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(43)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(44)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(45)])))
        ,as.numeric(paste(factor(Roadnamed4[c(13),c(46)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(47)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(48)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(49)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(50)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(51)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(52)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(53)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(54)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(55)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(56)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(57)]))),
        as.numeric(paste(factor(Roadnamed4[c(13),c(58)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(59)]))),as.numeric(paste(factor(Roadnamed4[c(13),c(60)]))), as.numeric(paste(factor(Roadnamed4[c(13),c(61)]))))

dfnames5d4<-data.frame(rdnamesd4,a4d4)

################################################################
###########      Respondent Type    ###################################################


Respontyped4<-mydata3[c(2:92),c(2,105:110)]

restyped4<-c(as.character(paste(factor(Respontyped4[c(2),c(2)]))),as.character(paste(factor(Respontyped4[c(2),c(3)]))),as.character(paste(factor(Respontyped4[c(2),c(4)]))), as.character(paste(factor(Respontyped4[c(2),c(5)]))), as.character(paste(factor(Respontyped4[c(2),c(6)]))), as.character(paste(factor(Respontyped4[c(2),c(7)]))), as.character(paste(factor(Respontyped4[c(2),c(8)]))), as.character(paste(factor(Respontyped4[c(2),c(9)]))),as.character(paste(factor(Respontyped4[c(2),c(10)]))),as.character(paste(factor(Respontyped4[c(2),c(11)]))),as.character(paste(factor(Respontyped4[c(2),c(12)]))), as.character(paste(factor(Respontyped4[c(2),c(13)]))))

very.disttRrestyped4<-c(as.character(paste(factor(Respontyped4[c(5),c(1)]))))
very.disRestyped4<-c(as.numeric(paste(factor(Respontyped4[c(5),c(2)]))),as.numeric(paste(factor(Respontyped4[c(5),c(3)]))),as.numeric(paste(factor(Respontyped4[c(5),c(4)]))), as.numeric(paste(factor(Respontyped4[c(5),c(5)]))), as.numeric(paste(factor(Respontyped4[c(5),c(6)]))), as.numeric(paste(factor(Respontyped4[c(5),c(7)]))), as.numeric(paste(factor(Respontyped4[c(5),c(8)]))), as.numeric(paste(factor(Respontyped4[c(5),c(9)]))),as.numeric(paste(factor(Respontyped4[c(5),c(10)]))),as.numeric(paste(factor(Respontyped4[c(5),c(11)]))),as.numeric(paste(factor(Respontyped4[c(5),c(12)]))), as.numeric(paste(factor(Respontyped4[c(5),c(13)]))))
dfRestypesd4<-data.frame(restyped4,very.disRestyped4)          

distt.restyped4<-c(as.character(paste(factor(Respontyped4[c(7),c(1)]))))
disRestyped4<-c(as.numeric(paste(factor(Respontyped4[c(7),c(2)]))),as.numeric(paste(factor(Respontyped4[c(7),c(3)]))),as.numeric(paste(factor(Respontyped4[c(7),c(4)]))), as.numeric(paste(factor(Respontyped4[c(7),c(5)]))), as.numeric(paste(factor(Respontyped4[c(7),c(6)]))), as.numeric(paste(factor(Respontyped4[c(7),c(7)]))), as.numeric(paste(factor(Respontyped4[c(7),c(8)]))), as.numeric(paste(factor(Respontyped4[c(7),c(9)]))),as.numeric(paste(factor(Respontyped4[c(7),c(10)]))),as.numeric(paste(factor(Respontyped4[c(7),c(11)]))),as.numeric(paste(factor(Respontyped4[c(7),c(12)]))), as.numeric(paste(factor(Respontyped4[c(7),c(13)]))))
dfRestypes2d4<-data.frame(restyped4,disRestyped4)


neiSaNorDisttRestyped4<-c(as.character(paste(factor(Respontyped4[c(9),c(1)]))))

neiSatNorDisRestyped4<-c(as.numeric(paste(factor(Respontyped4[c(9),c(2)]))),as.numeric(paste(factor(Respontyped4[c(9),c(3)]))),as.numeric(paste(factor(Respontyped4[c(9),c(4)]))), as.numeric(paste(factor(Respontyped4[c(9),c(5)]))), as.numeric(paste(factor(Respontyped4[c(9),c(6)]))), as.numeric(paste(factor(Respontyped4[c(9),c(7)]))), as.numeric(paste(factor(Respontyped4[c(9),c(8)]))), as.numeric(paste(factor(Respontyped4[c(9),c(9)]))),as.numeric(paste(factor(Respontyped4[c(9),c(10)]))),as.numeric(paste(factor(Respontyped4[c(9),c(11)]))),as.numeric(paste(factor(Respontyped4[c(9),c(12)]))), as.numeric(paste(factor(Respontyped4[c(9),c(13)]))))
dfRestypes3d4<-data.frame(restyped4,neiSatNorDisRestyped4)


satisfiedttRestyped4<-c(as.character(paste(factor(Respontyped4[c(11),c(1)]))))

satisfiedRestyped4<-c(as.numeric(paste(factor(Respontyped4[c(11),c(2)]))),as.numeric(paste(factor(Respontyped4[c(11),c(3)]))),as.numeric(paste(factor(Respontyped4[c(11),c(4)]))), as.numeric(paste(factor(Respontyped4[c(11),c(5)]))), as.numeric(paste(factor(Respontyped4[c(11),c(6)]))), as.numeric(paste(factor(Respontyped4[c(11),c(7)]))), as.numeric(paste(factor(Respontyped4[c(11),c(8)]))), as.numeric(paste(factor(Respontyped4[c(11),c(9)]))),as.numeric(paste(factor(Respontyped4[c(11),c(10)]))),as.numeric(paste(factor(Respontyped4[c(11),c(11)]))),as.numeric(paste(factor(Respontyped4[c(11),c(12)]))), as.numeric(paste(factor(Respontyped4[c(11),c(13)]))))
dfRestypes4<-data.frame(restyped4,satisfiedRestyped4)

verysattRestyped4<-c(as.character(paste(factor(Respontyped4[c(13),c(1)]))))

verySatRestyped4<-c(as.numeric(paste(factor(Respontyped4[c(13),c(2)]))),as.numeric(paste(factor(Respontyped4[c(13),c(3)]))),as.numeric(paste(factor(Respontyped4[c(13),c(4)]))), as.numeric(paste(factor(Respontyped4[c(13),c(5)]))), as.numeric(paste(factor(Respontyped4[c(13),c(6)]))), as.numeric(paste(factor(Respontyped4[c(13),c(7)]))), as.numeric(paste(factor(Respontyped4[c(13),c(8)]))), as.numeric(paste(factor(Respontyped4[c(13),c(9)]))),as.numeric(paste(factor(Respontyped4[c(13),c(10)]))),as.numeric(paste(factor(Respontyped4[c(13),c(11)]))),as.numeric(paste(factor(Respontyped4[c(13),c(12)]))), as.numeric(paste(factor(Respontyped4[c(13),c(13)]))))
dfRestypes5d4<-data.frame(restyped4,verySatRestyped4)
################################################################
###########      Age bracket    ###################################################

Agebracketd4<-mydata3[c(2:92),c(2,111:118)]

aged4<-c(as.character(paste(factor(Agebracketd4[c(2),c(2)]))),as.character(paste(factor(Agebracketd4[c(2),c(3)]))),as.character(paste(factor(Agebracketd4[c(2),c(4)]))), as.character(paste(factor(Agebracketd4[c(2),c(5)]))), as.character(paste(factor(Agebracketd4[c(2),c(6)]))), as.character(paste(factor(Agebracketd4[c(2),c(7)]))), as.character(paste(factor(Agebracketd4[c(2),c(8)]))), as.character(paste(factor(Agebracketd4[c(2),c(9)]))))

very.disttAged4<-c(as.character(paste(factor(Agebracketd4[c(5),c(1)]))))
very.disAged4<-c(as.numeric(paste(factor(Agebracketd4[c(5),c(2)]))),as.numeric(paste(factor(Agebracketd4[c(5),c(3)]))),as.numeric(paste(factor(Agebracketd4[c(5),c(4)]))), as.numeric(paste(factor(Agebracketd4[c(5),c(5)]))), as.numeric(paste(factor(Agebracketd4[c(5),c(6)]))), as.numeric(paste(factor(Agebracketd4[c(5),c(7)]))), as.numeric(paste(factor(Agebracketd4[c(5),c(8)]))), as.numeric(paste(factor(Agebracketd4[c(5),c(9)]))))

dfAged4<-data.frame(aged4,very.disAged4)          

distt.Aged4<-c(as.character(paste(factor(Agebracketd4[c(7),c(1)]))))
disAged4<-c(as.numeric(paste(factor(Agebracketd4[c(7),c(2)]))),as.numeric(paste(factor(Agebracketd4[c(7),c(3)]))),as.numeric(paste(factor(Agebracketd4[c(7),c(4)]))), as.numeric(paste(factor(Agebracketd4[c(7),c(5)]))), as.numeric(paste(factor(Agebracketd4[c(7),c(6)]))), as.numeric(paste(factor(Agebracketd4[c(7),c(7)]))), as.numeric(paste(factor(Agebracketd4[c(7),c(8)]))), as.numeric(paste(factor(Agebracketd4[c(7),c(9)]))))
dfAge2d4<-data.frame(aged4,disAged4)


neiSaNorDistt.Age4d4<-c(as.character(paste(factor(Agebracketd4[c(9),c(1)]))))

neiSatNorDisAged4<-c(as.numeric(paste(factor(Agebracketd4[c(9),c(2)]))),as.numeric(paste(factor(Agebracketd4[c(9),c(3)]))),as.numeric(paste(factor(Agebracketd4[c(9),c(4)]))), as.numeric(paste(factor(Agebracketd4[c(9),c(5)]))), as.numeric(paste(factor(Agebracketd4[c(9),c(6)]))), as.numeric(paste(factor(Agebracketd4[c(9),c(7)]))), as.numeric(paste(factor(Agebracketd4[c(9),c(8)]))), as.numeric(paste(factor(Agebracketd4[c(9),c(9)]))))
dfAge3d4<-data.frame(aged4,neiSatNorDisAged4)         

satisfiedttAged4<-c(as.character(paste(factor(Agebracketd4[c(11),c(1)]))))

satisfiedAged4<-c(as.numeric(paste(factor(Agebracketd4[c(11),c(2)]))),as.numeric(paste(factor(Agebracketd4[c(11),c(3)]))),as.numeric(paste(factor(Agebracketd4[c(11),c(4)]))), as.numeric(paste(factor(Agebracketd4[c(11),c(5)]))), as.numeric(paste(factor(Agebracketd4[c(11),c(6)]))), as.numeric(paste(factor(Agebracketd4[c(11),c(7)]))), as.numeric(paste(factor(Agebracketd4[c(11),c(8)]))), as.numeric(paste(factor(Agebracketd4[c(11),c(9)]))))
dfAge4d4<-data.frame(aged4,satisfiedAged4)

verysatt.Aged4<-c(as.character(paste(factor(Agebracketd4[c(13),c(1)]))))

verySatAged4<-c(as.numeric(paste(factor(Agebracketd4[c(13),c(2)]))),as.numeric(paste(factor(Agebracketd4[c(13),c(3)]))),as.numeric(paste(factor(Agebracketd4[c(13),c(4)]))), as.numeric(paste(factor(Agebracketd4[c(13),c(5)]))), as.numeric(paste(factor(Agebracketd4[c(13),c(6)]))), as.numeric(paste(factor(Agebracketd4[c(13),c(7)]))), as.numeric(paste(factor(Agebracketd4[c(13),c(8)]))), as.numeric(paste(factor(Agebracketd4[c(13),c(9)]))))
dfAge5d4<-data.frame(aged4,verySatAged4)




################################################################################################################################################################################################
################################
mydata4 <- read.csv("ww/UNRA5.csv", header = FALSE)

question7<-as.character(mydata4[c(1),c(1)])

################################Regions
regiond5q<-mydata4[2:33,2:9]

soutthttd5q<-factor(regiond5q[c(2),c(4)])
southernd5q<-as.numeric(paste(factor( regiond5q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(4)])))
var2d5q<-as.character(paste(factor( regiond5q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(1)])))
southd5q<-data.frame(var2d5q,southernd5q)

centttd5q<-factor(regiond5q[c(2),c(5)])
centrald5q<-as.numeric(paste(factor( regiond5q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(5)])))
centrd5q<-data.frame(var2d5q,centrald5q)

easttd5q<-factor(regiond5q[c(2),c(6)])
easternd5q<-as.numeric(paste(factor( regiond5q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(6)])))
eastd5q<-data.frame(var2d5q,easternd5q)

northttd5q<-factor(regiond5q[c(2),c(7)])
northernd5q<-as.numeric(paste(factor( regiond5q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(7)])))
northd5q<-data.frame(var2d5q,northernd5q)

westttd5q<-factor(regiond5q[c(2),c(8)])
westernd5q<-as.numeric(paste(factor( regiond5q[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(8)])))
westd5q<-data.frame(var2d5q,westernd5q)


################################DISTRICTS.
districtsd5<-mydata4[c(2:33),c(2,10:40)]

discsd5<-c(as.character(paste(factor(districtsd5[c(2),c(2)]))),as.character(paste(factor(districtsd5[c(2),c(3)]))),as.character(paste(factor(districtsd5[c(2),c(4)]))), as.character(paste(factor(districtsd5[c(2),c(5)]))), as.character(paste(factor(districtsd5[c(2),c(6)]))), as.character(paste(factor(districtsd5[c(2),c(7)]))), as.character(paste(factor(districtsd5[c(2),c(8)]))), as.character(paste(factor(districtsd5[c(2),c(9)]))),as.character(paste(factor(districtsd5[c(2),c(10)]))),as.character(paste(factor(districtsd5[c(2),c(11)]))),as.character(paste(factor(districtsd5[c(2),c(12)]))), as.character(paste(factor(districtsd5[c(2),c(13)]))),as.character(paste(factor(districtsd5[c(2),c(14)]))),as.character(paste(factor(districtsd5[c(2),c(15)]))),as.character(paste(factor(districtsd5[c(2),c(16)]))),
           as.character(paste(factor(districtsd5[c(2),c(17)]))),as.character(paste(factor(districtsd5[c(2),c(18)]))),as.character(paste(factor(districtsd5[c(2),c(19)]))),as.character(paste(factor(districtsd5[c(2),c(20)]))),as.character(paste(factor(districtsd5[c(2),c(21)]))),as.character(paste(factor(districtsd5[c(2),c(22)]))),as.character(paste(factor(districtsd5[c(2),c(23)]))),as.character(paste(factor(districtsd5[c(2),c(24)]))),as.character(paste(factor(districtsd5[c(2),c(25)]))),as.character(paste(factor(districtsd5[c(2),c(26)]))),as.character(paste(factor(districtsd5[c(2),c(27)]))),as.character(paste(factor(districtsd5[c(2),c(28)]))),as.character(paste(factor(districtsd5[c(2),c(29)]))),as.character(paste(factor(districtsd5[c(2),c(30)]))),as.character(paste(factor(districtsd5[c(2),c(31)]))),
           as.character(paste(factor(districtsd5[c(2),c(32)]))),as.character(paste(factor(districtsd5[c(2),c(33)]))),as.character(paste(factor(districtsd5[c(2),c(34)]))),as.character(paste(factor(districtsd5[c(2),c(35)]))),as.character(paste(factor(districtsd5[c(2),c(36)]))),as.character(paste(factor(districtsd5[c(2),c(37)]))),as.character(paste(factor(districtsd5[c(2),c(38)]))),as.character(paste(factor(districtsd5[c(2),c(39)]))),as.character(paste(factor(districtsd5[c(2),c(40)]))),as.character(paste(factor(districtsd5[c(2),c(41)]))),as.character(paste(factor(districtsd5[c(2),c(42)]))),as.character(paste(factor(districtsd5[c(2),c(43)]))),as.character(paste(factor(districtsd5[c(2),c(44)]))),as.character(paste(factor(districtsd5[c(2),c(45)]))))


a1ttd5<-c(as.character(paste(factor(districtsd5[c(5),c(1)]))))
a1d5<-c(as.numeric(paste(factor(districtsd5[c(5),c(2)]))),as.numeric(paste(factor(districtsd5[c(5),c(3)]))),as.numeric(paste(factor(districtsd5[c(5),c(4)]))), as.numeric(paste(factor(districtsd5[c(5),c(5)]))), as.numeric(paste(factor(districtsd5[c(5),c(6)]))), as.numeric(paste(factor(districtsd5[c(5),c(7)]))), as.numeric(paste(factor(districtsd5[c(5),c(8)]))), as.numeric(paste(factor(districtsd5[c(5),c(9)]))),as.numeric(paste(factor(districtsd5[c(5),c(10)]))),as.numeric(paste(factor(districtsd5[c(5),c(11)]))),as.numeric(paste(factor(districtsd5[c(5),c(12)]))), as.numeric(paste(factor(districtsd5[c(5),c(13)]))),as.numeric(paste(factor(districtsd5[c(5),c(14)]))),as.numeric(paste(factor(districtsd5[c(5),c(15)]))),as.numeric(paste(factor(districtsd5[c(5),c(16)]))),
        as.numeric(paste(factor(districtsd5[c(5),c(17)]))),as.numeric(paste(factor(districtsd5[c(5),c(18)]))),as.numeric(paste(factor(districtsd5[c(5),c(19)]))),as.numeric(paste(factor(districtsd5[c(5),c(20)]))),as.numeric(paste(factor(districtsd5[c(5),c(21)]))),as.numeric(paste(factor(districtsd5[c(5),c(22)]))),as.numeric(paste(factor(districtsd5[c(5),c(23)]))),as.numeric(paste(factor(districtsd5[c(5),c(24)]))),as.numeric(paste(factor(districtsd5[c(5),c(25)]))),as.numeric(paste(factor(districtsd5[c(5),c(26)]))),as.numeric(paste(factor(districtsd5[c(5),c(27)]))),as.numeric(paste(factor(districtsd5[c(5),c(28)]))),as.numeric(paste(factor(districtsd5[c(5),c(29)]))),as.numeric(paste(factor(districtsd5[c(5),c(30)]))),as.numeric(paste(factor(districtsd5[c(5),c(31)]))),
        as.numeric(paste(factor(districtsd5[c(5),c(32)]))),as.numeric(paste(factor(districtsd5[c(5),c(33)]))),as.numeric(paste(factor(districtsd5[c(5),c(34)]))),as.numeric(paste(factor(districtsd5[c(5),c(35)]))),as.numeric(paste(factor(districtsd5[c(5),c(36)]))),as.numeric(paste(factor(districtsd5[c(5),c(37)]))),as.numeric(paste(factor(districtsd5[c(5),c(38)]))),as.numeric(paste(factor(districtsd5[c(5),c(39)]))),as.numeric(paste(factor(districtsd5[c(5),c(40)]))),as.numeric(paste(factor(districtsd5[c(5),c(41)]))),as.numeric(paste(factor(districtsd5[c(5),c(42)]))),as.numeric(paste(factor(districtsd5[c(5),c(43)]))),as.numeric(paste(factor(districtsd5[c(5),c(44)]))),as.numeric(paste(factor(districtsd5[c(5),c(45)]))))
df1d5<-data.frame(discsd5,a1d5)


a2ttd5<-c(as.character(paste(factor(districtsd5[c(7),c(1)]))))
a2d5<-c(as.numeric(paste(factor(districtsd5[c(7),c(2)]))),as.numeric(paste(factor(districtsd5[c(7),c(3)]))),as.numeric(paste(factor(districtsd5[c(7),c(4)]))), as.numeric(paste(factor(districtsd5[c(7),c(5)]))), as.numeric(paste(factor(districtsd5[c(7),c(6)]))), as.numeric(paste(factor(districtsd5[c(7),c(7)]))), as.numeric(paste(factor(districtsd5[c(7),c(8)]))), as.numeric(paste(factor(districtsd5[c(7),c(9)]))),as.numeric(paste(factor(districtsd5[c(7),c(10)]))),as.numeric(paste(factor(districtsd5[c(7),c(11)]))),as.numeric(paste(factor(districtsd5[c(7),c(12)]))), as.numeric(paste(factor(districtsd5[c(7),c(13)]))),as.numeric(paste(factor(districtsd5[c(7),c(14)]))),as.numeric(paste(factor(districtsd5[c(7),c(15)]))),as.numeric(paste(factor(districtsd5[c(7),c(16)]))),
        as.numeric(paste(factor(districtsd5[c(7),c(17)]))),as.numeric(paste(factor(districtsd5[c(7),c(18)]))),as.numeric(paste(factor(districtsd5[c(7),c(19)]))),as.numeric(paste(factor(districtsd5[c(7),c(20)]))),as.numeric(paste(factor(districtsd5[c(7),c(21)]))),as.numeric(paste(factor(districtsd5[c(7),c(22)]))),as.numeric(paste(factor(districtsd5[c(7),c(23)]))),as.numeric(paste(factor(districtsd5[c(7),c(24)]))),as.numeric(paste(factor(districtsd5[c(7),c(25)]))),as.numeric(paste(factor(districtsd5[c(7),c(26)]))),as.numeric(paste(factor(districtsd5[c(7),c(27)]))),as.numeric(paste(factor(districtsd5[c(7),c(28)]))),as.numeric(paste(factor(districtsd5[c(7),c(29)]))),as.numeric(paste(factor(districtsd5[c(7),c(30)]))),as.numeric(paste(factor(districtsd5[c(7),c(31)]))),
        as.numeric(paste(factor(districtsd5[c(7),c(32)]))),as.numeric(paste(factor(districtsd5[c(7),c(33)]))),as.numeric(paste(factor(districtsd5[c(7),c(34)]))),as.numeric(paste(factor(districtsd5[c(7),c(35)]))),as.numeric(paste(factor(districtsd5[c(7),c(36)]))),as.numeric(paste(factor(districtsd5[c(7),c(37)]))),as.numeric(paste(factor(districtsd5[c(7),c(38)]))),as.numeric(paste(factor(districtsd5[c(7),c(39)]))),as.numeric(paste(factor(districtsd5[c(7),c(40)]))),as.numeric(paste(factor(districtsd5[c(7),c(41)]))),as.numeric(paste(factor(districtsd5[c(7),c(42)]))),as.numeric(paste(factor(districtsd5[c(7),c(43)]))),as.numeric(paste(factor(districtsd5[c(7),c(44)]))),as.numeric(paste(factor(districtsd5[c(7),c(45)]))))
df2d5<-data.frame(discsd5,a2d5)


a3ttd5<-c(as.character(paste(factor(districtsd5[c(9),c(1)]))))
a3d5 <-c(as.numeric(paste(factor(districtsd5[c(9),c(2)]))),as.numeric(paste(factor(districtsd5[c(9),c(3)]))),as.numeric(paste(factor(districtsd5[c(9),c(4)]))), as.numeric(paste(factor(districtsd5[c(9),c(5)]))), as.numeric(paste(factor(districtsd5[c(9),c(6)]))), as.numeric(paste(factor(districtsd5[c(9),c(7)]))), as.numeric(paste(factor(districtsd5[c(9),c(8)]))), as.numeric(paste(factor(districtsd5[c(9),c(9)]))),as.numeric(paste(factor(districtsd5[c(9),c(10)]))),as.numeric(paste(factor(districtsd5[c(9),c(11)]))),as.numeric(paste(factor(districtsd5[c(9),c(12)]))), as.numeric(paste(factor(districtsd5[c(9),c(13)]))),as.numeric(paste(factor(districtsd5[c(9),c(14)]))),as.numeric(paste(factor(districtsd5[c(9),c(15)]))),as.numeric(paste(factor(districtsd5[c(9),c(16)]))),
         as.numeric(paste(factor(districtsd5[c(9),c(17)]))),as.numeric(paste(factor(districtsd5[c(9),c(18)]))),as.numeric(paste(factor(districtsd5[c(9),c(19)]))),as.numeric(paste(factor(districtsd5[c(9),c(20)]))),as.numeric(paste(factor(districtsd5[c(9),c(21)]))),as.numeric(paste(factor(districtsd5[c(9),c(22)]))),as.numeric(paste(factor(districtsd5[c(9),c(23)]))),as.numeric(paste(factor(districtsd5[c(9),c(24)]))),as.numeric(paste(factor(districtsd5[c(9),c(25)]))),as.numeric(paste(factor(districtsd5[c(9),c(26)]))),as.numeric(paste(factor(districtsd5[c(9),c(27)]))),as.numeric(paste(factor(districtsd5[c(9),c(28)]))),as.numeric(paste(factor(districtsd5[c(9),c(29)]))),as.numeric(paste(factor(districtsd5[c(9),c(30)]))),as.numeric(paste(factor(districtsd5[c(9),c(31)]))),
         as.numeric(paste(factor(districtsd5[c(9),c(32)]))),as.numeric(paste(factor(districtsd5[c(9),c(33)]))),as.numeric(paste(factor(districtsd5[c(9),c(34)]))),as.numeric(paste(factor(districtsd5[c(9),c(35)]))),as.numeric(paste(factor(districtsd5[c(9),c(36)]))),as.numeric(paste(factor(districtsd5[c(9),c(37)]))),as.numeric(paste(factor(districtsd5[c(9),c(38)]))),as.numeric(paste(factor(districtsd5[c(9),c(39)]))),as.numeric(paste(factor(districtsd5[c(9),c(40)]))),as.numeric(paste(factor(districtsd5[c(9),c(41)]))),as.numeric(paste(factor(districtsd5[c(9),c(42)]))),as.numeric(paste(factor(districtsd5[c(9),c(43)]))),as.numeric(paste(factor(districtsd5[c(9),c(44)]))),as.numeric(paste(factor(districtsd5[c(9),c(45)]))))
df3d5<-data.frame(discsd5,a3d5)


a4ttd5<-c(as.character(paste(factor(districtsd5[c(11),c(1)]))))
a4d5<-c(as.numeric(paste(factor(districtsd5[c(11),c(2)]))),as.numeric(paste(factor(districtsd5[c(11),c(3)]))),as.numeric(paste(factor(districtsd5[c(11),c(4)]))), as.numeric(paste(factor(districtsd5[c(11),c(5)]))), as.numeric(paste(factor(districtsd5[c(11),c(6)]))), as.numeric(paste(factor(districtsd5[c(11),c(7)]))), as.numeric(paste(factor(districtsd5[c(11),c(8)]))), as.numeric(paste(factor(districtsd5[c(11),c(9)]))),as.numeric(paste(factor(districtsd5[c(11),c(10)]))),as.numeric(paste(factor(districtsd5[c(11),c(11)]))),as.numeric(paste(factor(districtsd5[c(11),c(12)]))), as.numeric(paste(factor(districtsd5[c(11),c(13)]))),as.numeric(paste(factor(districtsd5[c(11),c(14)]))),as.numeric(paste(factor(districtsd5[c(11),c(15)]))),as.numeric(paste(factor(districtsd5[c(11),c(16)]))),
        as.numeric(paste(factor(districtsd5[c(11),c(17)]))),as.numeric(paste(factor(districtsd5[c(11),c(18)]))),as.numeric(paste(factor(districtsd5[c(11),c(19)]))),as.numeric(paste(factor(districtsd5[c(11),c(20)]))),as.numeric(paste(factor(districtsd5[c(11),c(21)]))),as.numeric(paste(factor(districtsd5[c(11),c(22)]))),as.numeric(paste(factor(districtsd5[c(11),c(23)]))),as.numeric(paste(factor(districtsd5[c(11),c(24)]))),as.numeric(paste(factor(districtsd5[c(11),c(25)]))),as.numeric(paste(factor(districtsd5[c(11),c(26)]))),as.numeric(paste(factor(districtsd5[c(11),c(27)]))),as.numeric(paste(factor(districtsd5[c(11),c(28)]))),as.numeric(paste(factor(districtsd5[c(11),c(29)]))),as.numeric(paste(factor(districtsd5[c(11),c(30)]))),as.numeric(paste(factor(districtsd5[c(11),c(31)]))),
        as.numeric(paste(factor(districtsd5[c(11),c(32)]))),as.numeric(paste(factor(districtsd5[c(11),c(33)]))),as.numeric(paste(factor(districtsd5[c(11),c(34)]))),as.numeric(paste(factor(districtsd5[c(11),c(35)]))),as.numeric(paste(factor(districtsd5[c(11),c(36)]))),as.numeric(paste(factor(districtsd5[c(11),c(37)]))),as.numeric(paste(factor(districtsd5[c(11),c(38)]))),as.numeric(paste(factor(districtsd5[c(11),c(39)]))),as.numeric(paste(factor(districtsd5[c(11),c(40)]))),as.numeric(paste(factor(districtsd5[c(11),c(41)]))),as.numeric(paste(factor(districtsd5[c(11),c(42)]))),as.numeric(paste(factor(districtsd5[c(11),c(43)]))),as.numeric(paste(factor(districtsd5[c(11),c(44)]))),as.numeric(paste(factor(districtsd5[c(11),c(45)]))))
df4d5<-data.frame(discsd5,a4d5)

a5ttd5<-c(as.character(paste(factor(districtsd5[c(13),c(1)]))))
a5d5<-c(as.numeric(paste(factor(districtsd5[c(13),c(2)]))),as.numeric(paste(factor(districtsd5[c(13),c(3)]))),as.numeric(paste(factor(districtsd5[c(13),c(4)]))), as.numeric(paste(factor(districtsd5[c(13),c(5)]))), as.numeric(paste(factor(districtsd5[c(13),c(6)]))), as.numeric(paste(factor(districtsd5[c(13),c(7)]))), as.numeric(paste(factor(districtsd5[c(13),c(8)]))), as.numeric(paste(factor(districtsd5[c(13),c(9)]))),as.numeric(paste(factor(districtsd5[c(13),c(10)]))),as.numeric(paste(factor(districtsd5[c(13),c(11)]))),as.numeric(paste(factor(districtsd5[c(13),c(12)]))), as.numeric(paste(factor(districtsd5[c(13),c(13)]))),as.numeric(paste(factor(districtsd5[c(13),c(14)]))),as.numeric(paste(factor(districtsd5[c(13),c(15)]))),as.numeric(paste(factor(districtsd5[c(13),c(16)]))),
        as.numeric(paste(factor(districtsd5[c(13),c(17)]))),as.numeric(paste(factor(districtsd5[c(13),c(18)]))),as.numeric(paste(factor(districtsd5[c(13),c(19)]))),as.numeric(paste(factor(districtsd5[c(13),c(20)]))),as.numeric(paste(factor(districtsd5[c(13),c(21)]))),as.numeric(paste(factor(districtsd5[c(13),c(22)]))),as.numeric(paste(factor(districtsd5[c(13),c(23)]))),as.numeric(paste(factor(districtsd5[c(13),c(24)]))),as.numeric(paste(factor(districtsd5[c(13),c(25)]))),as.numeric(paste(factor(districtsd5[c(13),c(26)]))),as.numeric(paste(factor(districtsd5[c(13),c(27)]))),as.numeric(paste(factor(districtsd5[c(13),c(28)]))),as.numeric(paste(factor(districtsd5[c(13),c(29)]))),as.numeric(paste(factor(districtsd5[c(13),c(30)]))),as.numeric(paste(factor(districtsd5[c(13),c(31)]))),
        as.numeric(paste(factor(districtsd5[c(13),c(32)]))),as.numeric(paste(factor(districtsd5[c(13),c(33)]))),as.numeric(paste(factor(districtsd5[c(13),c(34)]))),as.numeric(paste(factor(districtsd5[c(13),c(35)]))),as.numeric(paste(factor(districtsd5[c(13),c(36)]))),as.numeric(paste(factor(districtsd5[c(13),c(37)]))),as.numeric(paste(factor(districtsd5[c(13),c(38)]))),as.numeric(paste(factor(districtsd5[c(13),c(39)]))),as.numeric(paste(factor(districtsd5[c(13),c(40)]))),as.numeric(paste(factor(districtsd5[c(13),c(41)]))),as.numeric(paste(factor(districtsd5[c(13),c(42)]))),as.numeric(paste(factor(districtsd5[c(13),c(43)]))),as.numeric(paste(factor(districtsd5[c(13),c(44)]))),as.numeric(paste(factor(districtsd5[c(13),c(45)]))))
df5d5<-data.frame(discsd5,a5d5)



################################road type.
roadtyped5<-mydata4[c(2:33),c(2,41:42)]

pavedttd5<-factor(roadtyped5[c(2),c(2)])
pavedd5<-as.numeric(paste(factor( roadtyped5[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(2)])))
var2roadtd5<-as.character(paste(factor( roadtyped5[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(1)])))
pavedroadtd5<-data.frame(var2roadtd5,pavedd5)


unpavedttd5<-factor(roadtyped5[c(2),c(3)])
unpavedd5<-as.numeric(paste(factor( roadtyped5[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(3)])))
unpavedroadtd5<-data.frame(var2roadtd5,unpavedd5)

################################respondent gender.
rsgenderd5<-mydata4[c(2:33),c(2,43:44)]

malettd5<-factor(rsgenderd5[c(2),c(2)])
maled5<-as.numeric(paste(factor( rsgenderd5[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(2)])))
var2rsgenderd5<-as.character(paste(factor( rsgenderd5[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(1)])))
maletd5<-data.frame(var2rsgenderd5,maled5)


femalettd5<-factor(rsgenderd5[c(2),c(3)])
femaled5<-as.numeric(paste(factor( rsgenderd5[c(5,7,9,11,13,15,17,19,21,23,25,27,29,31),c(3)])))
femaletd5<-data.frame(var2rsgenderd5,femaled5)


################################.Road names

Roadnamed5<-mydata4[c(2:33),c(2,45:104)]

rdnamesd5<-c(as.character(paste(factor(Roadnamed5[c(2),c(2)]))),as.character(paste(factor(Roadnamed5[c(2),c(3)]))),as.character(paste(factor(Roadnamed5[c(2),c(4)]))), as.character(paste(factor(Roadnamed5[c(2),c(5)]))), as.character(paste(factor(Roadnamed5[c(2),c(6)]))), as.character(paste(factor(Roadnamed5[c(2),c(7)]))), as.character(paste(factor(Roadnamed5[c(2),c(8)]))), as.character(paste(factor(Roadnamed5[c(2),c(9)]))),as.character(paste(factor(Roadnamed5[c(2),c(10)]))),as.character(paste(factor(Roadnamed5[c(2),c(11)]))),as.character(paste(factor(Roadnamed5[c(2),c(12)]))), as.character(paste(factor(Roadnamed5[c(2),c(13)]))),as.character(paste(factor(Roadnamed5[c(2),c(14)]))),as.character(paste(factor(Roadnamed5[c(2),c(15)]))),as.character(paste(factor(Roadnamed5[c(2),c(16)]))),
             as.character(paste(factor(Roadnamed5[c(2),c(17)]))),as.character(paste(factor(Roadnamed5[c(2),c(18)]))),as.character(paste(factor(Roadnamed5[c(2),c(19)]))),as.character(paste(factor(Roadnamed5[c(2),c(20)]))),as.character(paste(factor(Roadnamed5[c(2),c(21)]))),as.character(paste(factor(Roadnamed5[c(2),c(22)]))),as.character(paste(factor(Roadnamed5[c(2),c(23)]))),as.character(paste(factor(Roadnamed5[c(2),c(24)]))),as.character(paste(factor(Roadnamed5[c(2),c(25)]))),as.character(paste(factor(Roadnamed5[c(2),c(26)]))),as.character(paste(factor(Roadnamed5[c(2),c(27)]))),as.character(paste(factor(Roadnamed5[c(2),c(28)]))),as.character(paste(factor(Roadnamed5[c(2),c(29)]))),as.character(paste(factor(Roadnamed5[c(2),c(30)]))),as.character(paste(factor(Roadnamed5[c(2),c(31)]))),
             as.character(paste(factor(Roadnamed5[c(2),c(32)]))),as.character(paste(factor(Roadnamed5[c(2),c(33)]))),as.character(paste(factor(Roadnamed5[c(2),c(34)]))),as.character(paste(factor(Roadnamed5[c(2),c(35)]))),as.character(paste(factor(Roadnamed5[c(2),c(36)]))),as.character(paste(factor(Roadnamed5[c(2),c(37)]))),as.character(paste(factor(Roadnamed5[c(2),c(38)]))),as.character(paste(factor(Roadnamed5[c(2),c(39)]))),as.character(paste(factor(Roadnamed5[c(2),c(40)]))),as.character(paste(factor(Roadnamed5[c(2),c(41)]))),as.character(paste(factor(Roadnamed5[c(2),c(42)]))),as.character(paste(factor(Roadnamed5[c(2),c(43)]))),as.character(paste(factor(Roadnamed5[c(2),c(44)]))),as.character(paste(factor(Roadnamed5[c(2),c(45)])))
             ,as.character(paste(factor(Roadnamed5[c(2),c(46)]))),as.character(paste(factor(Roadnamed5[c(2),c(47)]))),as.character(paste(factor(Roadnamed5[c(2),c(48)]))), as.character(paste(factor(Roadnamed5[c(2),c(49)]))), as.character(paste(factor(Roadnamed5[c(2),c(50)]))), as.character(paste(factor(Roadnamed5[c(2),c(51)]))), as.character(paste(factor(Roadnamed5[c(2),c(52)]))), as.character(paste(factor(Roadnamed5[c(2),c(53)]))),as.character(paste(factor(Roadnamed5[c(2),c(54)]))),as.character(paste(factor(Roadnamed5[c(2),c(55)]))),as.character(paste(factor(Roadnamed5[c(2),c(56)]))), as.character(paste(factor(Roadnamed5[c(2),c(57)]))),
             as.character(paste(factor(Roadnamed5[c(2),c(58)]))),as.character(paste(factor(Roadnamed5[c(2),c(59)]))),as.character(paste(factor(Roadnamed5[c(2),c(60)]))), as.character(paste(factor(Roadnamed5[c(2),c(61)]))))


attd5<-c(as.character(paste(factor(Roadnamed5[c(5),c(1)]))))
ad5<-c(as.numeric(paste(factor(Roadnamed5[c(5),c(2)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(3)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(4)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(5)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(6)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(7)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(8)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(9)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(10)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(11)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(12)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(13)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(14)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(15)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(16)]))),
       as.numeric(paste(factor(Roadnamed5[c(5),c(17)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(18)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(19)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(20)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(21)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(22)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(23)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(24)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(25)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(26)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(27)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(28)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(29)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(30)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(31)]))),
       as.numeric(paste(factor(Roadnamed5[c(5),c(32)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(33)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(34)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(35)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(36)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(37)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(38)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(39)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(40)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(41)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(42)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(43)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(44)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(45)])))
       ,as.numeric(paste(factor(Roadnamed5[c(5),c(46)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(47)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(48)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(49)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(50)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(51)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(52)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(53)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(54)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(55)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(56)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(57)]))),
       as.numeric(paste(factor(Roadnamed5[c(5),c(58)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(59)]))),as.numeric(paste(factor(Roadnamed5[c(5),c(60)]))), as.numeric(paste(factor(Roadnamed5[c(5),c(61)]))))

dfnamesd5<-data.frame(rdnamesd5,ad5)          


a1ttd5<-c(as.character(paste(factor(Roadnamed5[c(7),c(1)]))))
a1d5<-c(as.numeric(paste(factor(Roadnamed5[c(7),c(2)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(3)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(4)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(5)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(6)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(7)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(8)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(9)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(10)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(11)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(12)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(13)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(14)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(15)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(16)]))),
        as.numeric(paste(factor(Roadnamed5[c(7),c(17)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(18)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(19)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(20)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(21)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(22)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(23)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(24)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(25)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(26)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(27)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(28)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(29)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(30)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(31)]))),
        as.numeric(paste(factor(Roadnamed5[c(7),c(32)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(33)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(34)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(35)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(36)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(37)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(38)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(39)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(40)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(41)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(42)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(43)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(44)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(45)])))
        ,as.numeric(paste(factor(Roadnamed5[c(7),c(46)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(47)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(48)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(49)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(50)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(51)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(52)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(53)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(54)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(55)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(56)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(57)]))),
        as.numeric(paste(factor(Roadnamed5[c(7),c(58)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(59)]))),as.numeric(paste(factor(Roadnamed5[c(7),c(60)]))), as.numeric(paste(factor(Roadnamed5[c(7),c(61)]))))

dfnames2d5<-data.frame(rdnamesd5,a1d5)

a2ttd5<-c(as.character(paste(factor(Roadnamed5[c(9),c(1)]))))
a2d5<-c(as.numeric(paste(factor(Roadnamed5[c(9),c(2)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(3)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(4)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(5)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(6)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(7)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(8)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(9)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(10)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(11)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(12)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(13)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(14)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(15)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(16)]))),
        as.numeric(paste(factor(Roadnamed5[c(9),c(17)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(18)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(19)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(20)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(21)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(22)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(23)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(24)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(25)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(26)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(27)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(28)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(29)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(30)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(31)]))),
        as.numeric(paste(factor(Roadnamed5[c(9),c(32)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(33)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(34)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(35)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(36)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(37)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(38)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(39)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(40)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(41)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(42)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(43)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(44)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(45)])))
        ,as.numeric(paste(factor(Roadnamed5[c(9),c(46)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(47)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(48)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(49)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(50)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(51)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(52)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(53)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(54)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(55)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(56)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(57)]))),
        as.numeric(paste(factor(Roadnamed5[c(9),c(58)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(59)]))),as.numeric(paste(factor(Roadnamed5[c(9),c(60)]))), as.numeric(paste(factor(Roadnamed5[c(9),c(61)]))))


dfnames3d5<-data.frame(rdnamesd5,a2d5)

a3ttd5<-c(as.character(paste(factor(Roadnamed5[c(11),c(1)]))))

a3d5<-c(as.numeric(paste(factor(Roadnamed5[c(11),c(2)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(3)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(4)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(5)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(6)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(7)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(8)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(9)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(10)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(11)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(12)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(13)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(14)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(15)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(16)]))),
        as.numeric(paste(factor(Roadnamed5[c(11),c(17)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(18)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(19)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(20)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(21)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(22)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(23)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(24)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(25)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(26)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(27)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(28)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(29)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(30)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(31)]))),
        as.numeric(paste(factor(Roadnamed5[c(11),c(32)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(33)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(34)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(35)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(36)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(37)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(38)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(39)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(40)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(41)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(42)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(43)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(44)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(45)])))
        ,as.numeric(paste(factor(Roadnamed5[c(11),c(46)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(47)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(48)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(49)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(50)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(51)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(52)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(53)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(54)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(55)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(56)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(57)]))),
        as.numeric(paste(factor(Roadnamed5[c(11),c(58)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(59)]))),as.numeric(paste(factor(Roadnamed5[c(11),c(60)]))), as.numeric(paste(factor(Roadnamed5[c(11),c(61)]))))


dfnames4d5<-data.frame(rdnamesd5,a3d5)


a4ttd5<-c(as.character(paste(factor(Roadnamed5[c(13),c(1)]))))

a4d5<-c(as.numeric(paste(factor(Roadnamed5[c(13),c(2)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(3)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(4)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(5)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(6)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(7)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(8)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(9)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(10)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(11)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(12)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(13)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(14)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(15)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(16)]))),
        as.numeric(paste(factor(Roadnamed5[c(13),c(17)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(18)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(19)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(20)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(21)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(22)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(23)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(24)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(25)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(26)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(27)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(28)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(29)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(30)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(31)]))),
        as.numeric(paste(factor(Roadnamed5[c(13),c(32)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(33)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(34)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(35)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(36)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(37)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(38)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(39)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(40)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(41)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(42)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(43)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(44)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(45)])))
        ,as.numeric(paste(factor(Roadnamed5[c(13),c(46)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(47)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(48)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(49)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(50)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(51)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(52)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(53)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(54)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(55)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(56)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(57)]))),
        as.numeric(paste(factor(Roadnamed5[c(13),c(58)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(59)]))),as.numeric(paste(factor(Roadnamed5[c(13),c(60)]))), as.numeric(paste(factor(Roadnamed5[c(13),c(61)]))))

dfnames5d5<-data.frame(rdnamesd5,a4d5)

################################################################
###########      Respondent Type    ###################################################


Respontyped5<-mydata4[c(2:33),c(2,105:110)]

restyped5<-c(as.character(paste(factor(Respontyped5[c(2),c(2)]))),as.character(paste(factor(Respontyped5[c(2),c(3)]))),as.character(paste(factor(Respontyped5[c(2),c(4)]))), as.character(paste(factor(Respontyped5[c(2),c(5)]))), as.character(paste(factor(Respontyped5[c(2),c(6)]))), as.character(paste(factor(Respontyped5[c(2),c(7)]))), as.character(paste(factor(Respontyped5[c(2),c(8)]))), as.character(paste(factor(Respontyped5[c(2),c(9)]))),as.character(paste(factor(Respontyped5[c(2),c(10)]))),as.character(paste(factor(Respontyped5[c(2),c(11)]))),as.character(paste(factor(Respontyped5[c(2),c(12)]))), as.character(paste(factor(Respontyped5[c(2),c(13)]))))

very.disttRrestyped5<-c(as.character(paste(factor(Respontyped5[c(5),c(1)]))))
very.disRestyped5<-c(as.numeric(paste(factor(Respontyped5[c(5),c(2)]))),as.numeric(paste(factor(Respontyped5[c(5),c(3)]))),as.numeric(paste(factor(Respontyped5[c(5),c(4)]))), as.numeric(paste(factor(Respontyped5[c(5),c(5)]))), as.numeric(paste(factor(Respontyped5[c(5),c(6)]))), as.numeric(paste(factor(Respontyped5[c(5),c(7)]))), as.numeric(paste(factor(Respontyped5[c(5),c(8)]))), as.numeric(paste(factor(Respontyped5[c(5),c(9)]))),as.numeric(paste(factor(Respontyped5[c(5),c(10)]))),as.numeric(paste(factor(Respontyped5[c(5),c(11)]))),as.numeric(paste(factor(Respontyped5[c(5),c(12)]))), as.numeric(paste(factor(Respontyped5[c(5),c(13)]))))
dfRestypesd5<-data.frame(restyped5,very.disRestyped5)          

distt.restyped5<-c(as.character(paste(factor(Respontyped5[c(7),c(1)]))))
disRestyped5<-c(as.numeric(paste(factor(Respontyped5[c(7),c(2)]))),as.numeric(paste(factor(Respontyped5[c(7),c(3)]))),as.numeric(paste(factor(Respontyped5[c(7),c(4)]))), as.numeric(paste(factor(Respontyped5[c(7),c(5)]))), as.numeric(paste(factor(Respontyped5[c(7),c(6)]))), as.numeric(paste(factor(Respontyped5[c(7),c(7)]))), as.numeric(paste(factor(Respontyped5[c(7),c(8)]))), as.numeric(paste(factor(Respontyped5[c(7),c(9)]))),as.numeric(paste(factor(Respontyped5[c(7),c(10)]))),as.numeric(paste(factor(Respontyped5[c(7),c(11)]))),as.numeric(paste(factor(Respontyped5[c(7),c(12)]))), as.numeric(paste(factor(Respontyped5[c(7),c(13)]))))
dfRestypes2d5<-data.frame(restyped5,disRestyped5)


neiSaNorDisttRestyped5<-c(as.character(paste(factor(Respontyped5[c(9),c(1)]))))

neiSatNorDisRestyped5<-c(as.numeric(paste(factor(Respontyped5[c(9),c(2)]))),as.numeric(paste(factor(Respontyped5[c(9),c(3)]))),as.numeric(paste(factor(Respontyped5[c(9),c(4)]))), as.numeric(paste(factor(Respontyped5[c(9),c(5)]))), as.numeric(paste(factor(Respontyped5[c(9),c(6)]))), as.numeric(paste(factor(Respontyped5[c(9),c(7)]))), as.numeric(paste(factor(Respontyped5[c(9),c(8)]))), as.numeric(paste(factor(Respontyped5[c(9),c(9)]))),as.numeric(paste(factor(Respontyped5[c(9),c(10)]))),as.numeric(paste(factor(Respontyped5[c(9),c(11)]))),as.numeric(paste(factor(Respontyped5[c(9),c(12)]))), as.numeric(paste(factor(Respontyped5[c(9),c(13)]))))
dfRestypes3d5<-data.frame(restyped5,neiSatNorDisRestyped5)


satisfiedttRestyped5<-c(as.character(paste(factor(Respontyped5[c(11),c(1)]))))

satisfiedRestyped5<-c(as.numeric(paste(factor(Respontyped5[c(11),c(2)]))),as.numeric(paste(factor(Respontyped5[c(11),c(3)]))),as.numeric(paste(factor(Respontyped5[c(11),c(4)]))), as.numeric(paste(factor(Respontyped5[c(11),c(5)]))), as.numeric(paste(factor(Respontyped5[c(11),c(6)]))), as.numeric(paste(factor(Respontyped5[c(11),c(7)]))), as.numeric(paste(factor(Respontyped5[c(11),c(8)]))), as.numeric(paste(factor(Respontyped5[c(11),c(9)]))),as.numeric(paste(factor(Respontyped5[c(11),c(10)]))),as.numeric(paste(factor(Respontyped5[c(11),c(11)]))),as.numeric(paste(factor(Respontyped5[c(11),c(12)]))), as.numeric(paste(factor(Respontyped5[c(11),c(13)]))))
dfRestypes5<-data.frame(restyped5,satisfiedRestyped5)

verysattRestyped5<-c(as.character(paste(factor(Respontyped5[c(13),c(1)]))))

verySatRestyped5<-c(as.numeric(paste(factor(Respontyped5[c(13),c(2)]))),as.numeric(paste(factor(Respontyped5[c(13),c(3)]))),as.numeric(paste(factor(Respontyped5[c(13),c(4)]))), as.numeric(paste(factor(Respontyped5[c(13),c(5)]))), as.numeric(paste(factor(Respontyped5[c(13),c(6)]))), as.numeric(paste(factor(Respontyped5[c(13),c(7)]))), as.numeric(paste(factor(Respontyped5[c(13),c(8)]))), as.numeric(paste(factor(Respontyped5[c(13),c(9)]))),as.numeric(paste(factor(Respontyped5[c(13),c(10)]))),as.numeric(paste(factor(Respontyped5[c(13),c(11)]))),as.numeric(paste(factor(Respontyped5[c(13),c(12)]))), as.numeric(paste(factor(Respontyped5[c(13),c(13)]))))
dfRestypes5d5<-data.frame(restyped5,verySatRestyped5)
################################################################
###########      Age bracket    ###################################################

Agebracketd5<-mydata4[c(2:33),c(2,111:118)]

aged5<-c(as.character(paste(factor(Agebracketd5[c(2),c(2)]))),as.character(paste(factor(Agebracketd5[c(2),c(3)]))),as.character(paste(factor(Agebracketd5[c(2),c(4)]))), as.character(paste(factor(Agebracketd5[c(2),c(5)]))), as.character(paste(factor(Agebracketd5[c(2),c(6)]))), as.character(paste(factor(Agebracketd5[c(2),c(7)]))), as.character(paste(factor(Agebracketd5[c(2),c(8)]))), as.character(paste(factor(Agebracketd5[c(2),c(9)]))))

very.disttAged5<-c(as.character(paste(factor(Agebracketd5[c(5),c(1)]))))
very.disAged5<-c(as.numeric(paste(factor(Agebracketd5[c(5),c(2)]))),as.numeric(paste(factor(Agebracketd5[c(5),c(3)]))),as.numeric(paste(factor(Agebracketd5[c(5),c(4)]))), as.numeric(paste(factor(Agebracketd5[c(5),c(5)]))), as.numeric(paste(factor(Agebracketd5[c(5),c(6)]))), as.numeric(paste(factor(Agebracketd5[c(5),c(7)]))), as.numeric(paste(factor(Agebracketd5[c(5),c(8)]))), as.numeric(paste(factor(Agebracketd5[c(5),c(9)]))))

dfAged5<-data.frame(aged5,very.disAged5)          

distt.Aged5<-c(as.character(paste(factor(Agebracketd5[c(7),c(1)]))))
disAged5<-c(as.numeric(paste(factor(Agebracketd5[c(7),c(2)]))),as.numeric(paste(factor(Agebracketd5[c(7),c(3)]))),as.numeric(paste(factor(Agebracketd5[c(7),c(4)]))), as.numeric(paste(factor(Agebracketd5[c(7),c(5)]))), as.numeric(paste(factor(Agebracketd5[c(7),c(6)]))), as.numeric(paste(factor(Agebracketd5[c(7),c(7)]))), as.numeric(paste(factor(Agebracketd5[c(7),c(8)]))), as.numeric(paste(factor(Agebracketd5[c(7),c(9)]))))
dfAge2d5<-data.frame(aged5,disAged5)


neiSaNorDistt.Age4d5<-c(as.character(paste(factor(Agebracketd5[c(9),c(1)]))))

neiSatNorDisAged5<-c(as.numeric(paste(factor(Agebracketd5[c(9),c(2)]))),as.numeric(paste(factor(Agebracketd5[c(9),c(3)]))),as.numeric(paste(factor(Agebracketd5[c(9),c(4)]))), as.numeric(paste(factor(Agebracketd5[c(9),c(5)]))), as.numeric(paste(factor(Agebracketd5[c(9),c(6)]))), as.numeric(paste(factor(Agebracketd5[c(9),c(7)]))), as.numeric(paste(factor(Agebracketd5[c(9),c(8)]))), as.numeric(paste(factor(Agebracketd5[c(9),c(9)]))))
dfAge3d5<-data.frame(aged5,neiSatNorDisAged5)         

satisfiedttAged5<-c(as.character(paste(factor(Agebracketd5[c(11),c(1)]))))

satisfiedAged5<-c(as.numeric(paste(factor(Agebracketd5[c(11),c(2)]))),as.numeric(paste(factor(Agebracketd5[c(11),c(3)]))),as.numeric(paste(factor(Agebracketd5[c(11),c(4)]))), as.numeric(paste(factor(Agebracketd5[c(11),c(5)]))), as.numeric(paste(factor(Agebracketd5[c(11),c(6)]))), as.numeric(paste(factor(Agebracketd5[c(11),c(7)]))), as.numeric(paste(factor(Agebracketd5[c(11),c(8)]))), as.numeric(paste(factor(Agebracketd5[c(11),c(9)]))))
dfAge4d5<-data.frame(aged5,satisfiedAged5)

verysatt.Aged5<-c(as.character(paste(factor(Agebracketd5[c(13),c(1)]))))

verySatAged5<-c(as.numeric(paste(factor(Agebracketd5[c(13),c(2)]))),as.numeric(paste(factor(Agebracketd5[c(13),c(3)]))),as.numeric(paste(factor(Agebracketd5[c(13),c(4)]))), as.numeric(paste(factor(Agebracketd5[c(13),c(5)]))), as.numeric(paste(factor(Agebracketd5[c(13),c(6)]))), as.numeric(paste(factor(Agebracketd5[c(13),c(7)]))), as.numeric(paste(factor(Agebracketd5[c(13),c(8)]))), as.numeric(paste(factor(Agebracketd5[c(13),c(9)]))))
dfAge5d5<-data.frame(aged5,verySatAged5)





################################################################################################################################################################################################
################################
Logged = FALSE;
PASSWORD <- data.frame(Brukernavn = "po", Passord = "123")

shinyServer(function(input, output) {
  # Define server logic required to summarize and view the selected dataset
  source("login.R",  local = TRUE)
  
  
  observe({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    hola<-read.csv(inFile$datapath, header=input$header, sep=input$sep,
                   quote=input$quote)
    
    write.csv(hola, file="../UNRASYS/UNRA.csv",row.names=FALSE,col.names=FALSE)
    
  })
  
  
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Data set",as.list(data_sets))
  })
  
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "UNRA1" = mydata,
           "UNRA2" = mydata1,
           "UNRA3" = mydata2,
           "UNRA4" = mydata3,
           "UNRA5" = mydata4)
  })
  
  
  
  
  # Choose question
  output$choose_question <- renderUI({
    
    if((input$dataset)== "UNRA1"){
      
      
      selectInput("questions", "QNS.", as.list(u1qns))
      
      
      
    }else if((input$dataset)== "UNRA2"){
      
      
      selectInput("questions", "QNS.", question3)
      
      
      
    }else if((input$dataset)== "UNRA3"){
      
      
      selectInput("questions", "QNS.", as.list(u1qnsd3))
      
      
      
    }
    else if((input$dataset)== "UNRA4"){
      
      
      selectInput("questions", "QNS.", question6)
      
      
      
    }else if((input$dataset)== "UNRA5"){
      
      
      selectInput("questions", "QNS.", question7)
      
      
      
    }
    
    
    
    
    
    
  })
  
  output$choose_category <- renderUI({
    
    if(((input$questions)== question)&&((input$dataset)=="UNRA1") ){
      
      selectInput("category", "Visualize by.", as.list(categqn1)) 
      
      
      
      
    } else if(((input$questions)== question1)&&((input$dataset)=="UNRA1") ){
      selectInput("category", "Visualize by.", as.list(categqn1))
      
    }else if(((input$questions)== question3)&&((input$dataset)=="UNRA2") ){
      selectInput("category", "Visualize by.", as.list(categqn1))
      
    }else if(((input$questions)== question4)&&((input$dataset)=="UNRA3") ){
      selectInput("category", "Visualize by.", as.list(categqn1))
      
    }else if(((input$questions)== question5)&&((input$dataset)=="UNRA3") ){
      selectInput("category", "Visualize by.", as.list(categqn1))
      
    }else if(((input$questions)== question6)&&((input$dataset)=="UNRA4") ){
      selectInput("category", "Visualize by.", as.list(categqn1))
      
    }else if(((input$questions)== question7)&&((input$dataset)=="UNRA5") ){
      selectInput("category", "Visualize by.", as.list(categqn1))
      
    }
  })
  
  
  ################################################################################################################################################################################################
  ################################......Regions plot
  output$plotsou <- renderPlot({
    
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      # Draw plot
      ggplot(south,  aes(x=south$var2, y=south$southern))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Region :",soutthtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      # Draw plot
      ggplot(southd3,  aes(x=southd3$var2d3, y=southd3$southernd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Region :",soutthttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      # Draw plot
      ggplot(southq2d3,  aes(x=southq2d3$var2q2d3, y=southq2d3$southernq2d3))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE),width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",soutthttq2d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=, vjust=1))
    }
    
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      # Draw plot
      ggplot(southq2,  aes(x=southq2$var2q2, y=southq2$southernq2))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE),width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",soutthttq2), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=, vjust=1))
    }
    
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Region"){
      # Draw plot
      ggplot(melted,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
            y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Region :",regiondt2q2soutt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
     
      
    }
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(southd4q,  aes(x=southd4q$var2d4q, y=southd4q$southernd4q))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6,
             subtitle=paste("Region :",soutthttd4q), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(southd5q,  aes(x=southd5q$var2d5q, y=southd5q$southernd5q))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7,
             subtitle=paste("Region :",soutthttd5q), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    
  },height=600)
  
  
  output$plotcen <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(centr,  aes(x=centr$var2, y=centr$central))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Region :",centtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(centrd3,  aes(x=centrd3$var2d3, y=centrd3$centrald3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Region :",centttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(centrq2d3,  aes(x=centrq2d3$var2q2d3, y=centrq2d3$centralq2d3))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",centttq2d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(centrq2,  aes(x=centrq2$var2q2, y=centrq2$centralq2))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",centttq2), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Region"){
      # Draw plot
      ggplot(meltedc,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Region :",regiondt2q2centtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(centrd4q,  aes(x=centrd4q$var2d4q, y=centrd4q$centrald4q))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6,
             subtitle=paste("Region :",centttd4q), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(centrd5q,  aes(x=centrd5q$var2d5q, y=centrd5q$centrald5q))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7,
             subtitle=paste("Region :",centttd5q), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  },height=600)  
  output$ploteas <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(east,  aes(x=east$var2, y=east$eastern))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Region :",eastt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(eastd3,  aes(x=eastd3$var2d3, y=eastd3$easternd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Region :",easttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(eastq2,  aes(x=eastq2$var2q2, y=eastq2$easternq2))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",easttq2), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    } else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(eastq2d3,  aes(x=eastq2d3$var2q2d3, y=eastq2d3$easternq2d3))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",easttq2d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Region"){
      # Draw plot
      ggplot(meltede,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Region :",regiondt2q2easttt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(eastd4q,  aes(x=eastd4q$var2d4q, y=eastd4q$easternd4q))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6,
             subtitle=paste("Region :",easttd4q), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(eastd5q,  aes(x=eastd5q$var2d5q, y=eastd5q$easternd5q))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7,
             subtitle=paste("Region :",easttd5q), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    
  },height=600)  
  
  output$plotnor <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(north,  aes(x=north$var2, y=north$northern))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Region :",northtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(northd3,  aes(x=northd3$var2d3, y=northd3$northernd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Region :",northttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(northq2d3,  aes(x=northq2d3$var2q2d3, y=northq2d3$northernq2d3))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",northttq2d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(northq2,  aes(x=northq2$var2q2, y=northq2$northernq2))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",northttq2), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
    }else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Region"){
      # Draw plot
      ggplot(meltedn,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Region :",regiondt2q2northtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Region"){
    # Draw plot
    ggplot(northd4q,  aes(x=northd4q$var2d4q, y=northd4q$northernd4q))+ 
      geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
      coord_flip() +
      labs(y= "Interviewees",
           x="Answers",
           title=question6,
           subtitle=paste("Region :",northttd4q), 
           caption="source:UNRA") + 
      theme(axis.text.x = element_text(angle=0, vjust=1))
    
    }
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Region"){
      # Draw plot
      ggplot(northd5q,  aes(x=northd5q$var2d5q, y=northd5q$northernd5q))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7,
             subtitle=paste("Region :",northttd5q), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
  
    
    
    
  },height=600)  
  
  output$plotwes <- renderPlot({
    if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(westd3,  aes(x=westd3$var2d3, y=westd3$westernd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Region :",westttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(westq2d3,  aes(x=westq2d3$var2q2d3, y=westq2d3$westernq2d3))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",westttq2d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(west,  aes(x=west$var2, y=west$western))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Region :",westtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      
      # Draw plot
      ggplot(westq2,  aes(x=westq2$var2q2, y=westq2$westernq2))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             subtitle=paste("Region :",westttq2), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Region"){
      # Draw plot
      ggplot(meltedw,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Region :",regiondt2q2westtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Region"){
    # Draw plot
    ggplot(westd4q,  aes(x=westd4q$var2d4q, y=westd4q$westernd4q))+ 
      geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
      coord_flip() +
      labs(y= "Interviewees",
           x="Answers",
           title=question6,
           subtitle=paste("Region :",westttd4q), 
           caption="source:UNRA") + 
      theme(axis.text.x = element_text(angle=0, vjust=1))
    
  }
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Region"){
      # Draw plot
      ggplot(westd5q,  aes(x=westd5q$var2d5q, y=westd5q$westernd5q))+ 
        geom_bar(stat="identity",position = position_stack(reverse = TRUE), width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7,
             subtitle=paste("Region :",westttd5q), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
  },height=600)  
  
  ## # #MAPS PLOTING # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # #
  output$map <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      ggplot(hkmapdfs,aes(long,lat,group=group,fill=value)) +
        geom_polygon() + 
        labs(fill="Number of people",
             y= "",
             x="",
             title=question, 
             subtitle=paste("Answer :",answerz), 
             caption="source: mpg") + 
        theme(axis.text.x = element_text(angle=, vjust=6))
      
      
      
    }
    
  })  
  
  
  
  output$map1 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      ggplot(hkmapdfs,aes(long,lat,group=group,fill=value2)) +
        geom_polygon() + 
        labs(fill="Number of people",
             y= "",
             x="",
             title=question, 
             subtitle=paste("Answer :",answer1), 
             caption="source: mpg") + 
        theme(axis.text.x = element_text(angle=, vjust=6))
      
      
      
    }
    
  })  
  
  output$map2 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      ggplot(hkmapdfs,aes(long,lat,group=group,fill=value3)) +
        geom_polygon() + 
        labs(fill="Number of people",
             y= "",
             x="",
             title=question, 
             subtitle=paste("Answer :",answer2), 
             caption="source: mpg") + 
        theme(axis.text.x = element_text(angle=, vjust=6))
      
      
      
    }
    
  })  
  
  output$map3 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      ggplot(hkmapdfs,aes(long,lat,group=group,fill=value4)) +
        geom_polygon() + 
        labs(fill="Number of people",
             y= "",
             x="",
             title=question, 
             subtitle=paste("Answer :",answer3), 
             caption="source: mpg") + 
        theme(axis.text.x = element_text(angle=, vjust=6))
      
      
      
    }
    
  })  
  
  output$map4 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Region"){
      
      ggplot(hkmapdfs,aes(long,lat,group=group,fill=value4)) +
        geom_polygon() + 
        labs(fill="Number of people",
             y= "",
             x="",
             title=question, 
             subtitle=paste("Answer :",answer4), 
             caption="source: mpg") + 
        theme(axis.text.x = element_text(angle=, vjust=6))
      
      
      
    }
    
  })  
  ## # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # #  
  
  ## # # # # # # # # # # # # # # # # # # # # # District plots
  #Districts very disatisfied.
  output$Dvd <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df1,  aes(x=df1$discs, y=df1$very.dis))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",very.distt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "District"){
      # Draw plot
      ggplot(meltedm,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("District :",districtdt2q2souttm), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df1d3,  aes(x=df1d3$discsd3, y=df1d3$very.disd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",very.disttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(rdstard3,  aes(x=rdstard3$discsQn2d3, y=rdstard3$roads.tarmd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",roads.are.tarmttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(rdstar,  aes(x=rdstar$discsQn2, y=rdstar$roads.tarm))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",roads.are.tarmtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "District"){
    
    # Draw plot
    ggplot(df1d4,  aes(x=df1d4$discsd4, y=df1d4$a1))+ 
      geom_bar(stat="identity", width=, fill="#2b3e50") + 
      labs(y= "Interviewees",
           x="Answers",
           title=question6, 
           subtitle=paste("Answer :",a1tt), 
           caption="source:UNRA") + 
      theme(axis.text.x = element_text(angle=90, vjust=1))
    
    }
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "District"){
      
      # Draw plot
      ggplot(df1d5,  aes(x=df1d5$discsd5, y=df1d5$a1d5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",a1ttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
  })  
  
  #Districts dissatisfied.
  output$Dd <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df2,  aes(x=df2$discs, y=df2$dis))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",distt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "District"){
      # Draw plot
      ggplot(meltedcs,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("District :",districtdt2q2centtts), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(no.filledpots,  aes(x=no.filledpots$discsQn2, y=no.filledpots$no.filledpot))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",no.filled.potholestt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(no.filledpotsd3,  aes(x=no.filledpotsd3$discsQn2d3, y=no.filledpotsd3$no.filledpotd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",no.filled.potholesttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df2d3,  aes(x=df2d3$discsd3, y=df2d3$disd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",disttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "District"){
      
      # Draw plot
      ggplot(df5d4,  aes(x=df5d4$discsd4, y=df5d4$a5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",a5tt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "District"){
      
      # Draw plot
      ggplot(df2d5,  aes(x=df2d5$discsd5, y=df2d5$a2d5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",a2ttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    
    
  })  
  
  
  #Districts neither satisfied nor dissatisfied.
  output$Dnsnd <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df3,  aes(x=df3$discs, y=df3$neiSatNorDis))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",neiSaNorDistt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df3d3,  aes(x=df3d3$discsd3, y=df3d3$neiSatNorDisd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",neiSaNorDisttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
   else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs,  aes(x=imprd.rd.sigs$discsQn2, y=imprd.rd.sigs$imprvd.rd.sig))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",imprd.rd.sigtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
   }
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigsd3,  aes(x=imprd.rd.sigsd3$discsQn2d3, y=imprd.rd.sigsd3$imprvd.rd.sigd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",imprd.rd.sigttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "District"){
      
      # Draw plot
      ggplot(df2d4,  aes(x=df2d4$discsd4, y=df2d4$a2))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",a2tt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "District"){
      
      # Draw plot
      ggplot(df3d5,  aes(x=df3d5$discsd5, y=df3d5$a3d5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",a3ttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
  })  
  
  
  #Districts satisfied.
  output$Dsa <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df4,  aes(x=df4$discs, y=df4$satisfied))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",satisfiedtt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df4d3,  aes(x=df4d3$discsd3, y=df4d3$satisfiedd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",satisfiedttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(mo.rds.con,  aes(x=mo.rds.con$discsQn2, y=mo.rds.con$moroads.cons))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",morerds.contt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(mo.rds.cond3,  aes(x=mo.rds.cond3$discsQn2d3, y=mo.rds.cond3$moroads.consd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",morerds.conttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "District"){
      
      # Draw plot
      ggplot(df3d4,  aes(x=df3d4$discsd4, y=df3d4$a3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",a3tt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "District"){
      
      # Draw plot
      ggplot(df4d5,  aes(x=df4d5$discsd5, y=df4d5$a4d5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",a4ttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
  })  
  
  
  
  
  #Districts very satisfied.
  output$Dvrys <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df5,  aes(x=df5$discs, y=df5$verySa))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",verysattt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(df5d3,  aes(x=df5d3$discsd3, y=df5d3$verySatd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",verysatttd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(stric.usage.bbd3,  aes(x=stric.usage.bbd3$discsQn2d3, y=stric.usage.bbd3$strict.rd.usage.bodad3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",strict.usage.bodattd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "District"){
      
      
      # Draw plot
      ggplot(stric.usage.bb,  aes(x=stric.usage.bb$discsQn2, y=stric.usage.bb$strict.rd.usage.boda))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",strict.usage.bodatt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "District"){
      
      # Draw plot
      ggplot(df4d4,  aes(x=df4d4$discsd4, y=df4d4$a4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",a4tt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "District"){
      
      # Draw plot
      ggplot(df5d5,  aes(x=df5d5$discsd5, y=df5d5$a5d5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",a5ttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=90, vjust=1))
      
    }
    
    
    
  })
  
  ################################################################################################################################################################################################
  ################################......Road types plot 
  output$roadt <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
    ggplot(dfroads, aes(x = "", y=very.disroads, fill = factor(roadtypes))) + 
      geom_bar(width = 1, stat = "identity") +
      theme(axis.line = element_blank(), 
            plot.title = element_text(hjust=0.5)) + 
      labs(fill="Road Type.",
           x=NULL, 
           y=NULL, 
           title=question, 
           subtitle=paste("Answer :",very.disttroads),
           caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
    
    } 
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(rdstar2, aes(x = "", y=roads.tarm2, fill = factor(rdtypesqn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",roads.are.tarmtt2),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Road Type"){
      # Draw plot
      ggplot(meltedp,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Road type :",paved2q2souttp), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(rdstar2d3, aes(x = "", y=roads.tarm2d3, fill = factor(rdtypesqn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",roads.are.tarmtt2d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(dfroadsd3, aes(x = "", y=very.disroadsd3, fill = factor(Roadtyped3sd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",very.disttroadsd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(pavedroadtd4,  aes(x=pavedroadtd4$var2roadtd4, y=pavedroadtd4$pavedd4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",pavedttd4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(pavedroadtd5,  aes(x=pavedroadtd5$var2roadtd5, y=pavedroadtd5$pavedd5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",pavedttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
  },height=600)
  
  output$roadt1 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(dfroads2, aes(x = "", y=disroads, fill = factor(roadtypes))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question, 
             subtitle=paste("Answer :",disttroads),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(mo.rds.con2, aes(x = "", y=moroads.cons2, fill = factor(rdtypesqn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",morerds.contt2),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Road Type"){
      # Draw plot
      ggplot(meltedcu,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Road type :",unpavedqcentttu), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(mo.rds.con2d3, aes(x = "", y=moroads.cons2d3, fill = factor(rdtypesqn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",morerds.contt2d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(dfroads2d3, aes(x = "", y=disroadsd3, fill = factor(Roadtyped3sd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",disttroadsd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    
  else  if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Road Type"){
    ggplot(unpavedroadtd4,  aes(x=unpavedroadtd4$var2roadtd4, y=unpavedroadtd4$unpavedd4))+ 
      geom_bar(stat="identity", width=, fill="#2b3e50") + 
      coord_flip() +
      labs(y= "Interviewees",
           x="Answers",
           title=question6, 
           subtitle=paste("Answer :",unpavedttd4), 
           caption="source:UNRA") + 
      theme(axis.text.x = element_text(angle=0, vjust=1))
    
  }
    else  if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Road Type"){
      ggplot(unpavedroadtd5,  aes(x=unpavedroadtd5$var2roadtd5, y=unpavedroadtd5$unpavedd5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",unpavedttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
  },height=600)
  
  output$roadt2 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(dfroads3, aes(x = "", y=neiSatNorDis.roads, fill = factor(roadtypes))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question, 
             subtitle=paste("Answer :",neiSaNorDistt.roads),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(no.filledpots2, aes(x = "", y=no.filledpot2, fill = factor(rdtypesqn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",no.filled.potholestt2),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(no.filledpots2d3, aes(x = "", y=no.filledpot2d3, fill = factor(rdtypesqn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",no.filled.potholestt2d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(dfroads3d3, aes(x = "", y=neiSatNorDis.roadsd3, fill = factor(Roadtyped3sd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",neiSaNorDistt.roadsd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
  },height=600)
  
  output$roadt3 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(dfroads4, aes(x = "", y=satisfiedroads, fill = factor(roadtypes))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question, 
             subtitle=paste("Answer :",satisfiedttroads),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs2, aes(x = "", y=imprvd.rd.sig2, fill = factor(rdtypesqn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",imprd.rd.sigtt2),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs2d3, aes(x = "", y=imprvd.rd.sig2d3, fill = factor(rdtypesqn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",imprd.rd.sigtt2d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(dfroads4d3, aes(x = "", y=satisfiedroadsd3, fill = factor(Roadtyped3sd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",satisfiedttroadsd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    
  },height=600)
  
  output$roadt4 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(dfroads5, aes(x = "", y=verySatroads, fill = factor(roadtypes))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question, 
             subtitle=paste("Answer :",verysatttroads),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(stric.usage.bb2, aes(x = "", y=strict.rd.usage.boda2, fill = factor(rdtypesqn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",strict.usage.bodatt2),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(stric.usage.bb2d3, aes(x = "", y=strict.rd.usage.boda2d3, fill = factor(rdtypesqn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",strict.usage.bodatt2d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Type"){
      
      
      # Draw plot
      ggplot(dfroads5d3, aes(x = "", y=verySatroadsd3, fill = factor(Roadtyped3sd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",verysatttroadsd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    
  },height=600)
  ################################################################################################################################################################################################
  ################################......Respondent Gender plot 
  output$gender <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespon, aes(x = "", y=very.disgender, fill = factor(gender))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question, 
             subtitle=paste("Answer :",very.disttgender),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(rdstar3, aes(x = "", y=roads.tarm3, fill = factor(genderQn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",roads.are.tarmtt3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Respondent Gender"){
      # Draw plot
      ggplot(meltedfe,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Respondent Gender :",female2q2souttfe), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(rdstar3d3, aes(x = "", y=roads.tarm3d3, fill = factor(genderQn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",roads.are.tarmtt3d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespond3, aes(x = "", y=very.disgenderd3, fill = factor(genderd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",very.disttgenderd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    
    else  if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Respondent Gender"){
      ggplot(maletd4,  aes(x=maletd4$var2rsgenderd4, y=maletd4$maled4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",malettd4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else  if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Respondent Gender"){
      ggplot(maletd5,  aes(x=maletd5$var2rsgenderd5, y=maletd5$maled5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",malettd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  },height=600)
  
  
  output$gender1 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespon2, aes(x = "", y=disgender, fill = factor(gender))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question, 
             subtitle=paste("Answer :",disttgender),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(mo.rds.con3, aes(x = "", y=moroads.cons3, fill = factor(genderQn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",morerds.contt3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Respondent Gender"){
      # Draw plot
      ggplot(meltedma,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Respondent Gender :",maleqcentttma), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(mo.rds.con3d3, aes(x = "", y=moroads.cons3d3, fill = factor(genderQn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",morerds.contt3d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespon2d3, aes(x = "", y=disgenderd3, fill = factor(genderd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",disttgenderd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    
    else  if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Respondent Gender"){
      ggplot(femaletd4,  aes(x=femaletd4$var2rsgenderd4, y=femaletd4$femaled4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",femalettd4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else  if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Respondent Gender"){
      ggplot(femaletd5,  aes(x=femaletd5$var2rsgenderd5, y=femaletd5$femaled5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",femalettd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
  },height=600)
  
  
  output$gender2 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespon3, aes(x = "", y=neiSatNorDis.gender, fill = factor(gender))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question, 
             subtitle=paste("Answer :",neiSaNorDistt.gender),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(no.filledpots3, aes(x = "", y=no.filledpot3, fill = factor(genderQn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",no.filled.potholestt3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(no.filledpots3d3, aes(x = "", y=no.filledpot3d3, fill = factor(genderQn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",no.filled.potholestt3d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespon3d3, aes(x = "", y=neiSatNorDis.genderd3, fill = factor(genderd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",neiSaNorDistt.genderd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    
  },height=600)
 
  output$gender3 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespon4, aes(x = "", y=satisfiedgender, fill = factor(gender))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question, 
             subtitle=paste("Answer :",satisfiedttGender),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs3, aes(x = "", y=imprvd.rd.sig3, fill = factor(genderQn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",imprd.rd.sigtt3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs3d3, aes(x = "", y=imprvd.rd.sig3d3, fill = factor(genderQn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",imprd.rd.sigtt3d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespon4d3, aes(x = "", y=satisfiedgenderd3, fill = factor(genderd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",satisfiedttGenderd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    
    
  },height=600)
  
  
  output$gender4 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespon5, aes(x = "", y=verySatGender, fill = factor(gender))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question, 
             subtitle=paste("Answer :",verysatttgender),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(stric.usage.bb3, aes(x = "", y=strict.rd.usage.boda3, fill = factor(genderQn2))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question1, 
             subtitle=paste("Answer :",strict.usage.bodatt3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(stric.usage.bb3d3, aes(x = "", y=strict.rd.usage.boda3d3, fill = factor(genderQn2d3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question5, 
             subtitle=paste("Answer :",strict.usage.bodatt3d3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    } 
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Gender"){
      
      
      # Draw plot
      ggplot(dfrespon5d3, aes(x = "", y=verySatGenderd3, fill = factor(genderd3))) + 
        geom_bar(width = 1, stat = "identity") +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        labs(fill="Road Type.",
             x=NULL, 
             y=NULL, 
             title=question4, 
             subtitle=paste("Answer :",verysatttgenderd3),
             caption="Source: UNRA")+
        coord_polar(theta = "y", start=0)
      
    }
    
    
  },height=600)
  
  
  ## # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # #  
  
  ## # # # # # # # # # # # # # # # # # # # # # Road names plots  ## # # # # # # # # # # # # # # # # # # # # #
  #road names very disatisfied.
  output$rdname <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames,  aes(x=dfnames$rdnames, y=dfnames$very.disname))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",very.disttrdname), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(rdstar4,  aes(x=rdstar4$rdnamesQn2, y=rdstar4$roads.tarm4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",roads.are.tarmtt4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(rdstar4d3,  aes(x=rdstar4d3$rdnamesQn2d3, y=rdstar4d3$roads.tarm4d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",roads.are.tarmtt4d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Road Name"){
      # Draw plot
      ggplot(meltedrd,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Road Name:",roadn2q2souttrd), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnamesd3,  aes(x=dfnamesd3$rdnamesd3, y=dfnamesd3$very.disnamed3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",very.disttrdnamed3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnamesd4,  aes(x=dfnamesd4$rdnamesd4, y=dfnamesd4$ad4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",attd4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnamesd5,  aes(x=dfnamesd5$rdnamesd5, y=dfnamesd5$ad5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",attd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
  }, height=1000 , width = 1100)  
  
  #Road names disatisfied.
  output$rdname1 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames2,  aes(x=dfnames2$rdnames, y=dfnames2$dis.name))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :", distt.name), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(mo.rds.con4,  aes(x=mo.rds.con4$rdnamesQn2, y=mo.rds.con4$moroads.cons4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",morerds.contt4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Road Name"){
      # Draw plot
      ggplot(meltedcrd1,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Road Name:",roadnqcentttrd1), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(mo.rds.con4d3,  aes(x=mo.rds.con4d3$rdnamesQn2d3, y=mo.rds.con4d3$moroads.cons4d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",morerds.contt4d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames2d3,  aes(x=dfnames2d3$rdnamesd3, y=dfnames2d3$dis.named3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :", distt.named3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames2d4,  aes(x=dfnames2d4$rdnamesd4, y=dfnames2d4$a1d4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",a1ttd4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames2d5,  aes(x=dfnames2d5$rdnamesd5, y=dfnames2d5$a1d5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",a1ttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
  }, height=1000 , width = 1100)  
  
  #road names neither nor .
  output$rdname2 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames3,  aes(x=dfnames3$rdnames, y=dfnames3$neiSatNorDis.name))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",neiSaNorDistt.name), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(no.filledpots4,  aes(x=no.filledpots4$rdnamesQn2, y=no.filledpots4$no.filledpot4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",no.filled.potholestt4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(no.filledpots4d3,  aes(x=no.filledpots4d3$rdnamesQn2d3, y=no.filledpots4d3$no.filledpot4d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",no.filled.potholestt4d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames3d3,  aes(x=dfnames3d3$rdnamesd3, y=dfnames3d3$neiSatNorDis.named3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",neiSaNorDistt.named3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames3d4,  aes(x=dfnames3d4$rdnamesd4, y=dfnames3d4$a2d4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",a2ttd4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames3d5,  aes(x=dfnames3d5$rdnamesd5, y=dfnames3d5$a2d5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",a2ttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
  }, height=1000 , width = 1100)  
  
  #Road names satisfied.
  output$rdname3 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames4,  aes(x=dfnames4$rdnames, y=dfnames4$satisfied.names))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",satisfiedtt.names), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs4,  aes(x=imprd.rd.sigs4$rdnamesQn2, y=imprd.rd.sigs4$imprvd.rd.sig4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",imprd.rd.sigtt4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs4d3,  aes(x=imprd.rd.sigs4d3$rdnamesQn2d3, y=imprd.rd.sigs4d3$imprvd.rd.sig4d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",imprd.rd.sigtt4d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames4d3,  aes(x=dfnames4d3$rdnamesd3, y=dfnames4d3$satisfied.namesd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",satisfiedtt.namesd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames4d4,  aes(x=dfnames4d4$rdnamesd4, y=dfnames4d4$a3d4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",a3ttd4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames4d5,  aes(x=dfnames4d5$rdnamesd5, y=dfnames4d5$a3d5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",a3ttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  }, height=1000 , width = 1100)  
  
  #road names very satisfied.
  output$rdname4 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames5,  aes(x=dfnames5$rdnames, y=dfnames5$verySat.names))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",verysatt.names), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(stric.usage.bb4,  aes(x=stric.usage.bb4$rdnamesQn2, y=stric.usage.bb4$strict.rd.usage.boda4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",strict.usage.bodatt4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(stric.usage.bb4d3,  aes(x=stric.usage.bb4d3$rdnamesQn2d3, y=stric.usage.bb4d3$strict.rd.usage.boda4d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",strict.usage.bodatt4d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
   else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames5d3,  aes(x=dfnames5d3$rdnamesd3, y=dfnames5d3$verySat.namesd3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",verysatt.namesd3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Road Name"){
      
      
      # Draw plot
      ggplot(dfnames5d5,  aes(x=dfnames5d5$rdnamesd5, y=dfnames5d5$a4d5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        coord_flip() +
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",a4ttd5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    
  }, height=1000 , width = 1100)  
  
  
  ## # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # #  
  
  ## # # # # # # # # # # # # # # # # # # # # # Response Type plots  ## # # # # # # # # # # # # # # # # # # # # #
  #response type very disatisfied.
  output$respt <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes,  aes(x=dfRestypes$restype, y=dfRestypes$very.disRestype))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",very.disttRrestype), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(rdstar5,  aes(x=rdstar5$restypeQn2, y=rdstar5$roads.tarm5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",roads.are.tarmtt5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Respondent Type"){
      # Draw plot
      ggplot(meltedrdt,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Respondent Type:",respduttrdt), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(rdstar5d3,  aes(x=rdstar5d3$restypeQn2d3, y=rdstar5d3$roads.tarm5d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",roads.are.tarmtt5d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypesd3,  aes(x=dfRestypesd3$restyped3, y=dfRestypesd3$very.disRestyped3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",very.disttRrestyped3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypesd4,  aes(x=dfRestypesd4$restyped4, y=dfRestypesd4$very.disRestyped4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",very.disttRrestyped4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypesd5,  aes(x=dfRestypesd5$restyped5, y=dfRestypesd5$very.disRestyped5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",very.disttRrestyped5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
  })  
  
  
  #response type  disatisfied.
  output$respt1 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes2,  aes(x=dfRestypes2$restype, y=dfRestypes2$disRestype))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",distt.restype), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(mo.rds.con5,  aes(x=mo.rds.con5$restypeQn2, y=mo.rds.con5$moroads.cons5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",morerds.contt5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Respondent Type"){
      # Draw plot
      ggplot(meltedcrdt1,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Respondent Type:",respduttrdt1), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(mo.rds.con5d3,  aes(x=mo.rds.con5d3$restypeQn2d3, y=mo.rds.con5d3$moroads.cons5d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",morerds.contt5d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes2d3,  aes(x=dfRestypes2d3$restyped3, y=dfRestypes2d3$disRestyped3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",distt.restyped3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes2d4,  aes(x=dfRestypes2d4$restyped4, y=dfRestypes2d4$disRestyped4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",distt.restyped4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes2d5,  aes(x=dfRestypes2d5$restyped5, y=dfRestypes2d5$disRestyped5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",distt.restyped5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
  })  
  
  #response type  Neither nor.
  output$respt2 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes3,  aes(x=dfRestypes3$restype, y=dfRestypes3$neiSatNorDisRestype))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",neiSaNorDisttRestype), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(no.filledpots5,  aes(x=no.filledpots5$restypeQn2, y=no.filledpots5$no.filledpot5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",no.filled.potholestt5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(no.filledpots5d3,  aes(x=no.filledpots5d3$restypeQn2d3, y=no.filledpots5d3$no.filledpot5d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",no.filled.potholestt5d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes3d3,  aes(x=dfRestypes3d3$restyped3, y=dfRestypes3d3$neiSatNorDisRestyped3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",neiSaNorDisttRestyped3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes3d4,  aes(x=dfRestypes3d4$restyped4, y=dfRestypes3d4$neiSatNorDisRestyped4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",neiSaNorDisttRestyped4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes3d5,  aes(x=dfRestypes3d5$restyped5, y=dfRestypes3d5$neiSatNorDisRestyped5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",neiSaNorDisttRestyped5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  })  
  
  #response type  Satisfied.
  output$respt3 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes4,  aes(x=dfRestypes4$restype, y=dfRestypes4$satisfiedRestype))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",satisfiedttRestype), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs5,  aes(x=imprd.rd.sigs5$restypeQn2, y=imprd.rd.sigs5$imprvd.rd.sig5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",imprd.rd.sigtt5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs5d3,  aes(x=imprd.rd.sigs5d3$restypeQn2d3, y=imprd.rd.sigs5d3$imprvd.rd.sig5d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",imprd.rd.sigtt5d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes4d3,  aes(x=dfRestypes4d3$restyped3, y=dfRestypes4d3$satisfiedRestyped3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",satisfiedttRestyped3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes4,  aes(x=dfRestypes4$restyped4, y=dfRestypes4$satisfiedRestyped4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",satisfiedttRestyped4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes5,  aes(x=dfRestypes5$restyped5, y=dfRestypes5$satisfiedRestyped5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",satisfiedttRestyped5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  })  
  
  
  #response type very Satisfied.
  output$respt4 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes5,  aes(x=dfRestypes5$restype, y=dfRestypes5$verySatRestype))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",verysattRestype), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(stric.usage.bb5,  aes(x=stric.usage.bb5$restypeQn2, y=stric.usage.bb5$strict.rd.usage.boda5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",strict.usage.bodatt5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(stric.usage.bb5d3,  aes(x=stric.usage.bb5d3$restypeQn2d3, y=stric.usage.bb5d3$strict.rd.usage.boda5d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",strict.usage.bodatt5d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes5d3,  aes(x=dfRestypes5d3$restyped3, y=dfRestypes5d3$verySatRestyped3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",verysattRestyped3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes5d4,  aes(x=dfRestypes5d4$restyped4, y=dfRestypes5d4$verySatRestyped4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",verysattRestyped4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Respondent Type"){
      
      
      # Draw plot
      ggplot(dfRestypes5d5,  aes(x=dfRestypes5d5$restyped5, y=dfRestypes5d5$verySatRestyped5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",verysattRestyped5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  })  

  ## # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # ### # # # # # # # # # # # # # # # # # # # # #  
  
  ## # # # # # # # # # # # # # # # # # # # # # Age bracket plots  ## # # # # # # # # # # # # # # # # # # # # #
  #age barket very disatisfied.
  output$age <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge,  aes(x=dfAge$age, y=dfAge$very.disAge))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",very.disttAge), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(rdstar6,  aes(x=rdstar6$ageQn2, y=rdstar6$roads.tarm6))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",roads.are.tarmtt6), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Age bracket"){
      # Draw plot
      ggplot(meltedrdag,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Age bracket:",ageuttrdag), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(rdstar6d3,  aes(x=rdstar6d3$ageQn2d3, y=rdstar6d3$roads.tarm6d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",roads.are.tarmtt6d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAged3,  aes(x=dfAged3$aged3, y=dfAged3$very.disAged3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",very.disttAged3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAged4,  aes(x=dfAged4$aged4, y=dfAged4$very.disAged4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",very.disttAged4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAged5,  aes(x=dfAged5$aged5, y=dfAged5$very.disAged5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",very.disttAged5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  })  
  
  
  #age bracket  disatisfied.
  output$age1 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge2,  aes(x=dfAge2$age, y=dfAge2$disAge))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",distt.Age), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(mo.rds.con6,  aes(x=mo.rds.con6$ageQn2, y=mo.rds.con6$moroads.cons6))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",morerds.contt6), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question3)&&((input$dataset)=="UNRA2")&&(input$category)== "Age bracket"){
      # Draw plot
      ggplot(meltedcrdag1,aes(x=cats,y=value,fill=variable)) +
        geom_bar(colour="black",stat="identity", position = "dodge",alpha=.3)+
        coord_flip() +
        
        labs(fill="General satisfaction",
             y= "Interviewees",
             x="Answers",
             title=question3, 
             subtitle=paste("Age bracket:",agettrdag1), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
      
      
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(mo.rds.con6d3,  aes(x=mo.rds.con6d3$ageQn2d3, y=mo.rds.con6d3$moroads.cons6d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",morerds.contt6d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
   else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge2d3,  aes(x=dfAge2d3$aged3, y=dfAge2d3$disAged3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",distt.Aged3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge2d4,  aes(x=dfAge2d4$aged4, y=dfAge2d4$disAged4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",distt.Aged4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge2d5,  aes(x=dfAge2d5$aged5, y=dfAge2d5$disAged5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",distt.Aged5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  })  
  
  #age bracket  Neither nor.
  output$age2 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge3,  aes(x=dfAge3$age, y=dfAge3$neiSatNorDisAge))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",neiSaNorDistt.Age), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(no.filledpots6,  aes(x=no.filledpots6$ageQn2, y=no.filledpots6$no.filledpot6))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",no.filled.potholestt6), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(no.filledpots6d3,  aes(x=no.filledpots6d3$ageQn2d3, y=no.filledpots6d3$no.filledpot6d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",no.filled.potholestt6d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge3d3,  aes(x=dfAge3d3$aged3, y=dfAge3d3$neiSatNorDisAged3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",neiSaNorDistt.Aged3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge3d4,  aes(x=dfAge3d4$aged4, y=dfAge3d4$neiSatNorDisAged4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",neiSaNorDistt.Age4d4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge3d5,  aes(x=dfAge3d5$aged5, y=dfAge3d5$neiSatNorDisAged5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",neiSaNorDistt.Age4d5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    
  })  
  
  #age bracket  Satisfied.
  output$age3 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge4,  aes(x=dfAge4$age, y=dfAge4$satisfiedAge))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",satisfiedttAge), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs6,  aes(x=imprd.rd.sigs6$ageQn2, y=imprd.rd.sigs6$imprvd.rd.sig6))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",imprd.rd.sigtt6), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(imprd.rd.sigs6d3,  aes(x=imprd.rd.sigs6d3$ageQn2d3, y=imprd.rd.sigs6d3$imprvd.rd.sig6d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",imprd.rd.sigtt6d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
   else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge4d3,  aes(x=dfAge4d3$aged3, y=dfAge4d3$satisfiedAged3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",satisfiedttAged3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge4d4,  aes(x=dfAge4d4$aged4, y=dfAge4d4$satisfiedAged4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",satisfiedttAged4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge4d5,  aes(x=dfAge4d5$aged5, y=dfAge4d5$satisfiedAged5))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",satisfiedttAged5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  })  
  
  
  #age bracket very Satisfied.
  output$age4 <- renderPlot({
    if(((input$questions)== question)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge5,  aes(x=dfAge5$age, y=dfAge5$verySatAge))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question, 
             subtitle=paste("Answer :",verysatt.Age), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    else if(((input$questions)== question1)&&((input$dataset)=="UNRA1")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(stric.usage.bb6,  aes(x=stric.usage.bb6$ageQn2, y=stric.usage.bb6$strict.rd.usage.boda6))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question1, 
             subtitle=paste("Answer :",strict.usage.bodatt6), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question5)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(stric.usage.bb6d3,  aes(x=stric.usage.bb6d3$ageQn2d3, y=stric.usage.bb6d3$strict.rd.usage.boda6d3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question5, 
             subtitle=paste("Answer :",strict.usage.bodatt6d3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question4)&&((input$dataset)=="UNRA3")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge5d3,  aes(x=dfAge5d3$aged3, y=dfAge5d3$verySatAged3))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question4, 
             subtitle=paste("Answer :",verysatt.Aged3), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question6)&&((input$dataset)=="UNRA4")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge5d4,  aes(x=dfAge5d4$aged4, y=dfAge5d4$verySatAged4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question6, 
             subtitle=paste("Answer :",verysatt.Aged4), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
    else if(((input$questions)== question7)&&((input$dataset)=="UNRA5")&&(input$category)== "Age bracket"){
      
      
      # Draw plot
      ggplot(dfAge5d4,  aes(x=dfAge5d4$aged4, y=dfAge5d4$verySatAged4))+ 
        geom_bar(stat="identity", width=, fill="#2b3e50") + 
        labs(y= "Interviewees",
             x="Answers",
             title=question7, 
             subtitle=paste("Answer :",verysatt.Aged5), 
             caption="source:UNRA") + 
        theme(axis.text.x = element_text(angle=0, vjust=1))
      
    }
    
  })  
  
  
})


