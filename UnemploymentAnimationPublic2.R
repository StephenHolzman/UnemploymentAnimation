###Animate U.S.A seasonally adjusted unemployment rate choropleth
###Data was pulled from the Bureau of Labor Statistics using the Data Finder tool for January 1976 to June 2015
###Data Finder tool is at http://beta.bls.gov/dataQuery/find
###Downloading each state as a CSV file, you'll notice the BLS doesn't change the name of the downloaded file.
###I changed the file names to statename.csv as appropriate.
###If you know a cleaner source of this data, I'd love to hear about it.


library(rgdal)
library(ggplot2)
library(gridExtra)
library(grid)
library(proj4)
library(ggthemes)

#Initialize and create state index
statenamesGEO <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")
states <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New_Hampshire","New_Jersey","New_Mexico","New_York","North_Carolina","North_Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode_Island","South_Carolina","South_Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West_Virginia","Wisconsin","Wyoming")
statesGEO <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","11","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New_Hampshire","New_Jersey","New_Mexico","New_York","North_Carolina","North_Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode_Island","South_Carolina","South_Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West_Virginia","Wisconsin","Wyoming","72")
udata <- matrix(NA,nrow=474,ncol=50)
temp<-NULL

#read in data and store plot for each state
for (state in states){
  assign(paste(state,"stats",sep=""),read.csv(paste("~/Documents/BLSHistoricalUnemployment/",state,".csv",sep="")))
  temp <- read.csv(paste("~/Documents/BLSHistoricalUnemployment/",state,".csv",sep=""))
  udata[,match(state,states)] <- temp$Value
  print(state)
  temp <- data.frame(temp)
  temp$x <- 1:474
  p <- ggplot(data=temp,aes(x,Value))
  p <- p + ggtitle(state)
  p <- p + geom_path()
  p <- p + scale_x_continuous(limits=c(-11,481),breaks=seq(-11,481,by=60),labels=c(1975,1980,1985,1990,1995,2000,2005,2010,2015))
  p <- p + scale_y_continuous(limits=c(0,20),breaks=seq(0,20,by=5),labels=c("0%","5%","10%","15%","20%"))
  p <- p + labs(y = "Unemployment Rate",x="Year",family="Courier")
  p <- p + theme(axis.text.x = element_text(family="Courier", size =16,color="black", angle = 00, vjust=.25))
  p <- p + theme(axis.text.y = element_text(family="Courier", size =16,color="black", angle = 00, vjust=.25))
  p <- p + theme(axis.title.y = element_text(family="Courier", size=18, angle=90, vjust=0.25))
  p <- p + theme(axis.title.x = element_text(family="Courier", size=18, angle=00, vjust=0.25))
  p <- p + theme(plot.title=element_text(family="Courier", face="bold", size=20))
  assign(paste(state,"plot",sep=""),p)
}

#Choropleth design heavily influenced by 
#Shapefile can be found at https://www.census.gov/geo/maps-data/data/cbf/cbf_state.html
us <- readOGR(dsn="~/Documents/cb_2014_us_state_20m", layer="cb_2014_us_state_20m")
us_aea <- spTransform(us, CRS("+proj=stere +lat_0=5 +lon_0=-100 +x_0=0 +y_0=0  +a=6370997 +b=6370997+units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

#mapping process heavily influenced by post at http://www.r-bloggers.com/moving-the-earth-well-alaska-hawaii-with-r/
#extract, rotate, shrink and move alaska
alaska <- us_aea[us_aea$STATEFP=="02",]
alaska <- elide(alaska, rotate=-35)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 3.5)
alaska <- elide(alaska, shift=c(-2800000, 2200000))
proj4string(alaska) <- proj4string(us_aea)

# extract, rotate, shrink, and move hawaii
hawaii <- us_aea[us_aea$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-15)
hawaii <- elide(hawaii, scale=max(apply(bbox(hawaii), 1, diff)) / 1.15)
hawaii <- elide(hawaii, shift=c(-1000000, 2200000))
proj4string(hawaii) <- proj4string(us_aea)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
us_aea <- rbind(us_aea, alaska, hawaii)

map <- fortify(us_aea, region="GEOID")



#Loop through each year/month to create frames
for (x in 1:474){
us_aea@data$unemployment <- rep(0,51)

for (state in statenamesGEO){
  us_aea@data$unemployment[match(state,us_aea@data$NAME)]<-udata[x,match(state,statenamesGEO)]
  print(x)
}
gg <- ggplot()
gg <- gg + geom_map(data=map, map=map,aes(x=long, y=lat, map_id=id, group=group),fill="grey94", color="black", size=1)
gg <- gg + geom_map(data=us_aea@data, map=map, aes(map_id=GEOID, fill=unemployment),color="black", size=0.75)
gg <- gg + scale_fill_gradientn(guide=guide_colourbar(barwidth = 2.5, direction="vertical",barheight = 30,title.position="top",title=""),limits=c(0,20),colours=c("royalblue4","white","lightsalmon","red3","dark red"),na.value="#ffffff",labels=rep("",5))
gg <- gg + theme(legend.title.align=.7)
gg <- gg + coord_equal()
gg <- gg + theme_map()
gg <- gg + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
gg <- gg + theme(plot.title=element_text(size=40,hjust=0))
gg <- gg + theme(axis.text.x=element_blank(), axis.text.y=element_blank())
gg <- gg + theme(legend.position=c(.95,.10),legend.title=element_text(size=20))
gg <- gg + theme(plot.background = element_rect(fill = "grey94",colour="grey94"))
gg <- gg + theme(panel.background = element_rect(fill = "grey94",colour="grey94"))
gg <- gg + theme(legend.background = element_rect(fill= "grey94"))
#gg <- gg+ geom_text(aes(x=750000, y=5200000), label="U.S.A. Seasonally Adjusted Unemployment",family="Courier",size=20,alpha=1)
gg <- gg + geom_text(aes(x=1770000, y=5200000), label=1976+x%/%12,family="Courier",size=50)
month <- x%%12
if (month == 0){
  month<-12
}
gg <- gg + geom_text(aes(x=1900000, y=4950000), label=sprintf("Month: %02d", month),family="Courier",size=15)
 
png(file=paste("~/Documents/BLSHistoricalUnemployment/",x,".png",sep=""),width=1920, height=1080)

print(gg + theme(plot.margin=unit(c(3.5,1,0,0),"cm")))
dev.off()

}

#multiplot function from cookbook-r.com
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Create multiplot to compare individual states
png("~/Documents/unemployment_multiplot.png",height=10000,width=2200)
multiplot(Alabamaplot, Alaskaplot, Arizonaplot, Arkansasplot, Californiaplot, Coloradoplot,Connecticutplot,Delawareplot,Floridaplot,Georgiaplot,Hawaiiplot,Idahoplot,Illinoisplot,Indianaplot,Iowaplot,Kansasplot,Kentuckyplot,Louisianaplot,Maineplot,Marylandplot,Massachusettsplot,Michiganplot,Minnesotaplot,Mississippiplot,Missouriplot,Montanaplot,Nebraskaplot,Nevadaplot,New_Hampshireplot,New_Jerseyplot,New_Mexicoplot,New_Yorkplot,North_Carolinaplot,North_Dakotaplot,Ohioplot,Oklahomaplot,Oregonplot,Pennsylvaniaplot,Rhode_Islandplot,South_Carolinaplot,South_Dakotaplot,Tennesseeplot,Texasplot,Utahplot,Vermontplot, Virginiaplot,Washingtonplot,West_Virginiaplot,Wisconsinplot,Wyomingplot,cols=4)
dev.off()




  