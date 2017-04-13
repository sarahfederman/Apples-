library(ggplot2);library(devtools);library(dplyr);library(stringr);library(maps);library(mapdata);library(ggmap);library(sp);library(raster);library(maptools);library(rgdal);library(RColorBrewer);library(plyr);library(grid);library(gstat); library(classInt);library(gridExtra);library(colorspace);library(grid)

###################################
rm(list=ls())
##############################
setwd("~/Dropbox/apple_data/Data/cleaned_data")
###############################
ny1<-read.table("orhanophosphate_sum_combined.csv", header=T, sep=",")
#ny$county<-paste(ny$county, ", NY", sep="")
#ny$county<-as.character(ny$county)
ny1$county<-tolower(ny1$county)

invalid<-c("invalidcounty, ny", "unreportedcounty, ny", "irregularcounty, ny", "illegiblecounty, ny" )
ny1<-filter(ny1, !county %in% invalid)


#for(i in 1:length(ny$county)){
 # temp<-geocode(ny$county[i]) #Geocode each location and store it in temp
  #ny$lon[i]<-temp$lon           #Assign the lon
  #ny$lat[i]<-temp$lat           #Assign the lat
#}

#write.table(ny, "geocode_summary", sep="\t", quote=F, row.names=F)
 
ny1<-as.data.frame(ny1)
ny<-read.table("geocode_summary", sep="\t", header=T)
ny$county<-gsub(", ny", "", ny$county) 
#summarize geocoded data by to get total active ingredients used by county and year
spc <- ny %>%
    dplyr::: group_by(county, year) %>%
    dplyr::: summarise(pounds=sum(AIused))
spc$county<-as.character(spc$county) 
spc<-as.data.frame(spc)
spc$log<-log(spc$pounds)


#summarize geocoded data to get average active ingredients used by county over a 6 year period
spc1 <- spc %>%
    dplyr::: group_by(county) %>%
    dplyr::: summarise(pounds=mean(pounds))
spc1$county<-as.character(spc1$county) 
spc1<-as.data.frame(spc1)

########
#summarize by active ingredient over 6 years
#standard deviations show that there is a lot of variation year to year. we'll have to deal with that in downstream analyses
spc2 <- ny %>%
    dplyr::: group_by(county, ai_name) %>%
    dplyr::: summarise(pounds=mean(AIused), std=sd(AIused), count=n())
spc2$county<-as.character(spc2$county) 
spc2<-as.data.frame(spc2)
spc2$pounds[is.na(spc2$pounds)] <- 0
spc2$std<-round(spc2$std, 3)

##################################

NY<-shapefile('~/Dropbox/apple_data/NY_counties_clip/NY_counties_clip.shp') #note that the shapefile is read in as a spatial polygons data frame
NY<-fortify(NY, region="NAME")
NY$id<-tolower(NY$id)

#############################################
#BEFORE MAPPING: need to do some descriptive statistics to determine the best range of intervals needed to map the data in the most informative manner

# across all of the years
min1<-min(spc$pounds)
max1<-max(spc$pounds)
diff1<-max1-min1
std1<-sd(spc$pounds)

equal.interval1 <- round(seq (min1, max1, by = diff1/10),3)
quantile.interval1<-round(quantile(spc$pounds, probs=seq(0,1, by = 1/10)),3)
std.interval1<- round(c(seq(min1,max1, by=std1), max1),3)
natural.interval1<-round(classIntervals(spc$pounds, n = 10, style = "jenks")$brks,3)

spc$pounds.equal<-cut(spc$pounds, breaks = equal.interval1, include.lowest = TRUE)
spc$pounds.quantile<-cut(spc$pounds, breaks = quantile.interval1, include.lowest = TRUE)
spc$pounds.std<-cut(spc$pounds, breaks = std.interval1, include.lowest = TRUE)
spc$pounds.natural<-cut(spc$pounds, breaks = natural.interval1, include.lowest = TRUE)

ggplot(spc1, aes(x=pounds)) + geom_histogram() +geom_vline(aes(xintercept=pounds), color='red') #plot of distribution of pounds AI used

ggplot(spc, aes(x=pounds)) +geom_vline(aes(xintercept=pounds), color='red')+ geom_histogram(breaks=natural.interval1, stat='bin') 

ggplot(spc, aes(x=pounds)) +geom_vline(aes(xintercept=pounds), color='red')+ geom_histogram(breaks=quantile.interval1, stat='bin') 

ggplot(spc, aes(x=pounds)) +geom_vline(aes(xintercept=pounds), color='red')+ geom_histogram(breaks=std.interval1, stat='bin') 

ggplot(spc, aes(x=pounds)) +geom_vline(aes(xintercept=pounds), color='red')+ geom_histogram(breaks=equal.interval1, stat='bin') 


#6 year average
min2<-min(spc1$pounds)
max2<-max(spc1$pounds)
diff2<-max2-min2
std2<-sd(spc1$pounds)

equal.interval2 <- seq (min2, max2, by = diff2/10)
quantile.interval2<-quantile(spc1$pounds, probs=seq(0,1, by = 1/10))
std.interval2<-c(seq(min2,max2, by=std2), max2)
natural.interval2<-classIntervals(spc1$pounds, n = 10, style = "jenks")$brks

spc1$pounds.equal<-cut(spc1$pounds, breaks = equal.interval2, include.lowest = TRUE)
spc1$pounds.quantile<-cut(spc1$pounds, breaks = quantile.interval2, include.lowest = TRUE)
spc1$pounds.std<-cut(spc1$pounds, breaks = std.interval2, include.lowest = TRUE)
spc1$pounds.natural<-cut(spc1$pounds, breaks = natural.interval2, include.lowest = TRUE)

####################################
#MAP EACH YEAR SEPARATELY
#####################################

#try a single year as a tester
ggplot() + geom_map(data=subset(spc, year==2008), aes(map_id=county, fill=log),
	map=NY) + expand_limits(x=NY$long, y=NY$lat)

##Make the Loop for the jenks breaks (natural breaks) data to plot each year separately
library(lattice)
for(i in 1:length(unique(ny$year))){
	pdf(paste("organo_ny_binned",unique(ny$year)[i], ".pdf", sep=""))
	print(ggplot() + geom_map(data=subset(spc, year==unique(ny$year)[i]), aes(map_id=county, fill=pounds.natural),
	map=NY) + expand_limits(x=NY$long, y=NY$lat) + scale_fill +map.theme)		
	dev.off()
	print(i)
}
####################################
#plot the 6 year average with natural breaks

pdf("six year average, organophosphate use NY State.pdf")
print(ggplot() + geom_map(data= spc1, aes(map_id=county, fill=pounds.natural),
	map=NY) + expand_limits(x=NY$long, y=NY$lat) + scale_fill + map.theme)
	dev.off()

#######################################
#Now, let's make those maps pretty...

#color scheme:

colors<-diverge_hcl(10, c=100, l=c(50,90), power = c(1))
scale_fill<-scale_fill_manual(values=colors)
qplot(color, fill=color, data=data.frame(color=colors)) + scale_fill + common.theme + guides(fill=F) + ylab(NULL)

common.theme = theme(axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title=element_blank(),
    axis.text.x=element_blank(),
    axis.title.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_text(size=10, face='bold'),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())
map.theme = common.theme + theme(axis.title.y=element_blank())




top.graph = theme(plot.margin = unit(c(0,.5,-.5, .5), "line"))
middle.graph = theme(plot.margin = unit(c(-.5,.5,-.5, .5), "line"))
bottom.graph = theme(plot.margin = unit(c(-.5,.5,.5, .5), "line"))
common.scale_y = scale_y_continuous(limits=c(0,60)) #get some space between the histograms


common.vline = geom_vline(aes(xintercept = pounds), color=colors[1], size=0.5)
interval_rug<- function(breaks) {return (geom_linerange(aes(x=pounds), ymin=5, ymax=0, data.frame(pounds=breaks)))}

hist<-function(interval, position, label) {
	
	return(
	ggplot(spc1, aes(x=pounds))+
	common.vline +
	geom_histogram(breaks = interval, fill = "black", color ="black") +
	geom_line (y=0) + #faux x-axis
	interval_rug(interval)+
	common.scale_y +
	position +
	common.theme +
	ylab(label)

	)
}

hist.equal<-hist(equal.interval2, top.graph, "Equal Interval")
hist.quantile<-hist(quantile.interval2, middle.graph, "Quantile")
hist.std<-hist(std.interval2, middle.graph, "Std Deviation")
hist.natural<-hist(natural.interval2, bottom.graph, "Natural Breaks")

map<- function (cuts) {
	return(
	ggplot() + geom_map (data= spc1, aes(map_id=county, fill = cuts), map=NY) + expand_limits(x=NY$long, y=NY$lat) + scale_fill + guides (fill=FALSE) + map.theme
	)
}

map.equal<-map(spc1$pounds.equal)
map.quantile<-map(spc1$pounds.quantile)
map.std<-map(spc1$pounds.std)
map.natural<-map(spc1$pounds.natural)

grid.arrange(	
	hist.equal, map.equal, 
	hist.quantile, map.quantile,
	hist.std, map.std, 
	hist.natural, map.natural,
	ncol =2, widths=c(4,1), heights=c(3,3,3,3,3))


###############################
for(i in 2008:2013){
	#i=2013
	g<-
	(spc, year==i)
	hist<-function(interval, position, label) {
	return(
	ggplot(spc, aes(x=pounds))+
	common.vline +
	geom_histogram(breaks = interval, fill = "black", color ="black") +
	geom_line (y=0) + #faux x-axis
	interval_rug(interval)+
	common.scale_y +
	position +
	common.theme +
	ylab(label)

	)
}
	hist.equal<-hist(equal.interval1, top.graph, "Equal Interval")
	hist.quantile<-hist(quantile.interval1, middle.graph, "Quantile")
	hist.std<-hist(std.interval1, middle.graph, "Std Deviation")
	hist.natural<-hist(natural.interval1, bottom.graph, "Natural Breaks")
	
	map<- function (cuts) {
	return(
	ggplot() + geom_map (data= g, aes(map_id=county, fill = cuts), map=NY) + expand_limits(x=NY$long, y=NY$lat) + scale_fill + guides (fill=FALSE) + map.theme
	)
}

	map.equal<-map(g$pounds.equal)
	map.quantile<-map(g$pounds.quantile)
	map.std<-map(g$pounds.std)
	map.natural<-map(g$pounds.natural)

	
	pdf(paste("organo_ny",i, ".pdf", sep=""))
	print(grid.arrange(	
	hist.equal, map.equal, 
	hist.quantile, map.quantile,
	hist.std, map.std, 
	hist.natural, map.natural,
	ncol =2, widths=c(4,1), heights=c(3,3,3,3,3))	)	
	dev.off()
	print(i)
}
################################################
#plot each active ingredient indvidually


# across all of the years
min3<-min(spc2$pounds)
max3<-max(spc2$pounds)
diff3<-max3-min3
std3<-sd(spc2$pounds)

equal.interval3 <- round(seq (min3, max3, by = diff1/10),3)
quantile.interval3<-round(quantile(spc2$pounds, probs=seq(0,1, by = 1/10)),3)
std.interval3<- round(c(seq(min3,max3, by=std3), max3),3)
natural.interval3<-round(classIntervals(spc2$pounds, n = 10, style = "jenks")$brks,3)

spc2$pounds.equal<-cut(spc2$pounds, breaks = equal.interval3, include.lowest = TRUE)
spc2$pounds.quantile<-cut(spc2$pounds, breaks = quantile.interval3, include.lowest = TRUE)
spc2$pounds.std<-cut(spc2$pounds, breaks = std.interval3, include.lowest = TRUE)
spc2$pounds.natural<-cut(spc2$pounds, breaks = natural.interval3, include.lowest = TRUE)
spc2$pounds




colors<-diverge_hcl(10, c=100, l=c(50,90), power = c(1))
scale_fill<-scale_fill_manual(values=colors)
qplot(color, fill=color, data=data.frame(color=colors)) + scale_fill + common.theme + guides(fill=F) + ylab(NULL)


for(i in unique(spc2$ai_name)){
	g<-filter(spc2, ai_name==i)
	std<-sd(g$pounds)
	min<-min(g$pounds)
	max<-max(g$pounds)
	std.interval<-c(seq(min, max, by=std), max)
	g$pounds.std<-cut(g$pounds, breaks=std.interval, include.lowest=T)
	pdf(paste("ai_std_",i, ".pdf", sep=""))
	print(ggplot() + geom_map(data=g, aes(map_id=county, fill=pounds.std),
	map=NY) + expand_limits(x=NY$long, y=NY$lat) + scale_fill +map.theme +  geom_map(data=spc2, aes(map_id=county, fill=NA),color="grey", map=NY))	
	dev.off()
	print(i)
}

for(i in unique(spc2$ai_name)){
	g<-filter(spc2, ai_name==i)
	pdf(paste("ai_avg_",i, ".pdf", sep=""))
	print(ggplot() + geom_map(data=g, aes(map_id=county, fill=pounds.natural),
	map=NY) + expand_limits(x=NY$long, y=NY$lat) + scale_fill +map.theme +  geom_map(data=spc2, aes(map_id=county, fill=NA),color="grey", map=NY))	
	dev.off()
	print(i)
}

