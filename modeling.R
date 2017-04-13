#install.packages("nlme")
library(nlme)
#install.packages("lme4")
install.packages("sjPlot")
library(lme4);library(sjPlot);library(car);library(dplyr);library(ggplot2);library(reshape2)
###################################
rm(list=ls())
##############################
setwd("~/Dropbox/apple_data/Data/cleaned_data")
###############################
ny<-read.table("geocode_summary", sep="\t", header=T)
ny$county<-gsub(", ny", "", ny$county)
#
spc <- ny %>%
    dplyr::: group_by(county, year) %>%
    dplyr::: summarise(pounds=sum(AIused))
spc$county<-as.character(spc$county) 
spc<-as.data.frame(spc)
spc$log<-log(spc$pounds)

#
spc1 <- spc %>%
    dplyr::: group_by(county) %>%
    dplyr::: summarise(pounds=mean(pounds))
spc1$county<-as.character(spc1$county) 
spc1<-as.data.frame(spc1)

##
spc2 <- spc %>%
    dplyr::: group_by(year) %>%
    dplyr::: summarise(pounds=mean(pounds))
spc2<-as.data.frame(spc2)

#

common<- filter(spc, county %in% c("monroe", "nassau", "erie", "westchester", "suffolk"))
##############################




m1<-lmer(AIused~year + (year|county), data=ny)

m2<-lmer(AIused~year + (1|county) + (1|ai_name), data=ny)

m3<-lmer(AIused~year + (1|county/ai_name), data=ny)


summary(m3)

#

sjp.lmer(m1, sort= "year")

ggplot (common, aes(x=year, y=pounds, group=county, color=county))+ geom_point(shape=16) + stat_smooth(method="lm")+ theme_bw()

#

s1<-lmer(pounds~year + (1 | county), data=spc)

anova(s1)

ggplot(common, aes(county, pounds, fill=county, color=county)) + geom_boxplot() + theme_bw(base_size=12)


names(spc)
ggplot(spc, aes(x = county, y = log, fill = county)) +
        geom_boxplot() + 
        theme(legend.position="none", axis.title.x = element_blank(), 
              axis.text.x= element_text(angle=45, hjust = 1)) +
        ggtitle("Average OP use in NY State by County (2008-2013)") +
        ylab("OP use (pounds)")
#

ggplot(spc1, aes(x = county, y = pounds, fill = county)) +
        geom_boxplot() + 
        theme(legend.position="none", axis.title.x = element_blank(), 
              axis.text.x= element_text(angle=45, hjust = 1)) +
        ggtitle("Average OP use in NY State by County (2008-2013)") +
        ylab("OP use (pounds)")
        
        
        
        
############
results<-data.frame(int=NA, slope=NA, Pr=NA, Rsq=NA)
for (i in unique(spc$county)){
	print(i)
	#i="westchester"
	c<-filter(spc, county == i)
	
	lm1<-lm(pounds~year, data=c)
	coefficients(lm1)
	results[i,1]<-summary(lm1)$coefficients[1,1]
	results[i,2]<-summary(lm1)$coefficients[2,1]
	results[i,3]<-summary(lm1)$coefficients[2,4]
	results[i,4]<-summary(lm1)$r.squared
	
	
}



results<-as.data.frame(results)
write.csv(results, "regression_results.csv", quote=F)

#############
spc3<-dcast(spc, county~year, value.var="pounds")
write.csv(spc3, "AI_used_by_year.csv", quote=F)
max<-max(spc$pounds)
min<-min(spc$pounds)
