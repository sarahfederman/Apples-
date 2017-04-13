###################
#DATA MUNGING, APPLES AND PESTICIDES
#####################
library(dplyr);library(tidyr);library(magrittr)
#####################
#CHECK OUT THE DATA
####################
rm(list=ls())
getwd()
setwd("~/Dropbox/apple_data/Data/2012")
files <- dir("~/Dropbox/apple_data/Data/2012") #remind yourself what's in the directory
#############let's check out our datafiles
pestuse<-read.csv("Use_2012_Cnty.csv", skip=3, header=T) #skip first three lines on this dataset, because they're metadata
pestuse$cnty_name <- gsub("[[:space:]]", "", pestuse$cnty_name)
ingredients<-read.table("~/Dropbox/apple_data/Data/cornell_data/active_ingredients_2012.txt", sep="|", header=F, quote="", fill=T)
ingredients<-ingredients[,1:3]
colnames(ingredients)<-c("AI_code", "ai_name", "name_type") #give the epa_reg_number the same column name as the pesticide use data
products<-read.table("~/Dropbox/apple_data/Data/cornell_data/products_2012.txt", sep="|", header=F)
products<-products[,1:16]
colnames(products)<-c("prod_id", "reg_nr_19", "epa_company_num", "epa_prod_num", "epa_distrib_num", "epa_reg_num", "epa_reg_num_fix", "density", "audit_code", "restricted", "formstate", "reporting_year", "invalid_codes", "epa_ind", "flag_processed", "flag_cooltower")
prod_ingredients<-read.table("~/Dropbox/apple_data/Data/cornell_data/prod_ingredients_2012.txt", sep="|", header=F, fill=T)
prod_ingredients<-prod_ingredients[,1:5]
colnames(prod_ingredients)<-c("prod_ingred_id", "reg_nr_19", "AI_code", "prod_id", "ingred_percent")
#######################################
#JOIN PESTICIDE USE WITH ACTIVE INGREDIENT NUMBER AND THEN BY REGISTRATION NUMBER
#######################################
join1<-left_join(prod_ingredients, ingredients)
##########################################
#JOIN BY PRODUCT INGREDIENT NUMBER
#############################################
join2<-left_join(join1, products, by="prod_id")
###########################################
#JOIN BY EPA REGISTRATION NUMBER
##########################################
join3<-left_join(pestuse, join2)
##########################################
#NOW PULL OUT THE ORGANOPHOSPHATES
###########################################
#test
chlor<-which(join3$ai_name=="CHLORPYRIFOS")
acephate<-which(join3$ai_name=="ACEPHATE")
coumaphos<-which(join3$ai_name=="COUMAPHOS") #this doesn't seem to be in the list (2012, 2008,2009,2010,2011,2013)

ai<-read.csv("~/Dropbox/apple_data/Data/active_ingredients.csv")
ai.list<-as.vector(ai[,1])
ai.list<-toupper(ai.list)

filter(join3, ai_name %in% ai.list)->organo_ny
ny_organophosphates<-unique(organo_ny$ai_name)
filter(join3, ai_name == "CHLORPYRIFOS")->chlor_ny

write.table(organo_ny, "organophosphates_NY_2012.txt", quote=F, row.names=F, sep="\t")
#write.csv(ny_organophosphates, "active ingredients in NY 2012")
#write.table(chlor_ny, "chlorpyrifos_NY_2012.txt", quote=F, row.names=F, sep="\t")

#######################################
#test to make sure it was written correctly
ny<-read.table("organophosphates_NY_2012.txt", header=T, sep="\t")
##########################
#now do some quick and dirty conversions so that we get an 'amount applied' that's all in the same unit
ny$volume_qty <- gsub(",", "", ny$volume_qty)
ny$weight_qty <- gsub(",", "", ny$weight_qty)
ny$volume_qty <- gsub("^$", "0", ny$volume_qty)
ny$weight_qty <- gsub("^$", "0", ny$weight_qty)

ny$volume_qty<-as.numeric(as.character(ny$volume_qty))
ny$weight_qty<-as.numeric(as.character(ny$weight_qty))

ny_solid<-filter(ny, weight_qty > 0)
ny_liquid<-filter(ny, volume_qty > 0)
check<-filter(ny_liquid, weight_qty > 0)
write.csv(check, "double_reporting_2012.csv", quote=F)

##########################
formulation<-read.csv("double_reporting.csv", header=T)
ny_solid<-left_join(ny_solid, formulation)
ny_liquid<-left_join(ny_liquid, formulation)


ny_liquid<- within(ny_liquid, volume_qty[formulation[,9]=="SOLID"] <- 0)
ny_solid<- within(ny_solid, weight_qty[formulation[,9]=="LIQUID"] <- 0)
ny_solid<-filter(ny_solid, weight_qty > 0)
ny_liquid<-filter(ny_liquid, volume_qty > 0)

drop1<-c("volume_qty")
drop2<-c("weight_qty")

ny_liquid<-ny_liquid[, !(names(ny_liquid)%in%drop2)]
ny_solid<-ny_solid[, !(names(ny_solid)%in%drop1)]
#############################
ny_liquid$pound<- ny_liquid$volume_qty*(ny_liquid$ingred_percent/100)
ny_liquid$percentByWeight<-ny_liquid$pound/(ny_liquid$volume_qty*8.3)
ny_solid$pound<- ny_solid$weight_qty*(ny_solid$ingred_percent/100)
##########################

ny1<-full_join(ny_liquid, ny_solid) #by=c("cnty_name", "epa_reg_num"))
ny1<-dplyr ::: select(ny1, county=cnty_name, epa_reg=epa_reg_num, volume=volume_qty, weight=weight_qty, AI=AI_code, ingred_percent=ingred_percent, ai_name=ai_name, year=reporting_year, pounds_ai=pound, percentByWeight=percentByWeight, pound=pound)

#summary table

sumtable<-ny1 %>%
group_by(county, ai_name)%>%
summarise(AIused=sum(pound))
sumtable$year<-2012

write.csv(sumtable, "organophosphate_summary_table_2012.csv", quote=F)

#################################
#now make a combined dataset with years 2008-2013
rm(list=ls())
getwd()
setwd("~/Dropbox/apple_data/Data/cleaned_data")
##############################
t1<-read.csv("organophosphate_summary_table_2008.csv", header=T)
t2<-read.csv("organophosphate_summary_table_2009.csv", header=T)
t3<-read.csv("organophosphate_summary_table_2010.csv", header=T)
t4<-read.csv("organophosphate_summary_table_2011.csv", header=T)
t5<-read.csv("organophosphate_summary_table_2012.csv", header=T)
t6<-read.csv("organophosphate_summary_table_2013.csv", header=T)
#make sure that all column names are ordered in the same way with the same names before merging

t1<-dplyr:::select(t1, county=cnty_name, ai_name, AIused, year)
t2<-dplyr:::select(t2, county=cnty_name, ai_name, AIused, year)
t3<-dplyr:::select(t3, county, ai_name, AIused, year)
t4<-dplyr:::select(t4, county, ai_name, AIused, year)
t5<-dplyr:::select(t5, county, ai_name, AIused, year)
t6<-dplyr:::select(t6, county, ai_name, AIused, year)

#########
combined<-rbind(t1,t2,t3,t4,t5,t6)
write.csv(combined, "orhanophosphate_sum_combined.csv", quote=F)








