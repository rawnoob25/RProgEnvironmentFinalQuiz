

makeTbl<-function(){
  setwd(paste0("C:/Users/That/Desktop/R/RProgEnvt/quiz_data","/data"))
  spec_data<-read_csv("daily_SPEC_2014.csv.bz2")
}

spec_data<-makeTbl()

##Q1:What is average Arithmetic.Mean 
#for "Bromine PM2.5 LC" in the state of Wisconsin in this dataset?

q1<-function(){
  useful<-spec_data%>%select(`Parameter Name`,`State Name`,`Arithmetic Mean`) 
  usefulFiltered<-useful%>%filter(`State Name`=="Wisconsin",`Parameter Name`=="Bromine PM2.5 LC")
  meanVal<-usefulFiltered%>%summarize("Average Bromine PM2.5 LC lvl in Wisconsin"=mean(`Arithmetic Mean`))
  return (meanVal)
}

##Q2:Calculate the average of each chemical constituent 
##across all states, monitoring sites and all time points. 

x<-spec_data%>%group_by(`Parameter Name`)%>%summarize(meanParamVals=mean(`Arithmetic Mean`))%>%arrange(desc(meanParamVals))


q2<-function(){
  x%>%filter(`Parameter Name`=="Sodium PM2.5 LC"|`Parameter Name`=="Sulfur PM2.5 LC"
             |`Parameter Name`=="OC CSN Unadjusted PM2.5 LC TOT"
             |`Parameter Name`=="EC2 PM2.5 LC")
}


#Q3:Which monitoring site has the highest average 
#level of "Sulfate PM2.5 LC" across all time?
#Indicate the state code, county code, and site number.
q3<-function(){
  q3tbl<-spec_data%>%group_by(`State Code`,`County Code`,`Site Num`)
  relevantQ3Tbl<-q3tbl%>%filter(`Parameter Name`=="Sulfate PM2.5 LC")
  relevantQ3Tbl%>%select(`State Code`,`County Code`,`Site Num`,`Parameter Name`,`Arithmetic Mean`)%>%arrange(desc(`Arithmetic Mean`))
}

#Q4:What is the absolute difference in the average levels of "EC PM2.5 LC TOR" between the 
#states California and Arizona, across all time and all monitoring sites?
q4<-function(){
  x<-spec_data%>%filter(`Parameter Name`=="EC PM2.5 LC TOR",`State Name`=="California"|`State Name`=="Arizona")
  grouped<-x%>%group_by(`State Name`)
  sumStats<-(grouped%>%select(`State Name`,`Parameter Name`,`Arithmetic Mean`)%>%summarize(meanlvl=mean(`Arithmetic Mean`)))
  print(paste("dim sum stats:",dim(sumStats)))
  print(paste("nrow sum stats:",nrow(sumStats)))
  print(paste("ncol sum stats:",ncol(sumStats)))
  print(paste("arizona data:",sumStats[1,2]))
  print(paste("california data:",sumStats[2,2]))
  return(abs(sumStats[1,2]-sumStats[2,2]))
}


#Q5:What is the median level of "OC PM2.5 LC TOR" in the western United States, across all time? 
#Define western as any monitoring location that has a Longitude LESS THAN -100.

q5<-function(){
  rel<-spec_data%>%filter(`Parameter Name`=="OC PM2.5 LC TOR"&Longitude< -100)
  sumRel<-rel%>%summarize(median_val=median(`Arithmetic Mean`))
  return(sumRel)
}

#Q6:Use the readxl package to read the file aqs_sites.xlsx 
#into R (you may need to install the package first). 
#This file contains metadata about each of the monitoring 
#sites in the EPA's monitoring system. In particular, the
#"Land Use" and "Location Setting" variables contain
#information about what kinds of areas the monitors 
#are located in (i.e. "residential" vs. "forest").

#How many monitoring sites are labelled as 
#both RESIDENTIAL for "Land Use" and SUBURBAN for "Location Setting"?
library(readxl)
aqsSites<-read_excel("aqs_sites.xlsx")
q6<-function(){
  rel<-aqsSites%>%filter(`Land Use`=="RESIDENTIAL"&`Location Setting`=="SUBURBAN")
  return (rel%>%summarize(count=n()))
}


#Q7:What is the median level of "EC PM2.5 LC TOR" amongst monitoring 
#sites that are labelled as both "RESIDENTIAL" and "SUBURBAN" in the eastern 
#U.S., where eastern is defined as Longitude greater than or equal to -100?
q7<-function(){
  ofInterest<-aqsSites%>%filter(`Land Use`=="RESIDENTIAL"&`Location Setting`=="SUBURBAN"&Longitude>= -100)
  #print(paste("uniqueLandUseVals:",ofInterest$`Land Use`%>%unique))
  #print(paste("uniqueLocSettingVals:",ofInterest$`Location Setting`%>%unique))
  #print(max(ofInterest$Longitude))
  #return(names(ofInterest))
  pK<-ofInterest%>%select(`State Code`,`County Code`,`Site Number`)
  
  vals3<-pK%>%mutate(`State Code`=str_pad(vals$`State Code`,width=2,side="left",pad="0"),
                       `County Code`=str_pad(vals$`County Code`,width=3,side="left",pad="0"),
                       `Site Num`=str_pad(vals$`Site Number`,width=4,side="left",pad="0"))%>%select(`State Code`,`County Code`,`Site Num`)
  
  #print(names(spec_data))
  #print(names(vals3))
  #print(intersect(names(spec_data),names(vals3)))
  k<-inner_join(spec_data,vals3,by=c("State Code"="State Code","County Code"="County Code","Site Num"="Site Num"))
  
  print("names(k)")
  print(names(k))
  
  ##useful<-k%>%filter(`Parameter Name`=="EC PM2.5 LC TOR")
  
  return (k)
}

q7reworked<-function(){
  ofInterest<-aqsSites%>%filter(`Land Use`=="RESIDENTIAL"&`Location Setting`=="SUBURBAN"&Longitude>= -100)  
  pk<-ofInterest%>%select(`State Code`,`County Code`,`Site Number`)
  pkFormatted<-pk%>%mutate(`State Code`=str_pad(pk$`State Code`,width=2,side="left",pad="0"),
                           `County Code`=str_pad(pk$`County Code`,width=3,side="left",pad="0"),
                           `Site Number`=str_pad(pk$`Site Number`,width=4,side="left",pad="0"))
  
  k<-inner_join(spec_data,pkFormatted,by=c("State Code"="State Code","County Code"
                                           ="County Code","Site Num"="Site Number"))
  relevantParam<-k%>%filter(`Parameter Name`=="EC PM2.5 LC TOR")
  
  return (relevantParam%>%summarize(medianVal=median(`Arithmetic Mean`,na.rm=T)))
}

q8<-function(){
  ofInterest<-aqsSites%>%filter(`Land Use`=="COMMERCIAL")
  pk<-ofInterest%>%select(`State Code`,`County Code`,`Site Number`)
  pkFormatted<-pk%>%mutate(`State Code`=str_pad(pk$`State Code`,width=2,side="left",pad="0"),
                           `County Code`=str_pad(pk$`County Code`,width=3,side="left",pad="0"),
                           `Site Number`=str_pad(pk$`Site Number`,width=4,side="left",pad="0"))
  useful<-inner_join(spec_data,pkFormatted,by=c("State Code"="State Code","County Code"="County Code",
                                                "Site Num"="Site Number"))
  
  useful_ParamOfInterest<-useful%>%filter(`Parameter Name`=="Sulfate PM2.5 LC")%>%group_by(months(`Date Local`))
  
  summary_data<-useful_ParamOfInterest%>%summarize(monthlyAvgs=mean(`Arithmetic Mean`))
  a<-summary_data ##simplified naming
  
  
  
  return (a[a[,2]==max(a[,2]),])
}

q9<-function(){
  correctMonitor<-spec_data%>%filter(`State Code`=="06",`County Code`=="065",
                                     `Site Num`=="8001",`Parameter Name`=="Total Nitrate PM2.5 LC"
                                     |`Parameter Name`=="Sulfate PM2.5 LC")
  collapsed<-correctMonitor%>%group_by(`Date Local`,`Parameter Name`)%>%mutate("Daily Avg"=mean(`Arithmetic Mean`))
  
  ofInterestV1<-collapsed%>%select(-`Arithmetic Mean`)
  ofInterestV2<-ofInterestV1%>%select(`Date Local`,`Parameter Name`,`Daily Avg`)%>%unique
  
  spreadOnConstituents<-ofInterestV2%>%spread(`Parameter Name`,`Daily Avg`)
  
  
  
  return (spreadOnConstituents%>%filter(`Sulfate PM2.5 LC`+`Total Nitrate PM2.5 LC`>10)%>%nrow)
}




###If you call arrange(desc(correl)) piping in result of call to q10() as first param
#you should get the data you need...answer is apparently wrong on the quiz
q10<-function(){
  grouped<-spec_data%>%filter(`Parameter Name`=="Sulfate PM2.5 LC"
                              |`Parameter Name`=="Total Nitrate PM2.5 LC")%>%group_by(`Parameter Name`,`Date Local`)
  withDailyAvg<-grouped%>%mutate("Daily Avg"=mean(`Arithmetic Mean`))%>%select(`State Code`,`County Code`,`Site Num`,`Date Local`,`Parameter Name`,`Daily Avg`)%>%unique
  
  withSpreadConstituents<-withDailyAvg%>%spread(`Parameter Name`,`Daily Avg`)
  
  groupedOnIdentifier<-withSpreadConstituents%>%group_by(`State Code`,`County Code`,`Site Num`)
  
  
  return (groupedOnIdentifier%>%summarize(correl=cor(`Sulfate PM2.5 LC`,`Total Nitrate PM2.5 LC`)))
}

q10Reworked<-function(){
#filter so we have only those obs w/ Param Name in c("Sulfate PM2.5 LC","Total Nitrate PM2.5 LC")
# group by(State Code,County Code,Site Num,Date Local,Param Name)
# mutate(dailyAvg=mean(Arithmetic Mean))
#select(State Code,County Code,Site Num,Date Local,Param Name,DailyAvg)%>%unique
#spread(Param Name,Daily Avg) #distinct values of Param Name become column headers and values
#are taken from DailyAvg column
filtered<-spec_data%>%filter(`Parameter Name`=="Sulfate PM2.5 LC"|`Parameter Name`=="Total Nitrate PM2.5 LC")
initialGrouping<-filtered%>%group_by(`State Code`,`County Code`,`Site Num`,`Date Local`,`Parameter Name`)
withDailyAvg<-initialGrouping%>%mutate("Daily Avg"=mean(`Arithmetic Mean`))
colsOfInterest<-withDailyAvg%>%select(`State Code`,`County Code`,`Site Num`,`Date Local`,`Parameter Name`,`Daily Avg`)%>%unique
#the reason there's "unique" on the previous line is that "Daily Avg would be present for every single
#observation; if at a particular monitor (id'd by State Code,COunty Code and Site Num)
#for a particular Parameter Name on a particular Date, there's more than one observation, then
#there'll be as many "Daily Avg" values as there are observations on that site monitor for that Parameter on that
#on that date...since we selected Daily Avg (note how `Arithmetic Mean` is not present in our select statement),
#unless we terminate the pipe chain w/ "unique", we're gonna have duplicate observations

spreadOnConstituent<-colsOfInterest%>%spread(`Parameter Name`,`Daily Avg`)%>%rename(sulf=`Sulfate PM2.5 LC`,nitr=`Total Nitrate PM2.5 LC`)

#NOW we can group on the variables that uniquely identify a monitor (State Code,County Code,Site Num)
#
#
#



toReturn<-spreadOnConstituent%>%group_by(`State Code`,`County Code`,`Site Num`)%>%summarize(correl=cor(sulf,nitr))

return(toReturn)  
}

#Q10:
#Which monitoring site in the dataset has the highest correlation between "Sulfate PM2.5 LC" 
#and "Total Nitrate PM2.5 LC" across all dates? Identify the monitoring site by it's State, 
#County, and Site Number code.
#For each of the chemical constituents, there will be some dates that have multiple Sample.Value's 
#at a monitoring site. When there are multiple values on a given date, take the average of the 
#constituent values for that date.
#Correlations between to variables can be computed with the cor() function.

q10ReworkedNoComments<-function(){
filtered<-spec_data%>%filter(`Parameter Name`=="Sulfate PM2.5 LC"|`Parameter Name`=="Total Nitrate PM2.5 LC")
initialGrouping<-filtered%>%group_by(`State Code`,`County Code`,`Site Num`,`Date Local`,`Parameter Name`)
withDailyAvg<-initialGrouping%>%mutate("Daily Avg"=mean(`Arithmetic Mean`))
colsOfInterest<-withDailyAvg%>%select(`State Code`,`County Code`,`Site Num`,`Date Local`,`Parameter Name`,`Daily Avg`)%>%unique
spreadOnConstituent<-colsOfInterest%>%spread(`Parameter Name`,`Daily Avg`)%>%rename(sulf=`Sulfate PM2.5 LC`,nitr=`Total Nitrate PM2.5 LC`)
toReturn<-spreadOnConstituent%>%group_by(`State Code`,`County Code`,`Site Num`)%>%summarize(correl=cor(sulf,nitr))
return(toReturn)  
}


#Q:How many (site monitor,date,parameter name) triples are there if the parameters  are restricted to Sulfate PM2.5 LC and Total Nitrate PM2.5 LC? 
q10Variant1V1<-function(){
	setwd("C:/Users/That/Desktop/R/RProgEnvt/quiz_data/data")
	spec_data<-read_csv("daily_SPEC_2014.csv.bz2")
	t1<-spec_data%>%filter(`Parameter Name`=="Sulfate PM2.5 LC"|
`Parameter Name`=="Total Nitrate PM2.5 LC")
	t2<-t1%>%group_by(`State Code`,`County Code`,`Site Num`,
`Date Local`,`Parameter Name`)	
	t3<-t2%>%mutate(DailyAvg=mean(`Arithmetic Mean`))
	t4<-t3%>%select(`State Code`,`County Code`,`Site Num`,`Parameter Name`,`Date Local`,val=`Arithmetic Mean`,DailyAvg)
	t4%>%summarize(count=n())%>%nrow #here count is the number of observations for each group
			
}

#Q:How many (site monitor,date,parameter name) triples are there if the parameters  are restricted to Sulfate PM2.5 LC and Total Nitrate PM2.5 LC? 
#(same question as q10Variant1V1())
q10Variant1V2<-function(){
	setwd("C:/Users/That/Desktop/R/RProgEnvt/quiz_data/data")
	spec_data<-read_csv("daily_SPEC_2014.csv.bz2")
	t1<-spec_data%>%filter(`Parameter Name`=="Sulfate PM2.5 LC"|
`Parameter Name`=="Total Nitrate PM2.5 LC")
	t2<-t1%>%group_by(`State Code`,`County Code`,`Site Num`,
`Date Local`,`Parameter Name`)	
	t3<-t2%>%mutate(DailyAvg=mean(`Arithmetic Mean`))
	t4<-t3%>%select(`State Code`,`County Code`,`Site Num`,`Parameter Name`,`Date Local`,val=`Arithmetic Mean`,DailyAvg)
	t4%>%select(-val)%>%unique%>%nrow
}

