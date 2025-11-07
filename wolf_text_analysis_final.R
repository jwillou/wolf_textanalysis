library(stm)
library(tm)
library(SnowballC)
library(Rtsne)
library(rsvd) 
library(geometry)
library(scales)
setwd("~/Uwyo/Wolf text analysis")
#save.image(file="analysis_quotesandtimes.RData") #can load to reduce comp time, data/analyis version 

####data cleaning and setup####
#removed non printing characters in .xlsx with clean()
#simplified headers
#replaced , ' " (three types) with blank

#read in data, select relevant rows
data = read.csv("wolf_article_data_3_31_25.csv", fill = TRUE)
data = data[!is.na(data$Number),]
table(data$Branch)

#assign date as count from earliest day
monthdays = data.frame(month=1:12, days=c(31,28,31,30,31,30,31,31,30,31,30,31), cdays=c(0,31,59,90,120,151,181,212,243,273,304,334))
data$Seqdate = data$SeqdateC = data$Year = data$Month = data$Day = rep(NA, nrow(data))
for(r in 1:nrow(data)){
  if(is.na(data$Date_Published[r])){next}
  t = strsplit(data$Date_Published[r], split="/")[[1]][3]
  tt = t
  if(t<=24&!is.na(t)){
    tt = paste("20",t,sep="")
  }
  if(t>24&!is.na(t)){
    tt = paste("19",t,sep="")
  }
  data$Year[r]  = tt
  data$Month[r] = strsplit(data$Date_Published[r], split="/")[[1]][1]
  data$Day[r]   = strsplit(data$Date_Published[r], split="/")[[1]][2]
  data$Seqdate[r] = ((as.numeric(data$Year[r])-1) * 365) + as.numeric(monthdays$cdays[monthdays$month==data$Month[r]]) + as.numeric(data$Day[r])
  month = as.character(data$Month[r])
  if(length(strsplit(month,"")[[1]])==1){month=paste("0",month,sep="")}
  day = data$Day[r]
  if(length(strsplit(day,"")[[1]])==1){day=paste("0",day,sep="")}
  data$SeqdateC[r] = paste(data$Year[r], month, day, sep="")
}

#filter to include only relevant data
rdata = data[data$Relevant=="Y",]

#organize branch info
rdata$Branch[rdata$Branch==""] = NA
branches = c("Judicial","Executive","Legislative","Public/Private","NGO")
rdata = rdata[rdata$Branch %in% branches, ]
table(rdata$Branch)

#set up two analysis datasets - quotes and full articles
quotes   = data.frame(Number=rdata$Number, Seqdate=rdata$Seqdate, Date_bin=rdata$Date_bin, Branch=rdata$Branch, Quote=rdata$Quote)
articles = data.frame(Number=rdata$Number, Seqdate=rdata$Seqdate, Date_bin=rdata$Date_bin, Branch=rdata$Branch, Content=rdata$Content)

analyze_quote = data.frame(docs = quotes$Quote, level = quotes$Branch, date = quotes$Seqdate)
analyze_quote = analyze_quote[complete.cases(analyze_quote),]
metadata_quote = analyze_quote[,2:3]
metadata_quote = as.data.frame(metadata_quote)

analyze_time = data.frame(docs = articles$Content, level = articles$Date_bin, date = articles$Seqdate)
analyze_time = analyze_time[complete.cases(analyze_time),]
metadata_time = analyze_time[,2:3]
metadata_time = as.data.frame(metadata_time)

####analyze data to identify topics - quotes####
lowerthreshold = 10  #words that do not appear in at least this many documents will be removed
text.quote   = textProcessor(documents = analyze_quote$docs, metadata = metadata_quote, removestopwords=T, customstopwords=c("get","like","one","re", "said","say","says","will", "wolves", "wolf", "gray", "wyoming", "idaho", "montana"))
output.quote = prepDocuments(text.quote$documents, text.quote$vocab, text.quote$meta, lower.thresh = lowerthreshold) #should this be higher?
output.quote.stm = stm(documents = output.quote$documents, vocab = output.quote$vocab,  prevalence =~ level, K = 5, max.em.its = 10000, data = output.quote$meta, init.type = "Spectral", seed = 2112)
output.quote.sel = selectModel(output.quote$documents, output.quote$vocab, prevalence =~ level, K = 5, max.em.its = 10000,  data = output.quote$meta, init.type = "Spectral", runs = 20, seed = 2112)
summary(output.quote.stm)
names(output.quote.sel)



#labelTopics(output.quote.stm)
plotModels(output.quote.sel)

#regressions and plots by political actor (level)
dir.create("output_actors")
sink(paste("output_actors/output_", lowerthreshold, "_combined.txt", sep=""), append=F)
print(text.quote)
print(summary(output.quote.stm))
print("#A measure of how often the top words in a topic appear together in the same documents.")
print("#Higher semantic coherence usually indicates that a topic is more meaningful and interpretable.")
print(output.quote.sel$semcoh[[1]])
print("#A measure of how unique the top words in a topic are to that topic (i.e., how rarely they appear in other topics).")
print("#High exclusivity means that the words defining a topic are not shared with other topics, which aids in distinguishing them.")
print(output.quote.sel$exclusivity[[1]])

actors = c("Executive", "Judicial", "Legislative", "Public/Private", "NGO")
lactors = length(actors)
colors7 = c("dodgerblue3", "darkorchid4", "firebrick3", "darkorange3", "goldenrod2", "chartreuse4", "navyblue")
for(i in 1:5){
  topic = paste(summary(output.quote.stm)[[1]][i,], collapse=" ")
  level.eff  = estimateEffect(c(i) ~ (level)-1, output.quote.stm, metadata = output.quote$meta, uncertainty = "None")
  print(topic)
  print(summary(level.eff))
  
  #make nice looking means plot
  pdf(file=paste("output_actors/output_", lowerthreshold, "_topic", i, "_combined.pdf", sep=""), width=8, height=5, onefile=T)
  reg = summary(level.eff)$tables[[1]]
  reg = as.data.frame(reg)
  reg$cats = rep(0, nrow(reg))
  for(r in 1:nrow(reg)){
    reg$cats[r] = strsplit(rownames(reg)[r], split="level")[[1]][2]
  }
  
  reg$lower_ci_95 = reg$Estimate - (reg$`Std. Error` * 1.96)
  reg$upper_ci_95 = reg$Estimate + (reg$`Std. Error` * 1.96)
  
  print(reg)
  
  ytop = 0.7 #upper ylim
  plot(-100,-100, xlim=c(0,(nrow(reg)+0.5)), ylim=c(0,ytop), xlab="Political Actor", ylab="Coefficient Estimate", axes=F, main=topic)
  axis(1,at=1:lactors, labels=actors, pos=0)
  axis(2,at=seq(0,ytop,0.1), labels=seq(0,ytop,0.1), pos=0)
  segments(x0=0,x1=(lactors+0.5),y0=0,y1=0)
  segments(x0=0,x1=(lactors+0.5),y0=ytop,y1=ytop)
  segments(x0=(lactors+0.5),x1=(lactors+0.5),y0=0,y1=ytop)
  for(a in 1:length(actors)){
    polygon(x=c((a-0.3), (a+0.3), (a+0.3), (a-0.3)), y=c(0,0,reg$Estimate[reg$cats==actors[a]],reg$Estimate[reg$cats==actors[a]]), col=colors7[a], border=T)
    segments(x0=(a-0.1), x1=(a+0.1), y0=(reg$Estimate[reg$cats==actors[a]]-(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), y1=(reg$Estimate[reg$cats==actors[a]]-(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), lwd=2)
    segments(x0=(a-0.1), x1=(a+0.1), y0=(reg$Estimate[reg$cats==actors[a]]+(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), y1=(reg$Estimate[reg$cats==actors[a]]+(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), lwd=2)
    segments(x0=a, x1=a, y0=(reg$Estimate[reg$cats==actors[a]]-(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), y1=(reg$Estimate[reg$cats==actors[a]]+(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), lwd=2)
  }
  dev.off()
}
sink()

####analyze data to identify topics - documents####
lowerthreshold = 10  #words that do not appear in at least this many documents will be removed
text.time   = textProcessor(documents = analyze_time$docs, metadata = metadata_time, removestopwords=T, customstopwords=c("get","like","one","re", "said","say","says","will", "wolves", "wolf", "gray", "wyoming", "idaho", "montana"))
output.time = prepDocuments(text.time$documents, text.time$vocab, text.time$meta, lower.thresh = lowerthreshold) #should this be higher?
output.time.stm = stm(documents = output.time$documents, vocab = output.time$vocab,  prevalence =~ level, K = 5, max.em.its = 10000, data = output.time$meta, init.type = "Spectral", seed = 2112)
output.time.sel = selectModel(output.time$documents, output.time$vocab, prevalence =~ level, K = 5, max.em.its = 10000,  data = output.time$meta, init.type = "Spectral", runs = 20, seed = 2112)
summary(output.time.stm)
names(output.time.sel)

plotModels(output.time.sel)

#regressions and plots by political actor (level)
dir.create("output_time")
sink(paste("output_time/output_", lowerthreshold, "_combined.txt", sep=""), append=F)
print(text.time)
print(summary(output.time.stm))
print("#A measure of how often the top words in a topic appear together in the same documents.")
print("#Higher semantic coherence usually indicates that a topic is more meaningful and interpretable.")
print(output.time.sel$semcoh[[1]])
print("#A measure of how unique the top words in a topic are to that topic (i.e., how rarely they appear in other topics).")
print("#High exclusivity means that the words defining a topic are not shared with other topics, which aids in distinguishing them.")
print(output.time.sel$exclusivity[[1]])

actors = c("Before", "During", "After")
lactors = length(actors)
colors3 = c("dodgerblue3", "goldenrod2", "chartreuse4")
for(i in 1:5){
  topic = paste(summary(output.time.stm)[[1]][i,], collapse=" ")
  level.eff  = estimateEffect(c(i) ~ (level)-1, output.time.stm, metadata = output.time$meta, uncertainty = "None")
  print(topic)
  print(summary(level.eff))
  
  #make nice looking means plot
  pdf(file=paste("output_time/output_", lowerthreshold, "_topic", i, "_combined.pdf", sep=""), width=5, height=5, onefile=T)
  reg = summary(level.eff)$tables[[1]]
  reg = as.data.frame(reg)
  reg$cats = rep(0, nrow(reg))
  for(r in 1:nrow(reg)){
    reg$cats[r] = strsplit(rownames(reg)[r], split="level")[[1]][2]
  }
  
  reg$lower_ci_95 = reg$Estimate - (reg$`Std. Error` * 1.96)
  reg$upper_ci_95 = reg$Estimate + (reg$`Std. Error` * 1.96)
  
  print(reg)
  
  ytop = 1 #upper ylim
  plot(-100,-100, xlim=c(0,(nrow(reg)+0.5)), ylim=c(0,ytop), xlab="Time Period", ylab="Coefficient Estimate", axes=F, main=topic)
  axis(1,at=1:lactors, labels=actors, pos=0)
  axis(2,at=seq(0,ytop,0.1), labels=seq(0,ytop,0.1), pos=0)
  segments(x0=0,x1=(lactors+0.5),y0=0,y1=0)
  segments(x0=0,x1=(lactors+0.5),y0=ytop,y1=ytop)
  segments(x0=(lactors+0.5),x1=(lactors+0.5),y0=0,y1=ytop)
  for(a in 1:length(actors)){
    polygon(x=c((a-0.3), (a+0.3), (a+0.3), (a-0.3)), y=c(0,0,reg$Estimate[reg$cats==actors[a]],reg$Estimate[reg$cats==actors[a]]), col=colors3[a], border=T)
    segments(x0=(a-0.1), x1=(a+0.1), y0=(reg$Estimate[reg$cats==actors[a]]-(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), y1=(reg$Estimate[reg$cats==actors[a]]-(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), lwd=2)
    segments(x0=(a-0.1), x1=(a+0.1), y0=(reg$Estimate[reg$cats==actors[a]]+(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), y1=(reg$Estimate[reg$cats==actors[a]]+(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), lwd=2)
    segments(x0=a, x1=a, y0=(reg$Estimate[reg$cats==actors[a]]-(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), y1=(reg$Estimate[reg$cats==actors[a]]+(reg$`Std. Error`[reg$cats==actors[a]]*1.96)), lwd=2)
  }
  dev.off()
}
sink()


#

