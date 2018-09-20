### Figures and Tables
setwd()
setwd("") 
list.files()
library(scales)
library(reshape2)
library(ggplot2)
require(RColorBrewer)
library(ReporteRs)

## just aesthetic theme for plot in the function below
fte_theme <- function(xaxis=TRUE, size=1, topmargin=-2, keysize=2) {
  require(RColorBrewer)
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[7]
  color.axis.title = palette[8]
  color.title = palette[9]
  if(xaxis){ foo<-element_text(size=10,color=color.axis.text)}
  else(foo<-element_blank())
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,
                                        size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    # Format the legend
    theme(legend.position="bottom") +
    theme(legend.background = element_rect(fill=color.background, linetype = 0)) +
    theme(legend.text = element_text(size=10*size,color=color.axis.title)) +
    theme(legend.title=element_blank()) +
    theme(legend.margin=margin(t=topmargin, b=topmargin, unit="pt")) +
    theme(legend.key = element_rect(colour = color.background, size=0)) +
    theme(legend.key.size = unit(keysize, "cm"))+
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=50*size, vjust=1.25)) +
    theme(axis.text.y =element_text(size=10*size,color=color.axis.text)) +
    theme(axis.text.x=foo) +
    theme(axis.title.x=element_text(size=12*size,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=12*size,color=color.axis.title, vjust=1.25)) +
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.1), "cm")) + ## def c(0.35, 0.2, 0.3, 0.35) +
    # Facet
    theme(strip.text.x = element_text(size = 10*size*.8, colour = color.axis.text, 
                                      face = "bold")) +
    theme(strip.background = element_rect(fill=color.grid.major, linetype = 0)) 
}

## READ FILES
npb<-read.table(file = "npb_summary.txt", sep=";", dec = ",", header=T, nrows = -1, quote="", comment.char="")
npc<-read.table(file = "npc_summary.txt", sep=",",  header=T, nrows = -1, quote="", comment.char="")

head(npc)
length(npc$created_at) ## 189
length(npb$created_at) ## 82
### Read the centre codind file
dpath<-c("") # PATH
centres<-read.csv(file = paste(dpath,"npd_centre.csv", sep=""), sep=";", dec = ",", header=T, nrows = -1, quote="", comment.char="")
centres<-centres[,1:4]
strip.blanks <- function (x) gsub("^\\s+|\\s+$", "", x)
names(centres)<-gsub("[.]", "", strip.blanks(gsub("X.", "", names(centres))))
head(centres)
centres$id<-as.numeric(gsub("\"", "" ,centres$id))
centres$name<-gsub("\\\\s", "'s ", gsub("'", "",
                                        gsub("\"", "", centres$name)))
centres$code<-gsub("'", "", gsub("\"", "", centres$code))


## GT THE DV and DC values and translate to numeric
nama<-c(grep("DV", names(npb)),grep("DC", names(npb)),
        grep("errors", names(npb)),grep("total", names(npb)),grep("unknown", names(npb)))
npb[,nama]<-sapply(npb[,nama], function(a){as.numeric(gsub(",", ".", a))})

sapply(npc, class)
nama<-c(grep("DV", names(npc)),grep("DC", names(npc)),
        grep("errors", names(npc)),grep("total", names(npc)),grep("unknown", names(npc)))
npc[,nama]<-sapply(npc[,nama], function(a){as.numeric(gsub(",", ".", a))})

## typos
names(npc)<-gsub("clincal", "clinical", gsub("familiy", "family" ,names(npc)))

nrow(npb<-subset(npb, !is.na(centre_id)))
sum(with(npc, table(centre_id, useNA="ifany")))
## PREPARE Datasets for plotting; means by center
datamanage<-function(origdata=npb, centreid="centre_id"){
  types<-gsub("DC_", "", c(grep("DC", names(origdata), value=T)))
  dfs<- list()
  
  for (i in 1:length(types)){
    dfs[[i]]<-data.frame(DC=c(with(origdata, tapply(get(paste0("DC_",types[[i]])),
                                                    get(centreid), mean, na.rm=T))),
                         DV=c(with(origdata, tapply(get(paste0("DV_", types[[i]])), 
                                                    get(centreid), mean, na.rm=T))))
    dfs[[i]]$centre<-rownames(dfs[[i]])
    dfs[[i]]$n<-c(with(origdata, table(get(centreid))))
    N<-sum(dfs[[i]]$n)
    dfs[[i]]<-dfs[[i]][order(dfs[[i]]$n, decreasing = T),] 
    alldc<-with(origdata, mean(get(paste0("DC_", types[[i]])), na.rm=T))
    alldv<-with(origdata, mean(get(paste0("DV_", types[[i]])), na.rm=T))
    dfs[[i]]<-  rbind(c(alldc, alldv, "Total", N/2), dfs[[i]])
    dfs[[i]]$right<-cumsum(dfs[[i]]$n) + 9*c(0:(nrow(dfs[[i]])-1))
    dfs[[i]]$left<-dfs[[i]]$right - as.numeric(dfs[[i]]$n) 
    dfs[[i]][,!names(dfs[[i]])%in%"centre"]<-sapply(dfs[[i]][,!names(dfs[[i]])%in%"centre"], 
                                                    as.numeric)
    names(dfs)[i]<-types[i]
  }
  dfs
}

npbdat<-datamanage() ## here are the calculated values
npcdat<-datamanage(origdata=npc)

na.0<-function(a) ifelse(is.na(a), 0, a)
### and twice  more reformat the datasets for plotting
meltfun<-function(data=npbdat[[1]], id.vars="centre"){
  data$dq<-with(data, na.0(DC)+na.0(DV) )
  meltdat<-melt(data[,c(1:3)],id.vars = id.vars)
  meltdat$value<-as.numeric(meltdat$value)
  meltdat$n<- rep(data$n,2)
  meltdat$right<- rep(data$right,2)
  meltdat$left<- rep(data$left,2)
  meltdat$dq<- rep(data$dq,2)/2
  meltdat
}


npb_meltd<-lapply(npbdat, meltfun)
npc_meltd<-lapply(npcdat, meltfun)
37+11+4+2+1
doplotdata<-function(dataf=npb_meltd){
  for(i in 1:length(dataf)){ dataf[[i]]$type<-names(dataf)[i] }
  da<-do.call("rbind", dataf)
  foox<-centres$name[match(da$centre, centres$id)]
  da$centre2<-ifelse(!is.na(foox), foox, as.character(da$centre))
  coox<-centres$code[match(da$centre, centres$id)]
  da$centrecode<-ifelse(!is.na(coox), coox, as.character(da$centre))
  da
}

npb_plotdata<-doplotdata()
npc_plotdata<-doplotdata(dataf = npc_meltd)

### Assign countrycodes
npb_plotdata$countrycode<-with(npb_plotdata, ifelse(centrecode=="BA1", "Spain 1", ifelse(centrecode=="UD2",
                                                                                         "Italy 1", 
                                                                                         ifelse(centrecode=="BI1", "UK 1",
                                                                                                ifelse(centrecode=="BI3", "UK 2",
                                                                                                       ifelse(centrecode=="PR1", "Czech 1", centrecode))))))


npc_plotdata$countrycode<-with(npc_plotdata, 
                               ifelse(centrecode=="BA1", "Spain 1", 
                                      ifelse(centrecode=="UD2",  "Italy 1", 
                                             ifelse(centrecode=="BI1", "UK 1",
                                                    ifelse(centrecode=="BI3", "UK 2",
                                                           ifelse(centrecode=="PR1", "Czech 1", 
                                                                  ifelse(centrecode=="TU2", "Germany 1",
                                                                         ifelse(centrecode=="MA3", "UK 3", 
                                                                                ifelse(centrecode=="LO6", "UK 4", 
                                                                                       ifelse(centrecode=="DU1", "Ireland 1", centrecode)
                                                                                )))))))))


unique(c(npb_plotdata$type, npc_plotdata$type))

### Assign form data names

npb_plotdata$type<-with(npb_plotdata,
                        ifelse(type%in%c("pat","patient"),"Patient Enrolment",
                               ifelse(type=="lab", "Lab Diagnosis",
                                      ifelse(type=="family","Family History",
                                             ifelse(type=="clinical","Clinical History",
                                                    ifelse(type=="med", "Treatments",
                                                           ifelse(type=="visits","Follow up visits", type)))))))


npc_plotdata$type<-with(npc_plotdata,
                        ifelse(type%in%c("pat","patient"), "Patient Enrolment",
                               ifelse(type=="lab", "Lab Diagnosis",
                                      ifelse(type=="family","Family History",
                                             ifelse(type=="clinical","Clinical History",
                                                    ifelse(type=="med", "Treatments",
                                                           ifelse(type=="visits","Follow up visits", type)))))))


## change DV -> DA
npc_plotdata$variable<-ifelse(npc_plotdata$variable =="DV", "DA", as.character(npc_plotdata$variable))
npb_plotdata$variable<-ifelse(npb_plotdata$variable =="DV", "DA", as.character(npb_plotdata$variable))


###############################
######## Finally: the PLot ####
###############################

col1<-"#CCDAC7"
col2<- "#84ACCE"

plotfun<-function(data=npb_plotdata, col1="#CCDAC7", col2="#84ACCE",
                  centrevar="centrecode", ordervar="dq", 
                  decreasing=FALSE, labels=TRUE, round=1, xaxis=TRUE, report.N=TRUE,
                  size=1, topmargin=-2, keysize=0.5 ){
  ### make dc negative, dv positive to tweak
  ##df<-m_pat
  data$value2<-with(data, ifelse(variable=="DC", -1*value, value))
  data$width<-data$right-data$left
  
  sub<-subset(data, get(centrevar)!="Total")
  namit<-names(sort(with(sub, tapply(get(ordervar), get(centrevar), sum, na.rm=T)), 
                    decreasing = decreasing))
  data$centreid<-with(data, factor(get(centrevar), levels=c(namit, "Total")))
  
  if(report.N){
    annat<-sub[,"n"][match(namit, sub[,centrevar])]
    dqq<-with(data, tapply(dq, get(centrevar), mean, na.rm=T))
    dataq<-dqq[match(namit, names(dqq))]
    
    totn<-sum(annat)
    totdq<-mean(subset(data, get(centrevar)=="Total")$dq, na.rm=T)
    
    labelit<-c(paste(namit, "\n(N=", annat, ", DQ=", round(dataq, round), "%)", sep=""), 
               paste("Total","\n(N=", totn, ", DQ=", round(totdq,round), "%)", sep=""))
    
    
  }
  else(labelit<- levels(data$centreid))
  
  p<-ggplot(data,aes(x=centreid,y=value2,fill=variable, label=round(value,round),
                     width=rescale(width,c(0.4,1)))) +
    geom_bar(stat="identity",position="stack" ) +coord_flip()+ 
    fte_theme(xaxis = xaxis, size=size, topmargin=topmargin, keysize=keysize) + 
    xlab("") + ylab("Data quality (%)") + facet_grid(~type) + 
    scale_y_continuous(breaks=seq(-100, 100, by=50) ,labels=abs(seq(-100, 100, by=50))) +
    scale_fill_manual(values=c(col1, col2))+
    scale_x_discrete(labels=c(labelit)) 
  
  palette <- brewer.pal("Greys", n=9)
  color.axis.title = palette[8]
  if(labels){p<-p+geom_text(size = 3.5*size, color=color.axis.title, position = position_stack(vjust = 0.5))}
  
  p
}


##width 3,19 inch
## THEY Look like these
plotfun(data = npb_plotdata, ordervar = "dq", centrevar = "countrycode", xaxis=F)
plotfun(data=npc_plotdata, ordervar ="dq", centrevar="countrycode" , size=.5)

##############################
## ADJUSTMENTS
##############################

## 1. remove those from cetres UK4, Italy 1, Germany 1, UK 3? 

notthese<-c("UK 4", "Italy 1", "Germany 1", "UK 3") 
plotfun(data=subset(npc_plotdata, !(countrycode%in%notthese)), ordervar ="dq", centrevar="countrycode" )


## 2. modify arguments of plot
plotfun(data=subset(npc_plotdata, !(countrycode%in%notthese)), ordervar ="dq", centrevar="countrycode", xaxis=F, round=0 )

npcplotname<-"npc_plot2.png"
## To save it in png -file rn from here
width<-3.5
png(npcplotname, width = width, height=width*.65 , res=2000, units="in")
plotfun(data=subset(npc_plotdata, !(countrycode%in%notthese)), ordervar ="dq", centrevar="countrycode", 
        xaxis=F, round=0, size=0.5, topmargin = -8, keysize=.3)
dev.off()


npbplotname<-"npb_plot2.png"
## To save it in png -file rn from here
width<-3.5
png(npbplotname, width = width, height=width*.65 , res=2000, units="in")
plotfun(data=npb_plotdata, ordervar ="dq", centrevar="countrycode", 
        xaxis=F, round=0, size=0.5, topmargin = -8, keysize=.3)
dev.off()

### Tables

if(100=0){
  #install.packages("gdtools")
  library(ReporteRs)
  doc = docx( title = 'Result tables' )
  doc = addTitle( doc , 'NPB dataset', level = 1)
  doc = addFlexTable( doc , vanilla.table(npb_plotdata) )
  doc = addTitle( doc , 'NPC dataset', level = 1)
  doc = addFlexTable( doc , vanilla.table(npc_plotdata) )
  writeDoc(doc, "resultables.docx")
}