## user log analysis
setwd("")

## READ FILES
dpath1<-("/Desktop/databases/")
alog<-read.csv2(paste0(dpath1, "audit_log.csv")) ## from database
subj<-read.csv2(paste0(dpath1, "subject_session.csv")) ## from mysql database

dpath2<-("databases/log/")
alog2<-read.csv2(paste0(dpath2,"all_merged_cleaned.csv"))

nrow(alog2)
with(alog2, table(status))
with(alog2, table(dt))

head(alog2)
alog$created_dt<-substr(alog$created_at, 2,11)


parsetime<-function(data=alog, timevar="created_at", dateformat="%Y-%m-%d",
                    timeformat="%Y-%m-%d %H:%M:%S"){
  dt<-substr(data[,timevar], start=2, stop = 11)
  time<-substr(data[,timevar], start=13, stop = 20)
  dt<-as.Date(dt, format=dateformat)
  
  g<-paste(dt, time, sep=" ")
  strptime<-strptime(g, format = timeformat)
  data.frame(dt, strptime)
}

foo<-parsetime()
names(foo)<-c("created_dt", "created_time")
alog<-cbind(alog,foo)

head(alog)
foo<-parsetime(timevar="updated_at")
names(foo)<-c("updated_dt", "updated_time")
alog<-cbind(alog,foo)


head(subj)
foo<-parsetime(data=subj, timevar="expired_at")
names(foo)<-c("expired_dt", "expired_time")
subj<-cbind(subj,foo)

foo<-parsetime(data=subj, timevar="created_at")
names(foo)<-c("created_dt", "created_time")
subj<-cbind(subj,foo)

foo<-parsetime(data=subj, timevar="updated_at")
names(foo)<-c("updated_dt", "updated_time")
subj<-cbind(subj,foo)

## Check: expired time vs updated time?

with(subj, table(expired_time==updated_time))
subset(subj, expired_time!=updated_time)

with(subj, table(is.na(expired_time))) 
with(subj, table(is.na(updated_time))) 

with(subj, table(is.na(created_time)))


boxplot(subj$timediff<-as.numeric(with(subj, difftime(time1=updated_time, time2=created_time, units="mins"))))
summary(subj$timediff)

ff<-as.numeric(with(subj, difftime(time1=updated_time, time2=created_time, units="days")))
subset(subj, timediff>96000)[,c("updated_time", "created_time", "timediff")]


mintim<-with(subj, min(c(updated_time, created_time)))
maxtim<-with(subj, max(c(updated_time, created_time)))

length(mintim:maxtim)
log(subj$timediff)

ncolors<-length(tt<-with(subj,table(subject_id)))
library(RColorBrewer)

palette<-c(  brewer.pal("Set1", n=7),
             brewer.pal("Set2", n=8),
             brewer.pal("Set3", n=12),
             brewer.pal("BrBG", n=11))

subj$subject_id<-factor(subj$subject_id)
sorted<-do.call("rbind",split(subj, subj$subject_id))
sorted$col<-mypal[match( sorted$subject_id,names(mypal))]

sorted$index<-1:nrow(sorted)
names(sorted)
table(sorted$keep<-with(sorted, ifelse(timediff>720, FALSE, TRUE) )) 
## keep only those with time<=720 mins, aka 12h
final<-subset(sorted, keep)
ncolors<-length(tt<-with(final,table(subject_id)))
mypal<-palette[(1:ncolors)]
names(mypal)<-names(tt)


below20<-names(which(tt<20))
over20<-names(which(tt>=20))
nrow(sub1<-subset(final, subject_id%in%below20))
sub1$index<-1:nrow(sub1)

nrow(sub2<-subset(final, subject_id%in%over20))
sub2$index<-1:nrow(sub2)


par(mar=c(7,5,3,3))
plot(0,0,xlim=c(mintim,maxtim), ylim=c(1,nrow(sub1)), type="n",
     ylab="",
     xlab="", xaxs="i",
     yaxs="i", bty="n", yaxt="n", xaxt="n")
segments(x0=sub1$created_time, x1=sub1$updated_time, y0=sub1$index, col=sub1$col, lwd=3)
xas<-seq(mintim,maxtim, length.out=30)
axis(1, at=seq(mintim,maxtim, length.out=30), label=format(xas, "%Y-%m-%d"), las=2)
mtext("Session Time",1, line=5.6)
at<-match(below20, sub1$subject_id)
axis(2, at=at, las=2, labels=below20)
mtext("Subject id",2, line=4)


par(mar=c(7,5,3,3))
plot(0,0,xlim=c(mintim,maxtim), ylim=c(1,nrow(sub2)), type="n",
     ylab="",
     xlab="", xaxs="i",
     yaxs="i", bty="n", yaxt="n", xaxt="n")
segments(x0=sub2$created_time, x1=sub2$updated_time, y0=sub2$index, col=sub2$col, lwd=2)
xas<-seq(mintim,maxtim, length.out=30)
axis(1, at=seq(mintim,maxtim, length.out=30), label=format(xas, "%Y-%m-%d"), las=2)
mtext("Session Time",1, line=5.6)
at<-match(over20, sub2$subject_id)
axis(2, at=at, las=2, labels=over20)
mtext("Subject id",2, line=4)


grep("log", list.files(dpath1), value=T)

par(mar=c(7,5,3,3))
plot(0,0,xlim=c(mintim,maxtim), ylim=c(0,max((final$timediff))), type="n",
     ylab="",
     xlab="", xaxs="i",
     yaxs="i", bty="n", yaxt="n", xaxt="n")
segments(x0=final$created_time, y0=0, y1=(final$timediff), col=final$col, lwd=2, lend=2)
axis(2, at=seq(0,max(final$timediff), by=60), las=2)
xas<-seq(mintim,maxtim, length.out=30)
axis(1, at=seq(mintim,maxtim, length.out=30), label=format(xas, "%Y-%m-%d"), las=2)
mtext("Session duration (minutes)",2, line=4)
mtext("Time",1, line=5.6)


## add completeness
head(comp<-read.csv2(paste0(dpath1,"completeness_log.csv")))
foo<-parsetime(data=comp, timevar="updated_at")
names(foo)<-c("updated_dt", "updated_time")
comp<-cbind(comp,foo)
foo<-parsetime(data=comp, timevar="created_at")
names(foo)<-c("created_dt", "created_time")
comp<-cbind(comp,foo)

head(comp)

with(comp, summary(created_time))
with(comp, table(target_table))

head(subj)
12*60

?boxplot
class(final)
dev.off()
boxplot(timediff~subject_id, data=final, xlab="Subject id")
summary(final$timediff)

boxplot(timediff~subject_id, data=subset(subj, subject_id!="10"), xlab="Subject id")


final<-subset(subj,subject_id!="10")
final$one<-1
final$keep<-ifelse(final$timediff>720, FALSE, TRUE)
table(final$keep)
final$month<-with(final, format(created_dt, "%G-%m"))
minmax<-with(final, range(created_dt))
dts<-as.Date(as.Date(minmax[1]):as.Date(minmax[2]), origin="1970-01-01")

mts<-unique(format(dts, "%G-%m"))
final$mt<-factor(as.character(final$month), levels=as.character(mts))

##head(comp)
##comp$week<-with(comp, format(created_dt, "%G-%W"))
##comp$wk<-factor(as.character(comp$week), levels=as.character(wks))
##comp$complete<-as.numeric(as.character(comp$completeness))
#comp$month<-with(comp, format(created_dt, "%G-%m"))
#comp$mt<-factor(as.character(comp$month), levels=as.character(mts))
#comp$complete<-as.numeric(as.character(comp$completeness))

## 
head(final)
table(final$stage<-with(final, ifelse(created_dt<as.Date("2016-02-01"), 1, 
                                      ifelse(created_dt<as.Date("2016-05-01"), 2, 3))))

with(subset(final,keep),tapply(timediff, stage, median))

kruskal.test(subset(final,keep & stage<3)$timediff,subset(final,keep & stage<3)$stage )
kruskal.test(subset(final,keep & stage!=2)$timediff,subset(final,keep & stage!=2)$stage )

## only those with time <=720 mins
bars<-(with(subset(final,keep), tapply(one, mt, sum, na.rm=T)))
line1<-with(subset(final,keep), tapply(timediff, mt, mean, na.rm=T))

with(subset(final,keep),sd(timediff))
na-0<-function(a) ifelse(is.na(a), 0, a)
sd(na.0(bars))
summary(bars) 
## scale the session times and completeness to same scale
summary(ll1<-(75*line1/max(line1, na.rm=T))) 
barplot(bars, ylim=c(0,100), xlab="Month", ylab="N sessions")
lines(ll1, col="red", lwd=2, lty=2)

####### all
summary(line2<-with(final, tapply(timediff, mt, median, na.rm=T))) ## uh oh
summary(ll2<-(75*line2/max(line2, na.rm=T))) 
lines(ll2, col="blue", lwd=2) ## das doch blÃ¶d, eine dominiert da ja...


####### <720 min  
summary(line3<-with(subset(final, keep), 
                    tapply(timediff, mt, median, na.rm=T))) ## uh oh
summary(ll3<-(75*line3/max(line3, na.rm=T))) 
lines(ll3, col="purple", lwd=2) 

col1="#CCDAC7"; col2="#84ACCE";col3<-"#E77728"
linecol<-"#114B5F"
bcols<-rep(col2, length(bars))
bcols[which(names(bars)=="2016-02"):length(bcols)]<-col3
bcols[which(names(bars)=="2016-05"):length(bcols)]<-col1


########## HERE THE PLot
filenam<-"sessiontimes.png"
png(filenam, width=600, height=500)
ymax<-108
par(las=2, mar=c(6,4.5,2,4.5))
b<-barplot(bars, ylim=c(0,ymax), 
           xlab="", ylab="N sessions",border=NA,
           col = bcols, xaxt="n", cex.axis = 1.5, cex.lab = 1.5)
lines(x=c(b),y=ll3, col=linecol, lwd=3, lty=1)
legend("topleft", fill=c(col2,col3,col1), 
       legend=c("no feedback","intrinsic", "extrinsic"), 
       border=NA, bty="n",title="User Motivation", cex = 1.5)
labs<-seq(0, max(line3, na.rm=T), by=10)
at<-seq(0,75, length.out=length(labs) )
at2<-c(at,at[length(at)]+(1:3)*diff(at)[1])
labs2<-c(labs,labs[length(labs)]+(1:3)*diff(labs)[1])
axis(side=4, at=at2, labels = labs2, cex.axis=1.5)
mtext(side=4, line=3, text = "Median session time (minutes)", las=0, cex=1.3)
sekv<-seq(1, length(b), by=3)
labsit<-names(bars)[sekv]
 
text( x=c(b)[sekv]-2, y=-8.5, labsit, xpd=TRUE, srt=45, cex=1.5)
mtext(side=1, line=4.5, text="Year-Month", cex=1.3, las=0)




