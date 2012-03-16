rates <- read.csv("../tables/all.csv")
x = rates$week
title=''
start = 1
y1 = rates$indirect.hours.returns.2011/rates$direct.hours.returns.2011
y1 = y1[start:52]
x = x[start:52]
y2 = rates$rate.returns.2011
y2 = y2[start:52]
y1 = y1/y1[1]
y2 = y2/y2[1]
y_range = range(y1, y2,  na.rm=T)
#png(file="return_direct_indirect_percent.png", width=5, height=3.5, units="in", res=res)
#par(mar=margins) 
#par('family'='serif')
#plot(x, type='n', xlab="week in year", ylab="units/man hour", las=1, main=title, ylim=y_range, axes=F, xlim=range(x))
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#rect(37, -10, 39, 1000, col=rec_col, border=NA, xpd=F)
#lines(x,y1,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
#lines(x,y2,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
#legend("bottomleft", c("2010","2011"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='n')
#dev.off()

title = "Indirect to Direct Labor and Rate\nReturns"
bottom.margin = 4
top.margin=.5
if (title != '') top.margin = 3
if (months.grid == T) bottom.margin = 6
ylab = "percentage from baseline" 
xlab = "Week in Year"
xlab.text <- xlab
if (months.grid == T) xlab = ''


y1 = rates$indirect.hours.returns.2011/rates$direct.hours.returns.2011
y1 = y1[start:52]
y2 = rates$rate.returns.2011
y2 = y2[start:52]
y2 = y2/y2[1]
y1 = y1/y1[1]
y1 = y1 * 100
y2 = y2 * 100
y_range = range(y1,y2,  na.rm=T)
#png(file="return_direct_indirect.png", width=5, height=3.5, units="in", res=res)
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=y_range, axes=F, xlim=range(x))
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#abline( h=100, lty=2, lwd=1, col=base_line_col)
#text(x=35, y = 120, "baseline", cex = months.font)
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
#lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
#legend("topleft", c("inidirect %","rate"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='n')
#source("make_x_axis.R")
#dev.off()

r_start = 26
r_end = 27
title = "Returns Rate"
ylab = "units/man hour"
#png(file="return_rate_indirect2.png", width=5,
#height=6, units="in", res=res)
#par(mfrow=c(2,1)) 
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#y2 = rates$rate.returns.2011
#y2 = y2[start:52]
#x_range = range(x)
#y_range = range(y2)
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title,
#ylim=y_range, axes=F, xlim=x_range)
#rect(r_start, -6, r_end, 40, col=rec_col, border=NA, xpd=F)
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#lines(x,y2, col=line_colors[2], lwd=lwd, lty=line_types[2])
#source("make_x_axis.R")
#
#title = "Percent of Indirect to Total Labor"
#ylab = "percentage"
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#y1 =
#rates$indirect.hours.returns.2011/(rates$direct.hours.returns.2011 +
#rates$indirect.hours.returns.2011)
#y1 = y1[start:52]
#y1 = y1 * 100
#y_range = range(y1)
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=y_range, axes=F, xlim=range(x))
#rect(r_start, -6, r_end, 140, col=rec_col, border=NA, xpd=F)
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#text(x=35, y = 120, "baseline", cex = months.font)
#lines(x,y1,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
#source("make_x_axis.R")
#
#dev.off()
#=============================================


title = "Percent of Indirect to Total Labor"
ylab = "percentage"
png(file="return_percent_indirect.png", width=5,
height=3.5, units="in", res=res)
top.margin = .5
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
y1 = rates$indirect.hours.returns.2011/(rates$direct.hours.returns.2011 +
rates$indirect.hours.returns.2011)
y1 = y1[start:52]
y1 = y1 * 100
y_range = range(y1)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=y_range, axes=F, xlim=range(x))
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
arrows(length=.1, x0=22, x1=26, y0=30, y1=30)
text(x=22, y=30, pos=2, labels="disruption", offset=0)
lines(x,y1,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
source("make_x_axis.R")


dev.off()

title = "Direct Rate Returns"
ylab = "units/man hour"
#png(file="return_direct.png", width=5,
#height=3.5, units="in", res=res)
#top.margin = .5
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#y = rates$units.returns.2011/rates$direct.hours.returns.2011
## y = y[14:52]
#y = y[start:52]
#y_range = range(y)
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=y_range, axes=F, xlim=range(x))
#rect(38, -10, 39, 1000, col=rec_col, border=NA, xpd=F)
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#lines(x,y,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
#source("make_x_axis.R")
#
#
#dev.off()
