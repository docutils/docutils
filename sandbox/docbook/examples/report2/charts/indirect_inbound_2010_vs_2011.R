rates <- read.csv("../tables/all.csv")
x = rates$week
title ="Direct Rate Inbound, 2011, 2010"
y1 = rates$units.inbound.2011/rates$direct.hours.inbound.2011 
y2 = rates$units.inbound.2010/rates$direct.hours.inbound.2010 
png(file="direct_vs_inbound_total_2010_2011.png",
width=5, height=7, units="in", res=res)
par(mfrow = c(2,1))
xlab.text="Week in Year"
xlab = xlab.text
ylab = "units/man hour"
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par(mar=margins) 
par('family'='serif')
y_range = range(y1,y2, na.rm=T)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, 
        ylim=c(y_range), axes=F, xlim=c(range(x)))
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)

lines(x,y1,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
lines(x,y2,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
source("make_x_axis.R")
legend("topright", c("2010","2011"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white",
 box.lwd=box.lwd)

#t.test(y1, y2, var.equal=T, alter="g", mu=23.4, conf.int = 97.5)

# second graph
title ="Thoughput Rate, Inbound, 2010, 2011"
y1 = rates$rate.inbound.2010
y2 = rates$rate.inbound.2011
y_range <- range(y1, y2, na.rm=T)
xlab.text="week in year"
xlab = xlab.text
ylab = "units/man hour"
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F)
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2, type="l", col=line_colors[2], lwd=lwd, lty=line_types[2])
legend("bottomleft", c("2010","2011"), cex=legend_font_size, 
 col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
source("make_x_axis.R")

# 8592292121 Billy Redmond's number
# t.test(y1,y2, var.equal=T, alter="g", mu=5.9, conf.level = .975)
# 5.9
# t.test(y1,y2, var.equal=T, conf.level=.9999 )

dev.off()

#png(file="charts/indirect_percent_inbound_2011.png",
#width=5, height=7, units="in", res=res)
#make.title <- F
#title ="percent change indirect inbound, 2011"
#
#y <- rates$indirect.hours.inbound.2011
#y1 <- rates$indirect.hours.car.putaway.2011/rates$hours.car.putaway.2011
#y2 <- rates$indirect.hours.static.putaway.2011/rates$hours.static.putaway.2011
#y3 <- rates$indirect.hours.receive.2011/rates$hours.receive.2011
#y1 <- y1/y1[1]
#y2 <- y2/y2[1]
#y3 <- y3/y3[1]
#y1 <- y1 * 100
#y2 <- y2 * 100
#y3 <- y3 * 100
#y_range <- range(y1,y2,y3, na.rm=T)
#xlab.text="week in year"
#xlab = xlab.text
#ylab = "percent change"
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#if (months.grid == T) bottom.margin = 6
#if (months.grid == T) xlab = ''
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F)
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#abline( h=100, lty=2, lwd=2, col="red")
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
#lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
#lines(x,y3,  col=line_colors[3], type="l", lwd=lwd, lty=line_types[3])
#legend("topleft", c("carousel","static", "receive"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
#source("make_x_axis.R")
#
## 8592292121 Billy Redmond's number
## t.test(y1,y2, var.equal=T, alter="g", mu=6, conf.int = 97.5)
#
#dev.off()

#png(file="charts/indirect_percent_inbound_2010.png",
#width=5, height=7, units="in", res=res)
#title ="percent change indirect inbound, 2010"
#
#y <- rates$indirect.hours.inbound.2010
#y1 <- rates$indirect.hours.car.putaway.2010/rates$hours.car.putaway.2010
#y2 <- rates$indirect.hours.static.putaway.2010/rates$hours.static.putaway.2010
#y3 <- rates$indirect.hours.receive.2010/rates$hours.receive.2010
#y1 <- y1/mean(y1)
#y2 <- y2/mean(y2)
#y3 <- y3/mean(y3)
#y1 <- y1 * 100
#y2 <- y2 * 100
#y3 <- y3 * 100
#y_range <- range(y1,y2,y3, na.rm=T)
#xlab.text="week in year"
#xlab = xlab.text
#ylab = "percent change"
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#if (months.grid == T) bottom.margin = 6
#if (months.grid == T) xlab = ''
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F)
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#abline( h=100, lty=2, lwd=2, col="red")
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
#lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
#lines(x,y3,  col=line_colors[3], type="l", lwd=lwd, lty=line_types[3])
#legend("topleft", c("carousel","static", "receive"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
#source("make_x_axis.R")
#
## 8592292121 Billy Redmond's number
## t.test(y1,y2, var.equal=T, alter="g", mu=6, conf.int = 97.5)
#
#dev.off()

png(file="indirect_inbound_2010_2011.png",
width=5, height=7, units="in", res=res)
title ="percent change indirect inbound, 2010, 2011"

y1 <- rates$indirect.hours.inbound.2010/rates$hours.inbound.2010
y2 <- rates$indirect.hours.inbound.2011/rates$hours.inbound.2011
rates$indirect.hours.2010
y1 = y1/y1[1]
y2 = y2/y2[1]
y1 = y1 * 100
y2 = y2 * 100
y_range <- range(y1,y2, na.rm=T)
xlab.text="week in year"
xlab = xlab.text
ylab = "percent change"
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F)
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
abline( h=100, lty=2, lwd=2, col="red")
lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
legend("topleft", c("2010","2011"), cex=legend_font_size, 
 col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
source("make_x_axis.R")

# 8592292121 Billy Redmond's number
# t.test(y1,y2, var.equal=T, alter="g", mu=6, conf.int = 97.5)

dev.off()

