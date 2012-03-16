rates <- read.csv("../tables/all.csv")
title = "Indirect to Total Labor, as Percent \n 2010, 2011"
y1 = rates$indirect.hours.outbound.2011/(rates$direct.hours.outbound.2011 + rates$indirect.hours.outbound.2011)
y2 = rates$indirect.hours.outbound.2010/(rates$direct.hours.outbound.2010 + rates$indirect.hours.outbound.2010)
x = rates$week
y1 <- y1 * 100
y2 <- y2 * 100
png(file="indirect_out_vs_total_2010_2011.png",
width=5, height=3.5, units="in", res=res)
xlab.text="Week in Year"
xlab = xlab.text
ylab = "percentage"
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
rect(38, -10, 39, 300, col=rec_col, border=NA, xpd = F)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
abline( h=100,  lwd=1, col=base_line_col, lty=base_line_type)

lines(x,y1,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
lines(x,y2,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
source("make_x_axis.R")

legend("topleft", c("2010","2011"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
dev.off()

result <- t.test(var.equal=T, y2,y1, alternative="g")
y1 = rates$direct.hours.outbound.2011/1000
y2 = rates$direct.hours.outbound.2010/1000
title = "Direct Hours, 2010, 2011"
#png(file="direct_out_hours_2010_2011.png",
#width=5, height=3.5, units="in", res=res)
#xlab.text="Week in Year"
#xlab = xlab.text
#ylab = "hours in thousands"
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#if (months.grid == T) bottom.margin = 6
#if (months.grid == T) xlab = ''
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par(mar=margins) 
#par('family'='serif')
#y_range = range(y1,y2, na.rm=T)
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, 
#        ylim=c(y_range), axes=F, xlim=c(range(x)))
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#rect(38, -10, 39, 300, col=rec_col, border=NA, xpd = F)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#abline( h=100,  lwd=1, col=base_line_col, lty=base_line_type)
#
#lines(x,y1,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
#lines(x,y2,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
#source("make_x_axis.R")
#
#legend("topleft", c("2010","2011"), cex=legend_font_size, 
# col=line_colors,  lty=line_types,  bty='y', bg="white",
# box.lwd=box.lwd)
#dev.off()

title ="Direct Rate 2011, 2010"
make.title = T
y1 = rates$units.outbound.2011/rates$direct.hours.outbound.2011 
y2 = rates$units.outbound.2010/rates$direct.hours.outbound.2010 
png(file="direct_vs_total_2010_2011.png",
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
abline( h=100,  lwd=1, col=base_line_col, lty=base_line_type)

lines(x,y1,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
lines(x,y2,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
source("make_x_axis.R")

legend("bottomleft", c("2010","2011"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white",
 box.lwd=box.lwd)

# t.test(y2, y1, var.equal=T, alter="g", mu=13, conf.int = 97.5)

# second graph
title ="Outbound Rate, Total"
y1 = rates$rate.outbound.2010
y2 = rates$rate.outbound.2011
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
# t.test(y1,y2, var.equal=T, alter="g", mu=6, conf.int = 97.5)

dev.off()
make.title = F

#png(file="indirect_outbound_2010_2011.png",
#width=5, height=7, units="in", res=res)
#title ="percent change indirect outbound, 2010, 2011"
#
#y1 <- rates$indirect.hours.inbound.2010/rates$hours.inbound.2010
#y2 <- rates$indirect.hours.inbound.2011/rates$hours.inbound.2011
#rates$indirect.hours.2010
#y1 = y1/y1[1]
#y2 = y2/y2[1]
#y1 = y1 * 100
#y2 = y2 * 100
#y_range <- range(y1,y2, na.rm=T)
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
#legend("topleft", c("2010","2011"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
#source("make_x_axis.R")
#
## 8592292121 Billy Redmond's number
## t.test(y1,y2, var.equal=T, alter="g", mu=6, conf.int = 97.5)
#
#dev.off()

