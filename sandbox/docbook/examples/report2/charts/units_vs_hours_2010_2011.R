rates <- read.csv("../tables/all.csv")
title = "Units vs. Direct Hours"
x <- rates$week
y1 <-rates$units.outbound.2011
y2 <-rates$hours.outbound.2011
y1 <- y1/y1[1]
y2 <- y2/y2[1]
y1 <- y1 * 100
y2 <- y2 * 100
y3 <- rates$units.outbound.2011/rates$hours.outbound.2011
y3 <- y3/y3[1]
y3 <- y3 * 100
png(file="units_hours_2011.png",
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
y_range = range(y1,y2,y3, na.rm=T)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, 
        ylim=c(y_range), axes=F, xlim=c(range(x)))
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
abline(h=100, lty=2, col='red')
text(x=50, y = 125, 'baseline')

lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
# lines(x,y3,  col=line_colors[3], type="l", lwd=lwd, lty=line_types[3])
source("make_x_axis.R")

legend("topleft", c("units","hours"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
dev.off()

y1 <-rates$units.outbound.2010
y2 <-rates$hours.outbound.2010
y1 <- y1/y1[1]
y2 <- y2/y2[1]
y1 <- y1 * 100
y2 <- y2 * 100
png(file="units_hours_2010.png",
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
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
abline(h=100, lty=2, col='red')
text(x=50, y = 125, 'baseline')

lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
source("make_x_axis.R")

legend("topleft", c("units","hours"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
dev.off()

title = "Units vs. Direct Hours"
x <- rates$week[24:42]
base1 <- rates$units.outbound.2011[1]
base2 <- rates$hours.outbound.2011[1]
y1 <-rates$units.outbound.2011[24:42]
y2 <-rates$hours.outbound.2011[24:42]
y1 <- y1/base1
y2 <- y2/base2
y1 <- y1 * 100
y2 <- y2 * 100
#png(file="units_hours_pre_2011.png",
#width=5, height=3.5, units="in", res=res)
#xlab.text="Week in Year"
#xlab = xlab.text
#ylab = "percentage"
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
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
#lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
#
#x.poly = c(x, rev(x))
#y.poly = c(y1, rev(y2))
#polygon(x.poly, y.poly, col="lightgray", border=NA)
#
#
#source("make_x_axis.R")
#
#legend("topleft", c("units","hours"), cex=legend_font_size, 
# col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
#dev.off()
rm(base1,base2,y1,y2)

# figure out extra cost

base1 <- rates$hours.outbound.2010[1]
base2 <- rates$units.outbound.2010[1]
y1 <-rates$hours.outbound.2010[38:41]
y2 <-rates$units.outbound.2010[38:41]
y1 <- y1/base1
y2 <- y2/base2
percent.diff <- y1-y2
percent.diff <- percent.diff * 100

title = "Units vs. Direct Hours"
x <- rates$week[24:41]
base1 <- rates$units.outbound.2011[1]
base2 <- rates$hours.outbound.2011[1]
y1 <-rates$units.outbound.2011[24:41]
y2 <-rates$hours.outbound.2011[24:41]
y1 <- y1/base1
y2 <- y2/base2
y1 <- y1 * 100
y2 <- y2 * 100
y2[15:18]
y2[15:18] <- y2[15:18]  - percent.diff
png(file="outbound_costs.png",
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
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)

lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])

x.poly = c(x, rev(x))
y.poly = c(y1, rev(y2))
polygon(x.poly, y.poly, col="lightgray", border=NA)


source("make_x_axis.R")

legend("topleft", c("units","hours"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
dev.off()
hypo <- y1/100 * rates$hours.outbound.2011[1] # should have been, without cutover
real <- y2/100 * rates$hours.outbound.2011[1]
extra.hours <- real - hypo
total.cost.outbound <- format(sum(extra.hours) * 16.1, big.mark=",", scientific=F) #total cost
# cat("extra hours are", sum(extra.hours), "total cost is", total.cost.outbound, "\n")
d <- as.data.frame(cbind(real,hypo, extra.hours))
# write.csv(d, file="temp2.csv")
