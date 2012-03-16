foot <- read.csv("../tables/foot.csv")
rates <- read.csv("../tables/all.csv")
title = "Static Rate, 2010 vs 2011, with adjustment line"
x = rates$week
y1 = rates$rate.static.picking.2010
y2 = rates$rate.static.picking.2011
temp <- y2[9:52] * 1.3
y3 <- c(y2[1:8], temp)
y4 <-  foot$units.non.foot.pick.2011 /(foot$hours.non.foot.pick.2011 * 57/90) # from stats
y5 <-  foot$units.non.foot.pick.2011 /(foot$hours.non.foot.pick.2011 * 118/202) # Anne Murphy

y_range <- range(y1, y2, y3, y4, y5, na.rm=T)
#png(file="static_picking_2010_2011_adj.png", width=width,
#height=3.5, units="in", res=res)
#xlab.text="week in year"
#xlab = xlab.text
#ylab = "units/man hour"
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
##lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
#lines(x,y2, type="l", col=line_colors[1], lwd=lwd, lty=line_types[1])
#lines(x,y3, type="l", col=line_colors[2], lwd=lwd, lty=line_types[2])
#lines(x,y4, type="l", col=line_colors[3], lwd=lwd, lty=line_types[3])
#lines(x,y5, type="l", col=line_colors[4], lwd=lwd, lty=line_types[4])
#legend("topright", c("normal", "adj by graph","adjusted x 1.6", "adjusted x 1.7"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
#source("make_x_axis.R")
#
#dev.off()

png(file="static_picking_2010_2011_adj2.png", width=width,
height=3.5, units="in", res=res)
y2 <- y2[9:40]
y3 <- y3[9:40]
y4 <- y4[9:40]
y5 <- y5[9:40]
x <- x[9:40]
y_range <- range(y1, y2, y3, y4, y5, na.rm=T)
x_range <- c(9:40)
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
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F, xlim=c(9,40))
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2, type="l", col=line_colors[1], lwd=lwd, lty=line_types[1])
lines(x,y3, type="l", col=line_colors[2], lwd=lwd, lty=line_types[2])
lines(x,y4, type="l", col=line_colors[3], lwd=lwd, lty=line_types[3])
lines(x,y5, type="l", col=line_colors[4], lwd=lwd, lty=line_types[4])
legend("topright", c("normal", "adj by graph","adjusted x 1.6", "adjusted x 1.7"), cex=legend_font_size, 
 col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
source("make_x_axis.R")

dev.off()

min.v <- c()
max.v <- c()
for (n in c(1:length(x))){
	max <- max(y3[n], y4[n], y5[n])
	min <- min(y3[n], y4[n], y5[n])
	min.v[length(min.v) + 1] <- min
	max.v[length(max.v) + 1] <- max
}

min.v <- min.v[1:29]
max.v <- max.v[1:29]
y3 = rates$rate.static.picking.2011[1:29]
for (n in c(1:length(min.v))){
	if ( y3[n] > min.v[n]) min.v[n] <- y3[n]
	}

#png(file="static_picking_2010_2011_adj3.png", width=width,
#height=3.5, units="in", res=res)
#title <- "Static Picking with Uncertainties"
y1 <- min.v
y2 <- max.v
#x <- x[1:29]
#y1[6] <- 75 # get rid of outlier
#y_range <- range(y1, y2,y3, na.rm=T)
#xlab.text="week in year"
#xlab = xlab.text
#ylab = "units/man hour"
#if (make.title == T ) top.margin = 3
#if (make.title != T ) title="" 
#if (months.grid == T) bottom.margin = 6
#if (months.grid == T) xlab = ''
#margins=c(bottom.margin, 4, top.margin, 0.5) 
#par(mar=margins) 
#par('family'='serif')
#plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F, xlim=c(9,40))
#axis(1, col=ax_col)
#axis(2, col=ax_col, las=1, at=axTicks(2))
#box(col=box_col)
#abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
##lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
#lines(x,y1, type="l", col=line_colors[1], lwd=lwd, lty=line_types[1])
#lines(x,y2, type="l", col=line_colors[2], lwd=lwd, lty=line_types[2])
#lines(x,y3, type="l", col=line_colors[3], lwd=lwd, lty=line_types[4])
#x.poly = c(x, rev(x))
#y.poly = c(y2, rev(y1))
#polygon(x.poly, y.poly, col="lightgray", border=NA)
#legend("topright", c("low", "high","normal"), cex=legend_font_size, 
# col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
#source("make_x_axis.R")
#
#dev.off()

avg <- y1 + y2
avg <- avg/2
png(file="static_picking_2010_2011_adj4.png", width=width,
height=3.5, units="in", res=res)
title <- "Static Picking with Adjustment"
x = rates$week
y1 = rates$rate.static.picking.2010
y2 = rates$rate.static.picking.2011
#temp <- y2[9:52] * 1.19
#temp <- y2[9:52] + 19
temp <- y2[7:52] * 1.3
y3 <- c(y2[1:6],temp)
y4 <- c(54,55.5,55.5, rep(NA, length(x) - 3)) # 2012
y4 <- y4 * 1.3 # adjust 2012
y_range <- range(y1, y2,y3,y4, na.rm=T)
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
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F )
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y3, type="l", col=line_colors[3], lwd=lwd, lty=line_types[4])
lines(x,y1, type="l", col=line_colors[1], lwd=lwd, lty=line_types[1])
lines(x,y2, type="l", col=line_colors[2], lwd=lwd, lty=line_types[2])
#lines(x,y4, type="l", col=line_colors[4], lwd=lwd, lty=line_types[4])
legend("bottomleft", c("2010", "2011","2011 adjusted"), cex=legend_font_size, 
 col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
source("make_x_axis.R")

dev.off()

# less generous adjustment
png(file="static_picking_2010_2011_adj5.png", width=width,
height=3.5, units="in", res=res)
title <- "Static Picking with Adjustment"
x = rates$week
y1 = rates$rate.static.picking.2010
y2 = rates$rate.static.picking.2011
temp <- y2[7:52] * 1.19
y3 <- c(y2[1:6],temp)
y4 <- c(54,55.5,55.5, rep(NA, length(x) - 3)) # 2012
y4 <- y4 * 1.19 # adjust 2012
y_range <- range(y1, y2,y3,y4, na.rm=T)
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
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F )
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
#lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y3, type="l", col=line_colors[3], lwd=lwd, lty=line_types[4])
lines(x,y1, type="l", col=line_colors[1], lwd=lwd, lty=line_types[1])
lines(x,y2, type="l", col=line_colors[2], lwd=lwd, lty=line_types[2])
#lines(x,y4, type="l", col=line_colors[4], lwd=lwd, lty=line_types[4])
legend("bottomleft", c("2010", "2011","2011 adjusted"), cex=legend_font_size, 
 col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
source("make_x_axis.R")

dev.off()
