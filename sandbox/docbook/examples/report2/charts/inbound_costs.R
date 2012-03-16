rates <- read.csv("../tables/all.csv")
title <- "Units vs. Hours Inbound 2011"
x <- rates$week
y1 <-rates$units.inbound.2011 - rates$units.returns.2011
y2 <-rates$hours.inbound.2011 - rates$hours.returns.2011
y1 <- y1/y1[1]
y2 <- y2/y2[1]
y1 <- y1 * 100
y2 <- y2 * 100
y3 <- rates$units.inbound.2011/rates$hours.inbound.2011
y3 <- y3/y3[1]
y3 <- y3 * 100
png(file="units_hours_inbound_2011.png",
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

lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
# lines(x,y3,  col=line_colors[3], type="l", lwd=lwd, lty=line_types[3])
source("make_x_axis.R")

legend("topleft", c("units","hours"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
dev.off()

title = "Units vs. Hours Inbound 2010"
x <- rates$week
y1 <-rates$units.inbound.2010
y2 <-rates$hours.inbound.2010
y1 <- y1/y1[1]
y2 <- y2/y2[1]
y1 <- y1 * 100
y2 <- y2 * 100
y3 <- rates$units.inbound.2010/rates$hours.inbound.2010
y3 <- y3/y3[1]
y3 <- y3 * 100
png(file="units_hours_inbound_2010.png",
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

lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
# lines(x,y3,  col=line_colors[3], type="l", lwd=lwd, lty=line_types[3])
source("make_x_axis.R")

legend("topleft", c("units","hours"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
dev.off()

# figure out costs
y1 <-rates$units.inbound.2011 - rates$units.returns.2011
y2 <-rates$hours.inbound.2011 - rates$hours.returns.2011
y1 <- y1/y1[1]
y2 <- y2/y2[1]
y1 <- y1 * 100
y2 <- y2 * 100

y3 <-rates$units.inbound.2010 
y4 <-rates$hours.inbound.2010 
y3 <- y3/y3[1]
y4 <- y4/y4[1]
y3 <- y3 * 100
y4 <- y4 * 100
diff <- y4 - y3
y1 <- y1 + diff

title = "costs"
x <- x[24:41]
y1 <- y1[24:41]
y2 <- y2[24:41]
png(file="inbound_cost.png",
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



hours.adj <-rates$hours.inbound.2011 - rates$hours.returns.2011


hypo <- y1/100 * hours.adj[1] # should have been, without cutover
real <- y2/100 * hours.adj[1]
extra.hours <- real - hypo
total.cost.inbound <- format(sum(extra.hours) * 16.1, big.mark=",", scientific=F) #total cost
