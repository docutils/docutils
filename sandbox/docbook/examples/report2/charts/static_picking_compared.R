rates <- read.csv("../tables/all.csv")
x = rates$week
png(file="static_vs_other.png", width=width, height=7, units="in", res=res)
# first graph
make.title <- T
y = rates$rate.outbound.2011
y_range <- range(y, na.rm=T)
title = "Outbound Rate over Time"
par(mfrow=c(3,1)) 
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
lines(x,y,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
source("make_x_axis.R")



rm(y)

title = "Static Rate,  2011"
x = rates$week
y = rates$rate.static.picking.2011
y_range <- range(y, na.rm=T)
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
lines(x,y,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
source("make_x_axis.R")

# second chart

# third chart
# other

total.units = rates$units.singles.2011 + rates$units.multis.2011 +
 rates$units.car.pick.2011 + rates$units.shipping.2011
total.hours = rates$hours.singles.2011 + rates$hours.multis.2011 +
 rates$hours.car.pick.2011 + rates$hours.shipping.2011
other.rate <- total.units/total.hours

title = "Other Rates,  2011"
x = rates$week
y = other.rate
y_range <- range(y, na.rm=T)
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
lines(x,y,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
source("make_x_axis.R")


dev.off()
