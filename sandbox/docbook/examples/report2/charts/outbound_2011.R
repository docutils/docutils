rates <- read.csv("../tables/all.csv")
title = "Oubound, 2011"
y = rates$rate.outbound.2011
x = rates$week
xlab.text="week in year"
xlab = xlab.text
ylab = "units/man hour"
png(file="outbound_rate_2011.png", width=width, height=3.5, units="in", res=res )
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
y_range <- range(y, na.rm=T)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1,
main=title, ylim=c(y_range), axes=F, )
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2) )
box(col=box_col)
rect(37, -10, 39, 1000, col=rec_col, border=NA, xpd=F)
abline( h=axTicks(2) , lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y, col=line_colors[2], lwd=lwd)
error_bars = predict(lm(y[1:38]~x[1:38]), interval='confidence')
# segments(1,error_bars[1], 38, error_bars[38], col=line_colors[2])
error_bars = predict(lm(y[39:51]~x[39:51]), interval='confidence')
# segments(39,error_bars[1], 51, error_bars[51-39], col=line_colors[2])
arrows(x0=13, x1=9, y0=32,y1=32, length=.1)
text(pos = 4, x=13,y=32,"migration")
arrows(x0=28, x1=24, y0=28,y1=28, length=.1)
text(pos = 4, x=28,y=28,"testing start")
source("make_x_axis.R")
dev.off()
