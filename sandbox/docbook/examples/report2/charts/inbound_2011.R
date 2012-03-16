rates <- read.csv("../tables/all.csv")
title=''
x = rates$week
y = rates$rate.inbound.2011
png(file="inbound_rate_2011.png", width=5, height=3.5, units="in", res=res )
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
y_range = range(y, na.rm=T)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1,
main=title, ylim=c(y_range), axes=F, )
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col='lightgray')
rect(38, -10, 39, 1000, col="lightgray", border=NA, xpd = F)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x ,y, col=line_colors[2], xlim=c(1,50), lwd=lwd)
z = lsfit(1:38,y[1:38])
error_bars = predict(lm(y[1:38]~x[1:38]), interval='confidence')
segments(1,error_bars[1], 38, error_bars[38], col=line_colors[2])
error_bars = predict(lm(y[39:51]~x[39:51]), interval='confidence')
segments(39,error_bars[1], 51, error_bars[51-39], col=line_colors[2])
source("make_x_axis.R")

dev.off()
