rates <- read.csv("../tables/all.csv")
x <- rates$week
title <- ''
y <- rates$rate.shipping.2011
png(file="shipping_rate_2011.png", width=width, height=3.5, units="in", res=res )
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
y_range = range(y)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=y_range, axes=F)
rect(38, -10, 39, 450, col="lightgray", border=NA)
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y,  col=line_colors[2], type="l", lwd=lwd)
source("make_x_axis.R")
arrows(x0=39, x1=32, y0=300, y1=300, length=.1)
arrows(x0=39, x1=36, y0=300, y1=225, length=.1)
text(pos=4, labels="PANDA stations\n removed", x=39,y=300, cex=.8)
dev.off()
