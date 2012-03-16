rates <- read.csv("../tables/all.csv")
x = rates$week
y = rates$rate.returns.2011
title = "Returns Rate 2011"
ylab = "units/man hour"
xlab.text = "week in year"
xlab=xlab.text
if (months.grid == T) xlab = ''
months.grid=T
png(file="return_rate_2011_2.png", width=5,
height=3.5, units="in", res=res)
bottom.margin = 4
top.margin = .5
if (make.title == T ) top.margin = 3
if (make.title != T ) title=""
if (months.grid == T) bottom.margin = 6
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
x_range = range(x)
y_range = range(y)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title,
ylim=y_range, axes=F, xlim=x_range)
rect(38, -6, 39, 40, col=rec_col, border=NA, xpd=F)
arrows(x0=30, y0=27, x1=27, y1=27, length=.1)
text(pos=4, labels="disruption", x=30, y=27)
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y, col=line_colors[2], lwd=lwd, lty=line_types[2])
source("make_x_axis.R")

dev.off()
