rates <- read.csv("../tables/all.csv")
x = rates$week
y1 = rates$rate.outbound.2010
y2 = rates$rate.outbound.2011
title=''
y_range <- range(y1, y2, na.rm=T)
png(file="outbound_rate_2010_2011.png", width=width,
height=3.5, units="in", res=res)
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
dev.off()
