rates <- read.csv("../tables/all.csv")
title = "Total Rate vs. Direct Rate"
x = rates$week
y1 = rates$units.outbound.2011/rates$outbound.direct
y2 = rates$rate.outbound.2011
png(file="direct_vs_total_outbound2_arrows.png", width=5, height=3.5, units="in", res=res)
ylab = "percent increase/decrease" 
xlab.text = "week in year"
xlab = xlab.text
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
y_range = range(y1, y2)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, 
        ylim=y_range, axes=F, xlim=c(range(x)))
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
rect(38, -10, 39, 300, col="lightgray", border=NA, xpd = F)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[2])
lines(x,y2,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
legend("topright", c("direct rate","total rate"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=box.lwd)
source("make_x_axis.R")
arrow_start_off = 1
arrow_end_off = 2
arrows(x0 = x[8], y0 = y1[8] - arrow_start_off, x1=x[8], y1=y2[8] + arrow_end_off, length=.15)
arrows(x0 = x[25], y0 = y1[25] - arrow_start_off, x1=x[25], y1=y2[25] + arrow_end_off, length=.15)
arrows(x0 = x[52], y0 = y1[52] + 1, x1=x[52], y1=y2[52] + arrow_end_off - 2, length=.15)
dev.off()

