rates <- read.csv("../tables/all.csv")
y1 = rates$outbound.indirect/rates$outbound.direct

y1 = rates$indirect.hours.outbound.2011/(rates$direct.hours.outbound.2011 + rates$indirect.hours.outbound.2011)
y2 = rates$rate.outbound.2011
x = rates$week

y1p = y1/y1[1]
y1p = y1p * 100
y2p = y2/y2[1]
y2p = y2p * 100
png(file="indirect_out_vs_outbound_percent.png", width=width, height=3.5, units="in", res=res)
title="Indirect to Total Ratio vs. Outbound"
ylab = "percent increase/decrease" 
xlab.text = "Week in Year"
xlab = xlab.text
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
if (months.grid == T) bottom.margin = 6
if (months.grid == T) xlab = ''
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
y_range = range(y1p,y2p, na.rm=T)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, 
        ylim=c(y_range), axes=F, xlim=c(range(x)))
axis(1, col=ax_col)
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
rect(38, -10, 39, 300, col="lightgray", border=NA, xpd = F)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
abline( h=100,  lwd=1, col=base_line_col, lty= base_line_type)
lines(x,y1p,  col=line_colors[2], type="l", lwd=lwd, lty=line_types[2])
lines(x,y2p,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
legend("topleft", c("outbound rate","indirect rate"), cex=legend_font_size, 
 col=line_colors,  lty=line_types,  bty='y', bg="white", box.lwd=0)
text(x=45, y=105, cex=.8, "base line")
error_bars = predict(lm(y1p[1:38]~x[1:38]), interval='confidence')
# segments(1,error_bars[1], 38, error_bars[38], col=line_colors[2])
error_bars = predict(lm(y2p[1:38]~x[1:38]), interval='confidence')
# segments(1,error_bars[1], 38, error_bars[38], col=line_colors[1])
source("make_x_axis.R")

dev.off()
