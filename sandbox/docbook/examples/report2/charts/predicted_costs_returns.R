data = read.csv("../tables/projected_rates_returns.csv")
attach(data)
x <- c(1:12)
title <- 'Predicted Rate of Returns, 2012'
y1 <- Rate.Zappos
y2 <- Rate.FCSW
png(file="projected_rate_returns.png", width=5,
height=3.5, units="in", res=res)
xlab.text="month"
xlab = xlab.text
ylab = "units/man hour"
if (make.title == T ) top.margin = 3
if (make.title != T ) title="" 
margins=c(bottom.margin, 4, top.margin, 0.5) 
par(mar=margins) 
par('family'='serif')
y_range = range(y1, y2, na.rm=T)
plot(x, type='n', xlab=xlab, ylab=ylab, las=1, main=title, ylim=c(y_range), axes=F)
at = c(1,2,3,4,5,6,7,8,9,10,11,12)
axis(1, at=at, labels=months, col=ax_col, lab=NULL)  
axis(2, col=ax_col, las=1, at=axTicks(2))
box(col=box_col)
abline( h=axTicks(2), lty=1, lwd=lwd_grid, col=ax_col)
lines(x,y1,  col=line_colors[1], type="l", lwd=lwd, lty=line_types[1])
lines(x,y2, col=line_colors[2],  lwd=lwd, lty=line_types[2])
legend("bottomleft", c("Zappos","FCSW"), cex=legend_font_size, 
 col=line_colors,  lty=line_types, bty='y', bg="white", box.lwd=box.lwd)
dev.off()
