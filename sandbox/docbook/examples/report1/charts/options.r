library('getopt')
spec = matrix(c(
  'draft' , 'd', 0, "logical",
  'make.title' , 't', 0, "logical",
  'bw' , 'b', 0, "logical"
), ncol=4, byrow=T)
opt = getopt(spec)
if (length(opt$draft) > 0 && opt$draft == T){
	res = 100
	path = 'force_displacment_draft.png'
}else{
	res = 500
	path = 'force_displacment.png'
}
if (length(opt$bw) > 0 && opt$bw == T){
    line_types = c(2,1,3,4,5,6)
    line_colors = c(rep("black", 10))
    bar_color = c("black", "lightgray")
    base_line_col = "black"
}
