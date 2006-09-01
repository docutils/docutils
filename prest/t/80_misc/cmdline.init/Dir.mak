RST_FLAG_badopt = -Q 2>&1 | cat 
POSTPROCESS_badopt = - | head -2

RST_FLAG_noargs = 2>&1 | cat 
POSTPROCESS_noargs = - | head -1

RST_FLAG_opt_d = -d
POSTPROCESS_opt_d = 2>&1

RST_FLAG_opt_dx2 = -d -d
POSTPROCESS_opt_dx2 = 2>&1

RST_FLAG_opt_dx3 = -d -d -d
POSTPROCESS_opt_dx3 = 2>&1

RST_FLAG_opt_h = -h
POSTPROCESS_opt_h = | egrep '^(Description of|(Defines|Documentation) for)'

RST_FLAG_opt_V = -V 2>&1 | cat 
POSTPROCESS_opt_V = - | head -1

