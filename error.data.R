### Error patersn in R:

###_ TRACEBACK
sdfsdf#sdfsf 4343
8: as.array(rv1) at master_pbm.R#58
sdfsf
7: eval(expr, envir, enclos) at pbm_funcs.R#337
6: eval(ue_chebuild.rv_ll_st.dim) at pbm_funcs.R#337
5: eval(expr, envir, enclos) at pbm_funcs.R#337
4: eval(ue_BUILD) at pbm_funcs.R#337
3: eval(substitute(expr), envir, enclos) at pbm_funcs.R#336
2: evalq({
eval(ue_BUILD)
eval(ue_VALIDATE)
}, envir = bcell) at pbm_funcs.R#336
1: bcell(name = "DATA", prototype = .M$dc, LUR = .p(parent = LUR,
ix = ixDM, ix_dim = 1), beta = .p(parent = beta, ix = rep.int(1L,
length(ixDM)), ix_dim = 1L), st = as.matrix(data$dD[, 1:2]),
ll = rep.int(-Inf, length(ixDM)), rv = c(1, 1))
0:


###_ SOURCE (UNEXPECTED SYMBOL)
Error in source(file = "/home/vitoshka/Dropbox/works/pbm/master_pbm.R") :
 /home/vitoshka/Dropbox/works/pbm/master_pbm.R:494:15: unexpected symbol

Error in source(file = "/home/vitoshka/Dropbox/works/pbm/master_pbm.R") : /home/vitoshka/Dropbox/works/pbm/master_pbm.R:494:15: unexpected symbol

###_ RECOVER:

1: BC(name = "DATA", prototype = .M$dc, LUR = .p(parent = LUR, ix = ixDM, ix_dim = 1), beta = .p(parent = beta, ix = rep.int(1, length(ixDM)), ix_dim = 1)
2: pbm_funcs.R#403: evalq(.e(q0_INIT.C), envir = bcell)
3: pbm_funcs.R#403: eval(substitute(expr), envir, enclos)
4: pbm_funcs.R#403: .e(q0_INIT.C)
5: pbm_funcs.R#3: eval(expr, envir, envir)


          ra
