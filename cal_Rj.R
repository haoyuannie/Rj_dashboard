
##
lpdf_GI <- function(dt) dgamma(dt, 
                               # translate gamma distributed mean-variance to shape-sale parameter
                               shape = (input$gamma_mean^2)/input$gamma_variance, 
                               scale = input$gamma_variance/input$gamma_mean, 
                               # use log probability to deal with small probabilities
                               log = input$Log1)
lpdf_SP <- function(dd) dexp(dd,
                             # translate exponential distributed mean to rate
                             rate = 1/input$dist, 
                             log = input$Log2)

# Calculate non-adjusted reproductive numbers
# res_adj <- calc.Rj(t = df$date, x1 = df$long, x2 = df$lat,
#                    lpdf_GI = lpdf_GI, lpdf_SP = lpdf_SP, adj.sp = T)
# 
# #summary(res_adj$Rj) 
# 
# 
# res <- calc.Rj(t = df$date, 
#                lpdf_GI = lpdf_GI, adj.sp = F)
# #summary(res$Rj)
# 
# df$Rj <- res$Rj
# df$Rj_adj <- res_adj$Rj

##