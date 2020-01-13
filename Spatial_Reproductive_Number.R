
library("Matrix")
library("ggplot2")
library("sf")
library("gganimate")
library("gifski")
library("png")
library("transformr")
# calculate matrix of transmission likelihood (consider generation interval)
# calculate matrix of transmission likelihood (consider generation interval)
calc.lik_GI <- function(t, lpdf_GI , cond=1){
  
  t <- as.numeric(t)
  N <- length(t)
  
  
  if(cond==1){
    dts <- pmax(rep(t, times=N) - rep(t, each=N), 0)
    dT <- Matrix(dts, nrow = N, ncol = N, byrow = T)
    dT@x <-  lpdf_GI(dT@x)
    return(dT)
    
  } else if( cond==2){
    
    ind <- c()
    for(i in 2:N) ind <- c(ind, 1:(i-1))
    dts <- rep(t, times=c(1:N)-1) - t[ind] 
    
    ind <- c()
    for(i in 1:(N-1)) ind <- c(ind, (1:i)-1L)
    pind <- c(0L)
    for(i in 1:(N)) pind <- c(pind, pind[i]+i-1L)
    dT <- new("dtCMatrix", i = ind , p = pind, x= lpdf_GI(dts),
              uplo = "U", diag = "N",
              Dim = c(N, N), Dimnames = list(NULL, NULL))
    return(dT)
  }
}


# calculate matrix of transmission likelihood (consider spatial proximity)
calc.lik_SP <- function(x1, x2, lpdf_SP, lon_lat = T ){
  
  N <- length(x1)
  if(N != length(x2)) stop("length differs in x- and y-coordinte")
  
  x1 <- as.numeric(x1)*pi/180
  x2 <- as.numeric(x2)*pi/180
  
  dx1s <- rep(x1, times=N) - rep(x1, each=N)
  x2s_1 <- rep(x2, times=N)
  x2s_2 <-  rep(x2, each=N)
  dx2s <- x2s_1 - x2s_2
  dss <- sin(dx2s/2)^2+ cos(x2s_1)*cos(x2s_2)*sin(dx1s/2)^2
  dss <- 2*6371E3*asin(sqrt(dss)) 
  
  dss[dss==0] <- 1
  ind <- c(1)
  for(i in 2:N) ind <- c(ind, (i-1)*N + c(1:i))
  dss[ind] <- 0
  
  dS <- Matrix(dss, nrow = N, ncol = N, byrow = T)
  dS@x <-  lpdf_SP(dS@x)
  
  return(dS)
}


# calculate individual R and matrix of transmission probability
calc.Rj <- function(t, x1=NULL, x2=NULL, lpdf_GI, lpdf_SP=NULL, adj.sp=T){
  
  dd <- data.frame(t = as.numeric(t))
  dd$count <- 1
  if(adj.sp){ 
    dd$x1 <- as.numeric(x1)*pi/180 
    dd$x2 <- as.numeric(x2)*pi/180
  }
  dd<- (dd[order(dd$t),])
  
  lik_gi <- calc.lik_GI(t = dd$t, lpdf_GI)
  if(adj.sp){ 
    lik_sp <- calc.lik_SP(x1 = dd$x1, x2 = dd$x2, lpdf_SP)
    lik_sp[lik_gi==0] <- 0
    lik_sp <- drop0(lik_sp)
    lik_t <- lik_gi + lik_sp
  } else { 
    lik_t <- lik_gi }
  
  lik_t@x <- exp(lik_t@x)
  
  Pij <- lik_t %*% Diagonal(x = 1 / colSums(lik_t,na.rm = T) )
  
  Rj <- rowSums(Pij,na.rm = T)
  
  return(list(Rj=Rj, Pij=Pij)) 
} 


summary.y <- function(x,probs=c(.25,.5,.75)){
  x <- stats::na.omit(x)
  v <- quantile(x, probs)
  data.frame(ymin = v[1],y = v[2],  ymax = v[3])}


# plot as time-varying R
plot.Rt <- function(t, Rjs =list("Adjusted"=NULL, "Non-adjusted"=NULL)){
  
  if(class(t)!="Date") t <- as.numeric(t)
  dat <- data.frame(t = t, 
                    Rj1 = Rjs[[1]] )
  if (length(Rjs)==2) dat$Rj2 <- Rjs[[2]]
  dat <- dat[order(dat$t),]
  
  
  colset<- c("#D2691E","#267347")
  names(colset) <- names(Rjs)
  y_up <- quantile(aggregate(formula=Rj1~t,data = dat,FUN = median)$Rj1, 0.99)
  
  GG <- ggplot(data = dat,aes(x = t,y=Rj1))+
    stat_summary_bin(binwidth = 1,color=colset[1],alpha=0.2, size=1.5,
                     fun.data = summary.y ,geom = "linerange")+
    stat_summary_bin(aes(color=names(Rjs)[1]),binwidth = 1,size=1.1,alpha=.85,
                     fun.y = "median",geom = "line")+
    scale_color_manual(values = colset, name="Type")+
    coord_cartesian(ylim=c(0,y_up),expand = F)+
    annotate(geom="segment",x = extendrange(dat$t)[1], 
             xend = extendrange(dat$t)[2], y = 1, yend = 1,
             size=1, alpha=.3)+
    labs(x = "Time", y="Time-varying reproductive number")+
    theme_bw(base_size = 16) %+replace%
    theme(panel.grid=element_blank(),
          legend.position = c(.99,.99),legend.justification = c(1,1) )
  
  
  if (length(Rjs)==2){
    GG <- GG+
      stat_summary_bin(aes(x = t,y=Rj2,color=names(Rjs)[2]),binwidth = 1,size=1.1,alpha=.85,
                       fun.y = "median",geom = "line")+
      stat_summary_bin(aes(x = t,y=Rj2),binwidth = 1,color=colset[2],alpha=0.2,size=1.5,
                       fun.data = summary.y ,geom = "linerange")}
  return(GG)
}


# plot epi curve
plot.epi <- function(t){
  if(class(t)!="Date") t <- as.numeric(t)
  dat <- data.frame(t = t)
  GG <- ggplot(data = dat)+
    geom_histogram(aes(x = t), color="grey80", fill="#e02c50",alpha=1, binwidth=7)+
    theme_bw(base_size = 16) %+replace%
    theme(legend.position = c(.99,.99),legend.justification = c(1,1), 
          panel.grid=element_blank() )+
    labs(x = "Time", y="Weekly incident cases")
  return(GG)
}


# plot point pattern of individual R 
rescale.bnd <- function(bnd){
  out <- bnd
  ind <- which.max(c((bnd[3]-bnd[1]),(bnd[4]-bnd[2])))
  c <- (4/3)*(bnd[4]-bnd[2])/(bnd[3]-bnd[1])
  if(ind==1){
    mid <- (bnd[4]+bnd[2])/2
    d <- (1/c)*(bnd[4]-bnd[2])/2
    out[c(2,4)] <- c(mid-d, mid+d)
  } else if(ind==2){
    mid <- (bnd[3]+bnd[1])/2
    d <- c*(bnd[3]-bnd[1])/2
    out[c(1,3)] <- c(mid-d, mid+d)
  }
  return(out)
}

create.basemap <- function(b){
  G <- ggplot()+
    theme_bw(base_size = 16) %+replace%
    theme(panel.background = element_rect(fill="#f9efda"),
          legend.background = element_blank(),
          legend.position = c(1,0), legend.justification = c(1,0))
  if(!is.null(b)){ 
    G <- G + geom_sf(data = b,fill="#f9efda", color="#b3b3b3")+
      theme(panel.background = element_rect(fill="#a2c0da"), 
            panel.grid = element_line(color="#b6cde2"))
  }
  G
}


create.pts_sf <- function(x1, x2,crs_pts=NULL, base_map=NULL){
  pts<-lapply(1:length(x1),function(i) st_point(c(x1[i],x2[i])))
  if(is.null(crs_pts) & !is.null(base_map)) crs_pts <- st_crs(base_map)
  if(is.null(crs_pts))  crs_pts <- 4326
  st_sf( geometry=st_sfc(pts),crs = crs_pts)
}

cen.pts <- function(sdf, bnd){
  d1 <- st_coordinates(sdf$geometry)[,1] >= bnd[1]
  d2 <- st_coordinates(sdf$geometry)[,1] <= bnd[3]
  d3 <- st_coordinates(sdf$geometry)[,2] >= bnd[2]
  d4 <- st_coordinates(sdf$geometry)[,2] <= bnd[4]
  return(sdf[(d1&d2&d3&d4),])
}

get.qRj <- function(Rj, qn=10){
  qRj <- 100*ecdf(Rj)(Rj)
  qRj <- cut(qRj, breaks = seq(0,100,length.out = qn+1),right = F,include.lowest = T)
  qRj
}

plot.kde <-  function(x1, x2, ngrid=100,  crs_pts=NULL, bnd=NULL, base_map=NULL){
  
  pts <- create.pts_sf(x1, x2, crs_pts, base_map )
  if(is.null(bnd)) bnd <- st_bbox(pts)
  bnd <- rescale.bnd(bnd)
  
  pts <- cbind(pts, st_coordinates(pts$geometry) )
  bwidth <- (MASS::bandwidth.nrd(pts$X)+MASS::bandwidth.nrd(pts$Y))/2
  
  G0 <- create.basemap(base_map)
  bs <- G0$theme$text$size
  
  G0 +
    stat_density_2d(data = pts,
                    mapping = aes(x=X, y=Y, fill = log(..level..)), alpha=.7,
                    geom = "polygon", h = bwidth, n=ngrid)+
    scale_fill_distiller(palette = "RdYlBu",
                         guide =  guide_colorbar(barwidth=unit(.5,"npc"),
                                                 barheight=unit(.02,"npc"),
                                                 direction = "horizontal",
                                                 title="Clustering tendency (log)",
                                                 title.theme = element_text(size=bs*.6),
                                                 label.theme = element_text(size=bs*.6),
                                                 title.position = "top"))+
    coord_sf(xlim = c(bnd[1],bnd[3]),ylim =c(bnd[2],bnd[4]) ,
             crs = st_crs(pts))+
    labs(x="", y="")
  
}

plot.hex <- function(x1, x2, Rj=NULL, nbin=30,
                     crs_pts=NULL, bnd=NULL,  base_map=NULL){
  
  pts <- create.pts_sf(x1, x2, crs_pts, base_map )
  pts <- cbind(pts, st_coordinates(pts$geometry) )
  if(is.null(bnd)) bnd <- st_bbox(pts)
  bnd <- rescale.bnd(bnd)
  
  if(!is.null(Rj)){ 
    z <- Rj 
    zlab <- "Mean Rj"
    f.agg <- mean
    colset <- "YlGnBu"
  }else{ 
    z <- rep(1, length(x1))
    zlab <- "Counts"
    f.agg <- sum
    colset <- "YlOrRd"
  }
  pts <- cbind(pts, z)
  
  pts <-cen.pts(pts, bnd)
  pts.hex <- hexbin::hexbin(pts$X, pts$Y, IDs=T, xbins=nbin ,
                            xbnds=c(bnd[1], bnd[3]),
                            ybnds=c(bnd[2], bnd[4]),shape =1
  )
  pts$cID <- pts.hex@cID
  z.hex <- aggregate(formula= z ~ cID, data=pts, FUN = f.agg)
  
  gdf <- data.frame(hexbin::hcell2xy(pts.hex),
                    cell = pts.hex@cell,
                    z = z.hex$z)
  G0 <- create.basemap(base_map)
  bs <- G0$theme$text$size
  G0 +
    geom_hex(data = gdf, mapping = aes(x=x, y=y, fill=z), 
             colour="white", alpha=.7, stat="identity")+
    scale_fill_distiller(palette = colset, direction = 1,
                         guide =  guide_colorbar(barwidth=unit(.5,"npc"),
                                                 barheight=unit(.02,"npc"),
                                                 direction = "horizontal",
                                                 title = zlab,
                                                 title.theme = element_text(size=bs*.6),
                                                 label.theme = element_text(size=bs*.6),
                                                 title.position = "top"))+
    coord_sf(xlim = c(bnd[1], bnd[3]),ylim =c(bnd[2], bnd[4]), expand=F,
             crs = st_crs(pts))+
    labs(x="", y="")
  
}

plot.points <- function(x1, x2, Rj=NULL, 
                        crs_pts=NULL, bnd=NULL,  base_map=NULL){
  
  pts <- create.pts_sf(x1, x2, crs_pts, base_map )
  if(is.null(bnd)) bnd <- st_bbox(pts)
  bnd <- rescale.bnd(bnd)
  
  
  if(is.null(Rj)){ 
    pts$qRj <- factor("i")
    colset<-c("i"="#A03638")
    psset<-c("i"=3)
    leg <- F
  } else{
    qn <- 10
    pts$qRj <- get.qRj(Rj, qn = qn)    
    colset <- colorRampPalette(c("#eac77b","#8c0d26"))(qn)
    psset <- seq(.5,6,length.out = qn)
    names(colset) <- levels(pts$qRj)
    names(psset) <- levels(pts$qRj)
    leg <- "point"
  }
  
  
  G0 <- create.basemap(base_map)
  bs <- G0$theme$text$size
  gui <- guide_legend(direction = "horizontal",
                      title = "Percentile of Rj",
                      title.theme = element_text(size=bs*.6),
                      label.theme = element_text(size=bs*.6),
                      title.position = "top")
  G <- G0 +
    geom_sf(data = pts,mapping = aes(size=qRj, color=qRj),alpha=.5, 
            show.legend = leg)+
    scale_color_manual(values = colset,drop=F)+
    scale_size_manual(values = psset,drop=F)+
    guides(size = gui, col = gui)+
    coord_sf(xlim = c(bnd[1],bnd[3]),ylim =c(bnd[2],bnd[4]) ,crs = crs_pts)+
    theme(legend.background = element_rect(color="white", 
                                           fill="#fbf6e9"),
          legend.key = element_blank(),
          legend.position = c(.98,.02))
  
  return(G)
}


animate.points <- function(t, x1, x2, Rj, dt, crs_pts=NULL, bnd=NULL,  base_map=NULL){
  
  # t <- df$date
  # x1<-df$long
  # x2<-df$lat
  # Rj <-NULL
  # dt<-15
  # crs_pts=NULL
  # base_map = base_tn 
  # bnd = c(120.1,22.9,120.4,23.1)
  
  pts <-create.pts_sf(x1, x2, base_map = base_map )
  pts$t <- t
  pts$tg <- cut(t, breaks =  seq(min(t),max(t)+dt,dt),right = F)
  
  if(!is.null(Rj)){ 
    pts$Rj <- Rj
    qn <- 10
    colset <- colorRampPalette(c("#eac77b","#8c0d26"))(qn)
    psset <- seq(.5,6,length.out = qn)
    names(colset) <- levels(get.qRj(1,qn = qn))
    names(psset) <- levels(get.qRj(1,qn = qn))
    leg <- "point"
  } else {
    pts$qRj <- factor("i")
    colset<-c("i"="#A03638")
    psset<-c("i"=3)
    leg <- F
  }
  
  pts <- lapply(levels(pts$tg), FUN = function(m){
    sdf <- pts[as.character(pts$tg)==m,]
    if(!is.null(Rj)) sdf$qRj <- get.qRj(sdf$Rj,qn = qn)
    sdf
  })
  
  pts <- do.call("rbind",pts)
  pts$t.int <- as.integer(pts$tg)
  if(is.null(bnd)) bnd <- st_bbox(pts)
  bnd <- rescale.bnd(bnd)
  G0 <- create.basemap(base_map)
  bs <- G0$theme$text$size
  gui <- guide_legend(direction = "horizontal",
                      title = "Percentile of Rj",
                      title.theme = element_text(size=bs*.6),
                      label.theme = element_text(size=bs*.6),
                      title.position = "top")
  G <- G0 +
    geom_sf(data = pts,mapping = aes(size=qRj, color=qRj),
            alpha=.5, show.legend = leg)+
    scale_color_manual(values = colset,drop=F)+
    scale_size_manual(values = psset,drop=F)+
    guides(size = gui, col = gui)+
    coord_sf(xlim = c(bnd[1],bnd[3]),ylim =c(bnd[2],bnd[4]) ,crs = crs_pts)+
    theme(legend.background = element_rect(color="white", 
                                           fill="#fbf6e9"),
          legend.key = element_blank(),
          legend.position = c(.98,.02))
  
  G <- G + transition_time(t.int) +
    labs(title = "Step: {frame_time}")
  
  animate(G, fps = 1,duration=max(pts$t.int))
}


animate.hex <- function(t, x1, x2, Rj, dt, nbin=30, 
                        crs_pts=NULL, bnd=NULL,  base_map=NULL){
  
  # t <- df$date
  # x1<-df$long
  # x2<-df$lat
  # Rj <-df$Rj_adj
  # dt<-7
  # crs_pts=NULL
  # base_map = base_tn
  # nbin=30
  # bnd = c(120.1,22.9,120.4,23.1)
  
  pts <- create.pts_sf(x1, x2, crs_pts, base_map )
  pts <- cbind(pts,t , st_coordinates(pts$geometry) )
  pts$tg <- cut(t, breaks =  seq(min(t),max(t)+dt,dt),right = F)
  
  
  if(!is.null(Rj)){ 
    z <- Rj 
    zlab <- "Mean Rj"
    f.agg <- mean
    colset <- "YlGnBu"
  }else{ 
    z <- rep(1, length(x1))
    zlab <- "Counts"
    f.agg <- sum
    colset <- "OrRd"
  }
  pts <- cbind(pts, z)
  pts <- cen.pts(pts, bnd)
  pts$tg <- droplevels(pts$tg)
  
  dum <- hexbin::hgridcent(xbins=nbin ,shape =1,
                           xbnds=c(bnd[1], bnd[3]),
                           ybnds=c(bnd[2], bnd[4]))
  dum <- data.frame (x = -dum$x[1] - c(0, dum$dx/2),
                     y = -dum$y[1] - c (0, dum$dy),
                     cell = c(0,0),
                     z = c(NA,NA))
  
  df.hex <- lapply(levels(pts$tg), FUN = function(m){
    sdf <- pts[as.character(pts$tg)==m,]
    sdf.hex <- hexbin::hexbin(sdf$X, sdf$Y, IDs=T, xbins=nbin ,
                              xbnds=c(bnd[1], bnd[3]),
                              ybnds=c(bnd[2], bnd[4]),shape =1)
    d <- dum; d$tg <- m
    gdf <- data.frame(hexbin::hcell2xy(sdf.hex),
                      cell = sdf.hex@cell,
                      z = hexbin::hexTapply(sdf.hex, sdf$z, f.agg),
                      tg = m)
    rbind(d,gdf)
  })
  
  df.hex <- do.call("rbind",df.hex)
  df.hex$t.int <- as.integer(factor(df.hex$tg, levels = levels(pts$tg)))
  
  if(is.null(bnd)) bnd <- st_bbox(pts)
  bnd <- rescale.bnd(bnd)
  G0 <- create.basemap(base_map)
  bs <- G0$theme$text$size
  G <- G0+
    geom_hex(data = df.hex, mapping = aes(x=x, y=y, fill=z),
             colour="white", alpha=.7, stat="identity")+
    scale_fill_distiller(palette = colset, 
                         na.value = NA, direction = 1,
                         limits=quantile(df.hex$z,probs = c(.01,.99),na.rm = T),
                         oob=scales::squish,
                         guide =  guide_colorbar(barwidth=unit(.5,"npc"),
                                                 barheight=unit(.02,"npc"),
                                                 direction = "horizontal",
                                                 title = zlab,
                                                 title.theme = element_text(size=bs*.6),
                                                 label.theme = element_text(size=bs*.6),
                                                 title.position = "top")
    )+
    coord_sf(xlim = c(bnd[1], bnd[3]),ylim =c(bnd[2], bnd[4]), expand=F,
             crs = st_crs(pts))+
    labs(x="", y="")
  
  G <- G + transition_time(t.int) +
    labs(title = "Step: {frame_time}")
  
  animate(G, fps = 1,duration=max(df.hex$t.int))
}


