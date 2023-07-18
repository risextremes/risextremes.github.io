
###negative log-Likelihood function evaluation for MLE in K4D #########
#auglag in alabama
loglik2.mle <- function(theta){
  # Programmed by Jeong-Soo Park, 10 Dec. 2018
  # Dept of Stat. Chonnam National University, Korea
  eps.like <- c(1.e-40)
  max.like <- 1.e+15
  c1<- rep(NA,5)
  c1<- cons2.mle(theta)
  if( min(c1) < 0 ) {
  max.like<- max.like +(runif(1)*40 -10)
  # cat( "mllnew=", mllnew,"\n" )
  # mllnew <- mllnew + (runif(1) -.5)
  # return(mllnew)
  return(max.like)
  }
  k <- theta[1]
  h <- theta[2]
  al <- theta[3]
  xi <- theta[4]
  nobs <- length(x.dat)
  G <- rep(NA, nobs)
  W <- rep(NA, nobs)
  work <- rep(NA, nobs)
  G2 <- rep(NA, nobs)
  W2 <- rep(NA, nobs)
  work <- x.dat-xi
  G <- 1-(k*work)/al
  G2 <- G^((k-1)/k)
  W <- 1- h*exp( log(G)/k )
  W2 <- exp( log(W)*(h-1)/h )
  W2 <- exp( log(W)*(h-1)/h )
  W2 <- W2[!is.na(W2)]
  G2 <- G2[!is.na(G2)]
  W2 <- W2[!is.infinite(W2)]
  G2 <- G2[!is.infinite(G2)]
  W2 <- W2[!(W2 < 0)]
  G2 <- G2[!(G2 < 0)]
  W2[W2 < eps.like]<- eps.like
  G2[G2 < eps.like]<- eps.like
  nobs_new<- min( length(W2), length(G2) )
  if( nobs_new < nobs ) {
    mllnew <- max.like
    return(mllnew)
  }

  mllnew<- nobs_new*log(al) + sum(log(G2)) + sum(log(W2))
  if(mllnew < 0) { mllnew<- max.like }
  return(mllnew)
}





year = 1995; month = c("01", "02", "03")
request_base <- list(
  "dataset_short_name" = "reanalysis-era5-single-levels",
  "product_type"   = "ensemble_members",
  "variable"       = vars,
  "year"           = as.character(year),
  "month"          = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  "day"            = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  "time"           = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00","15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  "area"           = "45.5/12/46/12.5",
  "format"         = "netcdf",
  "target"         = "vceExtraVars.nc"
  )

request1 <- request_base
request1$year <- seq(1945, 1948)
request1$target<- "vceExtraVars1.nc"

request1$year <- seq(2021,2022); request1$year 


ncfile <- wf_request(
    user = "188147",
    request = request1,   
    transfer = TRUE,  
    path = "../data/",
    verbose = FALSE, 
    time_out = 12*3600
    )

# png("gevCurve35.png", width=3710,height=1060,units="px",res=180)
par(bg = NA, mai = c(0.1,0.1,0.1,0.1), lwd = 2)
curve(ilaprosUtils::dgev(x,30,6,-0.1), from = 18, to = 80, n = 5000, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
# png("expCurveRed.png", width=3710,height=1060,units="px",res=180)
par(bg = NA, mai = c(0.1,0.1,0.1,0.1), lwd = 4, col = "#9B0014")
curve(dexp(x,0.6), from = 0, to = 9, n = 50000, bty = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")

# dev.off()

x <- matrix(c(1,2,3,1,2,3), ncol = 2); z <- matrix(rnorm(6),ncol = 2); image(x[,1],x[,2],z)

rgb(0.65,.85,0.9)

#x <- matrix(c(1,2,3,1,2,3), ncol = 2); z <- matrix(rnorm(6),ncol = 2); 
#image(x[,1],x[,2],z, col = hcl.colors(400, palette = "viridis", alpha = 0.6), xaxt = "n", yaxt = "n")

xl <- 28; yl <- 9
xdirection <- seq(0,1,length.out =  xl); ydirection <- seq(0,1,length.out =  yl)
set.seed(1242)
x <- matrix(rep(seq(1,xl),yl), ncol = yl); z <- matrix(rnorm(xl*yl, -2*sin(4*pi*xdirection)),ncol = yl); 
z[,yl][z[,yl] > 0]  <- runif(length(z[,yl][z[,yl] > 0]),1,1.4)

# png("G:\\Shared drives\\RISE\\2_Sito\\img\\hero\\heroR.png", width=1920,height=540,units="px",res=180)

par( mai = c(0,0,0,0), col = NA)
image(xdirection , ydirection ,z, 
       col = hcl.colors(400, palette = "Viridis", alpha = 0.4), xaxt = "n", yaxt = "n")
#text(x = .5, y = .5, col = "darkblue", ".5.5")
text(x = .1, y = .1, col = "darkblue", ".1.1")
#text(x = .3, y = .3, col = "darkblue", ".3.3")
#text(x = .1, y = .8, col = "darkblue", ".1.8")
#text(x = .5, y = .8, col = "darkblue", ".5.8")
text(x = .8, y = .8, col = "darkblue", ".8.8")
text(x = .9, y = .9, col = "darkblue", ".9.9")
#text(x = .05, y = .45, col = "darkblue", 
#     latex2exp::TeX("$\\tilde{f}(y) = \\sum_{j=1}^{\\infty}\\pi_j \\phi(y; \\tilde{\\mu}_j, \\sigma^2)$"))
#text(x = .75, y = .95, col = "darkblue", 
#     latex2exp::TeX("H(y) = $1 - \\left(1+ \\xi \\frac{y}{\\sigma}\\right)^{-1/\\xi}$"))
# latex2exp::TeX("$y|\\mu, \\sigma, \\xi \\sim GEV(\\mu, \\sigma, \\xi)$"))

dev.off()



install.packages("extrafont")
library(extrafont)

par(family = "LM Roman 10")









Tommaso Rigon 			120E	Richiesti (seminario fatto)
Fabrizio Laurini			200E	Richiesti
Roberta Pappadà		    	  60E	Richiesti
Omiros Papaspiliopoulos  	200E	Richiesti
Ioannis Kosmidis			300E	Richiesti
Ruggero Bellio 			    0E	Non viene richiesto co-finanziamento 
Cecilia Viscardi		 	???

Totale: 880 euro richiesti.