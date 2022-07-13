deaseats_algo <- function(yt, order_poly = 1, frequency = 12, kernel_fun = "Epanechnikov") {
  ## y, s, p, kn are as before

  ##### input parameters
  bopt <- bStart
  bopt_old <- 0
  bopt_old2 <- 0
  n <- length(y)
  k <- order_poly + 1
  bmax <- 0.5 - 1/n     #### The maximum bandwidth 
  bmin <- 1 * s / n     #### The minimum bandwidth 

  v <- k
  pv <- k + 1
  runc <- 1
  
  cb <- 0.05            # Default
  
############# Delta >=0 

  n1 <- trunc(n * cb)

  ###### Kernel constants
    
  wk_0
  m <- 100000
  u <- (-m:m) / (m + 0.5)
  wk <- (1 - u^2)^(kn - 1)
  wk <- wk / sum(wk) * m
  R2 <- sum(wk^2) / (m - 0.5)
   
           if(k==2){wkp_wk}
           if(k==4 && kn==1){
	          wkp_3/8*(3-5*u^2)
             wkp_wkp/sum(wkp)*m
            }
           if(k==4 && kn==2){
	          wkp_15/32*(3-10*u^2+7*u^4)
             wkp_wkp/sum(wkp)*m
            }
           if(k==4 && kn==3){
	          wkp_105/64*(1-5*u^2+7*u^4-3*u**6)
             wkp_wkp/sum(wkp)*m
            }
	  
           Rp_sum(wkp^2)/(m-0.5)
           mukp_sum((u^4)*wkp)/m

        #### two constant in the bandwidth
           c1_factorial(k)**2/(2*k)
           c2_(1-2*d)*(Rp+(s-1)*R2)/(mukp)**2

        ####  difference-based estimation of the variance
                sigDC_1   #### 1 or 2
                if(sigDC==1){sig2D_VarSeaTSD(y,s)$sig2D1}
                if(sigDC==2){sig2D_VarSeaTSD(y,s)$sig2D2}
                sig2_sig2D

        #### the main iteration
           noi_20          ### maximal nuber of iterations

           for(i in 1:noi)
             {
              if(runc==1)
                { 	
                cat("This is the ", i, "th iteration.", fill=T)
         	       if(i>=2){hvold_hv}

########## Estimate the variance

############# The inflation method    ###############	
	             if(k==2){bv_1*bopt^(5/7)}   #### the most important idea
	             if(k==4){bv_bopt^(9/13)}  #### the most important idea

	              bv_max(bv, bmin)
                 bv_min(bv, bmax)
                 hv_trunc(bv*n+0.5)
                 bvP_trunc(bv*1000+0.5)/1000
                 cat("hv=", hv, "    bv=", bvP)

     
                 gv_DerivativeG(y, s, v, pv, kn, hv)$gvt
                 I2_sum(gv[n1:(n-n1)]**2)/n
                 

                 ### the optimal bandwidth
                    c3_sig2/I2 

                    bopt_(c1*c2*c3)**(1/(2*k+1))*n**(-1/(2*k+1))
                    bopt_max(bopt, bmin)
                    bopt_min(bopt, bmax)

                 I0_i

                 hopt_trunc(bopt*n+0.5)
                 boP_trunc(bopt*1000+0.5)/1000              
                 cat("    hopt=", hopt, "    bopt=", boP, fill=T)

                 dh_hvold-hv
                 if(i>=3 && dh==0){runc_0}
                }
             }

         #### smooth with the selected bandwidth
            hopt_trunc(bopt*n+0.5)

            ye_DeSeaTS(y, s, p, kn, hopt)

         #### final results
           result_list(ye=ye, bopt=bopt, hopt=hopt, sig2D=sig2D, I0=I0)
  result 
}