simulacion <- function(n.juv = NA, n.ad = NA, nac = 0.25, p.juv = 0.5, mort.juv = 0.25, mort.ad = 0.1){
  inc.juv  <-  rpois(1, n.ad * nac)
  dec.juv <- rpois(1, (p.juv + mort.juv) * n.juv)
  inc.ad <- rpois(1, p.juv * n.juv) 
  dec.ad <- rpois(1, mort.ad * n.ad)
  
  return(list(d.juv = inc.juv - dec.juv,
              d.ad = inc.ad - dec.ad,
              total.juv = n.juv + inc.juv - dec.juv,
              total.ad = n.ad + inc.ad - dec.ad))
}
