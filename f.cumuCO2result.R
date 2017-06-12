# functie om cumuCO2 uit te rekenen aan de hand van een gegeven Ttarget
# voorwaarden: gemiddelde en std van T2010, TCRE en CO22010 zijn gegeven
f.cumuCO2result <- function(Ttarget, N, f.seed = 21) {
  require(lhs)
  set.seed(f.seed)
  x <- randomLHS(N, 4)
  # geef namen
  colnames(x) <- c("Ttarget", "T2010", "TCRE", "CO22010")
  
  # transformeer random LHS naar LHS met goede parameters
  f.Ttarget <- rep(Ttarget, N)
  T2010 <- qnorm(x[,2], mean=T2010mean, sd=T2010std)
  TCRE <- qnorm(x[,3], mean=TCREmean,sd=TCREstd)
  CO22010 <- qnorm(x[,4], mean=CO22010mean, sd=CO22010std)
  
  # run model
  return(mapply(oneRun, f.Ttarget, T2010, TCRE, CO22010))
}