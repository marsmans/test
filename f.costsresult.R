# functie om cumuCO2 uit te rekenen aan de hand van een gegeven Ttarget
# voorwaarden: gemiddelde en std van costs.slope, en model zijn gegeven
f.costsresult <- function(cumuCO2result, N, f.seed = 21) {
  require(lhs)
  set.seed(f.seed)
  costs.x <- randomLHS(N, 2)
  # geef namen
  colnames(costs.x) <- c("cost.slope","baselineCO2")
  
  # transformeer random LHS naar LHS met goede parameters
  #costs.slope <- qnorm(costs.x[,1], mean=costs_mean, sd=costs.std)
  costs.slope <- qpert(costs.x[,1], coef(gUL)[2], costs_mean, coef(gLL)[2], shape = 4)
  baselineCO2 <- rep(6000, N)
  
  # run model
  return(mapply(costs.oneRun, cumuCO2result, costs.slope, baselineCO2))
}