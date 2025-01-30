library(smoof)
library(ecr)
library(ggplot2)
library(kableExtra)

set.seed(123)

# alpine01
alpine01_2D <- makeAlpine01Function(2)
alpine01_10D <- makeAlpine01Function(10)
alpine01_20D <- makeAlpine01Function(20)

# Schwefel
schwefel_2D <- makeSchwefelFunction(2) 
schwefel_10D <- makeSchwefelFunction(10) 
schwefel_20D <- makeSchwefelFunction(20) 


# Funkcja zwracająca losowy punkt z przestrzeni n-wymiarowej (dziedziny są symetryczne względem 0):
getRandomPoint <- function(dimensions, domain){
  rndPoint <- replicate(n=dimensions, runif(1,-domain,domain))
  return(rndPoint)
}


# PSA

performPRS <- function(numberOfExec, givenFunc, pointsGenerator, dimensions, domain){
  generatedPoints <- replicate(numberOfExec, pointsGenerator(dimensions, domain))
  pointsMat <- matrix(generatedPoints, nrow = numberOfExec)
  res <- apply(pointsMat, 1, givenFunc)
  return(min(res))
}

alpine01_2D_result_PRS <- replicate(100, performPRS(1000, alpine01_2D, getRandomPoint, 2, 10))
alpine01_10D_result_PRS <- replicate(100, performPRS(1000, alpine01_10D, getRandomPoint, 10, 10))
alpine01_20D_result_PRS <- replicate(100, performPRS(1000, alpine01_20D, getRandomPoint, 20, 10))

schwefel_2D_result_PRS <-  replicate(100, performPRS(1000, schwefel_2D, getRandomPoint, 2, 10))
schwefel_10D_result_PRS <-  replicate(100, performPRS(1000, schwefel_10D, getRandomPoint, 10, 10))
schwefel_20D_result_PRS <-  replicate(100, performPRS(1000, schwefel_20D, getRandomPoint, 20, 10))

mean_alpine01_2D_result_PRS <- mean(alpine01_2D_result_PRS)
mean_alpine01_10D_result_PRS <- mean(alpine01_10D_result_PRS)
mean_alpine01_20D_result_PRS <- mean(alpine01_20D_result_PRS)


mean_schwefel_2D_result_PRS <- mean(schwefel_2D_result_PRS)
mean_schwefel_10D_result_PRS <- mean(schwefel_10D_result_PRS)
mean_schwefel_20D_result_PRS <- mean(schwefel_20D_result_PRS)


# GA

performGA <- function(reapet, numberOfExec, givenFunc, dimensions, domain, numParents, numChildren) {
  maxEvals <- list(stopOnEvals(numberOfExec))
  
  lower <- replicate(dimensions, -domain)
  upper <- replicate(dimensions, domain)
  result <- replicate(reapet, ecr(
    givenFunc,
    n.dim = dimensions,
    lower = lower,
    upper = upper,
    minimize = TRUE,
    representation = "float",
    mu = numParents,
    lambda = numChildren,
    terminators = maxEvals,
    mutator = setup(mutGauss, lower=lower, upper=upper)
  )$best.y)
  
  return(result)
}


alpine01_2D_result_GA <- performGA(100, 1000, alpine01_2D, 2, 10, 50L, 25L)
alpine01_10D_result_GA <- performGA(100, 1000, alpine01_10D, 10, 10, 50L, 25L)
alpine01_20D_result_GA <- performGA(100, 1000, alpine01_20D, 20, 10, 50L, 25L)


schwefel_2D_result_GA <- performGA(100, 1000, schwefel_2D, 2, 10, 50L, 25L)
schwefel_10D_result_GA <- performGA(100, 1000, schwefel_10D, 10, 10, 50L, 25L)
schwefel_20D_result_GA <- performGA(100, 1000, schwefel_20D, 20, 10, 50L, 25L)


mean_alpine01_2D_result_GA <- mean(alpine01_2D_result_GA)
mean_alpine01_10D_result_GA <- mean(alpine01_10D_result_GA)
mean_alpine01_20D_result_GA <- mean(alpine01_20D_result_GA)


mean_schwefel_2D_result_GA <- mean(schwefel_2D_result_GA)
mean_schwefel_10D_result_GA <- mean(schwefel_10D_result_GA)
mean_schwefel_20D_result_GA <- mean(schwefel_20D_result_GA)


# Wizualizacja
visualizeResults <- function(vec1, vec2, mean1, mean2, name1, name2, minimum, binw1, binw2){
  df <- data.frame(
    type=factor(rep(c(name1, name2), each=100)),
    result=round(c(vec1, vec2))
  )
  df1 <- data.frame(
    type=factor(rep(name1)),
    values = vec1)
  df2 <- data.frame(
    type=factor(rep(name2)),
    values = vec2)
  
  
  p1 <- ggplot(df1, aes(x=values))+
    geom_histogram(binwidth = binw1, color="black", fill="blue", alpha=0.4)+
    geom_vline(xintercept=mean1, color="red", linetype="dashed", size=1)+
    geom_vline(xintercept=minimum, color='darkgreen', size=1)+
    ggtitle(name1)
  
  p2 <- ggplot(df2, aes(x=values))+
    geom_histogram(binwidth = binw2, color="black", fill="blue", alpha=0.4)+
    geom_vline(xintercept=mean2, color="red", linetype="dashed", size=1)+
    geom_vline(xintercept=minimum, color='darkgreen', size=1)+
    ggtitle(name2)
  
  
  
  vp <- ggplot(df, aes(x=result , y=type)) + geom_violin(trim=TRUE) + stat_summary(fun = "mean", geom = "point", aes(color = "Mean")) + scale_colour_manual(values = c("red", "blue"), name = "") 
  
  plot(p1)
  plot(p2)
  plot(vp)
}

visualizeResults(alpine01_2D_result_PRS, alpine01_2D_result_GA, mean_alpine01_2D_result_PRS, mean_alpine01_2D_result_GA, "Alpine01 2D PRS", "Alpine01 2D GA", 0, 0.017, 0.0012)
visualizeResults(alpine01_10D_result_PRS, alpine01_10D_result_GA, mean_alpine01_10D_result_PRS, mean_alpine01_10D_result_GA,"Alpine01 10D PRS", "Alpine01 10D GA", 0, 0.75, 0.35)
visualizeResults(alpine01_20D_result_PRS, alpine01_20D_result_GA, mean_alpine01_20D_result_PRS, mean_alpine01_20D_result_GA, "Alpine01 20D PRS", "Alpine01 20D GA", 0, 2, 0.75)



visualizeResults(schwefel_2D_result_PRS, schwefel_2D_result_GA, mean_schwefel_2D_result_PRS, mean_schwefel_2D_result_GA, "Schwefel 2D PRS", "Schwefel 2D GA", 0, 4, 0.4)
visualizeResults(schwefel_10D_result_PRS, schwefel_10D_result_GA, mean_schwefel_10D_result_PRS, mean_schwefel_10D_result_GA, "Schwefel 10D PRS", "Schwefel 10D GA", 0, 1000000, 500)
visualizeResults(schwefel_20D_result_PRS, schwefel_20D_result_GA, mean_schwefel_20D_result_PRS, mean_schwefel_20D_result_GA, "Schwefel 20D PRS", "Schwefel 20D GA", 0, 6000000, 3000)



# Porównanie wyników

df <- data.frame(
  Schwefel_2D = c(mean_schwefel_2D_result_PRS, mean_schwefel_2D_result_GA, abs(mean_schwefel_2D_result_PRS -mean_schwefel_2D_result_GA ),0),
  Schwefel_10D = c(mean_schwefel_10D_result_PRS, mean_schwefel_10D_result_GA, abs(mean_schwefel_10D_result_PRS - mean_schwefel_10D_result_GA),0),
  Schwefel_20D = c(mean_schwefel_20D_result_PRS, mean_schwefel_20D_result_GA, abs(mean_schwefel_20D_result_PRS - mean_schwefel_20D_result_GA),0),
  
  Alpine01_2D = c(mean_alpine01_2D_result_PRS, mean_alpine01_2D_result_GA, abs(mean_alpine01_2D_result_PRS - mean_alpine01_2D_result_GA),0),
  Alpine01_10D = c(mean_alpine01_10D_result_PRS, mean_alpine01_10D_result_GA, abs(mean_alpine01_10D_result_PRS - mean_alpine01_10D_result_GA), 0),
  Alpine01_20D = c(mean_alpine01_20D_result_PRS, mean_alpine01_20D_result_GA, abs(mean_alpine01_20D_result_PRS - mean_alpine01_20D_result_GA), 0)
)

row.names(df) <- c("PRS", "GA", "Różnica","Globalne minimum")


kable(df) %>%
  kable_styling(font_size = 12, position = "center", latex_options = c("striped", "hold_position")) %>% 
  row_spec(0, bold = TRUE, color = "white", background = "gray")


schwefel2Dtest = t.test(schwefel_2D_result_PRS, schwefel_2D_result_GA, paired = TRUE)
schwefel10Dtest = t.test(schwefel_10D_result_PRS,schwefel_10D_result_GA, paired = TRUE)
schwefel20Dtest = t.test(schwefel_20D_result_PRS,schwefel_20D_result_GA, paired = TRUE)

alpine2Dtest = t.test(alpine01_2D_result_PRS, alpine01_2D_result_GA, paired = TRUE)
alpine10Dtest = t.test(alpine01_10D_result_PRS, alpine01_10D_result_GA, paired = TRUE)
alpine20Dtest = t.test(alpine01_20D_result_PRS, alpine01_20D_result_GA, paired = TRUE)

print(schwefel2Dtest)
print(schwefel10Dtest)
print(schwefel20Dtest)
print(alpine2Dtest)
print(alpine10Dtest)
print(alpine20Dtest)




















