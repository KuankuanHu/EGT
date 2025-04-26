# Functions
fitness_C <- function(c, p_otherSex){
  return(1/(1-(1-c)*p_otherSex))
}

fitness_D <- function(c, p_otherSex, p_otherSexCop){
  return(p_otherSexCop* 1/(1-p_otherSex))
}

#N = 100

### Equal sex ratio, equal initial conditions
cost = 0.5
gen = 50
p1 = 0.5
p2 = 0.5
init_p1C = 0.1
init_p2C = 0.1

sex1_freq_change <- c(init_p1C)
sex2_freq_change <- c(init_p2C)

for (i in 1:gen) {
  p1C <- tail(sex1_freq_change, 1)
  p2C <- tail(sex2_freq_change, 1)
  
  fitness1C <- fitness_C(cost, p2)
  fitness1D <- fitness_D(cost, p2, p2C)
  fitness2C <- fitness_C(cost, p1)
  fitness2D <- fitness_D(cost, p1, p1C)
  
  mean_fitness1 <- p1C * fitness1C + (1-p1C) * fitness1D
  mean_fitness2 <- p2C * fitness2C  + (1-p2C) * fitness2D
  
  rslt_p1C <- p1C * fitness1C/mean_fitness1
  rslt_p2C <- p2C * fitness2C/mean_fitness2
  
  sex1_freq_change <- append(sex1_freq_change, rslt_p1C)
  sex2_freq_change <- append(sex2_freq_change, rslt_p2C)
}

equal_ratio <- data.frame(Generation = seq(1, 51, 1), sex1 = sex1_freq_change, sex2 = sex2_freq_change)
equal_ratio <- pivot_longer(equal_ratio, !Generation, values_to = "Frequency", names_to = "Sex")
ggplot(equal_ratio, aes(x = Generation, y = Frequency, color = Sex)) + geom_line() + theme_classic()

### Unbalanced Sex Ratio
cost = 0.1
gen = 50
p1 = 0.45
p2 = 0.55
init_p1C = 0.1
init_p2C = 0.1

sex1_freq_change <- c(init_p1C)
sex2_freq_change <- c(init_p2C)

for (i in 1:gen) {
  p1C <- tail(sex1_freq_change, 1)
  p2C <- tail(sex2_freq_change, 1)
  
  fitness1C <- fitness_C(cost, p2)
  fitness1D <- fitness_D(cost, p2, p2C)
  fitness2C <- fitness_C(cost, p1)
  fitness2D <- fitness_D(cost, p1, p1C)
  
  mean_fitness1 <- p1C * fitness1C + (1-p1C) * fitness1D
  mean_fitness2 <- p2C * fitness2C  + (1-p2C) * fitness2D
  
  rslt_p1C <- p1C * fitness1C/mean_fitness1
  rslt_p2C <- p2C * fitness2C/mean_fitness2
  
  sex1_freq_change <- append(sex1_freq_change, rslt_p1C)
  sex2_freq_change <- append(sex2_freq_change, rslt_p2C)
}

equal_ratio <- data.frame(Generation = seq(1, 51, 1), sex1 = sex1_freq_change, sex2 = sex2_freq_change)
equal_ratio <- pivot_longer(equal_ratio, !Generation, values_to = "Frequency", names_to = "Sex")
ggplot(equal_ratio, aes(x = Generation, y = Frequency, color = Sex)) + geom_line() + theme_classic()
