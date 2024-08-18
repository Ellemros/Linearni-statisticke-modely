# setwd("")
source("funkce-LSM2-2023.R")

# 2. pøíklad--------------------------------------------------------------------------------------------------------------------------
load("baseball_hit.Rdata")

# (a) Lineární regresní model---------------------------------------------------------------------------------------------------------
mod <- lm(distance ~ angle + I(angle^2), data = data)


# (b) Pásy spolehlivosti s pravdìpodobností pokrytí 95%-------------------------------------------------------------------------------
xx <- seq(from = min(data$angle), to = max(data$angle), 
          length.out = 200)

MatScheffe <- sapply(1:length(xx), function(i){
  result2 <- ScheffePS1(mod = mod, A = diag(3), 
                        b = c(1, xx[i], xx[i]^2),
                        alpha = 0.05)
  return(result2)
})


# (c) Predikce pro úhel 55 stupòù-----------------------------------------------------------------------------------------------------
prediction <- predict(mod, newdata = data.frame(angle = 55), 
                      interval = "prediction", level = 0.95)


# (d) Simultánní intervalový odhad støední hodnoty pro úhly 20, 25 a 30 stupòù s Bonferroniho adjustací-------------------------------
angles <- c(20, 25, 30)
alpha <- 0.05
bonferroni.alpha <- alpha / length(angles)
simultaneous.interval <- predict(mod, newdata = 
                                   data.frame(angle = angles),
                                 interval = "confidence",
                                 level = 1 - bonferroni.alpha)
lower.bound.simultaneous <- simultaneous.interval[, "lwr"]
upper.bound.simultaneous <- simultaneous.interval[, "upr"]


# (e) Vykreslení výsledkù-------------------------------------------------------------------------------------------------------------
plot(data$angle, data$distance, pch = 16, xlab = "Úhel odpalu (ve °)",
     ylab = "Délka odpalu (v mm)")
curve(coef(mod)[1] + coef(mod)[2] * x + coef(mod)[3] * x^2, add = TRUE, 
      col = "black")

# Vyplnìní pásù spolehlivost
polygon(c(xx, rev(xx)), c(MatScheffe[1,], rev(MatScheffe[2,])),
        col = rgb(1, 0, 0, 0.1), border = NA)

# Bodové a intervalové odhady pro predikce
points(55, prediction[1], col = "blue", pch = 16)
arrows(55, prediction[2], 55, prediction[3], col = "blue", length = 0.03, 
       angle = 90, code = 3)

# Simultánní intervalový odhad støední hodnoty pro úhly 20, 25 a 30 stupòù s
# Bonferroniho adjustací
arrows(angles, lower.bound.simultaneous, angles, upper.bound.simultaneous,
       col = "red", length = 0.03,  angle = 90, code = 3)

# Legenda
legend("bottomright", legend = c("Lineární model", "Schéffeho PS",
                                    "Predikèní IS", "Simultanní IS"),
       col = c("black", rgb(1, 0, 0, 0.1), "blue", "red"), lty = c(1, 0, 1, 1),
       lwd = c(1, 1, 1, 1), pch = c(NA, 15, 16, NA), cex = 0.7)


# 3. pøíklad--------------------------------------------------------------------------------------------------------------------------
data <- read.table("lrm-foot.txt", header = T, sep = "", dec = ".")

# (a) Èištìní dat + vykreslení krabicových grafù--------------------------------------------------------------------------------------
any(is.na(data)) # Chybìjící hodnoty?
unique(data$sex) # Prvky mimo obor hodnot?
sort(unique(data$foot.L)) # Prvky mimo obor hodnot?
sort(unique(data$body.H)) # Prvky mimo obor hodnot?

# Krabicové diagramy
boxplot(body.H ~ sex, data = data, 
        main = "Výška studentù podle pohlaví", 
        xlab = "Pohlaví", ylab = "Výška",
        col = c("blue", "red"), 
        border = "black", 
        notch = T, 
        notchwidth = 0.5, 
        medcol = c("#FFFFFF", "#FFFFFF"), 
        whiskcol = c("#000000", "#000000"), 
        staplecol = c("#000000", "#000000"), 
        boxwex = c(0.5, 0.5), 
        lwd = 1, 
        outpch = 16,
        outcol = "brown", 
        names = c("Muži", "Ženy"), 
        cex.axis = 1.2) 


# (b) Výbìr modelu--------------------------------------------------------------------------------------------------------------------
# Model s interakcí
model1 <- lm(body.H ~ sex * foot.L, data = data)
summary(model1)

# Model bez interakce (ANCOVA)
model2 <- lm(body.H ~ sex + foot.L, data = data)
summary(model2)

# Model bez vlivu promìnné foot.L
model3 <- lm(body.H ~ foot.L, data = data)
summary(model3)
coefficients(model3)


# (c) Vytvoøení bodového grafu + vykreslení modelù------------------------------------------------------------------------------------
plot(data$foot.L, data$body.H, pch = 16, 
     col=ifelse(data$sex == "m", "blue", "red"),
     xlab = "Délka chodidla (v mm)", ylab = "Tìlesná výška (v mm)",
     main = "Závislost tìlesné výšky na délce chodidla")

# Vykreslení modelù
xx <- seq(min(data$foot.L), max(data$foot.L), length = 100)

# Model 1
yy.model1.male <- predict(model1, newdata = data.frame(foot.L = xx, sex = "m"))  
lines(xx, yy.model1.male, col = "pink", lwd = 1)
yy.model1.female <- predict(model1, newdata = data.frame(foot.L = xx,
                                                         sex = "f"))  
lines(xx, yy.model1.female, col = "purple", lwd = 1)

# Model 2
yy_model2 <- predict(model2, newdata = data.frame(foot.L = xx, sex = "m"))  
lines(xx, yy_model2, col = "orange", lwd = 1)
yy_model2 <- predict(model2, newdata = data.frame(foot.L = xx, sex = "f"))  
lines(xx, yy_model2, col = "black", lwd = 1)

# Model 3
lines(data$foot.L, predict(model3), col = "green", lwd = 1)

# Legenda
legend("topleft", legend = c("Muži", "Ženy", "Model 1 - Muži",
                             "Model 1 - Ženy", "Model 2 - Muži", 
                             "Model 2 - Ženy", "Model 3"),
       col = c("blue", "red", "pink", "purple", "orange", "black", "green"),
       lwd = c(NA, NA, 1, 1, 1, 1, 1), cex = 0.7,
       pch = c(19, 19, NA, NA, NA, NA, NA))


# (d) Vykreslení grafu a simultánní odhad------------------------------------------------------------------------------
plot(data$foot.L, data$body.H, pch = 16, col = "black",
     xlab = "Délka chodidla (v mm)", ylab = "Tìlesná výška (v mm)",
     main = "Závislost tìlesné výšky na délce chodidla")
lines(data$foot.L, predict(model3), col = "green")

# Výpoèet minimální a maximální délky chodidla
min.foot <- min(data$foot.L)
max.foot <- max(data$foot.L)

# Vyplnìní pásu spolehlivosti
xx <- seq(from = min.foot, to = max.foot, length.out = 200)

MatScheffe <- sapply(1:length(xx), function(i) {
  result2 <- ScheffePS2(mod = model3, A = diag(2), b = c(1, xx[i]), 
                        alpha = 0.05)
  return(result2)
})

polygon(c(xx, rev(xx)), c(MatScheffe[1,], rev(MatScheffe[2,])), 
        col = rgb(0, 1, 0, 0.1), border = NA)

# Odhad støední hodnoty tìlesné výšky pro jedince s délkou chodidla 
# 230, 250 a 270 mm
foot.lengths <- c(230, 250, 270)
alpha <- 0.05
alpha.adjusted <- (1 - (1 - alpha) ^ (1 / length(foot.lengths)))/2
simultaneous.interval <- predict(model3, 
                                 newdata = data.frame(foot.L = foot.lengths), 
                                 interval = "confidence",
                                 level = 1 - alpha.adjusted)
lower.bound.simultaneous <- simultaneous.interval[, "lwr"]
upper.bound.simultaneous <- simultaneous.interval[, "upr"]
arrows(foot.lengths, lower.bound.simultaneous, foot.lengths, 
       upper.bound.simultaneous, col = "red", length = 0.03,  angle = 90, 
       code = 3)

# Legenda
legend("topleft", c("Lineární model", "Schéffeho PS", "Simultánní IS"),
       col = c("green", rgb(0, 1, 0, 0.1), "red"),
       lty = c(1, 0, 1),
       pch = c(NA, 15, NA),
       cex = 0.8)

# (e) Predikce výšky chlapce, jenž má délku chodidla 240 mm--------------------------------------------------------------------------
prediction <- predict(model3, newdata = data.frame(foot.L = 240),
                      interval = "prediction", level = 0.95)
points(240, prediction[1], col = "blue", pch = 16)
arrows(240, prediction[2], 240, prediction[3], col = "blue", length = 0.03, 
       angle = 90, code = 3)

# Legenda
legend("topleft", c("Lineární model", "Schéffeho PS", "Simultánní IS",
                    "Predikèní IS"),
       col = c("green", rgb(0, 1, 0, 0.1), "red", "blue"),
       lty = c(1, 0, 1, 1),
       pch = c(NA, 15, NA, 16),
       cex = 0.8)
