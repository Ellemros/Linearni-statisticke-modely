# setwd("")
source("funkce-LSM2-2023.R")

# 2. p��klad--------------------------------------------------------------------------------------------------------------------------
load("baseball_hit.Rdata")

# (a) Line�rn� regresn� model---------------------------------------------------------------------------------------------------------
mod <- lm(distance ~ angle + I(angle^2), data = data)


# (b) P�sy spolehlivosti s pravd�podobnost� pokryt� 95%-------------------------------------------------------------------------------
xx <- seq(from = min(data$angle), to = max(data$angle), 
          length.out = 200)

MatScheffe <- sapply(1:length(xx), function(i){
  result2 <- ScheffePS1(mod = mod, A = diag(3), 
                        b = c(1, xx[i], xx[i]^2),
                        alpha = 0.05)
  return(result2)
})


# (c) Predikce pro �hel 55 stup��-----------------------------------------------------------------------------------------------------
prediction <- predict(mod, newdata = data.frame(angle = 55), 
                      interval = "prediction", level = 0.95)


# (d) Simult�nn� intervalov� odhad st�edn� hodnoty pro �hly 20, 25 a 30 stup�� s Bonferroniho adjustac�-------------------------------
angles <- c(20, 25, 30)
alpha <- 0.05
bonferroni.alpha <- alpha / length(angles)
simultaneous.interval <- predict(mod, newdata = 
                                   data.frame(angle = angles),
                                 interval = "confidence",
                                 level = 1 - bonferroni.alpha)
lower.bound.simultaneous <- simultaneous.interval[, "lwr"]
upper.bound.simultaneous <- simultaneous.interval[, "upr"]


# (e) Vykreslen� v�sledk�-------------------------------------------------------------------------------------------------------------
plot(data$angle, data$distance, pch = 16, xlab = "�hel odpalu (ve �)",
     ylab = "D�lka odpalu (v mm)")
curve(coef(mod)[1] + coef(mod)[2] * x + coef(mod)[3] * x^2, add = TRUE, 
      col = "black")

# Vypln�n� p�s� spolehlivost
polygon(c(xx, rev(xx)), c(MatScheffe[1,], rev(MatScheffe[2,])),
        col = rgb(1, 0, 0, 0.1), border = NA)

# Bodov� a intervalov� odhady pro predikce
points(55, prediction[1], col = "blue", pch = 16)
arrows(55, prediction[2], 55, prediction[3], col = "blue", length = 0.03, 
       angle = 90, code = 3)

# Simult�nn� intervalov� odhad st�edn� hodnoty pro �hly 20, 25 a 30 stup�� s
# Bonferroniho adjustac�
arrows(angles, lower.bound.simultaneous, angles, upper.bound.simultaneous,
       col = "red", length = 0.03,  angle = 90, code = 3)

# Legenda
legend("bottomright", legend = c("Line�rn� model", "Sch�ffeho PS",
                                    "Predik�n� IS", "Simultann� IS"),
       col = c("black", rgb(1, 0, 0, 0.1), "blue", "red"), lty = c(1, 0, 1, 1),
       lwd = c(1, 1, 1, 1), pch = c(NA, 15, 16, NA), cex = 0.7)


# 3. p��klad--------------------------------------------------------------------------------------------------------------------------
data <- read.table("lrm-foot.txt", header = T, sep = "", dec = ".")

# (a) �i�t�n� dat + vykreslen� krabicov�ch graf�--------------------------------------------------------------------------------------
any(is.na(data)) # Chyb�j�c� hodnoty?
unique(data$sex) # Prvky mimo obor hodnot?
sort(unique(data$foot.L)) # Prvky mimo obor hodnot?
sort(unique(data$body.H)) # Prvky mimo obor hodnot?

# Krabicov� diagramy
boxplot(body.H ~ sex, data = data, 
        main = "V��ka student� podle pohlav�", 
        xlab = "Pohlav�", ylab = "V��ka",
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
        names = c("Mu�i", "�eny"), 
        cex.axis = 1.2) 


# (b) V�b�r modelu--------------------------------------------------------------------------------------------------------------------
# Model s interakc�
model1 <- lm(body.H ~ sex * foot.L, data = data)
summary(model1)

# Model bez interakce (ANCOVA)
model2 <- lm(body.H ~ sex + foot.L, data = data)
summary(model2)

# Model bez vlivu prom�nn� foot.L
model3 <- lm(body.H ~ foot.L, data = data)
summary(model3)
coefficients(model3)


# (c) Vytvo�en� bodov�ho grafu + vykreslen� model�------------------------------------------------------------------------------------
plot(data$foot.L, data$body.H, pch = 16, 
     col=ifelse(data$sex == "m", "blue", "red"),
     xlab = "D�lka chodidla (v mm)", ylab = "T�lesn� v��ka (v mm)",
     main = "Z�vislost t�lesn� v��ky na d�lce chodidla")

# Vykreslen� model�
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
legend("topleft", legend = c("Mu�i", "�eny", "Model 1 - Mu�i",
                             "Model 1 - �eny", "Model 2 - Mu�i", 
                             "Model 2 - �eny", "Model 3"),
       col = c("blue", "red", "pink", "purple", "orange", "black", "green"),
       lwd = c(NA, NA, 1, 1, 1, 1, 1), cex = 0.7,
       pch = c(19, 19, NA, NA, NA, NA, NA))


# (d) Vykreslen� grafu a simult�nn� odhad------------------------------------------------------------------------------
plot(data$foot.L, data$body.H, pch = 16, col = "black",
     xlab = "D�lka chodidla (v mm)", ylab = "T�lesn� v��ka (v mm)",
     main = "Z�vislost t�lesn� v��ky na d�lce chodidla")
lines(data$foot.L, predict(model3), col = "green")

# V�po�et minim�ln� a maxim�ln� d�lky chodidla
min.foot <- min(data$foot.L)
max.foot <- max(data$foot.L)

# Vypln�n� p�su spolehlivosti
xx <- seq(from = min.foot, to = max.foot, length.out = 200)

MatScheffe <- sapply(1:length(xx), function(i) {
  result2 <- ScheffePS2(mod = model3, A = diag(2), b = c(1, xx[i]), 
                        alpha = 0.05)
  return(result2)
})

polygon(c(xx, rev(xx)), c(MatScheffe[1,], rev(MatScheffe[2,])), 
        col = rgb(0, 1, 0, 0.1), border = NA)

# Odhad st�edn� hodnoty t�lesn� v��ky pro jedince s d�lkou chodidla 
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
legend("topleft", c("Line�rn� model", "Sch�ffeho PS", "Simult�nn� IS"),
       col = c("green", rgb(0, 1, 0, 0.1), "red"),
       lty = c(1, 0, 1),
       pch = c(NA, 15, NA),
       cex = 0.8)

# (e) Predikce v��ky chlapce, jen� m� d�lku chodidla 240 mm--------------------------------------------------------------------------
prediction <- predict(model3, newdata = data.frame(foot.L = 240),
                      interval = "prediction", level = 0.95)
points(240, prediction[1], col = "blue", pch = 16)
arrows(240, prediction[2], 240, prediction[3], col = "blue", length = 0.03, 
       angle = 90, code = 3)

# Legenda
legend("topleft", c("Line�rn� model", "Sch�ffeho PS", "Simult�nn� IS",
                    "Predik�n� IS"),
       col = c("green", rgb(0, 1, 0, 0.1), "red", "blue"),
       lty = c(1, 0, 1, 1),
       pch = c(NA, 15, NA, 16),
       cex = 0.8)
