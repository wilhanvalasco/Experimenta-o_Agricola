# ------------------------------
# 1) Dados
# ------------------------------
dados <- data.frame(
  Trat = rep(c("Controle", "Aditivo", "Oleo", "Extrato"), each = 5),
  Resp = c(
    400, 405, 403, 398, 410,    # Controle
    730, 715, 719, 735, 729,    # Aditivo Orgânico
    500, 510, 480, 515, 490,    # Óleo Essencial
    520, 530, 500, 515, 490     # Extrato
  )
)
head(dados)

# ------------------------------
# 2) ANOVA
# ------------------------------
m <- aov(Resp ~ Trat, data = dados)
summary(m)

# ------------------------------
# 3) Teste de Tukey
# ------------------------------
tukey <- TukeyHSD(m)
tukey

# ------------------------------
# 4) Grupos de Tukey (letras)
# ------------------------------
library(agricolae)
tukey.agricolae <- HSD.test(m, "Trat", group = TRUE)
tukey.agricolae$groups
