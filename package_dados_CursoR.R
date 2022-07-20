# Base de dados - Curso R ------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 19/07/22 ---------------------------------------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(dados)
library(dplyr)
library(ggplot2)
library(magrittr)
library(tidyr)
library(forcats)

# Identificar bases disponíveis e carregar dados -------------------------------------------------------------------------------------------

av <- dados::avioes
View(av)

# Selecionar dados -------------------------------------------------------------------------------------------------------------------------

av1 <- av %>%
  select(ano, fabricante, motores, assentos, tipo_motor) %>%
  filter(fabricante %in% c("EMBRAER", "AIRBUS INDUSTRIE", "BOEING", "AIRBUS"))
View(av1)

# Análises ---------------------------------------------------------------------------------------------------------------------------------

av2 <- av1 %>%
  group_by(fabricante) %>%
  summarise(media = mean(assentos),
            sd = sd(assentos),
            n = n(),
            se = sd/sqrt(n))
av2

ggplot(av2, aes(x = fabricante, y = media)) +
  geom_col() +
  geom_errorbar(aes(x = fabricante, y = media, ymin = media - sd,
                    ymax = media + sd), width = 0.25, size = 0.8)
