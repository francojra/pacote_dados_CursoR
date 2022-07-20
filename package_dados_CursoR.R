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
  geom_col(fill = "#a6611a") +
  geom_errorbar(aes(x = fabricante, y = media, ymin = media - sd,
                    ymax = media + sd), width = 0.25, size = 0.8) +
  labs(x = "Fabricantes", y = "Número médio de assentos") +
  theme(axis.text = element_text(size = 8.5, color = "black"))

av3 <- av1 %>%
  group_by(tipo_motor) %>%
  summarise(media = mean(assentos),
            sd = sd(assentos),
            n = n(),
            se = sd/sqrt(n))
av3

ggplot(av3, aes(x = tipo_motor, y = media)) +
  geom_col(fill = "#a6611a") +
  geom_errorbar(aes(x = tipo_motor, y = media, ymin = media - sd,
                    ymax = media + sd), width = 0.25, size = 0.8) +
  labs(x = "Tipo de motor", y = "Número médio de assentos") +
  theme(axis.text = element_text(size = 8.5, color = "black"))

av4 <- av1 %>%
  filter(ano != 1984) %>%
  group_by(ano) %>%
  summarise(media = mean(assentos),
            sd = sd(assentos),
            n = n(),
            se = sd/sqrt(n)) %>%
  drop_na()
av4

ggplot(av4, aes(x = ano, y = media, group = ano)) +
  geom_point(color = "#a6611a", size = 5) +
  #geom_line(aes(x = ano, y = media, group = ano)) +
  geom_errorbar(aes(x = ano, y = media, ymin = media - se,
                    ymax = media + se), width = 0.25, size = 0.8) +
  labs(x = "Ano", y = "Número médio de assentos") +
  theme(axis.text = element_text(size = 8.5, color = "black"))
