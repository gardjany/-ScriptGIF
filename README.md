# Instale os pacotes se necessário
# install.packages("tidyverse", "gganimate", "transformr")
# Instalar pacote auxiliar
install.packages("janitor")
library(janitor)
library(tidyverse)
library(gganimate)
library(transformr)
# Ler e limpar os dados
dados <- read.csv("caminho_do_arquivo.csv", sep = ";")
dados <- janitor::clean_names(dados)

# Filtrar SP
dados_sp <- dados %>% filter(uf == "UF")

# Longo + angulo
dados_long <- dados_sp %>%
  pivot_longer(cols = jan:dez, names_to = "mes", values_to = "temperatura") %>%
  mutate(
    mes = factor(mes, levels = c("jan", "fev", "mar", "abr", "mai", "jun",
                                 "jul", "ago", "set", "out", "nov", "dez")),
    mes_num = as.numeric(mes),
    angle = mes_num * 30 - 30
  )

# Grafico
p <- ggplot(dados_long, aes(x = angle, y = temperatura, group = nome_da_estacao)) +
  geom_path(alpha = 0.3, linewidth = 0.5, color = "steelblue") +
  coord_polar(start = 0, direction = -1) +
  scale_x_continuous(breaks = seq(0, 330, 30),
                     labels = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                                "Jul", "Ago", "Set", "Out", "Nov", "Dez")) +
  theme_minimal() +
  labs(title = "Espiral Climática – Temperatura Máxima Média (UF, 1991–2020)")

# Salvar Animacao
anim <- p + transition_reveal(mes_num)
anim_save("Nome_UF_Espiral_Climatica.gif", animation = anim, width = 800, height = 800, res = 100)
