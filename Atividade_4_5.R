library(infer)
library(EnvStats)
library(tidyverse)
library(DescTools)

set.seed(9)
amostra <- rep_sample_n(amostra_190090090 ,size = 500, reps = 1, replace = TRUE)


# NOTA MT E REGIÃO
amostra$REGIAO[amostra$REGIAO == '1'] <- "Norte"
amostra$REGIAO[amostra$REGIAO == '2'] <- "Nordeste"
amostra$REGIAO[amostra$REGIAO == '3'] <- "Sudeste"
amostra$REGIAO[amostra$REGIAO == '4'] <- "Sul"
amostra$REGIAO[amostra$REGIAO == '5'] <- "Centro Oeste"

amostra %>%
  ggplot(aes(x=REGIAO, y=NOTA_MT)) +
  geom_boxplot(fill=c('#008080'), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Região", y="Nota Matemática") +
  scale_x_discrete(limits = c("Norte","Nordeste","Centro Oeste","Sudeste","Sul")) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

ggsave("regiao.png", width = 160, height = 80, units = "mm")

amostra %>%
  group_by(REGIAO)%>%
  summarize(media = mean(NOTA_MT),
            variância = var(NOTA_MT),
            desvio_padrão = sd(NOTA_MT),
            min = min(NOTA_MT),
            q25 = quantile(NOTA_MT, probs = .25),
            mediana  = quantile(NOTA_MT, probs = .5),
            q75 = quantile(NOTA_MT, probs = .75),
            max = max(NOTA_MT))

# teste de normalidade
shapiro.test(amostra$NOTA_MT) # não normal 

# teste de homocedasticidade
LeveneTest(amostra$NOTA_MT,amostra$REGIAO,center = mean) # aceita


# kruskal
kruskal.test(amostra$NOTA_MT,amostra$REGIAO) # rejeita
# comparação multipla
pairwise.wilcox.test(amostra$NOTA_MT,amostra$REGIAO,p.adjust.method = "bonferroni")

# anova
aov_res <- aov(amostra$NOTA_MT ~ amostra$REGIAO) # rejeita
summary(aov_res)
# comparação multipla
pairwise.t.test(amostra$NOTA_MT,amostra$REGIAO,p.adjust.method = "bonferroni")



# NOTA LP E USO DE TELA

amostra$USO_TEMPO_TELAS[amostra$USO_TEMPO_TELAS == 'A'] <- "Menos de\n1 hora"
amostra$USO_TEMPO_TELAS[amostra$USO_TEMPO_TELAS == 'B'] <- "Entre 1 e 2\nhoras"
amostra$USO_TEMPO_TELAS[amostra$USO_TEMPO_TELAS == 'C'] <- "Mais de 2\nhoras,\naté 3 horas"
amostra$USO_TEMPO_TELAS[amostra$USO_TEMPO_TELAS == 'D'] <- "Mais de 3\nhoras"
amostra$USO_TEMPO_TELAS[amostra$USO_TEMPO_TELAS == 'E'] <- "Menos de\n1 hora"
amostra <- amostra %>% drop_na(USO_TEMPO_TELAS)

amostra %>%
  ggplot(aes(x=USO_TEMPO_TELAS, y=NOTA_LP)) +
  geom_boxplot(fill=c('#008080'), width = 0.5) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="Tempo de Uso de Tela", y="Nota Língua Potuguesa") +
  scale_x_discrete(limits = c("Menos de\n1 hora","Entre 1 e 2\nhoras",
                              "Mais de 2\nhoras,\naté 3 horas","Mais de 3\nhoras")) +
  theme_bw() +
  theme(axis.title.y=element_text(colour="black", size=12),
        axis.title.x = element_text(colour="black", size=12),
        axis.text = element_text(colour = "black", size=9.5),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black")) 

 ggsave("tela.png", width = 160, height = 80, units = "mm")

amostra %>%
  group_by(USO_TEMPO_TELAS)%>%
  summarize(media = mean(NOTA_LP),
            variância = var(NOTA_LP),
            desvio_padrão = sd(NOTA_LP),
            min = min(NOTA_LP),
            q25 = quantile(NOTA_LP, probs = .25),
            mediana  = quantile(NOTA_LP, probs = .5),
            q75 = quantile(NOTA_LP, probs = .75),
            max = max(NOTA_LP))

# teste de normalidade
shapiro.test(amostra$NOTA_LP) # não normal 

# teste de homocedasticidade
LeveneTest(amostra$NOTA_LP,amostra$USO_TEMPO_TELAS,center = mean) # aceita

# kruskal
kruskal.test(amostra$NOTA_LP,amostra$USO_TEMPO_TELAS) # rejeita
# comparação multipla
pairwise.wilcox.test(amostra$NOTA_LP,amostra$USO_TEMPO_TELAS,p.adjust.method = "bonferroni")

# ANOVA
aov_res <- aov(amostra$NOTA_LP ~ amostra$USO_TEMPO_TELAS) # rejeita
summary(aov_res)
# comparação multipla
pairwise.t.test(amostra$NOTA_LP,amostra$USO_TEMPO_TELAS,p.adjust.method = "bonferroni")
