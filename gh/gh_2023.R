# Scraping GH nominaciones ------------------------------------------------
# 1. Scraping tablas wikipedia
# 2. Red de nominaciones
# 3. Votos por semana
# 4. Grid arrange

# Libs --------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(rlist)
library(stringi)
library(ggraph)
library(igraph)
library(grid)
library(gridExtra)
library(ggh4x)

# Scraping ----------------------------------------------------------------
url = 'https://es.wikipedia.org/wiki/Anexo:Und%C3%A9cima_temporada_de_Gran_hermano_(programa_de_televisi%C3%B3n_argentino)'

tablas <- url %>%
  read_html() %>%
  html_nodes("table")

# Participantes
tabla_participantes <- tablas %>% 
  # Tabla 3
  .[3] %>%
  html_table(fill = TRUE) %>%
  data.frame() %>%
  janitor::clean_names() %>%
  separate(
    nombre_1,
    into = c("participantes", "descrip"),
    sep = " ",
    extra = "merge"
  ) %>%
  mutate(estado = ifelse(estado != "En competencia", "Eliminado", "En competencia"))

# Nominaciones
tabla_nominaciones <- tablas %>% 
  # Tabla 6
  .[6] %>%
  html_table(fill = TRUE) %>% 
  data.frame() %>% 
  slice(3:24) %>% 
  select(participantes = Var.1, Semana.1, Semana.2, Semana.3) %>% 
  janitor::clean_names() %>% 
  mutate(
    across(semana_1:semana_3,
           ~ str_replace_all(., "(?<=[a-z])(?=[A-Z])", ", "))
  ) %>% 
  pivot_longer(cols=semana_1:semana_3, names_to='semana') %>% 
  separate(value, c("votos_2","votos_1"), ", ") 

tabla_nominaciones <- tabla_nominaciones %>%
  pivot_longer(cols = starts_with("votos"), names_to = "votos", 
               values_to = "votado") %>% 
  mutate(
    semana = str_replace(semana, pattern="semana_",""),
    votos = str_replace(votos, pattern="votos_","")
  ) %>% 
  filter(
    votado %in% tabla_nominaciones$participantes
  )

# Votos
tabla_votos <- tablas %>% 
  # Tabla 7
  .[7] %>%
  html_table(fill = TRUE) %>% 
  data.frame() %>% 
  slice(2:23) %>% 
  janitor::clean_names() %>% 
  mutate(total_de_votos_recibidos = as.numeric(total_de_votos_recibidos)) 

# Df
df <- tabla_nominaciones %>% 
  left_join(tabla_votos %>% select(participantes, total_de_votos_recibidos), 
            by='participantes') %>% 
  select(participantes, votado, semana, votos )

semanas <- length(df$semana %>% unique())
semanas_colores <- colorRampPalette(c("grey", "darkblue"))(semanas)

# Plot network ------------------------------------------------------------
g <- graph_from_data_frame(df)

vertices <- data.frame(participantes = V(g)$name) %>% 
  left_join(tabla_votos, by='participantes') %>% 
  left_join(tabla_participantes %>% select(participantes, estado), 
            by='participantes') %>% 
  select(participantes, total_de_votos_recibidos, estado)

V(g)$votos_totales <- vertices$total_de_votos_recibidos
V(g)$estado <- vertices$estado

set.seed(42) # Reproducibilidad del layout
p_network <- g %>% 
  ggraph(layout = 'igraph', algorithm = 'nicely') + 
  geom_edge_fan2(
    aes(color = as.factor(semana), width = as.numeric(votos)),
    show.legend = TRUE, alpha=0.9,
    arrow = arrow(angle = 20, length = unit(0.3, "cm"),
                  ends = "last", type = "closed")
  ) + 
  geom_node_point(aes(size=votos_totales, color=estado), show.legend = FALSE) +
  geom_node_label(aes(label=name, size=votos_totales, color=estado), 
                  show.legend = FALSE, repel = TRUE, family='mono') + 
  scale_edge_color_manual(values = semanas_colores) +
  scale_color_manual(values=c("red","black"))+
  scale_edge_width(range = c(0.5, 1))+
  guides(size = "none", color="none", edge_width="none") +
  labs(edge_color='Semana')+
  theme_void()+
  theme(
    legend.direction = 'horizontal',
    text = element_text(family = "mono"),
    legend.position = "bottom"
  )

p_network


# Plot votos por semana ------------------------------------------------
eliminados <- tabla_participantes %>% 
  filter(estado=='Eliminado') %>% 
  pull(participantes)

temp <- tabla_votos %>% 
  select(participantes, starts_with('semana')) %>% 
  pivot_longer(starts_with('semana'), names_to='semana', values_to='votos') %>% 
  mutate(
    votos=as.numeric(votos), 
    semana=as.numeric(
      str_replace(ifelse(semana=="semana", 'semana_0',semana), 'semana_','')
    )+1,
    semana = paste0('Semana ',semana),
    estado = ifelse(participantes %in% eliminados,'Eliminado','En competencia'),
    point_color = ifelse(estado=="Eliminado",0,semana)
  ) %>% 
  filter(!is.na(votos)) %>% 
  # Votos para nominación creo que son 5 (?)
  filter(votos>=5)

p_votos <- temp %>% ggplot(
    aes(
      x = votos,
      y = tidytext::reorder_within(participantes, votos, semana),
      yend = tidytext::reorder_within(participantes, votos, semana),
      color = point_color,
      fill = factor(semana)
    )
  )+
  geom_segment(aes(x=0, xend=votos))+
  geom_point(aes(color=point_color), size=3)+
  scale_color_manual(values=c('red',semanas_colores)) +
  facet_grid2(semana~., scales = 'free_y', 
    strip = ggh4x::strip_themed(
      background_y = list(
        element_rect(fill = semanas_colores[1]),
        element_rect(fill = semanas_colores[2]),
        element_rect(fill = semanas_colores[3])
      ), 
    )
  )+
  labs(y='', x='Votos')+
  scale_fill_manual(values = semanas_colores) +
  theme_bw()+
  tidytext::scale_y_reordered() +
  theme(
    legend.direction = 'horizontal',
    plot.title=element_text(hjust=0.5, size=30),
    plot.subtitle = element_text(hjust=0.5, size=20),
    text = element_text(family = "mono"),
    legend.position = "none", 
    strip.text = element_text(color='white', face='bold', size=10))

p_votos

p <- grid.arrange(p_network, p_votos, ncol=2, widths=c(7,3))

title = textGrob("Gran hermano 2023",
  gp = gpar(fontface = 1, fontsize = 30, fontfamily='mono'),
  hjust = 0.5, x = 0.5
)

subtitle = textGrob("Nominaciones por semana",
                 gp = gpar(fontface = 1, fontsize = 20, fontfamily='mono'),
                 hjust = 0.5, x = 0.5
)

footnote <- grobTree(
  gp = gpar(fontface = 1, fontsize = 12, fontfamily='mono'), 
  textGrob(label = "En ", name = "title1",
           x = unit(0.2, "lines"), 
           hjust = 0, vjust = 0),
  textGrob(label = "rojo", name = "title2",
           x = grobWidth("title1") + unit(0.2, "lines"), 
           hjust = 0, vjust = 0, gp = gpar(col = "red")),
  textGrob(label = " los participantes que abandonaron la casa", name = "title3",
           x = grobWidth("title1") + grobWidth("title2") + unit(0.2, "lines"), 
           hjust = 0, vjust = 0)
)


caption = textGrob("@karbartolome",
  gp = gpar(fontface = 1, fontsize = 10, fontfamily='mono'),
  hjust = 1, x = 0.95
)

p_final <- grid.arrange(title,
                        subtitle,
                        p,
                        footnote,
                        caption,
                        heights = c(0.5, 0.5, 8, 0.8, 0.3))

# Save --------------------------------------------------------------------
ggsave2 <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
ggsave2("gh_2023_network.png", plot=p_final, width=8, height=8)
