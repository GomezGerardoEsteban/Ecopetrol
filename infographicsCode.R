


rm(list = ls())

library(tidyverse)
library(glue)
library(ggtext)
library(patchwork)

owd <- read.csv("owid-energy-data.csv")

paises <- c("Colombia",
            "World")

consumoElect <- paises %>% map(.f = ~{
  
  owd %>%  
    select(country, year, iso_code, population, gdp, electricity_demand) %>% 
    filter(country == .x) %>% 
    mutate(perCapita = (electricity_demand*1000000/population))
  
})

consumoElect <- consumoElect %>% bind_rows()

consumoElect <- consumoElect %>% 
  filter(!is.na(perCapita))

consumoElect <- consumoElect %>% 
  group_by(country) %>% 
  mutate(mediana = median(perCapita),
         media = mean(perCapita))

consumoElect <- consumoElect %>% 
  mutate(iso_code = ifelse(iso_code == "", "World", iso_code),
         relleno = ifelse(iso_code == "COL", 1,
                          ifelse(iso_code == "World", 2, 0)))

graph5 <- consumoElect %>% 
  ggplot(mapping = aes(x = reorder(iso_code, mediana), y = perCapita, fill = as.factor(relleno))) +
  geom_boxplot(show.legend = F) +
  scale_fill_manual(values = c("#FFC300", "#900C3F")) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Demanda de electricidad",
       subtitle = NULL,
       y = "Megavatios-Hora (MWh) por habitante",
       x = NULL,
       caption = NULL) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

CO2Emissions <- paises %>% map(.f = ~{
  
  owd %>%  
    select(country, year, iso_code, population, gdp, greenhouse_gas_emissions) %>% 
    filter(country == .x) %>% 
    mutate(perCapita = (greenhouse_gas_emissions*1000000/population))
  
})


CO2Emissions <- CO2Emissions %>% bind_rows()

CO2Emissions <- CO2Emissions %>% 
  filter(!is.na(perCapita))

CO2Emissions <- CO2Emissions %>% 
  group_by(country) %>% 
  mutate(mediana = median(perCapita),
         media = mean(perCapita))

CO2Emissions <- CO2Emissions %>% 
  mutate(iso_code = ifelse(iso_code == "", "World", iso_code),
         relleno = ifelse(iso_code == "COL", 1,
                          ifelse(iso_code == "World", 2, 0)))

graph6 <- CO2Emissions %>% 
  ggplot(mapping = aes(x = reorder(iso_code, mediana), y = perCapita, fill = as.factor(relleno))) +
  geom_boxplot(show.legend = F) +
  scale_fill_manual(values = c("#FFC300", "#900C3F")) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Emisiones de CO2",
       subtitle = NULL,
       y = "Toneladas de CO2 equivalente por habitante",
       x = NULL,
       caption = NULL) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

Colombia <- owd %>% filter(country == "Colombia" | country == "World") %>% 
  select(country, year, iso_code, ends_with("electricity"))

Colombia <- Colombia %>% 
  gather(key = tipo, value = valor, 4:length(Colombia))

source_elec <- c("biofuel_electricity",
                 "hydro_electricity",
                 "coal_electricity",
                 "gas_electricity",
                 "oil_electricity",
                 "wind_electricity",
                 "solar_electricity",
                 "other_renewable_exc_biofuel_electricity")

map1 <- source_elec %>% 
  map(.f = ~{
    
    Colombia %>% 
      filter(tipo == .x,
             year > 1989)
  })


map1 <- map1 %>% bind_rows()

map1 <- map1 %>% 
  mutate(renovable = case_when(
    tipo == "biofuel_electricity" ~ 1,
    tipo == "hydro_electricity" ~ 2, 
    tipo == "solar_electricity" ~ 1, 
    tipo == "wind_electricity" ~ 1,
    tipo == "other_renewable_exc_biofuel_electricity" ~ 1,
    tipo == "coal_electricity" ~ 0,
    tipo == "gas_electricity" ~ 0,
    tipo == "oil_electricity" ~ 0),
    nombre = case_when(
      tipo == "biofuel_electricity" ~ "Bioenergía",
      tipo == "hydro_electricity" ~ "Hidroeléct.", 
      tipo == "solar_electricity" ~ "Solar", 
      tipo == "wind_electricity" ~ "Eólica", 
      tipo == "coal_electricity" ~ "Carbón",
      tipo == "gas_electricity" ~ "Gas",
      tipo == "oil_electricity" ~ "Oil",
      tipo == "other_renewable_exc_biofuel_electricity" ~ "Otras Renov."))

map1 <- map1 %>% 
  mutate(nombre = factor(nombre, levels = c("Carbón", "Oil", "Gas", 
                                            "Bioenergía", "Solar", "Eólica", "Otras Renov.",
                                            "Hidroeléct.")),
         renovable = factor(renovable))

alpha_max <- 1
alpha_min <- 0.6
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), 
  seq(alpha_max, alpha_min, length.out = 3),
  alpha_max)

anotacion <- glue::glue(
  "En Colombia, la participación de <span style = 'color:#249206;'>renovables</span> <br>distintas a la <span style = 'color:#3667A6;'>hidroeléctrica</span> en 2022 fue<br>menor al <span style = 'color:#249206;'>2 por ciento</span>."
)

graph1 <- map1 %>% 
  filter(year > 1999 & nombre != "Otras Renov." & country == "Colombia") %>% 
  ggplot(mapping = aes(x = year, y = valor, fill = renovable, alpha = nombre)) +
  geom_area(col = "white") +
  scale_alpha_manual(values = alpha_vals) +
  scale_fill_manual(values = c("#000000", "#249206", "#3667A6")) +
  guides(
    fill = guide_none(),
    alpha = guide_legend(override.aes = list(fill = c(rep("#283227", 3),
                                                      rep("#249206", 3),
                                                      "#3667A6")))
  ) +
  annotate(geom = "richtext",
           x = 2005,
           y = 75,
           label = anotacion,
           size = 2.5,
           label.color = NA
  ) +
  scale_x_continuous(breaks = 2000:2022) +
  scale_y_continuous(n.breaks = 10) +
  labs(title = "Electricidad por fuente primaria en Colombia",
       subtitle = "Electricidad medida en Teravatios-Hora (TWh)",
       x = NULL,
       y = "Teravatios-Hora",
       caption = "Fuente: Elaboración propia en base Our World in Data (OWD)") +
  
  theme_test() +
  theme(plot.title = element_text(hjust = 0.0, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 45),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


map2 <- map1 %>% 
  filter(year == 2022) %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(prop = round(valor/sum(valor)*100,2)) %>% 
  ungroup() %>% 
  mutate(etiqueta = paste(nombre, prop, "%"),
         etiquetas = ifelse(country == "Colombia" & renovable == 1, NA,
                            ifelse(nombre == "Otras Renov.", NA, etiqueta)))


alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), 
  seq(alpha_max, alpha_min, length.out = 4),
  alpha_max)

graph2 <- map2 %>% 
  ggplot(mapping = aes(x = as.factor(country), y = prop, fill = renovable, alpha = nombre)) +
  geom_col(col = "white", width = 1) +
  scale_alpha_manual(values = alpha_vals) +
  scale_fill_manual(values = c("#000000", "#249206", "#3667A6")) +
  guides(
    fill = guide_none(),
    alpha = guide_legend(override.aes = list(fill = c(rep("#283227", 3),
                                                      rep("#249206", 4),
                                                      "#3667A6")))
  ) +
  geom_text(
    data = map2,
    aes(label = etiquetas),
    position = position_stack(vjust = 0.70),
    col = 'white',
    size = 2,
    fontface = 'bold'
  ) +
  labs(title = "Participación porcentual por fuente",
       subtitle = NULL,
       x = NULL,
       y = "Porcentaje") +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.0, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

Comb <- graph2 + graph5 + graph6

tib_summary_text <- tibble(
  x = 0, 
  y = c(1.8, 1.5, 1.2, 0.2), 
  label = c("<span style='color:#FFC300'>Colombia</span><span style='color:#5C6A5E'>, cuenta con una<br>matriz eléctrica relativamente<br>limpia</span>.",
            "<span style='color:#5C6A5E'>Consume un</span> 55%, <span style='color:#5C6A5E'>menos de<br>electricidad que el promedio</span> <br> <span style='color:#900C3F'>mundial</span> <span style='color:#5C6A5E'>por habitante</span>.",
            "<span style='color:#5C6A5E'>Emite un</span> 83.5% <span style='color:#5C6A5E'>menos de CO2<br>equivalente para la<br>generación de electricidad</span>.",
            "<span style='color:#5C6A5E'>Sin embargo, sus avances hacia<br>la</span> <span style='color:#249206'>Transición Energética</span> <span style='color:#5C6A5E'>en el<br>sector, deben producirse con<br>el ánimo de reducir la<br>exposición a sequías, a<br>partir de una</span> <span style='color:#249206'>**diversificación<br>en renovables**</span><span style='color:#5C6A5E'>, que reduzca la<br>importancia de las fuentes<br></span> <span style='color:#3667A6'>Hidroeléctricas</span>."
  )
)


text_plot <- tib_summary_text %>% 
  ggplot() +
  geom_richtext(
    aes(x, y, label = label),
    size = 3,
    hjust = 0,
    vjust = 0,
    label.colour = NA
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2), clip = 'off') +
  # clip = 'off' is important for putting it together later.
  theme_void()

Comb <- Comb +
  labs(caption = "Fuente: elaboración propia en base a Our World in Data (OWD)") +
  theme(plot.caption = element_text(size=7, hjust=0.0, face="italic", color="black"))

infograph1 <- Comb +
  text_plot +
  # Make text plot narrower
  plot_layout(widths = c(0.3, 0.2, 0.2, 0.3))

infograph1 

###########


crecimientoFuentes <- source_elec %>% map(.f = ~{
  
  p <- Colombia %>% 
    filter(tipo == .x & country == "Colombia" & !is.na(valor) & year > 1999) %>% 
    mutate(lag1 = lag(valor), 
           tasa = (valor/lag1 - 1)*100)
  
  p <- p[(dim(p)[1]-3):dim(p)[1], ]
  
  p <- p %>% 
    mutate(indice = valor/valor[1]*100)
})

crecimientoFuentes <- crecimientoFuentes %>% bind_rows()

crecimientoFuentes <- crecimientoFuentes %>% 
  mutate(renovable = case_when(
    tipo == "biofuel_electricity" ~ 1,
    tipo == "hydro_electricity" ~ 2, 
    tipo == "solar_electricity" ~ 1, 
    tipo == "wind_electricity" ~ 1,
    tipo == "other_renewable_exc_biofuel_electricity" ~ 1,
    tipo == "coal_electricity" ~ 0,
    tipo == "gas_electricity" ~ 0,
    tipo == "oil_electricity" ~ 0),
    nombre = case_when(
      tipo == "biofuel_electricity" ~ "Bioenergía",
      tipo == "hydro_electricity" ~ "Hidroeléct.", 
      tipo == "solar_electricity" ~ "Solar", 
      tipo == "wind_electricity" ~ "Eólica", 
      tipo == "coal_electricity" ~ "Carbón",
      tipo == "gas_electricity" ~ "Gas",
      tipo == "oil_electricity" ~ "Oil",
      tipo == "other_renewable_exc_biofuel_electricity" ~ "Otras Renov."))

crecimientoFuentes <- crecimientoFuentes %>% 
  mutate(nombre = factor(nombre, levels = c("Carbón", "Oil", "Gas", 
                                            "Bioenergía", "Solar", "Eólica", "Otras Renov.",
                                            "Hidroeléct.")))

crecimientoFuentes <- crecimientoFuentes %>% 
  mutate(logs = log(indice))


vec_nombres <- crecimientoFuentes$nombre

crecimientos <- vec_nombres %>% 
  map(.f = ~{
    
    p <- crecimientoFuentes %>% 
      ungroup() %>% 
      filter(nombre == .x) %>% 
      summarise(crecimiento = (indice[4] / indice[1] - 1)*100,
                nombre = .x)
    
  })

crecimientos <- crecimientos %>% bind_rows()

crecimientos <- crecimientos %>% 
  distinct()

crecimientoFuentes <- crecimientoFuentes %>% 
  left_join(y = crecimientos,
            by = c("nombre" = "nombre"))

crecimientoFuentes <- crecimientoFuentes %>% 
  mutate(crecimiento = round(crecimiento, 1),
         etiqueta = ifelse(crecimiento > 0, paste(nombre, "+", crecimiento, "%"),
                           paste(nombre, crecimiento, "%")))

etiquetas <- unique(crecimientoFuentes$etiqueta)

alpha_max <- 1
alpha_min <- 0.6
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3), alpha_max)

graph3 <- crecimientoFuentes %>% 
  filter(tipo != "other_renewable_exc_biofuel_electricity" & 
           renovable == 1 | renovable == 2) %>% 
  ggplot(mapping = aes(x = year, 
                       y = indice, 
                       col = as.factor(renovable), 
                       alpha = nombre,
                       linetype = nombre)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.5) +
  scale_alpha_manual(values = alpha_vals) +
  scale_color_manual(values = c("#249206", "#3667A6")) +
  scale_linetype_manual(values = c("longdash", "dotdash", rep("solid", 2))) +
  labs(title = "Crecimiento Renovables",
       subtitle = NULL, 
       y = "Índice generación (2019 = 100)",
       x = NULL,
       caption = NULL) +
  annotate(geom = "text",
           x = c(2020, 2021, 2021.2, 2020.5),
           y = c(75, 35, 140, 220),
           col = c("#3667A6", "#249206", "#249206","#249206"),
           alpha = c(1, 0.6, 1, 0.8),
           label = c(etiquetas[2], etiquetas[6], etiquetas[1], etiquetas[7]),
           size = c(rep(3,4))) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.0, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")



alpha_max <- 1
alpha_min <- 0.3
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 3))

graph4 <- crecimientoFuentes %>% 
  filter(renovable == 0) %>% 
  ggplot(mapping = aes(x = year, 
                       y = indice, 
                       col = as.factor(renovable), 
                       alpha = nombre,
                       linetype = nombre)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.5) +
  scale_alpha_manual(values = alpha_vals) +
  scale_color_manual(values = c("#000000")) +
  scale_linetype_manual(values = c("longdash", "dotdash", "solid")) +
  labs(title = "Crecimiento No Renovables",
       subtitle = NULL,
       y = NULL,
       x = NULL,
       caption = NULL) +
  annotate(geom = "text",
           x = c(2020, rep(2021.5,2)),
           y = c(90, 85, 97),
           col = c("#000000"),
           alpha = c(0.65, 1, 0.40),
           label = c(etiquetas[5], etiquetas[3], etiquetas[4]),
           size = 3) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.0, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

fotovoltaica <- read.csv2("fotovoltaica.csv")

# Capacidad efectiva neta solar = 278.66
# Capacidad efectiva neta eolica = 18.42

anotacion1 <- glue::glue("Capacidad Efectiva Neta <span style = 'color:#008B0F;'>278.66</span> MW <br> Factor de Planta <span style = 'color:#008B0F;'>0.250</span>")

anotacion2 <- glue::glue("Capacidad Efectiva Neta <span style = 'color:#5A00C1;'>18.42</span> MW <br> Factor de Planta <span style = 'color:#5A00c1;'>0.208</span>")

#008B0F
#C1B500

inestabilidad <- fotovoltaica %>% 
  ggplot(mapping = aes(x = Hora)) +
  geom_line(aes(y = Media_Solar), col = "#008B0F", linewidth = 1) +
  geom_line(aes(y = Media_Eolica*20), col = "#5A00C1", linewidth = 1) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~./20, name = "MW - Eólica",
                                         breaks = 
                                           seq(from = 0, to = 10, by = 1)), n.breaks = 10) +
  scale_x_continuous(breaks = 1:24) +
  labs(title = "Comportamiento medio por hora de plantas solares y eólicas en operación durante 2022 en Colombia",
       y = "MW - Solar",
       x = "Hora del día",
       caption = "Fuente: elaboración propia en base a OWD y XM") +
  annotate(geom = "richtext",
           x = c(4, 21),
           y = c(150, 150),
           label = c(anotacion1, anotacion2),
           size = 3) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.5, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 8, color = "#008B0F"),
        axis.title.y.right = element_text(size = 8, color = "#5A00C1"),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


tib_summary_text2 <- tibble(
  x = 0, 
  y = c(0.1), 
  label = c("<span style='color:#5C6A5E'>La energía solar <span style='color:#249206'>**(+ 276.9%)**</span> y la<br>energía eólica <span style='color:#249206'>**(+ 16.7%)**</span> han sido<br>las de mayor crecimiento</span> <span style='color:#5C6A5E'>en los<br>ultimos años, si bien es un logro<br>importante, hay que tener cuidado<br>con los efectos que estas fuentes<br>pueden tener</span> <span style='color:#5C6A5E'>en el sistema<br>eléctrico, debido a su <span style='color:#B30A02'>inestabilidad</span>.</span>"
  )
)


text_plot2 <- tib_summary_text2 %>% 
  ggplot() +
  geom_richtext(
    aes(x, y, label = label),
    size = 3,
    hjust = 0,
    vjust = 0,
    label.colour = NA
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2), clip = 'off') +
  # clip = 'off' is important for putting it together later.
  theme_void()


infograph2 <- (graph3 + text_plot2 + graph4) / inestabilidad

infograph2 
  
##########

tib_summary_text3 <- tibble(
  x = 0, 
  y = c(1.4, 1, 0.3), 
  label = c("<span style='color:#5C6A5E'>Es por ello que la <span style='color:#FFC300'>Transición</span> <br> <span style='color:#473CF1'>Energética</span> <span style='color:#C91818'>Justa</span> (TEJ), debiera<br>promover a través de incentivos<br>diferenciados, el desarrollo de<br>energias con mayor estabilidad<br>como la <span style='color:#97600A'>geotermia</span> y la <span style='color:#97600A'>biomasa</span>.</span>",
            "<span style='color:#5C6A5E'><span style='color:#97600A'>Estas fuentes</span> enfrentan<br>mayores costos de entrada, pero<br>no dependen del clima para<br> la generación de eléctricidad.</span>",
            "<span style='color:#5C6A5E'>Con esos desarrollos, se<br>aportaria a la transición y<br>seguridad energética del pais,<br>teniendo en cuenta que el proceso<br>de mejora o reemplazo de energias<br> convencionales, no puede en<br>ningún momento, comprometer el<br>suministro de servicios energéticos</span>."
  )
)


text_plot3 <- tib_summary_text3 %>% 
  ggplot() +
  geom_richtext(
    aes(x, y, label = label),
    size = 3,
    hjust = 0,
    vjust = 0,
    label.colour = NA
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 2), clip = 'off') +
  # clip = 'off' is important for putting it together later.
  theme_void()



mejor <- tibble(
  "Costos de Instalación" = c(2353, 3991, 857, 1325),
  "Factor de Capacidad" = c(0.68, 0.77, 0.17, 0.39),
  "Costo de kWh" = c(0.067, 0.068, 0.048, 0.033),
  Fuente = c("Bioenergía", "Geotermia", "Solar FV", "Eólica Terr.")
)


mejor <- mejor %>% 
  gather(key = variable, value = valor, 1:3)

mejor <- mejor %>% 
  mutate(Fuente = factor(Fuente, levels = c("Geotermia", "Bioenergía", "Solar FV", "Eólica Terr.")),
         factorP = case_when(Fuente == "Geotermia" | Fuente == "Bioenergía" ~ 1,
                             Fuente == "Solar FV" | Fuente == "Eólica Terr." ~ 0))

alpha_max <- 1
alpha_min <- 0.7
alpha_vals <- c(
  seq(alpha_max, alpha_min, length.out = 2),
  seq(alpha_max, alpha_min, length.out = 2))

g1 <- mejor %>% 
  filter(variable == "Factor de Capacidad") %>% 
  ggplot(mapping = aes(x = valor, y = Fuente, fill = as.factor(factorP), alpha = Fuente)) +
  scale_fill_manual(values = c("#0A9713","#97600A")) +
  scale_alpha_manual(values = alpha_vals) +
  scale_y_discrete(limits = rev(levels(mejor$Fuente)), position = "left") +
  geom_col(show.legend = F) +
  geom_text(aes(label = valor), position = position_nudge(x = 0.03), show.legend = F, size = 3) +
  geom_text(aes(label = Fuente), position = position_stack(vjust = 0.5), show.legend = F, size = 3, color = "black") +
  scale_x_continuous(position = "top", n.breaks = 10) +
  labs(x = "Factor de planta",
       y = NULL) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.0, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")

g2 <- mejor %>% 
  filter(variable == "Costos de Instalación") %>% 
  ggplot(mapping = aes(x = valor, y = Fuente, fill = as.factor(factorP), alpha = Fuente)) +
  scale_fill_manual(values = c("#0A9713","#97600A")) +
  scale_alpha_manual(values = alpha_vals) +
  scale_y_discrete(limits = rev(levels(mejor$Fuente)), position = "left") +
  geom_col(show.legend = F) +
  geom_text(aes(label = valor), position = position_nudge(x = 125), show.legend = F, size = 3) +
  geom_text(aes(label = Fuente), position = position_stack(vjust = 0.5), show.legend = F, size = 3, color = "black") +
  scale_x_continuous(position = "top", n.breaks = 10) +
  labs(x = "Costo total de instalación por kWh (USD/kWh)",
       y = NULL) +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.0, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=6, hjust=1, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


g3 <- mejor %>% 
  filter(variable == "Costo de kWh") %>% 
  ggplot(mapping = aes(x = valor, y = Fuente, fill = as.factor(factorP), alpha = Fuente)) +
  scale_fill_manual(values = c("#0A9713","#97600A")) +
  scale_alpha_manual(values = alpha_vals) +
  scale_y_discrete(limits = rev(levels(mejor$Fuente)), position = "left") +
  geom_col(show.legend = F) +
  geom_text(aes(label = valor), position = position_nudge(x = 0.003), show.legend = F, size = 3) +
  geom_text(aes(label = Fuente), position = position_stack(vjust = 0.5), show.legend = F, size = 3, color = "black") +
  scale_x_continuous(position = "top", n.breaks = 10) +
  labs(x = "Costo nivelado de la electricidad (USD/kWh)",
       y = NULL,
       caption = "Fuente: elaboración propia en base a IRENA 2021") +
  theme_test() +
  theme(plot.title = element_text(hjust = 0.0, size = 8),
        plot.subtitle = element_text(hjust = 0.0, size = 7),
        plot.caption = element_text(size=7, hjust=0.0, face="italic", color="black"),
        axis.text.x = element_text(size = 6, angle = 0),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.title.x = element_text(size = 7),
        legend.title = element_text(size=7, hjust = 0.5), 
        legend.text = element_text(size=6),
        legend.position = "none")


Comb3 <- g1 / g2 / g3

infograph3 <- text_plot3 +
  Comb3 +
  # Make text plot narrower
  plot_layout(widths = c(0.3, 0.7))

infograph3


info <- infograph1 / infograph2 / infograph3


ggsave(filename = "info.png",
       plot = info,
       width = 21,
       height = 40,
       units = "cm",
       dpi = 500)

