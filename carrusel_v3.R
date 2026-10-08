# =============================================================================
# Carrusel de difusión — formato horizontal 1920 × 1080 px (16:9)
# Requiere ejecutar antes TF_CEMD_v3.Rmd, que genera results/objetos_carrusel.rds
# Ejecutar con el botón "Source" (Ctrl+Shift+S), NO con "Knit" ni "Compile Report":
# el script genera el PDF por sí mismo con cairo_pdf().
# =============================================================================

suppressMessages({
  library(tidyverse)
  library(sf)
  library(patchwork)
  library(gridExtra)
  library(grid)
})

# ----------------------------------------------------------------- parámetros
ENLACE <- "https://github.com/carloxmd/TF_CienciaDatos_IyII.git" 
AUTOR  <- "Carlos Morales Dávila · Arquitecto Urbanista"
F      <- "Lato"                         # cambiar a "sans" si Lato no está instalada
NAR <- "#D55E00"; GRIS <- "#6B7280"; OSC <- "#111827"; FONDO <- "#FFFFFF"; CLARO <- "#E5E7EB"

o <- readRDS("results/objetos_carrusel.rds")
sin <- o$siniestros; dist <- o$distritos; red <- o$red

# ----------------------------------------------------------------- utilidades
fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
envolver <- function(x, n) paste(strwrap(x, n), collapse = "\n")

# Lienzo con coordenadas exactas 0–1 (sin expansión de ejes)
lienzo <- function() {
  ggplot() +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void() +
    theme(plot.background = element_rect(fill = FONDO, color = NA),
          plot.margin = margin(0, 0, 0, 0))
}
txt <- function(p, x, y, label, size, col = OSC, face = "plain", hjust = 0, lh = 1.1) {
  p + annotate("text", x = x, y = y, label = label, size = size, colour = col,
               family = F, fontface = face, hjust = hjust, vjust = 1, lineheight = lh)
}
encabezado <- function(p, etiqueta, titulo, ancho = 30) {
  p <- txt(p, 0.05, 0.90, etiqueta, 6.5, NAR, "bold")
  txt(p, 0.05, 0.84, envolver(titulo, ancho), 10.5, OSC, "bold", lh = 1.0)
}
pie <- function(p, n, total) {
  p <- p + annotate("segment", x = 0.05, xend = 0.95, y = 0.075, yend = 0.075, colour = CLARO, linewidth = 0.4)
  p <- txt(p, 0.05, 0.055, AUTOR, 4.2, GRIS)
  txt(p, 0.95, 0.055, paste0(n, " / ", total), 4.2, GRIS, hjust = 1)
}
tema_graf <- theme_minimal(base_family = F, base_size = 17) +
  theme(plot.background = element_rect(fill = FONDO, color = NA),
        panel.background = element_rect(fill = FONDO, color = NA),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        axis.text = element_text(color = GRIS))
tema_mapa <- theme_void(base_family = F) +
  theme(plot.background = element_rect(fill = FONDO, color = NA),
        panel.background = element_rect(fill = FONDO, color = NA))

tabla <- function(df, tam = 15) {
  th <- ttheme_minimal(
    base_family = F, base_size = tam,
    core = list(fg_params = list(hjust = 0, x = 0.04, col = OSC),
                bg_params = list(fill = c(FONDO, "#F1F2F4"), col = NA)),
    colhead = list(fg_params = list(hjust = 0, x = 0.04, col = NAR, fontface = "bold"),
                   bg_params = list(fill = FONDO, col = NA))
  )
  g <- tableGrob(df, rows = NULL, theme = th)
  g <- gtable::gtable_add_grob(g, segmentsGrob(x0 = 0, x1 = 1, y0 = 0, y1 = 0, gp = gpar(col = NAR, lwd = 2)),
                               t = 1, l = 1, r = ncol(g))
  wrap_elements(full = g) + theme(plot.background = element_rect(fill = FONDO, color = NA))
}

# Encuadre urbano para mapas (núcleo donde se concentran los resultados)
encuadre <- function(obj, margen = 2500) {
  bb <- st_bbox(obj)
  list(x = c(bb["xmin"] - margen, bb["xmax"] + margen), y = c(bb["ymin"] - margen, bb["ymax"] + margen))
}

# ----------------------------------------------------------------- datos derivados
sin_df <- st_drop_geometry(sin)
es_pe1 <- str_detect(coalesce(sin_df$cod_carretera, ""), "^PE-1")
crit <- o$tramos %>% filter(critico_clase)
n_crit <- nrow(crit); km_crit <- sum(crit$longitud_m) / 1000; sin_crit <- sum(crit$siniestros)
n_crit_pe1 <- sum(str_detect(crit$nombre, "^PANAMERICANA (NORTE|SUR)$"))
km_red <- sum(as.numeric(st_length(red))) / 1000
robustas <- o$hex500 %>% filter(categoria == "Robusta (tres escalas)")
pct_rob <- 100 * mean(lengths(st_intersects(filter(sin, !is.na(distrito_geo)), robustas)) > 0)

nombre_via <- function(x) {
  str_to_title(x) %>% str_replace("^Central$", "Carretera Central") %>%
    str_replace("Tupac", "Túpac") %>% str_replace("Nestor", "Néstor")
}
etq_distrito <- function(x) {
  x %>% str_to_title() %>% str_replace_all(c(" De " = " de ", " Del " = " del ", " La " = " la ")) %>%
    str_replace_all(c("Lurin" = "Lurín", "Rimac" = "Rímac", "Martin" = "Martín", "Jesus" = "Jesús",
                      "Maria" = "María", "Ancon" = "Ancón", "Pachacamac" = "Pachacámac"))
}

TOTAL <- 12
sl <- list()

# 1 ---------------------------------------------------------------- Portada
m <- ggplot() +
  geom_sf(data = dist, fill = "#ECEDEF", color = "white", linewidth = 0.25) +
  geom_sf(data = filter(red, clase_via == "Expresa o nacional"), color = "#9CA3AF", linewidth = 0.3) +
  geom_sf(data = robustas, fill = NAR, color = NA) +
  coord_sf(datum = NA) + tema_mapa
p <- lienzo() + annotate("rect", xmin = 0.05, xmax = 0.11, ymin = 0.795, ymax = 0.802, fill = NAR)
p <- txt(p, 0.05, 0.75, "En Lima y Callao, las\nmuertes en las vías\nno aumentaron en\ntoda la ciudad.", 12, OSC, "bold", lh = 1.0)
p <- txt(p, 0.05, 0.40, "Aumentaron en un corredor.", 11, NAR, "bold")
p <- txt(p, 0.05, 0.30, "Análisis espacial y temporal de 1,658 siniestros\nde tránsito fatales registrados por el ONSV, 2021–2024", 5.6, GRIS, lh = 1.2)
sl[[1]] <- pie(p, 1, TOTAL) + inset_element(m, 0.50, 0.09, 0.97, 0.98, align_to = "full")

# 2 ---------------------------------------------------------------- Magnitud
p <- encabezado(lienzo(), "La magnitud", "Más de la mitad de las víctimas eran peatones", 60)
bloque <- function(p, x, num, desc, col) {
  p <- txt(p, x, 0.62, num, 26, col, "bold")
  txt(p, x, 0.38, desc, 6, GRIS, lh = 1.2)
}
p <- bloque(p, 0.05, "1,724", "personas fallecidas\nen 1,658 siniestros\n(2021–2024)", OSC)
p <- bloque(p, 0.38, "54%", "de las personas fallecidas\neran peatones", NAR)
p <- bloque(p, 0.68, "23%", "es la proporción de peatones\nentre las víctimas en el mundo\n(OPS, 2023)", GRIS)
sl[[2]] <- pie(p, 2, TOTAL)

# 3 ---------------------------------------------------------------- Serie metropolitana
a <- sin_df %>% count(anio)
g <- ggplot(a, aes(factor(anio), n)) + geom_col(fill = "#9CA3AF", width = 0.6) +
  geom_text(aes(label = n), vjust = -0.6, family = F, size = 6.5, color = OSC) +
  scale_y_continuous(limits = c(0, 520), breaks = NULL) + labs(x = NULL, y = NULL) + tema_graf
p <- encabezado(lienzo(), "La serie metropolitana", "+11% entre 2021 y 2024, pero no es concluyente", 26)
p <- txt(p, 0.05, 0.58, envolver("La variación no supera la fluctuación esperable en conteos anuales (prueba de Poisson, p ≈ 0.13).", 46), 6, GRIS, lh = 1.25)
p <- txt(p, 0.05, 0.42, envolver("Además, 2021 tuvo inmovilización nocturna y, durante meses, autos particulares prohibidos los domingos (D.S. 076-2021-PCM).", 46), 6, GRIS, lh = 1.25)
p <- txt(p, 0.05, 0.20, "La pregunta es: ¿dónde creció?", 6.2, OSC, "bold")
sl[[3]] <- pie(p, 3, TOTAL) + inset_element(g, 0.50, 0.12, 0.96, 0.92, align_to = "full")

# 4 ---------------------------------------------------------------- Panamericana vs resto
x <- sin_df %>% mutate(v = if_else(es_pe1, "Panamericana", "Resto de la red")) %>% count(anio, v)
g <- ggplot(x, aes(anio, n, color = v)) + geom_line(linewidth = 1.8) + geom_point(size = 4.5) +
  geom_text(aes(label = n), vjust = -1.1, family = F, size = 6, show.legend = FALSE) +
  scale_color_manual(values = c("Panamericana" = NAR, "Resto de la red" = GRIS), name = NULL) +
  scale_y_continuous(limits = c(0, 380), breaks = NULL) + scale_x_continuous(breaks = 2021:2024) +
  labs(x = NULL, y = NULL) + tema_graf + theme(legend.position = "top", legend.text = element_text(size = 17))
p <- encabezado(lienzo(), "El hallazgo central", "Todo el aumento ocurrió en la Panamericana", 26)
p <- txt(p, 0.05, 0.52, "Panamericana: 80 → 123 siniestros", 6.2, NAR, "bold")
p <- txt(p, 0.05, 0.46, "+54%, p ≈ 0.003", 5.6, GRIS)
p <- txt(p, 0.05, 0.38, "Resto de la red: 309 → 310", 6.2, GRIS, "bold")
p <- txt(p, 0.05, 0.30, envolver("Resultado confirmado con el código de vía del ONSV y con la localización de cada siniestro.", 44), 5.2, GRIS, lh = 1.25)
sl[[4]] <- pie(p, 4, TOTAL) + inset_element(g, 0.50, 0.12, 0.96, 0.92, align_to = "full")

# 5 ---------------------------------------------------------------- Ramales (tabla)
tr <- o$ramales %>%
  transmute(Ramal = str_remove(ramal, " \\(.*\\)"), `2021` = a2021, `2022` = a2022, `2023` = a2023, `2024` = a2024,
            `Variación` = variacion, `Valor p` = if_else(p < 0.001, "< 0.001", sprintf("%.2f", p)))
p <- encabezado(lienzo(), "Sobre todo, en el sur", "La Panamericana Sur duplicó con creces sus siniestros", 22)
p <- txt(p, 0.05, 0.52, envolver("El crecimiento metropolitano es, principalmente, un fenómeno del eje sur. La Panamericana Norte no muestra un cambio estadísticamente significativo.", 34), 5.6, GRIS, lh = 1.25)
p <- txt(p, 0.42, 0.33, "Siniestros fatales por año. Prueba exacta de Poisson, 2024 frente a 2021.", 4.4, GRIS)
sl[[5]] <- pie(p, 5, TOTAL) + inset_element(tabla(tr, 17), 0.40, 0.38, 0.97, 0.68, align_to = "full")

# 6 ---------------------------------------------------------------- Mapa de tramos críticos
e <- encuadre(crit, 4000)
m <- ggplot() +
  geom_sf(data = dist, fill = "#ECEDEF", color = "white", linewidth = 0.4) +
  geom_sf(data = red, color = "#C9CDD3", linewidth = 0.25) +
  geom_sf(data = st_buffer(crit, 350), fill = NAR, color = NAR) +
  coord_sf(xlim = e$x, ylim = e$y, datum = NA) + tema_mapa
p <- encabezado(lienzo(), "Dónde intervenir", paste(n_crit, "tramos de 500 m concentran el", paste0(fmt(100 * sin_crit / 1658, 1), "%"), "de los siniestros"), 26)
p <- txt(p, 0.05, 0.50, paste0(fmt(km_crit), " km"), 14, NAR, "bold")
p <- txt(p, 0.05, 0.40, paste0("de una red vial principal de ", fmt(km_red), " km"), 5.6, GRIS)
p <- txt(p, 0.05, 0.32, paste0(n_crit_pe1, " de los ", n_crit, " tramos están en la Panamericana"), 6, OSC, "bold")
p <- txt(p, 0.05, 0.25, envolver("Exceso significativo de siniestros frente a vías de su misma clase (Poisson, corrección FDR).", 46), 5, GRIS, lh = 1.25)
sl[[6]] <- pie(p, 6, TOTAL) + inset_element(m, 0.50, 0.09, 0.97, 0.97, align_to = "full")

# 7 ---------------------------------------------------------------- Tabla de tramos críticos
tt <- crit %>% st_drop_geometry() %>% arrange(desc(siniestros), desc(fallecidos)) %>% slice_head(n = 10) %>%
  transmute(Vía = nombre_via(nombre), Distrito = etq_distrito(distrito),
            Siniestros = siniestros, Fallecidos = fallecidos, `Peatones fallecidos` = peatones,
            Esperados = sprintf("%.1f", esperado_clase))
p <- encabezado(lienzo(), "Los tramos más críticos", "Diez tramos de 500 m con más siniestros que los esperados", 70)
p <- txt(p, 0.05, 0.135, "Esperados: siniestros que corresponderían al tramo según su longitud y su clase de vía. 2021–2024.", 4.4, GRIS)
sl[[7]] <- pie(p, 7, TOTAL) + inset_element(tabla(tt, 18), 0.04, 0.15, 0.96, 0.76, align_to = "full")

# 8 ---------------------------------------------------------------- Concentraciones multiescala
e <- encuadre(robustas, 3000)
m <- ggplot() +
  geom_sf(data = dist, fill = "#ECEDEF", color = "white", linewidth = 0.4) +
  geom_sf(data = filter(o$hex500, categoria == "Solo en algunas escalas"), fill = "#FCA5A5", color = NA) +
  geom_sf(data = robustas, fill = "#B91C1C", color = NA) +
  geom_sf(data = filter(red, clase_via == "Expresa o nacional"), color = "#4B5563", linewidth = 0.3) +
  coord_sf(xlim = e$x, ylim = e$y, datum = NA) + tema_mapa
te <- o$tabla_escalas %>% transmute(`Hexágono` = paste(fmt(`Tamaño del hexágono (m)`), "m"),
                                    `Celdas calientes` = fmt(`Celdas calientes`),
                                    `Siniestros en ellas` = fmt(`Siniestros en celdas calientes`))
p <- encabezado(lienzo(), "¿Depende de la escala?", "Solo cuenta lo que persiste en las tres escalas", 26)
p <- txt(p, 0.05, 0.31, envolver(paste0("Las zonas robustas (rojo oscuro) contienen el ", fmt(pct_rob, 1), "% de los siniestros y siguen la Panamericana Norte y la Vía de Evitamiento. Gi* con 9,999 permutaciones y corrección FDR."), 46), 4.8, GRIS, lh = 1.25)
sl[[8]] <- pie(p, 8, TOTAL) +
  inset_element(tabla(te, 15), 0.04, 0.34, 0.48, 0.60, align_to = "full") +
  inset_element(m, 0.50, 0.09, 0.97, 0.95, align_to = "full")

# 9 ---------------------------------------------------------------- Madrugada y fuga
perf <- sin_df %>% mutate(v = if_else(es_pe1, "Panamericana", "Resto de la red")) %>% group_by(v) %>%
  summarise(`Siniestros de madrugada (0–6 h)` = 100 * mean(periodo_dia == "Madrugada", na.rm = TRUE),
            `Siniestros con vehículo fugado` = 100 * mean(hay_vehiculo_fugado == "Sí"), .groups = "drop") %>%
  pivot_longer(-v) %>% mutate(name = fct_rev(factor(name)))
g <- ggplot(perf, aes(value, fct_rev(v), fill = v)) + geom_col(width = 0.65) +
  geom_text(aes(label = paste0(fmt(value), "%")), hjust = -0.15, family = F, size = 6.5, color = OSC) +
  facet_wrap(~name, ncol = 1) +
  scale_fill_manual(values = c("Panamericana" = NAR, "Resto de la red" = "#9CA3AF"), guide = "none") +
  scale_x_continuous(limits = c(0, 45), breaks = NULL) + labs(x = NULL, y = NULL) + tema_graf +
  theme(strip.text = element_text(hjust = 0, face = "bold", size = 17, color = OSC),
        panel.grid.major = element_blank(), axis.text.y = element_text(size = 16, color = OSC))
pct_madr_pe1 <- 100 * mean(es_pe1[sin_df$periodo_dia == "Madrugada" & !is.na(sin_df$periodo_dia)])
p <- encabezado(lienzo(), "Cuándo y cómo", "En la Panamericana, la madrugada pesa casi el doble", 26)
p <- txt(p, 0.05, 0.50, paste0(fmt(pct_madr_pe1), "%"), 14, NAR, "bold")
p <- txt(p, 0.05, 0.40, envolver("de todos los siniestros de madrugada del área metropolitana ocurrió en esta vía.", 44), 5.6, GRIS, lh = 1.25)
p <- txt(p, 0.05, 0.27, envolver("La fuga del vehículo, también duplicada, limita la información sobre los responsables.", 44), 5.2, GRIS, lh = 1.25)
sl[[9]] <- pie(p, 9, TOTAL) + inset_element(g, 0.50, 0.12, 0.96, 0.92, align_to = "full")

# 10 --------------------------------------------------------------- Mapas de tasas
mapa_tasa <- function(var, titulo) {
  ggplot(o$tasas) + geom_sf(aes(fill = .data[[var]]), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(option = "magma", direction = -1, name = NULL) +
    labs(title = titulo) + coord_sf(datum = NA) + tema_mapa +
    theme(plot.title = element_text(family = F, face = "bold", size = 18, hjust = 0.5, color = OSC),
          legend.position = "right", legend.text = element_text(family = F, size = 13, color = GRIS),
          legend.key.height = unit(1.2, "cm"))
}
p <- encabezado(lienzo(), "¿Dónde es más peligroso?", "Depende de cómo se mida", 60)
p <- txt(p, 0.05, 0.135, "Tasas bayesianas empíricas por distrito, 2021–2024. Población: INEI, Censos Nacionales 2025.", 4.4, GRIS)
sl[[10]] <- pie(p, 10, TOTAL) +
  inset_element(mapa_tasa("tasa_pob_eb", "Fallecidos por 100 mil habitantes al año"), 0.03, 0.15, 0.50, 0.76, align_to = "full") +
  inset_element(mapa_tasa("tasa_km_eb", "Fallecidos por 100 km de vía al año"), 0.50, 0.15, 0.97, 0.76, align_to = "full")

# 11 --------------------------------------------------------------- Tabla de tasas
top <- function(var) o$tasas %>% st_drop_geometry() %>% arrange(desc(.data[[var]])) %>% slice_head(n = 6) %>%
  transmute(d = etq_distrito(distrito_geo), t = sprintf("%.1f", .data[[var]]))
t1 <- top("tasa_pob_eb"); t2 <- top("tasa_km_eb")
tt <- tibble(`#` = 1:6, `Por habitante` = t1$d, `Tasa` = t1$t, `Por km de vía` = t2$d, `Tasa ` = t2$t)
p <- encabezado(lienzo(), "Dos problemas distintos", "Corredores de paso y redes urbanas densas", 60)
p <- txt(p, 0.05, 0.70, envolver("Por habitante destacan distritos atravesados por la Panamericana, donde la carga la pone quien circula, no quien reside.", 52), 5.2, GRIS, lh = 1.25)
p <- txt(p, 0.05, 0.47, envolver("Por kilómetro de vía destacan distritos densos con redes cortas e intensamente usadas.", 52), 5.2, GRIS, lh = 1.25)
p <- txt(p, 0.05, 0.30, envolver("Cada tipo de territorio necesita una estrategia diferente.", 52), 6, OSC, "bold", lh = 1.2)
sl[[11]] <- pie(p, 11, TOTAL) + inset_element(tabla(tt, 21), 0.47, 0.18, 0.98, 0.74, align_to = "full")

# 12 --------------------------------------------------------------- Método y cierre
p <- encabezado(lienzo(), "Cómo se hizo", "Datos abiertos y método reproducible", 60)
izq <- paste(
  "• ONSV: siniestros, personas y vehículos",
  "• INEI: Censos Nacionales 2025 y límites",
  "• IMP y Municipalidad del Callao: red vial", sep = "\n")
der <- paste(
  "• Pruebas de Poisson y chi cuadrado",
  "• Tramos viales de 500 m",
  "• Gi* en hexágonos de 250, 500 y 1000 m (FDR)",
  "• Tasas bayesianas por habitante y por km", sep = "\n")
p <- txt(p, 0.05, 0.68, "Fuentes", 6.2, NAR, "bold")
p <- txt(p, 0.52, 0.68, "Métodos", 6.2, NAR, "bold")
p <- txt(p, 0.05, 0.61, izq, 5.6, OSC, lh = 1.45)
p <- txt(p, 0.52, 0.61, der, 5.6, OSC, lh = 1.45)
p <- txt(p, 0.05, 0.33, envolver("Limitaciones: estudio descriptivo, no causal; datos preliminares; sin información de flujos vehiculares.", 100), 5, GRIS)
p <- p + annotate("rect", xmin = 0.05, xmax = 0.95, ymin = 0.13, ymax = 0.25, fill = "#FDF0E8")
p <- txt(p, 0.07, 0.215, paste("Informe completo, código y datos:", ENLACE), 6.4, NAR, "bold")
sl[[12]] <- pie(p, 12, TOTAL)

# ----------------------------------------------------------------- exportar
cairo_pdf("carrusel.pdf", width = 1920 / 150, height = 1080 / 150, onefile = TRUE, family = F)
# Todas las láminas pasan por patchwork con margen exterior cero, para que
# las que no tienen mapas ni tablas insertados (2 y 12) tengan el mismo encuadre
uniformar <- function(x) {
  x + plot_annotation(theme = theme(
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = FONDO, color = NA)
  ))
}
for (k in seq_along(sl)) print(uniformar(sl[[k]]))
invisible(dev.off())
message("Carrusel generado: ", length(sl), " láminas")
