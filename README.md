# Siniestros de tránsito fatales en Lima y Callao, 2021–2024

Análisis espacial y temporal de los 1,658 siniestros de tránsito fatales registrados por el Observatorio Nacional de Seguridad Vial (ONSV) en las provincias de Lima y Callao entre 2021 y 2024, en los que fallecieron 1,724 personas.

Elaborado sobre la base del trabajo final de los módulos de Ciencia de Datos I y II del Posgrado en Big Data e Inteligencia Territorial – Cohorte 7 – Facultad Latinoamericana de Ciencias Sociales - FLACSO Argentina.

Publicado en GitHub el 8 de Octubre de 2026.

**Autor:** MSc. Arq. Carlos Morales Dávila

------------------------------------------------------------------------

## Principales hallazgos

- **Todo el aumento ocurrió en la Panamericana.** Los siniestros fatales en la Panamericana Norte y Sur pasaron de 80 en 2021 a 123 en 2024 (+54%, p ≈ 0.003). En el resto de la red vial se mantuvieron prácticamente constantes: 309 y 310.
- **Sobre todo, en el sur.** La Panamericana Sur pasó de 33 a 71 siniestros (p \< 0.001). La Panamericana Norte no muestra un cambio significativo.
- **Pocos tramos concentran el problema.** De los 5,959 tramos de unos 500 m en que se dividió la red vial principal, 22 presentan un exceso significativo de siniestros; 17 de ellos están en la Panamericana.
- **Los peatones son la mayoría de las víctimas.** El 54.1% de las personas fallecidas eran peatones.
- **La madrugada pesa casi el doble en la Panamericana.** El 34% de sus siniestros ocurrió de madrugada, frente al 18% en el resto de la red, y la vía concentra el 39% de todos los siniestros de madrugada del área metropolitana.
- **El "dónde" depende de cómo se mida.** Por habitante destacan los distritos atravesados por la Panamericana; por kilómetro de vía, los distritos urbanos densos.
- **La serie metropolitana no muestra una reducción**, aunque su variación entre 2021 y 2024 (+11.3%) no es estadísticamente concluyente.

El estudio es descriptivo y exploratorio: identifica patrones, pero no establece relaciones causales.

## Contenido del repositorio

| Archivo | Descripción |
|------------------------------------|------------------------------------|
| `TF_CEMD_v3.Rmd` | Documento fuente: texto, código en R y cálculos de todas las cifras del informe. |
| `index.html` | Informe compilado. El código de cada etapa puede desplegarse desde el propio documento. |
| `carrusel.R` | Script que genera el carrusel de difusión (12 láminas, 1920 × 1080 px) a partir de los resultados del informe. |
| `data/` | Datos de entrada empleados en la investigación (ver más abajo). |
| `results/` | Productos generados al compilar el informe: base maestra (CSV y GeoPackage) y objetos para el carrusel. |
| `figures/` | Cada gráfico y mapa estático del informe en PNG, generado al compilar. |

El informe puede consultarse en línea en <https://carloxmd.github.io/siniestralidad_vial/>, o descargando `index.html` y abriéndolo en un navegador.

## Métodos

- **Integración de bases:** siniestros, personas y vehículos del ONSV, consolidados en una base maestra de un registro por siniestro y validada espacialmente.
- **Análisis temporal:** prueba exacta y regresión de Poisson para la evolución anual; pruebas chi cuadrado para la relación entre día de la semana y período del día.
- **Tramos de la red vial:** tramos de unos 500 m con exceso significativo de siniestros frente a vías de su misma clase (Poisson, corrección FDR).
- **Concentraciones espaciales:** estadístico Gi\* en mallas hexagonales de 250, 500 y 1000 m, con 9,999 permutaciones y corrección FDR. Solo se consideran robustas las concentraciones que persisten en las tres escalas.
- **Tasas distritales:** fallecidos por habitante y por kilómetro de red vial principal, suavizadas con el estimador bayesiano empírico.

## Fuentes de datos

Los datos de entrada empleados en la investigación se incluyen en la carpeta `data/`, tal como fueron descargados de cada fuente:

| Fuente | Archivo en `data/` |
|------------------------------------|------------------------------------|
| ONSV – Siniestros fatales 2021–2025 (preliminar) | `BBDD ONSV - SINIESTROS FATALES 2021-2025 (preliminar).xlsx` |
| ONSV – Personas involucradas 2021–2025 (preliminar) | `BBDD ONSV - PERSONAS 2021-2025 (preliminar).xlsx` |
| ONSV – Vehículos involucrados 2021–2025 (preliminar) | `BBDD ONSV - VEHICULOS 2021-2025 (preliminar).xlsx` |
| INEI – Límites distritales 2023 | `limite_distrital_2023.gpkg` |
| IMP – Sistema Vial Metropolitano | `red_vial_lima.gpkg` |
| MPC – Red vial y calles urbanas del PDM Callao 2040 | `red_vial_callao.gpkg` |
| INEI – Censos Nacionales 2025, población distrital de Lima | `poblacion_censo2025_lima.xlsx` |
| INEI – Censos Nacionales 2025, población distrital del Callao | `poblacion_censo2025_callao.xlsx` |

Las versiones vigentes de las bases del ONSV pueden consultarse en <https://www.onsv.gob.pe/datosabiertos> y las de los límites distritales del INEI en <https://ide.inei.gob.pe/#capas>. Como las bases del ONSV son preliminares y se actualizan, una descarga posterior puede no coincidir con los archivos de `data/`; para reproducir los resultados del informe, usa los archivos del repositorio.

## Cómo reproducir el análisis

1.  **Clona el repositorio.** Los datos de entrada ya están en `data/`. Crea una carpeta `results/` vacía, si no existe.

2.  **Instala los paquetes** necesarios en R:

    ``` r
    install.packages(c(
      "tidyverse", "sf", "lwgeom", "spdep", "leaflet", "viridis", "patchwork",
      "readxl", "writexl", "janitor", "stringi", "knitr", "rmarkdown",
      "htmltools", "htmlwidgets"
    ))
    ```

3.  **Compila el informe** desde la raíz del repositorio:

    ``` r
    rmarkdown::render("TF_CEMD_v3.Rmd", output_file = "index.html")
    ```

    El botón *Knit* de RStudio produce el mismo resultado, porque el documento define `index.html` como nombre de salida. La compilación genera el HTML y guarda en `results/` la base maestra (CSV y GeoPackage) y los objetos que usa el carrusel. El cálculo del Gi\* con 9,999 permutaciones toma varios minutos; queda en caché para las compilaciones siguientes.

    Al final del documento, un bloque de verificación comprueba que las cifras escritas en el Resumen y en Métodos coinciden con los datos. Si alguna deja de coincidir, la compilación se detiene e indica cuál.

4.  **Genera el carrusel** (opcional). Después de compilar el informe, ejecuta `carrusel.R` con *Source* en RStudio. Requiere además los paquetes `gridExtra` y `gtable`, y la fuente Lato (o cambia `F <- "Lato"` por `F <- "sans"` en el script). El resultado es `carrusel_linkedin.pdf`.

## Limitaciones

- Los datos del ONSV tienen carácter preliminar y comprenden solo siniestros fatales.
- Ningún indicador incorpora flujos vehiculares ni peatonales, por lo que las tasas no son medidas de riesgo en sentido estricto.
- El año 2021 estuvo afectado por restricciones de movilidad asociadas a la COVID-19, lo que condiciona la comparación con los años siguientes.
- Los resultados espaciales dependen de la escala y del método de inferencia; por ello se privilegian los que son robustos entre escalas.

El detalle de estas y otras limitaciones está en la sección 5.5 del informe.

## Cómo citar

Morales Dávila, C. (2026). *Patrones espaciales y temporales de los siniestros de tránsito fatales en Lima y Callao, 2021–2024* (versión ajustada, 8 de octubre de 2026). <https://github.com/carloxmd/siniestralidad_vial>
