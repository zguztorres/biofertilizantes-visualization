# ============================================
# 01_limpieza_datos.R
# Limpieza de datos de encuesta de biofertilizantes
# Visualización de Datos Biológicos - USAL
# ============================================

# Cargar librerías
library(readxl)
library(tidyverse)
library(janitor)

# Configurar
options(stringsAsFactors = FALSE)
set.seed(2026)


# 1. Cargar datos raw ------------------------------------------------
cat("📂 Cargando archivo raw...\n")

encuesta_raw <- read_excel("data/raw/encuesta_raw.xlsx", 
                           sheet = "Respuestas de formulario 1")

cat("✅ Datos cargados:", nrow(encuesta_raw), "filas,", 
    ncol(encuesta_raw), "columnas\n\n")

# 2. Función para extraer números de textos -------------------------
extraer_numero <- function(x) {
  if(is.na(x)) return(NA_real_)
  if(is.numeric(x)) return(x)
  num <- str_extract(as.character(x), "\\d+\\.?\\d*")
  if(is.na(num)) return(NA_real_)
  return(as.numeric(num))
}

# 3. Limpieza principal ---------------------------------------------
cat("🧹 Limpiando datos...\n")

encuesta_clean <- encuesta_raw %>%
  # Limpiar nombres de columnas
  clean_names() %>%
  # Renombrar columnas relevantes
  rename(
    biofertilizante = 2,
    cultivo = 3,
    manzanas = 4,
    compuestos = 5,
    produccion = 6,
    desarrollo_descripcion = 7,
    frecuencia_plaguicidas = 8,
    cambio_plaguicidas_descripcion = 9,
    cambio_plagas = 10,
    frecuencia_biofertilizante = 11
  ) %>%
  mutate(
    # Limpiar producción (extraer número)
    produccion_qq_mz = sapply(produccion, extraer_numero),
    
    # Limpiar manzanas (1/2 manzana = 0.5)
    manzanas_clean = case_when(
      str_detect(tolower(manzanas), "1/2|media") ~ 0.5,
      TRUE ~ extraer_numero(manzanas)
    ),
    
    # Estandarizar nombres de cultivos
    cultivo_clean = str_to_lower(cultivo) %>%
      case_when(
        str_detect(., "maíz|maiz") ~ "Maíz",
        str_detect(., "sorgo|maicillo") ~ "Sorgo",
        str_detect(., "arroz") ~ "Arroz",
        str_detect(., "caña") ~ "Caña de azúcar",
        TRUE ~ "Otro"
      ),
    
    # Estandarizar nombres de biofertilizantes
    biofertilizante_clean = str_to_lower(biofertilizante) %>%
      case_when(
        str_detect(., "biopro") ~ "Biopro",
        str_detect(., "bioamigo|bio amigo") ~ "Bioamigo",
        str_detect(., "orgosanto|organosato") ~ "Organosato",
        str_detect(., "crop plus") ~ "Crop Plus",
        str_detect(., "albamin") ~ "Albamin",
        str_detect(., "nitroesten") ~ "Nitroesten",
        str_detect(., "bocachi") ~ "Bocachi",
        str_detect(., "polvo de roca|gallinaza|rastrojo|micorriza") ~ "Mezcla artesanal",
        TRUE ~ "Otro"
      ),
    
    # Simplificar respuesta de cambio en plagas
    cambio_plagas_simple = case_when(
      str_detect(tolower(cambio_plagas), "disminu|menos|bajo|redu") ~ "Disminuyó",
      str_detect(tolower(cambio_plagas), "aument|más") ~ "Aumentó",
      TRUE ~ "Sin cambio o NA"
    ),
    
    # Extraer frecuencia de plaguicidas (número)
    frecuencia_plaguicidas_num = extraer_numero(frecuencia_plaguicidas)
  ) %>%
  # Filtrar filas sin cultivo válido
  filter(cultivo_clean != "Otro", !is.na(produccion_qq_mz))

cat("Limpieza completada\n")
cat("   Filas resultantes:", nrow(encuesta_clean), "\n\n")

# 4. Estadísticas rápidas -------------------------------------------
cat("Resumen de datos:\n")
cat("--------------------\n")
cat("Cultivos:\n")
print(table(encuesta_clean$cultivo_clean))

cat("\nBiofertilizantes:\n")
print(table(encuesta_clean$biofertilizante_clean))

cat("\nProducción (qq/mz):\n")
print(summary(encuesta_clean$produccion_qq_mz))

cat("\nCambio en plagas:\n")
print(table(encuesta_clean$cambio_plagas_simple))

# 5. Guardar datos limpios ------------------------------------------
write_csv(encuesta_clean, "data/processed/encuesta_limpia.csv")
cat("\n Datos guardados en: data/processed/encuesta_limpia.csv\n")

# 6. Mostrar primeras filas -----------------------------------------
cat("\n Primeras 5 filas:\n")
print(head(encuesta_clean, 5))

cat("\n Proceso completado con éxito!\n")
