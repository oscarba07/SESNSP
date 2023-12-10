### Preparacion de datos de incidencia delictiva para TPR
### fuente: https://www.gob.mx/sesnsp/acciones-y-programas/datos-abiertos-de-incidencia-delictiva?state=published
setwd('###')
setwd(paste0(getwd(),'/SESNSP/2309'))

paquetes <- c('magrittr','tidyr','stringr','dplyr','plotly')
for (paq in paquetes) {
  if (!require(paq,character.only = T)) {
    install.packages(paq,dependencies = T)
    library(paq, character.only = T)
  }
}

mes <- 'sep'
p_inic <- '2023/04/01' # Periodo inicial 
p_fin <- '2023/09/01' # Periodo que ya no se incluye
p_pas <- '2023/03/01' # Periodo pasado
  
## Incidencia delictiva del fuero federal
### Guardar tablas como CSV UTF-8 para poder leerlas
idff <- read.csv(paste0('IDEFF_',mes,'23.csv'),na.strings = "", encoding = 'UTF-8')
idff <- pivot_longer(idff,cols = 7:length(names(idff)),names_to = 'mes'
                     ,values_to = 'inc_del')
Sys.setlocale("LC_TIME", "Spanish")
idff$fecha <- paste(idff$AÑO,str_to_lower(idff$mes),'01',sep='/') %>% 
  as.Date(.,format='%Y/%B/%d')

idff_tpr <- aggregate(subset(idff,fecha>=p_inic&fecha<p_fin)
                      ,inc_del~ENTIDAD+INEGI, FUN=sum)

## Incidencia delictiva del fuero comun (ef,mun)
idfc <- read.csv(paste0('IDEFC_NM_',mes,'23.csv'),na.strings = "", encoding = 'UTF-8')
idfc <- pivot_longer(idfc,cols = 8:length(names(idfc)),names_to = 'mes'
                     , values_to = 'inc_del')
idfc$fecha <- paste(idfc$Año,str_to_lower(idfc$mes),'01',sep='/') %>% 
  as.Date(.,format='%Y/%B/%d')

idfc_tpr <- aggregate(subset(idfc,fecha>=p_inic&fecha<p_fin)
                      ,inc_del~Entidad+Clave_Ent, FUN=sum)

id_ef_tpr <- inner_join(idff_tpr,idfc_tpr, by = c('INEGI'='Clave_Ent'))
id_ef_tpr$total <- id_ef_tpr$inc_del.x + id_ef_tpr$inc_del.y
id_ef_tpr <- subset(id_ef_tpr,select = c('INEGI','ENTIDAD','inc_del.x','inc_del.y'
                                 ,'total'))
names(id_ef_tpr)[c(3,4)] <- c('inc_del_ff','inc_del_fc')

id_ef_tpr <- id_ef_tpr[order(id_ef_tpr$total,decreasing = T),] 

write.csv(id_ef_tpr,'incidencia_delictiva_EF.csv', fileEncoding = 'UTF-8')

## Nacional
idfc_nal <- aggregate(data = idfc, inc_del ~ fecha, FUN = sum)
idff_nal <- aggregate(data = idff, inc_del ~ fecha, FUN = sum)
id_nal <- inner_join(idfc_nal, idff_nal, by = 'fecha')
names(id_nal) <- c('fecha', 'inc_del_fc','inc_del_ff')
id_nal$tot <- id_nal$inc_del_fc + id_nal$inc_del_ff


(id_nal$tot[id_nal$fecha==p_fin] / id_nal$tot[id_nal$fecha==p_pas]-1)*100
id_nal$tot[id_nal$fecha==p_fin] - id_nal$tot[id_nal$fecha==p_pas]

id_nal_tpr <- subset(id_nal, fecha>=p_inic&fecha<p_fin)
sum(id_nal_tpr$tot)


# Graficos ----------------------------------------------------------------
{
  ilob <- 'rgb(30,45,190)'
  ilor <- "rgb(250,60,75)"
  ilodb <- 'rgb(35,0,80)'
  ilot <- 'rgb(5,210,210)'
  iloy <- 'rgb(255,205,45)'
  ilop <- 'rgb(150,10,85)'
  ilogre <- 'rgb(140,225,100)'
  ilogra <- 'rgb(235.245.253)'
} 

fig1 <- plot_ly(id_nal
               ,x = id_nal$fecha 
               ,y = id_nal$tot 
               ,type = 'scatter', mode = 'lines+markers'
               ,marker = list(color=ilor) 
               ,line = list(color=ilor)
) %>% layout(title = paste("Crime Incidence in Mexico"),
             paper_bgcolor = 'rgb(255,255,255)', 
             plot_bgcolor  = 'rgb(229,229,229)',
             xaxis         = list(title          = 'Date',
                                  gridcolor      = 'rgb(255,255,255)',
                                  showgrid       = TRUE,
                                  showline       = TRUE,
                                  showticklabels = TRUE,
                                  tickcolor      = 'rgb(127,127,127)',
                                  ticks          = 'inside',
                                  zeroline       = FALSE),
             yaxis         = list(title          = 'Cases',
                                  gridcolor      = 'rgb(255,255,255)',
                                  showgrid       = TRUE,
                                  showline       = TRUE,
                                  showticklabels = TRUE,
                                  tickcolor      = 'rgb(127,127,127)',
                                  ticks          = 'inside',
                                  zeroline       = FALSE,
                                  categoryorder = "array"
             )
)
fig1

## Jalisco
idff_jal <- subset(idff,ENTIDAD=='JALISCO'&AÑO>=2022) %>% aggregate(.,inc_del~AÑO+mes
                                                          ,FUN=sum)
idfc_jal <- subset(idfc,Entidad=='Jalisco'&Año>=2022) %>% aggregate(.,inc_del~Año+mes
                                                                    ,FUN=sum)
jal_id_fc <- subset(idfc,Entidad=='Jalisco') %>% 
  aggregate(.,inc_del~Entidad+fecha,FUN=sum)

jal_id_ff <- subset(idff,ENTIDAD=='JALISCO') %>% 
  aggregate(.,inc_del~ENTIDAD+fecha,FUN=sum)

jal_id <- inner_join(jal_id_fc,jal_id_ff,by='fecha')
jal_id$total <- jal_id$inc_del.x + jal_id$inc_del.y

fig <- plot_ly(jal_id
              ,x = jal_id$fecha 
              ,y = jal_id$total 
              ,type = 'scatter', mode = 'lines+markers'
              ,marker = list(color=ilor) 
              ,line = list(color=ilor)
              ) %>% layout(title = paste("Crime Incidence in Jalisco"),
                           paper_bgcolor = 'rgb(255,255,255)', 
                           plot_bgcolor  = 'rgb(229,229,229)',
                           xaxis         = list(title          = 'Date',
                                                gridcolor      = 'rgb(255,255,255)',
                                                showgrid       = TRUE,
                                                showline       = TRUE,
                                                showticklabels = TRUE,
                                                tickcolor      = 'rgb(127,127,127)',
                                                ticks          = 'inside',
                                                zeroline       = FALSE),
                           yaxis         = list(title          = 'Cases',
                                                gridcolor      = 'rgb(255,255,255)',
                                                showgrid       = TRUE,
                                                showline       = TRUE,
                                                showticklabels = TRUE,
                                                tickcolor      = 'rgb(127,127,127)',
                                                ticks          = 'inside',
                                                zeroline       = FALSE,
                                                categoryorder = "array"
                           )
              )
fig

(jal_id$total[jal_id$fecha==p_fin] / jal_id$total[jal_id$fecha==p_pas]-1)*100
jal_id$total[jal_id$fecha==p_fin] - jal_id$total[jal_id$fecha==p_pas]
