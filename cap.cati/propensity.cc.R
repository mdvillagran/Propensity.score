
library(MatchIt)
library(dplyr)
library(car)
library(ggplot2)
library(haven)
library(gridExtra)
library(reshape) # recodificar columnas por nombres
library(texreg)
library(openxlsx)

library(Matching) # paquete que nos permiten evaluar el balance de la muestra
library(ebal) # paquete que nos permiten evaluar el balance de la muestra

#Abrimos nuestra base de datos

rm(list = ls())

setwd("C:/Users/Villagran/Desktop/datavoz/cati.capi")

base <- read_sav("220518 BBDD Homologada Experimento CEP v3.sav")

###### variables clave

# Sexo -> SD_1

# Edad -> SD_2EXACTA

# Educacion -> SD_4 (recode 99 to NA)

# Ocupacion -> SD_5 (recode 9 to NA)

# Jefe de hogar -> SD_6 (recode 1, 2 to NA)


base$SD_4[base$SD_4==99]<-NA
base$SD_5[base$SD_5==9]<-NA
base$SD_6[base$SD_6==8]<-NA
base$SD_6[base$SD_6==9]<-NA

## SELECCION DE VARIABLES CLAVE

                        
base1 <- base %>% dplyr::select( # tratamiento
                         MODO,tipo_vivienda,
                          # variables de control
                         SD_1,SD_2EXACTA,SD_4,SD_5,SD_6,
                          # Variables dependientes
                         IND_POLITIZACION,M1_9,M2_8,M2_9,
                         M1_13_1_rec,M1_13_2_rec,M1_13_3_rec,M1_13_4_rec,M1_13_5_rec,
                          # ponderador
                         pond_unificado)


## dejamos solamente las observaciones con el campo tipo de vivienda casa, depto
#base1<- base1 %>% filter(tipo_vivienda %in% c(1, 2))


# Creamos variable de identificación
base1$id<-rownames(base1)

#base1<-droplevels(as_factor(base1))

base1 = reshape::rename(base1, c(SD_1="sexo",SD_2EXACTA ="edad", SD_4 ="educacion",
                        SD_5="ocupacion",SD_6 = "jefe.hogar" ))


sapply(base1,class)

## la función propensity precisa quitar observaciones NA de las covariables

base1 <- base1[!is.na(base1$sexo),]
base1 <- base1[!is.na(base1$edad),]
base1 <- base1[!is.na(base1$educacion),]
base1 <- base1[!is.na(base1$ocupacion),]
base1 <- base1[!is.na(base1$jefe.hogar),]


# para la evaluación de balance, por comodidad  cambiaremos variables ordinales
# en numéricas o binarias en dummyes

base1$sexo[base1$sexo==2]<-0 # mujer
base1$sexo<-as.numeric(base1$sexo)

base1$edad<-as.numeric((base1$edad))

base1$educacion<-as.numeric(base1$educacion)

base1$ocupacion<-as.numeric(base1$ocupacion)

base1$ocupacion <-Recode(base1$ocupacion,"1=2;2=1;3=0")

# 0, No trabaja
# 1, No trabaja pero busca empleo
# 2, Trabaja


base1$jefe.hogar<-as.numeric(base1$jefe.hogar)
base1$jefe.hogar[base1$jefe.hogar==2]<-0 # No es jefe de hogar


base1$MODO<-as.numeric(base1$MODO)

# CATI/TELEFONO = 1
# CAPI = 0

base1$MODO <- Recode(base1$MODO, 
                          "1=0; 
                             2=1")


################################################################################
######################## METODO PARA EVALUAR BALANCE ###########################
################################################################################

# Evaluamos balance de las variables originales


bal.1<-MatchBalance(MODO~sexo + edad + educacion + ocupacion + jefe.hogar, 
                    data=base1,
                    match.out = NULL, ks=TRUE)

# Herramienta para visualizar de forma más amigable los resultados

bal1.label  <-c("sexo","edad","educacion", "ocupacion","jefe.hogar")

bal1.m1  <- baltest.collect(matchbal.out=bal.1,var.names=bal1.label,after=FALSE)

# OUTPUT
round(bal1.m1,2)



# "Mean.Tr" corresponde a los valores medios de "CAPI" / Dado que previamente se recodificó como 0
# "Mean.Co" corresponde a los valores medios de "CATI" / Dado que previamente se recodificó como 0

# "diff.pooled" reporta la diferencia estandarizada absoluta; valores bajo 15 son los ideales

# Todos los "pval" inferiores a 0.05 hablan de desbalance

# "KS pval" evalúa distribución en variables de más de dos niveles continuas (por eso
# "jefe.hogar" y "sexo" arrojan NA). Cuando es significativo indica diferencias importantes
# en cuanto a distribución

# RESUMEN: Hay diferencias entre los grupos de acuerdo a "Sexo", "Edad", "Educación" y
# "Ocupación". No hay diferencias en cuanto a "jefe.hogar"


################################################################################
#######################  Elaboramos nuestro propensity score #################
################################################################################



m_ps <- stats::glm(MODO ~ sexo + edad + educacion + ocupacion + jefe.hogar, 
            family = binomial(), data = base1)

screenreg(m_ps)

# Propensity asociado a cada MODO (1-probabilidad/propensity de ser escogidos, 
# en los controles;Y probabilidad de ser escogidos, en los tratados)

prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     MODO = m_ps$model$MODO)

# agregamos propensity a nuestra base1

base1<-cbind(base1, prs_df)


# Creamos un segundo ponderador IPW

base1$w<- 1/base1$pr_score


################################################################################
####### Evaluamos nuevamente balance de las variables originales ################
################################################################################

bal.1<-MatchBalance(MODO~sexo + edad + educacion + ocupacion + jefe.hogar,
                    data=base1,weights = base1$prs_df,
                    match.out = NULL, ks=TRUE)

# Herramienta para visualizar de forma más amigable los resultados

bal1.label  <-c("sexo","edad","educacion", "ocupacion","jefe.hogar")

bal1.m1  <- baltest.collect(matchbal.out=bal.1,var.names=bal1.label,after=FALSE)

# OUTPUT
round(bal1.m1,2)


################################################################################

" En este punto no comprendo por qué el análisis de balance sigue manifestando
discrepancias de la misma magnitud entre los grupos, a pesar de que agregamos 
pesos de acuerdo al propensity"

################################################################################





# Visualización propensity CAPI/ CATI

labs <- paste("Propensity", c("CAPI", "CATI"))

prs_df %>%
  mutate(MODO1 = ifelse(prs_df$MODO == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white", bins = 30) +
  facet_wrap(~MODO1) +
  theme_bw()




#matching #
mod_match <- matchit(MODO ~ sexo + edad + educacion + ocupacion + jefe.hogar,
                     s.weights = base1$pond_unificado,
                     method = "cem", data = base1)


#Resumen general del matching
resumen <- summary(mod_match)

#
resumen$sum.all -> total
resumen$sum.matched -> match
lista <- list(total, match)
write.xlsx(lista, "Tabla Matching.xlsx")

#Visualización gráfica del emparejamiento



dta_m <- match.data(mod_match) #Base de datos con matching ejecutado


###################

################################################################################

"NO ESTOY SEGURO DE QUÉ VARIABLE CORRESPONDE A DISTANCIA"

#Inspección general del matching: líneas de tratamiento y control deben ser similares

fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  if (variable == 'personas') dta$variable <- dta$variable
  dta$MODO <- as.factor(dta$MODO)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = MODO)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

#################### MODIFICAR ############################

library(gridExtra)
grid.arrange(
  fn_bal(dta_m, "sexo"),
  fn_bal(dta_m, "edad"), 
  fn_bal(dta_m, "educacion"),
  fn_bal(dta_m, "ocupacion") ,
  fn_bal(dta_m, "jefe.hogar"),
  nrow = 5, widths = c(1, 0.8)
)

#Estimar efecto del tratamiento


banner_cp <- dta_m %>% tab_cols(total(),Cuestionario) %>% tab_weight(weights)

####SELECCIONAR SUBSET
demograficas <- dta_m %>% select(S4,S3_rec,S5_rec,S6,S6b,GSE,P1_rec,P2_rec,P3_rec,B5,C2_fct)
percepciones <- dta_m %>% select(C3,B7_1_1,B7_1_2,B7_1_3,B7_1_4,B7_1_5,B7_1_6,B7_2_1,B7_2_2,B7_2_3,B7_2_4,B7_2_5,B7_2_6)

####CREAR FUNCIONES
tab_pct_sig = . %>% tab_stat_cpct(                # funcion
  total_statistic = c("w_cases",                   # total de casos ponderados
                      "u_cases"),                  # total de casos no poderados
  total_label = c("Número de casos ponderados",    # etiqueta de "w_cases"
                  "número de casos no ponderados") # etiqueta de "u_cases"
) %>% 
  tab_last_sig_cpct(bonferroni = TRUE)             # TRUE=calcula las 
# diferencias proporcionales

tab_means_sig = . %>% tab_stat_mean_sd_n(   # funcion
  labels = c("Mean",                        # etiquetas que tendran las filas.
             "sd",                          ## Siguen el mismo orden que la funcion:
             "N")) %>%                      ## mean, sd, n
  tab_last_sig_means(bonferroni = TRUE)     ## TRUE=calcula las diferencias de media


# Categoricas sin ponderador
list_table_cat_sp = lapply(demograficas, function(variable) {
  banner_cp %>% 
    tab_cells(variable) %>%
    tab_pct_sig %>%
    tab_pivot()
})

###GUARDAR DATA MATCH
write_sav(dta_m,"GPS MAYO MATCHING2.sav")

####EXPORTAR EXCEL UNICO
banner_full <- createWorkbook()  # generamos documento en blanco
sh1 <- addWorksheet(banner_full, # generamos una hoja para insertar las proporciones
                    "Proporciones ponderadas")
xl_write(list_table_cat_sp,          # lista con nuestros resultados la seccion 1
         banner_full,                          # nuestro objeto workbook creado anteriormente
         sh1,                                  # hoja especifica para medias
         col_symbols_to_remove = "#",          # eilimanr # de las columnas
         row_symbols_to_remove = "#",          # eliminar # de las filas
         other_col_labels_formats = list("#" = createStyle(textDecoration = "bold")))


saveWorkbook(banner_full,                       # objeto a exportar
             "tablas/Diferencias percepcion Matching weights.xlsx",  # nombre de la hoja
             overwrite = TRUE)

#####GRAFICOS
dta_m2 <- read_sav("GPS MAYO MATCHING2.sav")

tab_var <- dta_m2 %>% filter(C3 %>% is.na() %>% not) %>% group_by(Cuestionario) %>% count(C3, wt = weights) %>% mutate(porcentaje = n*100/sum(n)) %>% rename("categoria" = "C3")
tab_var <- tab_var %>% as_factor()

plot_1 <- ggplot(tab_var, aes(x=categoria,y=porcentaje, fill=Cuestionario))+
  geom_bar(stat="identity", position=position_dodge())+
  theme_bw()+
  scale_fill_manual(values = c("#009E73","#F0E442"))+
  labs(y=NULL, x = NULL, title = "Si hoy fueran las elecciones y se votara el plebiscito de salida de la nueva constitución,\ncon los antecedentes que posees en la actualidad, votarías",
       subtitle = "Según versión del cuestionario")+
  scale_y_continuous(limits = c(0,101))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size = 10),
        legend.title = element_blank(), legend.position = "top")+
  geom_text(aes(x=categoria,y=porcentaje,label = paste0(round(porcentaje,2),"%")), 
            position=position_dodge(width = 1), vjust = -1, size=4)

ggsave(filename = "plot_voto_cuestionario.png", plot = plot_1, width = 20, height = 10, dpi = 400, units = "cm")


####GRAFICO B8 POR Cuestionario
###PRIMERA PARTE
tab_var_1 <- dta_m2 %>% filter(B7_1_1 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_1_1, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 1") %>% rename("Categoria" = "B7_1_1")
tab_var_2 <- dta_m2 %>% filter(B7_1_2 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_1_2, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 2") %>% rename("Categoria" = "B7_1_2")
tab_var_3 <- dta_m2 %>% filter(B7_1_3 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_1_3, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 3") %>% rename("Categoria" = "B7_1_3")
tab_var_4 <- dta_m2 %>% filter(B7_1_4 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_1_4, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 4") %>% rename("Categoria" = "B7_1_4")
tab_var_5 <- dta_m2 %>% filter(B7_1_5 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_1_5, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 5") %>% rename("Categoria" = "B7_1_5")
tab_var_6 <- dta_m2 %>% filter(B7_1_6 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_1_6, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 6") %>% rename("Categoria" = "B7_1_6")

tab_var <- bind_rows(tab_var_1,tab_var_2,tab_var_3,tab_var_4,tab_var_5,tab_var_6) %>% as_factor()
tab_var$Categoria <- factor(tab_var$Categoria, levels = levels(tab_var$Categoria)[c(4,3,2,1,5,6)])

plot_1<- ggplot(tab_var, aes(x=variable,y=porcentaje,fill=Categoria))+
  geom_bar(stat="identity")+
  scale_fill_ordinal()+
  theme_bw()+
  labs(y=NULL, x = NULL, title = "Si la nueva constitución contiene el siguiente artículo, ¿Cuán probable es que Usted aprueba\no rechace la propuesta de nueva constitución en el plebiscito de salida?",
       subtitle = "Según versión del cuestionario")+
  scale_y_continuous(limits = c(0,101))+
  geom_text(aes(x=variable,y=porcentaje,label = ifelse(Categoria == "No Sabe / No Contesta", sprintf(""), paste0(round(porcentaje,1),"%"))), 
            position = position_stack(vjust=0.5),size=3, colour = "white")+
  geom_text(aes(x=variable,y=porcentaje,label = ifelse(Categoria != "No Sabe / No Contesta", sprintf(""), paste0(round(porcentaje,1),"%"))), 
            position = position_stack(vjust=0.5),size=3, colour = "black")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size = 9, angle=45, hjust=1),
        legend.title = element_blank(), legend.position = "top", legend.text = element_text(size = 10))+
  facet_wrap(.~Cuestionario)

###SEGUNDA PARTE
tab_var_1 <- dta_m2 %>% filter(B7_2_1 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_2_1, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 7") %>% rename("Categoria" = "B7_2_1")
tab_var_2 <- dta_m2 %>% filter(B7_2_2 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_2_2, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 8") %>% rename("Categoria" = "B7_2_2")
tab_var_3 <- dta_m2 %>% filter(B7_2_3 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_2_3, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 9") %>% rename("Categoria" = "B7_2_3")
tab_var_4 <- dta_m2 %>% filter(B7_2_4 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_2_4, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 10") %>% rename("Categoria" = "B7_2_4")
tab_var_5 <- dta_m2 %>% filter(B7_2_5 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_2_5, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 11") %>% rename("Categoria" = "B7_2_5")
tab_var_6 <- dta_m2 %>% filter(B7_2_6 %>% is.na() %>% not(), Cuestionario %>% is.na() %>% not()) %>% group_by(Cuestionario) %>% count(B7_2_6, wt = weights) %>% mutate(porcentaje = n*100/sum(n), variable = "Afirmación 12") %>% rename("Categoria" = "B7_2_6")

tab_var <- bind_rows(tab_var_1,tab_var_2,tab_var_3,tab_var_4,tab_var_5,tab_var_6) %>% as_factor()
tab_var$Categoria <- factor(tab_var$Categoria, levels = levels(tab_var$Categoria)[c(4,3,2,1,5,6)])
tab_var$variable <- as.factor(tab_var$variable)
tab_var$variable <- factor(tab_var$variable, levels = levels(tab_var$variable)[c(4,5,6,1,2,3)])

plot_2<- ggplot(tab_var, aes(x=variable,y=porcentaje,fill=Categoria))+
  geom_bar(stat="identity")+
  scale_fill_ordinal()+
  theme_bw()+
  labs(y=NULL, x = NULL, title = NULL,
       subtitle = NULL)+
  scale_y_continuous(limits = c(0,101))+
  geom_text(aes(x=variable,y=porcentaje,label = ifelse(Categoria == "No Sabe / No Contesta", sprintf(""), paste0(round(porcentaje,1),"%"))), 
            position = position_stack(vjust=0.5),size=3, colour = "white")+
  geom_text(aes(x=variable,y=porcentaje,label = ifelse(Categoria != "No Sabe / No Contesta", sprintf(""), paste0(round(porcentaje,1),"%"))), 
            position = position_stack(vjust=0.5),size=3, colour = "black")+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size = 9, angle=45, hjust=1),
        legend.title = element_blank(), legend.position = "none", legend.text = element_text(size = 10))+
  facet_wrap(.~Cuestionario)

plot_total <- plot_1 / plot_2

ggsave(filename = "plot_B7_Cuestionario.png", plot = plot_total, width = 25, height = 15, dpi = 400, units = "cm") #nombre de archivo debe "plot_nombrevariable"
