
library(MatchIt)
library(dplyr)
library(ggplot2)
library(haven)
library(gridExtra)
library(reshape)
library(texreg)
library(openxlsx)

library(Matching)
library(ebal)

#Abriremos nuestra base de datos

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

                        
base1 <- base %>% select( # tratamiento
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

base1$id<-rownames(base1)

##Convertir variables compuestas a factor / o numericas 
# Check columns classes

base1$SD_2EXACTA<-as.numeric((base1$SD_2EXACTA))

base1<-droplevels(as_factor(base1))

colnames(base1)["SD_1"]<-"sexo"

base1 = reshape::rename(base1, c(SD_1="sexo",SD_2EXACTA ="edad", SD_4 ="educacion",
                        SD_5="ocupacion",SD_6 = "jefe.hogar" ))


sapply(base1,class)

## la función propensity precisa quitar observaciones NA de las covariables

base1 <- base1[!is.na(base1$sexo),]
base1 <- base1[!is.na(base1$edad),]
base1 <- base1[!is.na(base1$educacion),]
base1 <- base1[!is.na(base1$ocupacion),]
base1 <- base1[!is.na(base1$jefe.hogar),]


# Evaluamos balance de las variables originales

base1$dummymodo<-ifelse(base1$MODO=="CAPI",1,0)


bal.1<-MatchBalance(dummymodo~sexo + edad + educacion + ocupacion + jefe.hogar, 
                    data=base1,
                    match.out = NULL, ks=TRUE)


#Elaboraremos nuestro propensity score



m_ps <- glm(MODO ~ sexo + edad + educacion + ocupacion + jefe.hogar, 
            family = binomial, data = base1)

screenreg(m_ps)



prs_df <- data.frame(pr_score = predict(m_ps, type = "response"),
                     MODO = m_ps$model$MODO)


#Podemos ver el propensity score de cada sujeto
base1<- cbind(base1, prs_df)


#Región de soporte común mediante graficos 

labs <- paste("Propensity", c("Versión B", "Versión A"))

prs_df %>%
  mutate(MODO1 = ifelse(prs_df$MODO== 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~MODO) +
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

bal.1<-MatchBalance(dummymodo~sexo + edad + educacion + ocupacion + jefe.hogar, 
                    data=dta_m,
                    weights = dta_m$pr_score,
                    match.out = NULL, ks=TRUE)


#
#
#
#
#
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
  fn_bal(dta_m, "S4"),
  fn_bal(dta_m, "S3_rec") + theme(legend.position = "none"),
  fn_bal(dta_m, "S5_rec"),
  fn_bal(dta_m, "S6") + theme(legend.position = "none"),
  fn_bal(dta_m, "S6b"),
  fn_bal(dta_m, "GSE"),
  fn_bal(dta_m, "P1_rec"),
  fn_bal(dta_m, "P2_rec"),
  fn_bal(dta_m, "P3_rec"),
  fn_bal(dta_m, "B5"),
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
