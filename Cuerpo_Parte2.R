
# SE COLOCA EL PRODUCTO

tod04 <-  mutate(tod03, MAR_PERD = ifelse(tod03$PERIODICIDAD_CANT<90 & tod03$MAR_PROD==0,1,0), MAR_TA= MAR_PERD+MAR_PROD, MAR_AGR=ifelse(MAR_TA>0,1,0))

tod05 <- mutate(tod04, PRO_CLAS = ifelse(tod04$SUB_CNT=="35",  "C1",
                                         ifelse(tod04$SUB_CNT=="36", "C2",
                                                ifelse(tod04$SUB_CNT=="37", "A1",
                                                       ifelse(tod04$SUB_CNT=="38", "A2",
                                                              ifelse(tod04$SUB_CNT=="04" | tod04$SUB_CNT=="05" |  tod04$SUB_CNT=="31" | tod04$SUB_CNT=="33" | tod04$SUB_CNT=="15" | tod04$SUB_CNT=="11", "C0",
                                                                     ifelse(tod04$SUB_CNT=="22", "A0",
                                                                            ifelse(tod04$SUB_CNT=="18", "HV",
                                                                                   ifelse(tod04$SUB_CNT=="28", "MC",
                                                                                          ifelse(tod04$SUB_CNT=="08", "AU",
                                                                                                 ifelse(tod04$SUB_CNT=="02", "SB",
                                                                                                        ifelse(tod04$SUB_CNT=="47", "M1",
                                                                                                               ifelse(tod04$SUB_CNT=="48", "M2",
                                                                                                        "OT")))))))))))))

# SE VUELVE A COLOCAR LA PROVISION

tod06 <- mutate(tod05, DIG1= str_sub(tod04$CUENTA_CONTABLE, 1, 1),
                DIG2 = str_sub(tod04$CUENTA_CONTABLE, 4, 5),
                MAR_CUEN=ifelse(DIG1==1 & DIG2=="02",1,
                                ifelse(DIG1==1 & DIG2=="11",1,
                                       ifelse(DIG1 != 1,1,
                                              ifelse(DIG1==1,0,""))))) %>%
  filter(MAR_CUEN==0)

# COLOCACION DE LA CONDICION DIAS DE CAPITAL

pr_mc <- select(tod06, IBS, NOMBRE_CLIENTE, PRESTAMO, DIA_VEN_EST, CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL, MAR_AGR, PRO_CLAS)

# CONDICION DE UN PRODUCTO AGRICOLA

prmca <- filter(pr_mc, PRO_CLAS=="A1" | PRO_CLAS=="A0")

pr_mca <- mutate(prmca, POR_C1 = ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[1] & prmca$MAR_AGR==1, T4$Porcentaje[1],
                                        ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[2] & prmca$DIA_VEN_EST > T4$dias_mora_min[2] & prmca$MAR_AGR==1 , T4$Porcentaje[2],
                                               ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[3] & prmca$DIA_VEN_EST > T4$dias_mora_min[3] & prmca$MAR_AGR==1 , T4$Porcentaje[3],
                                                      ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[4] & prmca$DIA_VEN_EST > T4$dias_mora_min[4] & prmca$MAR_AGR==1 , T4$Porcentaje[4],
                                                             ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[5] & prmca$DIA_VEN_EST > T4$dias_mora_min[5] & prmca$MAR_AGR==1 , T4$Porcentaje[5],
                                                                    ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[6] & prmca$DIA_VEN_EST > T4$dias_mora_min[6] & prmca$MAR_AGR==1 , T4$Porcentaje[6],
                                                                           ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[7] & prmca$DIA_VEN_EST > T4$dias_mora_min[7] & prmca$MAR_AGR==1 , T4$Porcentaje[7],
                                                                                  ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[8] & prmca$DIA_VEN_EST > T4$dias_mora_min[8] & prmca$MAR_AGR==1 , T4$Porcentaje[8],
                                                                                         ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[9] & prmca$DIA_VEN_EST > T4$dias_mora_min[9] & prmca$MAR_AGR==1 , T4$Porcentaje[9],
                                                                                                ifelse(prmca$DIA_VEN_EST < T4$dias_mora_max[10] & prmca$DIA_VEN_EST > T4$dias_mora_min[10] & prmca$MAR_AGR==1 , T4$Porcentaje[10],
                                                                                                       ifelse(prmca$DIA_VEN_EST > T4$dias_mora_max[11] & prmca$MAR_AGR==1 , T4$Porcentaje[11],
                                                                                                              
                                                                                                              ifelse(prmca$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL == T3$max_cuotas[1] & prmca$MAR_AGR==0 , T3$Porcentaje[1],
                                                                                                                     ifelse(prmca$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL == T3$max_cuotas[2] & prmca$MAR_AGR==0 , T3$Porcentaje[2],
                                                                                                                            ifelse(prmca$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL < T3$max_cuotas[6] & prmca$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL > T3$max_cuotas[2] & prmca$MAR_AGR==0 , T4$Porcentaje[3],
                                                                                                                                   ifelse(prmca$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL < T3$max_cuotas[12] & prmca$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL > T3$max_cuotas[5] & prmca$MAR_AGR==0 , T4$Porcentaje[6],
                                                                                                                                          ifelse(prmca$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL == T3$max_cuotas[12] & prmca$MAR_AGR==0 , T3$Porcentaje[12],
                                                                                                                                                 0)))))))))))))))))

# CONDICION DE UN PRODUCTO COMERCIAL

prmcc <- filter(pr_mc, PRO_CLAS=="C1" | PRO_CLAS=="C0")

pr_mcc <- mutate(prmcc, POR_C1 = ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[1] , T1$Por_Com[1],
                                        ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[2] & prmcc$DIA_VEN_EST > T1$dias_mora_min[2] , T1$Por_Com[2],
                                               ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[3] & prmcc$DIA_VEN_EST > T1$dias_mora_min[3] , T1$Por_Com[3],
                                                      ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[4] & prmcc$DIA_VEN_EST > T1$dias_mora_min[4] , T1$Por_Com[4],
                                                             ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[5] & prmcc$DIA_VEN_EST > T1$dias_mora_min[5] , T1$Por_Com[5],
                                                                    ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[6] & prmcc$DIA_VEN_EST > T1$dias_mora_min[6] , T1$Por_Com[6],
                                                                           ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[7] & prmcc$DIA_VEN_EST > T1$dias_mora_min[7] , T1$Por_Com[7],
                                                                                  ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[8] & prmcc$DIA_VEN_EST > T1$dias_mora_min[8] , T1$Por_Com[8],
                                                                                         ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[9] & prmcc$DIA_VEN_EST > T1$dias_mora_min[9] , T1$Por_Com[9],
                                                                                                ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[10] & prmcc$DIA_VEN_EST > T1$dias_mora_min[10] , T1$Por_Com[10],
                                                                                                       ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[11] & prmcc$DIA_VEN_EST > T1$dias_mora_min[11] , T1$Por_Com[11],
                                                                                                              ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[12] & prmcc$DIA_VEN_EST > T1$dias_mora_min[12] , T1$Por_Com[12],
                                                                                                                     ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[13] & prmcc$DIA_VEN_EST > T1$dias_mora_min[13] , T1$Por_Com[13],
                                                                                                                            ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[14] & prmcc$DIA_VEN_EST > T1$dias_mora_min[14] , T1$Por_Com[14],
                                                                                                                                   ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[15] & prmcc$DIA_VEN_EST > T1$dias_mora_min[15] , T1$Por_Com[15],
                                                                                                                                          ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[16] & prmcc$DIA_VEN_EST > T1$dias_mora_min[16] , T1$Por_Com[16],
                                                                                                                                                 ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[17] & prmcc$DIA_VEN_EST > T1$dias_mora_min[17] , T1$Por_Com[17],
                                                                                                                                                        ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[18] & prmcc$DIA_VEN_EST > T1$dias_mora_min[18] , T1$Por_Com[18],
                                                                                                                                                               ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[19] & prmcc$DIA_VEN_EST > T1$dias_mora_min[19] , T1$Por_Com[19],
                                                                                                                                                                      ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[20] & prmcc$DIA_VEN_EST > T1$dias_mora_min[20] , T1$Por_Com[20],
                                                                                                                                                                             ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[21] & prmcc$DIA_VEN_EST > T1$dias_mora_min[21] , T1$Por_Com[21],
                                                                                                                                                                                    ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[22] & prmcc$DIA_VEN_EST > T1$dias_mora_min[22] , T1$Por_Com[22],
                                                                                                                                                                                           ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[23] & prmcc$DIA_VEN_EST > T1$dias_mora_min[23] , T1$Por_Com[23],
                                                                                                                                                                                                  ifelse(prmcc$DIA_VEN_EST < T1$dias_mora_max[24] & prmcc$DIA_VEN_EST > T1$dias_mora_min[24] , T1$Por_Com[24],
                                                                                                                                                                                                         ifelse(prmcc$DIA_VEN_EST > T1$dias_mora_max[25] , T1$Por_Com[25],
                                                                                                                                                                                                                0))))))))))))))))))))))))))


# CONDICION DE UN PRODUCTO MICRO

prmic <- filter(pr_mc, PRO_CLAS=="M1" | PRO_CLAS=="M0")

pr_mic <- mutate(prmic, POR_C1 = ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[1] , T1$Por_Micro[1],
                                        ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[2] & prmic$DIA_VEN_EST > T1$dias_mora_min[2] , T1$Por_Micro[2],
                                               ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[3] & prmic$DIA_VEN_EST > T1$dias_mora_min[3] , T1$Por_Micro[3],
                                                      ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[4] & prmic$DIA_VEN_EST > T1$dias_mora_min[4] , T1$Por_Micro[4],
                                                             ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[5] & prmic$DIA_VEN_EST > T1$dias_mora_min[5] , T1$Por_Micro[5],
                                                                    ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[6] & prmic$DIA_VEN_EST > T1$dias_mora_min[6] , T1$Por_Micro[6],
                                                                           ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[7] & prmic$DIA_VEN_EST > T1$dias_mora_min[7] , T1$Por_Micro[7],
                                                                                  ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[8] & prmic$DIA_VEN_EST > T1$dias_mora_min[8] , T1$Por_Micro[8],
                                                                                         ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[9] & prmic$DIA_VEN_EST > T1$dias_mora_min[9] , T1$Por_Micro[9],
                                                                                                ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[10] & prmic$DIA_VEN_EST > T1$dias_mora_min[10] , T1$Por_Micro[10],
                                                                                                       ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[11] & prmic$DIA_VEN_EST > T1$dias_mora_min[11] , T1$Por_Micro[11],
                                                                                                              ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[12] & prmic$DIA_VEN_EST > T1$dias_mora_min[12] , T1$Por_Micro[12],
                                                                                                                     ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[13] & prmic$DIA_VEN_EST > T1$dias_mora_min[13] , T1$Por_Micro[13],
                                                                                                                            ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[14] & prmic$DIA_VEN_EST > T1$dias_mora_min[14] , T1$Por_Micro[14],
                                                                                                                                   ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[15] & prmic$DIA_VEN_EST > T1$dias_mora_min[15] , T1$Por_Micro[15],
                                                                                                                                          ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[16] & prmic$DIA_VEN_EST > T1$dias_mora_min[16] , T1$Por_Micro[16],
                                                                                                                                                 ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[17] & prmic$DIA_VEN_EST > T1$dias_mora_min[17] , T1$Por_Micro[17],
                                                                                                                                                        ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[18] & prmic$DIA_VEN_EST > T1$dias_mora_min[18] , T1$Por_Micro[18],
                                                                                                                                                               ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[19] & prmic$DIA_VEN_EST > T1$dias_mora_min[19] , T1$Por_Micro[19],
                                                                                                                                                                      ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[20] & prmic$DIA_VEN_EST > T1$dias_mora_min[20] , T1$Por_Micro[20],
                                                                                                                                                                             ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[21] & prmic$DIA_VEN_EST > T1$dias_mora_min[21] , T1$Por_Micro[21],
                                                                                                                                                                                    ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[22] & prmic$DIA_VEN_EST > T1$dias_mora_min[22] , T1$Por_Micro[22],
                                                                                                                                                                                           ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[23] & prmic$DIA_VEN_EST > T1$dias_mora_min[23] , T1$Por_Micro[23],
                                                                                                                                                                                                  ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[24] & prmic$DIA_VEN_EST > T1$dias_mora_min[24] , T1$Por_Micro[24],
                                                                                                                                                                                                         ifelse(prmic$DIA_VEN_EST > T1$dias_mora_max[25] , T1$Por_Micro[25],
                                                                                                                                                                                                                0))))))))))))))))))))))))))

# CONDICION DE UN PRODUCTO DE AUTOS

prmcv <- filter(pr_mc, PRO_CLAS=="AU")

pr_mcv <- mutate(prmcv, POR_C1 = ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[1] , T1$Por_Aut[1],
                                        ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[2] & prmcv$DIA_VEN_EST > T1$dias_mora_min[2] , T1$Por_Aut[2],
                                               ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[3] & prmcv$DIA_VEN_EST > T1$dias_mora_min[3] , T1$Por_Aut[3],
                                                      ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[4] & prmcv$DIA_VEN_EST > T1$dias_mora_min[4] , T1$Por_Aut[4],
                                                             ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[5] & prmcv$DIA_VEN_EST > T1$dias_mora_min[5] , T1$Por_Aut[5],
                                                                    ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[6] & prmcv$DIA_VEN_EST > T1$dias_mora_min[6] , T1$Por_Aut[6],
                                                                           ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[7] & prmcv$DIA_VEN_EST > T1$dias_mora_min[7] , T1$Por_Aut[7],
                                                                                  ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[8] & prmcv$DIA_VEN_EST > T1$dias_mora_min[8] , T1$Por_Aut[8],
                                                                                         ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[9] & prmcv$DIA_VEN_EST > T1$dias_mora_min[9] , T1$Por_Aut[9],
                                                                                                ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[10] & prmcv$DIA_VEN_EST > T1$dias_mora_min[10] , T1$Por_Aut[10],
                                                                                                       ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[11] & prmcv$DIA_VEN_EST > T1$dias_mora_min[11] , T1$Por_Aut[11],
                                                                                                              ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[12] & prmcv$DIA_VEN_EST > T1$dias_mora_min[12] , T1$Por_Aut[12],
                                                                                                                     ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[13] & prmcv$DIA_VEN_EST > T1$dias_mora_min[13] , T1$Por_Aut[13],
                                                                                                                            ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[14] & prmcv$DIA_VEN_EST > T1$dias_mora_min[14] , T1$Por_Aut[14],
                                                                                                                                   ifelse(prmcv$DIA_VEN_EST < T1$dias_mora_max[15] & prmcv$DIA_VEN_EST > T1$dias_mora_min[15] , T1$Por_Aut[15],
                                                                                                                                          ifelse(prmcv$DIA_VEN_EST > T1$dias_mora_max[16] , T1$Por_Aut[16],
                                                                                                                                                 0)))))))))))))))))

# CONDICION DE UN PRODUCTO DE HIPOTECARIO

prmch <- filter(pr_mc, PRO_CLAS=="HV")

pr_mch <- mutate(prmch, POR_C1 = ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[1] , T1$Por_Hip[1],
                                        ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[2] & prmch$DIA_VEN_EST > T1$dias_mora_min[2] , T1$Por_Hip[2],
                                               ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[3] & prmch$DIA_VEN_EST > T1$dias_mora_min[3] , T1$Por_Hip[3],
                                                      ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[4] & prmch$DIA_VEN_EST > T1$dias_mora_min[4] , T1$Por_Hip[4],
                                                             ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[5] & prmch$DIA_VEN_EST > T1$dias_mora_min[5] , T1$Por_Hip[5],
                                                                    ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[6] & prmch$DIA_VEN_EST > T1$dias_mora_min[6] , T1$Por_Hip[6],
                                                                           ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[7] & prmch$DIA_VEN_EST > T1$dias_mora_min[7] , T1$Por_Hip[7],
                                                                                  ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[8] & prmch$DIA_VEN_EST > T1$dias_mora_min[8] , T1$Por_Hip[8],
                                                                                         ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[9] & prmch$DIA_VEN_EST > T1$dias_mora_min[9] , T1$Por_Hip[9],
                                                                                                ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[10] & prmch$DIA_VEN_EST > T1$dias_mora_min[10] , T1$Por_Hip[10],
                                                                                                       ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[11] & prmch$DIA_VEN_EST > T1$dias_mora_min[11] , T1$Por_Hip[11],
                                                                                                              ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[12] & prmch$DIA_VEN_EST > T1$dias_mora_min[12] , T1$Por_Hip[12],
                                                                                                                     ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[13] & prmch$DIA_VEN_EST > T1$dias_mora_min[13] , T1$Por_Hip[13],
                                                                                                                            ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[14] & prmch$DIA_VEN_EST > T1$dias_mora_min[14] , T1$Por_Hip[14],
                                                                                                                                   ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[15] & prmch$DIA_VEN_EST > T1$dias_mora_min[15] , T1$Por_Hip[15],
                                                                                                                                          ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[16] & prmch$DIA_VEN_EST > T1$dias_mora_min[16] , T1$Por_Hip[16],
                                                                                                                                                 ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[17] & prmch$DIA_VEN_EST > T1$dias_mora_min[17] , T1$Por_Hip[17],
                                                                                                                                                        ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[18] & prmch$DIA_VEN_EST > T1$dias_mora_min[18] , T1$Por_Hip[18],
                                                                                                                                                               ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[19] & prmch$DIA_VEN_EST > T1$dias_mora_min[19] , T1$Por_Hip[19],
                                                                                                                                                                      ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[20] & prmch$DIA_VEN_EST > T1$dias_mora_min[20] , T1$Por_Hip[20],
                                                                                                                                                                             ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[21] & prmch$DIA_VEN_EST > T1$dias_mora_min[21] , T1$Por_Hip[21],
                                                                                                                                                                                    ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[22] & prmch$DIA_VEN_EST > T1$dias_mora_min[22] , T1$Por_Hip[22],
                                                                                                                                                                                           ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[23] & prmch$DIA_VEN_EST > T1$dias_mora_min[23] , T1$Por_Hip[23],
                                                                                                                                                                                                  ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[25] & prmch$DIA_VEN_EST > T1$dias_mora_min[25] , T1$Por_Hip[25],
                                                                                                                                                                                                         ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[26] & prmch$DIA_VEN_EST > T1$dias_mora_min[26] , T1$Por_Hip[26],
                                                                                                                                                                                                                ifelse(prmch$DIA_VEN_EST < T1$dias_mora_max[27] & prmch$DIA_VEN_EST > T1$dias_mora_min[27] , T1$Por_Hip[27],
                                                                                                                                                                                                                       ifelse(prmch$DIA_VEN_EST > T1$dias_mora_max[28] , T1$Por_Hip[28],
                                                                                                                                                                                                                              0))))))))))))))))))))))))))))



# SE HACE LA UNION DE LOS PRODUCTOS

pmc <- rbind(pr_mca, pr_mcc, pr_mch, pr_mcv, pr_mic)

# SE SELECCIONA LAS VARIABLES

pmc1 <- select(pmc, PRESTAMO, POR_C1)

# SE TRAE EL PORCENTAJE

tod07 <- left_join(tod06, pmc1, by="PRESTAMO")

tod07$POR_C1 <- ifelse(is.na(tod07$POR_C1),0,tod07$POR_C1)

# COLOCACION DE LA CONDICION DIAS DE INTERES

de_mi <- mutate(tod07, DIA_INT_EST=DIAS_VENCIDOS+ds)

pr_mi <- mutate(de_mi, POR_C2=ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[1] , T2$Porcentaje[1],
                                     ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[2] & de_mi$DIA_INT_EST > T2$dias_mora_min[2] , T2$Porcentaje[2],
                                            ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[3] & de_mi$DIA_INT_EST > T2$dias_mora_min[3] , T2$Porcentaje[3],
                                                   ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[4] & de_mi$DIA_INT_EST > T2$dias_mora_min[4] , T2$Porcentaje[4],
                                                          ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[5] & de_mi$DIA_INT_EST > T2$dias_mora_min[5] , T2$Porcentaje[5],
                                                                 ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[6] & de_mi$DIA_INT_EST > T2$dias_mora_min[6] , T2$Porcentaje[6],
                                                                        ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[7] & de_mi$DIA_INT_EST > T2$dias_mora_min[7] , T2$Porcentaje[7],
                                                                               ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[8] & de_mi$DIA_INT_EST > T2$dias_mora_min[8] , T2$Porcentaje[8],
                                                                                      ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[9] & de_mi$DIA_INT_EST > T2$dias_mora_min[9] , T2$Porcentaje[9],
                                                                                             ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[10] & de_mi$DIA_INT_EST > T2$dias_mora_min[10] , T2$Porcentaje[10],
                                                                                                    ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[11] & de_mi$DIA_INT_EST > T2$dias_mora_min[11] , T2$Porcentaje[11],
                                                                                                           ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[12] & de_mi$DIA_INT_EST > T2$dias_mora_min[12] , T2$Porcentaje[12],
                                                                                                                  ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[13] & de_mi$DIA_INT_EST > T2$dias_mora_min[13] , T2$Porcentaje[13],
                                                                                                                         ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[14] & de_mi$DIA_INT_EST > T2$dias_mora_min[14] , T2$Porcentaje[14],
                                                                                                                                ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[15] & de_mi$DIA_INT_EST > T2$dias_mora_min[15] , T2$Porcentaje[15],
                                                                                                                                       ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[16] & de_mi$DIA_INT_EST > T2$dias_mora_min[16] , T2$Porcentaje[16],
                                                                                                                                              ifelse(de_mi$DIA_INT_EST < T2$dias_mora_max[17] & de_mi$DIA_INT_EST > T2$dias_mora_min[17] , T2$Porcentaje[17],
                                                                                                                                                     ifelse(de_mi$DIA_INT_EST > T2$dias_mora_max[18] , T2$Porcentaje[18],
                                                                                                                                                            0)))))))))))))))))))
# COLOCACION DE LA CONDICION PRORROGAS

pr_pr<- mutate(pr_mi, POR_C3=ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[1], T5$Porcentaje[1],
                                    ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[2], T5$Porcentaje[2],
                                           ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[3], T5$Porcentaje[3],
                                                  ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[4], T5$Porcentaje[4],
                                                         ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[5], T5$Porcentaje[5],
                                                                ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[6], T5$Porcentaje[6],
                                                                       ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[7], T5$Porcentaje[7],
                                                                              ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[8], T5$Porcentaje[8],
                                                                                     ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[9], T5$Porcentaje[9],
                                                                                            ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[10], T5$Porcentaje[10],
                                                                                                   ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[11], T5$Porcentaje[11],
                                                                                                          ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[12], T5$Porcentaje[12],
                                                                                                                 ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[13], T5$Porcentaje[13],
                                                                                                                        ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[14], T5$Porcentaje[14],
                                                                                                                               ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[15], T5$Porcentaje[15],
                                                                                                                                      ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[16], T5$Porcentaje[16],
                                                                                                                                             ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[17], T5$Porcentaje[17],
                                                                                                                                                    ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[18], T5$Porcentaje[18],
                                                                                                                                                           ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[19], T5$Porcentaje[19],
                                                                                                                                                                  ifelse(pr_mi$CANTIDAD_PRORROGAS == T5$max_prorrogas[20], T5$Porcentaje[20],
                                                                                                                                                                         0)))))))))))))))))))))

# COLOCACION DE LA CONDICION FUERA DE PLAZO

pr_fp <- select(pr_pr, IBS, NOMBRE_CLIENTE, PRESTAMO, DIAS_FP, CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL, MAR_AGR, PRO_CLAS)

# CONDICION DE AGRICOLA

prfpa <- filter(pr_fp, PRO_CLAS=="A1" | PRO_CLAS=="A0")

pr_fpa <- mutate(prfpa, POR_C4 = ifelse(prfpa$DIAS_FP < T4$dias_mora_max[1] & prfpa$MAR_AGR==1, T4$Porcentaje[1],
                                        ifelse(prfpa$DIAS_FP < T4$dias_mora_max[2] & prfpa$DIAS_FP > T4$dias_mora_min[2] & prfpa$MAR_AGR==1 , T4$Porcentaje[2],
                                               ifelse(prfpa$DIAS_FP < T4$dias_mora_max[3] & prfpa$DIAS_FP > T4$dias_mora_min[3] & prfpa$MAR_AGR==1 , T4$Porcentaje[3],
                                                      ifelse(prfpa$DIAS_FP < T4$dias_mora_max[4] & prfpa$DIAS_FP > T4$dias_mora_min[4] & prfpa$MAR_AGR==1 , T4$Porcentaje[4],
                                                             ifelse(prfpa$DIAS_FP < T4$dias_mora_max[5] & prfpa$DIAS_FP > T4$dias_mora_min[5] & prfpa$MAR_AGR==1 , T4$Porcentaje[5],
                                                                    ifelse(prfpa$DIAS_FP < T4$dias_mora_max[6] & prfpa$DIAS_FP > T4$dias_mora_min[6] & prfpa$MAR_AGR==1 , T4$Porcentaje[6],
                                                                           ifelse(prfpa$DIAS_FP < T4$dias_mora_max[7] & prfpa$DIAS_FP > T4$dias_mora_min[7] & prfpa$MAR_AGR==1 , T4$Porcentaje[7],
                                                                                  ifelse(prfpa$DIAS_FP < T4$dias_mora_max[8] & prfpa$DIAS_FP > T4$dias_mora_min[8] & prfpa$MAR_AGR==1 , T4$Porcentaje[8],
                                                                                         ifelse(prfpa$DIAS_FP < T4$dias_mora_max[9] & prfpa$DIAS_FP > T4$dias_mora_min[9] & prfpa$MAR_AGR==1 , T4$Porcentaje[9],
                                                                                                ifelse(prfpa$DIAS_FP < T4$dias_mora_max[10] & prfpa$DIAS_FP > T4$dias_mora_min[10] & prfpa$MAR_AGR==1 , T4$Porcentaje[10],
                                                                                                       ifelse(prfpa$DIAS_FP > T4$dias_mora_max[11] & prfpa$MAR_AGR==1 , T4$Porcentaje[11],
                                                                                                              
                                                                                                              ifelse(prfpa$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL == T3$max_cuotas[1] & prfpa$MAR_AGR==0 , T3$Porcentaje[1],
                                                                                                                     ifelse(prfpa$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL == T3$max_cuotas[2] & prfpa$MAR_AGR==0 , T3$Porcentaje[2],
                                                                                                                            ifelse(prfpa$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL < T3$max_cuotas[6] & prfpa$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL > T3$max_cuotas[2] & prfpa$MAR_AGR==0 , T4$Porcentaje[3],
                                                                                                                                   ifelse(prfpa$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL < T3$max_cuotas[12] & prfpa$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL > T3$max_cuotas[5] & prfpa$MAR_AGR==0 , T4$Porcentaje[6],
                                                                                                                                          ifelse(prfpa$CANTIDAD_CUOTAS_VENCIDAS_COMERCIAL == T3$max_cuotas[12] & prfpa$MAR_AGR==0 , T3$Porcentaje[12],
                                                                                                                                                 0)))))))))))))))))

# CONDICION DE COMERCIAL

prfpc <- filter(pr_fp, PRO_CLAS=="C1" | PRO_CLAS=="C0")

pr_fpc <- mutate(prfpc, POR_C4 = ifelse(prfpc$DIAS_FP < T1$dias_mora_max[1] , T1$Por_Com[1],
                                        ifelse(prfpc$DIAS_FP < T1$dias_mora_max[2] & prfpc$DIAS_FP > T1$dias_mora_min[2] , T1$Por_Com[2],
                                               ifelse(prfpc$DIAS_FP < T1$dias_mora_max[3] & prfpc$DIAS_FP > T1$dias_mora_min[3] , T1$Por_Com[3],
                                                      ifelse(prfpc$DIAS_FP < T1$dias_mora_max[4] & prfpc$DIAS_FP > T1$dias_mora_min[4] , T1$Por_Com[4],
                                                             ifelse(prfpc$DIAS_FP < T1$dias_mora_max[5] & prfpc$DIAS_FP > T1$dias_mora_min[5] , T1$Por_Com[5],
                                                                    ifelse(prfpc$DIAS_FP < T1$dias_mora_max[6] & prfpc$DIAS_FP > T1$dias_mora_min[6] , T1$Por_Com[6],
                                                                           ifelse(prfpc$DIAS_FP < T1$dias_mora_max[7] & prfpc$DIAS_FP > T1$dias_mora_min[7] , T1$Por_Com[7],
                                                                                  ifelse(prfpc$DIAS_FP < T1$dias_mora_max[8] & prfpc$DIAS_FP > T1$dias_mora_min[8] , T1$Por_Com[8],
                                                                                         ifelse(prfpc$DIAS_FP < T1$dias_mora_max[9] & prfpc$DIAS_FP > T1$dias_mora_min[9] , T1$Por_Com[9],
                                                                                                ifelse(prfpc$DIAS_FP < T1$dias_mora_max[10] & prfpc$DIAS_FP > T1$dias_mora_min[10] , T1$Por_Com[10],
                                                                                                       ifelse(prfpc$DIAS_FP < T1$dias_mora_max[11] & prfpc$DIAS_FP > T1$dias_mora_min[11] , T1$Por_Com[11],
                                                                                                              ifelse(prfpc$DIAS_FP < T1$dias_mora_max[12] & prfpc$DIAS_FP > T1$dias_mora_min[12] , T1$Por_Com[12],
                                                                                                                     ifelse(prfpc$DIAS_FP < T1$dias_mora_max[13] & prfpc$DIAS_FP > T1$dias_mora_min[13] , T1$Por_Com[13],
                                                                                                                            ifelse(prfpc$DIAS_FP < T1$dias_mora_max[14] & prfpc$DIAS_FP > T1$dias_mora_min[14] , T1$Por_Com[14],
                                                                                                                                   ifelse(prfpc$DIAS_FP < T1$dias_mora_max[15] & prfpc$DIAS_FP > T1$dias_mora_min[15] , T1$Por_Com[15],
                                                                                                                                          ifelse(prfpc$DIAS_FP < T1$dias_mora_max[16] & prfpc$DIAS_FP > T1$dias_mora_min[16] , T1$Por_Com[16],
                                                                                                                                                 ifelse(prfpc$DIAS_FP < T1$dias_mora_max[17] & prfpc$DIAS_FP > T1$dias_mora_min[17] , T1$Por_Com[17],
                                                                                                                                                        ifelse(prfpc$DIAS_FP < T1$dias_mora_max[18] & prfpc$DIAS_FP > T1$dias_mora_min[18] , T1$Por_Com[18],
                                                                                                                                                               ifelse(prfpc$DIAS_FP < T1$dias_mora_max[19] & prfpc$DIAS_FP > T1$dias_mora_min[19] , T1$Por_Com[19],
                                                                                                                                                                      ifelse(prfpc$DIAS_FP < T1$dias_mora_max[20] & prfpc$DIAS_FP > T1$dias_mora_min[20] , T1$Por_Com[20],
                                                                                                                                                                             ifelse(prfpc$DIAS_FP < T1$dias_mora_max[21] & prfpc$DIAS_FP > T1$dias_mora_min[21] , T1$Por_Com[21],
                                                                                                                                                                                    ifelse(prfpc$DIAS_FP < T1$dias_mora_max[22] & prfpc$DIAS_FP > T1$dias_mora_min[22] , T1$Por_Com[22],
                                                                                                                                                                                           ifelse(prfpc$DIAS_FP < T1$dias_mora_max[23] & prfpc$DIAS_FP > T1$dias_mora_min[23] , T1$Por_Com[23],
                                                                                                                                                                                                  ifelse(prfpc$DIAS_FP < T1$dias_mora_max[24] & prfpc$DIAS_FP > T1$dias_mora_min[24] , T1$Por_Com[24],
                                                                                                                                                                                                         ifelse(prfpc$DIAS_FP > T1$dias_mora_max[25] , T1$Por_Com[25],
                                                                                                                                                                                                                0))))))))))))))))))))))))))




# CONDICION DE UN PRODUCTO MICRO

prmic <- filter(pr_mc, PRO_CLAS=="M1" | PRO_CLAS=="M0")

pr_mic <- mutate(prmic, POR_C1 = ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[1] , T1$Por_Micro[1],
                                        ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[2] & prmic$DIA_VEN_EST > T1$dias_mora_min[2] , T1$Por_Micro[2],
                                               ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[3] & prmic$DIA_VEN_EST > T1$dias_mora_min[3] , T1$Por_Micro[3],
                                                      ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[4] & prmic$DIA_VEN_EST > T1$dias_mora_min[4] , T1$Por_Micro[4],
                                                             ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[5] & prmic$DIA_VEN_EST > T1$dias_mora_min[5] , T1$Por_Micro[5],
                                                                    ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[6] & prmic$DIA_VEN_EST > T1$dias_mora_min[6] , T1$Por_Micro[6],
                                                                           ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[7] & prmic$DIA_VEN_EST > T1$dias_mora_min[7] , T1$Por_Micro[7],
                                                                                  ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[8] & prmic$DIA_VEN_EST > T1$dias_mora_min[8] , T1$Por_Micro[8],
                                                                                         ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[9] & prmic$DIA_VEN_EST > T1$dias_mora_min[9] , T1$Por_Micro[9],
                                                                                                ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[10] & prmic$DIA_VEN_EST > T1$dias_mora_min[10] , T1$Por_Micro[10],
                                                                                                       ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[11] & prmic$DIA_VEN_EST > T1$dias_mora_min[11] , T1$Por_Micro[11],
                                                                                                              ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[12] & prmic$DIA_VEN_EST > T1$dias_mora_min[12] , T1$Por_Micro[12],
                                                                                                                     ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[13] & prmic$DIA_VEN_EST > T1$dias_mora_min[13] , T1$Por_Micro[13],
                                                                                                                            ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[14] & prmic$DIA_VEN_EST > T1$dias_mora_min[14] , T1$Por_Micro[14],
                                                                                                                                   ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[15] & prmic$DIA_VEN_EST > T1$dias_mora_min[15] , T1$Por_Micro[15],
                                                                                                                                          ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[16] & prmic$DIA_VEN_EST > T1$dias_mora_min[16] , T1$Por_Micro[16],
                                                                                                                                                 ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[17] & prmic$DIA_VEN_EST > T1$dias_mora_min[17] , T1$Por_Micro[17],
                                                                                                                                                        ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[18] & prmic$DIA_VEN_EST > T1$dias_mora_min[18] , T1$Por_Micro[18],
                                                                                                                                                               ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[19] & prmic$DIA_VEN_EST > T1$dias_mora_min[19] , T1$Por_Micro[19],
                                                                                                                                                                      ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[20] & prmic$DIA_VEN_EST > T1$dias_mora_min[20] , T1$Por_Micro[20],
                                                                                                                                                                             ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[21] & prmic$DIA_VEN_EST > T1$dias_mora_min[21] , T1$Por_Micro[21],
                                                                                                                                                                                    ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[22] & prmic$DIA_VEN_EST > T1$dias_mora_min[22] , T1$Por_Micro[22],
                                                                                                                                                                                           ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[23] & prmic$DIA_VEN_EST > T1$dias_mora_min[23] , T1$Por_Micro[23],
                                                                                                                                                                                                  ifelse(prmic$DIA_VEN_EST < T1$dias_mora_max[24] & prmic$DIA_VEN_EST > T1$dias_mora_min[24] , T1$Por_Micro[24],
                                                                                                                                                                                                         ifelse(prmic$DIA_VEN_EST > T1$dias_mora_max[25] , T1$Por_Micro[25],
                                                                                                                                                                                                                0))))))))))))))))))))))))))



# CONDICION DE UN PRODUCTO DE AUTOS

prfpv <- filter(pr_fp, PRO_CLAS=="AU")

pr_fpv <- mutate(prfpv, POR_C4 = ifelse(prfpv$DIAS_FP < T1$dias_mora_max[1] , T1$Por_Aut[1],
                                        ifelse(prfpv$DIAS_FP < T1$dias_mora_max[2] & prfpv$DIAS_FP > T1$dias_mora_min[2] , T1$Por_Aut[2],
                                               ifelse(prfpv$DIAS_FP < T1$dias_mora_max[3] & prfpv$DIAS_FP > T1$dias_mora_min[3] , T1$Por_Aut[3],
                                                      ifelse(prfpv$DIAS_FP < T1$dias_mora_max[4] & prfpv$DIAS_FP > T1$dias_mora_min[4] , T1$Por_Aut[4],
                                                             ifelse(prfpv$DIAS_FP < T1$dias_mora_max[5] & prfpv$DIAS_FP > T1$dias_mora_min[5] , T1$Por_Aut[5],
                                                                    ifelse(prfpv$DIAS_FP < T1$dias_mora_max[6] & prfpv$DIAS_FP > T1$dias_mora_min[6] , T1$Por_Aut[6],
                                                                           ifelse(prfpv$DIAS_FP < T1$dias_mora_max[7] & prfpv$DIAS_FP > T1$dias_mora_min[7] , T1$Por_Aut[7],
                                                                                  ifelse(prfpv$DIAS_FP < T1$dias_mora_max[8] & prfpv$DIAS_FP > T1$dias_mora_min[8] , T1$Por_Aut[8],
                                                                                         ifelse(prfpv$DIAS_FP < T1$dias_mora_max[9] & prfpv$DIAS_FP > T1$dias_mora_min[9] , T1$Por_Aut[9],
                                                                                                ifelse(prfpv$DIAS_FP < T1$dias_mora_max[10] & prfpv$DIAS_FP > T1$dias_mora_min[10] , T1$Por_Aut[10],
                                                                                                       ifelse(prfpv$DIAS_FP < T1$dias_mora_max[11] & prfpv$DIAS_FP > T1$dias_mora_min[11] , T1$Por_Aut[11],
                                                                                                              ifelse(prfpv$DIAS_FP < T1$dias_mora_max[12] & prfpv$DIAS_FP > T1$dias_mora_min[12] , T1$Por_Aut[12],
                                                                                                                     ifelse(prfpv$DIAS_FP < T1$dias_mora_max[13] & prfpv$DIAS_FP > T1$dias_mora_min[13] , T1$Por_Aut[13],
                                                                                                                            ifelse(prfpv$DIAS_FP < T1$dias_mora_max[14] & prfpv$DIAS_FP > T1$dias_mora_min[14] , T1$Por_Aut[14],
                                                                                                                                   ifelse(prfpv$DIAS_FP < T1$dias_mora_max[15] & prfpv$DIAS_FP > T1$dias_mora_min[15] , T1$Por_Aut[15],
                                                                                                                                          ifelse(prfpv$DIAS_FP > T1$dias_mora_max[16] , T1$Por_Aut[16],
                                                                                                                                                 0)))))))))))))))))

# CONDICION DE UN PRODUCTO DE HIPOTECARIO

prfph <- filter(pr_fp, PRO_CLAS=="HV")

pr_fph <- mutate(prfph, POR_C4 = ifelse(prfph$DIAS_FP < T1$dias_mora_max[1] , T1$Por_Hip[1],
                                        ifelse(prfph$DIAS_FP < T1$dias_mora_max[2] & prfph$DIAS_FP > T1$dias_mora_min[2] , T1$Por_Hip[2],
                                               ifelse(prfph$DIAS_FP < T1$dias_mora_max[3] & prfph$DIAS_FP > T1$dias_mora_min[3] , T1$Por_Hip[3],
                                                      ifelse(prfph$DIAS_FP < T1$dias_mora_max[4] & prfph$DIAS_FP > T1$dias_mora_min[4] , T1$Por_Hip[4],
                                                             ifelse(prfph$DIAS_FP < T1$dias_mora_max[5] & prfph$DIAS_FP > T1$dias_mora_min[5] , T1$Por_Hip[5],
                                                                    ifelse(prfph$DIAS_FP < T1$dias_mora_max[6] & prfph$DIAS_FP > T1$dias_mora_min[6] , T1$Por_Hip[6],
                                                                           ifelse(prfph$DIAS_FP < T1$dias_mora_max[7] & prfph$DIAS_FP > T1$dias_mora_min[7] , T1$Por_Hip[7],
                                                                                  ifelse(prfph$DIAS_FP < T1$dias_mora_max[8] & prfph$DIAS_FP > T1$dias_mora_min[8] , T1$Por_Hip[8],
                                                                                         ifelse(prfph$DIAS_FP < T1$dias_mora_max[9] & prfph$DIAS_FP > T1$dias_mora_min[9] , T1$Por_Hip[9],
                                                                                                ifelse(prfph$DIAS_FP < T1$dias_mora_max[10] & prfph$DIAS_FP > T1$dias_mora_min[10] , T1$Por_Hip[10],
                                                                                                       ifelse(prfph$DIAS_FP < T1$dias_mora_max[11] & prfph$DIAS_FP > T1$dias_mora_min[11] , T1$Por_Hip[11],
                                                                                                              ifelse(prfph$DIAS_FP < T1$dias_mora_max[12] & prfph$DIAS_FP > T1$dias_mora_min[12] , T1$Por_Hip[12],
                                                                                                                     ifelse(prfph$DIAS_FP < T1$dias_mora_max[13] & prfph$DIAS_FP > T1$dias_mora_min[13] , T1$Por_Hip[13],
                                                                                                                            ifelse(prfph$DIAS_FP < T1$dias_mora_max[14] & prfph$DIAS_FP > T1$dias_mora_min[14] , T1$Por_Hip[14],
                                                                                                                                   ifelse(prfph$DIAS_FP < T1$dias_mora_max[15] & prfph$DIAS_FP > T1$dias_mora_min[15] , T1$Por_Hip[15],
                                                                                                                                          ifelse(prfph$DIAS_FP < T1$dias_mora_max[16] & prfph$DIAS_FP > T1$dias_mora_min[16] , T1$Por_Hip[16],
                                                                                                                                                 ifelse(prfph$DIAS_FP < T1$dias_mora_max[17] & prfph$DIAS_FP > T1$dias_mora_min[17] , T1$Por_Hip[17],
                                                                                                                                                        ifelse(prfph$DIAS_FP < T1$dias_mora_max[18] & prfph$DIAS_FP > T1$dias_mora_min[18] , T1$Por_Hip[18],
                                                                                                                                                               ifelse(prfph$DIAS_FP < T1$dias_mora_max[19] & prfph$DIAS_FP > T1$dias_mora_min[19] , T1$Por_Hip[19],
                                                                                                                                                                      ifelse(prfph$DIAS_FP < T1$dias_mora_max[20] & prfph$DIAS_FP > T1$dias_mora_min[20] , T1$Por_Hip[20],
                                                                                                                                                                             ifelse(prfph$DIAS_FP < T1$dias_mora_max[21] & prfph$DIAS_FP > T1$dias_mora_min[21] , T1$Por_Hip[21],
                                                                                                                                                                                    ifelse(prfph$DIAS_FP < T1$dias_mora_max[22] & prfph$DIAS_FP > T1$dias_mora_min[22] , T1$Por_Hip[22],
                                                                                                                                                                                           ifelse(prfph$DIAS_FP < T1$dias_mora_max[23] & prfph$DIAS_FP > T1$dias_mora_min[23] , T1$Por_Hip[23],
                                                                                                                                                                                                  ifelse(prfph$DIAS_FP < T1$dias_mora_max[25] & prfph$DIAS_FP > T1$dias_mora_min[25] , T1$Por_Hip[25],
                                                                                                                                                                                                         ifelse(prfph$DIAS_FP < T1$dias_mora_max[26] & prfph$DIAS_FP > T1$dias_mora_min[26] , T1$Por_Hip[26],
                                                                                                                                                                                                                ifelse(prfph$DIAS_FP < T1$dias_mora_max[27] & prfph$DIAS_FP > T1$dias_mora_min[27] , T1$Por_Hip[27],
                                                                                                                                                                                                                       ifelse(prfph$DIAS_FP > T1$dias_mora_max[28] , T1$Por_Hip[28],
                                                                                                                                                                                                                              0))))))))))))))))))))))))))))

# SE HACE LA UNION DE LOS PRODUCTOS

pfp <- rbind(pr_fpa, pr_fpc, pr_fph, pr_fpv)

# SE SELECCIONA LAS VARIABLES

pfp1 <- select(pfp, PRESTAMO, POR_C4)

# SE TRAE EL PORCENTAJE

tod08 <- left_join(pr_pr, pfp1, by="PRESTAMO")

tod08$POR_C4 <- ifelse(is.na(tod08$POR_C4),0,tod08$POR_C4)

# COLOCAR LA PROVISION DE LITIGIO Y REESTRUCTURADO

tod09 <- mutate(tod08, POR_C7 = ifelse(tod08$ESTATUS_CONTABLE==3,0.15,
                                       ifelse(tod08$ESTATUS_CONTABLE==4,0.60,0)))

# COLOCAR EL MAXIMO DE LA PROVISION

tod10 <- mutate(tod09, PR_DEF = pmax(POR_C1, POR_C2, POR_C3 , POR_C4 , POR_C7 ,na.rm = TRUE))

