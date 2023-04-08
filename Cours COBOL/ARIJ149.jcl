//ARI0409E JOB (ACCT#),'ARIO149',MSGCLASS=H,CLASS=A,
//             REGION=4M,MSGLEVEL=(1,1),NOTIFY=&SYSUID,TIME=(0,30)
//*
//* **************************************************************
//* *                                                            *
//* *                     ESTIAC INSTITUT                        *
//* *                                                            *
//* *           UNITE DE FORMATION COBOL PROGRAMMATION           *
//* *                                                            *
//* *   EXECUTION DU PROGRAMME ARIO1gu CORRESPONDANT AU TP N°1   *
//* *                                                            *
//* **************************************************************
//* **************************************************************
//* * PREMIERE ETAPE :                                           *
//* * EXECUTION DU PROGRAMME PRINCIPAL                           *
//* **************************************************************
//STEPTP1  EXEC PGM=ARIO149
//SYSOUT   DD  SYSOUT=*,OUTLIM=800
//* **************************************************************
//* * DECLARATION DE LA BIBLIOTHEQUE QUI CONTIENT LE LOAD MODULE *
//* **************************************************************
//STEPLIB  DD  DSN=ARI04.ARI0409.LOAD,DISP=SHR
//* **************************************************************
//* * DECLARATION DU FICHIER DES MOUVEMENTS EN ENTREE            *
//* **************************************************************
//INP001   DD  DSN=ARI04.ARI0409.FMVTS,DISP=SHR
//
