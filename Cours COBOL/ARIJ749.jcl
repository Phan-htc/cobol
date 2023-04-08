//ARIgguuE JOB (ACCT#),'ARIO7GU',MSGCLASS=H,CLASS=A,
//             REGION=4M,MSGLEVEL=(1,1),NOTIFY=&SYSUID,TIME=(0,30)
//* **************************************************************
//* *                                                            *
//* *                     ESTIAC INSTITUT                        *
//* *                                                            *
//* *           UNITE DE FORMATION COBOL PROGRAMMATION           *
//* *                                                            *
//* *   EXECUTION DU PROGRAMME ARIO7GU CORRESPONDANT AU TP N°7   *
//* *                                                            *
//* **************************************************************
//* **************************************************************
//* * PREMIERE ETAPE:                                            *
//* * EXECUTION DU PROGRAMME PRINCIPALE                          *
//* **************************************************************
//TP7     EXEC PGM=ARIO7GU
//* **************************************************************
//* * DECLARATION DE LA BIBLIOTHEQUE DU LOAD MODULE              *
//* **************************************************************
//STEPLIB  DD  DSN=ARISYS.ADREF.XV99R00.LOAD,DISP=SHR
//* **************************************************************
//* * IMPRESSION                                                 *
//* **************************************************************
//SYSOUT   DD  SYSOUT=*
//SYSABOUT DD  SYSOUT=*
//SYSDBOUT DD  SYSOUT=*
//* **************************************************************
//* * DONNEES EN ENTREE DE LA SYSIN                              *
//* **************************************************************
//SYSIN    DD  *
16
01
03
30
15
07
31
23
04
29
27
$$
/*
