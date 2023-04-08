//ARI0409E JOB (ACCT#),'ARIO249',MSGCLASS=H,CLASS=A,
//             REGION=4M,MSGLEVEL=(1,1),NOTIFY=&SYSUID,TIME=(0,30)
//*
//* **************************************************************
//* *                                                            *
//* *                     ESTIAC INSTITUT                        *
//* *                                                            *
//* *           UNITE DE FORMATION COBOL PROGRAMMATION           *
//* *                                                            *
//* *   EXECUTION DU PROGRAMME ARIO249 CORRESPONDANT AU TP N°2   *
//* *                                                            *
//* **************************************************************
//* * HISTORIQUE MAJ :                                           *
//* *    --> 02/11/21 | ETATCLI ET ETATANO SONT A DEFINIR.       *
//* *            MSI  | ANCIEN FICHIER ARIJ249$.                 *
//* **************************************************************
//*
//* **************************************************************
//* * PREMIERE ETAPE : SUPPRESSION DES EVENTUELS FICHIERS CREES  *
//* * PAR LE PROGRAMME PRINCIPAL                                 *
//* **************************************************************
//SUPETAT EXEC PGM=IEFBR14
//FILE1    DD  DSN=ARI04.ARI0409.ETATCLI,UNIT=3390,
//             VOL=SER=WRK001,DISP=(OLD,DELETE)
//FILE2    DD  DSN=ARI04.ARI0409.ETATANO,UNIT=3390,
//             VOL=SER=WRK001,DISP=(OLD,DELETE)
//*
//* **************************************************************
//* *DEUXIEME ETAPE : EXECUTION DU PROGRAMME PRINCIPAL           *
//* **************************************************************
//STARIO2 EXEC PGM=ARIO249
//SYSOUT   DD  SYSOUT=*,OUTLIM=800
//* **************************************************************
//* *DECLARATION DE LA BIBLIOTHEQUE QUI CONTIENT LE LOAD MODULE  *
//* **************************************************************
//STEPLIB  DD  DSN=ARI04.ARI0409.LOAD,DISP=SHR
//* **************************************************************
//* *DECLARATION DU FICHIER MOUVEMENT EN ENTREE                  *
//* **************************************************************
//INP001   DD  DSN=ARI04.ARI0409.FMVTS,DISP=SHR
//* **************************************************************
//* *IMPRESSION DES RELEVES : ETATCLI ET ETATANO                 *
//* **************************************************************
//*
//* ECRIRE ICI LES CARTES CORRESPONDANT AUX FICHIERS D'IMPRESSION
//ETATCLI  DD  DSN=ARI04.ARI0409.ETATCLI,DISP=(NEW,CATLG,DELETE),
//             VOL=SER=WRK001,UNIT=3390,SPACE=(TRK,(1,1)),
//             DCB=(LRECL=81,RECFM=FB,BLKSIZE=8100,DSORG=PS)
//*
//ETATANO  DD  DSN=ARI04.ARI0409.ETATANO,DISP=(NEW,CATLG,DELETE),
//             VOL=SER=WRK001,UNIT=3390,SPACE=(TRK,(1,1)),
//             DCB=(LRECL=81,RECFM=FB,BLKSIZE=8100,DSORG=PS)
