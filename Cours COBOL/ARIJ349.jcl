//ARI0409E JOB (ACCT#),'ARIO349',MSGCLASS=H,CLASS=A,
//             REGION=4M,MSGLEVEL=(1,1),NOTIFY=&SYSUID,TIME=(0,30)
//*
//* **************************************************************
//* *                                                            *
//* *                     ESTIAC INSTITUT                        *
//* *                                                            *
//* *           UNITE DE FORMATION COBOL PROGRAMMATION           *
//* *                                                            *
//* *   EXECUTION DU PROGRAMME ARIO349 CORRESPONDANT AU TP N°3   *
//* *                                                            *
//* **************************************************************
//*
//* **************************************************************
//* * PREMIERE ETAPE : SUPPRESSION DES FICHIERS CREES PAR LE     *
//* * PROGRAMME PRINCIPAL                                        *
//* **************************************************************
//SUPFCPT EXEC PGM=IEFBR14
//FILE1    DD  DSN=ARI04.ARI0409.FCPTS,
//             UNIT=3390,VOL=SER=WRK001,DISP=(OLD,DELETE)
//*
//* **************************************************************
//* * DEUXIEME ETAPE : EXECUTION DU PROGRAMME PRINCIPAL          *
//* **************************************************************
//STARIO3 EXEC PGM=ARIO349
//* **************************************************************
//* * DECLARATION DE LA BIBLIOTHEQUE SUR LOAD MODULE             *
//* **************************************************************
//STEPLIB  DD  DSN=ARI04.ARI0409.LOAD,DISP=SHR
//SYSOUT   DD  SYSOUT=*,OUTLIM=800
//* **************************************************************
//* * DECLARATION DES FICHIERS EN ENTREE                         *
//* **************************************************************
//INP001   DD  DSN=ARI04.ARI0409.FMVTS,DISP=(OLD,KEEP),
//             UNIT=3390,VOL=SER=WRK001
//INP002   DD  DSN=ARI04.ARI0409.FCPTE,DISP=(OLD,KEEP),
//             UNIT=3390,VOL=SER=WRK001
//* **************************************************************
//* * DECLARATION DES FICHIERS EN SORTIE                         *
//* **************************************************************
//OUT001   DD  DSN=ARI04.ARI0409.FCPTS,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=3390,VOL=SER=WRK001,
//             SPACE=(TRK,(1,1)),
//             DCB=(RECFM=FB,LRECL=50,BLKSIZE=5000)
//ETATCLI  DD  SYSOUT=*,OUTLIM=800
//ETATANO  DD  SYSOUT=*,OUTLIM=800
//
