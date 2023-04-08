//ARI0409E JOB (ACCT#),'ARIO449',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
//             NOTIFY=&SYSUID,REGION=4M,TIME=(0,30)
//* **************************************************************
//* * DECLARATION DE LA BIBLIOTHEQUE DU LOAD MODULE              *
//* **************************************************************
//JOBLIB   DD  DSN=ARISYS.ADREF.XV99R00.LOAD,DISP=SHR
//*
//* **************************************************************
//* *                                                            *
//* *                     ESTIAC INSTITUT                        *
//* *                                                            *
//* *           UNITE DE FORMATION COBOL PROGRAMMATION           *
//* *                                                            *
//* *   EXECUTION DU PROGRAMME ARIO449 CORRESPONDANT AU TP N°4   *
//* *                                                            *
//* **************************************************************
//*
//* **************************************************************
//* * PREMIERE ETAPE : SUPPRESSION DU FICHIER CPTE-ES CREE PAR   *
//* * UNE PRECEDENTE EXECUTION DU PROGRAMME                      *
//* * SI LE FICHIER N'EXISTAIT PAS, ON MODIFIE LE RC POUR EVITER *
//* * UN ARRET DE L'EXECUTION                                    *
//* **************************************************************
//DELETE  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 DELETE ARI04.ARI0409.CPTES.KSDS
 IF LASTCC <= 8 THEN SET MAXCC = 0
/*
//* **************************************************************
//* * DEUXIEME ETAPE : DEFINITION DU CLUSTER KSDS CPTE-ES        *
//* **************************************************************
//DEFINE  EXEC  PGM=IDCAMS
//SYSPRINT  DD  SYSOUT=*
//SYSIN     DD  *
 DEFINE CLUSTER (NAME(ARI04.ARI0409.CPTES.KSDS)             -
                   VOLUME(WRK001)                           -
                   TRACKS(3 1)                              -
                   FREESPACE(20 20)                         -
                   KEYS(10 0)                               -
                   RECORDSIZE(50 50)                        -
                   INDEXED)                                 -
         DATA    (NAME(ARI04.ARI0409.CPTES.KSDS.D))         -
         INDEX   (NAME(ARI04.ARI0409.CPTES.KSDS.I))
/*
//* **************************************************************
//* * TROISIEME ETAPE : REMPLISSAGE DU FICHIER KSDS CPTE-ES      *
//* * A PARTIR DU FICHIER QSAM CPTETRI                           *
//* **************************************************************
//REPRO  EXEC  PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//DDIN     DD  DSN=ARI04.ARI0409.CPTETRI,DISP=SHR
//DDOUT    DD  DSN=ARI04.ARI0409.CPTES.KSDS,DISP=SHR
//SYSIN    DD  *
 REPRO INFILE(DDIN)   -
       OUTFILE(DDOUT)
 PRINT INDATASET(ARI04.ARI0409.CPTES.KSDS)
/*
//*
//* **************************************************************
//* * QUATRIEME ETAPE : EXECUTION DU PROGRAMME ARIO449           *
//* **************************************************************
//STARIO4 EXEC PGM=ARIO449
//STEPLIB  DD  DSN=ARI04.ARI0409.LOAD,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*,OUTLIM=800
//* **************************************************************
//* * DECLARATION DES FICHIERS                                   *
//* **************************************************************
//INP001   DD  DSN=ARI04.ARI0409.FMVTS,DISP=SHR
//IO001    DD  DSN=ARI04.ARI0409.CPTES.KSDS,DISP=SHR
//ETATCLI  DD  SYSOUT=*,OUTLIM=500
//ETATANO  DD  SYSOUT=*,OUTLIM=500
//*
//*
//* **************************************************************
//* * CINQUIEME ETAPE : EDITION DES FICHIERS                     *
//* **************************************************************
//EDITION EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
 PRINT INDATASET(ARI04.ARI0409.CPTETRI) -
       CHARACTER
 PRINT INDATASET(ARI04.ARI0409.CPTES.KSDS) -
       CHARACTER
 PRINT INDATASET(ARI04.ARI0409.FMVTS) -
       CHARACTER
/*
//
