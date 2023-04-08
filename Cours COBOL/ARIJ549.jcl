//ARI0409E  JOB (ACCT#),'ARIO549',CLASS=A,MSGCLASS=H,
//          NOTIFY=&SYSUID,TIME=(0,30),COND=(8,LT)
//JOBLIB   DD  DSN=ARISYS.ADREF.XV99R00.LOAD,DISP=SHR
//*
//* **************************************************************
//* *                                                            *
//* *                     ESTIAC INSTITUT                        *
//* *                                                            *
//* *           UNITE DE FORMATION COBOL PROGRAMMATION           *
//* *                                                            *
//* *   EXECUTION DU PROGRAMME ARIO549 CORRESPONDANT AU TP N°5   *
//* *                                                            *
//* **************************************************************
//*
//* **************************************************************
//* * ETAPE 1 : SUPPRESSION DU FICHIER FCPTE CREE PAR            *
//* * LES PRECEDENTE EXECUTION DU PROGRAMME                      *
//* * SI LE FICHIER N'EXISTAIT PAS, ON MODIFIE LE RC POUR EVITER *
//* * UN ARRET DE L'EXECUTION                                    *
//* *                                                            *
//* * ETAPE 2 : DEFINITION DU CLUSTER KSDS FCPTE DE LA           *
//* * CLE PRINCIPALE                                             *
//* *                                                            *
//* * ETAPE 3 : REMPLISSAGE DU FICHIER KSDS FCPTE A              *
//* * PARTIR DU FICHIER QSAM FCPTE                               *
//* *                                                            *
//* * ETAPE 4 : DEFINITION DE LA CLE SECONDAIRE                  *
//* *                                                            *
//* * ETAPE 5 : DEFINITION DU CHEMIN D'ACCES                     *
//* *                                                            *
//* * ETAPE 6 : IMPRESSION DU KSDS POUR POUVOIR VOIR LE          *
//* * CONTENU DU FICHIER                                         *
//* **************************************************************
//*
//DELDEF  EXEC PGM=IDCAMS
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  *
   DELETE (ARI04.ARI0409.KCPTE5) CLUSTER
   IF MAXCC = 8 THEN SET MAXCC = 0
   DEFINE CLUSTER (NAME(ARI04.ARI0409.KCPTE5)         -
                   VOLUME(WRK001)                            -
                   TRACKS(3 1)                               -
                   FREESPACE(20 20)                          -
                   KEYS(10 0)                                -
                   RECORDSIZE(80 80)                         -
                   INDEXED)                                  -
          DATA    (NAME(ARI04.ARI0409.KCPTE5.D)) -
          INDEX   (NAME(ARI04.ARI0409.KCPTE5.I))
   REPRO INDATASET(ARI04.ARI0409.TP5.FCPTE)          -
         OUTDATASET(ARI04.ARI0409.KCPTE5)
   DEFINE AIX     (NAME(ARI04.ARI0409.KCPTE5.AIX) -
                   VOLUME(WRK001)                            -
                   RELATE(ARI04.ARI0409.KCPTE5) -
                   TRACKS(3 1)                               -
                   NONUNIQUEKEY                              -
                   UPGRADE                                   -
                   FREESPACE(20 20)                          -
                   KEYS(20 10)                               -
                   RECORDSIZE(65 65))                        -
          DATA    (NAME(ARI04.ARI0409.KCPTE5.AIX.D)) -
          INDEX   (NAME(ARI04.ARI0409.KCPTE5.AIX.I))
      DEFINE PATH (NAME(ARI04.ARI0409.KCPTE5.PATH) -
                   PATHENTRY(ARI04.ARI0409.KCPTE5.AIX)-
                   UPDATE)
         BLDINDEX  INDATASET(ARI04.ARI0409.KCPTE5) -
                   OUTDATASET (ARI04.ARI0409.KCPTE5.AIX)-
                   INTERNALSORT
   PRINT INDATASET(ARI04.ARI0409.KCPTE5)
/*
//*
//* **************************************************************
//* * ETAPE 7                                                    *
//* * EXECUTION DU PROGRAMME                                     *
//* **************************************************************
//STARIO5 EXEC PGM=ARIO549
//STEPLIB  DD  DSN=ARI04.ARI0409.LOAD,DISP=SHR
//* **************************************************************
//* * IMPRESSION                                                 *
//* **************************************************************
//SYSPRINT DD  SYSOUT=*
//SYSABOUT DD  SYSOUT=*
//SYSDBOUT DD  SYSOUT=*
//SYSOUT   DD  SYSOUT=*,OUTLIM=800
//* **************************************************************
//* * FICHIER KSDS EN INPUT                                      *
//* **************************************************************
//INP001   DD  DISP=SHR,DSN=ARI04.ARI0409.KCPTE5
//INP0011  DD  DISP=SHR,DSN=ARI04.ARI0409.KCPTE5.PATH
//* **************************************************************
//* * IMPRESSION DES RELEVES                                     *
//* **************************************************************
//ETATCLI  DD  SYSOUT=*,OUTLIM=500
//ETATANO  DD  SYSOUT=*,OUTLIM=500
//* **************************************************************
//* * DONNE EN ENTREE DE LA SYSIN                                *
//* **************************************************************
//SYSIN    DD  *
BDUPONT        DUPONT              DUPONT
ACARLIN REMIS  00000000010001400000
BDUPONT        BERNARD             ZZ
ACARLIN REMIS  00000000010001700000
ACARLIN REMIS  00011000010001400000
BCARLIN REGIS  AZRANI PAUL         DATAIN BRUNO
B              DALTON              NORBERT
AREDBERG FRANCKOOOOOOOOO10000000010
AREDBERG FRANCK0000000001OOOOOOOO10
BCARLIN REGIS  PETERSEN SVEN       ZHORC EDDY
BCARLIN REGIS  PETERSEN SVEN       ZREDAH HERVE
BBOHR ADRIANA  ZHORC EDDY          PETERSEN SVEN
AREDBERG FRANCK00000000450000000001
BCARLIN REGIS  0000000000000000000100000000007777777777
B              0000000000000000    DELTIERY ROMAIN
B              MAURICE             111111111111111
TBOHR ADRIANA  00000000450000000001
3BOHR ADRIANA  00000000450000000001
AREDBERG FRANCK00000000260000000030
F
$$$
//
