//ARI0409C JOB (ACCT#),'ARIOT49',MSGCLASS=H,REGION=4M,
//    CLASS=A,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*===================================================================*
//*                         ESTIAC   INSTITUT                         *
//*                                                                   *
//*                      ETAPE DE COMPILATION COBOL                   *
//*                                                                   *
//* POUR COMPILER VOTRE JOB COBOL VOUS DEVEZ REMPLACER                *
//* LES PARAMETRES PAR :                                              *
//*                                                                   *
//*  MBR : NOM DU SOURCE                                              *
//*  SRC : CHEMIN D'ACCES DU FICHIER SOURCE                           *
//*  LMOD : CHEMIN D'ACCES DU LOAD                                    *
//*  COB.SYSLIB : CHEMIN D'ACCES DES COPY                             *
//*                                                                   *
//*===================================================================*
//*
//        JCLLIB ORDER=(ARISYS.ADREF.XV99R00.COBOL.ISPSLIB)
//*
//* ETAPE DE COMPILATION DU PROGRAMME COBOL
//*
//STEP1   EXEC PCOMPCOB,
//         MBR=ARIT149,
//         SRC=ARI04.ARI0409.SRC,
//         LMOD=ARI04.ARI0409.LOAD
//COB.SYSLIB DD DSN=ARI04.ARI0409.CPY,DISP=SHR
//
