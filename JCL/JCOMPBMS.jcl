//ARI0409C JOB 'COMPBMS',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),
// NOTIFY=&SYSUID
//*
//* *=================================================================*
//* *                  E S T I A C   I N S T I T U T                  *
//* *                                                                 *
//* *                    ETAPE DE COMPILATION MAP                     *
//* *                                                                 *
//* * AVANT DE COMPILER VOTRE MAP VOUS DEVEZ REMPLACER LE CHEMIN      *
//* * D'ACCES DE LA SRCELIB, LOADLIB, DSECTLIB PAR LE CHEMIN DE VOTRE *
//* * BIBLIOTHEUQUE COMME DANS L'EXEMPLE SUIVANT:                     *
//* *                                                                 *
//* * REMPLACER DSN=ARISYS.ADREF.XV99R00.BMS                          *
//* * PAR       DSN=ARIGG.ARIGGUU.BMS                                 *
//* *                                                                 *
//* * OU GG EST VOTRE GROUPE ET UU VOTRE USER                         *
//* *                                                                 *
//* * REMPLACER LE NOM DE VOTRE MAPS A LA PLACE DE ARINGUX            *
//* *                                                                 *
//* * SRCELIB  : CHEMIN D'ACCES DU FICHIER SOURCE                     *
//* * LOADLIB  : CHEMIN D'ACCES DU LOAD                               *
//* * DSECTLIB : CHEMIN D'ACCES DES COPIES                            *
//* *                                                                 *
//* *=================================================================*
//        JCLLIB ORDER=(ARISYS.ADREF.XV99R00.BMS.ISPSLIB)
//*
//* CHARGEMENT DES BIBLIOTHEQUES
//*
// SET SRCELIB=ARI04.ARI0409.BMS
// SET LOADLIB=ARISYS.ADREF.XV99R00.CICS.LOAD
// SET DSECTLIB=ARISYS.ADREF.XV99R00.CICS.CPY
//*
//* COMPILATION DE LA MAP
//*
//ARIN491 EXEC PCOMPBMS,MEMBER=ARIN491
//
