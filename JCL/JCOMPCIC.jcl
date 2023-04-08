//ARI0409C JOB 'COMPCICS',MSGCLASS=H,CLASS=A,
//             REGION=4M,MSGLEVEL=(1,1),NOTIFY=&SYSUID,COND=(4,LT),
//             RESTART=*
//* *=================================================================*
//* *                  E S T I A C   I N S T I T U T                  *
//* *                                                                 *
//* *                PRECOMPILATION ET COMPILATION CICS               *
//* *                                                                 *
//* * AVANT DE COMPILER VOTRE PROGRAMME VOUS DEVEZ REMPLACER LE CHEMIN*
//* * D'ACCES A VOTRE PROGRAMME DANS LA TRN.SYSIN COMME DANS L'EXEMPLE*
//* * SUIVANT :                                                       *
//* *                                                                 *
//* * REMPLACER GG PAR VOTRE GROUPE , UU PAR VOTRE USER ET X LE NUMERO*
//* * DE VOTRE PROGRAMME                                              *
//* *                                                                 *
//* * DANS LES PARAMETRES:                                            *
//* * CPY= CHEMIN D'ACCES DES COPY                                    *
//* * LOAD= CHEMIN D'ACCES DU LOAD                                    *
//* * MEMBER= NOM DU PROGRAMME                                        *
//* *                                                                 *
//* *=================================================================*
//*
//        JCLLIB ORDER=(ARISYS.ADREF.XV99R00.CICS.ISPSLIB)
//*
//* APPEL DU PROGRAMME DE PRECOMPILATION ET DE COMPILATION CICS
//*
//APPLPROG EXEC PCOMPCIC,
//       INDEX='DFH320.CICS',
//       DSCTLIB='DFH320.CICS.SDFHCOB',
//       OUTC=*,                     Class for print output
//       REG=4M,                     Region size for all steps
//       LNKPARM='LIST,XREF',        Link edit parameters
//       WORK=SYSDA,                 Unit for work datasets
//       CPY='ARISYS.ADREF.XV99R00.CICS.CPY',
//       LOAD='ARISYS.ADREF.XV99R00.CICS.LOAD',
//       MEMBER='ARIC491'
//*
//* PROGRAMME SOURCE A COMPILER
//*
//TRN.SYSIN DD DSN=ARI04.ARI0409.SRC(ARIC491),DISP=SHR
/*
