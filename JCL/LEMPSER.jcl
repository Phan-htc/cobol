//ARIGGUUA JOB (ACCT#),'LOAD',CLASS=A,MSGCLASS=X,
//             NOTIFY=&SYSUID,REGION=4M,COND=(8,LT),
//             RESTART=*,TIME=(0,15)
//*===================================================================*
//*                       EXECUTION DU LOAD                           *
//*                                                                   *
//* ATTENTION! AVANT DE SOUMETTRE CE JOB, MODIFIEZ LE JOBNAME (1ERE   *
//*     LIGNE) EN REMPLACANT GGUU PAR LE NUMERO DE VOTRE USERID       *
//*===================================================================*
//*===================================================================*
//LOAD     EXEC PGM=DSNUTILB,PARM='DB9G,,',COND=(8,LT)
//STEPLIB  DD DSN=DSN910.SDSNLOAD,DISP=SHR
//SORTWK01 DD DSN=&&SORTWK01,
//            DISP=(MOD,DELETE,CATLG),
//            SPACE=(16384,(20,20),,,ROUND),UNIT=3390
//SORTWK02 DD DSN=&&SORTWK02,
//            DISP=(MOD,DELETE,CATLG),
//            SPACE=(16384,(20,20),,,ROUND),UNIT=3390
//SYSREC1  DD DSN=ARISYS.ADREF.XV99R00.DB2.DATA(EMPLOYE),DISP=SHR
//SYSREC2  DD DSN=ARISYS.ADREF.XV99R00.DB2.DATA(SERVIS),DISP=SHR
//SYSDISC  DD DSN=&&SYSDISC,DISP=(MOD,CATLG),
//            SPACE=(16384,(20,20),,,ROUND),UNIT=3390
//SYSUT1   DD DSN=&&SYSUT1,DISP=(MOD,DELETE,CATLG),
//            SPACE=(16384,(20,20),,,ROUND),UNIT=3390
//SORTOUT  DD DSN=&&SORTOUT,DISP=(MOD,DELETE,CATLG),
//            SPACE=(16384,(20,20),,,ROUND),UNIT=3390
//SYSERR   DD DSN=&&SYSERR,DISP=(MOD,DELETE,CATLG),
//            SPACE=(16384,(20,20),,,ROUND),UNIT=3390
//SYSMAP   DD DSN=&&SYSMAP,DISP=(MOD,DELETE,CATLG),
//            SPACE=(16384,(20,20),,,ROUND),UNIT=3390
//SYSPRINT DD SYSOUT=*
//UTPRINT  DD SYSOUT=*
//SYSIN    DD *

//
