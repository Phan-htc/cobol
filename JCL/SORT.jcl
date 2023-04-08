//ARI0409A JOB (ACCT#),CLASS=A,MSGCLASS=X,
//    MSGLEVEL=(1,1),NOTIFY=&SYSUID
//*
//* ********************************************************************
//*                                                                    *
//*  EXEMPLE DE TRI                                                    *
//*                                                                    *
//* ********************************************************************
//*
//STEPSORT EXEC PGM=SORT
//SORTIN   DD  DSN=ARI04.ARI0409.CPTNOTRI,DISP=SHR
//SORTOUT  DD  DSN=ARI04.ARI0409.CPTETRI,DISP=(NEW,CATLG,DELETE),
//             VOL=SER=WRK001,UNIT=3390,
//             SPACE=(TRK,(1,1))
//SORTWK01 DD   UNIT=SYSALLDA,SPACE=(CYL,(1,1))
//SORTWK02 DD   UNIT=SYSALLDA,SPACE=(CYL,(1,1))
//SYSOUT   DD   SYSOUT=*
//SYSIN    DD   *
 SORT FIELDS=(1,10,CH,A)
/*
