//ARI0409J JOB 'RUNSTATS',CLASS=A,MSGCLASS=H,
//             NOTIFY=,REGION=4M,COND=(8,LT),
//             RESTART=*,TIME=(0,15)
//*===================================================================
//* RUNSTATS
//*===================================================================
//RUNSTATS EXEC PGM=DSNUTILB,PARM='DB9G,,',COND=(8,LT)
//STEPLIB  DD DSN=DSN910.SDSNEXIT,DISP=SHR
//         DD DSN=DSN910.RUNLIB.LOAD,DISP=SHR
//         DD DSN=DSN910.SDSNLOAD,DISP=SHR
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
RUNSTATS
//
