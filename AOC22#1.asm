//*
//ASM       EXEC PGM=ASMA90,PARM='NODECK,XREF(SHORT)'
//SYSLIB    DD  DSN=SYS1.MACLIB,DISP=SHR
//          DD  DSN=SYS1.MODGEN,DISP=SHR
//*         DD  DSN=SYS1.ISFMAC,DISP=SHR
//SYSUT1    DD UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT2    DD UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSUT3    DD UNIT=SYSDA,SPACE=(CYL,(10,5))
//SYSPRINT  DD SYSOUT=*
//SYSLIN    DD SPACE=(CYL,10),UNIT=VIO,DISP=(,PASS)
//SYSIN     DD *,SYMBOLS=EXECSYS
         PRINT ON,NODATA,NOGEN
PROG1    CSECT
* STANDARD LINKAGE
**********************************************************************
         STM   R14,R12,12(R13)        SAVE CALLER'S REGS
         BASR  R12,R0                 ESTABLISH
         USING *,R12                  ADDRESSABILITY
         ST    R13,SAVEAREA+4         BACK-CHAIN CALLER'S FROM MINE
         LA    R13,SAVEAREA           POINT TO MY LOWER-LEVEL SA
**********************  BEGIN LOGIC  *********************************
         OPEN  (FILEIN,(INPUT))
         OPEN  (FILEOUT,(OUTPUT))
*
         SR    R7,R7          Clear R7 - Will have the max value
         SR    R4,R4          Clear R4 - Will have the current addition
GET_LINE GET   FILEIN,RECIN
         MVC   RECOUT,BLANK   Blank Output
         MVC   RECOUT(8),RECIN
         LA    R5,RECIN            START OF SCAN
         LA    R6,80(0,R5)         END OF SCAN
         LA    0,C' '              GET NEXT " " blank
DD_SRST  SRST  R6,R5
*        BC    1,DD_SRST           LOOP IF CHAR/CPU LIMIT
         BC    2,BOOM              blank not fOUND, abend
         LA    R5,1(R5)            Calc length of ### -1 for EX
         SR    R6,R5               get difference
         JM    BLANK_LINE
*        ST    R6,RECOUT+10        store in output just to see
         EX    R6,PACK             EX PACK  CPACKED,RECIN(0)
*        MVC   RECOUT+20(L'CPACKED),CPACKED
         CVB   R2,CPACKED
         AR    R4,R2
*        ST    R2,RECOUT+20        store in output just to see
*        LA    R2,L'TEMPNUM
*        SR    R2,R6               get position for insert
*        PACK  CPACKED,RECIN(4)
*        CVB   R2,CPACKED
*        PUT   FILEOUT,RECOUT
         J     GET_LINE
*
BLANK_LINE EQU *
         MVC   RECOUT,=CL80'  ** NEW ELF **  '
         CR    R4,R7          Is this max value?
         JL    NOT_MAXVAL     Nope
         LR    R7,R4          Yes, save max register
NOT_MAXVAL EQU *
*        ST    R7,BIN_VAL
*        UNPK  OUT_VAL(9),BIN_VAL(5)  UNPACK
*        TR    OUT_VAL(8),HEXTAB-240 TRANSLATE
         CVD   R7,CPACKED
         UNPK  OUT_VAL(9),CPACKED(L'CPACKED+1)
         MVI   OUT_VAL+7,C' '  Get rid of the "C" for packed decimal
         ST    R7,RECOUT+30        store in output just to see
         ST    R4,RECOUT+20        store in output just to see
         SR    R4,R4          Clear R4 - Will have the current addition
         PUT   FILEOUT,RECOUT
         J     GET_LINE
*
FILE#EOD EQU   *
         CLOSE FILEIN
         CLOSE FILEOUT
*********************** STARDARD EXIT ********************************
EXIT     EQU   *                      BRANCH TO HERE FOR NORMAL RETURN
         L     R13,SAVEAREA+4         POINT TO CALLER'S SAVE AREA
         LM    R14,R12,12(R13)        RESTORE CALLER'S REGS
         LA    R15,0                  SET RETURN CODE REG 15 = 0
         BR    R14
BOOM     EQU   *                      BRANCH TO HERE FOR NORMAL RETURN
         ABEND 1
**********************  DATA AREAS   *********************************
FILEIN   DCB   DSORG=PS,                                               X
               MACRF=(GM),                                             X
               DEVD=DA,                                                X
               DDNAME=FILEIN,                                          X
               EODAD=FILE#EOD,                                         X
               RECFM=FB,                                               X
               LRECL=80
*
FILEOUT  DCB   DSORG=PS,                                               X
               MACRF=(PM),                                             X
               DEVD=DA,                                                X
               DDNAME=FILEOUT,                                         X
               RECFM=FB,                                               X
               LRECL=80
RECIN    DS    CL80       INPUT AREA FOR RECORDS
BLANK    DC    C' '       TO BLANK RECOUT
*
BIN_VAL  DS    X'FFFFFFFF',X'FF'
* Record out
RECOUT   DS    CL80       OUTPUT AREA FOR RECORDS
         ORG   *-L'OUT_VAL
OUT_VAL  DC    CL8'XXXXXXXX',C'<'
*
TEMPNUM  DS    CL8
CPACKED  DS    D
PACK     PACK  CPACKED,RECIN(0)
ZEROES   DS    8C'0'
SAVEAREA DC    18F'0'     AREA FOR MY CALLEE TO SAVE & RESTORE MY REGS
*
HEXTAB   DC    C'0123456789ABCDEF'
*
         LTORG
         YREGS
         END
/*
//LINK    EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=SHR,DSN=LEO.LOAD(AOC22#1)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//*
//GO        EXEC PGM=AOC22#1,COND=(0,NE)
//STEPLIB  DD DISP=SHR,DSN=LEO.LOAD
//SYSPRINT DD SYSOUT=*
//DATASORT DD SYSOUT=*
//FILEIN   DD  *
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000

/*
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
