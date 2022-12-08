//*
//C         EXEC PGM=ASMA90,PARM='NODECK,XREF(SHORT)'
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
         SR    R4,R4          Clear R4 - Will have the current addition
GET_LINE GET   FILEIN,RECIN
         MVC   RECOUT,BLANK   Blank Output
         MVC   RECOUT(15),RECIN
* Get first value
         LA    R5,RECIN            START OF SCAN
         LA    R6,RECIN+L'RECIN    END OF SCAN
         LA    0,C'-'              GET NEXT dash
         SRST  R6,R5
         BC    2,BOOM              not found, abend
         LA    R2,1(0,R6)          save pos. of next number on R2
         SR    R6,R5               get difference
         AHI   R6,-1               Calc length of ### -1 for EX
         JM    BOOM                Abend if negative
         EX    R6,PACK             EX PACK  CPACKED,0(0,R5)
         CVB   R7,CPACKED          Save first value on R7
*        CVD   R2,CPACKED
         UNPK  RECOUT+20(9),CPACKED(L'CPACKED+1)
         MVI   RECOUT+20+7,C' '  Get rid of the "C" for packed decimal
* Get second value
         LR    R5,R2               Start of SCAN
         LA    R6,RECIN+L'RECIN    END OF SCAN
         LA    0,C','              GET NEXT comma
         SRST  R6,R5
         BC    2,BOOM              not found, abend
         LA    R2,1(0,R6)          save pos. of next number on R2
         SR    R6,R5               get difference
         AHI   R6,-1               Calc length of ### -1 for EX
         JM    BOOM                Abend if negative
         EX    R6,PACK             EX PACK  CPACKED,0(0,R5)
         CVB   R8,CPACKED          Save second value on R8
*        CVD   R2,CPACKED
         UNPK  RECOUT+30(9),CPACKED(L'CPACKED+1)
         MVI   RECOUT+30+7,C' '  Get rid of the "C" for packed decimal
* Get third value
         LR    R5,R2               Start of SCAN
         LA    R6,RECIN+L'RECIN    END OF SCAN
         LA    0,C'-'              GET NEXT dash
         SRST  R6,R5
         BC    2,BOOM              not found, abend
         LA    R2,1(0,R6)          save pos. of next number on R2
         SR    R6,R5               get difference
         AHI   R6,-1               Calc length of ### -1 for EX
         JM    BOOM                Abend if negative
         EX    R6,PACK             EX PACK  CPACKED,0(0,R5)
         CVB   R9,CPACKED          Save third value on R9
*        CVD   R2,CPACKED
         UNPK  RECOUT+40(9),CPACKED(L'CPACKED+1)
         MVI   RECOUT+40+7,C' '  Get rid of the "C" for packed decimal
* Get forth value
         LR    R5,R2               Start of SCAN
         LA    R6,RECIN+L'RECIN    END OF SCAN
         LA    0,C' '              GET NEXT space
         SRST  R6,R5
         BC    2,BOOM              not found, abend
         LA    R2,1(0,R6)          save pos. of next number on R2
         SR    R6,R5               get difference
         AHI   R6,-1               Calc length of ### -1 for EX
         JM    BOOM                Abend if negative
         EX    R6,PACK             EX PACK  CPACKED,0(0,R5)
         CVB   R10,CPACKED         Save forth value on R10
*        CVD   R2,CPACKED
         UNPK  RECOUT+50(9),CPACKED(L'CPACKED+1)
         MVI   RECOUT+50+7,C' '  Get rid of the "C" for packed decimal
*
* Got all the values, now we need to figure out if:
*   (R7 < R9 & R8 < R9) OR (R7 > R10)
*
         CR    R7,R9
         JNL   CHECK2    R7 >= R9
         CR    R8,R9
         JNL   CHECK2    R8 >= R9
         J     NOPE
CHECK2   CR    R7,R10
         JH    NOPE
         J     YEP
YEP      MVC   RECOUT+60(10),=CL10' *YEP* '
         AHI   R4,1
         CVD   R4,CPACKED
         UNPK  RECOUT+70(9),CPACKED(L'CPACKED+1)
         MVI   RECOUT+70+7,C' '  Get rid of the "C" for packed decimal
         J     GO_PUT
NOPE     MVC   RECOUT+60(10),=CL10' *NOPE* '
GO_PUT   EQU   *
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
PACK     PACK  CPACKED,0(0,R5)
ZEROES   DS    8C'0'
SAVEAREA DC    18F'0'     AREA FOR MY CALLEE TO SAVE & RESTORE MY REGS
*
HEXTAB   DC    C'0123456789ABCDEF'
*
         LTORG
         YREGS
         END
/*
//L       EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=(,PASS),UNIT=SYSALLDA,SPACE=(CYL,(1,1,1)),
//         DSN=&&GOSET(GO)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.C.SYSLIN
//*
//G        EXEC PGM=*.L.SYSLMOD,COND=((8,LT,C),(8,LT,L))
//FILEIN   DD  *
2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
/*
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
