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
PROG1    AMODE 31
PROG1    RMODE 24
* STANDARD LINKAGE
**********************************************************************
         STM   R14,R12,12(R13)        SAVE CALLER'S REGS
         BASR  R12,R0                 ESTABLISH
         USING *,R12                  ADDRESSABILITY
         ST    R13,SAVEAREA+4         BACK-CHAIN CALLER'S FROM MINE
         LA    R13,SAVEAREA           POINT TO MY LOWER-LEVEL SA
**********************  BEGIN LOGIC  *********************************
         OPEN  (STACKINI,(INPUT))
         OPEN  (FILEIN,(INPUT))
         OPEN  (FILEOUT,(OUTPUT))
*
         SR    R4,R4          Clear R4 - Will have the current addition
* R7 will have the count to move
* R8 will have address of move from
* R9 will have address of move to
* R11 will have storage for the piles
         STORAGE OBTAIN,LENGTH=STACKMAX*STACKNUM,LOC=31,ADDR=(R11)
         LR    R2,R11         Start R2, will have Stack offset
GET_INI  GET   STACKINI,0(R2)
         LA    R2,STACKMAX(,R2)   Go to next stack
         J     GET_INI            Go get next
INIT#EOD EQU   *
GET_LINE GET   FILEIN,RECIN
         MVC   RECOUT,BLANK   Blank Output
         MVC   RECOUT(15),RECIN
         MVC   RECOUT+20(8),0*STACKMAX(R11)
         MVC   RECOUT+30(8),1*STACKMAX(R11)
         MVC   RECOUT+40(8),2*STACKMAX(R11)
*
* Get first value
         LA    R5,RECIN+5          START OF SCAN (after the "move")
         LA    R6,RECIN+L'RECIN    END OF SCAN
         LA    0,C' '              GET NEXT space
         SRST  R6,R5
         BC    4,J1                Found
         ABEND 1
J1       EQU   *
         LA    R2,6(0,R6)          save pos. of next number on R2
         SR    R6,R5               get difference
         AHI   R6,-1               Calc length of ### -1 for EX
         JNM   J2                  Abend if negative
         ABEND 2
J2       EQU   *
         EX    R6,PACK             EX PACK  CPACKED,0(0,R5)
         CVB   R7,CPACKED          Save first value on R7
*        UNPK  RECOUT+50(9),CPACKED(L'CPACKED+1)
*        MVI   RECOUT+50+7,C' '  Get rid of the "C" for packed decimal
* Get second value
         LR    R5,R2               Start of SCAN
         LA    R6,RECIN+L'RECIN    END OF SCAN
         LA    0,C' '              GET NEXT space
         SRST  R6,R5
         BC    4,J3                Found
         ABEND 3
J3       EQU   *
         LA    R2,4(0,R6)          save pos. of next number on R2
         SR    R6,R5               get difference
         AHI   R6,-1               Calc length of ### -1 for EX
         JNM   J4                  Abend if negative
         ABEND 4
J4       EQU   *
         EX    R6,PACK             EX PACK  CPACKED,0(0,R5)
         CVB   R1,CPACKED          Save second value on R8
         AHI   R1,-1               First offset starts at zero not 1
         MHI   R1,STACKMAX         Multiply with stack size
*        CVD   R1,CPACKED          Convert to decimal again to display
*        UNPK  RECOUT+60(9),CPACKED(L'CPACKED+1)
*        MVI   RECOUT+60+7,C' '  Get rid of the "C" for packed decimal
*        Here we have the start of the stack, we need to find the end
         LA    0,C' '              GET NEXT space
         LA    R1,0(R1,R11)        Offset from the work area
         LA    R8,STACKMAX(,R1)    Get end of search
         SRST  R8,R1                 go search
         BC    4,J5                Found
         ABEND 5
J5       EQU   *
         SR    R8,R7               Go back to how many crates we pick
* Get third value
         LR    R5,R2               Start of SCAN
         LA    R6,RECIN+L'RECIN    END OF SCAN
         LA    0,C' '              GET NEXT space
         SRST  R6,R5
         BC    4,J6                Found
         ABEND 6
J6       EQU   *
*        LA    R2,1(0,R6)          save pos. of next number on R2
         SR    R6,R5               get difference
         AHI   R6,-1               Calc length of ### -1 for EX
         JNM   J7                  Abend if negative
         ABEND 7
J7       EQU   *
         EX    R6,PACK             EX PACK  CPACKED,0(0,R5)
         CVB   R1,CPACKED          Save third value on R9
         AHI   R1,-1               First offset starts at zero not 1
         MHI   R1,STACKMAX         Multiply with stack size
*        CVD   R1,CPACKED          Convert to decimal again to display
*        UNPK  RECOUT+70(9),CPACKED(L'CPACKED+1)
*        MVI   RECOUT+70+7,C' '  Get rid of the "C" for packed decimal
*        Here we have the start of the stack, we need to find the end
         LA    0,C' '              GET NEXT space
         LA    R1,0(R1,R11)        Offset from the work area
         LA    R9,STACKMAX(,R1)    Get end of search
         SRST  R9,R1                 go search
         BC    4,J8                Found
         ABEND 8
J8       EQU   *
* We have all offsets, time to move the crate
         LA    R0,C' ' Move until the blank
         MVST  R9,R8   Copy R8 into R9
         MVI   0(R8),C' '        Clear Crate from R8
         MVC   RECOUT+50(8),0*STACKMAX(R11)
         MVC   RECOUT+60(8),1*STACKMAX(R11)
         MVC   RECOUT+70(8),2*STACKMAX(R11)
         PUT   FILEOUT,RECOUT
         J     GET_LINE
*
FILE#EOD EQU   *
         MVC   RECOUT,BLANK   Blank Output
* End of file, let's spit out the top crates
         LA    R4,STACKNUM
         LA    R3,RECOUT    R3 will have offset into RECOUT
         LR    R2,R11       R2 will have offset into work
END_LOOP EQU *
         LA    R5,0(,R2)           START OF SCAN
         LA    R6,STACKMAX(,R2)    END OF SCAN
         LA    0,C' '              GET NEXT space
         SRST  R6,R5
         BC    4,J9                Found
         ABEND 9
J9       EQU   *
         AHI   R6,-1               Go back from space
         MVC   0(1,R3),0(R6)       Move last crate Char
         LA    R3,1(,R3)           Go to next recout char
         LA    R2,STACKMAX(,R2)    Go to next stack
         BCT   R4,END_LOOP
         PUT   FILEOUT,RECOUT
         STORAGE RELEASE,LENGTH=STACKMAX*STACKNUM,ADDR=(R11)
         CLOSE STACKINI
         CLOSE FILEIN
         CLOSE FILEOUT
*********************** STARDARD EXIT ********************************
EXIT     EQU   *                      BRANCH TO HERE FOR NORMAL RETURN
         L     R13,SAVEAREA+4         POINT TO CALLER'S SAVE AREA
         LM    R14,R12,12(R13)        RESTORE CALLER'S REGS
         LA    R15,0                  SET RETURN CODE REG 15 = 0
         BR    R14
**********************  DATA AREAS   *********************************
STACKINI DCB   DSORG=PS,                                               X
               MACRF=(GM),                                             X
               DEVD=DA,                                                X
               DDNAME=STACKINI,                                        X
               EODAD=INIT#EOD,                                         X
               RECFM=FB,                                               X
               LRECL=80
*
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
STACKMAX EQU   256
STACKNUM EQU   9
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
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//STACKINI DD  *
SCVN
ZMJHNS
MCTGJND
TDFJWRM
PFH
CTZHJ
DPRQFSLZ
CSLHDFPW
DSMPFNGZ
//FILEIN   DD  *
move 3 from 9 to 6
move 7 from 6 to 2
move 1 from 1 to 5
move 7 from 7 to 1
move 3 from 9 to 7
move 1 from 9 to 1
move 1 from 7 to 2
move 11 from 1 to 8
move 9 from 8 to 2
move 1 from 6 to 7
move 3 from 7 to 3
move 7 from 3 to 4
move 9 from 8 to 7
move 3 from 3 to 1
move 2 from 5 to 2
move 6 from 7 to 3
move 1 from 1 to 7
move 1 from 9 to 2
move 1 from 5 to 3
move 1 from 8 to 2
move 2 from 7 to 5
move 1 from 1 to 4
move 3 from 5 to 8
move 2 from 8 to 7
move 1 from 8 to 9
move 7 from 3 to 1
move 8 from 2 to 5
move 3 from 7 to 3
move 1 from 5 to 1
move 1 from 9 to 6
move 1 from 7 to 4
move 1 from 6 to 3
move 1 from 7 to 1
move 9 from 4 to 5
move 8 from 1 to 2
move 3 from 3 to 2
move 1 from 1 to 6
move 7 from 5 to 6
move 1 from 1 to 5
move 1 from 3 to 5
move 21 from 2 to 3
move 8 from 6 to 3
move 5 from 4 to 9
move 9 from 3 to 8
move 17 from 3 to 5
move 6 from 2 to 1
move 2 from 9 to 1
move 3 from 3 to 6
move 3 from 2 to 5
move 7 from 8 to 2
move 3 from 6 to 9
move 2 from 2 to 4
move 1 from 2 to 6
move 2 from 2 to 6
move 2 from 6 to 5
move 1 from 6 to 1
move 2 from 2 to 7
move 1 from 8 to 2
move 4 from 9 to 1
move 4 from 1 to 6
move 1 from 8 to 5
move 3 from 6 to 9
move 1 from 9 to 1
move 2 from 9 to 2
move 4 from 4 to 5
move 1 from 7 to 8
move 1 from 7 to 5
move 8 from 1 to 8
move 1 from 1 to 9
move 1 from 6 to 8
move 2 from 2 to 6
move 1 from 1 to 3
move 1 from 2 to 5
move 1 from 3 to 4
move 3 from 9 to 4
move 4 from 4 to 1
move 29 from 5 to 1
move 2 from 6 to 3
move 2 from 3 to 5
move 2 from 5 to 9
move 7 from 8 to 1
move 3 from 8 to 6
move 6 from 1 to 6
move 2 from 9 to 8
move 2 from 5 to 3
move 3 from 5 to 6
move 2 from 5 to 6
move 1 from 5 to 1
move 2 from 3 to 9
move 1 from 8 to 6
move 1 from 8 to 3
move 1 from 3 to 5
move 5 from 1 to 5
move 5 from 6 to 2
move 25 from 1 to 9
move 9 from 9 to 3
move 7 from 6 to 8
move 9 from 5 to 9
move 2 from 6 to 5
move 6 from 9 to 7
move 1 from 6 to 8
move 3 from 2 to 1
move 3 from 8 to 1
move 5 from 9 to 6
move 3 from 9 to 1
move 4 from 6 to 9
move 2 from 7 to 4
move 1 from 4 to 1
move 1 from 6 to 2
move 7 from 1 to 6
move 1 from 9 to 8
move 9 from 3 to 9
move 5 from 1 to 7
move 1 from 5 to 7
move 3 from 1 to 7
move 3 from 6 to 7
move 8 from 9 to 1
move 3 from 7 to 3
move 1 from 5 to 6
move 3 from 1 to 7
move 4 from 1 to 4
move 2 from 8 to 5
move 1 from 4 to 2
move 3 from 2 to 7
move 2 from 6 to 4
move 1 from 1 to 2
move 18 from 7 to 5
move 1 from 7 to 5
move 1 from 2 to 3
move 4 from 5 to 9
move 1 from 2 to 1
move 2 from 3 to 9
move 2 from 3 to 4
move 2 from 6 to 5
move 1 from 8 to 3
move 4 from 9 to 7
move 1 from 1 to 9
move 3 from 5 to 2
move 2 from 8 to 6
move 2 from 6 to 1
move 5 from 5 to 7
move 7 from 9 to 7
move 11 from 5 to 9
move 3 from 7 to 6
move 6 from 4 to 9
move 5 from 7 to 3
move 6 from 3 to 6
move 2 from 1 to 2
move 2 from 4 to 9
move 6 from 9 to 2
move 1 from 7 to 5
move 10 from 2 to 9
move 4 from 9 to 4
move 1 from 4 to 3
move 31 from 9 to 3
move 1 from 9 to 4
move 6 from 3 to 8
move 1 from 5 to 8
move 5 from 6 to 4
move 4 from 3 to 2
move 1 from 4 to 6
move 22 from 3 to 7
move 6 from 4 to 7
move 4 from 6 to 2
move 8 from 8 to 1
move 3 from 2 to 8
move 2 from 1 to 9
move 1 from 2 to 6
move 3 from 2 to 5
move 2 from 5 to 4
move 2 from 6 to 4
move 24 from 7 to 4
move 1 from 7 to 4
move 2 from 1 to 5
move 2 from 9 to 6
move 10 from 4 to 6
move 3 from 1 to 6
move 6 from 7 to 1
move 2 from 2 to 3
move 1 from 7 to 4
move 2 from 8 to 4
move 1 from 8 to 5
move 4 from 5 to 2
move 5 from 4 to 1
move 2 from 7 to 8
move 2 from 8 to 4
move 5 from 6 to 3
move 2 from 4 to 3
move 1 from 7 to 5
move 2 from 3 to 6
move 1 from 5 to 1
move 3 from 6 to 8
move 11 from 4 to 3
move 7 from 6 to 1
move 3 from 8 to 1
move 1 from 2 to 3
move 2 from 6 to 9
move 2 from 2 to 3
move 3 from 4 to 3
move 2 from 9 to 4
move 1 from 6 to 3
move 5 from 1 to 2
move 2 from 4 to 3
move 24 from 3 to 7
move 3 from 3 to 9
move 1 from 2 to 6
move 1 from 2 to 5
move 1 from 6 to 1
move 4 from 2 to 1
move 2 from 9 to 2
move 1 from 2 to 4
move 18 from 7 to 5
move 1 from 2 to 1
move 1 from 9 to 1
move 2 from 5 to 7
move 13 from 1 to 8
move 3 from 4 to 9
move 7 from 1 to 7
move 13 from 7 to 6
move 1 from 9 to 5
move 3 from 4 to 3
move 1 from 9 to 8
move 3 from 1 to 3
move 1 from 9 to 5
move 2 from 1 to 4
move 2 from 7 to 3
move 4 from 3 to 1
move 1 from 1 to 5
move 9 from 6 to 7
move 5 from 7 to 1
move 2 from 4 to 1
move 4 from 6 to 1
move 3 from 5 to 3
move 3 from 3 to 5
move 7 from 1 to 6
move 6 from 6 to 1
move 1 from 6 to 8
move 2 from 7 to 9
move 2 from 1 to 5
move 1 from 3 to 7
move 7 from 5 to 9
move 10 from 1 to 5
move 8 from 8 to 4
move 6 from 4 to 8
move 1 from 4 to 1
move 2 from 9 to 8
move 2 from 1 to 3
move 2 from 7 to 3
move 1 from 7 to 8
move 4 from 3 to 8
move 1 from 3 to 2
move 20 from 5 to 8
move 1 from 2 to 4
move 4 from 9 to 4
move 4 from 4 to 5
move 18 from 8 to 6
move 3 from 9 to 6
move 1 from 3 to 9
move 10 from 8 to 7
move 7 from 7 to 9
move 7 from 8 to 5
move 3 from 7 to 8
move 6 from 5 to 1
move 6 from 9 to 4
move 1 from 9 to 6
move 1 from 3 to 6
move 1 from 8 to 5
move 1 from 9 to 4
move 12 from 6 to 7
move 5 from 7 to 1
move 6 from 8 to 5
move 1 from 5 to 1
move 3 from 5 to 3
move 8 from 4 to 9
move 2 from 3 to 7
move 4 from 7 to 2
move 10 from 5 to 6
move 11 from 1 to 6
move 4 from 2 to 5
move 1 from 3 to 8
move 1 from 8 to 9
move 1 from 4 to 7
move 3 from 7 to 4
move 1 from 1 to 6
move 1 from 4 to 7
move 1 from 7 to 1
move 4 from 5 to 2
move 3 from 7 to 1
move 2 from 4 to 8
move 20 from 6 to 8
move 4 from 1 to 5
move 2 from 5 to 2
move 6 from 6 to 1
move 5 from 1 to 8
move 7 from 6 to 2
move 6 from 9 to 7
move 2 from 9 to 8
move 2 from 7 to 4
move 4 from 2 to 6
move 3 from 5 to 8
move 12 from 8 to 7
move 1 from 4 to 3
move 1 from 2 to 9
move 1 from 9 to 2
move 1 from 6 to 8
move 1 from 3 to 1
move 2 from 1 to 6
move 1 from 4 to 2
move 3 from 6 to 2
move 2 from 5 to 7
move 1 from 9 to 8
move 6 from 2 to 4
move 17 from 7 to 1
move 10 from 1 to 7
move 4 from 2 to 6
move 10 from 7 to 8
move 3 from 6 to 2
move 4 from 4 to 1
move 2 from 6 to 4
move 4 from 2 to 6
move 1 from 7 to 1
move 2 from 4 to 3
move 12 from 1 to 7
move 5 from 6 to 3
move 17 from 8 to 2
move 4 from 3 to 8
move 1 from 4 to 2
move 20 from 8 to 7
move 19 from 2 to 6
move 7 from 6 to 3
move 7 from 3 to 5
move 2 from 5 to 7
move 4 from 6 to 9
move 1 from 4 to 2
move 1 from 2 to 1
move 2 from 3 to 6
move 1 from 2 to 6
move 1 from 3 to 1
move 4 from 6 to 2
move 1 from 5 to 9
move 7 from 7 to 3
move 7 from 3 to 8
move 5 from 8 to 1
move 2 from 8 to 3
move 1 from 2 to 1
move 3 from 5 to 6
move 1 from 3 to 9
move 2 from 9 to 2
move 8 from 1 to 7
move 3 from 7 to 6
move 2 from 2 to 4
move 21 from 7 to 3
move 10 from 3 to 1
move 2 from 9 to 2
move 7 from 3 to 4
move 3 from 3 to 7
move 4 from 2 to 3
move 3 from 7 to 8
move 1 from 3 to 6
move 1 from 3 to 2
move 4 from 7 to 9
move 10 from 1 to 6
move 1 from 5 to 9
move 6 from 7 to 2
move 24 from 6 to 5
move 2 from 8 to 4
move 1 from 8 to 6
move 2 from 2 to 9
move 5 from 2 to 7
move 1 from 2 to 9
move 11 from 4 to 1
move 3 from 3 to 2
move 4 from 9 to 7
move 1 from 1 to 5
move 1 from 6 to 1
move 5 from 1 to 9
move 5 from 9 to 7
move 5 from 7 to 5
move 23 from 5 to 2
move 5 from 7 to 8
move 6 from 5 to 6
move 1 from 3 to 7
move 1 from 5 to 7
move 6 from 7 to 8
move 3 from 6 to 1
move 2 from 8 to 7
move 4 from 2 to 1
move 4 from 8 to 5
move 7 from 2 to 3
move 1 from 7 to 4
move 1 from 4 to 7
move 4 from 3 to 8
move 6 from 1 to 9
move 4 from 8 to 6
move 3 from 1 to 5
move 3 from 8 to 5
move 1 from 1 to 8
move 3 from 9 to 1
move 3 from 6 to 7
move 1 from 7 to 9
move 3 from 8 to 3
move 8 from 5 to 7
move 11 from 2 to 8
move 5 from 8 to 3
move 1 from 8 to 7
move 10 from 3 to 4
move 2 from 5 to 8
move 3 from 9 to 2
move 1 from 9 to 6
move 7 from 2 to 7
move 6 from 9 to 4
move 1 from 8 to 5
move 3 from 6 to 8
move 1 from 5 to 3
move 2 from 3 to 1
move 6 from 1 to 3
move 13 from 7 to 5
move 16 from 4 to 3
move 2 from 1 to 5
move 5 from 5 to 4
move 11 from 3 to 4
move 2 from 7 to 1
move 7 from 3 to 1
move 2 from 8 to 3
move 8 from 1 to 9
move 12 from 4 to 8
move 1 from 1 to 4
move 2 from 6 to 2
move 3 from 7 to 8
move 2 from 4 to 6
move 5 from 8 to 1
move 3 from 7 to 5
move 6 from 5 to 7
move 2 from 2 to 5
move 1 from 4 to 9
move 5 from 1 to 8
move 6 from 3 to 1
move 7 from 5 to 7
move 7 from 9 to 2
move 1 from 6 to 7
move 1 from 1 to 9
move 2 from 5 to 3
move 2 from 9 to 6
move 13 from 7 to 3
move 2 from 6 to 1
move 1 from 9 to 2
move 16 from 8 to 7
move 6 from 8 to 5
move 3 from 2 to 5
move 4 from 2 to 1
move 3 from 1 to 8
move 2 from 8 to 9
move 1 from 8 to 7
move 1 from 2 to 1
move 8 from 3 to 1
move 1 from 4 to 5
move 1 from 6 to 3
move 2 from 9 to 7
move 5 from 1 to 4
move 15 from 7 to 9
move 11 from 9 to 3
move 7 from 1 to 3
move 2 from 1 to 6
move 1 from 6 to 3
move 2 from 4 to 5
move 2 from 4 to 9
move 7 from 5 to 9
move 5 from 9 to 3
move 1 from 1 to 6
move 5 from 5 to 9
move 1 from 4 to 8
move 1 from 8 to 4
move 3 from 7 to 4
move 8 from 9 to 5
move 1 from 6 to 4
move 4 from 9 to 3
move 1 from 9 to 3
move 23 from 3 to 1
move 12 from 1 to 2
move 6 from 1 to 9
move 5 from 9 to 7
move 3 from 3 to 7
move 6 from 4 to 3
move 1 from 6 to 8
move 6 from 1 to 2
move 3 from 7 to 3
move 3 from 2 to 5
move 10 from 3 to 5
move 1 from 1 to 8
move 12 from 2 to 5
move 3 from 2 to 9
move 2 from 8 to 4
move 13 from 5 to 1
move 2 from 9 to 2
move 2 from 1 to 3
move 11 from 3 to 1
move 2 from 2 to 1
move 2 from 1 to 9
move 16 from 1 to 7
move 17 from 5 to 8
move 1 from 1 to 2
move 3 from 9 to 6
