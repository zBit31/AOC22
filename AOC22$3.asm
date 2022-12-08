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
*        SR    R7,R7          Clear R7 - Will have the max value 1
*        SR    R8,R8          Clear R8 - Will have the max value 2
*        SR    R9,R9          Clear R9 - Will have the max value 3
         SR    R4,R4          Clear R4 - Will have the current addition
GET_LINE GET   FILEIN,RECIN1
         GET   FILEIN,RECIN2
         GET   FILEIN,RECIN3
         MVC   RECOUT,BLANK   Blank Output
         MVC   RECOUT(10),RECIN1
*        MVC   RECOUT+22(1),0(R7)
         LA    R5,RECIN1      Char to search
FIND_LOOP EQU *
         SR    R0,R0
         ICM   R0,B'0001',0(R5)  Load character on R0 for SRST
         LA    R6,RECIN2+80      END OF SCAN
         LA    R7,RECIN2         Start OF SCAN
         SRST  R6,R7
         BC    2,NOT_FOUND
*        MVC   RECOUT+50(10),=CL10' FOUND '
FIND_LOOP_2 EQU *
         LA    R6,RECIN3+80      END OF SCAN
         LA    R7,RECIN3         Start OF SCAN
         SRST  R6,R7
         BC    2,NOT_FOUND
         ST    R0,RECOUT+60
         ST    R0,RECOUT+64        Write the char
         TR    RECOUT+64(4),HEXTAB translate to the value
         L     R0,RECOUT+64        Load on R0
         AR    R4,R0               Add to R4
         CVD   R4,CPACKED          Go display the sum
         UNPK  RECOUT+20(9),CPACKED(L'CPACKED+1)
         MVI   RECOUT+20+7,C' '  Get rid of the "C" for packed decimal
         J     GO_PUT
NOT_FOUND EQU *
         LA    R5,1(0,R5)           go to next byte
         CR    R5,R6
         BL    FIND_LOOP
         ABEND 2
*        MVC   RECOUT+50(10),=CL10' NOT FOUND '
*        ST    R6,RECOUT+10        store in output just to see
*        EX    R6,PACK             EX PACK  CPACKED,RECIN(0)
*        MVC   RECOUT+20(L'CPACKED),CPACKED
*        CVB   R2,CPACKED
*        AR    R4,R2
*        ST    R2,RECOUT+20        store in output just to see
*        LA    R2,L'TEMPNUM
*        SR    R2,R6               get position for insert
*        PACK  CPACKED,RECIN(4)
*        CVB   R2,CPACKED
GO_PUT   EQU   *
         PUT   FILEOUT,RECOUT
         J     GET_LINE
*
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
RECIN1   DS    CL80       INPUT AREA1 FOR RECORDS
RECIN2   DS    CL80       INPUT AREA2 FOR RECORDS
RECIN3   DS    CL80       INPUT AREA3 FOR RECORDS
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
*PACK     PACK  CPACKED,RECIN(0)
ZEROES   DS    8C'0'
SAVEAREA DC    18F'0'     AREA FOR MY CALLEE TO SAVE & RESTORE MY REGS
*
HEXTAB   DC    XL256'CC'
         ORG   HEXTAB+C'a'
         DC    X'01'
         ORG   HEXTAB+C'b'
         DC    X'02'
         ORG   HEXTAB+C'c'
         DC    X'03'
         ORG   HEXTAB+C'd'
         DC    X'04'
         ORG   HEXTAB+C'e'
         DC    X'05'
         ORG   HEXTAB+C'f'
         DC    X'06'
         ORG   HEXTAB+C'g'
         DC    X'07'
         ORG   HEXTAB+C'h'
         DC    X'08'
         ORG   HEXTAB+C'i'
         DC    X'09'
         ORG   HEXTAB+C'j'
         DC    X'0A'
         ORG   HEXTAB+C'k'
         DC    X'0B'
         ORG   HEXTAB+C'l'
         DC    X'0C'
         ORG   HEXTAB+C'm'
         DC    X'0D'
         ORG   HEXTAB+C'n'
         DC    X'0E'
         ORG   HEXTAB+C'o'
         DC    X'0F'
         ORG   HEXTAB+C'p'
         DC    X'10'
         ORG   HEXTAB+C'q'
         DC    X'11'
         ORG   HEXTAB+C'r'
         DC    X'12'
         ORG   HEXTAB+C's'
         DC    X'13'
         ORG   HEXTAB+C't'
         DC    X'14'
         ORG   HEXTAB+C'u'
         DC    X'15'
         ORG   HEXTAB+C'v'
         DC    X'16'
         ORG   HEXTAB+C'w'
         DC    X'17'
         ORG   HEXTAB+C'x'
         DC    X'18'
         ORG   HEXTAB+C'y'
         DC    X'19'
         ORG   HEXTAB+C'z'
         DC    X'1A'
         ORG   HEXTAB+C'A'
         DC    X'1B'
         ORG   HEXTAB+C'B'
         DC    X'1C'
         ORG   HEXTAB+C'C'
         DC    X'1D'
         ORG   HEXTAB+C'D'
         DC    X'1E'
         ORG   HEXTAB+C'E'
         DC    X'1F'
         ORG   HEXTAB+C'F'
         DC    X'20'
         ORG   HEXTAB+C'G'
         DC    X'21'
         ORG   HEXTAB+C'H'
         DC    X'22'
         ORG   HEXTAB+C'I'
         DC    X'23'
         ORG   HEXTAB+C'J'
         DC    X'24'
         ORG   HEXTAB+C'K'
         DC    X'25'
         ORG   HEXTAB+C'L'
         DC    X'26'
         ORG   HEXTAB+C'M'
         DC    X'27'
         ORG   HEXTAB+C'N'
         DC    X'28'
         ORG   HEXTAB+C'O'
         DC    X'29'
         ORG   HEXTAB+C'P'
         DC    X'2A'
         ORG   HEXTAB+C'Q'
         DC    X'2B'
         ORG   HEXTAB+C'R'
         DC    X'2C'
         ORG   HEXTAB+C'S'
         DC    X'2D'
         ORG   HEXTAB+C'T'
         DC    X'2E'
         ORG   HEXTAB+C'U'
         DC    X'2F'
         ORG   HEXTAB+C'V'
         DC    X'30'
         ORG   HEXTAB+C'W'
         DC    X'31'
         ORG   HEXTAB+C'X'
         DC    X'32'
         ORG   HEXTAB+C'Y'
         DC    X'33'
         ORG   HEXTAB+C'Z'
         DC    X'34'
         ORG
*
         LTORG
         YREGS
         END
/*
//LINK    EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=SHR,DSN=O990E.LEO.LOAD(AOC22)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//*
//GO        EXEC PGM=AOC22,COND=(0,NE)
//STEPLIB  DD DISP=SHR,DSN=O990E.LEO.LOAD
//SYSPRINT DD SYSOUT=*
//DATASORT DD SYSOUT=*
//FILEIN   DD  *
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
/*
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
