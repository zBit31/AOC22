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
*        SR    R4,R4          Clear R4 - Will have the current addition
GET_LINE GET   FILEIN,RECIN
         MVC   RECOUT,BLANK   Blank Output
         MVC   RECOUT(20),RECIN
         LA    R5,RECIN            START OF SCAN
         LA    R6,80(0,R5)         END OF SCAN
         LA    0,C' '              GET NEXT " " blank
         SRST  R6,R5
         BC    2,BOOM              blank not fOUND, abend
         SR    R6,R5               get difference
         SRL   R6,1                shift right (divide by 2)
         CVD   R6,CPACKED
         UNPK  RECOUT+40(9),CPACKED(L'CPACKED+1)
         MVI   RECOUT+40+7,C' '  Get rid of the "C" for packed decimal
         LA    R7,0(R6,R5)         Load R7 with R6+R5, mid of string
*        MVC   RECOUT+22(1),0(R7)
FIND_LOOP EQU *
         SR    R0,R0
         ICM   R0,B'0001',0(R5)  Load character on R0 for SRST
         LA    R6,RECIN+80         END OF SCAN
         SRST  R6,R7
         BC    2,NOT_FOUND
         MVC   RECOUT+50(10),=CL10' FOUND '
         ST    R0,RECOUT+60
         ST    R0,RECOUT+64
         TR    RECOUT+64(4),HEXTAB
         J     GO_PUT
NOT_FOUND EQU *
         LA    R5,1(0,R5)           go to next byte
         CR    R5,R7
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
HEXTAB   DC    XL256'00'
         ORG   HEXTAB+C'a'
         DC    F'1'
         ORG   HEXTAB+C'b'
         DC    F'2'
         ORG   HEXTAB+C'c'
         DC    F'3'
         ORG   HEXTAB+C'd'
         DC    F'4'
         ORG   HEXTAB+C'e'
         DC    F'5'
         ORG   HEXTAB+C'f'
         DC    F'6'
         ORG   HEXTAB+C'g'
         DC    F'7'
         ORG   HEXTAB+C'h'
         DC    F'8'
         ORG   HEXTAB+C'i'
         DC    F'9'
         ORG   HEXTAB+C'j'
         DC    F'10'
         ORG   HEXTAB+C'k'
         DC    F'11'
         ORG   HEXTAB+C'l'
         DC    F'12'
         ORG   HEXTAB+C'm'
         DC    F'13'
         ORG   HEXTAB+C'n'
         DC    F'14'
         ORG   HEXTAB+C'o'
         DC    F'15'
         ORG   HEXTAB+C'p'
         DC    F'16'
         ORG   HEXTAB+C'q'
         DC    F'17'
         ORG   HEXTAB+C'r'
         DC    F'18'
         ORG   HEXTAB+C's'
         DC    F'19'
         ORG   HEXTAB+C't'
         DC    F'20'
         ORG   HEXTAB+C'u'
         DC    F'21'
         ORG   HEXTAB+C'v'
         DC    F'22'
         ORG   HEXTAB+C'w'
         DC    F'23'
         ORG   HEXTAB+C'x'
         DC    F'24'
         ORG   HEXTAB+C'y'
         DC    F'25'
         ORG   HEXTAB+C'z'
         DC    F'26'
         ORG   HEXTAB+C'A'
         DC    F'27'
         ORG   HEXTAB+C'B'
         DC    F'28'
         ORG   HEXTAB+C'C'
         DC    F'29'
         ORG   HEXTAB+C'D'
         DC    F'30'
         ORG   HEXTAB+C'E'
         DC    F'31'
         ORG   HEXTAB+C'F'
         DC    F'32'
         ORG   HEXTAB+C'G'
         DC    F'33'
         ORG   HEXTAB+C'H'
         DC    F'34'
         ORG   HEXTAB+C'I'
         DC    F'35'
         ORG   HEXTAB+C'J'
         DC    F'36'
         ORG   HEXTAB+C'K'
         DC    F'37'
         ORG   HEXTAB+C'L'
         DC    F'38'
         ORG   HEXTAB+C'M'
         DC    F'39'
         ORG   HEXTAB+C'N'
         DC    F'40'
         ORG   HEXTAB+C'O'
         DC    F'41'
         ORG   HEXTAB+C'P'
         DC    F'42'
         ORG   HEXTAB+C'Q'
         DC    F'43'
         ORG   HEXTAB+C'R'
         DC    F'44'
         ORG   HEXTAB+C'S'
         DC    F'45'
         ORG   HEXTAB+C'T'
         DC    F'46'
         ORG   HEXTAB+C'U'
         DC    F'47'
         ORG   HEXTAB+C'V'
         DC    F'48'
         ORG   HEXTAB+C'W'
         DC    F'49'
         ORG   HEXTAB+C'X'
         DC    F'50'
         ORG   HEXTAB+C'Y'
         DC    F'51'
         ORG   HEXTAB+C'Z'
         DC    F'52'
         ORG
*
         LTORG
         YREGS
         END
/*
//LINK    EXEC PGM=IEWL,COND=(0,NE),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=SHR,DSN=MY.LOAD(AOC22)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.ASM.SYSLIN
//*
//GO        EXEC PGM=AOC22,COND=(0,NE)
//STEPLIB  DD DISP=SHR,DSN=MY.LOAD
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
