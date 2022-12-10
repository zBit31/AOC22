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
         BAKR  R14,0               SAVE ALL REGISTERS              @NSC
         BASR  R12,R0                 ESTABLISH
         USING *,R12                  ADDRESSABILITY
         LA    R13,SAVEAREA           POINT TO MY LOWER-LEVEL SA
**********************  BEGIN LOGIC  *********************************
         OPEN  (FILEIN,(INPUT))
         OPEN  (FILEOUT,(OUTPUT))
*
         STORAGE OBTAIN,LENGTH=WORKLEN,LOC=31,ADDR=(R11)
GET_LINE GET   FILEIN,0(R11)  Get text work
         MVI   RECOUT,C' '                  Blank Output
         MVC   RECOUT+1(L'RECOUT-1),RECOUT  Blank Output
         MVC   RECOUT(15),0(R11)
         LA    R4,0(,R11)     R4 - Will have search start
         AHI   R4,-1         Take one out to account for +1 on loop
LOOP_START EQU *
         AHI   R4,1           Go to next char
         CLC   3(1,R4),0(R4)
         BE    LOOP_START  Char found,loop again
         CLC   3(1,R4),1(R4)
         BE    LOOP_START  Char found,loop again
         CLC   3(1,R4),2(R4)
         BE    LOOP_START  Char found,loop again
         CLC   2(1,R4),0(R4)
         BE    LOOP_START  Char found,loop again
         CLC   2(1,R4),1(R4)
         BE    LOOP_START  Char found,loop again
         CLC   1(1,R4),0(R4)
         BE    LOOP_START  Char found,loop again
* start-of-packet found
         AHI   R4,4   Add 3 to get to char on R0
         SR    R4,R11 Subtract R11 to get the offset from work start
         CVD   R4,CPACKED          Convert to decimal again to display
         UNPK  RECOUT+60(9),CPACKED(L'CPACKED+1)
         MVI   RECOUT+60+7,C' '  Get rid of the "C" for packed decimal
P        PUT   FILEOUT,RECOUT

         J     GET_LINE
*
FILE#EOD EQU   *
         STORAGE RELEASE,LENGTH=WORKLEN,ADDR=(R11)
         CLOSE FILEIN
         CLOSE FILEOUT
*********************** STARDARD EXIT ********************************
EXIT     EQU   *                      BRANCH TO HERE FOR NORMAL RETURN
         LA    R15,0                  SET RETURN CODE REG 15 = 0
         PR
**********************  DATA AREAS   *********************************
FILEIN   DCB   DSORG=PS,                                               X
               MACRF=(GM),                                             X
               DEVD=DA,                                                X
               DDNAME=FILEIN,                                          X
               EODAD=FILE#EOD,                                         X
               RECFM=FB,                                               X
               LRECL=6000
*
FILEOUT  DCB   DSORG=PS,                                               X
               MACRF=(PM),                                             X
               DEVD=DA,                                                X
               DDNAME=FILEOUT,                                         X
               RECFM=FB,                                               X
               LRECL=80
RECIN    DS    CL80       INPUT AREA FOR RECORDS
*
BIN_VAL  DS    X'FFFFFFFF',X'FF'
* Record out
RECOUT   DS    CL80       OUTPUT AREA FOR RECORDS
         ORG   *-L'OUT_VAL
OUT_VAL  DC    CL8'XXXXXXXX',C'<'
*
CPACKED  DS    D
PACK     PACK  CPACKED,0(0,R5)
ZEROES   DS    8C'0'
SAVEAREA DC    18F'0'     AREA FOR MY CALLEE TO SAVE & RESTORE MY REGS
*
HEXTAB   DC    C'0123456789ABCDEF'
*
WORKLEN  EQU   256*1024*1024
*
         LTORG
         YREGS
         END
/*
//L       EXEC PGM=IEWL,COND=(8,LT,C),
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
//FILEIN   DD  DISP=SHR,DSN=AOC22#6.INPUT
//XILEIN   DD  *
bvwbjplbgvbhsrlpgdmjqwftvncz
nppdvjthqldpwncqszvftbrmjlhg
nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
