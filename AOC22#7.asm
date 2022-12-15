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
         STORAGE OBTAIN,LENGTH=WORKLEN,LOC=31,ADDR=(R11),CHECKZERO=YES
         CHI   R15,20
         JNE   ABEND     Abend if work area not zeroed
* R11 will have the address of storage obtained
         LR    R10,R11
* R10 will have address of current dir we are working on
         SR    R4,R4
* R4 will have the total file size for the current dir
         SR    R3,R3
* R3 will have the total file size for dirs < 100000
GET_LINE GET   FILEIN,RECIN   Get text
         MVI   RECOUT,C' '                  Blank Output
         MVC   RECOUT+1(L'RECOUT-1),RECOUT  Blank Output
         MVC   RECOUT(15),RECIN
         CLC   RECIN(7),=CL7'$ cd ..'
         JE    BACK_DIR
         CLC   RECIN(5),=CL5'$ cd '
         JE    NEW_DIR
         CLC   RECIN(4),=CL4'$ ls'
         JE    GET_LINE        NOP, get next
         CLC   RECIN(4),=CL4'dir '
         JE    GET_LINE        NOP, get next
         J     NEW_FILE
NEW_DIR  EQU *
         ST    R4,0(R10)       Store current total
         AHI   R10,4           Go to the next dir (fullword)
         SR    R4,R4           Zero R4
         J     GET_LINE        Get next
BACK_DIR EQU *
         CFI   R4,THRESHOLD        Compare dir size with thresh
         JH    GO_PACK1
         AR    R3,R4               R4 <= thresh, add it
GO_PACK1 CVD   R4,CPACKED          Convert to decimal to display
         UNPK  RECOUT+30(13),CPACKED(L'CPACKED+1)
         MVI   RECOUT+30+11,C' ' Get rid of the "C" for packed dec
         AHI   R10,-4          Go to the prev dir (fullword)
         A     R4,0(,R10)      Add subdir to current total
         PUT   FILEOUT,RECOUT
         J     GET_LINE        Get next
NEW_FILE EQU *
* Get first value - file size
         LA    R5,RECIN            START OF SCAN (after the "move")
         LA    R6,RECIN+L'RECIN    END OF SCAN
         LA    0,C' '              GET NEXT space
         SRST  R6,R5
         BC    2,ABEND Not found???
         SR    R6,R5               get difference
         AHI   R6,-1               Calc length of ### -1 for EX
         EX    R6,PACK             EX PACK  CPACKED,0(0,R5)
         CVB   R0,CPACKED          Convert file size on R0
         AR    R4,R0               Add to total
*        ST    R7,RECOUT+50
*        LA    R0,X'02'                 char to search
*        SRST  R4,R3
*        BC    B'0010',NEXT_CHA    Char not found, search next
*        AHI   R5,1
*        J     LOOP_START
*        CVD   R4,CPACKED          Convert to decimal again to display
*        UNPK  RECOUT+60(13),CPACKED(L'CPACKED+1)
*        MVI   RECOUT+60+7,C' '  Get rid of the "C" for packed decimal
*
         J     GET_LINE
*
FILE#EOD EQU   *
         CR    R10,R11
         JNH   DONE
         CFI   R4,THRESHOLD        Compare dir size with thresh
         JH    GO_PACK2
         AR    R3,R4               R4 <= thresh, add it
GO_PACK2 CVD   R4,CPACKED          Convert to decimal to display
         UNPK  RECOUT+30(13),CPACKED(L'CPACKED+1)
         MVI   RECOUT+30+11,C' ' Get rid of the "C" for packed dec
         AHI   R10,-4          Go to the prev dir (fullword)
         A     R4,0(,R10)      Add subdir to current total
         PUT   FILEOUT,RECOUT
         J     FILE#EOD
DONE     CVD   R3,CPACKED          Convert to decimal to display
         UNPK  RECOUT+00(13),CPACKED(L'CPACKED+1)
         MVI   RECOUT+00+11,C' ' Get rid of the "C" for packed dec
         PUT   FILEOUT,RECOUT
         STORAGE RELEASE,LENGTH=WORKLEN,ADDR=(R11)
         CLOSE FILEIN
         CLOSE FILEOUT
*********************** STARDARD EXIT ********************************
EXIT     EQU   *                      BRANCH TO HERE FOR NORMAL RETURN
         LA    R15,0                  SET RETURN CODE REG 15 = 0
         PR
*
ABEND    ABEND 1
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
THRESHOLD EQU  100000    Max sub dirs size to count
*URDIR   DC    X'01F0F0F0'
*NITDIR  DC    X'02F0F0F0'
*DIRLIST DC    (MAXSUBDIR)X'02F0F0F0'
*DIRLIST_LEN EQU *-WDIRLIST
*
         LTORG
         YREGS
FILETAB  DSECT
FDIRLIST DS    (MAXSUBDIR)F
WORKLEN  EQU   *-FDIRLIST     Working storage needed
MAXSUBDIR EQU  300       Max sub dirs for a file to be in
         END
/*
//L       EXEC PGM=IEWL,COND=(8,LE,C),
// PARM='XREF,LET,LIST,NCAL'
//SYSLMOD  DD  DISP=(,PASS),UNIT=SYSALLDA,SPACE=(CYL,(1,1,1)),
//         DSN=&&GOSET(GO)
//SYSUT1   DD  UNIT=SYSDA,SPACE=(1024,(50,20))
//SYSPRINT DD  SYSOUT=*
//APFBYPAS DD  DUMMY
//SYSLIN   DD  DISP=OLD,DSN=*.C.SYSLIN
//*
//G        EXEC PGM=*.L.SYSLMOD,COND=((8,LE,C),(8,LE,L))
//FILEOUT  DD  SYSOUT=*
//SYSUDUMP DD  SYSOUT=*,OUTLIM=5000
//FILEIN   DD  *
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
