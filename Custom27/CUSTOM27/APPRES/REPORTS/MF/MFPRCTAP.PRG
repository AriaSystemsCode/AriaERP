*:***************************************************************************
*: Program file  : MFPRCTAP.PRG
*: Program desc. : Print Cutting Tickets for Apparel Resources
*: For Report    : MFPRCTAP.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Manufactoring
*: Date          : 07/31/2001
*: Developer     : AHMED MAHER - (AMH)
*: Reference     : C102403,1
*:***************************************************************************
*: Passed Parameters  : None.
*:***************************************************************************
*:Modifications:
*:***************************************************************************
*:
SELECT (lcMainF)
LOCATE
IF EOF()
  RETURN
ENDIF

lcMainF2 = gfTempName()
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+15,4]

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'QTY9'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'QTY10'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'QTY11'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ1'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ2'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ3'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ4'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ5'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ6'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ7'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ8'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ9'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ10'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ11'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'PGS'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 3
laFileStru[lnFileStru,4] = 0

CREATE CURSOR (lcMainF2) FROM ARRAY laFileStru
INDEX ON CUTTKT+cWareCode+Style+Dyelot+NoteFlag TAG (lcMainF2) OF (lcMainF2)
 
lnMemoWdth = SET('MEMOWIDTH')
SET MEMOWIDTH TO 121

SELECT (lcMainF)
LOCATE
lcCurKey = SPACE(12)
STORE 0 TO lnLinesCnt,lnStyLnCnt,lnCutLnCnt
SCAN
  IF !EMPTY(lcCurKey) .AND. !(lcCurKey == CUTTKT+CWARECODE)
    =lfGetPgs(lnLinesCnt,lnStyLnCnt,lnCutLnCnt)
    STORE 0 TO lnLinesCnt,lnStyLnCnt,lnCutLnCnt
  ENDIF
  lcCurKey = CUTTKT+CWARECODE
  IF SEEK(CUTTKT+CWARECODE+SUBSTR(STYLE,1,15),lcMainF2) .AND. NOTEFLAG = 'N'
    SELECT (lcMainF2)
    REPLACE QTY9  WITH EVALUATE(lcMainF+'.QTY1'),;
            QTY10 WITH EVALUATE(lcMainF+'.QTY2'),;
            QTY11 WITH EVALUATE(lcMainF+'.QTY3'),;
            SZ9   WITH SCALE.SZ1,;
            SZ10  WITH SCALE.SZ2,;
            SZ11  WITH SCALE.SZ3,;
            TOTQTY WITH TOTQTY+EVALUATE(lcMainF+'.TOTQTY')
  ELSE
    SELECT (lcMainF)
    SCATTER MEMO MEMVAR
    SELECT SCALE
    SCATTER FIELDS SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8 MEMVAR
    IF NOTEFLAG <> 'N'
      SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY BLANK MEMVAR
      IF NOTEFLAG = 'S'
        lnStyLnCnt = MEMLINES(Notes)
      ELSE
        lnCutLnCnt = MEMLINES(Notes)
      ENDIF
      IF SEEK(CUTTKT+CWARECODE+SUBSTR(STYLE,1,15),lcMainF2)
        SELECT (lcMainF2)
        SCATTER FIELDS SZ1,SZ2,SZ3,SZ4,SZ5,SZ6,SZ7,SZ8,SZ9,SZ10,SZ11 MEMVAR
      ENDIF
    ELSE
      lnLinesCnt = lnLinesCnt + 1
    ENDIF
    SELECT (lcMainF2)
    APPEND BLANK
    GATHER MEMO MEMVAR
  ENDIF
ENDSCAN
SET RELATION TO

SET MEMOWIDTH TO lnMemoWdth
IF !EMPTY(lcCurKey)
  =lfGetPgs(lnLinesCnt,lnStyLnCnt,lnCutLnCnt)
ENDIF

SELECT (lcMainF2)
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO CutTkt INTO CUTTKTH ADDITIVE
SET RELATION TO CUTTKT INTO (lcMainF) ADDITIVE
*-- End Of Option Program.

*!*************************************************************
*! Name      : lfGetPgs
*! Developer : AHMED MAHER (AMH)
*! Date      : 08/01/2001
*! Purpose   : Get the number of pages for each C/T.
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Passed Parameters  : lnLinesCnt > the number of lines in the C/T.
*!                      lnStyLnCnt > the number of lines for style notes in the C/T.
*!                      lnCutLnCnt > the number of lines for C/T notes in the C/T.
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetPgs(lnLinesCnt,lnStyLnCnt,lnCutLnCnt)
*!*************************************************************
*
FUNCTION lfGetPgs
PARAMETERS lnLinesCnt,lnStyLnCnt,lnCutLnCnt

*-- lnLinesPgs --> the number of pages for lines of each C/T.
*-- lnLstLnCnt --> the number of lines in the last page.
*-- lnStyFrsLn --> the number of lines for style notes in the first page.
*-- lnStylePgs --> the number of pages for style notes of each C/T.
*-- lnLstStCnt --> the number of lines for style notes in the last page.
*-- lnCutFrsLn --> the number of lines for C/T notes in the first page.
*-- lnCutPgs   --> the number of pages for C/T notes of each C/T.
PRIVATE lnLinesPgs,lnLstLnCnt,lnStyFrsLn,lnStylePgs,lnLstStCnt,lnCutFrsLn,lnCutPgs

STORE 0 TO lnLinesPgs,lnStylePgs,lnCutPgs
lnLinesPgs = CEILING(((lnLinesCnt-6)/7)+1)

*-- if there is a style or C/T notes
IF lnStyLnCnt+lnCutLnCnt > 0
  IF lnLinesCnt < 7
    lnLstLnCnt = lnLinesCnt
  ELSE
    lnLstLnCnt = MOD(lnLinesCnt-6,7)
    lnLstLnCnt = IIF(lnLstLnCnt=0,7,lnLstLnCnt)
  ENDIF
ENDIF

IF lnStyLnCnt > 0   && Style notes exist
  lnStyFrsLn = INT(33-(lnLstLnCnt*13/3)-IIF(MOD(lnLstLnCnt,3)=0,1,0))+IIF(lnLinesCnt<7,;
               -7-IIF(MOD(lnLstLnCnt,3)=0,0,1)+IIF(lnLstLnCnt=1,1,0),0)
  lnStylePgs = CEILING((lnStyLnCnt-IIF(lnLinesCnt=6,33,lnStyFrsLn))/34)+IIF(lnLinesCnt=6,1,0)
  IF lnCutLnCnt > 0   && C/T notes exist and Style notes exist
    IF lnStyLnCnt = lnStyFrsLn
      lnLstStCnt = lnStyFrsLn
    ELSE
      lnLstStCnt = MOD(lnStyLnCnt-lnStyFrsLn,34)
      lnLstStCnt = IIF(lnLstStCnt=0,34,lnLstStCnt)
    ENDIF
    lnCutFrsLn = IIF(lnLstStCnt=34,0,33-lnLstStCnt-IIF(lnLinesCnt=6 .AND. lnStyLnCnt=lnStyFrsLn,1,0))
    lnCutPgs = CEILING((lnCutLnCnt-lnCutFrsLn)/34)
  ENDIF
ELSE
  IF lnCutLnCnt > 0   && C/T notes exist and Style notes not exist
    lnCutFrsLn = INT(33-(lnLstLnCnt*13/3)-IIF(MOD(lnLstLnCnt,3)=0,1,0))+IIF(lnLinesCnt<7,;
                 -7-IIF(MOD(lnLstLnCnt,3)=0,0,1)+IIF(lnLstLnCnt=1,1,0),0)
    lnCutPgs = CEILING((lnCutLnCnt-IIF(lnLinesCnt=6,33,lnCutFrsLn))/34)+IIF(lnLinesCnt=6,1,0)
  ENDIF
ENDIF

SELECT (lcMainF2)
SEEK lcCurKey
REPLACE REST WHILE CUTTKT+cWareCode+Style+Dyelot+NoteFlag = lcCurKey;
        PGS WITH lnLinesPgs+lnStylePgs+lnCutPgs
*-- end of lfGetPgs.