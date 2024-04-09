*:************************************************************************
*: Program file  : POADPOJL.PRG (C#102235)
*: Program desc. : Adornment Spread Sheet Report,
*:        System : Aria Apparel System (A2.7)
*: Module        : Style Purchase Order (PO)
*:      Developer: Ahmed Maher (AMH)
*:************************************************************************
*: Calls :PROCEDURES :
*:        FUNCTIONS  : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*:Modifications :
*:****************************************************************************************
*: B804473,1 AMH 10/18/2001 Fix the bug of sorting the colors by SN.
*: B804462,1 AMH 10/21/2001 Fix the bug of wrong information displayed in the ador. browse form.
*: B804569,1 KHM 01/09/2001 Fix the bug of not displaying the correct design name.
*: B605355,1 AMH 01/10/2002 Fix the bug of sometimes don't print the first adornmet line.
*: C102533,1 AMH 01/13/2002 Add account code to the desgine code at Frx.
*: B605969,1 RAE 05/27/2002 Eliminate the duplication on thread sequence.
*: B605989,4 AMH 06/12/2002 Rename the status field in ICDESIGN file to cdsgstatus.
*: B127072,1 NNA 03/27/2005 Fix bug that in some cases the report Misses the order information
*:****************************************************************************************

lcTmpCur  = gfTempName()
lcTmpCur1 = gfTempName()

*C102533,1 AMH Add account code field [Start]
*CREATE CURSOR (lcTmpCur);
              (CPO C(6),CSTATUS C(10),CPIKTKT C(6),CORDER C(6),DSTART D,DCOMPLETE D,;
               CORDNOTE C(61),CDESGCODE C(6),CDESGDESC C(35),CEMBLOC C(6),NSTITCHES N(6,0),;
               CEMBSTATUS C(10),CNAMEDROP1 C(35),CNAMEDROP2 C(35),CNAMEDROP3 C(35),;
               NLINENO N(6,0),CSTYLE C(19),NQTY1 N(6,0),NQTY2 N(6,0),NQTY3 N(6,0),;
               NQTY4 N(6,0),NQTY5 N(6,0),NQTY6 N(6,0),NQTY7 N(6,0),NQTY8 N(6,0),NTOTQTY N(7,0),;
               MTHREADCLR M,MNOTES M,CSCALE C(3),OBJECTID C(10),NEMBNUM N(2,0))
CREATE CURSOR (lcTmpCur);
              (CPO C(6),CSTATUS C(10),CPIKTKT C(6),CORDER C(6),DSTART D,DCOMPLETE D,;
               CORDNOTE C(61),CDESGCODE C(6),CDESGDESC C(35),CEMBLOC C(6),NSTITCHES N(6,0),;
               CEMBSTATUS C(10),CNAMEDROP1 C(35),CNAMEDROP2 C(35),CNAMEDROP3 C(35),;
               NLINENO N(6,0),CSTYLE C(19),NQTY1 N(6,0),NQTY2 N(6,0),NQTY3 N(6,0),;
               NQTY4 N(6,0),NQTY5 N(6,0),NQTY6 N(6,0),NQTY7 N(6,0),NQTY8 N(6,0),NTOTQTY N(7,0),;
               MTHREADCLR M,MNOTES M,CSCALE C(3),OBJECTID C(10),NEMBNUM N(2,0),CACCOUNT C(5))
*C102533,1 AMH [End]

INDEX ON CPO+CDESGCODE+OBJECTID+CSTYLE+STR(NLINENO,6) TAG (lcTmpCur)  OF (lcTmpCur)
INDEX ON CPO+CDESGCODE+OBJECTID+STR(NLINENO,6)+CSTYLE TAG (lcTmpCur1) OF (lcTmpCur)

SELECT CUTPICK
SET ORDER TO TAG CUTPKORD
*-- To get the selected Po if any.
lcPoFile = ''
llUsePo = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.PO'),1)
IF lnPosition > 0
  lcPoFile = laOGFxFlt[lnPosition,6]
  llUsePo = IIF(!EMPTY(lcPoFile) .AND. USED(lcPoFile) .AND. RECCOUNT(lcPoFile)>0,.T.,.F.)
ENDIF

*B804569,1 KHM 01/09/2001 (Begin) Changing the order of the file.
SET ORDER TO TAG ACCTDSGN IN ICDESIGN
*B804569,1 KHM 01/09/2001 (End)

IF llUsePo
  SELECT (lcPoFile)
  SET RELATION TO 'A'+PO INTO POSHDR ADDITIVE
ENDIF
SELECT POSHDR
SET RELATION TO
SET RELATION TO 'A'+PO INTO POSLN ADDITIVE
SELECT POSLN
SET RELATION TO '2'+PO+STR(LINENO,6) INTO CUTPICK ADDITIVE
SELECT CUTPICK
SET RELATION TO 'O'+ORDER INTO ORDHDR ADDITIVE
SET RELATION TO 'O'+ORDER+CORDLINE INTO ORDLINE ADDITIVE
IF llUsePo
  SELECT (lcPoFile)
ELSE
  SELECT POSHDR
ENDIF
LOCATE
SCAN REST WHILE POSHDR.CSTYTYPE+POSHDR.PO = 'A' FOR EVALUATE(lcRpExp)
  *B605355,1 AMH Seek to the frist record in bomvar [Start]
  *IF SEEK('A'+PO,'POSLN') .AND. SEEK('AD'+PO+STR(POSLN.LINENO,6),'BOMVAR')
  IF SEEK('A'+PO,'POSLN') .AND. SEEK('AD'+PO,'BOMVAR')
  *B605355,1 AMH [End]
    SELECT BOMVAR
    lcEmbStr = SPACE(0)
    SCAN REST WHILE cIdType+cCost_Id+STR(LineNo,6)+cDsgnCode+cStylePos = 'AD'+POSHDR.PO
      
      *B804569,1 KHM 01/09/2001 (Begin) Changing the Seek command
      *=SEEK(CDSGNCODE,'ICDESIGN')
      =SEEK(ORDLINE.ACCOUNT+ORDLINE.STORE+CDSGNCODE,'ICDESIGN')
      *B804569,1 KHM 01/09/2001 (End)
      
      =SEEK('G'+ORDLINE.ACCOUNT+ORDLINE.STORE+CDSGNCODE,'OBJLINK')
      IF !(CDSGNCODE $ lcEmbStr)
        lcEmbStr = lcEmbStr + CDSGNCODE + '|'
      ENDIF
      
      *B605355,1 AMH Locate to the correct record in Posln [Start]
      IF SEEK('A'+POSHDR.PO,'POSLN')
        SELECT POSLN
        LOCATE REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd = 'A'+POSHDR.PO;
                    FOR LINENO = BOMVAR.LINENO .AND. TRANCD = '1'
      ENDIF
      *B605355,1 AMH [End]
      
      SELECT (lcTmpCur)
      APPEND BLANK
      
      *C102533,1 AMH Add account code field [Start]
      *REPLACE CPO        WITH POSHDR.PO,;
              CSTATUS    WITH lfGetStats(POSHDR.STATUS),;
              CPIKTKT    WITH POSLN.SHIPNO,;
              CORDER     WITH ORDHDR.ORDER,;
              DSTART     WITH ORDHDR.START,;
              DCOMPLETE  WITH ORDHDR.COMPLETE,;
              CORDNOTE   WITH ORDHDR.NOTE1 + IIF(EMPTY(ORDHDR.NOTE1),'',' ') + ORDHDR.NOTE2,;
              CDESGCODE  WITH BOMVAR.CDSGNCODE,;
              CDESGDESC  WITH ICDESIGN.CDSNNAME,;
              CEMBLOC    WITH BOMVAR.CSTYLEPOS,;
              NSTITCHES  WITH BOMVAR.NSTITCHES,;
              CEMBSTATUS WITH lfGetStats(ICDESIGN.STATUS),;
              CNAMEDROP1 WITH BOMVAR.CNAMEDRP1,;
              CNAMEDROP2 WITH BOMVAR.CNAMEDRP2,;
              CNAMEDROP3 WITH BOMVAR.CNAMEDRP3,;
              OBJECTID   WITH IIF(EMPTY(OBJLINK.COBJECT_ID),'ZZZZZZZZZZ',OBJLINK.COBJECT_ID),;
              NLINENO    WITH 999999,;
              CSTYLE     WITH REPLICATE('Z',19),;
              NEMBNUM    WITH OCCURS('|',SUBSTR(lcEmbStr,1,AT(BOMVAR.CDSGNCODE,lcEmbStr)-1))+1
      
      *B605989,4 AMH Rename the status field in ICDESIGN file to cdsgstatus [Start]
      *REPLACE CPO        WITH POSHDR.PO,;
              CSTATUS    WITH lfGetStats(POSHDR.STATUS),;
              CPIKTKT    WITH POSLN.SHIPNO,;
              CORDER     WITH ORDHDR.ORDER,;
              DSTART     WITH ORDHDR.START,;
              DCOMPLETE  WITH ORDHDR.COMPLETE,;
              CORDNOTE   WITH ORDHDR.NOTE1 + IIF(EMPTY(ORDHDR.NOTE1),'',' ') + ORDHDR.NOTE2,;
              CDESGCODE  WITH BOMVAR.CDSGNCODE,;
              CDESGDESC  WITH ICDESIGN.CDSNNAME,;
              CEMBLOC    WITH BOMVAR.CSTYLEPOS,;
              NSTITCHES  WITH BOMVAR.NSTITCHES,;
              CEMBSTATUS WITH lfGetStats(ICDESIGN.STATUS),;
              CNAMEDROP1 WITH BOMVAR.CNAMEDRP1,;
              CNAMEDROP2 WITH BOMVAR.CNAMEDRP2,;
              CNAMEDROP3 WITH BOMVAR.CNAMEDRP3,;
              OBJECTID   WITH IIF(EMPTY(OBJLINK.COBJECT_ID),'ZZZZZZZZZZ',OBJLINK.COBJECT_ID),;
              NLINENO    WITH 999999,;
              CSTYLE     WITH REPLICATE('Z',19),;
              NEMBNUM    WITH OCCURS('|',SUBSTR(lcEmbStr,1,AT(BOMVAR.CDSGNCODE,lcEmbStr)-1))+1,;
              CACCOUNT   WITH ORDHDR.ACCOUNT
      REPLACE CPO        WITH POSHDR.PO,;
              CSTATUS    WITH lfGetStats(POSHDR.STATUS),;
              CPIKTKT    WITH POSLN.SHIPNO,;
              CORDER     WITH ORDHDR.ORDER,;
              DSTART     WITH ORDHDR.START,;
              DCOMPLETE  WITH ORDHDR.COMPLETE,;
              CORDNOTE   WITH ORDHDR.NOTE1 + IIF(EMPTY(ORDHDR.NOTE1),'',' ') + ORDHDR.NOTE2,;
              CDESGCODE  WITH BOMVAR.CDSGNCODE,;
              CDESGDESC  WITH ICDESIGN.CDSNNAME,;
              CEMBLOC    WITH BOMVAR.CSTYLEPOS,;
              NSTITCHES  WITH BOMVAR.NSTITCHES,;
              CEMBSTATUS WITH lfGetStats(ICDESIGN.CDSGSTATUS),;
              CNAMEDROP1 WITH BOMVAR.CNAMEDRP1,;
              CNAMEDROP2 WITH BOMVAR.CNAMEDRP2,;
              CNAMEDROP3 WITH BOMVAR.CNAMEDRP3,;
              OBJECTID   WITH IIF(EMPTY(OBJLINK.COBJECT_ID),'ZZZZZZZZZZ',OBJLINK.COBJECT_ID),;
              NLINENO    WITH 999999,;
              CSTYLE     WITH REPLICATE('Z',19),;
              NEMBNUM    WITH OCCURS('|',SUBSTR(lcEmbStr,1,AT(BOMVAR.CDSGNCODE,lcEmbStr)-1))+1,;
              CACCOUNT   WITH ORDHDR.ACCOUNT
      *B605989,4 AMH [End]
      
      *C102533,1 AMH [End]

      *B804473,1 AMH Fix the bug of sorting the colors by SN [Start]
      *IF SEEK('AD'+POSHDR.PO+BOMVAR.CDSGNCODE+STR(BOMVAR.LINENO,6),'THREADCL')
      
      *B127072,1 NNA 03/27/2005 (Begin) Add &lcTmpCur..CEMBLOC to the Seek Expression
      *IF SEEK('AD'+POSHDR.PO+STR(BOMVAR.LINENO,6)+BOMVAR.CDSGNCODE,'THREADCL')
      IF SEEK('AD'+POSHDR.PO+STR(BOMVAR.LINENO,6)+BOMVAR.CDSGNCODE+&lcTmpCur..CEMBLOC,'THREADCL')
      *B127072,1 NNA (End)
      
      *B804473,1 AMH [End]
      
        STORE 0 TO lnLineNo, lnClrCnt
        STORE '' TO lcClrLbl, lcClrCod
        SELECT THREADCL
        *B804473,1 AMH Fix the bug of sorting the colors by SN [Start]
        *SCAN REST WHILE cIdType+cCost_Id+cDsgnCode+STR(LineNo,6)+cStylePos+cThreadCl =;
                        'AD'+POSHDR.PO+BOMVAR.CDSGNCODE+STR(BOMVAR.LINENO,6)
        *B605969,1 RAE Eliminate the duplication on thread sequence. [Start]                        
        *SCAN REST WHILE CIDTYPE+CCOST_ID+STR(LINENO,6)+CDSGNCODE+CSTYLEPOS+CSERIAL =;
                        'AD'+POSHDR.PO+STR(BOMVAR.LINENO,6)+BOMVAR.CDSGNCODE
        *B804473,1 AMH [End]
        SCAN REST WHILE CIDTYPE+CCOST_ID+STR(LINENO,6)+CDSGNCODE+CSTYLEPOS+CSERIAL =;
                        'AD'+POSHDR.PO+STR(BOMVAR.LINENO,6)+BOMVAR.CDSGNCODE+&lcTmpCur..CEMBLOC
        *B605969,1 RAE [End]
          IF lnLineNo <> LINENO
            IF SEEK('A'+POSHDR.PO,'POSLN')
              SELECT POSLN
              LOCATE REST WHILE cStyType+PO+Style+STR(LineNo,6)+TranCd = 'A'+POSHDR.PO;
                          FOR LINENO = THREADCL.LINENO .AND. TRANCD = '1'
            ENDIF
            SELECT (lcTmpCur)
            REPLACE NLINENO    WITH THREADCL.LINENO,;
                    CSTYLE     WITH POSLN.STYLE,;
                    CSCALE     WITH POSLN.SCALE,;
                    NQTY1      WITH POSLN.QTY1,;
                    NQTY2      WITH POSLN.QTY2,;
                    NQTY3      WITH POSLN.QTY3,;
                    NQTY4      WITH POSLN.QTY4,;
                    NQTY5      WITH POSLN.QTY5,;
                    NQTY6      WITH POSLN.QTY6,;
                    NQTY7      WITH POSLN.QTY7,;
                    NQTY8      WITH POSLN.QTY8,;
                    NTOTQTY    WITH POSLN.TOTQTY
            lnLineNo = THREADCL.LINENO
          ENDIF
          lnClrCnt = lnClrCnt + 1
          lcClrLbl = lcClrLbl + 'COLOR' + STR(lnClrCnt,2,0) + ' '
          lcClrCod = lcClrCod + PADR(THREADCL.CTHREADCL,6) + '  '
          IF MOD(lnClrCnt,5) = 0
            SELECT (lcTmpCur)
            REPLACE MTHREADCLR WITH MTHREADCLR + lcClrLbl + CHR(13) + lcClrCod + CHR(13)
            STORE '' TO lcClrLbl, lcClrCod
          ENDIF
        ENDSCAN
        IF !EMPTY(lcClrLbl)
          SELECT (lcTmpCur)
          REPLACE MTHREADCLR WITH MTHREADCLR + lcClrLbl + CHR(13) + lcClrCod
        ENDIF
      ENDIF
    ENDSCAN
    IF SEEK('B'+ORDHDR.ORDER,'NOTEPAD')
      SELECT (lcTmpCur)
      REPLACE MNOTES WITH NOTEPAD.MNOTES
    ENDIF
  ENDIF
ENDSCAN

SELECT (lcTmpCur)
SET RELATION TO 'S'+CSCALE INTO SCALE ADDITIVE
SET RELATION TO OBJECTID INTO OBJECTS ADDITIVE
LOCATE
IF lcRpSortBy = 'S'
  SET ORDER TO TAG (lcTmpCur)
ELSE
  SET ORDER TO TAG (lcTmpCur1)
ENDIF
DO gfDispRe WITH EVAL('lcRpForm')
USE IN (lcTmpCur)
SELECT CUTPICK
SET RELATION TO
SELECT POSHDR
SET RELATION TO
IF llUsePo
  SELECT (lcPoFile)
  SET RELATION TO
ENDIF

*!*************************************************************
*! Name      : lfGetStats
*! Developer : Ahmed Maher (AMH)
*! Date      : 05/28/2001
*! Purpose   : function to get the status of PO.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  Status
*!*************************************************************
*! Example            :  =lfGetStats()
*!*************************************************************
FUNCTION lfGetStats
PARAMETERS lcStatus

DO CASE
  CASE lcStatus = 'O'
    RETURN 'OPEN'
  CASE lcStatus = 'C'
    RETURN 'COMPLETED'
  CASE lcStatus = 'X'
    RETURN 'CANCELED'
  CASE lcStatus = 'A'
    RETURN 'APPROVED'
  CASE lcStatus = 'P'
    RETURN 'PENDING'
  OTHERWISE
    RETURN ''
ENDCASE
*--end of lfGetStats.

*B804462,1 AMH [Start]
*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MAHER
*! Date      : 10/21/2001
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SELECT POSHDR
    SET RELATION TO PADR(Poshdr.vendor,6) INTO WAREHOUS ADDITIVE
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE
*B804462,1 AMH [End]