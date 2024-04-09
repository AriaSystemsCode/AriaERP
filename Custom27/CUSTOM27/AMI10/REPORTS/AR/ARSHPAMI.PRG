*:***************************************************************************
*: Program file  : ARSHPAMI.PRG
*: Program desc. : CUSTOMIZED SHIPPING LOG FOR AMI.
*! Date          : 05/22/2001
*: System        : Aria Advantage Series.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102334
*:
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ARSHPAMI
*:***************************************************************************

*--THE COLOR LENGTH
STORE 0 TO lnClrLnAM , lnClrPosAM
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnAM  = LEN(laItemSeg[lnCount,3])
    lnClrPosAM = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
STORE 0 TO lnLenthAM
lnLenthAM = LEN(gfItemMask('PM'))

*--Section for creating and collecting the data.
=lfCreatTmp()
=lfColctDat()

SELECT (lclines)
SET RELATION TO "S" + &lclines..SCALE INTO SCALE ADDITIVE
LOCATE

DO gfDispRe WITH EVALUATE('lcFormName')

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/22/2001
*! Purpose   : Function to creat temp. file hold the data.
*!*************************************************************
*! Called from : ARSHPAMI.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCreatTmp

CREATE Table (lclines) (Invoice M(10)  , STYLE C(lnLenthAM) , Color C(lnClrLnAM) ,;
                        SCALE C(3)     , Qty1 N(6)          , Qty2 N(6)          ,;
                        Qty3 N(6)      , Qty4 N(6)          , Qty5 N(6)          ,;
                        Qty6 N(6)      , Qty7 N(6)          , Qty8 N(6)          ,;
                        TOTQTY N(15)   , Price N(12,2)      , TotVAl N(14,2))

INDEX ON STYLE + COLOR TAG STYLE OF (gcWorkDir+lcLines)

*--End of lfCreatTmp.
*!*************************************************************
*! Name      : lfColctDat
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/22/2001
*! Purpose   : Function to Collect the data from the invline.
*!*************************************************************
*! Called from : ARSHPAMI.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfcolctDat
PRIVATE lcAlias

STORE SPACE(0) TO lcDataOrd
STORE .F. TO llOrdExp

*--Check if the user select sales order or not.
lnDatPost = AT('ORDHDR.',LCRPEXP)
IF lnDatPost > 0
  =lfCutExp()
  llOrdExp = .T.
ENDIF

lcAlias = ALIAS()
SELECT INVHDR
SET RELATION TO INVHDR.INVOICE INTO INVLINE ADDITIVE
SCAN FOR &LCRPEXP
  WAIT WINDOW 'Selecting Records For The Report ...' NOWAIT

  SELECT INVLINE
  SCAN REST WHILE INVOICE + STR(LINENO,6) = INVHDR.INVOICE
    SCATTER MEMVAR MEMO
    lcStyle = LEFT(INVLINE.STYLE,lnLenthAM)
    lcColor = SUBSTR(INVLINE.STYLE,lnClrPosAM,lnClrLnAM)
    SELECT (lclines)
    IF SEEK(lcStyle+lcColor)
      REPLACE &lclines..Qty1   WITH &lclines..Qty1   + M.QTY1   ,;
              &lclines..Qty2   WITH &lclines..Qty2   + M.QTY2   ,;
              &lclines..Qty3   WITH &lclines..Qty3   + M.QTY3   ,;
              &lclines..Qty4   WITH &lclines..Qty4   + M.QTY4   ,;
              &lclines..Qty5   WITH &lclines..Qty5   + M.QTY5   ,;
              &lclines..Qty6   WITH &lclines..Qty6   + M.QTY6   ,;
              &lclines..Qty7   WITH &lclines..Qty7   + M.QTY7   ,;
              &lclines..Qty8   WITH &lclines..Qty8   + M.QTY8   ,;
              &lclines..TOTQTY WITH &lclines..TOTQTY + M.TOTQTY ,;
              &lclines..TotVAl WITH &lclines..TotVAl + (M.TOTQTY * &lclines..Price),;
              &lclines..Invoice WITH "|" + Invline.invoice ADDITIVE
    ELSE
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE &lclines..Style   WITH lcStyle ,;
              &lclines..Color   WITH lcColor ,;
              &lclines..TotVAl  WITH &lclines..TOTQTY * &lclines..Price
    ENDIF
  ENDSCAN
ENDSCAN
SELECT INVHDR
SET RELATION TO
SELECT(lcAlias)

IF llOrdExp
  lcLineTag = ORDER('INVLINE')
  lcOrdrTag = ORDER('ORDHDR')

  SET ORDER TO TAG INVLINEO IN INVLINE
  SET ORDER TO TAG ORDHDR IN ORDHDR

  SELECT ORDHDR
  SET RELATION TO ORDHDR.ORDER INTO INVLINE ADDITIVE
  SELECT INVLINE
  SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE

  lcAlias = ALIAS()
  SELECT ORDHDR
  SCAN FOR &lcDataOrd
    WAIT WINDOW 'Selecting Records For The Report ...' NOWAIT

    SELECT INVLINE
    SCAN REST WHILE ORDER + STR(LINENO,6) + INVOICE = ORDHDR.ORDER

      SELECT INVLINE
      SCATTER MEMVAR MEMO
      lcStyle = LEFT(INVLINE.STYLE,lnLenthAM)
      lcColor = SUBSTR(INVLINE.STYLE,lnClrPosAM,lnClrLnAM)
      SELECT (lclines)
      IF !(SEEK(lcStyle+lcColor) AND INVLINE.INVOICE $ &lclines..Invoice)
        IF SEEK(lcStyle+lcColor)
          REPLACE &lclines..Qty1   WITH &lclines..Qty1   + M.QTY1   ,;
                  &lclines..Qty2   WITH &lclines..Qty2   + M.QTY2   ,;
                  &lclines..Qty3   WITH &lclines..Qty3   + M.QTY3   ,;
                  &lclines..Qty4   WITH &lclines..Qty4   + M.QTY4   ,;
                  &lclines..Qty5   WITH &lclines..Qty5   + M.QTY5   ,;
                  &lclines..Qty6   WITH &lclines..Qty6   + M.QTY6   ,;
                  &lclines..Qty7   WITH &lclines..Qty7   + M.QTY7   ,;
                  &lclines..Qty8   WITH &lclines..Qty8   + M.QTY8   ,;
                  &lclines..TOTQTY WITH &lclines..TOTQTY + M.TOTQTY ,;
                  &lclines..TotVAl WITH &lclines..TotVAl + (M.TOTQTY * &lclines..Price),;
                  &lclines..Invoice WITH "|" + Invline.invoice ADDITIVE
        ELSE
          APPEND BLANK
          GATHER MEMVAR MEMO
          REPLACE &lclines..Style   WITH lcStyle ,;
                  &lclines..Color   WITH lcColor ,;
                  &lclines..TotVAl  WITH &lclines..TOTQTY * &lclines..Price
        ENDIF
      ENDIF
    ENDSCAN
  ENDSCAN

  SET ORDER TO TAG (lcLineTag) IN INVLINE
  SET ORDER TO TAG (lcOrdrTag) IN ORDHDR

  SELECT ORDHDR
  SET RELATION TO
  SELECT INVLINE
  SET RELATION TO

  SELECT(lcAlias)
ENDIF

*--End of lfcolctDat.
*!*************************************************************
*! Name      : lfCutExp
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/22/2001
*! Purpose   : Function to cut the lcrpexp.
*!*************************************************************
*! Called from : ARSHPAMI.PRG
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCutExp

lnDatPost = AT('INLIST(ORDHDR.',LCRPEXP)
IF lnDatPost = 0
  lnDatPost = AT('BETWEEN(ORDHDR.',LCRPEXP)
ENDIF

lcDataOrd = SUBSTR(lcRpExp ,lnDatPost)
lcRpExp = STRTRAN(lcRpExp , SUBSTR(lcRpExp ,lnDatPost) ,".T.")

*--End of lfCutExp.
*!*************************************************************
*! Name      : lfSROrder
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 05/22/2001
*! Purpose   : Set function for the ORDER number option in case
*!             of In Range.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : 1) 'S' To set the relations
*!                     2) 'R' To release the relations
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfSROrder
PARAMETERS lcParm

IF lcParm = 'S'
  lcInvHdTag = ORDER('INVHDR')
  lcInvlnTag = ORDER('INVLINE')
  lcOrderTag = ORDER('ORDHDR')

  lcAlias = SELECT(0)
  SET ORDER TO TAG INVHDR IN INVHDR
  SET ORDER TO TAG INVLINEO IN INVLINE
  SET ORDER TO TAG ORDHDR IN ORDHDR

  SELECT ORDHDR
  SET RELATION TO ORDHDR.ORDER INTO INVLINE ADDITIVE
  SELECT INVLINE
  SET RELATION TO INVLINE.INVOICE INTO INVHDR ADDITIVE

ELSE

  lcAlias = SELECT(0)
  SET ORDER TO TAG (lcInvHdTag) IN INVHDR
  SET ORDER TO TAG (lcInvlnTag) IN INVLINE
  SET ORDER TO TAG (lcOrderTag) IN ORDHDR

  SELECT ORDHDR
  SET RELATION TO
  SELECT INVLINE
  SET RELATION TO

  SELECT(lcAlias)
ENDIF

*--End of lfSROrder.