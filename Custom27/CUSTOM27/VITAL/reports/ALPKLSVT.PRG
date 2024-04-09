*:***************************************************************************
*: Program file  : ALPKLSVT.PRG
*: Program desc. : CUSTOMIZED PACKING LIST FOR VITAL.
*: Date          : 12/18/2001
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : BASSEM RAFAAT ERNEST (BWA)
*: Tracking Job Number: C102506
*: 
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKLSVT
*:***************************************************************************
*: Modifications :
***************************************************************************

DIMENSION laScalVt[5,9]
STORE SPACE(0) TO laScalVt

SELECT (lcPakLnTmp)
INDEX ON PACK_NO + STR(NO_CART,4) + lcType + STYLE + STR(nOrdLineNO,6) TAG (lcPakLTmp2) OF (gcWorkDir+lcPakLnTmp+'.CDX')

DIMENSION laPckLnN[1,4]
=AFIELDS(laPckLnN)

CREATE CURSOR (lcPakLnNew) FROM ARRAY laPckLnN
SELECT (lcPakLnNew)
ZAP
INDEX ON PACK_NO + lcType + STYLE + STR(nOrdLineNO,6) TAG (lcPakLnNew) OF (gcWorkDir + lcPakLnNew +'.CDX')

STORE SPACE(0) TO lcPack_No

SELECT (lcPakLnTmp)
DELETE ALL FOR LCTYPE = "B"
REPLACE ALL &lcPakLnTmp..llPrint WITH .F.
REPLACE ALL &lcPakLnTmp..lcType WITH "A"

SCAN
  IF lcPack_No # PACK_NO
    IF !EMPTY(lcPack_No)
      SKIP - 1
      REPLACE &lcPakLnTmp..llPrint WITH .T.
      SKIP
    ENDIF
    lnLineNo = 0
  ENDIF  

  SCATTER MEMVAR MEMO
  SELECT (lcPakLnNew)
  IF SEEK(&lcPakLnTmp..PACK_NO + "B" + &lcPakLnTmp..STYLE , lcPakLnNew )
    REPLACE &lcPakLnNew..qty1   WITH &lcPakLnNew..qty1   + M.qty1 ,;
            &lcPakLnNew..qty2   WITH &lcPakLnNew..qty2   + M.qty2 ,;
            &lcPakLnNew..qty3   WITH &lcPakLnNew..qty3   + M.qty3 ,;
            &lcPakLnNew..qty4   WITH &lcPakLnNew..qty4   + M.qty4 ,;
            &lcPakLnNew..qty5   WITH &lcPakLnNew..qty5   + M.qty5 ,;
            &lcPakLnNew..qty6   WITH &lcPakLnNew..qty6   + M.qty6 ,;
            &lcPakLnNew..qty7   WITH &lcPakLnNew..qty7   + M.qty7 ,;
            &lcPakLnNew..qty8   WITH &lcPakLnNew..qty8   + M.qty8 ,;
            &lcPakLnNew..Totqty WITH &lcPakLnNew..Totqty + M.Totqty
  ELSE
    lnLineNo = lnLineNo + 1
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE &lcPakLnNew..lcType  WITH "B"      ,;
            &lcPakLnNew..no_cart WITH 9999     ,;
            &lcPakLnNew..line_no WITH lnLineNo ,;
            &lcPakLnNew..llPrint WITH IIF(lnLineNo = 1 , .T. , .F.)

    IF llRpPrnNot AND lnLineNo = 1
      APPEND BLANK
      REPLACE &lcPakLnNew..PACK_NO WITH M.PACK_NO ,;
              &lcPakLnNew..line_no WITH 999999    ,;
              &lcPakLnNew..STYLE   WITH "ZZZZZZZ" ,;
              &lcPakLnNew..no_cart WITH 9999      ,;
              &lcPakLnNew..llPrint WITH .F.       ,;
              &lcPakLnNew..lcType  WITH "B"
    ENDIF

 ENDIF
 lcPack_No = &lcPakLnTmp..PACK_NO
ENDSCAN

SELECT (lcPakLnTmp)
IF !SEEK("ZZZZZZ")
  SKIP - 1
  REPLACE &lcPakLnTmp..llPrint WITH .T.
ENDIF

*-- Append all the records from the new file with the type "B".
SELECT (lcPakLnNew)
SCAN
  SCATTER MEMVAR MEMO
  SELECT (lcPakLnTmp)
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN

SELECT (lcPakLnTmp)
LOCATE

SELECT (lcPackTmp)

*-- Calculate spent time in collecting data.
lcEdTime = TIME()  && Time in which we finish collect data.
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcPackTmp))) +  ' in ' + ALLTRIM(STR(lfCollTime(lcStTime,lcEdTime),6,2)) + ' Seconds...' TIMEOUT 2

DO gfDispRe WITH EVAL('lcFormName')
SET DEVICE TO SCREEN
llALPakLst = .F.
WAIT CLEAR

                       *-- End of the Program --*
*!*************************************************************
*! Name      : lfPrtScal
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/18/2001
*! Purpose   : Function to print the scale in the summary portion.
*!*************************************************************
*! Called from : ALPKLSVT.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPrtScal()
*!*************************************************************
FUNCTION lfPrtScal
PARAMETER lcReturn
PRIVATE lcAlias , lcKeyVt , lcKeyScl , lcKeyStVT

lcAlias = SELECT(0)
STORE SPACE(0) TO laScalVt , lcScaleVt
STORE 0 TO lnLevel

SELECT STYLE
lcKeyStVT = EVAL(KEY())

SELECT (lcPakLnTmp)
lcKeyVt = EVAL(KEY())
=SEEK(PACK_NO)
SCAN REST WHILE PACK_NO + lcType + STYLE + STR(nOrdLineNO,6) = PACK_NO + "A"
  IF SEEK(&lcPakLnTmp..STYLE , 'STYLE')
    IF !(ALLTRIM(SCALE.SCALE) $ lcScaleVt)
      lcScaleVt = lcScaleVt + ALLTRIM(SCALE.SCALE)
      lnLevel = lnLevel + 1
      IF lnLevel < 6
       laScalVt[lnLevel,1] = ALLTRIM(SCALE.SCALE)
        FOR lnLop = 1 TO 8
          lcSz = "Sz" + ALLTRIM(STR(lnLop))
          IF !EMPTY(SCALE.&lcSz)
            laScalVt[lnLevel , lnLop+1 ] = SCALE.&lcSz
          ENDIF
        ENDFOR
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

=SEEK(lcKeyVt)

SELECT STYLE
=SEEK(lcKeyStVT)

SELECT(lcAlias)
RETURN ""

*--End of lfPrtScal.