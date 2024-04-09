*:***************************************************************************
*: Program file  : ALPKTKDC.PRG
*: Program desc. : CUSTOMIZED PICK TICKET Form FOR DCC
*: Date          : 11/24/2008
*: System        : Aria4xp.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mostafa Eid(MOS)
*: Tracking #    : 201075

*:***************************************************************************
*: Calls : 
*:    Procedures :
*:    Functions  :
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   :
*:***************************************************************************
*: Example : DO ALPKTKDC
*:***************************************************************************
*modifications 
*B608961,1 TMI 8/12/2009 The report run wrong address when printing a batch
*B608961,3 TMI 3/9/2009 The batch prints novalue, wrong size
*B609032,1 HES 10/11/2009 Convert Convert Pick Ticket [T20080806.0004]
*B610191,1 HIA 01/13/2013 AL - error emailing pick ticket at DCC [T20121205.0002]
*:***************************************************************************
PRIVATE lcAlasDl , lcOrdDl , lcKeyDL , lcOldName , lcPhonUser
PRIVATE lcHldScal , lcKey , lcStyClr , lcValStClr , lcScalVal , lcCdm1Val , lcOpenPO , ldAvalbl
STORE SPACE(0) TO lcHldScal , lcKey , lcStyClr , lcValStClr , lcScalVal , lcCdm1Val
STORE SPACE(0) TO  lcOpenPO
STORE 0 TO lnClrLnGl , lnClrPosGl , lnStyLnGl , lnStyPosGl , lnScaLnGl , lnScaPosGl
STORE {} TO ldAvalbl
lcAlasDl = SELECT(0)
lcOrdDl  = ORDER()
lcKeyDL  = EVAL(KEY())
llAlpktk = .F.
=lfChkStrct()

lnLastLine = 0
lnLastLinX = 0

IF !USED('POSHDR')
  =gfOpenTable(oAriaApplication.DataDir+'POSHDR','POSHDR','SH')
ENDIF

IF !USED('POSLN')
 =gfOpenTable(oAriaApplication.DataDir+'POSLN','POSLNS','SH')
ENDIF 
 
SELECT (lcTmpOrdL)

*--Save the Original name to restore it in the end of the program.
lcOldName = lcTmpOrdL

=lfCreatTmp()

SELECT (lcTmpOrdL)
*B130696,1 EIH  03/01/2006 Fix bug that not printing lines from sales order that not piked [Begin]. 
LOCATE
STORE '' TO lcOrder
STORE '' TO lcstore

SCAN 
  IF lcOrder = &lcTmpOrdL..order AND lcstore = &lcTmpOrdL..STORE
    LOOP
  ENDIF
  lcOrder = &lcTmpOrdL..order
  lcstore = &lcTmpOrdL..STORE
 
  SELECT ORDLINE
  lcOldOrder = ORDER()
  gfsetOrder("ORDLINST")
  =gfSEEK('O'+lcOrder+lcstore,'ORDLINE','ORDLINST')
  SCAN REST WHILE cordtype+ORDER+STORE = 'O'+lcOrder+lcstore  FOR TotQty <> 0
    IF !EMPTY(PIKTKT)
      LOOP
    ELSE
      lcPiktkt = &lcTmpOrdL..piktkt
    ENDIF
    SCATTER MEMVAR MEMO
    M.PIKTKT = lcPiktkt
    INSERT INTO (lcTmpOrdL) FROM MEMVAR
  ENDSCAN
  SELECT ordline
  SET ORDER TO &lcOldOrder
  SELECT (lcTmpOrdL)
ENDSCAN
LOCATE
*B130696,1 EIH  03/01/2006 [End]. 

SCAN
  IF LineNo = 0 .AND. Empty(Style)
    Loop
  ENDIF
  lcKey = SUBSTR(STYLE , lnScaPosGl , 2)
  IF !(lcKey $ lcHldScal)
    =lfGetSizes()
    lcHldScal = lcHldScal + IIF(EMPTY(lcHldScal) , "" , "," ) + lcKey
  ENDIF
  SCATTER MEMVAR MEMO
  SELECT (lcAdStyGrp)
  APPEND BLANK
  GATHER MEMVAR MEMO
  REPLACE Account    WITH CUSTOMER.Account                ,;
          cDelivery  WITH IIF(CUSTOMER.llDelivery,'Y','N'),;
          cGroupkey  WITH 'zzzzzz'                        ,;
          StyGrop    WITH STYLE.CSTYGROUP                 ,;
          StyLoc     WITH STYLE.LOCATION                  ,;
          cStyMajor  WITH Style.cStyMajor                ,;
          cConslPikt WITH 'Picking Tickets:  '+ Piktkt.Piktkt,;
          Status     WITH Piktkt.status
ENDSCAN

SELECT (lcAdStyGrp)
INDEX ON  PIKTKT + Account + Store + cDelivery + cGroupkey + cType + StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
SET ORDER TO lcGroup

SELECT (lcTmpOrdL)
SET RELATION TO

lcTmpOrdL = lcAdStyGrp

SELECT (lcTmpOrdL)
LOCATE
lcOldPiktk = ''

SCAN
  =lfGetFit()
  IF !SEEK('zzzzzz'+Piktkt ,lcTmpGroup)
      SELECT (lcTmpGroup)
      APPEN BLANK
      REPLACE Piktkt WITH &lcTmpOrdL..Piktkt,;
              CGroupKey WITH 'zzzzzz'
      SELECT (lcTmpOrdL)
      STORE Piktkt TO lcOldPiktk 
  ENDIF    
ENDSCAN

SELECT (lcTmpGroup)
LOCATE
lcFrmPKTK = Piktkt
GOTO BOTT
lcToPktk = Piktkt
lnToTPktk = RECCOUNT(lcTmpGroup)

*--Collect Data to print in case of use bin location.
IF llUseBin
 lcBnStyGrp = gfTempName()
 lcReptSty = gfTempName()
 
  =lfUseBin()
ENDIF

lctmpPktkt =''
SELECT (lcTmpFit)
llcheckFit = .F.
LOCATE

SCAN
  IF lctmpPktkt <> PIKTKT
  
    *HES
    lcCurSty = STYLE
    lcCurPikTkt = PIKTKT
    LOCATE FOR STYLE = lcCurSty AND PIKTKT = lcCurPikTkt AND !EMPTY(Sz1dl)
    *HES
    
    SCATT MEMVAR MEMO
    m.cType = 'C'
    INSERT INTO (lcTmpOrdL) FROM MEMVAR
    m.cType = 'D'
    INSERT INTO (lcTmpOrdL) FROM MEMVAR
    lctmpPktkt = PIKTKT
    
  ENDIF
  SCATT MEMVAR MEMO
  INSERT INTO (lcTmpOrdL) FROM MEMVAR
  llcheckFit = .T.
ENDSCAN
*B608961,3 TMI 3/9/2009 [START] comment this part, no need for this last line
*!*    IF llcheckFit
*!*      m.cType = 'X'
*!*      INSERT INTO (lcTmpOrdL) FROM MEMVAR
*!*    ENDIF  
*B608961,3 TMI 3/9/2009 [end  ] comment this part, no need for this last line

SELECT (lcTmpOrdL)
SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE
SET RELATION TO CGroupKey+PikTkt INTO (lcTmpGroup) ADDITIVE

IF llRpOrdLNt
  SET RELATION TO 'O' + Order + STR(LineNo,6) INTO ORDLINE ADDITIVE
ENDIF

SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SELECT PIKTKT
SET RELATION TO cWareCode INTO WAREHOUS
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT (lcTmpOrdL)
LOCATE
SET ORDER TO lcGroup

*B128694,1 EIH  08/19/2005  Fix bug that not print header of NON STOCK PRODUCTS [Begin]
*B608961,3 TMI 3/9/2009 Update lPrntHdr [start]
*!*    STORE ' ' to lcKey
*!*    SCAN
*!*      IF lcKey = &lcTmpOrdL..SCALE+&lcTmpOrdL..PIKTKT
*!*        REPLACE LPrntHdr WITH .F.
*!*      ELSE
*!*        REPLACE LPrntHdr WITH .T.
*!*      ENDIF
*!*      lcKey = &lcTmpOrdL..SCALE+&lcTmpOrdL..PIKTKT
*!*    ENDSCAN
STORE ' ' to lcKey
SCAN 
  IF lcKey = &lcTmpOrdL..Sz1DL
    REPLACE LPrntHdr WITH .F.
  ELSE
    REPLACE LPrntHdr WITH .T.
  ENDIF
  lcKey = &lcTmpOrdL..Sz1DL
ENDSCAN
*B608961,3 TMI 3/9/2009 Update lPrntHdr [end  ]
LOCATE
STORE ' ' to lcKey

SCAN FOR CTYPE = 'F'
  IF lcKey = &lcTmpOrdL..STYLE+&lcTmpOrdL..SCALE+&lcTmpOrdL..PIKTKT
    STORE 0 TO lnQTY1,lnQTY2,lnQTY3,lnQTY4,lnQTY5,lnQTY6,lnQTY7,lnQTY8,lnQTY9,lnQTY10,lnQTY11,lnQTY12,lnQTY13,lnQTY14,lnQTY15,lnQTY16,lnTotQty
    FOR I = 1 TO 16 
      lccI = ALLTRIM(STR(I))
      lnQTY&lccI = QTY&lccI
    ENDFOR
    lnTotQty = TotQty
    SKIP -1
    FOR I = 1 TO 16 
      lccI = ALLTRIM(STR(I))
      
      *B609032,1 HES 10/11/2009 Convert Pick Ticket [Start]
      IF llOgFltCh
      *B609032,1 HES 10/11/2009 Convert Pick Ticket [End]
      
      REPLACE QTY&lccI WITH QTY&lccI+lnQTY&lccI 
      
      *B609032,1 HES 10/11/2009 Convert Pick Ticket [Start]
      ENDIF
      *B609032,1 HES 10/11/2009 Convert Pick Ticket [End]
      
    ENDFOR
    
    *B609032,1 HES 10/11/2009 Convert Pick Ticket [Start]
    IF llOgFltCh
    *B609032,1 HES 10/11/2009 Convert Pick Ticket [End]
    
    REPLACE TotQty WITH TotQty + lnTotQty 
    
    *B609032,1 HES 10/11/2009 Convert Pick Ticket [Start]
    ENDIF
    *B609032,1 HES 10/11/2009 Convert Pick Ticket [End]    
    
    SKIP 
    DELETE
  ENDIF
  lcKey = &lcTmpOrdL..STYLE+&lcTmpOrdL..SCALE+&lcTmpOrdL..PIKTKT
ENDSCAN

LOCATE
*B128694,1 EIH  08/19/2005  [End]


ORD_NAME =''
PIK_NAME =''
=FullName()

SELECT (lcORDHDR)
DELETE FOR EMPTY(ORDER)

SELECT (lcTmpOrdL)
LOCATE

DO gfDispRe WITH EVAL('lcFormName')

*B130696,1 EIH  03/01/2006 Fix bug that not printing lines from sales order that not piked [Begin]. 
DELETE FOR EMPTY(PIKDATE)
*B130696,1 EIH  03/01/2006 [End].

*--Print Bulk Summary
IF llRpBlkSum
  SELECT (lcTmpFit)
  ZAP
  INDEX ON cLocation + LEFT(Style,lnScaPosGl + 1)+cType+cDim1  TAG lcTmpFit
  SELECT (lcBnStyGrp)  
  SCAN FOR cType = 'B'
    =lfGetSmFit()
  ENDSCAN
  
  lcRpFrmId = lcFormBulk
  =gfCrtFrm(lcRpFrmId,"",llOGRefForm)
  =lfRepPltFr(lcRpFrmId)
  SELECT(lcTmpFit)
  SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
  LOCATE
  IF !EOF()
    =gfDispRe(lcRpFrmId)
  ENDIF
ENDIF
lcFormName = 'ALPKTKDC'
*--Restore the Original name.
lcTmpOrdL = lcOldName

SELECT (lcAdStyGrp)
SET RELATION TO

SELECT(lcAlasDl)
SET ORDER TO TAG &lcOrdDl
=SEEK(lcKeyDL)

**--Function to clear the Temp. file.
=lfBasToClr(lcAdStyGrp , 'F')

*!*************************************************************
*! Name      : lfCreatTmp
*! Developer : Mostafa Eid(MOS)
*! Date      : 01/19/2009
*! Purpose   : Creat the Tmp. file.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCreatTmp()
*!*************************************************************

FUNCTION lfCreatTmp

SELECT (lcTmpOrdL)
=AFIELDS(laTmpStru)
lnTmpStru = ALEN(laTmpStru,1)

DIMENSION laTmpStru[lnTmpStru + 36 ,18]

lnI = 0
lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'StyGrop'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

*-- Field hold the style group data.
lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'StyLoc'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

laTmpStru[lnTmpStru + 3 ,1] = 'Account'
laTmpStru[lnTmpStru + 3 ,2] = 'C'
laTmpStru[lnTmpStru + 3 ,3] = 5
laTmpStru[lnTmpStru + 3 ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'CGroupKey'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cDelivery'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cConslPikt'
laTmpStru[lnTmpStru + lnI ,2] = 'M'
laTmpStru[lnTmpStru + lnI ,3] = 10
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cStyMajor'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 19
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cLocation'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 10
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cType'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 1
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Ponofolo'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'DatAvlbl'
laTmpStru[lnTmpStru + lnI ,2] = 'D'
laTmpStru[lnTmpStru + lnI ,3] = 8
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Status'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 1
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'cdim1'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz1DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz2DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz3DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz4DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz5DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz6DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz7DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz8DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz9DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz10DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz11DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz12DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz13DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz14DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz15DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Sz16DL'
laTmpStru[lnTmpStru + lnI ,2] = 'C'
laTmpStru[lnTmpStru + lnI ,3] = 5
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Qty9'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Qty10'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Qty11'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0

lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Qty12'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Qty13'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Qty14'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Qty15'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0


lnI = lnI + 1
laTmpStru[lnTmpStru + lnI ,1] = 'Qty16'
laTmpStru[lnTmpStru + lnI ,2] = 'N'
laTmpStru[lnTmpStru + lnI ,3] = 6
laTmpStru[lnTmpStru + lnI ,4] = 0


FOR lnI = 1 TO ALEN(laTmpStru,1)-lnTmpStru
  STORE .F. TO laTmpStru[lnTmpStru+lnI,5],laTmpStru[lnTmpStru+lnI,6]
  STORE ''  TO laTmpStru[lnTmpStru+lnI,7],laTmpStru[lnTmpStru+lnI,8],laTmpStru[lnTmpStru+lnI,9],laTmpStru[lnTmpStru+lnI,10],laTmpStru[lnTmpStru+lnI,11],;
               laTmpStru[lnTmpStru+lnI,12],laTmpStru[lnTmpStru+lnI,13],laTmpStru[lnTmpStru+lnI,14],laTmpStru[lnTmpStru+lnI,15],laTmpStru[lnTmpStru+lnI,16]
  STORE 0  TO laTmpStru[lnTmpStru+lnI,17],laTmpStru[lnTmpStru+lnI,18]
ENDFOR  

lcAdStyGrp = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir + lcAdStyGrp) FROM ARRAY laTmpStru
INDEX ON ACCOUNT + STORE + cStyMajor TAG lcGroup1
INDEX ON ACCOUNT + STORE + Style TAG lcGroup2 ADDITIVE 

DO CASE
  CASE lcRpPrGrS = "O"
      IF lcRpPrGrA = 'A'
        INDEX ON  Status + Account + Store + cDelivery + cGroupkey + PIKTKT +cType + StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
      ELSE
        INDEX ON  PIKTKT + Account + Store + cDelivery + cGroupkey + cType + StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
      ENDIF  
  CASE lcRpPrGrS = "P"
      INDEX ON  Status +PIKTKT + Account + Store + cDelivery + cGroupkey +cType + StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
ENDCASE
SET ORDER TO lcGroup1

*=gfCrtTmp(lcTmpFit,@laTmpStru)
CREATE TABLE (oAriaApplication.WorkDir+ lcTmpFit) FROM ARRAY laTmpStru
INDEX ON PIKTKT + LEFT(Style,lnScaPosGl + 1) + cdim1  TAG lcTmpFit

CREATE TABLE (oAriaApplication.WorkDir+lcTmpGroup) ( CGroupKey C(6) , Piktkt C(6))
INDEX ON CGroupKey+PikTkt  Tag (lcTmpGroup)

*--File create the scale and sizes.
CREATE TABLE (oAriaApplication.WorkDir + lcTmpSizes) ( ScalFld C(2) , IndxDm2 N(3) , cDim1 C(5) , llPrnSDc L ,;
                                        Sz1 C(5) , Sz2 C(5) , Sz3 C(5) , Sz4 C(5) , Sz5 C(5) , Sz6 C(5) , Sz7 C(5) , Sz8 C(5) ,;
                                        Sz9 C(5) , Sz10 C(5) , Sz11 C(5) , Sz12 C(5) , Sz13 C(5) , Sz14 C(5) , Sz15 C(5) , Sz16 C(5))

INDEX ON ScalFld + ALLTRIM(STR(IndxDm2)) Tag SortScal
INDEX ON ScalFld + cDim1 Tag (lcTmpSizes) ADDITIVE

*-------------- End Of lfCreatTmp ---------------

*!*************************************************************
*! Name      : lfBasToClr
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*! Purpose   : deleting temp. files.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : 1) lcFilName : hold the file name or array hold more than one file
*!                   : 2) lcTypFun  : 'F' for one file
*!                   :              : 'A' for array hold more than one file.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfBasToClr(CUTTTEMP , 'F')     >> one file.
*!             : =lfBasToClr(@laFileName , 'A')  >> more than one file.
*!*************************************************************
FUNCTION lfBasToClr
PARAMETERS lcFilName , lcTypFun

IF lcTypFun = "F"
  IF USED(lcFilName)
    SELECT (lcFilName)
    USE
  ENDIF
ELSE
  FOR lnLop = 1 TO ALEN(lcFilName,1)
    IF USED(lcfilname[lnLop])
      SELECT (lcfilname[lnLop])
      USE
    ENDIF
  ENDFOR
ENDIF

*--End of lfBasToClr.
*!*************************************************************
*! Name      : LFDelPhon
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*! Purpose   : Function to delete the phone number.
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =LFDelPhon()
*!*************************************************************
FUNCTION LFDelPhon
PARAMETER lcReturn

FOR LnPh = 1 TO ALEN(laSoldTo,1)
  IF 'Phone#' $ laSoldTo[LnPh]
    laSoldTo[LnPh,1] = SPACE(0)
  ENDIF
ENDFOR

FOR LnPh = 1 TO ALEN(laShipTo,1)
  IF 'Phone#' $ laShipTo[LnPh]
    laShipTo[LnPh,1] = SPACE(0)
  ENDIF
ENDFOR

RETURN ""
*--End of LFDelPhon
*!*************************************************************
*! Name      : lfUseBin
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*! Purpose   : Function follow concept of Using Bin Locations.
*!*************************************************************
*! Called from : ALPKLSDB.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfUseBin()
*!*************************************************************
FUNCTION lfUseBin
PRIVATE lcRePsTY , lcCurPktkt , lcRepBin 

*B609032,1 HES 10/11/2009 Use the appropraie index for Bin Issue [Start]
*!*	IF !USED('PKBINLOC')                      
*!*	  =gfOpenTable('PKBINLOC','PKBINPKT','SH')
*!*	ENDIF

IF !USED('PKBINLOC')                      
  =gfOpenTable('PKBINLOC','PKLINE','SH') && PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION
ELSE 
  SELECT PKBINLOC
  gfSetOrder('PKLINE')
ENDIF
*B609032,1 HES 10/11/2009 Use the appropraie index for Bin Issue [End

=lfCrtTmp()
lcRePsTY   = ''
lcCurPktkt = ''
lcRepBin   = ''
SELECT (lcTmpOrdL)
SCAN
    SCATT MEMVAR MEMO
    lcBnPikTkt = PIKTKT

    IF !(OCCURS(',',m.cConslPikt)>0)
*B609032,1 HES 10/11/2009 To handle the issue of didn't add a stock line if this style has more than 1 line in SO [Start]  
*!*	      IF SEEK(Account + Store +  PIKTKT + STYLE,lcReptSty)
*!*	        LOOP
*!*	      ELSE
*B609032,1 HES 10/11/2009 Convert Pick Ticket [End]
        INSERT INTO (lcReptSty) FROM MEMVAR
*B609032,1 HES 10/11/2009 Convert Pick Ticket [Start]        
*!*	      ENDIF
*B609032,1 HES 10/11/2009 To handle the issue of didn't add a stock line if this style has more than 1 line in SO [End]
    ENDIF  

    SELECT PKBINLOC  && PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION
    *B609032,1 HES 10/11/2009 Just load the neede data to prevent adding wrong data [Start]
*!*	    IF gfSEEK(lcBnPikTkt)
*!*	      SCAN REST WHILE PIKTKT+CWARECODE+CLOCATION+STYLE = ALLTRIM(lcBnPikTkt)    
    IF gfSEEK(lcBnPikTkt+&lcTmpOrdL..CWARECODE+STR(&lcTmpOrdL..LINENO,6)+&lcTmpOrdL..Style)
      SCAN REST WHILE PikTkt+cWareCode+STR(LINENO,6)+STYLE+CLOCATION = ALLTRIM(lcBnPikTkt)
    *B609032,1 HES 10/11/2009 Just load the neede data to prevent adding wrong data [End]
       
        *B609032,1 HES 10/11/2009 Prevent a Nonstock line to be processed [Start]      
*!*	        IF !(PKBINLOC.STYLE = &lcTmpOrdL..Style)
*!*	          LOOP
*!*	        ENDIF      
        IF !(PKBINLOC.STYLE = &lcTmpOrdL..Style) OR &lcTmpOrdL..totpik = 0
          LOOP
        ENDIF
        *B609032,1 HES 10/11/2009 Prevent a Nonstock line to be processed [End]

        FOR lncI = 1 TO 8 
          lccI = ALLTRIM(STR(lncI,1))
            m.Pik&lccI  =  Qty&lccI 
        ENDFOR
        
        m.cLocation = cLocation
        m.cWareCode = cWareCode
        IF !SEEK(m.Account+m.Store+cWareCode+cLocation+Style+PikTkt,lcBnStyGrp)
          =SEEK('S'+&lcTmpOrdL..Scale,'SCALE')
          m.cDim1 =Scale.cDim1 
          SELECT (lcBnStyGrp)
          APPEND BLANK
          m.cType = 'B'
          GATHER MEMVAR MEMO
        ELSE
          SELECT (lcBnStyGrp)
          REPLACE Pik1 WITH Pik1 + m.Pik1,;
                  Pik2 WITH Pik2 + m.Pik2;
                  Pik3 WITH Pik3 + m.Pik3;
                  Pik4 WITH Pik4 + m.Pik4;
                  Pik5 WITH Pik5 + m.Pik5;
                  Pik6 WITH Pik6 + m.Pik6;
                  Pik7 WITH Pik7 + m.Pik7;
                  Pik8 WITH Pik8 + m.Pik8
        ENDIF 
      ENDSCAN
    ENDIF  
ENDSCAN

*B609032,1 HES 10/11/2009 Reset the Index as default [Start]
SELECT PKBINLOC
gfSetOrder('PKBINPKT')
*B609032,1 HES 10/11/2009 Reset the Index as default [End]

lcTmpOrdL = lcBnStyGrp 
SELECT (lcTmpOrdL)
INDEX ON   PIKTKT + Account + Store + cDelivery + cGroupkey +cType+ cLocation+ StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
SET ORDER TO lcGroup

LOCATE
*--End of lfUseBin.
*!*************************************************************
*! Name      : lfCrtTmp
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*! Purpose   : Function follow concept of Using Bin Locations.
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfCrtTmp()
*!*************************************************************
FUNCTION lfCrtTmp

SELECT (lcTmpOrdL)
=AFIELDS(laTmpStru)
lnTmpStru = ALEN(laTmpStru,1)
*B128694,1 EIH  08/19/2005  Add field to Fix bug that not print header of NON STOCK PRODUCTS [Begin]

DIMENSION laTmpStru[ALEN(laTmpStru , 1) + 1 , 18]
laTmpStru[ALEN(laTmpStru , 1) , 1] = 'LPrntHdr'
laTmpStru[ALEN(laTmpStru , 1) , 2] = 'L'
laTmpStru[ALEN(laTmpStru , 1) , 3] = 1
laTmpStru[ALEN(laTmpStru , 1) , 4] = 0

*B128694,1 EIH  08/19/2005  [End]

FOR lnI = 1 TO ALEN(laTmpStru,1)-lnTmpStru
  STORE .F. TO laTmpStru[lnTmpStru+lnI,5],laTmpStru[lnTmpStru+lnI,6]
  STORE ''  TO laTmpStru[lnTmpStru+lnI,7],laTmpStru[lnTmpStru+lnI,8],laTmpStru[lnTmpStru+lnI,9],laTmpStru[lnTmpStru+lnI,10],laTmpStru[lnTmpStru+lnI,11],;
               laTmpStru[lnTmpStru+lnI,12],laTmpStru[lnTmpStru+lnI,13],laTmpStru[lnTmpStru+lnI,14],laTmpStru[lnTmpStru+lnI,15],laTmpStru[lnTmpStru+lnI,16]
  STORE 0  TO laTmpStru[lnTmpStru+lnI,17],laTmpStru[lnTmpStru+lnI,18]
ENDFOR  

lcBnStyGrp = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir+ lcBnStyGrp) FROM ARRAY laTmpStru

INDEX ON cLocation+ACCOUNT + STORE + cStyMajor TAG lcGroup1
INDEX ON cLocation+ACCOUNT + STORE + Style TAG lcGroup2 ADDITIVE

DO CASE
  CASE lcRpPrGrS = "O"
      IF lcRpPrGrA = 'A'
        INDEX ON  Status + Account + Store + cDelivery + cGroupkey + PIKTKT +cType+ cLocation+ StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
      ELSE
        INDEX ON  Status + PIKTKT + Account + Store + cDelivery + cGroupkey +cType+ cLocation+ StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
      ENDIF
  CASE lcRpPrGrS = "P"
      INDEX ON   PIKTKT + Account + Store + cDelivery + cGroupkey +cType+ cLocation+ StyGrop + STYLE + CGRUPDETAL + STR(LINENO,6) TAG lcGroup ADDITIVE
ENDCASE

INDEX ON cWareCode+cLocation+Style TAG lcGroup3
SET ORDER TO lcGroup3

lcReptSty = gfTempName()
CREATE TABLE (oAriaApplication.WorkDir + lcReptSty) FROM ARRAY laTmpStru
INDEX ON Account + Store +  PIKTKT + STYLE  TAG lcReptSty ADDITIVE

*--End of lfCrtTmp.
*!*************************************************************
*! Name      : lfAddField
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*! Purpose   : Add fields to the array of file structure.
*!*************************************************************
*! Called from :
*!*************************************************************
*! Passed Parameters : lcFldName -- Field Name
*!                   : lcFldType -- Field Type (C;N;L....M)
*!                   : lnFldLen  -- Field Length
*!                   : lnFldDec  -- Field Decimal
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfAddField()
*!*************************************************************
FUNCTION lfAddField
PARAMETERS lcStruArry , lcFldName , lcFldType , lnFldLen , lnFldDec
lnFldPos  = ALEN(&lcStruArry,1) + IIF(TYPE('&lcStruArry') = 'L', 0 , 1 )
DIMENSION &lcStruArry[lnFldPos , 4]
&lcStruArry[lnFldPos , 1]	= lcFldName
&lcStruArry[lnFldPos , 2]	= lcFldType
&lcStruArry[lnFldPos , 3]	= lnFldLen
&lcStruArry[lnFldPos , 4]	= lnFldDec

*--End of lfAddField.

*!*************************************************************
*! Name      : lfGetSizes
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*! Purpose   : Function to collect the scale data.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example            : =lfGetSizes()
*!*************************************************************
FUNCTION lfGetSizes
PRIVATE lcAlias

lcAlias = ALIAS()
PRIVATE lnI , lnHdr , lnScalRec , lnContSrt
STORE 0 TO lnI , lnHdr , lnScalRec , lnContSrt

SELECT SCALE
lnScalRec = IIF(EOF('SCALE') , 0 , RECNO('SCALE'))
LOCATE

IF SEEK("S" + SUBSTR(&lcTmpOrdL..STYLE , lnScaPosGl , 2))
  lnContSrt = 1
  SCAN FOR TYPE + SCALE + PREPAK = "S" + lcKey
    SCATTER MEMVAR MEMO
    SELECT (lcTmpSizes)
    SET ORDER TO TAG (lcTmpSizes)

    IF SEEK(lcKey + cDim1 , lcTmpSizes)
      IF &lcTmpSizes..cDim1 == SCALE.Cdim1
        FOR lnCrtTmp = 1 TO 8
          lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp+8))
          lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
          IF !EMPTY(SCALE.&lcSizFld)
            REPLACE &lcTmpSizes..&lcNumSiz WITH SCALE.&lcSizFld ,;
                    &lcTmpSizes..cDim1     WITH SCALE.Cdim1
          ENDIF
        ENDFOR
      ELSE
        lnContSrt = lnContSrt + 1
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE &lcTmpSizes..ScalFld WITH LEFT(SCALE.SCALE,2) ,;
                &lcTmpSizes..cDim1   WITH SCALE.Cdim1         ,;
                &lcTmpSizes..IndxDm2 WITH lnContSrt
      ENDIF
    ELSE
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE &lcTmpSizes..ScalFld WITH LEFT(SCALE.SCALE,2) ,;
              &lcTmpSizes..IndxDm2 WITH 1
    ENDIF
  ENDSCAN
ENDIF

SELECT (lcTmpSizes)
SET ORDER TO TAG SortScal
REPLACE &lcTmpSizes..llPrnSDc WITH .T.

SELECT SCALE
IF lnScalRec <> 0
  GOTO lnScalRec IN SCALE
ENDIF

SELECT(lcAlias)

*--End of lfGetSizes.

*!*************************************************************
*! Name      : lfChkStrct
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     :
*!         Procedures : ....
*!         Functions  : ....
*!*************************************************************
*! Called from        : 
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLnGl  = LEN(laItemSeg[lnCount,3])
    lnClrPosGL = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLnGl  = LEN(laItemSeg[lnCount,3])
    lnStyPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLnGl  = LEN(laItemSeg[lnCount,3])
    lnScaPosGl = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--End of lfChkStrct.
*:*************************************************************
*: Name      : lfOpenPo
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*: Purpose   : Function to check for the open PO.
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfOpenPo()
*:*************************************************************

FUNCTION lfOpenPo

PRIVATE lcAlasPo , lcEvalKyPo , lcStyCheck
STORE SPACE(0) TO lcOpenPO , lcStyCheck
STORE {} TO ldAvalbl
STORE .F. TO llRtrnVl

lcAlasPo = SELECT(0)
SELECT POSLN
lcEvalKyPo = EVAL(KEY())
gfSEEK('0001'+&lcTmpFit..STYLE,'POSLN','POSLNS')
SCAN REST WHILE CINVTYPE+STYLE+CBUSDOCU+CSTYTYPE+PO+STR(LINENO,6)+TRANCD  = '0001'+&lcTmpFit..STYLE FOR ;
gfSEEK(CBUSDOCU+CSTYTYPE+PO,'POSHDR','POSHDR') AND POSHDR.STATUS $ "HO"
lcStyCheck = POSLN.STYLE
  SKIP
  IF lcStyCheck == POSLN.STYLE
    IF TRANCD = ALLTRIM(STR(1))
  	  SKIP - 1
    ENDIF
  ELSE
    SKIP - 1
    lcOpenPO = POSHDR.PO
    ldAvalbl = POSHDR.AVAILABLE + 5
    llRtrnVl = .T.
    EXIT
  ENDIF
ENDSCAN

=SEEK(lcEvalKyPo)
SELECT(lcAlasPo)

RETURN llRtrnVl

*--End of lfOpenPo.
*:*************************************************************
*: Name      : lfGetFit
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*: Purpose   : Function to check fit.
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfGetFit()
*:*************************************************************
FUNCTION lfGetFit
PRIVATE lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty
STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty
SELECT (lcTmpOrdL)
FOR lncI = 1 TO 8 
  lccI = ALLTRIM(STR(lncI,1))
  IF !EMPTY(&lcTmpOrdL..Qty&lccI) AND (&lcTmpOrdL..Qty&lccI > &lcTmpOrdL..Pik&lccI ) 
    lnQty&lccI  =  (&lcTmpOrdL..Qty&lccI - &lcTmpOrdL..Pik&lccI ) 
    lnTotQty    = lnTotQty +lnQty&lccI
  ENDIF  
ENDFOR


IF !EMPTY(lnTotQty)

  =SEEK('S'+Scale,'SCALE')
  IF !SEEK(PIKTKT + LEFT(Style,lnScaPosGl + 1)+Scale.cdim1,lcTmpFit)
    SCATT MEMVAR MEMO
    SELECT(lcTmpFit)
    APPEND BLANK
    m.ctype = 'F'
    
    *B128694,1 EIH  08/19/2005  Fix bug that not print valid scales [Begin]
    *B608961,3 TMI 3/9/2009 locate the correct scale [start]
    *=SEEK( LEFT(&lcTmpOrdL..SCALE,2) , lcTmpSizes )
    =SEEK( LEFT(&lcTmpOrdL..SCALE,2)+SCALE.CDIM1 , lcTmpSizes , lcTmpSizes )
    *B608961,3 TMI 3/9/2009 locate the correct scale [end]
    *B608961,3,TMI 3/9/2009 reset the lnCI2 to 0 [start]
    *lnCI2 = 1
    lnCI2 = 0
    *B608961,3,TMI 3/9/2009 reset the lnCI2 to 0 [end  ]
    FOR lnCI = 1 TO 16
      lccI = ALLTRIM(STR(lncI))
      IF (SCALE.Sz1 <> &lcTmpSizes..Sz&lccI)
        LOOP
      ELSE
        lnCI2 = lnCI - 1
        EXIT
      ENDIF
    ENDFOR
    FOR lncI = 1 TO 8 
      lccI = ALLTRIM(STR(lncI ,1))
      m.Qty&lccI  = 0
    ENDFOR
    
    *B128694,1 EIH  08/19/2005  [End]
    
    FOR lncI = 1 TO 8 
      lccI = ALLTRIM(STR(lncI ,1))
      *B128694,1 EIH  08/19/2005  Fix bug that not print valid scales [Begin]
      *m.Qty&lccI1  = lnQty&lccI  
      lccI3 = ALLTRIM(STR(lncI+lnCI2))
      m.Qty&lccI3  = lnQty&lccI  
      *B128694,1 EIH  08/19/2005  [End]
    ENDFOR
    
    m.TotQty    = lnTotQty 
    m.cdim1 = Scale.cdim1
    GATHER MEMVAR MEMO
 
    IF lfOpenPo()
      REPLACE &lcTmpFit..Ponofolo WITH lcOpenPO ,;
              &lcTmpFit..DatAvlbl WITH ldAvalbl
    ENDIF

    *B128694,1 EIH  08/19/2005  Fix bug that not print valid scales [Begin]
    *=SEEK( LEFT(&lcTmpOrdL..SCALE,1) , lcTmpSizes )
    
    *B608961,3 TMI 3/9/2009 locate the correct scale [start]
    *=SEEK( LEFT(&lcTmpOrdL..SCALE,2) , lcTmpSizes )
    =SEEK( LEFT(&lcTmpOrdL..SCALE,2)+SCALE.CDIM1 , lcTmpSizes , lcTmpSizes )
    *B608961,3 TMI 3/9/2009 locate the correct scale [end]
    *B128694,1 EIH  08/19/2005  [End]
    
    FOR lnCrtTmp = 1 TO 16
      lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp)) + "dl"
      lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
      IF !EMPTY(&lcTmpSizes..&lcSizFld)
        REPLACE &lcTmpFit..&lcNumSiz WITH &lcTmpSizes..&lcSizFld
      ENDIF
    ENDFOR
    
  ELSE
    IF &lcTmpOrdL..Scale =  &lcTmpFit..Scale
      SCATT MEMVAR MEMO
      SELECT(lcTmpFit)
      APPEND BLANK
      m.ctype = 'F'
      FOR lncI = 1 TO 8 
        lccI = ALLTRIM(STR(lncI,1))
        m.Qty&lccI  = lnQty&lccI  +Qty&lccI
      ENDFOR
      m.TotQty    = lnTotQty + TotQty
      GATHER MEMVAR MEMO
    ELSE
      SELECT(lcTmpFit)
      FOR lncI = 9 TO 16 
        IF lncI > 9 
          lccI = ALLTRIM(STR(lncI,2))
          lccIO = ALLTRIM(STR(lncI-8,1))
        ELSE
          lccI = ALLTRIM(STR(lncI,1))
          lccIO = ALLTRIM(STR(lncI-8,1))
        ENDIF
        m.Qty&lccI  = lnQty&lccIO + Qty&lccI
      ENDFOR
      m.TotQty    = lnTotQty + TotQty
      REPLACE QTY9 WITH m.QTY9 ,;
              QTY10 WITH m.QTY10 ,;
              QTY11 WITH m.QTY11 ,;
              QTY12 WITH m.QTY12 ,;
              QTY13 WITH m.QTY13 ,;
              QTY14 WITH m.QTY14 ,;
              QTY15 WITH m.QTY15 ,;
              QTY16 WITH m.QTY16 ,;
              TOTQTY WITH m.TotQTY 
    ENDIF
  ENDIF
ENDIF

SELECT(lcTmpOrdL)


*:*************************************************************
*: Name      : lfvOStat
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*: Purpose   : Function to Validate Piktkt Status.
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfvOStat()
*:*************************************************************
FUNCTION lfvOStat
PARAMETER LCDUMMY

CLEAR READ
LCDUMMY = .T.

RETURN LCDUMMY

*:*************************************************************
*: Name      : lfGetSmFit
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
*: Purpose   : Function to Summary fit.
*:*************************************************************
*: Called from :
*:*************************************************************
*: Calls       : ....
*:*************************************************************
*: Passed Parameters : None
*:*************************************************************
*: Return      : None
*:*************************************************************
*: Example     : =lfGetFit()
*:*************************************************************
FUNCTION lfGetSmFit

PRIVATE lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty
STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnTotQty
SELECT (lcBnStyGrp)
FOR lncI = 1 TO 8 
  lccI = ALLTRIM(STR(lncI,1))
  IF !EMPTY(&lcBnStyGrp..Pik&lccI) 
    lnQty&lccI  =  &lcBnStyGrp..Pik&lccI 
    lnTotQty    = lnTotQty +lnQty&lccI
  ENDIF  
ENDFOR

IF !EMPTY(lnTotQty)
  IF !SEEK(cLocation + LEFT(Style,lnScaPosGl + 1),lcTmpFit)
    SCATT MEMVAR MEMO
    SELECT(lcTmpFit)
    APPEND BLANK
    m.ctype = 'A'
    GATHER MEMVAR MEMO
    =SEEK( LEFT(&lcBnStyGrp..SCALE,2) , lcTmpSizes )
    FOR lnCrtTmp = 1 TO 16
      lcNumSiz = "Sz" + ALLTRIM(STR(lnCrtTmp)) + "dl"
      lcSizFld = "Sz" + ALLTRIM(STR(lnCrtTmp))
      IF !EMPTY(&lcTmpSizes..&lcSizFld)
        REPLACE &lcTmpFit..&lcNumSiz WITH &lcTmpSizes..&lcSizFld
      ENDIF
    ENDFOR
  ENDIF
  SELECT (lcBnStyGrp)
  =SEEK('S'+Scale,'SCALE')
  IF !SEEK(cLocation + LEFT(Style,lnScaPosGl + 1)+'F'+Scale.cdim1,lcTmpFit) 
    SCATT MEMVAR MEMO
    SELECT(lcTmpFit)
    APPEND BLANK
    m.ctype = 'F'
    
    
       
    *B128694,1 EIH  08/19/2005  Fix bug that not print valid scales [Begin]
    =SEEK( LEFT(&lcTmpOrdL..SCALE,2) , lcTmpSizes )
    *B608961,3,TMI 3/9/2009 reset the lnCI2 to 0 [start]
    *lnCI2 = 1
    lnCI2 = 0
    *B608961,3,TMI 3/9/2009 reset the lnCI2 to 0 [end  ]
    FOR lnCI = 1 TO 16
      lccI = ALLTRIM(STR(lncI))
      IF (SCALE.Sz1 <> &lcTmpSizes..Sz&lccI)
        LOOP
      ELSE
        lnCI2 = lnCI - 1
        EXIT
      ENDIF
    ENDFOR
    FOR lncI = 1 TO 8 
      lccI = ALLTRIM(STR(lncI ,1))
      m.Qty&lccI  = 0
    ENDFOR
    *B128694,1 EIH  08/19/2005 [END]
    
    
    FOR lncI = 1 TO 8 
      lccI = ALLTRIM(STR(lncI,1))
      *B128694,1 EIH  08/19/2005  Fix bug that not print valid scales [Begin]
      *m.Qty&lccI1  = lnQty&lccI  
      lccI3 = ALLTRIM(STR(lncI+lnCI2))
      m.Qty&lccI3  = lnQty&lccI  
      *B128694,1 EIH  08/19/2005  [End]
    ENDFOR
    m.TotQty    = lnTotQty 
    m.cdim1 = Scale.cdim1
    GATHER MEMVAR MEMO
    
  ELSE
    IF &lcBnStyGrp..Scale =  &lcTmpFit..Scale 
      SELECT(lcTmpFit)
      FOR lncI = 1 TO 8 
        lccI = ALLTRIM(STR(lncI,1))
        m.Qty&lccI  = lnQty&lccI  +Qty&lccI
      ENDFOR
      m.TotQty    = lnTotQty + TotQty
      REPLACE QTY1 WITH m.QTY1 ,;
              QTY2 WITH m.QTY2 ,;
              QTY3 WITH m.QTY3 ,;
              QTY4 WITH m.QTY4 ,;
              QTY5 WITH m.QTY5 ,;
              QTY6 WITH m.QTY6 ,;
              QTY7 WITH m.QTY7 ,;
              QTY8 WITH m.QTY8 ,;
              TOTQTY WITH m.TotQTY 
    ELSE
      SELECT(lcTmpFit)
      FOR lncI = 9 TO 16 
        IF lncI > 9 
          lccI = ALLTRIM(STR(lncI,2))
          lccIO = ALLTRIM(STR(lncI-8,1))
        ELSE
          lccI = ALLTRIM(STR(lncI,1))
          lccIO = ALLTRIM(STR(lncI-8,1))
        ENDIF
        m.Qty&lccI  = lnQty&lccIO + Qty&lccI
      ENDFOR
      m.TotQty    = lnTotQty + TotQty
      REPLACE QTY9 WITH m.QTY9 ,;
              QTY10 WITH m.QTY10 ,;
              QTY11 WITH m.QTY11 ,;
              QTY12 WITH m.QTY12 ,;
              QTY13 WITH m.QTY13 ,;
              QTY14 WITH m.QTY14 ,;
              QTY15 WITH m.QTY15 ,;
              QTY16 WITH m.QTY16 ,;
              TOTQTY WITH m.TotQTY 
    ENDIF
  ENDIF
ENDIF

SELECT(lcBnStyGrp)

*B130696,1 EIH  03/01/2006 Printing Total in alpkblk.frx [Begin] 
*!*************************************************************
*! Name      : lfGtTotpk
*! Developer : EHAB ISMAIL HAMED(EIH)
*! Date      : 03/09/2006
*! Purpose   : Get Total Piktkt from Pikline file
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGtTotpk()
*!*************************************************************
FUNCTION lfGtTotpk
PARAMETERS lnReturn
lnReturn =0
lnAlias = ALIAS()

STORE '' TO lcPikTkt
lcPikTkt = &lcTmpOrdL..Piktkt
*SCAN 
*!*	  IF ALLTRIM(lcPikTkt) = ALLTRIM(&lcTmpFit..Piktkt)
*!*	    LOOP
*!*	  ENDIF

SELECT PIKTKT
=SEEK(lcPikTkt)
IF PIKTKT.STATUS = 'O'
  IF SEEK('O'+PIKTKT.ORDER,'ORDLINE')
    SELECT ORDLINE
    SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + PIKTKT.Order FOR PIkTKT = lcPikTkt
      lnReturn = lnReturn+ TotPik 
    ENDSCAN
  ENDIF
ELSE
  IF SEEK(lcPikTkt,'PIKLINE')
    SELECT PIKLINE
    SCAN REST WHILE piktkt+order+STR(lineno,6) = lcPikTkt
      lnReturn = lnReturn+ TotPik 
    ENDSCAN
  ENDIF
ENDIF
*ENDSCAN  
SELECT (lnAlias)
RETURN lnReturn

*!*************************************************************
*! Name      : lfGtTotpk2
*! Developer : EHAB ISMAIL HAMED(EIH)
*! Date      : 03/09/2006
*! Purpose   : Get Total Piktkt from Pikline file
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGtTotpk2()
*!*************************************************************
FUNCTION lfGtTotpk2
PARAMETERS lnReturn
lnReturn =0
lnAlias = ALIAS()
SELECT PIKTKT
=SEEK(&lcTmpOrdL..Piktkt)
IF PIKTKT.STATUS = 'O'
  IF SEEK('O'+&lcTmpOrdL..ORDER,'ORDLINE')
    SELECT ORDLINE
    SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + &lcTmpOrdL..Order
      IF &lcTmpOrdL..Piktkt <> OrdLine.Piktkt
        LOOP
      ENDIF
      lnReturn = lnReturn+ TotPik 
    ENDSCAN
  ENDIF
ELSE
  IF SEEK(&lcTmpOrdL..PIKTKT,'PIKLINE')
    SELECT PIKLINE
    SCAN REST WHILE piktkt+order+STR(lineno,6) = &lcTmpOrdL..Piktkt
      lnReturn = lnReturn+ TotPik 
    ENDSCAN
  ENDIF
ENDIF
SELECT (lnAlias)
RETURN lnReturn

*B130696,1 EIH  03/01/2006 [End] 
*!*************************************************************
*! Name      : lfGtTotpk3
*! Developer : EHAB ISMAIL HAMED(EIH)
*! Date      : 03/09/2006
*! Purpose   : Get Total Piktkt from PkBinLoc file
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGtTotpk3()
*!*************************************************************
FUNCTION lfGtTotpk3
PARAMETERS lnReturn
lnReturn =0
lnAlias = ALIAS()
SELECT PKBINLOC
lcoldord = ORDER()
=gfSetOrder('Pkbinpkt')

=gfSEEK(&lcTmpOrdL..Piktkt)
SCAN REST WHILE piktkt+cwarecode+clocation+style = &lcTmpOrdL..Piktkt
  lnReturn = lnReturn+ TOTQTY
ENDSCAN

SELECT PKBINLOC
gfSetORder(lcoldord)

SELECT (lnAlias)
RETURN lnReturn
*:**************************************************************************
*:* Name        : lfchkShpAd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/24/2008
*:* Purpose     : check the shipping address
*:***************************************************************************
*:* Called from : frx
*:***************************************************************************
FUNCTION lfchkShpAd  

*B608961,1 tmi
LOCAL laShipTo,lnSlct
lnSlct = SELECT(0)
DIMENSION laShipTo[5]
laShipTo = ''
*B608961,1 TMI

*B608961,1 tmi 8/18/2009
LOCAL lcShpTName
lcShpTName = ''
*B608961,1 tmi 8/18/2009


lnLastLine = 0
lnLastLinX = 0

SELECT (lcORDHDR)
=SEEK('O'+ &lcTmpOrdL..order)
IF &lcORDHDR..Alt_ShpTo

  SELECT (lcORDHDR)
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5

ELSE 
     *=SEEK('M'+ &lcORDHDR..ACCOUNT,'Customer' ) 
  *TMI 9/23/2009
  *=SEEK(IIF(!EMPTY(&lcORDHDR..STORE),'S','M')+ &lcORDHDR..ACCOUNT+&lcORDHDR..STORE,'Customer' ) 
  =SEEK(IIF(!EMPTY(&lctmpordl..STORE),'S','M')+ &lcORDHDR..ACCOUNT+&lcORDHDR..STORE,'Customer' ) 
  *TMI 9/23/2009
  lcShpTName  = IIF(!EMPTY(lcShpTName  ),lcShpTName  ,CUSTOMER.STNAME)
  laShipTo[1] = IIF(!EMPTY(laShipTo[1] ),laShipTo[1] ,CUSTOMER.CADDRESS1)
  laShipTo[2] = IIF(!EMPTY(laShipTo[2] ),laShipTo[2] ,CUSTOMER.CADDRESS2)
  laShipTo[3] = IIF(!EMPTY(laShipTo[3] ),laShipTo[3] ,CUSTOMER.CADDRESS3)
  laShipTo[4] = IIF(!EMPTY(laShipTo[4] ),laShipTo[4] ,CUSTOMER.CADDRESS4)
  laShipTo[5] = IIF(!EMPTY(laShipTo[5] ),laShipTo[5] ,CUSTOMER.CADDRESS5)

ENDIF  

lcRet = ALLTRIM(lcShpTName) + ;
        IIF(!EMPTY(laShipTo[1]),", ",'') + ALLTRIM(laShipTo[1]) + ;
        IIF(!EMPTY(laShipTo[2]),", ",'') + ALLTRIM(laShipTo[2]) + ;
        IIF(!EMPTY(laShipTo[3]),", ",'') + ALLTRIM(laShipTo[3]) + ;
        IIF(!EMPTY(laShipTo[4]),", ",'') + ALLTRIM(laShipTo[4]) + ;
        IIF(!EMPTY(laShipTo[5]),", ",'') + ALLTRIM(laShipTo[5])

*B608961,1 TMI 8/17/2009
SELECT (lnSlct) 
*B608961,1 TMI 8/17/2009

RETURN lcRet


*-- end of lfchkShpAd.

*!*************************************************************
*! Name      : LFDelPhon
*! Developer : BASSEM RAFAAT ERNEST (BWA)
*! Date      : 12/16/2002
*! Purpose   : Function to delete the phone number.
*!*************************************************************
*! Called from : ALPKLSDL.FRX
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =LFDelPhon()
*!*************************************************************
FUNCTION LFDelPhon
PARAMETER lcReturn

FOR LnPh = 1 TO ALEN(laSoldTo,1)
  IF 'Phone#' $ laSoldTo[LnPh]
    laSoldTo[LnPh,1] = SPACE(0)
  ENDIF
ENDFOR

FOR LnPh = 1 TO ALEN(laShipTo,1)
  IF 'Phone#' $ laShipTo[LnPh]
    laShipTo[LnPh,1] = SPACE(0)
  ENDIF
ENDFOR

RETURN ""
*--End of LFDelPhon

FUNCTION FullName 
*B610191,1 HIA 01/13/2013 AL - error emailing pick ticket at DCC [T20121205.0002][Start]
*!*	IF !USED('SYUUSER')
*!*	 =gfOpenTable('SYUUSER','CUSER_ID','SH')
*!*	ENDIF
*!*	SELECT SYUUSER
*!*	gfseek(ORDHDR.cadd_user,'SYUUSER','CUSER_ID' )
*!*	ORD_NAME = SYUUSER.CUSR_NAME 
*!*	gfseek(piktkt.cadd_user,'SYUUSER','CUSER_ID' )
*!*	PIK_NAME = SYUUSER.CUSR_NAME  

lcSYUUSER = gfTempName()
IF !USED(lcSYUUSER)
 =gfOpenTable('SYUUSER','CUSER_ID','SH',lcSYUUSER)
ENDIF
SELECT (lcSYUUSER)
gfseek(ORDHDR.cadd_user,lcSYUUSER,'CUSER_ID' )
ORD_NAME = &lcSYUUSER..CUSR_NAME 
gfseek(piktkt.cadd_user,lcSYUUSER,'CUSER_ID' )
PIK_NAME = &lcSYUUSER..CUSR_NAME  

ENDFUNC 
*B610191,1 HIA 01/13/2013 AL - error emailing pick ticket at DCC [T20121205.0002][End]

FUNCTION lfLstLn
lcPikTkt = &lcTmpOrdL..PIktkt
lnLastLine = 0
lnLastLinX = 0
*lcStyle =  SUBSTR(&lcTmpOrdL..Style,1,lnLenthM1)
SELECT(lcTmpOrdL)
lcAlias = ALIAS() 
lnRecNo = RECNO()

SCAN FOR piktkt =lcPikTkt AND ctype ="B"
  lnLastLine = Lineno 
ENDSCAN 

LOCATE 
SCAN FOR piktkt =lcPikTkt AND ctype ="F"
  lnLastLinX = Lineno 
ENDSCAN 

IF BETWEEN(lnRecNo ,1,RECCOUNT())
  GO RECORD lnRecNo 
ENDIF 
SELECT(lcAlias) 
RETURN ''

*!*************************************************************
*! Name      : lfGtTotval
*! Developer : Mostafa Eid(mos)
*! Date      : 01/25/2009
*! Purpose   : Get Total value 
*!*************************************************************
*! Called from :
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGtTotval()
*!*************************************************************

FUNCTION lfGtTotval
PARAMETERS lnReturn
lnReturn =0
lnAlias = ALIAS()

*!*  STORE '' TO lcPikTkt
*!*  SCAN 
*!*    IF ALLTRIM(lcPikTkt) = ALLTRIM(&lcTmpFit..Piktkt)
*!*      LOOP
*!*    ENDIF

*!*    SELECT PIKTKT
*!*    =SEEK(&lcTmpFit..Piktkt)
*!*    IF PIKTKT.STATUS = 'O'
*!*      IF SEEK('O'+&lcTmpFit..ORDER,'ORDLINE')
*!*        SELECT ORDLINE
*!*        SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + &lcTmpFit..Order
*!*          IF &lcTmpFit..Piktkt <> OrdLine.Piktkt
*!*            LOOP
*!*          ENDIF
*!*          lnReturn = lnReturn+ TotPik * Price 
*!*        ENDSCAN
*!*      ENDIF
*!*    ELSE
*!*      IF SEEK(&lcTmpFit..PIKTKT,'PIKLINE')
*!*        SELECT PIKLINE
*!*        SCAN REST WHILE piktkt+order+STR(lineno,6) = &lcTmpFit..Piktkt
*!*          lnReturn = lnReturn+ TotPik * Price 
*!*        ENDSCAN
*!*      ENDIF
*!*    ENDIF
*!*    SELECT (lcTmpFit)
*!*    lcPikTkt = &lcTmpFit..Piktkt
*!*  ENDSCAN  
STORE '' TO lcPikTkt
lcPikTkt = &lcTmpOrdL..Piktkt

SELECT PIKTKT
*B608961,3 TMI 9/3/2009 Get the first line [START]
LOCATE
*B608961,3 TMI 9/3/2009 Get the first line [END  ]
=SEEK(lcPikTkt)
IF PIKTKT.STATUS = 'O'
  IF SEEK('O'+PIKTKT.ORDER,'ORDLINE')
    SELECT ORDLINE
    SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + PIKTKT.Order FOR PIkTKT = lcPikTkt
      lnReturn = lnReturn+ TotPik * Price
    ENDSCAN
  ENDIF
ELSE
  IF SEEK(lcPikTkt,'PIKLINE')
    SELECT PIKLINE
    SCAN REST WHILE piktkt+order+STR(lineno,6) = lcPikTkt
      lnReturn = lnReturn+ TotPik * Price
    ENDSCAN
  ENDIF
ENDIF


SELECT (lnAlias)
RETURN lnReturn

