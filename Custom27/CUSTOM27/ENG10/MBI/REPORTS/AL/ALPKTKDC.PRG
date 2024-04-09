*:***************************************************************************
*: Program file  : ALPKTKDC.PRG
*: Program desc. : CUSTOMIZED PICK TICKET Form FOR DCC
*: Date          : 03/07/2005
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mohamed Shokry (MHM)
*: Tracking Job Number: C123850
*: Based on      : 200429 , 200511 and 037816
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
*: Modification:
*:B128694,1 EIH  08/19/2005  Fix bug that not print header of NON STOCK PRODUCTS, Available PO Not Valid
*:B128694,1 EIH  08/19/2005  And wrong Qty's and scales.
*: C127511,1 NNA 09/15/2005 Use Ordhdr.cContRef instead of Ordhdr.Custpo or InvHdr.Custpo
*: C127511,1 NNA            that Because Customer need This Field to Be 30 Chr. (Fixed in the FRX)
*:B130696,1 EIH  03/01/2006 Fix bug that not printing lines from sales order that not piked. 
*:B131672,1 MHM  04/05/2006 Fix bug that not working correctlly in printing lines from sales order that not piked. 
*****************************************************************************

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

IF !USED('POSHDR')
  =gfOpenFile(gcDataDir+'POSHDR',gcDataDir+'POSHDR','SH')
ENDIF

IF !USED('POSLN')
  =gfOpenFile(gcDataDir+'POSLN',gcDataDir+'POSLNS','SH')
ELSE
  SELECT POSLN
  SET ORDER TO TAG POSLNS
ENDIF

*--Get User Phone.
SELECT SYUUSER
SEEK gcUser_ID
lcPhonUser = CUSR_PHON

SELECT (lcTmpOrdL)

*--Save the Original name to restore it in the end of the program.
lcOldName = lcTmpOrdL

*--Section create the temp. file.
=lfCreatTmp()

SELECT (lcTmpOrdL)

*B130696,1 EIH  03/01/2006 Fix bug that not printing lines from sales order that not piked [Begin]. 
LOCATE
STORE '' TO lcOrder
SCAN 
  IF lcOrder = &lcTmpOrdL..order
    LOOP
  ENDIF
  lcOrder = &lcTmpOrdL..order
  SELECT ORDLINE
  =SEEK('O'+lcOrder)
  
  *:B131672,1 MHM  04/05/2006 Fix bug that not working correctlly in printing [Start]
  *SCAN WHILE  ORDER =lcOrder 
  SCAN REST WHILE cordtype + ORDER = 'O' + lcOrder  FOR TotQty <> 0
  *:B131672,1  [End]
  
    IF !EMPTY(PIKTKT)
      LOOP
    ELSE
      lcPiktkt = &lcTmpOrdL..piktkt
    ENDIF
    SCATTER MEMVAR MEMO
    M.PIKTKT = lcPiktkt
    INSERT INTO (lcTmpOrdL) FROM MEMVAR
  ENDSCAN
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
IF llcheckFit
  m.cType = 'X'
  INSERT INTO (lcTmpOrdL) FROM MEMVAR
ENDIF  

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
STORE ' ' to lcKey
SCAN
  IF lcKey = &lcTmpOrdL..SCALE+&lcTmpOrdL..PIKTKT
    REPLACE LPrntHdr WITH .F.
  ELSE
    REPLACE LPrntHdr WITH .T.
  ENDIF
  lcKey = &lcTmpOrdL..SCALE+&lcTmpOrdL..PIKTKT
ENDSCAN
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
      REPLACE QTY&lccI WITH QTY&lccI+lnQTY&lccI 
    ENDFOR
    REPLACE TotQty WITH TotQty + lnTotQty 
    SKIP 
    DELETE
  ENDIF
  lcKey = &lcTmpOrdL..STYLE+&lcTmpOrdL..SCALE+&lcTmpOrdL..PIKTKT
ENDSCAN
LOCATE
*B128694,1 EIH  08/19/2005  [End]



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
  =gfCrtFrm(lcRpFrmId,lcOGFormArr,llOGRefForm)
  =lfRepPltFr(lcRpFrmId)
  SELECT(lcTmpFit)
  SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
  LOCATE
  IF !EOF()
    =gfDispRe(lcRpFrmId)
  ENDIF
ENDIF

*--Restore the Original name.
lcTmpOrdL = lcOldName

SELECT (lcAdStyGrp)
SET RELATION TO

SELECT(lcAlasDl)
SET ORDER TO TAG &lcOrdDl
=SEEK(lcKeyDL)

**--Function to clear the Temp. file.
=lfBasToClr(lcAdStyGrp , 'F')

                       *-- End of the Program --*
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
IF !USED('PKBINLOC')
  =gfOpenFile(gcDataDir+'PKBINLOC','PKBINPKT','SH') 
ENDIF

=lfCrtTmp()
lcRePsTY   = ''
lcCurPktkt = ''
lcRepBin   = ''
SELECT (lcTmpOrdL)
SCAN
    SCATT MEMVAR MEMO
    lcBnPikTkt = PIKTKT

    IF !(OCCURS(',',m.cConslPikt)>0)
      IF SEEK(Account + Store +  PIKTKT + STYLE,lcReptSty)
        LOOP
      ELSE
        INSERT INTO (lcReptSty) FROM MEMVAR
      ENDIF
    ENDIF  

    
    *IF lcCurPktkt <> lcBnPikTkt
    *  lcCurPktkt = lcBnPikTkt
    *  lcRePsTY = ''
    *  lcRepBin   = ''
    *ENDIF
    
    SELECT PKBINLOC  
    IF SEEK(lcBnPikTkt)
      SCAN REST WHILE PikTkt+cWareCode+cLocation+Style = ALLTRIM(lcBnPikTkt) 
        IF !(PKBINLOC.STYLE = &lcTmpOrdL..Style)
          LOOP
        ENDIF
        *IF lcRePsTY = STYLE AND lcRepBin = cLocation
        *  LOOP
        *ELSE
        *  lcRePsTY = STYLE 
        *   lcRepBin = cLocation
        *ENDIF
        

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
          
          *B131672,1 MHM  04/05/2006 [Start]
          m.CGRUPDETAL = 'D'
          *B131672,1 MHM  04/05/2006 [End]
          
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

lcTmpOrdL = lcBnStyGrp 
SELECT (lcTmpOrdL)

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

*B128694,1 EIH  08/19/2005  Add field to Fix bug that not print header of NON STOCK PRODUCTS [Begin]

DIMENSION laTmpStru[ALEN(laTmpStru , 1) + 1 , 4]
laTmpStru[ALEN(laTmpStru , 1) , 1] = 'LPrntHdr'
laTmpStru[ALEN(laTmpStru , 1) , 2] = 'L'
laTmpStru[ALEN(laTmpStru , 1) , 3] = 1
laTmpStru[ALEN(laTmpStru , 1) , 4] = 0

*B128694,1 EIH  08/19/2005  [End]


CREATE TABLE (gcWorkDir + lcBnStyGrp) FROM ARRAY laTmpStru

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

CREATE TABLE (gcWorkDir + lcReptSty) FROM ARRAY laTmpStru
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
*! Name      : lfCreatTmp
*! Developer : Mohamed Shokry(BWA)
*! Date      : 20/03/2005
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

=lfAddField("laTmpStru", "StyGrop"   , "C" ,6  ,0)    && Field hold the style group data.
=lfAddField("laTmpStru", "StyLoc"    , "C" ,6  ,0)    && Field hold the style group data.
=lfAddField("laTmpStru", "Account"   , "C" ,5  ,0)
=lfAddField("laTmpStru", "CGroupKey" , "C" ,6  ,0)
=lfAddField("laTmpStru", "cDelivery" , "C" ,6  ,0)
=lfAddField("laTmpStru", "cConslPikt", "M" ,10 ,0)
=lfAddField("laTmpStru", "cStyMajor" , "C" ,19 ,0)
=lfAddField("laTmpStru", "cLocation" , "C" ,10 ,0)
=lfAddField("laTmpStru", "cWareCode" , "C" ,6  ,0)
=lfAddField("laTmpStru", "cType"     , "C" ,1  ,0)
=lfAddField("laTmpStru", "Ponofolo"  , "C" ,6 ,0)      && this field to use it in print the Po Number in the ToFollow section.
=lfAddField("laTmpStru", "DatAvlbl"  , "D" ,8 ,0)      && this field to use it in print the available date in the ToFollow section.
=lfAddField("laTmpStru", "Status"    , "C" ,1 ,0)      && this field to use it in Stats of PIKTKT
=lfAddField("laTmpStru", "cdim1"     , "C" ,5 ,0)      && this field to use it in Scale Dimansion

FOR lnCrtTmp = 1 TO 16
  lcNumSiz = ALLTRIM(STR(lnCrtTmp))
  =lfAddField("laTmpStru", "Sz"  + lcNumSiz + "DL", "C" , 5 ,0)
ENDFOR

FOR lnCrtTmp = 1 TO 8
  lcNumQty = ALLTRIM(STR(lnCrtTmp+8))
  =lfAddField("laTmpStru", "Qty"  + lcNumQty , "N" , 6 ,0)
ENDFOR

CREATE TABLE (gcWorkDir + lcAdStyGrp) FROM ARRAY laTmpStru

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


CREATE TABLE (gcWorkDir + lcTmpFit) FROM ARRAY laTmpStru
INDEX ON PIKTKT + LEFT(Style,lnScaPosGl + 1) + cdim1  TAG lcTmpFit

CREATE TABLE (gcWorkDir+lcTmpGroup) ( CGroupKey C(6) , Piktkt C(6))
INDEX ON CGroupKey+PikTkt  Tag (lcTmpGroup)

*--File create the scale and sizes.
CREATE TABLE (gcWorkDir + lcTmpSizes) ( ScalFld C(2) , IndxDm2 N(3) , cDim1 C(5) , llPrnSDc L ,;
                                        Sz1 C(5) , Sz2 C(5) , Sz3 C(5) , Sz4 C(5) , Sz5 C(5) , Sz6 C(5) , Sz7 C(5) , Sz8 C(5) ,;
                                        Sz9 C(5) , Sz10 C(5) , Sz11 C(5) , Sz12 C(5) , Sz13 C(5) , Sz14 C(5) , Sz15 C(5) , Sz16 C(5))

INDEX ON ScalFld + ALLTRIM(STR(IndxDm2)) Tag SortScal
INDEX ON ScalFld + cDim1 Tag (lcTmpSizes) ADDITIVE


*--End of lfCreatTmp.
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
*B128694,1 EIH  08/19/2005  Fix bug that not valid Po [Begin]
SET RELATION TO
SET RELATION TO Posln.cstytype+ Posln.po INTO Poshdr ADDITIVE
*B128694,1 EIH  08/19/2005 .[End]
=SEEK(&lcTmpFit..STYLE)
=SEEK(CSTYTYPE+PO,'POSHDR')
SCAN REST WHILE STYLE+CSTYTYPE+PO+STR(LINENO,6)+TRANCD = &lcTmpFit..STYLE FOR POSHDR.STATUS $ "HO"
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
    =SEEK( LEFT(&lcTmpOrdL..SCALE,2) , lcTmpSizes )
    lnCI2 = 1
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
    =SEEK( LEFT(&lcTmpOrdL..SCALE,2) , lcTmpSizes )
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
    lnCI2 = 1
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
SCAN 
  IF ALLTRIM(lcPikTkt) = ALLTRIM(&lcTmpFit..Piktkt)
    LOOP
  ENDIF

  SELECT PIKTKT
  =SEEK(&lcTmpFit..Piktkt)
  IF PIKTKT.STATUS = 'O'
    IF SEEK('O'+&lcTmpFit..ORDER,'ORDLINE')
      SELECT ORDLINE
      SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O' + &lcTmpFit..Order
        IF &lcTmpFit..Piktkt <> OrdLine.Piktkt
          LOOP
        ENDIF
        lnReturn = lnReturn+ TotPik 
      ENDSCAN
    ENDIF
  ELSE
    IF SEEK(&lcTmpFit..PIKTKT,'PIKLINE')
      SELECT PIKLINE
      SCAN REST WHILE piktkt+order+STR(lineno,6) = &lcTmpFit..Piktkt
        lnReturn = lnReturn+ TotPik 
      ENDSCAN
    ENDIF
  ENDIF
  SELECT (lcTmpFit)
  lcPikTkt = &lcTmpFit..Piktkt
ENDSCAN  
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
SET ORDER TO Pkbinpkt

=SEEK(&lcTmpOrdL..Piktkt)
SCAN REST WHILE piktkt+cwarecode+clocation+style = &lcTmpOrdL..Piktkt
  lnReturn = lnReturn+ TOTQTY
ENDSCAN

SELECT PKBINLOC
SET ORDER TO &lcoldord

SELECT (lnAlias)
RETURN lnReturn
