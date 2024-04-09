*:***************************************************************************
*: Program file  : SOORCNGW     
*: Program desc. : Customer Order confirmation Report(HK) for GMA 
*: For Report    : SOORCNGW.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Heba Mohamed Amin (HMA)
*: Date          : 06/27/2004
*: Reference     : C038212,1
*:***************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe ,
*:               : gfRltFld, gfCodDes.
*:               : lfGetLogo,lfAdrShift,lfSolSpAdr,lfHeadVar,lfGetNotes,
*:               : lfNoteHead,lfNoteData,lfEndGroup,
*:               : lfwOldVal,lfsChOrder
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO Soorcngw
*:***************************************************************************
*:Modifications:
*:E124450,1 HMA 09/12/2004 Open the currency file to retrieve 
*:                        the Net Amount with the Order currency .
*:E999999,1 HMA 01/26/2005 Use lfSrAcc()to validate account browser after 
*:                        change it from LIKE to INLIST Browser  .
*:E124540,1 HMA 02/20/2005 show the Currency Code in Frx,Fix in Prg. and FRX.
*:B126801,1 HMA 03/17/2005 Show message of priniting no of records before preview the report.
*:E126800,1 HMA 04/03/2005 Don't Show the order status [Complete]  
*:B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line 
*           in the Option grid if the order status or order type has changed.
*:***************************************************************************
#INCLUDE R:\Aria4xp\reports\so\soorcngw.H

lcTime     = TIME()                     && Variable to hold the Time
*B126801,1 HMA 03/17/2005 Show message of priniting no of records before preview the report [BEGIN].
lcStTime     = TIME()                     && Variable to hold starting time of collecting data
*B126801,1 HMA 03/17/2005 Show message of priniting no of records before preview the report [END].
lnLastRec  = 0                          && Record No. Of last record in order group.
lcStore    = ''
lcDivLName = ''
*-- Note Variables [begin]
lcOrdsNum=''
lcTitle    = ''                         && Title of Note.
lcNotes    = ''                         && Notes.
llPrintBox = .F.
llEndGroup = .F.                        && Flag to know if we are at the end of the Group
llPrntBoth = .T.                        && Flag to know we print both line notes and notepad.
llAprvFlag = .F.                        && Flag to print approval
llTitle    = .T.                        && Flag to print Detail header.


*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF llFrTime
  lcStyTitle = gfItemMask('HI')        && Title of the style.
  lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.
  lcObj_Id  = ''                       && Object Identification in Objlink file.
 
  *-- if this company have a logo, put it in temp. file and then use it in .FRX
  IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
     SEEK(OBJLINK.cObject_ID,'OBJECTS')
    = lfGetLogo()  && Function to Fill the temp. With company Logo.
  ENDIF
ENDIF

*--create temp file for ordline file
lcTempOrd = loOgScroll.gfTempName()
*--- Relation between opened files [begin]
*-- Note that the files was opened in Rep. Gen.
SELECT ORDHDR
SET RELATION TO cordtype+ order INTO Ordline ADDITIVE
*--create temp file for ordline file + some other fields
DIMENSION laTempStru[1,18]
SELECT ORDLINE
lnFldsLen = AFIELDS(laTempStru)
*E124540,1 HMA 02/20/2005 Increase The Dimension To Add A new field that will Hold The currency Code [Begin]
*DIMENSION laTempStru[lnFldsLen + 17,18]
DIMENSION laTempStru[lnFldsLen + 18,18]
*E124540,1 HMA [End]
laTempStru[lnFldsLen + 1,1] = "UpcSize1"
laTempStru[lnFldsLen + 1,2] = "C"
laTempStru[lnFldsLen + 1,3] = 7
laTempStru[lnFldsLen + 1,4] = 0

laTempStru[lnFldsLen + 2,1] = "UpcSize2"
laTempStru[lnFldsLen + 2,2] = "C"
laTempStru[lnFldsLen + 2,3] = 7
laTempStru[lnFldsLen + 2,4] = 0

laTempStru[lnFldsLen + 3,1] = "UpcSize3"
laTempStru[lnFldsLen + 3,2] = "C"
laTempStru[lnFldsLen + 3,3] = 7
laTempStru[lnFldsLen + 3,4] = 0

laTempStru[lnFldsLen + 4,1] = "UpcSize4"
laTempStru[lnFldsLen + 4,2] = "C"
laTempStru[lnFldsLen + 4,3] = 7
laTempStru[lnFldsLen + 4,4] = 0

laTempStru[lnFldsLen + 5,1] = "UpcSize5"
laTempStru[lnFldsLen + 5,2] = "C"
laTempStru[lnFldsLen + 5,3] = 7
laTempStru[lnFldsLen + 5,4] = 0

laTempStru[lnFldsLen + 6,1] = "UpcSize6"
laTempStru[lnFldsLen + 6,2] = "C"
laTempStru[lnFldsLen + 6,3] = 7
laTempStru[lnFldsLen + 6,4] = 0

laTempStru[lnFldsLen + 7,1] = "UpcSize7"
laTempStru[lnFldsLen + 7,2] = "C"
laTempStru[lnFldsLen + 7,3] = 7
laTempStru[lnFldsLen + 7,4] = 0

laTempStru[lnFldsLen + 8,1] = "UpcSize8"
laTempStru[lnFldsLen + 8,2] = "C"
laTempStru[lnFldsLen + 8,3] = 7
laTempStru[lnFldsLen + 8,4] = 0

laTempStru[lnFldsLen + 9,1] = "SkuSize1"
laTempStru[lnFldsLen + 9,2] = "C"
laTempStru[lnFldsLen + 9,3] = 7
laTempStru[lnFldsLen + 9,4] = 0

laTempStru[lnFldsLen + 10,1] = "SkuSize2"
laTempStru[lnFldsLen + 10,2] = "C"
laTempStru[lnFldsLen + 10,3] = 7
laTempStru[lnFldsLen + 10,4] = 0

laTempStru[lnFldsLen + 11,1] = "SkuSize3"
laTempStru[lnFldsLen + 11,2] = "C"
laTempStru[lnFldsLen + 11,3] = 7
laTempStru[lnFldsLen + 11,4] = 0

laTempStru[lnFldsLen + 12,1] = "SkuSize4"
laTempStru[lnFldsLen + 12,2] = "C"
laTempStru[lnFldsLen + 12,3] = 7
laTempStru[lnFldsLen + 12,4] = 0

laTempStru[lnFldsLen + 13,1] = "SkuSize5"
laTempStru[lnFldsLen + 13,2] = "C"
laTempStru[lnFldsLen + 13,3] = 7
laTempStru[lnFldsLen + 13,4] = 0

laTempStru[lnFldsLen + 14,1] = "SkuSize6"
laTempStru[lnFldsLen + 14,2] = "C"
laTempStru[lnFldsLen + 14,3] = 7
laTempStru[lnFldsLen + 14,4] = 0

laTempStru[lnFldsLen + 15,1] = "SkuSize7"
laTempStru[lnFldsLen + 15,2] = "C"
laTempStru[lnFldsLen + 15,3] = 7
laTempStru[lnFldsLen + 15,4] = 0

laTempStru[lnFldsLen + 16,1] = "SkuSize8"
laTempStru[lnFldsLen + 16,2] = "C"
laTempStru[lnFldsLen + 16,3] = 7
laTempStru[lnFldsLen + 16,4] = 0

laTempStru[lnFldsLen + 17,1] = "Sku"
laTempStru[lnFldsLen + 17,2] = "C"
laTempStru[lnFldsLen + 17,3] = 16
laTempStru[lnFldsLen + 17,4] = 0

*E124540,1 HMA 02/20/2005 A new Field in the Temp file To have the OrdHdr.cCurrCode[Begin]
laTempStru[lnFldsLen + 18,1] = "CurrCode"
laTempStru[lnFldsLen + 18,2] = "C"
laTempStru[lnFldsLen + 18,3] = 3
laTempStru[lnFldsLen + 18,4] = 0
*E124540,1 HMA 02/20/2005 [End]

*E124540,1 HMA 02/20/2005 [Begin]
FOR  lnInc=7 TO 16 
*  FOR lnTempStru=1 TO 17
  FOR lnTempStru=1 TO 18
    STORE SPACE(1) TO laTempStru[lnFldsLen+lnTempStru,lnInc]
  ENDFOR 
ENDFOR  
*FOR lnTempStru=1 TO 17
FOR lnTempStru=1 TO 18
  STORE 0  TO laTempStru[lnFldsLen+lnTempStru,17], laTempStru[lnFldsLen+lnTempStru,18]
ENDFOR 
*E124540,1 HMA 02/20/2005 [End]
gfCrtTmp(lcTempOrd,@laTempStru,"CORDTYPE + ORDER + STORE + STR(LINENO,6)",lcTempOrd,.F.)

*--E124450,1 HMA 09/12/2004 Open the currency file to retrieve the Net Amount with the Order currency [Begin]
IF !USED('SYCCURR')
  =gfOpenFile(oAriaApplication.SysPath+'SYCCURR',oAriaApplication.SysPath+'Ccurrcode','SH')
ENDIF
*--E124450,1 HMA 09/12/2004 Open the currency file to retrieve the Net Amount with the Order currency [End]



SELECT ORDHDR

lnLenSku = SKUTMPL.Len1

SELECT SKUTMPL

=SEEK('S')
lnLenSku = SKUTMPL.Len1
=lfCollectData()
*!*	SELECT ORDHDR
*!*	IF !EMPTY(lcRpExp)
*!*	  lcRpExp = lcRpExp + [ AND ]
*!*	ENDIF
*!*	IF lcRpOrdSta ="A"
*!*	  lcRpExp = lcRpExp + [!(ORDHDR.Status $ "BX")]
*!*	ELSE
*!*	  lcRpExp = lcRpExp + [(ORDHDR.Status $ lcRpOrdSta)]
*!*	ENDIF

*!*	*--order type

*!*	IF lcRpOrdTyp !="A"
*!*	  IF !EMPTY(lcRpExp)
*!*	    lcRpExp = lcRpExp + [ AND ]
*!*	  ENDIF
*!*	  lcRpExp = lcRpExp + [(ORDHDR.CordType $ lcRpOrdTyp)]
*!*	ENDIF

*!*	IF !EMPTY(lcRpAcc)
*!*	  IF !EMPTY(lcRpExp)
*!*	    lcRpExp = lcRpExp + [ AND ]
*!*	  ENDIF
*!*	  lcRpExp = lcRpExp + [(ORDHDR.ACCOUNT = lcRpAcc)]
*!*	ENDIF

*!*	*-- Collect data
*!*	SCAN FOR &lcRpExp
*!*	  SELE ORDLINE
*!*	  SCAN REST WHILE CORDTYPE+ORDER = ORDHDR.CORDTYPE+ORDHDR.ORDER
*!*	    SCATT MEMVAR MEMO
*!*		*----------
*!*	    STORE '' TO m.SKUSize1 , m.SKUSize2 , m.SKUSize3 , m.SKUSize4 , m.SKUSize5 ,;
*!*		         m.SKUSize6 , m.SKUSize7 , m.SKUSize8 ,m.SKU

*!*	    STORE '' TO m.UpcSize1 , m.UpcSize2 , m.UpcSize3 , m.UpcSize4 , m.UpcSize5 ,;
*!*		         m.UpcSize6 , m.UpcSize7 , m.UpcSize8

*!*	    =SEEK('S' + m.Scale,"SCALE")
*!*	    FOR lnCount = 1 TO Scale.Cnt
*!*		    lcCount = STR(lnCount , 1)
*!*	        IF SEEK(m.Style + lcCount , "StyleUPC")
*!*	          m.UPCSize&lcCount= StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
*!*	        ENDIF
*!*	    ENDFOR

*!*	    IF SEEK('S' + OrdLine.Account + m.Style,'SPCK_LIN')
*!*	      SELECT SPCK_LIN
*!*		  m.SKU=LEFT(Pack_Id,lnLenSku)
*!*	      lnCount = 1
*!*		  SCAN FOR Type + Account + Style + Pack_ID= 'S' + OrdLine.Account + m.Style
*!*			lcCount = STR(lnCount , 1)
*!*		    m.SkuSize&lcCount = AllTrIM(SUBSTR(PACK_ID,lnlensku,Len(Pack_ID)))
*!*	    	lnCount = 1 + lnCount
*!*		  ENDSCAN
*!*	    ENDIF
*!*		*----------
*!*	    INSERT INTO (lcTempOrd) FROM MEMVAR
*!*	  ENDSCAN
*!*	ENDSCAN

*GO TOP
SELECT ORDHDR
SET RELATION OFF INTO ORDLINE

SELECT ORDHDR

SET RELATION TO cordtype+ order INTO (lcTempOrd) ADDITIVE

SET RELATION TO cwarecode INTO Warehous ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SELECT (lcTempOrd)

SET FILTER TO TotQty != 0
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO style INTO Style ADDITIVE

SELECT OBJLINK_A
SET RELATION TO cobject_id INTO OBJECTS_A ADDITIVE
*--- Relation between opened files [end]

IF llFrTime
  llFrTime = .F.  && After this time all of your variablrs have been defined, you not need to goto any llFrTime block again.
  DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
  laCompAdd = ''                    && Array to hold the Company address
  laSoldTo = ''                     && Array to hold the Sold To address
  laShipTo = ''                     && Array to hold the Ship To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  *-- Get company Address 
  lcWorkArea = SELECT()
  LOCAL loRDA , lnResult
  PRIVATE  lcSqlCommand , lnResult
  lcSqlCommand=[SELECT cCom_Name,cCom_Phon,cCom_fax,cCont_code,cAddress1,cAddress2,cAddress3,cAddress4,cAddress5,cAddress6 FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
  lnResult  = oAriaApplication.remotesystemdata.execute(lcSqlCommand,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
  IF lnResult >= 1 
    lcCompName = cCom_Name
    lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
    lcCompFax  = cCom_fax              && Variable to hold the Company Fax
    lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  
    *-- Load Company address.
    laCompAdd[1] = gfGetAdr('SYCCOMP', '' , '' , '' , 1)
    laCompAdd[2] = gfGetAdr('SYCCOMP', '' , '' , '' , 2)
    laCompAdd[3] = gfGetAdr('SYCCOMP', '' , '' , '' , 3)
    laCompAdd[4] = gfGetAdr('SYCCOMP', '' , '' , '' , 4)
    laCompAdd[5] = gfGetAdr('SYCCOMP', '' , '' , '' , 5)
    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF 
  SELECT (lcWorkArea) 

ENDIF

lcSkipExpr  = [&lcTempOrd] && Expression we skip to later.

*-- lcNoteLns : Name of Temp. Loop File which is used to print both line notes
*--           : and notepad from notepad file.
*--           : note that this name and temp. file is created
*--           : one for every optional grid session run.
lcNoteLns = IIF(EMPTY(lcNoteLns),gfTempName(),lcNoteLns)

*-- if you don't find temp. file, create it if you have both types of notes.
IF !USED(lcNoteLns)
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  INDEX ON cRecord TAG (lcNoteLns) OF (oAriaApplication.WorkDir+lcNoteLns)
  FOR lnI = 1 TO 2
    APPEND BLANK
    REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
ENDIF

SELECT (lcTempOrd)
SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
lcSkipExpr = [&lcTempOrd,&lcNoteLns]


*-- if we are in case of print both types of notes.  [begin]

*-- Select Master report file.
SELECT ORDHDR
SET SKIP TO &lcSkipExpr
*B126801,1 HMA 03/17/2005 Show message of priniting no of records before preview the report [BEGIN].
lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)
WAIT WINDOW LANG_Soorcn_SelMessage +' '+ ALLTRIM(STR(RECCOUNT(lcTempOrd))) + LANG_Soorcn_Records + ALLTRIM(STR(lnInterval,6,2)) + LANG_Soorcn_Seconds  NOWAIT
*B126801,1 HMA 03/17/2005 Show message of priniting no of records before preview the report [END].
loogScroll.cCROrientation = 'P'
DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp


SELECT ORDHDR

SET RELATION TO

IF USED(lcTempOrd)
  SELECT(lcTempOrd)
  USE
  ERASE (oAriaApplication.WorkDir+lcTempOrd+'.DBF')
  ERASE (oAriaApplication.WorkDir+lcTempOrd+'.CDX')
ENDIF
*-- End of Report code.

*!*************************************************************
*! Name      : lfsChOrder
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Rise change order flag, in range browse screen.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsChOrder()
*!*************************************************************
*! Note      : S symbol is [S,Set]
*!*************************************************************
FUNCTION lfsChOrder
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'
    SELECT ORDHDR
    DO CASE
      *--FIX ALL AND CHANGE lcRpOrdTyp
      CASE lcRpOrdTyp = "A"
        SET FILTER TO IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
           STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
           ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0)

        LOCATE

      CASE lcRpOrdTyp = "O"
        SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta)

        LOCATE FOR CORDTYPE+ORDER = "O"

      CASE lcRpOrdTyp = "C"
        SET FILTER TO (CORDTYPE + ORDER = "C") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta)

        LOCATE FOR CORDTYPE+ORDER = "C"

      CASE lcRpOrdTyp = "T"
        SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
           STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
           ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0)
        LOCATE FOR CORDTYPE+ORDER = "T"

    ENDCASE
  
  CASE lcParm = 'R'
   
    SELECT ORDHDR
    SET FILTER TO
    llClearSel = .F.

ENDCASE
*-- End of lfsChOrder.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : When function to get the Old value
*!*************************************************************
*! Called from : Some of the Option Grid fields
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(OGSYS18())      && Varible to hold the old value
lcOldVal = EVALUATE(OGSYS18(.T.))

*-- End of lfwOldVal.
*!*************************************************************
*! Name      : lfvType
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Transaction Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvType()
*!*************************************************************
FUNCTION lfvType

IF lcRpOrdTyp $ "TC"
  lcRpEDIFlt = "B"
  ClearRead()
ELSE
  IF (laOldVal = 'T') .OR. (laOldVal = 'C')
    lcRpEDIFlt = "B"
    ClearRead()
  ENDIF
ENDIF
llClearSel = .T.

IF loogScroll.lcRpOrdTyp <> laOldVal
  =lfEmpty()
ENDIF 


*B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line 
*in the Option grid if the order status or order type has changed. [Begin]

lnOrdPos = ASCAN(loOGScroll.laogFxflt,'ORDHDR.ORDER')
IF lnOrdPos <> 0 
 lnOrdPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnOrdPos,1)
 lcOrders = loOGScroll.laogFxflt[lnOrdPos,6]
 IF !EMPTY(lcOrders)
   SELECT(lcOrders)
   ZAP 
 ENDIF 
ENDIF  

*B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line 
*in the Option grid if the order status or order type has changed. [End]
*-- End of lfvType.

*!*************************************************************
*! Name      : lfvOrdSta
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Order Status Validation
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvOrdSta()
*!*************************************************************
FUNCTION lfvOrdSta

llClearSel = .T.
*B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line 
*in the Option grid if the order status or order type has changed. [Begin]

lnOrdPos = ASCAN(loOGScroll.laogFxflt,'ORDHDR.ORDER')
IF lnOrdPos <> 0 
 lnOrdPos = ASUBSCRIPT(loOGScroll.laogFxflt,lnOrdPos,1)
 lcOrders = loOGScroll.laogFxflt[lnOrdPos,6]
 IF !EMPTY(lcOrders)
   SELECT(lcOrders)
   ZAP 
 ENDIF 
ENDIF  

*B127895,1 HMA 05/12/2005 delete the selected records from transaction # Line 
*in the Option grid if the order status or order type has changed. [End]
*-- End of lfvOrdSta.
*!*************************************************************
*! Name      : lfsrAcc
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Change account flag, in range browse screen.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example   : =lfsrAcc()
*!*************************************************************
*! Note      : S symbol is [S,Set] , R symbol is Reset.
*!*************************************************************
*! Modification :E999999,1 HMA 01/26/2005 Use lfSrAcc()to validate 
*!      account browser after change it from LIKE to INLIST Browser.
*!*************************************************************
*!*	FUNCTION lfsrAcc
*!*	PARAMETERS lcParm

*!*	SELECT Customer
*!*	SET ORDER TO Customer
*!*	LOCATE
*E999999,1 HMA 01/26/2005 Use lfSrAcc()to validate account browser after change it from LIKE to INLIST Browser [BEGIN] 
FUNCTION lfsrAcc
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'
    GO TOP IN CUSTOMER
  CASE lcParm = 'R'
    llClearAcc = .F.
ENDCASE
*E999999,1 HMA 01/26/2005 Use lfSrAcc()to validate account browser after change it from LIKE to INLIST Browser [END] 

*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Function to Save the company logo in temp. file
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : SORDCON.PRG
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetLogo()
*!*************************************************************
FUNCTION lfGetLogo
llLogo = .T.
lcLogoPic = gfTempName()
lcObj_Id = OBJLINK.cObject_ID
*-- Select general field which have company logo.
SELECT gobject;
 FROM Objects         ;
 WHERE Objects.cobject_id = lcObj_Id ;
 INTO CURSOR (lcLogoPic)

*-- End of lfGetLogo.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : SORDCON.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5

  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])

    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5

  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*-- End of lfAdrShift.
*!*************************************************************
*! Name      : lfAprvFlag
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Function that control appearance of
*!           : Approval objects in SORDCONA.FRX
*!*************************************************************
*! Called from : [Option Grid] "Approval Object" in Header Band.
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Logical
*!*************************************************************
*! Example     : = lfAprvFlag()
*!*************************************************************
FUNCTION lfAprvFlag
llAprvFlag = !EMPTY(OrdHdr.Approval)
RETURN llAprvFlag

*-- End of lfAprvFlag.

*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : Heba Mohamed Amin (HMA)
*! Date        : 06/27/2004
*! Purpose     : Function to fill the approparate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : SORDCONA.FRX [Variable lcDum in the report]
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : lfBoxPrn,lfNoteHead,lfNoteData
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes

lcNotes    = lfNoteData()     && Note Data.

IF !EMPTY(lcNotes)
  lcTitle    = lfNoteHead()     && Title of the note (Line Note OR NotePad).
ELSE
  lcTitle    =""
ENDIF

llPrintBox = !EMPTY(lcTitle)  && If it's .T. Report Print box around notes.
llTitle    = RECNO(lcTempOrd) != lnLastRec
RETURN ''
*-- End of lfGetNotes.
 
*!*************************************************************
*! Name        : lfNoteHead
*! Developer   : Heba Mohamed Amin (HMA)
*! Date        : 06/27/2004
*! Purpose     : Function to fill the approparate Note Title.
*!             : ("Line Notes" OR "Order NotePad" OR "Contract NotePad") .
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteHead()
*!*************************************************************
FUNCTION lfNoteHead
lcNoteHead = ''
*-- If you have order lines.
IF ORDHDR.LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE
      CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))

        lcNoteHead = LANG_Soorcn_Line  + ALLTRIM(STR(&lcTempOrd..LineNo)) +' '+ LANG_Soorcn_NotePad

      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType='O',LANG_Soorcn_OrdNotPad,IIF(OrdHdr.cOrdType='T',LANG_Soorcn_EdiTmpNotPad,LANG_Soorcn_ContNotPad)),'')

    ENDCASE
  ELSE && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))

        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'


      CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType='O',LANG_Soorcn_OrdNotPad,IIF(OrdHdr.cOrdType='T',LANG_Soorcn_EdiTmpNotPad,LANG_Soorcn_ContNotPad)),'')

    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteHead

*-- End of lfNoteHead

*!*************************************************************
*! Name        : lfNoteData
*! Developer   : Heba Mohamed Amin (HMA)
*! Date        : 06/27/2004
*! Purpose     : Function to fill the approparate Note Data Field in report.
*!*************************************************************
*! Called from : lfGetNotes Function.
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : None
*!*************************************************************
*! Example           : = lfNoteData()
*!*************************************************************
FUNCTION lfNoteData
lcNoteData  = ''
lcPrntNote = ''
*-- If you have order lines.
IF ORDHDR.LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE
      CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))

        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote


      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))
        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
    ENDCASE
  ELSE  && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE

      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))

        lcNoteData  =  ALLTRIM(&lcTempOrd..Note_Mem)
        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote

      CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteData
*-- End of lfNoteData.

*!*************************************************************
*! Name        : lfHeadVar
*! Developer   : Heba Mohamed Amin (HMA)
*! Date        : 06/27/2004
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : SORDCONA.FRX [Header Band]
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : lfSolSpAdr
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar
lcAlias = ALIAS()   && Save Current alias.
llEndGroup = .F.    && Start of new Group.
= lfSolSpAdr()      && Call Function that fill header data [SoldTo and ShipTo]
SELECT (lcAlias)    && Restore before function alias.
RETURN ''
*-- End of lfHeadVar.

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Function to Get the Sold to Address, Ship to Address,
*!           : the Description of the Ship Via, Season,
*!           : Special Instructions, and Terms.
*!*************************************************************
*! Called from : lfHeadVar Function
*!*************************************************************
*! Calls       :
*!              Procedures : ....
*!              Functions  : gfRltFld, gfCodDes, gfGetAdr, lfAdrShift.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ....
*!*************************************************************
FUNCTION lfSolSpAdr

lnSavAlias = SELECT(0)

lcStore = &lcTempOrd..Store

= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')

SELECT CUSTOMER
IF ORDHDR.MULTI = 'Y'
  = SEEK('S' + &lcTempOrd..Account + &lcTempOrd..Store , "CUSTOMER")

  IF ALLTRIM(ORDHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ENDIF
ENDIF

lcSolTName = BTName
lcShpTName = IIF(ORDHDR.Alt_ShpTo , ORDHDR.STName , IIF(EMPTY(DBA) , STName , DBA))

laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')

= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.

*-- IF alternate ship to address
IF ORDHDR.Alt_ShpTo
  SELECT ORDHDR
  lcShpTName = STName
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE    && Else

  IF !EMPTY(CUSTOMER.DIST_CTR)
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcStore = lcStore +LANG_Soorcn_DistCenter+ Customer.Store
  ENDIF

  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  IF TYPE('lcCurrKey') = 'C'
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF

ENDIF    && End of IF
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
SELECT (lnSavAlias)

*-- End of lfSolSpAdr.


*!*************************************************************
*! Name      : lfLastRec
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Calculate last Record in order details.
*!*************************************************************
*! Called from : [SORDCONA.FRX, ORDER GROUP HEADER BAND]
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : NULL
*!*************************************************************
*! Example     : = lfLastRec()
*!*************************************************************
FUNCTION lfLastRec
PRIVATE lcThAlias,lnThRec,lcThStore
lcThAlias = ALIAS()           && Save Current Alias.
SELECT (lcTempOrd)

lnThRec = RECNO(lcTempOrd)    && Save Current record #.
lcThStore = Store
LOCATE REST FOR ( cordtype+order+store+style+STR(lineno,6) > OrdHdr.cordtype + OrdHdr.order + lcThStore)
IF (order != OrdHdr.order) OR (store != lcThStore)
  SKIP -1
ENDIF

lnLastRec = RECNO(lcTempOrd)

IF BETWEEN(lnThRec,1,RECCOUNT(lcTempOrd))
  GO lnThRec IN (lcTempOrd)    && Restore Record #
ELSE
  GO TOP IN (lcTempOrd)    && Restore Record #
ENDIF

IF lnLastRec <= 0
  lcOrdsNum = ''
ENDIF


SELECT (lcThAlias)            && Restore Alias.
RETURN ''

*-- End of lfLastRec.
*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/27/2004
*! Purpose   : Function to Update the End of Group flag
*!*************************************************************
*! Called from : SORDCONA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Null
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
llEndGroup = .T.   && We are in the end of the group (i.e : Order end.)
llTitle    = .T.
RETURN '    '
*-- End of lfEndGroup.

*!**************************************************************************
*! Name      : lfvAccount
*! Developer   : Heba Mohamed Amin (HMA)
*! Date        : 06/27/2004
*! Purpose   : Validation function for the Customer Account field
*!**************************************************************************
*! Called from : Customer Account field [Option Grid]
*!**************************************************************************
*! Calls       : CusBrowM()
*!**************************************************************************
*! Example     : =lfvAccount()
*!**************************************************************************
*
FUNCTION lfvAccount
PRIVATE lcObjVal,lcObjName


lcObjVal = EVALUATE(OGSYS18(.T.))  && Varible to hold  the value of the current GET field
lcObjName = OGSYS18(.T.)          && Varible to hold  the name of the memory variable used to create the current GET field

*-- Avoid recurresion clear read.
IF !(lcObjVal == lcOldVal)
  PRIVATE lnAlsNo,lcCustOrd
  lnAlsNo = SELECT(0)
  SELECT CUSTOMER
  lcCustOrd = ORDER()
  SET ORDER TO TAG CUSTOMER

  *IF The user want to Browse or if the Account he entered is not in the file
  IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
    llObjRet = CusBrowM(@lcObjVal , '' , 'M')
    lcObjVal = IIF(llObjRet , lcObjVal , lcOldVal)
    &lcObjName = lcObjVal
  ENDIF    && End of IF
ENDIF

IF (lcobjVal <> lcOldVal) 
  =lfEmpty()
ENDIF 

*-- If the user changed the stored account (old value)

  SET ORDER TO &lcCustOrd
  SELECT(lnAlsNo)


*-- End of lfvAcct.

*!*************************************************************
*! Name      : lfEmpty
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/29/2004
*! Purpose   : to Empty the header which hold the selected transactions
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfEmpty

lnTransPos=lfItmPos('ORDHDR.ORDER')
IF lnTransPos >0
  loOGScroll.laOGFxFlt[lnTransPos,6] = ''
ENDIF 

*-- End of lfEmpty.

*!*************************************************************
*! Name      : lfItmPos
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 06/29/2004
*! Purpose   : to get the position of the fixed filter in OG
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Modification : ....
*!*************************************************************
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(loOGScroll.laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos

*-- End of lfItmPos.



*!*************************************************************
*! Name      : lfGetSt
*! Developer : Ahmed Ibrahim
*! Date      : 11/24/99
*! Purpose   : Get EDI Status
*! Job No.   : *B802772,4 AMM
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetSt()
*!*************************************************************
FUNCTION lfGetSt

PRIVATE lcRetStat
lcRetStat = SPACE(0)
IF EVAL(lcTempOrd +'.cOrdType')#'T' .OR. ORDHDR.MON_FLG # 'G'
  RETURN ''
ENDIF

DO CASE
  CASE EVAL(lcTempOrd +'.CLINESTAT')='AI'
    lcRetStat = 'New Item'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='DI'
    lcRetStat = 'Deleted Item'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='QD'  
    lcRetStat = 'Quantity Decreased'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='QI'
    lcRetStat = 'Quantity Increased'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='PC'  
    lcRetStat = 'Price Changed'
  *B803690,1 AMM Add new status 'PQ'
  CASE EVAL(lcTempOrd +'.CLINESTAT')='PQ'  
    lcRetStat = 'Quantity Changed'
  *B803690,1 AMM end
    
ENDCASE





*----------------------------------------------------------------------------------------------------------*
*!*************************************************************
*! Name      : lfCollectData
*! Developer : Ahmed Ibrahim
*! Date      : 11/24/99
*! Purpose   : Collect the data 
*! Job No.   :
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfCollectData()
*!*************************************************************
FUNCTION lfCollectData
	IF !EMPTY(lcRpExp)
	  lcRpExp = lcRpExp + [ AND ]
	ENDIF
	IF lcRpOrdSta ="A"
  *E124540,1 HMA 02/20/2005 Show the order status [Bid] also [Begin]
  *lcRpExp = lcRpExp + [!(ORDHDR.Status $ "BX")]
  *E126800,1 HMA 04/03/2005 Don't Show the order status [Complete]  [Begin]
  *lcRpExp = lcRpExp + [!(ORDHDR.Status $ "X")]  
  lcRpExp = lcRpExp + [!(ORDHDR.Status $ "CX")]  
  *E126800,1 HMA 04/03/2005 Don't Show the order status [Complete]  [End]
  *E124540,1 HMA 02/20/2005 Show the order status [Bid] also [END]
	ELSE
	  lcRpExp = lcRpExp + [(ORDHDR.Status $ lcRpOrdSta)]
	ENDIF

	*--order type

	IF lcRpOrdTyp !="A"
	  IF !EMPTY(lcRpExp)
	    lcRpExp = lcRpExp + [ AND ]
	  ENDIF
	  lcRpExp = lcRpExp + [(ORDHDR.CordType $ lcRpOrdTyp)]
	ENDIF

*E999999,1 HMA 01/26/2005 replace account Filter from LIKE to INLIST [BEGIN]
*!*		IF !EMPTY(lcRpAcc)
*!*		  IF !EMPTY(lcRpExp)
*!*		    lcRpExp = lcRpExp + [ AND ]
*!*		  ENDIF
*!*		  lcRpExp = lcRpExp + [(ORDHDR.ACCOUNT = lcRpAcc)]
*!*		ENDIF
*E999999,1 HMA 01/26/2005 replace account Filter from LIKE to INLIST [BEGIN]

IF EMPTY(loOgScroll.laOgFxFlt(1,6))  &&user didn't select customers.

	SELECT ORDHDR
	*-- Collect data
	IF lcRpOrdTyp $ 'TCO'
		=SEEK(lcRpOrdTyp)
		lcOrdTypeScan = "ORDHDR.CORDTYPE+ORDHDR.ORDER = lcRpOrdTyp"
	ELSE
		lcOrdTypeScan  = ".T."
	ENDIF 
	SCAN WHILE &lcOrdTypeScan  FOR &lcRpExp
	  SELECT  ORDLINE
	  
      SCAN REST WHILE CORDTYPE + ORDER + STORE + STR(LINENO,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
	  
	    SCATT MEMVAR MEMO
		*----------
	    STORE '' TO m.SKUSize1 , m.SKUSize2 , m.SKUSize3 , m.SKUSize4 , m.SKUSize5 ,;
		         m.SKUSize6 , m.SKUSize7 , m.SKUSize8 ,m.SKU

    *E124540,1 HMA 02/20/2005  Reset The Memory Variable that Hold The Currency Code (m.CurrCode)[Begin]
    *STORE '' TO m.UpcSize1 , m.UpcSize2 , m.UpcSize3 , m.UpcSize4 , m.UpcSize5 ,;
           m.UpcSize6 , m.UpcSize7 , m.UpcSize8 
    STORE '' TO m.UpcSize1 , m.UpcSize2 , m.UpcSize3 , m.UpcSize4 , m.UpcSize5 ,;
           m.UpcSize6 , m.UpcSize7 , m.UpcSize8 ,m.CurrCode
    *-- get the Currency Code from the order Header To insert it in the Temp File
    m.CurrCode = ORDHDR.cCurrCode
    *E124540,1 HMA 02/20/2005  [End]

	    =SEEK('S' + m.Scale,"SCALE")
	    FOR lnCount = 1 TO Scale.Cnt
		    lcCount = STR(lnCount , 1)
	        IF SEEK(m.Style + lcCount , "StyleUPC")
	          m.UPCSize&lcCount= StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
	        ENDIF
	    ENDFOR

	    IF SEEK('S' + OrdLine.Account + m.Style,'SPCK_LIN')
	      SELECT SPCK_LIN
		  m.SKU=LEFT(Pack_Id,lnLenSku)
	      lnCount = 1
		  SCAN FOR Type + Account + Style + Pack_ID= 'S' + OrdLine.Account + m.Style
			lcCount = STR(lnCount , 1)
		    m.SkuSize&lcCount = AllTrIM(SUBSTR(PACK_ID,lnlensku,Len(Pack_ID)))
	    	lnCount = 1 + lnCount
		  ENDSCAN
	    ENDIF
		*----------
	    INSERT INTO (lcTempOrd) FROM MEMVAR
	  ENDSCAN
	ENDSCAN

ELSE

	lcOrderTmp  = ALLTRIM(loOgScroll.laOgFxFlt(1,6))
	*-- Collect data
	SELECT (lcOrderTmp)
	SCAN 
		=SEEK(lcRpOrdTyp+ &lcOrderTmp..Order, 'ORDHDR')
		SELE ORDLINE
		*SCAN REST WHILE CORDTYPE+ORDER = ORDHDR.CORDTYPE+ORDHDR.ORDER
		 SCAN REST WHILE CORDTYPE + ORDER + STORE + STR(LINENO,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
		
		    SCATT MEMVAR MEMO
			*----------
		    STORE '' TO m.SKUSize1 , m.SKUSize2 , m.SKUSize3 , m.SKUSize4 , m.SKUSize5 ,;
			         m.SKUSize6 , m.SKUSize7 , m.SKUSize8 ,m.SKU

       *E124540,1 HMA 02/20/2005  Reset The Memory Variable that Hold The Currency Code (m.CurrCode)[Begin]
       *STORE '' TO m.UpcSize1 , m.UpcSize2 , m.UpcSize3 , m.UpcSize4 , m.UpcSize5 ,;
              m.UpcSize6 , m.UpcSize7 , m.UpcSize8 
        STORE '' TO m.UpcSize1 , m.UpcSize2 , m.UpcSize3 , m.UpcSize4 , m.UpcSize5 ,;
              m.UpcSize6 , m.UpcSize7 , m.UpcSize8 ,m.CurrCode
       *-- get the Currency Code from the order Header To insert it in the Temp File
        m.CurrCode = ORDHDR.cCurrCode
       *E124540,1 HMA 02/20/2005  [End]

		    =SEEK('S' + m.Scale,"SCALE")
		    FOR lnCount = 1 TO Scale.Cnt
			    lcCount = STR(lnCount , 1)
		        IF SEEK(m.Style + lcCount , "StyleUPC")
		          m.UPCSize&lcCount= StyleUPC.cUPCNum2 + StyleUPC.cUPCNum3
		        ENDIF
		    ENDFOR

		    IF SEEK('S' + OrdLine.Account + m.Style,'SPCK_LIN')
		      SELECT SPCK_LIN
			  	m.SKU=LEFT(Pack_Id,lnLenSku)
		      lnCount = 1
				  SCAN FOR Type + Account + Style + Pack_ID= 'S' + OrdLine.Account + m.Style
					lcCount = STR(lnCount , 1)
				    m.SkuSize&lcCount = AllTrIM(SUBSTR(PACK_ID,lnlensku,Len(Pack_ID)))
			    	lnCount = 1 + lnCount
				  ENDSCAN
		    ENDIF
		 INSERT INTO (lcTempOrd) FROM MEMVAR
		 ENDSCAN
	ENDSCAN 		 
ENDIF 

*!*************************************************************
*! Name      : lfCollTime
*! Developer : Heba Mohamed Amin (HMA)
*! Date      : 03/17/2005
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
*! Passed Parameters  : Start collection date,End collection date
*!*************************************************************
*! Returns            : Spent time.
*!*************************************************************
*! Modification :according to B126801
*!*************************************************************
FUNCTION lfCollTime
PARAMETERS lcStart,lcEnd

lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)