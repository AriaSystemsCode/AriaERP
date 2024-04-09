*!*:**********************************************************************
*: Program file       : MaMatP
*: Program description: Material Purchase Order
*: Module             : Material (MA)
*: Developer          : Heba Fathi (HFK)
*: Tracking Job Number: 037632
*: Date               : 12/08/2004
*:**********************************************************************
*: Calls: 
*:         Programs   : 
*:         Screens    : 
*: Global Function    :gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe,
*:                     gfRltFld,gfCodDes,gfGetMemVar,gfOptMsg
*:**********************************************************************
*: Called From: 
*:**********************************************************************
*: Passed Parameters  : Work File if print material requiment report
*:**********************************************************************
*: Notes         : It's so hard for all of us when we print more than one notepad
*:               : from master file, The following is its steps respectively
*:               : 1- Make Temp. Dummy file has two records have indexed 
*:               :    data to relate it with master loop file and to print
*:               :    notes, for example in this report 
*:               :    'N1'-> Print Item Notepad,'N2'-> Print PO Notepad.
*:               : 2- set skip to this file and conditions use it to print.
*:               : 3- In .Frx do the following...
*:               :    - Call functions from group headers to know in which
*:               :      record you print notepads, for example in this report
*:               :      lfMPOEnd and lfFabEnd Functions. 
*:               :    - Call function from detail band to notepad data like lfGetNotes
*:               :      in this report. I respect that any one may ask me why 
*:               :      I call this function and don't print directely from 
*:               :      the field, this is because we want to print 
*:               :      Box around the notepad, thus you can see that my code
*:               :      clear both lcTitle and lcNotes every time we does not
*:               :      have notepad data.
*:               :   - Print when condition for Box around notepad is under 
*:               :     this expression !EMPTY(lcTitle).
*:***************************************************************************
*: Example : DO MaMatPo
*:***************************************************************************
*: Modification: 
*: B128016,1 KHM 05/17/2005 Add 'M' to the indicate the key in the notepad for materials 
*: B128519,1 SMM 06/15/2005 Adjust UOM Buy
*: B129084,1 HFK 07/25/2005 Enhance Performance
*: B129381,1 HFK 08/15/2005 Get Material Description.
*: B127974,1 MMT 05/13/2005, fix bug of add field in layout to hold line location
*: B608134,1 MMT 06/19/2007 fix bug of wrong order of costing elements in Summmary Folder[T20070531.0012]
*: B608154,1 MMT 07/08/2007 fix bug of not printing Material reference                  [T20070619.0013]
*: B608394,1 MMT 12/27/2007 Fix Bug of wrong Ship to address [T20071027.0001]
*: C200961,1 MMT 03/06/2008 Add Reference Field to fields Selected form POSLN[T20080204.0007]	
*: N037435,1 MMT 08/28/2008 Convert Contract screen to Aria4[T20061226.0014]
*: B609515,1 TMI 02/05/2011 Allowing to print BID MA PO's [T20101229.0002]
*: B609515,1 TMI            I added a new filter to the laOgVrFlt with the PO.Status, but the original code was hard coded to 
*: B609515,1 TMI            deal with the laOgHdFlt so to not change more code I found that it is better just add the missing statuses to the PO.STATUS fileter in the laOgHdFlt and to add a new typical one in the laOgVrFlt with the related line in the SYREPUVR
*: B609515,1 TMI            I also within this fix changed the phrase (INDEX= <indexname>) to WITH(INDEX(<indexname>))
*: B610095,1 HIA 09/24/2012 add complete and dylot fields for reason line [T20120821.0010]
*: E303387,1 MMT 05/13/2013 Select the Contents field from the POSLN table to be used in PO form IK[T20130312.0031]
*:***************************************************************************
*:
#INCLUDE R:\ARIA4XP\REPORTS\MA\MAMATP.H
PARAMETERS lcFileName


*!* N037435,1 MMT 08/28/2008 Convert Contract screen to Aria4[Start]
IF TYPE('lcFromC') <> 'U'
  lcRpForm = lcFromC
ENDIF 
*!* N037435,1 MMT 08/28/2008 Convert Contract screen to Aria4[End]


*! B128016,1 SMM Let the Orientation to be Portrait [START]
loOGScroll.cCROrientation = 'P'
*! B128016,1 SMM Let the Orientation to be Portrait [END]
*-- Check if printing from MPO screen [Start]
IF TYPE('lcNewMPos') = 'C' .AND. USED(lcNewMPos)
  lcRpExp  = lcRpExp + [ AND SEEK(PO,lcNewMPos)]
ENDIF

IF TYPE('loCodes') <> 'O'
  loCodes = CreateObject("RemoteTable","CODES","CODES","CODES",SET("Datasession"))
ENDIF 

*: B610095,1 HIA 09/24/2012 add complete and dylot fields for reason line [T20120821.0010][Begin]
STORE .f. TO ldye, lcdt
lcdt = (gfGetMemVar('M_DLIVDATE')=='Y')
ldye = (gfGetMemVar('M_MATDYE')=='Y')
*: B610095,1 HIA 09/24/2012 add complete and dylot fields for reason line [T20120821.0010][End]

lcTime     = TIME()                     && Variable to hold the Time
lnFabEnd   = 0                          && Record No. Of first record in fabric group.
lnMPOEnd   = 0                          && Record No. Of last record  in PO#    group.
lcDivLName = ''                         && Variable to hold division long name.

*- Call lfIsApparel to find out if the form is an FRX or an @... SAY
*- report. If it is an @ SAY, get the fullpath of the program 
*- (by calling lfIsApparl())
lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)
llNoRec = .F.
llExternal = .F.                        && .T. if called from another program.
*-- if called from another program [Begin]
IF TYPE('lcFileName') = 'C'
  *-- Define variables that defined in OG for this report [begin]
  STORE ' ' TO lcCompName,lcRpMsg1,lcRpMsg2,lcRpMsg3,lcTerms,lcDivDesc,lcShipVia
  STORE .F. TO llRpPrtPn,llRpPrtMn,llRpFCurr
  STORE .T. TO llExternal,llRpCostCt,llRpVenRef
  *-- Define variables that defined in OG for this report [end  ]

  *-- open files used by this report [begin]
  llCust    = gfOpenFile(oAriaApplication.DataDir+'CUSTOMER','CUSTOMER','SH')
  llObjects = gfOpenFile(oAriaApplication.DataDir+'OBJECTS','Objectid','SH')
  llObjLnk  = gfOpenFile(oAriaApplication.DataDir+'OBJLINK','Objlnkty','SH')
  llCurrFile= gfOpenFile(oAriaApplication.SysPath+'SYCCURR','Ccurrcode','SH')
  llCodes   = gfOpenFile(oAriaApplication.DataDir+'CODES','CODES','SH')
  *-- open files used by this report [end  ]
ENDIF

*-- flag to company logo
llHaveLogo = .F.
*-- lcMerch   : Variable hold Currency symbol + total Merchadise amount.
*-- lcFreight : Variable hold Currency symbol + Freight.
*-- lcTax     : Variable hold Currency symbol + Tax.
*-- lcQuota   : Variable hold Currency symbol + Quota.
STORE '' TO lcMerch,lcFreight,lcTax,lcQuota
*-- Print Note Variables [begin]
lcTitle    = ''          && Hold Title of Notepad. 
lcCHTitle    = ''          && Hold Title of Notepad. 

lcNotes    = ''          && Hold Notepad.
*-- Print Note Variables [end]
llEndGroup = .F.                        && Flag to know if we are at the end of the Group
*-  check if the compny have the logo
SELECT ObjLink
SELECT Objects
llHaveLogo = SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')

*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF EMPTY(lcCompName)
  *- Open Fabric file 
  *-HFK, 07/25/2005, B129084,enhance performane
  IF TYPE('loItem') <> 'O'
    loItem = CreateObject("RemoteTable","ITEM","STYLE","ITEM",SET("Datasession"))
    loItem.SetOrder('STYLE')
    SELECT ITEM 
  ENDIF 
  
*!*    lcItemSelect = " SELECT Item.Style,Item.cItemFld2,Item.cItemFld3,Item.[Desc],UOM.CUOM_B as UOMBUY From Item (INDEX = Style) "
*!*    lcItemSelect = lcItemSelect + " INNER JOIN UOM (INDEX = UOM) ON ITEM.CCONVBUY = UOM.CUOMCODE WHERE ITEM.cInvType = 0002 "
*!*    lnItemResult = loOGScroll.oRDA.SqlRun (lcItemSelect,"ITEM",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
*!*    IF lnItemResult > 0
*!*      lnBuffering = CURSORGETPROP("Buffering","ITEM")
*!*      =CURSORSETPROP("Buffering",3,"ITEM")
*!*      SELECT ITEM
*!*      INDEX ON STYLE TAG STYLE
*!*    ELSE 
*!*      =loOGScroll.oRDA.CheckRetResult("SqlRun",lnItemResult,.T.)
*!*      RETURN .F.
*!*    ENDIF
  *-HFK, 07/25/2005, B129084,enhance performane
  *-- if this company have a logo, point to logo record, 
  *-- else seek put pointer at EOF.
  DECLARE laCompAdd[6] , laVendor[6] , laShipTo[6] , laDivLName[1,2]
  laCompAdd = ''                   && Array to hold the Company address
  laVendor  = ''                   && Array to hold vendor address
  laShipTo  = ''                   && Array to hold the Ship To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  *-- Get company Address [begin].
  lcSycCompSelect=[SELECT * FROM SYCCOMP WHERE Ccomp_id=']+oAriaApplication.ActiveCompanyID+[']
  lnCompResult = oAriaApplication.remotesystemdata.execute(lcSycCompSelect,"","SYCCOMP","",oAriaApplication.SystemConnectionString,3,"",SET("DATASESSION")) 
  IF lnCompResult >= 1 
    lcCompName = cCom_Name             && Company Name.
    lcCompPhon = cCom_Phon             && Company Phone.
    lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.
    laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
    laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
    laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
    laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
    laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
    *-laCompAdd[6] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
    *laCompAdd[6]   = LANG_MAMATP_Phone + TRANSFORM(lcCompPhon , lcPhonPict)  
    laCompAdd[6]   = LANG_MAMATP_Phone + TRANSFORM(lcCompPhon , "@R "+lcPhonPict)      
    = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  ENDIF 
  *-- Get company Address [end].

  *-- lcNoteLns : Name of Temp. Dummy Loop File which is used to 
  *--           : print both Fabric notepad and PO notepad from notepad file.
  *--           : note that this name and temp. file is created 
  *--           : one for every optional grid seasson run.
  *-- Create dummy loop file, and fill it with two records have [begin]
  *-- values 'N1' and 'N2' respectivly
  lcNoteLns = loOGScroll.gfTempName()
  gfCrtTmp(lcNoteLns,"(cRecord C(2))","cRecord",lcNoteLns,.T.)
  SELECT (lcNoteLns)
  *--HDM 05/31/1999 [End]  
  FOR lnI = 1 TO 2
    APPEND BLANK
    REPLACE cRecord WITH "N"+STR(lnI,1)
  ENDFOR
  *-- Create dummy loop file [end]
ENDIF  && end if it's first time you run option Grid.


IF loOGScroll.llOGFltCh  && if there is a change in filter
  IF USED('POSLN')
    USE IN POSLN
  ENDIF 

  IF USED('POSHDRA')
    USE IN POSHDRA
  ENDIF 

  IF USED('POSLN_A')
    USE IN POSLN_A
  ENDIF 

  IF USED('POSHDRM') 
    USE IN POSHDRM
  ENDIF 

  lcPath = oAriaApplication.WorkDir
  lcFileName = "POSLN_A.DBF"
  IF File('&lcPath&lcFileName')
    ERASE lcPath + "POSLN_A.DBF" 
    ERASE lcPath + "POSLN_A.CDX" 
  ENDIF 

  lcFileName = "POSLN.DBF"
  IF File('&lcPath&lcFileName')
    ERASE lcPath + "POSLN.DBF" 
    ERASE lcPath + "POSLN.CDX" 
  ENDIF 

  lcFileName = "POSHDRA.DBF"
  IF File('&lcPath&lcFileName')
    ERASE lcPath + "POSHDRA.DBF" 
    ERASE lcPath + "POSHDRA.CDX" 
  ENDIF 

  lcFileName = "POSHDRM.DBF"
  IF File('&lcPath&lcFileName')
    ERASE lcPath + "POSHDRM.DBF" 
    ERASE lcPath + "POSHDRM.CDX" 
  ENDIF 

  *- identify Sql condistion
  lcTypeCond = IIF(lcRpForm='P'," CBUSDOCU ='P' ",IIF(lcRpForm = 'R' ," CBUSDOCU = 'R' ",IIF(lcRpForm = 'C'," CBUSDOCU = 'C' "," CBUSDOCU IN ('P','R','C')")))
  lcTypeCond = lcTypeCond +  " AND CSTYTYPE = 'M' "
  IF !EMPTY(loOGScroll.lcRpSqlExp)
    lnPoslnCondPos = ATC('POSLN.TRANCD',loOGSCroll.lcRpSqlExp)
    lnEndCond = lnPoslnCondPos - 5
    lcScattered = SUBSTR(loOGScroll.lcRpSqlExp,1,lnEndCond)
    lcPoCondition = lcTypeCond + " AND " + lcScattered
  ENDIF 
  *B127974,1 MMT 05/13/2005, fix bug of add field in layout to hold line location[Start]
  lcPosHdrSelect = " SELECT cBusDocu,cStyType,PO,Complete,cFob,Origin,Insurance,cPriceCur,Contact,Phone,Entered,Vendor,lMultiWare,"
  *  lcPosHdrSelect = " SELECT cBusDocu,cStyType,PO,Complete,cFob,Origin,Insurance,cPriceCur,Contact,Phone,Entered,Vendor,"
  *B127974,1 MMT 05/13/2005, fix bug of add field in layout to hold line location[End]
  lcPosHdrSelect = lcPosHdrSelect + " cDutyCur,nFCost1,nICost1,nFCost2,nICost2,nFCost3,nICost3,nFCost4,nICost4,CDIVISION,"
  lcPosHdrSelect = lcPosHdrSelect + " COutAddr1,COutAddr2,COutAddr3,COutAddr4,COutAddr5,LC,CTermCode,cWareCode,ShipVia,"
  *B609515,1 TMI 02/05/2011 [Start] changed the phrase (INDEX= <indexname>) to WITH(INDEX(<indexname>))
  *lcPosHdrSelect = lcPosHdrSelect + " cRequis,Status,QuotaCat,nStyOrder FROM POSHDR (INDEX = POSHDR) WHERE " + lcPoCondition
  lcPosHdrSelect = lcPosHdrSelect + " cRequis,Status,QuotaCat,nStyOrder FROM POSHDR WITH(INDEX(POSHDR)) WHERE " + lcPoCondition
  *B609515,1 TMI 02/05/2011 [End  ] 
  lnPosHdrAResult = loOGScroll.oRDA.SqlRun (lcPosHdrSelect,"POSHDRA",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
  IF lnPosHdrAResult > 0
    lnBuffering = CURSORGETPROP("Buffering","POSHDRA")
    =CURSORSETPROP("Buffering",3,"POSHDRA")
    SELECT POSHDRA
    INDEX ON cBusDocu + cStyType + PO TAG POSHDRA 
  ELSE 
    =loOGScroll.oRDA.CheckRetResult("SqlRun",lnPosHdrAResult,.T.)
    RETURN .F.
  ENDIF

  IF !USED('POSHDRM')
    lnPosHdrResult = loOGScroll.oRDA.SqlRun (lcPosHdrSelect,"POSHDRM",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
    IF lnPosHdrResult > 0
      lnBuffering = CURSORGETPROP("Buffering","POSHDRM")
      =CURSORSETPROP("Buffering",3,"POSHDRM")
      SELECT POSHDRM
      INDEX ON cBusDocu + cStyType + PO TAG POSHDRM
    ELSE 
      =loOGScroll.oRDA.CheckRetResult("SqlRun",lnPosHdrResult,.T.)
      RETURN .F.
    ENDIF
  ENDIF 

  *-- Get fields from POSLN file as it is not opened from OG.
  DO CASE
    CASE lcRpForm = 'P'
      lcPosLnType = " POSLN.CBUSDOCU = 'P' AND POSLN.CSTYTYPE = 'M' "
    CASE lcRpForm = 'R'
      lcPosLnType = " POSLN.CBUSDOCU = 'R' AND POSLN.CSTYTYPE = 'M' "
    CASE lcRpForm = 'C'
      lcPosLnType = " POSLN.CBUSDOCU = 'C' AND POSLN.CSTYTYPE = 'M' "
    OTHERWISE
      lcPosLnType = " POSLN.CBUSDOCU IN ('P','R','C') AND POSLN.CSTYTYPE = 'M' "
  ENDCASE

  lcPosLnCond = lcPosLnType + " And " + loOGScroll.lcRpSqlExp
  *--B127974,1 MMT 05/13/2005, fix bug of add location field to layout[Start]
  *  lcPosLnSelect = " SELECT PosLn.CBUSDOCU,PosLn.CSTYTYPE,PosLn.PO,PosLn.CINVTYPE,PosLn.STYLE,PosLn.[LINENO],PosLn.TRANCD,"
  lcPosLnSelect = " SELECT PosLn.CBUSDOCU,PosLn.CSTYTYPE,PosLn.PO,PosLn.CINVTYPE,PosLn.STYLE,PosLn.[LINENO],PosLn.TRANCD,PosLn.Cwarecode,"
  *--B127974,1 MMT 05/13/2005, fix bug of add location field to layout[End]
  lcPosLnSelect = lcPosLnSelect + " PosLn.nFCost1,PosLn.nICost1,PosLn.TotQty,PosLn.Style,PosLn.Width,PosLn.Pattern "
  *! B128519,1 SMM 06/15/2005 Adjust UOM Buy [START]
  lcPosLnSelect = lcPosLnSelect + " ,UOM.cUOM_B as UOMBUY " 
  *! B128519,1 SMM 06/15/2005 Adjust UOM Buy [END]



  *C200961,1 MMT 03/06/2008 Add Reference Field to fields Selected form POSLN[Start]	
  lcPosLnSelect = lcPosLnSelect + " ,POSLN.reference" 
  *C200961,1 MMT 03/06/2008 Add Reference Field to fields Selected form POSLN[End]	


  *: B608154,1 MMT 07/08/2007 fix bug of not printing Material reference[Start]
  lcPosLnSelect = lcPosLnSelect + " ,POSLN.CVENCOlR,POSLN.CVENFAB" 
  *: B608134,1 MMT 07/08/2007 fix bug of not printing Material reference[End]

  
  *! B127557,1 HFK 07/03/2005 Add ShipVia and HTSUS [Start]
  lcPosLnSelect = lcPosLnSelect + " ,PosLn.chtsNo,Posln.ShipVia " 
  *! B127557,1 HFK 07/03/2005 Adjust UOM Buy [End]
  
  *!B130808,1 AGR 02/01/2006 [Start]
  lcPosLnSelect = lcPosLnSelect + " ,PosLn.cVenSty " 
  *!B130808,1 AGR 02/01/2006 [End]
  *: E303387,1 MMT 05/13/2013 Select the Contents field from the POSLN table to be used in PO form IK[Start]
  lcPosLnSelect = lcPosLnSelect + " ,posln.cItemFld3 " 
  *: E303387,1 MMT 05/13/2013 Select the Contents field from the POSLN table to be used in PO form IK[End]
  *: B610095,1 HIA 09/24/2012 add complete and dylot fields for reason line [T20120821.0010][Begin]
  lcPosLnSelect = lcPosLnSelect + " ,PosLn.dyelot ,PosLn.complete " 
  *: B610095,1 HIA 09/24/2012 add complete and dylot fields for reason line [T20120821.0010][End  ]

  *B609515,1 TMI 02/05/2011 [Start] changed the phrase (INDEX= <indexname>) to WITH(INDEX(<indexname>))
  *lcPosLnSelect = lcPosLnSelect + " FROM PosLn (INDEX = PosLn) Inner Join PosHdr (Index = PosHdr) On " 
  lcPosLnSelect = lcPosLnSelect + " FROM PosLn WITH(INDEX(PosLn)) Inner Join PosHdr WITH(Index(PosHdr)) On " 
  *B609515,1 TMI 02/05/2011 [End  ] 
  lcPosLnSelect = lcPosLnSelect + " PosHdr.CBUSDOCU+PosHdr.CSTYTYPE+PosHdr.PO = POSLN.CBUSDOCU+POSLN.CSTYTYPE+POSLN.PO " 

  *! B128519,1 SMM 06/15/2005 Adjust UOM Buy [START]
  lcPosLnSelect = lcPosLnSelect + " Inner Join UOM on POSLN.cUOMCODE = UOM.cUOMCODE " 
  *! B128519,1 SMM 06/15/2005 Adjust UOM Buy [END]

  lcPosLnSelect = lcPosLnSelect + " WHERE " + lcPosLnCond
  lnPosLnResult = loOGScroll.oRDA.SqlRun (lcPosLnSelect,"PosLn",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
  IF lnPosLnResult > 0
    lnBuffering = CURSORGETPROP("Buffering","PosLn")
    =CURSORSETPROP("Buffering",3,"PosLn")
    SELECT PosLn 
    INDEX ON CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD TAG PosLn 
  ELSE 
    =loOGScroll.oRDA.CheckRetResult("SqlRun",lnPosLnResult,.T.)
    RETURN .F.
  ENDIF

  lnPosLnAResult = loOGScroll.oRDA.SqlRun (lcPosLnSelect,"PosLn_A",,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession"))
  IF lnPosLnAResult > 0
    lnBuffering = CURSORGETPROP("Buffering","PosLn_A")
    =CURSORSETPROP("Buffering",3,"PosLn_A")
    SELECT PosLn_A 
    INDEX ON CBUSDOCU+CSTYTYPE+PO+CINVTYPE+STYLE+STR(LINENO,6)+TRANCD TAG PosLn_A
  ELSE 
    =loOGScroll.oRDA.CheckRetResult("SqlRun",lnPosLnAResult,.T.)
    RETURN .F.
  ENDIF
ENDIF && if filter changed


lcSkipExpr = [PosLn]    && Skip Expression.
GO TOP IN (lcNoteLns)      && Current refrence is 'N1'

*-HFK, 07/25/2005, B129084,enhance performane
IF TYPE('loItem') <> 'O'
  loItem = CreateObject("RemoteTable","ITEM","STYLE","ITEM",SET("Datasession"))
  loItem.SetOrder('STYLE')
  SELECT ITEM 
ENDIF 
*-HFK, 07/25/2005, B129084,enhance performane

*-- Set relation between used files [Begin]
SELECT PosLn

*-- Re-establish the relation as oAriaApplication.ActiveCompanyID removed from codes file
SET RELATION TO 'N' + SUBSTR(STYLE,lnMajorLen+2) + "N" + "COLOR" INTO Codes
SET RELATION TO Style INTO Item ADDITIVE

*-- if called from its OG.
IF !llExternal
  SET RELATION TO 'G'+SUBSTR(STYLE,1,12) INTO Notepad ADDITIVE
ENDIF  
*-- if you print PO Notepad set relation to dummy file.
IF llRpPrtPn
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  lcSkipExpr = [PosLn,&lcNoteLns]
ENDIF  && end if you print PO Notepad set relation to dummy file.

SELECT PosHdrA
SET RELATION TO Vendor        INTO Apvendor
SET RELATION TO cWareCode     INTO WareHous ADDITIVE
SET RELATION TO cBusDocu + cStyType + PO INTO PosLn ADDITIVE

*-- if called from its OG.
IF !llExternal
  *128016,1 KHM 05/17/2005 Add 'M' to the indicate the key in the notepad for materials [Begin]
  *SET RELATION TO cBusDocu + PO INTO NOTEPAD_A ADDITIVE
  SET RELATION TO 'M'+cBusDocu + PO INTO NOTEPAD_A ADDITIVE
  *128016,1 KHM 05/17/2005 [End]

ENDIF
*-- Set relation between used files [End]
SET SKIP TO &lcSkipExpr
*-- if called from another program
IF llExternal
  lcRpExp  = [cBusDocu = 'P' AND SEEK(PO,lcFileName)]  && Make report expression.

  *-HFK
  lcRpExp = STRTRAN(lcRpExp,'POSHDR','POSHDRM')
  *-HFK
  *-B127874, HFK 05/11/2005 [Start]
  IF RECCOUNT()<>0
    lcRecCount = LTRIM(STR(RECCOUNT(),7))
    WAIT WINDOW 'Selected &lcRecCount RECORDS FOR REPORT...' TIMEOUT 1
  ENDIF 
  *-B127874, HFK 05/11/2005 [End]  
  DO gfDispRe WITH EVAL('lcRpRName') , 'FOR ' + lcRpExp  && Call report form.
  USE IN (lcNoteLns)  && Clear memory from temp. dummy cursor.
  IF llCust
    USE IN CUSTOMER
  ENDIF    
  IF llObjects
    USE IN OBJECTS
  ENDIF
  IF llObjLnk
    USE IN OBJLINK
  ENDIF  
  IF llCurrFile
    USE IN SYCCURR
  ENDIF
  IF llCodes
    USE IN CODES
  ENDIF
  SELECT PosHdrM
  SET RELATION TO
  
  SELECT PosLn
  SET RELATION TO

ELSE  && else called from its OG
  *-- lcRepExpr : Expression to loop around wanted records only.
  lcRepExpr = [IIF(llRpPrtPn,IIF(&lcNoteLns..cRecord = 'N2',RECNO('PosLn') = lnMPOEnd,.T.),.T.)] 
  
  *-- We define this variable in the Material 
  *-- Purchase Order program to fix the problem that the program didn't print 
  *-- the contract and return PO. [Begin]
  IF TYPE('lcTypPrint ') $ 'UL'
    lcRpExp   = [cBusDocu + PO = IIF(lcRpForm = 'A','',lcRpForm) AND ] +;
                IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr) 
  ELSE
    lcRpExp   = [cBusDocu + PO = lctypPrint .AND. ] +;
                IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr) 
  ENDIF
  *-- to run an optional program
  =lfOptProg()

  IF llIsAparel
    DO EVAL('lcPrgName')
    IF !llNoRec
      DO ENDREPORT
    ENDIF
  ELSE
    *-HFK
    lcRpExp = STRTRAN(lcRpExp,'POSHDR','POSHDRM')
    *-HFK

    *-B127874, HFK 05/11/2005 [Start]
    IF RECCOUNT()<>0
      lcRecCount = LTRIM(STR(RECCOUNT(),7))
      WAIT WINDOW 'Selected &lcRecCount RECORDS FOR REPORT...' TIMEOUT 1
    ENDIF 
    *-B127874, HFK 05/11/2005 [End]
    *-HFK  
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp  && Call report form.  
    *-HFK
  ENDIF      
ENDIF
RETURN
*-- end of Report Code.
*------------------- Functions section -----------------*
*------------------------- Begin -----------------------*
*----
*-- 1) Begin of Functions called from report code.
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Passed Parameters : Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*-FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *-IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *-IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.
*-- 1) End of Functions called from report code.
*!
*-- 2) Begin of Functions called from Option Grid.
*!
*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.
*!
*!*************************************************************
*! Name      : lfvTrnTyp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Unselect transactions due to changing PO Type .
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfvTrnTyp
lnPos = ASCAN(loOGScroll.laogVrFlt,'POSHDR.PO')
IF lnPos <> 0 
 lnPos = ASUBSCRIPT(loOGScroll.laogVrFlt,lnPos,1)
 lcPos = loOGScroll.laogVrFlt[lnPos,6]
 IF !EMPTY(lcPos)
   SELECT(lcPos)
   ZAP 
 ENDIF 
ENDIF  

llClearit = (lcRpForm # 'A' )   && Unselect selected transactions.
lcRpMaKey = IIF(lcRpForm='A','',lcRpForm)

DO CASE
  CASE lcRpForm = 'P'
    lcKeyExpr = 'PM'
  CASE lcRpForm = 'R'
    lcKeyExpr = 'RM'
  CASE lcRpForm = 'C'
    lcKeyExpr = 'CM'
  OTHERWISE
    lcKeyExpr = ""
ENDCASE
ClearRead()
IF lcRpForm ='A'
  lcPoFlds = "CBUSDOCU :R :H='Type',PO :R :H='P\O #',STATUS :R :H= 'S',VENDOR :R :H='Vendor',APVENDOR.cVenComp:R :H ='Name',COMPLETE:R :H='Complete',TOTQTY= OPEN+ RECEIVE :R :H ='Tot. Qty',POTOTAL :R :H='Amount',RECEIVE :R :H='Receive',OPEN :R :H='Open'"
ELSE
  lcPoFlds = " PO :R  :H= 'P\O #' , STATUS :R :H= 'S' , VENDOR :R :H= 'Vendor' , APVENDOR.cVenComp :R :H = 'Name'  ,COMPLETE :R :H= 'Complete' , TOTQTY= OPEN+ RECEIVE :R :H = 'Tot. Qty'  ,POTOTAL :R :H= 'Amount' , RECEIVE :R :H= 'Receive' , OPEN :R :H= 'Open' "
ENDIF
llClrVend = .T.
*-- end of lfvTrnTyp.
*!
*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/05/98
*! Purpose   : Set Files used by In Range operator, and then rest it.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*!
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO POSHDR IN POSHDR
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO VENDOR INTO APVENDOR
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
    SET ORDER TO 0 IN POSHDR   
    *-SET ORDER TO 0 IN APVENDOR
    llClearit = .F.
ENDCASE
*-- end of lfsrvTrans.
*!
*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Function that we call when Close the option grid.
*!           : to clear our variables.
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfClearRep

*-- Close Fabric file.
IF (ASCAN(loOGScroll.laSelFile,'ITEM') = 0) AND USED('ITEM')
  USE IN ITEM
ENDIF

IF USED(lcNoteLns)
 USE IN (lcNoteLns)  && Clear memory from temp. dummy cursor.
ENDIF
*!
*!*************************************************************
*! Name      : RefreshOptMsg
*! Developer : Heba Fathi (HFK)
*! Date      : 12/09/2004
*! Purpose   : Display Optional Message in its Text Box
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION RefreshOptMsg
  IF EMPTY(lcRpMsg1) .AND. EMPTY(lcRpMsg2) .AND. EMPTY(lcRpMsg3)
    RETURN ""
  ELSE 
    RETURN ALLTRIM(lcRpMsg1) + IIF(EMPTY(lcRpMsg2),"",", ") +;
           ALLTRIM(lcRpMsg2) + IIF(EMPTY(lcRpMsg3),"",", ") +;
           ALLTRIM(lcRpMsg3)
  ENDIF 
ENDFUNC 
*!
*-- end of lfClearRep.
*-- 2) End of Functions called from Option Grid.
*--
*-- 3) Begin of Functions called from report form (MAMATPA.FRX).
*!*************************************************************
*! Name      : lfHeadVar
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/08/1998
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfHeadVar
PRIVATE lcPriceSmb , lcDutySmb

*-- Get Currency symbol for Price, and Duty [begin]
lcPriceSmb  = PADL(ALLTRIM(IIF(llRpFCurr,IIF(SEEK(PosHdrA.cPriceCur,'SYCCURR'),SycCurr.cCurrSmbl,''),IIF(SEEK(oAriaApplication.BaseCurrency,'SYCCURR'),SycCurr.cCurrSmbl,''))),3)
lcDutySmb   = PADL(ALLTRIM(IIF(llRpFCurr,IIF(SEEK(PosHdrA.cDutyCur ,'SYCCURR'),SycCurr.cCurrSmbl,''),IIF(SEEK(oAriaApplication.BaseCurrency,'SYCCURR'),SycCurr.cCurrSmbl,''))),3)

*-- Get Currency symbol for Price, and Duty [end]
lcMerch     = lcPriceSmb + ALLTRIM(STR(IIF(llRpFCurr,PosHdrA.nFCost1,PosHdrA.nICost1),13,3))

*: B608134,1 MMT 06/19/2007 fix bug of wrong order of costing elements in Summmary Folder[Start]
*!*	lcFreight   = lcDutySmb  + ALLTRIM(STR(IIF(llRpFCurr,PosHdrA.nFCost2,PosHdrA.nICost2),13,3))
*!*	lcTax       = lcDutySmb  + ALLTRIM(STR(IIF(llRpFCurr,PosHdrA.nFCost3,PosHdrA.nICost3),13,3))
*!*	lcQuota     = lcDutySmb  + ALLTRIM(STR(IIF(llRpFCurr,PosHdrA.nFCost4,PosHdrA.nICost4),13,3))
lcFreight   = lcDutySmb  + ALLTRIM(STR(IIF(llRpFCurr,PosHdrA.nFCost4,PosHdrA.nICost4),13,3))
lcTax       = lcDutySmb  + ALLTRIM(STR(IIF(llRpFCurr,PosHdrA.nFCost2,PosHdrA.nICost2),13,3))
lcQuota     = lcDutySmb  + ALLTRIM(STR(IIF(llRpFCurr,PosHdrA.nFCost3,PosHdrA.nICost3),13,3))
*: B608134,1 MMT 06/19/2007 fix bug of wrong order of costing elements in Summmary Folder[End]


= gfRltFld(PosHdrA.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
lcDivDesc   = gfCodDes(PosHdrA.CDIVISION, 'CDIVISION')     && Division description.
lcShipVia   = gfCodDes(PosHdrA.ShipVia , 'SHIPVIA')        && Ship Via description.
lcTerms     = gfCodDes(PosHdrA.CTermCode   , 'CTERMCODE')  && Terms description.

*-- Fill Vendor array with its data [Begin]
laVendor[1] = APVENDOR.CVenComp
laVendor[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
laVendor[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
laVendor[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
laVendor[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
laVendor[6] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
*-- Get the vendor addresses
= lfAdrShift('laVendor')
*-- Fill Vendor array with its data [End]

*-- Fill Ship to array with its data. [begin]
laShipTo[1] = WareHous.cDesc

*B608394,1 MMT 12/27/2007 Fix Bug of wrong Ship to address [Start]
*!*  laShipTo[2] = PosHdrM.COutAddr1
*!*  laShipTo[3] = PosHdrM.COutAddr2
*!*  laShipTo[4] = PosHdrM.COutAddr3
*!*  laShipTo[5] = PosHdrM.COutAddr4
*!*  laShipTo[6] = PosHdrM.COutAddr5

laShipTo[2] = PosHdrA.COutAddr1
laShipTo[3] = PosHdrA.COutAddr2
laShipTo[4] = PosHdrA.COutAddr3
laShipTo[5] = PosHdrA.COutAddr4
laShipTo[6] = PosHdrA.COutAddr5
*B608394,1 MMT 12/27/2007 Fix Bug of wrong Ship to address [End]

= lfAdrShift('laShipTo')
*-- Fill Ship to array with its data. [begin]

*-- Raise flage to be initialized evry new page[Start]
llEndGroup = .F.                        
GO RECNO('PosLn') IN PosLn    && Refresh relation with PO Fabric Line file.
RETURN ''
*-- end of lfHeadVar.
*!
*!*************************************************************
*! Name      : lfMPOEnd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/06/1998
*! Purpose   : Know the record number of line number at which 
*!           : we print Material PO Notepad, and this is because 
*!           : we print memo fields from withen detail band. 
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfMPOEnd
*-- if called from its OG
IF !llExternal
  PRIVATE lcSelected
  lcSelected = ALIAS()           && Save Current Alias.
  SELECT PosLn_A
  SET FILTER TO cBusDocu+PO  = PosLn.cBusDocu + PosLn.po AND TRANCD = '1'
  GO BOTTOM
  lnMPOEnd = RECNO('PosLn_A')
  SET FILTER TO
  SELECT (lcSelected)            && Restore Alias.
ENDIF  
llEndGroup  = .F.  && Initially Asure that we does not reach end of outer group.RETURN ''
RETURN ''
*-- end of lfMPOEnd.
*!
*!*************************************************************
*! Name      : lfFabEnd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/06/1998
*! Purpose   : Know the record number of line number at which 
*!           : we print Item Notepad, and this is because 
*!           : we print memo fields from withen detail band. 
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfFabEnd
*-- if called from its OG
IF !llExternal
  PRIVATE lcSelected
  lcSelected = ALIAS()           && Save Current Alias.
  SELECT PosLn_A
  SET FILTER TO cBusDocu+Po+SUBSTR(Style,1,lnMajorLen)= PosLn.cBusDocu + PosLn.Po + substr(PosLn.Style,1,lnMajorLen) AND TRANCD = '1'
  GO BOTTOM
  lnFabEnd = RECNO('PosLn_A')
  SET FILTER TO
  SELECT (lcSelected)            && Restore Alias.
ENDIF  
RETURN ''
*-- end of lfFabEnd.
*!
*!*************************************************************
*! Name      : lfGetNotes
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/06/1998
*! Purpose   : Fill variables for Notepad printing with its 
*!           : corressponding data. 
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfGetNotes

*-- B129381,1 HFK [Start]
loCodes.Seek('N' + SUBSTR(PosLn.STYLE,lnMajorLen+2) + "N" + "COLOR")
loItem.Seek('0002'+Posln.Style)
*-- B129381,1 HFK [End]

*-- if external call
IF llExternal
  RETURN ''
ENDIF
STORE '' TO lcTitle,lcChTitle,lcNotes
*-- if you are at First position in dummy file [See notes in program header]
IF &lcNoteLns..cRecord = 'N1'
  *-- if user want to print Item notepad and you find notes in notepad file.
  IF llRpPrtMn AND (RECNO('PosLn') = lnFabEnd) AND !EMPTY(ALLTRIM(NOTEPAD.MNOTES))
    lcTitle = LANG_MAMATP_Item + SUBSTR(PosLn.Style,1,lnMajorLen) + LANG_MAMATP_NotePad

    lcCHTitle = LANG_MAMATP_Item + ' 条款' + SUBSTR(PosLn.Style,1,lnMajorLen) + LANG_MAMATP_NotePad + ' 备注'

    lcNotes = ALLTRIM(NOTEPAD.MNOTES)
  ENDIF  && end if user want to print Item notepad.
ELSE  && else you are at second position in dummy file.
  *-- if user want to print Material PO notepad and you find notes in notepad file.
  IF llRpPrtPn AND (RECNO('PosLn') = lnMPOEnd) AND !EMPTY(ALLTRIM(NOTEPAD_A.MNOTES))
    lcTitle =   IIF(PosHdrM.cBusDocu = 'P',LANG_MAMATP_PurOrd,;
                IIF(PosHdrM.cBusDocu = 'R',LANG_MAMATP_Return,LANG_MAMATP_Contr)) + LANG_MAMATP_Notes1

    lcCHTitle = IIF(PosHdrM.cBusDocu = 'P',LANG_MAMATP_PurOrd + ' 定购单',;
                IIF(PosHdrM.cBusDocu = 'R',LANG_MAMATP_Return + ' 回单',LANG_MAMATP_Contr + ' 合同')) + LANG_MAMATP_Notes1 + '备注'

    lcNotes = ALLTRIM(NOTEPAD_A.MNOTES)
  ENDIF   && end if user want to print Material PO notepad.
ENDIF     && end if you are at First position in dummy file.
RETURN ''
*-- end of lfGetNotes
*!
*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/04/1998
*! Purpose   : Rise end group flag which control page footer data.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfEndGroup
*-- Set this variable .T. to don't print the word "CONTINUED"
*-- and then print Totals.
llEndGroup = .T.
RETURN ''
*-- end of lfEndGroup.
*-- 3) End of Functions called from report form (MAMATPOA.FRX).
*------------------------- End -------------------------*
*------------------- Functions section -----------------*
*!
*!*************************************************************
*! Name      : lfGetDisc
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/04/1998
*! Purpose   : Rise end group flag which control page footer data.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
*- B127974,1 MMT [Start]
FUNCTION lfGetDisc

lnalias =SELECT(0)
llLine = .F.
IF poshdra.lmultiware
  llLine = .T.
ELSE
  SELECT distinct posln.shipVia FROM posln WHERE po=PosHdrA.po INTO CURSOR lcShipRec
  SELECT lcShipRec
  IF RECCOUNT() > 1
    llLine = .T.
  ELSE
    llLine = .F.
  ENDIF
ENDIF
SELECT (lnAlias)
RETURN IIF(poshdra.lmultiware OR llLine,'At Line Level',lcShipVia)
*- B127974,1 MMT [End]
*!
*!*************************************************************
*! Name      : lfGetItmDsc
*! Developer : Heba Fathi (HFK)
*! Date      : 08/15/2005
*! Purpose   : Get Material Description
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*!
FUNCTION lfGetItmDsc
*-- B129381,1 HFK [Start]
loCodes.Seek('N' + SUBSTR(PosLn.STYLE,lnMajorLen+2) + "N" + "COLOR")
loItem.Seek('0002'+Posln.Style)
*-- B129381,1 HFK [End]

*!*************************************************************
*! Name      : lfGetItmDsc
*! Developer : Tarek Mohammed Ibrahim 
*! Date      : 03/02/2011
*! Purpose   : Get Status Description
*!*************************************************************
*B609515,1 TMI 02/05/2011 
FUNCTION lfGtStatusDesc
LPARAMETERS lcStatus
LOCAL lcRetSts 
lcRetSts = ""

DO CASE
CASE lcStatus = 'O'
  lcRetSts ="[ Open ]"
CASE lcStatus = 'C'
  lcRetSts ="[ Complete ]"
CASE lcStatus = 'A'
  lcRetSts ="[ Actualized ]"
CASE lcStatus = 'X'
  lcRetSts ="[ Cancel ]"
CASE lcStatus = 'S'
  lcRetSts ="[ Close ]"
CASE lcStatus = 'H'
  lcRetSts ="[ Hold ]"
CASE lcStatus = 'B'
  lcRetSts ="[ Bid ]"
ENDCASE
RETURN lcRetSts