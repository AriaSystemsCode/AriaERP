*:***************************************************************************
*: Program file  : MaMatP
*: Program desc. : Material Purchase Order
*: For Report    : MAMATPA.FRX
*: System        : Aria Advantage Series.
*: Module        : Material (MA)
*: Developer     : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe ,
*:               : gfRltFld, gfCodDes,gfADel , gfGetMemVar,gfOptMsg,gfBrows.
*:               : lfAdrShift,lfSolSpAdr,lfHeadVar,lfGetNotes,
*:               : lfNoteHead,lfNoteData,lfEndGroup,lfFormName,
*:               : lfvOptMsg,lfwOldVal,lfvOrder,lfClearRep.
*:***************************************************************************
*: Passed Parameters  : Work File if print material requiment report.
*:***************************************************************************
*: Notes         : It's so hard for all of us when we print more than one notepad
*:               : from master file, The following is its steps respectively
*:               : 1- Make Temp. Dummy file has two records have indexed 
*:               :    data to relate it with master loop file and to print
*:               :    notes, for example in this report 
*:               :    'N1'-> Print Item Notepad,'N2'-> Print PO Notepad.
*:               : 2- set skip to this file and coditions use it to print.
*:               : 3- In .Frx do the following...
*:               :    - Call functions from group headers to know in which
*:               :      record you print notepads, for example in this report
*:               :      lfMPOEnd and lfFabEnd Functions. 
*:               :    - Call function from detail band to notepad data like lfGetNotes
*:               :      in this report. I respect that any one may ask me why 
*:               :      I call this function and don't print directely from 
*:               :      the field, I can say this is because we want to print 
*:               :      Box around the notepad, thus you can see that my code
*:               :      clear both lcTitle and lcNotes every time we does not
*:               :      have notepad data.
*:               :   - Print when condition for Box around notepad is under 
*:               :     this expression !EMPTY(lcTitle).
*:***************************************************************************
*: Example : DO MaMatPo
*:***************************************************************************
*: This Program is due to E300899,1 ...
*B602204,1 MAB 11/16/1998 Print company logo under its flag condition...
*E300847,4 MAB 12/07/1998 Adjust report to called from another program (Requirement Report)
*E300871,6 SAMEH 02/15/99 Use gfDispRe if the selected form is an FRX,
*E300871,6 				  otherwise, run the report program of the form.
*E300847,4 
*B603110,1 Re-establish the relation BETWEEN pofln and codes files as Comp. name removed from codes file
*B602913,1 HDM 08/18/1999 Print Costing Categories should default to 'Yes'
*                         Transaction # => in list => must display all PO. statues as 
*                         canceled + open . it displays only the open PO.

*-- Add Parameter to this program because material requirment report call this prog
*-- Note that the passed parameter is name of temp. file have orders to print
*E301077,72 MAB 03/15/1999 Open Fabric file, Close it when exit.
*C101484,1 BASSEM 05/18/99 Call optional program for forms
*B802883,1 BWA 12/25/99 Fix the bug of change the transaction option to inrange and 
*                       and fix the vendor option by get 2 records in the <syrepuvr.dbf>
*B603622,1 ADEL 16/05/2000 When browsing Tranaction # we get the vendor name the same for all POs
*B603622,1                 regardless of the Vendor code. So fix this bug by calling lfsrvTrans()
*B603622,1                 in this program .This fixing done in the "Transaction #" line in OG (syrepuvr).
*B603792,1 BWA 08/03/2000 Fix the bug of the inlist browse for the transactions doesn't show in numeric sequence[SYREPUVR]
*B803321,1 ANN 09/03/2000 Fix The Bug Of while printing the Ship To muts be ship from when print Return M PO (MAMATPA.FRX)
*B604655,1 WAB 07/11/2001 Fix the bug of print the logo only the frsit time your run the report
*B604756,1 MHM 08/23/2001 Fix the bug of print the the footer in wrong way 
*B605044,1 ABD 10/30/2001 Fix problem that the Purchase Order program didn't
*B605044,1 ABD            Print the return Po and Contract.
*B605498,1 RAE 02/18/2002 Print width from POFLN in FRX
*B605935,1 AMH 06/10/2002 Fix the bug of printing MPO from MPO screen without notepad.
*B122787,1 NNA 05/27/2004 Fix bug that the report not printing the Supplier item and Color Code
*B122787,1 NNA            that by add Fields for these in the FRX.
*B123114,1 BWA 06/03/2004 Fix the bug of the printing MPO with status "L" Fix in the sydreprt.
*E127808,1 EIH 06/02/2005 In case of england print "Tax Description - Tax Code" in right hand side of
*E127808,1 EIH            Company address6.
*B129606,1 EIH 09/29/2005 Fix bug that when calling from another program's we declare llRpVenRef.
*B131582,1 AHU 04/06/2006 adding the fax#  and adding the fax# text and edit box in the mamatpa.frx 
*B608069,1 NNA 05/02/2007 Fix bug that a label called “VAT REG NO” be printed even if the company is not England 
*B608069,1 NNA            this label must be printed only with England Company
*:***************************************************************************
PARAMETERS lcFileName

*B605935,1 AMH Check if printing from MPO screen [Start]
IF TYPE('lcNewMPos') = 'C' .AND. USED(lcNewMPos)
  lcRpExp  = lcRpExp + [ AND SEEK(POMat,lcNewMPos)]
ENDIF
*B605935,1 AMH [End]

*E301077,72 Open fabric file [Begin
IF !USED('FABRIC')
  USE (gcDataDir+'FABRIC') ORDER TAG FABRIC IN 0 SHARED
ENDIF
*E301077,72 Open fabric file [End
lcTime     = TIME()                     && Variable to hold the Time
lnFabEnd   = 0                          && Record No. Of first record in fabric group.
lnMPOEnd   = 0                          && Record No. Of last record  in PO#    group.
lcDivLName = ''                         && Variable to hold division long name.

*E300871,6 Call lfIsApparel to find out if the form is an FRX or an @... SAY
*E300871,6 report. If it is an @ SAY, get the fullpath of the program 
*E300871,6 (by calling lfIsApparl())
lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)
*llPrntComp = TYPE('llPrntComp') = 'L' .AND. llPrntComp 
*E300871,6 end

*---SSH
llNoRec = .F.
*---SSH


*E127808,1 EIH 06/02/2005 Check if company is england or not and set lcTaxDeCod with
*E127808,1 EIH            New value [Begin].  
STORE SPACE(0) TO lcTaxDeCod

*B608069,1 NNA 05/02/2007 (Begin) Initial a Variable called llIsEnglnd to be true if the company country is England
llIsEnglnd = .F.
*IF gcContCode = 'ENG'
IF UPPER(ALLTRIM(gcContCode)) = 'ENG'
llIsEnglnd = .T.
*B608069,1 NNA (End)

  lcTaxDeCod = ALLTRIM(gfGetMemVar('M_TAX_REFE'))
ENDIF
*E127808,1 EIH [End].






*E300847,4 
llExternal = .F.                        && .T. if called from another program.

*E300847,4 *-- if called from another program [Begin]
IF TYPE('lcFileName') = 'C'
  *-- Define variables that defined in OG for this report [begin]
  STORE ' ' TO lcCompName,lcRpMsg1,lcRpMsg2,lcRpMsg3,lcTerms,lcDivDesc,lcShipVia
  
  
  *B129606,1 EIH 09/29/2005 Fix bug that when calling from another program's we declare llRpVenRef [Begin].
  *STORE .F. TO llRpPrtPn,llRpPrtMn,llRpFCurr
  STORE .F. TO llRpPrtPn,llRpPrtMn,llRpFCurr,llRpVenRef
  *B129606,1 EIH 09/29/2005 [End].
  
  STORE .T. TO llExternal,llRpCostCt
  *-- Define variables that defined in OG for this report [end  ]

  *-- open files used by this report [begin]
  llCust    = gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')
  llObjects = gfOpenFile(gcDataDir+'OBJECTS','Objectid','SH')
  llObjLnk  = gfOpenFile(gcDataDir+'OBJLINK','Objlnkty','SH')
  llCurrFile= gfOpenFile(gcSysHome+'SYCCURR','Ccurrcode','SH')
  llCodes   = gfOpenFile(gcDataDir+'CODES','CODES','SH')
  *-- open files used by this report [end  ]
ENDIF
*E300847,4 *-- if called from another program [End  ]

*B602204 Add flag to company logo
llHaveLogo = .F.

*-- lcMerch   : Variable hold Currency symbol + total Merchadise amount.
*-- lcFreight : Variable hold Currency symbol + Freight.
*-- lcTax     : Variable hold Currency symbol + Tax.
*-- lcQuota   : Variable hold Currency symbol + Quota.
STORE '' TO lcMerch,lcFreight,lcTax,lcQuota

*-- Print Note Variables [begin]
lcTitle    = ''          && Hold Title of Notepad. 
lcNotes    = ''          && Hold Notepad.
*-- Print Note Variables [end]

llEndGroup = .F.                        && Flag to know if we are at the end of the Group
*B604655,1 WAB (Start) - check if the compny have the logo
llHaveLogo = SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')
*B604655,1 WAB (End) 
*-- if it's first time you run option Grid, i.e: you have unknown variables.
IF EMPTY(lcCompName)

  *-- if this company have a logo, point to logo record, 
  *-- else seek put pointer at EOF.
  
  *B602204 Add Flag .T. if company have logo. 
  *B604655,1 WAB (Start) - commit this line and remove before check if the frist time to run the option grid
  *llHaveLogo = SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')
  *B604655,1 WAB (End)
  DECLARE laCompAdd[6] , laVendor[6] , laShipTo[6] , laDivLName[1,2]
  laCompAdd = ''                   && Array to hold the Company address
  laVendor  = ''                   && Array to hold vendor address
  laShipTo  = ''                   && Array to hold the Ship To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = 'lcDivLName'

  
    
  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcCompName = cCom_Name             && Company Name.
  lcCompPhon = cCom_Phon             && Company Phone.
  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

  *B131582,1  AHU 04/06/2006 adding the fax# [Start]
  lcCompFax = cCom_fax             && Fax number.
  *B131582,1   [End]


  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  laCompAdd[6] = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
      
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  *-- Get company Address [end].

  *-- lcNoteLns : Name of Temp. Dummy Loop File which is used to 
  *--           : print both Fabric notepad and PO notepad from notepad file.
  *--           : note that this name and temp. file is created 
  *--           : one for every optional grid seasson run.
  lcNoteLns = gfTempName()

  *-- Create dummy loop file, and fill it with two records have [begin]
  *-- values 'N1' and 'N2' respectivly
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  *--HDM 05/31/1999 [Start] Define variable to hold Cursor Index Full Path
  *INDEX ON cRecord TAG (lcCPath + lcNoteLns)
  *HAYTHAR
  ZAP
  
  lcFullPath = SET('FULLPATH')
  SET FULLPATH ON
  lcCPath = DBF(lcNoteLns)
  lcCPath = STRTRAN(lcCPath,".TMP",".CDX")
  INDEX ON cRecord TAG lcNoteLns OF (lcCPath)
  SET FULLPATH &lcFullPath
  *--HDM 05/31/1999 [End]  
  FOR lnI = 1 TO 2
    APPEND BLANK
    REPLACE cRecord WITH "N"+STR(lnI,1)
  ENDFOR
  *-- Create dummy loop file [end]
  
ENDIF  && end if it's first time you run option Grid.

lcSkipExpr = [POFLN]    && Skip Expression.
GO TOP IN (lcNoteLns)      && Current refrence is 'N1'

*-- Set relation between used files [Begin]
SELECT PoFLn

*-- HDM B603110,1 [START] Re-establish the relation as gcAct_Comp removed from codes file
*SET RELATION TO gcAct_Comp + PADR(color,6) + "N" + "COLOR" INTO Codes
SET RELATION TO 'N' + PADR(color,6) + "N" + "COLOR" INTO Codes
*-- HDM [End]

SET RELATION TO Fabric + Color INTO Fabric ADDITIVE

*E300847,4 *-- if called from its OG.
IF !llExternal
  SET RELATION TO 'G'+FABRIC INTO NOTEPAD ADDITIVE
ENDIF  

*-- if you print PO Notepad set relation to dummy file.
IF llRpPrtPn
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  lcSkipExpr = [POFLN,&lcNoteLns]
ENDIF  && end if you print PO Notepad set relation to dummy file.

SELECT PoFhdr
SET RELATION TO vendor INTO Apvendor
SET RELATION TO cWareCode INTO WareHous ADDITIVE
SET RELATION TO cMatType + POMat INTO POFLN ADDITIVE

*E300847,4 *-- if called from its OG.
IF !llExternal
  SET RELATION TO 'M'+ CMATTYPE + POMAT INTO NOTEPAD_A ADDITIVE
ENDIF
  
*-- Set relation between used files [End]

SET SKIP TO &lcSkipExpr

*E300847,4 *-- if called from another program

IF llExternal

  lcRpExp  = [cMatType = 'P' AND SEEK(POMat,lcFileName)]  && Make report expression.
 
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
  SELECT POFHDR
  SET RELATION TO
  
  SELECT POFLN
  SET RELATION TO

ELSE  && else called from its OG

  *-- lcRepExpr : Expression to loop around wanted records only.
  lcRepExpr = [IIF(llRpPrtPn,IIF(&lcNoteLns..cRecord = 'N2',RECNO('POFLN') = lnMPOEnd,.T.),.T.)] 
  
  *B605044,1 ABD - We define this variable in the Material 
  *B605044,1 ABD - Purchase Order program to fix the problem that the program didn't print 
  *B605044,1 ABD - the contract and return PO. [Begin]
  IF TYPE('lcTypPrint ') $ 'UL'
    lcRpExp   = [cMatType + POMat = IIF(lcRpForm = 'A','',lcRpForm) AND ] +;
                IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr) 
  ELSE
    lcRpExp   = [cMatType + POMat = lctypPrint .AND. ] +;
                IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr) 
  ENDIF
  *B605044,1 ABD - [End]
  *Renee Call lfOptProg() to run an optional program

  =lfOptProg()
  *Renee end
  *E300871,6 Call gfDispRe if the report is an FRX
  *DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp  && Call report form.
  IF llIsAparel
    DO EVAL('lcPrgName')
    IF !llNoRec
      DO ENDREPORT
    ENDIF
  ELSE
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp  && Call report form.  ENDIF
  ENDIF      
  *E300871,6 end
ENDIF
*E300847,4
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
*! Called from : MAMATPO.PRG 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdrShift()
*!*************************************************************
FUNCTION lfAdrShift
PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*-- 1) End of Functions called from report code.
*----

*----
*-- 2) Begin of Functions called from Option Grid.
*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Function to get Optional Message from the User
*!             [Validation function for the Push button Optional Message]
*!*************************************************************
*! Called from : Option Grid    [Optional Message option]
*!*************************************************************
*! Calls       : gfOptMsg()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOptMsg()
*!*************************************************************
FUNCTION lfvOptMsg
PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen

laOptMsg[1,1] = 'lcRpMsg1'        && 1st. line Variable
laOptMsg[2,1] = 'lcRpMsg2'        && 2nd. line Variable
laOptMsg[3,1] = 'lcRpMsg3'        && 3rd. line Variable
laOptMsg[1,2] = 75                && Line length

= gfOptMsg('laOptMsg')            && Call Function to write optional message.
*-- end of lfvOptMsg.

*!*************************************************************
*! Name      : lfvTrnTyp
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Unselect transactions due to changing PO Type .
*!*************************************************************
*! Called from : Option Grid    [Print Option]
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvTrnTyp()
*!*************************************************************
FUNCTION lfvTrnTyp
llClearit = (lcRpForm # 'A' )   && Unselect selected transactions.
lcRpMaKey = IIF(lcRpForm='A','',lcRpForm)
*-- end of lfvTrnTyp.

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : Mohamed Badran (MAB)
*! Date      : 07/05/98
*! Purpose   : Set Files used by In Range operator, and then rest it.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid [Transaction # Option]
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfsrvTrans()
*!*************************************************************
*! Note      : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO POFHDR IN POFHDR    
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POFHDR
    SET RELATION TO VENDOR INTO APVENDOR
  CASE lcParm = 'R'  && Reset code
    SELECT POFHDR
    SET RELATION TO
    SET ORDER TO 0 IN POFHDR    
    SET ORDER TO 0 IN APVENDOR
    llClearit = .F.
ENDCASE
*-- end of lfsrvTrans.

*!*************************************************************
*! Name      : lfvVend
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Valid function of the Vendor field.
*!*************************************************************
*! Called from : Option Grid [Vendor field option]
*!*************************************************************
*! Calls       : gfApVnBrow().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvVend()
*!*************************************************************
FUNCTION lfvVend
PRIVATE lcVar, lcObj
lcVar = SYS(18)                      && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(EVALUATE(SYS(18)))   && Varible to hold the current field value
SELECT APVENDOR
SET ORDER TO TAG VenCode 
*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcObj , 'APVENDOR'))
  =gfApVnBrow(@lcObj)
  IF !EMPTY(lcObj)
    &lcVar = lcObj      && Update the field
  ELSE
    &lcVar = laOldVal
  ENDIF
ENDIF
*-- end of lfvVend.

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Generic code to get the old value of the field.
*!*************************************************************
*! Called from : Option Grid (Option that we respect to use its old value)
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/01/1998
*! Purpose   : Function that we call when Close the option grid.
*!           : to clear our variables.
*!*************************************************************
*! Called from : [Option Grid] < Close >
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*E301077,72 Close Fabric file.
IF (ASCAN(laSelFile,'FABRIC') = 0) AND USED('FABRIC')
  USE IN FABRIC
ENDIF

IF USED(lcNoteLns)
 USE IN (lcNoteLns)  && Clear memory from temp. dummy cursor.
ENDIF
*-- end of lfClearRep.

*-- 2) End of Functions called from Option Grid.
*----

*----
*-- 3) Begin of Functions called from report form (MAMATPA.FRX).
*!*************************************************************
*! Name      : lfHeadVar
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/08/1998
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : Page header band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfRltFld,gfCodDes,gfGetAdr,lfAdrShift
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfHeadVar()
*!*************************************************************
FUNCTION lfHeadVar
PRIVATE lcPriceSmb , lcDutySmb

*-- Get Currency symbol for Price, and Duty [begin]
lcPriceSmb  = PADL(ALLTRIM(IIF(llRpFCurr,IIF(SEEK(POFHDR.cPriceCur,'SYCCURR'),SycCurr.cCurrSmbl,''),IIF(SEEK(gcBaseCurr,'SYCCURR'),SycCurr.cCurrSmbl,''))),3)
lcDutySmb   = PADL(ALLTRIM(IIF(llRpFCurr,IIF(SEEK(POFHDR.cDutyCur ,'SYCCURR'),SycCurr.cCurrSmbl,''),IIF(SEEK(gcBaseCurr,'SYCCURR'),SycCurr.cCurrSmbl,''))),3)
*-- Get Currency symbol for Price, and Duty [end]

lcMerch     = lcPriceSmb + ;
              ALLTRIM(STR(IIF(llRpFCurr,POFHDR.nCost1,POFHDR.neCost1),13,2))
lcFreight   = lcDutySmb  + ;
              ALLTRIM(STR(IIF(llRpFCurr,POFHDR.nCost2,POFHDR.neCost2),13,2))
lcTax       = lcDutySmb  + ;
              ALLTRIM(STR(IIF(llRpFCurr,POFHDR.nCost3,POFHDR.neCost3),13,2))
lcQuota     = lcDutySmb  + ;
              ALLTRIM(STR(IIF(llRpFCurr,POFHDR.nCost4,POFHDR.neCost4),13,2))

= gfRltFld(POFHDR.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcDivDesc   = gfCodDes(POFHDR.CDIVISION, 'CDIVISION')     && Division description.
lcShipVia   = gfCodDes(POFHDR.ShipVia , 'SHIPVIA')        && Ship Via description.
lcTerms     = gfCodDes(POFHDR.CTermCode   , 'CTERMCODE')  && Terms description.

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
laShipTo[2] = POFHDR.COutAddr1
laShipTo[3] = POFHDR.COutAddr2
laShipTo[4] = POFHDR.COutAddr3
laShipTo[5] = POFHDR.COutAddr4
laShipTo[6] = POFHDR.COutAddr5
= lfAdrShift('laShipTo')
*-- Fill Ship to array with its data. [begin]

*B604756,1 MHM 08/23/2001 Raise flage to be initialized evry new page[Start]
llEndGroup = .F.                        
*B604756,1 MHM [End]

GO RECNO('POFLN') IN POFLN    && Refresh relation with PO Fabric Line file.
RETURN ''
*-- end of lfHeadVar.

*!*************************************************************
*! Name      : lfMPOEnd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/06/1998
*! Purpose   : Know the record number of line number at which 
*!           : we print Material PO Notepad, and this is because 
*!           : we print memo fields from withen detail band. 
*!*************************************************************
*! Called from : POMAT Group header band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfMPOEnd()
*!*************************************************************
FUNCTION lfMPOEnd
*-- if called from its OG
IF !llExternal
  PRIVATE lcSelected
  lcSelected = ALIAS()           && Save Current Alias.
  SELECT POFLN_A
  SET FILTER TO cmattype+pomat+fabric+color+trancd+STR(RECNO(),7) = ;
                PoFLn.cmattype + PoFLn.pomat  AND TRANCD = '1'
  GO BOTTOM
  lnMPOEnd = RECNO('POFLN_A')
  SET FILTER TO
  SELECT (lcSelected)            && Restore Alias.
ENDIF  
llEndGroup  = .F.  && Initially Asure that we does not reach end of outer group.RETURN ''
RETURN ''
*-- end of lfMPOEnd.

*!*************************************************************
*! Name      : lfFabEnd
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/06/1998
*! Purpose   : Know the record number of line number at which 
*!           : we print Item Notepad, and this is because 
*!           : we print memo fields from withen detail band. 
*!*************************************************************
*! Called from : POFLN.FABRIC Group header band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFabEnd()
*!*************************************************************
FUNCTION lfFabEnd
*-- if called from its OG
IF !llExternal

  PRIVATE lcSelected
  lcSelected = ALIAS()           && Save Current Alias.

  SELECT POFLN_A
  SET FILTER TO cmattype+pomat+fabric+color+trancd+STR(RECNO(),7) = ;
                PoFLn.cmattype + PoFLn.pomat + PoFLn.fabric AND TRANCD = '1'

  GO BOTTOM
  lnFabEnd = RECNO('POFLN_A')
  SET FILTER TO
  SELECT (lcSelected)            && Restore Alias.

ENDIF  
RETURN ''
*-- end of lfFabEnd.

*!*************************************************************
*! Name      : lfGetNotes
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/06/1998
*! Purpose   : Fill variables for Notepad printing with its 
*!           : corressponding data. 
*!*************************************************************
*! Called from : Detail band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetNotes()
*!*************************************************************
FUNCTION lfGetNotes

*E300847,4 *-- if external call
IF llExternal
  RETURN ''
ENDIF

STORE '' TO lcTitle,lcNotes

*-- if you are at First position in dummy file [See notes in program header]
IF &lcNoteLns..cRecord = 'N1'

  *-- if user want to print Item notepad and you find notes in notepad file.
  IF llRpPrtMn AND (RECNO('POFLN') = lnFabEnd) AND ;
     !EMPTY(ALLTRIM(NOTEPAD.MNOTES))
            
    lcTitle = 'Item  ' + POFLN.Fabric + ' Notepad'
    lcNotes = ALLTRIM(NOTEPAD.MNOTES)
  ENDIF  && end if user want to print Item notepad.

ELSE  && else you are at second position in dummy file.

  *-- if user want to print Material PO notepad and you find notes in notepad file.
  IF llRpPrtPn AND (RECNO('POFLN') = lnMPOEnd) AND ;
     !EMPTY(ALLTRIM(NOTEPAD_A.MNOTES))

    lcTitle = IIF(POFHDR.CMATTYPE = 'P','Purchase Order ',;
              IIF(POFHDR.CMATTYPE = 'R','Return Purchase Order ','Contract ')) +;
              'Notepad'
    lcNotes = ALLTRIM(NOTEPAD_A.MNOTES)
  ENDIF   && end if user want to print Material PO notepad.

ENDIF     && end if you are at First position in dummy file.
RETURN ''
*-- end of lfGetNotes

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 07/04/1998
*! Purpose   : Rise end group flag which control page footer data.
*!*************************************************************
*! Called from : POMAT Group fotter band.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
*-- Set this variable .T. to don't print the word "CONTINUED"
*-- and then print Totals.
llEndGroup = .T.
RETURN ''
*-- end of lfEndGroup.

*-- 3) End of Functions called from report form (MAMATPOA.FRX).
*----

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : Mohamed Shokry (MHM)
*! Date      : 08/29/2001
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
*B604756,1
FUNCTION lfwOldVal
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvPo
*! Developer : Mohamed Shokry (MHM)
*! Date      : 08/29/2001
*! Purpose   : Validation function for the PO field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPo()
*!*************************************************************
*B604756,1
FUNCTION lfvPo
PRIVATE lcVar , lcObj , laTemp
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

*IF Statment to check if we are going to Browse

IF !EMPTY(lcObj) AND ('?' $ lcObj OR !IIF(lcRpForm = 'A'  ,;
  SEEK('P'+lcObj , 'POFHDR') OR SEEK('R'+lcObj , 'POFHDR') ,;
  SEEK(lcRpForm+lcObj , 'POFHDR')))
  SELECT POFHDR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "cmattype=IIF(lcRpForm='A',cmattype,'')"+;
               "                    :R :H= IIF(lcRpForm='A','Type',''),"+;
               "POMAT               :R :H= 'P\O #', "+;
               "STATUS              :R :H= 'S',"+;
               "VENDOR              :R :H= 'Vendor',"+;
               "APVENDOR.cVenComp   :R :H='Name',"+;
               "COMPLETE            :R :H='Complete',"+;
               "TOTQTY = NPO_OPEN+ NFBRECEIVE :R :H = 'Tot. Qty',"+;
               "POTOTAL             :R :H='Amount',"+;
               "NFBRECEIVE          :R :H='Receive'," +;
               "NPO_OPEN             :R :H='Open'"


  lcFile_Ttl = IIF(lcRpForm = 'A',"PO,Return PO and Contracts",IIF(lcRpForm ='P',"Purchase Order",;
                   IIF(lcRpForm ='R',"Return Purchase Order ","Contracts...")))

  lcBrowCond =  [FOR (IIF(lcRpForm = 'P' , POFHDR.cmattype = "P", IIF(lcRpForm = 'R' ,POFHDR.cmattype = "R",IIF(lcRpForm = 'C', POFHDR.cmattype = "C" , POFHDR.cmattype $ "PRC"))))]

  = gfBrows(lcBrowCond,'POMAT','laTemp')  
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvOrder.


*------------------------- End -------------------------*
*------------------- Functions section -----------------*
