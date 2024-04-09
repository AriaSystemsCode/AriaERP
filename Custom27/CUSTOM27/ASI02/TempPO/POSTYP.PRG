*:***************************************************************************
*: Program file  : POSTYP
*: Program desc. : Print PO, Contract and Return PO
*: For Report    : POSTYPA.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Purchase Order (PO)
*: Developer     : AHMED MOHAMMED MOHAMMED
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfwOldVal, lfvPO, lfvVend, lfGetLogo,
*:                 lfShiftArr
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO POSTYPO
*:***************************************************************************
*! Modifications :
*!E301062,1 AMM 11/11/98 Add a filter on the warehouse in the option grid.
*!E301095,1 AMM 12/14/98 Add a filter on the source warehouse in case of 
*!E301095,1 AMM          Inter location PO.
*!C101389,1 ASH 12/28/98 Call lfIsApparl to run FRX/..SAY forms
*!E301129,1 AMM 02/02/99 Add an option to print price after discount in case 
*!E301129,1 AMM          of inter location PO
*!E301111,1 AMM 02/11/99 Add the Vendor fax to report
*!B802017,1 AMM 03/10/99 Fix the bug of wrong printing of cost with base 
*!B802017,1 AMM          or foreign currency.
*!E301150,1 AMM 03/10/99 Reject style with no retail price in inter location PO
*!B602763,1 AMM 04/11/99 Change the title of print COST/PRICE.
*!B602800,1 AMM 04/09/99 1- Remove the option "ALL" from the "PRINT" selection
*!B602800,1 AMM          2- Clear selection in Tran# browse when changing the 
*!B602800,1 AMM          "PRINT" selection and make soft seek in the browse.
*!C200080,1 AMM 07/25/99 Consider the dye order case
*!B603088,1 WAB 08/04/99 print style PO With  @ say forms
*!B802514,1 AMM 09/06/1999 Fix the bug of not printing .FRX form with an optional program
*!C101527,1 ADEL 09/19/1999 Add possibility to erase all temp files created in any Optional program.
*!C101666,1 ADEL 09/22/1999 1) Enable or disable costing option in OG according to the user's  previlige
*!C101666,1                 2) Add a record in SYCTRIGG for KAKTUS and add GETINSULBL.PRG 
*!C101666,1                    in W:\ARIA27 directory to change the  "INSURANCE" header to "INDIA'S PO#"  in POSTYPA.FRX.
*!E301338,1 AMM 10/25/1999 Call PO bill of Material after printing the PO form
*!B802755,1 ABD 11/07/1999 Add Case Probability To Trans# Record In Field "Mbrwfltexp"
*!B802755,1                "P" For PO & "R" For Return PO & "C" For Contract &
*!B802755,1                "N" For Inter-Location PO & "D" For Dye Order 
*!B802893,1 RAMY 27/12/1999 Fix the bug of printing the notes when the PO has a canceled or 
*!B802893,1                 recived Qty
*!B603528,1 KHM 03/22/2000  Fix the bug of invalid path or file name when you
*!B603528,1                 run an @say form twice in the same session.
*:***************************************************************************
DECLARE laCompAdd[6,1], laDivLName[1,2], laVenAdr[1,5],laShpAdr[1,5]
STORE SPACE(0) TO laCompAdd, laShpAdr, lcVenName, lcDivDesc, lcTerms, ;
                  lcShpName, lcShipVia, lcStyTitle, lcDivLName, lcLogoPic, ;
                  lcTitle, lcNotes, lcCompPhon,laVenAdr
*C101527,1 (Begin) Declare an array which will hold the temp files names and directories.
*C101527,1 Note:  The first elememnt of the array holds the temp file or cursfor name.
*C101527,1        The seconf elememnt of the array holds the temp file or cursor directory.
DECLARE laOpTmpFls[1,2]
STORE " " TO laOpTmpFls
*C101527,1 (End)
*E301111,1 AMM Initialize variable to hold Fax Number
STORE SPACE(0) TO lcFax 
*E301111,1 AMM end
STORE .F. TO llEndGroup, llLogo, llPrintBox
lcNoteLns  = gfTempName()
llPrntBoth = llRpPrtSn AND llRpPrtPn     && Flag to know we print both Style and order notes.
lcSkipExpr = 'POSLN'                     && Skip expression
STORE .T. TO llPrtSn, llTitle                             && Variable to print style notes one time.
lnLstLn = 1
*E301129,1 AMM Initialize variables to get discount percentage
lnDisc  = 0
STORE {} TO ldStart, ldEnd
DIMENSION laDisc[3,2]
laDisc[1,1] = 'DISCPCNT'
laDisc[1,2] = "lnDisc"
laDisc[2,1] = 'START'
laDisc[2,2] = "ldStart"
laDisc[3,1] = 'DENDATE'
laDisc[3,2] = "ldEnd"
*E301129,1 AMM AMM end
* laCompAdd       Array to hold the Company addresses
* laDivLName      Array to get the Division long name
* laVenAdr        Array to hold the vendor addresses
* laShpAdr        Array to hold the ship to addresses

* lcCompName      Hold the company name
* lcCompPhon      Hold the Company Phone
* lcVenName       Hold vendor name
* lcLogoPic       Hold the temp name of the cursor created to hold the logo of the company
* lcNotes         Hold the Style or Transaction notepad
* lcDivDesc       Hold the division Describtion
* lcTerms         Hold the terms describtion
* lcShpName       Hold the Ship to Name.
* lcShipVia       Hold the ship via description
* lcStyTitle      Hold the Style title
* lcDivLName      Hold the division long name

* lnLstLn         Hold record number of last line in each PO

* llEndGroup      Used to check if we would print the 'CONTINUED' word.
* llLogo          Used to check if we are to printing the logo of company
* llPrintBox      Box around Notes.
* llPrtSn         Variable to print style notes one time.
* llTitle         Variable to print the style header

*************************************************************************
* Open objects and objlink tables again with these two temp. aliases.
TmpObjects = gfTempName()
TmpObjLink = gfTempName()
* If there is a company logo , create a cursor hold this logo
IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
  SEEK(OBJLINK.cObject_ID,'OBJECTS')
  = lfGetLogo()        && Function to Fill the temp cursor With company Logo.
ENDIF
lcStyTitle = gfItemMask('HI')
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSize  = LEN(lcMaj)                   && Length of the major
lcTime     = TIME()                       && Variable to hold the Time

*************************************************************************
* Open necessary tables and make necessary relations.
SELECT 0
USE (gcDataDir+'objects') AGAIN ALIAS &TmpObjects ORDER TAG objectid 
SELECT 0
USE (gcDataDir+'objlink') AGAIN ALIAS &TmpObjLink ORDER TAG Objlnkty 
SET RELATION TO cobject_id INTO &TMPObjects ADDITIVE

SELECT objlink
SET RELATION TO cobject_id INTO Objects ADDITIVE

SELECT POSLN
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO 'S'+SUBSTR(Posln.style,1,lnMajSize) INTO Objlink ADDITIVE
SET ORDER TO TAG POSLNW
*E301150,1 AMM Create temporary cursor to hold the rejected styles
IF lcRpPrice='P' .AND. INLIST(lcRpForm,'A','N')
  lcTmpRej = gfTempName()
  CREATE CURSOR (lcTmpRej) (PO C(6), STYLE C(19))
ENDIF
*E301150,1 AMM end
*E301338,1 AMM Collect data for the PO bill of material
IF llrPrtCs .AND. lcRpForm='P'
  STORE SPACE(0) TO lcCostItm, lcHead1, lcHead2,lcLotNo,lcStyMaj,lcPattrn,lcBOMTit
  lcMGroup = 'CUTTKT'
  lcInGroup = 'CUTTKT+TYP'
  lcBOMTit = 'Purchase Order#'
  lcCostF = gfTempName()
  IF !USED('Fabric')
    =gfOpenFile(gcDataDir+'FABRIC','FABRIC','SH')
  ENDIF
  IF !USED('CTKTBOM')
    =gfOpenFile(gcDataDir+'CTKTBOM','CTKTBOM','SH')
  ENDIF
  *-- Create array to hold the cost element titles.
  DIMENSION laCost[10,2]
  laCost[1,1]  = 'M_CMTYPE1 '
  laCost[2,1]  = 'M_CMTYPE2 '
  laCost[3,1]  = 'M_CMTYPE3 '
  laCost[4,1]  = 'M_CMTYPE4 '
  laCost[5,1]  = 'M_CMTYPE5 '
  laCost[6,1]  = 'M_CMSLBL1 '
  laCost[7,1]  = 'M_CMSLBL2 '
  laCost[8,1]  = 'M_CMSLBL3 '
  laCost[9,1]  = 'M_CMSLBL4 '
  laCost[10,1] = 'M_CMSLBL5 '

  =gfGetMemvar(@laCost,gcAct_Comp)

  *-- create the temporary file to print the cost sheet from.
  SELECT CTKTBOM
  COPY STRUCTURE TO (gcWorkDir+lcCostF)
  SELE 0
  USE (gcWorkDir+lcCostF) EXCL
  INDEX ON CUTTKT+Typ+cCatgTyp+Item+IClr TAG (lcCostF)
  SELECT poshdr
  SCAN FOR &lcRpExp 
    *-- If we will print cost sheet , insert cost sheet records in the temprorary file.
    IF llrPrtCs .AND. SEEK('I'+POSHDR.PO,'CTKTBOM')
      SELECT CTKTBOM
      SCAN WHILE cImTyp+CutTkt = 'I'+POSHDR.PO FOR cCatgTyp $ 'FTS'
        SCATTER MEMVAR
        INSERT INTO (lcCostF) FROM MEMVAR 
      ENDSCAN
    ENDIF
  ENDSCAN
ENDIF
*E301338,1 AMM end

SELECT poshdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
SET RELATION TO 'P'+POSHDR.PO INTO &TMPObjlink ADDITIVE
SET RELATION TO IIF(lcRpForm='A',POSHDR.cSTYType,lcRpForm)+PO INTO POSLN ADDITIVE
SET SKIP TO POSLN
*E301150,1 AMM Fill the cursor by style with no retail price
IF lcRpPrice='P' .AND. INLIST(lcRpForm,'A','N')
  SCAN FOR &lcRpExp .AND. POSHDR.cSTYType = 'N' .AND. STYLE.nSugRetPri = 0
    m.PO    = POSHDR.PO
    m.STYLE = POSLN.STYLE
    INSERT INTO (lcTmpRej) FROM MEMVAR
  ENDSCAN
  IF !EOF(lcTmpRej)
    lcBrFields = "PO :R :H='PO #'   ,STYLE :R :H='STYLE'"
    lcFile_Ttl = "Rejected Interlocation PO's "
    *E301150,1 AMM Message : One or more styles has no retail price, these styles 
    *E301150,1 AMM will be rejected. Do you want to view the rejected styles?
    IF gfModalGen("QRM34141B34001","Dialog") = 1
      SELECT (lcTmpRej)
      GO TOP
      =ARIABROW(.F.,lcFile_Ttl,.F.,.F.,.F.,.F.,.F.,"Fi\<nd;\<Order by;\<Descending;Fi\<lter;;\?\<Close",lcBrFields)
    ENDIF
    USE IN (lcTmpRej)
    RETURN
  ENDIF
  USE IN (lcTmpRej)
ENDIF
*E301150,1 AMM end

***********************************************************************
*E301150,1 AMM Open the file
*SELECT SYCCOMP
=gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
*E301150,1 AMM end
SEEK gcAct_Comp

lcCompName = cCom_Name
lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
lcPhonPict  = gfPhoneTem()          && Variable to hold the Company Phone Format
* Get the company addresses
laCompAdd[1]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
laCompAdd[2]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
laCompAdd[3]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
laCompAdd[4]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
laCompAdd[5]    = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
laCompAdd[6]    = 'Phone# : '+TRANSFORM(lcCompPhon , lcPhonPict)
* Pack the addresses array
DO lfShiftArr WITH laCompAdd

laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

IF !USED(lcNoteLns) AND llPrntBoth
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  INDEX ON cRecord TAG (lcNoteLns)
  FOR lnI = 1 TO 2
    APPEND BLANK
    REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
ENDIF  

IF llPrntBoth
  SELECT POSLN
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  lcSkipExpr = [POSLN,&lcNoteLns]
ENDIF

lcRepExpr = [IIF(llPrntBoth,IIF(&lcNoteLns..cRecord = 'N2',RECNO('POSLN') = lnLstLn,.T.),.T.)] 
lcRpExp   = IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr) 

SELECT POSHDR
*E301150,1 AMM Move to the beginning of the file
GO TOP
*E301150,1 AMM end
SET SKIP TO &lcSkipExpr

*C101389,1 Call any optional function 
=lfOptProg()
*C101389,1 Call lfIsApparl to run FRX/..SAY forms
*B802514,1 AMM Call .frx even if it has an optional program
*IF !lfIsApparl(@lcFormName) AND EMPTY(lcOptProg)

*B603528,1 KHM 03/22/2000  (Begin) Assigning the lcformname to lcPrgName
*B603528,1                 and passing lcPrgName to lfIsApparl function.
lcPrgName  = lcFormName
*IF !lfIsApparl(@lcFormName)
IF !lfIsApparl(@lcPrgName)
*B603528,1 KHM 03/22/2000  (End)
*B802514,1 AMM end
*C101389,1 end
*--Koko
IF TYPE('llFormEE') = 'U'
  DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
ENDIF
*!B603088,1 WAB at @say form do the prog. and call endreport function
*!B603088,1 WAB - START
ELSE

  *B603528,1 KHM 03/22/2000  (Begin) Passing lcPrgName instead of lcFormName.
  *DO EVAL('lcFormName')
  DO EVAL('lcPrgName')
  *B603528,1 KHM 03/22/2000 (End)
  
  IF !llNoRec			&& no record found
    DO ENDREPORT    
  ENDIF
*!B603088,1 WAB - END

*C101389,1 Add an ENDIF
ENDIF
*C101389,1 end

*E301338,1 AMM
*-- Print the cost sheet if required.
IF llrPrtCs  .AND. lcRpForm='P' .AND. !EOF(lcCostF)
  SELECT POSHDR
  SET RELATION TO
  SELECT (lcCostF)
  SET RELATION TO LEFT(item,7)+iclr INTO Fabric ADDITIVE
  SET RELATION TO 'P'+CutTkt INTO POSHDR ADDITIVE
  lcOldTmp = lcOGTmpForm
  lcOGTmpForm = ""
  lcOldPlatF  = lcOgPlatForm
  lcOgPlatForm = 'WINDOW'
  lcCSFrmN = 'MFPRCS'+ STRTRAN(lcFormName,'POSTYP')
  IF !FILE(gcRepHome+lcCSFrmN+'.FRX')
    lcCSFrmN = 'MFPRCSA'
  ENDIF
  DO gfDispRe WITH (lcCSFrmN)
  lcOGTmpForm  = lcOldTmp
  lcOgPlatForm = lcOldPlatF
ENDIF
*E301338,1 AMM end


*C101527,1 (Begin) Erase temp files created in any optional program.
IF !EMPTY(laOpTmpFls)
  FOR ArrElmnt = 1 TO ALEN(laOpTmpFls,1)
    IF USED(laOpTmpFls[ArrElmnt,1])
      USE IN laOpTmpFls[ArrElmnt,1]
      *--If it is a cursor, no directory exists.
      IF !EMPTY(laOpTmpFls[ArrElmnt,2])
        ERASE laOpTmpFls[ArrElmnt,2]+laOpTmpFls[ArrElmnt,1]+'.DBF'
        ERASE laOpTmpFls[ArrElmnt,2]+laOpTmpFls[ArrElmnt,1]+'.CDX'
      ENDIF
    ENDIF
  ENDFOR
ENDIF
*C101527,1 (End)

IF USED(TmpObjects)
  USE IN (TmpObjects)
ENDIF
IF USED(TmpObjLink)
  USE IN (TmpObjLink)
ENDIF

*!*************************************************************
*! Name      : lfGetCodes
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 03/25/1998
*! Purpose   : Get data to be printed on page header
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfRltFld(), gfCodDes(), gfGetAdr(), lfShiftArr()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetCodes()
*!*************************************************************
FUNCTION lfGetCodes
* Get the division long name.
*!C101389,1 ASH 12/28/98 (Begin) Hold the current Alias.
lcAlias = ALIAS()
*!C101389,1 ASH 12/28/98 (End)
=gfRltFld(POSHDR.cDivision , @laDivLName , 'CDIVISION')
llEndGroup  = .F.
lcDivDesc   = gfCodDes(POSHDR.cDIVISION, 'CDIVISION')
lcShipVia   = gfCodDes(POSHDR.ShipVia , 'SHIPVIA')
lcTerms     = gfCodDes(POSHDR.CTERMCODE, 'CTERMCODE')

IF POSHDR.cStyType # 'N'
  lcVenName   = APVENDOR.CVenComp
  *E301111,1 AMM Get vendor fax number
  lcFax       = APVENDOR.CFAXNO
  *E301111,1 AMM end
  * Get the vendor addresses
  laVenAdr[1] = gfGetAdr('APVENDOR' , '' , '' , '' , 1)
  laVenAdr[2] = gfGetAdr('APVENDOR' , '' , '' , '' , 2)
  laVenAdr[3] = gfGetAdr('APVENDOR' , '' , '' , '' , 3)
  laVenAdr[4] = gfGetAdr('APVENDOR' , '' , '' , '' , 4)
  laVenAdr[5] = gfGetAdr('APVENDOR' , '' , '' , '' , 5)
ELSE
    =SEEK(LEFT(PosHdr.Vendor,6),'WAREHOUS')
    lcVenName   = WAREHOUS.cDesc
    *E301111,1 AMM Get warehouse fax number
    lcFax       = WAREHOUS.cFAX
    *E301111,1 AMM end
    laVenAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
    laVenAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
    laVenAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
    laVenAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
    laVenAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
ENDIF
* pack the addresses array
DO lfShiftArr WITH laVenAdr
SELECT POSHDR
* Get ship to Name and addresses

IF !POSHDR.lmultiware
  * If single warehouse get it's addresses from the header file.
  lcShpName   = POSHDR.ShpName
  laShpAdr[1] = COutAddr1
  laShpAdr[2] = COutAddr2
  laShpAdr[3] = COutAddr3
  laShpAdr[4] = COutAddr4
  laShpAdr[5] = COutAddr5
ELSE
  * The ship to is on line level
  IF EMPTY(POSLN.account)
    * If account field is Empty , the ship to is to warehouse.
    =SEEK(posln.cwarecode,'WAREHOUS')
    lcShpName   = WAREHOUS.cDesc
    laShpAdr[1] = gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
    laShpAdr[2] = gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
    laShpAdr[3] = gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
    laShpAdr[4] = gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
    laShpAdr[5] = gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
  ELSE
    * The ship to is to account, so get its addresses from the customer file.
    =SEEK(IIF(EMPTY(POSLN.Store),'M'+POSLN.Account,;
                       'S'+POSLN.account + POSLN.Store),'CUSTOMER')
    lcShpName   = CUSTOMER.stname
    laShpAdr[1] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 1)
    laShpAdr[2] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 2)
    laShpAdr[3] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 3)
    laShpAdr[4] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 4)
    laShpAdr[5] = gfGetAdr('CUSTOMER' , '' , '' , '2' , 5)
  ENDIF
ENDIF
* Pack the addresses array
DO lfShiftArr WITH laShpAdr
*E301129,1 AMM Get the discount percentage
lnDisc = 0

IF !EMPTY(STYLE.cDiscCode)
  =gfRltFld(STYLE.cDiscCode,@laDisc,'CDISCCODE')
  IF (EMPTY(ldEnd) .AND. !EMPTY(ldStart) .AND. POSHDR.ENTERED < ldStart) .OR. ;
     (!EMPTY(ldEnd) .AND. EMPTY(ldStart) .AND. POSHDR.ENTERED > ldEnd) .OR. ;
     (!EMPTY(ldEnd) .AND. !EMPTY(ldStart) .AND. !BETWEEN(POSHDR.ENTERED,ldStart,ldEnd))
    lnDisc = 0
  ENDIF  
  
ENDIF
*E301129,1 AMM end

*!C101389,1 ASH 12/28/98 (Begin) Return the current alias before executing this function.
SELECT &lcAlias
*!C101389,1 ASH 12/28/98 (End)
RETURN ''

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 03/25/1998
*! Purpose   : To state that if we would print the word "Continued" 
*!             and to initialize some variables.
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfEndGroup()
*!*************************************************************
FUNCTION lfEndGroup
* Set this variable .T. to don't print the word "CONTINUED"
llEndGroup = .T.
* Initialize this variable here to print the style notes
llPrtSn    = .T.
* Initialize this variable here to print the style header
llTitle    = .T.
RETURN ''

*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 03/25/1998
*! Purpose   : To get the old value of the field
*!*************************************************************
*! Called from : POSTYPO.PRG
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

*!*************************************************************
*! Name      : lfvPO
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 03/25/1998
*! Purpose   : Valid function of the PO field.
*!*************************************************************
*! Called from : POSTYPO.PRG
*!*************************************************************
*! Calls       : gfBrows().
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPO()
*!*************************************************************
FUNCTION lfvPO

PRIVATE lcVar , lcObj , laTemp, lnAlias
lnAlias=SELECT(0)
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))   && Varible to hold the current field value
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))
SELECT POSHDR
SET ORDER TO TAG POSHDR
*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj .OR. !SEEK(lcRpForm+lcObj , 'POSHDR'))

  SELECT  APVENDOR 
  SET ORDER TO TAG VenCode 
  SELECT POSHDR
  SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  
  DIMENSION laTemp[1]
  laTemp = ''                           && Array to hold the Selected value
  * Change the header of the field PO according the user choice of printing 
  * (PO,Contract,Ret PO, All)
  lcHead = IIF(lcRpForm='A','Trans#',IIF(lcRpForm='P','PO#',;
                IIF(lcRpForm='C','Contract#',IIF(lcRpForm='R','Ret.PO#','Inter-Loc. PO')   )))  
  
  lcBrFields = "PO                :R :H= lcHead,"+;
               "STATUS            :R :H= 'Status' ,"+;
               "Vendor            :R :H= 'Vendor' ,"+;
               "APVENDOR.cVenComp :R :H= 'Name' ,"+;
               "Entered           :R :H= 'Entered' ,"+;
               "Complete          :R :H= 'Complete' ,"+;
               "Open              :R :H= 'Open' :P='999999' ,"+;
               "POTOTAL           :R :H= 'PoTotal' :P='9999999999.99' "
  
  lcFile_Ttl = IIF(lcRpForm='A','PO, Return PO, Contract',IIF(lcRpForm='P',;
                   'Purchase Order',IIF(lcRpForm='C','Contract',;
                   IIF(lcRpForm='R','Return Purchase Order','Inter-Location P/O'))))
          
  =gfBrows("FOR cStyType = IIF(lcRpForm = 'A' ,'', lcRpForm)",'PO','laTemp')
  SET RELATION TO
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfvVend
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 03/25/1998
*! Purpose   : Valid function of the Vendor field.
*!*************************************************************
*! Called from : POSTYPO.PRG
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
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(  ( EVALUATE(SYS(18)) )  )   && Varible to hold the current field value
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

*!*************************************************************
*! Name      : lfGetLogo
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 03/25/1998
*! Purpose   : Function to Save the company logo in temp. file 
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : POSTYPO.PRG
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
*-- Make cursor contain one field and one record holding the company logo
SELECT gobject;
 FROM Objects         ;
 WHERE Objects.cobject_id = lcObj_Id ;
 INTO CURSOR (lcLogoPic)
*-- end of lfGetLogo.

*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : AHMED MOHAMMED IBRAHIM (AMM)
*! Date        : 03/25/1998
*! Purpose     : Function to fill the apropriate Note data for report Notes.
*!             : (Line Notes OR NotePad) .
*!*************************************************************
*! Called from : POSTYPA.FRX [Variable lcGetN in the report]
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
PRIVATE lnAlias
lcTitle = ''
lcNotes = ''
lnAlias = SELECT(0)
*-- If we are to print both notes.
IF llPrntBoth
  *-- Note that the following Scheme
  *-- ....... cRecord = 'N1' ............. Style Notepad.
  *-- ....... cRecord = 'N2' ............. PO or Contract , or return Notepad.
  DO CASE 
    CASE &lcNoteLns..cRecord = 'N1' AND SEEK('F'+STYLE.cStyMajor,;
                    'Notepad') .AND. !EMPTY(ALLTRIM(Notepad.MNotes))
      lcTitle = IIF(llPrtSn,'Style Notepad','')
      lcNotes = IIF(llPrtSn,ALLTRIM(Notepad.MNotes),'')
    CASE &lcNoteLns..cRecord = 'N2' AND SEEK('P' + POSHDR.PO , 'NOTEPAD');
                     .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      *C200080,1 AMM  Consider the Dye order case
      *lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cStyType='P','Purchase Order Notepad',;
                   IIF(POSHDR.cStyType='C','Contract Notepad',;
                   IIF(POSHDR.cStyType='P','Return Purchase Order Notepad' ,;
                   'Inter-Location P/O Notepad'))),'')
      lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cStyType='P','Purchase Order Notepad',;
                   IIF(POSHDR.cStyType='C','Contract Notepad',;
                   IIF(POSHDR.cStyType='P','Return Purchase Order Notepad' ,;
                   IIF(POSHDR.cStyType='N','Inter-Location P/O Notepad',;
                   'Dye Order Notepad')))),'')
      *C200080,1 AMM end

      lcNotes = IIF(RECNO('POSLN') = lnLstLn, ;
                        ALLTRIM(NotePad.mNotes),'')
      
      llTitle = .F.
  ENDCASE
ELSE && Else You print either Style  or PO/Contract/Ret PO Notepad.
  DO CASE
    CASE llRpPrtSn AND SEEK('F'+STYLE.cStyMajor,'Notepad') .AND. ;
                       !EMPTY(ALLTRIM(Notepad.MNotes))
      lcTitle = IIF(llPrtSn,'Style Notepad','')
      lcNotes  =  IIF(llPrtSn,ALLTRIM(Notepad.MNotes),'')
    
    CASE llRpPrtPn .AND. SEEK('P' + POSHDR.PO , 'NOTEPAD');
                     .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      
      *C200080,1 AMM Consider the Dye order case
      *lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cStyType='P','Purchase Order Notepad',;
                   IIF(POSHDR.cStyType='C','Contract Notepad',;
                   IIF(POSHDR.cStyType='R','Return Purchase Order Notepad',;
                   'Inter-Location P/O Notepad'))),'')    
      lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cStyType='P','Purchase Order Notepad',;
                   IIF(POSHDR.cStyType='C','Contract Notepad',;
                   IIF(POSHDR.cStyType='R','Return Purchase Order Notepad',;
                   IIF(POSHDR.cStyType='N','Inter-Location P/O Notepad',;
                   'Dye Order Notepad')))),'')    
      *C200080,1 AMM end
      lcNotes  = IIF(RECNO('POSLN') = lnLstLn, ALLTRIM(NotePad.mNotes),'')
      llTitle = .F.
  ENDCASE
ENDIF
llPrintBox = !EMPTY(lcTitle)  && If it's .T. Report Print box around notes.
* State that we have already printed the style notepad (required to be 
* printed once)
llPrtSn    = .F.
SELECT (lnAlias)

RETURN ''

*!*************************************************************
*! Name        : lfShiftArr
*! Developer   : AHMED MOHAMMED IBRAHIM (AMM)
*! Date        : 03/25/1998
*! Purpose     : Function to Pack the passed array
*!*************************************************************
*! Calls       : 
*!              Procedures : None
*!              Functions  : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfShiftArr()
*!*************************************************************
FUNCTION lfShiftArr

PARAMETERS laArray
PRIVATE lnAlen,lnCount, lnC
* Get length of the array
lnALen = ALEN(laArray,1)
* check each element of the array if it is empty
FOR lnCount = 1 TO lnALen
  IF EMPTY(laArray[lnCount])
    * If any element is empty shift down the later elements
    FOR lnC = lnCount TO lnALen-1
      laArray[lnC]=laArray[lnC+1]
    ENDFOR
    laArray[lnAlen]=''
  ENDIF
ENDFOR

*!*************************************************************
*! Name        : lfGetLL 
*! Developer   : AHMED MOHAMMED IBRAHIM (AMM)
*! Date        : 03/25/1998
*! Purpose     : Function to get record number of last line in the PO
*!*************************************************************
*! Calls       : 
*!              Procedures : None
*!              Functions  : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfGetLL()
*!*************************************************************
FUNCTION lfGetLL 
PRIVATE lnAlias, lnRecNo

lnAlias = SELECT(0)
SELECT POSLN
lnRecNo = RECNO()
* Count to go to last record in the warehouse or account in the order.
*B802893 RAMY Add the TRANCD field to the count Expr [start]

*lcOldExp = PO+cWareCode+Account+store
*COUNT WHILE PO+cWareCode+Account+store = lcOldExp

lcOldExp = PO+cWareCode+Account+store + '1'
COUNT WHILE PO+cWareCode+Account+store + trancd = lcOldExp
*B802893 RAMY [end]
IF PO+cWareCode+Account+store # lcOldExp
  SKIP -1
ENDIF
* Get record number of last lien in the PO.
lnLstLn = RECNO()
SELECT (lnAlias)

* Refresh the relation between POSHDR and POSLN
IF !EOF('POSHDR')
  GOTO RECNO('POSHDR') IN POSHDR
ENDIF
GOTO lnRecNo IN POSLN

RETURN ''

*!*************************************************************
*! Name      : lfvOMsg
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 03/25/1998
*! Purpose   : To Open the Optional message screen
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOMsg()
*!*************************************************************
FUNCTION lfvOMsg

PRIVATE laOptMsg
DECLARE laOptMsg[3,2]       && Array to hold the name and length of the variables to be used in the Optional message screen
laOptMsg[1,1] = 'lcOMsg1'   && 1st. line Variable
laOptMsg[1,2] = 75          && Line length
laOptMsg[2,1] = 'lcOMsg2'   && 1st. line Variable
laOptMsg[2,2] = 75          && Line length
laOptMsg[3,1] = 'lcOMsg3'   && 1st. line Variable
laOptMsg[3,2] = 75          && Line length

= gfOptMsg('laOptMsg')      && Call Function to write optional message.

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/28/98
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
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
    *B602800,1 AMM restore variable original value
    llClear = .F.
    *B602800,1 AMM end
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE

*!*************************************************************
*! Name      : lfChkSysDy
*! Developer : AHMED AMER
*! Date      : 08/20/98
*! Purpose   : To check if the system uses dyelot or not
*!*************************************************************
*! Called from : Option grid (Variable llDyelot)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : = lfChkSysDy()
*!*************************************************************

FUNCTION lfChkSysDy

RETURN (ALLTRIM(UPPER(gfGetMemVar('M_DYELOT'))) = 'Y')


*!*************************************************************
*! Name      : lfvRpForm
*! Developer : Ahmed Mohame Ibrahim
*! Date      : 08/20/98
*! Purpose   : To Disable the vendor setting in the option grid 
*!             if the user chose to print Inter Location Purchase 
*!             Order, and enable it otherwise
*!*************************************************************
*! Called from : Option grid (Variable lcRpForm)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : = lfvRpForm()
*!*************************************************************
FUNCTION lfvRpForm
PRIVATE lnVendorPo, lnSrcLoc
lnVendorPo = 1
IF ASCAN(laOGVrFlt,'POSHDR.VENDOR') # 0
  *-- Get the position of the vendor in the varaible filter
  lnVendorPo = ASUBSCRIPT(laOGVrFlt, ASCAN(laOGVrFlt,'POSHDR.VENDOR'),1)
  IF lcRpForm # 'N'
    *-- Enable the vendor if not Inter Location PO
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnVendorPo] = .T.
  ELSE
    *-- Disable the vendor if Inter Location PO
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnVendorPo] = .F.
  ENDIF
  *-- Refresh the Option Grid
  = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnVendorPo)) + ',6]')
ENDIF
*E301095,1 AMM Enable the source location setting in case of Inter Location 
*E301095,1 AMM PO, and disable otherwise
IF ASCAN(laOGVrFlt,"LEFT(POSLN.VENDOR,6)") # 0
  *E301095,1 AMM Get the number of the source location element in the variable filter array 
  lnSrcLoc   = ASUBSCRIPT(laOGVrFlt, ASCAN(laOGVrFlt,"LEFT(POSLN.VENDOR,6)"),1)
  *E301095,1 AMM Enable the object if Interlocation only
  IF lcRpForm # 'N'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnSrcLoc] = .F.
  ELSE
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnSrcLoc] = .T.
  ENDIF
  *E301095,1 AMM Refresh the source location setting in the option grid
  = lfOGShowGet('laOGVrFlt[' + ALLTRIM(STR(lnSrcLoc)) + ',6]')
ENDIF
*E301095,1 AMM end
*E301129,1 AMM Enable "Print price after discount" setting in case of inter 
*E301129,1 AMM location PO, else disable.
IF ASCAN(laOgObjType,'llrpPdic') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llrpPdic'),1)
  laOGObjCnt[lnPos] = (lcRpForm = 'N') .AND. (lcRpPrice = 'P')
  = lfOGShowGet('llrpPdic')
ENDIF
*E301129,1 AMM end
*B602800,1 AMM make it true to clear selection 
llClear = .T.
*B602800,1 AMM end

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Ahmed Mohame Ibrahim
*! Date      : 12/14/98
*! Purpose   : The when function of the option grid
*!*************************************************************
*! Called from : Option grid (Variable lcRpForm)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen
*E301095,1 AMM Disable the source location setting at starting
IF ASCAN(laOGVrFlt,"LEFT(POSLN.VENDOR,6)") # 0
  *E301095,1 AMM Get the number of the source location element in the 
  *E301095,1 AMM variable filter array 
  lnSrcLoc   = ASUBSCRIPT(laOGVrFlt, ASCAN(laOGVrFlt,"LEFT(POSLN.VENDOR,6)"),1)
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnSrcLoc] = ;
                                            IIF(lcRpForm # 'N',.F.,.T.)
ENDIF

IF ASCAN(laOGVrFlt,'POSHDR.VENDOR') # 0
  *-- Get the position of the vendor in the varaible filter
  lnVendorPo = ASUBSCRIPT(laOGVrFlt, ASCAN(laOGVrFlt,'POSHDR.VENDOR'),1)
  laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOGVrFlt,1) + lnVendorPo] = ;
                                            IIF(lcRpForm # 'N',.T.,.F.)
ENDIF

*E301129,1 AMM Enable "Print price after discount" setting in case of inter 
*E301129,1 AMM location PO, else disable.
IF ASCAN(laOgObjType,'llrpPdic') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llrpPdic'),1)
  laOGObjCnt[lnPos] = (lcRpForm = 'N') .AND. (lcRpPrice = 'P')
  = lfOGShowGet('llrpPdic')
ENDIF
*C101666,1 (Begin) Get the user's previlige and enable or disable the cost option in OG accordingly.

llCostPrv  = gfUserPriv('IC','ICSTYLE','COSTING')
IF ASCAN(laOgObjType,'lcRpPrice') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lcRpPrice'),1)
  laOGObjCnt[lnPos] = llCostPrv
  = lfOGShowGet('lcRpPrice')
ENDIF
*C101666,1 (End)
*E301129,1 AMM end

*E301338,1 AMM If the user has no previlige on costing, disable the option
IF ASCAN(laOgObjType,'llrPrtCs') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llrPrtCs'),1)
  llrPrtCs = IIF(llCostPrv,llrPrtCs,llCostPrv)
  laOGObjCnt[lnPos] = llCostPrv
  = lfOGShowGet('llrPrtCs')
ENDIF
IF ASCAN(laOgObjType,'LLRPCost') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRPCost'),1)
  laOGObjCnt[lnPos] = llrPrtCs
  =lfOGShowGet('LLRPCost')
ENDIF
*E301338,1 AMM end

*!*************************************************************
*! Name      : lfvPrice
*! Developer : Ahmed Mohame Ibrahim
*! Date      : 08/20/98
*! Purpose   : Enable "Print price after discount" setting in case 
*!             of inter location PO, else disable.
*!*************************************************************
*! Called from : Option grid (Variable lcRpForm)
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : = lfvPrice()
*!*************************************************************
FUNCTION lfvPrice
*E301129,1 AMM Enable "Print price after discount" setting in case of inter 
*E301129,1 AMM location PO, else disable.
IF ASCAN(laOgObjType,'llrpPdic') # 0
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llrpPdic'),1)
  laOGObjCnt[lnPos] = (lcRpForm = 'N') .AND. (lcRpPrice = 'P')
  = lfOGShowGet('llrpPdic')
ENDIF
*E301129,1 AMM end



*!*************************************************************
*! Name      : lfGetTitle
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 07/15/1998
*! Purpose   : To get the cost element title.
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetTitle()
*!*************************************************************
FUNCTION lfGetTitle
PRIVATE lcSetEx
lcCostItm = ''
lcSetEx = SET('EXACT')
SET EXACT ON
lnAlias = SELECT(0)
SELECT (lcCostF)
*-- the LACOST array has 10 rows, 5 for category type, and 5 for the 
*-- coresponding titles.  so, get the row of the cost element and add 5 to 
*-- the row number to get the cost element title row number.
IF ASCAN(laCost,cCatgTyp) # 0
  lcCostItm = laCost[ASUBSCRIPT(laCost,ASCAN(laCost,cCatgTyp),1)+5,2]
ENDIF
lcHead1   = ''
lcHead2   = ''
IF cCatgTyp = 'T'
  lcHead1   = '                  DATE                 QTY      DATE     DIRECT'
  lcHead2   = 'VENDOR  ORDERD PO #  ORDRD RECVD. SHIPD.'
ELSE 
  IF cCatgTyp = 'F'
    lcHead1   = '[-------------------- PULLED -------------------]  TOTAL'
  ENDIF
ENDIF

SET EXACT &lcSetEx
SELECT (lnAlias)
RETURN ''


*!*************************************************************
*! Name      : lfGetOp
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/25/99
*! Purpose   : to be done on the starting of page, used in lot cost 
*!             sheet form 
*! Reference : *E301338,1 AMM
*!*************************************************************
*! Called from : MFPRCSA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetOp()
*!*************************************************************
FUNCTION lfGetOp


*!*************************************************************
*! Name      : lfStGroup
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/25/1999
*! Purpose   : To Initialize the variable llEndGroup
*! Reference : *E301338,1 AMM
*!*************************************************************
*! Called from : MFPRTCSA.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStGroup()
*!*************************************************************
FUNCTION lfStGroup
llEndGroup = .F.
RETURN ''


*!*************************************************************
*! Name      : lfvPrtCs
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 10/25/1999
*! Purpose   : Valid function of the print cost sheet setting on the option
*!             grid to enable or disable the setting of print cost on cost sheet.
*! Reference : *E301338,1 AMM
*!*************************************************************
*! Called from : OPTION GRID.
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrtCs()
*!*************************************************************
FUNCTION lfvPrtCs
*-- Get the position of the print cost setting in the array to enable or 
*-- disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRPCost'),1)
laOGObjCnt[lnPos] = llrPrtCs
= lfOGShowGet('LLRPCost')


