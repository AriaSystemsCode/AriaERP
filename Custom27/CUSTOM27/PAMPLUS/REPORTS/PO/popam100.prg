*:***************************************************************************
*: Program file  : POPAM100
*: Program desc. : Print PO, Contract and Return PO Custom for PAMPLUS
*: For Report    : POSTYCP.FRX
*: System        : Aria Advantage Series VER. 2.7
*: Module        : Purchase Order (PO)
*: Developer     : Hesham El_Sheltawi
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfGetCodes, lfEndGroup, lfwOldVal, lfvPO, lfvVend, lfGetLogo,
*:                 lfShiftArr
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*:Cust. Prog. # 101507, 603077
*:***************************************************************************
*! Modifications :


DECLARE laCompAdd[6,1], laDivLName[1,2], laVenAdr[1,5],laShpAdr[1,5]
STORE SPACE(0) TO laCompAdd, laShpAdr, lcVenName, lcDivDesc, lcTerms, ;
                  lcShpName, lcShipVia, lcStyTitle, lcDivLName, lcLogoPic, ;
                  lcTitle, lcNotes, lcCompPhon
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
* If there is a company logo , create a cursor hold this logo
lcStyTitle = gfItemMask('HI')
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSize  = LEN(lcMaj)                   && Length of the major
lcTime     = TIME()                       && Variable to hold the Time

*************************************************************************
* Open necessary tables and make necessary relations.

SELECT POSLN
SET RELATION TO STYLE INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET ORDER TO TAG POSLNW
SELECT poshdr
SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
SET RELATION TO IIF(lcRpForm='A',POSHDR.cSTYType,lcRpForm)+PO INTO POSLN ADDITIVE
SET SKIP TO POSLN
***********************************************************************

laDivLName[1,1] = 'DIVLNAME'      && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

lcSkipExpr = [POSLN]


*lcRepExpr = [IIF(llPrntBoth,IIF(&lcNoteLns..cRecord = 'N2',RECNO('POSLN') = lnLstLn,.T.),.T.)] 
*lcRpExp   = IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr) 

SELECT POSHDR
*E301150,1 AMM Move to the beginning of the file
GO TOP
*E301150,1 AMM end
SET SKIP TO &lcSkipExpr
*C101389,1 Call any optional function 

lnDsRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
llCanDisp = RECCOUNT()>0
IF !EMPTY(lcRpExp) 
  GO TOP
  lcLoctCond = 'For '+lcRpExp 
  LOCATE &lcLoctCond
  llCanDisp = FOUND()
ENDIF

IF lnDsRecNo<>0
  GO lnDsRecNo
ENDIF

IF !llCanDisp
  =gfModalGen("INM00052B00000","DIALOG")
  RETURN
ENDIF

=lfCreatTmps(@laFxFltCur)
=lfCreatTmps(@laVrFltCur)
DIMEN laRpFxCur[ALEN(laFxFltCur,1),ALEN(laFxFltCur,2)]
DIMEN laRpVrCur[ALEN(laVrFltCur,1),ALEN(laVrFltCur,2)]
=ACOPY(laFxFltCur,laRpFxCur)
=ACOPY(laVrFltCur,laRpVrCur)
lcRpDtDir = gcDataDir
lcRpWrDir = gcWorkDir
lcRpSysDir = gcSysHome
lcRpRpDir = gcRepHome
lcRpImDir = gcImagDir
lcRpComp = gcAct_Comp
lcRpDevice = gcDevice
lnRpMajSiz = lnMajSize
SAVE TO (gcWorkDir+lcTmpMemo+'.MEM') ALL LIKE l?RP* 
lcCommLine = (gcWorkDir+lcTmpMemo+'.MEM')
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
   ",gcRepHome+'PO\'+[POSTYCP.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
   RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF   



*C101389,1 Call lfIsApparl to run FRX/..SAY forms
*IF !lfIsApparl(@lcFormName) AND EMPTY(lcOptProg)
*C101389,1 end
 
*  DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

*C101389,1 Add an ENDIF
*ENDIF
*C101389,1 end


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
      lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cStyType='P','Purchase Order Notepad',;
                   IIF(POSHDR.cStyType='C','Contract Notepad',;
                   IIF(POSHDR.cStyType='P','Return Purchase Order Notepad' ,;
                   'Inter-Location P/O Notepad'))),'')


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
      
      lcTitle = IIF(RECNO('POSLN') = lnLstLn, IIF(POSHDR.cStyType='P','Purchase Order Notepad',;
                   IIF(POSHDR.cStyType='C','Contract Notepad',;
                   IIF(POSHDR.cStyType='R','Return Purchase Order Notepad',;
                   'Inter-Location P/O Notepad'))),'')    
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
lcOldExp = PO+cWareCode+Account+store
COUNT WHILE PO+cWareCode+Account+store = lcOldExp
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
lcogPlatForm = 'WINDOWS'
lcRepMode = 'Graphics'
llogstylech = .F.
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
*E301129,1 AMM end

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
*! Name      : lfCreatTmps
*! Developer : Hesham El-Sheltawi
*! Date      : 04/26/99
*! Purpose   : function to create temp files for the range cursors
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : .....
*!*************************************************************
*! Return      : Logical 
*!*************************************************************
*! Example     : = lfCreatTmps()
*!*************************************************************
FUNCTION lfCreatTmps
PARAMETERS laCursFiles
PRIVATE lnCount,lcTmpName
FOR lnCount = 1 TO ALEN(laCursFiles,1)
  IF !EMPTY(laCursFiles[lnCount,2]) AND USED(laCursFiles[lnCount,2])
    lcTmpName = gfTempName()
    SELECT (laCursFiles[lnCount,2])
    lcTag = TAG()
    lcKey = KEY()
    COPY STRUC TO (gcWorkDir+lcTmpName) &&WITH CDX
    USE (gcWorkDir+lcTmpName) IN 0 EXCL
    SELECT (lcTmpName)
    IF !EMPTY(lcTag)
      INDEX ON &lcKey TAG &lcTag ADDI
    ENDIF
    SELECT (laCursFiles[lnCount,2])
    SCAN
      SCATT MEMVAR MEMO
      INSERT INTO &lcTmpName FROM MEMVAR
    ENDSCAN
    USE IN (lcTmpName)
    USE IN (laCursFiles[lnCount,2])
    ERASE (gcWorkDir+laCursFiles[lnCount,2]+'.DBF')
    ERASE (gcWorkDir+laCursFiles[lnCount,2]+'.FPT')
    ERASE (gcWorkDir+laCursFiles[lnCount,2]+'.CDX')
    RENAME (gcWorkDir+lcTmpName+'.DBF') TO (gcWorkDir+laCursFiles[lnCount,2]+'.DBF')
    IF FILE(gcWorkDir+lcTmpName+'.FPT')
      RENAME (gcWorkDir+lcTmpName+'.FPT') TO (gcWorkDir+laCursFiles[lnCount,2]+'.FPT')
    ENDIF
    RENAME (gcWorkDir+lcTmpName+'.CDX') TO (gcWorkDir+laCursFiles[lnCount,2]+'.CDX')
    USE (gcWorkDir+laCursFiles[lnCount,2]) IN 0 ORDER 1
  ENDIF
ENDFOR

FUNCTION lfClearRep
ERASE (gcWorkDir+lcTmpMemo+'.MEM')
FOR lnCount = 1 TO ALEN(laFxFltCur,1)
  IF !EMPTY(laFxFltCur[lnCount,2]) AND USED(laFxFltCur[lnCount,2])
    USE IN (laFxFltCur[lnCount,2])
    ERASE (gcWorkDir+laFxFltCur[lnCount,2]+'.DBF')
    ERASE (gcWorkDir+laFxFltCur[lnCount,2]+'.FPT')
    ERASE (gcWorkDir+laFxFltCur[lnCount,2]+'.CDX')
  ENDIF
ENDFOR

FOR lnCount = 1 TO ALEN(laVrFltCur,1)
  IF !EMPTY(laVrFltCur[lnCount,2]) AND USED(laVrFltCur[lnCount,2])
    USE IN (laVrFltCur[lnCount,2])
    ERASE (gcWorkDir+laVrFltCur[lnCount,2]+'.DBF')
    ERASE (gcWorkDir+laVrFltCur[lnCount,2]+'.FPT')
    ERASE (gcWorkDir+laVrFltCur[lnCount,2]+'.CDX')
  ENDIF
ENDFOR
