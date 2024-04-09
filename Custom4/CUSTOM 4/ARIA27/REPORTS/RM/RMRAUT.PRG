*:***************************************************************************
*: Program file  : RmrAut.PRG
*: Program desc. : Return Authorization
*: For Report    : RmrAutB.FRX,RmrAutA.LBX
*: System        : Aria Advantage Series.
*: Module        : Return Merchandise (RM)
*: Developer     : Hossam El Etreby   (HDM)
*:***************************************************************************
*: Calls : 
*:    Procedures : 
*:    Functions  : 
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO RmrAut
*:***************************************************************************
*: This Program is due to E300947
*----------------------------------------------------------------------------
*E300871,5 KHM   02/09/99 Getting the report/form name and check if its
*E300871,5                an frx or @ say one.
*B802087,1 HDM   04/15/1999 Correct empty lines printing
*B802090,1 HDM   04/15/1999 Fix Bug of Icorrect warehouse address
*B602854,1 HDM   05/02/1999 Fix bug of Invalid path or file name
*E301332,1 HDM   10/25/1999 Develop new formate for stickers
*C101750,1 HDM   01/17/2000 Develop New Customized receipts format for TOCCA
*B603404,1 HDM   01/26/2000 Sticker form 'B' should be printed in sequential order
*B603172,1 SAM   02/29/2000 fix some bugs : 
*B603172,1                    1- Disable Print note pad option in option grid if 
*B603172,1                       report form is Sticker
*B603172,1                    2- print only 8 characters of customer PO# (it should be 15)
*B603172,1                 modifications for 1 is in prg ,for 2 is in frx -shift some items only-
*B803323,1 WAB   08/22/2000 Fix the bug of not printing the division long name
*B803327,1 WAB   08/23/2000 Fix the bug of displaying the setup printer window not the Aria setup 
*B803327,1 WAB              window when select print both ( R/A & Sticker ) & the report is text 
*E301643,1 MHM 29/07/2001 Prevent Electronic status to RM module 
*N120264,1 ABD 12/29/2003 Print sell price on the frx [Change made at the FRX only.]
*C121117,1 TMI 01/26/2004 Add the fax# 
*B038358,1 ASH 07/29/2004 Fix the bug of printing logo even there is no company logo.
*:***********************************************************************************************
lcDivLName = ''
lcReason   = ''
lcAccPhone = ''
*--HDM B602854,1 [Start] Define a new temp variable to hold the lcOGTmpFrm old Value
STORE '' TO lcOldTFrm 
lcFrmFrx = IIF(TYPE('lcFrmFrx')<>'U' AND !EMPTY(lcFrmFrx),lcFrmFrx,'' )
*--HDM B602854,1 [End]
llMWHouse = .F.
*C121117,1  TMI [Start] Define the fax #
lcFaxNo = ''
*C121117,1  TMI [End  ] 
lcCode = ''
STORE .F. TO llAgain
STORE 0 TO lnLast , lnLBLCnt , lnCurLbl,lnRec
STORE 1 TO lnCurrRec
*E300871,5 KHM   02/09/99 (Begin) Getting the form name and the type
*E300871,5                of the form whether its @ say or an FRX.
llNoRec    = .F.   &&Added 05/06/99
*lcPrgName = lcFormName
lcPrgName  = lcRpForm
*llIsAparel = lfIsApparl(@lcPrgName)
llPrntComp = TYPE('llPrntComp') = 'L' .AND. llPrntComp 
*E300871,5 KHM   02/09/99 (End)

lcLinkWith = (ALLTRIM(gfGetMemVar('M_WAREHOUS',gcAct_Comp)))
IF lcLinkWith = 'Y'
  llMWHouse = .T.
ELSE
  llMWHouse = .F.
ENDIF

*C101750,1 [Start] Define logical variable to determine
*                  if it's TOCCA formate or not
IF TYPE('lcTocca')= 'C' AND ALLTRIM(lcTocca) = 'Y'
  llTocca = .T.
  STORE '' TO lcVendCD , lcDunsNo , lcDept
ELSE
  llTocca = .F.
ENDIF

*C101750,1 [End]

DIMENSION laTmp[6,1]
lcTime=TIME()
DECLARE laWareAdd[6,1]
STORE '' TO lcDivLName,lcLines,lcDummyRel , lcDummySty
STORE 0 TO lnLastRec,lnLines
*--HDM B802087,1 [Start] Make Page length constant
*lnPageLns = IIF(llRpScale,12,28)
lnPageLns = 26
*--HDM B802087,1 [End] Make Page length constant
*B038358,1 ASH 07/29/2004 (Begin) Fix the bug of printing logo even there is no company logo.
llLogo = IIF(SEEK('*' + 'LOGO' , 'OBJLINK') .AND. SEEK(OBJLINK.cObject_ID ,;
         'OBJECTS') , .T. , .F.)        && Flag to know if we are to print the Company Logo
*B038358,1 ASH 07/29/2004 (End)

IF EMPTY(lcCompName)
  PRIVATE lcAliasNow
  lcAliasNow = ALIAS()
  *-- if this company have a logo, point to logo record, 
  *-- else seek put pointer at EOF.
  *B038358,1 ASH 07/29/2004 (Begin) Fix the bug of printing logo even there is no company logo.
  *= SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')
  *B038358,1 ASH 07/29/2004 (End)
  DECLARE laCompAdd[6,1],laDivLName[1,2],laSoldTo[7,1]
  *--HDM B802090,1 04/01/1999 [Start] Bug of not printing warehous addresses
  DECLARE laWareAdd[6,1]
  *--HDM B802090,1 04/01/1999 [End]
  laCompAdd = ''                   && Array to hold the Company address
  laSoldTo  = ''                   && Array to hold the Sold To address
  *B803323,1 WAB - (START)-Correct the name of the related field ( DIVLNAME instead of CDIVISION
  *laDivLName[1,1] = 'CDIVISION'    && Array to get the Division long name
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  *B803323,1 WAB - (END)
  laDivLName[1,2] = 'lcDivLName'
  laWareAdd=''
  *= gfRltFld(RetAuth.CDivision , @laDivLName , 'CDIVISION')  && Get Division.

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp
  
  lcCompName = cCom_Name             && Company Name.
  lcCompPhon = cCom_Phon             && Company Phone.
  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  laCompAdd[6] = 'Phone# : '+ TRANSFORM(lcCompPhon , lcPhonPict)

  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.

  *C101751,1[start] Variable to hold DUNS#
  IF llTocca
    lcDunsNo  = gfGetMemVar('XDUNS')
  ENDIF
  *C101751,1[end]

ENDIF
*----------
SELECT RETAUTH
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE

SET RELATION TO RANO INTO RALINE ADDITIVE

SELECT RETAUTH
SET RELATION TO Retauth.cwarecode INTO Warehous ADDITIVE

*--HDM
SET RELATION TO 'Z'+ Retauth.rano INTO Notepad ADDITIVE
*--HDM
*----------
SELECT Style
SET ORDER TO TAG Style

SELECT RALINE
SET RELATION TO style INTO Style

SELECT STYLE
SET RELATION TO 'S' + Scale INTO SCALE

*E300871,5 KHM  02/09/99 (Begin) Commenting the following lines and move them
*E300871,5               to be done when the form is an FRX one.
*SELECT RETAUTH
*SET SKIP TO RALINE
*E300871,5 KHM   02/09/99 (End)

*----------
*E301643,1 MHM 29/07/2001 Prevent Electronic status [Start]
lcRpExp = lcRpExp + " AND RETAUTH->STATUS <> 'E'"
*E301643,1 MHM 29/07/2001 [End]

DO CASE
  CASE lcRpType = 'S'
    lcRpForm = lfGetForm('RS')
    llIsAparel = lfIsApparl(@lcPrgName)
    SELECT RETAUTH
    *E301332,1 [Start] call lfGetForm with the form's special ID
    *                  To restore the new form's settings
    lcRpForm = lfGetForm('RS')
    *E301332,1 [End]
    = lfPrepHDR()
    *E301332,1 [Start] Check the special sets for this form
    *                  if it contains 'FRX' Run the sticker
    *                  as an frx Format
    *DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp , .F., 'L'
    IF lcFrmFrx = 'Y'
      *B603404,1[START] Save the Page advance value and set it to line feed
      lcSavPAdv = _PADVANCE
      _PADVANCE = 'LINEFEED'
      *B603404,1 [End]
      DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp
      *B603404,1 [Start] restore page advance settings
      _PADVANCE = lcSavPAdv
      *B603404,1 [end]

    ELSE
      DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp , .F., 'L'
    ENDIF
    *E301332,1 [End]
    =lfClrTemp()

  CASE lcRpType = 'R'
    *E300871,5 KHM   02/09/99 (Begin) Adding the checking of the report
    *E300871,5                whether its @ say or an Frx one.
    
    lcRpForm = lfGetForm('')

    *--HDM B602854,1 [Start] Keep lcOGTmpFrm old value
    lcOldTFrm = lcOGTmpForm
    *--HDM B602854,1 [End]
    llIsAparel = lfIsApparl(@lcPrgName)
    IF llIsAparel
      DO EVAL('lcPrgName')
      IF !llNoRec
        DO ENDREPORT
      ENDIF
    ELSE
      SELECT RETAUTH
      SET SKIP TO RALINE
      SELECT RETAUTH
      DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp
    ENDIF  
    *--HDM B602854,1 [Start] restore lcOGTmpFrm old value
    lcOGTmpForm = lcOldTFrm
    *--HDM B602854,1 [End]

    *E300871,5 KHM   02/09/99 (End)
  CASE lcRpType ='B' && Device Selection

    *-- Run The RECEIPT *.FRX
    *--HDM B602854,1 [Start] Keep lcOGTmpFrm old value
    lcOldTFrm = lcOGTmpForm
    *--HDM B602854,1 [End]
    lcRpForm = lfGetForm('')
    *E300871,5 KHM   02/09/99 (Begin) Adding the checking of the report
    *E300871,5                whether its @ say or an Frx one.
    llIsAparel = lfIsApparl(@lcPrgName)
    IF llIsAparel
      DO EVAL('lcPrgName')
      IF !llNoRec
        DO ENDREPORT
      ENDIF
    *E300871,5 KHM 02/09/99 (End)
    ELSE
      = lfRepPltFr(lcRpForm)
      SELECT RETAUTH
      SET SKIP TO RALINE
      SELECT RETAUTH
      DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp
      SET SKIP TO
    *E300871,5 KHM 02/09/99 (Begin)
    ENDIF
    *E300871,5 KHM 02/09/99 (End)

    *E301332,1 [Start] Check the special sets for this form
    *                  if it contains 'FRX' Run the sticker
    *                  as an frx Format
    *-- Run The Sticker *.lbx
    
    *B803323,1 WAB - (START)- save the cureent palatform 
    lcOldPltFrm = IIF(TYPE('OGPlatForm') = 'C' , OGPlatForm , IIF(TYPE('lcRepMode') = 'C',lcRepMode,''))
    *B803323,1 WAB - (END)
    
    lcRpForm = lfGetForm('RS')
    = lfRepPltFr(lcRpForm)
    *B803323,1 WAB -assign the value of the lcOGPlatForm to OGPlatForm cause of the value of this value 
    *B803323,1 WAB -(START)the pSetuo() diplay the windows printer setup or the aria printer setup 
    OGPlatForm = IIF(TYPE('lcOGPlatForm') = 'C' , lcOGPlatForm , IIF(TYPE('lcRepMode') = 'C',lcRepMode,''))
    *B803323,1 WAB -(END)
    
    IF gcDevice <> 'SCREEN'
      = pSetup(.T.,llOGBatchOk)
    ENDIF
    *-- Run The Report *.FRX
    =lfPrepHDR()
    lcOGTmpForm = ''
    *DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp , .F., 'L'
    IF lcFrmFrx = 'Y'
      *B603404,1[START] Save the Page advance value and set it to line feed
      lcSavPAdv = _PADVANCE
      _PADVANCE = 'LINEFEED'
      DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp
      *B603404,1 [Start] restore page advance settings
      _PADVANCE = lcSavPAdv
      *B603404,1 [end]

    ELSE
      DO gfDispRe WITH EVAL('lcRpForm') , 'FOR ' + lcRpExp , .F., 'L'
    ENDIF
    *E301332,1 [End]
    *B803323,1 WAB -(START)- Restore the value that we saved before running the 2nd report
    OGPlatForm = lcOldPltFrm 
    *B803323,1 WAB -(END)
    =lfClrTemp()
    *-- Restore the lcRpForm to Stickers
    *--HDM B602854,1 [Start] restore lcOGTmpFrm old value
    lcOGTmpForm = lcOldTFrm
    *--HDM B602854,1 [End]
    lcRpForm = lfGetForm('')
    = lfRepPltFr(lcRpForm)
    
ENDCASE
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hossam El Etreby   (HDM)
*! Date      : 04/08/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************

FUNCTION lfwRepWhen

SET ORDER TO RETAUTH IN RETAUTH    && To use it to validate R/A#      in option grid.
SET ORDER TO CUSTOMER IN CUSTOMER  && To use it to validate Account   in option grid.
*C101750,1 [Start] Open Inoice HDR Data File for Tocca
IF TYPE('lcTocca')= 'C' AND ALLTRIM(lcTocca) = 'Y'
  llInvOp = gfOpenFile(gcDataDir+'INVHDR' , 'INVHDR' , 'SH')
ENDIF

*B603172,1 [Start] Disable Print NotPad option in option grid if report form is sticker 
=lfvNPStat()
*B603172,1 [End]

*C101750,1 [End]
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvRA
*! Developer : Hossam El Etreby   (HDM)
*! Date      : 04/08/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Validate if RA# doesn't match the table display
*!               a browse
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvRA()
*!*************************************************************

FUNCTION lfvRA
PRIVATE lcVar , lcObj , laTemp

lcVar = SYS(18)
lcObj = EVALUATE(SYS(18))
lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

*-- IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) AND !(SEEK(lcObj , 'RETAUTH') AND RETAUTH.STATUS = 'O')
  SELECT RETAUTH
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  lcBrFields = "raNo                                 :H = 'RA#'     ,"+;
               "cPrinted=IIF(EMPTY(Flag),'NO','YES') :H = 'Printed' ,"+;
               "Account                              :H = 'Acct#'   ,"+;
               "Store                                :H = 'Store#'  ,"+;
               "RADate                               :H = 'Assued'  ,"+;
               "Void                                 :H = 'Void'    ,"+;
               "Auth                                 :H = 'Auth.Ps' ,"+;
               "Authamt                              :H = 'Auth.$$'"
               
  lcFile_Ttl = "RA Authorization"
  lcBrowCond = [FOR STATUS = "O"]
  lNThing    = gfBrows(lcBrowCond,'RANO','laTemp')

  *-- IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE
    lcObj = laOldVal
  ENDIF
ENDIF
&lcVar = lcObj      && Update the field

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvAccount
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Validation function for the Customer Account field
*!*************************************************************
*! Called from : Customer Account field [Option Grid]
*!*************************************************************
*! Calls       : CusBrowM()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvAccount
PRIVATE lcObjName , lcObjVal , llObjRet

lcObjName = SYS(18)      && Varible to hold  the name of the memory variable used to create the current GET field
lcObjVal = EVALUATE(SYS(18))      && Varible to hold  the value of the current GET field

*-- IF The user wants to Browse or the Account is not in the file
IF '?' $ lcObjVal .OR. (!EMPTY(lcObjVal) .AND. !SEEK('M' + lcObjVal , 'CUSTOMER'))
  llObjRet = CusBrowM(@lcObjVal , '' , 'M')
  lcObjVal = IIF(llObjRet , lcObjVal , laOldVal)
  &lcObjName = lcObjVal
ENDIF    && End of IF

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfOldVal
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Keeps the old value of the editor when cancel the browse
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfOldVal

laOldVal = EVALUATE(SYS(18))

*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfvRepType
*! Developer : Hossam El Etreby (HDM)
*! Date      : 04/08/1998
*! Purpose   : Determines Report type according to User Selection
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************

FUNCTION lfvRepType

lcRpForm = IIF(lcRpType = 'R' , lfGetForm('') ,;
               IIF(lcRpType = 'B' , lfGetForm('') , lfGetForm('RS')))

= lfRepPltFr(lcRpForm)
*B603172,1 [Start] Disable Print NotPad option in option grid if report form is sticker 
=lfvNPStat()
*B603172,1 [End]
*---------------------------------------------------------------------------
*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Hossam El Etreby
*! Date      : 03/25/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : RmrAut.Prg
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
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *-- IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    = ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF
ENDFOR

*-- FOR Loop to loop through the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  
  *-- IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*---------------------------------------------------------------------------
*!*************************************************************
*! Name        : lfHeader
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : RMRAUTB.FRX [Header Band]
*!*************************************************************
*! Calls       : FUNCTION => gfRltFld()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeader()
*!*************************************************************

FUNCTION lfHeader
PRIVATE lnPrevAl
= gfRltFld(RETAUTH.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
lcReason = gfCodDes(Retauth.reason,'REASON')
* fill Sold To address array


lnPrevAl = SELECT(0)
SELECT CUSTOMER
IF !EOF()
  GOTO RECNO()
ENDIF
SELECT(lnPrevAl)
laSoldTo[1] = CUSTOMER.STName

laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , IIF(Customer.Type='S','2',''))
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , IIF(Customer.Type='S','2',''))
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , IIF(Customer.Type='S','2',''))
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , IIF(Customer.Type='S','2',''))
laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , IIF(Customer.Type='S','2',''))
laSoldTo[7]  = TRANSFORM(Customer.Phone1,lcPhonPict)

laWareAdd[1]= gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laWareAdd[2]= gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laWareAdd[3]= gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laWareAdd[4]= gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laWareAdd[5]= gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
laWareAdd[6]= TRANSFORM(WareHous.cphone , lcPhonPict)

= lfAdrShift('laWareAdd')
= lfAdrShift('laSoldTo')    && Shift Sold To address if there is empty line.

*C101751,1 [Start] Get the Vendor # from Customwer file
IF llTocca
  lcVendCD = Customer.CCUSVEND
  lcDept = IIF(!EMPTY(retauth.INVOICE) AND SEEK(retauth.INVOICE,'INVHDR'),INVHDR.DEPT,'')
ENDIF
*C101751,1[End]

RETURN .T.
*---------------------------------------------------------------------------
*!*************************************************************
*! Name        : lfNoOfLns
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Function to Fill the rest of the page with DASHED Lines
*!*************************************************************
*! Called from : RMRAUTB.FRX [Header Band]
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfNoOfLns()
*!*************************************************************

FUNCTION lfNoOfLns

lcCurAl = SELECT(0)

SELECT RALINE_A
SET FILTER TO RANO = RALINE.RANO
COUNT TO lnLines  && FOR RANO = RALINE.RANO
COUNT TO lnLast
GO BOTTOM
lnLastRec = RECNO('RALINE_A')
*--HDM B802087,1 [Start]
*IF RECNO('RETAUTH')=1
IF (lnLines * IIF(llRpScale,4,2)) > lnPageLns

  *lnLines = ((lnLines / lnPageLns) - INT(lnLines / lnPageLns)) * lnPageLns

  lnAct = lnLines * IIF(llRpScale,4,2)
  lnPagesNo = lnAct / lnPageLns
  
  IF lnPagesNo > INT(lnPagesNo)
    lnPagesNo = INT(lnPagesNo+1)
  ENDIF
  lnLines   =  lnAct - (lnPagesNo * lnPageLns)
  IF lnLines < 0
    lnLines = (lnLines *(-1)) - 1
  ENDIF
 
ELSE
  *lnLines  = lnPageLns - lnLines
  lnLines = IIF(llRpScale,lnLines * 4,lnLines * 2)
  
ENDIF
*--HDM B802087,1 [End]
SET FILTER TO

IF lnLines >= 1
  IF llRpScale
    lcDashed = '--------------------------------                   ----              ----              ----              ----             ----              ----              ----              ----              ----'
    lcLines = lcDashed
  ELSE
    lcDashed = '--------------------------------                   -------------------------------------------------------------------------------------------------------------------           ----'
    lcLines = lcDashed
  ENDIF
  *--HDM B802087,1 [Start] Consider notepad lines if it's printed
  IF llRpNotes
    lnLines = lnLines - MEMLINES(NOTEPAD.MNOTES)
  ENDIF
  *--HDM B802087,1 [End] Consider notepad lines if it's printed
  FOR lnI = 1 TO lnLines
    lcLines = lcLines + IIF(lnI > 1, CHR(13) + lcDashed,'')
  ENDFOR
ENDIF
SELECT (lcCurAl)
RETURN ''
*------------------------------------------------------------
*!*************************************************************
*! Name        : lfDispAdd
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Function to Fill Address Array
*!*************************************************************
*! Called from : RMRAUTB.FRX [Header Band],RMRAUTA.LBX
*!*************************************************************
*! Calls       : lfAdrShift()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfDispAdd()
*!*************************************************************

FUNCTION lfDispAdd
IF EMPTY(lcCompName)
  PRIVATE lcAliasNow
  lcAliasNow = ALIAS()

  *-- if this company have a logo, point to logo record, 
  *-- else seek put pointer at EOF.
  
  = SEEK('*' + 'LOGO' , 'OBJLINK') AND SEEK(OBJLINK.cObject_ID,'OBJECTS')

  DECLARE laCompAdd[6,1],laDivLName[1,2],laSoldTo[7,1]
  DECLARE laWareAdd[6,1]
  laCompAdd = ''                   && Array to hold the Company address
  laSoldTo  = ''                   && Array to hold the Sold To address
  laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
  laDivLName[1,2] = ''

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp

  lcCompName = cCom_Name             && Company Name.
  lcCompPhon = cCom_Phon             && Company Phone.
  lcPhonPict = gfPhoneTem()          && Company Phone Picture Format.

  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  laCompAdd[6] = 'Phone# : '+ TRANSFORM(lcCompPhon , lcPhonPict)
  
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
ENDIF
= ACOPY(laCompAdd,laTmp) 
DO 'RMADD.SPR'

*------------------------------------------------------------------------
*!*************************************************************
*! Name        : lfRepFlag
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Modefies the printed flag to 'P' If device not SCREEN
*!*************************************************************
*! Called from : RMRAUTB.FRX , RMRAUTA.LBX
*!*************************************************************
*! Calls       : NONE
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfRepFlag()
*!*************************************************************

FUNCTION lfRepFlag

IF gcDevice <> 'SCREEN'
  REPLACE Flag WITH 'P'
ENDIF
*------------------------------------------------------------------------
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


FUNCTION lfRecCNt

IF lnLast = lnCurrRec
  lnCurrRec = 0
  *lnPageLns = lnPageLns + 28
ELSE
  lnCurrRec = lnCurrRec + 1
  *lnPageLns = lnPageLns - 1
ENDIF
*IF lnPageLns = 0
*  lnPageLns = 28
*ENDIF
RETURN ''
*!*************************************************************
*! Name        : lfHeader
*! Developer   : Hossam El Etreby
*! Date        : 04/08/1998
*! Purpose     : Function to fill the approparate data for report header.
*!*************************************************************
*! Called from : RMRAUTB.FRX [Header Band]
*!*************************************************************
*! Calls       : FUNCTION => gfRltFld()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfHeader()
*!*************************************************************
*! Due to bug 802090,1 Print labels according to no. of cartons
*! Called only from RMRAUTA.LBX
*!*************************************************************

FUNCTION lfLBHeader
PRIVATE lnPrevAl

IF lcCode <> RETAUTH.RANO
  lcCode = RETAUTH.RANO
  lnCurLbl = 0
ENDIF

lnCurLbl = lnCurLbl + 1

= gfRltFld(RETAUTH.CDivision , @laDivLName , 'CDIVISION')  && Get the division long name.
lcReason = gfCodDes(Retauth.reason,'REASON')
* fill Sold To address array
lnPrevAl = SELECT(0)
SELECT CUSTOMER
IF !EOF()
GOTO RECNO()
  SELECT(lnPrevAl)
ENDIF
laSoldTo[1] = CUSTOMER.STName

laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , IIF(Customer.Type='S','2',''))
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , IIF(Customer.Type='S','2',''))
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , IIF(Customer.Type='S','2',''))
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , IIF(Customer.Type='S','2',''))
laSoldTo[6] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , IIF(Customer.Type='S','2',''))
laSoldTo[7]  = TRANSFORM(Customer.Phone1,lcPhonPict)

*C121117,1  TMI [Start] Update the fax # value
lcFaxNo = CUSTOMER.FAX
*C121117,1  TMI [End  ] 

laWareAdd[1]= gfGetAdr('WAREHOUS' , '' , '' , '' , 1)
laWareAdd[2]= gfGetAdr('WAREHOUS' , '' , '' , '' , 2)
laWareAdd[3]= gfGetAdr('WAREHOUS' , '' , '' , '' , 3)
laWareAdd[4]= gfGetAdr('WAREHOUS' , '' , '' , '' , 4)
laWareAdd[5]= gfGetAdr('WAREHOUS' , '' , '' , '' , 5)
laWareAdd[6]= TRANSFORM(WareHous.cphone , lcPhonPict)

= lfAdrShift('laWareAdd')
= lfAdrShift('laSoldTo')    && Shift Sold To address if there is empty line.
RETURN .T.


*!*************************************************************
*! Name        : lfPrepHDR
*! Developer   : Hossam El Etreby
*! Date        : 04/27/1998
*! Purpose     : Function to craete a dummy file to be used to make
*!               a 1 to M relation according to cartos #
*!*************************************************************
*! Called from : RMRAUT.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfPrepHDR()
*!*************************************************************
*! Due to bug 802090,1 Print labels according to no. of cartons
*! Called only from RMRAUTA.LBX
*!*************************************************************

FUNCTION lfPrepHDR

lcDummyRel = gfTempName()
**-- Collect Styles abailable for these labels
*E301332,1 [Start] If this label is Format 'B' Create a new temp File
*                  Containes 8 Styles from the lines
IF lcFrmFrx = 'Y'
  lcDummySty = gfTempName()
  CREATE TABLE (gcWorkDir+lcDummySty) (Rano C(6) , Style1 C(19) , TotQty1 N(7),;
                Style2 C(19) , TotQty2 N(7),;
                Style3 C(19) , TotQty3 N(7),;
                Style4 C(19) , TotQty4 N(7),;
                Style5 C(19) , TotQty5 N(7),;
                Style6 C(19) , TotQty6 N(7),;
                Style7 C(19) , TotQty7 N(7),;
                Style8 C(19) , TotQty8 N(7))
                
  INDEX ON RANO TAG (lcDummySty)
ENDIF
*E301332,1 [End]

CREATE TABLE (gcWorkDir+lcDummyRel) (Rano C(6))
SELECT (lcDummyRel)
INDEX ON RANO TAG (lcDummyRel)
SELECT RETAUTH
GO TOP
SCAN FOR &lcRpExp
  WAIT WINDOW 'Preparing Labels Data' NOWAIT
  FOR lnLoop = 1 TO RETAUTH.CARTONS
    INSERT INTO (lcDummyRel) VALUES (RETAUTH.RANO)
  ENDFOR
  *E301332,1 [Start] If this label is Format 'B' Fill a new temp File
  *                  with 8 Styles from the lines
  
  IF lcFrmFrx = 'Y'
    SELECT (lcDummySty)
    APPEND BLANK
    REPLACE RANO WITH RETAUTH.RANO
    SELECT RALINE
    lnStyLoop = 0
    =SEEK(RETAUTH.RANO)
    SCAN REST WHILE RANO = RETAUTH.RANO
      lnStyLoop = lnStyLoop + 1
      IF lnStyLoop > 8
        EXIT
      ENDIF
      lcStyLoop = ALLTRIM(STR(lnStyLoop))
      SELECT (lcDummySty)
      REPLACE STYLE&lcStyLoop  WITH RALINE.STYLE,;
              TotQty&lcStyLoop WITH RALINE.TOTQTY
    ENDSCAN
  ENDIF
  *E301332,1 [End]
ENDSCAN

SET RELATION TO rano INTO (lcDummyRel) ADDITIVE
SET SKIP TO (lcDummyRel)
*E301332,1 [Start] If this label is Format 'B' Establish
*                  a relation with RETAUTH to point to
*                  desired styles in the temp file
IF lcFrmFrx = 'Y'
  SELECT RETAUTH
  SET RELATION TO rano INTO (lcDummySty) ADDITIVE
ENDIF
*E301332,1 [End]
*!*************************************************************
*! Name        : lfClrTemp
*! Developer   : Hossam El Etreby
*! Date        : 04/27/1998
*! Purpose     : FUNCTION to clear temporary file created by lfPrepHDR
*!*************************************************************
*! Called from : RMRAUT.PRG
*!*************************************************************
*! Calls       : 
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return            : Null
*!*************************************************************
*! Example           : = lfClrTemp()
*!*************************************************************
*! Due to bug 802090,1 Print labels according to no. of cartons
*! Called only from RMRAUTA.LBX
*!*************************************************************

FUNCTION lfClrTemp

SELECT RETAUTH
SET SKIP TO
SET RELATION TO
USE IN (lcDummyRel)
ERASE (gcWorkDir + lcDummyRel + '.*')
*E301332,1 [Start] If this label is Format 'B' Erase the generated temp File
IF lcFrmFrx = 'Y'
  USE IN (lcDummySty)
  ERASE (gcWorkDir + lcDummySty + '.*')
ENDIF
*E301332,1 [end]

*!*************************************************************
*! Name      : lfClearRep
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/28/1998
*! Purpose   : Function that we call when Close the option grid.
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

*C101750,1 [Start] Close INVHDR file if Tocca Format
IF TYPE('lcTocca')='C' AND ALLTRIM(lcTocca) ='Y' AND USED('INVHDR')
  USE IN INVHDR
ENDIF
*C101750,1 [End]


*B603172,1 [Start] added function
*!*************************************************************
*! Name      : lfvNPStat
*! Developer : Sameh Al-Desouki
*! Date      : 02/29/2000
*! Purpose   : Disable Print NotPad option in option grid
*!             if report form is sticker 
*!*************************************************************
*! Called from : RmrAut.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvNPStat()
*!*************************************************************
FUNCTION lfvNPStat
*-- Get the position of the print cost setting in the array to enable or 
*-- disable it.
lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'llRpNotes'),1)
laOGObjCnt[lnPos] = IIF(lcRpType = 'S',.F.,.T.)
= lfOGShowGet('llRpNotes')

*-- End of lfvNPStat.

*B603172,1 [End]