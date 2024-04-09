*:***************************************************************************
*: Program file  : Soorcn
*: Program desc. : Order Confirmation
*: For Report    : SOORCNA.FRX
*: System        : Aria Advantage Series.
*: Module        : Sales Order (SO)
*: Developer     : Mohamed Atia Badran (MAB)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : gfItemMask,gfPhoneTem,gfTempName,gfGetAdr,gfDispRe ,
*:               : gfRltFld, gfCodDes,gfADel , gfGetMemVar,gfOptMsg,gfBrows.
*:               : lfGetLogo,lfAdrShift,lfSolSpAdr,lfHeadVar,lfGetNotes,
*:               : lfNoteHead,lfNoteData,lfEndGroup,lfwRepWhen,lfFormName,
*:               : lfvOptMsg,lfwOldVal,lfvOrder,lfClearRep,lfsChOrder
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : All IF llFrTime Blocks executed one time in the option grid seasson.
*:         : and if user selection opens any temp. files that may be used later
*:         : in another selection I take this file open to use untill user choice
*:         : is to press < Close > button, to save more time. 
*:***************************************************************************
*: Example : DO Soorcn
*:***************************************************************************
*: This Program is due to E300835 ...
*
*: Modifications:
*:C101403,1 ADEL  02/23/1999 Call any form (if any) not to call FRXes only.
*:B802025,1 MAB   03/08/1999 1- Print distribution center address if found...
*:B802025,1 MAB   03/08/1999 2- Page per every store.(SOORCNA.FRX)...
*:E301168,1 HDM   03/14/1999 Add new sort option for order line no.
*:B802071,1 MAB   03/21/1999 Fix total quantity bug.
*:B802260,1 SSE   05/13/1999 Change Transaction# in OG from INLIST select 
*:                           one by one to SELECT RANGES and this required
*:                           to add function called (lfsChOrder) in PRG    
*:B802273,1 MAB   05/17/1999 Print style description from Ordline not from style file.
*:                           Modification was done in SOORCNA.FRX   .
*:C101525,1 ADEL  05/19/1999 Call the optional program function to be used in calling
*:C101525,1                  any external program doing something not found in this program.
*:E500271,1 WAM   06/24/1999 Print EDI temporary orders as well.
*:C101563,1 HDM   07/13/1999 Custom formate for Margaret O'Leary
*:E500271,1 SSE   07/14/1999 Add popup in OG to filter for the flag field named
*:E500271,1                  LEDIORDER which indicate whether this Order is coming
*:E500271,1                  from EDI or not 
*:B603087,1 WAB   08/16/1999 fix the bug (Operator/operand type mismatch) when choose all in order type
*:B603138,1 ALB   08/30/1999 Fix the bug of not being able to locate the order 
*:B603138,1                  even if it's valid.
*:B603196,1 ABD   10/18/1999 Fix the bug of Complete date doesn't print for each line
*:B603196,1                  in Order confirmation report & fix it in the (frx) report
*:B603196,1                  and this required to add  a record in sysfiles\syrepuvr.
*:B603261,1 RAMY 11/02/1999 Add print the order with status 'BID' and make the wanted changes
*:B603261,1                  to the sydreprt file (change the hidden filter) and the syrepuvr
*:B603261,1                  file (change the expersion used in the hidden filter)
*:E301343,1 RAMY 11/07/1999 Convert the FRX to text format
*:B603287,1 Adel 11/18/1999  Fix Invalid Path or file name when running any custom form
*:B802772,1 WAM  11/15/1999 Print Order Recap for EDI orders.
*:B802642,1 ABD  12/04/1999  Fix bug If the line Begin With '*'  Don't Print It. 
*:B802772,4 AMM  12/06/1999 Print the recap form in case of EDI orders
*:B802910,1 BWA  12/30/1999 Fix the bug of not printing the notepad.
*:E500320,1 NAD  01/19/2000 Print the company Fax No.  
*:B802987,1 HDM  01/24/2000 Don't print notes if it starts with '*'
*:B603405,1 WAM  01/26/2000 Print recap order report always for EDI orders
*:B802887,1 BWA  02/29/2000 Fix the bug If the Merchandise discount is zero it shouldn't print on the
*:B802887,1                 Order Confirmation Report, it should only show when there is an amount.[Fix in the FRX]
*:B803143,1 ABD  03/22/2000 Fix bug in option called PRINT when you click to select EDI
*:B803143,1 ABD             Temp Order to be printed it comes with an error 
*:B803143,1 ABD             Message 'SUBSCRIPT OUT OF BOUNDS'.
*B603579,1 ABD 04/13/2000 Fix bug that Didn't print correct CUSTPO in case of Multi PO.
*B603579,1 ABD            This need to change in the FRX Only.
*B803140,1 WMA 04/23/2000 The report print empty notes boxes
*E301401,1 ABD 06/07/2000 For EDI Temporary Orders, change price columns to labelled
*E301401,1 ABD            "Transmitted Price" and Amount to labelled "Wholesale Price," 
*E301401,1 ABD            Both on-screen and in outputs. [Modify in FRX]
*B803622,1 MHM 08/28/2000  Fix the bug of disable the setting of (print  order/cont.... ) instead
*B803622,1 MHM             of disable the setting of print order line notes.
*B803690,1 AMM 09/21/2000 Add a new status to the modified orders from EDI
*B803697,1 MHM 09/28/2000 Fix the bug of a delay in the order confirmation program in 
*B803697,1 MHM            case of select orders From EIL10
*B603285,1 MHM 10/03/2000 Fix the bug of flickers in OG screen (we change print option in syrepuvr 
*B603285,1 MHM            from lfvType() to lfvType() WHEN lfwOldVal() in Mvald_str field)
*B603713,11 MHM 10/15/2000 increase the length of the fields (Gros_Price ,Price, 
*B603713,11                 PriceA , PriceB , PriceC , Amount) With 2 digit in FRX form A 
*B803779,1 ABD 10/30/20000 Fix bug that the All  SO's printed the # 'X40LHC7A.CustPO' which is 
*B803779,1 ABD             Nothing like the PO# that was entered. It fix in Frx Forma "A".
*E301517,1 SSE 12/21/2000 Add a new standard Form called Form C which is found in A26 as Form B
*C102351,1 ADEL 07/12/2001 Clear read after selecting orders to enable Select Stores for GMA.
*B604784,1 SSE 08/29/2001 Fix bug of Selecting Stores if we select one Multi Store Order.  
*B804441,1 MHM 10/25/2001 Fix bug of wrong ShipVia in Multi Store Order.  
*C102512,1 MHM 02/18/2002 Add Sort by pack in GMA form
*C200343,1 RAE 05/12/2002 Print Pack ID without Detail in case of Print Pack Detail is "No".
*C102680,1 RAE 08/27/2002 Add new sort option (Location) to the Sort filter.
*B606583,1 BWA 12/15/2002 initial variable to check if print the report 
*                          from the main or from the optional program.
*C121868,1 BWA 04/28/2004 Add new trigger for APP10 customer.
*B127921,1 BWA 06/23/2005 Fix the bug of variable "MultiPo" not found when using the recap option.[ Add the recap record in the SYFRMCDH.DBF.]
*B131656,1 AHU 04/11/2006 Add the currency sign to Ammount in case of multy currency to Sales Order Confirmation Form A.
*:***************************************************************************
lcTime     = TIME()                     && Variable to hold the Time
lnLastRec  = 0                          && Record No. Of last record in order group.
lcStore    = ''
lcDivLName = ''
*-- Note Variables [begin]
lcOrdsNum=''
lcTitle    = ''                         && Title of Note. 
lcNotes    = ''                         && Notes.
llPrintBox = .F.                        && Box around Notes.

*C121868,1 BWA 04/28/2004 Add this variable in the program and delete it's record from the syrepuvr.[START]
llPrtCmpdt = gfGetMemVar('M_CMPDOLN')  && Checking Comp. Date by Order line. used in the complete date in the frx form A.
*C121868,1 BWA 04/28/2004.[END]

*B606583,1 BWA 12/15/2002 initial variable to check if print the report 
*                        from the main or from the optional prorgram.[START]
llSalsOrd = .T.          && llarpinv it will be a commen variable.
*B606583,1 BWA 12/15/2002 [END]

*-- Note Variables [end]
*B802772,4 AMM Restore original form setting
IF !llFrTime
  lcFormName = lfGetForm()
  lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),gfTempName(),lcOgTmpForm)
  =gfCrtFrm(lcFormName,lcOGFormArr,llOGRefForm)  && Create Temp. file for new form.
ENDIF
*B802772,4 AMM end
*B131656,1 AHU 04/11/2006 Add the currency sign to Ammount in case of multy currency to Sales Order Confirmation Form A. [START]
llMulCurr = gfGetMemVar('llMulCurr',gcAct_Comp)
lcCurrPost = "LEFT "                              && Default Value.
IF llMulCurr
  IF !USED('SYCINT')
    =gfOpenFile(gcsyshome + "SYCINT" , "CCONTCODE" , 'SH')
  ENDIF
  =SEEK(SycComp.cCont_Code,'SYCINT')
  lcCurrPost = SycInt.cCurrency

  IF !USED('SYCCURR')
    =gfOpenFile(gcsyshome + "SYCCURR" , "CCURRCODE" , 'SH')
  ENDIF
ENDIF
*B131656,1 AHU [END]

*C101403,1 (Begin) Get the form name.
llNoRec= .T.
lcPrgName  = lcFormName
llIsAparel = lfIsApparl(@lcPrgName)
*C101403,1 (End)

llEndGroup = .F.                        && Flag to know if we are at the end of the Group
llPrntBoth = llRpOrdLnt AND llRpOrdNot  && Flag to know we print both line notes and notepad.
llAprvFlag = .F.                        && Flag to print approval 
llTitle    = .T.                        && Flag to print Detail header.

lcTmpIndx = 'ACCOUNT + STORE + CTERMCODE + SHIPVIA + CUSTPO + DTOS(COMPLETE)'

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
*--E301168 HDM create temp file for ordline file [Start]
lcTempOrd = gfTempName()
*--E301168 HDM create temp file for ordline file [End]
*--- Relation between opened files [begin]
*-- Note that the files was opened in Rep. Gen.
SELECT ORDHDR
SET RELATION TO cordtype+ order INTO Ordline ADDITIVE

*--E301168 HDM create temp file for ordline file [Start]
DIMENSION laFileStru[1]
SELECT ORDLINE
= AFIELDS(laFilStruc)

*-- HDM C101563,1 [Start] Check if Mrgret O'Leary Custom form Add Some New Fields To the Temp. File
IF TYPE('lcMargFrm')='C' AND lcMargFrm = 'Y'
  STORE .T. TO llMargFrm
  =lfUsrVldFn('lfAddFlds','SOORCNMO')
ELSE
  STORE .F. TO llMargFrm
ENDIF
*-- HDM C101563,1 [End]

CREATE TABLE (gcWorkDir+lcTempOrd) FROM ARRAY laFilStruc
*B802772,1 Temporary file to Print Order Recap for EDI orders.
IF lcRpOrdTyp = "T"
  lcTempRcp = gfTempName()   && Temp file to collect data for recap
  lcTempRpt = gfTempName()   && Temp file to collect DOS resords for recap report

  CREATE TABLE (gcWorkDir+lcTempRcp) (Account C(5),Style C(19),Qty1 N(7),Qty2 N(7),;
  Qty3 N(7),Qty4 N(7),Qty5 N(7),Qty6 N(7),Qty7 N(7),Qty8 N(7),TotQty N(8), Amount N(13,2),;
  nSRP N(8,2),Scale C(3),note1 c(30),note2 c(20))
  INDEX ON Account+Style TAG (lcTempRcp)
ENDIF
*B802772,1 (End)

SELECT (lcTempOrd)
IF llMargFrm
  INDEX ON &lcTmpIndx TAG (lcTempOrd)
ELSE
  IF lcRpSortBy = 'S'
    INDEX ON CORDTYPE + ORDER + STORE + STYLE TAG (lcTempOrd)
    SET ORDER TO TAG (lcTempOrd)
  ELSE
    INDEX ON CORDTYPE + ORDER + STORE + STR(LINENO,6) TAG (lcTempOrd)
    SET ORDER TO TAG (lcTempOrd)
  ENDIF
ENDIF
*APPEND FROM (gcDataDir + 'ORDLINE.dbf') FOR &lcRpExp
SELECT ORDHDR

*MAB at 07/14/1999 Add Status to filter [Begin]
*B603261,1 RAMY Mark this line becuase of adding the status to the lcRpExp from the OG[start]
*IF !EMPTY(lcRpExp)
 *lcRpExp = lcRpExp + [ AND ]
*ENDIF
*lcRpExp = lcRpExp + [!(ORDHDR.Status $ "BX")]
*B603261,1 RAMY [end]
*MAB at 07/14/1999 [End  ]

*E500271,1 add the default LEDIORDER = .F. to the filter expression [Begin.]
lcEdiExpr = IIF(lcRpEDIFlt='O',[!ORDHDR.lEDIOrder],IIF(lcRpEDIFlt='E',[ORDHDR.lEDIOrder],""))
IF !EMPTY(lcEdiExpr)
  IF !EMPTY(lcRpExp)
    lcRpExp = lcRpExp + [ AND ]
  ENDIF
  lcRpExp = lcRpExp + lcEdiExpr
ENDIF
*E500271,1 add the default LEDIORDER = .F. to the filter expression [End.]

SCAN FOR &lcRpExp
  SELE ORDLINE
  SCAN REST WHILE CORDTYPE+ORDER = ORDHDR.CORDTYPE+ORDHDR.ORDER
    SCATT MEMVAR MEMO
    *-- HDM C101563,1 [Start] Check if Mrgret O'Leary Custom form Replace the New Fields on the Temp. File
    IF llMargFrm
      =lfUsrVldFn('lfRepFlds','SOORCNMO')
    ENDIF
    *-- HDM C101563,1 [End]
    INSERT INTO (lcTempOrd) FROM MEMVAR
    *B802772,1 Print Order Recap for EDI orders.
    IF lcRpOrdTyp = "T"
      SELECT (lcTempRcp)
      IF !SEEK(m.Account+m.Style)
        INSERT INTO (lcTempRcp) (Account,Style,nSRP,Scale) VALUES ;
                                (m.Account,m.Style,m.nSugRetPri,m.Scale)
      ENDIF
      REPLACE Qty1   WITH Qty1   + m.Qty1 ,;
              Qty2   WITH Qty2   + m.Qty2 ,;
              Qty3   WITH Qty3   + m.Qty3 ,;
              Qty4   WITH Qty4   + m.Qty4 ,;
              Qty5   WITH Qty5   + m.Qty5 ,;
              Qty6   WITH Qty6   + m.Qty6 ,;
              Qty7   WITH Qty7   + m.Qty7 ,;
              Qty8   WITH Qty8   + m.Qty8 ,;
              TotQty WITH TotQty + m.TotQty ,;
              Amount WITH Amount + m.TotQty*m.Price
    ENDIF
    *B802772,1 (End)

  ENDSCAN
ENDSCAN
*-- HDM C101563,1 [Start] Check if Mrgret O'Leary Custom form Replace the New Fields on the Temp. File
IF llMargFrm
  =lfUsrVldFn('lfChcFlds','SOORCNMO')
ENDIF
*-- HDM C101563,1 [End]

*--E301168 HDM create temp file for ordline file [End]
GO TOP
SELECT ORDHDR
SET RELATION OFF INTO ORDLINE

SELECT ORDHDR
*SET RELATION TO cordtype+ order INTO Ordline ADDITIVE
SET RELATION TO cordtype+ order INTO (lcTempOrd) ADDITIVE

SET RELATION TO cwarecode INTO Warehous ADDITIVE
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
*SELECT ORDLINE
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

  *-- Get company Address [begin].
  SELECT SYCCOMP
  SEEK gcAct_Comp
  lcCompName = cCom_Name
  lcCompPhon = cCom_Phon             && Variable to hold the Company Phone
  *----E500320,1 NAD [Begin] Get the company fax no. to print it 
  lcCompFax  = cCom_fax              && Variable to hold the Company Fax
  *----E500320,1 NAD [END]
  lcPhonPict = gfPhoneTem()          && Variable to hold the Company Phone Format
  *-- Load Company address.
  laCompAdd[1] = gfGetAdr('SYCCOMP' , '' , '' , '' , 1)
  laCompAdd[2] = gfGetAdr('SYCCOMP' , '' , '' , '' , 2)
  laCompAdd[3] = gfGetAdr('SYCCOMP' , '' , '' , '' , 3)
  laCompAdd[4] = gfGetAdr('SYCCOMP' , '' , '' , '' , 4)
  laCompAdd[5] = gfGetAdr('SYCCOMP' , '' , '' , '' , 5)
  = lfAdrShift('laCompAdd')    && Shift Company address if there is empty line.
  *-- Get company Address [end].
ENDIF

*lcSkipExpr = [Ordline]    && Expression we skip to later.
lcSkipExpr  = [&lcTempOrd]

*-- lcNoteLns : Name of Temp. Loop File which is used to print both line notes 
*--           : and notepad from notepad file.
*--           : note that this name and temp. file is created 
*--           : one for every optional grid seasson run.
lcNoteLns = IIF(EMPTY(lcNoteLns),gfTempName(),lcNoteLns)

*-- if you don't find temp. file, create it if you have both types of notes.
IF !USED(lcNoteLns) AND llPrntBoth
  CREATE CURSOR (lcNoteLns)  (cRecord C(2))
  INDEX ON cRecord TAG (lcNoteLns) OF (gcWorkDir+lcNoteLns)
  FOR lnI = 1 TO 2
    APPEND BLANK
   REPLACE cRecord WITH "N"+ALLTRIM(STR(lnI))
  ENDFOR
ENDIF

IF llPrntBoth
  *SELECT ORDLINE
  SELECT (lcTempOrd)
  SET RELATION TO 'N' INTO (lcNoteLns) ADDITIVE
  *lcSkipExpr = [Ordline,&lcNoteLns]
  *ramy
  *lcSkipExpr = [&lcTempOrd,&lcNoteLns]
  
  *:B802910,1 BWA  12/30/1999 Fix the bug of not printing the notepad.[START]
  *lcSkipExpr = [&lcTempOrd]
  lcSkipExpr = [&lcTempOrd,&lcNoteLns]
  *:B802910,1 BWA  12/30/1999 [END]
ENDIF

*-- if we are in case of print both types of notes.  [begin]

*-- Select Master report file.
SELECT ORDHDR
SET SKIP TO &lcSkipExpr

*lcRepExpr = [IIF(llPrntBoth,IIF(&lcNoteLns..cRecord = 'N2',RECNO('ORDLINE') = lnLastRec ,.T.),.T.)]
lcRepExpr = [IIF(llPrntBoth,IIF(&lcNoteLns..cRecord = 'N2',RECNO(lcTempOrd) = lnLastRec ,.T.),.T.)]

lcRpExp   = IIF(EMPTY(lcRpExp),lcRepExpr,lcRpExp + [ AND ] + lcRepExpr)
*C101525,1 (Begin) Add the optional program function to be used in getting 
*C101525,1         anything special not found in this program(soorcn).
*B603287,1 Comment out, it's already called before 
*llIsAparel = lfIsApparl(@lcPrgName)

=lfOptProg()
*-- SSH C101563,1 (START) Dont print some fields incase of cons. order
IF llMargFrm
  =lfUsrVldFn('lfDntPrn','SOORCNMO')
ENDIF

*C101525,1 (End)
*C101403,1 (Begin) Remark the following line and call the fit form.
*DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp

*B603405,1 Do not print order confirmation in case of print recap only
IF lcRpOrdTyp <> "T" OR !llRcpOnly 
*B603405,1 (End)

  IF llIsAparel
    =gfSetDevice()
    DO EVAL('lcPrgName')
    IF !llNoRec
      DO ENDREPORT
    ENDIF
  ELSE
    *--
    lcRpExp = lcRpExp + ' AND !EOF(lcTempOrd)'
    *--

    *B606583,1 BWA 12/15/2002 Check if print the report from the main program or the optional prorgram.[START]
    *  DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
    IF llSalsOrd
      DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
    ENDIF
    *B606583,1 BWA 12/15/2002 [END]

  ENDIF  
*B603405,1 Do not print order confirmation in case of print recap only
ENDIF
*B603405,1 (End)

*B802772,4 AMM Get recap form name and restore its setting
*B803143,1 ABD Select ordhdr and release the relation.[Begin]
SELECT ORDHDR
*B803143,1 ABD [End]

SET RELATION TO

*B603405,1 print recap order report always for EDI order 
*IF lcRpOrdTyp = "T" .AND. llRcpOnly .AND. !EOF(lctemprcp)
IF lcRpOrdTyp = "T"
*B603405,1 (End)
  lcRCFrmNam = lfGetForm('RC')
  lcPrgName  = lcRCFrmNam
  llIsAparel = lfIsApparl(@lcPrgName)
  =lfOptProg()
  SELECT (lctemprcp)
  SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK_A ADDITIVE
  SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
  SET RELATION TO Style INTO Style ADDITIVE
  SET RELATION TO 'M'+Account INTO Customer ADDITIVE
  lcOldForm = lcOGTmpForm
  IF llIsAparel
    =gfSetDevice()
    DO EVAL('lcPrgName')
    IF !llNoRec
      DO ENDREPORT
    ENDIF
  ELSE
    lcOgTmpForm = IIF(EMPTY(lcOgTmpForm),gfTempName(),lcOgTmpForm)
    =gfCrtFrm(lcRCFrmNam,lcOGFormArr,llOGRefForm)  && Create Temp. file for new form.
    SELECT (lctemprcp)
    GO TOP
    DO gfDispRe WITH EVAL('lcRCFrmNam')
  ENDIF 
  lcOGTmpForm = lcOldForm
ENDIF
*B802772,4 AMM end


*C101403,1 (End)
IF USED(lcTempOrd)
  SELECT(lcTempOrd)
  USE
  ERASE (gcWorkDir+lcTempOrd+'.DBF')
  ERASE (gcWorkDir+lcTempOrd+'.CDX')
ENDIF
*-- end of Report code.


*-- Function section 
*-------------------------------------------
*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
*-- end of lfGetLogo.

*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
*-- end of lfAdrShift.

*!*************************************************************
*! Name        : lfHeadVar
*! Developer   : Mohamed Badran (MAB)
*! Date        : 03/25/1998
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
*-- end of lfHeadVar.

*!*************************************************************
*! Name      : lfSolSpAdr
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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

*C101525,1 (Begin) Save the current alias.
lnSavAlias = SELECT(0)
*C101525,1 (End)

*lcStore = OrdLine.Store
lcStore = &lcTempOrd..Store

= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

lcShipVia = gfCodDes(ORDHDR.ShipVia , 'SHIPVIA'  )
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')

SELECT CUSTOMER
IF ORDHDR.MULTI = 'Y'
  *= SEEK('S' + OrdLine.Account + OrdLine.Store , "CUSTOMER")
  = SEEK('S' + &lcTempOrd..Account + &lcTempOrd..Store , "CUSTOMER")
  
  *B804441,1 MHM 10/25/2001 Get ShipVia at store level [Start]
  IF ALLTRIM(ORDHDR.ShipVia) = '*'
    lcShipVia = gfCodDes(CUSTOMER.ShipVia , 'SHIPVIA'  )
  ENDIF  
  *B804441,1 MHM [End]
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

  *B802025,1 Print distribution center address if found (1) Begin
  IF !EMPTY(CUSTOMER.DIST_CTR)  
    lcCurrKey = 'S' + Customer.Account + Customer.Store
    =SEEK('S' + Customer.Account + CUSTOMER.DIST_CTR , 'CUSTOMER')
    lcStore = lcStore + '  Dist. Center : ' + Customer.Store
  ENDIF
  *B802025,1 Print distribution center address if found (1) End

  lcShpTName = IIF(EMPTY(DBA) , STName , DBA)
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)

  *B802025,1 Print distribution center address if found (2) Begin
  IF TYPE('lcCurrKey') = 'C'
    = SEEK(lcCurrKey , 'CUSTOMER')
  ENDIF
  *B802025,1 Print distribution center address if found (2) End

ENDIF    && End of IF
= lfAdrShift('laShipTo')  && Shift Ship To address if there is empty line.
*C101525,1 (Begin)
SELECT (lnSavAlias)
*C101525,1 (End)
*-- end of lfSolSpAdr.


*!*************************************************************
*! Name        : lfGetNotes
*! Developer   : Mohamed Badran (MAB)
*! Date        : 03/25/1998
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
*STORE '' TO lcNotes, lcTitle

*B802987,1 [Start] get the notes title only if we will print a notes
*lcTitle    = lfNoteHead()     && Title of the note (Line Note OR NotePad).
*B802987,1 [End]
lcNotes    = lfNoteData()     && Note Data.

*B802987,1 [Start]
IF !EMPTY(lcNotes)
  lcTitle    = lfNoteHead()     && Title of the note (Line Note OR NotePad).
*B803140,1 WMA if notes field is empty, store space in var lcTitle [Start]
ELSE
  lcTitle    =""
*B803140,1 WMA [End]
ENDIF
*B802987,1 [End]

llPrintBox = !EMPTY(lcTitle)  && If it's .T. Report Print box around notes.
*llTitle    = RECNO('ORDLINE') != lnLastRec
llTitle    = RECNO(lcTempOrd) != lnLastRec
RETURN ''
*-- end of lfGetNotes.

*!*************************************************************
*! Name        : lfNoteHead
*! Developer   : Mohamed Badran (MAB)
*! Date        : 03/25/1998
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
      *CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(ORDLINE.Note_Mem))
       CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        *lcNoteHead = 'Line ' + ALLTRIM(STR(OrdLine.LineNo)) + ' Notepad'
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'
        
      *E500271,1 Print EDI temporary orders
      *CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))
           
      *E500271,1 (End)
      
        *lcNoteHead = IIF(RECNO('ORDLINE') = lnLastRec
        
        *E500271,1 Print EDI temporary orders
        *lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType = 'O','Order Notepad','Contract Notepad'),'')
        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType='O','Order Notepad',IIF(OrdHdr.cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')
        *E500271,1 (End)

    ENDCASE
  ELSE && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
      *CASE llRpOrdLNt AND !EMPTY(ALLTRIM(ORDLINE.Note_Mem))
      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
      
        *lcNoteHead = 'Line ' + ALLTRIM(STR(OrdLine.LineNo)) + ' Notepad'
        lcNoteHead = 'Line ' + ALLTRIM(STR(&lcTempOrd..LineNo)) + ' Notepad'

      *E500271,1 Print EDI temporary orders
      *CASE llRpOrdNot AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))

      CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

      *E500271,1 (End)
        *lcNoteHead = IIF(RECNO('ORDLINE') = lnLastRec
        
        *E500271,1 Print EDI temporary orders
        *lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType = 'O','Order Notepad','Contract Notepad'),'')

        lcNoteHead = IIF(RECNO(lcTempOrd) = lnLastRec,;
        IIF(OrdHdr.cOrdType='O','Order Notepad',IIF(OrdHdr.cOrdType='T','EDI Temporary Order Notepad','Contract Notepad')),'')
        *E500271,1 (End)
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteHead
*-- end of lfNoteHead

*!*************************************************************
*! Name        : lfNoteData
*! Developer   : Mohamed Badran (MAB)
*! Date        : 03/25/1998
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
*B802642,1 ABD Define Variable To Hold Line Notes.
lcPrntNote = ''
*B802642,1 end
*-- If you have order lines.
IF ORDHDR.LastLine > 0
  *-- if you print both notes.
  IF llPrntBoth
    *-- Note that the following Scheme
    *-- ....... cRecord = 'N1' ............. Line Notepad.
    *-- ....... cRecord = 'N2' ............. Order or Contract Notepad.
    DO CASE
      *CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(ORDLINE.Note_Mem))
      CASE &lcNoteLns..cRecord = 'N1' AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
        *lcNoteData  =  ALLTRIM(ORDLINE.Note_Mem)

        *B802987,1 [START] Print only lines that doesn't start with *
        *lcNoteData  =  ALLTRIM(&lcTempOrd..Note_Mem)

        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote

        *B802987,1 [End]

        
      *E500271,1 Print EDI temporary orders
      *CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      
      CASE &lcNoteLns..cRecord = 'N2' AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      *E500271,1 (End)

        *lcNoteData  = IIF(RECNO('ORDLINE') = lnLastRec,ALLTRIM(NotePad.mNotes),'')
        *B802642,1 ABD if the line Begin With '*'  Don't Print It. [ Begin ]
        *lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(NotePad.mNotes),'')
        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
        *B802642,1  ABD [ end ]
    ENDCASE
  ELSE  && Else You print either Line or Order/contract Notepad.
    *-- Note that the following Scheme
    *-- ....... llRoOrdLnt ............. Line Notepad.
    *-- ....... llRoOrdNot ............. Order or Contract Notepad.
    DO CASE
      *CASE llRpOrdLNt AND !EMPTY(ALLTRIM(ORDLINE.Note_Mem))

      CASE llRpOrdLNt AND !EMPTY(ALLTRIM(&lcTempOrd..Note_Mem))
        *lcNoteData  =  ALLTRIM(ORDLINE.Note_Mem)

        *B802987,1 [START] Print only line that doesn't start with *
        lcNoteData  =  ALLTRIM(&lcTempOrd..Note_Mem)
        For lnNotLine = 1 To MEMLINES(&lcTempOrd..Note_Mem)
          lcCurrLine = ALLTRIM(MLINE(&lcTempOrd..Note_Mem,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = lcPrntNote
        *B802987,1 [End]

      *E500271,1 Print EDI temporary orders
      *CASE llRpOrdNot AND SEEK('B' + Order , 'NOTEPAD') AND !EMPTY(ALLTRIM(NOTEPAD.mNotes))
      CASE llRpOrdNot AND SEEK('B'+IIF(OrdHdr.cOrdType='T','T','')+Order,'NOTEPAD') AND ;
           !EMPTY(ALLTRIM(NOTEPAD.mNotes))

      *E500271,1 (End)
        *lcNoteData  = IIF(RECNO('ORDLINE') = lnLastRec,ALLTRIM(NotePad.mNotes),'')
        *B802642,1 ABD if the line Begin With '*'  Don't Print It. [ Begin ]
        *lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(NotePad.mNotes),'')
        For lnNotLine = 1 To MEMLINES(NOTEPAD.MNOTES)
          lcCurrLine = ALLTRIM(MLINE(NOTEPAD.MNOTES,lnNotLine))
          IF LEFT(lcCurrLine,1) # '*'
            lcPrntNote = lcPrntNote + IIF(EMPTY(lcPrntNote),'',CHR(10)) + lcCurrLine
          ENDIF
        EndFor
        lcNoteData  = IIF(RECNO(lcTempOrd) = lnLastRec,ALLTRIM(lcPrntNote),'')
        *B802642,1  ABD [ end ]
    ENDCASE
  ENDIF
ENDIF
RETURN lcNoteData
*-- end of lfNoteData.

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
*-- end of lfEndGroup.

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfADel() , gfGetMemVar() , lfOGShowGet()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

*:B803622,1 MHM 08/28/2000 [Begin] 
PRIVATE lnPos1
*:B803622,1 MHM 08/28/2000  [End]

*IF The Add notes to order lines seting is set to No
IF gfGetMemVar('M_OR_NOTE') <> 'Y'
  *:B803622,1 MHM 08/28/2000 [Begin]  
  llRpOrdLnt = .F.
  *  laOGObjCnt[2] = .F.
  lnPos1 = ASCAN(laOgObjType,'llRpOrdLnt')
  IF lnPos1 > 0
    lnPos1= ASUBSCRIPT(laOgObjType,lnPos1,1)
    laOGObjCnt[lnPos1] = .F.
    =lfOGShowGet('llRpOrdLnt')  && Disable the Print order line notepad Object
  ENDIF
*:B803622,1 MHM 08/28/2000  [End]
ENDIF    && End of IF

*MAB 01/18/2000 [Begin]
*B802772,4 AMM Start, enable or disable the recap setting due to EDI order or not
*MAB lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRCPONLY'),1)
*MAB laOGObjCnt[lnPos] = lcRpOrdTyp = "T"
*MAB = lfOGShowGet('LLRCPONLY')
lnPos = ASCAN(laOgObjType,'LLRCPONLY')
IF lnPos > 0
  lnPos= ASUBSCRIPT(laOgObjType,lnPos,1)
  laOGObjCnt[lnPos] = lcRpOrdTyp = "T"
  = lfOGShowGet('LLRCPONLY')
ENDIF
*B802772,4 AMM end
*MAB 01/18/2000 [End  ]

SET ORDER TO ORDHDR IN ORDHDR  && To use it to validate orders in option grid.
*-- end of lfwRepWhen.

*!*************************************************************
*! Name      : lfFormName
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Function to get the Form name
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : Form name
*!*************************************************************
*! Example     : = lfFormName()
*!*************************************************************
*C101403,1 Remove this function because it is not used anymore.
*FUNCTION lfFormName

*RETURN 'SORDCONA'
*-- end of lfFormName.

*!*************************************************************
*! Name      : lfvOptMsg
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
*! Name      : lfwOldVal
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
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
laOldVal = EVALUATE(SYS(18))      && Varible to hold the old value
*-- end of lfwOldVal.

*!*************************************************************
*! Name      : lfvOrder
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/25/1998
*! Purpose   : Validation function for the Order field
*!*************************************************************
*! Called from : Order field [Option Grid]
*!*************************************************************
*! Calls       : gfBrows()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvOrder()
*!*************************************************************
FUNCTION lfvOrder

*C121868,1 BWA 04/28/2004 Trigger to be able to show the complete orders.[START]
IF gfDoTriger('SOORCN',PADR('COMPELET',10))
  RETURN
ENDIF
*C121868,1 BWA 04/28/2004.[END]

PRIVATE lcVar , lcObj , laTemp
lcVar = SYS(18)                && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = EVALUATE(SYS(18))      && Varible to hold the current field value

lcObj = IIF(EMPTY(lcObj) .OR. '?' $ lcObj , lcObj , PADL(ALLTRIM(lcObj) , 6 , '0'))

*IF Statment to check if we are going to Browse

*B603261,1 RAMY Change the if condition to add the variable that holds the status filter [start]
*IF !EMPTY(lcObj) AND ('?' $ lcObj OR !IIF(lcRpOrdTyp = 'B'  ,;
   SEEK('O'+lcObj , 'ORDHDR') OR SEEK('C'+lcObj , 'ORDHDR') ,;
   SEEK(lcRpOrdTyp+lcObj , 'ORDHDR')))

IF !EMPTY(lcObj) AND ('?' $ lcObj OR !IIF(lcRpOrdTyp = 'A'  ,;
   SEEK('O'+lcObj , 'ORDHDR') OR SEEK('C'+lcObj , 'ORDHDR') ,;
   SEEK(lcRpOrdTyp+lcObj , 'ORDHDR')))

*B603261,1 RAMY [end]

  SELECT ORDHDR
  DIMENSION laTemp[1]
  laTemp = ''      && Array to hold the Selected value

  *E500271,1 Print EDI temporary orders
  *lcBrFields = "CTYPE = IIF(cOrdType = 'C', 'Contract',IIF(cOrdType = 'O', 'Order',''))"+;
               ":R :H= 'Order Type' , "          +;
               "ORDER     :R :H= 'Order #' , "   +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "SHIPVIA   :R :H= 'ShipVia' ,"    +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "
  *lcFile_Ttl = IIF(lcRpOrdTyp = 'B',"Orders/Contracts",IIF(lcRpOrdTyp = 'O',;
                                    "Orders...","Contracts..."))

  lcBrFields = "CTYPE=IIF(cOrdType='C','Contract',IIF(cOrdType='O','Order',IIF(cOrdType='T','EDI Temp. Order','')))"+;
               ":R :H= 'Order Type' :20, "          +;
               "ORDER     :R :H= 'Order #' , "   +;
               "ACCOUNT   :R :H= 'Account' ,"    +;
               "STORE     :R :H= 'Store' ,"      +;
               "ENTERED   :R :H= 'Entered Date',"+;
               "SEASON    :R :H= 'Season' ,"     +;
               "cDIVISION :R :H= 'Division' ,"   +;
               "CTERMCODE :R :H= 'Terms' ,"      +;
               "SHIPVIA   :R :H= 'ShipVia' ,"    +;
               "STATUS    :R :H= 'Status ' ,"    +; 
               "OPEN      :R :H= 'Open Amt. ',"  +; 
               "BULK      :R :H= 'Bulk' "

  lcFile_Ttl = IIF(lcRpOrdTyp = 'A',"Orders/Contracts",IIF(lcRpOrdTyp='O',"Orders...",;
                   IIF(lcRpOrdTyp='T',"EDI Temporary Orders","Contracts...")))

  *E500271,1 (End)
  
  *E500271,1 Adding the LEDIORDER flag which detect receiving orders from 
  *E500271,1 EDI or not to the browse [Begin.]
  *lcBrowCond = [FOR !(STATUS $ "XB") AND OPEN >= 1 AND ] +;
  *             [ IIF(lcRpOrdTyp = "B",.T.,CORDTYPE = lcRpOrdTyp)]          
  
  *B603261,1 RAMY Change the browse cond to add the status filter [start]
  *lcBrowCond = [FOR !(ORDHDR.STATUS $ "XB") AND OPEN >= 1 AND ] +;
               [ IIF(lcRpOrdTyp = "B",.T.,CORDTYPE = lcRpOrdTyp) AND] +;
               [ IIF(lcRpEDIFlt='O',!ORDHDR.LEDIORDER,IIF(lcRpEDIFlt='E',ORDHDR.LEDIORDER,.T.))]
  
  *B802772,4 AMM Allow open quantity less than zero in EDI orders
  *lcBrowCond = [FOR (IIF(lcRpOrdSta = 'A' , !(ORDHDR.STATUS $ "XC") , ;
               ORDHDR.STATUS = lcRpOrdSta) AND OPEN >= 1 AND ] + ;
               [ IIF(lcRpOrdTyp = "A",.T.,CORDTYPE = lcRpOrdTyp) AND] +;
               [ IIF(lcRpEDIFlt='O',!ORDHDR.LEDIORDER,IIF(lcRpEDIFlt='E' , ;
               ORDHDR.LEDIORDER,.T.)))]
  lcBrowCond = [FOR (IIF(lcRpOrdSta = 'A' , !(ORDHDR.STATUS $ "XC") , ;
               ORDHDR.STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
               ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >= 1) AND ] + ;
               [ IIF(lcRpOrdTyp = "A",.T.,CORDTYPE = lcRpOrdTyp) AND] +;
               [ IIF(lcRpEDIFlt='O',!ORDHDR.LEDIORDER,IIF(lcRpEDIFlt='E' , ;
               ORDHDR.LEDIORDER,.T.)))]
  *B802772,4 AMM end
  
  *B603261,1 RAMY [end]
  *E500271,1 Adding the LEDIORDER flag [End.] 
  
  = gfBrows(lcBrowCond,'ORDER','laTemp')  
  *IF The user selected a record
  IF !EMPTY(laTemp[1])
    lcObj = laTemp[1]
  ELSE    && Else
    lcObj = laOldVal
  ENDIF    && End of IF
  
ENDIF    && End of IF
&lcVar = lcObj      && Update the field
*-- end of lfvOrder.

*!*************************************************************
*! Name      : lfAprvFlag
*! Developer : Mohamed Badran (MAB)
*! Date      : 03/30/1998
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
*-- end of lfAprvFlag.

*!*************************************************************
*! Name      : lfLastRec
*! Developer : Mohamed Badran (MAB)
*! Date      : 04/13/1998
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
*SELECT ORDLINE
SELECT (lcTempOrd)
*lnThRec = RECNO('ORDLINE')    && Save Current record #.
lnThRec = RECNO(lcTempOrd)    && Save Current record #.
lcThStore = Store
LOCATE REST FOR ( cordtype+order+store+style+STR(lineno,6) > OrdHdr.cordtype + OrdHdr.order + lcThStore)
IF (order != OrdHdr.order) OR (store != lcThStore)
  SKIP -1
ENDIF   
*lnLastRec = RECNO('ORDLINE')  && Evaluate record Number of last record in detail lines.
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
*-- end of lfLastRec.

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
*-- Close temp. opended files, if it used.
IF USED(lcLogoPic)
 USE IN (lcLogoPic)
ENDIF

IF USED(lcNoteLns)
 USE IN (lcNoteLns)
ENDIF
*-- end of lfClearRep.

*!*************************************************************
*! Name      : lfsChOrder
*! Developer : Sameh Saiid Ezzat (SSE)
*! Date      : 05/13/99
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
*B802260,1 
*
FUNCTION lfsChOrder
PARAMETERS lcParm

*C121868,1 BWA 04/28/2004 Trigger to be able to show the complete orders.[START]
IF gfDoTriger('SOORCN',PADR('SETRESET',10))
  RETURN
ENDIF
*C121868,1 BWA 04/28/2004.[END]

DO CASE 
  CASE lcParm = 'S'
    SELECT ORDHDR
    *500271,1 modify the set filter to include the Flag LEDIORDER field [Begin.]
    *SET FILTER TO IIF(lcRpOrdTyp = "B",.T.,CORDTYPE = lcRpOrdTyp) AND !(STATUS $ "XB") AND OPEN > 0 
    
    *B603261,1 RAMY Change the filter to add the status filter [start]
    *SET FILTER TO IIF(lcRpOrdTyp = "B",.T.,CORDTYPE = lcRpOrdTyp) AND !(ORDHDR.STATUS $ "XB") AND OPEN > 0 ;
                  AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder,IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))

    *B802772,4 AMM filter on OPEN>0 in cases other than EDI orders 
    *SET FILTER TO IIF(lcRpOrdTyp = "A", .T. , CORDTYPE = lcRpOrdTyp) AND ;
                  IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
                  STATUS = lcRpOrdSta) AND OPEN > 0 AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
                  IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))

    *B803697,1 MHM 09/28/2000 [START]
    *SET FILTER TO IIF(lcRpOrdTyp = "A", .T. , CORDTYPE = lcRpOrdTyp) AND ;
                  IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
                  STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
                  ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
                  IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
    *B802772,4 AMM end
    *B603261,1 RAMY [end]
    *500271,1 modify the set filter to include the Flag LEDIORDER field [End.]                  
    *GO TOP
    
    DO CASE
      *--FIX ALL AND CHANGE lcRpOrdTyp
      CASE lcRpOrdTyp = "A"
         SET FILTER TO IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
           STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
           ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
           IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))

         LOCATE
         
      CASE lcRpOrdTyp = "O" 
         SET FILTER TO (CORDTYPE + ORDER = "O") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         
            LOCATE FOR CORDTYPE+ORDER = "O"

      CASE lcRpOrdTyp = "C"
         SET FILTER TO (CORDTYPE + ORDER = "C") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta) AND  IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         
         LOCATE FOR CORDTYPE+ORDER = "C"

      CASE lcRpOrdTyp = "T" 
         SET FILTER TO (CORDTYPE + ORDER = "T") AND IIF(lcRpOrdSta= 'A', !(ORDHDR.STATUS $ "XC")  ,;
            STATUS = lcRpOrdSta) AND IIF(ORDHDR.cOrdType='T' .AND. ;
            ORDHDR.MON_FLG $ 'LG' ,.T.,OPEN >0) AND IIF(lcRpEDIFlt='O',!ORDHDR.lEdiOrder, ; 
            IIF(lcRpEDIFlt='E',ORDHDR.lEdiOrder,.T.))
         LOCATE FOR CORDTYPE+ORDER = "T"

    ENDCASE                  
    *B803697,1 MHM 09/28/2000 [END]
  CASE lcParm = 'R'
    SELECT ORDHDR
    SET FILTER TO
    llClearSel = .F.
ENDCASE
*B604784,1 SSE [Begin]
*C102351,1 (Begin) Clear Read
*CLEAR READ
*C102351,1 (End)
*B604784,1 SSE [End]
*-- end of lfsChOrder.

*!*************************************************************
*! Name      : lfvType
*! Developer : Mohamed Atia Badran (MAB)
*! Date      : 05/26/99
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
*
FUNCTION lfvType
*500271,1 clear read report variables in order to refresh the screen [Begin.]
*B603285,1 MHM 10/03/2000  [start]
*IF lcRpOrdTyp = "T"
IF lcRpOrdTyp $ "TC"
*B603285,1 MHM 10/03/2000  [End]
  lcRpEDIFlt = "B"
  CLEAR READ 
ELSE
  *:B603087,1 WAB - type mismatch
  *:B603087,1 WAB - START
  *IF laOldVal = 3
  *B603285,1 MHM 10/03/2000  [start]
  *IF laOldVal = '3'
  IF (laOldVal = 3) .OR. (laOldVal = 2)
  *B603285,1 MHM 10/03/2000  [End]
  *:B603087,1 WAB - END
    lcRpEDIFlt = "B"
    CLEAR READ 
  ENDIF  
ENDIF
*500271,1 clear read report variables in order to refresh the screen [End.]
llClearSel = .T.
*B802772,4 AMM Enable or disable the recap setting due to EDI orders or not

*B803143,1 ABD Remark the next lines .[Begin]
*lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'LLRCPONLY'),1)
*laOGObjCnt[lnPos] = lcRpOrdTyp = "T"
*= lfOGShowGet('LLRCPONLY')
*B803143,1 ABD [END]
*B802772,4 AMM end
*-- end of lfvType.


*!*************************************************************
*! Name      : lfModeVld
*! Developer : Ramy Mabrouk
*! Date      : 11/24/99
*! Purpose   : Report Mode Validation
*! Job No.   : E301343,1 
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfModeVld()
*!*************************************************************

FUNCTION lfModeVld
CLEAR READ
*-- end of lfModeVld.

*!*************************************************************
*! Name      : lfvOrdSta
*! Developer : Ramy Mabrouk
*! Date      : 11/24/99
*! Purpose   : Order Status Validation
*! Job No.   : E301343,1 
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

*-- end of lfvOrdSta.

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

RETURN lcRetStat
*!*************************************************************
*! Name      : lfSortDumy
*! Developer : Mohamed Shokry(MHM)
*! Date      : 02/18/2002
*! Purpose   : Fill Sort Arrays.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSortDumy()
*!*************************************************************
*C102512,1

FUNCTION lfSortDumy

IF RIGHT(lcFormName,2) ='GM'
  DIMENSION laSortDesc[3,1] , laSortVal[3,1]
  laSortDesc[1] = 'Style'
  laSortDesc[2] = 'Line Number'
  laSortDesc[3] = 'Pack'

  laSortVal[1] = 'S'
  laSortVal[2] = 'L'
  laSortVal[3] = 'P'
ELSE
  *C102680,1 RAE [Start]
  *  DIMENSION laSortDesc[2,1] , laSortVal[2,1]
  *  laSortDesc[1] = 'Style'
  *  laSortDesc[2] = 'Line Number'

  *  laSortVal[1] = 'S'
  *  laSortVal[2] = 'L'
  
  IF gfDoTriger('SOORCN',PADR('SRTBYLOC',10))
  
  ELSE
    DIMENSION laSortDesc[2,1] , laSortVal[2,1]
    laSortDesc[1] = 'Style'
    laSortDesc[2] = 'Line Number'

    laSortVal[1] = 'S'
    laSortVal[2] = 'L'    
  ENDIF
  *C102680,1 RAE [End]
ENDIF  

*--The End

*!*************************************************************
*! Name      : lfvPrntDtl
*! Developer : Rania Abdel Razik (RAE)
*! Date      : 05/19/2002
*! Purpose   : Change the sort by option to Pack when Print 
*!             Pack Detail is "No".
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvPrntDtl()
*!*************************************************************
*! C200343,1

FUNCTION lfvPrntDtl

IF RIGHT(lcFormName,2) ='GM'
  IF !llRpPrTPak
    lcRpSortBy = 'P'
    laSortDesc[1] = 'Pack'
    laSortDesc[2] = 'Line Number'
    laSortDesc[3] = 'Style'

    laSortVal[1] = 'P'
    laSortVal[2] = 'L'
    laSortVal[3] = 'S'
  ENDIF
ENDIF

*--End of lfvPrntDtl.
*B131656,1 AHU 04/11/2006 Add the currency sign to Ammount in case of multy currency to Sales Order Confirmation Form A. [START]
*!**************************************************************************
*! Name      : lfGetSign
*! Developer : Ahmed Maher Hussien (AHU)
*! Date      : 04/11/2006
*! Purpose   : Get the symbol of the used curr.
*!**************************************************************************
*! Example   : = lfGetSign()
*!**************************************************************************
FUNCTION lfGetSign
PRIVATE lcSign

lcSign = SPACE(3)
lcSign = IIF(SEEK(ORDHDR.CCURRCODE,'SYCCURR'),SYCCURR.cCurrSmbl,lcSign)

RETURN lcSign
*-- End of lfGetSign
*B131656,1 [END]
