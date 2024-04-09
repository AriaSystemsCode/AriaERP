*:****************************************************************************
*: Program file        : MFRBR10
*: Program desc.       : Print Work Order (for customer Robert Gasperd)
*: For Report          : MFRBR10A , MFRBR10B , MFRBR10C , MFRBRCS
*: System              : Aria Advantage Series VER. 2.7
*: Module              : Manufucturing (MF) , Purchase Orders (PO)
*: Developer           : RAMY MABROUK (RAMY)
*: Tracking Job Number : C200091
*: Date                : Jan/2000
*:****************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gfCodDes()
*:                 gfGetAdr()
*:                 gfDispRe()
*:                 gfTempName()
*:****************************************************************************
*: Called From         : System Menu (MF --> Output --> Print Work Order )
*:                       System Menu (PO --> Output --> Print Work Order )
*:****************************************************************************
*: Passed Parameters  : None
*:****************************************************************************
*: Example : DO MFRBR10
*:****************************************************************************
*: Modification        :
*: B803310,1 SSH Remove Style Separator.
*: B#803342,1 SSH Fix The following Bugs :-
*: 1- The program does not print BOM at all in case if you are 
*:    printing by "Work Order" and the CUTTKT is not allocated to any 
*:    sales order. 
*: 3- The "tracking tickets" does not match to the form I sent to 
*:    Cairo. The form height is completely different. The form is 
*:    8.5*11 and it prints 3 tickets on a single form. In other words,
*:    3 tickets makes one 8.5*11 page.
*: 4- When selecting to print both "Work orders" and "tracking 
*:    tickets", the program opens 2 select device screens to allow the
*:    user to direct the print job to any printer. If you click on 
*:    cancel on the second select device screen to avoid printing 
*:    tracking tickets, the program prints them anyway.
*: 5- We have to print "Work orders" selecting a printer driver. If 
*:    you select a driver, the upper part of the form (the line that 
*:    prints the page number) wraps the line on the form.
*: 6- The BOM layout is correct but please remove all graphic parts 
*:    out of the FRX in order to speed the printing process on 
*:    DOTMATRIX printer.
*: 7- When printing work orders and tracking tickets, and after the 
*:    program prints the work order, the program starts printing the 
*:    BOM on the end of the page and it does not start to print from 
*:    the beginning of next page. We need a page per CUTTKT BOM.
*: 8- I still can't scan off a DOTMATRIX printed form. When printing 
*:    the form on a DOTMATRIX printer, the program switches the print 
*:    quality status to "utility" and I think that this what might be 
*:    causing the problem. This problem is still happening even after 
*:    removing the printer drivers from the FRX.
*:
*: C101939,1 HBG 10/22/2000 Remove "Form to print" option from the OG ,and only 
*: C101939,1                Print "Work Order+Tracking Ticket" and changing 
*: C101939,1                the layout to be like the standard Order confirmation report
*: B803876,1 HBG 12/07/2000 Changing the Ordhdr.complete date instead of cuttkt.complete in .FRX
*: B803877,1 HBG 12/07/2000 Print Order line notepad instead of style notepad in case of 
*: B803877,1                select by work order
*: B803952,1 HBG 09/01/2001 Remove the completion date per line , in FRX
*: B803954,1 HBG 09/01/2001 Fix bug of TotQty printed only in the last sheet
*: B604181,1 HBG 02/06/2001 Fix bug of wrong printing Order line notepad
*: B804047,1 KHM 03/15/2001 Fixed the bug of not printing the correct description
*: B804047,1                in case of having repeated styles (styles that 
*: B804047,1                are the same) in case of printing by sales order. 
*: B804047,1                the fix is in the FRX
*:****************************************************************************

*C101939,1 HBG 10/22/2000 Declare variabels and arrays and open needed files[Begin]
lcTime     = TIME()                     && Variable to hold the Time
DECLARE laCompAdd[5,1] , laSoldTo[5,1] , laShipTo[5,1] , laDivLName[1,2]
laCompAdd = ''                    && Array to hold the Company address
laSoldTo = ''                     && Array to hold the Sold To address
laShipTo = ''                     && Array to hold the Ship To address
lcStatus = ''                     && Variabel to hold the status of cuttkt or Po or Order
llOpnComp = .F.                   && Flaf to check if we open the Companys File or not

STORE SPACE(0) TO  lcOrdCat , lcDivLName , lcSolTAcc , lcShpTAcc

laDivLName[1,1] = 'DIVLNAME'     && Array to get the Division long name
laDivLName[1,2] = 'lcDivLName'

=gfOpenFile(gcDataDir+'WAREHOUS','Warehous','SH')
=gfOpenFile(gcDataDir+'OBJECTS','Objectid','SH')
=gfOpenFile(gcDataDir+'OBJLINK','Objlnkty','SH')
=gfOpenFile(gcDataDir+'CODES','Codes','SH')


*-- Get company Address [begin].
IF !USED('SYCCOMP')
  llOpnComp = gfOpenFile(gcSysHome+'SYCCOMP','Ccomp_id','SH')
ENDIF
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

IF SEEK('*' + 'LOGO' , 'OBJLINK') AND ;
  SEEK(OBJLINK.cObject_ID,'OBJECTS')
  = lfGetLogo()  && Function to Fill the temp. With company Logo.
ENDIF
*C101939,1 [End] 


*-- Declare some variables [start]
STORE SPACE(0) TO lcCostItm, lcHead1, lcHead2
STORE .F. TO llEndGroup , llChange 
STORE .T. TO llGoOn 
llPrnNot = IIF(lcRpPrNt = 'Y' , .T. , .F.)
*: B803310,1 SSH Remove Style Separator.
DIMENSION laMajSeg[1,1]
lnCLen   = 0
lnClrPos = 0
lnSclPos = 0

=lfAdjSeg()
*: B803310,1 [End]
lcCent = SET('CENT')
SET CENT ON
IF gcAct_Appl = 'PO'
  lcMaster1 = 'POSHDR'
  lcMaster2 = 'POSLN'
ELSE
  lcMaster1 = 'CUTTKTH'
  lcMaster2 = 'CUTTKTL'
ENDIF
*-- end
*: B#803342,1 SSH Open Bar Code Cross referance table.
=gfOpenFile(gcDataDir+"cTktBarC", "Corgcode", "SH")
*: B#803342,1 SSH [END]
SELECT ORDLINE
SET ORDER TO TAG lcOrdLn
*-- Print work orders if any

*C101939,1 HBG 10/22/2000 Comment this block of code because it is no longer used[Begin]
*IF lcRpPrWt# 'C'
  *-- if select by sales order
  *IF lcRpSele = 'S'
  *DIMENSION laSoldTo[5] ,laShipTo[5]
  *STORE SPACE(0) TO lcSolTName , lcShpTName , lcShipVia , lcOrdCat , lcSolTAcc , lcShpTAcc
  *-- Set some relations 
    *=lfSeleSO()
    *SET DEVICE TO SCREEN
    *IF !(llGoOn)
     *= gfModalGen("INM00052B00000" , "DIALOG")
     *: B#803342,1 SSH Set device to screen
     *SET DEVICE TO SCREEN
     *RETURN
     *: B#803342,1 SSH [END]
    *ENDIF
  *ELSE && select by work order
    *-- Set some relations
    *=lfSeleWO()
    *SET DEVICE TO SCREEN
  *ENDIF
  *-- If we will print the bill of material
  *IF lcRpPBOM = 'Y'
   *-- Print bill of material
   *=lfPrnCS()
  *ENDIF
*ENDIF
*C101939,1 [End]

*-- Print track ticket if any
*C101939,1 HBG 10/22/2000 Comment this block of code because it is no longer used[Begin]  
*IF lcRpPrWt# 'B'
*C101939,1 [End]
  *-- Print tracking ticket
  =lfPrnTT()
*C101939,1 HBG 10/22/2000 Comment this block of code because it is no longer used[Begin]
*ENDIF
*C101939,1 [End]

SELECT (lcTemp)
lnMajorLen = LEN(gfItemMask("PM"))   && Style major length.
SET RELATION TO 'S'+SUBSTR(Style,1,lnMajorLen) INTO OBJLINK ADDITIVE
*B803877,1 HBG 12/07/2000  Set relation with OrdHdr & OrdLine in to cases of select by [Begin]
*IF lcRpSele = 'S'
*B803877,1 [End]
SET RELATION TO 'O' + ORDER INTO ORDHDR ADDITIVE
SET RELATION TO 'O' + ORDER INTO ORDLINE ADDITIVE
*B803877,1 HBG 12/07/2000 comment this line because we set relation with OrdHdr & OrdLine 
*B803877,1                in to cases of select by [Begin]
*ENDIF
*B803877,1 [End]
SET RELATION TO CUTTKT       INTO CUTTKTH ADDITIVE
SET RELATION TO 'P' + CUTTKT INTO POSHDR ADDITIVE
SELECT WAREHOUS
SET ORDER TO TAG Warehous 
SELECT CUTTKTH
SET RELATION TO cwarecode INTO WAREHOUS
SELECT OBJLINK
SET RELATION TO cobject_id INTO OBJECTS ADDITIVE

*C101939,1 HBG 10/22/2000 Select the temp file which used to print from and call the report[begin]      
SELECT (lcTemp)
SET ORDER TO Order
GO TOP
*-- Call the Report

DO gfDispRe WITH EVALUATE ('lcRepNam')
*C101939,1 [End]

*C101939,1 HBG 10/22/2000 Relase the relations[Begin]
SELECT (lcTemp)
SET RELATION TO
SELECT OBJLINK
SET RELATION TO
*C101939,1 [End]

*C101939,1 HBG 10/22/200 Bill of material will be printed after Sales order+Tracking ticket [Begin]
*-- If we will print the bill of material
IF lcRpPBOM = 'Y'
  *-- Print bill of material
  =lfPrnCS()
ENDIF
*C101939,1 [End]

SET CENT &lcCent

*!*************************************************************
*! Name      : lfvClrRd
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : Clear the read after calling the suppres expr
*!*************************************************************
*! Called from : MFRBR10.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvClrRd()
*!*************************************************************

FUNCTION lfvClrRd

= lfvPrtCs()

PRIVATE lnOldAls
lnOldAls = SELECT(0)

CLEAR READ
SELECT(lnOldAls)
*!*************************************************************
*! Name      : lfSeleSO
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : Prepare the program to print by SO
*!*************************************************************
*! Called from : MFRBR10.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSeleSO()
*!*************************************************************

FUNCTION lfSeleSO

*-- SAVE SOME SETTINGS
lcOldTFrm = lcOGTmpForm
lcOldFrm      = lcRepNam
lcAlias = SELECT(0)
lcRepNam = 'MFRBR10A'
= lfRepPltFr(lcRepNam)
*-- END SAVE

SELECT CUTPICK
GO TOP

LOCATE FOR IIF(gcAct_Appl = 'MF' , TRANCD = '1' , TRANCD = '2') AND &lcRpExp

IF FOUND() && Found records matches the lcRpExp
  SET KEY TO IIF(gcAct_Appl = 'MF' , '1' , '2')
  SET RELATION TO 'O' + ORDER INTO ORDHDR ADDITIVE
  SET RELATION TO 'O' + CUTPICK.ORDER + CUTPICK.STYLE INTO ORDLINE ADDITIVE
  SET RELATION TO CUTPICK.cTktNo INTO (lcMaster1) ADDITIVE
  SET RELATION TO 'B' + ORDER INTO NOTEPAD ADDITIVE  
  SELECT ORDHDR 
  SET RELATION TO 'M' + ACCOUNT INTO CUSTOMER
  SELECT ORDLINE
  SET RELATION TO STYLE INTO STYLE
  SELECT STYLE
  SET RELATION TO 'S' + SCALE INTO SCALE
  SELECT CUTPICK
  DO gfDispRe WITH EVAL('lcRepNam') , 'FOR ' + lcRpExp
  SET DEVICE TO SCREEN
ELSE  && Not found records matches the lcRpExp
  llGoOn = .F.
  RETURN
ENDIF
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfSeleWO
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : Prepare the program to print by WO
*!*************************************************************
*! Called from : MFRBR10.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSeleWO()
*!*************************************************************

FUNCTION lfSeleWO
*-- Save some settings

lcOldTFrm = lcOGTmpForm
lcOldFrm      = lcRepNam
lcRepNam = 'MFRBR10B'
lcAlias = SELECT(0)
= lfRepPltFr(lcRepNam)
*-- end save

lcExpr = lcRpExp

*-- If printing in 'MF'
IF gcAct_Appl = 'MF'
  SELECT CUTTKTL
  SET RELATION TO CUTTKT INTO CUTTKTH
  SET RELATION TO 'I' + CUTTKT INTO NOTEPAD ADDITIVE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  SELECT STYLE
  SET RELATION TO 'S' + SCALE INTO SCALE
  SELECT CUTTKTL
  lcExpr = STRTRAN(lcExpr , 'CUTPICK.CTKTNO' , 'CUTTKTL.CUTTKT')
  DO gfDispRe WITH EVAL('lcRepNam') , 'FOR ' + lcExpr
  SELECT CUTTKTL
  SET RELATION TO
ELSE && Printing in 'PO'
  SELECT POSLN
  SET RELATION TO 'P' + PO INTO POSHDR
  SET RELATION TO 'I' + PO INTO NOTEPAD ADDITIVE
  SET RELATION TO STYLE INTO STYLE ADDITIVE
  lcExpr = STRTRAN(lcExpr , 'CUTPICK.CTKTNO' , 'POSLN.PO')
  DO gfDispRe WITH EVAL('lcRepNam') , 'FOR ' + lcExpr
  SELECT POSLN
  SET RELATION TO
ENDIF

SELECT (lcAlias)

*!*************************************************************
*! Name      : lfGetS
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : Get codes used in the .FRX
*!*************************************************************
*! Called from : MFRBR10A.FRX (Work Order report)
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetS()
*!*************************************************************
FUNCTION lfGetS


*--new
lnAlias = SELECT(0)

STORE SPACE(0) TO  lcOrdCat , lcDivLName , lcSolTAcc , lcShpTAcc

SELECT ORDHDR

=SEEK(IIF(!EMPTY(ORDHDR.STORE) , 'S' , 'M') + ACCOUNT + STORE ,'CUSTOMER')
lcSolTAcc  = CUSTOMER.Account
lcSolTName = CUSTOMER.BTName
laSoldTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1 , '2')
laSoldTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2 , '2')
laSoldTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3 , '2')
laSoldTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4 , '2')
laSoldTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5 , '2')
= lfAdrShift('laSoldTo')

SELECT ORDHDR

*lcShpTName  = STName
lcShpTName  = IIF(!EMPTY(ORDHDR.STName) , STName , CUSTOMER.STNAME)
*-- IF alternate ship to address
IF !EMPTY(ORDHDR.STORE)
  lcShpTAcc   = Store
ENDIF
IF ORDHDR.Alt_ShpTo
  laShipTo[1] = cAddress1
  laShipTo[2] = cAddress2
  laShipTo[3] = cAddress3
  laShipTo[4] = cAddress4
  laShipTo[5] = cAddress5
ELSE
  *=SEEK('S'+ ACCOUNT + STORE ,'CUSTOMER')
  =SEEK(IIF(!EMPTY(ORDHDR.STORE) , 'S' , 'M') + ACCOUNT + STORE ,'CUSTOMER')
  laShipTo[1] = gfGetAdr('CUSTOMER' , '' , '' , '' , 1)
  laShipTo[2] = gfGetAdr('CUSTOMER' , '' , '' , '' , 2)
  laShipTo[3] = gfGetAdr('CUSTOMER' , '' , '' , '' , 3)
  laShipTo[4] = gfGetAdr('CUSTOMER' , '' , '' , '' , 4)
  laShipTo[5] = gfGetAdr('CUSTOMER' , '' , '' , '' , 5)
ENDIF
= lfAdrShift('laShipTo')

lcOrdCat   = gfCodDes(ORDHDR.CORDERCAT, 'CORDERCAT')
lcShipVia  = gfCodDes(ORDHDR.SHIPVIA, 'SHIPVIA')
*C101939,1 HBG 10/22/2000 Get the Season,Special inst. and terms code [Begin]
lcSeason  = gfCodDes(ORDHDR.Season  , 'SEASON'   )
lcSpcInst = gfCodDes(ORDHDR.SpcInst , 'SPCINST'  )
lcTerms   = gfCodDes(ORDHDR.CTERMCODE,'CTERMCODE')
*C101939,1 [End]

SELECT (lnAlias)


*!*************************************************************
*! Name      : lfCheckOrd
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : To check if we will print order notes or not
*!*************************************************************
*! Called from : MFRBR10A.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCheckOrd()
*!*************************************************************

FUNCTION lfCheckOrd
*-- Current alias is ........
lnAlias = SELECT(0)
SELECT IIF(lcRpSele = 'S' , 'CUTPICK' , lcMaster2)
lcOrdNo = IIF(lcRpSele = 'S' , ORDER , IIF(gcAct_Appl = 'PO' , PO , Cuttkt))
IF !EOF()
  SKIP 1
ENDIF
IF IIF(lcRpSele = 'S' , ORDER , IIF(gcAct_Appl = 'PO' , PO , Cuttkt)) # lcOrdNo
  IF llEndGroup
    llChange = .T.
  ENDIF  
ELSE
  llChange = .F.
ENDIF
IF !BOF()
  SKIP -1
ENDIF
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfStGroup
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : 
*!*************************************************************
*! Called from : MFRBRCS.FRX
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
*! Name      : lfEndGroup
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : Set this variable .T. to don't print the word "CONTINUED"
*!*************************************************************
*! Called from : MFRBRCS.FRX
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
RETURN ''


*!*************************************************************
*! Name      : lfGetTitle
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : To get the cost element title.
*!*************************************************************
*! Called from : MFRBRCS.FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetTitle()
*!*************************************************************

*To get the cost element title.
FUNCTION lfGetTitle
PRIVATE lcSetEx , lnRecNo
lcCostItm = ''
lcSetEx = SET('EXACT')
SET EXACT ON
lnAlias = SELECT(0)

SELECT (lcTmp)

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
*! Name      : lfDeclare
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : Declare arrayes used in select by Pop Up
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfDeclare()
*!*************************************************************

FUNCTION lfDeclare

DIME laDesc[2,1] , laRet[2,1]


IF gcAct_Appl = 'MF'
  laDesc[1,1] = 'Sales Order'
  laDesc[2,1] = 'Work Order'

  laRet[1,1]  = 'S'
  laRet[2,1]  = 'W'
ELSE
  laDesc[1,1] = 'Sales Order'
  laDesc[2,1] = 'Purchase Order'

  laRet[1,1]  = 'S'
  laRet[2,1]  = 'W'
ENDIF

*!*************************************************************
*! Name      : lfPrnCS
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : We will print Cost Sheet
*!*************************************************************
*! Called from : MFRBR10.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrnCS()
*!*************************************************************

FUNCTION lfPrnCS
*PRIVATE lcTmp

SELECT CTKTBOM
*IF !SEEK(IIF(gcAct_Appl = 'PO' , 'I' , 'M') + CUTPICK.cTktNo)
*  RETURN
*ENDIF

=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru , 1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1 , 1] = 'ORDER'
laFileStru[lnFileStru+1 , 2] = 'C'
laFileStru[lnFileStru+1 , 3] = 6
laFileStru[lnFileStru+1 , 4] = 0
*C101939,1 HBG 10/22/2000 Add field to hold the sequance of the operation[Begin]
lnFileStru = ALEN(laFileStru , 1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1 , 1] = 'Seq'
laFileStru[lnFileStru+1 , 2] = 'C'
laFileStru[lnFileStru+1 , 3] = 2
laFileStru[lnFileStru+1 , 4] = 0
*C101939,1 [END]

CREATE TABLE (gcWorkDir+lcTmp) FROM ARRAY laFileStru

*C101939,1 HBG 10/22/2000 Index the tabel to avoid repeting the data for each style, and sort by Seq. [Begin]
INDEX ON cuttkt+typ+item+iclr+seq+mfgcode+dyelot TAG(lcTmp) of (gcWorkDir+lcTmp)
*-- Open MFGOPRHD file to get the seq. of the operation 
=gfOpenFile(gcDataDir+'MFGOPRHD' , 'Mfgoprhd ' , 'SH')
*C101939,1 [END]

lcExpr = lcRpExp
*: B#803342,1 SSH Print Tr. Ticket Even if not generated from SO
IF lcRpSele = 'W'
  lcExpr = STRTRAN(lcExpr , 'CUTPICK.CTKTNO' ,;
                            IIF(gcAct_Appl = 'MF', 'CutTktH.CutTkt' ,'POSHDR.PO'))
  lcExpr = STRTRAN(lcExpr , 'POSHDR.CSTYTYPE')
  lcExpr = STRTRAN(lcExpr , '"P' , '"')
  IF gcAct_Appl = 'MF'
    SELECT CutTktH
  ELSE
    SELECT POSHDR
  ENDIF
ELSE
  lcExpr = STRTRAN(lcExpr , 'POSHDR.PO' , 'CUTPICK.CTKTNO')
  lcExpr = STRTRAN(lcExpr , 'POSHDR.CSTYTYPE')
  lcExpr = STRTRAN(lcExpr , '"P' , '"')
  SELECT CUTPICK
  SET RELATION TO IIF(gcAct_Appl = 'PO' , 'I' , 'M') + CTKTNO INTO CTKTBOM
ENDIF
*: B#803342,1 SSH [END]

*: B#803342,1 SSH Print Tr. Ticket Even if not generated from SO
*SCAN FOR IIF(gcAct_Appl = 'MF' , TRANCD = '1' , TRANCD = '2') AND &lcExpr
SCAN FOR IIF(lcRpSele = 'W',.T.,IIF(gcAct_Appl = 'MF' , TRANCD = '1' , TRANCD = '2')) AND &lcExpr
*: B#803342,1 SSH [END]
  SELECT CTKTBOM
  *: B#803342,1 SSH Print Tr. Ticket Even if not generated from SO
  IF lcRpSele = 'W'
    IF gcAct_Appl = 'MF'
      lcToEval = 'CutTktH.CutTkt'
    ELSE
      lcToEval = 'POSHDR.PO'
    ENDIF
  ELSE
    lcToEval = 'CUTPICK.CTKTNO'
  ENDIF
  *: B#803342,1 SSH [END]
  *IF SEEK(IIF(gcAct_Appl = 'PO' , 'I' , 'M') + CUTPICK.cTktNo)
  IF SEEK(IIF(gcAct_Appl = 'PO' , 'I' , 'M') + &lcToEval)
  *: B#803342,1 SSH Print Tr. Ticket Even if not generated from SO
    *SCAN FOR CUTTKT = CUTPICK.CTKTNO
    SCAN FOR CUTTKT = &lcToEval
  *: B#803342,1 SSH [END]
      SCATTER MEMVAR MEMO
      *: B#803342,1 SSH Print Tr. Ticket Even if not generated from SO
      *m.Order = CUTPICK.Order
      m.Order = IIF(lcRpSele = 'W', '',CUTPICK.Order)
      *: B#803342,1 SSH [END]
      *C101939,1 HBG 10/22/2000 Get the Seq of the operations [Begin]
      IF SEEK(IIF(gcAct_Appl = 'PO' , 'I' , 'M')+m.Cuttkt+m.mfgcode,'MFGOPRHD')
        m.Seq = MFGOPRHD.cOperSeq
      ELSE
        m.Seq = ' '
      ENDIF
      *-- If select by Sales order and this line not found in the temp file , insert it
      IF lcRpSele = 'S' 
        IF !SEEK(m.cuttkt+m.typ+m.item+m.iclr+m.seq+m.mfgcode+m.dyelot,lcTmp)
          INSERT INTO (lcTmp) FROM MEMVAR
        ENDIF 
      ELSE
      *C101939,1 [End] 
        INSERT INTO (lcTmp) FROM MEMVAR
      *C101939,1 HBG 10/22/2000 End if elect by Sales order and this line not found in the temp file [Begin]
      ENDIF  
      *C101939,1 [End]
    ENDSCAN
  ENDIF
  SELECT CUTPICK
ENDSCAN

SELECT (lcTmp)
GOTO TOP
IF RECCOUNT() = 0
  *= gfModalGen("INM00052B00000" , "DIALOG")
  *: B#803342,1 SSH Set device to screen
  SET DEVICE TO SCREEN
  Return
ENDIF


lcMGroup  = "EVALUATE(lcTmp+'.CUTTKT')"
lcInGroup = "EVALUATE(lcTmp+'.CUTTKT') + EVALUATE(lcTmp+'.TYP')"
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

SELECT (lcTmp)

SET RELATION TO LEFT(item,7)+iclr INTO Fabric ADDITIVE
SET RELATION TO IIF(gcAct_Appl = 'MF' , CutTkt , 'P' + CutTkt) INTO (lcMaster1) ADDITIVE
lcOldTFrm = lcOGTmpForm
lcOldFrm      = lcRepNam
lcRepNam      = 'MFRBRCS'
OgPlatForm    = 'WINDOWS'
SELECT (lcTmp)
lcTmpFName = lcOGTmpForm
lcOGTmpForm = ""
DO gfDispRe WITH EVAL('lcRepNam')
lcOGTmpForm = lcTmpFName
SELECT (lcTmp)
SET RELATION TO
USE
lcRepNam     = lcOldFrm

*!*************************************************************
*! Name      : lfPrnTT
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : print Traking Tickets
*!*************************************************************
*! Called from : MFRBR10.PRG
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrnTT()
*!*************************************************************

FUNCTION lfPrnTT

DIME laRelated[1,2]
*-- Save some settings
lcOldTFrm = lcOGTmpForm
lcOldFrm      = lcRepNam
*C101939,1 HBG 10/22/2000 Comment this line because only one report is used [Begin]
*lcRepNam      = 'MFRBR10C'
lnOprBrLin = 1    && Variabel to get 3 Operation ber line
STORE '' To lcSize , lcQuty  
STORE 0 TO lnTotQty , lnSize  
*C101939,1 [END]

lcAlias = SELECT(0)
*C101939,1 HBG 10/22/2000 Comment this line because only one report is used [Begin]
*= lfRepPltFr(lcRepNam)
*C101939,1 [END]
lnReturn = 0
lcExpr = lcRpExp

*C101939,1 HBG 10/22/2000 Add field to the temp file we print from[Begin]
*CREAT CURSOR (lcTemp) (Order   C(6)  , CUTTKT C(6)  , Size C(5)    , Qty N(8) , DESC C(60), ; 
                      STYLE   C(19) , Account C(5) , BTName C(30) , Opr C(6) , Due  D(8))

*B604181,1 HBG 02/06/2001 Add 'LineNo' field to be added to the index [Begin]
*CREAT CURSOR (lcTemp) (Order   C(6)  , CUTTKT C(6)  , Sizenum C(2) ,Size C(5) ,Type C(1), Qty N(8) , DESC C(60), ; 
                      STYLE   C(19) , Account C(5) ,Store C(8), BTName C(30) , CustPO C(15) ,;
                      Opr1 C(6) , Opr2 C(6) ,Opr3 C(6),OprDesc1 C(30) , OprDesc2 C(30) ,OprDesc3 C(30) ,Due  D(8), mNotes M,TotQty N(7) ) 
CREAT CURSOR (lcTemp) (Order   C(6)  , CUTTKT C(6)  , Sizenum C(2) ,Size C(5) ,Type C(1), Qty N(8) , DESC C(60), ; 
                      STYLE   C(19) ,LineNo C(6), Account C(5) ,Store C(8), BTName C(30) , CustPO C(15) ,;
                      Opr1 C(6) , Opr2 C(6) ,Opr3 C(6),OprDesc1 C(30) , OprDesc2 C(30) ,OprDesc3 C(30) ,Due  D(8), mNotes M,TotQty N(7) ) 
*B604181,1 [End]
                                            
*C101939,1 [END]                                       
*--- SSH B#803342,1 Change Indec
*INDEX ON STYLE + SIZE + OPR TAG Order of (gcWorkDir+lcTemp)
*C101939,1 HBG 10/22/2000 change the index of the temp file[Begin]
*INDEX ON CUTTKT + STYLE + SIZE + OPR TAG Order of (gcWorkDir+lcTemp)
IF lcRpSele = 'W'
  INDEX ON CUTTKT + STYLE + Sizenum + SIZE + TYPE TAG Order of (gcWorkDir+lcTemp)
ELSE
  *B604181,1 HBG 02/06/2001 Add 'LineNo' field to the index [Begin]
  *INDEX ON Order + CutTkt + STYLE + Sizenum + SIZE + TYPE TAG Order of (gcWorkDir+lcTemp)
  INDEX ON Order  + CutTkt + LineNo + STYLE  + Sizenum + SIZE + TYPE TAG Order of (gcWorkDir+lcTemp)
  *B604181,1 [End]
ENDIF


*C101939,1 [End]
*--- SSH
IF !USED(lcTmpHD)
  =gfOpenFile(gcDataDir+'MFGOPRHD' , '' , 'SH' , @lcTmpHD, .T.)
  =gfOpenFile(gcDataDir+'MFGOPRDT' , '' , 'SH' , @lcTmpDT, .T.)

  SELECT (lcTmpHD)
  *C101939,1 HBG 10/22/2000 change the index of the 'MFGOPRHD' file[Begin]
  *INDEX ON cimtyp+ctktno+citmmajor TAG MfgItem OF (gcWorkDir +  lcHDIndx + '.CDX')
  INDEX ON cimtyp+ctktno+citmmajor+coperseq TAG MfgItem OF (gcWorkDir +  lcHDIndx + '.CDX')
  *C101939,1 [End]
  

  SELECT (lcTmpDT)
  INDEX ON cimtyp+ctktno+item + cOprcode TAG MfgItem OF (gcWorkDir +  lcDTIndx + '.CDX')
ENDIF


DO CASE
   CASE lcRpSele = 'S'  && Case Select by sales order in 'MF' or 'PO'
     SELECT CUTPICK   
     SET RELATION TO
     *: B#803342,1 SSH Correct the relation.
     *SET RELATION TO 'O' + Order INTO ORDLINE ADDITIVE
     *B604181,1 HBG 02/06/2001 Correct the relation to print order line notepad correctly [Begin]
     *SET RELATION TO 'O' + Order+Style INTO ORDLINE ADDITIVE
     SET RELATION TO 'O' + Order+PADL(cordline,6)+Style INTO ORDLINE ADDITIVE
     *B604181,1[End]
     *: B#803342,1 SSH [END]
     IF gcAct_Appl = 'MF'
       *C101939,1 HBG 10/22/2000 Add style field to the relation [Begin]
       *SET RELATION TO CTKTNO INTO CUTTKTL ADDITIVE
       SET RELATION TO CTKTNO + STYLE INTO CUTTKTL ADDITIVE
       *C101939,1 [End]
     ELSE
       *C101939,1 HBG 10/22/2000 Add style field to the relation [Begin]
       *SET RELATION TO 'P' + CTKTNO INTO POSLN ADDITIVE
       SET RELATION TO 'P' + CTKTNO + STYLE INTO POSLN ADDITIVE
       *C101939,1 [End]
     ENDIF
     SELECT ORDLINE
     SET RELATION TO STYLE INTO STYLE
	 SET RELATION TO IIF(EMPTY(ORDLINE.STORE) , 'M' , 'S') + ACCOUNT INTO CUSTOMER ADDITIVE
     
     SELECT STYLE
     SET RELATION TO 'S' + SCALE INTO SCALE
     
     llFrstTime = .T.  && To check if there is first time enter the scan
     SELECT CUTPICK
     SCAN FOR &lcExpr
       IF !llFrstTime
         IF ORDLINE.Order <> lcOrder
           IF llPrnNot AND SEEK('B' + lcOrder,'NOTEPAD')
             *B803954,1 HBG 09/01/2001 Save TotQty when the order # Change [Begin]
             *INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , STYLE , Desc,mNotes) ;
                          VALUES (lcOrder , lcCutTkt ,ALLTRIM(STR(lnSize)), lcSize ,'B', lcStyle , lcDesc ,NotePad.mNotes) 
             *B604181,1 HBG 02/06/2001 update the 'LineNo' field [Begin]
             *INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , STYLE , Desc,mNotes , TotQty) ;
                          VALUES (lcOrder , lcCutTkt ,ALLTRIM(STR(lnSize)), lcSize ,'B', lcStyle , lcDesc ,NotePad.mNotes ,lnTotQty )              
             INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , STYLE ,LineNo, Desc,mNotes , TotQty) ;
                          VALUES (lcOrder , lcCutTkt ,ALLTRIM(STR(lnSize)), lcSize ,'B', lcStyle ,lnLineNo , lcDesc ,NotePad.mNotes ,lnTotQty )              
             *B604181,1 [End]                          
             lnTotQty = 0
           ELSE
             *B604181,1 HBG 02/06/2001 update the 'LineNo' field [Begin] 
             *INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , STYLE , Desc, TotQty) ;
                          VALUES (lcOrder , lcCutTkt ,ALLTRIM(STR(lnSize)), lcSize ,'B', lcStyle , lcDesc ,lnTotQty )              
             INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , STYLE ,LineNo , Desc, TotQty) ;
                          VALUES (lcOrder , lcCutTkt ,ALLTRIM(STR(lnSize)), lcSize ,'B', lcStyle ,lnLineNo , lcDesc ,lnTotQty )              
             *B604181,1 [End]                          
             lnTotQty = 0             
             *B803954,1 [End]
           ENDIF
         ENDIF
       ENDIF
       llFrstTime = .F.
       SELECT SCALE
       FOR lnCont = 1 TO Cnt
         lcSz  = EVALUATE('SZ'+ALLTRIM(STR(lnCont)))
         *B604181,1 HBG 02/06/2001 Get the Qty from the 'CutPick' file [Begin] 
         *lcQty = EVALUATE(IIF(gcAct_Appl = 'MF' , 'CUTTKTL.Qty' , 'POSLN.Qty' ) +ALLTRIM(STR(lnCont)))
         lcQty = EVALUATE('CUTPICK.Qty'+ALLTRIM(STR(lnCont)))
         *B604181,1 [End]
         IF lcQty > 0
             *C101939,1 HBG 10/22/2000 save the Size and size # and Qty to be used in case printing Note pad [Begin]
             lcSize = lcSz  
             lnSize = lnCont
             lcQuty = lcQty
             *C101939,1 [End] 
             IF SEEK(IIF(gcAct_Appl = 'PO' , 'I' , 'M') + CUTPICK.cTktNo , (lcTmpHD))
               *C101939,1 HBG 10/22/2000 set the variabel which control rinting 3 operation ber line to 1[End]
               lnOprBrLin = 1
               *C101939,1 [End]
               SELECT (lcTmpHD)
               SCAN REST WHILE IIF(gcAct_Appl = 'PO' , 'I' , 'M') + CUTPICK.cTktNo + CUTPICK.Style = IIF(gcAct_Appl = 'PO' , 'I' , 'M') + cTktNo
                 IF SEEK(IIF(gcAct_Appl = 'PO' , 'I' , 'M') + CUTPICK.cTktNo + CUTPICK.Style + EVALUATE(lcTmpHD + '.cOprCode') , (lcTmpDT))
                   ldDue = EVALUATE(lcTmpDT + '.DueDate')
                 ELSE
                   laRelated[1,1] = 'LEADTIME  '
                   laRelated[1,2] = 'lnReturn'
                   =GfRltFld(cOprCode , @laRelated , 'MFGCODE')
                   ldDue = gdSysDate + lnReturn
                 ENDIF
                 *C101939,1 HBG 10/22/2000 Insert line for Style Notepad and line for each 3 operations[Begin]
                 *INSERT INTO (lcTemp) (ORDER , CUTTKT , SIZE , QTY , Desc , STYLE , Account , BTName , Opr , Due) ;
                      VALUES (ORDLINE.ORDER , CUTPICK.CTKTNO , lcSz , lcQty , ORDLINE.DESC1 , STYLE.STYLE , CUSTOMER.Account , CUSTOMER.BTName , EVALUATE(lcTmpHD + '.cOprCode') , ldDue)
                 IF lnOprBrLin = 1
                   lcOprDesc =  gfCodDes(EVALUATE(lcTmpHD + '.cOprCode'),'MfgCode')
 
                   *B604181,1 HBG 02/06/2001 update the 'LineNo' field [Begin] 
                   *INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type, QTY , Desc , STYLE , Account , Store , BTName , CustPO , OprDesc1,Opr1, Due) ;
                        VALUES (ORDLINE.ORDER , CUTPICK.CTKTNO ,ALLTRIM(STR(lnCont)), lcSz , 'B', lcQty , ORDLINE.DESC1 , STYLE.STYLE , CUSTOMER.Account , CUSTOMER.Store , CUSTOMER.BTName , ORDLINE.CustPO , lcOprDesc ,EVALUATE(lcTmpHD + '.cOprCode'), ldDue) 
                   INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type, QTY , Desc , STYLE,LineNo , Account , Store , BTName , CustPO , OprDesc1,Opr1, Due) ;
                        VALUES (ORDLINE.ORDER , CUTPICK.CTKTNO ,ALLTRIM(STR(lnCont)), lcSz , 'B', lcQty , ORDLINE.DESC1 , STYLE.STYLE ,CUTPICK.cordline, CUSTOMER.Account , CUSTOMER.Store , CUSTOMER.BTName , ORDLINE.CustPO , lcOprDesc ,EVALUATE(lcTmpHD + '.cOprCode'), ldDue) 
                   *B604181,1 [End]          
                         
                   lnOprBrLin = lnOprBrLin +1  
                 ELSE
                   lcOpr = 'Opr'+ALLTRIM(STR(lnOprBrLin))
                   lcOprDes = 'OprDesc'+ALLTRIM(STR(lnOprBrLin))
                   SELECT(lcTemp)
                   lcOprDesc =  gfCodDes(EVALUATE(lcTmpHD + '.cOprCode'),'MfgCode')
                   REPLACE &lcOpr WITH EVALUATE(lcTmpHD + '.cOprCode')
                   REPLACE &lcOprDes WITH lcOprDesc
                   lnOprBrLin = IIF(lnOprBrLin < 3 ,lnOprBrLin +1 , 1)
                 ENDIF       
                 *C101939,1 [End]
               ENDSCAN
             ENDIF
         ENDIF
         SELECT SCALE
       ENDFOR
       IF !EMPTY(ORDLINE.note_mem) AND lnSize <> 0
         *B604181,1 HBG 02/06/2001 update the 'LineNo' field [Begin] 
         *INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , QTY , Desc , STYLE , Account , Store , BTName , CustPO , Due,mNotes) ;
                      VALUES (ORDLINE.ORDER , CUTPICK.CTKTNO ,ALLTRIM(STR(lnSize)), lcSize ,'A', lcQuty , ORDLINE.DESC1 , STYLE.STYLE , CUSTOMER.Account , CUSTOMER.Store , CUSTOMER.BTName , ORDLINE.CustPO  , ldDue ,ORDLINE.note_mem) 
         INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , QTY , Desc , STYLE ,LineNo, Account , Store , BTName , CustPO , Due,mNotes) ;
                      VALUES (ORDLINE.ORDER , CUTPICK.CTKTNO ,ALLTRIM(STR(lnSize)), lcSize ,'A', lcQuty , ORDLINE.DESC1 , STYLE.STYLE,CUTPICK.cordline , CUSTOMER.Account , CUSTOMER.Store , CUSTOMER.BTName , ORDLINE.CustPO  , ldDue ,ORDLINE.note_mem) 
         *B604181,1[End]                  
          
       ENDIF
       *C101939,1 HBG 10/22/2000 Save informtion of this lne to be used in printing not pad and total Qty[Begin]
       lcOrder  = ORDLINE.ORDER
       *B604181,1 HBG 02/06/2001 update the 'LineNo' field [Begin] 
       lnLineNo = CUTPICK.cordline
       *B604181,1 [End]
       lcCutTkt = CUTPICK.CTKTNO
       lcStyle  = STYLE.STYLE
       lcDesc   = ORDLINE.DESC1
       lnTotQty = lnTotQty + CUTPICK.TotQty
       *C101939,1 [End]
     ENDSCAN
     *C101939,1 HBG 10/22/2000 Get the cuttkt Notepad And TotQty[Begin]
     IF lnSize <> 0
       IF llPrnNot AND SEEK('B' + lcOrder,'NOTEPAD') 
         *B604181,1 HBG 02/06/2001 update the 'LineNo' field [Begin] 
         *INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , STYLE , Desc,mNotes,TotQty) ;
                      VALUES (lcOrder , lcCutTkt ,ALLTRIM(STR(lnSize)),;
                      lcSize , 'B' , lcStyle , lcDesc,;
                      NotePad.mNotes,lnTotQty) 
         INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , STYLE,LineNo , Desc,mNotes,TotQty) ;
                      VALUES (lcOrder , lcCutTkt ,ALLTRIM(STR(lnSize)),;
                      lcSize , 'B' , lcStyle ,lnLineNo, lcDesc,;
                      NotePad.mNotes,lnTotQty) 
         *B604181,1 [End]                          
       ELSE
         *B604181,1 HBG 02/06/2001 update the 'LineNo' field [Begin] 
         *INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , STYLE , Desc,TotQty) ;
                      VALUES (lcOrder , lcCutTkt ,ALLTRIM(STR(lnSize)),;
                      lcSize , 'B' , lcStyle , lcDesc,;
                      lnTotQty) 
         INSERT INTO (lcTemp) (ORDER , CUTTKT ,Sizenum, SIZE ,Type , STYLE,LineNo , Desc,TotQty) ;
                      VALUES (lcOrder , lcCutTkt ,ALLTRIM(STR(lnSize)),;
                      lcSize , 'B' , lcStyle,lnLineNo , lcDesc,;
                      lnTotQty) 
         *B604181,1 [End]              
       ENDIF                    
     ENDIF
     *C101939,1 [End]               
     SELECT CUTPICK
     SET RELATION TO
     
     SELECT STYLE
     SET RELATION TO
     
     SELECT ORDLINE
     SET RELATION TO

	
   CASE gcAct_Appl = 'MF' and lcRpSele = 'W'  && Select by W/O in 'MF'
     lcExpr = STRTRAN(lcExpr , 'CUTPICK.CTKTNO' , 'CUTTKTL.CUTTKT')
     SELECT CUTTKTL
     SET RELATION TO STYLE INTO STYLE
     SELECT STYLE
     SET RELATION TO 'S' + SCALE INTO SCALE
     llFrstTime = .T.  && To check if there is first time enter the scan
     *B803877,1 HBG 12/07/2000 Set the order of CUTPICK to get the order # [Begin]
     SELECT CUTPICK
     lcOldTag = TAG()
     SET ORDER TO Cutpkord
     *B803877,1 [End]
     SELECT CUTTKTL
     SCAN FOR &lcExpr
       *B803877,1 HBG 12/07/2000 Seek in CUTPICK to get the order # [Begin]
       =SEEK('1'+cuttkt+PADL(lineno,6),'CUTPICK')
       *B803877,1 [End]
       IF !llFrstTime
         IF CUTTKTL.CUTTKT <> lcCutTkt 
           IF llPrnNot AND SEEK('I' + lcCutTkt ,'NOTEPAD') 
             *B803877,1 HBG 12/07/2000 Save order# [Begin]
             *INSERT INTO (lcTemp) (CUTTKT         ,Sizenum, SIZE ,Type , STYLE , Desc,mNotes) ;
                          VALUES (lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,NotePad.mNotes)
             *B803954,1 HBG 09/01/2001 Save TotQty when the CutTkt # Change [Begin]
             *INSERT INTO (lcTemp) (ORDER, CUTTKT         ,Sizenum, SIZE ,Type , STYLE , Desc,mNotes) ;
                          VALUES (CUTPICK.Order, lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,NotePad.mNotes)
             INSERT INTO (lcTemp) (ORDER, CUTTKT         ,Sizenum, SIZE ,Type , STYLE , Desc,mNotes,TotQty) ;
                          VALUES (CUTPICK.Order, lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,NotePad.mNotes,lnTotQty)
             lnTotQty = 0 
           ELSE
             INSERT INTO (lcTemp) (ORDER, CUTTKT         ,Sizenum, SIZE ,Type , STYLE , Desc,TotQty) ;
                          VALUES (CUTPICK.Order, lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc,lnTotQty)
             lnTotQty = 0 
             *B803954,1 [End]
             *B803877,1 [End]      
           ENDIF
         ENDIF
       ENDIF
       
       llFrstTime = .F.
       SELECT SCALE
       FOR lnCont = 1 TO Cnt
         lcSz  = EVALUATE('SZ'+ALLTRIM(STR(lnCont)))
         lcQty = EVALUATE('CUTTKTL.Qty'+ALLTRIM(STR(lnCont)))
         IF lcQty > 0
           *C101939,1 HBG 10/22/2000 save the Size and size # and Qty to be used in case printing Note pad [Begin]
           lcSize = lcSz
           lnSize = lnCont
           lcQuty = lcQty
           *C101939,1 [End]
           *INSERT INTO (lcTemp) (CUTTKT , SIZE , QTY , Desc , STYLE) ;
                          VALUES (CUTTKTL.CUTTKT , lcSz , lcQty , STYLE.DESC1 , STYLE.STYLE)
           *C101939,1 HBG 10/22/2000 Comment This block of code because we won't get any infromations from 'MFGOPRDT' file [Begin]
           *IF SEEK('M' + CUTTKTL.CutTkt + CUTTKTL.Style , (lcTmpDT))
             *C101939,1 HBG 10/22/2000 set the variabel which control rinting 3 operation ber line to 1[End]
             *lnOprBrLin = 1
             *C101939,1 [End]
             *SELECT (lcTmpDT)
             *SCAN FOR 'M' + CUTTKTL.CutTkt + CUTTKTL.Style = 'M' + cTktNo + Item .AND. !EMPTY(DUEDATE) 
               *C101939,1 HBG 10/22/2000 Insert line for Style Notepad and line for each 3 operations[Begin]
               *INSERT INTO (lcTemp) (CUTTKT         , SIZE , QTY   , Desc        , STYLE       , Opr               , Due) ;
                             VALUES (CUTTKTL.CUTTKT , lcSz , lcQty , STYLE.DESC1 , STYLE.STYLE , EVALUATE(lcTmpDT +'.cOprCode') , EVALUATE(lcTmpDT +'.DueDate'))
               *IF lnOprBrLin = 1
                * lcOprDesc =  gfCodDes(EVALUATE(lcTmpDT +'.cOprCode'),'MfgCode')
                * INSERT INTO (lcTemp) (CUTTKT         ,Sizenum, SIZE ,Type , QTY   , Desc        , STYLE  , Opr1     , Due) ;
                              VALUES (CUTTKTL.CUTTKT ,ALLTRIM(STR(lnCont)), lcSz ,'B' , lcQty , STYLE.DESC1 , STYLE.STYLE ,  lcOprDesc  , EVALUATE(lcTmpDT +'.DueDate'))
                * lnOprBrLin = lnOprBrLin +1 
               *ELSE
                * lcOpr = 'Opr'+ALLTRIM(STR(lnOprBrLin))
                * SELECT(lcTemp)
                * lcOprDesc =  gfCodDes(EVALUATE(lcTmpDT +'.cOprCode'),'MfgCode')
                * REPLACE &lcOpr WITH lcOprDesc 
                * lnOprBrLin = IIF(lnOprBrLin < 3 ,lnOprBrLin +1 , 1)
              * ENDIF       
               *C101939,1 [End]
             *ENDSCAN
           *ELSE
           *C101939,1 [End]
             IF SEEK('M' + CUTTKTL.CutTkt , (lcTmpHD))
               *C101939,1 HBG 10/22/2000 set the variabel which control rinting 3 operation ber line to 1[End]
               lnOprBrLin = 1
               *C101939,1 [End]
               SELECT (lcTmpHD)
               laRelated[1,1] = 'LEADTIME  '
               laRelated[1,2] = 'lnReturn'
               SCAN FOR 'M' + CUTTKTL.CutTkt + CUTTKTL.Style = 'M' + cTktNo
                 =GfRltFld(cOprCode , @laRelated , 'MFGCODE')
                 ldDue = gdSysDate + lnReturn
                 *C101939,1 HBG 10/22/2000 Insert line for Style Notepad and line for each 3 operations[Begin]
                 *INSERT INTO (lcTemp) (CUTTKT         , SIZE , QTY   , Desc        , STYLE       , Opr                            , Due) ;
                             VALUES (CUTTKTL.CUTTKT , lcSz , lcQty , STYLE.DESC1 , STYLE.STYLE , EVALUATE(lcTmpHD +'.cOprCode') , ldDue)
                 IF lnOprBrLin = 1
                   lcOprDesc =  gfCodDes(EVALUATE(lcTmpHD +'.cOprCode'),'MfgCode')
                   *B803877,1 HBG 12/07/2000 Save order# [Begin]
                   *INSERT INTO (lcTemp) (CUTTKT         ,Sizenum, SIZE  ,Type, QTY   , Desc        , STYLE   ,OprDesc1, Opr1    , Due) ; 
                                VALUES (CUTTKTL.CUTTKT , ALLTRIM(STR(lnCont)), lcSz,'B',lcQty , STYLE.DESC1 , STYLE.STYLE , lcOprDesc,EVALUATE(lcTmpHD +'.cOprCode') ,ldDue)                    
                   INSERT INTO (lcTemp) (ORDER,CUTTKT         ,Sizenum, SIZE  ,Type, QTY   , Desc        , STYLE   ,OprDesc1, Opr1    , Due) ; 
                                VALUES (CUTPICK.Order,CUTTKTL.CUTTKT , ALLTRIM(STR(lnCont)), lcSz,'B',lcQty , STYLE.DESC1 , STYLE.STYLE , lcOprDesc,EVALUATE(lcTmpHD +'.cOprCode') ,ldDue)                    
                   *B803877,1 [End]
                   lnOprBrLin = lnOprBrLin +1   
                 ELSE
                   lcOpr = 'Opr'+ALLTRIM(STR(lnOprBrLin))
                   lcOprDes = 'OprDesc'+ALLTRIM(STR(lnOprBrLin))
                   SELECT(lcTemp)
                   lcOprDesc =  gfCodDes(EVALUATE(lcTmpHD +'.cOprCode'),'MfgCode')
                   REPLACE &lcOpr WITH EVALUATE(lcTmpHD +'.cOprCode')
                   REPLACE &lcOprDes WITH lcOprDesc 
                   lnOprBrLin = IIF(lnOprBrLin < 3 ,lnOprBrLin +1 , 1)
                 ENDIF       
                 *C101939,1 [End]
               ENDSCAN
             ENDIF
           *C101939,1 HBG 10/22/2000 Comment the End Of if seek in lcTmpHL [Begin]
           *ENDIF
           *C101939,1 [End]
         ENDIF
         SELECT SCALE
       ENDFOR
       *B803877,1 HBG 12/07/2000 Get order line notepad insted of style notepad [Begin]
       *IF SEEK('F' + STYLE.cstymajor,'NOTEPAD') AND lnSize <> 0
        * INSERT INTO (lcTemp) (CUTTKT         ,Sizenum, SIZE ,Type , QTY   , Desc        , STYLE       ,mNotes) ;
        *              VALUES (CUTTKTL.CUTTKT ,ALLTRIM(STR(lnSize)), lcSize,'A' , lcQuty , STYLE.DESC1 , STYLE.STYLE ,NotePad.mNotes)
       *ENDIF
       SELECT ORDLINE
       lcOrdTag = TAG()
       SET ORDER TO ORDLINE 
       SELECT SCALE
       *B804047,1 KHM 03/15/2001 (Begin) printing the order line notepad in 
       *B804047,1              1 case of printing by sales order
       *IF SEEK('O'+CUTPICK.Order+PADL(CUTTKTL.lineno,6),'ORDLINE')      
       IF lcRpSele = 'S' AND SEEK('O'+CUTPICK.Order+PADL(CUTTKTL.lineno,6),'ORDLINE')
       *B804047,1 KHM 03/15/2001 (End)
         IF !EMPTY(ORDLINE.note_mem) AND lnSize <> 0
           INSERT INTO (lcTemp) (ORDER, CUTTKT         ,Sizenum, SIZE ,Type , QTY   , Desc        , STYLE       ,mNotes) ;
                        VALUES (CUTPICK.Order, CUTTKTL.CUTTKT ,ALLTRIM(STR(lnSize)), lcSize,'A' , lcQuty , STYLE.DESC1 , STYLE.STYLE ,ORDLINE.note_mem)         
         ENDIF
       ENDIF
       SET ORDER TO &lcOrdTag IN OrdLine
       *B803877,1 [End]
       *C101939,1 HBG 10/22/2000 Save informtion of this lne to be used in printing not pad and total Qty[Begin]
       *B803877,1 HBG 12/07/2000 save Order#[begin]
       lcOrder  = CUTPICK.Order
       *B803877,1 [End]
       lcCutTkt = CUTTKTL.CUTTKT
       lcStyle  = STYLE.STYLE
       lcDesc   = STYLE.DESC1
       lnTotQty = lnTotQty + CUTTKTL.TotQty
       *C101939,1 [End]
     ENDSCAN
     *C101939,1 HBG 10/22/2000 Get the cuttkt Notepad And TotQty[Begin]
     IF lnSize <> 0
       IF llPrnNot AND SEEK('I' + lcCutTkt ,'NOTEPAD')
         *B803877,1 HBG 12/07/2000 save Order#[begin]
         *INSERT INTO (lcTemp) (CUTTKT         ,Sizenum, SIZE  ,Type, STYLE , Desc,mNotes,TotQty) ;
                      VALUES (lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,NotePad.mNotes,lnTotQty)
         INSERT INTO (lcTemp) (ORDER, CUTTKT         ,Sizenum, SIZE  ,Type, STYLE , Desc,mNotes,TotQty) ;
                      VALUES (lcOrder  , lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,NotePad.mNotes,lnTotQty)
         *B803877,1 [End]                          
       ELSE
         *B803877,1 HBG 12/07/2000 save Order#[begin]
         *INSERT INTO (lcTemp) (CUTTKT         ,Sizenum, SIZE  ,Type, STYLE , Desc,TotQty) ;
                      VALUES (lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,lnTotQty)       
         INSERT INTO (lcTemp) (ORDER, CUTTKT         ,Sizenum, SIZE  ,Type, STYLE , Desc,TotQty) ;
                      VALUES (lcOrder , lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,lnTotQty)       
         *B803877,1 [End]                                   
       ENDIF
     ENDIF
     *C101939,1 [End]   
     *B803877,1 HBG 12/07/2000 restore old index [Begin]
     SELECT CUTPICK
     SET ORDER TO &lcOldTag 
     *B803877,1 [End] 
   CASE gcAct_Appl = 'PO' and lcRpSele = 'W'  && Select by W/O in 'PO'
     lcExpr = STRTRAN(lcExpr , 'POSHDR.PO' , 'POSLN.PO')
     SELECT POSLN
     SET RELATION TO STYLE INTO STYLE
     SELECT STYLE
     SET RELATION TO 'S' + SCALE INTO SCALE
     *B803877,1 HBG 12/07/2000 Set the order of CUTPICK to get the order # [Begin]
     SELECT CUTPICK
     lcOldTag = TAG()
     SET ORDER TO Cutpkord
     *B803877,1 [End]
     llFrstTime = .T.  && To check if there is first time enter the scan
     
     SELECT POSLN
     SCAN FOR &lcExpr
       *B803877,1 HBG 12/07/2000 Seek in CUTPICK to get the order # [Begin]
       =SEEK('2'+cuttkt+PADL(lineno,6),'CUTPICK')
       *B803877,1 [End]
       IF !llFrstTime
         IF POSLN.PO <> lcCutTkt 
           IF llPrnNot AND SEEK('I' + lcCutTkt ,'NOTEPAD')
             *B803877,1 HBG 12/07/2000 save Order#[begin]
             *INSERT INTO (lcTemp) (CUTTKT         ,Sizenum, SIZE ,Type , STYLE , Desc,mNotes) ;
                          VALUES (lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,NotePad.mNotes)
             *B803954,1 HBG 09/01/2001 Save TotQty when the Po # Change [Begin]
             *INSERT INTO (lcTemp) (ORDER, CUTTKT         ,Sizenum, SIZE ,Type , STYLE , Desc,mNotes) ;
                          VALUES (CUTPICK.Order, lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,NotePad.mNotes)
             INSERT INTO (lcTemp) (ORDER, CUTTKT         ,Sizenum, SIZE ,Type , STYLE , Desc,mNotes,TotQty) ;
                          VALUES (CUTPICK.Order, lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc ,NotePad.mNotes,lnTotQty)                          
             lnTotQty = 0
           ELSE  
             INSERT INTO (lcTemp) (ORDER, CUTTKT         ,Sizenum, SIZE ,Type , STYLE , Desc,TotQty) ;
                          VALUES (CUTPICK.Order, lcCutTkt , ALLTRIM(STR(lnSize)), lcSize,'B', lcStyle , lcDesc,lnTotQty)                          
             lnTotQty = 0
             *B803954,1 [End]
             *B803877,1 [End]
           ENDIF
         ENDIF
       ENDIF
       llFrstTime = .F.
       SELECT SCALE
       FOR lnCont = 1 TO Cnt
         lcSz  = EVALUATE('SZ'+ALLTRIM(STR(lnCont)))
         lcQty = EVALUATE('POSLN.Qty'+ALLTRIM(STR(lnCont)))
         IF lcQty > 0
           *C101939,1 HBG 10/22/2000 save the Size and size # and Qty to be used in case printing Note pad [Begin]
           lcSize  = lcSz
           lnSize = lnCont
           lcQuty = lcQty
           *C101939,1 [End]
           *C101939,1 HBG 10/22/2000 Comment This block of code because we won't get any infromations from 'MFGOPRDT' file [Begin]
           *IF SEEK('I' + POSLN.PO + POSLN.Style , (lcTmpDT))
             *SELECT (lcTmpDT)
             *SCAN FOR 'I'+ POSLN.PO + POSLN.Style = 'I' + cTktNo + Item .AND. !EMPTY(DUEDATE)
               *C101939,1 HBG 10/22/2000 Insert line for Style Notepad and line for each 3 operations[Begin]
               *INSERT INTO (lcTemp) (CUTTKT         , SIZE , QTY   , Desc        , STYLE       , Opr               , Due) ;
                             VALUES (POSLN.PO , lcSz , lcQty , STYLE.DESC1 , STYLE.STYLE , EVALUATE(lcTmpDT +'.cOprCode') , EVALUATE(lcTmpDT +'.DueDate'))
               *IF lnOprBrLin = 1
               *  lcOprDesc =  gfCodDes(EVALUATE(lcTmpDT +'.cOprCode'),'MfgCode')
               *  INSERT INTO (lcTemp) (CUTTKT         ,Sizenum, SIZE ,Type , QTY   , Desc        , STYLE     , Opr1  , Due) ;
                             VALUES (POSLN.PO ,ALLTRIM(STR(lnCont)), lcSz ,'B', lcQty , STYLE.DESC1 , STYLE.STYLE ,lcOprDesc  , EVALUATE(lcTmpDT +'.DueDate'))                   
               *   lnOprBrLin = lnOprBrLin +1 
               *ELSE
               *  lcOpr = 'Opr'+ALLTRIM(STR(lnOprBrLin))
               *  SELECT(lcTemp)
               *  lcOprDesc =  gfCodDes(EVALUATE(lcTmpDT +'.cOprCode'),'MfgCode')
               *  REPLACE &lcOpr WITH lcOprDesc
                * lnOprBrLin = IIF(lnOprBrLin < 3 ,lnOprBrLin +1 , 1)
               *ENDIF   
               *C101939,1 [End]              
             *ENDSCAN
           *ELSE
           *C101939,1 [End]
             IF SEEK('I' + POSLN.PO , (lcTmpHD))
               *C101939,1 HBG 10/22/2000 set the variabel which control rinting 3 operation ber line to 1[End]
               lnOprBrLin = 1
               *C101939,1 [End]
               SELECT (lcTmpHD)
               laRelated[1,1] = 'LEADTIME  '
               laRelated[1,2] = 'lnReturn'
               SCAN FOR 'I'+ POSLN.PO = 'I' + cTktNo
                 =GfRltFld(cOprCode , @laRelated , 'MFGCODE')
                 ldDue = gdSysDate + lnReturn
                 *C101939,1 HBG 10/22/2000 Insert line for Style Notepad and line for each 3 operations[Begin]
                 *INSERT INTO (lcTemp) (CUTTKT         , SIZE , QTY   , Desc        , STYLE       , Opr               , Due) ;
                               VALUES (POSLN.PO       , lcSz , lcQty , STYLE.DESC1 , STYLE.STYLE , EVALUATE(lcTmpHD +'.cOprCode') , ldDue)
                 IF lnOprBrLin = 1
                   lcOprDesc =  gfCodDes(EVALUATE(lcTmpHD +'.cOprCode'),'MfgCode')
                   *B803877,1 HBG 12/07/2000 save Order#[begin]
                   *INSERT INTO (lcTemp) (CUTTKT        ,Sizenum , SIZE,Type  , QTY   , Desc        , STYLE    ,OprDesc1, Opr1   , Due) ;
                                VALUES (POSLN.PO       ,ALLTRIM(STR(lnCont)), lcSz ,'B' , lcQty , STYLE.DESC1 , STYLE.STYLE,lcOprDesc ,EVALUATE(lcTmpHD +'.cOprCode'), ldDue)
                   INSERT INTO (lcTemp) (ORDER, CUTTKT        ,Sizenum , SIZE,Type  , QTY   , Desc        , STYLE    ,OprDesc1, Opr1   , Due) ;
                                VALUES (CUTPICK.Order, POSLN.PO       ,ALLTRIM(STR(lnCont)), lcSz ,'B' , lcQty , STYLE.DESC1 , STYLE.STYLE,lcOprDesc ,EVALUATE(lcTmpHD +'.cOprCode'), ldDue)
                   *B803877,1 [End]                          
                   lnOprBrLin = lnOprBrLin +1
                 ELSE
                   lcOpr = 'Opr'+ALLTRIM(STR(lnOprBrLin))
                   lcOprDes = 'OprDesc'+ALLTRIM(STR(lnOprBrLin))
                   SELECT(lcTemp)
                   lcOprDesc =  gfCodDes(EVALUATE(lcTmpHD +'.cOprCode'),'MfgCode')
                   REPLACE &lcOpr WITH EVALUATE(lcTmpHD +'.cOprCode')
                   REPLACE &lcOprDes WITH lcOprDesc
                   lnOprBrLin = IIF(lnOprBrLin < 3 ,lnOprBrLin +1 , 1)
                 ENDIF   
                 *C101939,1 [End]                          
               ENDSCAN
             ENDIF
           *C101939,1 HBG 10/22/2000 Comment the End Of if seek in lcTmpHL [Begin]
           *ENDIF
           *C101939,1 [End]
         SELECT SCALE
         ENDIF
       ENDFOR
       *B803877,1 HBG 12/07/2000 Get order notpad insted of style notepad[begin]
       *IF SEEK('F' + STYLE.cstymajor,'NOTEPAD') AND lnSize <> 0
        * INSERT INTO (lcTemp) (CUTTKT         ,Sizenum, SIZE ,Type , QTY   , Desc        , STYLE       ,mNotes) ;
        *              VALUES (POSLN.PO ,ALLTRIM(STR(lnSize)), lcSize , 'A' , lcQuty , STYLE.DESC1 , STYLE.STYLE ,NotePad.mNotes)                   
       *ENDIF
       SELECT ORDLINE
       lcOrdTag = TAG()
       SET ORDER TO ORDLINE 
       SELECT SCALE      
       IF SEEK('O'+CUTPICK.Order+PADL(POSLN.lineno,6),'ORDLINE')
         IF !EMPTY(ORDLINE.note_mem) AND lnSize <> 0
           INSERT INTO (lcTemp) (ORDER,CUTTKT         ,Sizenum, SIZE ,Type , QTY   , Desc        , STYLE       ,mNotes) ;
                        VALUES (CUTPICK.Order,POSLN.PO ,ALLTRIM(STR(lnSize)), lcSize , 'A' , lcQuty , STYLE.DESC1 , STYLE.STYLE ,ORDLINE.note_mem)                   
         ENDIF
       ENDIF
       SET ORDER TO &lcOrdTag IN OrdLine
       *B803877,1 [End]
       *C101939,1 HBG 10/22/2000 Save informtion of this lne to be used in printing not pad and total Qty[Begin]
       *B803877,1 HBG 12/07/2000 save Order#[begin]
       lcOrder  = CUTPICK.Order
       *B803877,1 [End]
       lcCutTkt = POSLN.PO
       lcStyle  = STYLE.STYLE
       lcDesc   = STYLE.DESC1
       lnTotQty = lnTotQty  + POSLN.TotQty
       *C101939,1 [EnD]
     ENDSCAN
     *C101939,1 HBG 10/22/2000 Get the cuttkt Notepad And TotQty[Begin]
     IF lnSize <> 0
       IF llPrnNot AND SEEK('I' + lcCutTkt ,'NOTEPAD')
         *B803877,1 HBG 12/07/2000 save Order#[begin]
         *INSERT INTO (lcTemp) (CUTTKT        ,Sizenum , SIZE ,type , STYLE , Desc,mNotes,TotQty) ;
                      VALUES (lcCutTkt       ,ALLTRIM(STR(lnSize)), lcSize ,'B', lcStyle , lcDesc ,NotePad.mNotes,lnTotQty)
         INSERT INTO (lcTemp) (ORDRE, CUTTKT        ,Sizenum , SIZE ,type , STYLE , Desc,mNotes,TotQty) ;
                      VALUES (lcOrder ,lcCutTkt       ,ALLTRIM(STR(lnSize)), lcSize ,'B', lcStyle , lcDesc ,NotePad.mNotes,lnTotQty)             
         *B803877,1 [End]             
       ELSE
         *B803877,1 HBG 12/07/2000 save Order#[begin]
         *INSERT INTO (lcTemp) (CUTTKT        ,Sizenum , SIZE ,type , STYLE , Desc,TotQty) ;
                      VALUES (lcCutTkt       ,ALLTRIM(STR(lnSize)), lcSize ,'B', lcStyle , lcDesc ,lnTotQty)
         INSERT INTO (lcTemp) (ORDER, CUTTKT        ,Sizenum , SIZE ,type , STYLE , Desc,TotQty) ;
                      VALUES (lcOrder , lcCutTkt       ,ALLTRIM(STR(lnSize)), lcSize ,'B', lcStyle , lcDesc ,lnTotQty)
          *B803877,1 [End]                          
       ENDIF               
     ENDIF 
     *C101939,1 [End]
	 *B803877,1 HBG 12/07/2000 restore old index [Begin]
     SELECT CUTPICK
     SET ORDER TO &lcOldTag 
     *B803877,1 [End]
	 SELECT (lcTemp)
	 SET RELATION TO 'I' + CUTTKT + STYLE INTO (lcTmpHD) ADDITIVE
ENDCASE

SELECT (lcTemp)
GOTO TOP
IF RECCOUNT() = 0
  *C101939,1 HBG 10/22/2000 Comment these lines because they are no longer used [Begin]
  *IF lcRpPrWt = 'C'
   * = gfModalGen("INM00052B00000" , "DIALOG")
  *ENDIF
  *C101939,1 [End]
  *: B#803342,1 SSH Set device to screen
  SET DEVICE TO SCREEN
  Return
ENDIF
  
*-- When we press run the select device screen appeares again

lldummy = .T.
*C101939,1 HBG 10/22/2000 option 'Form to print' is removed from OG,so we can't use it[Begin]
*IF gcDevice <> 'SCREEN' AND lcRpPrWt # 'C'
IF gcDevice <> 'SCREEN'
*C101939,1 [End]
  SET DEVICE TO SCREEN
  *: B#803342,1 SSH Print Tr. Ticket Even if not generated from SO
  lcOldOgPlt = OGPlatForm
  OGPlatForm = 'WINDOWS'
  lldummy = pSetup(.T.,llOGBatchOk)
  OGPlatForm = lcOldOgPlt
  *: B#803342,1 SSH [END]
ENDIF
*: B803310,1 SSH Remove Style Separator.
*-- call the report
*C101939,1 HBG 10/22/2000 Comment these lines because they are no longer used [Begin]
*IF lldummy
 * lcOldFrm = lcOGTmpForm
 * lcOGTmpForm = ""
 * OGPlatForm = 'WINDOWS'
 * =gfDispRe( lcRepNam ,.F.,.F.,'L')
 * lcOGTmpForm = lcOldFrm
*ENDIF
*DO gfDispRep WITH EVAL('lcRepNam')
*: B803310,1 SSH Remove Style Separator.[END]
*-- Restore the saved settings
*C101939,1 [End]

SET DEVICE TO SCREEN
*C101939,1 HBG 10/22/2000 Comment these lines because they are no longer used [Begin]
*lcRepNam = IIF(lcRpSele = 'S' , 'MFRBR10A' , 'MFRBR10B')
*= lfRepPltFr(lcRepNam)
*lcRepNam    = lcOldFrm


*SELECT (lcTmpHD)
*CLOSE INDEX
*USE IN (lcTmpHD)
*ERASE (gcWorkDir + lcHDIndx + '.CDX')
*
*SELECT (lcTmpDT)
*CLOSE INDEX
*USE IN (lcTmpDT)
*ERASE (gcWorkDir + lcDTIndx + '.CDX')


*SELECT ORDLINE
*SET ORDER TO ORDLINES
*C101939,1 [End]

SELECT (lcTemp)
SET RELATION TO
*C101939,1 HBG 10/22/2000 Comment these lines because they are no longer used [Begin]
*USE
*C101939,1 [End]
SELECT (lcAlias)

*!*************************************************************
*! Name      : lfAdjSeg
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : Get the style code segments information.
*!*************************************************************
*! Called from : SOSRORD.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfAdjSeg()
*!*************************************************************
FUNCTION lfAdjSeg

STORE 0 TO lnCPos
*-- laMajSeg array holds the style code segments data
*-- laMajSeg[x,1] Holds segment type
*-- laMajSeg[x,2] Holds segment title
*-- laMajSeg[x,3] Holds segment picture
*-- laMajSeg[x,4] Holds segment Starting position
*-- laMajSeg[x,5] Holds segment description
*-- laMajSeg[x,6] Holds segment separator
*-- laMajSeg[x,7] Holds (.T./.F.) segment end major.
DIMENSION laMajSeg[1,1]

= gfItemMask(@laMajSeg)
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
FOR lnC = 1 TO ALEN(laMajSeg,1)
  *-- If the style major consists of one segment, don't display it, 
  *-- display the style major instead (style major will browse from the 
  *-- style file directly)
  IF lnC = 1 .AND. lnMajSeg = 1 
    LOOP
  ENDIF
  IF laMajSeg[lnC,1] = 'C'
    *lnCPos = lnC
    lnCPos = LEN(laMajSeg[lnC -1 ,3])
    IF EMPTY(laMajSeg[lnC,5])
      laMajSeg[lnC,5] = 'Color'
    ENDIF
    lnCLen = LEN(laMajSeg[lnC,3])
    *B803310,1 SSH Get Color Position.
    *EXIT
    lnClrPos = laMajSeg[lnC,4]
    *B803310,1 SSH [End]
  ENDIF
  *B803310,1 SSH Get Scale Position.
  IF laMajSeg[lnC,1] = 'S'
    *lnCPos = lnC
    lnCPos = LEN(laMajSeg[lnC -1 ,3])
    IF EMPTY(laMajSeg[lnC,5])
      laMajSeg[lnC,5] = 'Color'
    ENDIF
    lnSclPos = laMajSeg[lnC,4]
    EXIT
  ENDIF
  *B803310,1 SSH [END]
ENDFOR


*!*************************************************************
*! Name      : lfvPrtCs
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : Valid function of the print cost sheet setting on the option
*!             grid to enable or disable the setting of print cost on cost sheet.
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

IF ASCAN(laOgObjType,'lcRpPrnC') # 0
  lcRpPBOM = IIF(lcRpPBOM = 'Y' , .T. , .F.)
  lnPos= ASUBSCRIPT(laOgObjType,ASCAN(laOgObjType,'lcRpPrnC'),1)
  laOGObjCnt[lnPos] = lcRpPBOM
  = lfOGShowGet('lcRpPrnC')
  lcRpPBOM = IIF(lcRpPBOM = .T. , 'Y' , 'N')
ENDIF



*!*************************************************************
*! Name      : lfwWhen
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : The report when function
*!*************************************************************
*! Called from : MFRBR10.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwWhen()
*!*************************************************************
FUNCTION lfwWhen

SELECT ORDLINE
*B604181,1 HBG 02/06/2001 Correct the relation to print order line notepad correctly [Begin]
*INDEX ON cOrdType + Order + STYLE TAG lcOrdLn OF (gcWorkDir +  lcOrdLn + '.CDX')
INDEX ON cOrdType + Order + STR(lineno,6) + STYLE TAG lcOrdLn OF (gcWorkDir +  lcOrdLn + '.CDX')
*B604181,1 [End]

*!*************************************************************
*! Name      : lfClearRep
*! Developer : RAMY MABROUK
*! Date      : 01/16/2000
*! Purpose   : Clear The report function
*!*************************************************************
*! Called from : MFRBR10.PRG
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClearRep()
*!*************************************************************

FUNCTION lfClearRep

IF USED(lcTmpHD)
  SELECT (lcTmpHD)
  CLOSE INDEX
  USE IN (lcTmpHD)
  ERASE (gcWorkDir + lcHDIndx + '.CDX')

  SELECT (lcTmpDT)
  CLOSE INDEX
  USE IN (lcTmpDT)
  ERASE (gcWorkDir + lcDTIndx + '.CDX')
ENDIF

IF USED('WAREHOUS')
  SELECT WAREHOUS
  USE
ENDIF
IF USED('OBJECTS')
  SELECT OBJECTS
  USE
ENDIF
IF USED('OBJLINK')
  SELECT OBJLINK
  USE
ENDIF
IF USED('CODES')
  SELECT CODES
  USE
ENDIF
IF USED('SYCCOMP') AND llOpnComp
  SELECT SYCCOMP
  USE
ENDIF

SELECT ORDLINE
CLOSE INDEX
ERASE (gcWorkDir + lcOrdLn + '.CDX')


*!*************************************************************
*! Name      : lfRetSty
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/16/2000
*! Purpose   : Remove Separator from style code.
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfRetSty()
*!*************************************************************
FUNCTION lfRetSty
PRIVATE lcToRet
*--- lnCLen
*--- lnClrPos
*--- lnSclPos
*--- lnMajLen
*--- Example : lcToRet  = SUBSTR(Style,1,11)+SUBSTR(Style,13,lnCLen)+;
                          SUBSTR(Style,17)
lnMajLen = LEN(gfitemmask("PM"))
*: B#803342,1 SSH Commented Out
*lcToRet  = SUBSTR(Style,1,lnMajLen)+SUBSTR(Style,lnClrPos,lnCLen)+;
                          SUBSTR(Style,lnSclPos)
lcToRet  = Style
RETURN(lcToRet)


*!*************************************************************
*! Name      : lfReadBc
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/16/2000
*! Purpose   : Function to read bar code
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfReadBc()
*!*************************************************************
*: B#803342,1 SSH Bar Code function
FUNCTION lfReadBc
Para lcPasBarCd, lcRW
PRIVATE lnAlias

lnAlias = SELECT()
lcBarCode = ""

SELECT cTktBarC
IF lcRW = 'W'
   SET ORDER TO TAG Corgcode   
   IF !SEEK(lcPasBarCd)
     *--- SSH
     *lcBarCode = SYS(2007,PADR(lcPasBarCd,60))
     SET ORDER TO TAG Cgencode DESCENDING
     GO TOP
     lcBarCode = PADL(ALLTRIM(STR(VAL(Cgencode)+1)),10,'0')
     DO WHILE SEEK(lcBarCode)
       lcBarCode = PADL(ALLTRIM(STR(VAL(lcBarCode)+1)),10,'0')
     ENDDO
     *--- SSH
     APPEND BLANK
     REPLACE cOrgCode WITH lcPasBarCd,;
             cGenCode WITH lcBarCode
   ELSE
     lcBarCode = cGenCode
   ENDIF
ENDIF
IF lcRW = 'R'
   SET ORDER TO TAG Cgencode
   IF SEEK(lcPasBarCd)
     lcBarCode = cOrgCode
   ELSE
     *--Error
     lcBarCode = "**"
   ENDIF
ENDIF
SELECT(lnAlias )
RETURN ALLT(lcBarCode)

*!*************************************************************
*! Name      : lfPrntHdr
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/16/2000
*! Purpose   : Function to Print Header.
*!*************************************************************
*! Called from : FRX
*!*************************************************************
*! Calls       : None.
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfPrntHdr()
*!*************************************************************
*: B#803342,1 SSH Print Header
*C101939,1 HBG 10/22/2000 Comment these lines because they are no longer used [Begin]
*FUNCTION lfPrntHdr
*Private lcVar1, lcVar2, lcVar3, lcVar4

*lcVar1 = "Stock Order # : "+left(IIF(gcAct_Appl = 'MF' , CUTTKTH.CutTkt , POSHDR.PO),6)
*lcVar2 = "     Order Date: "+left(IIF(gcAct_Appl = 'MF' , DTOC(CUTTKTH.Entered) , DTOC(POSHDR.Entered)),10)
*lcVar3 = "     W/O Status: "+left(IIF(gcAct_Appl = 'MF' , IIF(CUTTKTH.Status = 'C' , 'Completed' , IIF(CUTTKTH.Status = 'X' , 'Canceled' , 'Open')) , IIF(POSHDR.Status = 'C' , 'Completed' , IIF(POSHDR.Status = 'X' , 'Canceled' , 'Open'))),9)
*lcVar4 = "     Page: "+PADR(_PageNo,3)
*Return(lcVar1+lcVar2+lcVar3+lcVar4)



*****************
*****************

*-------------------------------------------
*!*************************************************************
*! Name      : lfGetLogo
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/22/2000
*! Purpose   : Function to Save the company logo in temp. file 
*!             which is used after this to print the logo for company.
*!*************************************************************
*! Called from : MFRBR10.PRG
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
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/22/2000
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : MFRBR10.PRG 
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

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])
    
    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*--FOR Loop to loop the Address Array
FOR lnCount = 1 TO 5
  
  *--IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
*-- end of lfAdrShift.

*!*************************************************************
*! Name      : lfGetStatu
*! Developer : Hend Ghanem (HBG)
*! Date      : 10/22/2000
*! Purpose   : Function to Get the Stats
*!*************************************************************
*! Called from : .FRX
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetStatu()
*!*************************************************************
FUNCTION lfGetStatu

lcStatus = IIF(lcRpSele = 'C',IIF(gcAct_Appl = 'MF' ,; 
IIF(CUTTKTH.Status = 'C' , 'Completed' , IIF(CUTTKTH.Status = 'X' , 'Canceled' , 'Open')) ,;
IIF(POSHDR.Status = 'C' , 'Completed' , IIF(POSHDR.Status = 'X' , 'Canceled' , 'Open'))),;
IIF(ORDHDR.Status = 'C' , 'Completed' , IIF(ORDHDR.Status = 'X' , 'Canceled' , 'Open')))

RETURN

*-- end of lfGetStatu.