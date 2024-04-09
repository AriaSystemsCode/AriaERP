*:***************************************************************************
*: Program file  : SRAGERP
*: Program desc. : Age Commession payable
*: For screen    : ....
*:        System : Aria Advantage Series.4XP
*:        Module : Sales Rep. (SR)
*:     Developer : Mariam Mazahr (MMT) (N037504 [T20080821.0018])
*:***************************************************************************
PARAMETERS lcRepCode
#INCLUDE r:\aria4xp\prgs\sr\SRAGERP.h
STORE .F. TO llRepFile,llCommFile,llContinue

lcCommOrd = ''
lnCommRec = 0

lcTempRep = gfTempName() && Name of file that hold temporary Sales Rep. data.
loToolBarWindow = oAriaApplication.oToolBar.oWindParent
*-- Open required files. [begin]
IF !USED('SalesRep')
  llRepFile = gfOpenTable('SalesRep','SalesRep')
ELSE
  lcRepOrd = ORDER('SalesRep')
  lnRepRec = RECNO('SalesRep')
ENDIF
*-- Open required files. [end]

*N000682,1 12/12/2012 TMI Globlization changes[Start] Define the language related
lcHeaderFile  = ' '
LOCAL loFormSet
loFormSet = oAriaApplication
if type('HeaderAlias')='U'
  loFormSet.AddProperty('HeaderAlias','')
endif 
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcHeaderFile = ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "PRGS\sr\SRAGERP_H.XML"
  loFormSet.HeaderAlias = oAriaApplication.GetClassHeaderFile(lcHeaderFile)
ENDIF
*N000682,1 12/12/2012 TMI Globlization changes[Start]


SELECT SalesRep

*-- if calling from another program, not empty and valid Rep. Code .
IF TYPE('lcRepCode') = 'C' AND !EMPTY(lcRepCode) AND gfSEEK(lcRepCode)
  DIMENSION laRepStruc[1,4]

  laRepStruc[1,1] = 'RepCode'
  laRepStruc[1,2] = 'C'
  laRepStruc[1,3] = 3
  laRepStruc[1,4] = 0

  =gfCrtTmp(lcTempRep,@laRepStruc,'RepCode',lcTempRep,.f.)

  INSERT INTO (lcTempRep)                ;
              (RepCode) VALUES (lcRepCode)
  llContinue = .T.
ELSE
  lcBrowFlds = [RepCode   :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Rep,loFormSet.GetHeaderText("Lang_Rep",loFormSet.HeaderAlias))+[' :10,]    + ;
               [Name      :H= ']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Name,loFormSet.GetHeaderText("Lang_Name",loFormSet.HeaderAlias))+[':35 ,]    + ;
               [Phone     :H =']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Phone,loFormSet.GetHeaderText("Lang_Phone",loFormSet.HeaderAlias)) +[':16,]   + ;
               [cAddress6 :H =']+ IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Country,loFormSet.GetHeaderText("Lang_Country",loFormSet.HeaderAlias))+[':30,] + ;
               [Balance   :H =']+IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Balance,loFormSet.GetHeaderText("Lang_Balance",loFormSet.HeaderAlias))+[']

  *N000682,1 12/25/2012 TMI Globlization changes[Start] 
  *llContinue = gfBrowse(lcBrowFlds,IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Rep,loFormSet.GetHeaderText("Lang_Rep",loFormSet.HeaderAlias)),"SALESREP",'',.F.,.F.,.T.,.F.,.F.,.F.,;
                        lcTempRep,'RepCode',.F.,.F.,.F.,.F.,.F.,.F.,"SALESREP")
  lcFile_Ttl = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_Rep,loFormSet.GetHeaderText("Lang_Rep",loFormSet.HeaderAlias))                       
  llContinue = gfBrowse(lcBrowFlds,lcFile_Ttl,"SALESREP",'',.F.,.F.,.T.,.F.,.F.,.F.,;
                        lcTempRep,'RepCode',.F.,.F.,.F.,.F.,.F.,.F.,"SALESREP")
  *N000682,1 12/25/2012 TMI Globlization changes[End  ] 
ENDIF

IF llContinue
  DO lpAging  && Call procedure that evaluate RepCode aging..
ENDIF

*-- Close open files [Begin]
USE IN (lcTempRep)   && Erase cursor.

IF llRepFile
  =gfCloseTable('SALESREP')
ELSE
  SELECT SalesRep
  gfSetOrder(&lcRepOrd)
  IF lnRepRec > 0 AND lnRepRec <= RECCOUNT('SalesRep')
    GO lnRepRec
  ENDIF
ENDIF

IF llCommFile
  =gfCloseTable('REPCOMM')
ELSE
  IF USED('REPCOMM')
    SELECT RepComm
    gfSetOrder(&lcCommOrd)
    IF lnCommRec > 0 AND lnCommRec <= RECCOUNT('RepComm')
      GO lnCommRec
    ENDIF
  ENDIF

ENDIF
*-- Close open files [End.]
*-End of program code...

*!*************************************************************
*! Name      : lpAging
*! Developer : Mariam Mazahr (MMT)
*! Date      : 08/26/2008
*! Purpose   : Initiall variables used in calculate aging and loop around
*!           : temporary cursor hold all Rep Codes.
*!*************************************************************
*! Calls     :
*!             Procedures : lpCalAge
*!             Functions  : gfThermo
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpAging
*!*************************************************************
PROCEDURE lpAging
PRIVATE lcCommCode

*N000682,1 12/12/2012 TMI Globlization changes[Start] Define the language related
lcHeaderFile  = ' '
LOCAL loFormSet
loFormSet = oAriaApplication
if type('HeaderAlias')='U'
  loFormSet.AddProperty('HeaderAlias','')
endif 
IF oAriaApplication.oActivelang.cLang_ID <> "EN"
  lcHeaderFile = ADDBS(UPPER(ALLTRIM(oAriaApplication.LangPath))) + "PRGS\sr\SRAGERP_H.XML"
  loFormSet.HeaderAlias = oAriaApplication.GetClassHeaderFile(lcHeaderFile)
ENDIF
*N000682,1 12/12/2012 TMI Globlization changes[Start]

*-- Start calculations.
SELECT (lcTempRep)
COUNT TO lnAllReps FOR !DELETED()  && All Accounts in Cursor file.
lnThermNo  = 0					&& Initiall Value for thermo Fn.

IF lnAllReps > 0

  IF !USED('RepComm')

    llCommFile = gfOpenTable('RepComm','RepComm','SH')

  ELSE

    IF !llCommFile
      lcCommOrd = ORDER('RepComm')
      lnCommRec = RECNO('RepComm')
    ENDIF

  ENDIF

ENDIF

SELECT (lcTempRep)
oProgress = NEWOBJECT('ariaprogressbar',oAriaApplication.classdir+'utility.vcx')
oProgress.TotalProgress = lnAllReps
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*oProgress.lblFirstLabel.CAPTION = Lang_PogrssCaption
oProgress.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_PogrssCaption,loFormSet.GetHeaderText("Lang_PogrssCaption",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

oProgress.SHOW()


SELECT (lcTempRep)
SCAN
*N000682,1 11/20/2012 MMT Globlization changes[Start]
*	oProgress.lblFirstLabel.CAPTION = Lang_PogrssCaption  + &lcTempRep..RepCode
	oProgress.lblFirstLabel.CAPTION = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",Lang_PogrssCaption,loFormSet.GetHeaderText("Lang_PogrssCaption",loFormSet.HeaderAlias))  + &lcTempRep..RepCode
*N000682,1 11/20/2012 MMT Globlization changes[End]

	oProgress.CurrentProgress(lnThermNo)
	lnThermNo = lnThermNo + 1
	=gfSEEK(&lcTempRep..RepCode,'SalesRep')
	lcCommCode = &lcTempRep..RepCode
	DO lpCalAge   && Calculate age for current account.
ENDSCAN         && end Scan all Accounts.
*-- End calculations.
oProgress=NULL
IF TYPE('oAriaApplication.oToolBar.oWindParent')<>'U'
  oAriaApplication.oToolBar.oWindParent = loToolBarWindow
ENDIF

*!*************************************************************
*! Name      : lpCalAge
*! Developer : Mariam Mazahr (MMT)
*! Date      : 08/26/2008
*! Purpose   : Calculate AR Aging.
*!*************************************************************
*! Calls     :
*!             Procedures : lpReplRep
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpCalAge
*!*************************************************************
PROCEDURE lpCalAge
*-- Define variables that calculates data in RepComm file. [begin]
STORE 0.00 TO lnCredit
STORE 0.00 TO lnAge00   ,;
              lnAge30   ,;
              lnAge60   ,;
              lnAge90   ,;
              lnAge120  ,;
              lnBalance
*-- Define variables that calculates data in RepComm file. [end]

SELECT SalesRep
DO lpReplRep  && Clear data in SalesRep file, Note that: all data equal zero.

*-- If you find Transactions in RepComm file. [begin]
IF gfSEEK(lcCommCode,'RepComm')
  lnHeader = 1
  SELECT RepComm
  *-- scan around this RepCode in RepComm file.
  SCAN REST WHILE REPCODE+DTOS(DATE)+TRAN+TRANTYPE = lcCommCode
    *-- Outer if Block [begin]
    *-- if lnHeader = 1 this means that first transaction on RepComm which
    *-- must contain the beginning balance, thus we not delete this record
    *-- if amount of it equal zero.
    *-- else if lnHeader > 1, it means that Process remaining transactions
    *-- on RepComm (financial history file)
    IF lnHeader >= 2 AND RepComm.Amount = 0
      BLANK
      DELETE
    ELSE
      *-- if amount is negative, it means that credit case.
      IF RepComm.Amount < 0
        lnCredit = lnCredit + RepComm.Amount
      ELSE  && Sales Rep. have a commission, thus calculate its age.
        *-- Computes age of a transaction based on commission date [begin]
        *-- lnDays : Hold Number of days after transaction.
        *-- lcAges : Hold Variable to be changed (Age transaction variable).
        lnDays    = DATE() - RepComm.Date
        lcAges = 'lnAge'+ IIF(lnDays >= 120,'120',IIF(lnDays >= 90 ,'90'  ,;
                          IIF(lnDays >= 60 ,'60' ,IIF(lnDays >= 30 ,'30','00'))))

        &lcAges = &lcAges + RepComm.Amount  && Accomulate Age variable
        *-- Computes age of a transaction based on commission date [end]
      ENDIF
    ENDIF
    *-- Outer if Block [end]
    lnHeader = lnHeader + 1    && Counter to know what record we in.
  ENDSCAN   && end scan around this RepCode in RepComm file.

  lnBalance = lnAge00 + lnAge30 + lnAge60 + lnAge90 + lnAge120
  lnCredit  = ABS(lnCredit)

  IF lnCredit >= lnBalance
    STORE (lnBalance - lnCredit) TO lnAge00,lnBalance
    STORE 0.00 TO lnCredit
    STORE 0.00 TO lnAge30   ,;
                  lnAge60   ,;
                  lnAge90   ,;
                  lnAge120

  ENDIF

  IF lnCredit != 0
    FOR lnI = 120 TO 0 STEP -30
      lcAges   = 'lnAge'+IIF(lnI = 0 , '00' , ALLTRIM(STR(lnI)))

      IF lnCredit >= &lcAges
         lnCredit  = lnCredit - &lcAges
         &lcAges   = 0.00
      ELSE
         &lcAges   = &lcAges - lnCredit
         lnCredit  = 0.00
      ENDIF

    ENDFOR
    lnBalance = lnAge00 + lnAge30 + lnAge60 + lnAge90 + lnAge120
  ENDIF

  *-- Update SalesRep File.
  SELECT SalesRep
  DO lpReplRep
ENDIF
SELECT SalesRep

gfTableUpdate()
*-- end of lpCalAge.
*!*************************************************************
*! Name      : lpReplRep
*! Developer : Mariam Mazahr (MMT)
*! Date      : 08/26/2008
*! Purpose   : Transfer Aged data to SalesRep file.
*!*************************************************************
*! Calls     :
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : DO lpReplRep
*!*************************************************************
PROCEDURE lpReplRep
gfREPLACE("AgeDate    WITH DATE()," + ;
          "Current    WITH lnAge00 ,"+ ;
	      "Age30      WITH lnAge30 ,"+;
	      "Age60      WITH lnAge60   ,"+;
    	  "Age90      WITH lnAge90   ,"+;
	      "Age120     WITH lnAge120  ,"+;
    	  "Balance    WITH lnBalance")
*-- end of lpReplRep.
