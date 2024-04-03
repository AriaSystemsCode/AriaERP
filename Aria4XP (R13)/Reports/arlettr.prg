*:***************************************************************************
*: Program file  : ARLETTR
*: Program desc. : Customer Letters Report
*: For Report    : (ARLETTR.FRX)
*: System        : Aria4xp
*: Module        : Accounts Receivable (AR)
*: Developer     : Hassan Ibrahim Ali
*: Ticket        : T20061226.0016
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO ARLETTR
*:***************************************************************************
*: Modifications :
*: E302358,1 HIA 03/05/2007 convert the customer letter report to Aria4XP
*:***************************************************************************
*
#INCLUDE r:\aria4xp\reports\arlettr.h
IF EMPTY(lcRpPrint)
  *Message : 04192 => No XXX letters found, Cannot proceed!
  *Button  : 00000 => <Ok>

  * N000682 ,1 Thabet Handle globalization issues [Start]
  *= gfModalGen('TRM04192B00000','Dialog',"General Purpose")
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*= gfModalGen('TRM04192B00000','Dialog',LANG_GENERAL_PURPOSE)
= gfModalGen('TRM04192B00000','Dialog',IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_GENERAL_PURPOSE,oAriaApplication.GetHeaderText("LANG_GENERAL_PURPOSE",AHEADERFILE)))
*N000682,1 11/20/2012 MMT Globlization changes[End]

  * N000682 ,1 Thabet Handle globalization issues [END]
  RETURN
ENDIF

SET PROCEDURE TO (gcRepHome+"ARPLETT.fxp")
IF "Ext" $ lcRpLetTyp AND EMPTY(ldRpComplt)
  =lfvNewDate(.T.)
  RETURN
ENDIF

lcStarTime = TIME()
SET CENTURY ON

GO TOP IN (lcTempMast)
IF (lcRpPrint <> "R") AND EMPTY(&lcActivObj) AND EOF(lcTempMast)
  llHaveRecs = .F.
ELSE
  llHaveRecs = &lcRpLetTyp		&& Do Report/Letter Type.
ENDIF

IF !llHaveRecs
  *-- Message : There are no records to display...!
  *--                < Ok >
  *E302358,1 HIA 03/05/2007 convert the customer letter report to A4 [Begin]
  =gfModalGen('TRM00052B40011','ALERT')
  *E302358,1 HIA 03/05/2007 convert the customer letter report to A4 [End]
  RETURN
ENDIF
*-- end of Report Program code.

*!*************************************************************
*! Name      : lfInitial
*! Developer : Mohamed Badran (MAB)
*! Date      : 10/31/1999
*! Purpose   : Initial function which change procedure direction.
*!*************************************************************
*E301119,1
FUNCTION lfInitial
  lcOldProc = SET("PROCEDURE")
  SET PROCEDURE TO (gcRepHome+"ARPLETT.fxp")
  *-- end of lfInitial.

  *!*************************************************************
  *! Name      : lfSoldTo
  *! Developer : Mohamed Badran (MAB)
  *! Date      : 10/31/1999
  *! Purpose   : Sold to addresses.
  *!*************************************************************
  *
FUNCTION lfSoldTo
  *= gfRltFld(ORDHDR.cDivision , @laDivLName , 'CDIVISION')  && Get the division long name.

  lcSolTName = Account + " - " + Customer.BTName
  laSoldTo = ''
  =gfGetAdr('CUSTOMER' , '' , '' , '' , @laSoldTo,'2')
  = lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.
  = (lnRepStep = 2) AND lfAccInfo()
  = llGetSalut AND lfSalutes()
  RETURN ''
  *-- end of lfSoldTo.

  *!*************************************************************
  *! Name      : lfAccInfo
  *! Developer : Mohamed Badran (MAB)
  *! Date      : 10/31/1999
  *! Purpose   : 1- Get Customer Default letter.
  *!*************************************************************
  *
FUNCTION lfAccInfo
  PRIVATE llSeekMe , lcSeekVal
  llSeekMe = .T.
  *-- if user select default per account.
  IF EMPTY(&lcActivObj)

    IF RECCOUNT(lcTempMast) > 0 AND SEEK(Account,lcTempMast) AND;
        !EMPTY(&lcTempMast..cDefaLett)
      lcSeekVal = lcLetrTo + &lcTempMast..cDefaLett

    ELSE  && Get Default Letter and Do not seek again.

      SET ORDER TO Leterdefa IN LETTERS
      lcSeekVal = "D"+lcLetrTo+SUBSTR(lcActivObj,5,1)
      *-- if No default for this letter.
      IF !SEEK(lcSeekVal,"LETTERS")
        *-- First letter will be default for this letter.
        lcSeekVal = SPACE(1)+lcLetrTo+SUBSTR(lcActivObj,5,1)
        =SEEK(lcSeekVal,"LETTERS")
      ENDIF
      llSeekMe = .F.  && Do not seek me again.

    ENDIF

  ELSE  && user default a letter.

    lnRepStep = 1 && Do not call me again.
    lcSeekVal = lcLetrTo + &lcActivObj
  ENDIF

  IF llSeekMe
    SET ORDER TO Letterto IN LETTERS
    = SEEK(lcSeekVal,"LETTERS")
  ENDIF
  *-- end of lfAccInfo.

  *!*************************************************************
  *! Name      : lfSalutes
  *! Developer : Mohamed Badran (MAB)
  *! Date      : 10/31/1999
  *! Purpose   : 2- Get Customer Salutations.
  *!*************************************************************
  *
FUNCTION lfSalutes
  ***********************************
  SET ORDER TO Saluteid IN Salutes
  IF EMPTY(Customer.cTopSalut) OR !SEEK(Customer.cTopSalut+"T","Salutes")
    lcTopSalut = lcTopDefa
  ELSE
    lcTopSalut = Salutes.cSalDesc
  ENDIF

  IF EMPTY(Customer.cBotSalut) OR !SEEK(Customer.cBotSalut+"B","Salutes")
    lcBotSalut = lcBotDefa
  ELSE
    lcBotSalut = Salutes.cSalDesc
  ENDIF
  *-- end of lfSalutes.
