*:***************************************************************************
*: Program file  : APLETTR
*: Program desc. : Vendor Letters Report (302174,1)
*: For Report    : (APLETTR.FRX)
*: System        : Aria Advantage Series.
*: Module        : Accounts Payable (AP)
*: Developer     : Albert Raif (ALB)
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Example : DO APLETTR
*:***************************************************************************
*: This Report Program is due to 302174,1...
*:***************************************************************************
*: Modifications :
*:***************************************************************************
*
WAIT WINDOW "Vendor Collection code..." NOWAIT
SET PROCEDURE TO (gcRepHome+"ARPLETT")
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
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF
*-- end of Report Program code.

*!*************************************************************
*! Name      : lfInitial
*! Developer : Albert Raif (ALB)
*! Date      : 04/06/2003
*! Purpose   : Initial function which change procedure direction.
*!*************************************************************
*
FUNCTION lfInitial
lcOldProc = SET("PROCEDURE")
SET PROCEDURE TO (gcRepHome+"ARPLETT")
*-- end of lfInitial.

*!*************************************************************
*! Name      : lfBuyFrom
*! Developer : Albert Raif (ALB)
*! Date      : 04/06/2003
*! Purpose   : Sold to addresses.
*!*************************************************************
*
FUNCTION lfBuyFrom

lcSolTName = cVendCode + " - " + APVENDOR.cVenComp
laSoldTo = ''
laSoldTo[1]   = gfGetAdr('APVENDOR', 'VENCODE',cVendCode ,APVENDOR.cCont_Code,1)
laSoldTo[2]   = gfGetAdr('APVENDOR', 'VENCODE',cVendCode ,APVENDOR.cCont_Code,2)
laSoldTo[3]   = gfGetAdr('APVENDOR', 'VENCODE',cVendCode ,APVENDOR.cCont_Code,3)
laSoldTo[4]   = gfGetAdr('APVENDOR', 'VENCODE',cVendCode ,APVENDOR.cCont_Code,4)
laSoldTo[5]   = gfGetAdr('APVENDOR', 'VENCODE',cVendCode ,APVENDOR.cCont_Code,5)

= lfAdrShift('laSoldTo')  && Shift Sold To address if there is empty line.
= (lnRepStep = 2) AND lfAccInfo()

RETURN ''
*-- end of lfSoldTo.
*!*************************************************************
*! Name      : lfAccInfo
*! Developer : 
*! Date      : 
*! Purpose   : 1- Get Vendor Default letter.
*!*************************************************************
*
FUNCTION lfAccInfo
PRIVATE llSeekMe , lcSeekVal
llSeekMe = .T.
*-- if user select default per account.
IF EMPTY(&lcActivObj)

  IF RECCOUNT(lcTempMast) > 0 AND SEEK(cvendcode,lcTempMast) AND;
     !EMPTY(&lcTempMast..cDefaLett)
    lcSeekVal = lcLetrTo + &lcTempMast..cDefaLett
  
  ELSE  && Get Default Letter and Do not seek again.

    SET ORDER TO Leterdefa IN LETTERS
    lcSeekVal = "V"+lcLetrTo+SUBSTR(lcActivObj,5,1)
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

lcTopSalut = lcTopDefa
lcBotSalut = lcBotDefa
*-- end of lfAccInfo.

*!*************************************************************
*! Name      : lfvGetSal
*! Developer : Albert Raif (ALB)
*! Date      : 04/06/2003
*! Purpose   : Get Salute For Each Vendor
*!*************************************************************
*

FUNCTION lfvGetSal
PARAMETER lcVendCode
PRIVATE lcOldAlias,lcOldOrder
lcOldAlias = ALIAS()
IF SEEK(lcVendCode,"APVENDOR")
  SELECT SALUTES
  lcOldOrder = ORDER()
  SET ORDER TO SALUTEID
  IF !EMPTY(APVENDOR.CTOPSALUT) AND SEEK(APVENDOR.CTOPSALUT+"TV")
    lcTopSalut = CSALDESC
  ELSE
   lcTopSalut = lcTopDefa
  ENDIF
  IF !EMPTY(APVENDOR.CBOTSALUT) AND SEEK(APVENDOR.CBOTSALUT+"BV")
    lcBotSalut = CSALDESC
  ELSE
    lcBotSalut = lcBotDefa
  ENDIF
ENDIF
SELECT SALUTES
SET ORDER TO (lcOldOrder)
SELECT (lcOldAlias)
RETURN ""