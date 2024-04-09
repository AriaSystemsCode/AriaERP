*:***************************************************************************
*: Program file  : ARPINVDR.PRG
*: Program desc. : Customized Invoice Form for Direct Corporate Clothing [DIR03]
*: Date          : 25/11/2008
*: System        : A4XP.
*: Module        : ACCOUNT RECEIVABLE (AR)
*: Developer     : Mostafa Eid (MOS)
*: Tracking Job Number : C201076
*:**************************************************************************
*: Calls : FUNCTIONS  :
*:**************************************************************************
*: Example : DO ARPINVDR
*:**************************************************************************
*:Modification: T20080807.0003 - Amendment to Invoice for DR for Direct Corporate Clothing
*- C201076
*! B609301,1 MMT 06/15/2010 Fix bug of wrong totals accumlated[T20100602.0013]
*! B610220,1 HIA 01/28/2013 Aria4xp - AR - Consolidated invoice [T20130109.0024]
*! C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024]
*! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice
*! B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024]
*! B610492,1 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024]
*! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][Start]
*! B610680,1 TMI 02/18/2014 AR - invoice charges missing from invoice print [T20140211.0003]
*! B611190,1 MMT 09/22/2016 Custom invoice form DR prints incorrect Gross price when sort by Employee[T20160909.0003]
*! B611190,2 Sara.O 09/27/2016 Error while closing og report after review [T20160909.0003]
*! B611204,1 MMT 10/16/2016 Custom Invoice form DR prints incorrect values (by Employee sort)[T20161005.0003]
*! B611522,1 MMT 02/04/2018 Incorrect VAT calculations in DCC custom invoice form[T20180126.0001]
*:**************************************************************************

PRIVATE lcCentury
STORE '' TO lcCentury
lcCentury = SET('CENTURY')
SET CENTURY ON

IF !USED('PIKTKT')
  = gfOpenFile(gcDataDir+'PIKTKT',gcDataDir+'ORDPIK','SH')
ENDIF

IF !USED('PACK_HDR')  = gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH')
ENDIF

IF !USED('INVCHRG')
  =gfOpenFile(gcDataDir+'INVCHRG','INVCHRG','SH')
ENDIF

IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
ENDIF

IF !USED('CONSINVL')
  =gfOpenFile(gcDataDir+'CONSINVL','CINVLINE','SH')
ENDIF

DO CASE
CASE lcRpFormN = 'S'
  lcFormName  = 'ARPINVM'
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][Start]
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.Report.lcOGPlatForm = 'ARPINVM'
  ENDIF
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][End]
  =gfCrtFrm(lcFormName,'',.T.)

  SELECT INVHDR
  DO (gcRepHome + 'ARPINVM.PRG')
  RETURN

  ** Consolidated By Site
CASE lcRpFormN = 'C'
  lcRpExp = lcRpExp + " AND INVHDR.CONSOL='Y'"
  = lfSetRel_1()
  = lfCrtTmp()
  = lfColDataC()
  = lfSetRel_2()
  lcFormName  = 'ARPINVDR'

  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][Start]
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.Report.lcOGPlatForm = 'ARPINVDR'
    llOGRefForm = .T.
  ENDIF
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][End]
  =gfCrtFrm(lcFormName,'',llOGRefForm)
  SELECT INVHDR
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][Start]
  *DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  IF TYPE('lcXMLFileName') <> 'C'
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  ELSE
    SELECT INVHDR
    Set FILTER TO &lcRpExp
    LOCATE 
    IF EOF()
      SET FILTER TO 
      RETURN 
    ENDIF  
  
    loProgress.Percent = 0.9
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)

    PRIVATE loProxy
    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

    IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
      loOGScroll   = oAriaEnvironment.report
      oAriaEnvironment.report.OGLastForm = lcFormName
      oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
      loProgress.Percent = 1.0
      loProgress.Description = "Printing Report..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
    ENDIF
  ENDIF 
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][End]
  SET DEVICE TO SCREEN
  llarpinv = .F.

  ** Consolidated By D/N
CASE lcRpFormN = 'N'
  lcRpExp = lcRpExp + " AND INVHDR.CONSOL='Y'"
  = lfSetRel_1()
  = lfCrtTmp()
  = lfColDataN()
  =lfSetRel_2()
  lcFormName  = 'ARPINVDN'
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][Start]
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.Report.lcOGPlatForm = 'ARPINVDN'
  ENDIF
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][End]
  =gfCrtFrm(lcFormName,'',llOGRefForm)
  SELECT INVHDR
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][Start]
  *DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  IF TYPE('lcXMLFileName') <> 'C'
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  ELSE
    SELECT INVHDR
    Set FILTER TO &lcRpExp
    LOCATE 
    IF EOF()
      SET FILTER TO 
      RETURN 
    ENDIF
    
    loProgress.Percent = 0.9
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)

    PRIVATE loProxy
    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

    IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
      loOGScroll   = oAriaEnvironment.report
      oAriaEnvironment.report.OGLastForm = lcFormName
      oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
      loProgress.Percent = 1.0
      loProgress.Description = "Printing Report..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
    ENDIF
  ENDIF 
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][End]
  SET DEVICE TO SCREEN
  llarpinv = .F.

  *T20080807.0003 - MOS - 12/25/2008 [START]
  **Consolidated By Employee

CASE lcRpFormN = 'E'
  lcRpExp = lcRpExp + " AND INVHDR.CONSOL='Y'"
  = lfSetRel_1()
  = lfCrtTmp()
  = lfColDataE()
  = lfSetRel_2()
  lcFormName  = 'ARPINVDE'
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][Start]
  IF TYPE('lcXMLFileName') = 'C'
    oAriaEnvironment.Report.lcOGPlatForm = 'ARPINVDE'
  ENDIF
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][End]
  =gfCrtFrm(lcFormName,'',llOGRefForm)
  SELECT INVHDR
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][Start]
  *DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  IF TYPE('lcXMLFileName') <> 'C'
    DO gfDispRe WITH EVAL('lcFormName') , 'FOR ' + lcRpExp
  ELSE
    SELECT INVHDR
    Set FILTER TO &lcRpExp
    LOCATE 
    IF EOF()
      SET FILTER TO 
      RETURN 
    ENDIF
    
    loProgress.Percent = 0.9
    loProgress.Description = "Printing Report..."
    loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)

    PRIVATE loProxy
    loProxy = CREATEOBJECT("Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy")

    IF loProxy.GetRequest(lcRequestID, ClientID).Status = 3
      loOGScroll   = oAriaEnvironment.report
      oAriaEnvironment.report.OGLastForm = lcFormName
      oAriaEnvironment.report.print(oAriaEnvironment.report.OGLastForm)
      loProgress.Percent = 1.0
      loProgress.Description = "Printing Report..."
      loAgent.UpdateObjectProgress(lcRequestID, loProgress,ClientID)
    ENDIF
  ENDIF 
  *! C201593,1 SAB 09/10/2013 Convert The report to run from Request Builder [T20130717.0010][End]
  SET DEVICE TO SCREEN
  llarpinv = .F.
  *T20080807.0003 - MOS - 12/25/2008 [End]

ENDCASE
=lfRelease()
SET CENTURY &lcCentury

SELECT INVCHRG
USE

*!*************************************************************
*! Name        : lfCrtTmp
*! Developer   : Mostafa Eid (MOS)
*! Date        : 26/11/2008
*! Purpose     : Function to Creat the temp. File
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfCrtTmp()
*!*************************************************************
*!C123950,1

FUNCTION lfCrtTmp
DO CASE
CASE lcRpFormN = 'C'

  *-- check If File is created or not
  lcCoTmpFil = gftempname()
  IF USED(lcCoTmpFil) AND RECCOUNT(lcCoTmpFil) > 0
    USE IN (lcCoTmpFil)
  ENDIF
  *-- Create File

  IF !USED(lcCoTmpFil)

    lnI = 1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'Invoice'
    laTempStru[lnI,2] = 'C'
    laTempStru[lnI,3] = 6
    laTempStru[lnI,4] = 0

    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'STORE'
    laTempStru[lnI,2] = 'C'
    laTempStru[lnI,3] = 8
    laTempStru[lnI,4] = 0

    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'Dpoint'
    laTempStru[lnI,2] = 'C'
    laTempStru[lnI,3] = 50
    laTempStru[lnI,4] = 0

    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'NetVal'
    laTempStru[lnI,2] = 'N'
    laTempStru[lnI,3] = 10
    laTempStru[lnI,4] = 2

    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'Vat'
    laTempStru[lnI,2] = 'N'
    laTempStru[lnI,3] = 10
    laTempStru[lnI,4] = 5

    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'Gross'
    laTempStru[lnI,2] = 'N'
    laTempStru[lnI,3] = 10
    *! C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][Start]
    *laTempStru[lnI,4] = 0
    laTempStru[lnI,4] = 5
    *! C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][End]
    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'Order'
    laTempStru[lnI,2] = 'C'
    laTempStru[lnI,3] = 6
    laTempStru[lnI,4] = 0

    lnI = ALEN(laTempStru,1)+1
    DIMENSION laTempStru[lnI,4]
    laTempStru[lnI,1] = 'CustRef'
    laTempStru[lnI,2] = 'C'
    laTempStru[lnI,3] = 15
    laTempStru[lnI,4] = 0

    =gfCrtTmp(lcCoTmpFil,@laTempStru)
    SELECT (lcCoTmpFil)
    INDEX ON (INVOICE+STORE) TAG (lcCoTmpFil) OF (lcCoTmpFil)
  ENDIF

  IF USED(lcTmpDate) AND RECCOUNT(lcTmpDate) > 0
    USE IN (lcTmpDate)
  ENDIF
  *-- Create File
  IF !USED(lcTmpDate)

    lnI = 1
    DIMENSION laTempStru2[lnI,4]
    laTempStru2[lnI,1] = 'Invoice'
    laTempStru2[lnI,2] = 'C'
    laTempStru2[lnI,3] = 6
    laTempStru2[lnI,4] = 0

    lnI = ALEN(laTempStru2,1)+1
    DIMENSION laTempStru2[lnI,4]
    laTempStru2[lnI,1] = 'FDate'
    laTempStru2[lnI,2] = 'D'
    laTempStru2[lnI,3] = 8
    laTempStru2[lnI,4] = 0


    lnI = ALEN(laTempStru2,1)+1
    DIMENSION laTempStru2[lnI,4]
    laTempStru2[lnI,1] = 'ToDate'
    laTempStru2[lnI,2] = 'D'
    laTempStru2[lnI,3] = 8
    laTempStru2[lnI,4] = 0

    =gfCrtTmp(lcTmpDate,@laTempStru2)
    SELECT (lcTmpDate)
    INDEX ON (INVOICE) TAG (lcTmpDate) OF (lcTmpDate)
  ENDIF

  IF USED(lcSTmpChr) AND RECCOUNT(lcSTmpChr) > 0
    USE IN (lcSTmpChr)
  ENDIF
  *-- Create File
  IF !USED(lcSTmpChr)

    lnI = 1
    DIMENSION laTempStru3[lnI,4]
    laTempStru3[lnI,1] = 'Invoice'
    laTempStru3[lnI,2] = 'C'
    laTempStru3[lnI,3] = 6
    laTempStru3[lnI,4] = 0

    lnI = ALEN(laTempStru3,1)+1
    DIMENSION laTempStru3[lnI,4]
    laTempStru3[lnI,1] = 'STORE'
    laTempStru3[lnI,2] = 'C'
    laTempStru3[lnI,3] = 8
    laTempStru3[lnI,4] = 0

    lnI = ALEN(laTempStru3,1)+1
    DIMENSION laTempStru3[lnI,4]
    laTempStru3[lnI,1] = 'Dpoint'
    laTempStru3[lnI,2] = 'C'
    laTempStru3[lnI,3] = 50
    laTempStru3[lnI,4] = 0

    lnI = ALEN(laTempStru3,1)+1
    DIMENSION laTempStru3[lnI,4]
    laTempStru3[lnI,1] = 'NetVal'
    laTempStru3[lnI,2] = 'N'
    laTempStru3[lnI,3] = 10
    laTempStru3[lnI,4] = 2

    lnI = ALEN(laTempStru3,1)+1
    DIMENSION laTempStru3[lnI,4]
    laTempStru3[lnI,1] = 'Vat'
    laTempStru3[lnI,2] = 'N'
    laTempStru3[lnI,3] = 10
    laTempStru3[lnI,4] = 5

    lnI = ALEN(laTempStru3,1)+1
    DIMENSION laTempStru3[lnI,4]
    laTempStru3[lnI,1] = 'Gross'
    laTempStru3[lnI,2] = 'N'
    laTempStru3[lnI,3] = 10
    laTempStru3[lnI,4] = 5

    lnI = ALEN(laTempStru3,1)+1
    DIMENSION laTempStru3[lnI,4]
    laTempStru3[lnI,1] = 'Order'
    laTempStru3[lnI,2] = 'C'
    laTempStru3[lnI,3] = 6
    laTempStru3[lnI,4] = 0

    =gfCrtTmp(lcSTmpChr,@laTempStru3)
    SELECT (lcSTmpChr)
    INDEX ON (INVOICE+STORE) TAG (lcSTmpChr) OF (lcSTmpChr)
  ENDIF

CASE lcRpFormN = 'N'

  IF USED(lcDnTmpFil) AND RECCOUNT(lcDnTmpFil) > 0
    USE IN (lcDnTmpFil)
  ENDIF
  *-- Create File
  IF !USED(lcDnTmpFil)

    lnI = 1
    DIMENSION laTempStru4[lnI,4]
    laTempStru4[lnI,1] = 'Invoice'
    laTempStru4[lnI,2] = 'C'
    laTempStru4[lnI,3] = 6
    laTempStru4[lnI,4] = 0

    lnI = ALEN(laTempStru4,1)+1
    DIMENSION laTempStru4[lnI,4]
    laTempStru4[lnI,1] = 'CustRef'
    laTempStru4[lnI,2] = 'C'
    laTempStru4[lnI,3] = 15
    laTempStru4[lnI,4] = 0

    lnI = ALEN(laTempStru4,1)+1
    DIMENSION laTempStru4[lnI,4]
    laTempStru4[lnI,1] = 'Piktkt'
    laTempStru4[lnI,2] = 'C'
    laTempStru4[lnI,3] = 6
    laTempStru4[lnI,4] = 0

    lnI = ALEN(laTempStru4,1)+1
    DIMENSION laTempStru4[lnI,4]
    laTempStru4[lnI,1] = 'dShipDate'
    laTempStru4[lnI,2] = 'D'
    laTempStru4[lnI,3] = 8
    laTempStru4[lnI,4] = 0

    lnI = ALEN(laTempStru4,1)+1
    DIMENSION laTempStru4[lnI,4]
    laTempStru4[lnI,1] = 'NetVal'
    laTempStru4[lnI,2] = 'N'
    laTempStru4[lnI,3] = 10
    laTempStru4[lnI,4] = 2

    lnI = ALEN(laTempStru4,1)+1
    DIMENSION laTempStru4[lnI,4]
    laTempStru4[lnI,1] = 'Vat'
    laTempStru4[lnI,2] = 'N'
    laTempStru4[lnI,3] = 10
    laTempStru4[lnI,4] = 5


    lnI = ALEN(laTempStru4,1)+1
    DIMENSION laTempStru4[lnI,4]
    laTempStru4[lnI,1] = 'Gross'
    laTempStru4[lnI,2] = 'N'
    laTempStru4[lnI,3] = 10
    laTempStru4[lnI,4] = 5

    lnI = ALEN(laTempStru4,1)+1
    DIMENSION laTempStru4[lnI,4]
    laTempStru4[lnI,1] = 'Order'
    laTempStru4[lnI,2] = 'C'
    laTempStru4[lnI,3] = 6
    laTempStru4[lnI,4] = 0

    =gfCrtTmp(lcDnTmpFil,@laTempStru4)
    SELECT (lcDnTmpFil)
    INDEX ON (INVOICE+PIKTKT) TAG (lcDnTmpFil) OF (lcDnTmpFil)
  ENDIF


  IF USED(lcTotalTmp) AND RECCOUNT(lcTotalTmp) > 0
    USE IN (lcTotalTmp)
  ENDIF
  *-- Create File
  IF !USED(lcTotalTmp)

    lnI = 1
    DIMENSION laTempStru5[lnI,4]
    laTempStru5[lnI,1] = 'Invoice'
    laTempStru5[lnI,2] = 'C'
    laTempStru5[lnI,3] = 6
    laTempStru5[lnI,4] = 0

    lnI = ALEN(laTempStru5,1)+1
    DIMENSION laTempStru5[lnI,4]
    laTempStru5[lnI,1] = 'GTotVat'
    laTempStru5[lnI,2] = 'N'
    laTempStru5[lnI,3] = 10
    laTempStru5[lnI,4] = 5

    lnI = ALEN(laTempStru5,1)+1
    DIMENSION laTempStru5[lnI,4]
    laTempStru5[lnI,1] = 'GTotGross '
    laTempStru5[lnI,2] = 'N'
    laTempStru5[lnI,3] = 10
    laTempStru5[lnI,4] = 5

    =gfCrtTmp(lcTotalTmp,@laTempStru5)
    SELECT (lcTotalTmp)
    INDEX ON (INVOICE) TAG (lcTotalTmp) OF (lcTotalTmp)
  ENDIF

  IF USED(lcDnTmpChr) AND RECCOUNT(lcDnTmpChr) > 0
    USE IN (lcDnTmpChr)
  ENDIF
  *-- Create File
  IF !USED(lcDnTmpChr)

    lnI = 1
    DIMENSION laTempStru6[lnI,4]
    laTempStru6[lnI,1] = 'Invoice'
    laTempStru6[lnI,2] = 'C'
    laTempStru6[lnI,3] = 6
    laTempStru6[lnI,4] = 0

    lnI = ALEN(laTempStru6,1)+1
    DIMENSION laTempStru6[lnI,4]
    laTempStru6[lnI,1] = 'Piktkt'
    laTempStru6[lnI,2] = 'C'
    laTempStru6[lnI,3] = 6
    laTempStru6[lnI,4] = 0


    lnI = ALEN(laTempStru6,1)+1
    DIMENSION laTempStru6[lnI,4]
    laTempStru6[lnI,1] = 'NetVal'
    laTempStru6[lnI,2] = 'N'
    laTempStru6[lnI,3] = 10
    laTempStru6[lnI,4] = 2

    lnI = ALEN(laTempStru6,1)+1
    DIMENSION laTempStru6[lnI,4]
    laTempStru6[lnI,1] = 'Vat'
    laTempStru6[lnI,2] = 'N'
    laTempStru6[lnI,3] = 10
    laTempStru6[lnI,4] = 5


    lnI = ALEN(laTempStru6,1)+1
    DIMENSION laTempStru6[lnI,4]
    laTempStru6[lnI,1] = 'Gross '
    laTempStru6[lnI,2] = 'N'
    laTempStru6[lnI,3] = 10
    laTempStru6[lnI,4] = 5

    lnI = ALEN(laTempStru6,1)+1
    DIMENSION laTempStru6[lnI,4]
    laTempStru6[lnI,1] = 'Order '
    laTempStru6[lnI,2] = 'C'
    laTempStru6[lnI,3] = 6
    laTempStru6[lnI,4] = 0

    **! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [Start]
    lnI = ALEN(laTempStru6,1)+1
    DIMENSION laTempStru6[lnI,4]
    laTempStru6[lnI,1] = 'CSTORE '
    laTempStru6[lnI,2] = 'C'
    laTempStru6[lnI,3] = 8
    laTempStru6[lnI,4] = 0
    **! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [End]        

    =gfCrtTmp(lcDnTmpChr,@laTempStru6)
    SELECT (lcDnTmpChr)
    INDEX ON (INVOICE+PIKTKT) TAG (lcDnTmpChr) OF (lcDnTmpChr)

    *! B610419,3 SAB 07/31/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][Start]
    **! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [Start]
    *INDEX ON (INVOICE+CSTORE) TAG ("DnTmpChr") OF (lcDnTmpChr) ADDITIVE 
    *SET ORDER TO (lcDnTmpChr)
    **! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [End]        
    INDEX ON (INVOICE+ORDER+CSTORE+PIKTKT) TAG ("DnTmpChr") OF (lcDnTmpChr) ADDITIVE 
    SET ORDER TO (lcDnTmpChr)
    *! B610419,3 SAB 07/31/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][End]

  ENDIF

  ***Mos

CASE lcRpFormN = 'E'

  IF USED(lcEmpTmpFil) AND RECCOUNT(lcEmpTmpFil) > 0
    USE IN (lcEmpTmpFil)
  ENDIF
  *-- Create File
  IF !USED(lcEmpTmpFil)

    lnI = 1
    DIMENSION laTempStru7[lnI,4]
    laTempStru7[lnI,1] = 'Invoice'
    laTempStru7[lnI,2] = 'C'
    laTempStru7[lnI,3] = 6
    laTempStru7[lnI,4] = 0



    lnI = ALEN(laTempStru7,1)+1
    DIMENSION laTempStru7[lnI,4]
    laTempStru7[lnI,1] = 'STORE'
    laTempStru7[lnI,2] = 'C'
    laTempStru7[lnI,3] = 8
    laTempStru7[lnI,4] = 0

    lnI = ALEN(laTempStru7,1)+1
    DIMENSION laTempStru7[lnI,4]
    laTempStru7[lnI,1] = 'StaffNO'
    laTempStru7[lnI,2] = 'C'
    laTempStru7[lnI,3] = 12
    laTempStru7[lnI,4] = 0

    lnI = ALEN(laTempStru7,1)+1
    DIMENSION laTempStru7[lnI,4]
    laTempStru7[lnI,1] = 'Dpoint'
    laTempStru7[lnI,2] = 'C'
    laTempStru7[lnI,3] = 50
    laTempStru7[lnI,4] = 0

    lnI = ALEN(laTempStru7,1)+1
    DIMENSION laTempStru7[lnI,4]
    laTempStru7[lnI,1] = 'NetVal'
    laTempStru7[lnI,2] = 'N'
    laTempStru7[lnI,3] = 10
    laTempStru7[lnI,4] = 2


    lnI = ALEN(laTempStru7,1)+1
    DIMENSION laTempStru7[lnI,4]
    laTempStru7[lnI,1] = 'Vat'
    laTempStru7[lnI,2] = 'N'
    laTempStru7[lnI,3] = 10
    laTempStru7[lnI,4] = 5

    lnI = ALEN(laTempStru7,1)+1
    DIMENSION laTempStru7[lnI,4]
    laTempStru7[lnI,1] = 'Gross'
    laTempStru7[lnI,2] = 'N'
    laTempStru7[lnI,3] = 10
    *B611190,1 MMT 09/22/2016 Custom invoice form DR prints incorrect Gross price when sort by Employee[T20160909.0003][Start]
    *laTempStru7[lnI,4] = 0
    laTempStru7[lnI,4] = 5
    *B611190,1 MMT 09/22/2016 Custom invoice form DR prints incorrect Gross price when sort by Employee[T20160909.0003][End]

    lnI = ALEN(laTempStru7,1)+1
    DIMENSION laTempStru7[lnI,4]
    laTempStru7[lnI,1] = 'Order'
    laTempStru7[lnI,2] = 'C'
    laTempStru7[lnI,3] = 6
    laTempStru7[lnI,4] = 0

    lnI = ALEN(laTempStru7,1)+1
    DIMENSION laTempStru7[lnI,4]
    laTempStru7[lnI,1] = 'CustRef'
    laTempStru7[lnI,2] = 'C'
    laTempStru7[lnI,3] = 15
    laTempStru7[lnI,4] = 0

    =gfCrtTmp(lcEmpTmpFil,@laTempStru7)
    SELECT (lcEmpTmpFil)
    *! B611204,1 MMT 10/16/2016 Custom Invoice form DR prints incorrect values (by Employee sort)[T20161005.0003][Start]
    *INDEX ON (INVOICE+STORE+StaffNO) TAG (lcEmpTmpFil) OF (lcEmpTmpFil)
    INDEX ON (INVOICE+STORE+Order+StaffNO) TAG (lcEmpTmpFil) OF (lcEmpTmpFil)
    *! B611204,1 MMT 10/16/2016 Custom Invoice form DR prints incorrect values (by Employee sort)[T20161005.0003][End]
  ENDIF

  IF USED(lcDETmpChr) AND RECCOUNT(lcDETmpChr) > 0
    USE IN (lcDETmpChr)
  ENDIF
  *-- Create File
  IF !USED(lcDETmpChr)

    lnI = 1
    DIMENSION laTempStru8[lnI,4]
    laTempStru8[lnI,1] = 'Invoice'
    laTempStru8[lnI,2] = 'C'
    laTempStru8[lnI,3] = 6
    laTempStru8[lnI,4] = 0

    lnI = ALEN(laTempStru8,1)+1
    DIMENSION laTempStru8[lnI,4]
    laTempStru8[lnI,1] = 'STORE'
    laTempStru8[lnI,2] = 'C'
    laTempStru8[lnI,3] = 8
    laTempStru8[lnI,4] = 0



    lnI = ALEN(laTempStru8,1)+1
    DIMENSION laTempStru8[lnI,4]
    laTempStru8[lnI,1] = 'StaffNO'
    laTempStru8[lnI,2] = 'C'
    laTempStru8[lnI,3] = 12
    laTempStru8[lnI,4] = 0



    lnI = ALEN(laTempStru8,1)+1
    DIMENSION laTempStru8[lnI,4]
    laTempStru8[lnI,1] = 'Dpoint'
    laTempStru8[lnI,2] = 'C'
    laTempStru8[lnI,3] = 50
    laTempStru8[lnI,4] = 0


    lnI = ALEN(laTempStru8,1)+1
    DIMENSION laTempStru8[lnI,4]
    laTempStru8[lnI,1] = 'NetVal'
    laTempStru8[lnI,2] = 'N'
    laTempStru8[lnI,3] = 10
    laTempStru8[lnI,4] = 2

    lnI = ALEN(laTempStru8,1)+1
    DIMENSION laTempStru8[lnI,4]
    laTempStru8[lnI,1] = 'Vat'
    laTempStru8[lnI,2] = 'N'
    laTempStru8[lnI,3] = 10
    laTempStru8[lnI,4] = 5

    lnI = ALEN(laTempStru8,1)+1
    DIMENSION laTempStru8[lnI,4]
    laTempStru8[lnI,1] = 'Gross'
    laTempStru8[lnI,2] = 'N'
    laTempStru8[lnI,3] = 10
    laTempStru8[lnI,4] = 5

    lnI = ALEN(laTempStru8,1)+1
    DIMENSION laTempStru8[lnI,4]
    laTempStru8[lnI,1] = 'Order'
    laTempStru8[lnI,2] = 'C'
    laTempStru8[lnI,3] = 6
    laTempStru8[lnI,4] = 0

    *! B611190,2 Sara.O 09/27/2016 Error while closing og report after review [T20160909.0003][Start]
    *=gfCrtTmp(lcDETmpChr,@laTempStru8)
    *SELECT (lcDETmpChr)
    *INDEX ON (INVOICE+STORE+StaffNO) TAG (lcETmpDate) OF (lcETmpDate)
    
    =gfCrtTmp(lcDETmpChr,@laTempStru8)
	SELECT (lcDETmpChr)
	INDEX ON (INVOICE+STORE+StaffNO) TAG (lcDETmpChr) OF (lcDETmpChr)
    *! B611190,2 Sara.O 09/27/2016 Error while closing og report after review [T20160909.0003][End]
	
  ENDIF

  IF USED(lcETmpDate) AND RECCOUNT(lcETmpDate) > 0
    USE IN (lcETmpDate)
  ENDIF
  *-- Create File

  IF !USED(lcETmpDate)

    lnI = 1
    DIMENSION laTempStru9[lnI,4]
    laTempStru9[lnI,1] = 'Invoice'
    laTempStru9[lnI,2] = 'C'
    laTempStru9[lnI,3] = 6
    laTempStru9[lnI,4] = 0

    lnI = ALEN(laTempStru9,1)+1
    DIMENSION laTempStru9[lnI,4]
    laTempStru9[lnI,1] = 'FDate'
    laTempStru9[lnI,2] = 'D'
    laTempStru9[lnI,3] = 8
    laTempStru9[lnI,4] = 0


    lnI = ALEN(laTempStru9,1)+1
    DIMENSION laTempStru9[lnI,4]
    laTempStru9[lnI,1] = 'ToDate'
    laTempStru9[lnI,2] = 'D'
    laTempStru9[lnI,3] = 8
    laTempStru9[lnI,4] = 0

    =gfCrtTmp(lcETmpDate,@laTempStru9)
    SELECT (lcETmpDate)
    INDEX ON (INVOICE) TAG (lcETmpDate) OF (lcETmpDate)
  ENDIF

ENDCASE
*--End function lfCrtTmp.

*!*************************************************************
*! Name        : lfColDataC
*! Developer   : Nader Nabil (NNA)
*! Date        : 29/11/2004
*! Purpose     : Function to Collect data for form ARPINVDC
*!             : (Consolidated By Site)
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColDataC()
*!**!*************************************************************

*!C123950,1
FUNCTION lfColDataC
*:C127805,1 MMR 06/15/2005 Adding new variable to hold the old alias[Start]
PRIVATE lcStore , lnNetVal , lnVat , lnGross,lnOrder,lcAlias,lcAlias2
*:C127805,1 MMR [End]
STORE '' TO lcStore
STORE 0 TO lnNetVal , lnVat , lnGross , lnOrder
SELECT INVHDR
SCAN FOR &LCRPEXP
  *B609301,1 MMT 06/15/2010 Fix bug of wrong totals accumlated[Start]
  STORE 0 TO lnNetVal , lnVat , lnGross , lnOrder
  *B609301,1 MMT 06/15/2010 Fix bug of wrong totals accumlated[End]
  STORE {} TO ldFromDate , ldToDate
  SELECT Consinvh
  IF SEEK(INVHDR.INVOICE)
    SCAN REST WHILE Invoice+STORE+ORDER = INVHDR.INVOICE
      IF SEEK(Consinvh.ORDER+Consinvh.piktkt,'PIKTKT') AND;
          SEEK(Consinvh.ORDER+Consinvh.STORE+Consinvh.piktkt,'PACK_HDR')
        SELECT PACK_HDR
        IF !(lcStore == Consinvh.STORE)
          lcStore = Consinvh.STORE
          lnOrder = Consinvh.ORDER
          STORE 0 TO lnNetVal , lnVat , lnGross
        ENDIF
        IF PACK_HDR.dShipDate<>{}
          IF ldFromDate={} AND ldToDate = {}
            ldFromDate = PACK_HDR.dShipDate
            ldToDate   = PACK_HDR.dShipDate
          ELSE
            IF PACK_HDR.dShipDate < ldFromDate
              ldFromDate = PACK_HDR.dShipDate
            ENDIF
            IF PACK_HDR.dShipDate > ldToDate
              ldToDate = PACK_HDR.dShipDate
            ENDIF
          ENDIF
        ENDIF
        *! C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][Start]
        *lnNetVal = lnNetVal + (Consinvh.ShipAmt - Consinvh.Discount)
        *lnVat    = lnVat    + ((Consinvh.Shipamt*Consinvh.Tax_rate)/100)
        lnNetVal = lnNetVal + (Consinvh.ShipAmt + Consinvh.Discount)
        lnVat    = lnVat    + (((Consinvh.ShipAmt + Consinvh.Discount)*Consinvh.Tax_rate)/100)
        *! C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][End]
        lnGross  = lnVat + lnNetVal
        *:C127805,1 MMR 06/15/2005 Collecting Charges Data[Start]
        lcAlias = ALIAS()
        SELECT Consinvh
        *!* B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][Start]
        *IF SEEK(Consinvh.INVOICE + Consinvh.STORE,'INVCHRG') AND !SEEK(Consinvh.INVOICE + Consinvh.STORE,lcSTmpChr)
        *B610680,1 TMI 02/18/2014 18:49 [Start] remove the last seek that checks if the store in lcStmpchr
        *IF SEEK(Consinvh.INVOICE + Consinvh.ORDER + Consinvh.STORE + Consinvh.PIKTKT,'INVCHRG') AND !SEEK(Consinvh.INVOICE + Consinvh.STORE,lcSTmpChr)
        IF SEEK(Consinvh.INVOICE + Consinvh.ORDER + Consinvh.STORE + Consinvh.PIKTKT,'INVCHRG')
          *B610680,1 TMI 02/18/2014 18:49 [End  ] 
          *!* B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][End]
          SELECT INVCHRG
          *!* B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][Start]
          *SCAN REST WHILE invoice+cstore+cchrgcode = Consinvh.INVOICE + Consinvh.STORE
          SCAN REST WHILE invoice+order+cstore+piktkt+cchrgcode = Consinvh.INVOICE + Consinvh.ORDER + Consinvh.STORE + Consinvh.PIKTKT
          *!* B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][End]
            SELECT (lcSTmpChr)
            IF SEEK(Consinvh.Invoice + Consinvh.STORE)
              REPLACE NetVal  WITH NetVal + INVCHRG.nchrgamnt                 ,;
                Vat     WITH Vat + IIF(INVCHRG.ntaxrate>0               ,;
                (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)           ,;
                Gross   WITH  NetVal + Vat                              ,;
                ORDER   WITH  Consinvh.ORDER
            ELSE
              APPEND BLANK
              *! B610220,1 HIA 01/28/2013 Aria4xp - AR - Consolidated invoice [T20130109.0024][Start]
              *REPLACE Invoice   WITH INVCHRG.Invoice                                        ,;
              *        Store     WITH Consinvh.Piktkt                                        ,;
              *        Dpoint    WITH ALLTRIM(Consinvh.Store)                                ,;
              *        NetVal    WITH INVCHRG.nchrgamnt                                      ,;
              *        Vat       WITH IIF(INVCHRG.ntaxrate>0                                 ,;
              *                  (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)               ,;
              *        Gross     WITH NetVal + Vat                                           ,;
              *        Order     WITH Consinvh.Order


              REPLACE Invoice   WITH INVCHRG.Invoice                                        ,;
                STORE     WITH Consinvh.STORE                                         ,;
                Dpoint    WITH ALLTRIM(Consinvh.STORE)                                ,;
                NetVal    WITH INVCHRG.nchrgamnt                                      ,;
                Vat       WITH IIF(INVCHRG.ntaxrate>0                                 ,;
                (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)               ,;
                Gross     WITH NetVal + Vat                                           ,;
                ORDER     WITH Consinvh.ORDER
              *!B610220,1 HIA 01/28/2013 Aria4xp - AR - Consolidated invoice [T20130109.0024][End]
            ENDIF
          ENDSCAN
        ENDIF
        SELECT (lcAlias)
        *!B610220,1 HIA 01/28/2013 Aria4xp - AR - Consolidated invoice [T20130109.0024][Start]
        *!*	          SEEK(Consinvh.Invoice + Consinvh.Store)
        *!*	          lnN=EVAL(lcSTmpChr+'.NetVal')
        *!*	          lnV=EVAL(lcSTmpChr+'.Vat')
        *!*	          lnG=EVAL(lcSTmpChr+'.Gross')
        *!C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][Start]
        *IF  SEEK(Consinvh.Invoice + Consinvh.STORE)
        IF SEEK(Consinvh.Invoice + Consinvh.STORE,lcSTmpChr)
          *!C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][End]
          lnN = EVAL(lcSTmpChr+'.NetVal')
          lnV = EVAL(lcSTmpChr+'.Vat')
          lnG = EVAL(lcSTmpChr+'.Gross')
        ELSE
          lnN = 0
          *! B611522,1 MMT 02/04/2018 Incorrect VAT calculations in DCC custom invoice form[T20180126.0001][Start]
          *lnV = 0
          *lnG = 0
          lnV = Consinvh.Tax_Amt
          lnG = lnV 
          *! B611522,1 MMT 02/04/2018 Incorrect VAT calculations in DCC custom invoice form[T20180126.0001][End]
        ENDIF
        *!B610220,1 HIA 01/28/2013 Aria4xp - AR - Consolidated invoice [T20130109.0024][End]

        SELECT (lcCoTmpFil)
        IF SEEK(Consinvh.Invoice + Consinvh. STORE)
          REPLACE NetVal  WITH lnNetVal + lnN          , ;
            Vat     WITH lnVat + lnV             , ;
            Gross   WITH lnGross + lnG
        ELSE
          APPEND BLANK
          REPLACE Invoice WITH Consinvh.Invoice , ;
            STORE   WITH Consinvh.STORE   , ;
            Dpoint  WITH ALLTRIM(Consinvh.STORE)  ,;
            NetVal  WITH lnNetVal + lnN         , ;
            Vat     WITH lnVat + lnV            , ;
            Gross   WITH lnGross + lnG           ,;
            ORDER   WITH Consinvh.ORDER           ,;
            CustRef WITH PIKTKT.CUSTPO
        ENDIF
        *:C127805,1 MMR [End]
      ENDIF
    ENDSCAN
    SELECT (lcTmpDate)
    APPEND BLANK
    REPLACE INVOICE WITH INVHDR.INVOICE ,;
      FDATE   WITH ldFromDate     ,;
      TODATE  WITH ldToDate
  ENDIF
ENDSCAN
*--End function lfColDataC

*!*************************************************************
*! Name        : lfColDataN
*! Developer   : Nader Nabil (NNA)
*! Date        : 29/11/2004
*! Purpose     : Function to Collect data for form ARPINVDN
*!             : (Consolidated By D/N)
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColDataN()
*!*************************************************************
*!C123950,1

FUNCTION lfColDataN

PRIVATE lcPiktkt , lnNetVal , lnVat , lnGross ,lnOrder ,ldShpDate , lcCustPo ,lcAlias ,lcAlias2
*:C127805,1 MMR [End]
STORE '' TO lcPiktkt , lcCustPo ,lnOrder
STORE 0 TO lnNetVal , lnVat , lnGross
STORE {} TO ldShpDate

SELECT INVHDR
SCAN FOR &LCRPEXP
  STORE 0 TO lnTotalVat,lnTotalGr
  SELECT Consinvh
  IF SEEK(INVHDR.INVOICE)
    SCAN REST WHILE Invoice+STORE+ORDER = INVHDR.INVOICE
      IF SEEK(Consinvh.ORDER+Consinvh.piktkt,'PIKTKT') AND;
          SEEK(Consinvh.ORDER+Consinvh.STORE+Consinvh.piktkt,'PACK_HDR')
        *NNA 07/02/2005 (Begin) seek about the Piktkt also inside the Pack_hdr file
        SELECT PACK_HDR
        *LOCATE FOR PACK_HDR.ORDER = Consinvh.order AND PACK_HDR.PIKTKT = Consinvh.piktkt
        *IF FOUND()
        *NNA (End)
        IF !(lcPiktkt == Consinvh.Piktkt)
          lcPiktkt = Consinvh.Piktkt
          lnOrder = Consinvh.ORDER
          STORE 0 TO lnNetVal , lnVat , lnGross
          STORE {} TO ldShpDate
          STORE '' TO lcCustPo
        ENDIF
        *! C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][Start]
        *!*	        lnNetVal   = lnNetVal + (Consinvh.ShipAmt - Consinvh.Discount)
        *!*	        lnVat      = lnVat    + ((Consinvh.Shipamt*Consinvh.Tax_rate)/100)
        lnNetVal   = lnNetVal + (Consinvh.ShipAmt + Consinvh.Discount)
        lnVat      = lnVat    + (((Consinvh.ShipAmt + Consinvh.Discount)*Consinvh.Tax_rate)/100)
        *! C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][End]
        lnGross    = lnVat + lnNetVal
        lnTotalVat = lnTotalVat + lnVat
        lnTotalGr  = lnTotalGr + lnGross
        *:C127805,1 MMR 06/15/2005 Collecting Charges Data[Start]
        lcAlias = ALIAS()
        SELECT Consinvh
        *!* B610419,3 SAB 07/31/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][Start]
        **! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [Start]
        **IF SEEK(Consinvh.INVOICE,'INVCHRG') AND !SEEK(Consinvh.INVOICE,lcDnTmpChr)
        *IF SEEK(Consinvh.INVOICE,'INVCHRG') AND !SEEK(Consinvh.Invoice , lcDnTmpChr, "DnTmpChr")
        **! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [End]
        IF SEEK(Consinvh.INVOICE+Consinvh.ORDER+Consinvh.STORE+Consinvh.PIKTKT,'INVCHRG') AND !SEEK(Consinvh.INVOICE+Consinvh.ORDER+Consinvh.STORE+Consinvh.PIKTKT,lcDnTmpChr,"DnTmpChr")
        *!* B610419,3 SAB 07/31/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][End]
          SELECT INVCHRG
          *!* B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][Start]
          *SCAN REST WHILE invoice+cstore+cchrgcode = Consinvh.INVOICE
          SCAN REST WHILE invoice+order+cstore+piktkt+cchrgcode = Consinvh.INVOICE+Consinvh.ORDER+Consinvh.STORE+Consinvh.PIKTKT
          *!* B610419,1 SAB 07/09/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][End]
          
            SELECT (lcDnTmpChr)
            *!* B610419,2 SAB 07/18/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][Start]
            **! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [Start]
            **IF SEEK(Consinvh.Invoice + Consinvh.Piktkt)
            *IF SEEK(Consinvh.Invoice + consinvh.STORE, lcDnTmpChr, "DnTmpChr")
            *  *! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [End]
            IF SEEK(Consinvh.Invoice + Consinvh.ORDER + Consinvh.STORE + Consinvh.PIKTKT,lcDnTmpChr, "DnTmpChr")
            *!* B610419,2 SAB 07/18/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][End]
              
              REPLACE NetVal  WITH NetVal + INVCHRG.nchrgamnt                 ,;
                Vat     WITH Vat + IIF(INVCHRG.ntaxrate>0               ,;
                (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)           ,;
                Gross   WITH  NetVal + Vat
            ELSE
              APPEND BLANK
              REPLACE Invoice    WITH INVCHRG.Invoice                                    ,;
                Piktkt     WITH Consinvh.Piktkt                                    ,;
                NetVal     WITH INVCHRG.nchrgamnt                                  ,;
                Vat        WITH IIF(INVCHRG.ntaxrate>0                             ,;
                (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)           ,;
                Gross      WITH NetVal + Vat                                       ,;
                ORDER      WITH Consinvh.ORDER

              *! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [Start]
              REPLACE cStore WITH consinvh.STORE
              *! B610303,1 HIA 04/15/2013 T20130109.0024 - Aria4xp - AR - Consolidated invoice [End]              

            ENDIF

          ENDSCAN
        ENDIF
        SELECT (lcAlias)
        *:C127805,1 MMR [End]
        SELECT (lcDnTmpFil)
        IF SEEK(Consinvh.Invoice + Consinvh.Piktkt)
          REPLACE NetVal  WITH lnNetVal         , ;
            Vat     WITH lnVat            , ;
            Gross   WITH lnGross
        ELSE
          APPEND BLANK
          REPLACE Invoice   WITH Consinvh.Invoice   ,;
            Piktkt    WITH Consinvh.Piktkt    ,;
            CustRef   WITH PIKTKT.CUSTPO      ,;
            dShipDate WITH PACK_HDR.dShipDate ,;
            NetVal    WITH lnNetVal           ,;
            Vat       WITH lnVat              ,;
            Gross     WITH lnGross            ,;
            ORDER     WITH Consinvh.ORDER
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
  IF !SEEK(INVHDR.INVOICE,lcTotalTmp)
    *:C127805,1 MMR 06/15/2005 Collecting Total Vat and Gross[Start]
    lcAlias2=ALIAS()
    SELECT (lcDnTmpChr)
    SEEK INVHDR.INVOICE
    *! B610492,1 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024][Start]
    *lnVatCh=   Vat
    *lnGrossCh= Gross
    SUM  Vat,Gross to lnVatCh,lnGrossCh  rest while INVOICE+PIKTKT = INVHDR.INVOICE
    *! B610492,1 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024][End]
    SELECT (lcAlias2)
    INSERT INTO (lcTotalTmp)(INVOICE,GTotVat,GTotGross)  VALUES(INVHDR.INVOICE,(lnTotalVat+lnVatCh),(lnTotalGr+lnGrossCh))
    *:C127805,1 MMR
  ENDIF
ENDSCAN
*--End function lfColDataN.

*!*************************************************************
*! Name        : lfColDataE
*! Developer   : Mostafa Eid (MOS)
*! Date        : 01/05/2009
*! Purpose     : Function to Collect data for form ARPINVDE
*!             : (Consolidated By employee)
*!*************************************************************
*! Called from : ARPINVDE.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColDataE()
*!*	*!*************************************************************

FUNCTION lfColDataE
*:C127805,1 MMR 06/15/2005 Adding new variable to hold the old alias[Start]
PRIVATE lcStore ,lcStaffno, lnNetVal , lnVat , lnGross,lnOrder,lcAlias,lcAlias2
*:C127805,1 MMR [End]
STORE '' TO lcStore ,lcStaffno , lnOrder
STORE 0 TO lnNetVal , lnVat , lnGross
SELECT INVHDR
SCAN FOR &LCRPEXP
  STORE {} TO ldFromDate , ldToDate
  SELECT Consinvh
  IF SEEK(INVHDR.INVOICE)
    SCAN REST WHILE Invoice+STORE+ORDER = INVHDR.INVOICE
      IF SEEK(Consinvh.ORDER+Consinvh.piktkt,'PIKTKT') AND;
          SEEK(Consinvh.ORDER+Consinvh.STORE+Consinvh.piktkt,'PACK_HDR')AND ;
          SEEK(Consinvh.invoice+Consinvh.STORE +Consinvh.ORDER,'Consinvl')AND ;
          SEEK('O'+ Consinvl.ORDER + STR(Consinvl.LINENO,6),'ORDLINE','ORDLINE')
        SELECT PACK_HDR
        IF !(lnOrder == Consinvh.ORDER)
          lcStore = Consinvh.STORE
          lnOrder = Consinvh.ORDER
          STORE 0 TO lnNetVal , lnVat , lnGross
        ENDIF
        SELECT consinvl
        IF PACK_HDR.dShipDate<>{}
          IF ldFromDate={} AND ldToDate = {}
            ldFromDate = PACK_HDR.dShipDate
            ldToDate   = PACK_HDR.dShipDate
          ELSE
            IF PACK_HDR.dShipDate < ldFromDate
              ldFromDate = PACK_HDR.dShipDate
            ENDIF
            IF PACK_HDR.dShipDate > ldToDate
              ldToDate = PACK_HDR.dShipDate
            ENDIF
          ENDIF
        ENDIF
        *! C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][Start]
        *!*	        lnNetVal = lnNetVal + (Consinvh.ShipAmt - Consinvh.Discount)
        *!*	        lnVat    = lnVat    + ((Consinvh.Shipamt*Consinvh.Tax_rate)/100)
        lnNetVal = lnNetVal + (Consinvh.ShipAmt + Consinvh.Discount)
        lnVat    = lnVat    + (((Consinvh.ShipAmt + Consinvh.Discount)*Consinvh.Tax_rate)/100)
        *! C201551,2 MMT 02/28/2013 Incorrect VAT calculation in form by Site [T20130109.0024][End]
        lnGross  = lnVat + lnNetVal
        *:C127805,1 MMR 06/15/2005 Collecting Charges Data[Start]
        lcAlias = ALIAS()
        SELECT Consinvh
        IF SEEK(Consinvh.INVOICE ,'INVCHRG') AND !SEEK(Consinvh.INVOICE ,lcDETmpChr)
          SELECT INVCHRG
          *! B610419,3 SAB 07/31/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][Start]
          *SCAN REST WHILE invoice+cstore+cchrgcode = Consinvh.INVOICE
          SCAN REST WHILE invoice+order+cstore+piktkt+cchrgcode = Consinvh.INVOICE          
          *! B610419,3 SAB 07/31/2013 Fix englsih charges issue in case of Consolidated Invoice [T20130109.0024][End]
            SELECT (lcDETmpChr)
            IF SEEK(Consinvh.Invoice + Consinvh.STORE + Consinvh.ORDER)
              REPLACE NetVal  WITH NetVal + INVCHRG.nchrgamnt                 ,;
                Vat     WITH Vat + IIF(INVCHRG.ntaxrate>0               ,;
                (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)           ,;
                Gross   WITH  NetVal + Vat                              ,;
                ORDER   WITH  Consinvh.ORDER
            ELSE
              APPEND BLANK
              REPLACE Invoice   WITH INVCHRG.Invoice                                        ,;
                STORE     WITH Consinvh.Piktkt ,;
                StaffNO   WITH ordline.employee                                         ,;
                Dpoint    WITH ALLTRIM(Consinvh.STORE)                                ,;
                NetVal    WITH INVCHRG.nchrgamnt                                      ,;
                Vat       WITH IIF(INVCHRG.ntaxrate>0                                 ,;
                (INVCHRG.ntaxrate * INVCHRG.nchrgamnt)/100,0)               ,;
                Gross     WITH NetVal + Vat                                           ,;
                ORDER     WITH Consinvh.ORDER
            ENDIF
          ENDSCAN
        ENDIF

        lnN=EVAL(lcDETmpChr+'.NetVal')
        lnV=EVAL(lcDETmpChr+'.Vat')
        lnG=EVAL(lcDETmpChr+'.Gross')

        SELECT (lcEmpTmpFil)
        *INVOICE+Store+StaffNO
        *! B611204,1 MMT 10/16/2016 Custom Invoice form DR prints incorrect values (by Employee sort)[T20161005.0003][Start]
        *IF SEEK(Consinvh.Invoice + Consinvh.STORE + Consinvh.ORDER)
        IF SEEK(Consinvh.Invoice + Consinvh.STORE + Consinvh.ORDER+ordline.employee )
        *! B611204,1 MMT 10/16/2016 Custom Invoice form DR prints incorrect values (by Employee sort)[T20161005.0003][End]
          REPLACE NetVal  WITH lnNetVal + lnN          , ;
            Vat     WITH lnVat + lnV             , ;
            Gross   WITH lnGross + lnG
        ELSE
          APPEND BLANK
          REPLACE Invoice WITH Consinvh.Invoice , ;
            STORE   WITH Consinvh.STORE ,;
            StaffNO WITH ordline.employee               , ;
            Dpoint  WITH ALLTRIM(Consinvh.STORE)  ,;
            NetVal  WITH lnNetVal + lnN         , ;
            Vat     WITH lnVat + lnV            , ;
            Gross   WITH lnGross + lnG           ,;
            ORDER   WITH Consinvh.ORDER           ,;
            CustRef WITH PIKTKT.CUSTPO
        ENDIF
        *:C127805,1 MMR [End]

      ENDIF
    ENDSCAN
    SELECT (lcETmpDate)
    APPEND BLANK
    REPLACE INVOICE WITH INVHDR.INVOICE ,;
      FDATE   WITH ldFromDate     ,;
      TODATE  WITH ldToDate

  ENDIF
ENDSCAN
*--End function lfColDataC

*!*************************************************************
*! Name        : lfSetRel_1
*! Developer   : Nader Nabil (NNA)
*! Date        : 29/11/2004
*! Purpose     : Function to Creat Relationship between files
*!             : before collecting data.
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSetRel_1()
*!*************************************************************
*!C123950,1

FUNCTION lfSetRel_1
SELECT INVHDR
SET RELATION TO
SET RELATION TO INVOICE INTO Consinvh ADDITIVE
SELECT Consinvh
SET RELATION TO
SET RELATION TO Consinvh.ORDER+ Consinvh.piktkt INTO Piktkt ADDITIVE
SET RELATION TO 'S'+ Consinvh.account+ Consinvh.STORE INTO Customer ADDITIVE
SET RELATION TO INVOICE+STORE+ORDER INTO CONSINVl  ADDITIVE
SELECT PIKTKT
SET RELATION TO
SET RELATION TO Piktkt.ORDER INTO Pack_hdr ADDITIVE
SELECT Consinvl
SET RELATION TO 'O'+ ORDER INTO ordline ADDITIVE
*--End function lfSetRel_1.

*!*************************************************************
*! Name        : lfSetRel_2
*! Developer   : Nader Nabil (NNA)
*! Date        : 29/11/2004
*! Purpose     : Function to Creat Relationship between files
*!             : after collecting data.
*!*************************************************************
*! Called from : ARPINVDR.PRG
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSetRel_2()
*!*************************************************************
*!C123950,1
FUNCTION lfSetRel_2
IF llRpInvNot
  SELECT INVHDR
  SET RELATION TO
  SET RELATION TO '' INTO (lcTmpDbt)
  SET RELATION TO IIF(EMPTY(STORE) OR STORE = "********", 'M' + Account ,;
    'S' + Account + STORE) INTO CUSTOMER ADDITIVE
  *--NNA
  IF lcRpFormN = 'N'
    SET RELATION TO INVOICE INTO (lcTotalTmp) ADDITIVE
  ENDIF
  *--NNA
  SELECT (lcTmpDbt)
  SET RELATION TO
  SET RELATION TO IIF(CFILE_NUM = '3', INVHDR.Invoice, '*') INTO ARINSTMD
  SET RELATION TO IIF(CFILE_NUM = '1', INVHDR.Invoice, '*') INTO ;
    IIF(lcRpFormN = 'C',(lcCoTmpFil),IIF(lcRpFormN = 'E',(lcEmpTmpFil),(lcDnTmpFil))) ADDITIVE
  SELECT INVHDR
  SET SKIP TO (lcTmpDbt) , IIF(lcRpFormN = 'C',(lcCoTmpFil),IIF(lcRpFormN = 'E',(lcEmpTmpFil),(lcDnTmpFil))) , ARINSTMD
ELSE
  SELECT INVHDR
  SET RELATION TO
  SET RELATION TO INVHDR.INVOICE INTO IIF(lcRpFormN = 'C',(lcCoTmpFil),(lcDnTmpFil)) ADDITIVE
  SET SKIP TO IIF(lcRpFormN = 'C',(lcCoTmpFil),IIF(lcRpFormN = 'E',(lcEmpTmpFil),(lcDnTmpFil)))
  *--NNA
  IF lcRpFormN = 'N'
    SET RELATION TO INVOICE INTO (lcTotalTmp) ADDITIVE
    *:C127805,1 MMR 06/15/2005 Create Relation betwwen the two files to print in the FRX[Start]
    SET RELATION TO INVOICE+Piktkt INTO (lcDnTmpChr) ADDITIVE
    *:C127805,1 MMR 06/15/2005 [End]
  ENDIF
  *--NNA
ENDIF
IF lcRpFormN = 'C'
  SELECT INVHDR
  SET RELATION TO INVOICE INTO (lcTmpDate) ADDITIVE
ENDIF

IF lcRpFormN = 'E'
  SELECT INVHDR
  SET RELATION TO INVOICE INTO (lcETmpDate) ADDITIVE
ENDIF
*! B610492,1 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024][Start]
IF lcRpFormN = 'N'
  Sele (lcDnTmpFil)
  *! B610492,2 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024][Start]
  IF !(lower(lcDnTmpChr) $  lowe(SET("Relation" )))
  *! B610492,2 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024][End]
    SET RELATION TO INVOICE+Piktkt INTO (lcDnTmpChr) ADDITIVE
    SELECT INVHDR
  *! B610492,2 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024][Start]
  ENDIF
  *! B610492,2 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024][End]
ENDIF
*! B610492,1 MMT 09/02/2013 Fix report to show UK charges of each Picking ticket in the report details[T20130109.0024][End]


*--End function lfSetRel_2.
*!*************************************************************
*! Name      : lfPrtNotes
*! Developer : Nader Nabil (NNA)
*! Date      : 29/11/2004
*! Purpose   : Function to Evaluate Notes To be Printed
*! Returns   : Printed Notes
*!*************************************************************
*! Called from : ARPINVA,ARPINVZ .FRX (Notes Expression)
*!*************************************************************
*!C123950,1
FUNCTION lfPrtNotes
PARAMETER lcReturn
DO CASE
CASE llRpInvNot .AND. EVAL(lcTmpDbt+'.cfile_num')='2'                          ;
    .AND. !EMPTY(ALLTRIM(NOTEPAD.mNotes))                                     ;
    .AND. LEFT(ALLTRIM(STRTRAN(NOTEPAD.mNotes,CHR(13)+CHR(10),' ')),1) <> '*' ;
    .AND. SEEK('C' + INVHDR.Invoice , 'NOTEPAD')
  FOR lnLoop = 1 TO MEMLINES(NOTEPAD.mNotes)
    IF MLINE(NOTEPAD.mNotes , lnLoop) = CHR(13)
      lcNotes    = ALLTRIM(NOTEPAD.mNotes)
    ENDIF
  ENDFOR
  lcNotesTtl = 'Invoice Notes'
  lcNotes    = ALLTRIM(NOTEPAD.mNotes)
OTHERWISE
  STORE '' TO lcNotesTtl, lcNotes
ENDCASE

RETURN !EMPTY(lcNotesTtl)

*!*************************************************************
*! Name      : lfvRpForm
*! Developer : Nader Nabil (NNA)
*! Date      : 29/11/2004
*! Purpose   : Function to Validate Changing of the Forms.
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*!C123950,1
FUNCTION lfvRpForm
PARAMETERS lcDummy
=lfRepPltFr(lcFormName)
DO CASE
CASE lcRpFormN = 'S'
  lcFormName  = 'ARPINVM'
  lcOGPlatForm = IIF(lcRepAvlM $ 'BG' , 'WINDOWS ' , 'DOS')
  lcRepMode    = IIF(lcRepAvlM $ 'BG' , 'Graphics' , 'Text')
  lcRepModeS   = IIF(lcRepAvlM = 'B' , 'ENABLE' , 'DISABLE')
  SHOW GET lcRepMode &lcRepModeS
CASE lcRpFormN = 'C'
  lcFormName  = 'ARPINVDR'
  lcOGPlatForm = 'WINDOWS '
  lcRepMode    = 'Graphics'
  lcRepModeS   =  'DISABLE'
  SHOW GET lcRepMode &lcRepModeS
CASE lcRpFormN = 'N'
  lcFormName  = 'ARPINVDN'
  lcOGPlatForm = 'WINDOWS '
  lcRepMode    = 'Graphics'
  lcRepModeS   =  'DISABLE'
  SHOW GET lcRepMode &lcRepModeS
  ***MOS
CASE lcRpFormN = 'E'
  lcFormName  = 'ARPINVDE'
  lcOGPlatForm = 'WINDOWS '
  lcRepMode    = 'Graphics'
  lcRepModeS   =  'DISABLE'
  SHOW GET lcRepMode &lcRepModeS
  ***MOS
ENDCASE

lcDummy =.T.
RETURN lcDummy
*--End of Function lfvRpForm

*!*************************************************************
*! Name      : lfvRpForm
*! Developer : Nader Nabil (NNA)
*! Date      : 29/11/2004
*! Purpose   : RELEASE RELATIONSHIP AND FILES
*!*************************************************************
*! Called from : The PRG.
*!*************************************************************
*!C123950,1
FUNCTION lfRelease
DIMENSION laFiles[2]
STORE 'PIKTKT'   TO laFiles[1]
STORE 'PACK_HDR' TO laFiles[2]
FOR I = 1 TO 2
  IF USED(laFiles[I])
    SELECT &laFiles[I]
    SET RELATION TO
    USE IN &laFiles[I]
  ENDIF
ENDFOR
*--End of Function lfRelease
