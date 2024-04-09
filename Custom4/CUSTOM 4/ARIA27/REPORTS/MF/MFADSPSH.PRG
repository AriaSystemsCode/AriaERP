*:************************************************************************
*: Program file  : MFADSPSH.PRG (C# )
*: Program desc. : Adornment Spread Sheet Report,
*:        System : Aria Apparel System (A2.7)
*:        Module : MF (MANUFACTURING) 
*:      Developer: AHMED SALAH SHALABY _ (SSH)
*:************************************************************************
*: Calls :PROCEDURES :
*:        FUNCTIONS  : 
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*:Modification :-
*:B#603213,1 SSH 18/10/99   1-Adornment Cover Sheet:
*:                               Early start is not displayed 
*:                               One grand total at the end of the report which totals quantites from all designs from all pages.  Currently sub total is calculating only sum of values on last printed page.
*:                            2-Adornment Spreadsheet:
*:                               What is done is completely wrong
*:                               It should call the standard pick ticket form 'ALPKTKFP with a list of all pick tickets included in this adornment PO
*:                               However we also need ability to select the <PikTkt> in list.  It should only show me the pick tickets that are on this adorment PO
*:                               The name should be Adornment Pick Ticket as opposed to Adornment Spread Sheet
*:                               Ability to select all documents or one of them
*:                            3-Namedrop Report :
*:                               Orders with no namedrops are not supposed to be included in this report.  
*:                               Begin new page each time the Design changes and place Design Code in page title - centered.
*:                               missing info: sales order # , piktkt#, start date, allocated qty
*:                            4-Shipping Report :
*:                               missing info: sales order #, picking ticket #, start date
*:                            5-Optional Grid :
*:                               Select by one Adorn P/O only, and display a PoPup for :
*:                               All Piktkts , Not Printed , Selected ->(piktkts).
*:B#603251,1 SSH 31/10-99     1)  Adornment Cover Sheet
*:                                 The Total Pieces does not match the Total Qty on the Adornment
*:                                 Planning Tool screen.   These two numbers should match.
*:                            2) No Style Bin Location is being printed on the Adornment Pick Ticket.
*:                            3) The Shipping Report is showing the same line multiple times on the
*:                               report.  There should only be a single unique line printed.
*:                            4) The Shipping Report is missing the Order #, PikTkt, and Start
*:                               Date on the report.
*:                            5) The Namedrop Report is missing Order #, PikTkt, Start Date and
*:                               allocated Qty information on the report.
*:                            6) The Namedrop Report appears to be repeating duplicate lines.  The
*:                               report should only print unique lines.
*:B603298,1 TAK 11/23/1999  Fix of wrong data colloecting from bomvar file.
*:B603360,1 KHM 12/23/99    Fix the bug of no printing all the piktkt
*:B603360,1 KHM 12/23/99    of the selected order.
*:B603437,1 KHM 02/06/2000   Allowing the PT to be sorted by designs.
*:B603488,1 KHM 02/29/2000 Fix the bug of the difference between the qty
*:B603488,1                in the adornment planning tool and the cover page.
*:B000000,1 TAK 10/15/2000 (TTTT) Add a complete date to a report.
*:C102797,1 AMH 02/05/2003 Change the cDsgnCode field to be 8 Chr.
*:************************************************************************
*--- lcRpTmp take value from option grid.
DIMENSION laTmpOrdLS[1,4]
DIMENSION laTmpOrdHS[1,4]
lcOrdLinNo = 0
IF EMPTY(lcRpAdOrder )
  WAIT WINDOW "Adornment Order Can Not Be Empty.."
  SET DEVICE TO SCREE
  RETURN
ENDIF

IF llOgFltCh
  WAIT WINDOW "Selecting record for report .. Please Wait " NOWAIT
*--- Cover Sheet
  IF lcRPReport $ 'AC'
*TTTT Added new field EarComp.
      USE IN IIF(USED(lcRpCvrSh),lcRpCvrSh,0)
      
      *C102797,1 AMH Change the cDsgnCode field to be 8 Chr. [Start]
      *CREATE TABLE (gcWorkDir+lcRpCvrSh) (PO C(6),DESIGN C(6), TotPiec N(15.5), EarDat D,;
                    cdsgnlink C(3),TmpRank N(5), EarComp D)
      CREATE TABLE (gcWorkDir+lcRpCvrSh) (PO C(6),DESIGN C(8), TotPiec N(15.5), EarDat D,;
                    cdsgnlink C(3),TmpRank N(5), EarComp D)
      *C102797,1 AMH [End]
      
      INDEX ON PO + DESIGN TAG (lcRpCvrSh)
      SELECT ORDHDR
      
      *C102797,1 AMH Change the cDsgnCode field to be 8 Chr. [Start]
      *CREATE TABLE (gcWorkDir+lcRpOrdTmp) (PO C(6),SORD C(6), DESIGN C(6), TotPiec N(15.5), EarDat D,;
                    Complete D,Start D,Account C(5),Priority C(3),;
                    cdsgnlink C(3),TmpRank N(5),LineNo N(6), EarComp D )
      CREATE TABLE (gcWorkDir+lcRpOrdTmp) (PO C(6),SORD C(6), DESIGN C(8), TotPiec N(15.5), EarDat D,;
                    Complete D,Start D,Account C(5),Priority C(3),;
                    cdsgnlink C(3),TmpRank N(5),LineNo N(6), EarComp D )
      *C102797,1 AMH [End]
      
      INDEX ON PO + DESIGN +SORD+STR(LineNo,6) TAG (lcRpOrdTmp)
  ENDIF

*---Spred Sheet OR  Name Drop Report OR Shipping Report.
  IF lcRPReport $ 'AHNS'
      USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
      
      *C102797,1 AMH Change the cDsgnCode field to be 8 Chr. [Start]
      *CREATE TABLE (gcWorkDir+lcRpTmp) (PO C(6),DESIGN C(6),TotPiec N(15.5),;
                   DrpId C(6),Store C(8),ACCOUNT C(5),Order C(6),Pk C(6),cdsgnlink C(3),Date d,pos C(6),;
                   LineNo N(6) )
      CREATE TABLE (gcWorkDir+lcRpTmp) (PO C(6),DESIGN C(8),TotPiec N(15.5),;
                   DrpId C(6),Store C(8),ACCOUNT C(5),Order C(6),Pk C(6),cdsgnlink C(3),Date d,pos C(6),;
                   LineNo N(6) )
      *C102797,1 AMH [End]
      
      *B603360,1 KHM 12/23/99 (Begin) Adding the pk to the index
      *INDEX ON PO + DESIGN +DrpId +Account+Store+STR(LineNo,6)TAG (lcRpTmp)
      INDEX ON PO+DESIGN+DrpId+Account+Store+Pk+STR(LineNo,6) TAG (lcRpTmp)
      *B603360,1 KHM 12/23/99 (End)
  ENDIF
  SELECT POSHDR
  SET FILTER TO cStyType = 'A'
  *--Prepering data.
  IF lcRPReport $ 'AC'
    =lfColData()
    =lfPrepData()
  ENDIF
  IF lcRPReport $ 'AHNS'

    *B603437,1 KHM 02/06/2000 (Begin) Adding the following cursor in order
    *B603437,1                to get the pick tickets sorted by design code.
    IF lcRPReport $ 'AS'
      lcPTDesign = gfTempName()
      lcSndIndex = gfTempName()
      lcTemp     = ''
      
      *C102797,1 AMH Change the cDsgnCode field to be 8 Chr. [Start]
      *CREATE CURSOR (lcPTDesign) (cPikTkt C(6), cDesign C(6), cSortNo C(6))
      CREATE CURSOR (lcPTDesign) (cPikTkt C(6), cDesign C(8), cSortNo C(6))
      *C102797,1 AMH [End]
      
      INDEX ON cPikTkt TAG (lcPTDesign) OF (lcPTDesign)
    ENDIF
    *B603437,1 KHM 02/06/2000 (End)
    =lfDataSH()
  ENDIF
  *--

ENDIF


**---Cover Sheet|Spread Sheet|Name Drop|Shipping Report|All~C|S|N|H|A
***************************************************************************
IF lcRPReport $ 'AC'
  lcTitle =  'Adornment order cover sheet report'
  lcRpForm  = 'MFADSPCH'
  =gfCrtFrm(lcRpForm,lcOGFormArr,llOGRefForm)
  = lfRepPltFr(lcRpForm)
  SELECT (lcRpCvrSh)
  SET RELATION TO Design INTO (lcTmpRank)
  GOTO TOP
  DO gfDispRe WITH EVAL('lcRpForm') 
  SELECT (lcRpCvrSh)
  SET RELATION TO
ENDIF

***************************************************************************
IF lcRPReport $ 'AS' AND FILE(gcRepHome+'ALPKTKFP.FRX')
*---PikTkt Form (BEGIN)
  lcTitle =  '             Adornment Pick Ticket'
  lcRpForm  = 'ALPKTKFP'
  =gfCrtFrm(lcRpForm,lcOGFormArr,llOGRefForm)
  =lfRepPltFr(lcRpForm)
  SELECT(lcRpTmp)
  SELECT DISTINCT pk FROM (lcRpTmp) INTO DBF (gcWorkDir+lcRpPkTkt)
  SELECT(lcRpPkTkt)
  INDEX ON Pk TAG (lcRpPkTkt)  
  =lfPikTkt()
  GOTO TOP
  
  *B603437,1 KHM 02/06/2000 (Begin) Sorting the PT by design code.
  lnOldAlias = SELECT(0)
  INDEX ON PikTkt TAG (lcSndIndex)
  SELECT (lcPTDesign)
  INDEX ON cDesign+cPikTkt TAG (lcPTDesign) OF (lcPTDesign)
  lnSortNo = 0
  SCAN
    lnSortNo = lnSortNo + 1
    REPLACE cSortNo WITH PADL(lnSortNo,6,'0')
    SELECT (lnOldAlias)
    SEEK &lcPTDesign..cPikTkt
    REPLACE REST cSortNo WITH &lcPTDesign..cSortNo WHILE PikTkt = &lcPTDesign..cPikTkt
  ENDSCAN
  SELECT (lnOldAlias)
  INDEX ON cSortNo+cTktNo+Order+DsnPik+cstylepos+cDsgnCode+cNdrpid+cLocation+Style TAG (lcTemp) UNIQUE
  GOTO TOP
  *B603437,1 KHM 02/06/2000 (End)
  
  lcRpPkTy = 'A'  
  DO gfDispRe WITH gcrephome+EVAL('lcRpForm')
  SELECT (lcRpTmp)
  SET RELATION TO
  SELECT PIKTKT
  SET RELATION TO
  *IF The Temp. Order Line file is Opened in one of the work areas
  IF USED(lcTmpOrdL)
    SELECT (lcTmpOrdL)
    SET RELATION TO
    USE
  ENDIF    && End of IF
  IF USED(lcTmpOrdH)
    SELECT (lcTmpOrdH)
    SET RELATION TO
    USE
  ENDIF    && End of IF
ENDIF

***************************************************************************
IF lcRPReport $ 'AN'
  lcTitle =  '                  Name drop report'
  lcRpForm  = 'MFADSPNH'
  =gfCrtFrm(lcRpForm,lcOGFormArr,llOGRefForm)
  =lfRepPltFr(lcRpForm)
  SELECT (lcRpTmp)
  SET ORDER TO (lcRpTmp)
  GOTO TOP
  DO gfDispRe WITH EVAL('lcRpForm') 
  SELECT (lcRpTmp)
  SET ORDER TO (lcRpTmp)
  SET RELATION TO
ENDIF

***************************************************************************
IF lcRPReport $ 'AH'
  lcTitle =  '                   Shipping report'
  lcRpForm  = 'MFADSPHH'
  =gfCrtFrm(lcRpForm,lcOGFormArr,llOGRefForm)
  =lfRepPltFr(lcRpForm)
  SELECT (lcRpTmp)
  
  *B603360,1 KHM 12/23/99 (Begin) Adding the pk to the index
  *INDEX ON PO+DESIGN+Order+Account+Store TAG lcRTmpUX UNIQUE 
  INDEX ON PO+DESIGN+Order+Account+Store+Pk TAG lcRTmpUX UNIQUE 
  *B603360,1 KHM 12/23/99 (End)
  
  SET ORDER TO lcRTmpUX
  GOTO TOP
  DO gfDispRe WITH EVAL('lcRpForm') 
  SELECT (lcRpTmp)
  SET ORDER TO (lcRpTmp)
  SET RELATION TO
ENDIF
SET DEVICE TO SCREE
*---Functions
RETURN



*!*************************************************************
*! Name      : lfColData
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 24/07/1999
*! Purpose   : Function Colect Data.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfColData()
*!*************************************************************
FUNCTION lfColData

lcPo = lcRpAdOrder
SELECT BOMVAR
=SEEK('AD'+lcPo)
SCAN REST WHILE cidtype+ccost_id+STR(lineno,6) = 'AD'+lcPo
  WAIT WINDOW 'Colect data for Adr. order # '+lcPo NOWAIT
  lcline = STR(lineno,6)
  
  *B603360,1 KHM 12/23/99 (Begin) Checking on the PosLn to get the PikTkt#
  =SEEK('A'+lcPo,'PosLn')
  SELECT PosLn
  LOCATE REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'A'+lcPo;
              FOR STR(LineNo,6) = lcline AND TranCd = '1'
  IF !FOUND()
    SELECT BOMVAR
    LOOP
  ENDIF            
  =SEEK(PosLn.ShipNo,'PikTkt')
  IF PikTkt.Status = 'X'
    SELECT BOMVAR
    LOOP
  ENDIF  
  
  IF PikTkt.Status = 'C'
    =SEEK(PikTkt.PikTkt+PikTkt.Order,'PikLine')
    SELECT PIKLINE
    LOCATE REST WHILE piktkt+order+STR(lineno,6)=PikTkt.PikTkt+PikTkt.Order ;
                  FOR STR(nPoLineNo,6) = lcline
    llFound = FOUND()
    SELECT BOMVAR

    IF !llFound
      LOOP
    ENDIF

  ELSE   && PikTkt.Status $ 'OHP'
  
    SELECT CUTPICK
    =SEEK('2'+lcPo)
    *B603488,1 KHM 02/29/2000 (Begin) Changing the locate command by changing
    *B603488,1                the ALLTRIM(lcline) to be PADL(lcLine,6)
    *LOCATE REST WHILE trancd+ctktno+style = '2'+lcPo FOR ALLTRIM(cTktLineNo) = ALLTRIM(lcline)
    LOCATE REST WHILE trancd+ctktno+style = '2'+lcPo FOR cTktLineNo = PADL(lcline,6)
    *B603488,1 KHM 02/29/2000 (End)
    IF !FOUND()
      SELECT BOMVAR
      LOOP
    ENDIF
  
    lnOrdLn  = INT(VAL(CUTPICK.cOrdLine))
    lcSOrder = CUTPICK.Order

    SELECT ORDLINE
    IF !SEEK('O'+lcSOrder+STR(lnOrdLn,6),'OrdLine') OR ORDLINE.Piktkt='*'
       SELECT BOMVAR
       LOOP
    ENDIF
  
  ENDIF
  *B603360,1 KHM 12/23/99 (End) 
    
    
   
  *--Check existing of ordline piktk in selection.
  IF !lfVldPikt()
    LOOP
  ENDIF
   
  *B603360,1 KHM 12/23/99 (Begin) Seeking from the PikTkt
  *=SEEK('O'+lcSOrder,'ORDHDR')
  =SEEK('O'+Piktkt.Order,'ORDHDR')
  *B603360,1 KHM 12/23/99 (End)
  
  =SEEK(BOMVAR.cdsgnCode,'ICDESIGN')

  SELECT (lcRpOrdTmp)   && index ->PO + DESIGN +SORDTAG+Account
  IF !SEEK(lcPo+BOMVAR.cdsgnCode+ORDHDR.Order+STR(BomVar.LineNo,6))
    APPEND BLANK
  ENDIF
  *B603360,1 KHM 12/23/99 (Begin) Changing the replacement.
  *REPLACE PO        WITH lcPo,;
          Design    WITH BOMVAR.cdsgnCode  ,;
          TotPiec   WITH OrdLine.TotPik    ,;
          cdsgnlink WITH ICDESIGN.cdsgnlink,;
          LineNo    WITH BomVar.LineNo  ,;
          SOrd      WITH ORDHDR.ORDER   ,;
          Complete  WITH ORDHDR.Complete,;
          Start     WITH ORDHDR.Start   ,;
          Account   WITH ORDHDR.Account ,;
          Priority  WITH ORDHDR.Priority

  REPLACE PO        WITH lcPo,;
          Design    WITH BOMVAR.cdsgnCode  ,;
          TotPiec   WITH IIF(PikTkt.Status='C',PikLine.TotPik,OrdLine.TotPik),;
          cdsgnlink WITH ICDESIGN.cdsgnlink,;
          LineNo    WITH BomVar.LineNo  ,;
          SOrd      WITH ORDHDR.ORDER   ,;
          Complete  WITH ORDHDR.Complete,;
          Start     WITH ORDHDR.Start   ,;
          Account   WITH ORDHDR.Account ,;
          Priority  WITH ORDHDR.Priority
  *B603360,1 KHM 12/23/99 (End)
ENDSCAN

*!*************************************************************
*! Name      : lfPrepData
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 24/07/1999
*! Purpose   : Function Calculate Rank.
*!*************************************************************
*! Example            :  =lfPrepData()
*!*************************************************************
FUNCTION lfPrepData

SELECT (lcRpOrdTmp)
GOTO TOP
lnRecNo = RECCOUNT()
lnBy3   = ROUND(lnRecNo/3,0)
lnFirst = lnRecNo - (2*lnBy3)
lnSecnd = lnBy3
lnThird = lnBy3
IF FILE(gcDataDir+'SORTLEVL'+'.MEM')
  RESTORE FROM gcDataDir+'SORTLEVL'+'.MEM' ADDITIVE
  INDEX ON LASORTARY[1,2]+LASORTARY[2,2]+LASORTARY[3,2]+LASORTARY[4,2];
           TAG (lcRpOrdTmp)+'1'
ELSE
  INDEX ON SORD TAG (lcRpOrdTmp)+'1'
ENDIF

GOTO TOP
IF lnRecNo < 3
  REPLACE ALL TmpRank WITH 1
ELSE
  REPLACE TmpRank WITH 1 NEXT lnFirst
  SKIP
  REPLACE TmpRank WITH 2 NEXT lnSecnd
  SKIP
  REPLACE TmpRank WITH 3 NEXT lnThird
ENDIF

*---lcRpTmp
SET ORDER TO (lcRpOrdTmp)
GOTO TOP
DO WHILE !EOF()
  lcAdOr = PO
  lcDes  = DESIGN
  SCAN REST WHILE PO + DESIGN = lcAdOr+lcDes
    WAIT WINDOW 'Calculate rank...Please Wait ' NOWAIT
    SELECT (lcRpCvrSh)
    IF !SEEK(lcAdOr+lcDes)
*TTTT added EarComp to be updated.
      APPEND BLANK
      REPLACE PO        WITH lcAdOr,;
              DESIGN    WITH lcDes ,;
              TotPiec   WITH &lcRpOrdTmp..TotPiec,;
              EarDat    WITH &lcRpOrdTmp..Start,;
              EarComp   WITH &lcRpOrdTmp..Complete,;
              cdsgnlink WITH &lcRpOrdTmp..cdsgnlink,;
              TmpRank   WITH &lcRpOrdTmp..TmpRank
    ELSE
      REPLACE TotPiec   WITH TotPiec+&lcRpOrdTmp..TotPiec,;
              TmpRank   WITH TmpRank+&lcRpOrdTmp..TmpRank
      IF &lcRpOrdTmp..Start < EarDat 
        REPLACE EarDat  WITH &lcRpOrdTmp..Start,;
                EarComp WITH &lcRpOrdTmp..Complete
      ENDIF
    ENDIF
  ENDSCAN
ENDDO
SELECT (lcRpCvrSh)
SET ORDER TO

SELECT Design,TmpRank,000 AS DsgRank FROM (lcRpCvrSh) INTO DBF (lcTmpRank)
SELECT (lcTmpRank)
INDEX ON TmpRank TAG (lcTmpRank)
GO TOP
lnDsgRank = 0
lnOldTmRk = 0
SCAN
  IF lnOldTmRk <> TmpRank
    lnDsgRank = lnDsgRank + 1
    lnOldTmRk = TmpRank
  ENDIF
  REPLACE DsgRank WITH lnDsgRank 
ENDSCAN
INDEX ON Design TAG (lcTmpRank)
SET ORDER TO TAG (lcTmpRank)
SELECT (lcRpCvrSh)
SET ORDER TO (lcRpCvrSh)


*!*************************************************************
*! Name      : lfDataSH
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 24/07/1999
*! Purpose   : Functio to collect data.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDataSH()
*!*************************************************************
FUNCTION  lfDataSH

lcPo = lcRpAdOrder
SELECT BOMVAR
=SEEK('AD'+lcPo)
*B603298,1 comment for condition.
*SCAN REST WHILE cidtype+ccost_id+STR(lineno,6) = 'AD'+lcPo ;
*          FOR IIF(lcRPReport $ 'AN',!EMPTY(BOMVAR.CndrpId),.T.)

SCAN REST WHILE cidtype+ccost_id+STR(lineno,6) = 'AD'+lcPo

  WAIT WINDOW 'Colecting data for Adr. order # '+lcPo NOWAIT
  lcline = STR(lineno,6)
  SCATTER MEMVAR MEMO

  *B603360,1 KHM 12/23/99 (Begin) Checkig of the posln to get the piktkt#
  =SEEK('A'+lcPo,'PosLn')
  SELECT PosLn
  LOCATE REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'A'+lcPo;
              FOR STR(LineNo,6) = lcline AND TranCd = '1'
  IF !FOUND()
    SELECT BOMVAR
    LOOP
  ENDIF            
  =SEEK(PosLn.ShipNo,'PikTkt')
  IF PikTkt.Status = 'X'
    SELECT BOMVAR
    LOOP
  ENDIF  
  
  IF PikTkt.Status = 'C'
    =SEEK(PikTkt.PikTkt+PikTkt.Order,'PikLine')
    SELECT PIKLINE
    LOCATE REST WHILE piktkt+order+STR(lineno,6)=PikTkt.PikTkt+PikTkt.Order ;
                  FOR STR(nPoLineNo,6) = lcline
    llFound = FOUND()
    SELECT BOMVAR

    IF !llFound
      LOOP
    ENDIF

  ELSE   && PikTkt.Status $ 'OHP'

    SELECT CUTPICK
    =SEEK('2'+lcPo)
    *B603488,1 KHM 02/29/2000 (Begin) Changing the locate command by changing
    *B603488,1                the ALLTRIM(lcline) to be PADL(lcLine,6)
    *LOCATE REST WHILE trancd+ctktno+style = '2'+lcPo FOR ALLTRIM(cTktLineNo) = ALLTRIM(lcline)
    LOCATE REST WHILE trancd+ctktno+style = '2'+lcPo FOR cTktLineNo = PADL(lcline,6)
    *B603488,1 KHM 02/29/2000 (End) 
    IF !FOUND()
      SELECT BOMVAR
      LOOP
    ENDIF
  

    lnOrdLn  = INT(VAL(CUTPICK.cOrdLine))
    lcSOrder = CUTPICK.Order

    SELECT ORDLINE
    IF !SEEK('O'+lcSOrder+STR(lnOrdLn,6),'OrdLine') OR ORDLINE.Piktkt='*'
       SELECT BOMVAR
       LOOP
    ENDIF
  
  ENDIF
  *B603360,1 KHM 12/23/99 (End)


  *--Check existing of ordline piktk in selection.
  IF !lfVldPikt()
    LOOP
  ENDIF

  SELECT (lcRpTmp)    && index ->PO + DESIGN +DrpId +Account+Store+STR(LineNo,6)
  *B603360,1 KHM 12/23/99 (Begin) Adding the PikTkt.PikTkt to the seek command  
  *IF !SEEK(lcPo+BOMVAR.cdsgnCode+BOMVAR.cnDrpId +ORDLINE.Account+ORDLINE.Store)
  IF !SEEK(lcPo+BOMVAR.cdsgnCode+BOMVAR.cnDrpId+PikTkt.Account+;
                                        PikTkt.Store+PikTkt.PikTkt)
  *B603360,1 KHM 12/23/99 (End)
    APPEND BLANK
    *B603360,1 KHM 12/23/99 (Begin) changing the replacements.
    *REPLACE TotPiec WITH ORDLINE.TotPik
    REPLACE TotPiec WITH IIF(PikTkt.Status='C',PikLine.TotPik,ORDLINE.TotPik)
    *B603360,1 KHM 12/23/99 (End)
  ELSE
    *B603360,1 KHM 12/23/99 (Begin) changing the replacements.
    *REPLACE TotPiec WITH TotPiec+ORDLINE.TotPik
    REPLACE TotPiec WITH TotPiec+IIF(PikTkt.Status='C',PikLine.TotPik,ORDLINE.TotPik)
    *B603360,1 KHM 12/23/99 (End)
  ENDIF
  *B603360,1 KHM 12/23/99 (Begin) changing the replacements.
  *REPLACE ORDER   WITH ORDLINE.ORDER  ,;
          ACCOUNT WITH ORDLINE.ACCOUNT,;
          Date    WITH ORDLINE.Start  ,;
          Account WITH ORDLINE.Account,;
          Store   WITH ORDLINE.Store  ,;
          pk      WITH ORDLINE.PikTkt ,;
          LineNo  WITH BOMVAR.LineNo  ,;
          PO      WITH lcPo           ,;
          POS     WITH BOMVAR.cStylePos,;
          DrpId   WITH BOMVAR.cnDrpId  ,;
          DESIGN  WITH BOMVAR.cDsgnCode
  REPLACE ORDER   WITH PikTkt.ORDER  ,;
          ACCOUNT WITH PikTkt.ACCOUNT,;
          Date    WITH IIF(PikTkt.Status='C',PikLine.Start,ORDLINE.Start),;
          Store   WITH PikTkt.Store  ,;
          Pk      WITH PikTkt.PikTkt ,;
          LineNo  WITH BOMVAR.LineNo  ,;
          PO      WITH lcPo           ,;
          POS     WITH BOMVAR.cStylePos,;
          DrpId   WITH BOMVAR.cnDrpId  ,;
          DESIGN  WITH BOMVAR.cDsgnCode
  *B603360,1 KHM 12/23/99 (End)
  
  *B603437,1 KHM 02/06/2000 (Begin) If the PT was not found in the lcPTDesign
  *B603437,1                cursor then add it, otherwise check if the 
  *B603437,1                coming design is smaller than the existing one 
  *B603437,1                then replace the small design.
  IF lcRPReport $ 'AS'
    IF !SEEK (Pk,lcPTDesign)
      INSERT INTO (lcPTDesign) (cPikTkt,cDesign) VALUES (PikTkt.PikTkt,BOMVAR.cDsgnCode)
    ELSE
      IF BOMVAR.cDsgnCode < &lcPTDesign..cDesign
        REPLACE &lcPTDesign..cDesign WITH BOMVAR.cDsgnCode
      ENDIF
    ENDIF
  ENDIF  
  *B603437,1 KHM 02/06/2000 (End)
ENDSCAN

*!*************************************************************
*! Name      : lfVldPikt
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 24/07/1999
*! Purpose   : Function Ceck piktkt.
*!*************************************************************
FUNCTION lfVldPikt

DO CASE
  CASE lcRPTkt = 'S'
    IF USED(laOgFxFlt[1,6]) AND RECCOUNT(laOgFxFlt[1,6]) <> 0
      *B603360,1 KHM 12/23/99 (Begin) Changing the Seek Command
      *IF !SEEK(ORDLINE.Piktkt,laOgFxFlt[1,6])
      IF !SEEK(PikTkt.Piktkt,laOgFxFlt[1,6])
      *B603360,1 KHM 12/23/99 (End)
        RETURN .F.
      ENDIF
    ENDIF
  CASE lcRPTkt = 'N'
    *B603360,1 KHM 12/23/99 (Begin) Changing the seek command.
    *IF !SEEK(ORDLINE.Piktkt,'PIKTKT')
    IF EOF('PIKTKT')
    *B603360,1 KHM 12/23/99 (End)
      RETURN .F.
    ELSE
      IF PIKTKT.PrtFlag = 'P'
        RETURN .F.
      ENDIF
    ENDIF
ENDCASE
RETURN .T.


*!*************************************************************
*! Name      : lfRtRank
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 24/07/1999
*! Purpose   : Function Print Rank.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfRtRank()
*!*************************************************************
FUNCTION lfRtRank
RETURN(&lcTmpRank..DsgRank)



*:***************************************************************************
*: Program file  : ALPKTKFP
*: Program desc. : Custom Picking Ticket Form for Fresh Produce
*! Date          : 17/10/1999
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Ahmed Salah Shalaby - (SSH)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO ALPKTKFP
*:***************************************************************************
FUNCTION lfALPKTKFP

*-- lcTemp represents the temp file to collect the data needed for
*-- the custom form. It will be taken from the standard collected data
*-- plus some added fields
STORE '' TO lcTemp,lcTempHd
lcTemp   = gfTempName()
lcTempHd = gfTempName()
*-- lcTmpOrdL is the standard colleted temp file
SELECT (lcTmpOrdL)
*-- copy structure to array laFileStru
=AFIELDS(laFileStru)           
*-- add needed fields for the custom form
lnFileStru = ALEN(laFileStru,1)
*B603437,1 KHM 02/06/2000 (Begin) Increasing the number of elements in the Array
*DIMENSION laFileStru[lnFileStru + 10, 4]
DIMENSION laFileStru[lnFileStru + 11, 4]
*B603437,1 KHM 02/06/2000 (End)

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'CTktNo'  && adornment po#
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cDsgnCode'  && design code
laFileStru[lnFileStru ,2] = 'C'

*C102797,1 AMH Change the cDsgnCode field to be 8 Chr. [Start]
*laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,3] = 8
*C102797,1 AMH [End]

laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cNdrpid'  && name drop id
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cstylepos'  && Style Pos.
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Account'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 5
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Store' 
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 8
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'TOTALQTY'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 8
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cLocation' 
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cpicktype'  && adornment po#
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 2
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'DsnPik'  && DesCode+PikTkt
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 15
laFileStru[lnFileStru ,4] = 0

*B603437,1 KHM 02/06/2000 (Begin) Adding a new field to hold the sequntial
*B603437,1                No. of the design for each PT.
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cSortNo'  && The sequential No. of the Design.
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
*B603437,1 KHM 02/06/2000 (End)

CREATE TABLE  (gcWorkDir+lcTemp)  FROM ARRAY laFileStru
=gfOpenFile(gcWorkDir+lcTemp ,'','EX')


*--Create header file for this line.

DIMENSION laFileStr1[3,4]
laFileStr1[1 ,1] = 'CTktNo'  && adornment po#
laFileStr1[1 ,2] = 'C'
laFileStr1[1 ,3] = 6
laFileStr1[1 ,4] = 0
laFileStr1[2 ,1] = 'TotPik'  && design code
laFileStr1[2 ,2] = 'N'
laFileStr1[2 ,3] = 8
laFileStr1[2 ,4] = 0
laFileStr1[3 ,1] = 'cpicktype'  && adornment po#
laFileStr1[3 ,2] = 'C'
laFileStr1[3 ,3] = 2
laFileStr1[3 ,4] = 0

CREATE TABLE  (gcWorkDir+lcTempHd) FROM ARRAY laFileStr1
=gfOpenFile(gcWorkDir+lcTempHd,'','EX')
INDEX ON CTktNo TAG (lcTempHd)

*-- open bomvar, icdesign, icnamdrp, cutpick
STORE .F. TO llBomvar, llDesign, llNamDrp, llCutPick
IF !llBomvar AND !USED('BOMVAR')
  llBomvar = gfOpenFile(gcDataDir+'BOMVAR',gcDataDir+'BOMVAR','SH')  && cidtype+ccost_id+STR(lineno,6)
ENDIF
IF !llDesign AND !USED('ICDESIGN')
  llDesign = gfOpenFile(gcDataDir+'ICDESIGN',gcDataDir+'ICDESIGN','SH')  && cdsgncode
ENDIF
IF !llNamDrp AND !USED('ICNAMDRP')
  llNamDrp =  gfOpenFile(gcDataDir+'ICNAMDRP',gcDataDir+'IcNamDrpId','SH')  && cndrpid
ENDIF
IF !llCutPick AND !USED('CUTPICK')
  llCutPick = gfOpenFile(gcDataDir+'CUTPICK',gcDataDir+'CUTORD','SH')  &&trancd+order+cordline
ENDIF

*--
*-- scan throught lcTmpOrdL to evaluate each adornment po# (if exist)
*--,design and namedrop
SELECT whsloc
SET ORDER TO Whslocst
SELECT BOMVAR
SET ORDER TO BOMVAR
SELECT CUTPICK
SET ORDER TO CUTORD
SELECT (lcTmpOrdL)
GOTO TOP
*--

SCAN FOR cgrupdetal='D'
  SCATTER MEMVAR MEMO
  =SEEK('O'+m.Order+STR(m.lineno,6),'OrdLine')
  *-- we may have one adornment po per one or several piktkt
  *-- or not yet the adornment po been made, in this case
  *-- we'll collect so adornments
  IF SEEK('2'+ m.Order +STR(m.lineno,6),'CUTPICK')
    lcKey = "'AD' + CUTPICK.cTktNo + PADL(ALLTRIM(cutpick.ctktlineno),6)"
  ELSE
    lcKey = "'SO' + &lcTmpOrdL..Order + STR(&lcTmpOrdL..LineNo,6)"
  ENDIF
  *-- either adornment po designs/name drops or order designs/name
  IF SEEK(&lcKey,'BOMVAR')
    m.cpicktype = "A"
    SELECT (lcTempHd)
    IF !SEEK(IIF(BOMVAR.cIdType='AD',BOMVAR.cCost_id,&lcTmpOrdL..Piktkt))
      APPEND BLANK
    ENDIF
    REPLACE cTktNo WITH IIF(BOMVAR.cIdType='AD',BOMVAR.cCost_id,&lcTmpOrdL..Piktkt) ,;
            TotPik WITH TotPik + &lcTmpOrdL..TotPik

    SELECT BOMVAR
    SCAN WHILE  cIdType + cCost_Id + STR(lineno,6) = &lcKey
      SELECT (lcTemp)
      APPEND BLANK
      GATHER MEMVAR MEMO
*---SSHNEW
      REPLACE cTktNo     WITH IIF(BOMVAR.cIdType = 'AD',BOMVAR.cCost_id,&lcTmpOrdL..Piktkt)  ,;
              cDsgnCode  WITH BOMVAR.cDsgnCode ,;
              cNdrpid    WITH BOMVAR.cNdrpid ,;
              cstylepos  WITH Bomvar.cstylepos,;
              Account    WITH ORDLINE.Account,;
              Store      WITH ORDLINE.Store

      lcWhCode = IIF(SEEK('A'+BOMVAR.cCost_Id,'POSHDR'),PADR(POSHDR.Vendor,6),ORDLINE.cWareCode)
      IF SEEK(OrdLine.Style+SPACE(06)+lcWhCode,'whsloc')
         REPLACE cLocation WITH whsloc.cLocation
      ENDIF

    ENDSCAN
  ELSE  && Pick tiket was blank (not adornment).
    m.cpicktype = "B"
    SELECT (lcTempHd)
    IF !SEEK(&lcTmpOrdL..Piktkt)
      APPEND BLANK
    ENDIF
    REPLACE cTktNo WITH &lcTmpOrdL..Piktkt ,;
            TotPik WITH TotPik + &lcTmpOrdL..TotPik
    SELECT (lcTemp)
    APPEND BLANK
    GATHER MEMVAR MEMO
    REPLACE cTktNo    WITH &lcTmpOrdL..Piktkt,;
            Account   WITH ORDLINE.Account,;
            Store     WITH ORDLINE.Store

    lcWhCode = IIF(SEEK(ORDLINE.Piktkt,'PIKTKT'),PIKTKT.cWareCode,ORDLINE.cWareCode)
    IF SEEK(OrdLine.Style+SPACE(06)+lcWhCode,'whsloc')
       REPLACE cLocation WITH whsloc.cLocation
    ENDIF

  ENDIF
ENDSCAN

SELECT (lcTempHd)
SCAN
  SELECT (lcTemp)
  REPLACE ALL TOTALQTY WITH &lcTempHd..TotPik FOR cTktNo = &lcTempHd..cTktNo
ENDSCAN
SELECT ICNAMDRP
SET ORDER TO IcNamDrpId
SELECT ICDESIGN
SET ORDER TO ICDESIGN

SELECT (lcTemp)
INDEX ON cTktNo+Order+PikTkt+cstylepos+cDsgnCode+cNdrpid+cLocation+Style TAG (lcTemp)
LCPK='  '
SCAN
  IF LCPK <> PIKTKT
    LcDsgn = CDSGNCODE
    LCPK   = PIKTKT
  ENDIF
  REPLACE DsnPik WITH LcDsgn+PIKTKT
ENDSCAN
SET RELATION TO
SET RELATION TO Cdsgncode INTO ICDESIGN ADDITIVE
SET RELATION TO Cndrpid INTO ICNAMDRP ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SET RELATION TO "O"+order+STR(lineno,6) INTO Ordline ADDITIVE
SET RELATION TO "O"+order INTO ORDHDR ADDITIVE
*SET RELATION TO IIF(EMPTY(Store) , 'M'+Account ,;
                      'S'+Account+Store) INTO CUSTOMER ADDITIVE
*INDEX ON cTktNo+Order+PikTkt+cstylepos+cDsgnCode+cNdrpid+cLocation+Style TAG (lcTemp)
INDEX ON cTktNo+Order+DsnPik+cstylepos+cDsgnCode+cNdrpid+cLocation+Style TAG (lcTemp) UNIQUE
SELECT (lcTemp)
GOTO TOP
RETURN


*:***************************************************************************
*: Program file  : lfPikTkt
*: Program desc. : Custom Picking Ticket Form for Fresh Produce
*! Date          : 17/10/1999
*: System        : Aria Advantage Series.
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Ahmed Salah Shalaby - (SSH)
*:***************************************************************************
*: Calls : 
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : =lfPikTkt()
*:***************************************************************************
FUNCTION lfPikTkt

=lfGTmpOrdL()

SELECT (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdL)
SET ORDER TO TAG (lcTmpOrdH) IN &lcTmpOrdH

SET RELATION TO Order + PikTkt INTO &lcTmpOrdH
SET RELATION TO PikTkt INTO PIKTKT ADDITIVE
SET RELATION TO 'O' + Order INTO ORDHDR ADDITIVE

SET RELATION TO Style INTO STYLE ADDITIVE
SET RELATION TO 'S' + Scale INTO SCALE ADDITIVE
SELECT PIKTKT
SET RELATION TO cWareCode INTO WAREHOUS
SET RELATION TO IIF(EMPTY(Store) , 'M' + Account ,;
                    'S' + Account + Store) INTO CUSTOMER ADDITIVE
SELECT (lcTmpOrdL)
GO TOP
=lfALPKTKFP()


*!*************************************************************
*! Name      : lfGTmpOrdL
*! Developer : Ahmed Salah Shalaby _(SSH)
*! Date      : 17/10/1999
*! Purpose   : Function to colect the neede data in a temp. Order lines file
*!*************************************************************
*! Called from : ALPKTK.PRG
*!*************************************************************
*! Calls       : gfThermo()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
 *! Return      : None
*!*************************************************************
*
FUNCTION lfGTmpOrdL

PRIVATE lcForExp , lnTotRec , lnCurRec , lnRest , lnSavRec , lcOrder,;
 	    lcCDXName, lcFullPath
lcRpExp = 'SEEK(PikTkt,lcRpPkTkt)'
=lfInit()
lcForExp = '!EOF("PIKTKT")' + IIF(EMPTY(lcRpExp) , '' ,;
           ' .AND. (' + lcRpExp + ')')        && Variable to hold the FOR Condition to SCAN for on the ORDLINE file

*IF The Temp. Order Line file is Opened in one of the work areas
IF USED(lcTmpOrdL)
  USE IN (lcTmpOrdL)
ENDIF    && End of IF

CREATE CURSOR (lcTmpOrdL) FROM ARRAY laTmpOrdLS
ZAP
lcFullPath = SET("FULLPATH")
SET FULLPATH ON
lcCDXName  = STRTRAN(DBF() , ".TMP" , ".CDX")
SET FULLPATH &lcFullPath
INDEX ON PikTkt + Order + cGrupDetal + STR(LineNo , 6) ;
	 TAG (lcTmpOrdL) OF (lcCDXName)
*IF The Temp. Order Header file is Opened in one of the work areas
IF USED(lcTmpOrdH)
  USE IN (lcTmpOrdH)
ENDIF    && End of IF

CREATE CURSOR (lcTmpOrdH) FROM ARRAY laTmpOrdHS
ZAP
lcFullPath = SET("FULLPATH")
SET FULLPATH ON
lcCDXName = STRTRAN(DBF() , ".TMP" , ".CDX")
SET FULLPATH &lcFullPath
INDEX ON Order + PikTkt ; 
	 TAG (lcTmpOrdH) OF (lcCDXName)
SELECT (lcTmpOrdH)
SET ORDER TO

SELECT PIKTKT
SET ORDER TO
SET RELATION TO 'O' + Order INTO ORDHDR
lnTotRec = RECCOUNT()      && Varible to hold the Total count to be done [For the thermometer]
lnCurRec = 0               && Varible to hold the current count to be done [For the thermometer]

PRIVATE lcOldSelD
lcOldSelD = SET ("DEVICE")
SET DEVICE TO SCREEN
*SCAN Loop to scan the PIKTKT file FOR the Option Grid condition
SCAN FOR &lcRpExp
  lnCurRec = lnCurRec + 1
  INSERT INTO (lcTmpOrdH) (Order , PikTkt , Status) VALUES;
                          (PIKTKT.Order , PIKTKT.PikTkt , PIKTKT.Status)

*B603360,1 KHM 12/23/99 (Begin) The following code is commenting out.
* =gfThermo(lnTotRec , lnCurRec , "Selecting picking tickets. " ,;
                       'Pick ticket # :' + PIKTKT.PikTkt)
*B603360,1 KHM 12/23/99 (End)
ENDSCAN    && End of SCAN Loop

*IF The number of records Processed is less than the Total count to be done

*B603360,1 KHM 12/23/99 (Begin) The following code is commented out.
*IF lnTotRec > lnCurRec .AND. lnCurRec <> 0
  
  *FOR Loop to loop for the remains of Total count to be done
*  FOR lnRest = lnCurRec TO lnTotRec
*    =gfThermo(lnTotRec , lnRest , "Selecting picking tickets. " , '')
*  ENDFOR    && End of FOR Loop
*ENDIF    && End of IF
*B603360,1 KHM 12/23/99 (End)

SET ORDER TO TAG PIKTKT
SET RELATION TO

SELECT ORDLINE
SET RELATION TO PikTkt INTO PIKTKT
SET RELATION TO Style INTO STYLE ADDITIVE
SELECT (lcTmpOrdH)
SET ORDER TO TAG (lcTmpOrdH)
lnTotRec = RECCOUNT()      && Varible to hold the Total count to be done [For the thermometer]
lnCurRec = 0               && Varible to hold the current count to be done [For the thermometer]

*B603360,1 KHM 12/23/99 Changing the condition in the Scan Command
*SCAN FOR Status <> 'X'
SCAN FOR Status $ 'OHP'
*B603360,1 KHM 12/23/99 (End)
  lnCurRec = lnCurRec + 1
  lnSavRec = RECNO()
  
  *IF There is one or more records for this Order in the ORDLINE file
  IF SEEK('O' + &lcTmpOrdH..Order , 'ORDLINE')
    M.cGrupDetal = 'D'
    lcOrder = Order
    SELECT ORDLINE
    *SCAN Loop to scan the ORDLINE file FOR The current Order and FOR the
    *Option Grid condition
    SCAN REST;
        WHILE Order = lcOrder;
          FOR &lcForExp
      SELECT (lcTmpOrdH)
      SEEK ORDLINE.Order + ORDLINE.PikTkt
      REPLACE nWeight WITH nWeight + (STYLE.nStyWeight * ORDLINE.totpik)
      SELECT ORDLINE
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDSCAN    && End of SCAN Loop
    SCATTER MEMVAR MEMO BLANK
    SELECT (lcTmpOrdH)
    GO lnSavRec
    M.Order = Order
    LOCATE REST FOR Order + PikTkt > M.Order
    SKIP -1
  ENDIF    && End of IF
*B603360,1 KHM 12/23/99 (Begin) The following code is commented out.
*  =gfThermo(lnTotRec , lnCurRec , "Selecting order lines for pick tickets. " ,;
                       'Pick ticket # :' + PikTkt)
*B603360,1 KHM 12/23/99 (End)                       
ENDSCAN    && End of SCAN Loop

SELECT ORDLINE
SET RELATION TO
SELECT PIKLINE
SET RELATION TO Style INTO STYLE
SELECT (lcTmpOrdH)
*SCAN Loop to scan the Temp. Order Header file FOR Status = 'X' [Released]
*B603360,1 KHM 12/23/99 (Begin) Changing the condition in the Scan Command.
*SCAN FOR Status = 'X'
SCAN FOR Status $ 'CX'
*B603360,1 KHM 12/23/99 (End)
  lnCurRec = lnCurRec + 1
  *IF There is one or more records for this Order in the PIKLINE file
  IF SEEK(&lcTmpOrdH..PikTkt + &lcTmpOrdH..Order , 'PIKLINE')
    M.cGrupDetal = 'D'
    SELECT PIKLINE
    *SCAN Loop to scan the PIKLINE file FOR The current Pick Ticket & Order
    *B603360,1 KHM 12/23/99 (Begin) Optimizing the Scan
    *SCAN REST;
        WHILE PikTkt + Order = &lcTmpOrdH..PikTkt + &lcTmpOrdH..Order
    SCAN REST WHILE Piktkt+Order+STR(LineNo,6)=&lcTmpOrdH..PikTkt+&lcTmpOrdH..Order
    *B603360,1 KHM 12/23/99 (End)    
      SELECT (lcTmpOrdH)
      REPLACE nWeight WITH nWeight + (STYLE.nStyWeight * PIKLINE.totpik)
      SELECT PIKLINE
      SCATTER MEMVAR MEMO
      INSERT INTO (lcTmpOrdL)  FROM MEMVAR
    ENDSCAN    && End of SCAN Loop
    SCATTER MEMVAR MEMO BLANK
    SELECT (lcTmpOrdH)
  ENDIF    && End of IF
*B603360,1 KHM 12/23/99 (Begin) The following code is commented out.
*  =gfThermo(lnTotRec , lnCurRec , "Selecting order lines for pick tickets. " ,;
                       'Pick ticket # :' + PikTkt)
*B603360,1 KHM 12/23/99 (End)
ENDSCAN    && End of SCAN Loop
SELECT PIKLINE
SET RELATION TO
*IF The number of records Processed is less than the Total count to be done
*B603360,1 KHM 12/23/99 (Begin) The following code is commented out.
*IF lnTotRec > lnCurRec .AND. lnCurRec <> 0
  *FOR Loop to loop for the remains of Total count to be done
*  FOR lnRest = lnCurRec TO lnTotRec
*    =gfThermo(lnTotRec , lnRest ,;
              "Selecting order lines for pick tickets. " , '')
*  ENDFOR    && End of FOR Loop
*ENDIF    && End of IF
*B603360,1 KHM 12/23/99 (End)
SET DEVICE TO &lcOldSelD

*!*************************************************************
*! Name      : lfInit
*! Developer : Ahmed Salah Shalaby _(SSH)
*! Date      : 17/10/1999
*! Purpose   : Function to Init Arrayes Name.
*!*************************************************************
*! Called from : ALPKTK.PRG
*!*************************************************************
*! Calls       : gfThermo()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
 *! Return      : None
*!*************************************************************
FUNCTION lfInit

DIMENSION laTmpOrdLS[1,4]
=lfAFields('laTmpOrdLS' , 'ORDLINE' , 'LIKE QTY* , ORD* , PIK* , LINENO' +;
                          ' , SCALE , TOTQTY , TOTPIK , PRICE , GROUP' +;
                          ' , STYLE , DYELOT')

DIMENSION laTmpOrdLS[ALEN(laTmpOrdLS , 1) + 1 , 4]
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 1] = 'cGrupDetal'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 2] = 'C'
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 3] = 1
laTmpOrdLS[ALEN(laTmpOrdLS , 1) , 4] = 0

DIMENSION laTmpOrdHS[4,4]
=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'ORDER') , 4 , 1)
=ACOPY(laTmpOrdLS , laTmpOrdHS , ASCAN(laTmpOrdLS , 'PIKTKT') , 4 , 5)
laTmpOrdHS[3 , 1] = 'NWEIGHT'
laTmpOrdHS[3 , 2] = 'N'
laTmpOrdHS[3 , 3] = 10
laTmpOrdHS[3 , 4] = 2
laTmpOrdHS[4 , 1] = 'STATUS'
laTmpOrdHS[4 , 2] = 'C'
laTmpOrdHS[4 , 3] = 1
laTmpOrdHS[4 , 4] = 0

SET ORDER TO PIKTKT IN PIKTKT
SET ORDER TO CUSTOMER IN CUSTOMER
SET ORDER TO WAREHOUS IN WAREHOUS
SET ORDER TO PIKLINE IN PIKLINE
SET ORDER TO ORDHDR IN ORDHDR
SET ORDER TO ORDLINE IN ORDLINE

*!*************************************************************
*! Name      : lfAFields
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 17/10/1999
*! Purpose   : Function to places information about the structure of the
*!             a selected table into an array for a selected fields
*!*************************************************************
*! Called from : lfwRepWhen()
*!*************************************************************
*! Calls       : gfTempName()
*!*************************************************************
*! Passed Parameters : 1) The Array name
*!                     2) The File name
*!                     3) The Fields to get information about 
*!                        [You can use keleton <skel>]
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAFields

PARAMETERS lcArrayNam , lcFile , lcFields

PRIVATE lcOldAlias , lcTempName

*IF The Array name Parameter is not of type character or empty
IF TYPE('lcArrayNam') <> 'C' .OR. EMPTY(lcArrayNam)
  RETURN
ENDIF    && End of IF

lcFile = IIF(TYPE('lcFile') <> 'C' , '' , lcFile)
lcFields = IIF(TYPE('lcFields') <> 'C' .OR. EMPTY(lcFields) , '' ,;
               'FIELDS ' + lcFields)

lcOldAlias = ALIAS()               && Variable to save the old Alis

*IF The File name Parameter is not empty and this file is Opened in one
*of the work areas
IF !EMPTY(lcFile) .AND. USED(lcFile)
  SELECT (lcFile)
ENDIF    && End of IF

*IF The Fields Parameter is empty
IF EMPTY(lcFields)
  =AFIELDS(&lcArrayNam)
ELSE    && Else
  lcTempName = gfTempName()          && Variable to hold a temp name
  COPY STRUCTURE EXTENDED TO &gcWorkDir.&lcTempName &lcFields
  
  SELECT * FROM &gcWorkDir.&lcTempName INTO ARRAY &lcArrayNam
  *IF The temp. file (lcTempName) is Opened in one of the work areas
  *[The file we used for COPY STRUCTURE EXTENDED]
  IF USED(lcTempName)
    USE IN &lcTempName
  ENDIF    && End of IF
  *IF The temp. file (lcTempName) exist in the Temp. Dir.
  *[The file we used for COPY STRUCTURE EXTENDED]
  IF FILE(gcWorkDir + lcTempName + '.DBF')
    ERASE &gcWorkdir.&lcTempName..DBF
  ENDIF    && End of IF
ENDIF    && End of IF
*IF The Variable we used to save the old Alias is not empty
IF !EMPTY(lcOldAlias)
  SELECT (lcOldAlias)
ENDIF    && End of IF

*!*************************************************************
*! Name      : lfEndGroup
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 17/10/1999
*! Purpose   : Function to Update the End of Group flag and to update
*!             the PrtFlag field in the PIKTKT file if the divice is not
*!             Screen
*!*************************************************************
*! Called from : ALPKTKTA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfEndGroup

llEndGroup = .T.

*IF The Divice is not Screen
IF gcDevice <> 'SCREEN'
  =SEEK(PikTkt,'PikTkt')
  REPLACE PIKTKT.PrtFlag WITH 'P'
ENDIF    && End of IF
RETURN ''

*!*************************************************************
*! Name      : lfUsrVldFn
*! Developer : Bassem Rafaat (BWA)
*! Date      : 05/17/1999
*! Purpose   : Function to run any function withen its program
*!*************************************************************
*! Called from : Global in MA reports 
*!*************************************************************
*! Calls       : passed function as a parameter
*!*************************************************************
*! Passed Parameters : lcFncNam,lcFncLoc,lcParam
*!                     lcFncNam  : Variable hold the name function 
*!                     lcFncLoc  : Variable hold the function path
*!                     lcParam   : Variable hold the parameters 
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUsrVldFn()
*!*************************************************************
*C101484,1 Add a function to enable calling user defined functions
*C101484,1 placed on disk.
FUNCTION lfUsrVldFn
PARAMETERS lcFncNam,lcFncLoc,lcParam
*-- lcRetrn variable to hold the return value
PRIVATE lcRetrn
IF !EMPTY(lcFncNam)
  lcRetrn  = SPACE(0)
  IF EMPTY(lcFncLoc) .AND. !EMPTY(lcOptProg)
    lcFncLoc = lcOptProg
  ENDIF
  IF !EMPTY(lcFncLoc)  
    lcFncLoc= IIF(FILE(gcRepHome+lcFncLoc+'.FXP'),;
                       gcRepHome+lcFncLoc,;
  	  			       IIF(FILE(gcRepHome+gcAct_Appl+'\'+lcFncLoc+'.FXP'),;
  				            gcRepHome+gcAct_Appl+'\'+lcFncLoc,;
  				            gcRepHome+LEFT(lcFncLoc,2)+'\'+lcFncLoc))

    IF EMPTY(lcParam)
      DO (lcFncNam) IN (lcFncLoc) WITH lcRetrn
    ELSE
      DO (lcFncNam) IN (lcFncLoc) WITH lcRetrn,&lcParam
    ENDIF
    RETURN lcRetrn
  ENDIF  
ENDIF


*!*************************************************************
*! Name      : BC_Code39
*! Purpose   : Print BarCode
*!*************************************************************
*! Example   :  bc_code39('000010')
*!*************************************************************
FUNCTION BC_Code39
Parameters lcRet,m

IF EMPTY(m)
  RETURN ''
ENDIF

full  = "Basic"
check = 0
private t,x,p,cval,chktot
set talk off
* Generates character pattern for code 39 bar codes
*    m  = text to decode into a character pattern
*
* Returns:
*    Bar pattern for code 39 bar code.
*    If an illegal value is found then a null will
*    be returned to the user.

*** if the parameter is not a character then quit
if type('m')<>"C"
  return ""
endif
*** verify full parameter
if type('full')<>"C"
  full = "BASIC"
  cval = 2
else
  full = upper(full)
  if full<>"FULL"
    full = "BASIC"
    cval = 2
  else
    cval = 3
  endif
endif

*** verify check parameter
if type('check')<>'N'
  check = 0
else
  if check <> 1
    check = 0
  endif
endif

*** Check to see if user added stop/start
*** character and returns null if true
if at("*"," "+m)>1
  return ""
endif

*** decode message into bar code character pattern and place
*** it into the variable

* initialize the output string and add the start code to it
p="*"

* Dimension the conversion array
dimension cvtarr[4,128]

* Define the conversion array
cvtarr[1,1]=chr(0)
cvtarr[2,1]=""
cvtarr[3,1]="%U"
cvtarr[4,1]=0
cvtarr[1,2]=chr(1)
cvtarr[2,2]=""
cvtarr[3,2]="$A"
cvtarr[4,2]=0
cvtarr[1,3]=chr(2)
cvtarr[2,3]=""
cvtarr[3,3]="$B"
cvtarr[4,3]=0
cvtarr[1,4]=chr(3)
cvtarr[2,4]=""
cvtarr[3,4]="$C"
cvtarr[4,4]=0
cvtarr[1,5]=chr(4)
cvtarr[2,5]=""
cvtarr[3,5]="$D"
cvtarr[4,5]=0
cvtarr[1,6]=chr(5)
cvtarr[2,6]=""
cvtarr[3,6]="$E"
cvtarr[4,6]=0
cvtarr[1,7]=chr(6)
cvtarr[2,7]=""
cvtarr[3,7]="$F"
cvtarr[4,7]=0
cvtarr[1,8]=chr(7)
cvtarr[2,8]=""
cvtarr[3,8]="$G"
cvtarr[4,8]=0
cvtarr[1,9]=chr(8)
cvtarr[2,9]=""
cvtarr[3,9]="$H"
cvtarr[4,9]=0
cvtarr[1,10]=chr(9)
cvtarr[2,10]=""
cvtarr[3,10]="$I"
cvtarr[4,10]=0
cvtarr[1,11]=chr(10)
cvtarr[2,11]=""
cvtarr[3,11]="$J"
cvtarr[4,11]=0
cvtarr[1,12]=chr(11)
cvtarr[2,12]=""
cvtarr[3,12]="$K"
cvtarr[4,12]=0
cvtarr[1,13]=chr(12)
cvtarr[2,13]=""
cvtarr[3,13]="$L"
cvtarr[4,13]=0
cvtarr[1,14]=chr(13)
cvtarr[2,14]=""
cvtarr[3,14]="$M"
cvtarr[4,14]=0
cvtarr[1,15]=chr(14)
cvtarr[2,15]=""
cvtarr[3,15]="$N"
cvtarr[4,15]=0
cvtarr[1,16]=chr(15)
cvtarr[2,16]=""
cvtarr[3,16]="$O"
cvtarr[4,16]=0
cvtarr[1,17]=chr(16)
cvtarr[2,17]=""
cvtarr[3,17]="$P"
cvtarr[4,17]=0
cvtarr[1,18]=chr(17)
cvtarr[2,18]=""
cvtarr[3,18]="$Q"
cvtarr[4,18]=0
cvtarr[1,19]=chr(18)
cvtarr[2,19]=""
cvtarr[3,19]="$R"
cvtarr[4,19]=0
cvtarr[1,20]=chr(19)
cvtarr[2,20]=""
cvtarr[3,20]="$S"
cvtarr[4,20]=0
cvtarr[1,21]=chr(20)
cvtarr[2,21]=""
cvtarr[3,21]="$T"
cvtarr[4,21]=0
cvtarr[1,22]=chr(21)
cvtarr[2,22]=""
cvtarr[3,22]="$U"
cvtarr[4,22]=0
cvtarr[1,23]=chr(22)
cvtarr[2,23]=""
cvtarr[3,23]="$V"
cvtarr[4,23]=0
cvtarr[1,24]=chr(23)
cvtarr[2,24]=""
cvtarr[3,24]="$W"
cvtarr[4,24]=0
cvtarr[1,25]=chr(24)
cvtarr[2,25]=""
cvtarr[3,25]="$X"
cvtarr[4,25]=0
cvtarr[1,26]=chr(25)
cvtarr[2,26]=""
cvtarr[3,26]="$Y"
cvtarr[4,26]=0
cvtarr[1,27]=chr(26)
cvtarr[2,27]=""
cvtarr[3,27]="$Z"
cvtarr[4,27]=0
cvtarr[1,28]=chr(27)
cvtarr[2,28]=""
cvtarr[3,28]="%A"
cvtarr[4,28]=0
cvtarr[1,29]=chr(28)
cvtarr[2,29]=""
cvtarr[3,29]="%B"
cvtarr[4,29]=0
cvtarr[1,30]=chr(29)
cvtarr[2,30]=""
cvtarr[3,30]="%C"
cvtarr[4,30]=0
cvtarr[1,31]=chr(30)
cvtarr[2,31]=""
cvtarr[3,31]="%D"
cvtarr[4,31]=0
cvtarr[1,32]=chr(31)
cvtarr[2,32]=""
cvtarr[3,32]="%E"
cvtarr[4,32]=0
cvtarr[1,33]=chr(32)
cvtarr[2,33]=" "
cvtarr[3,33]=" "
cvtarr[4,33]=38
cvtarr[1,34]="!"
cvtarr[2,34]=""
cvtarr[3,34]="/A"
cvtarr[4,34]=0
cvtarr[1,35]=chr(34)
cvtarr[2,35]=""
cvtarr[3,35]="/B"
cvtarr[4,35]=0
cvtarr[1,36]="#"
cvtarr[2,36]=""
cvtarr[3,36]="/C"
cvtarr[4,36]=0
cvtarr[1,37]="$"
cvtarr[2,37]="$"
cvtarr[3,37]="/D"
cvtarr[4,37]=39
cvtarr[1,38]="%"
cvtarr[2,38]="%"
cvtarr[3,38]="/E"
cvtarr[4,38]=42
cvtarr[1,39]="&"
cvtarr[2,39]=""
cvtarr[3,39]="/F"
cvtarr[4,39]=0
cvtarr[1,40]=chr(39)
cvtarr[2,40]=""
cvtarr[3,40]="/G"
cvtarr[4,40]=0
cvtarr[1,41]="("
cvtarr[2,41]=""
cvtarr[3,41]="/H"
cvtarr[4,41]=0
cvtarr[1,42]=")"
cvtarr[2,42]=""
cvtarr[3,42]="/I"
cvtarr[4,42]=0
cvtarr[1,43]="*"
cvtarr[2,43]=""
cvtarr[3,43]="/J"
cvtarr[4,43]=0
cvtarr[1,44]="+"
cvtarr[2,44]="+"
cvtarr[3,44]="/K"
cvtarr[4,44]=41
cvtarr[1,45]=","
cvtarr[2,45]=""
cvtarr[3,45]="/L"
cvtarr[4,45]=0
cvtarr[1,46]="-"
cvtarr[2,46]="-"
cvtarr[3,46]="-"
cvtarr[4,46]=36
cvtarr[1,47]="."
cvtarr[2,47]="."
cvtarr[3,47]="."
cvtarr[4,47]=37
cvtarr[1,48]="/"
cvtarr[2,48]="/"
cvtarr[3,48]="/O"
cvtarr[4,48]=40
cvtarr[1,49]="0"
cvtarr[2,49]="0"
cvtarr[3,49]="0"
cvtarr[4,49]=0
cvtarr[1,50]="1"
cvtarr[2,50]="1"
cvtarr[3,50]="1"
cvtarr[4,50]=1
cvtarr[1,51]="2"
cvtarr[2,51]="2"
cvtarr[3,51]="2"
cvtarr[4,51]=2
cvtarr[1,52]="3"
cvtarr[2,52]="3"
cvtarr[3,52]="3"
cvtarr[4,52]=3
cvtarr[1,53]="4"
cvtarr[2,53]="4"
cvtarr[3,53]="4"
cvtarr[4,53]=4
cvtarr[1,54]="5"
cvtarr[2,54]="5"
cvtarr[3,54]="5"
cvtarr[4,54]=5
cvtarr[1,55]="6"
cvtarr[2,55]="6"
cvtarr[3,55]="6"
cvtarr[4,55]=6
cvtarr[1,56]="7"
cvtarr[2,56]="7"
cvtarr[3,56]="7"
cvtarr[4,56]=7
cvtarr[1,57]="8"
cvtarr[2,57]="8"
cvtarr[3,57]="8"
cvtarr[4,57]=8
cvtarr[1,58]="9"
cvtarr[2,58]="9"
cvtarr[3,58]="9"
cvtarr[4,58]=9
cvtarr[1,59]=":"
cvtarr[2,59]=""
cvtarr[3,59]="/Z"
cvtarr[4,59]=0
cvtarr[1,60]=";"
cvtarr[2,60]=""
cvtarr[3,60]="%F"
cvtarr[4,60]=0
cvtarr[1,61]="<"
cvtarr[2,61]=""
cvtarr[3,61]="%G"
cvtarr[4,61]=0
cvtarr[1,62]="="
cvtarr[2,62]=""
cvtarr[3,62]="%H"
cvtarr[4,62]=0
cvtarr[1,63]=">"
cvtarr[2,63]=""
cvtarr[3,63]="%I"
cvtarr[4,63]=0
cvtarr[1,64]="?"
cvtarr[2,64]=""
cvtarr[3,64]="%J"
cvtarr[4,64]=0
cvtarr[1,65]="@"
cvtarr[2,65]=""
cvtarr[3,65]="%V"
cvtarr[4,65]=0
cvtarr[1,66]="A"
cvtarr[2,66]="A"
cvtarr[3,66]="A"
cvtarr[4,66]=10
cvtarr[1,67]="B"
cvtarr[2,67]="B"
cvtarr[3,67]="B"
cvtarr[4,67]=11
cvtarr[1,68]="C"
cvtarr[2,68]="C"
cvtarr[3,68]="C"
cvtarr[4,68]=12
cvtarr[1,69]="D"
cvtarr[2,69]="D"
cvtarr[3,69]="D"
cvtarr[4,69]=13
cvtarr[1,70]="E"
cvtarr[2,70]="E"
cvtarr[3,70]="E"
cvtarr[4,70]=14
cvtarr[1,71]="F"
cvtarr[2,71]="F"
cvtarr[3,71]="F"
cvtarr[4,71]=15
cvtarr[1,72]="G"
cvtarr[2,72]="G"
cvtarr[3,72]="G"
cvtarr[4,72]=16
cvtarr[1,73]="H"
cvtarr[2,73]="H"
cvtarr[3,73]="H"
cvtarr[4,73]=17
cvtarr[1,74]="I"
cvtarr[2,74]="I"
cvtarr[3,74]="I"
cvtarr[4,74]=18
cvtarr[1,75]="J"
cvtarr[2,75]="J"
cvtarr[3,75]="J"
cvtarr[4,75]=19
cvtarr[1,76]="K"
cvtarr[2,76]="K"
cvtarr[3,76]="K"
cvtarr[4,76]=20
cvtarr[1,77]="L"
cvtarr[2,77]="L"
cvtarr[3,77]="L"
cvtarr[4,77]=21
cvtarr[1,78]="M"
cvtarr[2,78]="M"
cvtarr[3,78]="M"
cvtarr[4,78]=22
cvtarr[1,79]="N"
cvtarr[2,79]="N"
cvtarr[3,79]="N"
cvtarr[4,79]=23
cvtarr[1,80]="O"
cvtarr[2,80]="O"
cvtarr[3,80]="O"
cvtarr[4,80]=24
cvtarr[1,81]="P"
cvtarr[2,81]="P"
cvtarr[3,81]="P"
cvtarr[4,81]=25
cvtarr[1,82]="Q"
cvtarr[2,82]="Q"
cvtarr[3,82]="Q"
cvtarr[4,82]=26
cvtarr[1,83]="R"
cvtarr[2,83]="R"
cvtarr[3,83]="R"
cvtarr[4,83]=27
cvtarr[1,84]="S"
cvtarr[2,84]="S"
cvtarr[3,84]="S"
cvtarr[4,84]=28
cvtarr[1,85]="T"
cvtarr[2,85]="T"
cvtarr[3,85]="T"
cvtarr[4,85]=29
cvtarr[1,86]="U"
cvtarr[2,86]="U"
cvtarr[3,86]="U"
cvtarr[4,86]=30
cvtarr[1,87]="V"
cvtarr[2,87]="V"
cvtarr[3,87]="V"
cvtarr[4,87]=31
cvtarr[1,88]="W"
cvtarr[2,88]="W"
cvtarr[3,88]="W"
cvtarr[4,88]=32
cvtarr[1,89]="X"
cvtarr[2,89]="X"
cvtarr[3,89]="X"
cvtarr[4,89]=33
cvtarr[1,90]="Y"
cvtarr[2,90]="Y"
cvtarr[3,90]="Y"
cvtarr[4,90]=34
cvtarr[1,91]="Z"
cvtarr[2,91]="Z"
cvtarr[3,91]="Z"
cvtarr[4,91]=35
cvtarr[1,92]="["
cvtarr[2,92]=""
cvtarr[3,92]="%K"
cvtarr[4,92]=0
cvtarr[1,93]="\"
cvtarr[2,93]=""
cvtarr[3,93]="%L"
cvtarr[4,93]=0
cvtarr[1,94]="]"
cvtarr[2,94]=""
cvtarr[3,94]="%M"
cvtarr[4,94]=0
cvtarr[1,95]="^"
cvtarr[2,95]=""
cvtarr[3,95]="%N"
cvtarr[4,95]=0
cvtarr[1,96]="_"
cvtarr[2,96]=""
cvtarr[3,96]="%O"
cvtarr[4,96]=0
cvtarr[1,97]=chr(96)
cvtarr[2,97]=""
cvtarr[3,97]="%W"
cvtarr[4,97]=0
cvtarr[1,98]="a"
cvtarr[2,98]=""
cvtarr[3,98]="+A"
cvtarr[4,98]=0
cvtarr[1,99]="b"
cvtarr[2,99]=""
cvtarr[3,99]="+B"
cvtarr[4,99]=0
cvtarr[1,100]="c"
cvtarr[2,100]=""
cvtarr[3,100]="+C"
cvtarr[4,100]=0
cvtarr[1,101]="d"
cvtarr[2,101]=""
cvtarr[3,101]="+D"
cvtarr[4,101]=0
cvtarr[1,102]="e"
cvtarr[2,102]=""
cvtarr[3,102]="+E"
cvtarr[4,102]=0
cvtarr[1,103]="f"
cvtarr[2,103]=""
cvtarr[3,103]="+F"
cvtarr[4,103]=0
cvtarr[1,104]="g"
cvtarr[2,104]=""
cvtarr[3,104]="+G"
cvtarr[4,104]=0
cvtarr[1,105]="h"
cvtarr[2,105]=""
cvtarr[3,105]="+H"
cvtarr[4,105]=0
cvtarr[1,106]="i"
cvtarr[2,106]=""
cvtarr[3,106]="+I"
cvtarr[4,106]=0
cvtarr[1,107]="j"
cvtarr[2,107]=""
cvtarr[3,107]="+J"
cvtarr[4,107]=0
cvtarr[1,108]="k"
cvtarr[2,108]=""
cvtarr[3,108]="+K"
cvtarr[4,108]=0
cvtarr[1,109]="l"
cvtarr[2,109]=""
cvtarr[3,109]="+L"
cvtarr[4,109]=0
cvtarr[1,110]="m"
cvtarr[2,110]=""
cvtarr[3,110]="+M"
cvtarr[4,110]=0
cvtarr[1,111]="n"
cvtarr[2,111]=""
cvtarr[3,111]="+N"
cvtarr[4,111]=0
cvtarr[1,112]="o"
cvtarr[2,112]=""
cvtarr[3,112]="+O"
cvtarr[4,112]=0
cvtarr[1,113]="p"
cvtarr[2,113]=""
cvtarr[3,113]="+P"
cvtarr[4,113]=0
cvtarr[1,114]="q"
cvtarr[2,114]=""
cvtarr[3,114]="+Q"
cvtarr[4,114]=0
cvtarr[1,115]="r"
cvtarr[2,115]=""
cvtarr[3,115]="+R"
cvtarr[4,115]=0
cvtarr[1,116]="s"
cvtarr[2,116]=""
cvtarr[3,116]="+S"
cvtarr[4,116]=0
cvtarr[1,117]="t"
cvtarr[2,117]=""
cvtarr[3,117]="+T"
cvtarr[4,117]=0
cvtarr[1,118]="u"
cvtarr[2,118]=""
cvtarr[3,118]="+U"
cvtarr[4,118]=0
cvtarr[1,119]="v"
cvtarr[2,119]=""
cvtarr[3,119]="+V"
cvtarr[4,119]=0
cvtarr[1,120]="w"
cvtarr[2,120]=""
cvtarr[3,120]="+W"
cvtarr[4,120]=0
cvtarr[1,121]="x"
cvtarr[2,121]=""
cvtarr[3,121]="+X"
cvtarr[4,121]=0
cvtarr[1,122]="y"
cvtarr[2,122]=""
cvtarr[3,122]="+Y"
cvtarr[4,122]=0
cvtarr[1,123]="z"
cvtarr[2,123]=""
cvtarr[3,123]="+Z"
cvtarr[4,123]=0
cvtarr[1,124]="{"
cvtarr[2,124]=""
cvtarr[3,124]="%P"
cvtarr[4,124]=0
cvtarr[1,125]="|"
cvtarr[2,125]=""
cvtarr[3,125]="%Q"
cvtarr[4,125]=0
cvtarr[1,126]="}"
cvtarr[2,126]=""
cvtarr[3,126]="%R"
cvtarr[4,126]=0
cvtarr[1,127]="~"
cvtarr[2,127]=""
cvtarr[3,127]="%S"
cvtarr[4,127]=0
cvtarr[1,128]=chr(127)
cvtarr[2,128]=""
cvtarr[3,128]="%T"
cvtarr[4,128]=0

chktot = 0
*** process the message
FOR x = 1 TO len(m)
  t = substr(m, x, 1)
  rtnc = asubscript(cvtarr,ascan(cvtarr,t,aelement(cvtarr,1,1),128),2)
  if rtnc <> 0
    p = p + cvtarr[cval,rtnc]
    chktot = chktot + cvtarr[4,rtnc]
  endif
endfor

if check = 1
  chkstr = 43 - (chktot % 43)
  rtnc = asubscript(cvtarr,ascan(cvtarr,chkstr,aelement(cvtarr,4,1),128),2)
  if rtnc <> 0
    p = p + cvtarr[1,rtnc]
  endif
endif

rtnp = ALLT(p) + "*"

*** Return the bit pattern
lcRet = rtnp
*--- END1



*!*************************************************************
*! Name      : lfWOGrid
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 29/10/1998
*! Purpose   : When option grid function.
*!*************************************************************
*! Calls     : From Option Grid.
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfWOGrid()
*!*************************************************************
FUNCTION lfWOGrid

SELECT POSHDR
SET FILTER TO cStyType = 'A'
SET RELATION TO
SET RELATION TO vendor INTO Warehous ADDITIVE
GOTO TOP


*!*************************************************************
*! Name      : lfClearRep
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 24/07/1999
*! Purpose   : Function to Clear temp file.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClearRep()
*!*************************************************************
FUNCTION lfClearRep

*--- Global variable to indicate if the selection criteria has been changed or not.
llOgFltCh = .T.
*---Erase the temp file
USE IN IIF(USED(lcRpOrdTmp),lcRpOrdTmp,0)
ERASE &gcWorkDir.&lcRpOrdTmp+'.DBF'
ERASE &gcWorkDir.&lcRpOrdTmp+'.CDX'
ERASE &gcWorkDir.&lcRpOrdTmp+'1'+'.CDX'
*---lcRpTmp
USE IN IIF(USED(lcRpTmp),lcRpTmp,0)
ERASE &gcWorkDir.&lcRpTmp+'.DBF'
ERASE &gcWorkDir.&lcRpTmp+'.CDX'
*---lcRpCvrSh
USE IN IIF(USED(lcRpCvrSh),lcRpCvrSh,0)
ERASE &gcWorkDir.&lcRpCvrSh+'.DBF'
ERASE &gcWorkDir.&lcRpCvrSh+'.CDX'

*---lcTmpRank
USE IN IIF(USED(lcTmpRank),lcTmpRank,0)
ERASE &gcWorkDir.&lcTmpRank+'.DBF'
ERASE &gcWorkDir.&lcTmpRank+'.CDX'

*!*************************************************************
*! Name      : lfvRep
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 24/07/1999
*! Purpose   : Valid function for report name popup.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRep()
*!*************************************************************
FUNCTION lfvRep

DO CASE
  CASE lcRPReport $ 'AC'
    lcRpForm  = 'MFADSPCH'
  CASE lcRPReport = 'S'
    lcRpForm  = 'MFADSPSH'
  CASE lcRPReport = 'N'
    lcRpForm  = 'MFADSPNH'
  CASE lcRPReport = 'H'
    lcRpForm  = 'MFADSPHH'
ENDCASE

*!*************************************************************
*! Name      : lfClr
*! Developer : AHMED SALAH SHALABY - (SSH)
*! Date      : 24/07/1999
*! Purpose   : Function to clear read.
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClr()
*!*************************************************************
FUNCTION lfClr
CLEAR READ


*!*************************************************************
*! Name      : lfvAdOrd
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 17/10/1999
*! Purpose   : Valid function for po#
*!*************************************************************
*! Called from : ALPKTKTA.FRX
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : ''
*!*************************************************************
*
FUNCTION lfvAdOrd

lcPOFld = VARREAD()
lcPONo = &lcPOFld
IF !EMPTY(lcPONo) .AND. ('?' $ lcPONo .OR. !SEEK('A'+lcPONo , 'POSHDR'))
  DO POSBrow WITH lcPONo,"",'A'
ENDIF
&lcPOFld = lcPONo

IF EMPTY(lcPONo)
  WAIT WINDOW "Adornment Order Can not be empty..!" NOWAIT
ELSE
  USE IN IIF(USED(lcRpBrTmp),lcRpBrTmp,0)
  SELECT ORDLINE
  COPY STRUCTURE TO &gcWorkDir.&lcRpBrTmp
  SELECT 0
  USE (gcWorkDir+lcRpBrTmp) EXCLUSIVE
  INDEX ON PIKTKT TAG (lcRpBrTmp)
  ZAP

  WAIT WINDOW 'Selecting pick tickets.' NOWAIT
  SELECT cutpick
  SET ORDER TO Cutpick
  GOTO TOP
  =SEEK('2'+lcRpAdOrder )
  SCAN REST WHILE trancd+ctktno+style = '2'+lcRpAdOrder 
    lcOrder = ORDER
    lcLine  = cOrdLine
    SELECT ORDLINE
    SET ORDER TO Ordline
    =SEEK('O'+lcOrder+PADR(lcLine,6))
    SCAN REST WHILE cordtype+order+STR(lineno,6) = 'O'+lcOrder+PADR(lcLine,6);
              FOR PIkTkt <> '******' AND SEEK(PIkTkt,'PikTkt') AND PIKTKT.Status<>'X' AND ;
                  IIF(lcRPTkt = 'N',PikTkt.PrtFlag <> 'P',.T.)
      SCATTER MEMVAR MEMO
      IF !SEEK(OrdLine.PikTkt,lcRpBrTmp)
         INSERT INTO (lcRpBrTmp) FROM MEMVAR
      ELSE
        SELECT (lcRpBrTmp)
        REPLACE TotQty WITH TotQty + m.TotQty,;
                TotPik WITH TotPik + m.TotPik
      ENDIF   
    ENDSCAN
  ENDSCAN
  
  *B603360,1 KHM 12/23/99 (Begin) Adding the following code to get the
  *B603360,1 KHM 12/23/99 completed piktkts.
  IF SEEK('A'+lcRpAdOrder,'PosLn')
    SELECT PosLn
    SCAN REST WHILE cStyType+Po+Style+STR(LineNo,6)+TranCd = 'A'+lcRpAdOrder
      =SEEK(ShipNo,'PikTkt')
      IF PIKTKT.Status = 'C' AND SEEK(PikTkt.PikTkt,'PikLine')
        STORE 0 TO lnTotQty,lnTotPik
        SELECT PikLine        
        SCAN REST WHILE PikTkt+Order+STR(LineNo,6) = PosLn.ShipNo
          lnTotQty = lnTotQty + TotQty
          lnTotPik = lnTotPik + TotPik
        ENDSCAN
        IF !SEEK(PosLn.ShipNo,lcRpBrTmp)
         INSERT INTO (lcRpBrTmp) (PikTkt,Order,Account,Store,TotQty,TotPik,;
                     PikDate);
                     VALUES (PikTkt.PikTkt,PikTkt.Order,PikTkt.Account,;
                             PikTkt.Store,lnTotQty,lnTotPik,PikTkt.Date)
                             
        ENDIF
      ENDIF  
    ENDSCAN    
  ENDIF
  WAIT CLEAR
  *B603360,1 KHM 12/23/99 (End)
  
  SELECT (lcRpBrTmp)
  INDEX ON PikTkt TAG (lcRpBrTmp)
ENDIF
CLEAR READ
