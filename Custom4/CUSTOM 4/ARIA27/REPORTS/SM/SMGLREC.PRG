*:E301248   AHM 06/03/1999 Adding new Tran. type "I" in styinvjl file
*:E301248                  for issunig style from PO/CT cost sheet
*:B802357   AHM 06/27/1999 Adding POP UP for all companies in option grid
*:B603156   AHM 09/13/1999 fix the bug of wrong sigen of Discount amount 
*:B603680   WMA 06/11/2000 shows all foreign currency crdit memos as 'out of balance' 
*:B603680                  for cat key 020.
*:B603680                  The Tran Amount must be in Base currency For.
*:B603983,1 SSE 10/23/2000 Fix bug of adding new Tran. type 'LK' Inventory Locking type 
*:B605445,1 ABD 01/29/2002 Fix Bug Alias in use in the new release.
*:B038431,1 NNA 09/07/2004 Fix bug that if the 'RM' or "PS' not installed we'll get an error massage
*:B038431,1 NNA            'File ....\Rethdr.dbf Doesn't Exist' 
*:B608940,1 AHS 07/15/2009 G/L Entry Reconciliation includes Inventory entries even when system flag is set to No      
**********************************************************************

IF EMPTY(gcAct_Comp) AND EMPTY(lcDataDir)
  *-- You have to select a company first.
  *-- <OK>
  = gfModalGen("INM00192B00000","Dialog") = 1
  RETURN
ENDIF
  
STORE '' TO M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO

*:B802357 (Start)
*=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',gcAct_Comp)
=gfGetMemVar('M_POST_PPRD,M_SYS_DIR,M_GL_VERS,M_GL_CO',lcRPSelCom)
*:B802357 (End)

lcSysDir   = ALLTRIM(M_SYS_DIR)
lcGlVers   = ALLTRIM(M_GL_VERS)
lcGlComp   = ALLTRIM(M_GL_CO)

IF lcGlVers = 'S'            &&   <<<... SBT 2.5 ... >>>
  =gfOpenFile(lcSysDir+'SYCCOMP',lcSysDir+'COMPID','SH')
  =gfOpenFile(lcSysDir+'SYCHFIS',lcSysDir+'COMPID1','SH')
  =gfOpenFile(lcSysDir+'SYCDFIS',lcSysDir+'COMPID1','SH')
ELSE
  =gfOpenFile(gcSysHome+'SYCCOMP',gcSysHome+'CCOMP_ID','SH')

  *B802357(Start)
  *=gfOpenFile(gcDataDir+'FISHD',gcDataDir+'COMPFYEAR','SH')
  *=gfOpenFile(gcDataDir+'FSPRD',gcDataDir+'COMFYRPRDI','SH')
  *B605445,1 ABD - Fix Bug Alias in use. [Begin]
  IF USED('FISHD')
    USE IN FISHD
  ENDIF
  IF USED('FSPRD')
    USE IN FSPRD
  ENDIF
  *B605445,1 ABD - [End]
  
  =gfOpenFile(lcDataDir+'FISHD','COMPFYEAR','SH')
  
  =gfOpenFile(lcDataDir+'FSPRD','COMFYRPRDI','SH')
  *B802357(End)

ENDIF

********************************
lcFltExp = STRTRAN(lcRpExp,'GLDIST.TRAN_DATE','DPOSTDATE')

lcTime     =  gfGetTime()

STORE .F. TO llGlDist,llInvHdr,llInvLine,llRetHdr,llRetLine,llOpenChrg,;
             llDebit,llCredit,llArHist,llStyInvJl,llMatInvJl

lcTarTrans = ''
IF !EMPTY(laRPTranTa)
  FOR lnI = 1 TO ALEN(laRPTranTa)
    lcTranType = laTran[ASUBSCRIPT(laTran,ASCAN(laTran,laRPTranTa[lnI]),1),2]
    lcTarTrans = lcTarTrans + "," + lcTranType
  ENDFOR
ENDIF

IF !USED('gldist')
  *B802357(Start)
  *USE (gcDataDir+'gldist.dbf') IN 0 SHARED
  USE (lcDataDir+'gldist.dbf') IN 0 SHARED
  *B802357(End)
  llGlDist = .T.
ENDIF  

lcGenDist = gfTempName()
lcHistory = gfTempName()

DO lpCreatTmp

WAIT "Collecting data..." WINDOW NOWAIT

*!********************************************************************
*-- Transaction : IA,IP,IN,VI,PO,CT,RM,VR                      (Start)
*!********************************************************************

*B603983,1 Check for Type 'LK' Inventory Locking in selected Type [Begin]
*IF EMPTY(lcTarTrans) OR "IP" $ lcTarTrans OR "IA" $ lcTarTrans OR ;
                        "PO" $ lcTarTrans OR "CT" $ lcTarTrans OR ;
                        "IN" $ lcTarTrans OR "VI" $ lcTarTrans OR ;
                        "RM" $ lcTarTrans OR "VR" $ lcTarTrans
IF EMPTY(lcTarTrans) OR "IP" $ lcTarTrans OR "IA" $ lcTarTrans OR ;
                        "PO" $ lcTarTrans OR "CT" $ lcTarTrans OR ;
                        "IN" $ lcTarTrans OR "VI" $ lcTarTrans OR ;
                        "RM" $ lcTarTrans OR "VR" $ lcTarTrans OR ;
                        "LK" $ lcTarTrans
*B603983,1 Check for Type 'LK' Inventory Locking in selected Type [End]

  lcTrType = ''
  IF EMPTY(lcTarTrans)
    lcTrType = ''
  ELSE
    IF "IA" $ lcTarTrans
      lcTrType = lcTrType + "1"
    ENDIF
    IF "IP" $ lcTarTrans
      lcTrType = lcTrType + "2"
    ENDIF
    IF "IN" $ lcTarTrans
      lcTrType = lcTrType + "3"
    ENDIF
    IF "VI" $ lcTarTrans
      lcTrType = lcTrType + "4"
    ENDIF
    IF "CT" $ lcTarTrans
      lcTrType = lcTrType + "5"
    ENDIF
    IF "PO" $ lcTarTrans
      lcTrType = lcTrType + "6"
    ENDIF
    IF "RM" $ lcTarTrans
      lcTrType = lcTrType + "7"
    ENDIF
    IF "VR" $ lcTarTrans
      lcTrType = lcTrType + "8"
    ENDIF

    *B603983,1 Add a new type for Inventory Locking [Begin]
    IF "LK" $ lcTarTrans
      lcTrType = lcTrType + "9"
    ENDIF
    *B603983,1 Add a new type for Inventory Locking [End]
  ENDIF

  lcScanExp = IIF(EMPTY(lcTrType),'.T.',[StyInvJl.cTrType $ lcTrType])
  
  IF !USED('StyInvJl')
    *B802357(Start)
    *USE (gcDataDir+'StyInvJl.dbf')   IN 0 SHARED
    USE (lcDataDir+'StyInvJl.dbf')   IN 0 SHARED
    *B802357(End)
    llStyInvJl = .T.
  ENDIF  
  
  lcStyJorFl = STRTRAN(lcFltExp,"DPOSTDATE","DTRDATE")
  
  SELECT StyInvJl
  SCAN FOR &lcScanExp AND &lcStyJorFl
    WAIT "Style Journal file ,Tran# " + cTrCode WINDOW NOWAIT
    STORE '' TO lcGlYear , lcGlPeriod
    = lfChkPrd(StyInvJl.dTrDate,'lcGlYear','lcGlPeriod')

    lcCurrCode = ""
    lnCurrUnit = 0
    lnExRate   = 0
 
    DO CASE
      *E301248 (Start)
      CASE StyInvJl.cTrType = "I"
        lcTranType = "IA"
        lcOthSide  = '013'
      *E301248 (End)
      CASE StyInvJl.cTrType = "1"
        lcTranType = "IA"
        lcOthSide  = '007'
      CASE StyInvJl.cTrType = "2"
        lcTranType = "IP"
        lcOthSide  = '007'
      CASE StyInvJl.cTrType = "3"
        lcTranType = "IN"
        lcOthSide  = '008'
      CASE StyInvJl.cTrType = "4"
        lcTranType = "VI"
        lcOthSide  = '008'
      CASE StyInvJl.cTrType = "5"
        lcTranType = "CT"
        lcOthSide  = '013'
      CASE StyInvJl.cTrType = "6"
        lcTranType = "PO"
        lcOthSide  = '013'
      CASE StyInvJl.cTrType = "7"
        lcTranType = "RM"
        lcOthSide  = '008'
      CASE StyInvJl.cTrType = "8"
        lcTranType = "VR"
        lcOthSide  = '008'
      
      *B603983,1 Checking for Locking type [Begin]
      CASE StyInvJl.cTrType = "9"
        lcTranType = "LK"
        lcOthSide  = '007'
      *B603983,1 Checking for Locking type [End]

    ENDCASE

    *--Finished good Inventory

    DO lpGLDist WITH .T.,'New','006',lcTranType,StyInvJl.cTrCode,;
                     StyInvJl.nStkVal,StyInvJl.dTrDate,;
                     StyInvJl.cIcAcnt,lcGlPeriod,lcGlYear,;
                     lcCurrCode,lnCurrUnit,lnExRate   
    
    *--The other side
    DO lpGLDist WITH .T.,'New',lcOthSide,lcTranType,StyInvJl.cTrCode,;
                     -StyInvJl.nStkVal,StyInvJl.dTrDate,;
                     StyInvJl.cAdjAcct,lcGlPeriod,lcGlYear,;
                     lcCurrCode,lnCurrUnit,lnExRate
  ENDSCAN
  IF llStyInvJl
    USE IN StyInvJl
  ENDIF  
ENDIF
*!********************************************************************
*-- Transaction : IA,IP,IN,VI,PO,CT,RM,VR                        (End)
*!********************************************************************

*!**********************************
*-- Transaction : IN,VI      (Start)
*!**********************************
IF EMPTY(lcTarTrans) OR "IN" $ lcTarTrans OR "VI" $ lcTarTrans
  IF UPPER(ALLTRIM(gcContCode))='ENG'
    IF !USED('InvChrg')

      *B802357(Start)
      *USE (gcDataDir+'InvChrg.DBF') IN 0 SHARED
      USE (lcDataDir+'InvChrg.DBF') IN 0 SHARED
      *B802357(End)

      llOpenChrg = .T.
      SELECT InvChrg
      SET ORDER TO InvChrg
    ELSE
      lcChrgTag = ORDER()
    ENDIF
  ENDIF
  IF !USED('InvHdr')
  
    *B802357(Start)
    *USE (gcDataDir+'InvHdr.dbf')   IN 0 SHARED
    USE (lcDataDir+'InvHdr.dbf')   IN 0 SHARED
    *B802357(End)
    
    llInvHdr = .T.
  ENDIF  
  IF !USED('InvLine')
    
    *B802357(Start)
    *USE (gcDataDir+'InvLine.dbf')  IN 0 SHARED
    USE (lcDataDir+'InvLine.dbf')  IN 0 SHARED
    *B802357(End)
    
    llInvLine = .T.
  ENDIF  

  SELECT InvHdr
  SET ORDER TO InvHdr
  SELECT InvLine
  SET ORDER TO InvLine

  SELECT InvHdr
  SET RELATION TO Invoice INTO InvLine  ADDITIVE
  

  lcINFulFlt = ''
  lcINFltr   = STRTRAN(lcFltExp,'DPOSTDATE','INVHDR.DPOSTDATE')
  lcVIFltr   = STRTRAN(lcFltExp,'DPOSTDATE','INVHDR.VDATE')

  IF EMPTY(lcFltExp) OR lcFltExp = '.T.'
    lcINFltr   = '.T.'
    lcVIFltr   = '.T.'
    lcINFulFlt = '.T.'
  ELSE
    IF "IN" $ lcTarTrans
      lcINFulFlt = lcINFltr
    ENDIF
    IF  "VI" $ lcTarTrans
      lcINFulFlt = lcINFulFlt + IIF(EMPTY(lcINFulFlt),'','OR ') + lcVIFltr
    ENDIF
    IF EMPTY(lcINFulFlt)
      lcINFulFlt = '.T.'
    ENDIF
  ENDIF

  SCAN FOR &lcINFulFlt
    WAIT IIF(InvHdr.Status='V',"Void invoice# ","invoice# ") + InvHdr.Invoice WINDOW NOWAIT
    lcTotChgFl = "InvHdr."  + IIF(InvHdr.Status='V','V','') + "TotalChg"
    lcCodFl    = "InvHdr."  + IIF(InvHdr.Status='V','V','') + "Cod"
    lcFrgtFl   = "InvHdr."  + IIF(InvHdr.Status='V','V','') + "Freight"
    lcInsuFl   = "InvHdr."  + IIF(InvHdr.Status='V','V','') + "Insur"
    lcTaxAmtFl = "InvHdr."  + IIF(InvHdr.Status='V','V','') + "Tax_Amt"
    lcDiscFl   = "InvHdr."  + IIF(InvHdr.Status='V','V','') + "Discount"
    lcShpAmtFl = "InvHdr."  + IIF(InvHdr.Status='V','V','') + "ShipAmt"

    STORE '' TO lcGlYear , lcGlPeriod

    = lfChkPrd(InvHdr.dPostDate,'lcGlYear','lcGlPeriod')

    lcCurrCode = InvHdr.cCurrCode
    lnCurrUnit = InvHdr.nCurrUnit
    lnExRate   = InvHdr.nExRate
       
    *-- Credit Freight
    IF UPPER(ALLTRIM(gcContCode))='ENG' AND SEEK(InvHdr.Invoice,'InvChrg')
      SELECT InvChrg
      SCAN REST WHILE Invoice+cStore+cChrgcode = InvHdr.Invoice
        IF (EMPTY(lcTarTrans) OR "IN" $ lcTarTrans) AND &lcINFltr
          DO lpGLDist WITH .F.,'New','004','IN',InvHdr.Invoice,;
                           -(InvChrg.nChrgAmnt),InvHdr.dPostDate,;
                           InvHdr.cFrgtAcnt,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
        ENDIF
        IF (EMPTY(lcTarTrans) OR "VI" $ lcTarTrans) AND InvHdr.Status='V' AND &lcVIFltr
          DO lpGLDist WITH .F.,'New','004','VI',InvHdr.Invoice,;
                           (InvChrg.nChrgAmnt),InvHdr.dPostDate,;
                           InvHdr.cFrgtAcnt,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
        ENDIF

      ENDSCAN
    ELSE
      IF (EMPTY(lcTarTrans) OR "IN" $ lcTarTrans) AND &lcINFltr
        DO lpGLDist WITH .F.,'New','004','IN',InvHdr.Invoice,;
                         -(&lcCodFl.+&lcFrgtFl.+&lcInsuFl.),InvHdr.dPostDate,;
                         InvHdr.cFrgtAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
      ENDIF
      IF (EMPTY(lcTarTrans) OR "VI" $ lcTarTrans) AND InvHdr.Status='V' AND &lcVIFltr
        DO lpGLDist WITH .F.,'New','004','VI',InvHdr.Invoice,;
                         (&lcCodFl.+&lcFrgtFl.+&lcInsuFl.),InvHdr.dPostDate,;
                         InvHdr.cFrgtAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
      ENDIF

    ENDIF
  
    IF (EMPTY(lcTarTrans) OR "IN" $ lcTarTrans) AND &lcINFltr
      *-- Credit Sales tax
      DO lpGLDist WITH .F.,'New','014','IN',InvHdr.Invoice,;
                       -(&lcTaxAmtFl.),InvHdr.dPostDate,;
                       InvHdr.cTaxAcnt,lcGlPeriod,lcGlYear,;
                       lcCurrCode,lnCurrUnit,lnExRate
    ENDIF
    IF (EMPTY(lcTarTrans) OR "VI" $ lcTarTrans) AND InvHdr.Status='V' AND &lcVIFltr
      *-- Debit Sales tax
      DO lpGLDist WITH .F.,'New','014','VI',InvHdr.Invoice,;
                       (&lcTaxAmtFl.),InvHdr.dPostDate,;
                       InvHdr.cTaxAcnt,lcGlPeriod,lcGlYear,;
                       lcCurrCode,lnCurrUnit,lnExRate
    ENDIF

    SELECT InvLine
    SCAN REST WHILE InvLine.Invoice + STR(lineno,6) = InvHdr.Invoice
      IF (EMPTY(lcTarTrans) OR "IN" $ lcTarTrans) AND &lcINFltr
        *-- Credit Sales revenue
        DO lpGLDist WITH .T.,'New','003','IN',InvHdr.Invoice,;
                         -(InvLine.nEqvAmnt),InvHdr.dPostDate,;
                         InvLine.cSalesAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
      ENDIF
      IF (EMPTY(lcTarTrans) OR "VI" $ lcTarTrans) AND InvHdr.Status='V' AND &lcVIFltr
        *-- Debit Sales revenue
        DO lpGLDist WITH .T.,'New','003','VI',InvHdr.Invoice,;
                         (InvLine.nEqvAmnt),InvHdr.dPostDate,;
                         InvLine.cSalesAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
      ENDIF
      
      IF (EMPTY(lcTarTrans) OR "IN" $ lcTarTrans) AND &lcINFltr
        *-- Debit Discount
        *B603156 (Start)
        *--Discount field is put in InvHdr file with Negative sigen
        *DO lpGLDist WITH .T.,'New','005','IN',InvHdr.Invoice,;
                         InvLine.nEqvAmnt*(&lcDiscFl./&lcShpAmtFl.),InvHdr.dPostDate,;
                         InvLine.cDiscAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
        DO lpGLDist WITH .T.,'New','005','IN',InvHdr.Invoice,;
                         -InvLine.nEqvAmnt*(&lcDiscFl./&lcShpAmtFl.),InvHdr.dPostDate,;
                         InvLine.cDiscAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
      *B603156 (End)
      ENDIF
      IF (EMPTY(lcTarTrans) OR "VI" $ lcTarTrans) AND InvHdr.Status='V' AND &lcVIFltr
        *-- Credit Discount
        *B603156 (Start)
        *DO lpGLDist WITH .T.,'New','005','VI',InvHdr.Invoice,;
                         -InvLine.nEqvAmnt*(&lcDiscFl./&lcShpAmtFl.),InvHdr.dPostDate,;
                         InvLine.cDiscAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
        DO lpGLDist WITH .T.,'New','005','VI',InvHdr.Invoice,;
                         InvLine.nEqvAmnt*(&lcDiscFl./&lcShpAmtFl.),InvHdr.dPostDate,;
                         InvLine.cDiscAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
        *B603156 (End)
      ENDIF

    ENDSCAN
  ENDSCAN
  
  SELECT InvHdr
  SET RELATION OFF INTO InvLine
  
  IF USED('InvChrg')
    IF llOpenChrg
      USE IN InvChrg
    ELSE 
      SET ORDER TO lcChrgTag IN InvChrg
    ENDIF
  ENDIF
  IF llInvHdr
    USE IN InvHdr
  ENDIF  
  IF llInvLine
    USE IN InvLine
  ENDIF  
  
ENDIF
*!**********************************
*-- Transaction : IN,VI (End)
*!**********************************

*!**********************************
*-- Transaction : RM,VR (Start)
*!**********************************

*B038431,1 NNA 09/07/2004 (Begin) Check if the RM or PS module is installed to open the Rethdr File
IF 'RM' $ gcCmpModules .OR. 'PS' $ gcCmpModules
*B038431,1 NNA (End)

  IF EMPTY(lcTarTrans) OR "RM" $ lcTarTrans OR "VR" $ lcTarTrans
    IF !USED('RetHdr')
    
      *B802357(Start)
      *USE (gcDataDir+'RetHdr.dbf')   IN 0 SHARED
      USE (lcDataDir+'RetHdr.dbf')   IN 0 SHARED
      *B802357(End)
    
      llRetHdr = .T.
    ENDIF  
    IF !USED('RetLine')
  
      *B802357(Start)
      *USE (gcDataDir+'RetLine.dbf')  IN 0 SHARED
      USE (lcDataDir+'RetLine.dbf')  IN 0 SHARED
      *B802357(End)
    
      llRetLine = .T.
    ENDIF  

    SELECT RetHdr
    SET ORDER TO RetHdr
    SELECT RetLine
    SET ORDER TO RetLine

    SELECT RetHdr
    SET RELATION TO CRMemo INTO RetLine ADDITIVE
    SCAN FOR &lcFltExp
      STORE '' TO lcGlYear , lcGlPeriod

      = lfChkPrd(RetHdr.dPostDate,'lcGlYear','lcGlPeriod')
      lcCurrCode = RetHdr.cCurrCode
      lnCurrUnit = RetHdr.nCurrUnit
      lnExRate   = RetHdr.nExRate
   
      IF (EMPTY(lcTarTrans) OR "RM" $ lcTarTrans)
        WAIT "Credit memo# " + RetHdr.CRMemo WINDOW NOWAIT
        *-- Debit Freight
        DO lpGLDist WITH .F.,'New','004','RM',RetHdr.CRMemo,;
                         IIF(RetHdr.Status='V',RetHdr.VOther,RetHdr.Other),RetHdr.dPostDate,;
                         RetHdr.cFrgtAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
        *-- Debit Sales tax
        DO lpGLDist WITH .F.,'New','014','RM',RetHdr.CRMemo,;
                         RetHdr.Tax_Amt+nPstAmt,RetHdr.dPostDate,;
                         RetHdr.cTaxAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
      ENDIF
      IF (EMPTY(lcTarTrans) OR "VR" $ lcTarTrans) AND RetHdr.Status = 'V'
        WAIT "Void credit memo# " + RetHdr.CRMemo WINDOW NOWAIT
        *-- Credit Freight
        DO lpGLDist WITH .F.,'New','004','VR',RetHdr.CRMemo,;
                         -RetHdr.VOther,RetHdr.dPostDate,;
                         RetHdr.cFrgtAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
        *-- Credit Sales tax
        DO lpGLDist WITH .F.,'New','014','VR',RetHdr.CRMemo,;
                         -(RetHdr.Tax_Amt+nPstAmt),RetHdr.dPostDate,;
                         RetHdr.cTaxAcnt,lcGlPeriod,lcGlYear,;
                         lcCurrCode,lnCurrUnit,lnExRate
      ENDIF
  
      SELECT RetLine
      SCAN REST WHILE crmemo+style+cret_linno+cret_trncd = RetHdr.CRMemo
        IF (EMPTY(lcTarTrans) OR "RM" $ lcTarTrans)
          WAIT "Credit memo# " + RetHdr.CRMemo WINDOW NOWAIT
          *-- Debit Return merchandise
         
          *B603680 WMA [Begin]
          *DO lpGLDist WITH .T.,'New','020','RM',RetHdr.CRMemo,;
          *                 RetLine.Gros_Price*RetLine.TotQty,RetHdr.dPostDate,;
          *                 RetLine.cSalesAcnt,lcGlPeriod,lcGlYear,;
          *                 lcCurrCode,lnCurrUnit,lnExRate
          DO lpGLDist WITH .F.,'New','020','RM',RetHdr.CRMemo,;
                           RetLine.Gros_Price*RetLine.TotQty,RetHdr.dPostDate,;
                           RetLine.cSalesAcnt,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
          *B603680 WMA [End]
          *-- Credit Discount
          WAIT "Credit memo# " + RetHdr.CRMemo WINDOW NOWAIT
          DO lpGLDist WITH .T.,'New','005','RM',RetHdr.CRMemo,;
                           -RetLine.Disc_Amt,RetHdr.dPostDate,;
                           RetLine.cDiscAcnt,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
        ENDIF
        
        IF (EMPTY(lcTarTrans) OR "VR" $ lcTarTrans) AND RetHdr.Status = 'V'
          *-- Credit Return merchandise
          WAIT "Void credit memo# " + RetHdr.CRMemo WINDOW NOWAIT
          *B603680 WMA [Begin]
          *DO lpGLDist WITH .T.,'New','020','VR',RetHdr.CRMemo,;
          *                 -RetLine.Gros_Price*RetLine.TotQty,RetHdr.dPostDate,;
          *                 RetLine.cSalesAcnt,lcGlPeriod,lcGlYear,;
          *                 lcCurrCode,lnCurrUnit,lnExRate
          DO lpGLDist WITH .F.,'New','020','VR',RetHdr.CRMemo,;
                           -RetLine.Gros_Price*RetLine.TotQty,RetHdr.dPostDate,;
                           RetLine.cSalesAcnt,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
          *B603680 WMA [End]
          *-- Debit Discount
          DO lpGLDist WITH .T.,'New','005','VR',RetHdr.CRMemo,;
                           RetLine.Disc_Amt,RetHdr.dPostDate,;
                           RetLine.cDiscAcnt,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
        ENDIF
      ENDSCAN
    ENDSCAN
  
    IF llRetHdr
      USE IN RetHdr
    ENDIF  
    IF llRetLine
      USE IN RetLine
    ENDIF  
  ENDIF

*B038431,1 NNA 09/07/2004 (Begin)
ENDIF
*B038431,1 NNA (End)
  
*!**********************************
*-- Transaction : RM,VR (End)
*!**********************************

*!**********************************
*-- Transaction : MO,MA,MP   (Start)
*!**********************************
IF EMPTY(lcTarTrans) OR "MP" $ lcTarTrans OR "MA" $ lcTarTrans OR ;
                        "MO" $ lcTarTrans

  IF EMPTY(lcTarTrans)
    lcScanExp = [INLIST(MatInvJl.cTranType,"1","2","3")]
  ELSE
    lcScanExp = [INLIST(MatInvJl.cTranType]
    IF "MO" $ lcTarTrans
      lcScanExp = lcScanExp + [,"1"]
    ENDIF
    IF "MA" $ lcTarTrans
      lcScanExp = lcScanExp + [,"2"]
    ENDIF
    IF "MO" $ lcTarTrans
      lcScanExp = lcScanExp + [,"3"]
    ENDIF
    lcScanExp = lcScanExp + [)]
  ENDIF

  IF !USED('MatInvJl')
  
    *B802357(Start)
    *USE (gcDataDir+'MatInvJl.dbf')   IN 0 SHARED
    USE (lcDataDir+'MatInvJl.dbf')   IN 0 SHARED
    *B802357(End)
    
    llMatInvJl = .T.
  ENDIF  
  
  lcMatJorFl = lcFltExp
  
  SELECT MatInvJl
  SCAN FOR &lcScanExp AND &lcMatJorFl
    STORE '' TO lcGlYear , lcGlPeriod
    = lfChkPrd(MatInvJl.dTranDate,'lcGlYear','lcGlPeriod')

    lcCurrCode = ""
    lnCurrUnit = 0
    lnExRate   = 0
 
    IF (EMPTY(lcTarTrans) OR ;
        "MO" $ lcTarTrans OR "MA" $ lcTarTrans OR ;
        "MP" $ lcTarTrans) ;
       AND MatInvJl.cTranType $ "123"
      DO CASE
        CASE MatInvJl.cTranType = "1"
          lcTranType  = "MO"
          lcOtherCatg = "013"
        CASE MatInvJl.cTranType = "2"
          lcTranType = "MA"
          lcOtherCatg = "016"
        CASE MatInvJl.cTranType = "3"
          lcTranType = "MP"
          lcOtherCatg = "016"
      ENDCASE
      DO CASE
        CASE MatInvJl.nIssued <>  0
          *-- Material inventory control GLAccount
          DO lpGLDist WITH .F.,'New','015',lcTranType,MatInvJl.cTran,;
                           -MatInvJl.nIssued*MatInvJl.nUnitCost,MatInvJl.dTranDate,;
                           MatInvJl.cMIcAcct,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
          *-- Material inventory adjuestment GLAccount
          DO lpGLDist WITH .F.,'New',lcOtherCatg,lcTranType,MatInvJl.cTran,;
                           MatInvJl.nIssued*MatInvJl.nUnitCost,MatInvJl.dTranDate,;
                           MatInvJl.cGLMatAdj,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate

          *-- This means that the OlsStk was negative
          *-- so we need to create adjustment entry
          IF MatInvJl.nIssued < 0
            lnCurrRec = RECNO('MatInvJl')
            lnOldCost = MatInvJl.nUnitCost
            lnOldStk  = MatInvJl.nIssued
            lcKeyExp  = cTran+cOprcode+clotno+cTranType+;
                        cFabric+cColor+cWareCode
            lcDyelot  = cDyelot
            
            *-- The Current Alias is "MatInvJl"
            SCAN FOR cTran+cOprcode+clotno+cTranType+;
                     cFabric+cColor+cWareCode = ;
                     lcKeyExp AND cDyelot = lcDyelot AND nReceived <> 0 ;
                     AND RECNO() <> lnCurrRec
              IF MatInvJl.nUnitCost <> lnOldCost
                lnAmount = MIN(ABS(lnOldStk),ABS(MatInvJl.nReceived)) * (lnOldCost-MatInvJl.nUnitCost)
                DO lpGLDist WITH .F.,'New','015',"MA",MatInvJl.cTran,;
                           lnAmount,MatInvJl.dTranDate,;
                           MatInvJl.cMIcAcct,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
                DO lpGLDist WITH .F.,'New',lcOtherCatg,"MA",MatInvJl.cTran,;
                           -lnAmount,MatInvJl.dTranDate,;
                           MatInvJl.cGLMatAdj,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
              ENDIF
            ENDSCAN
            IF RECNO()<>lnCurrRec AND (RECCOUNT('MatInvJl')>=lnCurrRec)
              GO lnCurrRec IN ('MatInvJl')
            ENDIF
          ENDIF

        CASE MatInvJl.nReceived <>  0
          *-- Material inventory control GLAccount
          DO lpGLDist WITH .F.,'New','015',lcTranType,MatInvJl.cTran,;
                           MatInvJl.nReceived*MatInvJl.nUnitCost,MatInvJl.dTranDate,;
                           MatInvJl.cMIcAcct,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
          *-- Material inventory adjuestment GLAccount
          DO lpGLDist WITH .F.,'New',lcOtherCatg,lcTranType,MatInvJl.cTran,;
                           -MatInvJl.nReceived*MatInvJl.nUnitCost,MatInvJl.dTranDate,;
                           MatInvJl.cGLMatAdj,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
      ENDCASE
    ENDIF

  ENDSCAN
  IF llMatInvJl
    USE IN MatInvJl
  ENDIF  

ENDIF
*!**********************************
*-- Transaction : MO,MA,MP     (End)
*!**********************************

*!**********************************
*-- Transaction : CR,KO,CA,DA (Start)
*!**********************************

IF EMPTY(lcTarTrans) OR "CR" $ lcTarTrans OR "KO" $ lcTarTrans OR;
                        "CA" $ lcTarTrans OR "DA" $ lcTarTrans OR;
                        "IN" $ lcTarTrans OR "VI" $ lcTarTrans OR;
                        "RM" $ lcTarTrans OR "VR" $ lcTarTrans
                        
  *--------------------------------------------------------------------*
  PRIVATE lcDrFltStr,lcDrFlt
  lcDrFltStr = ''
  IF "IN" $ lcTarTrans
    lcDrFltStr = lcDrFltStr + '1'
  ENDIF
  IF "DA" $ lcTarTrans
    lcDrFltStr = lcDrFltStr + '2'
  ENDIF
  lcDrFlt = IIF(EMPTY(lcDrFltStr),'.T.',[Debit.TranType $ lcDrFltStr])
  
  IF (EMPTY(lcTarTrans) OR "IN" $ lcTarTrans OR "DA" $ lcTarTrans)
    IF !USED('Debit')
      
      *B802357(Start)
      *USE (gcDataDir+'Debit.dbf')   IN 0 SHARED
      USE (lcDataDir+'Debit.dbf')   IN 0 SHARED
      *B802357(End)
      llDebit = .T.
    ENDIF  

    SELECT Debit
    SCAN FOR &lcFltExp AND &lcDrFlt
      WAIT "Debit file, Tran# " + Debit.Tran WINDOW NOWAIT
      STORE '' TO lcGlYear , lcGlPeriod
      = lfChkPrd(Debit.dPostDate,'lcGlYear','lcGlPeriod')
      
      lcCurrCode = Debit.cCurrCode
      lnCurrUnit = Debit.nCurrUnit
      lnExRate   = Debit.nExRate
      
      DO CASE
        CASE TranType = '1'
          IF (EMPTY(lcTarTrans) OR "IN" $ lcTarTrans)
            *-- Debit Account receivable 
            DO lpGLDist WITH .F.,'New','001','IN',Debit.Tran,;
                              Debit.Amount,Debit.TranDate,;
                              Debit.cArGlAcc,lcGlPeriod,lcGlYear,;
                              lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
        CASE TranType = '2'     &&Debit Adjustement
          *-- Credit Debit Adjustment 010
          DO lpGLDist WITH .F.,'New','010','DA',Debit.Tran,;
                           -Debit.Amount,Debit.TranDate,;
                           Debit.cAdjAcct,lcGlPeriod,lcGlYear,;
                          lcCurrCode,lnCurrUnit,lnExRate
          *-- Bebit Account Receivable 001
          DO lpGLDist WITH .F.,'New','001','DA',Debit.Tran,;
                           Debit.Amount,Debit.TranDate,;
                           Debit.cArGlAcc,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
      ENDCASE
    ENDSCAN
    IF llDebit
      USE IN Debit
    ENDIF  
  ENDIF


  *--------------------------------------------------------------------*
  PRIVATE lcCrFltStr,lcCrFlt

  lcCrFltStr = ''
  IF "RM" $ lcTarTrans
    lcCrFltStr = lcCrFltStr + '0'
  ENDIF
  IF "CR" $ lcTarTrans
    lcCrFltStr = lcCrFltStr + '4'
  ENDIF
  IF "CA" $ lcTarTrans
    lcCrFltStr = lcCrFltStr + '5'
  ENDIF
  lcCrFlt = IIF(EMPTY(lcCrFltStr),'.T.',[Credit.TranType $ lcCrFltStr])

  IF EMPTY(lcTarTrans) OR "RM" $ lcTarTrans OR "CR" $ lcTarTrans OR "CA" $ lcTarTrans
    IF !USED('Credit')
    
      *B802357(Start)
      *USE (gcDataDir+'Credit.dbf')   IN 0 SHARED
      USE (lcDataDir+'Credit.dbf')   IN 0 SHARED
      *B802357(End)
      
      llCredit = .T.
    ENDIF  

    SELECT Credit
    SCAN FOR &lcFltExp AND &lcCrFlt
      WAIT "Credit file, Tran# " + Credit.Tran WINDOW NOWAIT
      STORE '' TO lcGlYear , lcGlPeriod
      = lfChkPrd(Credit.dPostDate,'lcGlYear','lcGlPeriod')
      
      lcCurrCode = Credit.cCurrCode
      lnCurrUnit = Credit.nCurrUnit
      lnExRate   = Credit.nExRate
      
      DO CASE
        CASE TranType = '0'
          IF (EMPTY(lcTarTrans) OR "RM" $ lcTarTrans)
            *-- Credit Account receivable 
            DO lpGLDist WITH .F.,'New','001','RM',Credit.Tran,;
                           Credit.Amount,Credit.TranDate,;
                           Credit.cArGlAcc,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
        CASE TranType = '4'     &&Cash Receipt
          *-- Debit Cash Receipt 002
          DO lpGLDist WITH .F.,'New','002','CR',Credit.Tran,;
                           -Credit.Amount,Credit.TranDate,;
                           Credit.cAdjAcct,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
          *-- Credit Account Receivable 001
          DO lpGLDist WITH .F.,'New','001','CR',Credit.Tran,;
                           Credit.Amount,Credit.TranDate,;
                           Credit.cArGlAcc,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
      
        CASE TranType = '5'     &&Credit Adjustement
          *-- Debit Credit Adjustment 009
          DO lpGLDist WITH .F.,'New','009','CA',Credit.Tran,;
                           -Credit.Amount,Credit.TranDate,;
                           Credit.cAdjAcct,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
          *-- Credit Account Receivable 001
          DO lpGLDist WITH .F.,'New','001','CA',Credit.Tran,;
                           Credit.Amount,Credit.TranDate,;
                           Credit.cArGlAcc,lcGlPeriod,lcGlYear,;
                           lcCurrCode,lnCurrUnit,lnExRate
      ENDCASE
    ENDSCAN
    IF llCredit
      USE IN Credit
    ENDIF  
  ENDIF

  *--------------------------------------------------------------------*
  PRIVATE lcArFltStr,lcArFlt

  lcArFltStr = ''
  IF "RM" $ lcTarTrans
    lcArFltStr = lcArFltStr + '0'
  ENDIF
  IF "VR" $ lcTarTrans
    lcArFltStr = lcArFltStr + 'R'
  ENDIF
  IF "IN" $ lcTarTrans
    lcArFltStr = lcArFltStr + '1'
  ENDIF
  IF "VI" $ lcTarTrans
    lcArFltStr = lcArFltStr + 'I'
  ENDIF
  IF "DA" $ lcTarTrans
    lcArFltStr = lcArFltStr + '2'
  ENDIF
  IF "CR" $ lcTarTrans
    lcArFltStr = lcArFltStr + '4'
  ENDIF
  IF "CA" $ lcTarTrans
    lcArFltStr = lcArFltStr + '57'
  ENDIF
  lcArFlt = IIF(EMPTY(lcArFltStr),'.T.',[ArHist.TranType $ lcArFltStr])

  IF EMPTY(lcTarTrans) OR "CR" $ lcTarTrans OR "KO" $ lcTarTrans OR ;
                          "CA" $ lcTarTrans OR "DA" $ lcTarTrans OR ;
                          "IN" $ lcTarTrans OR "VI" $ lcTarTrans OR ;
                          "RM" $ lcTarTrans OR "VR" $ lcTarTrans
    IF !USED('ArHist')
     
      *B802357(Start)
      *USE (gcDataDir+'ArHist.dbf')   IN 0 SHARED
      USE (lcDataDir+'ArHist.dbf')   IN 0 SHARED
      *B802357(End)
      
      llArHist = .T.
    ENDIF
    
    IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans)
      lcHistory = gfTempName()
      = lfHistory()
    ENDIF
    
    SELECT ArHist
    SCAN FOR &lcFltExp AND &lcARFlt
      WAIT "ArHist file, Tran# " + ArHist.Tran WINDOW NOWAIT
      STORE '' TO lcGlYear , lcGlPeriod
      = lfChkPrd(ArHist.dPostDate,'lcGlYear','lcGlPeriod')
      
      lcCurrCode = ArHist.cCurrCode
      lnCurrUnit = ArHist.nCurrUnit
      lnExRate   = ArHist.nExRate
      
      DO CASE
        CASE TranType = "0"
          IF (EMPTY(lcTarTrans) OR "RM" $ lcTarTrans)
            *-- Credit Account receivable 
            DO lpGLDist WITH .F.,'New','001','RM',ArHist.Tran,;
                             ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
          IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans) AND ;
             SEEK(ArHist.History+ArHist.cArGlAcc,lcHistory)
            *-- Debit Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','KO',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF

        CASE TranType = "R"
          IF (EMPTY(lcTarTrans) OR "VR" $ lcTarTrans)
            *-- Debit Account receivable 
            DO lpGLDist WITH .F.,'New','001','VR',ArHist.Tran,;
                             ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
          IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans) AND ;
             SEEK(ArHist.History+ArHist.cArGlAcc,lcHistory)
            *-- CREDIT Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','KO',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF

        CASE TranType = "1"
          IF (EMPTY(lcTarTrans) OR "IN" $ lcTarTrans)
            *-- Debit Account receivable 
            DO lpGLDist WITH .F.,'New','001','IN',ArHist.Tran,;
                             ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
          IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans) AND ;
             SEEK(ArHist.History+ArHist.cArGlAcc,lcHistory)
            *-- Credit Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','KO',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF

        CASE TranType = "I"
          IF (EMPTY(lcTarTrans) OR "VI" $ lcTarTrans)
            *-- Debit Account receivable 
            DO lpGLDist WITH .F.,'New','001','VI',ArHist.Tran,;
                             ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
          IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans) AND ;
             SEEK(ArHist.History+ArHist.cArGlAcc,lcHistory)
            *-- Credit Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','KO',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF


        CASE TranType = "4"
          *-- Generate Cash receipt (CR) transactions
          IF (EMPTY(lcTarTrans) OR "CR" $ lcTarTrans)  &&Cash Reciept
            *-- Debit Cash Receipt 002
            DO lpGLDist WITH .F.,'New','002','CR',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cAdjAcct,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
            *-- Credit Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','CR',ArHist.Tran,;
                             ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
          *-- Generate Key-Off (KO) transactions
          IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans) AND ;
             SEEK(ArHist.History+ArHist.cArGlAcc,lcHistory)
            *-- Debit Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','KO',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
          

        CASE TranType = "2"       &&Debit Adjustement
          *-- Generate Debit Aduestment (DA) transactions
          IF (EMPTY(lcTarTrans) OR "DA" $ lcTarTrans)  &&Debit Adjuest
            *-- Credit Debit Adjustment 010
            DO lpGLDist WITH .F.,'New','010','DA',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cAdjAcct,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
            *-- Debit Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','DA',ArHist.Tran,;
                             ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
          *-- Generate Key-Off (KO) transactions (Start)
          IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans) AND ;
             SEEK(ArHist.History+ArHist.cArGlAcc,lcHistory)
            *-- Credit Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','KO',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF

        CASE TranType $ "57"   &&Credit Adjustement
          *-- Generate Crdeit Aduestment (CA) transactions
          IF (EMPTY(lcTarTrans) OR "CA" $ lcTarTrans)
            *-- Debit Credit Adjustment 009
            DO lpGLDist WITH .F.,'New','009','CA',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cAdjAcct,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
            *-- Credit Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','CA',ArHist.Tran,;
                             ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
          *-- Generate Key-Off (KO) transactions
          IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans) AND ;
             SEEK(ArHist.History+ArHist.cArGlAcc,lcHistory)
            *-- Debit Account Receivable 001
            DO lpGLDist WITH .F.,'New','001','KO',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF

        CASE TranType = "8"     &&Debit on Account
          *-- Generate Key-Off (KO) transactions
          IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans) AND ;
             SEEK(ArHist.History+ArHist.cArGlAcc,lcHistory)
            *-- Debit Account Receivable
            DO lpGLDist WITH .F.,'New','001','KO',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF

        CASE TranType = "9"     &&Credit on Account
          *-- Generate Key-Off (KO) transactions
          IF (EMPTY(lcTarTrans) OR "KO" $ lcTarTrans) AND ;
             SEEK(ArHist.History+ArHist.cArGlAcc,lcHistory)
            *-- Credit Account Receivable
            DO lpGLDist WITH .F.,'New','001','KO',ArHist.Tran,;
                             -ArHist.Amount,ArHist.TranDate,;
                             ArHist.cArGlAcc,lcGlPeriod,lcGlYear,;
                             lcCurrCode,lnCurrUnit,lnExRate
          ENDIF
      ENDCASE
    ENDSCAN
    IF llArHist
      USE IN ArHist
    ENDIF  
  ENDIF
ENDIF

*!**********************************
*-- Transaction : CR,KO,CA,DA (End)
*!**********************************

SELECT GLDist
IF EMPTY(lcTarTrans)
  lcForExp = lcRpExp
ELSE
  lcForExp = lcRpExp + IIF(EMPTY(lcFltExp),'',' AND ') + "Tran_Type $ lcTarTrans"
ENDIF  
SCAN FOR &lcForExp
  WAIT "GLDIST Tran# " + GLDist.Tran_No WINDOW NOWAIT
  DO lpGLDist WITH .T.,'Old',GLDist.Catg_Key,GLDist.Tran_Type,GLDist.Tran_No,;
                   GLDist.nEqvAmnt,GLDist.Tran_Date,;
                   GLDist.GLAccount,GLDist.GLPeriod,GLDist.GLFYear
ENDSCAN

*!**********************************

SELECT(lcGenDist)
GO TOP

WAIT CLEAR

IF lcRPOnlyDf = 'Y'
  lcDiffExp = [FOR LIKE("*Y",EVAL(KEY()))]
  DO gfDispRe WITH EVAL('lcRPFormNa'), lcDiffExp
ELSE 
  DO gfDispRe WITH EVAL('lcRPFormNa')
ENDIF  

IF USED (lcGenDist)
  USE IN (lcGenDist)
ENDIF
ERASE (gcWorkDir+lcGenDist.'DBF')
ERASE (gcWorkDir+lcGenDist.'CDX')

IF USED(lcHistory)
  USE IN (lcHistory)
ENDIF
ERASE (gcWorkDir+lcHistory.'DBF')
ERASE (gcWorkDir+lcHistory.'CDX')

*!***********************************************************************

PROCEDURE lpGLDist

PARAMETERS llEqvAmnt,lcSorcFile,lcCatgKey,lcTranType,lcTranNo,lnAmount,ldTranDate,;
           lcGLAcnt,lcGlPrd,lcGlYr,lcCurrCode,lnCurrUnit,lnExRate

PRIVATE llEqvAmnt,lcSorcFile,lcCatgKey,lcTranType,lcTranNo,lnAmount,ldTranDate,;
        lcGLAcnt,lcGlPrd,lcGlYr,lcCurrCode,lnCurrUnit,lnExRate

PRIVATE lnCurAlias


IF lnAmount <> 0
  lnCurAlias = SELECT(0)

  lcTranDesc = laTran[ASUBSCRIPT(laTran,ASCAN(laTran,lcTranType),1),1]

  *-- IF llEqvAmnt means the amount is passed with the equivalent value
  *-- and there is no need to Calculate it.
  
  IF llEqvAmnt OR EMPTY(lcCurrCode) .OR. INLIST(lcCatgKey, '006', '008');
      .OR. lcCurrCode = gcBaseCurr
    lcCurrCode = gcBaseCurr
    lnCurrUnit = 1
    lnExRate   = 1
    lnEqvAmnt  = lnAmount
  ELSE
    IF lnExRate > 0 .AND. lnCurrUnit > 0
      lcUntSin = ''
      lcExRSin = gfGetExSin(@lcUntSin, lcCurrCode)
      lnEqvAmnt  = ROUND(lnAmount &lcExRSin lnExRate ;
                                 &lcUntSin lnCurrUnit, 2)
    ELSE
      RETURN .F.
    ENDIF
  ENDIF

  IF UPPER(lcSorcFile) = "NEW"
    lcEqvFld  = lcGenDist + ".nGenEqvAmt"
    lcYearFld = lcGenDist + ".nGenFYear"
    lcPrdFld  = lcGenDist + ".nGenPeriod"
  ELSE
    lcEqvFld  = lcGenDist + ".nEqvAmnt"
    lcYearFld = lcGenDist + ".glFYear"
    lcPrdFld  = lcGenDist + ".glPeriod"
  ENDIF

  IF lcRPSort = "T"
    lcSeekExp = lcTranType+lcTranNo+lcCatgKey+lcGLAcnt
  ELSE
    lcSeekExp = lcGLAcnt+lcTranType+lcTranNo+lcCatgKey
  ENDIF
  
  IF SEEK(lcSeekExp,lcGenDist)
    lnEqvAmnt = lnEqvAmnt + &lcEqvFld
    lcCatgDesc = &lcGenDist..CATG_DESC 
    SELECT (lcGenDist)
  ELSE
    SELECT SyGlCatg
    lcCatgDesc = LOOKUP(CatgDesc,lcCatgKey,CatgKey)
    SELECT (lcGenDist)
    APPEND BLANK
  ENDIF  

  *REPLACE CATG_KEY   WITH lcCatgKey  ,;
          CATG_DESC  WITH lcCatgDesc ,;
          TRAN_TYPE  WITH lcTranType ,;
          TRAN_NO    WITH lcTranNo   ,;
          TRAN_DATE  WITH ldTranDate ,;
          GLACCOUNT  WITH lcGLAcnt   ,;
          ADD_DATE   WITH gdSysDate  ,;
          ADD_TIME   WITH TIME()     ,;
          ADD_USERID WITH gcUser_Id  ,;
          TRAN_DESC  WITH lcTranDesc ,;
          cCurrCode  WITH lcCurrCode ,;
          nCurrUnit  WITH lnCurrUnit ,;
          nExRate    WITH lnExRate   ,;
          &lcPrdFld  WITH lcGlPrd    ,;
          &lcYearFld WITH lcGlYr     ,;
          &lcEqvFld  WITH lnEqvAmnt  ,;
          cDiffer    WITH IIF(nGenEqvAmt-nEqvAmnt<>0,'Y','N')

  REPLACE CATG_KEY   WITH lcCatgKey  ,;
          CATG_DESC  WITH lcCatgDesc ,;
          TRAN_TYPE  WITH lcTranType ,;
          TRAN_NO    WITH lcTranNo   ,;
          TRAN_DATE  WITH ldTranDate ,;
          GLACCOUNT  WITH lcGLAcnt   ,;
          TRAN_DESC  WITH lcTranDesc ,;
          cCurrCode  WITH lcCurrCode ,;
          nCurrUnit  WITH lnCurrUnit ,;
          nExRate    WITH lnExRate   ,;
          &lcPrdFld  WITH lcGlPrd    ,;
          &lcYearFld WITH lcGlYr     ,;
          &lcEqvFld  WITH lnEqvAmnt  ,;
          cDiffer    WITH IIF(nGenEqvAmt-nEqvAmnt<>0,'Y','N')
  
  *=gfAdd_Info(lcGenDist)
  
  IF UPPER(lcSorcFile) = "OLD"
    REPLACE GLBatch WITH GLDist.GLBatch ,;
            Posted  WITH GLDIST.Posted
  ENDIF  
  
  *!B608940,1 AHS 07/15/2009 G/L Entry Reconciliation includes Inventory entries even when system flag is set to No [Start]
  IF gfGetMemVar('M_GL_COST') ='N'
   IF INLIST(lcCatgKey, '006', '007', '008', '011', '012', '013', '015', '016', '017', '018', '019', '021', '022', '023', '024', '025', '026', '027')
    DELETE 
   ENDIF 
  ENDIF
  *!B608940,1 AHS 07/15/2009 G/L Entry Reconciliation includes Inventory entries even when system flag is set to No [End]


  SELECT(lnCurAlias)
ENDIF          

*  SELECT(lnCurAlias)
*ENDIF  

*!***********************************************************************

FUNCTION lfChkPrd
PARAMETERS ldDate,lcFYear,lcPeriod

lnAlias = SELECT(0)

IF lcGlVers = 'S'            &&   <<<... SBT 2.5 ... >>>
  =SEEK(lcGlComp,'SYCCOMP')
  SELECT SYCDFIS
  IF SEEK(lcGlComp)
    LOCATE REST FOR BETWEEN(ldDate,Bdate,Edate) ;
                WHILE (ldDate >= Bdate) .AND. (CompId = lcGlComp)
  ENDIF
  &lcFYear  = SUBSTR(Yearprd,1,4)      && Transaction date year
  &lcPeriod = SUBSTR(Yearprd,5,2)      && Transaction date period     
ELSE
  SELECT FSPRD
  GO TOP
  LOCATE FOR BETWEEN(ldDate,Dfsppbgdt,Dfsppendt) ;
              WHILE (ldDate >= Dfsppbgdt) 
  &lcFYear  = Cfisfyear      && Transaction date year
  &lcPeriod = Cfspprdid      && Transaction date period     
ENDIF

SELECT(lnAlias)

*!***********************************************************************

FUNCTION lfTranType

DIMENSION laTran[1,2]
STORE SPACE(0) TO laTran

IF EMPTY(laRPTranSo)
  DIMENSION laRPTranTa[1]
  STORE SPACE(0) TO laRPTranTa

  DIMENSION laRPTranSo[1]
  STORE SPACE(0) TO laRPTranSo

  lnI = 0
  SELECT SyGLTran
  SCAN
    lnI = lnI + 1
    DIMENSION laTran[lnI,2]
    DIMENSION laRPTranSo[lnI,1]

    laTran[lnI,1]     = Tran_Desc
    laTran[lnI,2]     = Tran_Type
    laRPTranSo[lnI,1] = Tran_Desc
  ENDSCAN
ENDIF

*!***********************************************************************

FUNCTION lfvTranMov

= gfMover(@laRPTranSo,@laRPTranTa,"Transactions",.T.,.F.,.F.,.T.)

*!***********************************************************************

FUNCTION lfvSortBy

IF lcRPSort = "T"
  lcRPFormNa = "SMGLREC1"
ELSE
  lcRPFormNa = "SMGLREC2"
ENDIF    

*!***********************************************************************

PROCEDURE lpCreatTmp

SELECT GLDist
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
      
DIMENSION laFileStru[lnFileStru + 6, 4]
     
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Catg_Desc'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 30
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nGenGLAmt'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 14
laFileStru[lnFileStru ,4] = 2

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nGenEqvAmt'
laFileStru[lnFileStru ,2] = 'N'
laFileStru[lnFileStru ,3] = 15
laFileStru[lnFileStru ,4] = 2

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nGenFYear'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 4
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'nGenPeriod'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 2
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cDiffer'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0

DIMENSION laIndex[2,2]
laIndex[1,1] = "Tran_Type+Tran_No+Catg_Key+GlAccount+cDiffer"
laIndex[1,2] = "TranType"
laIndex[2,1] = "GlAccount+Tran_Type+Tran_No+Catg_Key+cDiffer"
laIndex[2,2] = "GlAccount"

=gfCrtTmp(lcGenDist,@laFileStru,@laIndex)

IF lcRPSort = "T"
  SET ORDER TO TranType IN (lcGenDist)
ELSE
  SET ORDER TO GlAccount IN (lcGenDist)
ENDIF  

***********************************************************

FUNCTION lfHistory

SELECT History,cArGlAcc,SUM(IIF(TranType$'89',Amount,-Amount)) AS Sum_Amt ;
FROM ArHist ;
INTO DBF (gcWorkDir+lcHistory) ;
GROUP BY History,cArGlAcc;
HAVING Sum_Amt <> 0 ;
ORDER BY History

INDEX ON History+cArGlAcc TAG (lcHistory) OF (gcWorkDir+lcHistory+'.CDX')

************************************************************

FUNCTION lfSelComp
*B802357

PRIVATE lnI,lnCurAlias,lcCurTag

IF TYPE('laCompDesc[1,1]') $ "UL" OR ;
   (ALEN(laCompDesc,1) = 1 AND EMPTY(laCompDesc[1,1]))
  
  DIMENSION laCompDesc[1,1],laCompCode[1,1]
  STORE SPACE(0) TO laCompDesc[1,1],laCompCode[1,1]
ENDIF

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
PRIVATE lnI
lnI = 1
SCAN
  lnI = ALEN(laCompDesc,1) + IIF(EMPTY(laCompDesc[lnI]),0,1)
  DIMENSION laCompDesc[lnI,1],laCompCode[lnI,1]
  laCompDesc[lnI,1] = SycComp.cComp_ID+"-"+SycComp.cCom_Name
  laCompCode[lnI,1] = SycComp.cComp_ID
ENDSCAN

SET ORDER TO &lcCurTag

SELECT (lnCurAlias)

***********************************************************

FUNCTION lfDefComp
*B802357

PRIVATE lcRet

IF EMPTY(gcAct_Comp)
  DIMENSION laCompDesc[ALEN(laCompDesc,1)+1,1],laCompCode[ALEN(laCompCode,1)+1,1]
  =AINS(laCompDesc , 1)
  =AINS(laCompCode , 1)
  laCompDesc[1,1] = "Select company"
  laCompCode[1,1] = "NoComp"
  lcRet = "NoComp"
ELSE
  lcRet = gcAct_Comp
  =lfDataDir(lcRet)
ENDIF

RETURN lcRet

**********************************************************

FUNCTION lfvSelComp
*B802357

=lfDataDir(lcRPSelCom)

*********************************************************

FUNCTION lfDataDir
*B802357
PARAMETERS lcComp

PRIVATE lcComp,lnCurAlias,lcCurTag

lnCurAlias = SELECT(0)
SELECT SycComp
lcCurTag = ORDER()
SET ORDER TO Ccomp_id
IF SEEK(lcComp)
  lcDataDir = gfGetDataDir(ALLTRIM(SycComp.cCom_dDir))
ENDIF  
SET ORDER TO &lcCurTag
SELECT (lnCurAlias)
