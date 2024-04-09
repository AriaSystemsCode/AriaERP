*!**************************************************************************
*! Name      : BINMAIN.PRG
*! Developer : Nader NABIL (NNA)
*! Date      : 02/14/2006
*! Purpose   : Collect all the Bin Location Functions in one program with new Design
*! Due To    : B#039660,B#039662,B#039663
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : this program related to CP#037816 , CP#123853 , B#128987
*!           : CP#126994 , B#128987 , B#129429 , B#129431 , B#130750 , CP#132193
*!**************************************************************************
*! Functions : Inventory Module    : lfDLCHKADJ,lfDLCHKTR,lfDLCHKTRN,lfDLUPDTGL,lfTRVLDSTY,
*!                                 : lfTRMDSTY,lfDLCKSVIC,lfDLGETBIN,lfvbinloc,lfvOk,lfInitVar,
*!                                 : lfDlModBrw,lfDlSavDat,lfDLVLDBIN,lfDLVLDSTY,lfDLMDYSTY,lfDLSVBNI,
*!                                 : lfDLSVBNR,lfDLASSBIN,lfDLBRWBIN,lfDLDELALL,lfDLPSTLCK,lfDLSELREC,
*!                                 : lfDLSELTMP,lfDLTMPBAT,lfDLSTYCRL,lfSTYCRL,lfUpdGLDist,lfUpdStep,
*!                                 : lfDEFNMNU,lfBinData,lfvsavlin,lfWShwlin,lfDLSAVCS,lfvBinBrow
*!                Alloction Module : lfChkForce Check if Use Bin Location set to Yes to Stop force allocation
*!                                 : lfAlSavOrd (Alloction By Order)
*!                                 : lfAlSavSty (Alloction By Style)
*!                                 : lfAlSavAut (Automatic Alloction )
*!                                 : lfUpdPkBin (Update the PKBinLoc file with the Picked Qty.)
*!                                 : lfChkPktk  (Compare Between the Allocated Qty. in the 
*!                                              Whbinloc file and Picked Qty. in Ordline
*!                                 : lfOpnFiles (Open Need files i.e Whbinloc,Pkbinloc,Whloc)
*!                                 : lfALRELORD (Release piktkt in Order Allocation screen , Style Allocation Screen
*!                                 :             Release PikTkt Screen ,Pick Ticket Screen and Sales order screen)
*!                                 : lfChkFirst (Check if there are stock enough to allocate or not)
*!                                 : lfChngQty  Force user to change Packing list Qty from the Allocation Programs if he want
*!             Accounts Receivable : lfARGETBIN,lfARDFNMNU,lfWArBnLoc,lfvArbnloc,lfDLINTVAR,lfvArOk,lfDLSAVINV
*!             (AR) Module         : lfDLARBIN,lfDLVODINV,lfDLARVOID,lfARCKSVBN,lfALSAVINV,lfArChkOrd,lfArChkScp,
*!                                 : lfPackDeff,lfChngShip,lfChkInvBn,lfARCHKSIZ,lfOldBinlc
*!                                 : 
*!       Style Purchase Order (PO) : LFPORECPAD,lfSAVBINS,lfDLSBNPOR,lfVLDRFRSH,lfDLCHKSAV,lfDVLDPOBN,lfDVLDPOAU,lfCrtBinLn,
*!                                 : lfDispScr,lplTab,lpLBackTab,lfReadAct,lfReadDAct,lfClearKey,lfBrowse,lfvBrowse,lfWbrowse,
*!                                 : lfValidBut,lfShipQty,lfBnOldVal,lfvBnQty,lfvRecBin,lfSavMltBn,lfDLVLDWRH,lfLDFNPORT,lfLDFNPOIS,
*!                                 : lFVPOBNLC,lfDLCHKBIN,lfADDFILDS,lfDLRCVQTY,lfDLUPDQTY,lfvPOScbn,lfvPOScOk
*!                                 : 
*!        Return Merchandise (RM)  : lfDLGTRMBN , lfvRMbinloc , lfvRmOk , lfADDRMFLD , lfDLSAVRM , lfRMDFNMNU ,
*!                                 : lfWBnLoc , lfRMCKSVBN , lfDLVODRM , lfDLRMVOID , lfRMCKVDBN
*!                                 : 
*!                  System Manager : lfUseBnLoc (Disable [Use Bin Location] setting if it is 'Y' (to privent switching from Yes to No))
*!                                 : 
*!                 General Section : lfChkStrct get the Style,Color and scale length.
*!                                 : lfOpnFiles Open the needed files for Bin Location System.
*!                                 : lfEraseFil Erase Temp. Files
*!                                 :
*!          Functions for B#130750 : lfADDSTYBN , lfAdStyScr , lfStyBnBrw , lfwhBnBrow , lfVNStyBin , lfVRStyBin
*!                                 : lfvNBinSty , lfOkStyBin , lfvSizQty , lfVExitOk , lfvNewCost
*!         Functions for CP#132193 : LFFILLSEC , LFVSECTION , LFBINSFLTR
*!**************************************************************************
*! Modifications
*! B130750,1 NNA 04/04/2006 Make some changes at inventory locking by adding new option called (Add bin to batch file)
*! B130750,1 NNA            to add new bin to a batch in the Edit Mode.
*! C132193,1 NNA 05/17/2006 Add new option called (Section) to the option grid of the inventory Locking to filter with it
*! C132754,1 TMI 07/19/2006 Add a new field for replenshment
*! B132562,1 TMI 08/14/2006 Fix an Error in receiving stock into multiple bins.
*T20060817.0014 TMI 08/23/2006 fixing some problems for binlocations related to issues 132562,132757,132677,132493,132307,132086,131267
*! B608036,1 TMI 04/11/2007 fix a bug of syntax error due to undefined variable ( check ticket # T20070410.0001 )
*! B608148,1 NNA 07/02/2007 Fix Bug that With the custom bin location system installed and with FIFO as the costing method, 
*! B608148,1 NNA            when posting an invntory locking batch, the system is updating receiving sessions based on 
*! B608148,1 NNA            records in BININVJL. This is causing a problem as the bininvjl file is not accurate (T20070601.0009)
*! C200861,1 NNA 09/21/2007 (T20070827.0004) Add new feature to Transfer inventory screen to give ability to user to move 
*! C200861,1 NNA            all stock from a bin to another automatically as soon as he select the style and move all option.
*! C200861,1 NNA            **********this custom did for Panache only*************
*!**************************************************************************
*!* laScrMode [1] --> SELECT MODE
*!* laScrMode [2] --> VIEW   MODE
*!* laScrMode [3] --> EDIT   MODE
*!* laScrMode [4] --> ADD    MODE

PARAMETER lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'
*!*Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*!*Start OF The Inventory Control Section.
*!*************************************************************
*!* Name      : lfUSRFELDS
*!* Developer : NADER NABIL - (NNA)
*!* Date      : 02/14/2006
*!* Module    : Inventory Control (IC)
*!* Purpose   : Function to update the style class Fields with 
*!*           : a default value ='A'
*!*************************************************************
*!* Called from : IcStyle.prg 
*!*************************************************************
*!* Passed Parameters  : ....
*!*************************************************************
*!* Returns   : None
*!*************************************************************
*!* Example   : DO lfUSRFELDS
*!*************************************************************
FUNCTION lfUSRFELDS
*-- In case we're in the style screen
IF !EMPTY(laUsrFields) AND laScrMode[4] AND lcProgname = 'ICSTYLE'
  FOR J = 1 TO ALEN(laUsrFields,1)
    IF 'CPRIMCLSS' $ ALLTRIM(laUsrFields[J,1]) OR ;
       'CSECCLSS'  $ ALLTRIM(laUsrFields[J,1]) OR ;
       'CREMCLSS'  $ ALLTRIM(laUsrFields[J,1])
      laUsrFields[J,6] = 'A'
    ENDIF
  ENDFOR
ENDIF
*-- In case we're in the sales order screen.
IF !EMPTY(laUsrFields) AND lcProgname = 'SOORD'
  FOR J = 1 TO ALEN(laUsrFields,1)
    IF ALLTRIM(laUsrFields[J,1]) = "CBLKPCK"
      laUsrFields[J,6] = CUSTOMER.CBLKPCK
    ENDIF
    *--Get the customer Default for OrdHdr.LCARTBC and make the default value for Lcharge=.T.
    IF ALLTRIM(laUsrFields[J,1]) = "LCARTBC"
      laUsrFields[J,6] = CUSTOMER.lCharge
    ENDIF
    IF ALLTRIM(laUsrFields[J,1]) = "LCHARGE"
      laUsrFields[J,6] = .T.
    ENDIF
  ENDFOR
ENDIF
*-- End of Function lfUSRFELDS.
*!***************************************************************************
*!* Name        : lfDLCHKADJ
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Check Data in case of adj. of physical data
*!***************************************************************************
*!* Called from : IcStyAd.prg , IcStyPh.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLCHKADJ()
*!***************************************************************************
FUNCTION lfDLCHKADJ
*-- Check setting of Add Bin Location Yes/No
IF gfGetMemVar('M_DLUSEBIN')
  SET ORDER TO TAG WhsLoc IN  WHSLOC
  IF !SEEK(lcWareH ,'WHSLOC')
    lcMsg2 = 'No bin location found for warehouse ' +lcWareH
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    _CUROBJ = OBJNUM(lcWareH)
    SET ORDER TO TAG WhsLocSt IN  WHSLOC
    RETURN
  ENDIF
  SET ORDER TO TAG WhsLocSt IN  WHSLOC
  IF !laScrMode[4]
    laScrMode = .F.
    laScrMode[4] = .T.
    =lfActBrow()
    llCUpdate = .T.
    SHOW GETS   
  ENDIF
ELSE
  IF !EMPTY(lcFromWare)
    laScrMode[1] = .F.
    laScrMode[4] = .T.
    llCUpdate = .T.
    SHOW GETS   
  ENDIF
ENDIF  

*-- End of Function lfDLCHKADJ.
*!***************************************************************************
*!* Name        : lfDISBINLC	
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Disable bin check box in case of inventory locking
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDISBINLC	()
*!***************************************************************************
FUNCTION lfDISBINLC	
SHOW GET lnBin DISABLE
*-- End of Function lfDISBINLC	.

*!***************************************************************************
*!* Name        : lfDLCHKTR
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Check Data in case of Transfer Inventory
*!***************************************************************************
*!* Called from : IcStyTr.prg 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLCHKTR()
*!***************************************************************************
FUNCTION lfDLCHKTR
*-- Check setting of Add Bin Location Yes/No
IF gfGetMemVar('M_DLUSEBIN')
  SET ORDER TO TAG WhsLoc IN  WHSLOC
  IF !SEEK(lcWareH ,'WHSLOC')
    lcMsg2 = 'No bin location found for warehouse ' +lcWareH
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    _CUROBJ = OBJNUM(lcWareH)
    SET ORDER TO TAG WhsLocSt IN  WHSLOC
    RETURN
  ENDIF
  SET ORDER TO TAG WhsLocSt IN  WHSLOC
  IF !laScrMode[4]
    laScrMode = .F.
    laScrMode[4] = .T.
    =lfActBrow()
    llCUpdate = .T.
    SHOW GETS   
  ENDIF
ELSE
  IF lcFromWare = lcToWare
    *--You cannot transfer from the same warehouse.
    =gfModalGen('TRM42055B42001','DIALOG')
    RETURN .F.
  ELSE
    IF !laScrMode[4]
      laScrMode = .F.
      laScrMode[4] = .T.
      =lfActBrow()
      llCUpdate = .T.
      SHOW GETS   
    ENDIF
  ENDIF
ENDIF  
*-- End of Function lfDLCHKTR.
*!***************************************************************************
*!* Name        : lfCHKBINS
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Check if the company dosen't use multi wharehouse and use 
*!*             : bin location yes.
*!***************************************************************************
*!* Called from : IcStyTr.prg 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCHKBINS()
*!***************************************************************************
FUNCTION lfCHKBINS
IF gfGetMemVar('M_DLUSEBIN')   && setting For use bin location Yes/No
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
*-- End of FUNCTION lfCHKBINS.
*!***************************************************************************
*!* Name        : lfDLCHKTRN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Check Transaction
*!***************************************************************************
*!* Called from : ICSTYTR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLCHKTRN()
*!***************************************************************************
FUNCTION lfDLCHKTRN
IF gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  *--check if Bin transfer to same class as or not
  PRIVATE lcFileNama
  lcFileNama = lcTmpAdj+"A"
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc
  *C200861,1 NNA 09/21/2007 (Begin) do this check only if the customer is not panache
  IF ASCAN(laEvntTrig,PADR("MOVEALL",10)) = 0 
  *C200861,1 (End)

    IF SEEK(IIF(lcType = 'T',lcToWare+&lcFileNamA..LocTo,lcFromWare++&lcFileNamA..LocFrom),'WHSLOC')
      llClass = .F.
      IF INLIST(TRIM(WhsLoc.CBINCLASS),TRIM(STYLE.CPRIMCLSS&lcCont),TRIM(STYLE.CSECCLSS&lcCont),TRIM(STYLE.CREMCLSS&lcCont))
        llClass = .T.
      ENDIF
      IF !llClass
        SELECT WHSLOC
        SET ORDER TO TAG WhsLocSt IN WHSLOC 
        *--You cannot transfer from the same warehouse.
        lcMsg2 = 'You cannot Issue the style Size to a bin of a different class.'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        m.Adj&lcCont = IIF(lcType = 'P',laOTStk[I],0)
        RETURN
      ENDIF
    ENDIF
 
  *C200861,1 NNA 09/21/2007 (Begin) End of if statment
  ENDIF
  *C200861,1 (End)

  IF lcType = 'A' AND m.Adj&lcCont < 0 
    IF ABS(m.Adj&lcCont) > (WHBINLOC.QTY&lcCont - WHBINLOC.ALO&lcCont)
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You can not reduce inventory more than the available ( ' + ;
        ALLTRIM(STR(WHBINLOC.QTY&lcCont - WHBINLOC.ALO&lcCont)) + " )")
      m.Adj&lcCont = 0
      RETURN
    ENDIF
  ENDIF
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt IN WHSLOC 
  IF !(&lcFileNamA..LocFrom = &lcFileNamA..LocTo AND  lcFromWare = lcToWare)
    IF !llModiMod
      laTStk[I] = laOTStk[I] + m.Adj&lcCont
    ELSE
      laTStk[I] = ( laOTStk[I] - &lcTmpAdj->Adj&lcCont ) + m.Adj&lcCont  
    ENDIF
    laTStk[9] = laTStk[1]+laTStk[2]+laTStk[3]+laTStk[4]+laTStk[5]+laTStk[6]+laTStk[7]+laTStk[8]
  ENDIF 
ELSE
  IF !llModiMod
    laTStk[I] = laOTStk[I] + m.Adj&lcCont
  ELSE
    laTStk[I] = ( laOTStk[I] - &lcTmpAdj->Adj&lcCont ) + m.Adj&lcCont  
  ENDIF
  laTStk[9] = laTStk[1]+laTStk[2]+laTStk[3]+laTStk[4]+laTStk[5]+laTStk[6]+laTStk[7]+laTStk[8]
ENDIF  
*-- End of Function lfDLCHKTRN.  
*!***************************************************************************
*!* Name        : lfDLUPDTGL
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update Gl records for Bin Location
*!***************************************************************************
*!* Called from : ICSTYTR.PRG,ICSTYPH.PRG -->ICINVSAV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLUPDTGL()
*!***************************************************************************
FUNCTION lfDLUPDTGL
IF gfGetMemVar('M_DLUSEBIN')
  IF lcType = 'T'
    PRIVATE lcFileNama
    lcFileNama = lcTmpAdj+"A"
    IF !(&lcFileNamA..LocFrom = &lcFileNamA..LocTo AND  lcFromWare = lcToWare)
      WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
      SELECT (lcTmpGlDt)
      *-- Generate a unique session number.
      lcGlSess = gfsequence('GLSESSION')
      REPLACE ALL GLSESSION WITH lcGlSess
      USE
      SELECT GLDIST  
      APPEND FROM &gcWorkDir.&lcTmpGlDt     
      *--Erase the lcTmpGlDt file.
      =lfEraseFil(lcTmpGlDt)
    ENDIF  
  ENDIF  
ELSE
  WAIT WINDOW 'Updating the general ledger distribution file.' NOWAIT
  SELECT (lcTmpGlDt)
  *-- Generate a unique session number.
  lcGlSess = gfsequence('GLSESSION')
  REPLACE ALL GLSESSION WITH lcGlSess
  USE
  SELECT GLDIST  
  APPEND FROM &gcWorkDir.&lcTmpGlDt     
  *--Erase the lcTmpGlDt file.
  =lfEraseFil(lcTmpGlDt)
ENDIF
*-- End of Function lfDLUPDTGL.
*!***************************************************************************
*!* Name        : lfTRVLDSTY
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validate Style in case of IC Transfeer.
*!***************************************************************************
*!* Called from : ICSTYTR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfTRVLDSTY()
*!***************************************************************************
FUNCTION lfTRVLDSTY
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN .F.
ENDIF
PRIVATE lcFileNama
lcFileNama = lcTmpAdj+"A"
IF !SEEK(lcToWare+&lcFileNamA..LocTo+m.style,'WHSLOC')
  =SEEK(lcToWare+&lcFileNamA..LocTO,'WHSLOC')
  lcLocSty = m.style
  SCATT MEMVAR MEMO
  m.cWareCode = lcToWare
  m.style     = lcLocSty
  m.clocation = &lcFileNamA..LocTo
  INSERT INTO WHSLOC FROM MEMVAR 
ENDIF
SET ORDER TO TAG WhsLocSt IN WHSLOC 
IF !llDyelot OR Style.cDye_Flg<>'Y'
  =lfGetInfo()
ELSE
  lcStyDesc = Desc1
  SHOW GET ibDye    ENABLE
  SHOW GET m.Dyelot ENABLE
  =lfRefresh('ICINVT_A')
ENDIF
IF SEEK(lcFromWare+&lcFileNamA..LocFrom+M.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.tOTstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laTStk[9] = m.Totstk
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    m.Stk&lcCount = 0
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = 0
  ENDFOR
  laTStk[9] = 0
  m.tOTstk  =0
  laOTStk[9] = 0
ENDIF
IF SEEK(lcToWare+&lcFileNamA..LocTo+M.Style,'WHBINLOC')
  laTStk[1]  =WHBINLOC.Qty1 - WHBINLOC.Alo1
  laTStk[2]  =WHBINLOC.Qty2 - WHBINLOC.Alo2
  laTStk[3]  =WHBINLOC.Qty3 - WHBINLOC.Alo3
  laTStk[4]  =WHBINLOC.Qty4 - WHBINLOC.Alo4
  laTStk[5]  =WHBINLOC.Qty5 - WHBINLOC.Alo5
  laTStk[6]  =WHBINLOC.Qty6 - WHBINLOC.Alo6
  laTStk[7]  =WHBINLOC.Qty7 - WHBINLOC.Alo7
  laTStk[8]  =WHBINLOC.Qty8 - WHBINLOC.Alo8
  laTStk[9]  =WHBINLOC.TotQty - WHBINLOC.TotAlo
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = laTStk[lnCount]
  ENDFOR
  laOTStk[9] =laTStk[9] 
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
    laOTStk[lnCount] = 0
  ENDFOR
  laOTStk[9] = 0
  laTStk[9] = 0
ENDIF

*C200861,1 NNA 09/21/2007 (Begin) Call a function to move all stock from a bin to another that is if the customer is panache
IF ASCAN(laEvntTrig,PADR("MOVEALL",10)) <> 0 AND (lcProgName=='ICSTYTR')
  lcMyCursor = lcTmpAdj+"M"
  IF !USED(lcMyCursor)
    CREATE CURSOR (lcMyCursor) (MOVEALL L(1))
    APPEND BLANK
    REPLACE &lcMyCursor..MOVEALL WITH .F.
  ENDIF
  =lfMoveAll()
ENDIF
*C200861,1 (End)
=lfRefresh('ICINVT_A')
RETURN .T.
*-- End of Function lfTRVLDSTY.
*!***************************************************************************
*!* Name        : lfMoveAll
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/20/2007
*!* Purpose     : in the transfer inventory screen if user choose to move all
*!*             : inventory from a bin to another
*!* Ticket/CP#  : T20070824.0004 / CP#200861   For Panache
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfMoveAll()
*!***************************************************************************
FUNCTION lfMoveAll
STORE 0 TO lnchoose 
lcMsg2 = 'Do you want to move all the stock for style : ' + m.style +' from Bin Location (' +;
          ALLTRIM(&lcFileNamA..LocFrom) + ') to Bin Location (' + ALLTRIM(&lcFileNamA..LocTo) + ') ? '
lnchoose =gfModalGen("QRM00000B42002","ALERT",.F.,.F.,lcMsg2)
IF lnchoose = 1  &&If user chose yes to move all stock
  IF SEEK(lcFromWare+&lcFileNamA..LocFrom+M.Style,'WHBINLOC')
    FOR lnCount = 1 TO 8 
      lcCount = ALLTRIM(STR(lnCount))
        m.stk&lcCount =WHBINLOC.Qty&lcCount
        laTStk[lnCount] = m.Stk&lcCount
    ENDFOR
    m.tOTstk =WHBINLOC.TotQty
    laTStk[9] = m.Totstk
  ELSE
    FOR lnCount = 1 TO 8 
      lcCount = ALLTRIM(STR(lnCount))
      m.Stk&lcCount    = 0
      laTStk[lnCount]  = 0
      laOTStk[lnCount] = 0
    ENDFOR
    laTStk[9] = 0
    m.tOTstk  =0
    laOTStk[9] = 0
  ENDIF
  IF SEEK(lcToWare+&lcFileNamA..LocTo+M.Style,'WHBINLOC')
    FOR lnCount = 1 TO 8 
      lcCount = ALLTRIM(STR(lnCount))
      laTStk[lnCount]  =WHBINLOC.Qty&lcCount - WHBINLOC.Alo&lcCount
      laOTStk[lnCount] = laTStk[lnCount]
    ENDFOR
    laTStk[9]  =WHBINLOC.TotQty - WHBINLOC.TotAlo
    laOTStk[9] =laTStk[9] 
  ELSE
    FOR lnCount = 1 TO 8 
      lcCount = ALLTRIM(STR(lnCount))
      laTStk[lnCount] = 0
      laOTStk[lnCount] = 0
    ENDFOR
    laOTStk[9] = 0
    laTStk[9] = 0
  ENDIF
  FOR lnCont=1 TO 8
    lcCont=STR(lnCont,1)   
    *-- Fill the transfer fileds with the available stosk at every size
    m.ADJ&lcCont = m.STK&lcCont
    SHOW GET m.ADJ&lcCont DISABLE
    *--call the standard function to do as standard
    DO LFVADJ IN (gcAppHome+'IC\ICINVTA') WITH lcCont
  ENDFOR
  m.TOTADJ = m.TOTSTK
  REPLACE &lcMyCursor..MOVEALL WITH .T.
ENDIF
RETURN
*-- End of Function lfMoveAll.
*!***************************************************************************
*!* Name        : lfMoveAlo
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 09/20/2007
*!* Purpose     : in the transfer inventory screen if user choose to move all
*!*             : inventory from a bin to another this function will move all
*!*             : allcotions numbers to the second Bin and check the pikbinloc file
*!* Ticket/CP#  : T20070824.0004 / CP#200861   For Panache
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfMoveAlo()
*!***************************************************************************
FUNCTION lfMoveAlo
IF !lfOpnFiles("PKBINLOC","PKBINLOC",'')
  RETURN
ENDIF
*--Get tle allocated figures from Locfrom
IF SEEK(lcFromWare+&lcFileNamA..LocFrom+Style.Style,'WHBINLOC')
  SELECT WHBINLOC
  STORE 0 TO MOVALO1,MOVALO2,MOVALO3,MOVALO4,MOVALO5,MOVALO6,MOVALO7,MOVALO8,MOVTOTALO
  FOR R=1 TO 8
    lcR = STR(R,1)
    MOVALO&lcR = WHBINLOC.ALO&lcR
  ENDFOR
  MOVTOTALO = MOVALO1+MOVALO2+MOVALO3+MOVALO4+MOVALO5+MOVALO6+MOVALO7+MOVALO8
  
  *--Update tle allocated figures in LocTo
  IF SEEK(lcToWare+&lcFileNamA..LocTo+Style.Style,'WHBINLOC')    
    FOR R=1 TO 8
      lcR = STR(R,1)
      REPLACE WHBINLOC.ALO&lcR WITH MOVALO&lcR 
    ENDFOR
    REPLACE WHBINLOC.TOTALO WITH MOVTOTALO
  ELSE
    SELECT WHBINLOC
    APPEND BLANK
    REPLACE STYLE      WITH Style.Style          ,;
            CWARECODE  WITH lcToWare             ,;
            clocation  WITH &lcFileNamA..LocTo   ,;
            TotQty     WITH 0
    FOR R=1 TO 8
      lcR = STR(R,1)
      REPLACE QTY&lcR WITH 0   ,;
              ALO&lcR WITH MOVALO&lcR 
    ENDFOR
    REPLACE WHBINLOC.TOTALO WITH MOVTOTALO
    lcOldOrd = ORDER('WHSLOC')
    SET ORDER TO TAG WhsLoc IN  WHSLOC
    IF SEEK(lcToWare + &lcFileNamA..LocTo + SPACE(19),'WHSLOC') 
      SELECT WHBINLOC
      REPLACE cBlkPck   WITH WHSLOC.cBlkPck  ,;
              cSection  WITH WHSLOC.cSection ,;
              cBinClass WITH WHSLOC.cBinClass
    ENDIF
    SET ORDER TO TAG &lcOldOrd IN  WHSLOC
    =gfAdd_Info('WHBINLOC')
  ENDIF
  *--Check the pkbinloc to change the Clocation field from FromLoc To LocTo
  IF SEEK(lcFromWare+&lcFileNamA..LocFrom+Style.Style,'PKBINLOC')
     SELECT PKBINLOC
     REPLACE CLOCATION WITH &lcFileNamA..LocTo  ;
             FOR CWARECODE+CLOCATION+STYLE = lcFromWare+&lcFileNamA..LocFrom+Style.Style ;
             AND !LINVOICED

  ENDIF
ENDIF  
*-- End of Function lfMoveAlo.
*!***************************************************************************
*!* Name        : lfTRMDSTY
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validate Style in case of IC Transfeer (modify ).
*!***************************************************************************
*!* Called from : ICSTYTR.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfTRMDSTY()
*!***************************************************************************
FUNCTION lfTRMDSTY
PRIVATE lcFileNama,lnCurAlias
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN .F.
ENDIF
lcFileNama = lcTmpAdj+"A"
lnCurAlias = SELECT()

IF SEEK(lcFromWare+&lcFileNamA..LocFrom+M.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.tOTstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laTStk[9] = m.Totstk
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    m.Stk&lcCount    = 0
    laTStk[lnCount]  = 0
    laOTStk[lnCount] = 0
  ENDFOR
  m.tOTstk   = 0
  laOTStk[9] = 0
  laTStk[9]  = 0
ENDIF
IF SEEK(lcToWare+&lcFileNamA..LocTo+M.Style,'WHBINLOC')
  laTStk[1]  =WHBINLOC.Qty1 - WHBINLOC.Alo1 +m.adj1
  laTStk[2]  =WHBINLOC.Qty2 - WHBINLOC.Alo2 +m.adj2
  laTStk[3]  =WHBINLOC.Qty3 - WHBINLOC.Alo3 +m.adj3
  laTStk[4]  =WHBINLOC.Qty4 - WHBINLOC.Alo4 +m.adj4
  laTStk[5]  =WHBINLOC.Qty5 - WHBINLOC.Alo5 +m.adj5
  laTStk[6]  =WHBINLOC.Qty6 - WHBINLOC.Alo6 +m.adj6
  laTStk[7]  =WHBINLOC.Qty7 - WHBINLOC.Alo7 +m.adj7
  laTStk[8]  =WHBINLOC.Qty8 - WHBINLOC.Alo8 +m.adj8
  laTStk[9]  =WHBINLOC.TotQty - WHBINLOC.TotAlo +m.Totadj
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laOTStk[lnCount] = laTStk[lnCount]
  ENDFOR
  laOTStk[9] =laTStk[9] 
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount] = 0
    laOTStk[lnCount] = 0
  ENDFOR
  laOTStk[9] = 0
  laTStk[9] = 0
ENDIF
SELECT (lnCurAlias)
*-- End of Function lfTRMDSTY.
*!***************************************************************************
*!* Name        : lfDLCKSVIC
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validate Ic Adj.
*!***************************************************************************
*!* Called from : IcStyAd.prg , IcStyPh.Prg , IcStyTr.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLCKSVIC()
*!***************************************************************************
FUNCTION lfDLCKSVIC
PRIVATE llmessag , lcCondition , lcFileNama
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN .F.
ENDIF
lcFileNama = lcTmpAdj+"A"
llmessag = .F.
SELECT (lcFileNamA)
LOCATE
lcCondition = IIF(lcType = 'T',;
              IIF(llMultiWH,"EMPTY(&lcFileNamA..LocFrom) OR EMPTY(&lcFileNamA..LocTO)","EMPTY(LCFROMWARE)"),;
              IIF(llMultiWH,"EMPTY(&lcFileNamA..LocFrom )","EMPTY(LCFROMWARE)"))
IF &lcCondition
  lcMsg2 = 'You can not add style without assign bin location.'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llmessag = .T.
ENDIF
RETURN llmessag
*-- End of Function lfDLCKSVIC.
*!***************************************************************************
*!* Name        : lfDLGETBIN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Screen of get bins in case of Inventory adj.,Transfer and 
*!*             : Physical Inventory.
*!***************************************************************************
*!* Called from : ICDLBIN.SPX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLGETBIN()
*!***************************************************************************
FUNCTION lfDLGETBIN
lnAlias = SELECT()
IF !lfOpnFiles("WHSLOC,WHBINLOC","WHSLOC,WHBINLOC","")
  RETURN
ENDIF
IF !SEEK(lcWareH ,'WHSLOC')
  SET ORDER TO TAG WhsLocSt IN  WHSLOC
  RETURN
ENDIF
SET ORDER TO TAG WhsLocSt IN  WHSLOC
lcBinLoc = ''
DIME laTempData[3]
STORE '' TO laTempData
DO (gcScrDir+gcWinAppl+"\ICDLBIN.SPX")
SELECT(lnAlias)
*-- End of Function lfDLGETBIN.
*!***************************************************************************
*!* Name        : lfvbinloc
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validation on Bin Location Selection in ICDLBIN.SPX
*!***************************************************************************
*!* Called from : ICDLBIN.SPX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvbinloc()
*!***************************************************************************
FUNCTION lfvbinloc
PRIVATE llFromSeek
STORE .F.TO llFromSeek
SELECT WhsLoc
IF EMPTY(lcBinLoc) AND !llbrowse
  RETURN
ENDIF
SET ORDER TO TAG WhsBin IN  WHSLOC
IF ASCAN(laEvntTrig,PADR("DLGETBIN",10)) <> 0 AND !(lcProgName=='ICINVLK')
  lcWarehs = IIF(lcType = 'T',IIF(lnWareNm = 1,lcFromWare,lcToWare),lcFromWare)
ELSE
  lcType = ''
  lcWarehs = laData[8]
ENDIF
IF llbrowse OR (!EMPTY(lcBinLoc) AND !SEEK(lcBinLoc+lcWarehs))
  llbrowse =.F. 
  lcBrFields = "clocation  :H='Bin',cBinClass  :H='Bin Class',cFlatHang  :H='Flat or Hang'"
  lcForExpr = " cWareCode = lcWareH  AND EMPTY(Style)"
  STORE '' TO laTempData
  =SEEK(ALLTRIM(lcBinLoc))
  =gfBrows([FOR &lcForExpr],'cLocation,cBinClass,cFlatHang','laTempData','WhsLoc')
  lcBinLoc=laTempData[1]
ELSE
  laTempData[1] = lcBinLoc
  laTempData[2] = WhsLoc.cbinclass
  laTempData[3] = WhsLoc.cflathang
  STORE .T.TO llFromSeek
ENDIF

SET ORDER TO TAG WhsLocSt IN  WHSLOC
PRIVATE lcFileNama
IF ASCAN(laEvntTrig,PADR("DLGETBIN",10)) <> 0 AND !(lcProgName=='ICINVLK')
  lcFileNama = lcTmpAdj+"A"
ENDIF
IF lcType = 'T'
  IF lnWareNm = 1
    IF !SEEK(lcWareH +ALLTRIM(lcBinLoc),'WHBINLOC')
      lcMsg2 = 'No stock available to transfer'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      lcBinLoc = ''
      _CUROBJ = OBJNUM(pbCan)
      RETURN      
    ENDIF
  ELSE
    IF &lcFileNamA..LocFrom = lcBinLoc And lcFromWare = lcToWare
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot transfer to the same bin Location'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      lcBinLoc = ''
      _CUROBJ = OBJNUM(pbCan)
      RETURN
    ENDIF

    *C200861,1 NNA 09/21/2007 (Begin) do this check only if the customer is not panache
    IF ASCAN(laEvntTrig,PADR("MOVEALL",10)) = 0 
    *C200861,1 (End)

      IF !(&lcFileNamA..FlatFrom = laTempData[3])
        *--You cannot transfer from the same warehouse.
        lcMsg2 = 'You cannot transfer the style to a bin of a different Hang/Flat.'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        lcBinLoc = ''
        _CUROBJ = OBJNUM(pbCan)
        RETURN
      ENDIF

    *C200861,1 NNA 09/21/2007 (Begin) End of if statment
    ENDIF
    *C200861,1 (End)

  ENDIF
ENDIF
IF llFromSeek
 =lfvOk()
ENDIF
*-- End of Function lfvbinloc.
*!***************************************************************************
*!* Name        : lfvOk
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validation on Bin Location Selection in ICDLBIN.SPX
*!***************************************************************************
*!* Called from : ICDLBIN.SPX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvOk()
*!***************************************************************************
FUNCTION lfvOk
PRIVATE lcFileName
IF EMPTY(lcBinLoc)
  lcMsg2 = 'You have to select a bin location'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  _CUROBJ = OBJNUM(lcBinLoc)
  llLoc  = .F.
  laScrMode = .F.
  laScrMode[1] = .T.
  llCUpdate = .F.
  SHOW GETS
  CLEAR READ  
  RETURN
ENDIF
*--If this code called from the inventory locking then go out to handle it from another function
IF ASCAN(laEvntTrig,PADR("DLGETBIN",10)) <> 0 AND !(lcProgName=='ICINVLK')
  lcFileName = lcTmpAdj+"A"
ELSE

  =lfOkStyBin()
  SHOW GETS
  CLEAR READ  
  RETURN
ENDIF
IF lcType = 'T'
  IF lnWareNm = 1
    REPLACE  &lcFileName..LocFrom    WITH lcBinLoc      ,;
             &lcFileName..ClassFrom  WITH laTempData[2] ,;
             &lcFileName..FlatFrom   WITH laTempData[3]
  ELSE
    REPLACE  &lcFileName..LocTo    WITH lcBinLoc        ,;
             &lcFileName..ClassTO  WITH laTempData[2]   ,;
             &lcFileName..FlatTo   WITH laTempData[3]
  ENDIF
ELSE
  REPLACE  &lcFileName..LocFrom  WITH lcBinLoc
ENDIF
CLEAR READ
*-- End of Function lfvOk.
*!***************************************************************************
*!* Name        : lfInitVar   
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : To Initiate variables for the new modification
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg , ICSTYTR.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfInitVar()
*!***************************************************************************
FUNCTION lfInitVar   
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN
ENDIF
lcFileNamA = lcTmpAdj+"A"
IF !USED(lcFileNamA)
  CREATE CURSOR (lcFileNamA) (LocFrom C(10),LocTo C(10),ClassFrom C(2), ClassTo C(2), FlatFrom C(1), FlatTo C(1))
  APPEND BLANK
ENDIF  
*-- End of Function lfInitVar.
*!***************************************************************************
*!* Name        : lfDlModBrw
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validate Style in case of IC Adj.
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg , ICSTYTR.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDlModBrw()
*!***************************************************************************
FUNCTION lfDlModBrw
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN
ENDIF
PRIVATE lcFileNama
lcFileNama = lcTmpAdj+"A"
IF llDyelot
  IF lcType = 'T'
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R ,"+;
                 "STYLE.DESC1 :24 :H='Description':10 :R,"+;
                 "Dyelot      :19 :H='Dyelot':R,"+;
                 "TotOld      :10 :H=' '+lcFromWare :R,"+;
                 "Total1 = TotAdj      :10 :H='B/Location '+&lcFileNama..LocFrom :R,"+;
                 "TotAdj      :10 :H='Transf.':R,"+;
                 "Total=(IIF(SEEK(Style+lcToWare+Dyelot,'STYDYE'),STYDYE.TotStk,0)+TotAdj) :10 :P='9999999' :H=' '+lcToWare :R,"+;
                 "Total2 = TotAdj  :10 :P='9999999' :H='B/Location '+&lcFileNama..locTo :R "
  ELSE
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE        :30 :H=lcStyHdr :R ,"+;
                 "STYLE.DESC1  :24 :H='Description' :R,"+;
                 "Dyelot       :19 :H='Dyelot':R,"+;
                 "TotOld       :10  :H='Existing':R,"+;
                 "Old_Cost     :09:P='999999.99'  :H='Old Cost':R,"+;
                 "&lcFileNama..LocFrom    :10 :H='Bin Location' :R,"+;   
                 "TotAdj       :10  :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Unt_Cost     :09:P='999999.99'  :H='New Cost':R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :10:P='9999999' :H='  Total':R "
  ENDIF               
ELSE
  IF lcType = 'T'
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R,"+;
                 "STYLE.DESC1 :24 :H='Description':R,"+;
                 "TotOld      :12 :H=' '+lcFromWare :R,"+;
                 "Total1 = TotAdj       :12 :H='B/Location '+&lcFileNama..locFrom :R,"+;
                 "TotAdj      :12 :H='Transfer':R,"+;
                 "nTotOldTo   :12 :P='9999999' :H=' '+lcToWare :R,"+;
                 "Total2 = TotAdj   :12 :P='9999999' :H='B/Location '+&lcFileNama..LocTo :R "
  ELSE
    lcBfields = [cDummi=IIF(RECNO()=lnCurrRec,'>',' ') :1 :H=' ',]+;
                 "STYLE       :30 :H=lcStyHdr :R,"+;
                 "STYLE.DESC1 :24 :H='Description':R,"+;
                 "TotOld      :12 :H='Existing':R,"+;
                 "Old_Cost    :09:P='999999.99' :H='Old Cost':R,"+;
                 "&lcFileNama..locFrom  :12 :H='B/Location  ' :R,"+;
                 "TotAdj      :12 :H=IIF(lcType='P','New Inv','Adj +\-'):R,"+;
                 "Unt_Cost    :09:P='999999.99' :H='New Cost':R,"+;
                 "Total=IIF(lcType='P',TotAdj,(TotOld+TotAdj)) :12 :P='9999999' :H='  Total':R "
  ENDIF               
ENDIF
*-- End of Function lfDlModBrw.
*!***************************************************************************
*!* Name        : lfDlSavDat
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Save Data
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg , ICSTYTR.Prg -->ICINVSAV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDlSavDat()
*!***************************************************************************
FUNCTION lfDlSavDat
PRIVATE lcFileNama
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN
ENDIF
lcFileNama = lcTmpAdj+"A"
IF !lfOpnFiles("STYINVJL,BININVJL,WHBINLOC,WHSLOC","STYINVJL,STYINVJL,WHBINLOC,WHSLOC","")
  RETURN
ENDIF
SELECT (lcTmpAdj)
LOCATE
IF lcType = 'T'
  SCAN
    *--from location
    IF SEEK(&lcTmpAdj..cfromware +&lcFileNamA..LocFrom +&lcTmpAdj..Style,'WHBINLOC') 
      SELECT WHBINLOC
      REPLACE Qty1       WITH MAX(Qty1- &lcTmpAdj..adj1,0),;
              Qty2       WITH MAX(Qty2- &lcTmpAdj..adj2,0),;
              Qty3       WITH MAX(Qty3- &lcTmpAdj..adj3,0),;
              Qty4       WITH MAX(Qty4- &lcTmpAdj..adj4,0),;
              Qty5       WITH MAX(Qty5- &lcTmpAdj..adj5,0),;
              Qty6       WITH MAX(Qty6- &lcTmpAdj..adj6,0),;
              Qty7       WITH MAX(Qty7- &lcTmpAdj..adj7,0),;
              Qty8       WITH MAX(Qty8- &lcTmpAdj..adj8,0),;
              TotQty     WITH MAX(TotQty- &lcTmpAdj..Totadj,0)
      IF TotQty = 0
        DELETE
      ENDIF
    ENDIF
    *--To location
    IF !SEEK(&lcTmpAdj..cToware +&lcFileNamA..LocTo +&lcTmpAdj..Style,'WHBINLOC') 
      SELECT WHBINLOC
      APPEND BLANK
      REPLACE STYLE      WITH &lcTmpAdj..Style     ,;
              CWARECODE  WITH &lcTmpAdj..cToware   ,;
              clocation  WITH &lcFileNamA..LocTo   ,;
              Qty1       WITH Qty1+ &lcTmpAdj..adj1,;
              Qty2       WITH Qty2+ &lcTmpAdj..adj2,;
              Qty3       WITH Qty3+ &lcTmpAdj..adj3,;
              Qty4       WITH Qty4+ &lcTmpAdj..adj4,;
              Qty5       WITH Qty5+ &lcTmpAdj..adj5,;
              Qty6       WITH Qty6+ &lcTmpAdj..adj6,;
              Qty7       WITH Qty7+ &lcTmpAdj..adj7,;
              Qty8       WITH Qty8+ &lcTmpAdj..adj8,;
              TotQty     WITH TotQty+ &lcTmpAdj..Totadj

        *--Update cBlkPck and cSection
        lcOldOrd = ORDER('WHSLOC')
        SET ORDER TO TAG WhsLoc IN  WHSLOC
        IF SEEK(&lcTmpAdj..cfromware +&lcFileNamA..LocFrom +SPACE(19),'WHSLOC') 
          SELECT WHBINLOC
          REPLACE cBlkPck   WITH WHSLOC.cBlkPck  ,;
                  cSection  WITH WHSLOC.cSection ,;
                  cBinClass WITH WHSLOC.cBinClass
        ENDIF
        SET ORDER TO TAG &lcOldOrd IN  WHSLOC
      =gfAdd_Info('WHBINLOC')
    ELSE
      SELECT WHBINLOC
      REPLACE Qty1   WITH Qty1+ &lcTmpAdj..adj1  ,;
              Qty2   WITH Qty2+ &lcTmpAdj..adj2  ,;
              Qty3   WITH Qty3+ &lcTmpAdj..adj3  ,;
              Qty4   WITH Qty4+ &lcTmpAdj..adj4  ,;
              Qty5   WITH Qty5+ &lcTmpAdj..adj5  ,;
              Qty6   WITH Qty6+ &lcTmpAdj..adj6  ,;
              Qty7   WITH Qty7+ &lcTmpAdj..adj7  ,;
              Qty8   WITH Qty8+ &lcTmpAdj..adj8  ,;
              TotQty WITH TotQty+ &lcTmpAdj..Totadj
    ENDIF
    SELECT WHBINLOC
    IF TotQty = 0
      DELETE
    ENDIF         
  ENDSCAN 
ELSE
  SCAN
    IF !SEEK(&lcTmpAdj..cfromware +&lcFileNamA..LocFrom +&lcTmpAdj..Style,'WHBINLOC') 
      SELECT WHBINLOC
      APPEND BLANK
      REPLACE STYLE      WITH &lcTmpAdj..Style,;
              CWARECODE  WITH &lcTmpAdj..cfromware,;
              clocation  WITH &lcFileNamA..LocFrom ,;
              Qty1       WITH Qty1+ &lcTmpAdj..adj1,;
              Qty2       WITH Qty2+ &lcTmpAdj..adj2,;
              Qty3       WITH Qty3+ &lcTmpAdj..adj3,;
              Qty4       WITH Qty4+ &lcTmpAdj..adj4,;
              Qty5       WITH Qty5+ &lcTmpAdj..adj5,;
              Qty6       WITH Qty6+ &lcTmpAdj..adj6,;
              Qty7       WITH Qty7+ &lcTmpAdj..adj7,;
              Qty8       WITH Qty8+ &lcTmpAdj..adj8,;
              TotQty     WITH TotQty+ &lcTmpAdj..Totadj

        *--Update cBlkPck and cSection
        lcOldOrd = ORDER('WHSLOC')
        SET ORDER TO TAG WhsLoc IN  WHSLOC
        IF SEEK(&lcTmpAdj..cfromware +&lcFileNamA..LocFrom +SPACE(19),'WHSLOC') 
          SELECT WHBINLOC
          REPLACE cBlkPck   WITH WHSLOC.cBlkPck  ,;
                  cSection  WITH WHSLOC.cSection ,;
                  cBinClass WITH WHSLOC.cBinClass
        ENDIF
        SET ORDER TO TAG &lcOldOrd IN  WHSLOC
      =gfAdd_Info('WHBINLOC')
      IF TotQty = 0
        DELETE
      ENDIF         
    ELSE
      IF lcType = 'P'
        SELECT WHBINLOC
        REPLACE Qty1       WITH MAX(&lcTmpAdj..adj1,0),;
                Qty2       WITH MAX(&lcTmpAdj..adj2,0),;
                Qty3       WITH MAX(&lcTmpAdj..adj3,0),;
                Qty4       WITH MAX(&lcTmpAdj..adj4,0),;
                Qty5       WITH MAX(&lcTmpAdj..adj5,0),;
                Qty6       WITH MAX(&lcTmpAdj..adj6,0),;
                Qty7       WITH MAX(&lcTmpAdj..adj7,0),;
                Qty8       WITH MAX(&lcTmpAdj..adj8,0),;
                TotQty     WITH MAX(&lcTmpAdj..Totadj,0)
        IF TotQty = 0
          DELETE
        ENDIF         
      ELSE
        SELECT WHBINLOC
        REPLACE Qty1       WITH MAX(Qty1+ &lcTmpAdj..adj1,0),;
                Qty2       WITH MAX(Qty2+ &lcTmpAdj..adj2,0),;
                Qty3       WITH MAX(Qty3+ &lcTmpAdj..adj3,0),;
                Qty4       WITH MAX(Qty4+ &lcTmpAdj..adj4,0),;
                Qty5       WITH MAX(Qty5+ &lcTmpAdj..adj5,0),;
                Qty6       WITH MAX(Qty6+ &lcTmpAdj..adj6,0),;
                Qty7       WITH MAX(Qty7+ &lcTmpAdj..adj7,0),;
                Qty8       WITH MAX(Qty8+ &lcTmpAdj..adj8,0),;
                TotQty     WITH MAX(TotQty+ &lcTmpAdj..Totadj,0)
        IF TotQty = 0
          DELETE
        ENDIF         
      ENDIF  
    ENDIF
  ENDSCAN 
ENDIF
*-- End of Function lfDlSavDat.
*!***************************************************************************
*!* Name        : lfDLVLDBIN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Return if called from custom Bin system
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg , ICSTYTR.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLVLDBIN()
*!***************************************************************************
FUNCTION lfDLVLDBIN
IF gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN
ENDIF
lcMsg2 = 'Sorry, bins cannot be assigned to styles here. You can assign a bin(s) to styles from the Location screen'
=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)

*-- End of Function lfDLVLDBIN.
*!***************************************************************************
*!* Name        : lfDLVLDSTY
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validate Style in case of IC Adj. and IC Physical.
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLVLDSTY()
*!***************************************************************************
FUNCTION lfDLVLDSTY
PRIVATE lcFileNama
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN .F.
ENDIF
lcFileNama = lcTmpAdj+"A"
IF !lfOpnFiles("WHSLOC,WHBINLOC","WHSLOC,WHBINLOC","")
  RETURN
ENDIF
IF !SEEK(lcFromWare+&lcFileNamA..LocFrom+m.style,'WHSLOC')
  =SEEK(lcFromWare+&lcFileNamA..LocFrom,'WHSLOC')
  lcLocSty = m.style
  SCATT MEMVAR MEMO
  m.cWareCode = lcFromWare
  m.style     = lcLocSty
  m.clocation = &lcFileNamA..LocFrom
  INSERT INTO WHSLOC FROM MEMVAR 
ENDIF
*C200861,1 NNA 09/21/2007 (Begin) Call a function to move all stock from a bin to another
IF ASCAN(laEvntTrig,PADR("MOVEALL",10)) = 0 AND (lcProgName=='ICSTYTR')
*C200861,1 (End)

  =SEEK(lcFromWare+&lcFileNamA..LocFrom,'WHSLOC')
  IF !(WhsLoc.cFlatHang = style.cflathang)
    SET ORDER TO TAG WhsLocSt IN WHSLOC 
    *--You cannot transfer from the same warehouse.
    lcMsg2 = 'You cannot Adjust a style with a bin of a different Hang/Flat.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    RETURN
  ENDIF
  llClass = .F.
  FOR lnCount = 1 TO 8
    IF llClass  
      EXIT
    ENDIF
    lcCount = STR(lnCount,1)
    IF (WhsLoc.cBinClass = style.cPrimClss&lcCount) .OR. ;
       (WhsLoc.cBinClass = style.cSecClss&lcCount)  .OR. ;
       (WhsLoc.cBinClass = style.cRemClss&lcCount)
      llClass = .T.
    ENDIF
  ENDFOR
  IF !llClass
    *--You cannot transfer from the same warehouse.
    SET ORDER TO TAG WhsLocSt IN WHSLOC 
    lcMsg2 = 'You cannot Adjust a style with a bin of a different class.'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    RETURN
  ENDIF

*C200861,1 NNA 09/21/2007 (Begin) End of if Statment
ENDIF
*C200861,1 (End)

SET ORDER TO TAG WhsLocSt IN WHSLOC 
IF !llDyelot OR Style.cDye_Flg<>'Y'
  =lfGetInfo()
ELSE
  lcStyDesc = Desc1
  SHOW GET ibDye    ENABLE
  SHOW GET m.Dyelot ENABLE
ENDIF
IF SEEK(lcFromWare+&lcFileNamA..LocFrom+m.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.Totstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  IF lcType = 'P'
    m.Adj1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
    m.Adj2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
    m.Adj3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
    m.Adj4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
    m.Adj5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
    m.Adj6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
    m.Adj7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
    m.Adj8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
    m.TotAdj =WHBINLOC.TotQty - WHBINLOC.TotAlo
  ENDIF  
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount]  = m.Stk&lcCount
    laOTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laOTStk[9] = m.Totstk
  laTStk[9] = m.Totstk
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    m.Stk&lcCount    = 0
    laTStk[lnCount]  = 0
    laOTStk[lnCount] = 0
    IF lcType = 'P'
      m.Adj&lcCount = 0
      m.TotAdj = 0
    ENDIF
  ENDFOR
  laOTStk[9] = 0
  m.tOTstk   = 0
  laTStk[9]  = 0
ENDIF
SHOW GET    m.Adj1   ENABLE
SHOW GET    m.Adj2   ENABLE
SHOW GET    m.Adj3   ENABLE
SHOW GET    m.Adj4   ENABLE
SHOW GET    m.Adj5   ENABLE
SHOW GET    m.Adj6   ENABLE
SHOW GET    m.Adj7   ENABLE
SHOW GET    m.Adj8   ENABLE
=lfRefresh('ICINVT_A')
RETURN .T.
*-- End of Function lfDLVLDSTY.
*!***************************************************************************
*!* Name        : lfDLMDYSTY
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validate in case of Modify IC Adj. And Physical.
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg
*!***************************************************************************
*!* Calls       : None
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLMDYSTY()
*!***************************************************************************
FUNCTION lfDLMDYSTY
PRIVATE lcFileNama ,lnCurAlias
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN .F.
ENDIF
lcFileNama = lcTmpAdj+"A"
lnCurAlias = SELECT()

IF SEEK(lcFromWare+&lcFileNamA..LocFrom+m.Style,'WHBINLOC')
  m.stk1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
  m.stk2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
  m.stk3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
  m.stk4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
  m.stk5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
  m.stk6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
  m.stk7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
  m.stk8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
  m.Totstk =WHBINLOC.TotQty - WHBINLOC.TotAlo
  IF lcType = 'P'
    m.Adj1   =WHBINLOC.Qty1 - WHBINLOC.Alo1
    m.Adj2   =WHBINLOC.Qty2 - WHBINLOC.Alo2
    m.Adj3   =WHBINLOC.Qty3 - WHBINLOC.Alo3
    m.Adj4   =WHBINLOC.Qty4 - WHBINLOC.Alo4
    m.Adj5   =WHBINLOC.Qty5 - WHBINLOC.Alo5
    m.Adj6   =WHBINLOC.Qty6 - WHBINLOC.Alo6
    m.Adj7   =WHBINLOC.Qty7 - WHBINLOC.Alo7
    m.Adj8   =WHBINLOC.Qty8 - WHBINLOC.Alo8
    m.TotAdj =WHBINLOC.TotQty - WHBINLOC.TotAlo
  ENDIF  
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    laTStk[lnCount]  = m.Stk&lcCount
    laOTStk[lnCount] = m.Stk&lcCount
  ENDFOR
  laTStk[9]  = m.Totstk
  laOTStk[9] = m.Totstk
ELSE
  FOR lnCount = 1 TO 8 
    lcCount = ALLTRIM(STR(lnCount))
    IF lcType = 'P'
      m.Adj&lcCount = 0
      m.TotAdj = 0
    ENDIF
    m.Stk&lcCount    = 0
    laTStk[lnCount]  = 0
    laOTStk[lnCount] = 0
  ENDFOR
  laTStk[9]  = 0
  laOTStk[9] = 0
  m.tOTstk   = 0
ENDIF
SELECT(lnCurAlias)
*-- End of Function lfDLMDYSTY.
*!***************************************************************************
*!* Name        : lfDLSVBNI
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update BinInvJL & from issue Styinvjl
*!***************************************************************************
*!* Called from : ICSTYAD.Prg , ICSTYPH.Prg , ICSTYTR.Prg
*!***************************************************************************
*!* Calls       : lfOpnFiles()
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSVBNI()
*!***************************************************************************
FUNCTION lfDLSVBNI
PRIVATE lcStyOrd,lnAlias,lcFileNama
lnAlias = SELECT(0)
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN
ENDIF
lcFileNama = lcTmpAdj+"A"
IF !lfOpnFiles("STYINVJL,BININVJL","STYINVJL,STYINVJL","")
  RETURN
ENDIF
SELECT STYINVJL 
lnCurrec = RECNO()
lcsty1 = Style
lcWarhos = &lcTmpAdj..cFromWare
IF !SEEK(Style+&lcTmpAdj..cFromWare+lcIsuSessNo)
  lcWarhos = &lcTmpAdj..cToWare
  =SEEK(lcsty1 + lcWarhos + lcIsuSessNo)
ENDIF
SCAN WHILE Style+cWareCode+cSession = lcsty1 + lcWarhos + lcIsuSessNo
  SCATT MEMVAR MEMO
  IF cirtype = 'I'
    m.clocation = &lcFileNama..LocFrom
  ELSE
    m.clocation = &lcFileNama..LocTO
    IF EMPTY(&lcFileNama..LocTO)
      m.clocation = &lcFileNama..LocFrom
    ENDIF  
  ENDIF  
  SELECT BININVJL
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN
SELECT STYINVJL 
IF BETWEEN(lnCurrec,1,RECCOUNT())
  GOTO lnCurrec IN STYINVJL
ENDIF
SELECT(lnAlias)
*-- End of Function lfDLSVBNI.
*!***************************************************************************
*!* Name        : lfDLSVBNR
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update BinInvJL & from RECEIVE Styinvjl
*!***************************************************************************
*!* Called from : ICINVSAV.PRG --> FOR PRGS. ICSTYTR.PRG,ICSTYPH.PRG,ICSYTAD.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSVBNR()
*!***************************************************************************
FUNCTION lfDLSVBNR
PRIVATE lcStyOrd,lnAlias,lcFileNama ,lnCrRec 
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN
ENDIF
lnAlias = SELECT(0)
lcFileNama = lcTmpAdj+"A"
*--Open the needed files
IF !lfOpnFiles("STYINVJL,BININVJL","STYINVJL,STYINVJL","")
  RETURN
ENDIF

*C200861,1 NNA 09/21/2007 (Begin) Call a function to move all allocated numbers from a bin to another
IF ASCAN(laEvntTrig,PADR("MOVEALL",10)) <> 0 AND (lcProgName=='ICSTYTR') 
  lcMyCursor = lcTmpAdj+"M"
  IF &lcMyCursor..MOVEALL
    =lfMoveAlo()
  ENDIF
ENDIF
*C200861,1 NNA (END)

SELECT STYINVJL
lnCrRec = RECNO()
IF SEEK(Style+&lcTmpAdj..cToWare+lcRcvSessNo)
  SCATT MEMVAR MEMO
  m.clocation = &lcFileNama..LocTo
  SELECT BININVJL
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDIF
SELECT STYINVJL
IF BETWEEN(lnCrRec,1,RECCOUNT())
  GOTO lnCrRec IN STYINVJL
ENDIF
*-- End of Function lfDLSVBNR.
*!***************************************************************************
*!* Name        : lfDLASSBIN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Assign bin location to temp file in Inventory locking
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLASSBIN()
*!***************************************************************************
FUNCTION lfDLASSBIN
IF gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  IF SEEK(m.STYLE+m.Color,lcTmpQuery) AND m.clocation = &lcTmpQuery..Bin
    m.BIN     = m.clocation
    SELECT (lcTmpQuery)
    REPLACE Stock WITH Stock + m.Stock,oStock WITH oStock + m.oStock
  ELSE
    m.BIN     = m.clocation
    INSERT INTO (lcTmpQuery) FROM MEMVAR
  ENDIF
  *--create Temp to store Bin Location Variable in
  PRIVATE lcFileNamA
  lcFileNamA = lcTmpQuery+"A"
  CREATE CURSOR (lcFileNamA) (cLocation C(10))
  APPEND BLANK
ENDIF
*-- End of Function lfDLASSBIN.
*!***************************************************************************
*!* Name        : lfDLBRWBIN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Add a Bin Location Field to Inventory Locking's Browse Screen
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLBRWBIN()
*!***************************************************************************
FUNCTION lfDLBRWBIN
IF gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  lcBrfield1 = [Style      :H = lcStyPic    ,] +[STYDESC:40 :H = 'Desc.'     ,] +;
               [Bin        :H = 'Bin '      ,] +[OStock     :H = 'Old Stock' ,] +;
               [OCost      :H = 'Old Cost'  ,] +[Stock      :H = 'New Stock' ,] +;
               [Cost       :H = 'New Cost'   ]
ENDIF
*-- End of Function lfDLBRWBIN.
*!***************************************************************************
*!* Name        : lfDLDELALL
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Delete all Records from the Temp File in the inventory 
*!*             : Locking Screen when Saving using Bin Location.
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLDELALL()
*!***************************************************************************
FUNCTION lfDLDELALL
IF gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  DELETE ALL FOR cbattype+cLkBatch+style+color+DYELOT = lcKey+laData[2]+&lcDetLin..Style+&lcDetLin..Color;
         AND &lcBatLin..clocation = &lcDetLin..clocation AND IIF(llDyelot AND lfStyDye(&lcDetLin..Style,&lcDetLin..Color),!EMPTY(&lcBatLin..DYELOT),.T.)
ELSE
  DELETE ALL FOR cbattype+cLkBatch+style+color+DYELOT = lcKey+laData[2]+&lcDetLin..Style+&lcDetLin..Color;
         AND IIF(llDyelot AND lfStyDye(&lcDetLin..Style,&lcDetLin..Color),!EMPTY(&lcBatLin..DYELOT),.T.)
ENDIF
*-- End of Function lfDLDELALL.
*!***************************************************************************
*!* Name        : lfDLPSTLCK
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid function for posting batch
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLPSTLCK()
*!***************************************************************************
FUNCTION lfDLPSTLCK
IF gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  lcBinLoc = MDINVNTL.clocation  
  PRIVATE lcFileNama
  lcFileNamA = lcTmpQuery+"A"
  REPLACE &lcFileNamA..cLocation WITH MDINVNTL.clocation  
  SCAN REST WHILE cbattype+cLkBatch+style+color+DYELOT = lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
       AND clocation  = lcBinLoc FOR   ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0                  ;
       OR  OLDSTK3<>0 OR OLDSTK4<>0 OR OLDSTK5<>0 OR OLDSTK6<>0 OR OLDSTK7<>0 OR OLDSTK8<>0)       ;
       OR  TotStk <> 0 )
    WAIT WINDOW NOWAIT "Posting Batch# " + MDINVNTH.cLkBatch + ;
         IIF(llMatModule,'  Item   - Color \ ' + PADR(STYLE,7)+'-'+COLOR ,lcStyPic +[\ ]+Style)
    lnCountLn = lnCountLn + 1
    =gfThermo(lnTotRec,lnCountLn,'Posting')
    *----Start Calculate Stk for posting
    lcAdjReason = MDINVNTL.cAdjReason
    IF !llMatModule
      FOR lnIndex = 1 TO 8
        lcSub = STR(lnIndex,1)
        laAdjust[lnIndex] = laAdjust[lnIndex] + MDINVNTL.Stk&lcSub
        laOldStk[lnIndex] = laOldStk[lnIndex] + MDINVNTL.OldStk&lcSub
      ENDFOR
    ENDIF
    laAdjust[9]  = laAdjust[9] + MDINVNTL.TOTSTK
    laAdjust[10] = MDINVNTL.COST
    laOldStk[9]  = laOldStk[9] + MDINVNTL.OldTOTSTK
  ENDSCAN
ELSE
  SCAN REST WHILE cbattype+cLkBatch+style+color+DYELOT = lcPType +lcPBatch+lcPStyle+lcPColor+lcDye1;
       FOR ((OldTotStk<>0 OR OLDSTK1<>0 OR OLDSTK2<>0 OR OLDSTK3<>0 OR OLDSTK4<>0 OR OLDSTK5<>0 OR OLDSTK6<>0 OR ;
           OLDSTK7<>0 OR OLDSTK8<>0) OR TotStk <> 0 )
    WAIT WINDOW "Posting Batch# " + MDINVNTH.cLkBatch +IIF(llMatModule,'  Item   - Color \ ' + PADR(STYLE,7)+'-'+COLOR ,;
         lcStyPic +[\ ]+Style)  NOWAIT
    lnCountLn = lnCountLn + 1
    =gfThermo(lnTotRec,lnCountLn,'Posting')
    *----Start Calculate Stk for posting
    lcAdjReason = MDINVNTL.cAdjReason
    IF !llMatModule
      FOR lnIndex = 1 TO 8
        lcSub = STR(lnIndex,1)
        laAdjust[lnIndex] = laAdjust[lnIndex] + MDINVNTL.Stk&lcSub
        laOldStk[lnIndex] = laOldStk[lnIndex] + MDINVNTL.OldStk&lcSub
      ENDFOR
    ENDIF
    laAdjust[9]  = laAdjust[9] + MDINVNTL.TOTSTK
    laAdjust[10] = MDINVNTL.COST
    laOldStk[9]  = laOldStk[9] + MDINVNTL.OldTOTSTK
  ENDSCAN
ENDIF
*-- End of Function lfDLPSTLCK.

*!***************************************************************************
*!* Name        : lfDLSELREC
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Select Bin Location for inventory locking
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSELREC()
*!***************************************************************************
FUNCTION lfDLSELREC
IF gfGetMemVar('M_DLUSEBIN')
  PRIVATE lcOldOrder,lnOldAlias
  lnOldAlias = SELECT(0)
  IF !llMatModul
    lcOldOrder = ORDER(lcTmpQuery)
    SELECT (lcTmpQuery)
    INDEX ON Style+Color+Bin TAG ItmClrBn
    SELECT(lnOldAlias)
    SCAN FOR &lcExpr AND IIF(llWareHus,CwareCode = ladata[8] ,.T.)
      SCAT MEMVAR MEMO
      IF !USED('WHBINLOC')
        =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH')
      ENDIF
      IF SEEK(STYDYE.style+STYDYE.cwarecode,'WHBINLOC')
        SELECT WHBINLOC
        SCAN REST WHILE style+cwarecode+clocation = STYDYE.style+STYDYE.cwarecode
          FOR lnContr = 1 TO 8
            lcContr = STR(lnContr,1)
            m.stk&lcContr = WHBINLOC.Qty&lcContr
          ENDFOR
          m.Totstk = WHBINLOC.TotQty
          m.StyDesc = Style.Desc1
          DO CASE
            CASE  lcCostMeth = 'S'  && Standard
              m.Cost    = STYLE.TotCost
              m.oCost   = STYLE.TotCost
            OTHERWISE       && 'A' Average
              m.Cost    = Ave_Cost
              m.oCost   = Ave_Cost
          ENDCASE
          m.ITEM    = ''
          m.STOCK   = WHBINLOC.TotQty
          m.oSTOCK  = WHBINLOC.TotQty
          m.nStkVal = (Ave_Cost*WHBINLOC.TotQty)
          m.BIN     = WHBINLOC.clocation

          llBin = .T.
          IF llLoc AND !EMPTY(laRpTarget)
            IF !EMPTY(WHBINLOC.clocation) AND ASCAN(laRpTarget,ALLTRIM(WHBINLOC.clocation)) <> 0
              llBin = .T.
            ELSE
              llBin = .F.
            ENDIF
          ENDIF

          *C132193,1 NNA 05/17/2006 (Begin) Filter by section Too
          *IF llBin
          llSec = .T.
          IF llLoc AND !EMPTY(laScTarget)
            IF !EMPTY(WHBINLOC.cSection) AND ASCAN(laScTarget,ALLTRIM(WHBINLOC.cSection)) <> 0
              llSec = .T.
            ELSE
              llSec = .F.
            ENDIF
          ENDIF
          IF llSec AND llBin
          *C132193,1 NNA (End)

            SELECT MDINVNTL
            SET ORDER TO MDINVNTLS
            WAIT WINDOW "Selecting Record... Please Wait" NOWAIT
            STORE .T. TO llCheck
            IF SEEK(m.Style+m.Color)
              SCAN REST WHILE style+color+cbattype+cLkBatch = m.Style+m.Color FOR IIF(llWareHus,cWareCode = ladata[8],.T.)
                IF SEEK(lcKey+MDINVNTL.cLkBatch,'MDINVNTH') AND MDINVNTH.Type $ 'HML'
                  llCheck = .F.
                ENDIF
              ENDSCAN
            ENDIF
            IF SEEK(m.Style + m.Color + m.Bin,lcTmpQuery)
              SCAN REST WHILE Style+Color = m.Style + m.Color
                IF BIN = cLocation
                  llCheck = .F.
                ENDIF
              ENDSCAN
            ELSE
              llCheck = .T.
            ENDIF
            IF llCheck
              INSERT INTO (lcTmpQuery) FROM MEMVAR
              SELECT(lcTmpQuery)
              REPLACE nStkVal WITH m.nStkVal
            ENDIF
          ENDIF
        ENDSCAN  
      ENDIF
    ENDSCAN
    *B608036,1 TMI [Start] rest the old order to the temp file lcTmpQuery
    SET ORDER TO TAG &lcOldOrder IN (lcTmpQuery)
    *B608036,1 TMI [END  ]
  ENDIF
ELSE
  SCAN FOR &lcExpr AND IIF(llWareHus,CwareCode = ladata[8] ,.T.)
    SCAT MEMVAR MEMO
    IF llMatModul
      m.FabDesc = FABRIC.Desc
      m.CLRDESC = gfCodDes(COLOR,'COLOR')
    ELSE
      m.StyDesc = Style.Desc1
    ENDIF
    DO CASE
      CASE  lcCostMeth = 'S'  && Standard
        m.Cost    = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
        m.oCost   = IIF(llMatModul,FABRIC.CostUse,STYLE.TotCost)
      OTHERWISE       && 'A' Average
        m.Cost    = IIF(llMAtModul,nFAve_Cost,Ave_Cost)
        m.oCost   = IIF(llMAtModul,nFAve_Cost,Ave_Cost)
    ENDCASE
    m.ITEM    = IIF(llMatModul,Fabric,'')
    m.STOCK   = IIF(llMatModul,OnHand,TotStk)
    m.oSTOCK  = IIF(llMatModul,OnHand,TotStk)
    m.nStkVal = nStkVal
    llToRet = .T.
    IF llLoc AND !EMPTY(laRpTarget)
      lnOldAls = SELECT(0)
      SELECT WhsLoc
      SET ORDER TO WhsLocSt
      IF  llWareHus
        =IIF(llMatModul,SEEK(PADR(m.ITEM,19)+m.Color+ladata[8]),SEEK(m.Style+m.Color+ladata[8]))
      ELSE
        =IIF(llMatModul,SEEK(PADR(m.ITEM,19)+m.Color),SEEK(m.Style+m.Color))
      ENDIF
      llToRet = .F.
      SCAN REST WHILE style+color+cwarecode+clocation=IIF(llMatModul,PADR(m.ITEM,19),m.Style)+m.Color+IIF(llWareHus,ladata[8],'');
            FOR ASCAN(laRpTarget,ALLTRIM(clocation)) <> 0
        llToRet = .T.
        EXIT
      ENDSCAN
      SET ORDER TO WhsLoc
      SELECT (lnOldAls)
    ENDIF
    IF llToRet
      SELECT MDINVNTL
      SET ORDER TO MDINVNTLS
      WAIT WINDOW "Selecting Record... Please Wait" NOWAIT
      =lfAddLTemp(IIF(llMatModul,PADR(m.ITEM,19),m.Style),m.Color)
    ENDIF
  ENDSCAN
  *B608036,1 TMI [Start] move this line to the upper if statement  
  *SET ORDER TO TAG &lcOldOrder IN (lcTmpQuery)
  *B608036,1 TMI [END  ]
  
  SELECT(lnOldAlias)  
ENDIF
*-- End of Function lfDLSELREC.
*!***************************************************************************
*!* Name        : lfDLSELTMP
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : select Records according to the bin location in case of 
*!*             : pushing Edit Button in the Inventory Locking Screen.
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSELTMP()
*!***************************************************************************
FUNCTION lfDLSELTMP
IF gfGetMemVar('M_DLUSEBIN')
   lcLocBin = &lcTmpQuery..Bin                         
  SELECT * FROM (lcBatLin) WHERE Style = lcSty AND cLocation = lcLocBin ;
         AND IIF(llDyelot AND lfStyDye(lcSty),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (gcWorkDir + lcDetLin)
  INDEX ON Style+color+dyelot+clocation TAG (lcDetLin)
  FOR I = 1 TO 8
    Z = STR (I,1)
    lnStk&Z = Stk&Z
  ENDFOR
  lnMCost   = COST
  lnOldCost = OldCOST
  lcReason  = cReason
  lcStyDesc = IIF(SEEK(lcSty,'Style'),Style.Desc1,'')
  lnNewTot  = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
ELSE
  SELECT * FROM (lcBatLin) WHERE Style = lcSty AND IIF(llDyelot AND lfStyDye(lcSty),!EMPTY(&lcBatLin..DYELOT),.T.) ;
         INTO DBF (gcWorkDir + lcDetLin)
  INDEX ON Style+color+dyelot+clocation TAG (lcDetLin)
ENDIF
*-- End of Function lfDLSELTMP.
*!***************************************************************************
*!* Name        : lfDLTMPBAT
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Get temp batch per bin location
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLTMPBAT()
*!***************************************************************************
FUNCTION lfDLTMPBAT
IF gfGetMemVar('M_DLUSEBIN')
  SELECT(lcTmpQuery)
  GOTO TOP
  SCAN
    SCATTER MEMVAR MEMO
    SELECT IIF(llMatModul,'FABDYE','STYDYE')
    REPLACE dLlokDate WITH laData[5]
    m.Style     = IIF(llMatModul,m.ITEM,m.Style)
    m.Creason   = IIF(llMatModul,'MATERIAL LOCK INVENTORY     ' ,'STYLE LOCK INVENTORY     ')
    m.cWareCode = ladata[8]
    lnAlias = SELECT(0)
    SELECT STYDYE
    =SEEK(m.Style+IIF(EMPTY(m.Color),'',m.Color)+IIF(llWareHus,ladata[8],''))
    DO CASE
      CASE  lcCostMeth = 'S'  && Standard
        m.Cost    = STYLE.TotCost
        m.OldCost = STYLE.TotCost
      OTHERWISE       && 'A' Average
        m.Cost    = STYDYE.Ave_Cost
        m.OldCost = STYDYE.Ave_Cost
    ENDCASE
    SCAN REST WHILE style+cwarecode+dyelot = m.Style+IIF(llWareHus,ladata[8],'')
      IF SEEK(STYDYE.style+STYDYE.cwarecode+m.Bin,'WHBINLOC')
        SELECT WHBINLOC
        SCAN REST WHILE style+cwarecode+clocation = STYDYE.style+STYDYE.cwarecode+m.Bin
          FOR lnInd = 1 TO 8
            Index = STR(lnInd,1)
            m.OldStk&Index = WHBINLOC.Qty&Index
            m.Stk&Index    = WHBINLOC.Qty&Index
          ENDFOR
          m.OldTotStk = WHBINLOC.TotQty
          m.TotStk    = WHBINLOC.TotQty
          m.Dyelot    = STYDYE.Dyelot
          m.Scale     = Style.Scale
          m.clocation = &lcTmpQuery..Bin
          INSERT INTO (lcBatLin) FROM MEMVAR
        ENDSCAN  
      ENDIF
    ENDSCAN
  ENDSCAN
ELSE
  SELECT(lcTmpQuery)
  GOTO TOP
  SCAN
    SCAT MEMVAR MEMO
    SELECT IIF(llMatModul,'FABDYE','STYDYE')
    REPLACE dLlokDate WITH ladata[5]
    m.Style     = IIF(llMatModul,m.ITEM,m.Style)
    m.Creason   = IIF(llMatModul,'MATERIAL LOCK INVENTORY     ' ,'STYLE LOCK INVENTORY     ')
    m.cWareCode = ladata[8]
    IF llMatModul
    *--- Parameters : 1-Item ,2-Color ,3-MultiMare
      =lfAddLin(SUBSTR(m.ITEM,1,7),m.Color)
    ELSE
      =lfAddLin(m.style,SPACE(06))
    ENDIF
  ENDSCAN
ENDIF
*-- End of Function lfDLTMPBAT.
*!***************************************************************************
*!* Name        : lfDLSTYCRL
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Trigger to Validate Post Style Per Bin Location in inv. Locking
*!***************************************************************************
*!* Called from : ICINVLK.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSTYCRL()
*!***************************************************************************
FUNCTION lfDLSTYCRL
IF gfGetMemVar('M_DLUSEBIN')
  IF TYPE('lcType') = 'U'
    PRIVATE lcFileNama
    lcFileNamA = lcTmpQuery +"A"
    lcBinLoc = &lcFileNamA..cLocation 
    lnRet = lfStyCrl('9',lcPStyle,ladata[8],lcDye1,ladata[6],'',@laAdjust,laAdjust[10],;
                     '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)
    SELECT WHBINLOC
    lcOldOrder = ORDER()
    SET ORDER TO Whbinloc
    SELECT (lcBatLin)
    SCAN FOR &lcBatLin..clocation = lcBinLoc
      IF SEEK(&lcBatLin..cwarecode + &lcBatLin..clocation+&lcBatLin..Style,'WHBINLOC') 
        SELECT WHBINLOC
        REPLACE Qty1   WITH MAX(&lcBatLin..stk1,0)  ,;
                Qty2   WITH MAX(&lcBatLin..stk2,0)  ,;
                Qty3   WITH MAX(&lcBatLin..stk3,0)  ,;
                Qty4   WITH MAX(&lcBatLin..stk4,0)  ,;
                Qty5   WITH MAX(&lcBatLin..stk5,0)  ,;
                Qty6   WITH MAX(&lcBatLin..stk6,0)  ,;
                Qty7   WITH MAX(&lcBatLin..stk7,0)  ,;
                Qty8   WITH MAX(&lcBatLin..stk8,0)  ,;
                TotQty WITH MAX(&lcBatLin..Totstk,0)
      ENDIF
    ENDSCAN
    SELECT WHBINLOC
    SET ORDER TO &lcOldOrder 
    SELECT MDINVNTL
  ELSE
    lnRet=lfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                   lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
  ENDIF
ELSE
   *-- to handle case of not bin location and called from inventory locking
   IF TYPE('lcAdjTyp') = 'U'
      lnRet = gfStyCrl('9',lcPStyle,ladata[8],lcDye1,ladata[6],'',@laAdjust,laAdjust[10],;
                           '',.T.,lcAdjReason,1,'MDINVNTL','NSTEPS',@laGLDistAr,0,'','',@laOldStk)
   ELSE
    lnRet=gfStyCrl(lcAdjTyp,Style,lcAdjWareH,Dyelot,Date,'',@laAdjust,lnACost,;
                   lcRefer,.T.,cAdjReason,0,'','',@laGLDistAr,0,"",lcAdjRef)
   ENDIF
ENDIF
*-- End of Function lfDLSTYCRL.
*!***************************************************************************
*!* Name        : lfSTYCRL
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : A Copy of GfStyCrl to be Applicable with Bin Location
*!***************************************************************************
*!* Called from : ICINVLK.PRG 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSTYCRL()
*!***************************************************************************
FUNCTION lfSTYCRL
PARAMETERS lcTrType,lcStyle,lcWareCode,lcSDyelot,ldTrDate,lcTrCode,laAdjStk,;
  lnNewCost,lcRefer,lcRISessn,lcAdjCdRsn,lnStarStep,lcTmpLFile,lcStepFld,;
  laGLInvAry,lnLineNo,lcLastRSess,lcAdjRef,laLockInfo, lnTranCost
*--Initialize function variables.
PRIVATE lcOldWAr,laOldstk,llChekUncmp,lnSAveCost,lnWAveCost,lnSOldStk,lnSOldCst,lnWOldStk,lnWOldCst,lcCostMeth,;
        lcAdjAcct,lcTmpJour,lcInvJour,lnRetStep,llUInvtry,lnSStkVal,lnWStkVal,lnTranCost,lnStkVal,lnLineNo,;
        lcLastRSess,lcAdjRef,lnDyeCost,lnPrvQty,lnPrvVal

STORE 0 TO lnStkVal,lnTranCost,lnSaveCost,lnSOldStk,lnSOldCst,lnWaveCost,lnWOldStk,lnWOldCst,laOldstk

IF TYPE('lnLineNo') = 'L'
  lnLineNo = 0
ENDIF  
IF TYPE('lcLastRSess') = 'L'
  lcLastRSess = SPACE(6)
ENDIF  
IF TYPE('lcAdjRef') = 'L'
  lcAdjRef = SPACE(6)
ENDIF
IF !(TYPE('lcType') = 'U')
  PRIVATE lcFileNama
  lcFileNama = lcTmpAdj+"A"
  lcBinLoc = &lcFileNama..LocFrom
ENDIF  

lcSysType = gfGetMemVar('M_SYSTYPE')
IF lcSysType = 'P'
  PRIVATE llOpnWarhs 
  llOpnWarhs = .F.
  IF !USED('WAREHOUS')
    llOpnWarhs = gfOpenFile(gcDataDir+"WAREHOUS","WAREHOUS","SH")
  ENDIF
  IF SEEK(lcWareCode,'WAREHOUS') AND WAREHOUS.cSiteId <> gcCurSite
    IF llOpnWarhs
      USE IN STYINVJL
    ENDIF
    RETURN (1)
  ENDIF
ENDIF
IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
ENDIF

*--Style and Warehouse Average Cost,Old Stock and Old Cost variables.

DIME laOldstk[9]
lcOldWAr   = ALIAS()                && Current Work aera.
lcAdjCdRsn = IIF(TYPE('lcAdjCdRsn') $ 'UL','',lcAdjCdRsn)
lcAdjAcct  = ' '                    && Adjustment Code GL Account.  
*--Dyelot if not used must be 10 chr len,needed in exprestion.
lcSDyelot  = IIF(EMPTY(lcSDyelot),SPACE(10),lcSDyelot)

*--Check Uncomplete session flag if steps are passed as value not as zero.
llChekUncmp = ( lnStarStep <> 0 )    
*--Check if needed to update G/L.
llGLUsed = IIF(TYPE('laGLInvAry') $ 'UL',.F.,IIF(EMPTY(laGLInvAry[1,1]),.F.,.T.))
*--Return step to continue for after exit the function.
lnRetStep   = 0
*--Check the costing method ,Average ,Standard ,FIFO or LIFO.
lcCostMeth = gfGetMemVar('M_Cost_Meth')


*--Check the existing of the style and
*--Point the record in style and style dyelot files.
IF ! SEEK(lcStyle,'STYLE') OR !SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  *--The style ???? record are missing,
  *--Cannot proceed with updating Stock,
  *--This transaction line will be ignored.
  =gfModalGen('TRM42114B42000','DIALOG',lcStyle)
  RETURN (0)
ENDIF


*--Check if StyInvJL file is Open.
llOpnJurnl=gfOpenFile(gcDataDir+"StyInvJl","StyInvJl","SH")

*T20060817.0014,3 tmi [start] important to add minuse signe in issuing case
*--Case of Invoice or Void credit memo transactions, Put the Qty as
*--negative values since it is an Issue transaction.
*--If it is an Inventory adjustment or Receve return P/o the Qty must
*--Passes to this function as negative values since we have a Receive
*--case for this Inventory adj. and Receive P/o.
IF lcTrType $ '38'
  FOR lnI=1 TO 9
    laAdjStk[lnI] = -(ABS(laAdjStk[lnI]))
  ENDFOR
ENDIF
*T20060817.0014,3


*--Check the Transaction Type if it Issue or Receive 'I' or 'R'.
*--Depends on Total adjusted stock is negative or positive.
lcIRType = IIF(laAdjStk[9]<0 AND lcTrType $ '123689I' , 'I' , 'R' )



*--Check the style Inventory Yes or No.
llUInvtry = STYLE.lInvSty
*--Get the Old Stock and Cost before updateing the new tansaction.

lnSOldStk = STYLE.TotStk
lnSOldCst = ABS(IIF(Style.TotStk=0,STYLE.Ave_Cost,STYLE.nStkVal / Style.TotStk))
lnWOldStk = STYDYE.TotStk

IF lcTrType = '9'
  lnWOldCst = IIF(laLockInfo[9]=0,0,laLockInfo[10]/laLockInfo[9])
ELSE
  lnWOldCst = ABS(IIF(StyDye.TotStk=0,STYDYE.Ave_Cost,STYDYE.nStkVal / StyDye.TotStk))
ENDIF  

*--Stock Value variable for style and StyDye.
lnSStkVal = IIF(lcTrType $ '29',0,STYLE.nStkVal )
lnOldSVal = IIF(lcTrType $ '29',STYDYE.nStkVal,0)

IF lcTrType = '9'
  lnWStkVal = laLockInfo[10]
ELSE
  lnWStkVal = IIF(lcTrType $ '29',0,STYDYE.nStkVal)
ENDIF

PRIVATE lnDyeCost
lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)
PRIVATE lnPrvQty,lnPrvVal
lnPrvQty = StyDye.TotStk
lnPrvVal = StyDye.nStkVal
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  lnPrvQty = StyDye.TotStk
  lnPrvVal = StyDye.TotStk * lnDyeCost
ENDIF

IF lcIRType = 'I' AND (lcTrType <> 'I' OR lcCostMeth $ 'FL')

  DO CASE
    CASE lcCostMeth = 'A'   && Average.
      lnNewCost = IIF((gfGetMemVar('M_WareHouse')='Y'),lnWOldCst ,lnSOldCst )

    CASE lcCostMeth = 'S'   && Standard.
      lnNewCost = STYLE.TotCost

    CASE lcCostMeth $ 'FL'  && FIFO or LIFO.
      *--In this case may be has more than cost so we hold this costs
      *--in lcTmpJour file that the following function will return.
      lcTmpJour = gfTempName()
      IF ! lfIsueCost(.F.)
        SELECT (lcOldWAr)
        RETURN (0)
      ENDIF
  ENDCASE
ENDIF

IF lcTrType = '9'
  = ACOPY(laLockInfo,laOldstk,1,9)
  lnOldSVal = laLockInfo[10]
ELSE
  SELECT STYDYE
  SCATTER FIELDS Stk1,Stk2,Stk3,Stk4,Stk5,Stk6,Stk7,Stk8,TotStk TO laOldstk
ENDIF

*--Temp Old Stock Array used in Physical or Markdown 
*--issue the old stock first and then receive the pysical quantity.
*-- In all other cases this array is Zero.

IF lcTrType $ '2'
  SELECT WHBINLOC
  IF !EMPTY(lcBinLoc)
    = SEEK(lcWareCode+lcBinLoc+lcStyle,'WHBINLOC')
  ENDIF
  FOR lnCnt = 1 TO 8 
    lcCnt = STR(lnCnt, 1)
    laOldstk[lnCnt] = WHBINLOC.Qty&lcCnt
  ENDFOR
  laOldstk[9] = WHBINLOC.TotQty
  lnOldSVal = laOldstk[9] * STYDYE.AVE_COST
  lnPrvQty  = laOldstk[9]
  lnPrvVal  = laOldstk[9] * STYDYE.AVE_COST
ENDIF

*--Calculate Avarage Cost for Style and StyDye records. 

*-- 1)  Update Style journal file. -------------------------------------
*--Read session no.
*--If receiving transaction and costing methos Lifo or Fifo make sure
*--that the session not duplecated fir same key.
lcRISessn = IIF( TYPE('lcRISessn')='C', lcRISessn ,'' )
IF lcIRType = 'R' AND lcCostMeth $ 'FL' AND !EMPTY(lcRISessn)
  lnJrlRec  = IIF(EOF('STYINVJL'),0,RECNO('STYINVJL'))
  lcRISessn = IIF(SEEK(lcStyle+lcWareCode+lcRISessn,'STYINVJL'),'',lcRISessn)
  IF lnJrlRec<>0
    GOTO lnJrlRec IN STYINVJL
  ENDIF
ENDIF
*--Read session no.
IF EMPTY(lcRISessn)
  lcRISessn = gfSequence('GLSESSION')
ENDIF

*--Read the adjustment code reason to get the GL Account.
IF !EMPTY(lcAdjCdRsn)
  DECLARE laTrmRltFd[1,2]
  laTrmRltFd[1,1] = 'GLACCOUNT'
  laTrmRltFd[1,2] = 'lcAdjAcct'
  =gfRltFld(lcAdjCdRsn , @laTrmRltFd , "CADJREASON")
ENDIF

*--Initialize next step to continue.
lnTmpStp = lnStarStep

*--Update journal for Issue Transaction ,FIFO or LIFO method.
IF lcIRType = 'I' AND lcCostMeth $ 'FL'

  lnIssTCst = 0
  lnIssTStk = 0

  SELECT (lcTmpJour)  
  *T20060817.0014,3   TMI [Start] remove the "cISession" from the key to not update key field within the scan loop
  INDEX ON Style+cWareCode+cDyelot+cRSession TAG &lcTmpJour
  *T20060817.0014,3   TMI [End  ] 
  SCAN
    REPLACE cSession  WITH lcRISessn,;
            cISession WITH cSession,;
            cTrCode   WITH IIF(cTrType $ "12" AND EMPTY(lcTrCode),cSession,cTrCode)
    SCATTER MEMVAR
    IF lfCheckUnCmp(lnTmpStp)      
      SELECT STYINVJL
      APPEND BLANK
      GATHER MEMVAR  
      lnIssTCst = lnIssTCst + m.nTotStk * m.nCost
      lnIssTStk = lnIssTStk + m.nTotStk
      REPLACE Reference  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
              cAdjReason WITH lcAdjCdRsn,;
              cAdjAcct   WITH lcAdjAcct,;
              nStkVal    WITH nTotStk * nCost,;
              LineNo     WITH lnLineNo,;
              nPrvSQty   WITH lnPrvQty,;
              nPrvSval   WITH lnPrvVal

      REPLACE cAdjRef    WITH lcAdjRef
      
      *-- Call global function to add audit fields info.
      =gfAdd_Info('STYINVJL')
      *--in case of Inventory lock.
      IF lcTrType='9'
        SCATT MEMVAR MEMO
        m.clocation   = lcBinLoc 
        INSERT INTO ('BININVJL') FROM MEMVAR
      ENDIF
      *--Update Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  

      *--Call TraceKey global function.
      =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    ENDIF  
    lnTmpStp = lnTmpStp + 1

    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    = lfStyWarDy()
  ENDSCAN
  IF lnIssTStk <> 0
    lnIssAvg = ABS(lnIssTCst / lnIssTStk )
  ELSE
    lnIssAvg = 0
  ENDIF  

  IF USED(lcTmpJour)
    USE IN (lcTmpJour)
  ENDIF
  *--Erase the temp. journal file.
  =lfEraseFil(lcTmpJour)

ELSE  && Not LIFO or FIFO or Receiving.

  *--Create an issue record for Physical inventory or 
  *--Markdown inventory transaction in Style inventory Journal.

  IF lcTrType $ '29'
    IF lfDoPhys('I')
      =lfIsuJlTr()
      STORE 0 TO lnPrvQty,lnPrvVal
    ENDIF
  ENDIF

  IF !(lcTrType $ '29') AND lcIRType = 'R' AND lnWOldStk < 0 AND lnWOldCst <> lnNewCost
    *-- This is to create 2 records in journal file
    *-- one for rec. the qty with it's old cost
    *-- the other for issue the qty with it's new cost
    = lfAdjRec()
  ENDIF

    *--Create a main record in journal file.
    IF !(lcTrType $ '29') OR (lcTrType $ '29' AND lfDoPhys('R'))          
      IF lfCheckUnCmp(lnTmpStp)      
        SELECT STYINVJL
        lnStkVal = laAdjStk[9] * lnNewCost
        APPEND BLANK
        REPLACE cSession   WITH lcRISessn,;
                Style      WITH lcStyle,;
                cWareCode  WITH lcWareCode,;
                cDyelot    WITH lcSDyelot,;
                dTrDate    WITH ldTrDate,;
                cTrType    WITH lcTrType,;
                cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),lcRISessn,lcTrCode),;
                nCost      WITH lnNewCost,;
                cIRType    WITH lcIRType,;
                nStk1      WITH laAdjStk[1],;
                nStk2      WITH laAdjStk[2],;
                nStk3      WITH laAdjStk[3],;
                nStk4      WITH laAdjStk[4],;
                nStk5      WITH laAdjStk[5],;
                nStk6      WITH laAdjStk[6],;
                nStk7      WITH laAdjStk[7],;
                nStk8      WITH laAdjStk[8],;
                nTotStk    WITH laAdjStk[9],;
                nStkVal    WITH lnStkVal   ,;
                Reference  WITH IIF(ctrType = '2' AND lcIRType = 'I','Auto. zeroing of stock',lcRefer),;
                lLockFlg   WITH IIF(lcTrType='9',.T.,lLockFlg),;
                cAdjReason WITH lcAdjCdRsn ,;
                cAdjAcct   WITH lcAdjAcct  ,;
                cISession  WITH IIF(cIRType='I',cSession,''),;
                cRSession  WITH IIF(cIRType='R',cSession,''),;
                LineNo     WITH lnLineNo,;
                nPrvSQty   WITH lnPrvQty,;
                nPrvSVal   WITH lnPrvVal

        REPLACE cAdjRef    WITH lcAdjRef

        REPLACE nTranCost WITH lnTranCost
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')
        *--in case of inventory lock
        IF lcTrType='9'
          SCATT MEMVAR MEMO
          m.clocation   = lcBinLoc 
          INSERT INTO ('BININVJL') FROM MEMVAR
        ENDIF
        *--Update Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

        *--Call TraceKey global function.
        =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
      ENDIF
      lnTmpStp = lnTmpStp + 1

      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()

    IF lcTrType = '9'
      = lfLkAdjRec()
    ENDIF
  ENDIF
ENDIF

*--Initialize next step to continue when return.
lnRetStep = lnTmpStp

*--Close style journal if this function open it.
IF llOpnJurnl AND USED("StyInvJl")
  USE IN STYINVJL
ENDIF

SELECT (lcOldWAr)
RETURN IIF(llChekUncmp , lnRetStep , 1 )
*-- End of Function lfSTYCRL.
*!*************************************************************
*!* Name      : lfUpdGLDist()
*!* Developer : Timour A. K.
*!* Date      : 01/22/98
*!* Module    : Inventory Control (IC)
*!* Purpose   : Update Temp G/L Distribution file.
*!*************************************************************
*:      [1] LinkCode  ,[2] Category Key ,[3] Amount sign
*:      [4] Tran Type ,[5] Tran No.     ,[6] Tran Date
*:      [7] Gl Year   ,[8] Gl Period    ,[9] Temp GlDist file name
*:      [10]Gl Account,[11]Currency Code,[12]CurrUnit,[13]Excg Rate.
*!*************************************************************
*! Call      : GLDIST
*!*************************************************************
*! Example   : =lfUpdGLDist()
*!*************************************************************
FUNCTION lfUpdGLDist

*-- llNegStkAd Showes if it is main record (Start)
*-- or it is adj. record because the stock is less than Zero
*-- AAMER 11/22/98
PARAMETERS llNegStkAd,llLockAdj

*-- llNegStkAd Showes if it is main record (End)
PRIVATE lnCurAlias

*--Donot update if no GL used.
IF ! llGLUsed
  RETURN
ENDIF

*-- This means it is Main Record (Start)

IF !llNegStkAd
*-- This means it is Main Record (End)

  *--Update Gl for Main inventory record for Isue or Receive.
  *- Receiving Trans.(+1,2,4,5,+6,7):    None
  *-  => +/-  lnAmount = Total Recv. Qty * New Recv. Cost     
  *- Issue Trans.(-1,-3,-6,-8,-2)     :  None
  *-  => +/-  lnAmount = Total Issue Qty * Issue Cost     
  FOR lnAln=1 TO ALEN(laGLInvAry,1)

    laGLInvAry[lnAln,5] = STYINVJL.cTrCode

    IF lfCheckUnCmp(lnTmpStp)
      
      lnGLEnAmount = STYINVJL.nStkVal * laGLInvAry[lnAln,3]

      DO GLDIST WITH laGLInvAry[lnAln,1],laGLInvAry[lnAln,2],lnGLEnAmount,laGLInvAry[lnAln,4],laGLInvAry[lnAln,5]  ,;
                     IIF(llLockAdj,laLockInfo[11],laGLInvAry[lnAln,6]),laGLInvAry[lnAln,7],laGLInvAry[lnAln,8]     ,;
                     laGLInvAry[lnAln,9],laGLInvAry[lnAln,10],laGLInvAry[lnAln,11],laGLInvAry[lnAln,12],laGLInvAry[lnAln,13]
      DO CASE
        CASE &laGLInvAry[lnAln,9]..catg_Key = '006'
          lnCurAlias = SELECT(0)
          SELECT StyInvJl
          REPLACE cICAcnt WITH &laGLInvAry[lnAln,9]..GLAccount
          SELECT (lnCurAlias)

        *--update cadjact field in all cases 

        *--Updae cAdjAcct if it is empty in all catg_keys (Start) AAMER 04/13/99
        *--not if it is empty and catg_key = '007' 
        CASE &laGLInvAry[lnAln,9]..catg_Key = '013' OR EMPTY(StyInvJl.cAdjAcct)
        *--Updae cAdjAcct if it is empty in all catg_keys (End)
          lnCurAlias = SELECT(0)
          SELECT StyInvJl
          REPLACE cAdjAcct WITH &laGLInvAry[lnAln,9]..GLAccount
          SELECT (lnCurAlias)
      ENDCASE
    
      *--Update Uncomplete session Step.
      =lfUpdStep(lnTmpStp)  
    ENDIF
    lnTmpStp = lnTmpStp + 1
  ENDFOR

*-- This means it is Adj. Record (Start)
ELSE

  lcStyLink = IIF(EMPTY(StyDye.GL_Link),Style.Link_Code,StyDye.GL_Link)
   DO GLDIST WITH lcStyLink,'006',StyInvJl.nStkVal,'IA',;
                 StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],StyInvJl.DtrDate),laGLInvAry[1,7],;
                 laGLInvAry[1,8],laGLInvAry[1,9],'','','',''                 
  lnCurAlias = SELECT(0)
  SELECT StyInvJl
  REPLACE cICAcnt WITH &laGLInvAry[1,9]..GLAccount
  SELECT (lnCurAlias)
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1

  DO GLDIST WITH lcStyLink,'007',-StyInvJl.nStkVal,'IA',StyInvJl.cTrCode,IIF(!EMPTY(laGLInvAry[1,6]),laGLInvAry[1,6],;
                 StyInvJl.DtrDate),laGLInvAry[1,7],laGLInvAry[1,8],laGLInvAry[1,9],StyInvJl.cAdjAcct,'','',''
                 
  IF EMPTY(StyInvJl.cAdjAcct)
    lnCurAlias = SELECT(0)
    SELECT StyInvJl
    REPLACE cAdjAcct WITH &laGLInvAry[1,9]..GLAccount
    SELECT (lnCurAlias)
  ENDIF
  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  
  lnTmpStp = lnTmpStp + 1
ENDIF
RETURN
*!***************************************************************************
*!* Name        : lfStyWarDy
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update the StyDye file
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfStyWarDy()
*!***************************************************************************
FUNCTION lfStyWarDy
PRIVATE lnCurAlias
lnCurAlias = SELECT(0)
PRIVATE llNew_Cost
llNew_Cost = .F. 

*-- what we do in case of avarage cost lcCostMeth = 'A' ??
IF lcIRType = 'R' .AND. !(lcTrType = '1' .AND. lcCostMeth $ 'FLA') .AND. !(lcTrType = '4' .AND. lcCostMeth $ 'FL' ) ;
  .AND. ( !(lcTrType $ '29') .OR. (lcTrType $ '29' AND lfDoPhys('R')))  
  llNew_Cost = .T.
ENDIF
lnValDiff=0
*--1 ) Update Stock and Avarege cost in Style Dyelot file Warehouse record.
IF lfCheckUnCmp(lnTmpStp)
  SELECT STYDYE
  =SEEK(lcStyle+lcWareCode+SPACE(10),'STYDYE')
  =RLOCK()
  lnPrvStk  = TotStk   && Old Stock
  lnStkVal  = nStkVal  && Old Stock Value
  lnAveCost = Ave_Cost && Old Average Cost
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
  IF StyInvJl.nTotStk > 0                && Receive transaction
    IF lnPrvStk < 0                      && The stock was negative
      
      IF StyInvJl.nTotStk + lnPrvStk < 0   && The stock still negative after receiving
        IF llNew_Cost
          lnStkVal = lnStkVal + StyInvJl.nTotStk * lnNewCost
        ELSE
          lnStkVal = lnStkVal +  StyInvJl.nTotStk * lnAveCost 
        ENDIF
      ELSE                               && Use the transaction cost if the stock will be > 0
        lnStkVal = (StyInvJl.nTotStk+lnPrvStk) * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
      ENDIF
    ELSE
      lnStkVal = lnStkVal + (StyInvJl.nTotStk * IIF(llNew_Cost,lnNewCost,StyInvJl.nCost))
    ENDIF
  ELSE                                   && Issue transaction
    IF lnPrvStk = 0                      && If it is the 1st transaction for this style or the stock became 0.
      lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
    ENDIF
    lnStkVal = TotStk * IIF(llNew_Cost,lnNewCost,IIF(lnprvstk=0,lnAveCost,lnstkval/lnprvstk))
  ENDIF
  IF TotStk = 0 AND StyInvJl.nTotStk > 0
     lnAveCost = IIF(llNew_Cost,lnNewCost,StyInvJl.nCost)
  ENDIF
  IF TotStk > 0
    lnAveCost = IIF(llNew_Cost,lnNewCost,lnStkVal/TotStk)
  ENDIF  
  IF lcIRType='I' AND !EMPTY(lcLastRSess)
    lnAveCost = IIF(TotStk=0,StyInvJl.nCost,lnStkVal/TotStk)
  ENDIF
  lnValDiff = lnStkVal - nStkVal
  REPLACE StyDye.nStkVal  WITH IIF(TotStk=0,0,lnStkVal),;
          StyDye.Ave_Cost WITH lnAveCost 
  UNLOCK 
  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep)  
  *--Call TraceKey global function.
  =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')
  lnPrvQty  = TotStk
  lnPrvVal  = nStkVal
  lnDyeCost = IIF(StyDye.TotStk = 0,StyDye.Ave_Cost,StyDye.nStkVal/StyDye.TotStk)
ENDIF 
lnTmpStp = lnTmpStp + 1
*--2 ) Update Stock and Avarege cost in Style file. ------------
IF lfCheckUnCmp(lnStarStep+1)
  SELECT STYLE
  =RLOCK()
  REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
          Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
          Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
          Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
          Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
          Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
          Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
          Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
          TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8,;
          nStkVal  WITH nStkVal + lnValDiff ,;
          Ave_Cost WITH IIF(TotStk = 0,Ave_Cost,ABS(nStkVal/TotStk))
  UNLOCK 
  *--Update Uncomplete session Step.
  =lfUpdStep(lnStarStep+1)  
  *--Call TraceKey global function.
  =gfTraceKey('STYLE',STYLE.Style,'M')
ENDIF 
lnTmpStp = lnTmpStp + 1
*--3 )  Update Stock in Style Dyelot file Dyelot record. --------

*- in Case Of Reciving PO and the cost method is Avarage Cost
IF lcIRType = 'R' .AND. lcTrType = '6' .AND. lcCostMeth = 'A'
  =SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  REPLACE STYDYE.Ave_Cost WITH STYLE.Ave_Cost
ENDIF
IF !EMPTY(lcSDyelot) AND SEEK(lcStyle+lcWareCode+lcSDyelot,'STYDYE')
  IF lfCheckUnCmp(lnStarStep+2)
    SELECT STYDYE
    =RLOCK()
    REPLACE Stk1     WITH Stk1 + IIF(llUInvtry,StyInvJl.nStk1,0),;
            Stk2     WITH Stk2 + IIF(llUInvtry,StyInvJl.nStk2,0),;
            Stk3     WITH Stk3 + IIF(llUInvtry,StyInvJl.nStk3,0),;
            Stk4     WITH Stk4 + IIF(llUInvtry,StyInvJl.nStk4,0),;
            Stk5     WITH Stk5 + IIF(llUInvtry,StyInvJl.nStk5,0),;
            Stk6     WITH Stk6 + IIF(llUInvtry,StyInvJl.nStk6,0),;
            Stk7     WITH Stk7 + IIF(llUInvtry,StyInvJl.nStk7,0),;
            Stk8     WITH Stk8 + IIF(llUInvtry,StyInvJl.nStk8,0),;
            TotStk   WITH Stk1 + Stk2+Stk3+Stk4+Stk5+Stk6+Stk7+Stk8
    UNLOCK 
    *--Update Uncomplete session Step.
    =lfUpdStep(lnStarStep+2)  
    *--Call TraceKey global function.
    =gfTraceKey('STYDYE',STYDYE.Style+STYDYE.cWareCode+STYDYE.Dyelot,'M')
    lnPrvQty = TotStk
    lnPrvVal = TotStk * lnDyeCost
  ENDIF 
ENDIF
lnTmpStp = lnTmpStp + 1
SELECT(lnCurAlias)
*-- End of Function lfStyWarDy.

*!***************************************************************************
*!* Name        : lfDoPhys
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Check if we can issue or receive in case of inventory Locking.
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : .T. OR .F.
*!***************************************************************************
*!* Example     : = lfDoPhys()
*!***************************************************************************
FUNCTION lfDoPhys
PARAMETERS lcRI
PRIVATE lcRI
lnRet = .F.
DO CASE
  CASE lcRI = 'I'
    *-- Issue if the old stock value or the old stock qty doen't equal zero 
    *-- or the cost value has changed or the balance has changed.
    lnRet = lnOldSVal   # 0 OR laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
            laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR lnWOldCst # lnNewCost OR ;
            laOldstk[9] # laAdjStk[9]
  CASE lcRI = 'R'
    *-- Receive only if the Issue record hasn't been issued or the new balance is not zero or cost value has changed
    lnRet = !(lnOldSVal   # 0 OR laOldstk[1] # 0 OR laOldstk[2] # 0 OR laOldstk[3] # 0 OR laOldstk[4] # 0 OR ;
              laOldstk[5] # 0 OR laOldstk[6] # 0 OR laOldstk[7] # 0 OR laOldstk[8] # 0 OR lnWOldCst   # lnNewCost OR ;
              laOldstk[9] # laAdjStk[9]      )   OR laAdjStk[9] # 0 OR lnWOldCst   # lnNewCost                      
ENDCASE
RETURN lnRet
*-- End of Function lfDoPhys.
*!***************************************************************************
*!* Name        : lfLkAdjRec
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : 
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfLkAdjRec()
*!***************************************************************************
FUNCTION lfLkAdjRec
PRIVATE lcJourTag,lnCurAlias,lnTotQty,lnTotVal,lnTranVal,lnDiffere
lnCurAlias = SELECT(0)
SELECT StyInvJl
lcJourTag = ORDER('StyInvJl')
SET ORDER TO StyInvJl
lnTotQty = laAdjStk[9]
lnTotVal = laAdjStk[9] * lnNewCost
lnTranVal = 0
lnDiffere = 0
IF SEEK(lcStyle+lcWareCode+lcRISessn,'StyInvJl')
  SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrDate)+cTrCode=lcStyle+lcWareCode+lcRISessn FOR cDyelot=lcSDyelot AND !lLockFlg
    lnTranVal = IIF(cIRType='I',nTotStk*lnNewCost,nStkVal)
    lnDiffere = lnDiffere + (nStkVal - lnTranVal)
    lnTotVal  = lnTotVal  + lnTranVal
    lnTotQty  = lnTotQty  + nTotStk
    lnNewCost = IIF(lnTotQty=0,lnNewCost,lnTotVal/lnTotQty)
  ENDSCAN
  lnDiffere = - 1 * lnDiffere
  IF lnDiffere # 0 AND lfCheckUnCmp(lnTmpStp)
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn       ,;
            Style      WITH lcStyle         ,;
            cWareCode  WITH lcWareCode      ,;
            cDyelot    WITH lcSDyelot       ,;
            dTrDate    WITH laLockInfo[11]  ,;
            cTrType    WITH lcTrType        ,;
            cTrCode    WITH IIF(EMPTY(lcTrCode),lcRISessn,lcTrCode),;
            cIRType    WITH IIF(lnDiffere<0,'I','R'),;
            nStkVal    WITH lnDiffere       ,;
            Reference  WITH 'Mark Down Adjustement Value',;
            cAdjReason WITH lcAdjCdRsn      ,;
            cAdjAcct   WITH lcAdjAcct       ,;
            cISession  WITH IIF(lnDiffere<0,cSession,''),;
            cRSession  WITH IIF(lnDiffere>0,cSession,''),;
            nPrvSQty   WITH lnPrvQty        ,;
            nPrvSVal   WITH lnPrvVal        ,;
            cAdjRef    WITH lcAdjRef        ,;
            nTranCost  WITH lnTranCost
    =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    lnTmpStp = lnTmpStp + 1
    =lfUpdGLDist(.F.,.T.)
    =lfStyWarDy()
  ENDIF
ENDIF
SET ORDER TO (lcJourTag) IN StyInvJl
SELECT (lnCurAlias)
*-- End of Function lfLkAdjRec.
*!***************************************************************************
*!* Name        : lfIsuJlTr
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : This function will update the journal file with Issue transaction
*!*             : record(s) for Physical or markdown transactions only.  
*!*             : If the method is Standard or Average we create only one Issue record
*!*             : for old stock before physical transaction was done Else if the method
*!*             : is LIFO or FIFO we create issue records depends on all open receivings
*!*             : exist in journal.
*!***************************************************************************
*! Notes        : This Function Copied from Gfstycrl.prg with some modification
*!              : from Mohamed Shokry to be Suitable for the bin location system
*!***************************************************************************
*!* Called from : Binmain.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfIsuJlTr()
*!***************************************************************************
FUNCTION lfIsuJlTr
IF lcCostMeth $ 'FL'  && ISSUE FIFO or LIFO.
  *--Get the open receivings.
  lcTmpJour = gfTempName()
  =lfIsueCost(.T.)
  SELECT (lcTmpJour)
  SCAN
    IF nTotStk <> 0 OR nStkVal <> 0
      REPLACE cSession  WITH lcRISessn,cISession WITH cSession,cTrCode WITH IIF(cTrType $ "12",cSession,cTrCode)
      SCATTER MEMVAR       
      IF lfCheckUnCmp(lnTmpStp)
        SELECT STYINVJL
        APPEND BLANK
        GATHER MEMVAR
        REPLACE Reference  WITH IIF(cTrType='2','Auto. zeroing of stock',lcRefer),;
                cAdjReason WITH lcAdjCdRsn      ,;
                cAdjAcct   WITH lcAdjAcct       ,;
                nStkVal    WITH IIF(nTotStk = 0,nStkVal,nTotStk * nCost),;
                nPrvSQty   WITH lnPrvQty        ,;
                nPrvSval   WITH lnPrvVal        ,;
                cAdjRef    WITH lcAdjRef        ,;
                lLockFlg   WITH (lcTrType='9')  ,;
                nTranCost  WITH lnTranCost
        
        *-- Call global function to add audit fields info.
        =gfAdd_Info('STYINVJL')
        *-- in case of inventory lock
        IF lcTrType='9'
          SCATT MEMVAR MEMO
          m.clocation   = lcBinLoc 
          INSERT INTO ('BININVJL') FROM MEMVAR
        ENDIF
        *--Update Uncomplete session Step.
        =lfUpdStep(lnTmpStp)  

        *--Call TraceKey global function.
        =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
      ENDIF  
      lnTmpStp = lnTmpStp + 1

      *--Update Temp G/L Distribution file.
      =lfUpdGLDist()
      =lfStyWarDy()
    ENDIF
  ENDSCAN
  USE
  *--Erase the temp. journal file.
  =lfEraseFil(lcTmpJour)
ELSE
  IF lfCheckUnCmp(lnTmpStp)       
    SELECT STYINVJL
    APPEND BLANK
    REPLACE cSession   WITH lcRISessn      ,;
            Style      WITH lcStyle        ,;
            cWareCode  WITH lcWareCode     ,;
            cDyelot    WITH lcSDyelot      ,;
            dTrDate    WITH ldTrDate       ,;
            cTrType    WITH lcTrType       ,;
            cTrCode    WITH IIF(cTrType $ "129" AND EMPTY(lcTrCode),cSession,lcTrCode),;
            nCost      WITH lnWOldCst      ,;
            cIRType    WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,"R","I"),;
            cISession  WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,'',cSession),;
            cRSession  WITH IIF(laOldstk[9]<0 OR lnOldSVal<0,cSession,''),;
            nStk1      WITH -(laOldstk[1]) ,;
            nStk2      WITH -(laOldstk[2]) ,;
            nStk3      WITH -(laOldstk[3]) ,;
            nStk4      WITH -(laOldstk[4]) ,;
            nStk5      WITH -(laOldstk[5]) ,;
            nStk6      WITH -(laOldstk[6]) ,;
            nStk7      WITH -(laOldstk[7]) ,;
            nStk8      WITH -(laOldstk[8]) ,;
            nTotStk    WITH -(laOldstk[9]) ,;
            nStkVal    WITH -(lnOldSVal)   ,;
            lLockFlg   WITH (lcTrType='9') ,;
            Reference  WITH IIF(cTrType = '2','Auto. zeroing of stock',lcRefer),;
            cAdjReason WITH lcAdjCdRsn     ,;
            cAdjAcct   WITH lcAdjAcct      ,;
            nPrvSQty   WITH lnPrvQty       ,;
            nPrvSVal   WITH lnPrvVal       ,;
            cAdjRef    WITH lcAdjRef       ,;
            nTranCost  WITH lnTranCost
    *-- Call global function to add audit fields info.
    =gfAdd_Info('STYINVJL')
    *-- in case of inventory lock.
    IF lcTrType='9'
      SCATT MEMVAR MEMO
      m.clocation   = lcBinLoc 
      INSERT INTO ('BININVJL') FROM MEMVAR
    ENDIF
    *--Update Uncomplete session Step.
    =lfUpdStep(lnTmpStp)  
    *--Call TraceKey global function.
    =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
    lnTmpStp = lnTmpStp + 1
    *--Update Temp G/L Distribution file.
    =lfUpdGLDist()
    =lfStyWarDy()
  ENDIF
ENDIF  
RETURN
*-- End of Function lfIsuJlTr.
*!***************************************************************************
*!* Name        : lfIsueCost
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : This function will get the journal records for the receiving
*!*             : transactions that will be applied on the current Issue 
*!*             : transaction and take the cost of the receiving to be a new 
*!*             : cost of issueing,depends on cost method FIFO or LIFO.    
*!***************************************************************************
*! Notes        : This Function Copied from Gfstycrl.prg with some modification
*!              : from Mohamed Shokry to be Suitable for the bin location system
*!***************************************************************************
*!* Called from : BINMAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfIsueCost()
*!***************************************************************************
FUNCTION lfIsueCost
PARAMETERS llForPhys
PRIVATE lcAlias,llContnu,laTotRcvd
lcAlias = ALIAS()
SELECT BININVJL
*--Create the Temp journal file with open receiving transactions.
*T20060817.0014,3   TMI [Start] I will do the following
*                             1) change the contents of the variable lcBinloc, it may contains more than one bin to select from
*                             2) add the location to the group by
*SELECT cSession,Style,cWareCode,cDyelot,dTrDate,cTrType,cTrCode,nCost,cIRType,cRSession,cISession,;
*       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',SUM(nStk4) AS 'nStk4',;
*       SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
*       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ,SPACE(6) as cAdjRef ;
*   FROM  BININVJL ;              
*   WHERE Style + cWareCode + cDyelot + cRSession + cISession = lcStyle + lcWareCode + lcSDyelot ;
*       AND clocation = lcBinLoc ;
*   GROUP BY BININVJL.Style,BININVJL.cWareCode,BININVJL.cDyelot,BININVJL.cRSession ;
*   ORDER BY BININVJL.Style,BININVJL.cWareCode,BININVJL.cDyelot,BININVJL.cRSession ;
*   INTO DBF (gcWorkDir+lcTmpJour)

SELECT cSession,Style,cWareCode,cLocation,cDyelot,dTrDate,cTrType,cTrCode,nCost,cIRType,cRSession,cISession,;
       SUM(nStk1) AS 'nStk1',SUM(nStk2) AS 'nStk2',SUM(nStk3) AS 'nStk3',SUM(nStk4) AS 'nStk4',;
       SUM(nStk5) AS 'nStk5',SUM(nStk6) AS 'nStk6',SUM(nStk7) AS 'nStk7',SUM(nStk8) AS 'nStk8',;
       SUM(nTotStk) AS 'nTotStk' ,SUM(nStkVal) AS 'nStkVal', .F. AS 'lNeeded' ,SPACE(6) as cAdjRef ;
   FROM  BININVJL ;              
   WHERE Style + cWareCode + cDyelot + cRSession + cISession = lcStyle + lcWareCode + lcSDyelot ;
       AND cLocation $ lcBinLoc ;
   GROUP BY BININVJL.Style,BININVJL.cWareCode,BININVJL.cDyelot,BININVJL.CLOCATION,BININVJL.cRSession ;
   ORDER BY BININVJL.Style,BININVJL.cWareCode,BININVJL.cDyelot,BININVJL.CLOCATION,BININVJL.cRSession ;
   INTO DBF (gcWorkDir+lcTmpJour)
*T20060817.0014,3   TMI [End  ] 
SELECT (lcTmpJour)
*B608148,1 NNA 07/02/2007 (BEGIN)
*DELETE ALL FOR nStk1=0 AND nStk2=0 AND nStk3=0 AND nStk4=0 AND nStk5=0 AND nStk6=0 AND nStk7=0 AND nStk8=0
DELETE ALL FOR nStk1<=0 AND nStk2<=0 AND nStk3<=0 AND nStk4<=0 AND nStk5<=0 AND nStk6<=0 AND nStk7<=0 AND nStk8<=0
*B608148,1 NNA (END)


GO TOP
IF EOF()
  IF !llForPhys
    *--No open receiving exist for style XXXX , This transaction line will be ignored.
    =gfModalGen('TRM42116B42000','DIALOG',lcStyle)
    USE
    RETURN .F.
  ELSE  && Get issue for Physical transactions.
    APPEND BLANK
    REPLACE cSession  WITH lcRISessn,;
            Style     WITH lcStyle,;
            cWareCode WITH lcWareCode,;
            cDyelot   WITH lcSDyelot,;
            nCost     WITH lnWOldCst,;
            dTrDate   WITH ldTrDate,;
            cTrType   WITH lcTrType,;
            cTrCode   WITH lcTrCode,;
            cIRType   WITH "I",;
            nStkVal   WITH lnOldSVal

    REPLACE cAdjRef    WITH lcAdjRef

    RETURN
  ENDIF 
ENDIF

*--For Not Phyical.
IF !llForPhys
  *--Indexing the file on Ascending or Descending expresion depends on LIFO or FIFO method.
  IF lcCostMeth = 'F'
    INDEX ON Style+cWareCode+cDyelot+cRSession+cISession TAG &lcTmpJour
  ELSE
    INDEX ON Style+cWareCode+cDyelot+cRSession+cISession DESCENDING TAG &lcTmpJour
  ENDIF
  GO TOP

  *--Start checking the only needed open receinving transaction for this
  *--issue transaction and put zero for all not needed receivings.

  *--Array to Hold the accomulation of the receiving untill it cover the issue quantity needed.
  DIME laTotRcvd[9]
  laTotRcvd = 0
  SCAN
    llContnu  = .F.
    FOR I=1 TO 8      
      Z=STR(I,1)
      IF ABS(laAdjStk[I]) > laTotRcvd[I]
        *T20060817.0014,3   TMI [Start] receive from the correct bin
        llContnu = .T.  && to gurantee that the llContnu is updated 
      ENDIF      
      
      *C200861,1 NNA 09/21/2007 (BEGIN) Remove the second condition because it is not correct that becuause lcTmpFile is for counting steps
      *IF ABS(laAdjStk[I]) > laTotRcvd[I] .AND. cLocation = &lcTmpLFile..Binloc&Z 
      IF ABS(laAdjStk[I]) > laTotRcvd[I] 
      *C200861,1 NNA (END)
      
        *T20060817.0014,3   TMI [End  ]   
        
        llContnu = .T.
        laTotRcvd[I] = laTotRcvd[I] + nStk&Z
        laTotRcvd[9] = nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        IF ABS(laAdjStk[I]) <= laTotRcvd[I]
          REPLACE nStk&Z  WITH nStk&Z - (laTotRcvd[I] - ABS(laAdjStk[I]))
          REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
          REPLACE lNeeded WITH .T.
        ELSE
          REPLACE lNeeded WITH .T.
        ENDIF   
      ELSE
        REPLACE nStk&Z  WITH 0
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE lNeeded WITH .T.
      ENDIF   
    ENDFOR 
    IF !llContnu 
      EXIT
    ENDIF
  ENDSCAN
  *--Check if all Issue quantity are covered by the receivings.
  IF llContnu 
    FOR I=1 TO 8 
      Z=STR(I,1)
      IF laAdjStk[I] < 0 and ABS(laAdjStk[I]) > laTotRcvd[I]
        *--The receiving quantity are not covered the issued quantity
        *--for Style XXXX , This transaction line will be ignored.
        =gfModalGen('TRM42115B42000','DIALOG',lcStyle)
        USE
        RETURN .F. 
      ENDIF
    ENDFOR
  ENDIF
  *--Delete all not needed receiving transactions.
  DELETE ALL FOR nTotStk = 0 OR !lNeeded
ENDIF

*--Change it to Issue transactions,to use it in updating master Journal file.
REPLACE ALL cIRType WITH "I"      ,;
            dTrDate WITH ldTrDate ,;
            cTrType WITH lcTrType ,;
            cTrCode WITH lcTrCode ,;
            nStk1   WITH -nStk1   ,;
            nStk2   WITH -nStk2   ,;  
            nStk3   WITH -nStk3   ,;  
            nStk4   WITH -nStk4   ,;  
            nStk5   WITH -nStk5   ,;  
            nStk6   WITH -nStk6   ,;  
            nStk7   WITH -nStk7   ,;  
            nStk8   WITH -nStk8   ,;  
            nTotStk WITH -nTotStk ,;
            nStkVal WITH -nStkVal

SELECT (lcAlias)
RETURN .T.
*-- End of Function lfIsueCost.

*!***************************************************************************
*!* Name        : lfAdjRec
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Add Receiving record and Issuing record in StyInvJl.
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAdjRec()
*!***************************************************************************
FUNCTION lfAdjRec
IF lfCheckUnCmp(lnTmpStp)
  SELECT STYINVJL
  APPEND BLANK
  REPLACE cSession   WITH lcRISessn     ,;
          Style      WITH lcStyle       ,;
          cWareCode  WITH lcWareCode    ,;
          cDyelot    WITH lcSDyelot     ,;
          dTrDate    WITH ldTrDate      ,;
          cTrType    WITH '1'           ,;
          cTrCode    WITH lcRISessn     ,;
          nCost      WITH lnWOldCst     ,;
          cIRType    WITH "R"           ,;
          nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[1],laAdjStk[1]),;
          nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[2],laAdjStk[2]),;
          nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[3],laAdjStk[3]),;
          nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[4],laAdjStk[4]),;
          nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[5],laAdjStk[5]),;
          nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[6],laAdjStk[6]),;
          nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[7],laAdjStk[7]),;
          nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-laOldstk[8],laAdjStk[8]),;
          nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
          nStkVal    WITH IIF(laOldstk[9]+laAdjStk[9]>=0,-lnWStkVal,nTotStk * lnWOldCst),;
          Reference  WITH "Auto cost adj. " + cTrCode ,;
          cAdjReason WITH lcAdjCdRsn    ,;
          cAdjAcct   WITH lcAdjAcct     ,;
          cRSession  WITH cSession      ,;
          nPrvSQty   WITH lnPrvQty      ,;
          nPrvSVal   WITH lnPrvVal      ,;
          cAdjRef    WITH lcAdjRef      ,;
          nTranCost  WITH lnTranCost

  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

  *--Call TraceKey global function.
  =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
ENDIF
lnTmpStp = lnTmpStp + 1

*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()

*-- Iss. with the new cost
IF lfCheckUnCmp(lnTmpStp)   
  SELECT STYINVJL
  APPEND BLANK
  REPLACE cSession   WITH lcRISessn      ,;
          Style      WITH lcStyle        ,;
          cWareCode  WITH lcWareCode     ,;
          cDyelot    WITH lcSDyelot      ,;
          dTrDate    WITH ldTrDate       ,;
          cTrType    WITH '1'            ,;
          cTrCode    WITH lcRISessn      ,;
          nCost      WITH lnNewCost      ,;
          cIRType    WITH "I",;
          nStk1      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[1],-laAdjStk[1]),;
          nStk2      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[2],-laAdjStk[2]),;
          nStk3      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[3],-laAdjStk[3]),;
          nStk4      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[4],-laAdjStk[4]),;
          nStk5      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[5],-laAdjStk[5]),;
          nStk6      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[6],-laAdjStk[6]),;
          nStk7      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[7],-laAdjStk[7]),;
          nStk8      WITH IIF(laOldstk[9]+laAdjStk[9]>=0,laOldstk[8],-laAdjStk[8]),;
          nTotStk    WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8,;
          nStkVal    WITH nTotStk * lnNewCost,;
          Reference  WITH "Auto cost adj. " + cTrCode ,;
          cAdjReason WITH lcAdjCdRsn     ,;
          cAdjAcct   WITH lcAdjAcct      ,;
          cISession  WITH cSession       ,;
          nPrvSQty   WITH lnPrvQty       ,;
          nPrvSVal   WITH lnPrvVal       ,;
          cAdjRef    WITH lcAdjRef       ,; 
          nTranCost  WITH lnTranCost
  *-- Call global function to add audit fields info.
  =gfAdd_Info('STYINVJL')

  *--Update Uncomplete session Step.
  =lfUpdStep(lnTmpStp)  

  *--Call TraceKey global function.
  =gfTraceKey('STYINVJL',STYINVJL.Style+STYINVJL.cWareCode+STYINVJL.cSession+DTOS(STYINVJL.dTrDate)+STYINVJL.cTrCode+STR(STYINVJL.lineNo,6),'A')
ENDIF
lnTmpStp = lnTmpStp + 1
*--Update Temp G/L Distribution file.
=lfUpdGLDist(.T.)
=lfStyWarDy()
*-- End of Function lfAdjRec.
*!***************************************************************************
*!* Name        : lfUpdStep
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Update uncomplete session step.
*!***************************************************************************
*!* Called from : Binmain.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUpdStep()
*!***************************************************************************
FUNCTION lfUpdStep
PARA lnCurntStep
PRIVATE lnAlas
IF llChekUncmp
  lnAlas = SELECT()
  SELECT (lcTmpLFile)
  =RLOCK()
  REPLACE &lcStepFld WITH lnCurntStep
  UNLOCK
  SELECT(lnAlas)
ENDIF
RETURN
*-- End of Function lfUpdStep.
*!***************************************************************************
*!* Name        : lfCheckUnCmp
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Function To Check uncomplete session Steps.
*!***************************************************************************
*!* Called from : BINMAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : .T. for Check , .F. for Already checked no need to check.
*!***************************************************************************
*!* Example     : = lfCheckUnCmp()
*!***************************************************************************
FUNCTION lfCheckUnCmp
PARA lnStepNo
IF !llChekUncmp
   RETURN .T.
ELSE
  RETURN ( &lcTmpLFile..&lcStepFld < lnStepNo )
ENDIF
*-- End of Function lfCheckUnCmp.
*!***************************************************************************
*!* Name        : lfDEFNMNU
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Define a Bin Detail's menu option.
*!***************************************************************************
*!* Called from : ICLOCN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDEFNMNU()
*!***************************************************************************
FUNCTION lfDEFNMNU
PRIVATE lnBarNo,llfound
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
*--check if the option pad is already defined on the sysmenu
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
	IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
        llfound = .T.
		EXIT
	ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' ' SKIP FOR IIF(TYPE('lcProgName')='C' AND lcProgName = "ICLOCN",.F.,.T.)
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
llFound = .F.
FOR lnCount = 1 TO CNTBAR('_OPTIONPOP')
  IF PRMBAR('_OPTIONPOP', LnCount) = 'Bins Detail'
    llfound = .T.
 	EXIT
  ENDIF
ENDFOR
IF !llfound
  lnBarNo = CNTBAR('_OPTIONPOP') + 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Bins Detail" SKIP FOR gfGetMemVar('M_WARELOC')='N'
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfBinData IN BINMAIN
ENDIF
RETURN
*-- End of Function lfDEFNMNU.
*!***************************************************************************
*!* Name        : lfBinData
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Show the Bin Detail's window 
*!***************************************************************************
*!* Called from : BinMain.prg -->lfDEFNMNU()
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfBinData()
*!***************************************************************************
FUNCTION lfBinData
PRIVATE lcFlatHang, lnCapacity,lcBinClass,lnBlkPck,lcBinSectn,lcWinTitle,llhasStock,lcBnLocatn
STORE '' TO lcWinTitle,lcFlatHang,lcBinClass,lcBinSectn
STORE .F. TO llhasStock
STORE 0 TO lnCapacity,lnBlkPck,lnStock
IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSLOC",'')
  RETURN
ENDIF
IF laScrMode[3] OR laScrMode[4]
  lcFlatHang = IIF(&lcWLocline..cFlatHang='H',2,1) 
  lcBinClass = &lcWLocline..cBinClass
  lnCapacity = &lcWLocline..nCapacity
  *--Get the value for lnBlkPck and section if screen in the edit or the adding mode
  lnBlkPck   = IIF(&lcWLocline..cBlkPck='B',2,1)
  lcBinSectn = &lcWLocline..cSection
ELSE
  lcFlatHang = IIF(WhsLoc.cFlatHang='H',2,1) 
  lcBinClass = WhsLoc.cBinClass
  lnCapacity = WhsLoc.nCapacity
 *--Get the value for CBLKPCK and Section if screen in the view or the selecting mode
 lnBlkPck   = IIF(WhsLoc.cBlkPck='B',2,1)
 lcBinSectn = WhsLoc.cSection
ENDIF
*C132754,1  TMI [Start] Define replenshement field to be used in the icflthn.scx screen
PRIVATE lnRplnsh,lcTbl
lcTbl = IIF(laScrMode[3] OR laScrMode[4],lcWLocline,'WHSLOC')
lnRplnsh = IIF(&lcTbl..CREPLENISH='Y',1,2)
*C132754,1  TMI [End  ] 
IF laScrMode[3]
  IF SEEK(LADATA[1]+&lcWLocline..cLocation,'WHBINLOC')
    llhasStock= .T.
  ENDIF
ENDIF
PUSH KEY
ON KEY
lcWinTitle = 'Warehouse detail for Bin : ' + ALLTRIM(IIF(laScrMode[3] OR laScrMode[4],&lcWLocline..cLocation,WhsLoc.cLocation))
DO (gcScrDir+gcWinAppl+"\ICFLTHN.SPX")
POP KEY
*-- End of Function lfBinData.
*!***************************************************************************
*!* Name        : lfWShwlin
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : When Function to disable or Enable tools on the screen.
*!***************************************************************************
*!* Called from : ICFLTHN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfWShwlin()
*!***************************************************************************
FUNCTION lfWShwlin
IF laScrMode[3] OR laScrMode[4]
  SHOW GET lnCapacity ENABLE
  SHOW GET pbSave     ENABLE
  IF llhasStock
    SHOW GET lnBlkPck   DISABLE
    SHOW GET lcFlatHang DISABLE
    SHOW GET lcBinClass DISABLE
    SHOW GET lcBinSectn DISABLE
  ELSE
    SHOW GET lnBlkPck   ENABLE
    SHOW GET lcFlatHang ENABLE
    SHOW GET lcBinClass ENABLE
    SHOW GET lcBinSectn ENABLE
  ENDIF
ELSE
  SHOW GET lcFlatHang DISABLE
  SHOW GET lcBinClass DISABLE
  SHOW GET lnCapacity DISABLE
  SHOW GET pbSave     DISABLE
  SHOW GET lcBinSectn DISABLE
  SHOW GET lnBlkPck   DISABLE
ENDIF
*-- End of Function lfWShwlin.
*!***************************************************************************
*!* Name        : lfDavMenu
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Add new bar to the Option menu at Style Screen
*!***************************************************************************
*!* Called from : Icstyle.Prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDavMenu()
*!***************************************************************************
FUNCTION lfDavMenu
PRIVATE lnBarNo,llfound
STORE .F. TO llFound
STORE 1 TO lnBarNo
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
*--check if the option pad is already defined on the sysmenu
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _LPOPOPT
  DEFINE POPUP _LPOPOPT MARGIN SHADOW
ENDIF
*-- Count Options BARS to add the new BAR at the end of the Popup
lnBarNo = CNTBAR('_LPOPOPT') + 1
lcForExp = IIF(gfGetMemVar('M_USEEXSSC'),[OR llAllColors],'')
*-- Define New options (Bin Locations),This Option Menu will be enable in the Edit mode.
DEFINE BAR lnBarNo OF _LPOPOPT PROMPT "\<Bin Locations" SKIP FOR (laScrMode[1]) &lcForExp
ON SELECTION BAR lnBarNo OF _LPOPOPT DO lfvBinBrow IN BINMAIN
RETURN
*-- End of Function lfDavMenu.
*!***************************************************************************
*!* Name        : lfvBinBrow
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Function to browse Bin Locations.
*!***************************************************************************
*!* Called from : lfDavMenu --> BINMAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvBinBrow()
*!***************************************************************************
FUNCTION lfvBinBrow
PRIVATE lcbrfields ,lnAlias,lcFile_Ttl,lcWhExp,lcSeekExp,lnClrLen,lnClrStPos,;
        lnStyLen,lnStyStPos,lnScaLen,lnScaStPos
STORE '' TO lcFile_Ttl,lcWhExp,lcSeekExp
STORE 0 TO lnClrLen,lnClrStPos,lnStyLen,lnStyStPos,lnScaLen,lnScaStPos
lnAlias=SELECT()
=lfChkStrct()
IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH') 
ENDIF
IF llAllColors AND ALLTRIM(lcWareCode) <> "All"
  SET ORDER TO TAG WHSTYBIN IN WHBINLOC
  lcSeekExp = 'PADR(ALLTRIM(lcWareCode),6)+lcmajor+lcnmkey'
ENDIF
IF !llAllColors AND ALLTRIM(lcWareCode) = "All"
  SET ORDER TO TAG WHBINLST IN WHBINLOC
  lcSeekExp = 'PADR(lcmajor+lcnmkey,19)'
ENDIF
IF !llAllColors AND ALLTRIM(lcWareCode) <> "All"
  SET ORDER TO TAG WHBINLST IN WHBINLOC
  lcSeekExp = 'PADR(lcmajor+lcnmkey,19)+lcWareCode'
ENDIF
IF llAllColors AND ALLTRIM(lcWareCode) = "All"
  SET ORDER TO TAG WHBINLST IN WHBINLOC
  lcSeekExp = 'lcmajor+lcnmkey'
ENDIF
IF !SEEK(&lcSeekExp,'WHBINLOC') 
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'No bin location found for Style '+ lcMajor+lcNMKey+ ' Warehouse ' +lcWareCode)
   RETURN
ENDIF
*-- if user Select a style with all colors (if not extended Size scale) and all locations then browse for Cwarecode,Locations,Color
*-- if user Select a style with all colors (if not extended Size scale) and a location then browse for Locations,Color
*-- if user Select a style with a color and a location then browse for Locations
*-- if user Select a style with a color and all locations then browse for cWarecode,Locations

lcBrFields = IIF(!llAllColors AND ALLTRIM(lcWareCode) <> "All",[clocation :15 :H='Location',],;
             IIF(!llAllColors AND ALLTRIM(lcWareCode) = "All",[cWareCode :15 :H='Warehouse',clocation :15 :H='Location',],;
             IIF( llAllColors AND ALLTRIM(lcWareCode) ="All",[cWareCode :15 :H='Warehouse',clocation :15 :H='Location',;
                 lcColor=gfCodDes(SUBSTR(STYLE,lnClrStPos,lnClrLen), "COLOR"):H='Colour',],;
                 [lcColor=gfCodDes(SUBSTR(STYLE,lnClrStPos,lnClrLen), "COLOR"):H='Colour',clocation :15 :H='Location',])))
FOR lnI=1 TO lnScaleCnt
  lcI=STR(lnI,1)
  IF !EMPTY(lcSz&lcI)
    lcBrFields = lcBrFields + "Qty"+lcI+" :H=PADL(lcSz"+lcI+",5) :P='9999999',"
  ENDIF
ENDFOR
lcBrFields = lcBrFields + "TotQty :H='Total ' :P='999999999',"
*--Add allocated data  **PADL(lcSz"+lcI+",5)
FOR lnI=1 TO lnScaleCnt
  lcI=STR(lnI,1)
  IF !EMPTY(lcSz&lcI)
    lcBrFields = lcBrFields + "Alo"+lcI+" :H='Alo '+PADL(lcSz"+lcI+",5)  :P='9999999',"
  ENDIF
ENDFOR
lcWhExp = IIF(ALLTRIM(lcWareCode) = "All",'.T.','cWareCode=lcWareCode')
lcBrFields = lcBrFields + "TotAlo :H='Total Alocated ' :P='999999999'"
SELECT WHBINLOC
IF ORDER()='WHSTYBIN'
  =ARIABROW([lcWareCode FOR STYLE=lcMajor+lcNMKey REST],'Bin Location',;
            gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
ELSE
  =ARIABROW([lcMajor+lcNMKey FOR &lcWhExp REST],'Bin Location',;
            gnbrhsrow1, gnbrhscol1, gnbrhsrow2, gnbrhscol2,'','Fi\<nd;Or\<der by;\<Descending;Fi\<lter;;\!\?\<Ok')
ENDIF

SELECT(lnAlias)
RETURN
*-- End of Function lfvBinBrow.
*!***************************************************************************
*!* Name        : lfvsavlin
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Save lines to warehouse file 
*!***************************************************************************
*!* Called from : ICFLTHN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvsavlin()
*!***************************************************************************
FUNCTION lfvsavlin
IF laScrMode[3] OR laScrMode[4]
  REPLACE &lcWLocline..cFlatHang WITH IIF(lcFlatHang=2,'H','F') 					,;
          &lcWLocline..cBinClass WITH IIF(EMPTY(lcBinClass),'',ALLTRIM(lcBinClass)) ,;
          &lcWLocline..nCapacity WITH lnCapacity                                    ,;
          &lcWLocline..cBlkPck   WITH IIF(lnBlkPck=2,'B','P')                       ,;
          &lcWLocline..cSection  WITH lcBinSectn                      
  *C132754,1  TMI [Start] update the replenshemnt field
  REPLACE &lcWLocline..CREPLENISH WITH IIF(lnRplnsh=1,'Y',' ')
  *C132754,1  TMI [End  ]           
  *T20060817.0014   TMI [Start] if any updates are done for the bin then mark this field as E to update only E , A fields
  REPLACE &lcWLocline..CSTATUS WITH 'E'
  *T20060817.0014   TMI [End  ] 
ENDIF
*-- End of Function lfvsavlin.
*!***************************************************************************
*!* Name        : lfDLSAVCS
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Save Bin Location Custom Fields (ICFLTHN.SCX)
*!***************************************************************************
*!* Called from : ICLOCN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSAVCS()
*!***************************************************************************
FUNCTION lfDLSAVCS
PRIVATE lnOldAlias,lnTempRec
lnOldAlias = SELECT(0)
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
SELECT(lcWlocLine)
lnTempRec = RECNO()
LOCATE
*T20060817.0014   TMI [Start] Update for fields with status "A" or "E"
*SCAN FOR cWareCode+cLocation+Style+Color = ladata[1] 
SCAN FOR cWareCode+cLocation+Style+Color = ladata[1] .AND. CSTATUS $ 'AE'
  *T20060817.0014   TMI [End  ] 
  IF SEEK(cWareCode+cLocation+Style+Color,'WHSLOC')
    SELECT WHSLOC
    REPLACE cFlatHang WITH &lcWLocline..cFlatHang ,;
            cBinClass WITH IIF(!EMPTY(&lcWLocline..cBinClass),&lcWLocline..cBinClass,'A'),;
            nCapacity WITH &lcWLocline..nCapacity ,;
            cBlkPck   WITH &lcWLocline..cBlkPck   ,;
            cSection  WITH &lcWLocline..cSection
    *C132754,1  TMI [Start] Update the CREPLENISH field
    REPLACE CREPLENISH WITH &lcWLocline..CREPLENISH
    *C132754,1  TMI [End  ] 
  ENDIF
ENDSCAN
IF BETWEEN(lnTempRec,1,RECCOUNT(lcWlocLine))
  GOTO lnTempRec IN (lcWlocLine)
ENDIF

SELECT(lnOldAlias)
*-- End of Function lfDLSAVCS.
*!********************** End of Inventory Section *!*************************
*!********************** Start of Allocation Section *!**********************
*!***************************************************************************
*!* Name        : lfCHKFORCE
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : if Use Bin Location set to Yes ,I'll Stop force allocation
*!***************************************************************************
*!* Called from : ALORDAL.PRG - ALSTYAL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCHKFORCE()
*!***************************************************************************
FUNCTION lfCHKFORCE
IF gfGetMemVar('M_DLUSEBIN')
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
*-- End of Function lfCHKFORCE.

*!***************************************************************************
*!* Name        : lfChngQty
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Prevent User to Change the Pack Qty and guide him to do that
*!*             : in the Pick Ticket Screen.
*!***************************************************************************
*!* Called from : AlPlist.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChngQty()
*!***************************************************************************
FUNCTION lfChngQty
IF gfGetMemVar('M_DLUSEBIN')
  IF lcProgName = 'ALPLIST' AND lnStyQty <> lcOldVal
    =gfModalGen("INM00000B00000","DIALOG",.F.,.F.,'You can not amend Qty. here,please do that ' + ;
    'at the pick ticket screen')
    IF _CUROBJ = OBJNUM(lnBrCtnQty)
      lnBrCtnQty = lcOldVal
      SHOW GET lnBrCtnQty
    ENDIF
    IF _CUROBJ = OBJNUM(lnStyQty)
      lnStyQty = lcOldVal
      SHOW GET lnStyQty
    ENDIF
  ENDIF
  IF lcProgName = 'ALAUTP' 
    lcPakQtyFl = 'm.'+VARREAD()
    IF &lcPakQtyFl <> lnOldQty
      =gfModalGen("INM00000B00000","DIALOG",.F.,.F.,'You can not amend Qty. here,please do that ' + ;
      'at the pick ticket screen')
      &lcPakQtyFl = lnOldQty
      SHOW GET &lcPakQtyFl
    ENDIF
  ENDIF
  RETURN .F.
ELSE
  RETURN .T.
ENDIF
*-- End of Function lfChngQty.
*!***************************************************************************
*!* Name        : lfAlSavOrd 
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Save Data to Order Allocation screen 
*!***************************************************************************
*!* Called from : ALORDAL.PRG (ALLOCATION BY ORDER)
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAlSavOrd()
*!***************************************************************************
FUNCTION lfAlSavOrd 

IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF

PRIVATE lnPicked,lnRemain,lcAsecend,llPickOne,lcStyle,lnOldAlias,laPikQty,lcBinloc,;
        lcPikTkt,lcSortBin,lcFlatHang,lcWareCode,llBlkPck,lcPrmClss,lcSecClss,lcRemClss

lnOldAlias = SELECT(0)
*T20060817.0014   TMI [Start] move the following line within the loop
*DIMENSION laPikQty[9]
*STORE 0 TO laPikQty,lnRemain,lnOldAlias,lnPicked
*STORE .F. TO llPickOne,llBlkPck
SELECT &lc_tmpOrdL
LOCATE
*T20060817.0014   TMI [End  ] 
lcPikTkt  = IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)) AND &lc_TmpOrdL..PikTkt<>"******",;
                &lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),PIKTKT.PIKTKT,ORDLINE.PIKTKT))

*-- Checking For Use bin location Yes/No Or lcPikTkt = "******" and Open the Needed Files.
IF !lfOpnFiles("WHBINLOC,PKBINLOC,WHSLOC","WHBINLST,PKLINE,WHSLOC",lcPikTkt)
  RETURN
ENDIF
llPickOne = gfGetMemVar('M_PICKONE',gcAct_Comp)  && Check if Always Pick from on bin location Yes or No.
llBlkPck  = gfGetMemVar('M_BULKPICK',gcAct_Comp) && Check if use Bulk/Pick Yes or No.
lcBinloc  = gfTempName()
lcSortBin = gfTempName()

*T20060817.0014   TMI [Start] create a new loop within the trigger
SELECT &lc_tmpOrdL
SCAN
  SCATTER MEMVAR MEMO
  =SEEK(M.STYLE,'STYLE')

  DIMENSION laPikQty[9]
  STORE 0 TO laPikQty,lnRemain,lnOldAlias,lnPicked
  ** STORE .F. TO llPickOne,llBlkPck  && it is wrong to include this line here , we get this variable from the option setup above  09/11/2006
  lcPikTkt  = IIF(!EMPTY(ALLTRIM(&lc_TmpOrdL..PikTkt)) AND &lc_TmpOrdL..PikTkt<>"******",;
                &lc_TmpOrdL..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),PIKTKT.PIKTKT,ORDLINE.PIKTKT))
  IF lcPikTkt = "******"
    LOOP
  ENDIF
  *T20060817.0014   TMI [End  ] 
  
  lcStyle   = Style.Style
  lcFlatHang= STYLE.cFlatHang
  lcWareCode= &lc_TmpOrdL..cWareCode
  ***!***
  
  IF llBlkPck       && if use Bulk/Pick Set to Yes.
  
    SELECT WHBINLOC
    SET RELATION TO
    SET RELATION TO WHBINLOC.CWARECODE+WHBINLOC.CLOCATION+SPACE(19) INTO WHSLOC ADDITIVE
    IF SEEK(lcStyle + lcWareCode ,'WHBINLOC') 
      SELECT WHBINLOC
      *--Scan in the Whbinloc File to get the Bin location that Saved in the Pkbinloc file.
      SCAN REST WHILE STYLE+CWARECODE+CLOCATION = lcStyle+ lcWareCode
        IF SEEK(lcPikTkt + lcWareCode + STR(&lc_TmpOrdL..LineNo,6) + lcStyle,'PKBINLOC') 
          SELECT PKBINLOC
          *T20060817.0014   TMI [Start] put the REST clause to enhance summation
          *SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laPikQty FOR PikTkt = lcPikTkt ;  
              AND cWarecode = lcWareCode AND Style = lcStyle AND LINENO = &lc_TmpOrdL..LineNo 
          SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laPikQty ;
              REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = ;
                         lcPikTkt+lcWareCode+STR(&lc_TmpOrdL..LineNo,6)+lcStyle
          *T20060817.0014   TMI [End  ] 
          
          *--if user Amend in the Piktkt By Decreased the picked qty
          IF (&lc_TmpOrdL..TOTPIK - laPikQty[9]) < 0   	
            SELECT PKBINLOC
            = SEEK(lcPikTkt+lcWareCode + STR(&lc_TmpOrdL..LineNo,6)+lcStyle,'PKBINLOC') 
            SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = ;
                 lcPikTkt+lcWareCode+STR(&lc_TmpOrdL..LineNo,6)+lcStyle
              IF SEEK(PKBINLOC.Style+PKBINLOC.cWarecode+PKBINLOC.cLocation ,'WHBINLOC')
                SELECT WHBINLOC
                *T20060817.0014   TMI [Start] this needs to be written in one line
                *FOR lnPkCount =1 To 8
                *  lcPkCount = ALLTRIM(STR(lnPkCount))
                *  REPLACE WHBINLOC.ALO&lcPkCount  WITH MAX(WHBINLOC.ALO&lcPkCount - PKBINLOC.Qty&lcPkCount,0)
                *ENDFOR 
                *REPLACE WHBINLOC.TOTALO WITH MAX(WHBINLOC.TOTALO - PKBINLOC.TOTQTY,0)
                REPLACE ALO1   WITH MAX(ALO1 - PKBINLOC.Qty1,0) ;
                        ALO2   WITH MAX(ALO2 - PKBINLOC.Qty2,0) ;
                        ALO3   WITH MAX(ALO3 - PKBINLOC.Qty3,0) ;
                        ALO4   WITH MAX(ALO4 - PKBINLOC.Qty4,0) ;
                        ALO5   WITH MAX(ALO5 - PKBINLOC.Qty5,0) ;
                        ALO6   WITH MAX(ALO6 - PKBINLOC.Qty6,0) ;
                        ALO7   WITH MAX(ALO7 - PKBINLOC.Qty7,0) ;
                        ALO8   WITH MAX(ALO8 - PKBINLOC.Qty8,0) ;
                        TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8              
                *T20060817.0014   TMI [End  ] 
              ENDIF    
  
              *T20060817.0014   TMI [Start] delete the related line in plbinloc
              SELECT PKBINLOC
              DELETE
              *T20060817.0014   TMI [End  ] 
  
            ENDSCAN  
            STORE 0 TO laPikQty
            SELECT PKBINLOC
            LOCATE
            *--Delete the old record to Add it again with the New Qty. and Location.
            *T20060817.0014   TMI [Start] delete this line in above in the same loop
            *DELETE ALL FOR PIKTKT = lcPikTkt AND cWareCode = lcWareCode AND STYLE = WHBINLOC.Style AND LINENO = &lc_TmpOrdL..LineNo
            *T20060817.0014   TMI [End  ] 
          ENDIF
        ENDIF  
      ENDSCAN
      SELECT WHBINLOC
      *-- Select statment to get style's records from Whbinloc file and sorted by CBlkPck Field (Depend on OrdHdr.cBlkPck)
      *-- and sorted by Qty(X) Depend on the current Size and if option (Always Pick from one Bin Location) is Set To Yes or No
      *-- lcAsecend -->Hold 'DESCENDING' if ORDHDR.CBLKPCK='P'(PICK) OR hold 'ASCENDING' if ORDHDR.CBLKPCK='B'(BULK)
      *-- I save the Result in the (lcBinLoc) then select records that have Qtys. and match the process size .
      lcAsecend = IIF(ORDHDR.cBlkPck='P','DESCENDING','')
      SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
           (Whbinloc.Qty1-Whbinloc.ALO1) AS QTY1,(Whbinloc.Qty2-Whbinloc.ALO2) AS QTY2,;
           (Whbinloc.Qty3-Whbinloc.ALO3) AS QTY3,(Whbinloc.Qty4-Whbinloc.ALO4) AS QTY4,;
           (Whbinloc.Qty5-Whbinloc.ALO5) AS QTY5,(Whbinloc.Qty6-Whbinloc.ALO6) AS QTY6,;
           (Whbinloc.Qty7-Whbinloc.ALO7) AS QTY7,(Whbinloc.Qty8-Whbinloc.ALO8) AS QTY8,;
           (Whbinloc.TotQty-Whbinloc.TOTALO) AS TOTQTY,Whsloc.cblkpck;
           FROM  Whbinloc, Whsloc;
           WHERE Whsloc.clocation = Whbinloc.clocation AND Whsloc.cWarecode = Whbinloc.cWarecode AND Whsloc.style = SPACE(19);
                 AND Whbinloc.cWarecode = lcWareCode AND Whbinloc.style = lcStyle INTO TABLE (gcWorkDir+lcBinLoc)
      FOR lnCnt = 1 TO 8
        lcCnt = ALLTRIM(STR(lnCnt))
        *-- if there are remain qty. not picked
        IF (&lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt]) > 0
          IF llPickOne				&& Always Pick From one Bin Location / Yes.
            *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field and Qty. for the current Size.
            SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
                  FROM  (gcWorkDir+lcBinLoc) ORDER BY cblkpck &lcAsecend, qty&lcCnt DESCENDING INTO CURSOR &lcSortBin
          ELSE                        && Always Pick From one Bin Location / No.
            *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field only for the current Size.
            SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
                  FROM  (gcWorkDir+lcBinLoc) ORDER BY cblkpck &lcAsecend INTO CURSOR &lcSortBin
          ENDIF
          SELECT (lcSortBin)
          lnRemain = &lc_TmpOrdL..Pik&lcCnt - laPikQty[lnCnt]
          SCAN FOR &lcSortBin..QTY&lcCnt > 0
            SELECT WHBINLOC
            IF &lcSortBin..QTY&lcCnt >= lnRemain
              IF SEEK(lcStyle + &lcSortBin..CWARECODE+&lcSortBin..CLOCATION)
                REPLACE WHBINLOC.ALO&lcCnt WITH MAX(WHBINLOC.ALO&lcCnt+lnRemain,0)               ,;
                        WHBINLOC.TOTALO    WITH MAX(WHBINLOC.TOTALO +lnRemain,0) 
                =lfUpdPkBin(lnRemain,lcCnt,lcPikTkt,&lc_TmpOrdL..Order,&lc_TmpOrdL..LineNo,lcFlatHang,'') 
                EXIT
              ENDIF
            ELSE
              IF SEEK(lcStyle + &lcSortBin..CWARECODE+&lcSortBin..CLOCATION)
                REPLACE WHBINLOC.ALO&lcCnt WITH WHBINLOC.ALO&lcCnt + &lcSortBin..QTY&lcCnt            ,;
                        WHBINLOC.TOTALO    WITH WHBINLOC.TOTALO +&lcSortBin..QTY&lcCnt
                lnRemain = lnRemain - &lcSortBin..Qty&lcCnt
                *--Updates Record in the PkBinLoc File
                =lfUpdPkBin(&lcSortBin..Qty&lcCnt,lcCnt,lcPikTkt,&lc_TmpOrdL..Order,&lc_TmpOrdL..LineNo,lcFlatHang,'')
              ENDIF
            ENDIF
          ENDSCAN
          SELECT WHBINLOC
        ENDIF
        lnPicked = 0
        lnRemain = 0
      ENDFOR
    ENDIF
  
  ELSE 		                        
  
    *--if use Bulk/Pick Set to No.
  
    IF SEEK(lcStyle + lcWareCode ,'WHBINLOC') 
      SELECT WHBINLOC
      *--Scan in the Whbinloc File to get the Bin location that Saved in the Pkbinloc file.
      SCAN REST WHILE STYLE+CWARECODE+CLOCATION = lcStyle+ lcWareCode
        IF SEEK(lcPikTkt+lcWareCode+STR(&lc_TmpOrdL..LineNo,6)+lcStyle ,'PKBINLOC') 
            SELECT PKBINLOC
            SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+lcWareCode+STR(&lc_TmpOrdL..LineNo,6)+lcStyle
              FOR lnPkCount =1 To 8
                lcPkCount = ALLTRIM(STR(lnPkCount))
                laPikQty[lnPkCount] =laPikQty[lnPkCount]+ PKBINLOC.QTY&lcPkCount
              ENDFOR
            ENDSCAN  
          laPikQty[9]= laPikQty[1]+laPikQty[2]+laPikQty[3]+laPikQty[4]+laPikQty[5]+laPikQty[6]+laPikQty[7]+laPikQty[8]
        ENDIF  
      ENDSCAN
  
      *--if user Amend in the Piktkt By Decreased the picked qty
      IF (&lc_TmpOrdL..TotPik - laPikQty[9]) < 0
        SELECT PKBINLOC
        IF SEEK(lcPikTkt+lcWareCode+STR(&lc_TmpOrdL..LineNo,6)+lcStyle)
          SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+lcWareCode +STR(&lc_TmpOrdL..LineNo,6)+lcStyle
            IF SEEK(PKBINLOC.STYLE+PKBINLOC.CWARECODE+PKBINLOC.CLOCATION ,'WHBINLOC')
              SELECT WHBINLOC
              *T20060817.0014   TMI [Start] remove the aliase refrence to enhance speed
              *REPLACE WHBINLOC.ALO1 WITH MAX(WHBINLOC.ALO1 - PKBINLOC.Qty1,0);
              *        WHBINLOC.ALO2 WITH MAX(WHBINLOC.ALO2 - PKBINLOC.Qty2,0);
              *        WHBINLOC.ALO3 WITH MAX(WHBINLOC.ALO3 - PKBINLOC.Qty3,0);
              *        WHBINLOC.ALO4 WITH MAX(WHBINLOC.ALO4 - PKBINLOC.Qty4,0);
              *        WHBINLOC.ALO5 WITH MAX(WHBINLOC.ALO5 - PKBINLOC.Qty5,0);
              *        WHBINLOC.ALO6 WITH MAX(WHBINLOC.ALO6 - PKBINLOC.Qty6,0);
              *        WHBINLOC.ALO7 WITH MAX(WHBINLOC.ALO7 - PKBINLOC.Qty7,0);
              *        WHBINLOC.ALO8 WITH MAX(WHBINLOC.ALO8 - PKBINLOC.Qty8,0);
              *        WHBINLOC.TOTALO WITH MAX(WHBINLOC.TOTALO - PKBINLOC.TotQty,0)
              REPLACE ALO1 WITH MAX(ALO1 - PKBINLOC.Qty1,0) ;
                      ALO2 WITH MAX(ALO2 - PKBINLOC.Qty2,0) ;
                      ALO3 WITH MAX(ALO3 - PKBINLOC.Qty3,0) ;
                      ALO4 WITH MAX(ALO4 - PKBINLOC.Qty4,0) ;
                      ALO5 WITH MAX(ALO5 - PKBINLOC.Qty5,0) ;
                      ALO6 WITH MAX(ALO6 - PKBINLOC.Qty6,0) ;
                      ALO7 WITH MAX(ALO7 - PKBINLOC.Qty7,0) ;
                      ALO8 WITH MAX(ALO8 - PKBINLOC.Qty8,0) ;
                      TOTALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
              *T20060817.0014   TMI [End  ] 
            ENDIF
            
            *T20060817.0014   TMI [Start] delete the related line in plbinloc
            SELECT PKBINLOC
            DELETE
            *T20060817.0014   TMI [End  ] 
            
          ENDSCAN
        ENDIF
        STORE 0 TO laPikQty
        SELECT PKBINLOC
        LOCATE
        *--Delete the old record to Add it again with the New Qty. and Location.
        *T20060817.0014   TMI [Start] replaced by the above delete command within the loop
        *DELETE ALL FOR PIKTKT = lcPikTkt AND cWareCode = lcWareCode AND STYLE = WHBINLOC.Style AND LINENO = &lc_TmpOrdL..LineNo
        *T20060817.0014   TMI [End  ] 
      ENDIF
    ENDIF
    IF SEEK(&lc_TmpOrdL..Style,'STYLE')
      lcStyle    = STYLE.STYLE
      IF SEEK(&lc_TmpOrdL..Style + lcWareCode  ,'WHBINLOC') 
        SELECT WHBINLOC
        SET RELATION TO
        SET RELATION TO WHBINLOC.CWARECODE+WHBINLOC.CLOCATION+SPACE(19) INTO WHSLOC ADDITIVE
        SELECT DISTINCT WHBINLOC.STYLE, WHBINLOC.CWARECODE, WHBINLOC.CLOCATION,;
                       (WHBINLOC.QTY1-WHBINLOC.ALO1) AS QTY1,(WHBINLOC.QTY2-WHBINLOC.ALO2) AS QTY2   ,;
                       (WHBINLOC.QTY3-WHBINLOC.ALO3) AS QTY3,(WHBINLOC.QTY4-WHBINLOC.ALO4) AS QTY4   ,;
                       (WHBINLOC.QTY5-WHBINLOC.ALO5) AS QTY5,(WHBINLOC.QTY6-WHBINLOC.ALO6) AS QTY6   ,;
                       (WHBINLOC.QTY7-WHBINLOC.ALO7) AS QTY7,(WHBINLOC.QTY8-WHBINLOC.ALO8) AS QTY8   ,;
                       (WHBINLOC.TOTQtY-WHBINLOC.TOTALO) AS TOTQTY,WHSLOC.CBINCLASS,WHSLOC.CFLATHANG  ;
              FROM  WHBINLOC, WHSLOC;
              WHERE WHSLOC.CLOCATION = WHBINLOC.CLOCATION AND WHSLOC.CWARECODE = WHBINLOC.CWARECODE ;
                    AND Whbinloc.cWarecode = lcWareCode AND WHSLOC.STYLE = SPACE(19) ;
                    AND WHBINLOC.STYLE = lcStyle AND WHSLOC.CFLATHANG = lcFlatHang ;
              ORDER BY WHBINLOC.CLOCATION ;
              INTO  TABLE (gcWorkDir+lcBinLoc)
        FOR I = 1 TO 8
          LCI = ALLTRIM(STR(I))
          lcPrmClss = STYLE.CPRIMCLSS&LCI
          lcSecClss = STYLE.CSECCLSS&LCI
          lcRemClss = STYLE.CREMCLSS&LCI
          IF !EMPTY(&lc_TmpOrdL..Pik&LCI) AND (&lc_TmpOrdL..Pik&LCI - laPikQty[I]) >0
            IF llPickOne				&& Always Pick From one Bin Location / Yes.
              *-- Select statment from (lcBinLoc) file to sort it by Bin Location Qty. for the current Size.
              SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CFLATHANG,CBINCLASS;
                  FROM  (gcWorkDir+lcBinLoc) ;
                  WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&LCI>0;
                  ORDER BY Qty&LCI DESCENDING ;
                  INTO  CURSOR &lcSortBin
            ELSE
              *-- Select statment from (lcBinLoc) file to sort it by Bin Class and Bin Location Alph. for the current Size.
              *-- Here I Create a new field called CBINTYPE, I fill it with '1' if CBINCLASS = lcPrmClss and So on
              SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CBINCLASS,CFLATHANG,;
                  IIF(CBINCLASS=lcPrmClss,'1',IIF(CBINCLASS=lcSecClss,'2','3')) AS CBINTYPE ;
                  FROM  (gcWorkDir+lcBinLoc) ;
                  WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&LCI>0;
                  ORDER BY CBINTYPE,CLOCATION ;
                  INTO  CURSOR &lcSortBin
            ENDIF
            IF RECCOUNT(lcSortBin)>0
              SELECT(lcSortBin)
              lnRemain = &lc_TmpOrdL..Pik&LCI - laPikQty[I]
              SCAN
                IF &lcSortBin..QTY&LCI >= lnRemain
                  IF SEEK(&lcSortBin..STYLE+&lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
                    SELECT WHBINLOC
                    REPLACE WHBINLOC.ALO&LCI WITH MAX(WHBINLOC.ALO&LCI+ lnRemain,0) ,;
                            WHBINLOC.TOTALO    WITH MAX(WHBINLOC.TOTALO +lnRemain,0)
                    =lfUpdPkBin(lnRemain,LCI,lcPikTkt,&lc_TmpOrdL..Order,&lc_TmpOrdL..LineNo,lcFlatHang,&lcSortBin..cBinClass)
                    EXIT
                  ENDIF
                ELSE
                  IF SEEK(&lcSortBin..STYLE+&lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
                    SELECT WHBINLOC
                    REPLACE WHBINLOC.ALO&LCI   WITH WHBINLOC.ALO&LCI+&lcSortBin..QTY&LCI ,;
                            WHBINLOC.TOTALO    WITH WHBINLOC.TOTALO + &lcSortBin..QTY&LCI
                    lnRemain = lnRemain - &lcSortBin..Qty&LCI
                    =lfUpdPkBin(&lcSortBin..Qty&LCI,LCI,lcPikTkt,&lc_TmpOrdL..Order,&lc_TmpOrdL..LineNo,lcFlatHang,&lcSortBin..cBinClass)
                  ENDIF
                ENDIF
              ENDSCAN  
            ENDIF
          ENDIF
        ENDFOR
      ENDIF
    ENDIF
  ENDIF
  *T20060817.0014   TMI [Start] call check fun. within the new added loop
  =lfChkPktk(lc_TmpOrdL,lcPiktkt,&lc_TmpOrdL..STYLE,&lc_TmpOrdL..LINENO)
ENDSCAN
*T20060817.0014   TMI [End  ] 

*-- Check if the allocation Qtys. is Equal to the ordline Picked Qty. or not
SELECT WHBINLOC
FLUSH

*T20060817.0014   TMI [Start] comment this line and move it up
*=lfChkPktk(lc_TmpOrdL,lcPiktkt,&lc_TmpOrdL..STYLE,&lc_TmpOrdL..LINENO)
*T20060817.0014   TMI [End  ] 

SELECT(lnOldAlias)
=lfEraseFil(lcBinLoc)
*-- End of Function lfAlSavOrd.

*!***************************************************************************
*!* Name        : lfAlSavSty
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Save Data to Style Allocation screen 
*!***************************************************************************
*!* Called from : ALSTYAL.PRG (ALLOCATION BY STYLE)
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAlSavSty()
*!***************************************************************************
FUNCTION lfAlSavSty

IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF

PRIVATE lnPicked,lnRemain,lcAsecend,llPickOne,lcStyle,lnOldAlias,laPikQty,lcBinloc
PRIVATE lcSortBin,lcFlatHang,lcWareCode,lcPrmClss,lcSecClss,lcRemClss
DIMENSION laPikQty[9]
STORE 0 TO laPikQty,lnRemain,lnOldAlias,lnPicked
STORE .F. TO llPickOne
lnOldAlias = SELECT(0)
*-- Checking For Use bin location Yes/No Or lcPikTkt = "******" and Open the Needed Files.
IF !lfOpnFiles("WHBINLOC,WHSLOC,PKBINLOC","WHBINLST,WHSLOC,PKLINE",lcPikTkt)
  RETURN
ENDIF
llPickOne = gfGetMemVar('M_PICKONE',gcAct_Comp)  && Check if Always Pick from on bin location Yes or No
lcBinloc  = gfTempName()
lcSortBin = gfTempName()
lcStyle   = Style.Style
lcFlatHang= STYLE.cFlatHang
lcWareCode= STYDYE.CWARECODE
*--if use Bulk/Pick Set to Yes.
IF gfGetMemVar('M_BULKPICK',gcAct_Comp)
  SELECT WHBINLOC
  SET RELATION TO
  SET RELATION TO Whbinloc.cwarecode+Whbinloc.clocation+SPACE(19) INTO Whsloc ADDITIVE
  IF SEEK(laData[1] + StyDye.cWareCode ,'WHBINLOC') 
    IF !EMPTY(lcPikTkt) AND SEEK(lcPikTkt + lcWareCode + STR(ORDLINE.LineNo,6) + lcStyle,'PKBINLOC') 
      SELECT PKBINLOC
      SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laPikQty FOR PikTkt = lcPikTkt ;
          AND cWarecode = lcWareCode AND Style = lcStyle AND LINENO = ORDLINE.LineNo
        *--if user Amend in the Piktkt By Decreased the picked qty
      IF (ORDLINE.TOTPIK - laPikQty[9]) < 0   	
        SELECT PKBINLOC
        = SEEK(lcPikTkt + lcWareCode + STR(ORDLINE.LineNo,6) + lcStyle,'PKBINLOC') 
        SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+lcWareCode+STR(ORDLINE.LineNo,6)+lcStyle
          IF SEEK(PKBINLOC.Style+PKBINLOC.cWarecode+PKBINLOC.cLocation ,'WHBINLOC')
            SELECT WHBINLOC
            FOR lnPkCount =1 To 8
              lcPkCount = ALLTRIM(STR(lnPkCount))
              REPLACE WHBINLOC.ALO&lcPkCount  WITH MAX(WHBINLOC.ALO&lcPkCount - PKBINLOC.Qty&lcPkCount,0)
            ENDFOR
                REPLACE WHBINLOC.TOTALO WITH MAX(WHBINLOC.TOTALO - PKBINLOC.TOTQTY,0)
            ENDIF    
        ENDSCAN  
        STORE 0 TO laPikQty
        SELECT PKBINLOC
        LOCATE
        *--Delete the old record to Add it again with the New Qty. and Location.
        DELETE ALL FOR PIKTKT = ORDLINE.PikTkt AND cWareCode = ORDLINE.cWareCode ;
               AND STYLE = ORDLINE.Style AND LINENO = ORDLINE.LineNo
      ENDIF
    ENDIF  
    SELECT WHBINLOC
    lcAsecend = IIF(ORDHDR.cBlkPck='P','DESCENDING','')
    SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
         (Whbinloc.Qty1-Whbinloc.ALO1) AS QTY1,(Whbinloc.Qty2-Whbinloc.ALO2) AS QTY2,;
         (Whbinloc.Qty3-Whbinloc.ALO3) AS QTY3,(Whbinloc.Qty4-Whbinloc.ALO4) AS QTY4,;
         (Whbinloc.Qty5-Whbinloc.ALO5) AS QTY5,(Whbinloc.Qty6-Whbinloc.ALO6) AS QTY6,;
         (Whbinloc.Qty7-Whbinloc.ALO7) AS QTY7,(Whbinloc.Qty8-Whbinloc.ALO8) AS QTY8,;
         (Whbinloc.TotQty-Whbinloc.TOTALO) AS TOTQTY,Whsloc.cblkpck;
         FROM  Whbinloc, Whsloc;
         WHERE Whsloc.clocation = Whbinloc.clocation AND Whsloc.cWarecode = Whbinloc.cWarecode AND Whsloc.style = SPACE(19) ;
               AND Whbinloc.cWarecode = lcWareCode AND Whbinloc.style = lcStyle INTO TABLE (gcWorkDir+lcBinLoc)
    FOR lnCnt = 1 TO 8
      lcCnt = ALLT(STR(lnCnt,2))
      *!* if there are remain qty. not picked
      IF (ORDLINE.Pik&lcCnt - laPikQty[lnCnt]) > 0
        IF llPickOne				&& Always Pick From one Bin Location / Yes.
          *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field and Qty. for the current Size.
          SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
                FROM  (gcWorkDir+lcBinLoc) ORDER BY cblkpck &lcAsecend, qty&lcCnt DESCENDING INTO CURSOR &lcSortBin
        ELSE                        && Always Pick From one Bin Location set to No.
          *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field for the current Size.
          SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
                FROM  (gcWorkDir+lcBinLoc) ORDER BY cblkpck &lcAsecend INTO CURSOR &lcSortBin
        ENDIF
        SELECT (lcSortBin)
        lnRemain = ORDLINE.Pik&lcCnt - laPikQty[lnCnt]
        SCAN FOR &lcSortBin..QTY&lcCnt > 0
          SELECT WHBINLOC
          IF &lcSortBin..QTY&lcCnt >= lnRemain
            IF SEEK(lcStyle + &lcSortBin..CWARECODE+&lcSortBin..CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnRemain,0)              ,;
                      WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnRemain,0) 
              =lfUpdPkBin(lnRemain,lcCnt,lcPikTkT,OrdHdr.Order,OrdLine.LineNo)
              EXIT
            ENDIF
          ELSE
            IF SEEK(lcStyle + &lcSortBin..CWARECODE+&lcSortBin..CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt + &lcSortBin..Qty&lcCnt      ,;
                      WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +&lcSortBin..Qty&lcCnt
              lnRemain = lnRemain - &lcSortBin..Qty&lcCnt
              *--Updates Record in the PkBinLoc File
              =lfUpdPkBin(&lcSortBin..Qty&lcCnt,lcCnt,lcPikTkT,OrdHdr.Order,OrdLine.LineNo)
            ENDIF
          ENDIF
        ENDSCAN
        SELECT WHBINLOC
      ENDIF
      lnPicked = 0
      lnRemain = 0
    ENDFOR
  ENDIF
*--if use Bulk/Pick Set to No.
ELSE
  IF SEEK(Style.Style+ StyDye.cWareCode ,'WHBINLOC') 
    SELECT WHBINLOC
    *--Scan in the Whbinloc File to get the Bin location that Saved in the Pkbinloc file.
    SCAN REST WHILE STYLE+CWARECODE+CLOCATION = Style.Style+ StyDye.cWareCode
      *--In Case of Edit.
      IF SEEK(lcPikTkt+OrdLine.cWareCode+STR(OrdLine.LineNo,6)+lcStyle ,'PKBINLOC') 
          SELECT PKBINLOC
          SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+OrdLine.cWareCode+STR(OrdLine.LineNo,6)+lcStyle
            FOR lnPkCount =1 To 8
              lcPkCount = ALLTRIM(STR(lnPkCount))
              laPikQty[lnPkCount] =laPikQty[lnPkCount]+ PKBINLOC.QTY&lcPkCount
            ENDFOR
          ENDSCAN  
        laPikQty[9]= laPikQty[1]+laPikQty[2]+laPikQty[3]+laPikQty[4]+laPikQty[5]+laPikQty[6]+laPikQty[7]+laPikQty[8]
      ENDIF  
    ENDSCAN
    *--if user Amend in the Piktkt By Decreased the picked qty
    IF (ORDLINE.TotPik - laPikQty[9]) < 0
      SELECT PKBINLOC
      IF SEEK(lcPikTkt+lcWareCode +STR(ORDLINE.LineNo,6)+lcStyle)
        SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+lcWareCode +STR(ORDLINE.LineNo,6)+lcStyle
          IF SEEK(PKBINLOC.STYLE+PKBINLOC.CWARECODE+PKBINLOC.CLOCATION ,'WHBINLOC')
            SELECT WHBINLOC
            REPLACE WHBINLOC.ALO1 WITH MAX(WHBINLOC.ALO1 - PKBINLOC.Qty1,0);
                    WHBINLOC.ALO2 WITH MAX(WHBINLOC.ALO2 - PKBINLOC.Qty2,0);
                    WHBINLOC.ALO3 WITH MAX(WHBINLOC.ALO3 - PKBINLOC.Qty3,0);
                    WHBINLOC.ALO4 WITH MAX(WHBINLOC.ALO4 - PKBINLOC.Qty4,0);
                    WHBINLOC.ALO5 WITH MAX(WHBINLOC.ALO5 - PKBINLOC.Qty5,0);
                    WHBINLOC.ALO6 WITH MAX(WHBINLOC.ALO6 - PKBINLOC.Qty6,0);
                    WHBINLOC.ALO7 WITH MAX(WHBINLOC.ALO7 - PKBINLOC.Qty7,0);
                    WHBINLOC.ALO8 WITH MAX(WHBINLOC.ALO8 - PKBINLOC.Qty8,0);
                    WHBINLOC.TOTALO WITH MAX(WHBINLOC.TOTALO - PKBINLOC.TotQty,0)
          ENDIF
        ENDSCAN
      ENDIF
      STORE 0 TO laPikQty
      SELECT PKBINLOC
      LOCATE
      *--Delete the old record to Add it again with the New Qty. and Location.
      DELETE ALL FOR PIKTKT = ORDLINE.PikTkt AND cWareCode = ORDLINE.cWareCode ;
             AND STYLE = ORDLINE.Style AND LINENO = ORDLINE.LineNo
    ENDIF
  ENDIF
  IF SEEK(lcStyle,'STYLE')
    IF SEEK(lcStyle + OrdLine.cWareCode ,'WHBINLOC') 
      SELECT WHBINLOC
      SET RELATION TO
      SET RELATION TO WHBINLOC.CWARECODE+WHBINLOC.CLOCATION+SPACE(19) INTO WHSLOC ADDITIVE
      SELECT DISTINCT WHBINLOC.STYLE, WHBINLOC.CWARECODE, WHBINLOC.CLOCATION,;
                     (WHBINLOC.QTY1-WHBINLOC.ALO1) AS QTY1,(WHBINLOC.QTY2-WHBINLOC.ALO2) AS QTY2  ,;
                     (WHBINLOC.QTY3-WHBINLOC.ALO3) AS QTY3,(WHBINLOC.QTY4-WHBINLOC.ALO4) AS QTY4  ,;
                     (WHBINLOC.QTY5-WHBINLOC.ALO5) AS QTY5,(WHBINLOC.QTY6-WHBINLOC.ALO6) AS QTY6  ,;
                     (WHBINLOC.QTY7-WHBINLOC.ALO7) AS QTY7,(WHBINLOC.QTY8-WHBINLOC.ALO8) AS QTY8  ,;
                     (WHBINLOC.TOTQtY-WHBINLOC.TOTALO) AS TOTQTY,WHSLOC.CBINCLASS,WHSLOC.CFLATHANG ;
            FROM  WHBINLOC, WHSLOC;
            WHERE WHSLOC.CLOCATION = WHBINLOC.CLOCATION AND WHSLOC.CWARECODE = WHBINLOC.CWARECODE ;
                  AND WHSLOC.STYLE = SPACE(19) AND Whbinloc.cWarecode = lcWareCode ;
                  AND WHBINLOC.STYLE = lcStyle AND WHSLOC.CFLATHANG = lcFlatHang AND Whbinloc.cWarecode = lcWareCode ;
            ORDER BY WHBINLOC.CLOCATION ;
            INTO  TABLE (gcWorkDir+lcBinLoc)
      FOR I = 1 TO 8
        LCI = ALLTRIM(STR(I))
        lcPrmClss = STYLE.CPRIMCLSS&LCI
        lcSecClss = STYLE.CSECCLSS&LCI
        lcRemClss = STYLE.CREMCLSS&LCI
        IF !EMPTY(OrdLine.Pik&LCI) AND (OrdLine.Pik&LCI - laPikQty[I]) >0
          IF llPickOne				&& Always Pick From one Bin Location / Yes.
            *-- Select statment from (lcBinLoc) file to sort it by Bin Location Qty. for the current Size.
            SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CBINCLASS,CFLATHANG;
                FROM  (gcWorkDir+lcBinLoc) ;
                WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&LCI>0;
                ORDER BY Qty&LCI DESCENDING ;
                INTO  CURSOR &lcSortBin
          ELSE
            *-- Select statment from (lcBinLoc) file to sort it by Bin Class and Bin Location Alph. for the current Size.
            *-- Here I Create a new field called CBINTYPE, I fill it with '1' if CBINCLASS = lcPrmClss and So on
            SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CBINCLASS,CFLATHANG,;
                IIF(CBINCLASS=lcPrmClss,'1',IIF(CBINCLASS=lcSecClss,'2','3')) AS CBINTYPE ;
                FROM  (gcWorkDir+lcBinLoc) ;
                WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&LCI>0;
                ORDER BY CBINTYPE,CLOCATION ;
                INTO  CURSOR &lcSortBin
          ENDIF
          IF RECCOUNT(lcSortBin)>0
            SELECT(lcSortBin)
            lnRemain = OrdLine.Pik&LCI - laPikQty[I]
            SCAN
              IF &lcSortBin..QTY&LCI >= lnRemain
                IF SEEK(&lcSortBin..STYLE+&lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
                  SELECT WHBINLOC
                  REPLACE WHBINLOC.ALO&LCI WITH MAX(WHBINLOC.ALO&LCI+ lnRemain,0) ,;
                          WHBINLOC.TOTALO    WITH MAX(WHBINLOC.TOTALO +lnRemain,0)
                  =lfUpdPkBin(lnRemain,LCI,lcPikTkt,OrdLine.Order,OrdLine.LineNo,lcFlatHang,&lcSortBin..cBinClass)
                  EXIT
                ENDIF
              ELSE
                IF SEEK(&lcSortBin..STYLE+&lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
                  SELECT WHBINLOC
                  REPLACE WHBINLOC.ALO&LCI   WITH WHBINLOC.ALO&LCI+&lcSortBin..QTY&LCI ,;
                          WHBINLOC.TOTALO    WITH WHBINLOC.TOTALO + &lcSortBin..QTY&LCI
                  lnRemain = lnRemain - &lcSortBin..Qty&LCI
                  =lfUpdPkBin(&lcSortBin..Qty&LCI,LCI,lcPikTkt,OrdLine.Order,OrdLine.LineNo,lcFlatHang,&lcSortBin..cBinClass)
                ENDIF
              ENDIF
            ENDSCAN  
          ENDIF
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
ENDIF
*-- Check if the allocation Qtys. is Equal to the ordline Picked Qty. or not
SELECT WHBINLOC
FLUSH
=lfChkPktk("ORDLINE",IIF(!EMPTY(lcPikTkT),lcPikTkT,OrdLine.PikTkT),lcStyle,Ordline.LINENO)
SELECT(lnOldAlias)
=lfEraseFil(lcBinLoc)

*-- End of Function lfAlSavSty.
*!***************************************************************************
*!* Name        : lfAlSavAut
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Save Data to Automatic Allocation screen 
*!***************************************************************************
*!* Called from : ALAutAL.PRG (ALLOCATION BY STYLE) , MBIMAIN.PRG (Allocate from 
*!*             : sales order screen (CP#200236)
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAlSavAut()
*!***************************************************************************
FUNCTION lfAlSavAut

IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF

PRIVATE lnPicked,lnRemain,lcAsecend,llPickOne,lcStyle,lnOldAlias,laPikQty,lcBinloc
PRIVATE lcPikTkt,lcSortBin,lcFlatHang,lcWareCode,lcOrdFile,lcPrmClss,lcSecClss,lcRemClss
DIMENSION laPikQty[9]
STORE 0 TO laPikQty,lnRemain,lnOldAlias,lnPicked
STORE '' TO lcPikTkt,lcOrdFile
STORE .F. TO llPickOne
*-- if this function called from Alautal.prg (Automatic Allocation) then file (lcTmpOrdLn) will be used
*-- but if it called from Mbimain.prg (Allocate from sales order) then file (lcOrdLine) will be used
IF TYPE('lcTmpOrdLn')<>'U'
  IF !USED(lcTmpOrdLn) AND USED(lcOrdLine)
    lcTmpOrdLn = lcOrdLine
  ELSE
    IF !USED(lcTmpOrdLn) AND USED('ORDLINE')
      lcTmpOrdLn = 'ORDLINE'
    ENDIF
  ENDIF
ELSE
  IF USED(lcOrdLine)
    lcTmpOrdLn = lcOrdLine  
  ELSE
    lcTmpOrdLn = OrdLine
  ENDIF
ENDIF
lnOldAlias = SELECT(0)
lcPikTkt  = IIF(!EMPTY(ALLTRIM(&lcTmpOrdLn..PikTkt)),&lcTmpOrdLn..PikTkt,IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),;
                PIKTKT.PIKTKT,ORDLINE.PIKTKT))
*--In Case of Pick from Sales Order Screen and in Edit Mode 
IF lcProgName='SOORD' AND laScrMode[3] AND lcPikTkt = "******"
  lcPikTkt = IIF(!EMPTY(ALLTRIM(PIKTKT.PIKTKT)),PIKTKT.PIKTKT,ORDLINE.PIKTKT)
ENDIF

*-- Checking For Use bin location Yes/No Or lcPikTkt = "******" and Open the Needed Files.
IF !lfOpnFiles("WHBINLOC,WHSLOC,PKBINLOC","WHBINLST,WHSLOC,PKLINE",lcPikTkt)
  RETURN
ENDIF
llPickOne = gfGetMemVar('M_PICKONE',gcAct_Comp)  && Check if Always Pick from on bin location Yes or No
lcBinloc  = gfTempName()
lcSortBin = gfTempName()
lcStyle   = IIF(SEEK(ORDLINE.STYLE,'STYDYE'),StyDye.Style,ORDLINE.STYLE)
lcFlatHang= IIF(SEEK(ORDLINE.STYLE,'STYLE'),STYLE.cFlatHang,'F')
lcWareCode= &lcTmpOrdLn..CWARECODE
*--if use Bulk/Pick Set to Yes.
IF gfGetMemVar('M_BULKPICK',gcAct_Comp)
  SELECT WHBINLOC
  SET RELATION TO
  SET RELATION TO Whbinloc.cwarecode+Whbinloc.clocation+SPACE(19) INTO Whsloc ADDITIVE
  IF SEEK(lcStyle + lcWareCode ,'WHBINLOC') 
    IF SEEK(lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LineNo,6)+lcStyle,'PKBINLOC') 
      SELECT PKBINLOC
      SUM REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt + lcWareCode + ;
          STR(&lcTmpOrdLn..LineNo,6) + lcStyle QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laPikQty 
      IF (&lcTmpOrdLn..TOTPIK - laPikQty[9]) < 0   	&& if user Decreased the picked qty
        = SEEK(lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LineNo,6)+lcStyle,'PKBINLOC') 
        SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+lcWareCode +;
                  STR(&lcTmpOrdLn..LineNo,6)+lcStyle
          IF SEEK(Style+cWarecode+cLocation ,'WHBINLOC')
            SELECT WHBINLOC
            FOR lnPkCount =1 To 8
              lcPkCount = ALLT(STR(lnPkCount,2))
              REPLACE WHBINLOC.Alo&lcPkCount  WITH MAX(WHBINLOC.Alo&lcPkCount - PKBINLOC.Qty&lcPkCount,0)
            ENDFOR
              REPLACE WHBINLOC.TOTALO WITH MAX(WHBINLOC.TOTALO - PKBINLOC.TOTQTY,0)
          ENDIF    
        ENDSCAN  
        STORE 0 TO laPikQty
        SELECT PKBINLOC
        IF SEEK(lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LineNo,6)+lcStyle) 
          DELETE REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+lcWareCode +;
                 STR(&lcTmpOrdLn..LineNo,6)+lcStyle
        ENDIF
      ENDIF
    ENDIF  
    SELECT WHBINLOC
    lcAsecend = IIF(ORDHDR.cBlkPck='P','DESCENDING','')
    *--IF we're in sales order screen and this a new order, so until this point Filed CBLKPCK still not Saved yet so I'll
    *--Depend on laUSrfields that hold the field name and value .
    
    IF lcProgName='SOORD' AND laScrMode[4]
      FOR J = 1 TO ALEN(laUsrFields,1)
        IF ALLTRIM(laUsrFields[J,1]) = "CBLKPCK"
          lcAsecend = IIF('P' $ laUsrFields[J,6],'DESCENDING','')
        ENDIF
      ENDFOR
    ENDIF
    SELECT DISTINCT Whbinloc.Style, Whbinloc.cWarecode, Whbinloc.cLocation,;
           (Whbinloc.Qty1-Whbinloc.Alo1) AS QTY1,(Whbinloc.Qty2-Whbinloc.Alo2) AS QTY2,;
           (Whbinloc.Qty3-Whbinloc.Alo3) AS QTY3,(Whbinloc.Qty4-Whbinloc.Alo4) AS QTY4,;
           (Whbinloc.Qty5-Whbinloc.Alo5) AS QTY5,(Whbinloc.Qty6-Whbinloc.Alo6) AS QTY6,;
           (Whbinloc.Qty7-Whbinloc.Alo7) AS QTY7,(Whbinloc.Qty8-Whbinloc.Alo8) AS QTY8,;
           (Whbinloc.TotQty-Whbinloc.TotAlo) AS TOTQTY,Whsloc.cblkpck;
           FROM  Whbinloc, Whsloc;
           WHERE Whsloc.clocation = Whbinloc.clocation AND Whsloc.cWarecode = Whbinloc.cWarecode AND Whsloc.style=SPACE(19);
                 AND Whbinloc.style = lcStyle AND Whbinloc.cWarecode = lcWareCode INTO TABLE (gcWorkDir+lcBinLoc)
    FOR lnCnt = 1 TO 8
      lcCnt = ALLT(STR(lnCnt,2))
      *!* if there are remain qty. not picked
      IF (&lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]) > 0
        IF llPickOne				&& Always Pick From one Bin Location / Yes.
          *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field and Qty. for the current Size.
          SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
                FROM  (gcWorkDir+lcBinLoc) ORDER BY cblkpck &lcAsecend, qty&lcCnt DESCENDING INTO CURSOR &lcSortBin
        ELSE				&& Always Pick From one Bin Location / No.
          *-- Select statment from (lcBinLoc) file to sort it by Cblkpck field Only for the current Size.
          SELECT Style,cWarecode,cLocation,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,cblkpck;
                FROM  (gcWorkDir+lcBinLoc) ORDER BY cblkpck &lcAsecend INTO CURSOR &lcSortBin
        ENDIF

        SELECT (lcSortBin)
        lnRemain = &lcTmpOrdLn..Pik&lcCnt - laPikQty[lnCnt]
        SCAN FOR &lcSortBin..QTY&lcCnt > 0
          SELECT WHBINLOC
          IF &lcSortBin..QTY&lcCnt >= lnRemain
            IF SEEK(lcStyle + &lcSortBin..CWARECODE+&lcSortBin..CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt+lnRemain,0)               ,;
                      WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo +lnRemain,0) 
              =lfUpdPkBin(lnRemain,lcCnt,lcPikTkT,OrdHdr.Order,&lcTmpOrdLn..LineNo)
              EXIT
            ENDIF
          ELSE
            IF SEEK(lcStyle + &lcSortBin..CWARECODE+&lcSortBin..CLOCATION)
              REPLACE WHBINLOC.Alo&lcCnt WITH WHBINLOC.Alo&lcCnt + &lcSortBin..Qty&lcCnt            ,;
                      WHBINLOC.TotAlo    WITH WHBINLOC.TotAlo +&lcSortBin..Qty&lcCnt
              lnRemain = lnRemain - &lcSortBin..Qty&lcCnt
              *--Updates Record in the PkBinLoc File
              =lfUpdPkBin(&lcSortBin..Qty&lcCnt,lcCnt,lcPikTkT,OrdHdr.Order,&lcTmpOrdLn..LineNo)
            ENDIF
          ENDIF
        ENDSCAN
        SELECT WHBINLOC
      ENDIF
      lnPicked = 0
      lnRemain = 0
    ENDFOR
  ENDIF
*--if use Bulk/Pick Set to No.
ELSE
  IF SEEK(Style.Style+ StyDye.cWareCode ,'WHBINLOC') 
    SELECT WHBINLOC
    *--Scan in the Whbinloc File to get the Bin location that Saved in the Pkbinloc file.
    SCAN REST WHILE STYLE+CWARECODE+CLOCATION = Style.Style+ StyDye.cWareCode
      *--In Case of Edit.
      IF SEEK(lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LineNo,6)+lcStyle ,'PKBINLOC') 
          SELECT PKBINLOC
          SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LineNo,6)+lcStyle
            FOR lnPkCount =1 To 8
              lcPkCount = ALLTRIM(STR(lnPkCount))
              laPikQty[lnPkCount] =laPikQty[lnPkCount]+ PKBINLOC.QTY&lcPkCount
            ENDFOR
          ENDSCAN  
        laPikQty[9]= laPikQty[1]+laPikQty[2]+laPikQty[3]+laPikQty[4]+laPikQty[5]+laPikQty[6]+laPikQty[7]+laPikQty[8]
      ENDIF  
    ENDSCAN
    *--if user Amend in the Piktkt By Decreased the picked qty
    IF (&lcTmpOrdLn..TotPik - laPikQty[9]) < 0
      SELECT PKBINLOC
      IF SEEK(lcPikTkt+lcWareCode +STR(&lcTmpOrdLn..LineNo,6)+lcStyle)
        SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LineNo,6)+lcStyle
          IF SEEK(PKBINLOC.STYLE+PKBINLOC.CWARECODE+PKBINLOC.CLOCATION ,'WHBINLOC')
            SELECT WHBINLOC
            REPLACE WHBINLOC.ALO1 WITH MAX(WHBINLOC.ALO1 - PKBINLOC.Qty1,0);
                    WHBINLOC.ALO2 WITH MAX(WHBINLOC.ALO2 - PKBINLOC.Qty2,0);
                    WHBINLOC.ALO3 WITH MAX(WHBINLOC.ALO3 - PKBINLOC.Qty3,0);
                    WHBINLOC.ALO4 WITH MAX(WHBINLOC.ALO4 - PKBINLOC.Qty4,0);
                    WHBINLOC.ALO5 WITH MAX(WHBINLOC.ALO5 - PKBINLOC.Qty5,0);
                    WHBINLOC.ALO6 WITH MAX(WHBINLOC.ALO6 - PKBINLOC.Qty6,0);
                    WHBINLOC.ALO7 WITH MAX(WHBINLOC.ALO7 - PKBINLOC.Qty7,0);
                    WHBINLOC.ALO8 WITH MAX(WHBINLOC.ALO8 - PKBINLOC.Qty8,0);
                    WHBINLOC.TOTALO WITH MAX(WHBINLOC.TOTALO - PKBINLOC.TotQty,0)
          ENDIF
        ENDSCAN
      ENDIF
      STORE 0 TO laPikQty
      SELECT PKBINLOC
      IF SEEK(lcPikTkt+lcWareCode+STR(&lcTmpOrdLn..LineNo,6)+lcStyle) 
        DELETE REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPikTkt+lcWareCode +;
              STR(&lcTmpOrdLn..LineNo,6)+lcStyle
      ENDIF
    ENDIF
  ENDIF
  IF SEEK(lcStyle,'STYLE')
    IF SEEK(lcStyle + &lcTmpOrdLn..cWareCode ,'WHBINLOC') 
      SELECT WHBINLOC
      SET RELATION TO
      SET RELATION TO WHBINLOC.CWARECODE+WHBINLOC.CLOCATION+SPACE(19) INTO WHSLOC ADDITIVE
      SELECT DISTINCT WHBINLOC.STYLE, WHBINLOC.CWARECODE, WHBINLOC.CLOCATION,;
                     (WHBINLOC.QTY1-WHBINLOC.ALO1) AS QTY1,(WHBINLOC.QTY2-WHBINLOC.ALO2) AS QTY2  ,;
                     (WHBINLOC.QTY3-WHBINLOC.ALO3) AS QTY3,(WHBINLOC.QTY4-WHBINLOC.ALO4) AS QTY4  ,;
                     (WHBINLOC.QTY5-WHBINLOC.ALO5) AS QTY5,(WHBINLOC.QTY6-WHBINLOC.ALO6) AS QTY6  ,;
                     (WHBINLOC.QTY7-WHBINLOC.ALO7) AS QTY7,(WHBINLOC.QTY8-WHBINLOC.ALO8) AS QTY8  ,;
                     (WHBINLOC.TOTQtY-WHBINLOC.TOTALO) AS TOTQTY,WHSLOC.CBINCLASS,WHSLOC.CFLATHANG ;
            FROM  WHBINLOC, WHSLOC;
            WHERE WHSLOC.CLOCATION = WHBINLOC.CLOCATION AND WHSLOC.CWARECODE=WHBINLOC.CWARECODE AND WHSLOC.STYLE=SPACE(19) ;
                  AND Whbinloc.cWarecode = lcWareCode AND WHBINLOC.STYLE = lcStyle AND WHSLOC.CFLATHANG = lcFlatHang ;
            INTO  TABLE (gcWorkDir+lcBinLoc)
      FOR I = 1 TO 8
        LCI = ALLTRIM(STR(I))
        lcPrmClss = STYLE.CPRIMCLSS&LCI
        lcSecClss = STYLE.CSECCLSS&LCI
        lcRemClss = STYLE.CREMCLSS&LCI
        IF !EMPTY(&lcTmpOrdLn..Pik&LCI) AND (&lcTmpOrdLn..Pik&LCI - laPikQty[I]) >0
          IF llPickOne				&& Always Pick From one Bin Location / Yes.
            *-- Select statment from (lcBinLoc) file to sort it by Bin Location Qty. for the current Size.
            SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CBINCLASS,CFLATHANG;
                FROM  (gcWorkDir+lcBinLoc) ;
                WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&LCI>0;
                ORDER BY Qty&LCI DESCENDING;
                INTO  CURSOR &lcSortBin
            ELSE				    && Always Pick From one Bin Location / No.
            *-- Select statment from (lcBinLoc) file to sort it by Bin Class and Bin Location Alph. for the current Size.
            *-- Here I Create a new field called CBINTYPE, I fill it with '1' if CBINCLASS = lcPrmClss and So on
            SELECT STYLE,CWARECODE,CLOCATION,QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,CBINCLASS,CFLATHANG,;
                IIF(CBINCLASS=lcPrmClss,'1',IIF(CBINCLASS=lcSecClss,'2','3')) AS CBINTYPE ;
                FROM  (gcWorkDir+lcBinLoc) ;
                WHERE INLIST(CBINCLASS,lcPrmClss,lcSecClss,lcRemClss) AND QTY&LCI>0;
                ORDER BY CBINTYPE,CLOCATION ;
                INTO  CURSOR &lcSortBin
          ENDIF

          IF RECCOUNT(lcSortBin)>0
            SELECT(lcSortBin)
            lnRemain = &lcTmpOrdLn..Pik&LCI - laPikQty[I]
            SCAN
              IF &lcSortBin..QTY&LCI >= lnRemain
                IF SEEK(&lcSortBin..STYLE+&lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
                  SELECT WHBINLOC
                  REPLACE WHBINLOC.ALO&LCI WITH MAX(WHBINLOC.ALO&LCI + lnRemain,0) ,;
                          WHBINLOC.TOTALO    WITH MAX(WHBINLOC.TOTALO +lnRemain,0)
                  =lfUpdPkBin(lnRemain,LCI,lcPikTkt,&lcTmpOrdLn..Order,&lcTmpOrdLn..LineNo,lcFlatHang,&lcSortBin..cBinClass)
                  EXIT
                ENDIF
              ELSE
                IF SEEK(&lcSortBin..STYLE+&lcSortBin..CWARECODE+&lcSortBin..CLOCATION,'WHBINLOC')
                  SELECT WHBINLOC
                  REPLACE WHBINLOC.ALO&LCI   WITH WHBINLOC.ALO&LCI+&lcSortBin..QTY&LCI ,;
                          WHBINLOC.TOTALO    WITH WHBINLOC.TOTALO + &lcSortBin..QTY&LCI
                  lnRemain = lnRemain - &lcSortBin..Qty&LCI
                  =lfUpdPkBin(&lcSortBin..Qty&LCI,LCI,lcPikTkt,&lcTmpOrdLn..Order,&lcTmpOrdLn..LineNo,lcFlatHang,&lcSortBin..cBinClass)
                ENDIF
              ENDIF
            ENDSCAN  
          ENDIF
        ENDIF
      ENDFOR
    ENDIF
  ENDIF
ENDIF
*-- Check if the allocation Qtys. is Equal to the ordline Picked Qty. or not
SELECT WHBINLOC
FLUSH
=lfChkPktk((lcTmpOrdLn),lcPikTkT,lcStyle,&lcTmpOrdLn..LINENO)
SELECT(lnOldAlias)
=lfEraseFil(lcBinLoc)
*-- End of Function lfAlSavAut.
*!***************************************************************************
*!* Name        : lfAlRelOrd 
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Release piktkt in Order Allocation screen and Style Allocation Screen
*!***************************************************************************
*!* Called from : ALORDAL.PRG , ALSTYAL.PRG , ALRELPI.PRG , ALPKTKT
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfALRELORD()
*!***************************************************************************
FUNCTION lfALRELORD 
=lfRelease('ORDLINE')
*-- End of Function lfALRELORD.
*!***************************************************************************
*!* Name        : lfAlRelAut
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Release piktkt in Automatic Allocation screen 
*!***************************************************************************
*!* Called from : ALAUTAL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfAlRelAut()
*!***************************************************************************
FUNCTION lfAlRelAut
=lfRelease(lcTmpOrdLn)
*-- End of Function lfALRELAUT.
*!***************************************************************************
*!* Name        : lfRelease
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Release piktkt in Both of 
*!*             :      -- Order Allocation screen       (AlordAl)
*!*             :      -- Style Allocation Screen       (AlStyAl)
*!*             :      -- Automatic Allocation Screen   (AlAutAl)
*!*             :      -- Piktkt Screen                 (AlPkTkt)
*!*             :      -- Release Piktkt Screen         (AlRelPi)
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfRelease()
*!***************************************************************************
FUNCTION lfRelease
PARAMETER lcFile
*-- Checking For Use bin location Yes/No Or lcPikTkt = "******" and Open the Needed Files.
IF !lfOpnFiles("WHBINLOC,PKBINLOC","WHBINLST,PKLINE",&lcFile..PikTkt)
  RETURN
ENDIF
SELECT PKBINLOC
IF SEEK(&lcFile..PikTkt + &lcFile..cWareCode + STR(&lcFile..LINENO,6) + &lcFile..STYLE)
  SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = ;
                  &lcFile..PikTkt + &lcFile..cWareCode + STR(&lcFile..LineNo,6) + &lcFile..Style
    IF SEEK(Style+ cWareCode+clocation ,'WHBINLOC') 
      *SELECT WHBINLOC
      *T20060817.0014   TMI [Start] replace this loop with one command to enhance speed
      *FOR lnCnt = 1 TO 8
      *  lcCnt = ALLT(STR(lnCnt,2))
      *  REPLACE WHBINLOC.ALO&lcCnt WITH MAX(WHBINLOC.ALO&lcCnt - PKBINLOC.Qty&lcCnt,0),;
      *          WHBINLOC.TotALO    WITH MAX(WHBINLOC.TotALO - PKBINLOC.Qty&lcCnt,0)
      *ENDFOR  
      SELECT WHBINLOC
      REPLACE ALO1   WITH MAX(ALO1 - PKBINLOC.Qty1 , 0 ) ;
              ALO2   WITH MAX(ALO2 - PKBINLOC.Qty2 , 0 ) ;
              ALO3   WITH MAX(ALO3 - PKBINLOC.Qty3 , 0 ) ;
              ALO4   WITH MAX(ALO4 - PKBINLOC.Qty4 , 0 ) ;
              ALO5   WITH MAX(ALO5 - PKBINLOC.Qty5 , 0 ) ;
              ALO6   WITH MAX(ALO6 - PKBINLOC.Qty6 , 0 ) ;
              ALO7   WITH MAX(ALO7 - PKBINLOC.Qty7 , 0 ) ;
              ALO8   WITH MAX(ALO8 - PKBINLOC.Qty8 , 0 ) ;
              TotALO WITH ALO1+ALO2+ALO3+ALO4+ALO5+ALO6+ALO7+ALO8
      *T20060817.0014   TMI [End  ] 
    ENDIF
    
    *T20060817.0014   TMI [Start] use this instead of the below loop to enhance speed
    SELECT PKBINLOC
    DELETE
    *T20060817.0014   TMI [End  ] 
    
  ENDSCAN
  
  *T20060817.0014   TMI [Start] comment the following lines and move them to the DELETE command above
  *SELECT PKBINLOC 
  *IF SEEK(&lcFile..PikTkt + &lcFile..cWareCode + STR(&lcFile..LINENO,6) + &lcFile..STYLE)
  *  DELETE REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = ;
  *                    &lcFile..PikTkt + &lcFile..cWareCode + STR(&lcFile..LINENO,6) + &lcFile..STYLE
  *ENDIF
  *T20060817.0014   TMI [End  ] 
ENDIF
SELECT WHBINLOC
FLUSH

*-- End of Function lfRelease.
*!***************************************************************************
*!* Name        : lfALMODAUT
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Modify Picked Qty in Automatic Allocation
*!***************************************************************************
*!* Called from : ALAUTAL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfALMODAUT()
*!***************************************************************************
FUNCTION lfALMODAUT
IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN .F.
ENDIF
lnAlias    = SELECT()
*-- lnQty -------> Qty per size
*-- lnSize -------> size
*-- lcPkTkt -------> PikTkt
*-- lcOrder -------> Order

lcSize = ALLT(STR(lnParm,2))

IF !lfOpnFiles("WHBINLOC,PKBINLOC","WHBINLOC,PKBINPKT","")
  RETURN
ENDIF
IF SEEK(&lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode ,'PKBINLOC') 
  SELECT PKBINLOC
  SCAN REST WHILE PikTkt+cWareCode = &lcTmpOrdLn..PikTkt+&lcTmpOrdLn..cWareCode ;
                  FOR STYLE = &lcTmpOrdLn..Style
                  
    IF SEEK(PKBINLOC.cWareCode+PKBINLOC.cLocation+PKBINLOC.Style ,'WHBINLOC')   
      REPLACE WHBINLOC.Alo&lcSize WITH WHBINLOC.Alo&lcSize +laPik[lnParm] - laOldVal,;
              WHBINLOC.TotAlo     WITH WHBINLOC.TotAlo +laPik[lnParm] - laOldVal
    ENDIF
    SELECT PKBINLOC
    REPLACE Qty&lcSize WITH laPik[lnParm],;
            TotQty     WITH TotQty +laPik[lnParm] - laOldVal
    IF TotQty = 0
      DELETE
    ENDIF 
  ENDSCAN  
ENDIF

SELECT(lnAlias)
*-- End of Function lfALMODAUT.
*!***************************************************************************
*!* Name        : lfUpdPkBin
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Update the PKBinLoc file with the Picked Qty.
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUpdPkBin()
*!***************************************************************************
FUNCTION lfUpdPkBin
PARAMETER lnQty,lcSize,lcPkTkt,lcOrder,lnLineNo,lcFltHng,lcClss
PRIVATE lcOldOrd
lnAlias    = SELECT()
IF lnQty = 0
  RETURN
ENDIF
IF EMPTY(lcFltHng)
  lcFltHng = STYLE.cFlatHang
ENDIF
IF EMPTY(lcClss)
  lcClss = IIF(!EMPTY(STYLE.cPrimClss&lcSize),STYLE.cPrimClss&lcSize,IIF(!EMPTY(STYLE.cSecClss&lcSize),STYLE.cSecClss&lcSize,;
               STYLE.cRemClss&lcSize))
ENDIF
lcOldOrd = ORDER('PKBINLOC')
SET ORDER TO TAG PKLINE IN PKBINLOC
IF !SEEK(lcPkTkt+lcWareCode+STR(lnLineNo,6) + lcStyle + WHBINLOC.cLocation ,'PKBINLOC')
  =SEEK(WHBINLOC.CWARECODE+WHBINLOC.CLOCATION+SPACE(19),'WHSLOC')
  SELECT PKBINLOC
  APPEND BLANK
  REPLACE STYLE              WITH WHBINLOC.Style            ,;
          CWARECODE          WITH WHBINLOC.cWareCode        ,;
          PIKTKT             WITH lcPkTkt                   ,;
          ORDER              WITH lcOrder                   ,;
          cFlatHang          WITH lcFltHng                  ,;
          clocation          WITH WHBINLOC.cLocation        ,;
          Qty&lcSize         WITH MAX(Qty&lcSize+lnQty,0)   ,;
          TotQty             WITH MAX(TotQty +lnQty,0)      ,;
          cBinClass&lcSize   WITH lcClss                    ,;
          LINENO             WITH lnLineNo
  =gfAdd_Info('PKBINLOC')
ELSE
  SELECT PKBINLOC
  REPLACE Qty&lcSize         WITH Qty&lcSize + lnQty        ,;
          TotQty             WITH TotQty + lnQty            ,;
          cBinClass&lcSize   WITH lcClss
ENDIF
SET ORDER TO TAG &lcOldOrd IN PKBINLOC
SELECT(lnAlias)
*--End of Function lfUpdPkBin.
*!***************************************************************************
*!* Name        : lfChkPktk
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Check if the Picked Qtys. that updated in the Pkbinloc and
*!*             : Whbinloc are equal to the OrdLine Picked Qtys.
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChkPktk()
*!***************************************************************************
FUNCTION lfChkPktk
PARAMETER lcTempFile,lcPktkt,lcSty,lnLine
IF EMPTY(lcPktkt)
  RETURN
ENDIF
PRIVATE lcOldOrd
DIMENSION laPkQty[9]
STORE 0 TO laPkQty

SELECT PKBINLOC
lcOldOrd = ORDER()
SET ORDER TO PKLINE
IF !SEEK(lcPktkt+&lcTempFile..cWareCode ,'PKBINLOC') 
  lcMsg2 = "No line updated to Bin Location Files Please Contact Aria Support"
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
ELSE
    FOR lnPkCount =1 To 8
      lcPkCount = ALLTRIM(STR(lnPkCount))
      SELECT PKBINLOC
      =SEEK(lcPktkt+&lcTempFile..cWareCode+STR(lnLine,6)+lcSty ,'PKBINLOC') 
      SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = lcPktkt+&lcTempFile..cWareCode +STR(lnLine,6)+lcSty
        laPkQty[lnPkCount] =laPkQty[lnPkCount]+ PKBINLOC.QTY&lcPkCount
      ENDSCAN  
    ENDFOR
    laPkQty[9]= laPkQty[1]+laPkQty[2]+laPkQty[3]+laPkQty[4]+laPkQty[5]+laPkQty[6]+laPkQty[7]+laPkQty[8]

    IF  &lcTempFile..TotPik <> laPkQty[9]
      lcMsg2 = "Piked Qty " + ALLTRIM(STR(&lcTempFile..TotPik)) +  " Not equal bin Piked Qty " + ALLTRIM(STR(laPkQty[9])) +;
               " for Style " + &lcTempFile..Style + " Piktkt " + lcPktkt + " Please contact aria support "
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    ENDIF  
ENDIF
SELECT PKBINLOC
SET ORDER TO &lcOldOrd
*-- End of Function lfChkPktk.
*!***************************************************************************
*!* Name        : lfChkFirst 
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Check if there are stock enough to allocate or not
*!***************************************************************************
*!* Called from : ALORDAL.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChkFirst()
*!***************************************************************************
FUNCTION lfChkFirst 
PARAMETER lcCurFile

IF !gfGetMemVar('M_DLUSEBIN')   && setting For Add bin location Yes/No
  RETURN 
ENDIF
PRIVATE lcTempRec,lnOldAlias,llCanSave,lcTmpSty,lcTmpWare,lcScanExp,lcOrdSty,lcOrdOrder,lcTempPik,lcLogFile,lnLogRecNo,lcTmpOrder
DIMENSION laPiked[8],laToPik[9]
STORE 0 TO lcTempRec,lnOldAlias,laPiked,laToPik,lnLogRecNo
STORE '' TO lcTmpSty,lcTmpWare,lcScanExp,lcOrdSty,lcOrdOrder,lcTempPik,lcLogFile,lcTmpOrder
STORE .T. TO llCanSave
lcLogFile  = gfTempName()
lnOldAlias = SELECT(0)
*-- Create Temp file that will hold any error occurs while importing.
CREATE TABLE (gcWorkDir+lcLogFile) (nRecNo N(5) , cError C(200),nFirst N(1))
INDEX ON ALLTRIM(STR(nfirst))+padl(nRecNo,fsize('nRecNo')) TAG LogRecno
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'--------------------------------------------------------------------',0)
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'Problems that prevent allocation',0)
INSERT INTO (lcLogFile) (NRECNO,cError,nFirst) VALUES (0,'--------------------------------------------------------------------',0)

*-- Checking For Use bin location Yes/No Or lcPikTkt = "******" and Open the Needed Files.
IF !lfOpnFiles("WHBINLOC,PKBINLOC,WHSLOC","WHBINLST,PKLINE,WHSLOC",'')
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,"Some of Bin Location's files can not open , so can not save" )
  RETURN .F.
ENDIF
SELECT (lcCurFile)
lcTempRec = RECNO()
IF lcProgName = 'ALORDAL'
  lcScanExp = ''
ENDIF
IF lcProgName = 'ALSTYAL'
  lcOrdSty  = &lcCurFile..Style 
  lcOrdOrder= &lcCurFile..Order
  lcScanExp = 'REST WHILE style+DTOS(complete)+cordtype+order+store+STR(lineno,6) =lcOrdSty FOR ORDER=lcOrdOrder'
ENDIF
IF lcProgName = 'ALAUTAL'
  lcScanExp = "FOR llSel .AND. TotPik > 0 .AND. nProcNo < 4"
ENDIF
SCAN &lcScanExp
  lcTmpSty  = &lcCurFile..Style 
  lcTmpWare = IIF(TYPE('lcPikWare') = 'C',lcPikWare,&lcCurFile..cWareCode)
  lcTempPik = &lcCurFile..PikTkt
  lcTmpOrder= &lcCurFile..Order
  IF SEEK(lcTmpSty+lcTmpWare,'STYDYE')
    SELECT WHBINLOC
    SELECT DISTINCT SUM(Whbinloc.Qty1),SUM(Whbinloc.Qty2),SUM(Whbinloc.Qty3),SUM(Whbinloc.Qty4),SUM(Whbinloc.Qty5),;
           SUM(Whbinloc.Qty6),SUM(Whbinloc.Qty7),SUM(Whbinloc.Qty8),count(STYLE);
           FROM Whbinloc;
           WHERE Whbinloc.style = lcTmpSty AND Whbinloc.cwarecode = lcTmpWare ;
           INTO ARRAY laToPik
    IF laToPik[9] = 0
      INSERT INTO (lcLogFile) (NRECNO,CERROR,nFirst) VALUES ;
            (lnLogRecNo+1,'Order : (' + lcTmpOrder +')'+' Style : (' +lcTmpSty +') has no stock in Bins' ,1)
    ELSE
      IF laToPik[1] <> STYDYE.STK1 OR ;
        laToPik[2] <> STYDYE.STK2 OR ;
        laToPik[3] <> STYDYE.STK3 OR ;
        laToPik[4] <> STYDYE.STK4 OR ;
        laToPik[5] <> STYDYE.STK5 OR ;
        laToPik[6] <> STYDYE.STK6 OR ;
        laToPik[7] <> STYDYE.STK7 OR ;
        laToPik[8] <> STYDYE.STK8
        INSERT INTO (lcLogFile) (NRECNO,CERROR,nFirst) VALUES ;
              (lnLogRecNo+1,'Order : (' + lcTmpOrder +')'+ ' Style : (' +lcTmpSty +')  has a mismatch between ' + ;
              'stock in warehouse and the accumulated stock in bins. Please run rebalance database for Inventory Control, then rebalance bin locations' ,1)
      ENDIF
    ENDIF
  ENDIF
ENDSCAN
IF RECCOUNT(lcLogFile)>3
  llCanSave = .F.
  IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'There are some problems so can not picked,Do you want to view the log report?') = 1
    lcWinTitl  = "Log Report"
	CREATE CURSOR TMPSTR (mStrRep M(120))
    SELECT TMPSTR 
    APPEND BLANK	
	SELECT (lcLogFile) 
	GO TOP
	lnLineSt = 1
	SCAN
      SELECT TMPSTR 
      IF &lcLogFile..NRECNO <> 0
        Replace mStrRep WITH mStrRep + Chr(10) + Chr(13)+ ALLTRIM(STR(&lcLogFile..nRecNo)) + ' - ' + &lcLogFile..cError
      ELSE
        Replace mStrRep WITH mStrRep + Chr(10) + Chr(13)+ &lcLogFile..cError
      ENDIF
      SELECT (lcLogFile)
    ENDSCAN
    SELECT TMPSTR
    DO (gcScrDir + 'SM\SMSTRREP.SPX')
  ENDIF
ENDIF
IF BETWEEN(lcTempRec,1,RECCOUNT(lcCurFile))
  GOTO lcTempRec IN (lcCurFile)
ENDIF
SELECT(lnOldAlias)
RETURN llCanSave
*-- End of Function lfChkFirst.

*!***************************************************************************
*!* Name        : LFCHKPACK
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Prevent user from change piktkt Qty. if it has a packing List.
*!***************************************************************************
*!* Called from : ALORDAL.PRG - 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = LFCHKPACK()
*!***************************************************************************
FUNCTION LFCHKPACK
PRIVATE lnOldAlias , llusedPack , lcOldOrder

lnOldAlias = SELECT(0)
llusedPack = USED('PACK_HDR')
lcOldOrder = IIF(llusedPack,ORDER('PACK_HDR'),"")
IF !lfOpnFiles("PACK_HDR","PACK_HDR",'')
  RETURN
ELSE
  DO CASE
    CASE lcProgName = 'ALORDAL'
      IF SEEK(ORDLINE.PikTkt,'PACK_HDR')
        =gfModalGen("INM44060B00000" , "DIALOG" , PADR(ORDLINE.PikTkt,6))
        &lcObjName = lcOldVal
        RETURN .F.
      ENDIF
    CASE lcProgName = 'ALAUTAL'
      IF SEEK(EVAL(lcTmpOrdLn+'.PikTkt'),'PACK_HDR')
        =gfModalGen("INM44060B00000" , "DIALOG" , PADR(EVAL(lcTmpOrdLn+'.PikTkt'),6))
        laPik[lnParm] = laOldVal
        RETURN .F.
      ENDIF
  ENDCASE
ENDIF
IF !llusedPack
  USE IN PACK_HDR
ELSE
  IF !EMPTY(lcOldOrder)
   SET ORDER TO TAG &lcOldOrder IN PACK_HDR
  ENDIF
ENDIF
*-- End of Function LFCHKPACK.
*!***************************************************************************
*!* Name        : lfvPrnt
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Allocation (AL)
*!* Purpose     : Print the Log Report if there are problems in allocation
*!***************************************************************************
*!* Called from : SMSTRREP.SPR in lfChkFirst
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvPrnt()
*!***************************************************************************
FUNCTION lfvPrnt
IF pSetup(.T.)
  gcOutFile = gcWorkDir+gfTempName()+'.TXT'
  COPY MEMO TMPSTR.mStrRep TO &gcOutFile
  gcDevice = 'PRINTER'
  DO ENDREPORT
  gcDevice = 'SCREEN'
ENDIF
*-- End of Function lfvPrnt.
*!******************** End OF The Allocation Section ************************
*!******************** Start OF The Accounts Receivable Section *************
*!***************************************************************************
*!* Name        : lfARCHKORD
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Check if there is a piktkt for the order or not
*!***************************************************************************
*!* Called from : ARIINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARCHKORD()
*!***************************************************************************
FUNCTION lfARCHKORD
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
IF !EMPTY(laData[1]) AND !SEEK(laData[1],'PIKTKT')
  lcMsg = 'There is no open pick ticket for order ' +laData[1] + ' , cannot ship . you have to allocate it first'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
  laData[1] = '' 
  _CUROBJ = OBJNUM(laData[1])
  RETURN .F.
ENDIF
RETURN .T.
*-- End of Function lfARCHKORD.
*!***************************************************************************
*!* Name        : lfARCHKSCP
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Check if there is a piktkt for the order or not when user
*!*             : select a Range of orders from the Scope Screen
*!***************************************************************************
*!* Called from : ARIINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARCHKSCP()
*!***************************************************************************
FUNCTION lfARCHKSCP
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
IF !SEEK(ORDHDR.ORDER,'PIKTKT')  
  lcMsg = 'There is no open pick ticket for order ' + ORDHDR.ORDER + ' , cannot ship . you have to allocate it first'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
  RETURN .F.
ENDIF
RETURN .T.
*-- End of Function lfARCHKSCP.
*!***************************************************************************
*!* Name        : lfARDFNMNU
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Add a new entry to the Option Menu - "Bin Locations"
*!***************************************************************************
*!* Called from : ARDINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARDFNMNU()
*!***************************************************************************
FUNCTION lfARDFNMNU
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN .F.
ENDIF
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _INQURYPOP
  DEFINE POPUP _INQURYPOP MARGIN SHADOW
ENDIF
*-- Count Options BARS to add the new BAR at the end of the Popup
lnBarNo = CNTBAR('_INQURYPOP') + 1
*-- Define New options (Bin Locations),This Option Menu will be enable in the Edit mode.
DEFINE BAR lnBarNo OF _INQURYPOP PROMPT "\<Bin Locations" SKIP FOR (laScrMode[1]) OR (laScrMode[2])
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfARGETBIN IN BINMAIN
RETURN
*-- End of Function lfARDFNMNU.
*!***************************************************************************
*!* Name        : lfARGETBIN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Screen of get bins in case of AR.
*!***************************************************************************
*!* Called from : lfARDFNMNU in Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARGETBIN()
*!***************************************************************************
FUNCTION lfARGETBIN
PRIVATE lcOldBinlc
STORE "" TO lcOldBinlc
lnAlias = SELECT()
IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLST,WHSLOC","")
  RETURN
ENDIF
lcBinLoc = ''
SELECT(lcInvLine)
SCATTER MEMVAR MEMO
STORE '' TO m.BinQty1,m.BinQty2,m.BinQty3,m.BinQty4,m.BinQty5,m.BinQty6,m.BinQty7,m.BinQty8
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\ARBNLOC.SPX")
POP KEY
SELECT(lnAlias)
*-- End of Function lfARGETBIN.
*!***************************************************************************
*!* Name        : lfWArBnLoc
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : When function for ARBNLOC.SPX
*!***************************************************************************
*!* Called from : ARBNLOC.SPX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfWArBnLoc()
*!***************************************************************************
FUNCTION lfWArBnLoc
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN .F.
ENDIF
FOR lnCount = 1 TO 8 
  lcCount = ALLTRIM(STR(lnCount))
  IF !EMPTY(Scale.Sz&lcCount) AND &lcInvLine..Qty&lcCount<>0
    SHOW GET m.BinLoc&lcCount ENABLE
  ELSE
    SHOW GET m.BinLoc&lcCount DISABLE
  ENDIF
ENDFOR  
SHOW GETS
*-- End of Function lfWArBnLoc.
*!***************************************************************************
*!* Name        : lfOldBinlc
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : When function for Bins Fields in ARBNLOC.SPX
*!***************************************************************************
*!* Called from : ARBNLOC.SPX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfOldBinlc()
*!***************************************************************************
FUNCTION lfOldBinlc
lcOldBinlc = EVALUATE('m.' + SYS(18))
*-- End of Function lfOldBinlc.
*!***************************************************************************
*!* Name        : lfvArbnloc
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Valid Function for Bins Fields in ARBNLOC.SPX
*!***************************************************************************
*!* Called from : ARBNLOC.SPX
*!***************************************************************************
*!* Parameters  : Size No.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvArbnloc()
*!***************************************************************************
FUNCTION lfvArbnloc
PARAMETERS lnCnxt
SELECT (lcInvLine)
lnRecNo = RECNO()
lcCnxt = STR(lnCnxt,1)
lcWareCode = m.cWareCode
IF llbrowse OR (!EMPTY(m.BinLoc&lcCnxt) AND !SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')) OR '?' $ m.BinLoc&lcCnxt
  llbrowse =.F. 
  SELECT WHBINLOC
  lcOldBnOrd = ORDER()
  SET ORDER TO TAG WHBINLST
  SET RELATION OFF INTO WHSLOC
  SET RELATION TO WHBINLOC.cWarecode+WHBINLOC.cLocation+SPACE(19) INTO WHSLOC ADDITIVE
  lcBrFields = "clocation :R :H='Bin',cBinClass :R :H='Bin Class',WHSLOC.cFlatHang :R :H='Flat or Hang',QTY1 :R :H='Stk1'"
  lcBrFields = lcBrFields + ",QTY2 :R :H='Stk2',QTY3 :R :H='Stk3',QTY4 :R :H='Stk4',QTY5 :R :H='Stk5',QTY6 :R :H='Stk6'"
  lcBrFields = lcBrFields + ",QTY7 :R :H='Stk7',QTY8 :R :H='Stk8',TOTQTY :R :H='Total'"
  lcForExpr = " cWareCode = lcWareCode AND Style = &lcInvLine..Style"
  lcBrTitle = "Available Bins Location"
  lcSelBin  = IIF(ARIABROW([FOR &lcForExpr],lcBrTitle,5,15,25,55,'','','cLocation','laBrowArr'),laBrowArr[1],'')
  m.BinLoc&lcCnxt = IIF(!EMPTY(lcSelBin),ALLTRIM(lcSelBin),lcOldBinlc)
  SHOW GET m.BinLoc&lcCnxt
  SET ORDER TO TAG &lcOldBnOrd
ENDIF
IF !EMPTY(m.BinLoc&lcCnxt) 
  lnAlias = SELECT()
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc
  IF !SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
    SET ORDER TO TAG WhsLocSt IN WHSLOC 
    =gfModalGen('INM00000B00000','','','',ALLTRIM(m.BinLoc&lcCnxt) + ' is not a valid Bin location')
    _CUROBJ = OBJNUM(m.BinLoc&lcCnxt)
    m.BinLoc&lcCnxt = ''
    RETURN
  ELSE
    =SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
    IF !(WhsLoc.cFlatHang = style.cflathang)
      *--You cannot transfer between different Hang/Flat Bins.
      lcMsg2 = 'You cannot issue the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      _CUROBJ = OBJNUM(m.BinLoc&lcCnxt)
      m.BinLoc&lcCnxt= ''
      RETURN
    ENDIF
    llClass = .F.
    IF INLIST(WhsLoc.CBINCLASS,STYLE.CPRIMCLSS&lcCnxt,STYLE.CSECCLSS&lcCnxt,STYLE.CREMCLSS&lcCnxt)
      llClass = .T.
    ENDIF
    IF !llClass
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot Issue the style to a bin of a different class.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      _CUROBJ = OBJNUM(m.BinLoc&lcCnxt)
      m.BinLoc&lcCnxt= ''
      RETURN
    ENDIF
    IF !SEEK(lcWareCode+LEFT(m.BinLoc&lcCnxt,10)+&lcInvLine..style,'WHSLOC')
      =SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
      lcLocSty = m.style
      SCATT MEMVAR MEMO
      m.cWareCode = lcWareCode
      m.style = lcLocSty
      m.clocation = m.BinLoc&lcCnxt
      INSERT INTO WHSLOC FROM MEMVAR 
    ENDIF
  ENDIF
  SELECT(lnAlias)
  IF !SEEK(&lcInvLine..Style+ &lcInvLine..cWareCode+m.BinLoc&lcCnxt ,'WHBINLOC') 
    lcMsg2 = 'Stock not available'
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    m.BinLoc&lcCnxt=''
    RETURN
  ENDIF
  IF &lcInvLine..QTY&lcCnxt>(WhBinLoc.Qty&lcCnxt - WhBinLoc.Alo&lcCnxt)
    lcMsg2 = 'Stock not available'
     =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
    m.BinLoc&lcCnxt=''
    RETURN
  ENDIF
  SELECT (lcInvLine)
  m.BinQTY&lcCnxt = (WhBinLoc.Qty&lcCnxt - WhBinLoc.Alo&lcCnxt )
  SHOW GETS
  SELECT (lcInvLine)
  GOTO lnRecNo
ENDIF
*-- End of Function lfvArbnloc.
*!***************************************************************************
*!* Name        : lfDLINTVAR
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Add new fields to the Temp. file (lcInvLine)
*!***************************************************************************
*!* Called from : Ardinv.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLINTVAR()
*!***************************************************************************
FUNCTION lfDLINTVAR
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+8,4]
FOR lnFld = 1 TO 8
  lcFld = ALLTRIM(STR(lnFld))
  laFileStru[lnFileStru+ lnFld , 1] = 'BinLoc' + lcFld
  laFileStru[lnFileStru+ lnFld , 2] = 'C'
  laFileStru[lnFileStru+ lnFld , 3] = 10
  laFileStru[lnFileStru+ lnFld , 4] = 0
ENDFOR
*-- End of Function lfDLINTVAR.
*!***************************************************************************
*!* Name        : lfvArOk
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Valid Function for OK Button in ARBNLOC.SPX
*!***************************************************************************
*!* Called from : ARBNLOC.SPX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvArOk()
*!***************************************************************************
FUNCTION lfvArOk
SELECT (lcInvLine)
REPLACE  BinLoc1  WITH m.BinLoc1,;
         BinLoc2  WITH m.BinLoc2,;
         BinLoc3  WITH m.BinLoc3,;
         BinLoc4  WITH m.BinLoc4,;
         BinLoc5  WITH m.BinLoc5,;
         BinLoc6  WITH m.BinLoc6,;
         BinLoc7  WITH m.BinLoc7,;
         BinLoc8  WITH m.BinLoc8
CLEAR READ
*-- End of Function lfvArOk.
*!***************************************************************************
*!* Name        : lfDLSAVINV
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Save a Direct invoice to Bin Location files
*!***************************************************************************
*!* Called from : ARDINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSAVINV()
*!***************************************************************************
FUNCTION lfDLSAVINV 
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
IF !lfOpnFiles("IVBINLOC","IVBINLIN","")
  RETURN
ENDIF
SELECT (lcInvHdr)
LOCATE 
SELECT (lcInvLine)
locline = 0
SCAN
  LocLine = LocLine + 1
  REPLACE LineNo WITH LocLine
ENDSCAN
LOCATE 
IF EOF()
  RETURN
ENDIF
SCAN
  *--Don't Update the Bin Location stock files if Not Style.linvsty 
  *T20060817.0014   TMI [Start] include all styles in invoice saving
  *IF !STYLE.LINVSTY
  *  LOOP
  *ENDIF
  *T20060817.0014   TMI [End  ] 
  FOR lnCount = 1  TO 8
    lcCount = ALLTRIM(STR(lnCount))
    IF !EMPTY(&lcInvLine..BinLoc&lcCount)
      
      *T20060817.0014   TMI [Start] comment these lines and move them below
      *-* IF SEEK(style+&lcInvLine..cWareCode+&lcInvLine..BinLoc&lcCount,'WHBINLOC')
      *-*   SELECT WHBINLOC
      *-*   REPLACE  Qty&lcCount    WITH MAX(Qty&lcCount - &lcInvLine..Qty&lcCount,0),;
      *-*            TotQty         WITH MAX(TotQty - &lcInvLine..Qty&lcCount,0)
      *T20060817.0014   TMI [End  ] 
      
        IF !SEEK(&lcInvHdr..Invoice+&lcInvLine..Style+STR(&lcInvLine..lineno,6)+&lcInvLine..cWareCode+&lcInvLine..BinLoc&lcCount ,'IVBINLOC')
          SELECT IVBINLOC
          APPEND BLANK
          REPLACE STYLE        WITH WHBINLOC.Style               ,;
                  CWARECODE    WITH WHBINLOC.cWareCode           ,;
                  ORDER        WITH &lcInvHdr..Order             ,;
                  Invoice      WITH &lcInvHdr..Invoice           ,;
                  clocation    WITH &lcInvLine..BinLoc&lcCount   ,;
                  Qty&lcCount  WITH &lcInvLine..Qty&lcCount      ,;
                  TotQty       WITH &lcInvLine..Qty&lcCount      ,;
                  LineNo  WITH &lcInvLine..LineNo      
           =gfAdd_Info('IVBINLOC')
        ELSE
          SELECT IVBINLOC
          REPLACE  Qty&lcCount WITH Qty&lcCount + &lcInvLine..Qty&lcCount ,;
                   TotQty      WITH TotQty + &lcInvLine..Qty&lcCount
        ENDIF

      *T20060817.0014   TMI [Start] if a styles has a line in whbinloc, update it
      IF SEEK(style+&lcInvLine..cWareCode+&lcInvLine..BinLoc&lcCount,'WHBINLOC')
        SELECT WHBINLOC
        REPLACE  Qty&lcCount    WITH MAX(Qty&lcCount - &lcInvLine..Qty&lcCount,0),;
                 TotQty         WITH MAX(TotQty - &lcInvLine..Qty&lcCount,0)
        *T20060817.0014   TMI [End  ] 
        SELECT WHBINLOC
        IF TotQty = 0
          DELETE           
        ENDIF         
      ENDIF
    ENDIF  
  ENDFOR  
  m.Invoice = &lcInvHdr..Invoice
  =lfDLARBIN()
ENDSCAN
*-- End of Function lfDLSAVINV.
*!***************************************************************************
*!* Name        : lfDLARBIN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Update BinInvJL & WHBINLOC file for sales Order Invoice
*!***************************************************************************
*!* Called from : ARINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLARBIN()
*!***************************************************************************
FUNCTION lfDLARBIN
PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
lnCurAlias = SELECT()
*- Check if the calling program is ariinv and no data in the field &lcDetFile..Order
IF  lcProgName = 'ARIINV'
  *--handle consolidated invoice case
  IF  &lcHdrFile..CONSOL = 'Y'
    RETURN
  ENDIF
  lcinvline = lcDetFile
  lcInvHdr = lcHdrFile
ENDIF

*--Open the needed files with indexes
IF !lfOpnFiles("IVBINLOC,STYINVJL,BININVJL","IVBINLIN,STYINVJL,STYINVJL","")
  RETURN
ENDIF

lcTmpIvbin = gfTempName()
lcTmpBnInv = gfTempName()

*--create a temp file for invoice bin location
SELECT IVBINLOC
DIMENSION laFileStru[1,4]
= AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpIvbin) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+STR(LineNo,6)+cLocation TAG (lcTmpIvbin)

*--create a temp file for bin invintory file
SELECT BININVJL
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpBnInv) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpBnInv)

SELECT (lcInvHdr)
lnHdrRec = RECNO()
SELECT IVBINLOC
SEEK(&lcInvHdr..Invoice)
SCAN REST WHILE Invoice+Style+STR(LineNo,6)+cWareCode+cLocation = &lcInvHdr..Invoice FOR TotQty <> 0
  SCATT MEMVAR MEMO
  INSERT INTO (lcTmpIvbin) FROM MEMVAR
ENDSCAN
SELECT (lcInvLine)
lnLineRec = RECNO()
=SEEK(Style+cWarecode+lcGlSession,'STYINVJL')
SELECT STYINVJL
SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrdate)+cTrcode+STR(LineNo,6)= ;
     &lcInvLine..Style+&lcInvLine..cWareCode+lcGlSession FOR (cTrcode==&lcInvHdr..Invoice) AND (LineNo==&lcInvLine..LineNo)
  SCATTER MEMVAR MEMO
  =SEEK(Style+cWareCode+STR(LineNo,6),lcTmpIvbin)
  SELECT(lcTmpIvbin)
  SCAN REST WHILE Style+cWareCode+STR(lineno,6)+clocation = STYINVJL.Style+STYINVJL.cWareCode+STR(STYINVJL.LineNo,6)
    llChkQty = .F.
    FOR lnI  = 1 TO 8
      lcI = ALLT(STR(lnI,2))
      IF (m.nStk&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.nStk&lcI))
        llChkQty = .T.
      ELSE
        llChkQty = .F.
        EXIT
      ENDIF
    ENDFOR
    IF llChkQty 
      SELECT (lcTmpBnInv)
      m.cLocation = &lcTmpIvbin..cLocation
      APPEND BLANK
      GATHER MEMVAR MEMO
      SELECT(lcTmpIvbin)
      FOR lnI = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        REPLACE Qty&lcI WITH Qty&lcI - ABS(m.nStk&lcI)
      ENDFOR  
      REPLACE TotQty WITH TotQty - ABS(m.nTotStk)
      IF TotQty = 0
        DELETE
      ENDIF
      EXIT
    ENDIF
    IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
      SELECT (lcTmpBnInv)
      m.cLocation = &lcTmpIvbin..cLocation
      APPEND BLANK
      GATHER MEMVAR MEMO
      FOR lnI = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        IF ABS(m.nStk&lcI) > &lcTmpIvbin..Qty&lcI
          REPLACE nStk&lcI WITH -1* &lcTmpIvbin..Qty&lcI
          m.nStk&lcI = m.nStk&lcI + &lcTmpIvbin..Qty&lcI
          m.nTotStk  = m.nTotStk  + &lcTmpIvbin..Qty&lcI
          REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - &lcTmpIvbin..Qty&lcI
          REPLACE &lcTmpIvbin..Qty&lcI WITH 0
        ELSE
          REPLACE nStk&lcI WITH m.nStk&lcI
          REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + m.nStk&lcI
          REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI + m.nStk&lcI
          m.nTotStk  = m.nTotStk  - m.nStk&lcI
          m.nStk&lcI = 0
        ENDIF 
      ENDFOR  
      REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8 ,;
              nstkval WITH nTotStk * nCost
      IF nTotStk = 0
        DELETE
      ENDIF
    ENDIF
  ENDSCAN
ENDSCAN                
IF BETWEEN(lnLineRec ,1,RECCOUNT(lcInvLine))
  GOTO lnLineRec IN (lcInvLine)
  m.LineNo = &lcInvLine..LineNo
ENDIF
IF BETWEEN(lnHdrRec ,1,RECCOUNT(lcInvHdr))
  GOTO lnHdrRec IN (lcInvHdr)
ENDIF

*--Update Bininvjl File
SELECT (lcTmpBnInv)
SCAN FOR nTotStk <> 0
  SCATTER MEMVAR MEMO
  *T20060817.0014   TMI [Start] commenting out the following lines,I do not understand the need for refrence checking , moreover it results in updating bininvjl 
  *                             wrongly.
  *-* IF SEEK(Style+cWareCode+cSession+DTOS(dTrdate)+cTrcode+STR(LineNo,6),'BININVJL')
  *-*   llFndRec = .F.
  *-*   SELECT BININVJL
  *-*   SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrdate)+cTrcode+STR(LineNo,6) = ;
  *-*     &lcTmpBnInv..Style+&lcTmpBnInv..cWareCode+&lcTmpBnInv..cSession+DTOS(&lcTmpBnInv..dTrdate)+&lcTmpBnInv..cTrcode+;
  *-*     STR(&lcTmpBnInv..LineNo,6) FOR CLOCATION = &lcTmpBnInv..CLOCATION
  *-*     IF BININVJL.REFERENCE == &lcTmpBnInv..REFERENCE
  *-*       llFndRec = .T.
  *-*       EXIT
  *-*     ENDIF
  *-*     IF !llFndRec
  *-*       INSERT INTO BININVJL FROM MEMVAR
  *-*     ENDIF
  *-*   ENDSCAN
  *-* ELSE
  *T20060817.0014   TMI [End  ] 
  
  INSERT INTO BININVJL FROM MEMVAR

  *T20060817.0014   TMI [Start] 
  *-* ENDIF
  *T20060817.0014   TMI [End  ] 
ENDSCAN
=lfEraseFil(lcTmpBnInv)
=lfEraseFil(lcTmpIvbin)

SELECT (lnCurAlias)
*-- End of Function lfDLARBIN.

*!***************************************************************************
*!* Name        : lfARCKSVBN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Check if bin Locations assigned to the invoice styles or not
*!***************************************************************************
*!* Called from : ARDINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARCKSVBN()
*!***************************************************************************
FUNCTION lfARCKSVBN
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN .F.
ENDIF
STORE .F. TO  llmessag
SELECT (lcInvLine)
LOCATE
SCAN 
  IF llmessag
    EXIT
  ENDIF
  m.loc1 = &lcInvLine..Binloc1
  m.loc2 = &lcInvLine..Binloc2
  m.loc3 = &lcInvLine..Binloc3
  m.loc4 = &lcInvLine..Binloc4
  m.loc5 = &lcInvLine..Binloc5
  m.loc6 = &lcInvLine..Binloc6
  m.loc7 = &lcInvLine..Binloc7
  m.loc8 = &lcInvLine..Binloc8
  FOR lnI = 1 TO 8
    lcI = ALLT(STR(lnI,2))
    *--Don't assign Bin Location if Not Style.linvsty
    *T20060817.0014   TMI [Start] include all styles in check
    *IF !EMPTY(&lcInvLine..Qty&lcI) AND STYLE.LINVSTY
    IF !EMPTY(&lcInvLine..Qty&lcI) 
      *T20060817.0014   TMI [End  ] 
      IF EMPTY(m.loc&lcI)
        lcMsg2 = 'You cannot save style '+&lcInvLine..Style + ' without assign bin location.'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        llmessag = .T.
        STORE .F. TO llShow,llCSave
        RETURN llCSave
      ENDIF
    ENDIF  
  ENDFOR
ENDSCAN
RETURN llCSave
*-- End of Function lfARCKSVBN.
*!***************************************************************************
*!* Name        : lfARCHKSIZ
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Check Invoice Line Per size for the avaliable Stk in bins
*!***************************************************************************
*!* Called from : Ardinv.prg --> lfvSizeQty
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfARCHKSIZ()
*!***************************************************************************
FUNCTION lfARCHKSIZ
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
IF m.Qty&lcQtyNo <> lcOldValue AND !EMPTY(ALLTRIM(&lcInvLine..Binloc&lcQtyNo))
  IF SEEK(&lcInvLine..Style+ &lcInvLine..cWareCode+ALLTRIM(&lcInvLine..Binloc&lcQtyNo) ,'WHBINLOC') 
    IF m.Qty&lcQtyNo > (WhBinLoc.Qty&lcQtyNo - WhBinLoc.Alo&lcQtyNo)
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Stock not available')
      m.Qty&lcQtyNo = lcOldValue
      _CUROBJ = _CUROBJ
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
RETURN .T.
*-- End of Function lfARCHKSIZ.

*!***************************************************************************
*!* Name        : lfDLVODINV
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Save Data to Void Invoice screen
*!***************************************************************************
*!* Called from : Ardinv.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLVODINV()
*!***************************************************************************
FUNCTION lfDLVODINV 
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF

*--Open the needed files with indexes
IF !lfOpnFiles("WHBINLOC,IVBINLOC,PKBINLOC,STYINVJL,BININVJL","WHBINLST,IVBINLIN,PKBINLIN,STYINVJL,STYINVJL","")
  RETURN
ENDIF
*-- In case of Voiding Consolidated invoices
IF INVHDR.CONSOL = 'Y'
  = lfConsVoid()
  = lfConsUpd()
  RETURN
ENDIF

=lfDLARVOID()

IF SEEK(InvLine.Invoice+InvLine.Style+STR(InvLine.LineNo,6)+InvHdr.cWareCode ,'IVBINLOC') 
  SELECT IVBINLOC
  SCAN REST WHILE Invoice+Style+STR(LineNo,6)+cWareCode+cLocation= ;
       InvLine.Invoice+InvLine.Style+STR(InvLine.LineNo,6)+InvHdr.cWareCode
    *T20060817.0014   TMI [Start] locate the style in style file
    =SEEK(IvBinLoc.Style,'STYLE')
    *T20060817.0014   TMI [End  ] 
    IF SEEK(IvBinLoc.Style+ IvBinLoc.cWareCode+IvBinLoc.cLocation ,'WHBINLOC') 
      SELECT WHBINLOC
      REPLACE Qty1   WITH Qty1+ IvBinLoc.Qty1    ,Qty2 WITH Qty2+ IvBinLoc.Qty2    ,;
              Qty3   WITH Qty3+ IvBinLoc.Qty3    ,Qty4 WITH Qty4+ IvBinLoc.Qty4    ,;
              Qty5   WITH Qty5+ IvBinLoc.Qty5    ,Qty6 WITH Qty6+ IvBinLoc.Qty6    ,;
              Qty7   WITH Qty7+ IvBinLoc.Qty7    ,Qty8 WITH Qty8+ IvBinLoc.Qty8    ,;
              TotQty WITH TotQty+ IvBinLoc.TotQty
    ELSE
      *T20060817.0014   TMI [Start] update whbinloc only for inv. styles
      IF STYLE.LINVSTY
        *T20060817.0014   TMI [End  ] 
        
        SELECT WHBINLOC
        APPEND BLANK
        REPLACE STYLE      WITH IvBinLoc.Style         ,CWARECODE  WITH IvBinLoc.cWareCode     ,;
                clocation  WITH IvBinLoc.cLocation     ,Qty1       WITH Qty1+ IvBinLoc.Qty1    ,;
                Qty2       WITH Qty2+ IvBinLoc.Qty2    ,Qty3       WITH Qty3+ IvBinLoc.Qty3    ,;
                Qty4       WITH Qty4+ IvBinLoc.Qty4    ,Qty5       WITH Qty5+ IvBinLoc.Qty5    ,;
                Qty6       WITH Qty6+ IvBinLoc.Qty6    ,Qty7       WITH Qty7+ IvBinLoc.Qty7    ,;
                Qty8       WITH Qty8+ IvBinLoc.Qty8    ,TotQty     WITH TotQty+ IvBinLoc.TotQty
              
        =gfAdd_Info('WHBINLOC')
      
        *T20060817.0014   TMI [Start] close the above if statement
      ENDIF
      *T20060817.0014   TMI [End  ] 
      
    ENDIF
    IF SEEK(InvHdr.PIKTKT+IvBinLoc.cWareCode+IvBinLoc.cLocation+IvBinLoc.Style+STR(IvBinLoc.LineNo,6),'PKBINLOC') 
      *T20060817.0014   TMI [Start] update whbinloc only for inv. styles
      IF STYLE.LINVSTY
        *T20060817.0014   TMI [End  ] 
   
        SELECT WHBINLOC
        REPLACE Alo1    WITH Alo1+ IvBinLoc.Qty1    ,Alo2 WITH Alo2+ IvBinLoc.Qty2    ,;
                Alo3    WITH Alo3+ IvBinLoc.Qty3    ,Alo4 WITH Alo4+ IvBinLoc.Qty4    ,;
                Alo5    WITH Alo5+ IvBinLoc.Qty5    ,Alo6 WITH Alo6+ IvBinLoc.Qty6    ,;
                Alo7    WITH Alo7+ IvBinLoc.Qty7    ,Alo8 WITH Alo8+ IvBinLoc.Qty8    ,;
                TotAlo  WITH TotAlo+ IvBinLoc.TotQty

        SELECT PKBINLOC
        REPLACE lInvoiced   WITH .F.
        
        *T20060817.0014   TMI [Start] close the above if condition
      ENDIF
      *T20060817.0014   TMI [End  ] 
      
    ENDIF
    SELECT IVBINLOC
    DELETE
  ENDSCAN
ENDIF
*-- End of Function lfDLVODINV.

*!***************************************************************************
*!* Name        : lfConsVoid
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Update BinInvJL & WHBINLOC file in case of void Invoice in 
*!*             : case of consolidated invoice
*!***************************************************************************
*!* Called from : lfDLVODINV IN BINMAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfConsVoid()
*!***************************************************************************
FUNCTION lfConsVoid
PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv,lnInvLinNo
lnCurAlias = SELECT()
lnInvLinNo = INVLINE.LINENO
lcTmpIvbin = gfTempName()
lcTmpBnInv = gfTempName()
*--create tmpfile for invoice bin location
SELECT IVBINLOC
DIMENSION laFileStru[1,4]
= AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpIvbin) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+STR(LineNo,6)+cLocation TAG (lcTmpIvbin)

*--create tmpfile for bin invintory file
SELECT BININVJL
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpBnInv) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpBnInv)

SELECT CONSINVL
=SEEK(INVHDR.INVOICE)
SCAN REST WHILE Invoice+Store+Order+Style+STR(LineNo,6) = INVHDR.INVOICE FOR STYLE = INVLINE.Style
  SELECT IVBINLOC
  =SEEK(InvHdr.Invoice+INVLINE.STYLE+STR(CONSINVL.LineNo,6))
  SCAN REST WHILE Invoice+Style+STR(LineNo,6)+cWareCode+cLocation = InvHdr.Invoice+INVLINE.STYLE+STR(CONSINVL.LineNo,6)
    SCATTER MEMVAR MEMO
    INSERT INTO (lcTmpIvbin) FROM MEMVAR
  ENDSCAN
ENDSCAN
SELECT InvLine
lnCurRec = RECNO()
=SEEK(INVHDR.INVOICE+STR(lnInvLinNo,6))
SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.INVOICE+STR(lnInvLinNo,6)
  =SEEK(Style+cWareCode,'STYINVJL')
  SELECT STYINVJL
  SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrdate)+cTrcode+STR(LineNo,6)= ;
       InvLine.style+InvLine.cwarecode FOR cTrtype ='4' AND cTrcode = INVHDR.INVOICE AND STR(LINENO,6)==STR(lnInvLinNo,6)
    SCATTER MEMVAR MEMO
    =SEEK(Style+cWareCode+STR(LineNo,6),lcTmpIvbin)
    SELECT CONSINVL
    =SEEK(INVHDR.INVOICE)
    SCAN REST WHILE Invoice+Store+Order+Style+STR(LineNo,6) = INVHDR.INVOICE FOR STYLE = STYINVJL.Style
      m.Lineno= CONSINVL.LineNo
      m.nSTK1 = CONSINVL.QTY1
      m.nSTK2 = CONSINVL.QTY2
      m.nSTK3 = CONSINVL.QTY3
      m.nSTK4 = CONSINVL.QTY4
      m.nSTK5 = CONSINVL.QTY5
      m.nSTK6 = CONSINVL.QTY6
      m.nSTK7 = CONSINVL.QTY7
      m.nSTK8 = CONSINVL.QTY8
      m.nTotStk = CONSINVL.TOTQTY
      SELECT(lcTmpIvbin)
      =SEEK(STYINVJL.Style+STYINVJL.cWareCode+STR(CONSINVL.LINENO,6))
      SCAN REST WHILE Style+cWareCode+STR(LineNo,6)+cLocation = STYINVJL.Style+STYINVJL.cWareCode+STR(CONSINVL.LINENO,6)
        llChkQty = .F.
        FOR lnI  = 1 TO 8
            lcI = ALLT(STR(lnI,2))
            IF (m.nStk&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.nStk&lcI))
              llChkQty = .T.
            ELSE
              llChkQty = .F.
              EXIT
            ENDIF
          ENDFOR
          IF llChkQty 
            SELECT (lcTmpBnInv)
            m.clocation = &lcTmpIvbin..clocation
            APPEND BLANK
            GATHER MEMVAR MEMO
            SELECT(lcTmpIvbin)
            FOR LNI = 1 TO 8
              LCI = ALLTRIM(STR(LNI))
              REPLACE Qty&LCI WITH Qty&LCI + ABS(m.nStk&LCI)
            ENDFOR  
            REPLACE TotQty WITH TotQty + ABS(m.nTotStk)
            EXIT
          ENDIF
          IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
            SELECT (lcTmpBnInv)
            m.clocation = &lcTmpIvbin..clocation
            APPEND BLANK
            GATHER MEMVAR MEMO
            FOR lnI = 1 TO 8
              lcI = ALLT(STR(lnI,2))
              IF ABS(m.nStk&lcI) > &lcTmpIvbin..Qty&lcI
                REPLACE nStk&lcI WITH  &lcTmpIvbin..Qty&lcI
                m.nStk&lcI = m.nStk&lcI - &lcTmpIvbin..Qty&lcI
                m.nTotStk  = m.nTotStk  - &lcTmpIvbin..Qty&lcI
                REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + &lcTmpIvbin..Qty&lcI
                REPLACE &lcTmpIvbin..Qty&lcI WITH 0
              ELSE
                REPLACE nStk&lcI WITH m.nStk&lcI
                REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - m.nStk&lcI
                REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI - m.nStk&lcI
                m.nTotStk  = m.nTotStk  + m.nStk&lcI
                m.nStk&lcI = 0
              ENDIF 
            ENDFOR  
            REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
            REPLACE nstkval WITH nTotStk * nCost
          ENDIF
        ENDSCAN
      ENDSCAN
  ENDSCAN
ENDSCAN
SELECT InvLine
IF BETWEEN(lnCurRec ,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF
SELECT (lcTmpBnInv)
SCAN
  SCATTER MEMVAR MEMO
  INSERT INTO BININVJL FROM MEMVAR
ENDSCAN
=lfEraseFil(lcTmpBnInv)
=lfEraseFil(lcTmpIvbin)

SELECT (lnCurAlias)
*-- End of Function lfConsVoid.

*!***************************************************************************
*!* Name        : lfConsUpd
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Save Data to Void Invoice screen in case of consolidated invoice
*!***************************************************************************
*!* Called from : Binmain.prg --> lfDLVODINV()
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfConsUpd()
*!***************************************************************************
FUNCTION lfConsUpd
SELECT CONSINVL
=SEEK(INVHDR.INVOICE)
SCAN REST WHILE Invoice+Store+Order+Style+STR(LineNo,6) = INVHDR.INVOICE FOR STYLE = INVLINE.Style
  SELECT IVBINLOC
  IF SEEK(InvLine.Invoice+InvLine.Style+STR(CONSINVL.LineNo,6)+InvHdr.cWareCode ,'IVBINLOC') 
    SELECT IVBINLOC
    SCAN REST WHILE Invoice+Style+STR(LineNo,6)+cWareCode+cLocation= ;
      InvLine.Invoice+InvLine.Style+STR(CONSINVL.LineNo,6)+InvHdr.cWareCode
      IF SEEK(IvBinLoc.Style+ IvBinLoc.cWareCode+IvBinLoc.cLocation ,'WHBINLOC') 
        SELECT WHBINLOC
        REPLACE Qty1   WITH Qty1+ IvBinLoc.Qty1    ,Qty2 WITH Qty2+ IvBinLoc.Qty2    ,;
                Qty3   WITH Qty3+ IvBinLoc.Qty3    ,Qty4 WITH Qty4+ IvBinLoc.Qty4    ,;
                Qty5   WITH Qty5+ IvBinLoc.Qty5    ,Qty6 WITH Qty6+ IvBinLoc.Qty6    ,;
                Qty7   WITH Qty7+ IvBinLoc.Qty7    ,Qty8 WITH Qty8+ IvBinLoc.Qty8    ,;
                TotQty WITH TotQty+ IvBinLoc.TotQty
      ELSE
        SELECT WHBINLOC
        APPEND BLANK
        REPLACE STYLE      WITH IvBinLoc.Style         ,CWARECODE  WITH IvBinLoc.cWareCode     ,;
                clocation  WITH IvBinLoc.cLocation     ,Qty1       WITH Qty1+ IvBinLoc.Qty1    ,;
                Qty2       WITH Qty2+ IvBinLoc.Qty2    ,Qty3       WITH Qty3+ IvBinLoc.Qty3    ,;
                Qty4       WITH Qty4+ IvBinLoc.Qty4    ,Qty5       WITH Qty5+ IvBinLoc.Qty5    ,;
                Qty6       WITH Qty6+ IvBinLoc.Qty6    ,Qty7       WITH Qty7+ IvBinLoc.Qty7    ,;
                Qty8       WITH Qty8+ IvBinLoc.Qty8    ,TotQty     WITH TotQty+ IvBinLoc.TotQty
        =gfAdd_Info('WHBINLOC')
      ENDIF
      *--Search in Consinvh File to get the piktkt Number.
      IF SEEK(CONSINVL.INVOICE+CONSINVL.STORE+CONSINVL.ORDER,'CONSINVH')
        IF SEEK(CONSINVH.PIKTKT+IvBinLoc.cWareCode+IvBinLoc.cLocation+IvBinLoc.Style+STR(IvBinLoc.LineNo,6),'PKBINLOC') 
          SELECT WHBINLOC
          REPLACE Alo1    WITH Alo1+ IvBinLoc.Qty1    ,Alo2 WITH Alo2+ IvBinLoc.Qty2    ,;
                  Alo3    WITH Alo3+ IvBinLoc.Qty3    ,Alo4 WITH Alo4+ IvBinLoc.Qty4    ,;
                  Alo5    WITH Alo5+ IvBinLoc.Qty5    ,Alo6 WITH Alo6+ IvBinLoc.Qty6    ,;
                  Alo7    WITH Alo7+ IvBinLoc.Qty7    ,Alo8 WITH Alo8+ IvBinLoc.Qty8    ,;
                  TotAlo  WITH TotAlo+ IvBinLoc.TotQty
          SELECT PKBINLOC
          REPLACE lInvoiced   WITH .F.
        ENDIF
      ENDIF
      SELECT IVBINLOC
      DELETE
    ENDSCAN
  ENDIF
ENDSCAN
*-- End of Function lfConsUpd.
*!***************************************************************************
*!* Name        : lfDLARVOID
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Update BinInvJL & WHBINLOC file in case of void Invoice
*!***************************************************************************
*!* Called from : lfDLVODINV IN BINMAIN.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLARVOID()
*!***************************************************************************
FUNCTION lfDLARVOID
PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv,lnInvLinNo
lnCurAlias = SELECT()
lnInvLinNo = INVLINE.LINENO
lcTmpIvbin = gfTempName()
lcTmpBnInv = gfTempName()
*--create tmpfile for invoice bin location
SELECT IVBINLOC
DIMENSION laFileStru[1,4]
= AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpIvbin) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+STR(LineNo,6)+cLocation TAG (lcTmpIvbin)

*--create tmpfile for bin invintory file
SELECT BININVJL
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpBnInv) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpBnInv)
SELECT IVBINLOC
SEEK(InvHdr.Invoice+INVLINE.STYLE+STR(INVLINE.LineNo,6))
SCAN REST WHILE Invoice+Style+STR(LineNo,6)+cWareCode+cLocation = InvHdr.Invoice+INVLINE.STYLE+STR(INVLINE.LineNo,6)
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTmpIvbin) FROM MEMVAR
ENDSCAN
SELECT InvLine
lnCurRec = RECNO()
=SEEK(INVHDR.INVOICE+STR(lnInvLinNo,6))
SCAN REST WHILE invoice+STR(lineno,6) = INVHDR.INVOICE+STR(lnInvLinNo,6)
  =SEEK(Style+cWareCode,'STYINVJL')
  SELECT STYINVJL
  SCAN REST WHILE Style+cWareCode+cSession+DTOS(dTrdate)+cTrcode+STR(LineNo,6)= ;
       InvLine.style+InvLine.cwarecode FOR cTrtype ='4' AND cTrcode = INVHDR.INVOICE AND STR(LINENO,6)==STR(lnInvLinNo,6)
    SCATTER MEMVAR MEMO
    =SEEK(Style+cWareCode+STR(LineNo,6),lcTmpIvbin)
    SELECT(lcTmpIvbin)
    SCAN REST WHILE Style+cWareCode+STR(LineNo,6)+cLocation = STYINVJL.Style+STYINVJL.cWareCode+STR(STYINVJL.LINENO,6)
      llChkQty = .F.
      FOR lnI  = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        IF (m.nStk&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.nStk&lcI))
          llChkQty = .T.
        ELSE
          llChkQty = .F.
          EXIT
        ENDIF
      ENDFOR
      IF llChkQty 
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        SELECT(lcTmpIvbin)
        FOR LNI = 1 TO 8
          LCI = ALLTRIM(STR(LNI))
          REPLACE Qty&LCI WITH Qty&LCI + ABS(m.nStk&LCI)
        ENDFOR  
        REPLACE TotQty WITH TotQty + ABS(m.nTotStk)
        EXIT
      ENDIF
      IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
        SELECT (lcTmpBnInv)
        m.clocation = &lcTmpIvbin..clocation
        APPEND BLANK
        GATHER MEMVAR MEMO
        FOR lnI = 1 TO 8
          lcI = ALLT(STR(lnI,2))
          IF ABS(m.nStk&lcI) > &lcTmpIvbin..Qty&lcI
            REPLACE nStk&lcI WITH  &lcTmpIvbin..Qty&lcI
            m.nStk&lcI = m.nStk&lcI - &lcTmpIvbin..Qty&lcI
            m.nTotStk  = m.nTotStk  - &lcTmpIvbin..Qty&lcI
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + &lcTmpIvbin..Qty&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH 0
          ELSE
            REPLACE nStk&lcI WITH m.nStk&lcI
            REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - m.nStk&lcI
            REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI - m.nStk&lcI
            m.nTotStk  = m.nTotStk  + m.nStk&lcI
            m.nStk&lcI = 0
          ENDIF 
        ENDFOR  
        REPLACE nTotStk WITH nStk1+nStk2+nStk3+nStk4+nStk5+nStk6+nStk7+nStk8
        REPLACE nstkval WITH nTotStk * nCost
      ENDIF
    ENDSCAN
  ENDSCAN                
ENDSCAN
SELECT InvLine
IF BETWEEN(lnCurRec ,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF
SELECT (lcTmpBnInv)
*T20060817.0014,3   TMI [Start] remove lines with empty stock
DELETE FOR NSTK1+NSTK2+NSTK3+NSTK4+NSTK5+NSTK6+NSTK7+NSTK8 = 0
GO TOP
*T20060817.0014,3   TMI [End  ] 
SCAN
  SCATTER MEMVAR MEMO
  INSERT INTO BININVJL FROM MEMVAR
ENDSCAN
=lfEraseFil(lcTmpBnInv)
=lfEraseFil(lcTmpIvbin)

SELECT (lnCurAlias)
*-- End of Function lfDLARVOID.
*!***************************************************************************
*!* Name        : lfChngShip
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Hold the old and new Qty if user changed in the Shipped Qty
*!***************************************************************************
*!* Called from : ARIINV.PRG -->lfvSizeQty
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfChngShip()
*!***************************************************************************
FUNCTION lfChngShip
STORE 0 TO lnOldAlias
IF m.QTY&lcSize=lcOldValue OR !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
IF m.QTY&lcSize <> lcOldValue 
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,"You can not change Shipped Qty. here , please amend your pick ticket")
  m.QTY&lcSize=lcOldValue
  RETURN
ENDIF
*-- End of Function lfChngShip.
*!***************************************************************************
*!* Name        : lfALSAVINV
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : Accounts Receivable (AR)
*!* Purpose     : Save Data for Invoice Sales Order screen
*!***************************************************************************
*!* Called from : ARINV.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfALSAVINV()
*!***************************************************************************
FUNCTION lfALSAVINV
STORE '' TO lcPkStyle
STORE .F. TO llPickOne,llBlkPck
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
IF !lfOpnFiles("WHBINLOC,PKBINLOC,IVBINLOC,WHSLOC","WHBINLST,PKLINE,IVBINLIN,WHSLOC","")
  RETURN
ENDIF
*--check in case of consolidate
IF  &lcHdrFile..CONSOL = 'Y'
  RETURN
ENDIF

llPickOne = gfGetMemVar('M_PICKONE',gcAct_Comp)  && Check if Always Pick from on bin location Yes or No.
llBlkPck  = gfGetMemVar('M_BULKPICK',gcAct_Comp) && Check if use Bulk/Pick Yes or No.

lcTmpCursr = lcDetFile+'A'
IF SEEK(&lcDetFile..PIKTKT + &lcDetFile..CWARECODE + STR(&lcDetFile..LineNo,6) + &lcDetFile..Style ,'PKBINLOC') 
  SELECT PKBINLOC
  SCAN REST WHILE PIKTKT+CWARECODE+STR(LINENO,6)+STYLE+CLOCATION = ;
      &lcDetFile..PIKTKT + &lcDetFile..CWARECODE + STR(&lcDetFile..LineNo,6) + &lcDetFile..Style
    FOR lnCnt = 1 TO 8
      lcCnt = ALLTRIM(STR(lnCnt))
      IF &lcDetFile..Pik&lcCnt <> 0
        *--Update PkbinLoc File               

        REPLACE PKBINLOC.lInvoiced  WITH .T.
        *--Update IvBinLoc File               
        IF !SEEK(lcInvNo+&lcDetFile..Style+STR(&lcDetFile..LineNo,6)+&lcDetFile..cWareCode+PKBINLOC.cLocation ,'IVBINLOC') 
          SELECT IVBINLOC
          APPEND BLANK
          REPLACE STYLE      WITH &lcDetFile..Style                                ,;
                  CWARECODE  WITH &lcDetFile..cWareCode                            ,;
                  ORDER      WITH &lcDetFile..Order                                ,;
                  Invoice    WITH lcInvNo                                          ,;
                  clocation  WITH PKBINLOC.cLocation                               ,;
                  Qty&lcCnt  WITH PKBINLOC.Qty&lcCnt                               ,;
                  TotQty     WITH PKBINLOC.Qty&lcCnt                               ,;
                  LineNo     WITH &lcDetFile..LineNo      
          =gfAdd_Info('IVBINLOC')
        ELSE
          SELECT IVBINLOC
          REPLACE Qty&lcCnt  WITH Qty&lcCnt + PKBINLOC.Qty&lcCnt                   ,;
                  TotQty     WITH TotQty+ PKBINLOC.Qty&lcCnt
        ENDIF
        IF SEEK(PKBINLOC.STYLE+PKBINLOC.CWARECODE+PKBINLOC.CLOCATION,'WHBINLOC')
          SELECT WHBINLOC
          REPLACE WHBINLOC.Alo&lcCnt WITH MAX(WHBINLOC.Alo&lcCnt - PKBINLOC.Qty&lcCnt,0) ,;
                  WHBINLOC.TotAlo    WITH MAX(WHBINLOC.TotAlo    - PKBINLOC.Qty&lcCnt,0) ,;
                  WHBINLOC.Qty&lcCnt WITH MAX(WHBINLOC.Qty&lcCnt - PKBINLOC.Qty&lcCnt,0) ,;
                  WHBINLOC.TotQty    WITH MAX(WHBINLOC.TotQty    - PKBINLOC.Qty&lcCnt ,0)
          IF TotQty = 0
            DELETE
          ENDIF         
        ENDIF  
      ENDIF
    ENDFOR
  ENDSCAN
ENDIF
*-- End of Function lfALSAVINV.
*!******************** End OF The Accounts Receivable Section ***************
*!******************** System Manger Section ********************************
*!***************************************************************************
*!* Name        : lfUseBnLoc
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Module      : System Manager (SM)
*!* Purpose     : Disable [Use Bin Location] setting if it is 'Y' (to privent 
*!*             : switching from Yes to No). that if there is any transaction
*!*             : in the Bininvjl file.
*!***************************************************************************
*!* Called from : IC.PRG (IC Application) with the Company Setup
*!***************************************************************************
*!* Calls       :
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfUseBnLoc()
*!***************************************************************************
FUNCTION lfUseBnLoc
PRIVATE lnRecCount
IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','Styinvjl','SH')
ENDIF
IF USED('BININVJL')
  COUNT TO lnRecCount
ENDIF
IF lnRecCount = 0
  RETURN
ELSE
  IF gfGetMemVar('M_WARELOC')="N"
    =lfOGShowGet('M_DLUSEBIN',M_DLUSEBIN=.T.)
  ELSE
    IF gfGetMemVar('M_DLUSEBIN')
      =lfOGShowGet('M_WARELOC',M_WARELOC="N")
      =lfOGShowGet('M_DLUSEBIN',M_DLUSEBIN=.F.)
    ENDIF
  ENDIF
ENDIF
IF USED('BININVJL')
  USE IN BININVJL
ENDIF
*-- End of Function lfUseBnLoc.
*!******************** End OF The System Manger Section *********************
*!******************** Section For Bug 130750 *******************************
*!***************************************************************************
*!* Name        : lfADDSTYBN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Add new option called (Add bin to batch file) at the inventory
*!*             : Locking screen.
*!***************************************************************************
*!* Called from : ICINVLK.PRG --> lpMainProc.
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     : = lfADDSTYBN()
*!***************************************************************************
*!B#130750
FUNCTION lfADDSTYBN
PRIVATE lnBarNo,llfound
IF !gfGetMemVar('M_DLUSEBIN') AND llMatModul
  RETURN
ENDIF
*--check if the option pad is already defined on the sysmenu
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
	IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
        llfound = .T.
		EXIT
	ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' ' SKIP FOR IIF(TYPE('lcProgName')='C' AND lcProgName = "ICLOCN",.F.,.T.)
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
llFound = .F.
FOR lnCount = 1 TO CNTBAR('_OPTIONPOP')
  IF PRMBAR('_OPTIONPOP', LnCount) = 'Add bin to batch file'
    llfound = .T.
 	EXIT
  ENDIF
ENDFOR
IF !llfound
  lnBarNo = CNTBAR('_OPTIONPOP') + 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Add bin to batch file" SKIP FOR !(laScrMode[3])
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfAdStyScr IN BINMAIN
ENDIF
RETURN
*-- End of Function lfADDSTYBN.
*!***************************************************************************
*!* Name        : lfAdStyScr
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Open a new Screen to Add a new Style/Bin to the Inventory
*!*             : Locking Batch file and therefore the whbinloc file
*!***************************************************************************
*!* Called from : Binmain.prg --> lfADDSTYBN
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     : = lfAdStyScr()
*!***************************************************************************
FUNCTION lfAdStyScr
STORE .F. TO llNewStyle,llbrowse
lcWareh = laData[8]
IF !lfOpnFiles("WHSLOC,WHBINLOC","WHSLOC,WHBINLOC","")
  RETURN
ENDIF
IF llLinkGL
  lnoldals = SELECT(0)
  SELECT CODES
  lcCodTag=TAG()
  SET ORDER TO TAG cCode_no
  IF !SEEK("N"+'CADJREASON','CODES')
    *--You have to edit the Adjustment reasons codes first, Cannot proceed.
    =gfModalGen('TRM42111B42001','DIALOG')
    SET ORDER TO TAG &lcCodTag
    SELECT &lnoldals
    RETURN 
  ELSE
    =SEEK("D"+'CADJREASON','CODES')
    lcDefAdjCd = Codes.Ccode_No 
  ENDIF  
  SET ORDER TO TAG &lcCodTag
  SELECT(lnoldals)
ENDIF
SELECT (lcBatLin)
SET ORDER TO lcBatInd
=SEEK(lcKey+laData[2]+&lcTmpQuery..Style+&lcTmpQuery..Color)
lcSty = Style  
lcClr = Color
=SEEK(lcSty,'Style')
llStyDye = (Style.cDye_Flg='Y')
SELECT (lcBatLin)
SET ORDER TO lcBatInd
=SEEK(lcKey+laData[2]+lcSty+lcClr)
IF llDyelot AND llStyDye
  LOCATE REST WHILE cbattype+cLkBatch+style+color+DYELOT = lcKey+laData[2]+lcSty+lcClr FOR !EMPTY(DYELOT)
  IF FOUND()
    lcDyelot = DYELOT
  ELSE
    =SEEK(lcKey+laData[2]+lcSty+lcClr)
  ENDIF
ENDIF
SELECT (lcBatLin)
lnLoc = IIF(ASCAN(laMloc,cLocation)<>0,ASCAN(laMloc,cLocation),1)
IF SEEK('S'+Scale,'Scale')
  FOR I = 1 TO 8
    Z = STR (I,1)
    lcSz&Z = Scale.Sz&Z
  ENDFOR
ENDIF
FOR I = 1 TO 8
  Z = STR (I,1)
  lnStk&Z = Stk&Z
ENDFOR
lnMCost   = COST
lnOldCost = OldCOST
lcReason  = cReason
lcStyDesc = IIF(SEEK(lcSty,'Style'),Style.Desc1,'')
lnNewTot  = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
lcLocBin = &lcTmpQuery..Bin                         
lcMDBrowTt = 'Detail '+lcStyPic
SELECT *,.F. AS llnew FROM (lcBatLin) WHERE Style = lcSty AND cLocation = lcLocBin ;
      AND IIF(llDyelot AND lfStyDye(lcSty),!EMPTY(&lcBatLin..DYELOT),.T.) INTO DBF (gcWorkDir + lcDetLin)
INDEX ON Style+Color+Dyelot+cLocation TAG (lcDetLin)
REPLACE ALL llNew WITH .F.
FOR I = 1 TO 8
  Z = STR (I,1)
  lnStk&Z = Stk&Z
ENDFOR
IF _TALLY = 0
  lcMes = lcStyPic +' '+ lcSty
  =gfModalGen('TRM42210B00000','DIALOG',lcMes)
  RETURN
ENDIF
FOR lnInd = 1 TO ALEN(laAdjCode,1)
  IF laAdjCode[lnInd,2] = IIF(!EMPTY(&lcDetLin..cAdjReason),&lcDetLin..cAdjReason,lcDefAdjCd)
    lnAdjCode = LnInd
    SHOW GET lnAdjCode
  ENDIF
ENDFOR
PUSH KEY
DO gcScrDir+("ICINVBN.SPX")
=lfActsBrow() 
POP KEY
*-- End of Function lfAdStyScr.
*!***************************************************************************
*!* Name        : lfStyBnBrw
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Function to avtivate detail screen browse.
*!***************************************************************************
*!* Called from : ICINVBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     : = lfStyBnBrw()
*!***************************************************************************
FUNCTION lfStyBnBrw
SELECT (lcDetLin)
lnCurrRec = RECNO()
IF llDyelot
  lcBrfield1 = [Style       :H = lcStyPic      :W=.F. ,] + [Dyelot      :H = 'Dyelot'      :W=.F. ,] +;
               [cLocation   :H = 'Bin'         :W=.F. ,] + [OldTotStk   :H = 'Old OnHand'  :W=.F. ,] +;
               [TotStk      :H = 'New OnHand'  :W=.F. ]
ELSE
  lcBrfield1 = [Style       :H = lcStyPic      :W=.F. ,] + [cLocation   :H = 'Bin'         :W=.F. ,] +;
               [OldTotStk   :H = 'Old OnHand'  :W=.F. ,] + [TotStk      :H = 'New OnHand'  :W=.F. ]
ENDIF
BROWSE FIELDS &lcBrField1 WHEN lfwhBnBrow();
       VALID :F lfvBrowse2() ;
       WINDOW ICINVBN2 IN WINDOW (gcHost);
       LOCK 0;
       NOAPPEND;
       NOCLEAR;
       NODELETE;
       NOMENU;
       NOWAIT;
       SAVE;
       TITLE lcMDBrowTt
*-- End of Function lfStyBnBrw.
*!***************************************************************************
*!* Name        : lfwhBnBrow
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : When Browse Temp. File
*!***************************************************************************
*!* Called from : ICINVBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     : = lfwhBnBrow()
*!***************************************************************************
FUNCTION lfwhBnBrow
SHOW WINDOW (lcMDBrowTt) REFRESH
lnMNewTot = IIF(llMatModul,&lcDetLin..OnHand,&lcDetLin..TotStk)
SHOW GET lnMNewTot
=lfGetAdjCd()
lcMReason = &lcDetLin..cReason 
lcReason = &lcDetLin..cReason 
lnMCost   = &lcDetLin..COST
lnOldCost = &lcDetLin..OldCOST
lcSty     = &lcDetLin..Style
lcStyDesc = IIF(SEEK(lcSty,'Style'),Style.Desc1,'')
SHOW GET lcMReason
SHOW GET lcReason
SHOW GET lnMCost
SHOW GET lnOldCost
SHOW GET lcStyDesc
SHOW GET lcSty

FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  lnStk&lcInd = STK&lcInd
  SHOW GET lnStk&lcInd
ENDFOR
lnNewTot = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
lnNewTot = IIF(llMatModul,OnHand,TOTSTK)

SHOW GET lnNewTot DISABLE
=lfRefresh('ICINVBN1')
=lfRefresh('ICINVBN5')

*-- End of Function lfwhBnBrow.
*!**************************************************************************
*!* Name        : lfVNStyBin
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Bin Locations valid for Receive into Multiple Bins Screen
*!***************************************************************************
*!* Called from : ICINVBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     :  = lfVNStyBin()
*!***************************************************************************
FUNCTION lfVNStyBin
STORE '' TO lcBinLoc,lcWarehs
STORE 0 TO lnMsgOptn
lnMsgOptn = gfModalGen('QRM00000B00025','DIALOG',.F.,.F.,'Do you want to add a new Style ?')
IF lnMsgOptn = 1
  llNewStyle = .T.
  STORE '' TO lcsty
  _CUROBJ = OBJNUM(lcSty)
ELSE
  IF lnMsgOptn = 2
    llNewStyle = .F.
    =lfDLGETBIN()
    =lfStyBnBrw()
    SHOW GETS
    _CUROBJ = OBJNUM(lnStk1)
  ELSE
    RETURN
  ENDIF
ENDIF
RETURN
*-- End of Function lfVNStyBin.
*!***************************************************************************
*!* Name        : lfVRStyBin
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid function to validate Removing a record.
*!***************************************************************************
*!* Called from : ICINVBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     : = lfVRStyBin()
*!***************************************************************************
FUNCTION lfVRStyBin
IF llNew
  DELETE
  GO BOTTOM
ELSE
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'This Style saved before in the batch file,it can not be deleted')  
ENDIF
=lfStyBnBrw()
*-- End of Function lfVRStyBin.
*!***************************************************************************
*!* Name        : lfvNBinSty
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid function to validate style field.
*!***************************************************************************
*!* Called from : ICINVBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     : = lfvNBinSty()
*!***************************************************************************
FUNCTION lfvNBinSty
PRIVATE lnAlias,lcOldSty
IF MDOWN() .AND. !llBrowse
  RETURN
ENDIF
STORE '' TO lcOldSty
lcOldSty = lcSty
lnAlias = SELECT(0)
llbrowse = llbrowse OR EMPTY(lcSty) OR '?' $ lcSty
IF llbrowse .OR. (!EMPTY(lcSty) .AND. !SEEK(lcSty,'STYLE'))
  IF !llbrowse .AND. !EMPTY(lcSty) .AND. !SEEK(lcSty,'STYLE')
    *-- give user message. to browse or reenter.
    IF gfModalGen("QRM00000B42014",.F.,.F.,.F.,'Style : '+lcSty+'is not found in the data file' ) = 2
      laData[1] = ''
      _CUROBJ = OBJNUM(IBSTYLE)
      RETURN
    ENDIF
  ENDIF
  lcSty = gfStyBrw('I',lcSty,"",.F.)
  llbrowse = .F.
ENDIF
lcSty = PADR(ALLTRIM(lcSty),19,' ')
SHOW GET lcSty
SELECT (lnAlias)
=lfDLGETBIN()
=lfStyBnBrw()
IF SEEK('S'+Scale,'Scale')
  FOR I = 1 TO 8
    Z = STR (I,1)
    lcSz&Z = Scale.Sz&Z
  ENDFOR
ENDIF
SHOW GETS
_CUROBJ = OBJNUM(lnStk1)
*-- End of Function lfvNBinSty.
*!***************************************************************************
*!* Name        : lfOkStyBin
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Validation on Bin Location Selection in ICDLBIN.SPX in case
*!*             : of locking inventory
*!***************************************************************************
*!* Called from : Binmain.Prg --> lfvOK
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     : = lfOkStyBin()
*!***************************************************************************
FUNCTION lfOkStyBin
SELECT (lcDetLin)
SCATTER MEMVAR MEMO
m.Clocation = lcBinLoc
IF llNewStyle AND SEEK(lcSty,'STYLE')
  SELECT STYLE
  SCATTER MEMVAR MEMO
  DO CASE
    CASE  lcCostMeth = 'S'  && Standard
      lnMCost    = STYLE.TotCost
      lnOldCost   = STYLE.TotCost
    OTHERWISE       && 'A' Average
      =SEEK(lcSty + laData[8],'STYDYE')
      lnMCost    = STYDYE.Ave_Cost
      lnOldCost   = STYDYE.Ave_Cost
  ENDCASE
  lcReason  = cReason
  lcStyDesc = IIF(SEEK(lcSty,'Style'),Style.Desc1,'')
ENDIF
IF SEEK(m.CBATTYPE+m.CLKBATCH+m.STYLE+m.COLOR+m.DYELOT+m.CLOCATION,lcBatLin)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'This bin is already existing in the Batch , please Choose another' )  
  RETURN
ENDIF
STORE 0 TO m.STK1,m.STK2,m.STK3,m.STK4,m.STK5,m.STK6,m.STK7,m.STK8,m.TOTSTK
STORE 0 TO lnSTK1,lnSTK2,lnSTK3,lnSTK4,lnSTK5,lnSTK6,lnSTK7,lnSTK8,lnNewTot
IF SEEK(IIF(ORDER('WHBINLOC')='WHBINLOC',m.cWareCode+m.CLOCATION+lcSty,lcSty+m.cWareCode+m.CLOCATION),'WHBINLOC')
  FOR lnStkI = 1 TO 8
    lcStkI = ALLTRIM(STR(lnStkI))
    m.OLDSTK&lcStkI = WHBINLOC.QTY&lcStkI
  ENDFOR 
  m.OldTotStk = WHBINLOC.TotQty
ELSE
  STORE 0 TO m.OLDSTK1,m.OLDSTK2,m.OLDSTK3,m.OLDSTK4,m.OLDSTK5,m.OLDSTK6,m.OLDSTK7,m.OLDSTK8,m.OLDTOTSTK
ENDIF
m.COST = lnMCost
m.OldCost = lnOldCost
IF !SEEK(m.Style+m.color+m.dyelot+m.clocation,lcDetLin)
  IF SEEK(&lcDetLin..CWARECODE+m.CLOCATION+&lcDetLin..STYLE,'WHBINLOC')
    FOR lnSizStk = 1 TO 8
      lcSizStk = ALLTRIM(STR(lnSizStk))
      m.STK&lcSizStk = WHBINLOC.QTY&lcSizStk
      lnStk&lcSizStk = WHBINLOC.QTY&lcSizStk
      m.OLDSTK&lcSizStk = WHBINLOC.QTY&lcSizStk
    ENDFOR
    STORE WHBINLOC.TOTQTY TO m.TOTSTK,m.OLDTOTSTK,lnNewTot
    m.llNew = .T.
    INSERT INTO (lcDetLin) FROM MEMVAR
    =gfAdd_Info(lcDetLin)
  ELSE
    m.llNew = .T.    
    INSERT INTO (lcDetLin) FROM MEMVAR
    =gfAdd_Info(lcDetLin)
  ENDIF
ENDIF
SELECT (lcDetLin)
LOCATE FOR STYLE+CLOCATION = m.STYLE+m.CLOCATION
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  REPLACE STK&lcInd WITH lnStk&lcInd
  SHOW GET lnStk&lcInd
ENDFOR
lnNewTot = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
REPLACE TOTSTK WITH lnNewTot
llNewStyle = .F.
*-- End of Function lfOkStyBin.
*!***************************************************************************
*!* Name        : lfvSizQty
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid Function Per Size/Quantity.
*!***************************************************************************
*!* Called from : ICINVBN.SPR --> Per Size
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     : = lfvSizQty()
*!***************************************************************************
FUNCTION lfvSizQty
IF MDOWN() 
  RETURN
ENDIF
SELECT (lcDetLin)
FOR lnInd = 1 TO 8
  lcInd = STR(lnInd,1)
  REPLACE STK&lcInd WITH lnStk&lcInd
ENDFOR
lnNewTot = lnStk1+lnStk2+lnStk3+lnStk4+lnStk5+lnStk6+lnStk7+lnStk8
REPLACE TOTSTK WITH lnNewTot
=lfStyBnBrw()
SHOW GET lnNewTot DISABLE
*-- End of Function lfvSizQty.
*!***************************************************************************
*!* Name        : lfvNewCost
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid Function for input a new cost.
*!***************************************************************************
*!* Called from : ICINVBN.SPR --> lnMCost field
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvNewCost()
*!***************************************************************************
FUNCTION lfvNewCost
IF MDOWN() 
  RETURN
ENDIF
SELECT (lcDetLin)
REPLACE COST WITH lnMCost

*-- End of Function lfvNewCost.
*!***************************************************************************
*!* Name        : lfVExitOk
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 04/04/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Valid Function for Save button in mdWn screen buton.
*!***************************************************************************
*!* Called from : ICINVBN.SPR --> Ok button
*!***************************************************************************
*!* Reference   : B#130750
*!***************************************************************************
*!* Example     : = lfVExitOk()
*!***************************************************************************
FUNCTION lfVExitOk
PRIVATE llBinFound
STORE .F. TO llBinFound
SELECT (lcDetLin)
SCAN
  SCATTER MEMVAR MEMO
  llBinFound = .F.
  SELECT(lcTmpQuery)
  =SEEK(m.STYLE+m.Color)
  SCAN REST WHILE STYLE+Color = m.STYLE+m.Color FOR BIN = &lcDetLin..clocation 
    m.BIN = m.clocation
    REPLACE STOCK WITH &lcDetLin..TotStk,Cost WITH lnMCost
    llBinFound = .T.
  ENDSCAN
  IF !llBinFound AND !(m.TotStk =0 AND m.OldTotStk = 0)
    m.BIN     = m.clocation
    m.StyDesc = IIF(SEEK(m.Style,'Style'),Style.Desc1,'')
    m.Stock   = m.TotStk
    m.OStock  = m.OldTotStk
    INSERT INTO (lcTmpQuery) FROM MEMVAR
    REPLACE OCost WITH lnOldCost,Cost WITH lnMCost
  ENDIF
ENDSCAN
SELECT (lcDetLin)
SCAN
  IF SEEK(LCKEY+LADATA[2]+&lcDetLin..Style+&lcDetLin..Color+&lcDetLin..Dyelot+&lcDetLin..cLocation,lcBatLin)
    SELECT (lcBatLin)
    DELETE
  ENDIF
ENDSCAN
SELECT (lcBatLin)
APPEND FROM (gcWorkDir+lcDetLin)
SELECT (lcDetLin)
SCAN
  IF !SEEK(CWARECODE+CLOCATION+STYLE,'WHBINLOC')  
    SELECT WHBINLOC
    APPEND BLANK
    REPLACE STYLE      WITH &lcDetLin..Style     ,;
            CWARECODE  WITH &lcDetLin..CWARECODE ,;
            clocation  WITH &lcDetLin..cLOCATION ,;
            Qty1       WITH 0					 ,;
            Qty2       WITH 0                    ,;
            Qty3       WITH 0                    ,;
            Qty4       WITH 0                    ,;
            Qty5       WITH 0                    ,;
            Qty6       WITH 0                    ,;
            Qty7       WITH 0                    ,;
            Qty8       WITH 0                    ,;
            TotQty     WITH 0                        
    *--Update cBlkPck and cSection
    lcOldOrd = ORDER('WHSLOC')
    SET ORDER TO TAG WhsLoc IN  WHSLOC
    IF SEEK(&lcDetLin..CWARECODE +&lcDetLin..clocation +SPACE(19),'WHSLOC') 
      SELECT WHBINLOC
      REPLACE cBlkPck   WITH WHSLOC.cBlkPck  ,;
              cSection  WITH WHSLOC.cSection ,;
              cBinClass WITH WHSLOC.cBinClass
    ENDIF
    SET ORDER TO TAG &lcOldOrd IN  WHSLOC
    =gfAdd_Info('WHBINLOC')
  ENDIF
ENDSCAN
ZAP
laData[3] = IIF(laData[3] = 'H','H','M')
SELECT (lcBatHdr)
GOTO TOP
REPLACE Type WITH laData[3]
CLEAR READ
IF USED(lcDetLin)
  USE IN (lcDetLin)
ENDIF
*--Erase the lcDetLin file.
=lfEraseFil(lcDetLin)

*-- End of Function lfVExitOk.
*!******************** End OF The Bug 130750 Section ************************
*!******************** Section For Custom 132193 ****************************
*!***************************************************************************
*!* Name        : LFFILLSEC
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 05/16/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : Fill Array laSource for section Mover at inventory locking Grid
*!***************************************************************************
*!* Called from : REPORTS\IC\ICSTYLST.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Reference   : CP#132193
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = LFFILLSEC()
*!***************************************************************************
FUNCTION LFFILLSEC
DIME laScSource[1,1]
DIME laScTarget[1,1]
SELECT DISTINCT CSECTION FROM WHSLOC WHERE CWARECODE =laData[8] AND !EMPTY(CSECTION) INTO ARRAY laScSource
*-- End of Function LFFILLSEC.
*!***************************************************************************
*!* Name        : LFVSECTION
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 05/16/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : create Sections mover at inventory locking Grid
*!***************************************************************************
*!* Called from : Inventory Locking Grid --> ICSTYLST.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Reference   : CP#132193
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = LFVSECTION()
*!***************************************************************************
FUNCTION LFVSECTION
IF EMPTY(laScSource)
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There are no defined Sections')
ELSE
  = gfMover(@laScSource,@laScTarget,'Sections',.T.,'')
ENDIF
*-- End of Function LFVSECTION.

*!***************************************************************************
*!* Name        : LFBINSFLTR
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 05/16/2006
*!* Module      : Inventory Control (IC)
*!* Purpose     : filter on Bins mover at inventory locking Grid according to
*!*             : selection of section mover
*!***************************************************************************
*!* Called from : REPORTS\IC\ICSTYLST.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Reference   : CP#132193
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = LFBINSFLTR()
*!***************************************************************************
FUNCTION LFBINSFLTR
IF !EMPTY(laScTarget)
  laRpSource = ''
  SELECT DISTINCT CLOCATION FROM WHSLOC WHERE CWARECODE =laData[8] AND !EMPTY(CLOCATION) AND ;
    ASCAN(laScTarget,CSECTION)>0 INTO ARRAY laRpSource 
  IF EMPTY(laRpSource)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'There are no assigned bins for the selected sections')
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.
*-- End of Function LFBINSFLTR.
*!******************** End OF The CP#132193 Section ********************

            ****************** start OF The Style Purchase Section **********
            
*!***************************************************************************
*!* Name        : lfSAVBINS
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Save Locations in WHBINLOC File from PO
*!***************************************************************************
*!* Called from : Poupdate.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSAVBINS()
*!***************************************************************************
FUNCTION lfSAVBINS
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
PRIVATE lnAlias
lnAlias = SELECT(0)
*T20060817.0014,1 (B132562) TMI [Start] in creating a table in fox the first 8 charcters only are recognized,  
*                             renaming the variable to be 8 characters only
*lcBinLine = lcTmpLine + 'A'
lcBinLine = "_"+SUBSTR(lcTmpLine,2)   && start the variable name with the underscore "_"
*T20060817.0014,1 (B132562) TMI [End  ] 

IF !lfOpnFiles("STYINVJL,BININVJL,WHBINLOC,WHSLOC","STYINVJL,STYINVJL,WHBINLOC,WHSLOC","")
  RETURN
ENDIF

SELECT (lcBinLine)
LOCATE
*T20060817.0014   TMI [Start] do not add non inv. styles to whbinloc
*SCAN 
SCAN FOR TOTSTK > 0 .AND. SEEK(STYLE,'STYLE') .AND. STYLE.LINVSTY 
  *T20060817.0014   TMI [End  ] 
  m.loc1 = &lcBinLine..loc1
  m.loc2 = &lcBinLine..loc2
  m.loc3 = &lcBinLine..loc3
  m.loc4 = &lcBinLine..loc4
  m.loc5 = &lcBinLine..loc5
  m.loc6 = &lcBinLine..loc6
  m.loc7 = &lcBinLine..loc7
  m.loc8 = &lcBinLine..loc8
  FOR R = 1 TO 8
    LCR = ALLT(STR(R,2))
    IF !EMPTY(m.loc&lcR) AND &lcBinLine..QTY&LCR > 0
      IF !SEEK(&lcBinLine..cWareCode+m.loc&LCR+&lcBinLine..style,'WHBINLOC')
        SET ORDER TO TAG WhsLoc IN  WHSLOC
        IF SEEK(&lcBinLine..cWareCode+m.loc&LCR,'WHSLOC')
          m.cbinclass = WHSLOC.cbinclass
          m.cBlkPck   = WHSLOC.cBlkPck
          m.cSection  = WHSLOC.cSection
        ENDIF
        SET ORDER TO TAG WhsLocSt IN  WHSLOC
        SELECT WHBINLOC
        APPEND BLANK
        REPLACE Style        WITH  &lcBinLine..style     ,;
  		        cWareCode    WITH  &lcBinLine..cWareCode ,;
  		        clocation    WITH  m.loc&LCR             ,;
  		        Qty&LCR      WITH  &lcBinLine..Qty&LCR   ,;
  		        TOTQTY       WITH  &lcBinLine..Qty&LCR   ,;
  		        cbinclass    WITH  m.cbinclass           ,;
                cBlkPck      WITH  m.cBlkPck             ,;
                cSection     WITH  m.cSection
        =gfAdd_Info('WHBINLOC')
      ELSE
        REPLACE  WHBINLOC.Qty&LCR WITH WHBINLOC.Qty&LCR + &lcBinLine..Qty&LCR,;
                 WHBINLOC.TOTQTY  WITH WHBINLOC.TOTQTY  + &lcBinLine..Qty&LCR
      ENDIF     
      lcOldOrder = ORDER('WHSLOC')
      SET ORDER TO TAG WhsLoc IN  WHSLOC
      IF !SEEK(&lcBinLine..cWareCode+m.Loc&LCR+&lcBinLine..Style,'WHSLOC')
        =SEEK(&lcBinLine..cWareCode+m.Loc&LCR,'WHSLOC')
        SCATT MEMVAR MEMO
        m.STYLE     = &lcBinLine..style
        m.clocation = m.loc&LCR
        INSERT INTO WHSLOC FROM MEMVAR 
      ENDIF
      SET ORDER TO TAG &lcOldOrder IN  WHSLOC
    ENDIF  
  ENDFOR
ENDSCAN
SELECT(lnAlias)
*-- End of Function lfSAVBINS.

*!***************************************************************************
*!* Name        : lfDLSBNPOR
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Update BininvJL from Styinvjl
*!***************************************************************************
*!* Called from : Poupdate.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSBNPOR()
*!***************************************************************************
FUNCTION lfDLSBNPOR
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF

PRIVATE lcStyOrd,lnAlias,lcCurTmp 
DIMENSION laBinArr[8]
*T20060817.0014,1 (B132562) TMI [Start] use different temp name since fox recognize only first 8 chars of a file alias
*lcBinLine = lcTmpLine + 'A'
lcBinLine = "_"+SUBSTR(lcTmpLine,2)
*T20060817.0014,1 (B132562) TMI [End  ] 
lnAlias   = SELECT(0)
lnGetRec  = RECNO() 
lnCurLin  = Lineno
lcStybn   = IIF(!EMPTY(&lcTmpLine..STYLE),&lcTmpLine..STYLE,m.STYLE)
*T20060817.0014(132757)   TMI [Start] check the case of 2nd quality/damaged style
lcStybn = IIF(!EMPTY(&lcTmpLine..cRetSty),&lcTmpLine..cRetSty,lcStybn)
*T20060817.0014   TMI [End  ] 
lcPobn    = IIF(!EMPTY(&lcTmpLine..PO),&lcTmpLine..Po,m.Po)

IF !lfOpnFiles("STYINVJL,BININVJL","STYINVJL,STYINVJL","")
  RETURN
ENDIF

lcCurTmp = gfTempName()
CREATE TABLE (gcWorkDir + lcCurTmp)(nstk1 N(7),nstk2 N(7),nstk3 N(7),nstk4 N(7),nstk5 N(7),nstk6 N(7),nstk7 N(7),;
       nstk8 N(7),nTotstk N(7),Binloc C(10))
INDEX ON Binloc  TAG (lcCurTmp)

SELECT (lcTmpLine)
IF SEEK(lcPobn + lcStybn + STR(lnCurLin,6) ,lcBinLine)
  SELECT(lcBinLine)
  SCAN REST WHILE PO + STYLE + STR(Lineno,6) = lcPobn + lcStybn + STR(lnCurLin,6)
    STORE '' TO laBinArr
    FOR I = 1 TO 8 
      LCI = ALLTRIM(STR(I,1))
      laBinArr[I] =  &lcBinLine..loc&LCI
    ENDFOR
    SELECT (lcCurTmp)
    FOR N = 1 To 8
      LCN = ALLTRIM(STR(N,1))
      IF &lcBinLine..Qty&LCN<> 0  
        IF !SEEK(laBinArr[N],lcCurTmp) 
          APPEND BLANK
          REPLACE nstk&LCN WITH &lcBinLine..Qty&LCN,;
                  nTotstk  WITH &lcBinLine..Qty&LCN,;
                  Binloc   WITH laBinArr[N]
        ELSE
          REPLACE nstk&LCN WITH &lcBinLine..Qty&LCN + nstk&LCN ,;
                  nTotstk  WITH nTotstk + &lcBiNLine..Qty&LCN
        ENDIF
      ENDIF  
    ENDFOR
  ENDSCAN
ENDIF
SELECT (lcCurTmp)
LOCATE
IF EOF()
  SELECT(lnAlias)
  IF BETWEEN(lnGetRec,1,RECCOUNT())
    GOTO lnGetRec 
  ENDIF
  IF USED(lcCurTmp)
    USE IN &lcCurTmp
  ENDIF  
  *--Erase the lcCurTmp file.
  =lfEraseFil(lcCurTmp)
  RETURN
ENDIF

SELECT STYINVJL
=SEEK(lcStybn + m.cwarecode + lcGlSession)
SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = lcStybn + m.cwarecode + lcGlSession
 IF (dtrdate = ldTrDate) AND (ctrcode = lcPobn) AND (lineno = m.lineno)
   EXIT
 ENDIF
ENDSCAN
SCATT MEMVAR MEMO
SELECT (lcCurTmp)
SCAN
  SCATT MEMVAR MEMO
  m.clocation = m.Binloc 
  m.nstkval   = m.nTotstk * m.nCost
  SELECT BININVJL
  IF !SEEK(lcStybn+m.cwarecode+m.csession+DTOS(m.dtrdate)+m.Ctrcode+STR(m.lineno,6))
    APPEND BLANK
    GATHER MEMVAR MEMO
  ELSE
    llBinFound = .F.
    SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = ;
         m.Style+m.cwarecode+m.csession+DTOS(m.dtrdate)+m.Ctrcode+STR(m.lineno,6) FOR ALLTRIM(cLocation) = ALLTRIM(m.clocation)
      llBinFound = .T.
    ENDSCAN
    IF !llBinFound
      APPEND BLANK
      GATHER MEMVAR MEMO
    ENDIF
  ENDIF
  llBinFound = .F.
ENDSCAN

IF USED(lcCurTmp)
  USE IN &lcCurTmp
ENDIF  
*--Erase the lcCurTmp file.
=lfEraseFil(lcCurTmp)

SELECT(lnAlias)
IF BETWEEN(lnGetRec,1,RECCOUNT())
  GOTO lnGetRec 
ENDIF
*-- End of Function lfDLSBNPOR.
*!***************************************************************************
*!* Name        : lfVLDRFRSH
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Refrish custom screen 
*!***************************************************************************
*!* Called from : Postrec.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfVLDRFRSH()
*!***************************************************************************
FUNCTION lfVLDRFRSH
IF gfGetMemVar('M_DLUSEBIN')
  =lfRefresh("PODLRCV")
ENDIF  
*-- End of Function lfVLDRFRSH.
*!*************************************************************************
*!* Name        : lfDLCHKSAV
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Validate Po Receiveing into multiple Bins.
*!***************************************************************************
*!* Called from : Postrec.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLCHKSAV()
*!***************************************************************************
FUNCTION lfDLCHKSAV

IF !gfGetMemVar('M_DLUSEBIN')
  RETURN .F.
ENDIF
STORE .F. TO llmessag , llReturn , llChckLoc , llChckCls , llNotAssgn
*T20060817.0014,1 (B132562) TMI [Start] use different temp name since fox recognize only first 8 chars of a file alias
*lcBinLine = lcTmpLine + 'A'
lcBinLine = "_"+SUBSTR(lcTmpLine,2)
*T20060817.0014,1 (B132562) TMI [End  ] 
DIMENSION laStyArr[1]
STORE '' TO laStyArr
SELECT (lcTmpLine)
lcOldOrder = ORDER()
SET ORDER TO POSLN
LOCATE
*T20060817.0014(132757)   TMI [Start] include the case of 2nd Quality/damaged styles
*SCAN FOR TRANCD ='1'
PRIVATE lcStyLocs
lcStyLocs = ''
SCAN FOR TRANCD $'1'+IIF(llMFCall,'3','4')
  *T20060817.0014   TMI [End  ]
  IF llReturn
    EXIT
  ENDIF
  *T20060817.0014   TMI [Start] include the case of non 1st quality styles
  *IF SEEK(&lcTmpLine..PO + &lcTmpLine..Style+ STR(&lcTmpLine..LINENO,6) ,lcBinLine)
  *  SELECT(lcBinLine)
  *  SCAN REST WHILE PO + STYLE + STR(LINENO,6) = &lcTmpLine..PO + &lcTmpLine..Style +STR(&lcTmpLine..LINENO,6)
  PRIVATE lcSeekSty
  lcSeekSty = IIF(TRANCD='1',&lcTmpLine..Style,&lcTmpLine..cRetSty)
  IF SEEK(&lcTmpLine..PO + lcSeekSty + STR(&lcTmpLine..LINENO,6) ,lcBinLine)
    SELECT(lcBinLine)
    SCAN REST WHILE PO + STYLE + STR(LINENO,6) = &lcTmpLine..PO + lcSeekSty +STR(&lcTmpLine..LINENO,6)
      *T20060817.0014   TMI [End  ] 
      m.loc1 = &lcBinLine..loc1
      m.loc2 = &lcBinLine..loc2
      m.loc3 = &lcBinLine..loc3
      m.loc4 = &lcBinLine..loc4
      m.loc5 = &lcBinLine..loc5
      m.loc6 = &lcBinLine..loc6
      m.loc7 = &lcBinLine..loc7
      m.loc8 = &lcBinLine..loc8
      FOR Lni = 1 TO 8
        Lci = ALLT(STR(Lni,2))
        IF !EMPTY(&lcBinLine..Qty&lcI)
          IF EMPTY(m.Loc&LcI) AND TOTSTK <> 0
            llChckLoc = .T.
            *T20060817.0014   TMI [Start] update the lcStylocs vairable 
            *=SEEK(Style,'STYLE')
            IF !lcSeekSty $ lcStyLocs
              lcStyLocs = lcStyLocs + lcSeekSty + CHR(13)
            ENDIF
            =SEEK(lcSeekSty,'STYLE')
            *T20060817.0014   TMI [End  ] 
            
            IF EMPTY(laStyArr[1])
              laStyArr[1] = Style.cflathang
            ELSE
              IF !(laStyArr[1] = Style.cflathang)
                llChckCls = .T.
                llReturn  = .T.
                EXIT
              ENDIF
            ENDIF
          ENDIF
        ENDIF  
      ENDFOR
    ENDSCAN
  ELSE
    llReturn   = .F.
    *T20060817.0014   TMI [Start] comment this assignment and replace it with the below one
    *IF TOTSTK <> 0
    *  llNotAssgn = .T.
    *ENDIF
    *T20060817.0014   TMI [End  ] 
    SCATTER MEMVAR MEMO
    INSERT INTO (lcBinLine) FROM MEMVAR
    *T20060817.0014(132757)   TMI [Start] update lcbinline with the correct style
    REPLACE &lcBinLine..STYLE WITH lcSeekSty
    SELECT &lcBinLine
    IF ((Qty1<>0 .AND. EMPTY(Loc1)) .OR. ;
        (Qty2<>0 .AND. EMPTY(Loc2)) .OR. ;
        (Qty3<>0 .AND. EMPTY(Loc3)) .OR. ;
        (Qty4<>0 .AND. EMPTY(Loc4)) .OR. ;
        (Qty5<>0 .AND. EMPTY(Loc5)) .OR. ;
        (Qty6<>0 .AND. EMPTY(Loc6)) .OR. ;
        (Qty7<>0 .AND. EMPTY(Loc7)) .OR. ;
        (Qty8<>0 .AND. EMPTY(Loc8)) );
      .AND. !lcSeekSty $ lcStyLocs
      lcStyLocs = lcStyLocs + lcSeekSty + CHR(13)
      llNotAssgn = .T.
    ENDIF
    *T20060817.0014   TMI [End  ] 
  ENDIF
ENDSCAN

*T20060817.0014   TMI [Start] Update the lcbinline lines from lcTmpLine by decreasing the 1st quality with 2nd and damaged
SELECT (lcBinLine)
LOCATE
SCAN FOR TRANCD='1'
  IF SEEK('P'+PO+STYLE+STR(LINENO,6)+'2',lcTmpLine)
    REPLACE QTY1 WITH &lcTmpLine..QTY1 ;
            QTY2 WITH &lcTmpLine..QTY2 ;
            QTY3 WITH &lcTmpLine..QTY3 ;
            QTY4 WITH &lcTmpLine..QTY4 ;
            QTY5 WITH &lcTmpLine..QTY5 ;
            QTY6 WITH &lcTmpLine..QTY6 ;
            QTY7 WITH &lcTmpLine..QTY7 ;
            QTY8 WITH &lcTmpLine..QTY8 ;
            TOTQTY WITH QTY1+QTY2+QTY3+QTY4+QTY5+QTY6+QTY7+QTY8         
  ELSE
    DELETE
  ENDIF
ENDSCAN
*T20060817.0014   TMI [End  ] 

SELECT (lcBinLine)
LOCATE
IF llChckCls
  lcMsg2 = 'You have mixed Flat and Hanging styles on this transaction - please enter the bin locations manually'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llReturn = .T.
  STORE .F. TO llShow,llCSave
  SELECT (lcTmpLine)
  SET ORDER TO &lcOldOrder
  LOCATE
  RETURN llReturn
ENDIF
IF llChckLoc OR llNotAssgn
  lcMsg2 = "There is a line or more that didn't assign to bins,you have to assign bin location before saving." + ;
          " Do you want to assign bin location now to not assigned Quantities?"
  *T20060817.0014   TMI [Start] open the smmsg screen
  *lnChoose =gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,lcMsg2)
  PUSH KEY
  ON KEY 
  lcLogMsg  = lcMsg2 + CHR(13) + CHR(13) + ;
           "Style(s) with missing locations:"+CHR(13)+;
           lcStyLocs
  PRIVATE llOk
  llOk = .F.
  DO (gcScrDir+"SMMSG.SPX")
  POP KEY
  lnChoose = IIF(llOk,1,2)
  *T20060817.0014   TMI [End  ] 
  IF lnChoose = 1
    IF !USED('WHSLOC') 
      =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    DIME laTempData[3]
    Store '' TO lcBinLoc , laTempData
    DO (gcScrDir+gcWinAppl+"\POdlbnSC.Spx")
    IF EMPTY(lcBinLoc)
      lcMsg2 = 'You have to assign bin location before saving .'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llReturn = .T.
      STORE .F. TO llShow,llCSave
      SELECT (lcTmpLine)
      SET ORDER TO &lcOldOrder
      LOCATE
      RETURN llReturn
    ENDIF
    SELECT (lcBinLine)
    llReturn = .F.
    SCAN
      =SEEK(&lcBinLine..Style,'Style')
      IF !(laTempData[3] = style.cflathang)
        *--'You cannot saving the style to a bin of a different Hang/Flat.'
        lcMsg2 = 'You cannot saving the style to a bin of a different Hang/Flat.'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        llReturn = .T.
        STORE .F. TO llShow,llCSave
        SELECT (lcTmpLine)
        SET ORDER TO &lcOldOrder
        LOCATE
        RETURN llReturn
      ELSE
        FOR I = 1 TO 8
          LCI = ALLTRIM(STR(I))
          IF EMPTY(&lcBinLine..LOC&LCI) AND &lcBinLine..QTY&LCI > 0
            REPLACE &lcBinLine..LOC&LCI  WITH laTempData[1]
          ENDIF           
        ENDFOR
      ENDIF
    ENDSCAN
    STORE .F. TO llShow,llCSave
    SELECT (lcTmpLine)
    SET ORDER TO &lcOldOrder
    LOCATE
    RETURN llReturn
  ELSE
    llReturn = .T.
    STORE .F. TO llShow,llCSave
    SELECT (lcTmpLine)
    SET ORDER TO &lcOldOrder
    LOCATE
    RETURN llReturn
  ENDIF  
ENDIF

SELECT (lcTmpLine)
SET ORDER TO &lcOldOrder
LOCATE
SHOW GETS
RETURN llReturn

*-- End of Function lfDLCHKSAV.
*!***************************************************************************
*!* Name        : lfDVLDPOBN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Accept issue/Return Styles Per Bin
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDVLDPOBN()
*!***************************************************************************
FUNCTION lfDVLDPOBN
IF gfGetMemVar('M_DLUSEBIN')
  llChkQty = .F.
  FOR lnI  = 1 TO 8
    lcI = ALLT(STR(lnI,2))  
    IF (ABS(laIssQty[lnI]) <> 0 ) AND (ABS(laIssQty[lnI]) > m.Bin_Qty&lcI)
      llChkQty = .T.
      EXIT
    ELSE
      llChkQty = .F.
    ENDIF
  ENDFOR
  IF llIssue AND llChkQty 
        lcStyle = m.Item
        *--The receiving quantity are not covered the issued quantity
        *--for Style XXXX , This transaction line will be ignored.
        lcMsg2 = 'There is not enough stock of component '+lcStyle+ ' on hand at this location - please adjust the Issue Quantity'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  ELSE
    =lfIssRetSty(m.Typ,m.cCatgTyp,m.Item,lcIssWare,m.Dyelot,@laIssQty,lnIssCost,;
               m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
               IIF(llIssue,SPACE(6),lcSession),IIF(llIssue,lcSession,SPACE(6)))
  ENDIF             
ELSE
  =lfIssRetSty(m.Typ,m.cCatgTyp,m.Item,lcIssWare,m.Dyelot,@laIssQty,lnIssCost,;
               m.cOprCode,IIF(EMPTY(m.cOprCode),'',laLots[lnLotNo]),ldIssDate,;
               IIF(llIssue,SPACE(6),lcSession),IIF(llIssue,lcSession,SPACE(6)))
ENDIF
*-- End of Function lfDVLDPOBN.
*!***************************************************************************
*!* Name        : lfDVLDPOAU
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Issue Lot cost items Per Bin
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDVLDPOAU()
*!***************************************************************************
FUNCTION lfDVLDPOAU
IF gfGetMemVar('M_DLUSEBIN')
  llChkQty = .F.
  DIMENSION laOldQty[9]
  STORE 0 TO laOldQty
  IF !USED('WHBINLOC')
    =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLOC','SH')
  ENDIF
  lcStyle = &lcIssLtFile..Item
  SELECT WHBINLOC
  lcOrdOrdr = ORDER()
  SET ORDER TO Whbinlst
  IF SEEK(lcStyle + laData[32])
    FOR lnCntr = 1 TO 9
      laOldQty[lnCntr] = laIssQty[lnCntr] 
    ENDFOR
    SCAN REST WHILE style+cwarecode+clocation = lcStyle + laData[32]
      FOR lnI  = 1 TO 8
        lcI = ALLT(STR(lnI,2))  
        IF (ABS(laOldQty[lnI]) <> 0 ) AND (ABS(laOldQty[lnI]) > (WHBINLOC.Qty&lcI-WHBINLOC.Alo&lcI))
           laOldQty[lnI]    = laOldQty[lnI] + (WHBINLOC.Qty&lcI-WHBINLOC.Alo&lcI)
           laOldQty[9]      = laOldQty[9] + (WHBINLOC.Qty&lcI-WHBINLOC.Alo&lcI)
        ELSE
           laOldQty[9]      = laOldQty[9] - laOldQty[lnI]
           laOldQty[lnI]    = 0
        ENDIF
      ENDFOR
    ENDSCAN
  ENDIF
  IF  laOldQty[9] = laIssQty[9] 
        lcStyle = &lcIssLtFile..Item
        *--The receiving quantity are not covered the issued quantity
        *--for Style XXXX , This transaction line will be ignored.
        lcMsg2 = 'There is not enough stock of component '+lcStyle+ ' on hand at this location - please adjust the Issue Quantity'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  ELSE
    SELECT (lcIssLtFile)
    =lfIssRetSty(Typ,cCatgTyp,lcStyle,cWareCode,Dyelot,;
                 @laIssQty,lnIssCost,lcOprCode,lcLotNo,ldIssDate,SPACE(6),lcSession)
  ENDIF             
ELSE
  =lfIssRetSty(Typ,cCatgTyp,Item,cWareCode,Dyelot,;
               @laIssQty,lnIssCost,lcOprCode,lcLotNo,ldIssDate,SPACE(6),lcSession)
ENDIF
*-- End of Function lfDVLDPOAU.
*!**************************************************************************
*!* Name        : LFPORECPAD
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Add option menu to the receiving PO Screen.
*!***************************************************************************
*!* Called from : Postrec.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : LFPORECPAD()
*!***************************************************************************
FUNCTION LFPORECPAD
*--check if the option pad is already defined on the sysmenu
PRIVATE llFound
STORE .F. TO llFound
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
    llfound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' ' SKIP FOR ;
         IIF(TYPE('lcProgName')='C' AND lcProgName = "POSTREC",.F.,.T.) OR (laScrMode[1]) OR (laScrMode[2]) 
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
  lnBarNo = 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Receive into Multiple Bins" SKIP FOR ;
             laScrMode[1] OR EMPTY(lcTCode) OR !gfGetMemvar('M_DLUSEBIN')
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDispScr in BINMAIN
ELSE
  *-- Count Options BARS to add the new BAR at the end of the Popup
  lnBarNo = CNTBAR(LEFT('_OPTIONPOP',10)) + 1

  *-- Skip The New BAR if this is the Select mode or the PO field is Empty.
  ON PAD _OPTION OF _MSYSMENU ACTIVATE POPUP _OPTIONPOP
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Receive into Multiple Bins" SKIP FOR ;
             laScrMode[1] OR EMPTY(lcTCode) OR !gfGetMemvar('M_DLUSEBIN')
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDispScr in BINMAIN
ENDIF
RETURN
*-- End of Function LFPORECPAD.
*!*************************************************************************
*!* Name        : lfCrtBinLn
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Create new cursor for the receive into Multiple Bins screen
*!***************************************************************************
*!* Called from : Postrec.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfCrtBinLn()
*!***************************************************************************
FUNCTION lfCrtBinLn
*T20060817.0014,1 (B132562) TMI [Start] use different temp name since fox recognize only first 8 chars of a file alias
*lcBinLine = lcTmpLine + 'A'
lcBinLine = "_"+SUBSTR(lcTmpLine,2)
*T20060817.0014,1 (B132562) TMI [End  ] 
SELECT(lcTmpLine)
= AFIELDS(laFileStru)
CREATE CURSOR (lcBinLine) FROM ARRAY laFileStru
INDEX ON PO + STYLE + STR(LINENO,6) TAG (lcBinLine)
*-- End of Function lfCrtBinLn.
*!**************************************************************************
*!* Name        : lfDispScr
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : 
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfDispScr()
*!***************************************************************************
FUNCTION lfDispScr
DECLARE laShipQty[9],laInputQty[9],laBinLoc[8],laBrowArr[1],laReceived[9]
STORE "" TO lcMjrPct,lcStyMjr,lcColor,lcStyDesc,lcBinLine,lcSlcStyle,laBinLoc,lcSize1,;
           lcSize2,lcSize3,lcSize4,lcSize5,lcSize6,lcSize7,lcSize8,laBrowArr,lcSelBin,lcWareCode
STORE 0 TO lnClrLen,lnClrStPos,lnStyLen,lnStyStPos,lnScaLen,lnScaStPos,lnBalQty1,lnBalQty2,;
           lnBalQty3,lnBalQty4,lnBalQty5,lnBalQty6,lnBalQty7,lnBalQty8,lnBalQty9,laShipQty,;
           laInputQty,lnRecNo,lnOldValue,lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,;
           lnQty8,lnQty9,lnMarker,lnLineno,LnPoNo
STORE .F. TO llNew ,llMove
= lfChkStrct() 
lcDetTtl   = "Detail Receiving Lines"
lcOkKey    = (gcBmpHome+ "OK.BMP")
lcRemKey   = (gcBmpHome+ "Remove.BMP")
lcCanKey   = (gcBmpHome+ "Can.BMP")
lcExtKey   = (gcBmpHome+ "EXTKEY.BMP")
lcNewKey   = (gcBmpHome+ "NEW.BMP")
STORE " " TO POSHPBN
*T20060817.0014,1 (B132562) TMI [Start] use different temp name since fox recognize only first 8 chars of a file alias
*lcBinLine  = lcTmpLine + 'A'
lcBinLine = "_"+SUBSTR(lcTmpLine,2)
*T20060817.0014,1 (B132562) TMI [End  ] 
SELECT(lcTmpLine)
lnRecNo = RECNO()
lcSlcStyle = &lcTmpLine..Style
lnLineNo   = &lcTmpLine..LineNo
lnPoNo     = &lcTmpLine..PO
lcWareCode = &lcTmpLine..cWareCode
lcWinTitle = "Receive into Multiple Bins For Purchase Order " + lnPoNo 
*--Get the Received Qty. as Cancel,Second and Damaged
SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laReceived FOR STYLE = lcSlcStyle AND LineNo =lnLineNo AND !(TRANCD $ '12')

IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpLine))
  GOTO lnRecNo IN (lcTmpLine)
ENDIF
*--get the Original Qty.
SCATTER FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty TO laShipQty
*--get the Opened Qty after remove the already received qty as Cancel,Second and Damaged
FOR I = 1 TO 9
  laShipQty[I] = laShipQty[I] - laReceived[I]
ENDFOR

IF !USED(lcBinLine)
  SELECT(lcTmpLine)
  LOCATE
  SCATTER MEMVAR MEMO
  SCAN FOR TRANCD='2' AND &lcTmpLine..Style = lcSlcStyle AND &lcTmpLine..LineNo =lnLineNo   
    SCATTER MEMVAR MEMO
    IF !EMPTY(ALLTRIM(LOC1 + LOC2 + LOC3 + LOC4 + LOC5 + LOC6 + LOC7 + LOC8))
      INSERT INTO (lcBinLine) FROM MEMVAR
      FOR INQT = 1 TO 8
        lcInqt = ALLTRIM(STR(INQT))
        IF !EMPTY(LOC&lcInqt)
          laInputQty[INQT] = laInputQty[INQT] + m.Qty&lcInqt
        ENDIF
      ENDFOR
    ENDIF
  ENDSCAN
ELSE
  SCATTER MEMVAR MEMO
  SELECT(lcBinLine)
  SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laInputQty FOR PO = lnPoNo AND STYLE = lcSlcStyle AND LineNo = lnLineNo

ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpLine))
  GOTO lnRecNo IN (lcTmpLine)
ENDIF

SELECT(lcBinLine)
*T20060817.0014,1 (B132562) TMI [Start] filter only on selected po/style/lineno
lcSvFLt = FILTER()
SET FILTER TO PO = lnPoNo AND STYLE = lcSlcStyle AND LineNo = lnLineNo
LOCATE
*- if there are lines with not entered location , remove them to allow reenter later
DELETE FOR EMPTY(LOC1+LOC2+LOC3+LOC4+LOC5+LOC6+LOC7+LOC8)
*T20060817.0014,1 (B132562) TMI [End  ] 
SET RELATION OFF INTO SCALE
SET RELATION TO 'S'+SCALE INTO SCALE
LOCATE FOR PO = lnPoNo AND STYLE = lcSlcStyle AND LineNo = lnLineNo
= SEEK('S'+&lcTmpLine..Scale,'SCALE')
lcSize1 = SCALE.SZ1
lcSize2 = SCALE.SZ2
lcSize3 = SCALE.SZ3
lcSize4 = SCALE.SZ4
lcSize5 = SCALE.SZ5
lcSize6 = SCALE.SZ6
lcSize7 = SCALE.SZ7
lcSize8 = SCALE.SZ8

FOR I = 1 TO 8
  lcI = ALLTRIM(STR(I))
  laBinLoc[I] = &lcBinLine..LOC&LCI
  lnQty&LCI   = &lcBinLine..Qty&LCI
ENDFOR

lnQty9   = &lcBinLine..TotQty 
lcStyMjr  = SUBSTR(lcSlcStyle,lnStyStPos,lnStyLen)
lcColor   = gfCodDes(SUBSTR(lcSlcStyle,lnClrStPos,lnClrLen),'COLOR')
IF SEEK(lcSlcStyle,'STYLE')
  lcStyDesc = STYLE.DESC1
ENDIF

PUSH KEY
ON KEY LABEL ENTER
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDetTtl) 
ON KEY LABEL TAB DO lplTab 
ON KEY LABEL BACKTAB DO lplBackTab 

lcMjrPct  = gfItemMask('PM')
DO (gcScrDir+gcWinAppl+"\POSHPBN.SPX")

*T20060817.0014,1 (B132562) TMI [Start] restore saved filter
PRIVATE lnSlct
lnSlct = SELECT()
SELECT (lcBinLine)
SET FILTER TO &lcSvFLt
GO TOP
SELECT (lnSlct)
*T20060817.0014,1 (B132562) TMI [End  ] 

IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpLine))
  GOTO lnRecNo IN (lcTmpLine)
ENDIF

ON KEY LABEL TAB
ON KEY LABEL BACKTAB
POP KEY
*-- End of Function lfDispScr.
*!**************************************************************************
*!* Name        : lplTab
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Trap TAB key
*!***************************************************************************
*!* Called from : Binmain.prg -->lfDispScr
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lplTab()
*!***************************************************************************
PROCEDURE lplTab
DO CASE
  CASE WONTOP() = lcDetTtl
    ACTIVATE WINDOW POSHPB3
   _CUROBJ=OBJNUM(laShipQty[1])  
  OTHER
    FOR I = 1 TO 8
      LCI=ALLTRIM(STR(I))
      IF _CUROBJ=OBJNUM(lnQty&LCI)  
        = lfvBnQty(I)
      ENDIF
      IF _CUROBJ=OBJNUM(laBinLoc[I])  
        = lfvRecBin(I)
      ENDIF
    ENDFOR
    IF llMove
      _CUROBJ = _CUROBJ + 1
    ENDIF
ENDCASE
*-- End of Function lplTab.
*!**************************************************************************
*!* Name        : lpLBackTab
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Trap BACKTAB key
*!***************************************************************************
*!* Called from : Binmain.prg -->lfDispScr
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lpLBackTab()
*!***************************************************************************
PROCEDURE lpLBackTab
DO CASE
  CASE WONTOP() = lcDetTtl
    ACTIVATE WINDOW POSHPB3
   _CUROBJ=OBJNUM(PbCancel)  
  OTHER
    FOR I = 1 TO 8
      LCI=ALLTRIM(STR(I))
      IF _CUROBJ=OBJNUM(lnQty&LCI)  
        = lfvBnQty(I)
      ELSE
        llMove = .T.
      ENDIF
    ENDFOR
    IF llMove
      _CUROBJ = _CUROBJ - 1
      llMove = .F.
    ENDIF
ENDCASE
*-- End of Function lpLBackTab.
*!*************************************************************
*!* Name      : lfReadAct
*!* Developer : NADER NABIL (NNA)
*!* Date      : 06/06/2006
*!* Module    : Style Purchase Order (PO)
*!* Purpose   : READ Activate function of invoice screen
*!*************************************************************
*!* Calls From: Poshpbn.Scx
*!*************************************************************
*!* Parameters: None
*!*************************************************************
*!* Returns   :  None.
*!*************************************************************
*!* Example   :  =lfReadAct()
*!*************************************************************
FUNCTION lfReadAct
IF glFromBrow
  =gfStopBrow()
  glFromBrow = .F.
ENDIF
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDetTtl) 
*--End of Function lfReadAct.
*!*************************************************************
*!* Name      : lfReadDAct
*!* Developer : NADER NABIL (NNA)
*!* Date      : 06/06/2006
*!* Module    : Style Purchase Order (PO)
*!* Purpose   : READ Deactivate function of screen Ord100
*!*************************************************************
*!* Calls From: Poshpbn.Scx
*!*************************************************************
*!* Parameters: None
*!*************************************************************
*!* Returns   :  .f.
*!*************************************************************
*!* Example   :  =lfReadDAct()
*!*************************************************************
FUNCTION lfReadDAct
IF WONTOP() = (lcWinTitle)
  *-- Set browsing variable
  glFromBrow  = .T.
ENDIF
ON KEY LABEL TAB DO lplTab 
ON KEY LABEL BACKTAB DO lpLBackTab 
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcDetTtl) 
=lfClearKey()
*--End of Function lfReadDAct.
*!**************************************************************************
*!* Name        : lfClearKey
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Clear the keys
*!***************************************************************************
*!* Called from : Binmain.prg --> lfReadDAct
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfClearKey()
*!***************************************************************************
FUNCTION lfClearKey
ON KEY LABEL ALT+B
ON KEY LABEL CTRL+Q
ON KEY LABEL CTRL+W
ON KEY LABEL CTRL+HOME
ON KEY LABEL CTRL+END
ON KEY LABEL ENTER
*-- End of Function lfClearKey.
*!*************************************************************
*!* Name       : lfBrowse
*!* Developer  : NNA - NADER NABIL ABD-ALMONAM
*!* Date       : 06/06/2006
*!* Module     : Style Purchase Order (PO)
*!* Purpose    : browse lines for the selected P/O
*!*************************************************************
*!* Calls From : Poshpbn.Scx
*!*************************************************************
*!* Parameters : None.
*!*************************************************************
*!* Returns    : None.
*!*************************************************************
*!* Example    : =lfBrowse()
*!*************************************************************
FUNCTION lfBrowse
STORE '' TO lcBrowFlds
STORE 0 TO lnCountRec,lnRecordNo
lnAlias = SELECT()
SELECT (lcBinLine)
lnMarker = RECNO()
lcBrowFlds = [lcMarker=IIF(RECNO() = lnMarker ,'>',' '):1:H=' ':W=.F. :5,]+;
             [cWareCode :R :H='Location' :15,QTY1  :R :H='Qty1' :10 ,QTY2 :R :H='Qty2' :10,]+;
             [QTY3      :R :H='Qty3'       :10,QTY4  :R :H='Qty4' :10 ,QTY5 :R :H='Qty5' :10,]+;
             [QTY6      :R :H='Qty6'       :10,QTY7  :R :H='Qty7' :10 ,QTY8 :R :H='Qty8' :10,]+;
             [TOTQTY    :R :H='TotQty'     :10]

BROWSE FIELDS &lcBrowFlds 				 ;
              FOR PO + STYLE = lnPoNo + lcSlcStyle 	 ;
              AND  lineNo = lnlineno     ;
			  NOAPPEND	  				 ;
			  NOCLEAR     				 ;
			  NODELETE    				 ;
			  NOMENU      				 ;
			  NOWAIT      				 ;
			  SAVE        				 ;
			  VALID :F lfvBrowse() 		 ;
			  WHEN lfwBrowse()			 ;
              TITLE lcDetTtl 			 ;
              WINDOW Poshpb2 IN WINDOW Poshpbn

_CUROBJ = OBJNUM(PbNew)
IF laScrMode[2]
  SHOW GET PbNew      DISABLE 
  SHOW GET PbRemove   DISABLE 
ENDIF
lnRecordNo = RECNO()
COUNT FOR (PO = lnPoNo) AND (STYLE = lcSlcStyle)  AND (lineNo = lnlineno) TO lnCountRec

IF BETWEEN(lnRecNo,1,RECCOUNT(lcBinLine))
  GOTO lnRecNo IN (lcBinLine)
ENDIF
FOR N = 1 TO 8
  LCN = ALLTRIM(STR(N))
  lnBalQty&LCN = laShipQty[N] - laInputQty[N]
  m.QTY&LCN = 0
  lcBin&LCN = &lcBinLine..Loc&LCN
  SHOW GET lcBin&LCN
ENDFOR
m.TOTQTY = 0
m.TranCd = '2'
ACTIVATE WINDOW (lcDetTtl) SAME
SELECT(lnAlias)

*-- End Of lfBrowse.
*!**************************************************************************
*!* Name        : lfvBrowse
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Validate on the Browse in Screen Poshpbn.Scx
*!***************************************************************************
*!* Called from : Binmain.prg -->lfBrowse
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfvBrowse()
*!***************************************************************************
FUNCTION lfvBrowse
IF TYPE('lcDetTtl') = 'C'
  IF WONTOP() # (lcDetTtl)
    = gfStopBrow()
  ENDIF
ENDIF
*-- End of Function lfvBrowse.
*!**************************************************************************
*!* Name        : lfWbrowse
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : When function for Screen's browse [Receive into Multiple Bins Screen]
*!***************************************************************************
*!* Called from : Binmain.prg -->lfBrowse
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : =lfWbrowse()
*!***************************************************************************
FUNCTION lfWbrowse
lnMarker = RECNO()
FOR I = 1 TO 8
  LCI = ALLTRIM(STR(I))
  laBinLoc[I] = &lcBinLine..Loc&LCI
  lnQty&LCI   = &lcBinLine..QTY&LCI
ENDFOR
lnQty9 = &lcBinLine..TotQty
SHOW GETS
SHOW WINDOW (lcDetTtl) REFRESH SAME
*-- End of Function lfWbrowse.
*!**************************************************************************
*!* Name        : lfValidBut
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Validation for New,Remove and Ok buttons
*!***************************************************************************
*!* Called from : Poshpbn.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfValidBut()
*!***************************************************************************
FUNCTION lfValidBut
PARAMETERS lcButton
PRIVATE llGo , lnCountRec , lcOldOrder,lnReturn
STORE 0 TO lnReturn,lnCountRec
llGo = .T.
*T20060817.0014,1 (B132562) TMI [Start] use different temp name since fox recognize only first 8 chars of a file alias
*lcBinLine = lcTmpLine + 'A'
lcBinLine = "_"+SUBSTR(lcTmpLine,2)
SELECT (lcBinLine)
*T20060817.0014,1 (B132562) TMI [End  ] 

lnOldAlias=SELECT(0)
DO CASE
  *--*--*--*--*--*--*--*-- New Button
  CASE lcButton = 'New'
    llNew = .T.
    SELECT(lcBinLine)
    FOR I = 1 TO 8
      LCI = ALLTRIM(STR(I))
      IF lnQty&LCI > 0
        IF EMPTY(laBinLoc[I])
          llGo = .F.
          EXIT
        ENDIF
      ENDIF
      IF lnQty&LCI = 0
        SHOW GET laBinLoc[I] ENABLE
        SHOW GET lnQty&LCI   ENABLE        
      ENDIF
    ENDFOR
    SHOW GET PbRemove ENABLE
    IF llGo 
      APPEND BLANK
      GATHER MEMVAR MEMO
      STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8,lnQty9
      STORE '' TO laBinLoc
      _CUROBJ = OBJNUM(laBinLoc[1])
    ELSE
      =gfModalGen("QRM00000B00000","DIALOG",.F.,.F.,'You have to assign bins before adding new record')
      FOR I = 1 TO 8
        IF EMPTY(laBinLoc[I]) AND laShipQty[I] > 0
          _CUROBJ = OBJNUM(laBinLoc[I])      
          EXIT
        ENDIF
      ENDFOR
      RETURN
    ENDIF
  *--*--*--*--*--*--*--*-- Remove Button
  CASE lcButton = 'Remove'
    SELECT(lcBinLine)
    FOR I = 1 TO 8
      LCI = ALLTRIM(STR(I))
      laInputQty[I] = laInputQty[I] - &lcBinLine..Qty&LCI
      lnBalQty&LCI  = laShipQty[I]  - laInputQty[I]
    ENDFOR
    *T20060817.0014   TMI [Start] update the laInputQty[9] element 
    laInputQty[9] = laInputQty[1]+laInputQty[2]+laInputQty[3]+laInputQty[4]+laInputQty[5]+laInputQty[6]+laInputQty[7]+laInputQty[8]
    *T20060817.0014   TMI [End  ] 
    DELETE
    LOCATE
    COUNT FOR !DELETED() TO lnCountRec 
    GO BOTTOM
    IF lnCountRec = 0
      SHOW GET PbRemove DISABLE
    ENDIF
    = lfWbrowse()
    SHOW GETS
  *--*--*--*--*--*--*--*-- Ok Button
  CASE lcButton = 'Ok'
    IF laShipQty[1]+laShipQty[2]+laShipQty[3]+laShipQty[4]+laShipQty[5]+laShipQty[6]+laShipQty[7]+laShipQty[8] > laInputQty[9]
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'You have to receive all open quantities to bins before save')
      RETURN
    ENDIF
    lcOldOrder = ORDER(lcTmpLine)
    SET ORDER TO TAG POSLN IN (lcTmpLine)
    SELECT (lcBinLine)
    LOCATE
    SELECT(lcTmpline)
    lnTmplRec = RECNO()
    DELETE ALL FOR cstytype+po+style+STR(lineno,6)+trancd = 'P' + lnPoNo + lcSlcStyle+STR(lnLineNo,6)+'2'
    SELECT (lcBinLine)
    llOk = .T.
    *T20060817.0014,1 (B132562) TMI [Start] loop only for the selected po/style/lineno
    *SCAN FOR TOTQTY > 0
    SCAN FOR TOTQTY > 0 ;
         .AND. cstytype+po+style+STR(lineno,6) = 'P'+lnPoNo+lcSlcStyle+STR(lnLineNo,6)          
      *T20060817.0014,1 (B132562) TMI [End  ] 
      FOR I = 1 TO 8
        LCI = ALLTRIM(STR(I))
        IF QTY&LCI > 0 AND EMPTY(LOC&LCI)
          llOk = .F.
          lnReturn = RECNO()
          EXIT  
        ENDIF
      ENDFOR
    ENDSCAN
    IF !llOk
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,"You have to assign all quantities to bins location before saving .")
      IF BETWEEN(lnReturn,1,RECCOUNT(lcBinLine))
        GOTO lnReturn IN (lcBinLine)
        lnMarker = lnReturn
      ENDIF
      FOR I = 1 TO 8
        IF EMPTY(laBinLoc[I]) AND laShipQty[I] > 0
          _CUROBJ = OBJNUM(laBinLoc[I])      
          EXIT
        ENDIF
      ENDFOR
      RETURN
    ENDIF
    
    SCAN FOR TOTQTY > 0 AND PO = lnPoNo AND STYLE = lcSlcStyle AND  LineNo = m.LineNo
      SCATTER MEMVAR MEMO
      IF !SEEK('P'+lnPoNo+m.Style+STR(m.LineNo,6)+'2',lcTmpLine)
        INSERT INTO (lcTmpLine) FROM MEMVAR
      ELSE
        SELECT (lcTmpLine)
        FOR I = 1 TO 8
          LCI = ALLTRIM(STR(I))
          REPLACE QTY&LCI	WITH QTY&LCI + m.Qty&LCI
        ENDFOR
        REPLACE TOTQTY	    WITH TOTQTY + m.TOTQTY
      ENDIF
    ENDSCAN
    IF SEEK('P'+lnPoNo+lcSlcStyle+STR(lnLineNo,6)+'1',lcTmpLine)
      SELECT(lCTmpLine)
      REPLACE TOTSTK WITH IIF(lcAuto='M' AND lcPType<>'S',TOTSTK + laInputQty[9],laInputQty[9])  ,;
              TOTBAL WITH lnBalQty9
    ENDIF    
    SELECT(lcBinLine)
    LOCATE
    lnTotalStk = 0
    SUM ALL TOTQTY TO lnTotalStk
    lnTotStk = lnTotalStk
    IF BETWEEN(lnRecNo,1,RECCOUNT(lcTmpLine))
      GOTO lnTmplRec IN (lcTmpLine)
    ENDIF
    SET ORDER TO TAG &lcOldOrder IN (lcTmpLine)
    CLEAR READ
  *--*--*--*--*--*--*--*-- Cancel Button
  CASE lcButton = 'Cancel'
    COUNT FOR STYLE = EVAL(lcTmpLine+'.STYLE') TO lnCountRec 
    IF lnCountRec > 0
      IF gfModalGen("QRM00031B00006","DIALOG") = 2
        RETURN
      ELSE
        SELECT(lcBinLine)
        LOCATE
        DELETE ALL FOR STYLE=EVAL(lcTmpLine+'.STYLE')
        CLEAR READ
      ENDIF
    ELSE
      CLEAR READ
    ENDIF
ENDCASE
SHOW GETS
lnMarker = RECNO()
SHOW WINDOW (lcDetTtl) REFRESH SAME
SELECT(lnOldAlias)

*-- End of Function lfValidBut.
*!**************************************************************************
*!* Name        : lfShipQty
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Validate the ship Qty fields
*!***************************************************************************
*!* Called from : Poshpbn.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfShipQty()
*!***************************************************************************
FUNCTION lfShipQty
PARAMETERS lnFldNo
lcFldNo = ALLTRIM(STR(lnFldNo))
lnBalQty&lcFldNo = laShipQty[lnFldNo] - laInputQty[lnFldNo]
SHOW GETS
*-- End of Function lfShipQty.
*!**************************************************************************
*!* Name        : lfBnOldVal
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Save old value for Receive into Multiple Bins Screen
*!***************************************************************************
*!* Called from : Poshpbn.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfBnOldVal()
*!***************************************************************************
FUNCTION lfBnOldVal
lcOldValue = EVALUATE(SYS(18))
*-- End of Function lfBnOldVal.
*!**************************************************************************
*!* Name        : lfvBnQty
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Valid on the Received Qty per Bin
*!***************************************************************************
*!* Called from : Poshpbn.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfvBnQty()
*!***************************************************************************
FUNCTION lfvBnQty
PARAMETERS lnFldNo
lcFldNo = ALLTRIM(STR(lnFldNo))
laInputQty[lnFldNo] =  MAX(laInputQty[lnFldNo] - lcOldValue,0)
IF lnQty&lcFldNo > (laShipQty[lnFldNo]-laInputQty[lnFldNo])
  lcMsg = "Bin quantity is greater than the remaining quantity "
  lcMsg = lcMsg + IIF((laShipQty[lnFldNo] - laInputQty[lnFldNo])>0, ", you can only add ",'')
  lcMsg = lcMsg + ALLTRIM(STR(laShipQty[lnFldNo] - laInputQty[lnFldNo])) 
  lcMsg = lcMsg + " , Do you want to increase ship quantity to over receive ?"
  IF gfModalGen('INM00000B34001',.F.,.F.,.F.,lcMsg) = 2
    lnQty&lcFldNo = IIF(lcOldValue=0,0,lcOldValue)
    laInputQty[lnFldNo] = laInputQty[lnFldNo] + lnQty&lcFldNo
    llMove = .F.
    _CUROBJ = OBJNUM(lnQty&lcFldNo)
    RETURN
  ELSE
    laShipQty[lnFldNo] = laShipQty[lnFldNo] + IIF(lcOldValue = 0 AND lnQty&lcFldNo > laShipQty[lnFldNo],;
                         (lnQty&lcFldNo - laShipQty[lnFldNo]), (lnQty&lcFldNo - lcOldValue))
  ENDIF
ENDIF
llMove = .T.
SELECT(lcBinLine)
lnTmpRec  = RECNO()
lnQty9              = (lnQty9 -lcOldValue) + lnQty&lcFldNo
REPLACE &lcBinLine..Qty&lcFldNo WITH lnQty&lcFldNo	,;
        &lcBinLine..TotQty      WITH lnQty9
GO BOTTOM
GO TOP
SUM QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY TO ARRAY laInputQty FOR PO=lnPoNo AND STYLE = lcSlcStyle AND LineNo =lnLineNo

IF BETWEEN(lnTmpRec,1,RECCOUNT(lcBinLine))
  GOTO lnTmpRec IN (lcBinLine)
ENDIF

lnBalQty&lcFldNo = laShipQty[lnFldNo] - laInputQty[lnFldNo]
SHOW GETS
SHOW WINDOW (lcDetTtl) REFRESH SAME
*-- End of Function lfvBnQty.
*!**************************************************************************
*!* Name        : lfvRecBin
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Bin Locations valid for Receive into Multiple Bins Screen
*!***************************************************************************
*!* Called from : POSHPBN.SPR
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     :  = lfvRecBin()
*!***************************************************************************
FUNCTION lfvRecBin
PARAMETERS lnFldNo
PRIVATE lcOldOrder,lcOldBnOrd,lcTmpLnOrd
lcFldNo = ALLTRIM(STR(lnFldNo))
lnAlias = SELECT()
lcOldOrder = ORDER('WHSLOC')
SET ORDER TO TAG WHSLOC IN WHSLOC
lcTmpLnOrd = ORDER(lcTmpLine)
SET ORDER TO TAG TmpLine1 IN (lcTmpLine)
IF llbrowse OR (!EMPTY(laBinLoc[lnFldNo]) AND !SEEK(lcWareCode+laBinLoc[lnFldNo],'WHSLOC')) OR;
   '?' $ laBinLoc[lnFldNo]
  llbrowse =.F. 
  SELECT WHSLOC
  lcOldBnOrd = ORDER()
  SET ORDER TO TAG WHSBIN
  lcBrFields = "cWareCode :R:h='Warehouse',clocation :R :H='Bin',cBinClass :R :H='Bin Class',cFlatHang :R :H='Flat or Hang'"
  lcForExpr = " cWareCode = lcWareCode AND Style = SPACE(19)"
  lcBrTitle = "Available Bins Location"
  lcSelBin  = IIF(ARIABROW([FOR &lcForExpr],lcBrTitle,5,15,25,55,'','','cLocation','laBrowArr'),laBrowArr[1],'')
  laBinLoc[lnFldNo] = IIF(!EMPTY(lcSelBin),ALLTRIM(lcSelBin),lcOldVAlue)
  SHOW GET laBinLoc[lnFldNo]
  SET ORDER TO TAG &lcOldBnOrd
ENDIF
IF EMPTY(laBinLoc[lnFldNo])
  llMove = .T.
  RETURN
ENDIF
IF EMPTY(laBinLoc[lnFldNo]) 
  SELECT (lcTmpLine)
  REPLACE &lcBinLine..Loc&lcFldNo WITH laBinLoc[lnFldNo]
ENDIF
IF !EMPTY(laBinLoc[lnFldNo]) 
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc
  IF !SEEK(lcWareCode+laBinLoc[lnFldNo],'WHSLOC')
    SET ORDER TO TAG WhsLocSt IN WHSLOC 
    =gfModalGen('INM00000B00000','','','',ALLTRIM(laBinLoc[lnFldNo]) + ' is not a valid location')
    _CUROBJ = OBJNUM(laBinLoc[lnFldNo])
    laBinLoc[lnFldNo] = ''
    RETURN
  ELSE
    =SEEK(lcWareCode+laBinLoc[lnFldNo],'WHSLOC')
    IF !(WhsLoc.cFlatHang = style.cflathang)
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot issue the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      laBinLoc[lnFldNo] = ''
      RETURN
    ENDIF
    llClass = .F.
    IF (WhsLoc.cBinClass == style.cPrimClss&lcFldNo)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cSecClss&lcFldNo)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cRemClss&lcFldNo)
      llClass = .T.
    ENDIF
    IF !llClass
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot Issue the style to a bin of a different class.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      laBinLoc[lnFldNo] = ''
      RETURN
    ENDIF
    SELECT (lcBinLine)
    REPLACE &lcBinLine..Loc&lcFldNo WITH laBinLoc[lnFldNo]
  ENDIF
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt
ENDIF
llMove = .T.
SET ORDER TO TAG &lcOldOrder IN WHSLOC
SELECT(lnAlias)
*-- End of Function lfvRecBin.
*!*************************************************************************
*!* Name        : lfSavMltBn
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Save Multiple Bins Location Qty. to whbnloc file
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfSavMltBn()
*!***************************************************************************
FUNCTION lfSavMltBn
PRIVATE lnAlias
lnAlias = SELECT(0)
*T20060817.0014,1 (B132562) TMI [Start] use different temp name since fox recognize only first 8 chars of a file alias
*lcBinLine = lcTmpLine + 'A'
lcBinLine = "_"+SUBSTR(lcTmpLine,2)
*T20060817.0014,1 (B132562) TMI [End  ] 
IF !lfOpnFiles("STYINVJL,BININVJL,WHBINLOC,WHSLOC","STYINVJL,STYINVJL,WHBINLOC,WHSLOC","")
  RETURN
ENDIF
SELECT (lcBinLine)
LOCATE
SCAN 
  FOR R = 1 TO 8
    Lcr = ALLT(STR(R,2))
    m.Loc&Lcr = &lcBinLine..Loc&Lcr
    IF !EMPTY(m.loc&lcR) AND &lcBinLine..QTY&LCR > 0
      IF !SEEK(&lcBinLine..cWareCode+m.loc&LCR+&lcBinLine..style,'WHBINLOC')
        SET ORDER TO TAG WhsLoc IN  WHSLOC
        IF SEEK(&lcBinLine..cWareCode+m.loc&LCR,'WHSLOC')
          m.cbinclass = WHSLOC.cbinclass
          m.cBlkPck   = WHSLOC.cBlkPck
          m.cSection  = WHSLOC.cSection
        ENDIF
        SET ORDER TO TAG WhsLocSt IN  WHSLOC
        SELECT WHBINLOC
        APPEND BLANK
        REPLACE Style        WITH  &lcBinLine..style     ,;
  		        cWareCode    WITH  &lcBinLine..cWareCode ,;
  		        clocation    WITH  m.loc&LCR             ,;
  		        Qty&LCR      WITH  &lcBinLine..Qty&LCR   ,;
  		        TOTQTY       WITH  &lcBinLine..Qty&LCR   ,;
  		        cbinclass    WITH  m.cbinclass           ,;
                cBlkPck      WITH  m.cBlkPck             ,;
                cSection     WITH  m.cSection
        =gfAdd_Info('WHBINLOC')
      ELSE
        REPLACE  WHBINLOC.Qty&LCR WITH WHBINLOC.Qty&LCR + &lcBinLine..Qty&LCR,;
                 WHBINLOC.TOTQTY  WITH WHBINLOC.TOTQTY  + &lcBinLine..Qty&LCR
      ENDIF     
      lcOldOrder = ORDER('WHSLOC')
      SET ORDER TO TAG WhsLoc IN  WHSLOC
      IF !SEEK(&lcBinLine..cWareCode+m.loc&LCR+&lcBinLine..style,'WHSLOC')
        =SEEK(EVAL(lcBinLine+'.cWareCode')+m.loc&LCR,'WHSLOC')
        SCATT MEMVAR MEMO
        m.STYLE     = &lcBinLine..style
        m.clocation = m.loc&LCR
        INSERT INTO WHSLOC FROM MEMVAR 
      ENDIF
      SET ORDER TO TAG &lcOldOrder IN  WHSLOC
    ENDIF  
  ENDFOR
ENDSCAN
SELECT(lnAlias)
*-- End of Function lfSavMltBn.
*!***************************************************************************
*!* Name        : lfDLVLDWRH
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Valid Warehouse with bin location
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLVLDWRH()
*!***************************************************************************
FUNCTION lfDLVLDWRH

IF gfGetMemVar('M_DLUSEBIN')
  STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE '' TO lcBinLoc
  lcIssWare = laStyWare[lnIssWare,2]
  =SEEK(m.Item+lcIssWare+space(10),'StyDye')
  lnIssCost = IIF(laSetups[10,2]='A',StyDye.Ave_Cost,Style.TotCost)
  =lfRefresh('PODLSISS')
  SHOW GETS 
ELSE
  lcIssWare = laStyWare[lnIssWare,2]
  =SEEK(m.Item+lcIssWare+space(10),'StyDye')
  lnIssCost = IIF(laSetups[10,2]='A',StyDye.Ave_Cost,Style.TotCost)
  =lfRefresh('MFSTYISS')
  SHOW GETS WINDOW 'MFSTYISS' ONLY
ENDIF
*-- End of Function lfDLVLDWRH.
*!***************************************************************************
*!* Name        : lfLDFNPORT
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Call Return Screen with modification of Bin Location
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfLDFNPORT()
*!***************************************************************************
FUNCTION lfLDFNPORT  
IF gfGetMemVar('M_DLUSEBIN')
  lnAlias = SELECT()
  IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSLOC","")
    RETURN
  ENDIF
  PRIVATE m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE '' TO lcBinLoc
  DO (gcScrDir+"PODLSISS.SPX") WITH .F.
  SELECT(lnAlias)
ELSE
  DO (gcScrDir+"MFSTYISS.SPX") WITH .F.
ENDIF
*-- End of Function lfLDFNPORT  .
*!***************************************************************************
*!* Name        : lfLDFNPOIS  
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Call Issue Screen with modification of Bin Location
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfLDFNPOIS()
*!***************************************************************************
FUNCTION lfLDFNPOIS  
IF gfGetMemVar('M_DLUSEBIN') 
  lnAlias = SELECT()
  IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSLOC","")
    RETURN
  ENDIF
  PRIVATE m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  STORE '' TO lcBinLoc
  DO (gcScrDir+"PODLSISS.SPX") WITH .T.
  SELECT(lnAlias)
ELSE
  DO (gcScrDir+"MFSTYISS.SPX") WITH .T.
ENDIF
*-- End of Function lfLDFNPOIS.
*!***************************************************************************
*!* Name        : lFVPOBNLC
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Validate bin location
*!***************************************************************************
*!* Called from : PODLSISS.SCX -->MFCSSH.PRG -->Issue/return in costing sheet
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lFVPOBNLC()
*!***************************************************************************
FUNCTION lFVPOBNLC
PRIVATE llFromSeek
STORE .F.TO llFromSeek
IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSBIN","")
  RETURN
ENDIF
SELECT WHSLOC
IF EMPTY(lcBinLoc) AND !llbrowse
  SET ORDER TO TAG WHSLOCST IN  WHSLOC
  RETURN
ENDIF
IF llbrowse OR (!EMPTY(lcBinLoc) AND !SEEK(lcBinLoc+lcIssWare))
  IF llIssue
    llbrowse =.F. 
    lnScaleCnt = Scale.cnt
    lcBrFields = "clocation  :H='Bin',cBinClass  :H='Bin Class',"
    FOR lnI=1 TO lnScaleCnt
      lcI=STR(lnI,1)
      IF !EMPTY(Scale.Sz&lcI)
        lcBrFields = lcBrFields + "Qty"+lcI+" :H=PADL(Scale.Sz"+lcI+",5) :P='9999999',"
      ENDIF
    ENDFOR
    lcBrFields = lcBrFields + "TotQty :H='Total ' :P='999999999',"
    FOR lnI=1 TO lnScaleCnt
      lcI=STR(lnI,1)
      IF !EMPTY(Scale.Sz&lcI)
        lcBrFields = lcBrFields + "Alo"+lcI+" :H='Alo '+PADL(Scale.Sz"+lcI+",5)  :P='9999999',"
      ENDIF
    ENDFOR
    lcBrFields = lcBrFields + "TotAlo :H='Total Allocated ' :P='999999999'"
    DIMENSION laTempData[3]
    STORE '' TO laTempData
    =SEEK(ALLTRIM(lcBinLoc))
    SELECT WHBINLOC 
    lcForExpr = " cWareCode = lcIssWare  AND (TotQty-TotAlo>0) AND STYLE = m.Item"
    =gfBrows([FOR &lcForExpr],'cLocation,cBinClass','laTempData','WHBINLOC')
    lcBinLoc=laTempData[1]
  ELSE
    llbrowse =.F. 
    lcBrFields = "clocation  :H='Bin',cBinClass  :H='Bin Class'"
    DIME laTempData[3]
    STORE '' TO laTempData
    =SEEK(ALLTRIM(lcBinLoc))
    lcForExpr = " cWareCode = lcIssWare  AND EMPTY(Style)"
    =gfBrows([FOR &lcForExpr],'cLocation,cBinClass,cFlatHang','laTempData','WhsLoc')
    lcBinLoc=laTempData[1]
  ENDIF
  SELECT WHBINLOC
  IF SEEK(lcIssWare+lcBinLoc+m.Item)
    m.Bin_Qty1 = Qty1 - Alo1
    m.Bin_Qty2 = Qty2 - Alo2
    m.Bin_Qty3 = Qty3 - Alo3
    m.Bin_Qty4 = Qty4 - Alo4
    m.Bin_Qty5 = Qty5 - Alo5
    m.Bin_Qty6 = Qty6 - Alo6
    m.Bin_Qty7 = Qty7 - Alo7
    m.Bin_Qty8 = Qty8 - Alo8 
  ELSE
    STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8
  ENDIF
  SHOW GETS  
ELSE
  SELECT WHBINLOC
  IF SEEK(lcIssWare+lcBinLoc+m.Item)
    m.Bin_Qty1 = Qty1 - Alo1
    m.Bin_Qty2 = Qty2 - Alo2
    m.Bin_Qty3 = Qty3 - Alo3
    m.Bin_Qty4 = Qty4 - Alo4
    m.Bin_Qty5 = Qty5 - Alo5
    m.Bin_Qty6 = Qty6 - Alo6
    m.Bin_Qty7 = Qty7 - Alo7
    m.Bin_Qty8 = Qty8 - Alo8 
  ELSE
    STORE 0 TO m.Bin_Qty1,m.Bin_Qty2,m.Bin_Qty3,m.Bin_Qty4,m.Bin_Qty5,m.Bin_Qty6,m.Bin_Qty7,m.Bin_Qty8 
  ENDIF
  SHOW GETS  
ENDIF
SET ORDER TO TAG WHSLOCST IN  WHSLOC
*-- End of Function lFVPOBNLC.
*!***************************************************************************
*!* Name        : lfDLCHKBIN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : validate bin location 
*!***************************************************************************
*!* Called from : MFCSSH.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLCHKBIN()
*!***************************************************************************
FUNCTION lfDLCHKBIN
PRIVATE llmessag
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
llmessag = .F.
IF EMPTY(lcBinLoc)
  lcMsg2 = 'You have to select a bin location'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llmessag = .T.
ENDIF
RETURN llmessag
*-- End of Function lfDLCHKBIN.
*!***************************************************************************
*!* Name        : lfADDFILDS
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Add Custom Fields for temp file 
*!***************************************************************************
*!* Called from : POSTREC.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfADDFILDS()
*!***************************************************************************
FUNCTION lfADDFILDS
IF gfGetMemVar('M_DLUSEBIN')
  lnFStru = ALEN(laFStru,1)
  DIMENSION laFStru[lnFStru+8,4]
  For I = 1 to 8
    LCI = ALLTRIM(STR(I))
    laFStru[lnFStru+I,1] = 'Loc&LCI'
    laFStru[lnFStru+I,2] = 'C'
    laFStru[lnFStru+I,3] = 10
    laFStru[lnFStru+I,4] = 0
  ENDFOR
ENDIF
*-- End of Function lfADDFILDS.
*!***************************************************************************
*!* Name        : lfDLRCVQTY
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : get received data screen for Bins logic
*!***************************************************************************
*!* Called from : POSTREC.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLRCVQTY()
*!***************************************************************************
FUNCTION lfDLRCVQTY
IF gfGetMemVar('M_DLUSEBIN')
  lnAlias = SELECT()
  IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLOC,WHSLOC","")
    RETURN
  ENDIF
  
  *--create array for locations
  DIME laBinLoc[8]
  STORE "" TO laBinLoc
  FOR lnI = 1 TO 8
    lcI = ALLT(STR(lnI,2))
    IF !EMPTY(&lcTmpLine..loc&lcI)
      laBinLoc[lnI] = &lcTmpLine..loc&lcI
    ENDIF
  ENDFOR
  DO (gcScrDir+gcWinAppl+"\PODLRCV.SPX")
  *T20060817.0014   TMI [Start] save the labinloc array in a mem file
  SAVE TO (gcWorkDir+lcTmpLine+'.mem') ALL LIKE laBinLoc*  
  *T20060817.0014   TMI [End  ] 
  SELECT(lnAlias)
ELSE
  DO (gcScrDir+"MFRCVQ.SPX")
ENDIF
*-- End of Function lfDLRCVQTY.

*:**************************************************************************
*:* Name        : lfvBnLoc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/17/2006
*:* Purpose     : valid function for selecting bins in PODLRCV screen
*:***************************************************************************
*:* Called from : PODLRCV.SCX screen
*:***************************************************************************
FUNCTION lfvBnLoc
PARAMETERS lnCnxt

PRIVATE lnSlct,lcOrder,laTempData
lnSlct = SELECT()
lcOrder = ORDER('WHSLOC')
IF EMPTY(laBinLoc[lnCnxt]) 
  RETURN
ENDIF
SELECT WhsLoc
SET ORDER TO TAG WhsBin
IF !SEEK(laBinLoc[lnCnxt]+lcWareCode,'WhsLoc')
  lcBrFields = "clocation :H='Bin',cBinClass :H='Bin Class',cFlatHang :H='Flat or Hang'"
  lcForExpr = " cWareCode = lcWareCode  AND EMPTY(Style)"
  DIMENSION laTempData[1]
  STORE '' TO laTempData
  =SEEK(laBinLoc[lnCnxt])
  =gfBrows([FOR &lcForExpr],'cLocation,cBinClass,cFlatHang','laTempData','WhsLoc')
  laBinLoc[lnCnxt] = laTempData[1]
ENDIF

SELECT WHSLOC
SET ORDER TO &lcOrder 
SELECT (lnSlct)

*-- end of lfvBnLoc.
*!***************************************************************************
*!* Name        : lfDLUPDQTY
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : Update BinInvJL & WHBINLOC file
*!***************************************************************************
*!* Called from : MFCSSH.prg 1 - from manual Issue (TYPE('lcBinLoc') <> 'U')
*!*             :            2 - from Automatic Issue (TYPE('lcBinLoc') = 'U')
*!*             :            3 - from Delete Po Cost sheet llFromDel
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLUPDQTY()
*!***************************************************************************
FUNCTION lfDLUPDQTY
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
PRIVATE lcOldOrder
DIMENSION laOldQty[9]
STORE 0 TO laOldQty
IF !lfOpnFiles("WHBINLOC,BININVJL","WHBINLOC,STYINVJL","")
  RETURN
ENDIF
SELECT STYINVJL
SCATT MEMVAR MEMO
lcGlSession = STYINVJL.csession
IF TYPE('lcBinLoc') = 'U' AND !llFromDel
  m.Item = &lcIssLtFile..Item
  SELECT WHBINLOC
  lcOrdOrdr = ORDER()
  SET ORDER TO Whbinlst
  IF SEEK(m.Item + laData[32])
    SELECT STYINVJL
    =SEEK(m.Item + laData[32] + lcGlSession)
    SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) = m.Item + laData[32] + lcGlSession     
      SCATT MEMVAR MEMO
      SELECT WHBINLOC    
      =SEEK(m.Item + laData[32])
      SCAN REST WHILE style+cwarecode+clocation = m.Item + laData[32]
        IF m.nTOtStk <> 0
          STORE 0 TO laOldQty
          FOR lnI  = 1 TO 8
            lcI = ALLT(STR(lnI,2))
            IF (m.nStk&lcI = 0 ) OR Qty&lcI - ALO&lcI + m.nStk&lcI > 0
              laOldQty[lnI]  = m.nStk&lcI
              laOldQty[9]    = laOldQty[9] + m.nStk&lcI
              m.nTOtStk      = m.nTOtStk - m.nStk&lcI
              m.nStk&lcI     = 0
            ELSE
              laOldQty[lnI] = -1 * Qty&lcI
              laOldQty[9]   = laOldQty[9] -1 * Qty&lcI
              m.nStk&lcI    = m.nStk&lcI + Qty&lcI
              m.nTOtStk     = m.nTOtStk + Qty&lcI
            ENDIF
          ENDFOR
          IF laOldQty[9] <>0 
            REPLACE QTY1   WITH QTY1   + laOldQty[1],;
                    QTY2   WITH QTY2   + laOldQty[2],;  
                    QTY3   WITH QTY3   + laOldQty[3],;  
                    QTY4   WITH QTY4   + laOldQty[4],;  
                    QTY5   WITH QTY5   + laOldQty[5],;  
                    QTY6   WITH QTY6   + laOldQty[6],;  
                    QTY7   WITH QTY7   + laOldQty[7],;  
                    QTY8   WITH QTY8   + laOldQty[8],;
                    TOTQTY WITH TOTQTY + laOldQty[9]
            m.clocation = clocation
            SELECT BININVJL
            APPEND BLANK
            GATHER MEMVAR MEMO
            REPLACE NSTK1   WITH laOldQty[1],;
                    NSTK2   WITH laOldQty[2],;  
                    NSTK3   WITH laOldQty[3],;  
                    NSTK4   WITH laOldQty[4],;  
                    NSTK5   WITH laOldQty[5],;  
                    NSTK6   WITH laOldQty[6],;  
                    NSTK7   WITH laOldQty[7],;  
                    NSTK8   WITH laOldQty[8],;
                    NTOTSTK WITH laOldQty[9],;
                    nstkval WITH laOldQty[9] * nCost
           ENDIF           
         ELSE
           EXIT
        ENDIF
      ENDSCAN    
    ENDSCAN
  ENDIF  
  SELECT WHBINLOC
  SET ORDER TO &lcOrdOrdr 
ELSE
  IF llFromDel
    IF !USED('WHSLOC') 
      =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    DIME laTempData[3]
    Store '' TO lcBinLoc , laTempData
    DO (gcScrDir+gcWinAppl+"\POdlbnSC.Spx")
    IF EMPTY(lcBinLoc)
      DO WHILE EMPTY(lcBinLoc)
        lcMsg2 = 'You have to assign bin location before saving .'
        =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
        DO (gcScrDir+gcWinAppl+"\POdlbnSC.Spx")
      ENDDO  
    ENDIF
    m.Iss_Qty1 = laIssued[1] 
    m.Iss_Qty2 = laIssued[2] 
    m.Iss_Qty3 = laIssued[3] 
    m.Iss_Qty4 = laIssued[4] 
    m.Iss_Qty5 = laIssued[5] 
    m.Iss_Qty6 = laIssued[6] 
    m.Iss_Qty7 = laIssued[7] 
    m.Iss_Qty8 = laIssued[8] 
    m.Item = STYINVJL.Style
  ENDIF
  SELECT STYINVJL
  =SEEK(m.Item + m.cWareCode + lcGlSession)
  SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6) =;
                  m.Item + m.cWareCode + lcGlSession     
    SCATTER MEMVAR MEMO              
    m.clocation = lcBinLoc
    SELECT BININVJL
    APPEND BLANK
    GATHER MEMVAR MEMO
    =gfAdd_Info('BININVJL')
    
  ENDSCAN
  SELECT WHBINLOC
  IF SEEK(m.cWareCode+lcBinLoc+m.Item)
    IF cirtype = 'R'
      REPLACE QTY1 WITH QTY1 + m.Iss_Qty1,;
              QTY2 WITH QTY2 + m.Iss_Qty2,;  
              QTY3 WITH QTY3 + m.Iss_Qty3,;  
              QTY4 WITH QTY4 + m.Iss_Qty4,;  
              QTY5 WITH QTY5 + m.Iss_Qty5,;  
              QTY6 WITH QTY6 + m.Iss_Qty6,;  
              QTY7 WITH QTY7 + m.Iss_Qty7,;  
              QTY8 WITH QTY8 + m.Iss_Qty8
      REPLACE TOTQTY WITH TOTQTY +(m.Iss_Qty1+m.Iss_Qty2+m.Iss_Qty3+m.Iss_Qty4+m.Iss_Qty5+m.Iss_Qty6+m.Iss_Qty7+m.Iss_Qty8)
    ELSE
      REPLACE QTY1 WITH QTY1 - m.Iss_Qty1,;
              QTY2 WITH QTY2 - m.Iss_Qty2,;  
              QTY3 WITH QTY3 - m.Iss_Qty3,;  
              QTY4 WITH QTY4 - m.Iss_Qty4,;  
              QTY5 WITH QTY5 - m.Iss_Qty5,;  
              QTY6 WITH QTY6 - m.Iss_Qty6,;  
              QTY7 WITH QTY7 - m.Iss_Qty7,;  
              QTY8 WITH QTY8 - m.Iss_Qty8
      REPLACE TOTQTY WITH TOTQTY -(m.Iss_Qty1+m.Iss_Qty2+m.Iss_Qty3+m.Iss_Qty4+m.Iss_Qty5+m.Iss_Qty6+m.Iss_Qty7+m.Iss_Qty8)
      IF TOTQTY = 0
        DELETE       
      ENDIF
    ENDIF    
  ELSE
    IF cirtype = 'R'
      m.clocation = lcBinLoc
      m.Style     = m.Item
      lcOldOrder = ORDER('WHSLOC')
      SET ORDER TO TAG WHSLOC IN WHSLOC
      =SEEK(m.cWareCode+m.clocation,'WHSLOC')
      m.cbinclass = WHSLOC.cbinclass
      APPEND BLANK
      GATHER MEMVAR MEMO
      REPLACE QTY1 WITH QTY1 + m.Iss_Qty1,;
              QTY2 WITH QTY2 + m.Iss_Qty2,;  
              QTY3 WITH QTY3 + m.Iss_Qty3,;  
              QTY4 WITH QTY4 + m.Iss_Qty4,;  
              QTY5 WITH QTY5 + m.Iss_Qty5,;  
              QTY6 WITH QTY6 + m.Iss_Qty6,;  
              QTY7 WITH QTY7 + m.Iss_Qty7,;  
              QTY8 WITH QTY8 + m.Iss_Qty8
      REPLACE TOTQTY WITH TOTQTY +(m.Iss_Qty1+m.Iss_Qty2+m.Iss_Qty3+m.Iss_Qty4+m.Iss_Qty5+m.Iss_Qty6+m.Iss_Qty7+m.Iss_Qty8)
      =gfAdd_Info('WHBINLOC')
      SELECT WHSLOC
      IF !SEEK(m.cWareCode+m.clocation+m.Style)
        APPEND BLANK
        GATHER MEMVAR MEMO        
        =gfAdd_Info('WHSLOC')
      ENDIF
      SET ORDER TO TAG &lcOldOrder
    ENDIF  
  ENDIF
ENDIF
*-- End of Function lfDLUPDQTY.
*!***************************************************************************
*!* Name        : lfvPOScbn
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : validate bin location in Po receiving screen
*!***************************************************************************
*!* Called from : PODLBNSC.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvPOScbn()
*!***************************************************************************
FUNCTION lfvPOScbn
PRIVATE llFromSeek
STORE .F.TO llFromSeek

SELECT WhsLoc
IF EMPTY(lcBinLoc) AND !llbrowse
  RETURN
ENDIF
SET ORDER TO TAG WhsBin IN  WHSLOC
IF llbrowse OR (!EMPTY(lcBinLoc) AND !SEEK(lcBinLoc+lcWareCode))
  llbrowse =.F. 
  lcBrFields = "clocation :H='Bin',cBinClass :H='Bin Class',cFlatHang :H='Flat or Hang'"
                 
  lcForExpr = " cWareCode = lcWareCode  AND EMPTY(Style)"
  STORE '' TO laTempData
  =SEEK(ALLTRIM(lcBinLoc))
  =gfBrows([FOR &lcForExpr],'cLocation,cBinClass,cFlatHang','laTempData','WhsLoc')
  lcBinLoc=laTempData[1]
ELSE
  laTempData[1] = lcBinLoc
  laTempData[2] = WhsLoc.cbinclass
  laTempData[3] = WhsLoc.cflathang
  STORE .T.TO llFromSeek
ENDIF
SET ORDER TO TAG WhsLocSt IN  WHSLOC
IF llFromSeek
 =lfvPOScOk()
ENDIF
*-- End of Function lfvPOScbn.
*!***************************************************************************
*!* Name        : lfvPOScOk
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Style Purchase Order (PO)
*!* Purpose     : validate bin location in Po receiving screen
*!***************************************************************************
*!* Called from : PODLBNSC.SCX
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvPOScOk()
*!***************************************************************************
FUNCTION lfvPOScOk
IF EMPTY(lcBinLoc)
  lcMsg2 = 'You have to assign Bin location'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  RETURN
ENDIF
CLEAR READ
*-- End of Function lfvPOScOk.
*!******************** End OF The Style Purchase Section ************
*!******************** start OF The Credit Memo Section **********
*!***************************************************************************
*!* Name        : lfDLGTRMBN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Screen of get bins in case of RM. 
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLGTRMBN()
*!***************************************************************************
FUNCTION lfDLGTRMBN
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
lnAlias = SELECT()
IF !lfOpnFiles("WHBINLOC,WHSLOC","WHBINLST,WHSLOC","")
  RETURN
ENDIF
SELECT(lcCrMemLin)
SCATTER MEMVAR MEMO
PUSH KEY
ON KEY
DO (gcScrDir+gcWinAppl+"\RMDLBIN.SPX")
POP KEY
SELECT(lnAlias)
*-- End of Function lfDLGTRMBN.
*!***************************************************************************
*!* Name        : lfvRMbinloc
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Screen of get bins in case of Direct invoice 
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvRMbinloc()
*!***************************************************************************
FUNCTION lfvRMbinloc
PARA lnCnxt
lcCnxt = STR(lnCnxt,1)
lcWareCode = &lcCrMemHdr..cWareCode
IF !EMPTY(m.BinLoc&lcCnxt) 
  lnAlias = SELECT()
  SELECT WHSLOC
  SET ORDER TO TAG WhsLoc
  IF !SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
    SET ORDER TO TAG WhsLocSt IN WHSLOC 
    =gfModalGen('INM00000B00000','','','',ALLTRIM(m.BinLoc&lcCnxt) + ' is not a valid location')
    _CUROBJ = OBJNUM(m.BinLoc&lcCnxt)
    m.BinLoc&lcCnxt = ''
    RETURN
  ELSE
    =SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
    IF !(WhsLoc.cFlatHang = style.cflathang)
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot issue the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      m.BinLoc&lcCnxt= ''
      RETURN
    ENDIF
    llClass = .F.
    lcCount = STR(lnCount,1)
    IF (WhsLoc.cBinClass == style.cPrimClss&lcCnxt)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cSecClss&lcCnxt)
      llClass = .T.
    ENDIF
    IF (WhsLoc.cBinClass == style.cRemClss&lcCnxt)
      llClass = .T.
    ENDIF
    IF !llClass
      *--You cannot transfer from the same warehouse.
      lcMsg2 = 'You cannot Issue the style to a bin of a different class.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      RETURN
    ENDIF
    IF !SEEK(lcWareCode+LEFT(m.BinLoc&lcCnxt,10)+m.style,'WHSLOC')
      =SEEK(lcWareCode+m.BinLoc&lcCnxt,'WHSLOC')
      lcLocSty = m.style
      SCATT MEMVAR MEMO
      m.cWareCode = lcWareCode
      m.style = lcLocSty
      m.clocation = m.BinLoc&lcCnxt
      INSERT INTO WHSLOC FROM MEMVAR 
    ENDIF
  ENDIF
  SELECT WHSLOC
  SET ORDER TO TAG WhsLocSt
  SELECT(lnAlias)
ENDIF
*-- End of Function lfvRMbinloc.
*!***************************************************************************
*!* Name        : lfvRmOk
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Screen of get bins in case of Credit memo. 
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfvRmOk()
*!***************************************************************************
FUNCTION lfvRmOk
SELECT (lcCrMemLin)
REPLACE  BinLoc1  WITH m.BinLoc1,;
         BinLoc2  WITH m.BinLoc2,;
         BinLoc3  WITH m.BinLoc3,;
         BinLoc4  WITH m.BinLoc4,;
         BinLoc5  WITH m.BinLoc5,;
         BinLoc6  WITH m.BinLoc6,;
         BinLoc7  WITH m.BinLoc7,;
         BinLoc8  WITH m.BinLoc8
CLEAR READ
*-- End of Function lfvRmOk.
*!***************************************************************************
*!* Name        : lfADDRMFLD
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Add Custom Fields for temp file 
*!***************************************************************************
*!* Called from : RMSAVE.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfADDRMFLD()
*!***************************************************************************
FUNCTION lfADDRMFLD
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
lnCrMemLin = ALEN(laCrMemLin,1)
DIMENSION laCrMemLin[lnCrMemLin+8,4]
FOR I = 1 TO 8
  Lci = ALLTRIM(STR(I))
  laCrMemLin[lnCrMemLin + I , 1] = 'BinLoc&Lci'
  laCrMemLin[lnCrMemLin + I , 2] = 'C'
  laCrMemLin[lnCrMemLin + I , 3] = 10
  laCrMemLin[lnCrMemLin + I , 4] = 0
ENDFOR
*-- End of Function lfADDRMFLD.
*!***************************************************************************
*!* Name        : lfDLSAVRM
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Save Credit Memo in WHBINLOC File
*!***************************************************************************
*!* Called from : RMCRMEM.PRG -->lpSavScr
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLSAVRM()
*!***************************************************************************
FUNCTION lfDLSAVRM

IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF
PRIVATE lnCurAlias,lcRmOrder
lnCurAlias = ALIAS()
STORE "" TO lcRmOrder

IF !USED('WHBINLOC')
  =gfOpenFile(gcDataDir+'WHBINLOC','WHBINLST','SH') 
ENDIF

SELECT WHBINLOC
lcOldOrder = ORDER()
SET ORDER TO WHBINLST

IF !lfOpnFiles("STYINVJL,BININVJL,RMBINLOC","STYINVJL,STYINVJL,RMBINCRM","")
  RETURN
ENDIF

lcRmOrder = ORDER('RMBINLOC')
SET ORDER TO TAG RMWARLINE IN RMBINLOC

DIMENSION laBinArr[8]
PRIVATE lcCurTmp
lcCurTmp = gfTempName()
CREATE TABLE (gcWorkDir + lcCurTmp)(NSTK1 N(7),NSTK2 N(7),NSTK3 N(7),NSTK4 N(7),NSTK5 N(7),NSTK6 N(7),NSTK7 N(7),;
       NSTK8 N(7), nTotstk N(7),LINENO N(6), Binloc C(10) )
INDEX ON STR(LINENO,6)+Binloc  TAG (lcCurTmp)
SELECT (lcCrMemLin)
LOCATE 
IF EOF()
  RETURN
ENDIF
PRIVATE llChkUpdt
SCAN
  =SEEK(Style,'STYLE')
  *T20060817.0014   TMI [Start] non inv. styles are added to journal file but just do not update thier stock
  *IF !STYLE.LINVSTY
  *  LOOP
  *ENDIF
  *T20060817.0014   TMI [End  ] 
  *-- Assign location per size
  FOR lncI = 1 TO 8 
    lccI = ALLTRIM(STR(lncI,1))
    laBinArr[lncI] =  &lcCrMemLin..BinLoc&lccI 
  ENDFOR

  SELECT (lcCurTmp)
  FOR lni = 1 To 8
    lcI = ALLTRIM(STR(lni,1))
    IF &lcCrMemLin..Qty&lcI<> 0  
      IF !SEEK(PADL(ALLTRIM(&lcCrMemLin..cRet_LINNO),6,' ')+laBinArr[lni],lcCurTmp) 
        APPEND BLANK
        REPLACE NSTK&lcI WITH &lcCrMemLin..Qty&lcI,;
                nTotstk  WITH &lcCrMemLin..Qty&lcI,;
                Binloc   WITH laBinArr[lni]       ,;
                LINENO   WITH ROUND(VAL(&lcCrMemLin..cRet_LINNO),0)
      ELSE
        REPLACE NSTK&lcI WITH NSTK&lcI+&lcCrMemLin..Qty&lcI,;
                nTotstk  WITH nTotstk + &lcCrMemLin..Qty&lcI
      ENDIF
    ENDIF  
  ENDFOR
  SELECT (lcCrMemLin)
  *T20060817.0014   TMI [Start] get the style to seek with
  PRIVATE lcSeekSty
  lcSeekSty = IIF(EMPTY(cRetsty),STYLE,cRetsty)
  =SEEK(lcSeekSty ,'STYLE')
  *T20060817.0014   TMI [End  ] 
  
  FOR lnCount = 1  TO 8
    lcCount = ALLTRIM(STR(lnCount))
    IF !EMPTY(&lcCrMemLin..BinLoc&lcCount)
    
      *--Updating Bin location with Returns in case of damaged and 2nd Quality.
      *T20060817.0014   TMI [Start] comment this condition and include it in one using lcSeekSty variable
      *IF EMPTY(&lcCrMemLin..Cretsty)           
        *--Update the Whbinloc file only if the style is inv-style 
        *=SEEK(&lcCrMemLin..STYLE,'STYLE')
        *T20060817.0014   TMI [End  ] 
        
        IF STYLE.LINVSTY           
        
          *T20060817.0014   TMI [Start] use lcSeekSty 
          *IF SEEK(style+&lcCrMemHdr..cWareCode+&lcCrMemLin..BinLoc&lcCount,'WHBINLOC')
          IF SEEK(lcSeekSty+&lcCrMemHdr..cWareCode+&lcCrMemLin..BinLoc&lcCount,'WHBINLOC')
            *T20060817.0014   TMI [End  ] 
    
            SELECT WHBINLOC
            REPLACE  Qty&lcCount    WITH Qty&lcCount + &lcCrMemLin..Qty&lcCount,;
                     TotQty         WITH TotQty + &lcCrMemLin..Qty&lcCount
          ELSE
            SELECT WHBINLOC
            APPEND BLANK
            *T20060817.0014   TMI [Start] update with lcSeekSty and update CBLKPCK, CBINCLASS, CSECTION
            *REPLACE  Style       WITH &lcCrMemLin..style,;
            *         cWareCode   WITH &lcCrMemHdr..cWareCode,;
            *         clocation   WITH &lcCrMemLin..BinLoc&lcCount,;
            *         Qty&lcCount WITH &lcCrMemLin..Qty&lcCount,;
            *         TotQty      WITH &lcCrMemLin..Qty&lcCount
            REPLACE  Style       WITH lcSeekSty                   ;
                     cWareCode   WITH &lcCrMemHdr..cWareCode      ;
                     clocation   WITH &lcCrMemLin..BinLoc&lcCount ;
                     Qty&lcCount WITH &lcCrMemLin..Qty&lcCount    ;
                     TotQty      WITH &lcCrMemLin..Qty&lcCount    ;
                     CBLKPCK     WITH WHSLOC.CBLKPCK              ;
                     CBINCLASS   WITH WHSLOC.CBINCLASS            ;
                     CSECTION    WITH WHSLOC.CSECTION  
            =gfAdd_Info('WHBINLOC')
            *T20060817.0014   TMI [End  ] 
          ENDIF           

        ENDIF

      *T20060817.0014   TMI [Start] comment these lines as they are included in the above condition
      *-* ELSE 
      *-*   IF SEEK(&lcCrMemLin..Cretsty+&lcCrMemHdr..cWareCode+&lcCrMemLin..BinLoc&lcCount,'WHBINLOC')
      *-*     SELECT WHBINLOC
      *-*     REPLACE  Qty&lcCount    WITH Qty&lcCount + &lcCrMemLin..Qty&lcCount,;
      *-*              TotQty         WITH TotQty + &lcCrMemLin..Qty&lcCount
      *-*   ELSE
      *-*     SELECT WHBINLOC
      *-*     APPEND BLANK
      *-*     REPLACE  Style       WITH &lcCrMemLin..Cretsty,;
      *-*              cWareCode   WITH &lcCrMemHdr..cWareCode,;
      *-*              clocation   WITH &lcCrMemLin..BinLoc&lcCount,;
      *-*              Qty&lcCount WITH &lcCrMemLin..Qty&lcCount,;
      *-*              TotQty      WITH &lcCrMemLin..Qty&lcCount
      *-*   ENDIF             
      *-*   =gfAdd_Info('WHBINLOC')
      *-* ENDIF
      *T20060817.0014   TMI [End  ] 
    ENDIF  
  ENDFOR  
  
  SELECT STYINVJL
  IF EMPTY(&lcCrMemLin..Cretsty)
    IF !SEEK(&lcCrMemLin..style+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(laData[5])+laData[1])
      lcRmGlSess = STR(VAL(lcRmGlSess)+1)
      lcRmGlsess = ALLTRIM(lcRmGlsess)
      IF LEN(lcRmGlsess) = 5
        lcRmGlsess = '0'+lcRmGlsess 
      ENDIF
      =SEEK(&lcCrMemLin..style+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(laData[5])+laData[1])
    ENDIF
  ELSE
    IF !SEEK(&lcCrMemLin..Cretsty+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(laData[5])+laData[1])
      lcRmGlSess = STR(VAL(lcRmGlSess)+1)
      lcRmGlsess = ALLTRIM(lcRmGlsess)
      IF LEN(lcRmGlsess) = 5
        lcRmGlsess = '0'+lcRmGlsess 
      ENDIF
      =SEEK(&lcCrMemLin..Cretsty+&lcCrMemHdr..cWareCode+lcRmGlSess+DTOS(laData[5])+laData[1])
    ENDIF
  ENDIF   
  SCATT MEMVAR MEMO
  lnCurCost = m.nCost  
  SELECT (lcCurTmp)
  SCAN
    SCATTER MEMVAR MEMO
    m.CLOCATION = m.BINLOC 
    m.nCost     = lnCurCost 
    m.NSTKval   = m.nTotstk * m.nCost
    SELECT BININVJL
    APPEND BLANK
    GATHER MEMVAR MEMO 
    FOR lni = 1 To 8
      SELECT RMBINLOC
      lcI = ALLTRIM(STR(lni,1))
      IF EMPTY(&lcCrMemLin..Cretsty)
        IF SEEK(&lcCrMemLin..crmemo+&lcCrMemHdr..cWareCode+PADL(ALLTRIM(&lcCrMemLin..cRet_LINNO),6,' ')+&lcCrMemLin..style+&lcCurTmp..Binloc)
          IF &lcCurTmp..NSTK&lcI <> 0  
            REPLACE Qty&lcI  WITH &lcCurTmp..NSTK&lcI,;
                    TotQty   WITH TotQty  + &lcCurTmp..NSTK&lcI
          ENDIF  
        ELSE
          SELECT RMBINLOC
          APPEND BLANK
          REPLACE  Style       WITH &lcCrMemLin..style,;
                   cWareCode   WITH &lcCrMemHdr..cWareCode,;
                   LineNo      WITH ROUND(VAL(&lcCrMemLin..cRet_LINNO),0),;
                   clocation   WITH &lcCurTmp..Binloc ,;
                   CrMemo      WITH &lcCrMemLin..crmemo,;
                   Qty&lcI     WITH &lcCurTmp..NSTK&lcI,;
                   TotQty      WITH TotQty  + &lcCurTmp..NSTK&lcI
          =gfAdd_Info('RMBINLOC')
        ENDIF
      ELSE
        IF SEEK(&lcCrMemLin..crmemo+&lcCrMemHdr..cWareCode+PADL(ALLTRIM(&lcCrMemLin..cRet_LINNO),6,' ')+&lcCrMemLin..Cretsty+&lcCurTmp..Binloc)
          IF &lcCurTmp..NSTK&lcI <> 0  
            REPLACE Qty&lcI  WITH &lcCurTmp..NSTK&lcI,;
                    TotQty   WITH TotQty  + &lcCurTmp..NSTK&lcI
          ENDIF  
        ELSE
          SELECT RMBINLOC 
          APPEND BLANK
          REPLACE  Style       WITH &lcCrMemLin..Cretsty,;
                   cWareCode   WITH &lcCrMemHdr..cWareCode,;
                   LineNo      WITH ROUND(VAL(&lcCrMemLin..cRet_LINNO),0),;
                   clocation   WITH &lcCurTmp..Binloc ,;
                   CrMemo      WITH &lcCrMemLin..crmemo,;
                   Qty&lcI     WITH &lcCurTmp..NSTK&lcI,;
                   TotQty      WITH TotQty  + &lcCurTmp..NSTK&lcI
          =gfAdd_Info('RMBINLOC')
        ENDIF
      ENDIF  
    ENDFOR
  ENDSCAN
  SELECT (lcCurTmp)
  ZAP
ENDSCAN

SELECT WHBINLOC
SET ORDER TO &lcOldOrder
SET ORDER TO TAG &lcRmOrder IN RMBINLOC
SELECT (lnCurAlias) 
*-- End of Function lfDLSAVRM.

*!***************************************************************************
*!* Name        : lfRMDFNMNU
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Define Bin Locations valid
*!***************************************************************************
*!* Called from : RMCRMEM.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfRMDFNMNU()
*!***************************************************************************
FUNCTION lfRMDFNMNU

IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF

*--check if the option pad is already defined on the sysmenu
llFound = .F.
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')		&& Number of pads
	IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', LnCount)) = 'Options'
        llfound = .T.
		EXIT
	ENDIF
ENDFOR
IF !llfound
  DEFINE PAD _Option OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , ' ' SKIP FOR IIF(TYPE('lcProgName')='C' AND lcProgName = "RMCRMEM",.F.,.T.) ;
  OR (laScrMode[1]) OR (laScrMode[2]) 
  ON PAD _Option OF _msysmenu ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF
llFound = .F.
FOR lnCount = 1 TO CNTBAR('_OPTIONPOP')
  IF PRMBAR('_OPTIONPOP', LnCount) = 'Bins Detail'
    llfound = .T.
 	EXIT
  ENDIF
ENDFOR
IF !llfound
  lnBarNo = CNTBAR('_OPTIONPOP') + 1
  DEFINE BAR lnBarNo OF _OPTIONPOP PROMPT "\<Bins Detail" SKIP FOR gfGetMemVar('M_WARELOC')='N'
  ON SELECTION BAR lnBarNo OF _OPTIONPOP DO lfDLGTRMBN IN BINMAIN
ENDIF
RETURN
*-- End of Function lfRMDFNMNU.
*!***************************************************************************
*!* Name        : lfWBnLoc
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Define Bin Locations valid
*!***************************************************************************
*!* Called from : 
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfWBnLoc()
*!***************************************************************************
FUNCTION lfWBnLoc
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN .F.
ENDIF
FOR lnCount = 1 TO 8 
  lcCount = ALLTRIM(STR(lnCount))
  IF !EMPTY(laSize[lnCount])
    SHOW GET m.BinLoc&lcCount ENABLE
  ELSE
    SHOW GET m.BinLoc&lcCount DISABLE
  ENDIF
ENDFOR  

SHOW GETS
*-- End of Function lfWBnLoc.

*!***************************************************************************
*!* Name        : lfRMCKSVBN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Define Bin Locations valid
*!***************************************************************************
*!* Called from : RMCRMEM.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfRMCKSVBN()
*!***************************************************************************
FUNCTION lfRMCKSVBN

IF !gfGetMemVar('M_DLUSEBIN')
  RETURN llCSave
ENDIF

STORE .F. TO  llmessag

*--check location if found or not to prevent user to save empty Bin
llChckLoc = .F.
llChckCls = .F.
DIMENSION laStyArr[1]
STORE '' TO laStyArr
SELECT (lcCrMemLin)

LOCATE
SCAN 
  IF llmessag
    EXIT
  ENDIF
  m.loc1 = &lcCrMemLin..Binloc1
  m.loc2 = &lcCrMemLin..Binloc2
  m.loc3 = &lcCrMemLin..Binloc3
  m.loc4 = &lcCrMemLin..Binloc4
  m.loc5 = &lcCrMemLin..Binloc5
  m.loc6 = &lcCrMemLin..Binloc6
  m.loc7 = &lcCrMemLin..Binloc7
  m.loc8 = &lcCrMemLin..Binloc8
  FOR lnI = 1 TO 8
    lcI = ALLT(STR(lnI,2))
    IF !EMPTY(&lcCrMemLin..Qty&lcI)
      IF EMPTY(m.loc&lcI)
        =SEEK(Style,'STYLE')

        *T20060817.0014   TMI [Start] include all styles in check
        *IF STYLE.LINVSTY
        *T20060817.0014   TMI [End  ] 

          llChckLoc = .T.

        *T20060817.0014   TMI [Start] close the above if condition
        *ENDIF
        *T20060817.0014   TMI [End  ] 

        IF EMPTY(laStyArr[1])
          laStyArr[1] = Style.cflathang
        ELSE
          IF !(laStyArr[1] = Style.cflathang)
            llChckCls = .T.
            EXIT
          ENDIF
        ENDIF
      ENDIF
    ENDIF  
  ENDFOR
ENDSCAN

SELECT (lcCrMemLin)
LOCATE
IF llChckCls
  lcMsg2 = 'You have mixed Flat and Hanging styles on this transaction - please enter the bin locations manually'
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
  llmessag = .T.
  STORE .F. TO llShow,llCSave
  RETURN llCSave
ENDIF

IF llChckLoc
  lcMsg2 = 'You have to assign bin location before saving . Do you want to assign bin location now ?'
  lnChoose =gfModalGen("QRM00000B38006","DIALOG",.F.,.F.,lcMsg2)
  IF lnChoose = 1
    IF !USED('WHSLOC') 
      =gfOpenFile(gcDataDir+'WHSLOC','WHSLOC','SH')
    ENDIF
    DIME laTempData[3]
    lcWarecode = laData[29]
    Store '' TO lcBinLoc , laTempData
    DO (gcScrDir+'PO'+"\POdlbnSC.Spx")
    IF EMPTY(lcBinLoc)
      lcMsg2 = 'You have to assign bin location before saving .'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llmessag = .T.
      STORE .F. TO llShow,llCSave
      RETURN llCSave
    ENDIF
    
    =SEEK(&lcCrMemLin..Style,'Style')
    IF !(WhsLoc.cFlatHang = style.cflathang)
      *-- 'You cannot saving the style to a bin of a different Hang/Flat.'
      lcMsg2 = 'You cannot saving the style to a bin of a different Hang/Flat.'
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg2)
      llmessag = .T.
      STORE .F. TO llShow,llCSave
      RETURN llCSave
    ENDIF
  ELSE
    llmessag = .T.
    STORE .F. TO llShow,llCSave
    RETURN llCSave
  ENDIF  
  SELECT (lcCrMemLin)
  SCAN
    REPLACE &lcCrMemLin..Binloc1  WITH laTempData[1],;
            &lcCrMemLin..Binloc2  WITH laTempData[1],;
            &lcCrMemLin..Binloc3  WITH laTempData[1],;
            &lcCrMemLin..Binloc4  WITH laTempData[1],;
            &lcCrMemLin..Binloc5  WITH laTempData[1],;
            &lcCrMemLin..Binloc6  WITH laTempData[1],;
            &lcCrMemLin..Binloc7  WITH laTempData[1],;
            &lcCrMemLin..Binloc8  WITH laTempData[1]
  ENDSCAN
  SHOW GETS
ENDIF
RETURN llCSave
*-- End of Function lfRMCKSVBN.
*!***************************************************************************
*!* Name        : lfDLVODRM
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Save Data to Void Credit memo screen
*!***************************************************************************
*!* Called from : RMCRMEM.PRG
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLVODRM()
*!***************************************************************************
FUNCTION lfDLVODRM

IF !gfGetMemVar('M_DLUSEBIN')
  RETURN
ENDIF

IF !lfOpnFiles("WHBINLOC,RMBINLOC","WHBINLST,RMBINCRM","")
  RETURN
ENDIF
STORE "" TO lcRetStyle
lcRetStyle = IIF(!EMPTY(RetLine.cRetSty),RetLine.cRetSty,RetLine.Style)

=lfDLRMVOID()

IF SEEK(RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode ,'RMBINLOC')
  SELECT RMBINLOC
  SCAN REST WHILE crmemo+style+cwarecode+clocation = RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode
    *T20060817.0014   TMI [Start] selects the correct order and check that the style is Inv. style
    *IF SEEK(RMBINLOC.Style+ RMBINLOC.cWareCode + RMBINLOC.cLocation ,'WHBINLOC')     
    IF RMBINLOC.LINENO = VAL(RETLINE.CRET_LINNO)
      IF SEEK(RMBINLOC.cWareCode + RMBINLOC.cLocation + RMBINLOC.Style,'WHBINLOC') .AND. ;
        SEEK(RMBINLOC.Style,'STYLE') .AND. STYLE.LINVSTY
        *T20060817.0014   TMI [End  ]
        SELECT WHBINLOC
        REPLACE Qty1   WITH Qty1 - RmBinLoc.Qty1  ;
                Qty2   WITH Qty2 - RmBinLoc.Qty2  ;
                Qty3   WITH Qty3 - RmBinLoc.Qty3  ;
                Qty4   WITH Qty4 - RmBinLoc.Qty4  ;
                Qty5   WITH Qty5 - RmBinLoc.Qty5  ;
                Qty6   WITH Qty6 - RmBinLoc.Qty6  ;
                Qty7   WITH Qty7 - RmBinLoc.Qty7  ;
                Qty8   WITH Qty8 - RmBinLoc.Qty8  ;
                TotQty WITH TotQty - RmBinLoc.TotQty
        IF TotQty =0
          DELETE 
        ENDIF        
      ENDIF
      SELECT RMBINLOC
      DELETE
      *T20060817.0014   TMI [Start] 
    ENDIF
    *T20060817.0014   TMI [End  ] 
  ENDSCAN
ENDIF
*-- End of Function lfDLVODRM.

*!***************************************************************************
*!* Name        : lfDLRMVOID
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Update BinInvJL & WHBINLOC file in case of void Credit memo
*!***************************************************************************
*!* Called from : BINMAIN.PRG --> lfDLVODRM
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfDLRMVOID()
*!***************************************************************************
FUNCTION lfDLRMVOID
PRIVATE lnCurAlias,lcTmpIvbin,lcTmpBnInv
lnCurAlias = SELECT()

IF !lfOpnFiles("WHBINLOC,STYINVJL,BININVJL,RMBINLOC","WHBINLOC,STYINVJL,STYINVJL,RMBINCRM","")
  RETURN
ENDIF

lcTmpIvbin = gfTempName()
lcTmpBnInv = gfTempName()

*--create tmpfile for invoice bin location
SELECT RMBINLOC
DIMENSION laFileStru[1,4]
= AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpIvbin) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpIvbin)

*--create tmpfile for bin invintory file
SELECT BININVJL
DIMENSION laFileStru[1,4]
=AFIELDS(laFileStru)
CREATE TABLE (gcWorkDir+lcTmpBnInv) FROM ARRAY laFileStru
INDEX ON Style+cWareCode+clocation TAG (lcTmpBnInv)

SELECT RMBINLOC

=SEEK(RETHDR.CRMEMO+lcRetStyle+RETHDR.cwarecode)
*T20060817.0014   TMI [Start] 
*SCAN REST WHILE crmemo+style+cwarecode+clocation = RETHDR.CRMEMO + lcRetStyle + RETHDR.cwarecode 
SCAN REST WHILE crmemo+style+cwarecode+clocation = RETHDR.CRMEMO + lcRetStyle + RETHDR.cwarecode ;
            FOR RMBINLOC.LINENO = VAL(RETLINE.CRET_LINNO)
  *T20060817.0014   TMI [End  ] 
  SCATT MEMVAR MEMO
  INSERT INTO (lcTmpIvbin) FROM MEMVAR
ENDSCAN
SELECT RETLINE
lnCurRec = RECNO()
=SEEK(lcRetStyle+RETHDR.cwarecode+lcVRGLSess,'STYINVJL')
SELECT STYINVJL

SCAN REST WHILE style+cwarecode+csession+DTOS(dtrdate)+ctrcode+STR(lineno,6)= lcRetStyle+RETHDR.cwarecode + lcVRGLSess ;
            FOR ctrtype = '8' And ctrcode = RETHDR.CRMEMO AND STR(LineNo,6) = PADL(ALLTRIM(RETLINE.CRET_LINNO),6,' ')
  SCATTER MEMVAR MEMO
  =SEEK(STYLE+CWARECODE,lcTmpIvbin)
  SELECT(lcTmpIvbin)
  SCAN REST WHILE STYLE+CWARECODE+CLOCATION = STYINVJL.STYLE+STYINVJL.CWARECODE
    llChkQty = .F.
    FOR lnI  = 1 TO 8
      lcI = ALLT(STR(lnI,2))
      IF (m.NSTK&lcI = 0 ) OR (&lcTmpIvbin..Qty&lcI > ABS(m.NSTK&lcI))
        llChkQty = .T.
      ELSE
        llChkQty = .F.
        EXIT
      ENDIF
    ENDFOR
    IF llChkQty 
      SELECT (lcTmpBnInv)
      m.clocation = &lcTmpIvbin..clocation
      APPEND BLANK
      GATHER MEMVAR MEMO
      SELECT(lcTmpIvbin)
      FOR lnI = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        REPLACE Qty&lcI WITH Qty&lcI + ABS(m.NSTK&lcI)
      ENDFOR  
      REPLACE TotQty WITH TotQty + ABS(m.nTotStk)
      EXIT
    ENDIF
    IF m.nTotStk <>0 AND &lcTmpIvbin..TotQty <> 0
      SELECT (lcTmpBnInv)
      m.clocation = &lcTmpIvbin..clocation
      APPEND BLANK
      GATHER MEMVAR MEMO
      FOR lnI = 1 TO 8
        lcI = ALLT(STR(lnI,2))
        IF ABS(m.NSTK&lcI) > &lcTmpIvbin..Qty&lcI
          REPLACE NSTK&lcI WITH  &lcTmpIvbin..Qty&lcI
          m.NSTK&lcI = m.NSTK&lcI - &lcTmpIvbin..Qty&lcI
          m.nTotStk  = m.nTotStk  - &lcTmpIvbin..Qty&lcI
          REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty + &lcTmpIvbin..Qty&lcI
          REPLACE &lcTmpIvbin..Qty&lcI WITH 0
        ELSE
          REPLACE NSTK&lcI WITH m.NSTK&lcI
          REPLACE &lcTmpIvbin..TotQty  WITH &lcTmpIvbin..TotQty - m.NSTK&lcI
          REPLACE &lcTmpIvbin..Qty&lcI WITH  &lcTmpIvbin..Qty&lcI - m.NSTK&lcI
          m.nTotStk  = m.nTotStk  + m.NSTK&lcI
          m.NSTK&lcI = 0
        ENDIF 
      ENDFOR  
      REPLACE nTotStk WITH NSTK1+NSTK2+NSTK3+NSTK4+NSTK5+NSTK6+NSTK7+NSTK8
      REPLACE NSTKval WITH nTotStk * nCost
    ENDIF
  ENDSCAN
ENDSCAN                

SELECT RETLINE
IF BETWEEN(lnCurRec ,1,RECCOUNT())
  GOTO lnCurRec 
ENDIF

SELECT (lcTmpBnInv)
*T20060817.0014,3   TMI [Start] remove lines with no stock
DELETE FOR NSTK1+NSTK2+NSTK3+NSTK4+NSTK5+NSTK6+NSTK7+NSTK8 = 0
GO TOP
*T20060817.0014,3   TMI [End  ] 
SCAN
 SCATTER MEMVAR MEMO
  INSERT INTO BININVJL FROM MEMVAR
ENDSCAN
*--Erase the lcTmpBnInv and lcTmpIvbin file.
=lfEraseFil(lcTmpBnInv)
=lfEraseFil(lcTmpIvbin)
SELECT (lnCurAlias)
*-- End of Function lfDLRMVOID.

*!***************************************************************************
*!* Name        : lfRMCKVDBN
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 06/06/2006
*!* Module      : Return Merchandise (RM)
*!* Purpose     : Validate void credit memo 
*!***************************************************************************
*!* Called from : RMCRMEM.PRG --> lpderscr
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfRMCKVDBN()
*!***************************************************************************
FUNCTION lfRMCKVDBN
IF !gfGetMemVar('M_DLUSEBIN')
  RETURN .F.
ENDIF
STORE .F. TO llMessage
STORE '' TO lcMsgstr
IF !lfOpnFiles("WHBINLOC,RMBINLOC","WHBINLST,RMBINCRM","")
  RETURN
ENDIF
STORE "" TO lcRetStyle
lcRetStyle = IIF(!EMPTY(RetLine.cRetSty),RetLine.cRetSty,RetLine.Style)
IF SEEK(RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode ,'RMBINLOC')
  SELECT RMBINLOC
  SCAN REST WHILE crmemo+style+cwarecode+clocation = RETHDR.CRMEMO+lcRetStyle+RETHDR.cWareCode
    *T20060817.0014   TMI [Start] do not check for stock availability for non inv. styles
    IF SEEK(lcRetStyle,'STYLE') .AND. !STYLE.LINVSTY
      LOOP
    ENDIF
    *T20060817.0014   TMI [End  ] 
    IF SEEK(RMBINLOC.Style+ RMBINLOC.cWareCode + RMBINLOC.cLocation ,'WHBINLOC') 
      SELECT WHBINLOC
      FOR lnCurCont = 1 TO 8
        lcCurCont = ALLTRIM(STR(lnCurCont))
        IF Qty&lcCurCont < RmBinLoc.Qty&lcCurCont
          llMessage = .T.
          lcMsgstr  = lcMsgstr + "Size"+ lcCurCont + " "
        ENDIF
      ENDFOR
    ELSE
      llMessage = .T.
      FOR lnCurCont = 1 TO 8
        lcCurCont = ALLTRIM(STR(lnCurCont))
        IF !EMPTY(RMBINLOC.Qty&lcCurCont)
          lcMsgstr  = lcMsgstr + "Size"+ lcCurCont + " "
        ENDIF  
      ENDFOR
    ENDIF
    IF llMessage
      *T20060817.0014   TMI [Start] write the message with correct spelling
      *lcMsgstr  = "No enoght stock for bin "+ RMBINLOC.clocation + "Sizes " + lcMsgstr  
      lcMsgstr  = "No enough stock for bin "+ RMBINLOC.clocation + "Sizes " + lcMsgstr  
      *T20060817.0014   TMI [End  ] 
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsgstr)
    ENDIF
  ENDSCAN
ENDIF
llReturn= IIF(llMessage,.F.,.T.)
RETURN llReturn
*-- End of Function lfRMCKVDBN.
*!******************** End OF The Credit Memo Section ************
*!******************** General Section **************************************
*!***************************************************************************
*!* Name        : lfOpnFiles
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Purpose     : Open the Needed files for Bin Location System and Check if
*!*             : Setting (Use Bin Location) is yes or no
*!***************************************************************************
*!* Called from : SMDLRBL.PRG
*!***************************************************************************
*!* Parameters  : lcFilesExp --> Files that need to open.
*!*             : lcTages    --> Tags for the opened files.
*!*             : lcCurPik   --> Current Piktkt.
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfOpnFiles()
*!***************************************************************************
FUNCTION lfOpnFiles
PARAMETERS lcFilesExp,lcTages,lcCurPik
DIMENSION laOpnFiles[1],laOpnTages[1]
IF !gfGetMemVar('M_DLUSEBIN',gcAct_Comp) OR lcCurPik == "******" 
  RETURN .F.
ENDIF
=GFSUBSTR(lcFilesExp,@laOpnFiles,',')
=GFSUBSTR(lcTages,@laOpnTages,',')
FOR I=1 TO ALEN(laOpnFiles,1)
  IF !USED(laOpnFiles[I])
    =gfOpenFile(gcDataDir+laOpnFiles[I],laOpnTages[I],'SH')
  ELSE
    SET ORDER TO TAG laOpnTages[I] IN laOpnFiles[I]
  ENDIF
ENDFOR
RETURN .T.
*-- End of Function lfOpnFiles.
*!***************************************************************************
*!* Name        : lfEraseFil
*!* Developer   : NNA - NADER NABIL ABD-ALMONAM
*!* Date        : 02/14/2006
*!* Purpose     : Erase Temp Files
*!***************************************************************************
*!* Called from : Binmain.prg
*!***************************************************************************
*!* Parameters  : None
*!***************************************************************************
*!* Return      : None
*!***************************************************************************
*!* Example     : = lfEraseFil()
*!***************************************************************************
FUNCTION lfEraseFil
PARAMETER lcFile
IF USED(lcFile)
  USE IN (lcFile)
ENDIF
ERASE (gcWorkDir +lcFile+".DBF")
ERASE (gcWorkDir +lcFile+".CDX")
ERASE (gcWorkDir +lcFile+".FPT")

*-- End of Function lfEraseFil.
*!*************************************************************
*! Name      : lfChkStrct
*! Developer : Nader Nabil (NNA)
*! Date      : 02/14/2006
*! Purpose   : Get the Style and Color Length.
*!*************************************************************
*! Calls     : Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from        : SOACJERE.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns     : None
*!*************************************************************
*! Example     : =lfChkStrct()
*!*************************************************************
FUNCTION lfChkStrct
*--THE COLOR LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen   = LEN(laItemSeg[lnCount,3])
    lnClrStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE STYLE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='F'
    lnStyLen  = LEN(laItemSeg[lnCount,3])
    lnStyStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--THE SCALE LENGTH
DECLARE laItemSeg[1]
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='S'
    lnScaLen   = LEN(laItemSeg[lnCount,3])
    lnScaStPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
*--End of lfChkStrct.

*:**************************************************************************
*:* Name        : lfUPRPLNSH
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/03/2006
*:* Purpose     : Add a trigger to update the CREPLENISH field for GPS00
*:***************************************************************************
*:* Called from : lfvNew in ICLOCN.PRG
*:***************************************************************************
*C132754,1  
FUNCTION lfUPRPLNSH
  
*- Define the default values for the new added bin
REPLACE cFlatHang  WITH 'F' ;
        cBinClass  WITH 'A' ;
        cBlkPck    WITH 'P' ;
        CREPLENISH WITH 'Y'

*-- end of lfUPRPLNSH.

*:**************************************************************************
*:* Name        : lfENDSBINS
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/17/2006
*:* Purpose     : Enable disable bins in case when the user entered 2nd/damaged quality
*:***************************************************************************
*:* Called from : lfvQty in POSTREC.PRG
*:***************************************************************************
*T20060817.0014  TMI (132757)
FUNCTION lfENDSBINS
PRIVATE lcVar,lcStat,lnQty

lcVar = SYS(18)

lnQty = laOrg[lnCnxt] - (laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])
IF lnQty < 0
  
  &lcVar = lcOldValue
  
ELSE

  laSok[lnCnxt] = laOrg[lnCnxt] - (laDam1[lnCnxt]+laDam2[lnCnxt]+laCan[lnCnxt])
  laBal[lnCnxt] = 0
  SHOW GET laSok[lnCnxt]
  SHOW GET laBal[lnCnxt]
  
  *IF 'LACAN' $ lcVar
  *ENDIF

  IF !'LACAN' $ lcVar
    lcStat = IIF( laDam1[lnCnxt]+laDam2[lnCnxt] > 0 , 'ENABLE' , 'DISABLE' )
    laBinLoc[lnCnxt] = IIF( laDam1[lnCnxt]+laDam2[lnCnxt] > 0 , laBinLoc[lnCnxt] , '' )
    SHOW GET laBinLoc[lnCnxt] &lcStat
  ENDIF

ENDIF
*-- end of lfENDSBINS.

*:**************************************************************************
*:* Name        : lfDISBNLOC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/17/2006
*:* Purpose     : Enable / disable bins fields when PODLRCV screen starts
*:***************************************************************************
*:* Called from : lfWMfScr in POSTREC.PRG
*:***************************************************************************
*T20060817.0014   TMI (132757)
FUNCTION lfDISBNLOC
PRIVATE I,lcStat,lnSlct,;
        lnRecno,m.STYLE,m.PO,m.LINENO

lnSlct = SELECT()

*- Get the binlocations id's from the 2nd / damaged quality lines
SELECT (lcTmpLine)
lnRecno = RECNO()
SCATTER FIELDS STYLE,PO,LINENO MEMVAR
LOCATE FOR PO+STYLE+STR(LINENO,6) = m.PO+m.STYLE+STR(m.LINENO,6) ;
        .AND. TRANCD > '2'
IF FOUND()
  SCATTER FIELDS LOC1,LOC2,LOC3,LOC4,LOC5,LOC6,LOC7,LOC8 TO laBinLoc
ENDIF
IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpLine))
  GOTO lnRecno IN &lcTmpLine
ENDIF

FOR I = 1 TO 8
  SHOW GET laSok[I]    DISABLE   
  lcStat = IIF( laDam1[I]+laDam2[I]>0 , 'ENABLE' , 'DISABLE' )
  SHOW GET laBinLoc[I] &lcStat
ENDFOR

SELECT (lnSlct)
*-- end of lfDISBNLOC.


*:**************************************************************************
*:* Name        : lfUPBNLOC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 08/27/2006
*:* Purpose     : Update bin location fields for 2nd/ damaged quality fields
*:***************************************************************************
*:* Called from : lfvEditQty IN POSTREC.PRG
*:***************************************************************************
*T20060817.0014
FUNCTION lfUPBNLOC
PRIVATE lnSlct,lnRecno,m.PO,m.Style,m.Lineno,laBinloc
RESTORE FROM (gcWorkDir+lcTmpLine+'.mem') ADDITIVE
ERASE (gcWorkDir+lcTmpLine+'.mem')

lnSlct = SELECT()
SELECT &lcTmpLine
lnRecno = RECNO()
SCATTER FIELDS PO,Style,Lineno MEMVAR
SCAN REST FOR PO+Style+STR(Lineno ,6) = m.PO+m.Style+STR(m.Lineno ,6) AND TRANCD>'2'
  GATHER FIELDS LOC1,LOC2,LOC3,LOC4,LOC5,LOC6,LOC7,LOC8 FROM laBinLoc
ENDSCAN

IF BETWEEN(lnRecno,1,RECCOUNT(lcTmpLine))
  GOTO lnRecno IN &lcTmpLine
ENDIF

SELECT (lnSlct)
*-- end of lfUPBNLOC.

*:**************************************************************************
*:* Name        : lfGFSTYCRL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : Sep-17-2006
*:* Purpose     : calls the lfStyCrl function for bin locations instead of the global one
*:***************************************************************************
*:* Called from : gpSaveInv in ARINV.PRG
*:***************************************************************************
*T20060817.0014   
FUNCTION lfGFSTYCRL
PRIVATE lnRet,lnK,lcK

lcBinLoc = ''
FOR lnK = 1 TO 8
  lcK = STR(lnK,1)
  lcBinLoc = lcBinLoc + &lcDetFile..Binloc&lcK + '|'
ENDFOR


lnRet = IIF(m.TotQty=0 OR CONSOL = 'Y',13,;
          lfStyCrl('3',m.Style,m.cWareCode,m.Dyelot,&lcHdrFile..InvDate,lcInvNo,;
          @laAdjStk,m.Cost,lcRefer,lcGlSession,'',13,lcDetFile,'nSteps',@laGlArray,m.LineNo))

RETURN lnRet
*-- end of lfGFSTYCRL.
*****

*:**************************************************************************
*:* Name        : lfRMSTYCRL
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 09/18/2006
*:* Purpose     : calls the lfStyCrl function for bin locations instead of the global one
*:***************************************************************************
*:* Called from : lpDelScr IN RMCRMEM.PRG
*:***************************************************************************
*T20060817.0014  
FUNCTION lfRMSTYCRL
PRIVATE lnRet,lnSlct,lcSvOrd,lnL,lcL
lnSlct = SELECT()

IF !USED('BININVJL')
  =gfOpenFile(gcDataDir+'BININVJL','MFGOPR')
ENDIF

*** I found that in case of voided credit memo the loop is on the file RETLINE and the temp file lcCrMemLin is not filled so 
*** I will fill it myself to use the BINLOC1,...,8 fields

SELECT BININVJL
lcSvOrd = ORDER()
SET ORDER TO MFGOPR
lcBinLoc = ''
=SEEK(laData[1],'BININVJL')
SCAN REST WHILE CTRCODE+COPRCODE+CLOTNO+CTRTYPE+STYLE+CWARECODE = laData[1] ;
            FOR CTRTYPE = '7'
  lcBinLoc = lcBinLoc + CLOCATION + '|'
  SELECT &lcCrMemLin
  &&- ACCOUNT+STYLE+CRET_LINNO+CRET_TRNCD
  IF !SEEK(RETLINE.ACCOUNT+RETLINE.STYLE+RETLINE.CRET_LINNO+RETLINE.CRET_TRNCD,lcCrMemLin)
    INSERT INTO &lcCrMemLin (ACCOUNT,STYLE,CRET_LINNO,CRET_TRNCD) VALUES (RETLINE.ACCOUNT,RETLINE.STYLE,RETLINE.CRET_LINNO,RETLINE.CRET_TRNCD)
  ENDIF
  FOR lnL = 1 TO 8
    lcL = STR(lnL,1)
    IF !EMPTY(BININVJL.nStk&lcL)
      REPLACE BINLOC&lcL WITH BININVJL.CLOCATION
    ENDIF
  ENDFOR
ENDSCAN

SELECT (lnSlct)
lnRet = lfStyCrl('8',IIF(!EMPTY(cRetSty) , cRetSty , Style),;
                 laData[29],Dyelot,CrDate,CrMemo,@laAdjQty,RetLine.Cost,;
                 lcRefer,lcVRGLSess,'',lnWhichLin,lcCrMemLin,'nSteps',@laGLDistAr,;
                 VAL(RetLine.cRet_LinNo) , lcRecSess)
RETURN lnRet                                  
*-- end of lfRMSTYCRL.
