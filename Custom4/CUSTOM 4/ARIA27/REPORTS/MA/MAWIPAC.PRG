*:--------------------------------------------------------------------------
*: Program file        : MAWIPAC.PRG
*: Program description : GL-WIP Account Report.
*: For screen          : None.
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : MA - Material.
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : E301693
*: Date                : 01st of September 2001
*:--------------------------------------------------------------------------
*: Calls               : MAWIPAD.FRX.  && Detail Form.
*:--------------------------------------------------------------------------
*: Called From         : System Menu
*:--------------------------------------------------------------------------
*: Passed Parameters   : None.
*:--------------------------------------------------------------------------
*: Example             : DO MAWIPAC
*:--------------------------------------------------------------------------
*: Modificatoins       :
*:B119523,1 ALB 09/03/2003 Display all costs with base currency
*:B120322,1 ABD 12/08/2003 Fix bug that the activity date range filter is not working. 
*:B120322,3 MHM 01/04/2004 Fix bug that the activity date range filter is not working. 
*:B121886,1 MHM 03/24/2004 1- Add vendor code and name in case of MAterial PO
*:B121886,1                2- The activity date range filter should be Between the from and to date
*:B122289,1 NNA 05/04/2004 Fix bug that the report show the Close Trans. Even her Closing date 
*:B122289,1 NNA            not included in the [Activity Date Range]
*:--------------------------------------------------------------------------
llGlLink  = (ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',gcAct_Comp)))='Y')
llApLink  = ('AP' $ gcCmpModules)

IF llGlLink  && If the system is linked to GL-Module, Else Return.
  *--------------------------------------------------------------
  *lcRpTmpFil                      && Variable hold the Temp File Name
  *                                && Initiated from Option Grid.
  *lcRpFromTyp                     && Form LayOut (Detail,Summary).
  lcGlCode  = lcRPLnkCod           && Gl-Link Code Variable.
  lcLinTmNam = ""
  lcGlAcnt  = lcRpGlAct            && Gl Account Code.
  lcCtPoSt  = lcRpTRStat           && MFG OR PO Material  Status Variable.
  lcSeason = ".T."
  lcDivision = ".T."
  lcHdrFl = ""
  STORE {} TO ldLowDat , ldHigDat  && Initial Date Range Variable.
  STORE "" TO lcLowSty , lcHigSty  && Initiate Style Range Variable.
  lcWhilCon = ""
  llDumyRet = lfSetRel()           && Call Function to set relations.
  lcSeltdPo = ""
  lcSeltdMF = ""
  lcMMFlt  = ".T."
  =lfCretFltr()
  WAIT WINDOW 'Collecting data ..' NOWAIT
  *--- Start Collecting Transactions...
  llDummy = lfClectData()
  WAIT CLEAR
  SELECT &lcRpTmpFil
  GO TOP
  IF EOF()
    *Message 'There are no records to display...!'
    =gfModalGen('TRM00052B00000','DIALOG')
  ELSE
    *lcRpForm = IIF(lcRpFromTyp='D',"MFWIPAD","MFWIPAS")
    lcRpForm = "MAWIPAD"
    *--- ssh
    *lcRpFromTyp='S'
    =gfCrtFrm(lcRpForm,lcOGFormArr,llOGRefForm)
    =lfRepPltFr(lcRpForm)
    SELECT &lcRpTmpFil
    GO TOP
    DO gfDispRe WITH EVAL('lcRpForm')
  ENDIF
ELSE
  =gfModalGen('TRM38195B00000','ALERT')
ENDIF
SET DEVICE TO SCREEN
RETURN


*!*************************************************************
*! Name      : lfClectData
*! Developer : AHMED SALAH SHALABY (SSH)
*!      Date : 25/09/2001
*! Purpose   : Function to collect Data.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClectData()
*!*************************************************************
*!
FUNCTION lfClectData

*B121886,1 MHM 03/24/2004 Add vendor code and name in case of MAterial PO [BEGIN]
*CREATE TABLE (gcWorkDir+lcRpTmpFil) (WIPAcc C(24),CtPoNo C(6),Type C(1),cType C(1),TranDate D,;
*                          TranType C(1),Desc C(20),Qty N(13,3),Cost N(12,7),llDontShow L ,llFabric L)

CREATE TABLE (gcWorkDir+lcRpTmpFil) (WIPAcc C(24),CtPoNo C(6),Type C(1),cType C(1),TranDate D,;
                          TranType C(1),Desc C(20),Qty N(13,3),Cost N(12,7),llDontShow L ,llFabric L,;
                          cVendCode C(8),cVendName C(30))
*B121886,1  [End]

                          

INDEX ON CType+CtPoNo TAG cTkTNo
INDEX ON Type+CtPoNo TAG TkTNo
INDEX ON WIPAcc+Type+CtPoNo+cType TAG &lcRpTmpFil

lcTmpCurs = gfTempName()
CREATE TABLE (gcWorkDir+lcTmpCurs) (CtPoNo C(6),Item C(7),Clr C(6),cSession C(06),lcIssRet C(1))
INDEX ON CtPoNo+Item +Clr+cSession+lcIssRet TAG (lcTmpCurs)

lcTmpNam = gfTempName()
USE gcDataDir+"GLDIST" AGAIN ALIAS (lcTmpNam) IN 0 SHARED

llUsePoTm = .F.  && Variable to indicate if the user select specific PO.
llUseCTTm = .F.  && Variable to indicate if the user select specific CT.

*-- Check if we are going to use temp file that holding selected PO from OG.
IF USED(lcSeltdPo)
  GO TOP IN (lcSeltdPo)
  llUsePoTm = !EOF(lcSeltdPo)
ENDIF

*-- Check if we are going to use temp file that holding selected CT from OG.
IF USED(lcSeltdMF)
  GO TOP IN (lcSeltdMF)
  llUseCTTm = !EOF(lcSeltdMF)
ENDIF

llDummy = llApLink .AND. !USED("ApvInvDt") .AND. gfOpenFile(gcDataDir+"ApvInvDt","Item",'SH')
llDummy = llApLink .AND. !USED("APDIST")   .AND. gfOpenFile(gcDataDir+"APDIST","Invvend",'SH')
DO CASE
  CASE lcRpMakBuy = "B"
    =lfUseTmp(llUseCTTm,"M")
    =lfUseTmp(llUsePOTm,"T")
  CASE lcRpMakBuy = "M"
    =lfUseTmp(llUseCTTm,"M")
  CASE lcRpMakBuy = "I"
    =lfUseTmp(llUsePOTm,"T")
ENDCASE
IF llApLink .AND. USED("ApvInvDt")
  USE IN ApvInvDt
  USE IN APDIST
ENDIF

*!*************************************************************
*! Name      : lfColPOMF
*! Developer : AHMED SALAH SHALABY (SSH)
*!      Date : 25/09/2001
*! Purpose   : Function to CutTkt/PO Lines.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfColPOMF()
*!*************************************************************
*!
FUNCTION  lfColPOMF
PARAMETER lc2ColFrm , lcTrNo

PRIVATE llFundTr , llMf
lcLinTmNam = gfTempName()
USE gcDataDir+(lc2ColFrm) AGAIN ALIAS (lcLinTmNam) IN 0 SHARED

llMf = (lc2ColFrm == "mmfgordd")
SELECT (lc2ColFrm)
SET ORDER TO &lc2ColFrm
GO TOP
lcHdrFl   = IIF(llMf,"mmfgordH","POFHDR")
llDummy = IIF(llMf,lfGetLin("T"),lfGetLin("P") .AND. lfGetLin("R"))
USE IN  (lcLinTmNam)

*!*************************************************************
*! Name      : lfSetRel
*! Developer : AHMED SALAH SHALABY (SSH)
*!      Date : 25/09/2001
*! Purpose   : Function to set nessesary relations.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfSetRel()
*!*************************************************************
*!
FUNCTION lfSetRel

SELECT POFHDR
SET RELATION TO Link_Code+'013' INTO Gl_Link
GO TOP
SELECT mmfgordH
SET RELATION TO Link_Code+'013' INTO Gl_Link
GO TOP

*!*************************************************************
*! Name      : lfCutDet
*! Developer : Ahmed Salah Shalaby (SSH)
*! Date      : 
*! Purpose   : Collecting data for either C/T or P/O (Detail Form).
*!*************************************************************
*! Calls     : 
*!*************************************************************
*: Passed Parameters  : lcCtPoT='P or R' (Imported) or 'T' (Manufactured)
*!*************************************************************
*! Example            :  =lfCutDet('T')
*!*************************************************************
*!
FUNCTION lfCutDet
PARAMETER lcCtPoT


*--- lc2ColFrm :: Detail File (CutTktL,OR,POSLN)
*--- lcHdrFl   :: Header File (CutTktH,OR,POFHDR)
*--- lcDetTrNo :: CutTkt# or PO#
SET ORDER TO TAG (lc2ColFrm) IN (lc2ColFrm)
IF BETWEEN(RECNO(lc2ColFrm),1,RECCOUNT(lc2ColFrm))
  GO RECNO(lc2ColFrm) IN (lc2ColFrm)
ENDIF
IF lcCtPoT = "T"
  IF SEEK(lcCtPoT+lcDetTrNo,'cTktBom')
    SELECT cTktBom
    SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = lcCtPoT+lcDetTrNo
      lcType=Typ
      ** fabric or trim.
      llFabTrim=(cCatgTyp='F') OR (cCatgTyp='T' AND Trim_Invt)
      ** Style component
      llStyComp=(cCatgTyp='S')
      lcDesc=IIF(llFabTrim OR llStyComp OR cCatgTyp='T',SUBSTR(Item,1,7)+"      "+Iclr,Desc)
      IF llStyComp
        lcDesc = Item
      ENDIF
      lcItem=Item
      lcIClr=IClr
      lcMfgCode = MfgCode
      ** Issue & Return
      DO CASE
        **Fabric or Trim
        CASE llFabTrim
          SET ORDER TO TAG MatInvJl IN MatInvJl
          IF SEEK(SUBSTR(lcItem,1,7)+lcIClr,"MatInvJl") .AND. SEEK(SUBSTR(lcItem,1,7)+lcIClr,'Fabric')
            SELECT MatInvJl
            SCAN WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession=;
                       SUBSTR(lcItem,1,7)+lcIClr;
                 FOR cTktNo = lcDetTrNo .AND. BETWEEN(dTranDate,ldLowDat,ldHigDat);
                 .AND. (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cGlMatAdj),Gl_Link.GlAcnt,cGlMatAdj));
                     .AND. ALLTRIM(cImTyp) = lcCtPoT &&.AND. !SEEK(lcDetTrNo +SUBSTR(lcItem,1,7)+lcIClr+MATINVJL.cisession+ IIF(MatInvJl.nIssued>0,'I','R'),lcTmpCurs)
              *--- BOMCOST
              *--- cbomtype+cimtyp+ctktno+item+iclr+mfgcode+cwarecode+cdyelot+crsession+cisession
              SELECT BOMCOST
              *=SEEK(lcType+lcCtPoT+lcDetTrNo+SUBSTR(lcItem,1,19)+lcIClr)
              LOCATE FOR cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+;
                         coprcode+clotno+cisession+crsession=  lcCtPoT+lcDetTrNo AND cisession = MATINVJL.cisession
                IF (FOUND() AND cTktBom.typ = BOMCOST.cBomType) OR !FOUND()
                 SELECT &lcRpTmpFil
                 APPEND BLANK 
                 REPLACE WIPAcc   WITH IIF(EMPTY(MatInvJl.cGlMatAdj),Gl_Link.GlAcnt,MatInvJl.cGlMatAdj);
                         CtPoNo   WITH lcDetTrNo,;
                         Type     WITH lcCtPoT,;
                         Desc     WITH lcDesc,;
                         cType    WITH lcType,;
                         TranDate WITH MatInvJl.dTranDate,;
                         TranType WITH IIF(MatInvJl.nIssued>0,'I','R'),;
                         Qty      WITH IIF(MatInvJl.cTranType='4' AND TranType='R',-1*MatInvJl.nReceived,MatInvJl.nIssued),;
                         Cost     WITH IIF(MatInvJl.nunitcost<>0,MatInvJl.nunitcost,MatInvJl.nuntcstbuy/IIF(Fabric.Conv=0,1,Fabric.Conv)),;
                         llFabric WITH .T.

                        *B121886,1 MHM 03/24/2004 Add vendor code and name in case of MAterial PO [BEGIN]
                        REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                                cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
                        *B121886,1 [End]
                         
                SELECT (lcTmpCurs)
                *--- CtPoNo+Item +Clr+cSession
                APPEND BLANK
                REPLACE CtPoNo   WITH lcDetTrNo,;
                        Item     WITH SUBSTR(lcItem,1,7),;
                        Clr      WITH lcIClr,;
                        cSession WITH MATINVJL.cisession,;
                        lcIssRet WITH IIF(MatInvJl.nIssued>0,'I','R')
              ENDIF
            ENDSCAN
          ENDIF
        ** Style Component
        CASE llStyComp
          *--- MAN Correct?
          SELECT StyInvJl
          =SEEK(lcItem)
          SCAN REST WHILE Style=lcItem ;
               FOR CTRCode=lcDetTrNo AND BETWEEN(dTrDate,ldLowDat,ldHigDat);
                 .AND. (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cAdjAcct),Gl_Link.GlAcnt,cAdjAcct))
            SELECT &lcRpTmpFil
            APPEND BLANK 
            REPLACE WIPAcc   WITH IIF(EMPTY(StyinvJl.cAdjAcct),Gl_Link.GlAcnt,StyinvJl.cAdjAcct),;
                    CtPoNo   WITH lcDetTrNo,;
                    Type     WITH lcCtPoT,;
                    Desc     WITH lcDesc,;
                    cType    WITH lcType,;
                    TranDate WITH StyInvJl.dTrDate,;
                    TranType WITH StyInvJl.cIRType,;
                    Qty      WITH -1*StyInvJl.nTotStk,;
                    Cost     WITH StyInvJl.nCost
                    *B121886,1 MHM 03/24/2004 Add vendor code and name in case of MAterial PO [BEGIN]
                    REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                            cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
                    *B121886,1 [End]
                         
                    
          ENDSCAN
      ENDCASE   
      ** Receive
      SELECT (lcLinTmNam)
      SET ORDER TO Mmfgordd
      SELECT (lcLinTmNam)
      =SEEK(lcDetTrNo))
      SCAN REST WHILE cmfgordno+cfabric+color+dyelot+trancd = lcDetTrNo;
           FOR TranCd='2'
        *B120322,3 MHM - the activity date range filter should be Between the from and to date.  [Begin]
        IF !(BETWEEN(&lcLinTmNam..drecvdate,ldLowDat,ldHigDat))
          Loop
        ENDIF
        *B120322,3  [End]
           
        lcRSession = cRSession
        lcMainItem = PADR(cFabric,19)
        lcMainFab  = PADR(Color,6)
        SELECT BomLine
        *--- cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
        =SEEK(lcCtPoT+'2'+lcDetTrNo)
        SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                   lcCtPoT+'2'+lcDetTrNo ;
             FOR cBomTyp+Style+sClr+Item+Iclr+MfgCode+cRSession=;
                 lcType+lcMainItem+lcMainFab+lcItem+lcIClr+lcMfgCode+lcRSession
          SELECT &lcRpTmpFil
          APPEND BLANK 
          REPLACE WIPAcc   WITH IIF(EMPTY(lcGlCode),Gl_Link.GlAcnt,lcGlAcnt),;
                  CtPoNo   WITH lcDetTrNo,;
                  Type     WITH lcCtPoT,;
                  Desc     WITH lcDesc,;
                  cType    WITH BomLine.CBomTyp,;
                  TranDate WITH &lcLinTmNam..dRecvDate,;
                  TranType WITH 'E',;
                  Qty      WITH -1*(BomLine.ItemQty),;
                  Cost     WITH IIF(lcCtPoT='M',BomLine.UnitCost,IIF(PoFHdr.Rate<>0 AND BomLine.cCatgTyp='P',BomLine.UnitCost/PoFHdr.Rate,BomLine.UnitCost))
                  
                  *B121886,1 MHM 03/24/2004 Add vendor code and name in case of MAterial PO [BEGIN]
                  REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                          cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
                  *B121886,1 [End]
                         
                  
          
          *B119523,1 ALB Display all costs with base currency [BEGIN]
          IF lcCtPoT='M'
            lnEqCose = BomLine.UnitCost
          ELSE
            IF BomLine.cCatgTyp='P'
              lcExSin2 = ''
              lcExSin1 = gfGetExSin(@lcExSin2,PoSHdr.CPriceCur)
              lnEqCose = BomLine.UnitCost &lcExSin1. PoSHdr.nPricerat
            ELSE
              IF BomLine.cCatgTyp='D'
                lcExSin2 = ''
                lcExSin1 = gfGetExSin(@lcExSin2,PoFHdr.CDutyCur)
                lnEqCose = BomLine.UnitCost &lcExSin1. PoFHdr.nDutyrat
              ELSE
                lnEqCose = BomLine.UnitCost
              ENDIF
            ENDIF
          ENDIF
          REPLACE Cost WITH lnEqCose
          *B119523,1 ALB Display all costs with base currency [END]

        ENDSCAN
      ENDSCAN
    ENDSCAN
    SELECT BomCost
    SET ORDER TO Pobomcls
    *--- cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession
    =SEEK(lcCtPoT+lcDetTrNo)
    IF BETWEEN(RECNO(lc2ColFrm),1,RECCOUNT(lc2ColFrm))
      GO RECNO(lc2ColFrm) IN (lc2ColFrm)
    ENDIF
    SCAN REST WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession=;
               lcCtPoT+lcDetTrNo;
         FOR   BETWEEN(dTranDate,ldLowDat,ldHigDat) .AND. cCostType$"MPD";
         AND  (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cWipAcnt),Gl_Link.GlAcnt,cWipAcnt))
      lcDesc = IIF(SEEK(lcCtPoT+lcDetTrNo+cBomType+SPACE(19)+SPACE(06)+mfgCode+cDyelot,"CTKTBOM"),CTKTBOM.Desc,"")
      *--- cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot
      SELECT &lcRpTmpFil
      APPEND BLANK
      REPLACE WIPAcc   WITH IIF(EMPTY(BOMCOST.cWipAcnt),Gl_Link.GlAcnt,BOMCOST.cWipAcnt),;
              CtPoNo   WITH lcDetTrNo,;
              Type     WITH lcCtPoT,;
              cType    WITH BOMCOST.cbomtype,;
              Desc     WITH lcDesc,;
              TranDate WITH BomCost.dTranDate,;
              Qty      WITH BomCost.nTotQty,;
              Cost     WITH BomCost.nTotCst/BomCost.nTotQty,;
              TranType WITH 'V'
              
              *B121886,1 MHM 03/24/2004 Add vendor code and name in case of MAterial PO [BEGIN]
              REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                      cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
              *B121886,1 [End]
                         
              
      *B119523,1 ALB Display all costs with base currency [BEGIN]
      IF lcCtPoT='M'
        lnEqCose = IIF(BomCost.nTotQty<>0,BomCost.nTotCst/BomCost.nTotQty,0)
      ELSE
        IF BomCost.cCostType='P'
          lcExSin2 = ''
          lcExSin1 = gfGetExSin(@lcExSin2,PoSHdr.CPriceCur)
          lnEqCose = (BomCost.nTotCst &lcExSin1. PoSHdr.nPricerat) / BomCost.nTotQty
        ELSE
          IF BomCost.cCostType='D'
            lcExSin2 = ''
            lcExSin1 = gfGetExSin(@lcExSin2,PoSHdr.CDutyCur)
            lnEqCose = (BomCost.nTotCst &lcExSin1. PoSHdr.nDutyrat) / BomCost.nTotQty
          ELSE
            lnEqCose = IIF(BomCost.nTotQty<>0,BomCost.nTotCst/BomCost.nTotQty,0)
          ENDIF
        ENDIF
      ENDIF
      REPLACE Cost WITH lnEqCose
      *B119523,1 ALB Display all costs with base currency [END]

    ENDSCAN
  ENDIF
ELSE  && Case Material Purchased Order and Return Material Purchased Order.
  =lfGetRc(lcDetTrNo,lcCtPoT)
  *--- Functoin to get data from ApvInvDt.DBF
  =lfGetApInv(IIF(lcCtPoT= "P","L","F")) && L = Mat Po Order
                                         && R = Return Mat Po Order
ENDIF
SELECT (lc2ColFrm)
RETURN

*!*************************************************************
*! Name      : lfStatus
*! Developer : AHMED SALAH SHALABY (SSH)
*!      Date : 25/09/2001
*! Purpose   : Get the C/T or P/O status to be printed in the report.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfStatus()
*!*************************************************************
*!
FUNCTION lfStatus
PARAMETER lcStatus
DO CASE
  CASE lcStatus='A'
    RETURN('Actualize')
  CASE lcStatus='C'
    RETURN('Complete')
  CASE lcStatus='H'
    RETURN('Hold')
  CASE lcStatus='O'
    RETURN('Open')
  CASE lcStatus$'SL'
    RETURN('Close')
  CASE lcStatus='X'
    RETURN('Cancel')
  OTHER
    RETURN('')
ENDCASE  



*!*************************************************************
*! Name      : lfvStyle
*! Developer : AHMED SALAH SHALABY (SSH)
*!      Date : 25/09/2001
*! Purpose   : Validation function for Validating Style Code
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvStyle()
*!*************************************************************
*!
FUNCTION lfvStyle

lcStyle = VARREAD()
IF EMPTY(EVAL(lcStyle))
  RETURN
ENDIF

lcTag = ORDER('STYLE')

SET ORDER TO cStyle IN STYLE

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(&lcStyle.,'Style') 
    &lcStyle = STYLE.cStyMajor
  ELSE
    &lcStyle = gfStyBrw('M',"","",.F.)
  ENDIF
ELSE
  &lcStyle = ''
ENDIF

SET ORDER TO lcTag IN STYLE


*!*************************************************************
*! Name      : lfvFabric
*! Developer : AHMED SALAH SHALABY (SSH)
*!      Date : 25/09/2001
*! Purpose   : Validation function for Validating fabric Code
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ...
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvFabric()
*!*************************************************************
*!
FUNCTION lfvFabric

lcFabObj = VARREAD()
lcFab    = &lcFabObj
IF LASTKEY() = 13 AND !MDOWN() .OR.  !EMPTY(lcFab)
  SELECT FABRIC
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ENDIF
SET ORDER TO FABRIC IN FABRIC


*!*************************************************************
*! Name      : lfNonMaj
*! Developer : Ahmed Salah (SSH)
*! Date      : 25/09/2001
*! Purpose   : To get the style nonmajor segement structure
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfNonMaj()
*!*************************************************************
*!
FUNCTION lfNonMaj

*-- Compute Free/Color Items in Style Structure. [Begin]
lnMajSeg  = gfItemMask('SM')  && No. of major segments.
DIMENSION laMajSeg[1,1]
= gfItemMask(@laMajSeg)

llStopConc = .F.

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSeg,1)

  lnNonMajPo = IIF(lnNonMajPo = 0,laMajSeg[lnI,4],lnNonMajPo)

  IF laMajSeg[lnI,1] = 'F' AND !llStopConc
  
    lcFreeClr  = IIF(EMPTY(lcFreeClr),laMajSeg[lnI,1],lcFreeClr)

    lcNonMajPi = IIF(EMPTY(lcNonMajPi),laMajSeg[lnI,3],;
                     lcNonMajPi + laMajSeg[lnI-1,6] + laMajSeg[lnI,3])

    lcNonMajT  = IIF(EMPTY(lcNonMajT),PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])),;
                     lcNonMajT + laMajSeg[lnI-1,6] + PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3])))

  ENDIF

  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSeg[lnI,1] = 'C' OR (!EMPTY(lcFreeClr) AND laMajSeg[lnI,1] != 'F')

    IF laMajSeg[lnI,1] = 'C'

      lnClrPo    = laMajSeg[lnI,4]

      lcFreeClr  = laMajSeg[lnI,1]    && which will be 'C'
  
      lcNonMajPi = laMajSeg[lnI,3]

      lcNonMajT  = PADR(laMajSeg[lnI,2],LEN(laMajSeg[lnI,3]))
  
      EXIT
  
    ELSE
      
      *-- this means that another type is found rather than color or free
      *-- and so we neednot to concat. to free variables
      llStopConc = .T.
      
    ENDIF

  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.

ENDFOR    && end Loop Around Non Major elements.

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTt = 'Only This ' + ALLTRIM(lcNonMajT)
*-- Compute Free/Color Items in Style Structure. [End]

RETURN ''

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AAMER (AHM)
*! Date      : 25/09/2001
*! Purpose   : Option Grid When function
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
*!
FUNCTION lfwOGWhen

*-- Check the cost access

lnDatapos = ASCAN(laOGFxFlt,'POFHDR.ENTERED')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  lcStr = ALLTRIM(DTOC(gdSysDate - DAY(gdSysDate)+1)) + '|' + ALLTRIM(DTOC(gdSysDate))
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDatapos,6] = lcStr
  ENDIF
ENDIF
DECLARE laSetups[4,2]

laSetups[1,1]  = "M_CTSLBL1"
laSetups[2,1]  = "M_CTSLBL2"
laSetups[3,1]  = "M_CTSLBL3"
laSetups[4,1]  = "M_CTSLBL4"

=gfGetMemVar(@laSetups,gcAct_Comp)
FOR lnInd = 1 TO 4
  lcInd = STR(lnInd,1)
  lcTDesc&lcInd = laSetups[lnInd,2]
ENDFOR
STORE "Merch."  TO lcPDesc1 , lcRDesc1
STORE "Freight" TO lcPDesc2 , lcRDesc2
STORE "Tax"     TO lcPDesc3 , lcRDesc3
STORE "Quota"   TO lcPDesc4 , lcRDesc4
*-- end of lfwOGWhen

*!*************************************************************
*! Name      : lfvLnkCod
*! Developer : Ahmed Salah (SSH)
*! Date      : 25/09/20001
*! Purpose   : Validate link code
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfvLnkCod()
*!*************************************************************
*!
FUNCTION lfvLnkCod

SET ORDER TO Gl_link1 IN GL_Link
lcCodeVar = VARREAD()
lcCodeVal = &lcCodeVar
IF !EMPTY(lcCodeVal)
  SELECT GL_Link
  *--- linktype+link_code
  IF !SEEK("05"+ALLTRIM(lcCodeVal)))
    llNothing  = lfGLLinkBrw()
  ELSE
    llNothing = .T.
  ENDIF
  &lcCodeVar = IIF(llNothing,GL_LINK.Link_Code,'')
  SET ORDER TO Gl_link IN GL_Link
  IF SEEK(&lcCodeVar+'013')
    lcRpGlAct = GL_Link.GlAcnt
  ELSE
    lcRpGlAct = SPACE(3)
  ENDIF
ENDIF  

*!*************************************************************
*! Name      : lfGLLinkBrw
*! Developer : Ahmed Salah Shalaby SSH
*! Date      : 25/09/20001
*! Purpose   : Browse link codes
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfGLLinkBrw()
*!*************************************************************
*!
FUNCTION lfGLLinkBrw

PRIVATE lcFields,laBrow,lnCurAlias,lcCurTag,llReturn,lcTag,lcBrFields,lcFile_Ttl
DIMENSION laBrow[1]
STORE SPACE(0) TO lcFields,laBrow
llReturn = .F.

lnCurAlias = SELECT(0)
lcTag      = ORDER()

lcFields    = "link_code"

lcBrFields = "Link_Code    :H='Link code',"+;
             "Description=GL_LINK.LinkDesc :H='Description'"

lcFile_Ttl  = 'Link codes'
lc2Sek = '05'
SELECT GL_LINK
SET ORDER TO Gl_link1
LOCATE
DECLARE laTemp[1]

llReturn  = gfBrows('lc2Sek', 'LinkType,link_code', 'laTemp')

SELECT(lnCurAlias)
SET ORDER TO (lcTag)
RETURN llReturn

*!*************************************************************
*! Name      : lfCretFltr
*! Developer : AHMED SALAH SHALABY (SSH)
*!      Date : 25/09/2001
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
*! Example     : = lfCretFltr()
*!*************************************************************
FUNCTION lfCretFltr
FOR lnInd = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'FABRIC.FABRIC' .AND. ALLTRIM(laOgFxFlt[lnInd,6]) <> "|" .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Fabric.
      lcMMFlt = lcMMFlt +" .AND. "+IIF(TYPE("cFABRIC")="C", "cFabric $ ["+ALLTRIM(laOgFxFlt[lnInd,6])+"]","Fabric $ ["+ALLTRIM(laOgFxFlt[lnInd,6])+"]")
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = "FABRIC.COLOR" .AND. ALLTRIM(laOgFxFlt[lnInd,6]) <> "|" .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Fabric Color
      lcMMFlt = lcMMFlt +" .AND. Color $ ["+laOgFxFlt[lnInd,6]+"]"
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'POFHDR.ENTERED' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Dates Range
      ldLowDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldHigDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
    
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'POFHDR.POMAT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6])) .AND. USED(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcSeltdPo = ALLTRIM(laOgFxFlt[lnInd,6])

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'MMFGORDH.CMFGORDNO' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6])) .AND. USED(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcSeltdMF = ALLTRIM(laOgFxFlt[lnInd,6])
  ENDCASE
ENDFOR

*!*************************************************************
*! Name      : lfChkSty
*! Developer : Ahmed Salah (SSH)
*! Date      : 25/09/20001
*! Purpose   : Check Style validity.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfChkSty()
*!*************************************************************
*!
FUNCTION lfChkSty
PARAMETER lcSty2Chk , llSeason


PRIVATE llContinuo , lnOldAls
llContinuo = .F.
IF lcSeason <> ".T."
  lcSeason = "Style"+SUBSTR(lcSeason,AT('.',lcSeason))
ENDIF
IF SEEK(lcSty2Chk,"Style")
  lnOldAls = SELECT(0)
  SELECT STYLE
  LOCATE REST WHILE Style = lcSty2Chk;
              FOR &lcWhilCon .AND. &lcStyFlt .AND. &lcSeason
  llContinuo = FOUND()
  SELECT(lnOldAls)
ENDIF
RETURN(llContinuo)

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : Ahemd Salaha Shalaby.
*! Date      : 08/23/98
*! Purpose   : To set relation on or off when running the in range function 
*!             of selecting PO in the option grid.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvTrans()
*!*************************************************************
FUNCTION lfsrvTrans
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE

*!*************************************************************
*! Name      : lfClrRed
*! Developer : Ahmed Salaha Shalaby
*! Date      : 08/23/98
*! Purpose   : Function to clear read in the option gris.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfClrRed()
*!*************************************************************
*!
FUNCTION lfClrRed

CLEAR READ


*!*************************************************************
*! Name      : lfUseTmp
*! Developer : Ahmed Salaha Shalaby
*! Date      : 08/23/98
*! Purpose   : Function to colect data from tempfiels or maser header 
*!           : file.
*!*************************************************************
*! Called from : Option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfUseTmp()
*!*************************************************************
*!
FUNCTION lfUseTmp
PARAMETER llUseTmp,lcCTPO
PRIVATE lnOldAls
lnOldAls = SELECT(0)

IF lcCTPO = "M"  && IF calling to select MFG MAterial.
  IF llUseTmp    && IF we are going to colect data for selected CT in
                 && temp file lcSeltdMF that created from OG.
    SELECT (lcSeltdMF)
    SCAN
      *-- Send the second Parameter to colect data for specific CT.
      =lfColPOMF("mmfgordd",cMfgOrdNo)
    ENDSCAN
  ELSE
    *-- Send second parameter empty to colect data for all CT.
    =lfColPOMF("mmfgordd","")
  ENDIF
ELSE  && IF calling to select PO.
  IF llUseTmp  && IF we are going to colect data for selected PO in
               && temp file lcSeltdMF that created from OG.
    SELECT (lcSeltdPO)
    SCAN
      *-- Send the second Parameter to colect data for specific PO.
      =lfColPOMF("PoFLN",POMAT)
    ENDSCAN
  ELSE
    *-- Send second parameter empty to colect data for all PO.
    =lfColPOMF("PoFLN","")
  ENDIF
ENDIF
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfSRVPo
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 03/09/1999
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfModalGen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfSRVPo()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
FUNCTION lfSRVPo
PARAMETERS lcParm
PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO Vencode IN APVENDOR
    SELECT POFHDR
    SET ORDER TO TAG POFHDR
    SET RELATION TO VENDOR INTO APVENDOR
    GO TOP IN POFHDR
  CASE lcParm = 'R'  && Reset code
    SELECT POFHDR
    SET RELATION TO 
    SET KEY TO
ENDCASE
*-- end of lfSRVPo


FUNCTION lfGetLin
PARAMETER lcPORET

*--- mmfgordH  : cMfgOrdNo.
*--- POFHDR    : cMatType + PoMat.
llFundTr  = IIF(llMf,SEEK(lcTrNo),SEEK(lcPORET+lcTrNo))
lcTrWhCnd = IIF(llMf,"cMfgOrdNo","cMatType+PoMat")
SELECT (lc2ColFrm)
IF llFundTr
  SELECT (lcHdrFl)
  =IIF(llMf,SEEK(lcTrNo),SEEK(lcPORET+lcTrNo))
  *B120322,1 ABD - the activity date range filter should be Between the from and to date.  [Begin]
  *SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,lcPORET+lcTrNo);
  *          FOR IIF(lcCtPoSt='A',.T.,;
  *          IIF(!llMf .AND. lcCtPoSt="S",&lcHdrFl..Status="L",&lcHdrFl..Status=lcCtPoSt)) .AND. ;
  *              &lcHdrFl..Entered >= ldLowDat .AND.;
  *              !EMPTY(&lcHdrFl..Link_Code) .AND. ;
  *            IIF(!EMPTY(lcGlCode),&lcHdrFl..Link_Code=lcGlCode,.T.)

  *B120322,3 MHM - the activity date range filter should be Between the from and to date.  [Begin]
  *SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,lcPORET+lcTrNo);
  *          FOR IIF(lcCtPoSt='A',.T.,;
  *          IIF(!llMf .AND. lcCtPoSt="S",&lcHdrFl..Status="L",&lcHdrFl..Status=lcCtPoSt)) .AND. ;
  *             BETWEEN(&lcHdrFl..Entered,ldLowDat,ldHigDat) .AND.;
  *              !EMPTY(&lcHdrFl..Link_Code) .AND. ;
  *            IIF(!EMPTY(lcGlCode),&lcHdrFl..Link_Code=lcGlCode,.T.)
  SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,lcPORET+lcTrNo);
            FOR IIF(lcCtPoSt='A',.T.,;
            IIF(!llMf .AND. lcCtPoSt="S",&lcHdrFl..Status="L",&lcHdrFl..Status=lcCtPoSt)) .AND. ;
                !EMPTY(&lcHdrFl..Link_Code) .AND. ;
              IIF(!EMPTY(lcGlCode),&lcHdrFl..Link_Code=lcGlCode,.T.)
  *B120322,3 MHM -  [Begin]

  *B120322,1 ABD - [End]
    lcDetTrNo = IIF(llMf,cMfgOrdNo,PoMat)
    SELECT (lc2ColFrm)
    =IIF(llMf,SEEK(lcDetTrNo),SEEK(lcPORET+lcDetTrNo))
    SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcDetTrNo,lcPORET+lcDetTrNo);
              FOR &lcMMFlt

      *B121886,1  MHM - the activity date range filter should be Between the from and to date.  [Begin]
      *IF !(IIF(UPPER(lc2ColFrm) ="POFLN" , BETWEEN(&lc2ColFrm..dPostDate,ldLowDat,ldHigDat),;
      *  BETWEEN(&lc2ColFrm..drecvdate,ldLowDat,ldHigDat)) .AND. TranCd $"23")
      * Loop
      *ENDIF
      *B121886,1   [End]
      *--- Case detail Form
      lcDetTrNo = IIF(llMf,cMfgOrdNo,PoMat)
      SELECT &lcRpTmpFil
      WAIT WINDOW "Collecting data for transaction# "+lcDetTrNo  NOWAIT
      LOCATE FOR Type+CtPoNo = IIF(llMf,"T",lcPORET)+lcDetTrNo
      IF !FOUND()
        *--- Colect Detail transactions.
        =lfCutDet(IIF(llMf,"T",lcPORET))
        *B122289,1 NNA 05/04/2004 (Begin) Get Record No of Pofln when the transCD ='1'
        lnrecNo =RECNO('POFLN')
        *B122289,1 NNA (End)
      ENDIF
      
      *B121886,1  MHM - the activity date range filter should be Between the from and to date.  [Begin]
      SELECT (lc2ColFrm)

      *B122289,1 NNA 05/04/2004 (Begin) move this code to another Position to get the Closed Record
      *IF !(IIF(UPPER(lc2ColFrm) ="POFLN" , BETWEEN(&lc2ColFrm..dPostDate,ldLowDat,ldHigDat),;
      *  BETWEEN(&lc2ColFrm..drecvdate,ldLowDat,ldHigDat)) .AND. TranCd $"234")
      *  LOOP
      *ENDIF
      *B122289,1 NNA (End)
      *B121886,1  [End]

      *--- Budget record for Closed Transaction (PO,CT)

      *B122289,1 NNA 05/04/2004 (Begin) if the PO status is Closed and we Print the Material PO
      *B122289,1 NNA             Report ,Seek for TranCD = '1' Because the Pointer Moved in 
      *B122289,1 NNA             function (lfGetRc)
      *IF &lcHdrFl..Status $ 'LS' AND &lc2ColFrm..TranCd = '1'
      IF &lcHdrFl..Status $ 'LS' AND IIF(llMf , &lc2ColFrm..TranCd = '1' , ;
         IIF(SEEK(lcPORET+lcDetTrNo) ,&lc2ColFrm..TranCd = '1',.F.))
      *B122289,1 NNA (End)
       
        PRIVATE lnTmInd , lcTmInd , lnCloseQty
        lnCloseQty = 0
        FOR lnTmInd = 1 TO 4
          lcTmInd = STR(lnTmInd,1)
          *B119523,1 ALB Display all costs with base currency [BEGIN]
          *lnCloseQty = lnCloseQty+(&lcHdrFl..nAct_Cost&lcTmInd - &lcHdrFl..nLan_Cost&lcTmInd)
          lnCloseQty = lnCloseQty+(&lcHdrFl..neActCost&lcTmInd - &lcHdrFl..neLanCost&lcTmInd)
          *B119523,1 ALB Display all costs with base currency [END]
        ENDFOR
        SELECT &lcRpTmpFil
        LOCATE FOR CtPoNo = lcDetTrNo AND Type = IIF(llMf,"T",lcPORET) AND TranType = 'S'
        IF !FOUND()
          SELECT (lcTmpNam)
          SET ORDER TO GlDistNo DESCENDING
          
          *B122289,1 NNA 05/04/2004 (Begin) Search For the Closed Record in the GlDist File
          *B122289,1 NNA             and Compare between the Trans. Date in this file and the 
          *B122289,1 NNA             Date Range(From the OG)
          *ldClDate = IIF(SEEK(lcDetTrNo,lcTmpNam),&lcTmpNam..Tran_Date,{})
          ldClDate = IIF(SEEK(lcDetTrNo + 'MC',lcTmpNam),&lcTmpNam..Tran_Date,{})
          IF BETWEEN(ldClDate,ldLowDat,ldHigDat)
          *B122289,1 NNA (End)
          
            SELECT &lcRpTmpFil
            APPEND BLANK
            REPLACE WIPAcc   WITH IIF(EMPTY(lcGlCode),Gl_Link.GlAcnt,lcGlAcnt),;
                  CtPoNo   WITH lcDetTrNo,;
                  Type     WITH IIF(llMf,"T",lcPORET),;
                  Desc     WITH ' ',;
                  cType    WITH '9',;
                  TranDate WITH ldClDate,;
                  TranType WITH 'S',;
                  Qty      WITH 1,;
                  Cost     WITH lnCloseQty*-1
          ENDIF
        ENDIF
      ENDIF
      *B122289,1 NNA 05/04/2004 (Begin) Return to previous Record in the Pofln to Do the Code that
      *B122289,1 NNA             moved Before
      IF !EOF()
        GOTO IIF(lnRecNo > RECCOUNT('POFLN'),RECCOUNT('POFLN'),lnRecNo) IN POFLN
      ELSE
        GOTO RECCOUNT('POFLN') IN POFLN
      ENDIF
      IF !(IIF(UPPER(lc2ColFrm) ="POFLN" , BETWEEN(&lc2ColFrm..dPostDate,ldLowDat,ldHigDat),;
        BETWEEN(&lc2ColFrm..drecvdate,ldLowDat,ldHigDat)) .AND. TranCd $"234")
        LOOP
      ENDIF
      *B122289,1 NNA 05/04/2004 (Begin)

    ENDSCAN
  ENDSCAN
ENDIF
RETURN

*******************************
FUNCTION lfGetApInv
PARAMETER lcPoRet
*******************************

SELECT ApvInvDt
*--- cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot
IF SEEK(lcPoRet+lcDetTrNo)
  SCAN REST WHILE cimtyp+ctktno+clotno+STR(lineno,6)+cbomtype+coprcode+item+color+cdyelot=;
                  lcPoRet+lcDetTrNo;
            FOR (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cAPDGlAct),Gl_Link.GlAcnt,cAPDGlAct))
    *--- 
    SELECT APDIST
    *--- cinvno+cvendcode+capdtrtyp
    =SEEK(ApvInvDt.cApInvNo+ApvInvDt.cvendcode)
    LOCATE REST WHILE cinvno+cvendcode+capdtrtyp = ApvInvDt.cApInvNo+ApvInvDt.cvendcode;
                FOR capdActId = "D" .AND. ALLTRIM(STR(nAPdLinNo)) == ALLTRIM(ApvInvDt.cApviLno)

    *B120322,3 MHM - the activity date range filter should be Between the from and to date.  [Begin]
    IF !(BETWEEN(APDIST.dapdtrdat,ldLowDat,ldHigDat))
      Loop
    ENDIF
    *B120322,3  [End]
                
    SELECT &lcRpTmpFil
    APPEND BLANK
    *B121886,1 MHM 03/24/2004 Add vendor code and name in case of MAterial PO [BEGIN]
    *REPLACE WIPAcc   WITH IIF(EMPTY(ApvInvDt.cAPDGlAct),Gl_Link.GlAcnt,ApvInvDt.cAPDGlAct),;
            CtPoNo   WITH lcDetTrNo,;
            Type     WITH lcCtPoT,;
            cType    WITH ApvInvDt.cbomtype,;
            Desc     WITH "",;
            TranDate WITH APDIST.dApDtrDat,;
            Qty      WITH ApvInvDt.nApTotQty,;
            Cost     WITH ApvInvDt.nApPrice,;
            TranType WITH 'V'
    
    REPLACE WIPAcc   WITH IIF(EMPTY(ApvInvDt.cAPDGlAct),Gl_Link.GlAcnt,ApvInvDt.cAPDGlAct),;
            CtPoNo   WITH lcDetTrNo,;
            Type     WITH lcCtPoT,;
            cType    WITH ApvInvDt.cbomtype,;
            Desc     WITH ALLTRIM(ApvInvDt.cApInvNo) + '  ' + ALLTRIM(ApvInvDt.cVendCode),;
            TranDate WITH APDIST.dApDtrDat,;
            Qty      WITH ApvInvDt.nApTotQty,;
            Cost     WITH ApvInvDt.nApPrice,;
            TranType WITH 'V'
            REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                    cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
            *B121886,1 [End]
                         
            
    *B119523,1 ALB Display all costs with base currency [BEGIN]
    REPLACE Cost WITH APDIST.NEqvAmnt / Qty
    *B119523,1 ALB Display all costs with base currency [END]
  ENDSCAN
ENDIF


FUNCTION lfGetRc
PARAMETER lcDetTrNo , lcCtPoT

** Receive
SET ORDER TO TAG (lc2ColFrm) IN (lc2ColFrm)
IF BETWEEN(RECNO("POFLN"),1,RECCOUNT("POFLN"))
  GO RECNO("POFLN") IN POFLN
ENDIF
SELECT POFLN
SET ORDER TO POFLN
SELECT POFLN
=SEEK(lcCtPoT+lcDetTrNo)
*--- cmattype+pomat+fabric+color+trancd
SCAN REST WHILE cmattype+pomat+fabric+color+trancd = lcCtPoT+lcDetTrNo;
          FOR TranCd='2'
    *B120322,3 MHM - the activity date range filter should be Between the from and to date.  [Begin]
    *IF !(BETWEEN(POFLN.dPostDate,ldLowDat,ldHigDat))
    *  Loop
    *ENDIF
    *B120322,3  [End]

    SELECT &lcRpTmpFil
    FOR lnInd = 1 TO 4
      lcInde = STR(lnInd,1)

      *B122289,1 NNA 05/04/2004 (Begin) Search too for the Trans. Date that matches the date Range
      *IF POFLN.nLan_Cost&lcInde <> 0
      IF POFLN.nLan_Cost&lcInde <> 0 .AND. BETWEEN(POFLN.dPostDate,ldLowDat,ldHigDat))
      *B122289,1 NNA (End)

        APPEND BLANK 
        *B120322,1 ABD - change the return description into the report and print the return with Positive Qty.  [Begin]
        *REPLACE WIPAcc   WITH IIF(EMPTY(lcGlCode),Gl_Link.GlAcnt,lcGlAcnt),;
        *        CtPoNo   WITH lcDetTrNo,;
        *        Type     WITH lcCtPoT,;
        *        Desc     WITH POFLN.fabric+POFLN.color,;
        *        cType    WITH STR(lnInd,1),;
        *        TranDate WITH POFLN.dPostDate,;
        *        TranType WITH 'E',;
        *        Qty      WITH -1*POFLN.nFabTotQty,;
        *        Cost     WITH POFLN.nLan_Cost&lcInde
        **B119523,1 ALB Display all costs with base currency [BEGIN]
        *REPLACE Cost  WITH POFLN.neLanCost&lcInde
        **B119523,1 ALB Display all costs with base currency [END]
        REPLACE WIPAcc   WITH IIF(EMPTY(lcGlCode),Gl_Link.GlAcnt,lcGlAcnt),;
                CtPoNo   WITH lcDetTrNo,;
                Type     WITH lcCtPoT,;
                Desc     WITH POFLN.fabric+POFLN.color,;
                cType    WITH STR(lnInd,1),;
                TranDate WITH POFLN.dPostDate,;
                TranType WITH IIF(POFLN.cmattype = 'R',' ','E'),;
                Qty      WITH IIF(POFLN.cmattype = 'R',POFLN.nFabTotQty,-1*POFLN.nFabTotQty),;
                Cost     WITH POFLN.neLanCost&lcInde
        *B121886,1 MHM 03/24/2004 Add vendor code and name in case of MAterial PO [BEGIN]
        REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
        *B121886,1 [End]
                         
        
        *B120322,1 ABD - [End]
      ENDIF
    ENDFOR
ENDSCAN

*!*************************************************************
*! Name      : lfGetVend
*! Developer : Mohamed Shokry (MHM)
*! Date      : 03/24/04
*! Purpose   : Function to return Vendor Name from vendor file
*!           : 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfGetVend()
*!*************************************************************
*!B121886,1

FUNCTION lfGetVend
PARAMETER lcVendCode

PRIVATE lcOldAlias,lcVendName
lcVendName = ''
lcOldAlias = ALIAS()
IF !USED("APVENDOR")
  =gfOpenFile(gcDataDir+'APVENDOR',gcDataDir+'VENCODE','SH')
ENDIF

SELECT APVENDOR
SET ORDER TO VENCODE
IF SEEK(lcVendCode)
  lcVendName = APVENDOR.cVenComp
ENDIF

SELECT (lcOldAlias)
RETURN lcVendName
