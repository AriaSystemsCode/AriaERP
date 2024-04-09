*:--------------------------------------------------------------------------
*: Program file        : MFWIPAC.PRG
*: Program description : GL-WIP Account Report.
*: For screen          : None.
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : MF,PO and MA
*: Developer Name      : Ahmed Salah Shalaby - (SSH)
*: Tracking Job Number : E301693
*: Date                : 01st of September 2001
*:--------------------------------------------------------------------------
*: Calls               : MFWIPAD.FRX.  && Detail Form.
*:--------------------------------------------------------------------------
*: Called From         : System Menu
*:--------------------------------------------------------------------------
*: Passed Parameters   : None.
*:--------------------------------------------------------------------------
*: Example             : DO MFWIPAC
*:--------------------------------------------------------------------------
*: Modificatoins       :
*B605483,1 KHM 02/07/2002 Changing the number of decimals to be 3 instead of 7.
*B606047,1 KHM 05/29/2002 Fix the bug of numeric over flow.
*B606067,1 KHM 06/05/2002 Fix the problem of alias CutTktH file not found in case
*B606067,1                of MF not installed.
*B606799,1 KHM 01/27/2003 Adding the activity date range to the 
*B607379,1 ALB 08/06/2003 Display all costs with base currency
*E302206,1 ALB 08/20/2003 Add vendor code and name in case of PO
*B120321,1 ABD 12/08/2003 Fix bug that the activity date range filter is not working. 
*B120321,3 MHM 12/31/2003 Fix bug that the activity date range filter is not working. 
*B122242,1 MHM 03/24/2004 Fix bug of the report does not report Material Issues . 
*B122296,1 NNA 04/27/2004 Fix bug that the report not reporting the 'Close' Transaction Even
*B122296,1 NNA            there is a Variance between Landed and Actual costs on the PO, and also
*B122296,1 NNA            Check for the closing date in the (GLDIST File) not from the POSHDR Or POSLN
*B124947,1 NNA 12/20/2004 Fix bug that if you made a PO with An exchange Rate then Recived it with
*B124947,1 NNA            another Rate you'll find that the Report Cost numbers calculates based on
*B124947,1 NNA            the poshdr Rate that Represent the Issue Rate.
*B129607,1 NNA 10/27/2005 Fix bug that report duplicating transaction if you repeated a style more one time in the PO
*B131251,1 AHU 04/27/2006 recorrect the Close Po value in GL-WIP report
*B607889,1 TMI 12/19/2006 treate with the type 'M' as type 'D'
*B608109,1 HBG 06/04/2007 Remove option style Make/Buy
*B608495,1 NNA 26/03/2008 Fix bug that The division sort does not work 
*:--------------------------------------------------------------------------
llGlLink  = (ALLTRIM(UPPER(gfGetMemVar('M_Link_GL',gcAct_Comp)))='Y')
llApLink  = ('AP' $ gcCmpModules)

*B124947,1 NNA 12/20/2004 (Begin) open APINVHDR to get the Exchange Rate from If the Transaction 
*B124947,1 NNA             is AP Invoice.
IF !USED("APINVHDR")
  =gfOpenFile(gcDataDir+'APINVHDR',gcDataDir+'INVVEND','SH')
ENDIF
*B124947,1 NNA (End)
*B606067,1 KHM 06/05/2002 (Begin) Check if MF installed then open cutkth and cuttktl
llMFInstall = ('MF' $ gcCmpModules)
STORE .F. TO llCutTktH,llCutTktLn
IF llMFInstall AND !USED('CUTTKTH.DBF')
  llCutTktH = gfOpenFile(gcDataDir+'CUTTKTH',gcDataDir+'CUTTKTH','SH')
ENDIF
IF llMFInstall AND !USED('CUTTKTL.DBF')
  llCutTktLn = gfOpenFile(gcDataDir+'CUTTKTL',gcDataDir+'CUTTKTLS','SH')
ENDIF
*B606067,1 KHM 06/05/2002 (End)

IF llGlLink  && If the system is linked to GL-Module, Else Return.
  *--------------------------------------------------------------
  *lcRpTmpFil                      && Variable hold the Temp File Name
  *                                && Initiated from Option Grid.
  *lcRpFromTyp                     && Form LayOut (Detail,Summary).
  lcGlCode  = lcRPLnkCod           && Gl-Link Code Variable.
  lcLinTmNam = ""
  lcGlAcnt  = lcRpGlAct            && Gl Account Code.
  lcCtPoSt  = lcRpTRStat           && CutTkt OR Po Status Variable.
  lcStyFlt  = ".T."                && Initiate Style file criteria
  lcSeason = ".T."
  lcDivision = ".T."
  lcHdrFl = ""
  lcStyFlt  = IIF(lcRpStatus<>"N", "Style.Status = lcRpStatus", lcStyFlt)
  
  *B608109,1 HBG 06/04/2007 Remove option style Make/Buy [Begin]
  *IF lcRpMakBuy <> "B"  && Not Both.
  *  lcStyFlt  = lcStyFlt + " .AND. Style.MAKE = (lcRpMakBuy=[M])"
  *ENDIF
  lcStyFlt  = lcStyFlt + " .AND. Style.MAKE = (gcAct_Appl = [MF])"
  *B608109,1 HBG 06/04/2007 [End]
  
  STORE {} TO ldLowDat , ldHigDat  && Initial Date Range Variable.
  STORE "" TO lcLowSty , lcHigSty  && Initiate Style Range Variable.
  lcWhilCon = ""
  llDumyRet = lfSetRel()           && Call Function to set relations.
  lcSeltdPo = ""
  lcSeltdCT = ""
  =lfCretFltr()
  lnMajLen = LEN(gfitemMask("PM"))
  WAIT WINDOW 'Collecting data ..' NOWAIT
  *--- PrePare style file for collecting data.
  llDummy = lfPreSty()
  llDummy = lfClectData()
  WAIT CLEAR
  SELECT &lcRpTmpFil
  GO TOP
  IF EOF()
    *Message 'There are no records to display...!'
    =gfModalGen('TRM00052B00000','DIALOG')
  ELSE
    *lcRpForm = IIF(lcRpFromTyp='D',"MFWIPAD","MFWIPAS")
    lcRpForm = "MFWIPAD"
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

*B606067,1 KHM 06/05/2002 (Begin) Close the CutTktH and CutTktL files.
IF llMFInstall 
  IF  llCutTktH
    USE IN CutTktH
  ENDIF  
  IF llCutTktLn
    USE IN CutTktL
  ENDIF
ENDIF  
*B606067,1 KHM 06/05/2002 (End)

SET DEVICE TO SCREEN

*B124947,1 NNA 12/20/2004 (Begin) Close file (APINVHDR)
IF USED("APINVHDR")
  USE IN APINVHDR
ENDIF
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

*E302206,1 ALB Add vendor code and name in case of PO [BEGIN]
*B605483,1 KHM 02/07/2002 (Begin) Changing the number of decimals of the cost
*B605483,1                field to be 3 instead of 7.
*CREATE TABLE (gcWorkDir+lcRpTmpFil) (WIPAcc C(24),CtPoNo C(6),Type C(1),cType C(1),TranDate D,;
                          TranType C(1),Desc C(20),Qty N(13,3),Cost N(12,7),llDontShow L ,llFabric L)
*CREATE TABLE (gcWorkDir+lcRpTmpFil) (WIPAcc C(24),CtPoNo C(6),Type C(1),cType C(1),TranDate D,;
                          TranType C(1),Desc C(20),Qty N(13,3),Cost N(12,3),llDontShow L ,llFabric L)
CREATE TABLE (gcWorkDir+lcRpTmpFil) (WIPAcc C(24),CtPoNo C(6),Type C(1),cType C(1),TranDate D,;
                          TranType C(1),Desc C(20),Qty N(13,3),Cost N(12,3),llDontShow L ,llFabric L,;
                          cVendCode C(8),cVendName C(30))

*B605483,1 KHM 02/07/2002 (End)
*E302206,1 ALB Add vendor code and name in case of PO [END]

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
IF USED(lcSeltdCT)
  GO TOP IN (lcSeltdCT)
  llUseCTTm = !EOF(lcSeltdCT)
ENDIF
*B608109,1 HBG 06/04/2007 Remove option style Make/Buy [Begin]
*DO CASE
*  CASE lcRpMakBuy = "B"
*    =lfUseTmp(llUseCTTm,"M")
*    =lfUseTmp(llUsePOTm,"P")
*  CASE lcRpMakBuy = "M"
*    =lfUseTmp(llUseCTTm,"M")
*  CASE lcRpMakBuy = "P"
*    =lfUseTmp(llUsePOTm,"P")
*ENDCASE
DO CASE
  CASE gcAct_Appl = "MF"
    =lfUseTmp(llUseCTTm,"M")
  CASE gcAct_Appl = "PO"    
    =lfUseTmp(llUsePOTm,"P")
ENDCASE
*B608109,1 HBG 06/04/2007 [End]

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

llMf = (lc2ColFrm == "CutTktL")
SELECT (lc2ColFrm)
SET ORDER TO &lc2ColFrm
GO TOP
lcHdrFl   = IIF(llMf,"CUTTKTH","POSHDR")
llFundTr  = IIF(llMf,SEEK(lcTrNo),SEEK("P"+lcTrNo))
lcTrWhCnd = IIF(llMf,"CutTkt","cStyType+PO")

*B608495,1 NNA 26/03/2008 (Begin) fix the following code to add the table name only if we did not add it before
*IF lcSeason <> ".T."
IF lcSeason <> ".T." AND !(UPPER(lcHdrFl) $ UPPER(lcSeason))
*B608495,1 NNA (End)

  lcSeason = lcHdrFl+lcSeason
ENDIF

*B608495,1 NNA 26/03/2008 (Begin) fix the following code to add the table name only if we did not add it before
*IF lcDivision <> ".T."
IF lcDivision <> ".T." AND !(UPPER(lcHdrFl) $ UPPER(lcDivision))
*B608495,1 NNA (End)

  lcDivision = lcHdrFl+lcDivision
ENDIF

SELECT (lc2ColFrm)
IF llFundTr
  SELECT (lcHdrFl)
  =IIF(llMf,SEEK(lcTrNo),SEEK("P"+lcTrNo))
  *B120321,1 ABD - the activity date range filter should be Between the from and to date.  [Begin]
  *SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,"P"+lcTrNo);
  *          FOR IIF(lcCtPoSt='A',.T.,&lcHdrFl..Status=lcCtPoSt) AND ;
  *              &lcHdrFl..Entered >= ldLowDat .AND.;
  *              !EMPTY(&lcHdrFl..Link_Code) AND ;
  *            IIF(!EMPTY(lcGlCode),&lcHdrFl..Link_Code=lcGlCode,.T.)  AND ;
  *            IIF(llMf,  &lcSeason .AND. &lcDivision ,&lcDivision)

  *B120321,3 MHM - the activity date range filter should be Between the from and to date.  [Begin]
  *SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,"P"+lcTrNo);
  *          FOR IIF(lcCtPoSt='A',.T.,&lcHdrFl..Status=lcCtPoSt) AND ;
  *              BETWEEN(&lcHdrFl..Entered,ldLowDat,ldHigDat) .AND.;
  *              !EMPTY(&lcHdrFl..Link_Code) AND ;
  *            IIF(!EMPTY(lcGlCode),&lcHdrFl..Link_Code=lcGlCode,.T.)  AND ;
  *            IIF(llMf,  &lcSeason .AND. &lcDivision ,&lcDivision)

  *SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,"P"+lcTrNo);
            FOR IIF(lcCtPoSt='A',.T.,&lcHdrFl..Status=lcCtPoSt) AND ;
                !EMPTY(&lcHdrFl..Link_Code) AND ;
              IIF(!EMPTY(lcGlCode),&lcHdrFl..Link_Code=lcGlCode,.T.)  AND ;
              IIF(llMf,  &lcSeason .AND. &lcDivision ,&lcDivision)

  *B608495,1 NNA 26/03/2008 (Begin) Add the Division Filter to the scanning condition
  *SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,"P"+lcTrNo);
            FOR IIF(lcCtPoSt='A',.T.,;
            IIF(!llMf .AND. lcCtPoSt="S",&lcHdrFl..Status="S",&lcHdrFl..Status=lcCtPoSt)) .AND. ;
                !EMPTY(&lcHdrFl..Link_Code) .AND. ;
              IIF(!EMPTY(lcGlCode),&lcHdrFl..Link_Code=lcGlCode,.T.)

  SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcTrNo,"P"+lcTrNo);
            FOR IIF(lcCtPoSt='A',.T.,;
            IIF(!llMf .AND. lcCtPoSt="S",&lcHdrFl..Status="S",&lcHdrFl..Status=lcCtPoSt)) .AND. ;
                !EMPTY(&lcHdrFl..Link_Code) .AND. ;
              IIF(!EMPTY(lcGlCode),&lcHdrFl..Link_Code=lcGlCode,.T.) AND ;
              IIF(llMf,  &lcSeason .AND. &lcDivision ,&lcDivision) 

  *B608495,1 NNA (End)
  *B120321,3 MHM  - [End]
  
    lcDetTrNo = IIF(llMf,CutTkt,PO)
    SELECT (lc2ColFrm)
    =IIF(llMf,SEEK(lcDetTrNo),SEEK("P"+lcDetTrNo))
    SCAN REST WHILE &lcTrWhCnd = IIF(llMf,lcDetTrNo,"P"+lcDetTrNo);
              FOR lfChkSty(&lc2ColFrm..Style,llMf)
      
      *B122242,1 MHM 3/24/2004  move this part to another place  [Start]. 
      *B120321,3 MHM - the activity date range filter should be Between the from and to date.  [Begin]
      *IF !(IIF(UPPER(lc2ColFrm) ="PosLN" , BETWEEN(&lc2ColFrm..Date,ldLowDat,ldHigDat),;
      *    BETWEEN(&lc2ColFrm..dPostDate,ldLowDat,ldHigDat)) .AND. ;
      *    IIF(UPPER(lc2ColFrm) ="PosLN" ,TranCd $"24",TranCd $"23"))
      *  LOOP
      *ENDIF
      *B120321,3 MHM - [End]
      *B122242,1 MHM  [End]. 
      
      *--- Case detail Form
      lcDetTrNo = IIF(llMf,CutTkt,PO)
      SELECT &lcRpTmpFil
      WAIT WINDOW "Collecting data for transaction# "+lcDetTrNo  NOWAIT
      LOCATE FOR Type+CtPoNo = IIF(llMf,'M',"I")+lcDetTrNo
      IF !FOUND()
        *--- Colect Detail transactions.
        =lfCutDet(IIF(llMf,'M',"I"))
      ENDIF
      *B122242,1 MHM 3/24/2004  Fix bug of the report does not report Material Issues [Start]. 
      *B122296,1 NNA 04/27/2004 (Begin) Move this Code to Befor the end of the scan to getting 
      *B122296,1 NNA                    The Close Transactions
      *IF !(IIF(UPPER(lc2ColFrm) ="POSLN" , BETWEEN(&lc2ColFrm..Date,ldLowDat,ldHigDat),;
      *    BETWEEN(&lc2ColFrm..dPostDate,ldLowDat,ldHigDat)) .AND. ;
      *    IIF(UPPER(lc2ColFrm) ="POSLN" ,&lc2ColFrm..TranCd $"24",&lc2ColFrm..TranCd $"23"))
      *  LOOP
      *ENDIF
      *B122296,1 NNA (End)
      *B122242,1 MHM  [End]. 
      
      *--- Budget record for Closed Transaction (PO,CT)
      IF &lcHdrFl..Status = 'S' AND &lc2ColFrm..TranCd = '1'
        PRIVATE lnTmInd , lcTmInd , lnCloseQty
        lnCloseQty = 0
        FOR lnTmInd = 1 TO 5
          lcTmInd = STR(lnTmInd,1)
          *B607379,1 ALB Display all costs with base currency [BEGIN]
      *B131251,1 AHU 04/27/2006 recorrect the Close Po value in GL-WIP report[BEGIN]
          lnCloseQty = lnCloseQty+(&lcHdrFl..nAct_Cost&lcTmInd - &lcHdrFl..nLan_Cost&lcTmInd)
          *lnCloseQty = lnCloseQty+(&lcHdrFl..nfActCost&lcTmInd - &lcHdrFl..nfLanCost&lcTmInd)
      *B131251,1 AHU 04/27/2006 recorrect the Close Po value in GL-WIP report[BEGIN]
          *B607379,1 ALB Display all costs with base currency [END]
        ENDFOR
        SELECT &lcRpTmpFil
        LOCATE FOR CtPoNo = lcDetTrNo AND Type = IIF(llMf,'M',"I") AND TranType = 'S'
        IF !FOUND()
          SELECT (lcTmpNam)
          SET ORDER TO GlDistNo DESCENDING

          *B122296,1 NNA 04/27/2004 (BEGIN) Seek on the Tran No and the Tran Type To get the Closed
          *B122296,1 NNA            PO and Check if the closeing Date (in the GLDIST File) 
          *B122296,1 NNA            is Between the the Date Rang To Display or not
          *ldClDate = IIF(SEEK(lcDetTrNo,lcTmpNam),&lcTmpNam..Tran_Date,{})
          ldClDate = IIF(SEEK(lcDetTrNo+'JC',lcTmpNam),&lcTmpNam..Tran_Date,{})
          IF BETWEEN(ldClDate,ldLowDat,ldHigDat)
          *B122296,1 NNA (END)

            SELECT &lcRpTmpFil
            APPEND BLANK
            REPLACE WIPAcc   WITH IIF(EMPTY(lcGlCode),Gl_Link.GlAcnt,lcGlAcnt),;
                    CtPoNo   WITH lcDetTrNo,;
                    Type     WITH IIF(llMf,'M',"I"),;
                    Desc     WITH ' ',;
                    cType    WITH '9',;
                    TranDate WITH ldClDate,;
                    TranType WITH 'S',;
                    Qty      WITH 1,;
                    Cost     WITH lnCloseQty*-1
          *E302206,1 ALB Add vendor code and name in case of PO [BEGIN]
            REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                    cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
          *B122296,1 NNA 04/27/2004 (BEGIN) Closing If Statement
          ENDIF
          *B122296,1 NNA (End)
          *E302206,1 ALB Add vendor code and name in case of PO [END]
        ENDIF
      ENDIF
      *B122296,1 NNA 04/27/2004 (Begin) This code Moved to this Position To do it But After Getting the Close Trans.
      IF !(IIF(UPPER(lc2ColFrm) ="POSLN" , BETWEEN(&lc2ColFrm..Date,ldLowDat,ldHigDat),;
          BETWEEN(&lc2ColFrm..dPostDate,ldLowDat,ldHigDat)) .AND. ;
          IIF(UPPER(lc2ColFrm) ="POSLN" ,&lc2ColFrm..TranCd $"24",&lc2ColFrm..TranCd $"23"))
        LOOP
      ENDIF
      *B122296,1 NNA (End)
    ENDSCAN
  ENDSCAN
ENDIF
USE IN  (lcLinTmNam)
RETURN


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

*SELECT CUTTKTL
*SET RELATION TO CutTkt INTO CutTktH
*GO TOP
*SELECT PoSLn
*SET RELATION TO "P"+Po INTO PosHdr
*GO TOP
*B606067,1 KHM 06/05/2002 (Begin) Check if MF installed and CutTktH file is opened.
IF llMFInstall AND llCutTktH
*B606067,1 KHM 06/05/2002 (End)
  SELECT CutTktH
  SET RELATION TO Link_Code+'013' INTO Gl_Link
*B606067,1 KHM 06/05/2002 (Begin) 
ENDIF
*B606067,1 KHM 06/05/2002 (End)
GO TOP
SELECT PoSHdr
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
*: Passed Parameters  : lcCtPoT='I' (Imported) or 'M' (Manufactured)
*!*************************************************************
*! Example            :  =lfCutDet('I')
*!*************************************************************
*!
FUNCTION lfCutDet
PARAMETER lcCtPoT


*--- lc2ColFrm :: Detail File (CutTktL,Or,POSLN)
*--- lcHdrFl   :: Header File (CutTktH,OR, PODHDR)
*--- lcDetTrNo    :: CutTkt# or PO#
SET ORDER TO TAG (lc2ColFrm) IN (lc2ColFrm)
  IF BETWEEN(RECNO(lc2ColFrm),1,RECCOUNT(lc2ColFrm))
    GO RECNO(lc2ColFrm) IN (lc2ColFrm)
  ENDIF

IF SEEK(lcCtPoT+lcDetTrNo,'cTktBom')
  SELECT cTktBom
  SCAN REST WHILE cimtyp+cuttkt+typ+item+iclr+mfgcode+dyelot = lcCtPoT+lcDetTrNo
    lcType=Typ
    ** fabric or trim.
    llFabTrim=(cCatgTyp='F') OR (cCatgTyp='T' AND Trim_Invt)
    ** style component
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
          
          *B605483,1 KHM 02/07/2002 (Begin) IF the low date is emtpy then
          *B605483,1                do not check on the date.
          *SCAN WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession=;
                     SUBSTR(lcItem,1,7)+lcIClr;
               FOR cTktNo = lcDetTrNo .AND. BETWEEN(dTranDate,ldLowDat,ldHigDat);
               .AND. (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cGlMatAdj),Gl_Link.GlAcnt,cGlMatAdj));
                   .AND. ALLTRIM(cImTyp) = lcCtPoT &&.AND. !SEEK(lcDetTrNo +SUBSTR(lcItem,1,7)+lcIClr+MATINVJL.cisession+ IIF(MatInvJl.nIssued>0,'I','R'),lcTmpCurs)
          SCAN WHILE cfabric+ccolor+cwarecode+cdyelot+crsession+cisession=;
                     SUBSTR(lcItem,1,7)+lcIClr;
               FOR cTktNo = lcDetTrNo .AND. IIF(ldLowDat={},.T.,BETWEEN(dTranDate,ldLowDat,ldHigDat));
               .AND. (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cGlMatAdj),Gl_Link.GlAcnt,cGlMatAdj));
                   .AND. ALLTRIM(cImTyp) = lcCtPoT &&.AND. !SEEK(lcDetTrNo +SUBSTR(lcItem,1,7)+lcIClr+MATINVJL.cisession+ IIF(MatInvJl.nIssued>0,'I','R'),lcTmpCurs)
          *B605483,1 KHM 02/07/2002 (End)
          
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
               *E302206,1 ALB Add vendor code and name in case of PO [BEGIN]
               REPLACE cVendCode WITH BOMCOST.cVendCode,;
                       cVendName WITH lfGetVend(cVendCode)
              *E302206,1 ALB Add vendor code and name in case of PO [END]

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
        
        *B605483,1 KHM 02/07/2002 (Begin) IF the low date is emtpy then
        *B605483,1                do not check on the date.
        *SCAN REST WHILE Style=lcItem ;
             FOR CTRCode=lcDetTrNo AND BETWEEN(dTrDate,ldLowDat,ldHigDat);
               .AND. (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cAdjAcct),Gl_Link.GlAcnt,cAdjAcct))
        SCAN REST WHILE Style=lcItem ;
             FOR CTRCode=lcDetTrNo AND IIF(ldLowDat={},.T.,BETWEEN(dTrDate,ldLowDat,ldHigDat));
               .AND. (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cAdjAcct),Gl_Link.GlAcnt,cAdjAcct))
        *B605483,1 KHM 02/07/2002 (End)
        
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
          *E302206,1 ALB Add vendor code and name in case of PO [BEGIN]
          REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                  cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
          *E302206,1 ALB Add vendor code and name in case of PO [END]
        ENDSCAN
    ENDCASE   

    ** Receive
    SELECT (lcLinTmNam)
    IF lcCtPoT='M'
      SET ORDER TO CUTTKTL
    ELSE
      SET ORDER TO POSLN
    ENDIF
    SELECT (lcLinTmNam)
    =SEEK(IIF(lcCtPoT='M',"","P")+lcDetTrNo))
    SCAN REST WHILE IIF(lcCtPoT='M',CutTkt,"P"+PO) = IIF(lcCtPoT='M',"","P")+lcDetTrNo;
         FOR TranCd='2'
      *B120321,3 MHM - the activity date range filter should be Between the from and to date.  [Begin]
      IF !(IIF(lcCtPoT='M', BETWEEN(&lcLinTmNam..dPostDate,ldLowDat,ldHigDat),;
          BETWEEN(&lcLinTmNam..Date,ldLowDat,ldHigDat))) 
        LOOP
      ENDIF
      *B120321,3 MHM - [End]
         
      lcRSession = cRSession
      SELECT BomLine
      *--- cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode
      
      *B129607,1 NNA 10/27/2005 (Begin) Seek and Scan for Line No.
      *=SEEK(lcCtPoT+'2'+lcDetTrNo)
      *SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                 lcCtPoT+'2'+lcDetTrNo ;
           FOR cBomTyp+Style+Item+Iclr+MfgCode+cRSession=;
               lcType+&lcLinTmNam..Style+lcItem+lcIClr+lcMfgCode+lcRSession
      =SEEK(lcCtPoT+'2'+lcDetTrNo+STR(&lcLinTmNam..LINENO,6))
      SCAN REST WHILE cimtyp+ctype+ctktno+STR(lineno,6)+cbomtyp+style+sclr+item+iclr+mfgcode=;
                 lcCtPoT+'2'+lcDetTrNo+STR(&lcLinTmNam..LINENO,6) ;
           FOR cBomTyp+Style+Item+Iclr+MfgCode+cRSession=;
               lcType+&lcLinTmNam..Style+lcItem+lcIClr+lcMfgCode+lcRSession
      *B129607,1 NNA (End)
      
        SELECT &lcRpTmpFil
        APPEND BLANK 
        REPLACE WIPAcc   WITH IIF(EMPTY(lcGlCode),Gl_Link.GlAcnt,lcGlAcnt),;
                CtPoNo   WITH lcDetTrNo,;
                Type     WITH lcCtPoT,;
                Desc     WITH lcDesc,;
                cType    WITH BomLine.CBomTyp,;
                TranDate WITH &lcLinTmNam..Date,;
                TranType WITH 'E',;
                Qty      WITH -1*(BomLine.ItemQty),;
                Cost     WITH IIF(lcCtPoT='M',BomLine.UnitCost,IIF(PoSHdr.Rate<>0 AND BomLine.cCatgTyp='P',BomLine.UnitCost/PoSHdr.Rate,BomLine.UnitCost))
        *E302206,1 ALB Add vendor code and name in case of PO [BEGIN]
        REPLACE cVendCode WITH IIF(llMf,'',&lcHdrFl..Vendor),;
                cVendName WITH IIF(llMf,'',lfGetVend(cVendCode))
        *E302206,1 ALB Add vendor code and name in case of PO [END]
        *B607379,1 ALB Display all costs with base currency [BEGIN]
        
        *B607889,1 TMI [Start] Treate with type 'M' as type 'D'
        *IF lcCtPoT='M'
        *  lnEqCose = BomLine.UnitCost
        *ELSE
        *B607889,1 TMI [End  ] 
        
          IF BomLine.cCatgTyp='P'
            lcExSin2 = ''
            lcExSin1 = gfGetExSin(@lcExSin2,PoSHdr.CPriceCur)
            *B124947,1 NNA 12/20/2004 (BEGIN) If this Transaction is Receiving then we get 
            *B124947,1 NNA            the Exchange Rate from The Posln instead of the PosHdr file
            *lnEqCose = BomLine.UnitCost &lcExSin1. PoSHdr.nPricerat
            lnOldAlias = SELECT(0)
            *B131251,1 AHU recorrect the Close Po value in GL-WIP report[begin]
            lnPoRecNo=RECNO('POSLN')
            *B131251,1 AHU recorrect the Close Po value in GL-WIP report[END]
             IF SEEK(Poshdr.cstytype+ Poshdr.po ,'POSLN')
               SELECT POSLN
               SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                    Poshdr.cstytype+ Poshdr.po FOR TRANCD='2' AND PosLn.cRsession = BomLine.cRsession
                 lnCostRate = POSLN.nLanprrat
               ENDSCAN
               lnEqCose = BomLine.UnitCost &lcExSin1. lnCostRate
               *B131251,1 AHU recorrect the Close Po value in GL-WIP report[begin]
                GOTO lnPoRecNo
               *B131251,1 AHU recorrect the Close Po value in GL-WIP report[END]
             ENDIF
            SELECT(lnOldAlias)
            *B124947,1 NNA (END)

          ELSE
            *B607889,1 TMI [Start] Treate with type 'M' as type 'D'
            *IF BomLine.cCatgTyp='D'
            IF BomLine.cCatgTyp $ 'MD'
              *B607889,1 TMI [End  ] 
              lcExSin2 = ''
              lcExSin1 = gfGetExSin(@lcExSin2,PoSHdr.CDutyCur)
              *B124947,1 NNA 12/20/2004 (BEGIN) If this Transaction is Receiving then we get 
              *B124947,1 NNA            the Exchange Rate from The Posln instead of the PosHdr file
              *lnEqCose = BomLine.UnitCost &lcExSin1. PoSHdr.nDutyrat
              lnOldAlias = SELECT(0)
              *B131251,1 AHU recorrect the Close Po value in GL-WIP report[begin]
              lnPoRecNo=RECNO('POSLN')
              *B131251,1 AHU recorrect the Close Po value in GL-WIP report[END]
              IF SEEK(Poshdr.cstytype+ Poshdr.po ,'POSLN')
                SELECT POSLN
                SCAN REST WHILE cstytype+po+style+STR(lineno,6)+trancd = ;
                     Poshdr.cstytype+ Poshdr.po FOR TRANCD='2' AND PosLn.cRsession = BomLine.cRsession
                  lnCostRate = POSLN.nLanDuRat
                ENDSCAN
                lnEqCose = BomLine.UnitCost &lcExSin1. lnCostRate
                *B131251,1 AHU recorrect the Close Po value in GL-WIP report[begin]
                GOTO lnPoRecNo
                *B131251,1 AHU recorrect the Close Po value in GL-WIP report[END]
              ENDIF
              SELECT(lnOldAlias)
              *B124947,1 NNA (END)

            ELSE
              lnEqCose = BomLine.UnitCost
            ENDIF
          ENDIF
       
        *B607889,1 TMI [Start] comment the following line
        *ENDIF
        *B607889,1 TMI [End  ] 
        REPLACE Cost WITH lnEqCose
        *B607379,1 ALB Display all costs with base currency [END]
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
  *B605483,1 KHM 02/07/2002 (Begin) IF the low date is emtpy then
  *B605483,1                do not check on the date.
  *SCAN REST WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession=;
             lcCtPoT+lcDetTrNo;
       FOR   BETWEEN(dTranDate,ldLowDat,ldHigDat) .AND. cCostType$"MPD";
       AND  (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cWipAcnt),Gl_Link.GlAcnt,cWipAcnt))
  SCAN REST WHILE cimtyp+ctktno+actualize+cbomtype+item+iclr+mfgcode+coprcode+clotno+cisession+crsession=;
             lcCtPoT+lcDetTrNo;
       FOR   IIF(ldLowDat={},.T.,BETWEEN(dTranDate,ldLowDat,ldHigDat)) .AND. cCostType$"MPD";
       AND  (EMPTY(lcGlCode) OR lcGlAcnt=IIF(EMPTY(cWipAcnt),Gl_Link.GlAcnt,cWipAcnt))
  *B605483,1 KHM 02/07/2002 (End)
  
    SELECT &lcRpTmpFil
    APPEND BLANK
    *B605483,1 KHM 02/07/2002 (Begin) Replacing the AP invoice & Vendor 
    *B605483,1                in the Desc field.    
    *REPLACE WIPAcc   WITH IIF(EMPTY(BOMCOST.cWipAcnt),Gl_Link.GlAcnt,BOMCOST.cWipAcnt),;
            CtPoNo   WITH lcDetTrNo,;
            Type     WITH lcCtPoT,;
            cType    WITH BOMCOST.cbomtype,;
            Desc     WITH lcDesc,;
            TranDate WITH BomCost.dTranDate,;
            Qty      WITH BomCost.nTotQty,;
            Cost     WITH BomCost.nTotCst/BomCost.nTotQty,;
            TranType WITH 'V'
    *B606047,1 KHM 05/29/2002 (Begin) Add the checking of nTotQty <> 0
    *REPLACE WIPAcc   WITH IIF(EMPTY(BOMCOST.cWipAcnt),Gl_Link.GlAcnt,BOMCOST.cWipAcnt),;
            CtPoNo   WITH lcDetTrNo,;
            Type     WITH lcCtPoT,;
            cType    WITH BOMCOST.cbomtype,;
            Desc     WITH ALLTRIM(BomCost.cApInvNo) + '  ' + ALLTRIM(BomCost.cVendCode),;
            TranDate WITH BomCost.dTranDate,;
            Qty      WITH BomCost.nTotQty,;
            Cost     WITH BomCost.nTotCst/BomCost.nTotQty,;
            TranType WITH 'V'
    
    REPLACE WIPAcc   WITH IIF(EMPTY(BOMCOST.cWipAcnt),Gl_Link.GlAcnt,BOMCOST.cWipAcnt),;
            CtPoNo   WITH lcDetTrNo,;
            Type     WITH lcCtPoT,;
            cType    WITH BOMCOST.cbomtype,;
            Desc     WITH ALLTRIM(BomCost.cApInvNo) + '  ' + ALLTRIM(BomCost.cVendCode),;
            TranDate WITH BomCost.dTranDate,;
            Qty      WITH BomCost.nTotQty,;
            Cost     WITH IIF(BomCost.nTotQty<>0,BomCost.nTotCst/BomCost.nTotQty,0),;
            TranType WITH 'V'
    *B606047,1 KHM 05/29/2002 (End)
    *E302206,1 ALB Add vendor code and name in case of PO [BEGIN]
    REPLACE cVendCode WITH BOMCOST.cVendCode,;
            cVendName WITH lfGetVend(cVendCode)
    *E302206,1 ALB Add vendor code and name in case of PO [END]
    *B607379,1 ALB Display all costs with base currency [BEGIN]

    IF lcCtPoT='M'
      lnEqCose = IIF(BomCost.nTotQty<>0,BomCost.nTotCst/BomCost.nTotQty,0)
    ELSE
      IF BomCost.cCostType='P'
        lcExSin2 = ''
        lcExSin1 = gfGetExSin(@lcExSin2,PoSHdr.CPriceCur)

        *B124947,1 NNA 12/20/2004 (BEGIN) If this Transaction is AP Invoice then we get 
        *B124947,1 NNA            the Exchange Rate from The ApinvHdr instead of the PosHdr file
        *lnEqCose = IIF(BomCost.nTotQty<>0,(BomCost.nTotCst &lcExSin1. PoSHdr.nPricerat) / BomCost.nTotQty,0)
        IF SEEK(BomCost.cApInvNo + BomCost.cVendCode ,'APINVHDR')
          lnEqCose = IIF(BomCost.nTotQty<>0,(BomCost.nTotCst &lcExSin1. ApinvHdr.nExrate) / BomCost.nTotQty,0)
        ENDIF
        *B124947,1 NNA (END)
      ELSE
        IF BomCost.cCostType='D'
          lcExSin2 = ''
          lcExSin1 = gfGetExSin(@lcExSin2,PoSHdr.CDutyCur)
          *B124947,1 NNA 12/20/2004 (BEGIN) If this Transaction is AP Invoice then we get 
          *B124947,1 NNA            the Exchange Rate from The ApinvHdr instead of the PosHdr file
          *lnEqCose = IIF(BomCost.nTotQty<>0,(BomCost.nTotCst &lcExSin1. PoSHdr.nDutyrat) / BomCost.nTotQty,0)
          IF SEEK(BomCost.cApInvNo + BomCost.cVendCode ,'APINVHDR')
            lnEqCose = IIF(BomCost.nTotQty<>0,(BomCost.nTotCst &lcExSin1. ApinvHdr.nExrate) / BomCost.nTotQty,0)            
          ENDIF
          *B124947,1 NNA (END)

        ELSE
          lnEqCose = IIF(BomCost.nTotQty<>0,BomCost.nTotCst/BomCost.nTotQty,0)
        ENDIF
      ENDIF
    ENDIF
    REPLACE Cost WITH lnEqCose
    *B607379,1 ALB Display all costs with base currency [END]
  ENDSCAN
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
  CASE lcStatus='S'
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

lnClrSgPo = ASUBSCRIPT(laOgFxFlt,;
            ASCAN(laOgFxFlt,'SUBSTR(STYLE.Style,lnClrPo,lnColorLen)'),1)

*-- Disable/enable Only This colors, Free Segment. [begin]

DO CASE
  CASE lcFreeClr = 'C'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOgFxFlt,1) + lnClrSgPo] = .T.
    = lfOGShowGet('laOgFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
  CASE lcFreeClr = 'F'
    laOGObjCnt[ALEN(laOGObjCnt,1) - ALEN(laOgFxFlt,1) + lnClrSgPo] = .F.
    = lfOGShowGet('laOgFxFlt[' + ALLTRIM(STR(lnClrSgPo)) + ',6]')
ENDCASE
lnDatapos = ASCAN(laOGFxFlt,'CUTTKTH.ENTERED')
IF lnDatapos > 0
  lnDatapos = ASUBSCRIPT(laOGFxFlt,lnDatapos,1)
  lcStr = ALLTRIM(DTOC(gdSysDate - DAY(gdSysDate)+1)) + '|' + ALLTRIM(DTOC(gdSysDate))
  IF EMPTY(laOGFxFlt[lnDatapos,6])
    laOGFxFlt[lnDatapos,6] = lcStr
  ENDIF
ENDIF

*B606799,1 KHM 01/27/2003 (Begin) Initializing the activity date range.
lnDataposP = ASCAN(laOGFxFlt,'POSHDR.ENTERED')
IF lnDataposP > 0
  lnDataposP = ASUBSCRIPT(laOGFxFlt,lnDataposP,1)
  lcStr = ALLTRIM(DTOC(gdSysDate - DAY(gdSysDate)+1)) + '|' + ALLTRIM(DTOC(gdSysDate))
  IF EMPTY(laOGFxFlt[lnDataposP,6])
    laOGFxFlt[lnDataposP,6] = lcStr
  ENDIF
ENDIF
*B606799,1 KHM 01/27/2003 (End)

DECLARE laSetups[10,2]
laSetups[1,1]  = 'M_C'+"M"+'SLBL1'
laSetups[2,1]  = 'M_C'+"M"+'SLBL2'
laSetups[3,1]  = 'M_C'+"M"+'SLBL3'
laSetups[4,1]  = 'M_C'+"M"+'SLBL4'
laSetups[5,1]  = 'M_C'+"M"+'SLBL5'

laSetups[6,1]   = 'M_C'+"I"+'SLBL1'
laSetups[7,1]   = 'M_C'+"I"+'SLBL2'
laSetups[8,1]   = 'M_C'+"I"+'SLBL3'
laSetups[9,1]   = 'M_C'+"I"+'SLBL4'
laSetups[10,1]  = 'M_C'+"I"+'SLBL5'

=gfGetMemVar(@laSetups,gcAct_Comp)

FOR lnInd = 1 TO 5
  lcInd = STR(lnInd,1)
  lcMDesc&lcInd = laSetups[lnInd,2]
  lcIDesc&lcInd = laSetups[lnInd+5,2]
ENDFOR
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
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.FABRIC' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Style.Fabric.
      lcStyFlt = lcStyFlt+" .AND. ALLTRIM(STYLE.FABRIC)==["+ALLTRIM(laOgFxFlt[lnInd,6])+"]"
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.SEASON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Season List
      *lcStyFlt = lcStyFlt+" .AND. Style.Season $ ["+laOgFxFlt[lnInd,6]+"]"
      lcSeason = ".Season $ ["+laOgFxFlt[lnInd,6]+"]"
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Divisoin List
      *lcStyFlt = lcStyFlt+" .AND. STYLE.CDIVISION $ ["+laOgFxFlt[lnInd,6]+"]"
      lcDivision = ".CDIVISION $ ["+laOgFxFlt[lnInd,6]+"]"
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYGROUP' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Style group
      lcStyFlt = lcStyFlt+" .AND. ALLTRIM(STYLE.CSTYGROUP) == ["+ALLTRIM(laOgFxFlt[lnInd,6])+"]"

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.PATTERN' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Style Pattern
      lcStyFlt = lcStyFlt+" .AND. ALLTRIM(STYLE.PATTERN) == ["+ALLTRIM(laOgFxFlt[lnInd,6])+"]"


    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = "SUBSTR(STYLE.Style,lnClrPo,lnColorLen)" .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Style Color
      lcStyFlt = lcStyFlt+" .AND. SUBSTR(STYLE.Style,lnClrPo,lnColorLen) $ ["+laOgFxFlt[lnInd,6]+"]"

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUTTKTH.ENTERED' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Dates Range
      ldLowDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldHigDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))

    *B606799,1 KHM 01/27/2003 (Begin)
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'POSHDR.ENTERED' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Dates Range
      ldLowDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],1,10)))
      ldHigDat = CTOD(ALLTRIM(SUBSTR(laOgFxFlt[lnInd,6],12,20)))
    *B606799,1 KHM 01/27/2003 (End)
    
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'STYLE.CSTYMAJOR' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Style Pattern
      lcLowSty = SUBSTR(laOgFxFlt[lnInd,6],1,AT("|",laOgFxFlt[lnInd,6])-1)
      lcHigSty = SUBSTR(laOgFxFlt[lnInd,6],AT("|",laOgFxFlt[lnInd,6])+1)

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'POSHDR.PO' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6])) .AND. USED(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcSeltdPo = ALLTRIM(laOgFxFlt[lnInd,6])
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUTTKTH.CUTTKT' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6])) .AND. USED(ALLTRIM(laOgFxFlt[lnInd,6]))
      lcSeltdCT = ALLTRIM(laOgFxFlt[lnInd,6])
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
*! Name      : lfPreSty
*! Developer : Ahmed Salah (SSH)
*! Date      : 25/09/20001
*! Purpose   : Prepare Style filter.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : =lfPreSty()
*!*************************************************************
*!
FUNCTION lfPreSty
PRIVATE lnOldAls
lnOldAls = SELECT(0)
SELECT Style
DO CASE
  CASE !EMPTY(lcLowSty) .AND. !EMPTY(lcHigSty)
    =SEEK(PADR(lcLowSty,lnMajLen))
    lcWhilCon = "PADR(STYLE.STYLE,lnMajLen) <= PADR(lcHigSty,lnMajLen)"
  CASE EMPTY(lcLowSty) .AND. !EMPTY(lcHigSty)
    GO TOP
    lcWhilCon = "PADR(STYLE.STYLE,lnMajLen) <= PADR(lcHigSty,lnMajLen)"
  OTHERWISE
    GO TOP
    lcWhilCon = ".T."
ENDCASE
SELECT(lnOldAls)

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
*! Purpose   : Function to colect data from tempfiles or master header 
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

IF lcCTPO = "M"  && IF calling to select CT.
  IF llUseTmp    && IF we are going to colect data for selected CT in
                 && temp file lcSeltdCT that created from OG.
    SELECT (lcSeltdCT)
    SCAN
      *-- Send the second Parameter to colect data for specific CT.
      =lfColPOMF("CutTktL",CUTTKT)
    ENDSCAN
  ELSE
    *-- Send second parameter empty to colect data for all CT.
    =lfColPOMF("CutTktL","")
  ENDIF
ELSE  && IF calling to select PO.
  IF llUseTmp  && IF we are going to colect data for selected PO in
               && temp file lcSeltdCT that created from OG.
    SELECT (lcSeltdPO)
    SCAN
      *-- Send the second Parameter to colect data for specific PO.
      =lfColPOMF("PosLN",PO)
    ENDSCAN
  ELSE
    *-- Send second parameter empty to colect data for all PO.
    =lfColPOMF("PosLN","")
  ENDIF
ENDIF
SELECT(lnOldAls)


*!*************************************************************
*! Name      : lfGetVend
*! Developer : Albert Raif (ALB)
*! Date      : 08/23/98
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
*!

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

