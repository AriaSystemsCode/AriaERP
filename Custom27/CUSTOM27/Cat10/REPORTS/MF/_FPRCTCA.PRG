*:***********************************************************************
*: PROG         : MfPrctCA.PRG 
*: DATE         : 01/28/2001
*: Developer    : Ahmed Abdel Naby (AAN)
*: DESC         : Cutting ticket form for Cathy Daniels (Cust. Prog. # 102114)
*:***********************************************************************
*: Calls        : 
*:         Functions  : 
*:         Procedures : 
*:***********************************************************************
*: Example      : DO MfPrctCA
*:***********************************************************************
* Modifications :
* B604555,1 AMH 06/14/2001 Adding the lot number to the cut ticket form
* B604575,1 AMH 06/21/2001 Fix some bugs in cut ticket form for Cathy Daniels.
* B604575,4 AMH 06/27/2001 Fix some bugs in cut ticket form for Cathy Daniels.
* B604627,4 WAB 07/05/2001 change the type of field LLTKT_PRT to be caracter instead of logic 
* B604830,1 AMH 08/26/2001 Print the Special Instructions.
*:***********************************************************************

*** Check To See If There Are Any Cutting Ticket Header Records

SELECT CUTTKTH
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  RETURN
ENDIF


PRIVATE lcCarLbl,lcCareDsc1,lcCareDsc2,lcCareDsc3,lcCareDsc4,lcCarDesc5

*AMH 03/22/2001 Add variable to hole the primary fabric [Start]
PRIVATE lcPrmFab, lcPrmClr
STORE SPACE(0) TO lcPrmFab, lcPrmClr
*AMH [End]

lcCarlbl = ''
lcCareDsc1 = ''
lcCareDsc2 = ''
lcCareDsc3 = ''
lcCareDsc4 = ''
lcCareDsc5 = ''

*AMH 03/22/2001 Add variable to hole the primary fabric [Start]
PRIVATE lcRltColor
lcRltColor = SPACE(0)
*AMH [End]

DECLARE laCare1[1,2]
DECLARE laCare2[1,2]
DECLARE laCare3[1,2]
DECLARE laCare4[1,2]
DECLARE laCare5[1,2]

*AMH 03/22/2001 Add variable to hole the primary fabric [Start]
DECLARE laColor[1,2]
laColor[1,1] = 'CNRFCODE'
laColor[1,2] = 'lcRltColor'
*AMH [End]

laCare1[1,1] = 'CDESC1'
laCare1[1,2] = 'lcCareDsc1'
laCare2[1,1] = 'CDESC2'
laCare2[1,2] = 'lcCareDsc2'
laCare3[1,1] = 'CDESC3'
laCare3[1,2] = 'lcCareDsc3'
laCare4[1,1] = 'CDESC4'
laCare4[1,2] = 'lcCareDsc4'
laCare5[1,1] = 'CDESC5'
laCare5[1,2] = 'lcCareDsc5'

= lfEvalSegs()

TmpCtTkt = GFTEMPNAME()

*AMH 03/22/2001 [Start]
*B604830,1 AMH Add the Special Instructions fields [Start]
*CREATE TABLE (gcWorkDir + TmpCtTkt);
       (cType C(2), cCutTkt C(6), cDivDesc C(30), cSewAt C(30), cMajor C(12),;
        cSeasDesc C(4), cSeasCod C(6), cCutAt C(30), cPackNo C(6), dComplete D,;
        cSalDesc1 C(50), cSalDesc2 C(50), cSalDesc3 C(50), cSalDesc4 C(50), cSalDesc5 C(50),;
        cCarLbl C(3), cCarDsc1 C(30), cCarDsc2 C(30), cCarDsc3 C(30), cCarDsc4 C(30), cCarDsc5 C(30),;
        cClrDesc C(30), nYards N(10), nPly N(3), cScale C(1), nQty1 N(6),;
        nQty2 N(6), nQty3 N(6),nQty4 N(6), nQty5 N(6), nQty6 N(6),;
        nQty7 N(6), nQty8 N(6), nTotQty N(7),cLineType C(10),nPieces N(10),;
        cItem C(30), cVendor C(30), cClr_Ref C(21), cFabDesc C(96), nUntQty N(7,3),;
        cUomUse C(3), nNeed N(8), cUomBuy C(3), nNum N(1,0), cSubNum C(1),;
        cFabDesc1 C(20), cFabDesc2 C(41), cPattern C(10), cMarker C(10), mNotes M, nLot_No N(4,0), cRev_No C(2))
CREATE TABLE (gcWorkDir + TmpCtTkt);
       (cType C(2), cCutTkt C(6), cDivDesc C(30), cSewAt C(30), cMajor C(12),;
        cSeasDesc C(4), cSeasCod C(6), cCutAt C(30), cPackNo C(6), dComplete D,;
        cSalDesc1 C(50), cSalDesc2 C(50), cSalDesc3 C(50), cSalDesc4 C(50), cSalDesc5 C(50),;
        cCarLbl C(3), cCarDsc1 C(30), cCarDsc2 C(30), cCarDsc3 C(30), cCarDsc4 C(30), cCarDsc5 C(30),;
        cClrDesc C(30), nYards N(10), nPly N(3), cScale C(1), nQty1 N(6),;
        nQty2 N(6), nQty3 N(6),nQty4 N(6), nQty5 N(6), nQty6 N(6),;
        nQty7 N(6), nQty8 N(6), nTotQty N(7),cLineType C(10),nPieces N(10),;
        cItem C(30), cVendor C(30), cClr_Ref C(21), cFabDesc C(96), nUntQty N(7,3),;
        cUomUse C(3), nNeed N(8), cUomBuy C(3), nNum N(1,0), cSubNum C(1),;
        cFabDesc1 C(20), cFabDesc2 C(41), cPattern C(10), cMarker C(10), mNotes M, nLot_No N(4,0), cRev_No C(2),;
        cFabric C(7), cPosition C(10), cItem_Type C(6), cMarker2 C(10))
*B604830,1 AMH [End]
*AMH [End]
SELECT Style
SET ORDER TO cStyle
SELECT cTktBom

*AMH 03/22/2001 Chang order [Start]
SET ORDER TO cTktyp
*AMH [End]

*AMH 04/01/2001 open BOM file [Start]
=gfOpenFile(gcDataDir+'BOM','BOMITEM','SH')
*AMH [End]

WAIT WINDOW 'Please Stand By. Selecting records ....' NOWAIT

SELECT CUTTKTH
SCAN FOR &lcRpExp
  WAIT WINDOW "Select Cut ticket #: " + CutTkt NOWAIT 
  
  =SEEK(Style, "Style")
  *AMH 03/22/2001 Get primary fabric [Start]
  lcPrmFab = STYLE.FABRIC
  lcPrmMrk = ''
  *AMH [End]
  
  *-- Function to get the cut at and sew at names.
  STORE '' TO lcCutAt,lcSewAt,lcCarlbl,lcCareDsc1,lcCareDsc2,lcCareDsc3,;
              lcCareDsc4,lcCareDsc5
  =lfChkVend(@lcCutAt,@lcSewAt)  

  *-- Get season and division description
  lcSeasDesc  = SUBSTR(gfCodDes(SEASON , 'SEASON'),1,4)
  lcDivDesc = gfCodDes(CDIVISION , 'CDIVISION')

  *-- Get care label descriptions.
  =lfGetCare(@lcCarlbl , @lcCareDsc1 , @lcCareDsc2 , @lcCareDsc3 , @lcCareDsc4 , @lcCareDsc5)
  SELECT (TmpCtTkt)
  lnMemoWd = SET('MEMOWIDTH')
  SET MEMOWIDTH TO 50
  lcMajor    = SUBSTR(Style.cStyMajor,1,lnMajorLen)
  lcSeasCod  = CutTktH.Season
  lcPack     = CutTktH.cPack
  ldComplete = CutTktH.Complete
  lcSalDesc1 = MLINE(Style.mSalesDesc,1)
  lcSalDesc2 = MLINE(Style.mSalesDesc,2)
  lcSalDesc3 = MLINE(Style.mSalesDesc,3)
  lcSalDesc4 = MLINE(Style.mSalesDesc,4)
  lcSalDesc5 = MLINE(Style.mSalesDesc,5)  
  SET MEMOWIDTH TO lnMemoWd
  *-- Get BOM Lines
  =lfGetBom()
  *-- Get Cut Ticket Lines
  =lfGetCTLin()
  *-- Get Cut Ticket Cut With.
  INSERT INTO (TmpCtTkt) (cType,cCutTkt,cMajor,cPattern,cRev_No) VALUES;
                       ('F',CutTktH.CutTkt,SUBSTR(Style.cStyMajor,1,lnMajorLen),;
                       SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2))
  = lfReplace()
  *B604830,1 AMH Print Special Instructions Data [Start]
  INSERT INTO (TmpCtTkt) (cType,cCutTkt,cPattern) VALUES;
                         ('I',CutTktH.CutTkt,SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2))
  = lfReplace()
  *B604830,1 AMH [End]
  *-- Get Cut Ticket NotePad
  IF llRPrTctN .AND. SEEK('I'+CUTTKTH.CUTTKT,'NOTEPAD')
    INSERT INTO (TmpCtTkt);
         (cType,cCutTkt,mNotes) VALUES;
         ('N',CutTktH.CutTkt,Notepad.MNotes)
    = lfReplace()
  ENDIF
ENDSCAN

SELECT (TmpCtTkt)
LOCATE
IF EOF()
  =gfModalGen('TRM00052B00000','DIALOG' )
  USE IN (TmpCtTkt)
  IF FILE(gcWorkDir  + TmpCtTkt  + '.DBF')
    ERASE (gcWorkDir + TmpCtTkt  + '.DBF')
  ENDIF
  llPoDisp = .F.
  SELECT (lcMainF)
  RETURN
ENDIF

*AMH 04/01/2001 open BOM file [Start]
USE IN BOM
*AMH [End]

USE IN (TmpCtTkt)

lcRpDtDir = gcDataDir
lcRpWrDir = gcWorkDir
lcRpSysDir = gcSysHome
lcRpRpDir = gcRepHome
lcRpImDir = gcImagDir
lcRpComp = gcAct_Comp
lcRpDevice = gcDevice
lcRpDefPth = gcDef_Path
lcRpTmpFle = TmpCtTkt
lcConnStr  = ""
SAVE TO (gcWorkDir+lcTmpMemo+'.MEM') ALL LIKE l?RP* 
lcCommLine = (gcWorkDir+lcTmpMemo+'.MEM')
lcLib=SYS(2004)+"foxtools.fll"
IF FILE(lcLib)
  SET LIBRARY TO (SYS(2004)+"FOXTOOLS.FLL") ADDITIVE
  SW_HIDE = 0
  lnFnWinExec =EVALUATE("RegFn('WinExec', 'CI', 'I')")
  =EVALUATE("CALLFN("+STR(lnFnWinExec)+;
   ",gcRepHome+'MF\'+[MFPCTCA.EXE ]+lcCommLine,"+STR(SW_Hide)+")")
   llPoDisp = .F.
   RELEASE LIBRARY (SYS(2004)+"FOXTOOLS.FLL")
ELSE
  WAIT "LIBRARY NOT FOUND" WINDOW
  RETURN .F.
ENDIF
SELECT (lcMainF)

*:****************************************************************
*: Name        : lfVSelec
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : sum a specific field for the current style in style file
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfVSelec()
*:****************************************************************
*
FUNCTION lfVSelec
PARAMETERS lcDumm
CLEAR READ
RETURN ""
*-- End of lfVSelec.

*:***************************************************************************
*: Name        : lfEvalSegs
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 17/08/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:***************************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:***************************************************************************
*: Calls       : ........
*:***************************************************************************
*: Return      : ........
*:***************************************************************************
*! Example     : = lfEvalSegs()
*:***************************************************************************
*
FUNCTION lfEvalSegs
STORE 0  TO  lnMajSeg,lnNonMajSt,lnMajorLen,lnFreeLen,lnColorLen
STORE "" TO lcMajPict,lcFree_Clr,lcNonMajPi,lcNonMajTl,lcColorTlt

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)
lcMajPict  = gfItemMask("PM")
*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]
    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lcNonMajPi = IIF(EMPTY(lcNonMajPi) .OR. laMajSegs[lnI,1]='C',;
                 laMajSegs[lnI,3],;
                 lcNonMajPi + laMajSegs[lnI-1,6] + laMajSegs[lnI,3])
    lcNonMajTl = IIF(EMPTY(lcNonMajTl) .OR. laMajSegs[lnI,1]='C',;
                 PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])),;
                 lcNonMajTl + laMajSegs[lnI-1,6] + PADR(laMajSegs[lnI,2],LEN(laMajSegs[lnI,3])))
  ENDIF                     
  *-- If you Find Color Type or Find Free Type and current type not Free.
  IF laMajSegs[lnI,1] = 'C' OR (!EMPTY(lcFree_Clr) AND laMajSegs[lnI,1] != 'F')
    EXIT
  ENDIF   && end If you Find Color Type or Find Free Type and current type not Free.
ENDFOR    && end Loop Around Non Major elements.
lnMajorLen = LEN(lcMajPict)
STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen

IF EMPTY (lcNonMajTl)
  lcColorTlt = 'Color'
ELSE 
  lcColorTlt = ALLTRIM(lcNonMajTl)
ENDIF
*-- end of lfEvalSegs.

*:****************************************************************
*: Name        : lfgetcare
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : 
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfgetcare()
*:****************************************************************
*
FUNCTION lfgetcare
PARAMETERS lccarlbl , lcCareDsc1 , lcCareDsc2 , lcCareDsc3 , lcCareDsc4 , lcCareDsc5
IF SEEK(Style, "Style") 
  lccarlbl=Style.cCarLbl
  IF !EMPTY(Style.cCarLbl)
    = gfRltFld(STYLE.cCarLbl , @laCare1 , 'CCARLBL')
    = gfRltFld(STYLE.cCarLbl , @laCare2 , 'CCARLBL')
    = gfRltFld(STYLE.cCarLbl , @laCare3 , 'CCARLBL')
    = gfRltFld(STYLE.cCarLbl , @laCare4 , 'CCARLBL')
    = gfRltFld(STYLE.cCarLbl , @laCare5 , 'CCARLBL')
  ENDIF
ENDIF
*--End of.

*:****************************************************************
*: Name        : lfChkVend
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : 
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfChkVend()
*:****************************************************************
*
FUNCTION lfChkVend
PARAMETERS lcCutAt,lcSewAt
PRIVATE lnAlias
lnAlias = SELECT()
SELECT MFGOPRHD
IF SEEK('M'+CUTTKTH.CUTTKT+'1 ') .AND. !lInHouse               
  IF SEEK(cContCode,'ApVendor')
    lcCutAt = ApVendor.cVenComp
  ENDIF
ENDIF
IF SEEK('M'+CUTTKTH.CUTTKT+'2 ') .AND. !lInHouse               
  IF SEEK(cContCode,'ApVendor')
    lcSewAt = ALLTRIM(ApVendor.cVenComp)
  ENDIF  
ENDIF
SELECT (lnAlias)
*-- End of lfChkVend.

*:****************************************************************
*: Name        : lfGetCTLin
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : Function to get Cut Ticket Lines
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfGetCTLin()
*:****************************************************************
*
FUNCTION lfGetCTLin
PRIVATE lcScale
lcScale = ""
IF SEEK(CutTktH.CutTkt,'CutTktL')
  SELECT CutTktL   
  SCAN REST WHILE CutTkt+Style+Dyelot+TranCd = CutTktH.CutTkt FOR TranCd = '1'
    *-- Get Color Description
    lcColor = SUBSTR(Style,lnNonMajSt,lnColorLen)
    lcColorDes = gfCodDes(lcColor, 'COLOR')
    SCATTER MEMVAR MEMO
    SELECT (TmpCtTkt)    
    INSERT INTO (TmpCtTkt) FROM MEMVAR
    REPLACE cType    WITH '5',;
    		cCutTkt  WITH CutTktH.CutTkt,;
            cClrDesc WITH lcColorDes,;
            cPattern WITH SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
            cRev_No  WITH RIGHT(ALLTRIM(STYLE.PATTERN),2),;
            cMarker  WITH lcPrmMrk
    = lfReplace()
    FOR lnCountr = 1 TO 8
      lcCountr = STR(lnCountr,1)        
      REPLACE nQty&lcCountr WITH m.Qty&lcCountr
    ENDFOR
    lcScale = Style.Scale
    REPLACE nTotQty   WITH nQty1+nQty2+nQty3+nQty4+nQty5+nQty6+nQty7+nQty8,;
            cLineType WITH "PROPOSED";
            cScale    WITH lcScale
    *-- Function to get the yards qty.cc
    lnPieces = 0
    lnYards  = lfGetYrds()
    REPLACE nYards WITH lnYards
    REPLACE nPieces WITH lnPieces
    
    *-- Insert a record for the Actual.
  *B604830,1 AMH add the marker field [Start]
  *INSERT INTO (TmpCtTkt);
       (cType,cCutTkt,cLineType,cScale) VALUES;
       ('5',CutTktH.CutTkt,"ACTUAL",lcScale)
  INSERT INTO (TmpCtTkt);
       (cType,cCutTkt,cLineType,cScale,cPattern,cRev_No,cMarker) VALUES;
       ('5',CutTktH.CutTkt,"ACTUAL",lcScale,SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),RIGHT(ALLTRIM(STYLE.PATTERN),2),lcPrmMrk)
  *B604830,1 AMH [End]
    = lfReplace()
    SELECT CutTktL
  ENDSCAN
ENDIF
*--End of lfGetCTLin.

******************************************************************
*: Name        : lfGetYrds
*: Developer   : Ahmed Abdel Naby (AAN)
*: Date        : 16/10/2000
*: Purpose     : 
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfGetYrds()
*:****************************************************************
*
FUNCTION lfGetYrds
PRIVATE lnRetValue, lnAlias
lnAlias = SELECT()
lnRetValue = 0
IF SEEK('M'+CUTTKTH.CUTTKT+PADR(lcPrmFab,19)+SUBSTR(CUTTKTL.Style,lnNonMajSt,lnColorLen),'cTktBom')
   SELECT cTktBom
   lnPieces   = Pieces
   lnUntQty   = UntQty
   lnRetValue = Pieces * lnUntQty
ENDIF
SELECT(lnAlias )
RETURN lnRetValue

*AMH 03/22/2001 Recollecting bill of material data [Start]
******************************************************************
*: Name        : lfGetBom
*: Developer   : Ahmed Maher (AMH)
*: Date        : 03/22/2001
*: Purpose     : Get Bom Lines.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfGetBom()
*:****************************************************************
*
FUNCTION lfGetBom

PRIVATE nConv,lcItem, lcVendor, lcClr_Des, lcFabDesc, lcUomUse, lcUomBuy, lnPieces, lnUntQty, lnNeed, lcClr_Ref
STORE '' to lcItem, lcClr_Des, lcFabDesc, lcUomUse, lcUomBuy, lcClr_Ref, lcVendor
STORE 0 to nConv , lnPieces, lnUntQty, lnNeed
llPrmFound = .F.
SELECT FABRIC
lcOrder = SET('ORDER')
SET ORDER TO FABRIC
IF SEEK('M'+CutTktH.CutTkt,'cTktBom')
  SELECT cTktBom
  lnNum = 1
  lnFrsRec = RECNO()
  LOCATE REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+CUTTKTH.CUTTKT;
         FOR CCATGTYP = 'F'
  IF FOUND()
    lnSubNum = 65
    lnSubNum1 = 65
    lnFabRec = RECNO()
    IF !EMPTY(lcPrmFab)
      =SEEK('M'+CUTTKTH.CUTTKT+PADR(lcPrmFab,19))
      LOCATE REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+CUTTKTH.CUTTKT+PADR(lcPrmFab,19);
             FOR CCATGTYP = 'F' .AND. SUBSTR(ITEM,1,1) <> 'L'
      IF FOUND()
        *B604575,4 AMH Scan for all color of brimary fabric [Start]
        *IF SEEK(PADR(ITEM,7)+ICLR,'FABRIC')
        *  lcClr_des = gfCodDes(cTktBom.Iclr , 'COLOR     ')
        *  = gfRltFld(CTKTBOM.ICLR , @laColor , 'COLOR     ')
        *  lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(lcRltColor)),1,21)
        *  lcPrmClr = CTKTBOM.ICLR
        *  llPrmFound = .T.
        *  IF SEEK(TYP+ITEM+ICLR+STYLE.CSTYMAJOR,'BOM')
        *    lcMarker = BOM.CMARKER
        *  ELSE
        *    IF SEEK(TYP+ITEM+'******'+STYLE.CSTYMAJOR,'BOM')
        *      lcMarker = BOM.CMARKER
        *    ELSE
        *      lcMarker = ''
        *    ENDIF
        *  ENDIF
        *  lcPrmMrk = lcMarker
        *  INSERT INTO (TmpCtTkt);
        *              (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
        *              ('2',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
        *              IIF(EMPTY(FABRIC.CONTENT),'',' ')+UPPER(TRIM(FABRIC.DESC))+;
        *              IIF(EMPTY(FABRIC.DESC),'',' ')+IIF(EMPTY(FABRIC.PATTERN),'',;
        *              'PATTERN #'+UPPER(TRIM(FABRIC.PATTERN))),1,96),'SELF')
        *  = lfReplace()
        *  INSERT INTO (TmpCtTkt);
        *              (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cMarker,;
        *              cClr_Ref,nPieces,nUntQty) VALUES;
        *              ('2M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum),'SELF',STYLE.PATTERN,lcMarker,;
        *              lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY)
        *  = lfReplace()
        *  lnSubNum = lnSubNum + 1
        *  lnSubNum1 = lnSubNum1 + 1
        *ENDIF
        lcPrmClr = ''
        SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+CUTTKTH.CUTTKT+PADR(lcPrmFab,19);
             FOR CCATGTYP = 'F' .AND. SUBSTR(ITEM,1,1) <> 'L'
          IF SEEK(PADR(ITEM,7)+ICLR,'FABRIC')
            lcClr_des = gfCodDes(cTktBom.Iclr , 'COLOR     ')
            = gfRltFld(CTKTBOM.ICLR , @laColor , 'COLOR     ')
            lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(lcRltColor)),1,21)
            lcPrmClr = IIF(EMPTY(lcPrmClr),CTKTBOM.ICLR,lcPrmClr)
            lcCurClr = CTKTBOM.ICLR
            llPrmFound = .T.
            *B604830,1 AMH Get the marker form ctktbom insted of bom [Start]
            *IF SEEK(TYP+ITEM+ICLR+STYLE.CSTYMAJOR,'BOM')
            *  lcMarker = BOM.CMARKER
            *ELSE
            *  IF SEEK(TYP+ITEM+'******'+STYLE.CSTYMAJOR,'BOM')
            *    lcMarker = BOM.CMARKER
            *  ELSE
            *    lcMarker = ''
            *  ENDIF
            *ENDIF
            lcMarker = CMARKER
            *B604830,1 AMH [End]
            lcPrmMrk = lcMarker
            IF lcPrmClr = lcCurClr
              INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
                          ('2',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                          IIF(EMPTY(FABRIC.CONTENT),'',' ')+UPPER(TRIM(FABRIC.DESC))+;
                          IIF(EMPTY(FABRIC.DESC),'',' ')+IIF(EMPTY(FABRIC.PATTERN),'',;
                          'PATTERN #'+UPPER(TRIM(FABRIC.PATTERN))),1,96),'SELF')
              = lfReplace()
              *B604830,1 AMH add the fabric field [Start]
              *INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cRev_No,cMarker,;
                          cClr_Ref,nPieces,nUntQty) VALUES;
                          ('2M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),'SELF',;
                           SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                           RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY)
              INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cRev_No,cMarker,;
                          cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                          ('2M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),'SELF',;
                           SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                           RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY,"")
              *B604830,1 AMH [End]
              = lfReplace()
              lnSubNum1 = lnSubNum1 + 1
            ELSE
              *B604830,1 AMH add the marker field [Start]
              *INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,nNum,cSubNum,cClr_Ref,nPieces,nUntQty) VALUES;
                          ('2M',CTKTBOM.CUTTKT,lnNum,' ',lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY)
              INSERT INTO (TmpCtTkt);
                          (cType,cCuttkt,cPattern,cRev_No,cMarker,nNum,cSubNum,cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                          ('2M',CTKTBOM.CUTTKT,SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                           RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lnNum,' ',lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY,"")
              *B604830,1 AMH [End]
              = lfReplace()
            ENDIF
            lnSubNum = lnSubNum + 1
          ENDIF
        ENDSCAN
        *B604575,4 AMH [End]
      ENDIF
    ENDIF
    GOTO lnFabRec
    *B604575,4 AMH Add Check for new item [Start]
    *lcNewItem = ''
    lcNewItem = SPACE(19)
    lcNewDesc = SPACE(20)
    *SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+CUTTKTH.CUTTKT;
              FOR CCATGTYP = 'F' .AND. SUBSTR(ITEM,1,1) <> 'L';
              .AND. IIF(llPrmFound,ITEM+ICLR<>PADR(lcPrmFab,19)+lcPrmClr,.T.)
    SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+CUTTKTH.CUTTKT;
              FOR CCATGTYP = 'F' .AND. SUBSTR(ITEM,1,1) <> 'L'
      *llNewItem = IIF(lcNewItem=ITEM .OR. IIF(llPrmFound,ITEM=PADR(lcPrmFab,19),.F.),.F.,.T.)
      llNewItem = ((ITEM<>PADR(lcPrmFab,19)) .OR. ALLTRIM(UPPER(DESC))<>'SELF') .AND. (ITEM+DESC<>lcNewItem+lcNewDesc)
      lcNewItem = ITEM
      lcNewDesc = DESC
    *B604575,4 AMH [End]
      IF SEEK(PADR(ITEM,7)+ICLR,'FABRIC')
        lcClr_des = gfCodDes(cTktBom.Iclr , 'COLOR     ')
        = gfRltFld(CTKTBOM.ICLR , @laColor , 'COLOR     ')
        lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(lcRltColor)),1,21)
        IF llNewItem
          *B604830,1 AMH Get the marker form ctktbom insted of bom [Start]
          *IF SEEK(TYP+ITEM+ICLR+STYLE.CSTYMAJOR,'BOM')
          *  lcMarker = BOM.CMARKER
          *ELSE
          *  IF SEEK(TYP+ITEM+'******'+STYLE.CSTYMAJOR,'BOM')
          *    lcMarker = BOM.CMARKER
          *  ELSE
          *    lcMarker = ''
          *  ENDIF
          *ENDIF
          lcMarker = CMARKER
          *B604830,1 AMH [End]
          *B604575,1 AMH Use ctktbom.desc insted of fabric.desc [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
                      ('2',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                      IIF(EMPTY(FABRIC.CONTENT),'',' ')+UPPER(TRIM(FABRIC.DESC))+;
                      IIF(EMPTY(FABRIC.DESC),'',' ')+IIF(EMPTY(FABRIC.PATTERN),'',;
                      'PATTERN #'+UPPER(TRIM(FABRIC.PATTERN))),1,96),;
                      UPPER(SUBSTR(ALLTRIM(FABRIC.DESC),1,20)))
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cFabDesc1) VALUES;
                      ('2',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                      IIF(EMPTY(FABRIC.CONTENT),'',' ')+UPPER(TRIM(FABRIC.DESC))+;
                      IIF(EMPTY(FABRIC.DESC),'',' ')+IIF(EMPTY(FABRIC.PATTERN),'',;
                      'PATTERN #'+UPPER(TRIM(FABRIC.PATTERN))),1,96),;
                      UPPER(SUBSTR(ALLTRIM(CTKTBOM.DESC),1,20)))
          *B604575,1 AMH [End]
          = lfReplace()
          *B604575,4 AMH print csubnum1 insted of csubnum [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cMarker,;
                      cClr_Ref,nPieces,nUntQty) VALUES;
                      ('2M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum),UPPER(SUBSTR(ALLTRIM(CTKTBOM.DESC),1,9)),;
                      STYLE.PATTERN,lcMarker,lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY)
          *B604830,1 AMH add the fabric field [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cRev_No,cMarker,;
                      cClr_Ref,nPieces,nUntQty) VALUES;
                      ('2M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),UPPER(SUBSTR(ALLTRIM(CTKTBOM.DESC),1,9)),;
                       SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY)
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cRev_No,cMarker,;
                      cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                      ('2M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),UPPER(SUBSTR(ALLTRIM(CTKTBOM.DESC),1,9)),;
                       SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY,SUBSTR(CTKTBOM.ITEM,1,7))
          *B604830,1 AMH [End]
          *B604575,4 AMH [End]
          = lfReplace()
          lnSubNum1 = lnSubNum1 + 1
        ELSE
          *B604575,4 AMH Dont print csubnum in case of colors of existing fabric [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cPattern,cMarker,cClr_Ref,nPieces,nUntQty) VALUES;
                      ('2M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum),STYLE.PATTERN,lcMarker,lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY)
          *B604830,1 AMH add the marker field [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cClr_Ref,nPieces,nUntQty) VALUES;
                      ('2M',CTKTBOM.CUTTKT,lnNum,' ',lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY)
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,cPattern,cRev_No,cMarker,nNum,cSubNum,cClr_Ref,nPieces,nUntQty,cFabric) VALUES;
                      ('2M',CTKTBOM.CUTTKT,SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lnNum,' ',lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY,SUBSTR(CTKTBOM.ITEM,1,7))
          *B604830,1 AMH [End]
          *B604575,4 AMH [End]
          = lfReplace()
        ENDIF
        lnSubNum = lnSubNum + 1
      ENDIF
    ENDSCAN
    lnNum = lnNum + 1
  ENDIF
  GOTO lnFrsRec
  LOCATE REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+CUTTKTH.CUTTKT;
         FOR CCATGTYP = 'F' .AND. SUBSTR(ITEM,1,1) = 'L'
  IF FOUND()
    STORE 65 TO lnSubNum , lnSubNum1
    lcNewItem = ''
    SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+CUTTKTH.CUTTKT+'L';
              FOR CCATGTYP = 'F'
      llNewItem = IIF(lcNewItem=ITEM,.F.,.T.)
      lcNewItem = ITEM
      IF SEEK(PADR(ITEM,7)+ICLR,'FABRIC')
        =SEEK(FABRIC.VENDOR,'APVENDOR')
        lcClr_des = gfCodDes(cTktBom.Iclr , 'COLOR     ')
        = gfRltFld(CTKTBOM.ICLR , @laColor , 'COLOR     ')
        lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(lcRltColor)),1,21)
        IF llNewItem
          *B604830,1 AMH Get the marker form ctktbom insted of bom [Start]
          *IF SEEK(TYP+ITEM+ICLR+STYLE.CSTYMAJOR,'BOM')
          *  lcMarker = BOM.CMARKER
          *ELSE
          *  IF SEEK(TYP+ITEM+'******'+STYLE.CSTYMAJOR,'BOM')
          *    lcMarker = BOM.CMARKER
          *  ELSE
          *    lcMarker = ''
          *  ENDIF
          *ENDIF
          lcMarker = CMARKER
          *B604830,1 AMH [End]
          *B604575,1 AMH Use apvendor.cshrtnam insted of apvendor.cvendcode [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cVendor) VALUES;
                      ('3',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                      IIF(EMPTY(FABRIC.CONTENT),'',' ')+UPPER(TRIM(FABRIC.DESC))+IIF(EMPTY(FABRIC.DESC),'',' ')+;
                      UPPER(TRIM(FABRIC.PATTERN)),1,96),APVENDOR.CVENDCODE)
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc,cVendor) VALUES;
                      ('3',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum1),SUBSTR(UPPER(TRIM(FABRIC.CONTENT))+;
                      IIF(EMPTY(FABRIC.CONTENT),'',' ')+UPPER(TRIM(FABRIC.DESC))+IIF(EMPTY(FABRIC.DESC),'',' ')+;
                      UPPER(TRIM(FABRIC.PATTERN)),1,96),APVENDOR.CSHRTNAM)
          = lfReplace()
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cMarker,;
                      cClr_Ref,nPieces,nUntQty) VALUES;
                      ('3M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum),APVENDOR.CVENDCODE,;
                      STYLE.PATTERN,lcMarker,lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY)
          *B604830,1 AMH add the fabric field [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cRev_No,cMarker,;
                      cClr_Ref,nPieces,nUntQty) VALUES;
                      ('3M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum),APVENDOR.CSHRTNAM,;
                       SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY)
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cFabDesc1,cPattern,cRev_No,cMarker,;
                      cClr_Ref,nPieces,nUntQty,cfabric) VALUES;
                      ('3M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum),APVENDOR.CSHRTNAM,;
                       SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY,SUBSTR(CTKTBOM.ITEM,1,7))
          *B604830,1 AMH [End]
          *B604575,1 AMH [End]
          = lfReplace()
          lnSubNum1 = lnSubNum1 + 1
        ELSE
          *B604830,1 AMH add the marker field [Start]
          *INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,nNum,cSubNum,cClr_Ref,nPieces,nUntQty,cVendor) VALUES;
                      ('3M',CTKTBOM.CUTTKT,lnNum,CHR(lnSubNum),;
                      lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY,APVENDOR.CSHRTNAM)
          INSERT INTO (TmpCtTkt);
                      (cType,cCuttkt,cPattern,cRev_No,cMarker,nNum,cSubNum,cClr_Ref,nPieces,nUntQty,cVendor,cFabric) VALUES;
                      ('3M',CTKTBOM.CUTTKT,SUBSTR(ALLTRIM(STYLE.PATTERN),1,LEN(ALLTRIM(STYLE.PATTERN))-2),;
                       RIGHT(ALLTRIM(STYLE.PATTERN),2),lcMarker,lnNum,CHR(lnSubNum),;
                      lcClr_Ref,CTKTBOM.PIECES,CTKTBOM.UNTQTY,APVENDOR.CSHRTNAM,SUBSTR(CTKTBOM.ITEM,1,7))
          *B604830,1 AMH [End]
          = lfReplace()
        ENDIF
        lnSubNum = lnSubNum + 1
      ENDIF
    ENDSCAN
    lnNum = lnNum + 1
  ENDIF
  GOTO lnFrsRec
  LOCATE REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+CUTTKTH.CUTTKT;
         FOR CCATGTYP = 'T'
  IF FOUND()
    SCAN REST WHILE cImtyp+Cuttkt+Item+Iclr+Mfgcode+Dyelot = 'M'+CUTTKTH.CUTTKT;
              FOR CCATGTYP = 'T'
      SELECT FABRIC
      *B604627,4 WAB (START) - change the type of field LLTKT_PRT to be caracter instead of logic .
      *IF SEEK(PADR(CTKTBOM.ITEM,7)+CTKTBOM.ICLR) .AND. LLTKT_PRT
      
      IF SEEK(PADR(CTKTBOM.ITEM,7)+CTKTBOM.ICLR) .AND. LLTKT_PRT = 'Y'
      *B604627,4 WAB (END) 
      
        =SEEK(VENDOR,'APVENDOR')
        IF CTKTBOM.ISSUE_QTY <> 0
          lcClr_des = gfCodDes(cTktBom.Iclr , 'COLOR     ')
          = gfRltFld(CTKTBOM.ICLR , @laColor , 'COLOR     ')
          lcItem = gfCodDes(ITEM_TYPE , 'ITEM_TYPE ')
          
          *B604575,4 AMH Use ctktbom.desc insted of fabric.desc [Start]
          *lcVendor = APVENDOR.CVENDCODE
          lcVendor = APVENDOR.CSHRTNAM
          *B604575,4 AMH [End]

          lcClr_Ref = SUBSTR(UPPER(TRIM(lcClr_Des)+' '+TRIM(lcRltColor)),1,14)
          lcFabDesc = SUBSTR(LTRIM(UPPER(CFABSHDESC)),1,41)
          lnPieces = CTKTBOM.PIECES
          lnUntQty = CTKTBOM.UNTQTY
          lcUomUse = UOMUSE
          lnNeed = IIF(CTKTUOM$'U ',CTKTBOM.ISSUE_QTY,CTKTBOM.ISSUE_QTY/CONV)
          lcUomBuy = IIF(CTKTUOM$'U ',UOMUSE,UOMBUY)
          INSERT INTO (TmpCtTkt);
               (cType,cCuttkt,nNum,cItem,cVendor,cClr_Ref,cFabDesc2,nPieces,nUntQty,;
                cUomUse,nNeed,cUomBuy,cItem_Type) VALUES;
               ('4',CTKTBOM.CUTTKT,lnNum,lcItem,lcVendor,lcCLr_Ref,lcFabDesc,lnPieces,lnUntQty,;
                lcUomUse,lnNeed,lcUomBuy,FABRIC.ITEM_TYPE)
          = lfReplace()
        ENDIF
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
SELECT FABRIC
SET ORDER TO &lcOrder.
*--End of lfGetBom.

******************************************************************
*: Name        : lfReplace
*: Developer   : Ahmed Maher (AMH)
*: Date        : 04/05/2001
*: Purpose     : Replace cutting ticket header data
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfReplace()
*:****************************************************************
*
FUNCTION lfReplace

lnAlias = SELECT(0)
SELECT (TmpCtTkt)
REPLACE cMajor    WITH lcMajor;
        cSeasCod  WITH lcSeasCod;
        cPackNo   WITH lcPack;
        dComplete WITH ldComplete;
        cSalDesc1 WITH lcSalDesc1;
        cSalDesc2 WITH lcSalDesc2;
        cSalDesc3 WITH lcSalDesc3;
        cSalDesc4 WITH lcSalDesc4;
        cSalDesc5 WITH lcSalDesc5;
        cDivDesc  WITH lcDivDesc;
        cSewAt    WITH lcSewAt;
        cSeasDesc WITH lcSeasDesc;
        cCutAt    WITH lcCutAt;
        cCarLbl   WITH ALLTRIM(lcCarlbl);
        cCarDsc1  WITH lcCareDsc1;
        cCarDsc2  WITH lcCareDsc2;
        cCarDsc3  WITH lcCareDsc3;
        cCarDsc4  WITH lcCareDsc4;
        cCarDsc5  WITH lcCareDsc5;
        nLot_No   WITH CUTTKTH.NLOT_NO
SELECT (lnAlias)
*-- end of lfReplace.

******************************************************************
*: Name        : lfvSelect
*: Developer   : Ahmed Maher (AMH)
*: Date        : 04/30/2001
*: Purpose     : Valid function of select option from OG.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : 
*:****************************************************************
*: Example     : =lfvSelect()
*:****************************************************************
*
FUNCTION lfvSelect
PARAMETERS lcDummy
lcDummy = .T.

IF lcRpSelec = 'S'
  laOgVrFlt[1,1] = 'CUTTKTH.CUTTKT'
  laOgVrFlt[1,2] = 'F'
  laOgVrFlt[1,3] = 'C'
  laOgVrFlt[1,4] = .T.
  laOgVrFlt[1,5] = 'In List'
  laOgVrFlt[1,6] = laVrFltCur[2,2]
  laOgVrFlt[1,7] = 'R'
ELSE
  STORE SPACE(0) TO laOgVrFlt
ENDIF
CLEAR READ
RETURN .T.
*-- end of lfvSelect.