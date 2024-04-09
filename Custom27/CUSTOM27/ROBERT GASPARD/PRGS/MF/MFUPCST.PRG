*:****************************************************************
*: Program file  : MFUPCST.PRG
*: Program desc. : Custom Cost Sheet Global Update.
*: System        : Aria Apparel System - Version 2.7.
*: Module        : Manufacturing  (MF)
*: Developer     : Abdou Elgendy - [ABD]
*: Date          : 01/14/2004.
*: Tracking Job Number : C#121084,1
*:****************************************************************
*: Calls         : 
*:               : FUNCTIONS  : lfUpCstFab , lfEvalSegs , lfSrSty1
*:               :            : lfSrSty2   , lfSRStyle  , lfStySum
*:               :            : lfFabSum   , lfSRVFab   , lfGetTypes
*:               :            : lfChkColor.
*:               -----------------------------------------------
*:               : PROCEDURE  : None.
*:****************************************************************
*: Passed Parameters  : None.
*:****************************************************************
*:C#121084,1.
*:****************************************************************
*:Modifications  :
*:B122459,1 NNA 04/07/2004 Fix Bug of that the update not working ( the problem that the customer
*:B122459,1 NNA            has 18 Chr. for the style structure, not 19)
*:****************************************************************
*:
*-- Start Declaration Variabls
STORE '' TO lcStyMajor,lcMajorPic,lcColorTlt,lcNonMajTlt,lcNonMajPi,;
            lcFree_Clr , lcStyExpr , lcFabExpr
            
STORE 0 TO lnO_T_S,lnMajSeg,lnNonMajSt,lnSupMajSt,lnFreeLen,;
           lnColorLen,lnDummyPos,lnRngAlias,lnMajorLen

llUpdates = .F.

DIMENSION  laIFabNo[1] , laMFabNo[1]
*-- Evaluate NonMajor Type and variables.
= lfEvalSegs ()

*-- End Declaration Variabls

IF !gfSetup()
  RETURN
ENDIF

DO WHILE .T.
  *-- Cal the Option Grid.
  lcExpr = gfOpGrid('MFUPCST' , .T.)

  DO CASE 
    CASE Type('lcExpr') = 'C'
      *- Message Text   :- Are you sure you want to do Global Update for all fabric(s) - style(s).
      *- Message No.    :- 000000.
      *- Buttom Message :- Ok
      *- Buttom Number  :- 00000.
      lcMessage = 'Are you sure you want to do Global Update for '+ IIF(lcExpr = ".T.",'all','selected') +' fabric(s) - style(s).'
      IF gfModalGen('INM00000B00006','F','ALERT',' ',lcMessage) = 1
        EXIT
      ENDIF
    CASE Type('lcExpr') = 'L' .AND.  lcExpr = .F.
      EXIT
  ENDCASE  
ENDDO

IF  Type('lcExpr') = 'L' .AND.  lcExpr = .F.
  RETURN
ENDIF

*-- lcStyExpr , lcFabExpr
IF  Type('lcExpr') = 'C' .AND. lcExpr # ".T."
  lnAndPos  = ATC('AND',lcExpr)
  IF lnAndPos = 0
    IF 'FABRIC.FABRIC' $ lcExpr
      lcFabExpr = lcExpr
    ELSE
      lcStyExpr = lcExpr
    ENDIF
  ELSE
    lcFabExpr = ALLTRIM(LEFT(lcExpr,lnAndPos-1))
    lcExpr    = ALLTRIM(STRTRAN(lcExpr,lcFabExpr,''))
    lcStyExpr = ALLTRIM(SUBSTR(lcExpr,4,LEN(lcExpr)))
  ENDIF    
ENDIF

IF EMPTY(lcFabExpr)
  *- Message Text   :- Global Update completed successful.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  lcMessage = 'You have to select a fabric, Can not proceed.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
  RETURN
ENDIF

*-- Get the Fabric Types , May be one fabric or may be more than one fabric.
IF !lfGetTypes ()
  lcMessage = 'There is no fabric in the style cost sheet.'  
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
  *-- Return if no fabrics found into the style element.
  RETURN
ENDIF

*-- Function to Update the Fabric and other related file.
= lfUpCstFab ()
IF llUpdates
  *- Message Text   :- Global update completed successfuly.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  lcMessage = 'Global update completed successfully.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
ELSE
  *- Message Text   :- nothing change.
  *- Message No.    :- 000000.
  *- Buttom Message :- Ok
  *- Buttom Number  :- 00000.
  lcMessage = 'The selected Item(s) not found in any style cost sheet. Nothing updated.'
  = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
ENDIF

RETURN
*-- End OF Code
*:*************************************************************
*: Name        : lfUpCstFab
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : Function to Update the Fabric and other related file.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfUpCstFab()
*:*************************************************************
*:
FUNCTION lfUpCstFab
PRIVATE lnPrv_Als

lnIFabNum = ALEN(laIFabNo)
lnMFabNum = ALEN(laMFabNo)
lnPrv_Als = SELECT (0)

SELECT BOM
SET ORDER TO BOMITEM

lcOldFabrc = ''
SELECT FABRIC
SCAN FOR &lcFabExpr
  WAIT WINDOW "Updating the cost of the "+Fabric.Fabric+" item for all styles using this fabric. Please wait!" NOWAIT
  *-- Get the First Fabric Only, not all fabric colors.
  SELECT BOM
  *- Update All the Imported Styles that include the fabric into the Style 
  *-- Cost Sheet.
  FOR lnLoop  =  1 TO lnIFabNum
    lcIcostNo = laIFabNo[lnLoop]
    IF SEEK(lcIcostNo+PADR(Fabric.Fabric,19),"BOM")
      SCAN REST WHILE typ+item+iclr+citmmajor+citmmask = lcIcostNo + PADR(Fabric.Fabric,19)
        llUpdates = .T.
        *-- Check for the Style .
        llUpdate = .F.
        IF Bom.iclr = '******'
          llUpdate = lfChkColor (Fabric.Fabric,Fabric.Color)
        ELSE
          IF Bom.iclr = Fabric.Color
            llUpdate = .T.
          ENDIF
        ENDIF

        IF llUpdate
          *-- Update The Style And Stydye.
          lcCurtStyl = IIF(RIGHT(Bom.citmMask,1)='*',LEFT(Bom.citmmajor,lnMajorLen),Bom.citmMask)  
          IF SEEK(lcCurtStyl,"STYLE") .AND. !(Style.Make).AND. (EMPTY(lcStyExpr) .OR. Eval(lcStyExpr))
            lnOldFCost = TotCost
            *-- Update the BOM file for the new fost for the style that use this fabric.
            REPLACE UntCost With Fabric.CostUse ,;
                    TotCost WITH (Fabric.CostUse * NbomTotQty)
          
            *-- Update the Style file for for the new cost.
            SELECT STYLE
            SCAN REST WHILE Style = lcCurtStyl
              REPLACE NiCost&lcIcostNo WITH (NiCost&lcIcostNo - lnOldFCost) + Bom.TotCost ,;
                      TotCost          WITH (TotCost - lnOldFCost) + Bom.TotCost
              IF TotStk = 0
                REPLACE Ave_Cost WITH (Ave_Cost - lnOldFCost) + Bom.TotCost
              ENDIF
            ENDSCAN
            *-- Update the Style file for for the new cost.

            *-- Update the Style Dye file for all wearhouse.
            IF SEEK(lcCurtStyl,"STYDYE")
              SELECT STYDYE
              SCAN REST WHILE style+cwarecode+dyelot = lcCurtStyl
                 IF TotStk = 0
                   REPLACE Ave_Cost WITH (Ave_Cost - lnOldFCost) + Bom.TotCost
                 ENDIF
              ENDSCAN
            ENDIF
            *-- End Update the Style Dye file for all wearhouse.
          ENDIF
        ENDIF  
      ENDSCAN   
    ENDIF
  ENDFOR

  SELECT BOM
  *- Update All the manufactring Styles that include the fabric into the Style 
  *-- Cost Sheet.
  FOR lnLoop  =  1 TO lnMFabNum
    lcIcostNo = laMFabNo[lnLoop]
    IF SEEK(lcIcostNo+PADR(Fabric.Fabric,19),"BOM")
      SCAN REST WHILE typ+item+iclr+citmmajor+citmmask = lcIcostNo + PADR(Fabric.Fabric,19)
        llUpdates = .T.      
        *-- Check IF first recored for fabric color and iclr = '******' , then The cost 
        *-- should be updated with the first color from the fabric file.
        llUpdate = .F.
        IF Bom.iclr = '******'
          llUpdate = lfChkColor (Fabric.Fabric,Fabric.Color)
        ELSE
          IF Bom.iclr = Fabric.Color
            llUpdate = .T.
          ENDIF
        ENDIF

        IF llUpdate

          *-- Check for the Style .
          *-- Update The Style And Stydye.
          
          *B122459,1 NNA 04/07/2004 (Begin) get the true lenth of the style (Alltrim)
          *lcCurtStyl = IIF(RIGHT(Bom.citmMask,1)='*',LEFT(Bom.citmmajor,lnMajorLen),Bom.citmMask)
          lcCurtStyl = IIF(RIGHT(ALLTRIM(Bom.citmMask),1)='*',LEFT(Bom.citmmajor,lnMajorLen),Bom.citmMask)
          *B122459,1 NNA (End)
          
          IF SEEK(lcCurtStyl,"STYLE") .AND. Style.Make .AND. (EMPTY(lcStyExpr) .OR. Eval(lcStyExpr))
            *-- Svae the Old cost for the fabric.
            lnOldFCost = TotCost
            *-- Update the BOM file for the new fost for the style that use this fabric.
            REPLACE UntCost With Fabric.CostUse ,;
                    TotCost WITH (Fabric.CostUse * NbomTotQty)
          
            *-- Update the Style file for for the new cost.
            SELECT STYLE
            SCAN REST WHILE Style = lcCurtStyl
              REPLACE NMCost&lcIcostNo WITH (NMCost&lcIcostNo - lnOldFCost) + Bom.TotCost ,;
                      TotCost          WITH (TotCost - lnOldFCost) + Bom.TotCost
              IF TotStk = 0
                REPLACE Ave_Cost WITH (Ave_Cost - lnOldFCost) + Bom.TotCost
              ENDIF
            ENDSCAN
            *-- Update the Style file for for the new cost.

            *-- Update the Style Dye file for all wearhouse.
            IF SEEK(lcCurtStyl,"STYDYE")
              SELECT STYDYE
              SCAN REST WHILE style+cwarecode+dyelot = lcCurtStyl
                IF TotStk = 0
                  REPLACE Ave_Cost WITH (Ave_Cost - lnOldFCost) + Bom.TotCost
                ENDIF
              ENDSCAN
            ENDIF
            *-- End Update the Style Dye file for all wearhouse.
          ENDIF
        ENDIF  
      ENDSCAN   
    ENDIF
  ENDFOR
ENDSCAN


SELECT(lnPrv_Als)
*-- End OF lfUpCstFab.
*:*************************************************************
*: Name        : lfEvalSegs
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : Evaluate NonMajor Type and variables.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfEvalSegs()
*:*************************************************************
*:
FUNCTION lfEvalSegs

lnMajSeg    = gfItemMask('SM')  && No. of major segments.
lcStyMajor  = gfItemMask('HM')
lcMajorPic  = gfItemMask('PM')
lnMajorLen  = LEN(lcMajorPic)


*-- Compute Free/Color Items in Style code Structure. [Begin]
DIMENSION laMajSegs[1,1]
= gfItemMask(@laMajSegs)

*-- Loop Around Non Major elements.
FOR lnI = lnMajSeg + 1 TO ALEN(laMajSegs,1)
  IF laMajSegs[lnI,1] $ 'CF'
    lcFree_Clr = laMajSegs[lnI,1]

    lnNonMajSt = IIF(lnNonMajSt=0 .OR. laMajSegs[lnI,1]='C',laMajSegs[lnI,4],lnNonMajSt)      && This item hold seg. start position.
    lnSupMajSt = lnNonMajSt
       
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

STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
*-- Compute Free/Color Items in Style code Structure. [End]

*-- End OF lfEvalSegs
*:*************************************************************
*: Name        : lfSrSty1
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : Style In Range Filter. 1.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfSrSty1()
*:*************************************************************
*:
FUNCTION lfSrSty1

PARAMETERS lcParm
=lfSRStyle(lcParm,"1")

*-- End Of lfSrSty1.
*:*************************************************************
*: Name        : lfSrSty2
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : Style In Range Filter. 2
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfSrSty2()
*:*************************************************************
*:
FUNCTION lfSrSty2
PARAMETERS lcParm

=lfSRStyle(lcParm,"2")

*-- end of lfSrSty2.
*:*************************************************************
*: Name        : lfSRStyle
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : Style In Range Filter.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfSRStyle()
*:*************************************************************
*:
FUNCTION lfSRStyle
PARAMETERS lcParm,lcFlagNo

DO CASE
  CASE lcParm = 'S'  && Set code
    lnRngAlias = SELECT(0)
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClrSty&lcFlagNo = .F.
    SELECT (lnRngAlias)
ENDCASE

*-- End of lfSRStyle.
*:*************************************************************
*: Name        : lfStySum
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : sum a specific field for the current style in style file
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfStySum()
*:*************************************************************
*:
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnTotcomp = 0

IF RECCOUNT('STYLE') != 0
  lnStyRec = RECNO('STYLE')
  SELECT Style_X
  SUM &lcCOMP TO lnTotcomp WHILE ALLTRIM(cStyMajor) = ALLTRIM(lcSty)
  SELECT Style
  IF BETWEEN(lnStyRec,1,RECCOUNT())
    GO lnStyRec
  ENDIF  
  DO CASE
    CASE lnAddToVar = 1
  	  lnO_T_S = lnTotcomp
    CASE lnAddToVar = 2
      lnO_T_S = lnO_T_S + lnTotcomp
    CASE lnAddToVar = 3
      lnO_T_S = lnO_T_S - lnTotcomp
  ENDCASE
ENDIF  
RETURN INT(lnTotcomp)
*-- end of lfStySum.
*:*************************************************************
*: Name        : lfFabSum
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : sum a specific field for the current fabric in fabric file
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid,fabric browse calculated fields.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfFabSum()
*:*************************************************************
*:
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp

PRIVATE lnFabRec
lnTotcomp = 0
IF RECCOUNT() != 0
  lnFabRec = RECNO('FABRIC')
  SELECT Fabric_X
  SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
  SELECT Fabric
  IF BETWEEN(lnFabRec,1,RECCOUNT())
    GO lnFabRec
  ENDIF
ENDIF  
RETURN INT(lnTotcomp)
*-- End of lfFabSum.
*:*************************************************************
*: Name        : lfSRVFab
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : control browsing primary fabric and validate 
*:             : selecting it in inlist function.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : Option Grid.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfSRVFab()
*:*************************************************************
*:
FUNCTION lfSRVFab
PARAMETERS lcParm

PRIVATE lcAlias,llHaveSty
DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to primary fabric
    *-- unique index.
    USE (gcDataDir+'Fabric') AGAIN ALIAS FABRIC_X ORDER TAG FABRIC IN 0
    SELECT FABRIC   
    SET ORDER TO TAG cFabric    
    SET RELATION TO FABRIC.FABRIC INTO FABRIC_X
    GO TOP IN FABRIC
  CASE lcParm = 'R'  && Reset code
    USE IN FABRIC_X
    SELECT FABRIC
    SET ORDER TO TAG FABRIC
    llClearFab = .F.
ENDCASE

*-- End of lfSRVFab.
*:*************************************************************
*: Name        : lfGetTypes
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : Get the Fabric Types , May be one fabric or 
*:             : may be more than one fabric.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Called from : the Program.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfGetTypes()
*:*************************************************************
*:
FUNCTION lfGetTypes

DIMENSION   laSetUp[10,2] , laIFabNo[1] , laMFabNo[1]
STORE '' TO laSetUp       , laIFabNo    , laMFabNo
STORE 0  TO lnIAryCont , lnMAryCont

laSetUp[1,1] = 'M_cIType1'
laSetUp[2,1] = 'M_cIType2'
laSetUp[3,1] = 'M_cIType3'
laSetUp[4,1] = 'M_cIType4'
laSetUp[5,1] = 'M_cIType5'


laSetUp[06,1] = 'M_cMType1'
laSetUp[07,1] = 'M_cMType2'
laSetUp[08,1] = 'M_cMType3'
laSetUp[09,1] = 'M_cMType4'
laSetUp[10,1] = 'M_cMType5'

=gfGetMemVar(@laSetUp)

lcIType1   = laSetUp[1,2]
lcIType2   = laSetUp[2,2]
lcIType3   = laSetUp[3,2]
lcIType4   = laSetUp[4,2]
lcIType5   = laSetUp[5,2]

*-- this loop for Sty. Imp Cost Elm. 1,2,3,4,5
FOR lnCheck = 1 TO 5  
  lcFabVar  = "lcIType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) $ 'FT' 
    lnIAryCont = lnIAryCont + 1
    IF !EMPTY(laIFabNo[1])
      DECLARE laIFabNo[lnIAryCont]
    ENDIF
    laIFabNo[lnIAryCont] = STR(lnCheck,1)
  ENDIF
ENDFOR


lcMType1   = laSetUp[06,2]
lcMType2   = laSetUp[07,2]
lcMType3   = laSetUp[08,2]
lcMType4   = laSetUp[09,2]
lcMType5   = laSetUp[10,2]

*-- this loop for Sty. Manfactring Cost Elm. 1,2,3,4,5
FOR lnCheck = 1 TO 5  
  lcFabVar  = "lcMType" + STR(lnCheck,1)
  IF EVAL(lcFabVar) $ 'FT' 
    lnMAryCont = lnMAryCont + 1
    IF !EMPTY(laMFabNo[1])
      DECLARE laMFabNo[lnMAryCont]
    ENDIF
    laMFabNo[lnMAryCont] = STR(lnCheck,1)
  ENDIF
ENDFOR


llRETURN =  (lnIAryCont > 0) .OR. (lnMAryCont > 0)
*-- If no fabric don't complete the process.
RETURN llRETURN
*-- End OF lfGetTypes
*:*************************************************************
*: Name        : lfChkColor
*: Developer   : Abdou Elgendy [ABD]
*: Date        : 01/14/2004.
*: Purpose     : Check if we at the first color or not.
*:*************************************************************
*: Calls     : 
*:             Procedures : ....
*:             Functions  : ....
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example     : = lfChkColor()
*:*************************************************************
*:
FUNCTION lfChkColor
PARAMETERS lcCurrFab , lcCurrColr
PRIVATE lnPrv_Als , llRETURN

llRETURN = .F.

lnPrv_Als = SELECT (0)
SELECT FABRIC
lnOld_RecN = RECNO()
IF SEEK(lcCurrFab)
  llRETURN = (lcCurrColr = Fabric.Color)
ENDIF

SELECT FABRIC
IF BETWEEN(lnOld_RecN,1,RECCOUNT())
  GOTO lnOld_RecN
ENDIF

SELECT(lnPrv_Als)


RETURN llRETURN
*-- End OF lfChkColor
*:*************************************************************