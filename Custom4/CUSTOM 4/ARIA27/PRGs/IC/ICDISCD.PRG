*:****************************************************************************************
*: Program file  : ICDISCD.PRG
*: Program desc. : Assign Discount Codes.
*: System        : Aria Apparel System (A27).
*: Module        : Inventory Contol  (IC)
*: Developer     : ABDOU ELGENDI - (ABD) Due to C#102050,1
*: Date          : 11/23/2000
*:****************************************************************************************
*: Calls 
*:               : FUNCTIONS  : gfOpGrid  , lfEvalSegs , lfsrvSty
*:               :            : lfStySum  , lfvDisCod  , lfvApply
*:               :            : lfOpenFils, lfClosFils , lfvWHouse
*:               :            : lfvclose  .
*:               -----------------------------------------------
*:               : PROCEDURE  : 
*:               :            : 
*:****************************************************************************************
*: Passed Parameters  : None
*:****************************************************************************************
*:C#102050,1          : 
*:****************************************************************************************
*:Modifications :
*B604093,1 ABD 01/07/2001 Fix the Following bugs:-
*B604093,1                1- There are infinite loop when select 3 ware houses descending.
*B604093,1                2- For all Cases all location in the mover screen must be 
*B604093,1                   Finished goods inventory
*B604093,1                3- browse style that I have acsess on it. in case of Point of sale
*E301525,1 HS  01/07/2001 When modifying a style(s) that already exist, and if the
*E301525,1                Inter-Company EDI module is installed and setup and the
*E301525,1                "System Type" system setup is set to "Back Office", mark any
*E301525,1                "Selection Codes" that contain the edited style(s) as
*E301525,1                "To Be Transmitted".
*B604205,1 KAM 12/03/2001 returned the style mask field
*E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char 
*:****************************************************************************************
*:
*-- Declaration variables.

*-- lcItmTtl   : Style Title
*-- lcItmPct   : Style Picture
*-- lnItmWid   : Style Length
*-- lnDiscCode : Discount code item
*-- llOpenCode : .T. if this program open codes file.
*-- llOpenStyl : .T. if this program open codes file.
*-- llOpenStyD : .T. if this program open codes file.
*-- llOpenWare : .T. if this program open warehouse file.
*-- llMultiwh  : .T. if the multiple warehouse feature is available.
*-- lcRpKidSty : Hold the kid. style group code.
*-- lcRpFshSty : Hold the Fsh. style group code.
*B604205,1 KAM 12/03/2001 [start]
*--assign space to variable which found in syrepuvr file
STORE ' ' TO lcRpStyMsk
*B604205,1 KAM [end]


STORE ''  To lcItmTtl   , lcItmPct   , lnItmWid ,lasource,latarget , lcExpr ,;
             lcNonMajPi , lcNonMajTl, lcRpKidSty , lcRpFshSty
STORE .F. To llOpenCode , llOpenStyl ,llOpenStyD ,llOpenWare
STORE 0   TO lnFreeLen , lnColorLen ,lnNonMajSt,lnMajorLen

*E301525,1 HS Add these lines to open EDI files [Begin]
*-- llOpenCatH : Flag to know if the program has opened the "EDI Catalog Header, EDICatgH" file.
*-- llOpenCatD : Flag to know if the program has opened the "EDI Catalog Detail, EDICatgD" file.
*-- llOpenTran : Flag to know if the program has opened the "EDI Transmission, EDITrans" file.
*-- llOpenAcPr : Flag to know if the program has opened the "EDI Account/Partner, EDIAcPrt" file.

STORE .F. To llOpenCatH , llOpenCatD , llOpenTran , llOpenAcPr
*E301525,1 HS Add these lines to open EDI files [End]

DIMENSION lasource[1],latarget[1]
lnDiscCode = 1  && For popup.
llMultiwh  = gfGetMemVar('M_WareHouse')='Y'

*B604093,1 ABD Get the System Type Setup. [Begin]
lcSysType = ''
lcSysType = gfGetMemVar('M_SYSTYPE')
lcSysType = IIF(EMPTY(lcSysType),'M',lcSysType)
llNCInstld = (OCCURS('NC',gcCmpModules) <> 0)
*B604093,1 ABD [End]


IF !gfSetup()
  RETURN
ENDIF

*-- This Array will define after call gfSetup function.
IF ASCAN(laEvntTrig , PADR('LLEAGLS',10)) <> 0
  llEagles = .T.
ENDIF     


*B604093,1 ABD - Add new browse exp in case I run this program from PS module
*B604093,1 ABD - And NC module is install and system type is Point Of sale. [Begin]
lcBrowfor = ""
IF lcSysType = 'P'
  lcBrowfor  = "FOR cOwner = gcCurSite  "
ENDIF
*B604093,1 ABD - [End]
*B604205,1 KAm [start]
*--we move the following statment befor calling option grade to use its values in op grade
lcItmTtl   = gfItemMask('HI')
lcItmPct   = gfItemMask('PI')
lnItmWid   = LEN(lcItmPct)
*B604205,1 KAM [end]
*-- Call the Option Grid.
lcExpr    = gfOpGrid('ICDISCD',.T.)

*-- If user close the option grid return and don't complete the program.
IF TYPE('lcExpr') = "L" .AND. !lcExpr
  RETURN
ENDIF

*B604093,1 ABD - Change the 2 fileds (Kid size group & fashion group)  to be
*B604093,1 ABD - Chr. to fix bug in the Option grid. 
*B604093,1 ABD - Also we will add Cowner filed to the Exp if Company
*B604093,1 ABD - Is Point of sale.[Begin].
*-- This Array will define after call gfSetup function.
IF ASCAN(laEvntTrig , PADR('LLEAGLS',10)) <> 0
  *-- Add the kid size group to scanexp.
  lcScnExp = IIF(EMPTY(lcRpKidSty),'',' .AND. STYLE.ckidszgrp = lcRpKidSty')
  
  *-- Add the kid Fash. group to scanexp.
  IF EMPTY(lcScnExp)
    lcScnExp = IIF(EMPTY(lcRpFshSty),'','STYLE.cstygrp = lcRpFshSty')
  ELSE
    lcScnExp = IIF(EMPTY(lcRpFshSty),lcScnExp,lcScnExp + ' .AND. STYLE.cstygrp = lcRpFshSty')
  ENDIF
  *-- add the new exp to the scan exp.
  lcExpr = lcExpr + lcScnExp
ENDIF
*-- add Owner exp to scan to scan exp if company is Point Of Sale.
IF lcSysType = 'P'
  lcExpr = lcExpr + ' .AND. STYLE.cOwner = gcCurSite'
ENDIF
*B604093,1 [End]

*B604205,1 KAM[start]
*--add to style mask field result to expression
lcExpr=IIF( '.T.' $ lcExpr,"LIKE(PADR(STRTRAN(lcRpStyMsk,'*','?'),19),STYLE)",;
          lcExpr + " .AND. " + "LIKE(PADR(STRTRAN(lcRpStyMsk,'*','?'),19),STYLE)") 

*--stopping execution the followin statment and moveing them in previous section
*lcItmTtl   = gfItemMask('HI')
*lcItmPct   = gfItemMask('PI')
*lnItmWid   = LEN(lcItmPct)
*B604205,1 KAM[end]
IF USED('CODES')
  SELECT CODES
  lcCodeOrd = ORDER()
  lnCodeRec = RECNO()
  SET ORDER TO
ELSE
  llOpenCode = gfOpenFile(gcDataDir+'CODES','','SH')
ENDIF  

*-- laDiscCode : Discount codes array.
DIMENSION laDiscCode[1]
SELECT DISTINCT cCode_No  ;
 FROM Codes               ;
 WHERE cdefcode   + cfld_name + ccode_no + cdiscrep + crltd_nam = "NCDISCCODE" ;
 ORDER BY 1                                                                    ;
 INTO ARRAY laDiscCode

IF _TALLY > 0

  *-- laDiscDesc : Discount description array.
  DIMENSION laDiscDesc[ALEN(laDiscCode,1)+1]
  laDiscDesc[1] = '                          N/A'
  SELECT CODES
  FOR lnI = 1 TO ALEN(laDiscCode,1)
    LOCATE FOR cdefcode   + cfld_name + ccode_no + cdiscrep + crltd_nam = ;  
               "NCDISCCODE " + PADR(laDiscCode[lnI],6) AND    ;
               cRltField = 'N'
    laDiscDesc[lnI+1] = cdiscrep
  ENDFOR

  DIMENSION laDiscCode[ALEN(laDiscCode,1)+1]
  =AINS(laDiscCode,1)
ELSE
  DIMENSION laDiscDesc[1],laDiscCode[1]
  laDiscDesc[1] = '                          N/A'

ENDIF

STORE SPACE(6) TO laDiscCode[1],lcDiscCode

*-- call function to open need files.
= lfOpenFils ()

*-- Fill array the source array with wearhouse.

*B604093,1 ABD- For all Cases all location in the mover screen must be 
*B604093,1 ABD- Finished goods inventory. [Begin]
IF llMultiwh
  *-- Add new condition on the warehouses that Finished Goods Inventory
  *-- And the curent side have acess on it.
  lcSitCond = "lStyInv .AND. "+IIF(lcSysType = 'P',"cSiteId = gcCurSite",".T.")
  *B604093,1 ABD- [End]
  SELECT Warehous
  GOTO TOP
  I = 1 
  *B604093,1 ABD- Scan for the new condation only. [Begin]
  *SCAN
  SCAN FOR &lcSitCond
    *B604093,1 ABD - [End]
    DIMENSION lasource[I]
    lasource [I] = cWarecode
    I = I +1
  ENDSCAN
  *B604093,1 ABD- [Begin]
ENDIF  
*-- Call this function to Evaluate NonMajor Type and variables after close 
*-- the option grid all variable are Empaty()
=lfEvalSegs()

SELECT STYLE
SET ORDER TO TAG STYLE

*-- title screen.
lcWindTitle = 'Assign Discount Code'
*-- Do screen code.
DO (gcScrDir + gcWinAppl + '\ICDISCD.SPR')

glQuitting = .T.  && This is modal screen, this rise quitting flag.

*-- call function to Close opened files.
= lfClosFils ()

RETURN
*-- End of program code.
*:****************************************************************
*: Name        : lfEvalSegs
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 11/23/2000
*: Purpose     : Evaluate NonMajor Type and variables.
*:****************************************************************
*: Called from : [Option Grid] lcDummy variable.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Passed Parameters : None
*:****************************************************************
*: Return      : None
*:****************************************************************
*: Example     : = lfEvalSegs()
*:****************************************************************
*
FUNCTION lfEvalSegs
  lnMajSeg    = gfItemMask('SM')  && No. of major segments.
  lnMajorLen  =  LEN(gfItemMask("PM"))
  *-- Compute Free/Color Items in Style code Structure. [Begin]
  DIMENSION laMajSegs[1,1]
  = gfItemMask(@laMajSegs)

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

  lcColorTlt = 'Only These ' + ALLTRIM(lcNonMajTlt) + 's.'
  
  STORE LEN(lcNonMajPi) TO lnFreeLen , lnColorLen
RETURN ''

*-- End of lfEvalSegs.
*:****************************************************************
*: Name        : lfsrvSty
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 11/23/2000
*: Purpose     : Rise change style flag, in range browse screen.
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : None
*:****************************************************************
*: Example     : =lfsrvSty()
*:****************************************************************
*: Note        : SRV symbol is [S,Set -- R,Reset -- V,Valid]
*:****************************************************************
*
FUNCTION lfSRVSty
PARAMETERS lcParm

DO CASE
  CASE lcParm = 'S'  && Set code
    *-- open this file in another alias to set order to Style Major 
    *-- unique index.
    USE (gcDataDir+'Style') AGAIN ALIAS STYLE_X ORDER TAG Style IN 0
    SELECT STYLE
    SET ORDER TO TAG Cstyle
    SET RELATION TO STYLE.STYLE INTO STYLE_X
    GO TOP IN STYLE
    llChStyle = .T.
  CASE lcParm = 'R'  && Reset code
    USE IN STYLE_X
    SELECT STYLE
    SET ORDER TO TAG STYLE
    llClearSty = .F.
ENDCASE

*-- End of lfsrvSty.
*:****************************************************************
*: Name        : lfStySum
*: Developer   : ABDOU ELGENDI - (ABD)
*: Date        : 11/23/2000
*: Purpose     : sum a specific field for the current style in style file
*:****************************************************************
*: Calls       : None.
*:****************************************************************
*: Called from : Option Grid,style browse calculated fields.
*:****************************************************************
*: Passed Parameters  : None
*:****************************************************************
*: Returns     : Calculated field value.
*:****************************************************************
*: Example     : =lfStySum()
*:****************************************************************
*
FUNCTION lfStySum
PARAMETERS lcSty,lccomp,lnAddToVar
PRIVATE lnStyRec
lnStyRec = IIF(BETWEEN(RECNO('STYLE'),1,RECCOUNT('STYLE')),RECNO('STYLE'),1)
lnTotcomp = 0
SELECT Style_X
SUM &lcCOMP TO lnTotcomp WHILE Style = ALLTRIM(lcSty)
SELECT Style
GO lnStyRec
DO CASE
  CASE lnAddToVar = 1
  	lnO_T_S = lnTotcomp
  CASE lnAddToVar = 2
  	lnO_T_S = lnO_T_S + lnTotcomp
  CASE lnAddToVar = 3
  	lnO_T_S = lnO_T_S - lnTotcomp
ENDCASE
RETURN INT(lnTotcomp)

*-- End of lfStySum.
*:*************************************************************
*: Name      : lfvDisCod
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/23/2000
*: Purpose   : Discount code validation. 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvDisCod()
*:*************************************************************
*
FUNCTION lfvDisCod
lcDiscCode = laDiscCode[lnDiscCode]  

*-- End of lfvDisCod.
*:*************************************************************
*: Name      : lfvApply
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/23/2000
*: Purpose   : if user press <Proceed> Button Validation. 
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfvApply()
*:*************************************************************
*
FUNCTION lfvApply

*-- Text Message : - Are you sure you want to proceed Updating.
*-- Text No      : - 42195.
*-- Text Button  : - Yes  - No .
*-- Button No    : -42002
IF gfModalGen('QRM42195B42002','ALERT','updating.') = 2
  RETURN
ENDIF



*-- lcTmpWareh :- temp file hold selected warehouse from target array.
PRIVATE  lcDisType , lcTmpWareh
STORE '' To lcDisType ,lcTmpWareh
llFindWare = .F.


*-- Get the disecound related filed to now which 
*-- type whole Sale Or Retail sale Or Both.
DECLARE laDisType[1,2]

laDisType[1,1] = 'COSTTAFECT'
laDisType[1,2] = 'lcDisType'
= gfRltFld(lcDiscCode , @laDisType, 'CDISCCODE')
IF llMultiwh
  FOR I = 1 TO ALEN(latarget,1)
    IF !EMPTY(latarget[I])
      llFindWare = .T.
      EXIT
    ENDIF
  ENDFOR
ELSE
  llFindWare = .T.
ENDIF  


*-- cheak if target array is empaty.

*-- If target array is empaty and discound related filed is 
*-- Retail sale return 

IF llMultiwh .AND.  lcDisType = 'R' .AND. !llFindWare
  *-- Text Message : - You must select a location first .
  *-- Text No      : - 42150.
  *-- Text Button  : - Ok.
  *-- Button No    : -00000
  = gfModalGen('TRM42150B00000','ALERT')
  _CUROBJ = OBJNUM(pbWHouse)
  RETURN
ENDIF

lcTmpWareh = gfTempName()
*-- Create temp file hold all wearehouses in the target array
CREATE TABLE (gcWorkDir+lcTmpWareh) (CwareCode C(6))

*-- Fill temp file with wearhouses 

*B604093,1 ABD Add new array that hold Source or Target array. [Begin]
DIMENSION laUpdatelc[1]
laUpdatelc = ''
*B604093,1 ABD [End]

*-in case multi location only.
IF llMultiwh
  IF EMPTY (latarget)
    lnSizeArry = ALEN(lasource,1)
    llFindWare = .F.
    *B604093,1 ABD Copy source array to update array . [Begin]
    =ACOPY(lasource,laUpdatelc)
    *B604093,1 ABD [End]
  ELSE
    lnSizeArry = ALEN(latarget,1)
    llFindWare = .T.
    *B604093,1 ABD Copy target array to update array . [Begin]
    =ACOPY(latarget,laUpdatelc)
    *B604093,1 ABD [End]
  
  ENDIF
  FOR I = 1 TO lnSizeArry
    *B604093,1 ABD Made this code more Optimization. [Begin]
    *INSERT INTO (lcTmpWareh) (cWareCode) Value (IIF(llFindWare,latarget[I],lasource[I]))
    INSERT INTO (lcTmpWareh) (cWareCode) Value (laUpdatelc [I])
    *B604039,1 ABD [End]
  ENDFOR
  *B604093,1 ABD in case not multi location get the defaulte location. [Begin]
ELSE
  SELECT WAREHOUS
  INSERT INTO (lcTmpWareh) (cWareCode) Value (WAREHOUS.cWareCode)
  *B604093,1 ABD [End]
ENDIF  
lnCount = 0
lcStyle = ''

*B604093,1 ABD Add only one check if the comapy is multi location. [Begin]
*SELECT STYLE
*SCAN FOR &lcExpr
*WAIT WINDOW 'Update discount code for Style: ' + Style NOWAIT
*B604093,1 ABD [End]




*- If Company Multi warehouse.
IF llMultiwh
  *B604093,1 ABD select style and scan on it for lcExpr. [Begin]
  SELECT STYLE
  SCAN FOR &lcExpr
    WAIT WINDOW 'Update discount code for Style: ' + Style NOWAIT
    *B604093,1 ABD [End]
      
    IF SUBSTR(lcStyle,1,lnMajorLen) <> SUBSTR(STYLE,1,lnMajorLen)
      lnCount = lnCount + 1
    ENDIF  
    lcStyle = Style
    IF SEEK(lcStyle,'STYDYE')
      *B604093,1 ABD -Remove this scan to fix bug that the brogram enter 
      *B604093,1 ABD -in infinite loop ,and the brogram will update all
      *B604093,1 ABD -the styles in the stydye file. [Begin]
      *SELECT STYDYE
      *SCAN REST WHILE style+cwarecode+dyelot = lcStyle
      *B604093,1 ABD [End]
      SELECT (lcTmpWareh)
      GOTO TOP
      *B604093,1 ABD remove the cheak to speed up the performance. [Begin]
      *SCAN REST WHILE !EOF()
      SCAN
        *B604093,1 ABD [End]
        IF SEEK(lcStyle+&lcTmpWareh..cWareCode,'STYDYE')
          SELECT StyDye
          *-- select the file and scan again to update the dyelot recored.
          SCAN REST WHILE style+cwarecode+dyelot = lcStyle+&lcTmpWareh..cWareCode
            =RLOCK()
            REPLACE cDiscCode WITH lcDiscCode
            UNLOCK
            
            *E301525,1 HS Add these lines to mark any "Selection Codes" that contain the
            *E301525,1    edited style(s) as "To Be Transmitted", if the Inter-Company
            *E301525,1    EDI module is installed and setup and the "System Type" system
            *E301525,1    setup is set to "Back Office" [Begin]
            
            *-- if the Inter-Company EDI module is installed and setup and the "System Type"
            *-- system setup is set to "Back Office"
            IF llNCInstld .AND. gfGetMemVar('M_SYSTYPE') = "B"
              *-- Go to the warehouse record in the "Warehouse" file to get the
              *-- "Site ID" for the current warehouse
              IF SEEK(cWareCode , "Warehous")
                *-- Get the "Partner Code" for the site of the current warehouse
                SELECT EDIAcPrt
                LOCATE FOR cSiteID = Warehous.cSiteID
                IF FOUND()
                  *-- Mark any "Selection Codes" that should be send for the site of the
                  *-- current warehouse and contains the edited style, mark it as
                  *-- "To Be Transmitted"
                  SELECT EDICatgH
                  IF SEEK(EDIAcPrt.Type + EDIAcPrt.cPartner)
                    SCAN REST;
                        WHILE Type + cPartner = EDIAcPrt.Type + EDIAcPrt.cPartner;
                          FOR cEDIStatus = "A"
                      
                      IF SEEK(Type + cPartner + cSelcCode + STYLE.Style , "EDICatgD")
                        SELECT EDICatgD
                        LOCATE FOR cEDIDStat = "A";
                          REST;
                         WHILE Type + cPartner + cSelcCode + Style = EDICatgH.Type +;
                                                                     EDICatgH.cPartner +;
                                                                     EDICatgH.cSelcCode +;
                                                                     STYLE.Style
                        
                        IF FOUND()
                          REPLACE lTransmit WITH .T.;
                             REST;
                              FOR cEDIDStat = "A";
                            WHILE Type + cPartner + cSelcCode + Style = EDICatgH.Type + ;
                                                                        EDICatgH.cPartner +;
                                                                        EDICatgH.cSelcCode +;
                                                                        STYLE.Style
                          
                          SELECT EDITrans
                          *E037853,1 HBG 16/02/2004 Change the width of Key field in EDITRANS to 40 char [Begin]
                          *IF SEEK("832" + PADR(EDICatgH.cSelcCode , 20) + EDICatgH.Type +;
                          *        EDICatgH.cPartner)
                          IF SEEK("832" + PADR(EDICatgH.cSelcCode , 40) + EDICatgH.Type +;
                                  EDICatgH.cPartner)  
                          *E037853,1 [End]        
                            IF cStatus <> "N"
                              REPLACE cStatus WITH "N"
                            ENDIF
                          ELSE
                            APPEND BLANK
                            REPLACE cEDITrnTyp WITH "832"              ,;
                                    Key        WITH EDICatgH.cSelcCode ,;
                                    Type       WITH EDICatgH.Type      ,;
                                    cPartner   WITH EDICatgH.cPartner  ,;
                                    cStatus    WITH "N"
                          ENDIF
                        
                          SELECT EDICatgH
                          REPLACE lTransmit WITH .T.
                        ENDIF
                      ENDIF
                    ENDSCAN
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            *E301525,1 HS Add these lines to mark any "Selection Codes" [End]
            
            SELECT STYLE
            IF &lcTmpWareh..cWareCode = STYLE.cdefware
              =RLOCK()
              REPLACE cDiscCode WITH lcDiscCode
              UNLOCK
            ENDIF  
          ENDSCAN
        ENDIF
        SELECT (lcTmpWareh)          
      ENDSCAN
      *B604093,1 ABD Remark the end scan that we close it. [Begin]
      *ENDSCAN
      *B604093,1 ABD [End]
      SELECT STYLE
    ENDIF  
  *B604093,1 ABD Move the EndScan after cheak on multi location. [Begin]
  ENDSCAN
  *B604093,1 ABD [End]
ELSE    
  *B604093,1 ABD Select style and scan on it for lcExpr. [Begin]
  SELECT STYLE
  SCAN FOR &lcExpr
    WAIT WINDOW 'Update discount code for Style: ' + Style NOWAIT
    *B604093,1 ABD [End]
    WAIT WINDOW 'Update discount code for Style: ' + Style NOWAIT
    IF SUBSTR(lcStyle,1,lnMajorLen) <> SUBSTR(STYLE,1,lnMajorLen)
      lnCount = lnCount + 1
    ENDIF  
    lcStyle = Style
    *B604093,1 ABD- Update the style only if location equal selected location,
    *B604093,1 ABD- & remove the scan. [Begin]
    *=RLOCK()
    *REPLACE cDiscCode WITH lcDiscCode
    *UNLOCK
    *SELECT STYDYE
    *SCAN REST WHILE style+cwarecode+dyelot = lcStyle
    *B604093,1 ABD [End]

    SELECT (lcTmpWareh)
    GOTO TOP
    *B604093,1 ABD remove the cheak to speed up the performance. [Begin]
    *SCAN REST WHILE !EOF()
    SCAN
      *B604093,1 ABD [End]
      IF SEEK(lcStyle+&lcTmpWareh..cWareCode,'STYDYE')
        SELECT StyDye
        *-- select the file and scan again to update the dyelot recored.
        SCAN REST WHILE style+cwarecode+dyelot = lcStyle
          =RLOCK()
          REPLACE cDiscCode WITH lcDiscCode
          UNLOCK
          *B604093,1 ABD Update the styles that have Defa location equla selected location. [Begin]
          SELECT STYLE
          IF &lcTmpWareh..cWareCode = STYLE.cdefware
            =RLOCK()
            REPLACE cDiscCode WITH lcDiscCode
            UNLOCK
          ENDIF  
          *B604093,1 ABD [End]
        ENDSCAN 
        SELECT (lcTmpWareh)
      ENDIF
    ENDSCAN
    *B604093,1 ABD Remark the next end scan.  [Begin]
    *ENDSCAN
    *B604093,1 ABD [End]
    SELECT STYLE
  *B604093,1 ABD Move the EndScan after the cheak on multi location. [Begin]
  ENDSCAN
  *B604093,1 ABD [End]
ENDIF  
*B604093,1 ABD Remark the next line. [End]
*ENDSCAN
*B604093,1 ABD [End]
*-- Clear wait window  
WAIT CLEAR

*-- Text Message : - Updating process completed successfully.
*-- Text No      : - 42219.
*-- Text Button  : - Ok.
*-- Button No    : -00000
= gfModalGen('INM42219B00000','ALERT',ALLTRIM(STR(lnCount))+' Style(s) Updating')
CLEAR READ

*-- End of lfvApply.
*:*************************************************************
*: Name      : lfOpenFils
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/23/2000
*: Purpose   : Open need file that close when run option grid.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfOpenFils()
*:*************************************************************
*
FUNCTION lfOpenFils

*-- Open the code file.
IF !USED('CODES')
  llOpenCode = gfOpenFile(gcDataDir+'CODES','','SH')
ENDIF

*-- Open the style file.
IF !USED('STYLE')
  llOpenStyl = gfOpenFile(gcDataDir+'STYLE','','SH')
ENDIF

*-- Open the Stydye file.
IF !USED('STYDYE')
  llOpenStyD = gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
ENDIF


*-- Open the WAREHOUSfile.
IF !USED('WAREHOUS')
  llOpenWare = gfOpenFile(gcDataDir+'WAREHOUS',gcDataDir+'WAREHOUS','SH')
ENDIF

*E301525,1 HS Add these lines to open EDI files [Begin]
*-- if the Inter-Company EDI module is installed and setup and the "System Type"
*-- system setup is set to "Back Office"
IF llNCInstld .AND. gfGetMemVar('M_SYSTYPE') = "B"
  IF !USED("EDICatgH")
    llOpenCatH = gfOpenFile(gcDataDir + 'EDICatgH' , gcDataDir + 'Account' , 'SH')
  ENDIF
  IF !USED("EDICatgD")
    llOpenCatD = gfOpenFile(gcDataDir + 'EDICatgD' , gcDataDir + 'Account' , 'SH')
  ENDIF
  IF !USED("EDITrans")
    llOpenTran = gfOpenFile(gcDataDir + 'EDITrans' , gcDataDir + 'TypeKey' , 'SH')
  ENDIF
  IF !USED("EDIAcPrt")
    llOpenAcPr = gfOpenFile(gcDataDir + 'EDIAcPrt' , gcDataDir + 'AccFact' , 'SH')
  ENDIF
ENDIF
*E301525,1 HS Add these lines to open EDI files [End]

*-- End OF lfOpenFils.
*:*************************************************************
*: Name      : lfClosFils
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/23/2000
*: Purpose   : Open need file that close when run option grid.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : None
*:*************************************************************
*: Returns   : None
*:*************************************************************
*: Example   : =lfClosFils()
*:*************************************************************
*
FUNCTION lfClosFils

*-- Close Codes file if this program open it.
IF llOpenCode
  =gfCloseFile('CODES')
ENDIF

*-- Close Style file if this program open it.
IF llOpenStyl
    =gfCloseFile('STYLE')
ENDIF

*-- Close StyDye file if this program open it.
IF llOpenStyD 
  =gfCloseFile('STYDYE')
ENDIF

IF llOpenWare
  =gfCloseFile('WAREHOUS')
ENDIF

*E301525,1 HS Add these lines to close EDI files [Begin]
IF llOpenCatH
  =gfCloseFile('EDICatgH')
ENDIF
IF llOpenCatD
  =gfCloseFile('EDICatgD')
ENDIF
IF llOpenTran
  =gfCloseFile('EDITrans')
ENDIF
IF llOpenAcPr
  =gfCloseFile('EDIAcPrt')
ENDIF
*E301525,1 HS Add these lines to close EDI files [End]

*-- End OF lfClosFils.
*:*************************************************************
*: Name      : lfvWHouse
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/23/2000
*: Purpose   : Valid function for push button < Warehouse >.
*:           : To Add/Remove warehouse.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvwhouse()
*:*************************************************************
*
FUNCTION lfvwhouse

=gfMover(@lasource, @latarget,"Select Warehouse",.T.)

*-- End Of lfvwhouse.
*:*************************************************************
*: Name      : lfvclose
*: Developer : ABDOU ELGENDI - (ABD)
*: Date      : 11/23/2000
*: Purpose   : Valid function for push button < Close >.
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfvclose()
*:*************************************************************
*
FUNCTION lfvclose

*-- Text Message : - Are you sure you want to cancel 
*-- Text Message : - this session and lose all your selection?
*-- Text No      : - 42150.
*-- Text Button  : - Ok.
*-- Button No    : -00000
IF gfModalGen('INM42063B42002','ALERT','selection?') = 2
  RETURN
ELSE
  Clear Read  
ENDIF

*-- End OF lfvclose.
*:*************************************************************
*:*************************************************************
*: Name      : lfVStyMsk
*: Developer : abdel-Rahim koura -KAM
*: Date      : 12/03/2001
*: Purpose   : assign astriks to style mask field
*:*************************************************************
*: Calls     : None.
*:*************************************************************
*: Passed Parameters  : ............
*:*************************************************************
*: Returns            : ............
*:*************************************************************
*: Example   : =lfVStyMsk()
*:*************************************************************
*B604205,1 KAM
FUNCTION lfVStyMsk
RETURN TRANSFORM(REPLICATE('*',lnItmWid),lcItmPct)
*--end of lfVStyMsk() function 





