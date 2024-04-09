*:****************************************************************************
*: Program file        : MAALEN10
*: Program desc.       : Custome Material Stock by Location
*: For Report          : MAALEN10.FRX
*: System              : Aria Advantage Series VER. 2.7
*: Module              : Material (MA)
*: Developer           : RAMY MABROUK
*: Tracking Job Number : C200158
*: Date                : February/2001
*:****************************************************************************
*: Calls :
*:    Procedures : ....
*:    Functions  : gfDispRe()
*:****************************************************************************
*: Called From         : System Menu (MA --> Output --> Custome Material Stock by Location)
*:****************************************************************************
*: Passed Parameters  : None
*:****************************************************************************
*: Example : DO MAALEN10
*:****************************************************************************
*: Modification        :
*!B604937,1 MHM 10/03/2001 Fix Problems with Customised  Material Stock 
*!B605041,1 MHM 10/03/2001 Fix Problems with Customised  Material Stock 
*!B605041,1                modify FRX to collect by Fabric Color
*!B605238,1 ADEL 01/08/02  Fix vendor (in prg) and value amount (in Frx).
*!C200314,1 WAB 04/08/2002 Add option to print Zero lots ( Y/N) 
*!B607850,1 TMI 11/30/2006 Fix a bug that if show zero qty = 'NO' then base the criteria on the cut off date qty not the on hand qty ( Ticket# T20060824.0007 )
*:****************************************************************************
*!C200158,4

*!B604937,1 Open MatInvJl in another alias [Start]
PRIVATE lcMatSeq
lcMatSeq = gfTempName()
=gfOpenFile(gcDataDir+'MATINVJL','MtInvSeq','SH', @lcMatSeq , .T.)
*!B604937,1 [End]

*B605238,1 (Begin) Open POSHDR
=gfOpenFile(gcDataDir+'POSHDR','POSHDR','SH')
*ADEL
=gfOpenFile(gcDataDir+'FABDYE','FABDYE','SH')
SELECT MATINVJL
SET RELATION TO cFABRIC + cColor +CWARECODE  INTO FABDYE ADDITIVE
*B605238,1 (End)

*--Zap the temp file used in the printing
SELECT (lcUseFile)
ZAP

lnCost     = 0
lnValue    = 0
lnReceived = 0
lnIssued   = 0
*C200314,1 WAB (START) - option to print zero lots (Y/N)
llPrnZrlot = (lcPrnZrlot='Y')
IF !llPrnZrlot
  lcTempfil = gfTempName()
  Create CURSOR (lcTempFil) ( cWareCode c(6), cFabric c(7), cColor c(6), cDyelot c(10), cRSession c(6))
ENDIF
*C200314,1 WAB (End)

*B607850,1 TMI [Start] open another alias to collect cutoff qty from
IF !USED('CUTOFFJL')
  USE (gcDataDir+'MATINVJL') AGAIN IN 0 ALIAS CUTOFFJL ORDER MATINVJL
ENDIF
lnCtDtPos = ASUBSCRIPT(laOgFxFlt , ASCAN(laOgFxFlt,'MATINVJL.DPOSTDATE') , 1)

lcSumAls = gfTempName()
CREATE TABLE (gcWorkDir+lcSumAls) (FABKEY C(19),Cutoff N(12,3))
INDEX ON FABKEY TAG &lcSumAls
*B607850,1 TMI [End  ] 

*--Set relation to get the fabric name
SELECT MATINVJL
SET RELATION TO cFABRIC + cColor INTO FABRIC ADDITIVE
*--Scan the MATINVJL file to get the matched fabrics
SCAN FOR &lcRpExp

  *- if the fabric on hand=0 and we do not print zero stock qty
  *ADEL
  *IF FABRIC.OnHand =<0 AND lcDisZero <> 'Y' && OR cTranType = '3' && cTranType = '3' TO BE REMOVED
  *B607850,1 TMI [Start] if disp zero is "NO" check the cutoff date qty not the onhand qty
  *IF (FABDYE.OnHand =<0 AND lcDisZero <> 'Y') OR EOF('FABRIC')
  *  LOOP
  *ENDIF
  IF EOF('FABRIC') 
    LOOP
  ENDIF
  IF (lcDisZero <> 'Y' AND !EMPTY(laOgFxFlt[lnCtDtPos,6]))
    lcFabKey = MATINVJL.CFABRIC+MATINVJL.CCOLOR+MATINVJL.CWARECODE
    IF !SEEK(lcFabKey,lcSumAls)
      SELECT CUTOFFJL
      =SEEK(lcFabKey,'CUTOFFJL')
      lnFabSum = 0
      SUM (NRECEIVED - NISSUED ) TO lnFabSum ;
         REST WHILE CFABRIC+CCOLOR+CWARECODE+CDYELOT+CRSESSION+CISESSION = lcFabKey ;
                FOR CUTOFFJL.DPOSTDATE <= laOgFxFlt[lnCtDtPos,6]
      INSERT INTO &lcSumAls (FABKEY,Cutoff) VALUES (lcFabKey,lnFabSum)
    ENDIF
    IF &lcSumAls..CutOff <= 0          
      LOOP
    ENDIF
  ENDIF
  SELECT MATINVJL  
  *B607850,1 TMI [End  ] 
  
  *--If not found the Fabric,Color,Warehouse,Dyelot in the temp file insert new record for it
  IF lcRepType = 'S'
    SELECT (lcUseFile) 
    *!C200158,4 RAMY
    *IF !SEEK(MATINVJL.cFabric + MATINVJL.cColor + MATINVJL.cWareCode + MATINVJL.cDyelot + MATINVJL.cRSession )
    IF !SEEK(MATINVJL.cWareCode + MATINVJL.cFabric + MATINVJL.cColor + MATINVJL.cDyelot + MATINVJL.cRSession )
      INSERT INTO (lcUseFile) (cTrn_Seq          , cFabric          , cColor          , cWareCode          , cDyelot          , cRSession          , cISession          , cDesc       , cTranType          , dTranDate          , dPostDate          , nUnitCost          , nUntCstBuy          , cTran) ;
                       VALUES (MATINVJL.cTrn_Seq , MATINVJL.cFabric , MATINVJL.cColor , MATINVJL.cWareCode , MATINVJL.cDyelot , MATINVJL.cRSession , MATINVJL.cISession , FABRIC.Desc , MATINVJL.cTranType , MATINVJL.dTranDate , MATINVJL.dPostDate , MATINVJL.nUnitCost , MATINVJL.nUntCstBuy , MATINVJL.cTran)
    ENDIF
    *!C200158,4
    DO CASE

      CASE MATINVJL.cTranType = '1'
         IF EMPTY(MATINVJL.cISession)
          REPLACE &lcUseFile..nReceived WITH &lcUseFile..nReceived + MATINVJL.nReceived
        ELSE
          REPLACE &lcUseFile..nIssued WITH &lcUseFile..nIssued + MATINVJL.nIssued
          *!B604937,1 MHM 10/03/2001 Comment this line to update in all case Issue and Recevied[Start]
          *=lfGetVend(cTran)
          *!B604937,1 MHM 10/03/2001 [End]
        ENDIF
        *!B604937,1 MHM 10/03/2001 Move this line to update in all case Issue and Recevied[Start]
        =lfGetVend(cTran)
        *!B604937,1 MHM 10/03/2001 [End]
        
      CASE MATINVJL.cTranType = '4'
        *!B604937,1 MHM 10/03/2001 Consider Case 4 (Return form CUtTkt )[Start]
        *REPLACE &lcUseFile..nIssued WITH  &lcUseFile..nIssued + MATINVJL.nIssued
        IF EMPTY(MATINVJL.cISession)
          REPLACE &lcUseFile..nIssued WITH  &lcUseFile..nIssued + MATINVJL.nIssued
        ELSE
          REPLACE &lcUseFile..nIssued WITH &lcUseFile..nIssued + MATINVJL.nIssued - MATINVJL.nReceived
        ENDIF
        =lfGetVend(cRSession)
        *!B604937,1 MHM 10/03/2001 [End]
          
      CASE MATINVJL.cTranType = '2'

        IF EMPTY(MATINVJL.cISession)
          REPLACE &lcUseFile..nReceived WITH &lcUseFile..nReceived + MATINVJL.nReceived
        ELSE
          *!B605041,1 MHM 10/03/2001 CHange the sign of adjus. to negative[Start]
          *REPLACE &lcUseFile..nAdj WITH (&lcUseFile..nAdj + MATINVJL.nIssued) &&* -1
          REPLACE &lcUseFile..nAdj WITH (&lcUseFile..nAdj + MATINVJL.nIssued * -1)
          *!B605041,1 MHM  [END]
        ENDIF

      CASE MATINVJL.cTranType = '3'
        IF !EMPTY(MATINVJL.cISession)
          *REPLACE &lcUseFile..nAdj WITH (MATINVJL.nReceived - MATINVJL.nIssued)
          REPLACE &lcUseFile..nAdj WITH (&lcUseFile..nAdj + MATINVJL.nIssued)
        ELSE
          REPLACE &lcUseFile..nReceived WITH (MATINVJL.nReceived - MATINVJL.nIssued)
        ENDIF
      
    ENDCASE

  ELSE
    IF RECCOUNT(lcUseFile) > 0
      
      IF cFabric + cColor + cWareCode + cDyelot + cRSession <> ;
         &lcUseFile..cFabric + &lcUseFile..cColor + &lcUseFile..cWareCode + ;
         &lcUseFile..cDyelot + &lcUseFile..cRSession

        *!C200158,4
        *REPLACE &lcUseFile..nAdj WITH IIF(&lcUseFile..cTranType = '2' , lnReceived - lnIssued , 0)
        lnBalanc = lnReceived - lnIssued
        
        IF lnBalanc > 0
          lnCost     = (&lcUseFile..nUnitCost * lnReceived - &lcUseFile..nUnitCost * lnIssued) / lnBalanc
        *C200314,1 WAB (START) - add option to print zero lots)
        ELSE
          IF !llPrnZrlot
            SELECT (lcTempFil)        
            APPEND BLANK
            REPLACE cWareCode WITH &lcUseFile..cWareCode , ;
                    cFabric   WITH &lcUseFile..cFabric   , ;
                    cColor    WITH &lcUseFile..cColor    , ;
                    cdyelot   WITH &lcUseFile..cDyelot   , ;
                    cRSession WITH &lcUseFile..cRSession 
            SELECT MATINVJL
          ENDIF
        *C200314,1 WAB (End) 
        ENDIF
        
        REPLACE &lcUseFile..nBalance WITH lnBalanc
        REPLACE &lcUseFile..nCost WITH lnCost
        REPLACE &lcUseFile..nValue WITH (lnReceived * &lcUseFile..nUnitCost) - (lnIssued * &lcUseFile..nUnitCost)
        lnIssued   = 0
        lnReceived = 0
      ENDIF
    ENDIF
    INSERT INTO (lcUseFile) (cTrn_Seq          , cFabric          , cColor          , cWareCode          , cDyelot          , cRSession          , cISession          , cDesc       , cTranType          , dTranDate          , dPostDate          , nUnitCost          , nUntCstBuy          , cTran) ;
                VALUES (MATINVJL.cTrn_Seq , MATINVJL.cFabric , MATINVJL.cColor , MATINVJL.cWareCode , MATINVJL.cDyelot , MATINVJL.cRSession , MATINVJL.cISession , FABRIC.Desc , MATINVJL.cTranType , MATINVJL.dTranDate , MATINVJL.dPostDate , MATINVJL.nUnitCost , MATINVJL.nUntCstBuy , MATINVJL.cTran)
  
    lnIssued   = lnIssued   + MATINVJL.nIssued
    lnReceived = lnReceived + MATINVJL.nReceived
     
    DO CASE
      CASE MATINVJL.cTranType = '1'
        *!C200158,4
        IF EMPTY(MATINVJL.cISession)
          REPLACE &lcUseFile..nReceived WITH &lcUseFile..nReceived + MATINVJL.nReceived - MATINVJL.nIssued
          *!B604937,1 MHM 10/03/2001 Comment this line to update in all case Issue and Recevied[Start]
          *=lfGetVend(cTran)
          *!B604937,1 MHM [End]
        ELSE
          REPLACE &lcUseFile..nIssued WITH MATINVJL.nIssued
        ENDIF
        *!B604937,1 MHM 10/03/2001 Move this line to update in all case Issue and Recevied[Start]
        =lfGetVend(cTran)
        *!B604937,1 MHM 10/03/2001 [End]
        
      CASE MATINVJL.cTranType = '4'
        *!B604937,1 MHM 10/03/2001 Consider Case 4 (Return form CUtTkt )[Start]
        *REPLACE &lcUseFile..nIssued WITH  MATINVJL.nIssued
        IF EMPTY(MATINVJL.cISession)
          REPLACE &lcUseFile..nReceived WITH  MATINVJL.nIssued
        ELSE
          REPLACE &lcUseFile..nIssued WITH &lcUseFile..nIssued + MATINVJL.nIssued - MATINVJL.nReceived
        ENDIF
        =lfGetVend(cRSession)
        *!B604937,1 MHM [End]
      CASE MATINVJL.cTranType = '2'
        *C200158,4
        *REPLACE &lcUseFile..nAdj WITH &lcUseFile..nAdj + (MATINVJL.nReceived - MATINVJL.nIssued)

        IF EMPTY(MATINVJL.cISession)
          REPLACE &lcUseFile..nReceived WITH MATINVJL.nReceived
        ELSE
          REPLACE &lcUseFile..nAdj WITH MATINVJL.nIssued * -1
        ENDIF

      *C200158,4
      CASE MATINVJL.cTranType = '3'
        IF !EMPTY(MATINVJL.cISession)
          REPLACE &lcUseFile..nAdj WITH (MATINVJL.nReceived - MATINVJL.nIssued)
        ELSE
          REPLACE &lcUseFile..nReceived WITH (MATINVJL.nReceived - MATINVJL.nIssued)
        ENDIF
      
    ENDCASE    
    
  ENDIF
ENDSCAN

*B607850,1 TMI [Start] close the opened temp cursor
IF USED('CUTOFFJL')
  USE IN CUTOFFJL
ENDIF
IF USED(lcSumAls)
  USE IN &lcSumAls
ENDIF
ERASE (gcWorkDir+lcSumAls+'.DBF')
ERASE (gcWorkDir+lcSumAls+'.CDX')
*B607850,1 TMI [End  ] 

SELECT (lcUseFile)


*!C200158,4
IF lcRepType = 'D'
  lnBalanc = lnReceived - lnIssued
  IF lnBalanc > 0
    lnCost = (nUnitCost * lnReceived - nUnitCost * lnIssued) / lnBalanc
  *C200314,1 WAB (START) - add option to print zero lots
  ELSE
    IF !llPrnZrlot
      SELECT (lcTempFil)
      APPEND BLANK
      REPLACE cWareCode WITH &lcUseFile..cWareCode , ;
              cFabric   WITH &lcUseFile..cFabric   , ;
              cColor    WITH &lcUseFile..cColor    , ;
              cdyelot   WITH &lcUseFile..cDyelot   , ;
              cRSession WITH &lcUseFile..cRSession 
      SELECT (lcUseFile)
    ENDIF
  *C200314,1 WAB (END) 
  ENDIF
  REPLACE nBalance WITH lnBalanc
  REPLACE nCost    WITH lnCost
  REPLACE nValue   WITH (lnReceived * nUnitCost) - (lnIssued * nUnitCost)
  lnIssued   = 0
  lnReceived = 0
ENDIF

*C200314,1 WAB (START) - if print zero lots , delete all records belong to the lost that have zero balanced
IF !llPrnZrlot
  SELECT (lcTempFil)
  SCAN
    IF SEEK(cWareCode + cFabric + cColor + cDyelot + cRSession ,lcUseFile)
      SELECT (lcUseFile)
      DELETE REST WHILE cWareCode + cFabric + cColor + cDyelot + cRSession= &lcTempFil..cWareCode + &lcTempFil..cFabric + ;
                        &lcTempFil..cColor + &lcTempFil..cDyelot + &lcTempFil..cRSession
    ENDIF
  ENDSCAN
  SELECT (lcUseFile)
  IF lcRepType = 'S'
    SCAN
      IF nReceived - nIssued - IIF(nAdj < 0 , -1* nAdj , nAdj) <= 0
        DELETE
      ENDIF
    ENDSCAN
  ENDIF
ENDIF
LOCATE
IF EOF()
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ENDIF
*C200314,1 WAB (End) 


lcSetCen = SET('CENT')
SET CENTURY ON
DO gfDispRe WITH EVALUATE('lcRepNam')
SET CENTURY &lcSetCen

*-----------------------------------------------------------------------------
*! Name      : lfCreatTemp
*! Developer : RAMY MABROUK (RAM)
*! Date      : 18/02/2001
*! Purpose   : Function to bulid the temp file used in the report
*-----------------------------------------------------------------------------
*! Calls       : 
*-----------------------------------------------------------------------------
*! Passed Parameters : None.
*-----------------------------------------------------------------------------
*! Return      : None
*-----------------------------------------------------------------------------
*! Example     : = lfCreatTemp()
*-----------------------------------------------------------------------------
FUNCTION lfCreatTemp

SELECT MATINVJL
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru , 1)
DIMENSION laFileStru[lnFileStru + 6 , 4]

laFileStru[lnFileStru + 1 , 1] = 'cDesc'
laFileStru[lnFileStru + 1 , 2] = 'C'
laFileStru[lnFileStru + 1 , 3] = 20
laFileStru[lnFileStru + 1 , 4] = 0

laFileStru[lnFileStru + 2 , 1] = 'cVendor'
laFileStru[lnFileStru + 2 , 2] = 'C'

*laFileStru[lnFileStru + 2 , 3] = 20
laFileStru[lnFileStru + 2 , 3] = 30
laFileStru[lnFileStru + 2 , 4] = 0

laFileStru[lnFileStru + 3 , 1] = 'nAdj'
laFileStru[lnFileStru + 3 , 2] = 'N'
laFileStru[lnFileStru + 3 , 3] = 12
laFileStru[lnFileStru + 3 , 4] = 3


laFileStru[lnFileStru + 4 , 1] = 'nBalance'
laFileStru[lnFileStru + 4 , 2] = 'N'
laFileStru[lnFileStru + 4 , 3] = 12
laFileStru[lnFileStru + 4 , 4] = 3

laFileStru[lnFileStru + 5 , 1] = 'nCost'
laFileStru[lnFileStru + 5 , 2] = 'N'
laFileStru[lnFileStru + 5 , 3] = 12
laFileStru[lnFileStru + 5 , 4] = 3

laFileStru[lnFileStru + 6 , 1] = 'nValue'
laFileStru[lnFileStru + 6 , 2] = 'N'
laFileStru[lnFileStru + 6 , 3] = 12
laFileStru[lnFileStru + 6 , 4] = 3

CREATE TABLE (gcWorkDir + lcUseFile) FROM ARRAY laFileStru
*!C200158,4 RAMY
*INDEX ON cFabric + cColor + cWareCode + cDyelot + cRSession + cISession TAG lcUseFile OF (gcWorkDir + lcUseFile + ".CDX")
INDEX ON cWareCode + cFabric + cColor + cDyelot + cRSession + cISession TAG lcUseFile OF (gcWorkDir + lcUseFile + ".CDX")

*-----------------------------------------------------------------------------
*! Name      : lfGetVend
*! Developer : RAMY MABROUK (RAM)
*! Date      : 18/02/2001
*! Purpose   : Function to get the vendore name for a PO
*-----------------------------------------------------------------------------
*! Calls       : 
*-----------------------------------------------------------------------------
*! Passed Parameters : lnTranNo -> PO no.
*-----------------------------------------------------------------------------
*! Return      : None
*-----------------------------------------------------------------------------
*! Example     : = lfGetVend()
*-----------------------------------------------------------------------------

FUNCTION lfGetVend
PARAMETER lnTranNo
lcAlias = ALIAS()

*!B604937,1 Handel case tow if MatInvJl.CtranType ='4' [Start]
IF MatInvJl.CtranType = '1'
*!B604937,1 [End]

  IF SEEK(IIF(EMPTY(cISession) , 'P' , 'R') + lnTranNo , "POFHDR")
    SELECT POFHDR
    IF SEEK(Vendor , "APVENDOR")
      REPLACE &lcUseFile..cVendor WITH APVENDOR.cVenComp
    ENDIF
  ENDIF
*!B604937,1 Handel case tow if MatInvJl.CtranType ='4' [Start]
ELSE
  SELECT(lcMatSeq)
  =SEEK(lnTranNo)
  lnTranNo = cTran
  *B605238,1 (Begin) Get Style PO vendor when ISSUE.
  *IF SEEK(IIF(EMPTY(cISession) , 'P' , 'R') + lnTranNo , "POFHDR")
  *  SELECT POFHDR
  *  IF SEEK(Vendor , "APVENDOR")
  *    REPLACE &lcUseFile..cVendor WITH APVENDOR.cVenComp
  *  ENDIF
  *ENDIF
  lcAlias = IIF(Matinvjl.CIMTYP = 'I','POSHDR','POFHDR')
  lcKey   = IIF(Matinvjl.CIMTYP = 'I',Matinvjl.cTran,lnTranNo)

  *lcAlias = IIF(CIMTYP = 'I','POSHDR','POFHDR')
  *lcKey   = IIF(CIMTYP = 'I',cTran,lnTranNo)
  IF SEEK(IIF(!EMPTY(MATINVJL.cISession) , 'P' , 'R') + lcKey, lcAlias) AND SEEK(&lcAlias..Vendor , "APVENDOR")
     REPLACE &lcUseFile..cVendor WITH APVENDOR.cVenComp
  ENDIF
  *B605238,1 (End)
ENDIF  
*!B604937,1 [End]

SELECT (lcAlias)

*-----------------------------------------------------------------------------
*! Name      : lfGetTranType
*! Developer : RAMY MABROUK (RAM)
*! Date      : 18/02/2001
*! Purpose   : Function to get the transaction type name (called from the frx)
*-----------------------------------------------------------------------------
*! Calls       : 
*-----------------------------------------------------------------------------
*! Passed Parameters :
*-----------------------------------------------------------------------------
*! Return      : None
*-----------------------------------------------------------------------------
*! Example     : = lfGetTranType()
*-----------------------------------------------------------------------------

*!C200158,4
FUNCTION lfGetTranType

DO CASE

  CASE cTranType = '1'
    IF EMPTY(cISession)
      RETURN "RCPT"
    ELSE
      RETURN "RTRN"
    ENDIF

  CASE cTranType = '4'
    RETURN "ISSUE"
  
  CASE cTranType = '2'
    RETURN "ADJ"
    
  CASE cTranType = '3'
    IF EMPTY(cISession)
      RETURN "PHIS"
    ELSE
      RETURN "ADJ"
    ENDIF

ENDCASE
*!**************************************************************************
*! Name      : lfvFabric
*: Developer : Mohamed Shokry (MHM)
*! Date      : 10/03/2001
*! Purpose   : validate fabric
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Calls       : FaBrow()
*!**************************************************************************
*! Passed Parameters : None
*!**************************************************************************
*! Return      : None
*!**************************************************************************
*! Example     : = lfvFabric()
*!**************************************************************************
* B604937
FUNCTION lfvFabric
lcFabObj = VARREAD()
lcFab    = &lcFabObj
llUseByMe = .F.

IF !USED('FABRIC')
  llUseByMe = .T.
  USE (gcDataDir+'FABRIC') IN 0 SHARE
ENDIF
  
lcTag = ORDER('FABRIC')
SET ORDER TO FABRIC IN FABRIC

IF LASTKEY() = 13 AND !MDOWN()
  IF SEEK(lcFab,'FABRIC') 
    &lcFabObj = FABRIC.Fabric
  ELSE
    = FaBrow(@lcFab,'*')
    &lcFabObj = lcFab
  ENDIF
ELSE
  &lcFabObj = ''
ENDIF
SET ORDER TO FABRIC IN FABRIC
IF llUseByMe
  USE IN FABRIC
ENDIF  
*-- End of lfvFabric.

*!*************************************************************
*! Name      : lfSRVFab
*: Developer : Mohamed Shokry (MHM)
*! Date      : 10/03/2001
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
*! Example   : =lfSRVFab()
*!*************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!*************************************************************
*!B604937,1
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
ENDCASE
*-- end of lfSRVFab.
*!*************************************************************
*! Name      : lfFabSum
*: Developer : Mohamed Shokry (MHM)
*! Date      : 10/03/2001
*! Purpose   : sum a specific field for the current fabric in fabric file
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
*!B604937,1
FUNCTION lfFabSum
PARAMETERS lcFab,lccomp
PRIVATE lnFabRec
lnTotcomp = 0
lnFabRec = IIF(RECNO('FABRIC') <= RECCOUNT('FABRIC'),RECNO('FABRIC'),1)

SELECT Fabric_X
SUM &lcCOMP TO lnTotcomp WHILE Fabric=lcFab
SELECT Fabric
GO lnFabRec
RETURN INT(lnTotcomp)
*-- end of lfFabSum.

