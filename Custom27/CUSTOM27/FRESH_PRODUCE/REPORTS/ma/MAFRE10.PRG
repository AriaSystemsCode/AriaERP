*:***************************************************************************
*: Program file  : MAFRE10.PRG
*: Program desc. : CUSTOMIZED ONHAND MATERIAL INVENTORY REPORT.
*! Date          : 05/31/2000
*: System        : Aria Advantage Series.
*: Module        : Matrial (MA)
*: Developer     : BASSEM RAFAAT (BWA)			
*: Tracking Job Number: C101877
*: 
*:***************************************************************************
*: Calls : 						
*:    Procedures : ....
*:***************************************************************************
*: Passed Parameters  : None
*:***************************************************************************
*: Notes   : ....
*:***************************************************************************
*: Example : DO MAONHAND
*:***************************************************************************

*--Get the full picture of the date.
lcCentury  = SET('CENTURY')
SET CENTURY ON

*--Initial Variables .
lcRpTrnCmp = ""
lnOnhand = 0

*--THE COLOR LENGTH
DECLARE laItemSeg[1]
STORE 0 TO lnClrLen,lnClrPos
=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR

*--Variables to check if the selection criteria is change or not.
llChanged = .F.

IF llOgFltCh OR lfChkFlt() OR llChngTrn

  DIMENSION laRpOL[1],laRpOC[1],laRpOT[1]
  STORE '' TO laRpOL,laRpOC,laRpOT
  =ACOPY(laRpTarget,laRpOL)
  =ACOPY(laRpTColor,laRpOC)
  =ACOPY(laRpTType,laRpOT)
  lcClrStr = ''
  lcTypStr = ''
  lcLocStr = ''
  lcFabFlt = '.T.'
  llChngTrn = .F.
  lnCost   = 0

  lcRpInvExp = IIF(EMPTY(lcRpInvExp),"APTCR",lcRpInvExp)
  
  =lfvDate()             && Function to get the value of the Tran. Date to print in the Frx.
  =lfvClrTyp()           && Function To prepare color,item type string and Location.
  =lfCollData()          && Function build the filter and colloect the data.
ENDIF

SELECT (lcRpLTmp)
IF lcSort1 = 'M'
  SET ORDER TO TAG (lcRpLTmp1)
  IF !('FABRIC INTO ' + (lcFabHand) $ SET('RELATION'))
    SET RELATION TO FABRIC INTO (lcFabHand) ADDITIVE
  ENDIF
ELSE
  SELECT (lcFabHand)
  LOCATE
  SET ORDER TO TAG (lcFabHand1)
  SELECT (lcRpLTmp)
  IF !('CWARECODE+FABRIC INTO ' + (lcFabHand) $ SET('RELATION'))
    SET RELATION TO CWARECODE+FABRIC INTO (lcFabHand) ADDITIVE
  ENDIF
ENDIF
LOCATE

DO gfDispRe WITH EVALUATE('lcRpForm')

*--Restore the setting of the cet century.
SET CENTURY &lcCentury
*!*************************************************************
*! Name      : lfCollData
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function build the filter and colloect the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfCollData

CREATE TABLE &gcWorkDir.&lcRpLTmp (TYPE C(15),FABRIC C(07) , NoofTran C(6) ,Cwarecode C(6),Date D , Onhand N(12,3),;
                                   WIP N(9,2) ,Received N(9,2) , Issued N(9,2) , Cost N(9,3) , Desc C(20) , TotOnHand N(9,2))

INDEX ON FABRIC+cWareCode+TYPE TAG (lcRpLTmp1)
INDEX ON cWareCode+FABRIC+ALLTRIM(TYPE)+DTOS(Date) TAG (lcRpDate)
INDEX ON cWareCode+FABRIC+ALLTRIM(TYPE)+NoofTran TAG (lcRpLTmp)

CREATE TABLE &gcWorkDir.&lcFabHand (FABRIC C(07), Cwarecode C(6) , TotOnHand N(9,2) , AllOnHnad N(9,2))
INDEX ON Cwarecode+FABRIC TAG (lcFabHand1)
INDEX ON FABRIC+Cwarecode TAG (lcFabHand)

*---Locatin
IF llMWHous AND !EMPTY(lcLocStr)
  lcFabFlt = lcFabFlt + ' AND FABDYE.cWareCode $ lcLocStr'
ENDIF

*--- Color
IF !EMPTY(lcClrStr)
  lcFabFlt = lcFabFlt + ' AND FABDYE.COLOR $ lcClrStr'
ENDIF

*--- Type
IF !EMPTY(lcTypStr)
  lcFabFlt = lcFabFlt + ' AND FABRIC.Item_Type $ lcTypStr'
ENDIF

*---Fabric
lnDataPos = ASCAN(laOGFxFlt,'FABRIC.FABRIC')
IF lnDataPos > 0
  lcFabTemp = laOGFxFlt[lnDataPos,6]
  IF USED(lcFabTemp)
    SELECT (lcFabTemp)   && Indexed ON Fabric
    GOTO TOP
    =lfGetData(!EOF())
  ELSE
    =lfGetData()
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfGetData
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function colloect the data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfGetData

PARAMETER llFrmTemp

PRIVATE lnCurAls
lnOnhand = 0 
lcWarecode = ""
lcFabric = SPACE(0)
lcColor  = SPACE(0)
lcWare = SPACE(0)
lnCurAls = SELECT(0)

IF 'C' $ lcRpInvExp
  lcBomIndx = gfTempName()
  SELECT BOMLINE
  INDEX ON  CIMTYP+CTKTNO+ITEM TAG BomSize OF (gcWorkDir + lcBomIndx+ '.CDX')
  SET ORDER TO TAG BomSize OF (gcWorkDir +lcBomIndx  + '.CDX')

  lcTmpIndx = gfTempName()
  SELECT CTKTBOM
  INDEX ON  ITEM+ICLR TAG lcItemClr OF (gcWorkDir + lcTmpIndx+ '.CDX')
  SET ORDER TO TAG lcItemClr OF (gcWorkDir +lcTmpIndx  + '.CDX')
ENDIF

SELECT FABDYE
SET RELATION TO
SET RELATION TO FABRIC+COLOR INTO FABRIC ADDITI

IF llFrmTemp
  SET RELATION TO FABRIC INTO (lcFabTemp)  ADDIT
ENDIF

GOTO TOP
SCAN FOR &lcFabFlt AND IIF(llFrmTemp , !EOF(lcFabTemp),.T.) AND EMPTY(FABDYE.DyeLot)
  SCAT MEMVAR MEMO
  m.Cost     = lfCost()
  m.Desc     = Fabric.Desc
  
  IF lcFabric # FABDYE.Fabric AND !EMPTY(lcFabric) .OR. lcWare # FABDYE.cWarecode
    lnOnhand = 0
  ENDIF
  lnOnhand   = lnOnhand + FABDYE.Onhand

  SELECT (lcFabHand)
    IF lcFabric # FABDYE.Fabric .OR. lcColor # FABDYE.Color .OR. lcWare # FABDYE.cWarecode
      IF !SEEK(FABDYE.Fabric+FABDYE.cWarecode ,lcFabHand )
        APPEND BLANK
        REPLACE &lcFabHand..FABRIC    WITH IIF(llFrmTemp ,&lcFabTemp..FABRIC , FABDYE.FABRIC) ,;
                &lcFabHand..TotOnHand WITH FABDYE.Onhand                                      ,;
                &lcFabHand..cWareCode WITH FABDYE.cWareCode
      ELSE
        REPLACE &lcFabHand..TotOnHand WITH &lcFabHand..TotOnHand + FABDYE.Onhand
      ENDIF
    ENDIF

  IF ( 'A' $ lcRpInvExp .OR. 'P' $ lcRpInvExp .OR. 'T' $ lcRpInvExp )
    =lfColTran(m.Fabric,FabDye.cWareCode,FabDye.Color)
  ENDIF

  IF ('C' $ lcRpInvExp)
    =lfColCut(m.Fabric,FabDye.cWareCode,m.Color)
  ENDIF
 
  IF 'R' $ lcRpInvExp
    =lfColRec(m.Fabric,FabDye.cWareCode,m.Color)
  ENDIF


  SELECT (lcRpLTmp)
  SET ORDER TO TAG (lcRpLTmp1)
  IF SEEK(M.FABRIC , lcRpLTmp )
    REPLACE OnHand WITH lnOnhand FOR FABRIC = FABDYE.Fabric
  ENDIF
  SET ORDER TO TAG (lcRpLTmp)

  IF lcFabric # FABDYE.Fabric AND !EMPTY(lcFabric)
    =lfTotValue()
  ENDIF

  lcFabric = FABDYE.Fabric
  lcColor  = FABDYE.Color
  lcWare = FABDYE.cWarecode

ENDSCAN

*--In the last fabric because the last one didn't 
*--use the lfTotValue function inside the scan.
IF lcFabric # FABDYE.Fabric AND !EMPTY(lcFabric)
  =lfTotValue()
ENDIF

IF 'C' $ lcRpInvExp
  SELECT BOMLINE
  CLOSE INDEX
  ERASE (gcWorkDir +lcBomIndx  + '.CDX')
  
  SELECT CTKTBOM
  CLOSE INDEX
  ERASE (gcWorkDir +lcTmpIndx  + '.CDX')
ENDIF

SELECT(lnCurAls)
*!*************************************************************
*! Name      : lfColTran
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function colloect the transaction data.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfColTran
PARAMETER lcFab , lcWare , lcClr
PRIVATE lcAlias , lcOrder

lcAlias = ALIAS()
SELECT (lcRpLTmp)
lcorder = ORDER()
SET ORDER TO TAG lcRpDate

IF SEEK(lcFab,'FINVTADJ') AND (FINVTADJ.TYPE $ 'APT')
  SELECT FINVTADJ
  SCAN REST WHILE Fabric + Color = lcFab ;
            FOR   Color = lcClr .AND. Type $ 'APT';
                  .AND. IIF(llMWHous,((cFromWare = lcWare) OR ;
                                  (cToWare   = lcWare)),.T.)  ;
                  .AND.  IIF(ldFDate <> {} ,BETWEEN(Date,ldFDate,ldTDate),;
                         IIF(ldTDate<>{} .AND. ldFDate={},.T.,Date>=ldTDate))
    SCAT MEMVAR MEMO
    DO CASE
      *--- Adjustments Inv.
      CASE Type = 'A'  .AND. (FInvtAdj.Type $ lcRpInvExp)
        m.TYPE     = 'Adjustments'
        m.Date     = FINVTADJ.DATE
        m.Received = FINVTADJ.NmTotAdj
        m.Issued   = FINVTADJ.NmTotAdj
        
        *--- cWareCode+FABRIC
        IF SEEK(m.cWareCode+m.FABRIC+'Adjustments'+DTOS(FINVTADJ.DATE) , lcRpLTmp)
          SELECT (lcRpLTmp)
          REPLACE Received  WITH Received + m.Received,;
                  Issued    WITH Issued   + m.Issued  ,;
                  Cost      WITH M.Cost               ,;
                  Date      WITH m.Date
        ELSE
          INSERT INTO (lcRpLTmp) FROM MEMVAR
        ENDIF
      *--- Physical Inv.
      CASE Type = 'P' .AND. IIF(llMWHous,(cFromWare = m.cWareCode),.T.) .AND. (FInvtAdj.Type $ lcRpInvExp)
        m.TYPE     = 'Physical'
        m.Date     = FINVTADJ.DATE
        m.Received = FINVTADJ.NmTotAdj
        m.Issued   = FINVTADJ.NmTotAdj

        *--- cWareCode+FABRIC
        IF SEEK(m.cWareCode+m.FABRIC+'Physical'+DTOS(FINVTADJ.DATE) , lcRpLTmp)
          SELECT (lcRpLTmp)
          REPLACE Received  WITH Received + m.Received,;
                  Issued    WITH Issued   + m.Issued  ,;
                  Cost      WITH M.Cost               ,;
                  Date      WITH m.Date
        ELSE
          INSERT INTO (lcRpLTmp) FROM MEMVAR
        ENDIF
      *--- Transfers Inv.
      CASE Type  = 'T' .AND. (FInvtAdj.Type $ lcRpInvExp)
        m.TYPE     = 'Transfers'
        m.Date     = FINVTADJ.DATE        
        m.Received =  0
        m.Issued   =  0
                
        IF m.cWareCode = FINVTADJ.cFromware
          m.Received  =  0
          m.Issued    =  FINVTADJ.NmTotAdj*-1
        ELSE
          IF m.cWareCode = FINVTADJ.cToware
            m.Received  =  FINVTADJ.NmTotAdj
            m.Issued    =  0
          ENDIF
        ENDIF
        *--- cWareCode+FABRIC
        IF SEEK(m.cWareCode+m.FABRIC+'Transfers'+DTOS(FINVTADJ.DATE) , lcRpLTmp)
          SELECT (lcRpLTmp)
          REPLACE Received  WITH Received + m.Received,;
                  Issued    WITH Issued   + m.Issued  ,;
                  Cost      WITH M.Cost               ,;
                  Date      WITH m.Date
        ELSE
          INSERT INTO (lcRpLTmp) FROM MEMVAR
        ENDIF
    ENDCASE
  ENDSCAN
ENDIF

SELECT (lcRpLTmp)
SET ORDER TO &lcorder

SELECT (lcAlias)
*!*************************************************************
*! Name      : lfColCut
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function colloect the data for the  Cut Ticket.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfColCut
PARAMETER lcFab , lcWare , lcClr
PRIVATE lcAlias , lnWip , lcCutTkt

DIMENSION WIP[8]
STORE 0 TO WIP
lcCutTkt = SPACE(0)
lcAlias = ALIAS()

SELECT CTKTBOM
IF SEEK(lcFab,'CTKTBOM') AND SEEK(Cuttkt,'CUTTKTH')
  SCAN REST WHILE ITEM+ICLR = lcFab ;
            FOR   ICLR = lcClr  .AND. ccatgtyp $ 'FT' .AND.  Lvoid = .F. ;
                  .AND. SEEK(Cuttkt,'CUTTKTH') ; 
                  .AND. CUTTKTH.STATUS = "O" ;
                  .AND. IIF(llMWHous,cwarecode = lcWare ,.T.)  ;
                  .AND. IIF(ldFDate <> {} ,BETWEEN(Date,ldFDate,ldTDate),;
                        IIF(ldTDate<>{} .AND. ldFDate={},.T.,Date>=ldTDate))
    SCAT MEMVAR MEMO
    m.TYPE     = 'Cut tickets'
    m.NoofTran = CTKTBOM.Cuttkt
    m.Date     = CTKTBOM.DATE
    m.Issued   = CTKTBOM.Used_Qty*-1       && The Used field to get the final qty. for the transactions.
    STORE 0 TO lnWip

    lcCutTkt = CTKTBOM.CUTTKT
    SELECT CUTTKTL
    IF SEEK(lcCutTkt , 'CUTTKTL')
      SCAN REST WHILE cuttkt+style+dyelot+trancd = lcCutTkt ;
                FOR SUBSTR(CUTTKTL.STYLE,lnClrPos,lnClrLen) = lcClr
        =lfWip()     && Function colloect the data for the work in process.
      ENDSCAN
      m.WIP = lnWip * CTKTBOM.untqty
    ENDIF  
          
    *--- Cut tickets TYPE
    IF SEEK(m.cWareCode+m.FABRIC+'Cut tickets'+ m.NoofTran , lcRpLTmp)
      SELECT (lcRpLTmp)
      REPLACE Issued    WITH Issued   + m.Issued  ,;
              Cost      WITH M.Cost               ,;
              Date      WITH m.Date               ,;
              WIP       WITH WIP + m.WIP
    ELSE
      INSERT INTO (lcRpLTmp) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)
*!*************************************************************
*! Name      : lfColRec
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function colloect the data for the Receiving.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfColRec
PARAMETER lcFab , lcWare , lcClr
PRIVATE lcAlias

lcAlias = ALIAS()
SELECT POFLN
IF SEEK(lcFab+lcClr,'POFLN')
  SCAN REST WHILE Fabric + Color  = lcFab + lcClr ;
            FOR  Trancd = "2" ;
                  .AND. IIF(llMWHous,cWarecode = lcWare ,.T.)  ;
                  .AND.  IIF(ldFDate <> {} ,BETWEEN(Date,ldFDate,ldTDate),;
                         IIF(ldTDate<>{} .AND. ldFDate={},.T.,Date>=ldTDate))
    SCAT MEMVAR MEMO
    m.TYPE     = 'Receiving'
    m.NoofTran = POFLN.Pomat
    m.Date     = POFLN.DATE
    m.Received = IIF(POFLN.cmattype = 'P' , POFLN.nFabtotqty , 0 )
    m.Issued   = IIF(POFLN.cmattype = 'R' , POFLN.nFabtotqty*-1 , 0 )

    *--- Cut tickets TYPE
    IF SEEK(lcWare+m.FABRIC+'Receiving'+ m.NoofTran , lcRpLTmp)
      SELECT (lcRpLTmp)
      REPLACE Received  WITH Received + m.Received,;
              Issued    WITH Issued   + m.Issued  ,;
              Cost      WITH M.Cost               ,;
              Date      WITH m.Date
              
    ELSE
      INSERT INTO (lcRpLTmp) FROM MEMVAR
    ENDIF
  ENDSCAN
ENDIF
SELECT (lcAlias)
*!*************************************************************
*! Name      : lfvLoc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function to valid location mover.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Option Grid.
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : = lfvLoc()
*!*************************************************************
FUNCTION lfvLoc

= gfMover(@laRpSource,@laRpTarget,'Material Location',.T.,'')

*!*************************************************************
*! Name      : lfFillLoc
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Vaildate function to fill location.
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillLoc()
*!*************************************************************
FUNCTION lfFillLoc

DIME laRpSource[1,1]
DIME laRpTarget[1,1]
STORE '' TO laRpSource,laRpTarget
SELECT WareHous
SELECT DISTINCT cWareCode FROM WareHous WHERE lMatInv INTO ARRAY laRpSource
*--lMatInv Field in the Warehouse for the Material Inventory

*!*************************************************************
*! Name      : lfvColor
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function to valid color mover.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : = lfvColor()
*!*************************************************************
FUNCTION lfvColor

= gfMover(@laRpSColor,@laRpTColor,'Color Code',.T.,'')

*!*************************************************************
*! Name      : lfvType
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function to valid type mover.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : None.
*!*************************************************************
*! Example   : = lfvType()
*!*************************************************************
FUNCTION lfvType

= gfMover(@laRpSType,@laRpTType,'Item Type',.T.,'')

*!*************************************************************
*! Name      : lfFillCode
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Vaildate function to fill codes (Color , Type).
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Example     : = lfFillCode()
*!*************************************************************
FUNCTION lfFillCode

*--- Color Array
DIME laRpSColor[1,1]
DIME laRpTColor[1,1]
*--- Item Type Array
DIME laRpSType[1,1]
DIME laRpTType[1,1]
STORE '' TO laRpSType,laRpTType,laRpSColor,laRpTColor
SELECT CODES
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'ITEM_TYPE ' AND crltField = 'N';
                                        INTO ARRAY laRpSType
GOTO TOP
SELECT DISTINCT cCode_NO+' - '+cDiscRep FROM CODES WHERE cDefCode+cFld_Name = 'N'+'COLOR     ' AND crltField = 'N';
                                        INTO ARRAY laRpSColor

*!**************************************************************************
*! Name      : lfSRVFab
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!**************************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSRVFab()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!**************************************************************************
FUNCTION lfSRVFab
PARAMETERS lcParm
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
*-- End of lfSRVFab.
*!*************************************************************
*! Name      : lfvInvTran
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : If user Press Inventory transaction button.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : gfMover
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfvInvTran()
*!*************************************************************
FUNCTION lfvInvTran
= gfMover(@laRpSorInv,@laRpTarInv,'Select Inventory Transaction type',.T.,'')
= lfInvExpr()
*-- end of lfvInvTran function. 

*!*************************************************************
*! Name      : lfInvExpr
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Evaluate Company expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : lfvInvTran
*!*************************************************************
*! Passed Parameters  : ....
*!*************************************************************
*! Returns            : ....
*!*************************************************************
*! Example   : = lfInvExpr()
*!*************************************************************
FUNCTION lfInvExpr

lcRpInvExp = ''

*-- if target array has no items 
IF EMPTY(laRpTarInv)
  =ASORT(laRpSorInv)
  
  *-- loop to get inventory transaction type expression.  
  FOR lnI = 1 TO ALEN(laRpSorInv,1)
    lcRpInvExp = IIF(EMPTY(lcRpInvExp),LEFT(laRpSorInv[lnI],1),;
                      lcRpInvExp + ','+LEFT(laRpSorInv[lnI],1))
  ENDFOR
ELSE  && there're items
  =ASORT(laRpTarInv)
  
  *-- loop to get inventory transaction type expression.
  FOR lnI = 1 TO ALEN(laRpTarInv,1)
    lcRpInvExp = IIF(EMPTY(lcRpInvExp),LEFT(laRpTarInv[lnI],1),;
                      lcRpInvExp + ','+LEFT(laRpTarInv[lnI],1))

  ENDFOR
ENDIF  && end if target array has no items 

*--Save the value of the lcRpInvExp to use it in the comparing if the user change the filter.
*--And get the  variable llChngTrn = .t. if the user change the selection
IF lcRpTrnCmp # lcRpInvExp
  llChngTrn = .T.
  lcRpTrnCmp = lcRpInvExp
ENDIF  

*-- end of lfInvExpr function .
*!*************************************************************
*! Name      : lfvClrTyp
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function To prepare color,item type string and Location.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvClrTyp
*-- Color

FOR lnInd = 1 TO ALEN(laRpTColor)
  lcClrStr = lcClrStr + PADR(laRpTColor[lnInd],6) + ' | '
ENDFOR
lcClrStr = IIF(ALLTRIM(lcClrStr) = '|','',lcClrStr)

*-- Item Type
FOR lnInd = 1 TO ALEN(laRpTType)
  lcTypStr = lcTypStr + PADR(laRpTType[lnInd],6) + ' | '
ENDFOR
lcTypStr = IIF(ALLTRIM(lcTypStr) = '|','',lcTypStr)

*-- Location
FOR lnInd = 1 TO ALEN(laRpTarget)
  lcLocStr = lcLocStr + PADR(laRpTarget[lnInd],8) + ' | '
ENDFOR
lcLocStr = IIF(ALLTRIM(lcLocStr) = '|','',lcLocStr)

*!*************************************************************
*! Name      : lfwRunGrid
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Initialzes source and target arrays for trans.type.
*!             Depend on company setting whether multiwarehouse
*!             or not, we'll have:(Adjustment/Physical/Transfers)
*!             or (Adjustment/Physical) respectively .      
*!             Note that this fn. is called from the report gen.
*!             (OG. when fn.) 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfwRunGrid()
*!*************************************************************
FUNCTION lfwRunGrid

*-- Define Mover for trans. type arrays.  [begin]
DECLARE laRpSorInv[5,1],laRpTarInv[5,1]
STORE 'Adjustments'    TO laRpSorInv[1],laRpTarInv[1]
STORE 'Physical'       TO laRpSorInv[2],laRpTarInv[2]
STORE 'Cut tickets'    TO laRpSorInv[3],laRpTarInv[3]
STORE 'Transfers'      TO laRpSorInv[4],laRpTarInv[4]
STORE 'Receiving'      TO laRpSorInv[5],laRpTarInv[5]

*-- Define Mover for trans. type arrays.  [end  ]
*-- end of lfwRunGrid function.

*!*************************************************************
*! Name      : lfvDate
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function to get the value of the Tran. Date from
*!           : the report criteria to print in the Frx. 
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvDate

*-- Gets the value of the ldFDate & ldTDate from the report criteria 
lnDataPos = ASCAN(laOGFxFlt,'FINVTADJ.DATE')
IF lnDataPos > 0
  ldFDate = CTOD(ALLTRIM(SUBSTR(laOGFxFlt[ASUBSCRIPT(laOGFxFlt,lnDataPos,1),6],1,10)))
  ldTDate = CTOD(ALLTRIM(SUBSTR(laOGFxFlt[ASUBSCRIPT(laOGFxFlt,lnDataPos,1),6],12,20)))
ENDIF


*!*************************************************************
*! Name      : lfChkFlt
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function to CHECK IF FILTER CHANGED.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfChkFlt
PRIVATE lcOldEx
lcOldEx = SET('EXACT')
SET EXACT ON

*---Locatin
FOR lnIndex = 1 TO ALEN(laRpTarget)
  IF ASCAN(laRpOL,laRpTarget[lnIndex]) = 0 .OR. ALEN(laRpTarget) # ALEN(laRpOL)
    llChanged = .T.
    EXIT
  ENDIF
ENDFOR

*--- Color
FOR lnIndex = 1 TO ALEN(laRpTColor)
  IF ASCAN(laRpOC,laRpTColor[lnIndex]) = 0 .OR. ALEN(laRpTColor) # ALEN(laRpOC)
    llChanged = .T.
    EXIT
  ENDIF
ENDFOR

*--- Type
FOR lnIndex = 1 TO ALEN(laRpTType)
  IF ASCAN(laRpOT,laRpTType[lnIndex]) = 0 .OR. ALEN(laRpTType) # ALEN(laRpOT)
    llChanged = .T.
    EXIT
  ENDIF
ENDFOR

SET EXACT &lcOldEx
RETURN llChanged

*!*************************************************************
*! Name      : lfCost
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function to GET THE COST IN BASE CURRENCY.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lnCost
*!*************************************************************
FUNCTION lfCost
PRIVATE lcAlias

STORE 0  TO lnBCost1 , lnBCost2 , lnCost1 , lnCost4
STORE 1  TO lnExRateP , lnUnit1 , lnExRateD , lnUnit2
STORE '' TO lcFrn1 , lcFrn2

lcFrn1  = gfGetExSin("",ALLTRIM(FABRIC.cPriceCur))
lcFrn2  = gfGetExSin("",ALLTRIM(FABRIC.cDutycur))
lnCost1 = IIF(FABRIC.MAke , FABRIC.nMCost1 , FABRIC.nfabcost)
lnCost4 = IIF(FABRIC.MAke , FABRIC.nMCost4 , FABRIC.nItm_Frt)

*-- Calculate the lnExRateP and lnUnit1 for the cost.
IF FABRIC.cPriceCur = gcBaseCurr
  lnExRateP = 1
  lnUnit1 = 1
ELSE
  lnAlias    = SELECT()
  SELECT SYCEXCH
  lnOldTag = TAG()
  SET ORDER TO TAG Currency DESCENDING
  IF !SEEK(gcBaseCurr+FABRIC.cPriceCur) OR SYCEXCH.nExRate = 0
    lnExRateP = SYCEXCH.nExRate
    lnUnit1 = 1
  ELSE
    lnExRateP = SYCEXCH.nExRate
    =SEEK(FABRIC.cPriceCur,"SycCurr")
    lnUnit1   = SycCurr.nCurrUnit
  ENDIF
  SET ORDER TO TAG &lnOldTag
  SELECT(lnAlias)
ENDIF  


*-- Calculate the lnExRateP and lnUnit1 for the Freight.
IF FABRIC.cDutycur = gcBaseCurr
  lnExRateD = 1
  lnUnit2   = 1 
ELSE
  lnAlias    = SELECT()
  SELECT SYCEXCH
  lnOldTag = TAG()
  SET ORDER TO TAG Currency DESCENDING
  IF !SEEK(gcBaseCurr+FABRIC.cDutycur) OR SYCEXCH.nExRate = 0
    lnExRateD = SYCEXCH.nExRate
    lnUnit2   = 1
  ELSE
    lnExRateD = SYCEXCH.nExRate
    =SEEK(FABRIC.cDutycur,"SycCurr")
    lnUnit2   = SycCurr.nCurrUnit
  ENDIF
  SET ORDER TO TAG &lnOldTag
  SELECT(lnAlias)
ENDIF  


*-- Calculate the cost.
lnBCost1   = IIF(lcFrn1 = '/',ROUND(lnCost1/lnExRateP/lnUnit1,3),ROUND(lnCost1*lnExRateP/lnUnit1,3))


*-- Calculate the Freight
lnBCost2   = IIF(lcFrn2 = '/',ROUND(lnCost4/lnExRateD/lnUnit2,3),ROUND(lnCost4*lnExRateD/lnUnit2,3))


*-- ADD the Cost and Freight
lnCost = lnBCost1 + lnBCost2

RETURN LNCOST

*!*************************************************************
*! Name      : lfTotValue
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function to get the onhand.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : lfTotValue
*!*************************************************************
FUNCTION lfTotValue
PRIVATE lcAlias , lcOrder

STORE 0 TO lnTotVal
lcAlias = ALIAS()

SELECT (lcFabHand)
lcOrder = ORDER()
=SEEK(lcFabric , lcFabHand)

SCAN WHILE EVAL(lcFabHand+'.FABRIC') = lcFabric
  lnTotVal = lnTotVal + EVAL(lcFabHand+'.TotOnHand')
ENDSCAN

REPLACE ALL AllOnHnad WITH lnTotVal FOR FABRIC = lcFabric


SET ORDER TO &lcOrder
SELECT(lcAlias)

RETURN ""

*!*************************************************************
*! Name      : lfWip
*! Developer : BASSEM RAFAAT (BWA)
*! Date      : 05/30/2000
*! Purpose   : Function colloect the data for the work in process.
*!*************************************************************
*! Called from : 
*!*************************************************************
*! Calls       : None()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfWip
PRIVATE lcSize

lcSize = "12345678"
IF SEEK('M' + lcCutTkt + lcFab , 'BOMLINE')
  lcSize = BOMLINE.CSIZES
ENDIF

IF TRANCD = '1'
  FOR I = 1 TO 8
    lcQtyT = 'QTY' + ALLTRIM(STR(I))
    IF ALLTRIM(STR(I)) $ lcSize
      WIP[I] = CUTTKTL.&lcQtyT
    ENDIF
  ENDFOR
ELSE && 2 , 3 , 4
  FOR I = 1 TO 8
    lcQtyT = 'QTY' + ALLTRIM(STR(I))
    IF ALLTRIM(STR(I)) $ lcSize
      WIP[I] =  IIF(WIP[I] < CUTTKTL.&lcQtyT , 0 , WIP[I] - CUTTKTL.&lcQtyT )
    ENDIF
  ENDFOR
ENDIF

lnWip = 0
FOR I = 1 TO 8
  IF ALLTRIM(STR(I)) $ lcSize
    lnWip = lnWip + WIP[I]
  ENDIF
ENDFOR

RETURN lnWip
              *-- END OF PROGRAM MAFRE10.PRG --*