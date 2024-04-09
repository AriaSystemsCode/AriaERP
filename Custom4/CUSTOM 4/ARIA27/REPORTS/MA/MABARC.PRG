*:**************************************************************************
*: Program file  : MABARC
*: Program desc. : Bar Code Printing
*: System        : Aria Advantage Series.
*: Module        : Materials (MA)
*: Developer     : Sameh Saiid Ezzat (SSE)
*: Date          : 08/16/2000
*: Reference     : E500361
*:**************************************************************************
*: Calls : 
*:    Procedures : lpCollData, lpCreatFil
*:                   
*:    Functions  : gfModalGen, lfwRepWhen, lfItmPos, lfSRRoll, lfSRVFab, 
*:                 lfFabSum, lfClearRep
*:**************************************************************************
*: Notes : This program works from Output and also from Transaction menu
*:         so we have variable called lcCallFrom defined in Transaction Prg
*:         MAINVC before calling the Report 
*:**************************************************************************
*: Example : DO MABARC
*:**************************************************************************
*B603950,5 SSE 10/16/2000 Fix bug of not collecting Bar Code Qunatities in a Right way 
*B603950,5                The Report was printing for each RollID Receive , Issue and
*B603950,5                Adjustment independently. 
*B603950,5                Also add the WareHouse to appear in Report FRX.
*B603950,5                Remove WhsLoc file from SydReprt (not used anymore).
*B603950,5                Add Fabric Color option in Select Grid 
*B803787,1 ABD 11/05/2000 Fix bug that the report doesn't print the Roll ID completely 
*B803787,1                It prints only18 characters out of 20. 
*B605212,1 ADEL 01/28/02  Let this program print MMFG orders labels also.
*B605659,1 RAE 03/06/2002 Fix the problem of printing the Roll Label from Receive Material
*                         Manufacturing Order
*B037166,1 ABD 09/15/2003 Fix bug that the Bar Code label show the roll Qty 
*B037166,1 ABD            Without Decimal,and print Fabric.UOMuse not Fabric.UomBuy.
*:**************************************************************************

*-- If user changed filter from OG [Begin.]
IF llOGFltCh
  *-- If Temp file is used and has records inside
  IF USED(lcWorkFile) AND RECCOUNT(lcWorkFile) > 0
    DO lpCreatFil
  ENDIF
  
  *-- If Report is working from Transaction Menu collect data this way
  IF TYPE('lcCallFrom') = "C"
    PRIVATE lcRollsID
    lcRollsID = ''
    
    SELECT Rolls
    
    *B603950,5 Adding TranCd = '1' to ensure getting Net Balance Quantity [Begin]
    *SCAN FOR &lcRpExp
    SCAN FOR &lcRpExp AND TranCd = '1' AND nQtyBal > 0
    *B603950,5 Adding TranCd = '1' to ensure getting Net Balance Quantity [End]
    
      *-- If duplicate RollID
      IF !(lcRollsID == crollid+crollitem+color)
        IF SEEK(cRollItem+Color,'Fabric')
          m.cFabDesc  = Fabric.Desc  
          m.cFabWidth = Fabric.Width
          m.cUOMBuy   = Fabric.UOMBuy
          
          *B037166,1 ABD - Fix bug that the Bar Code label show the roll Qty 
          *B037166,1 ABD - Without Decimal, and print Fabric.UOMuse. [Begin]
          m.cUOMUse   = Fabric.UOMUse
          *B037166,1 ABD - [End]
          
          m.cPattern  = Fabric.Pattern
        ENDIF
        
        *B603950,5 Allow Warehouse to appear in Report [Begin]
        *IF SEEK(cRollItem + SPACE(12) + Color + cWareCode,'WhsLoc')
	      *m.cLocation = WhsLoc.cLocation
        *ENDIF
        m.cLocation = cWareCode
        *B603950,5 Allow Warehouse to appear in Report [End]
         
        m.cFabric   = cRollItem
        m.cColor    = Color
        m.cDyelot   = Dyelot	
        
        *B603950,5 Replace Quantity Field with Balance Quantity [Begin]
        *m.nYarDage  = nQty
        m.nYarDage  = nQtyBal
        *B603950,5 Replace Quantity Field with Balance Quantity [End]
        
        m.cBarCode  = cRollId

        INSERT INTO (lcWorkFile) FROM MEMVAR 
     
      ENDIF
      *-- EndIf of duplicate RollID

      lcRollsID = cRollID + cRollItem + Color    
    ENDSCAN
  
  ELSE     && Report is working from OUTPUT Menu
     
    *B605212,1 (Begin) Adjust the hiden exp.
    *lcHidenExp = [POFLN.CMATTYPE = "P"  AND  POFLN.TRANCD = "2"]
    lcHidenExp = IIF(lcRpPrntBy = 'P',[POFLN.CMATTYPE = "P"  AND  POFLN.TRANCD = "2"],[TRANCD = "2"])
    *B605212,1 (End)
    
    *B603950,5 Remove The Fabric Color Expression from lcRpExp [Begin]
    *PRIVATE lcRollsExp , lcRItemExp
    PRIVATE lcRollsExp , lcRItemExp , lcColorExp
    lcColorExp = ".T."
    *B603950,5 Remove The Fabric Color Expression from lcRpExp [End]

    lcRollsExp = ".T."
    lcRItemExp = ".T."
  
    *B603950,5 Include The Fabric Color Expression check inside lcRpExp [Begin]
    *IF "ROLLS.CROLLID" $ lcRpExp OR "ROLLS.CROLLITEM" $ lcRpExp
    IF "ROLLS.CROLLID" $ lcRpExp OR "ROLLS.CROLLITEM" $ lcRpExp OR "ROLLS.COLOR" $ lcRpExp
    *B603950,5 Include The Fabric Color Expression check inside lcRpExp [Begin]
      
      *-- Declare Arrays used in generating rolls expr.
      DIMENSION laTempExpr[1] , laBrTmpFlt[1]
            
      *-- Copy all laOGFxFlt to another array    
      =ACOPY(laOGFxFlt,laTempExpr)

      *-- Copy Browse Array to another Temp array
      =ACOPY(laBrFldFlt,laBrTmpFlt)

      *-- Declare Fixed filter to hold only Rolls ID expression in order to generate
      *-- Rolls ID Expr alone & Declare Browse array to hold only Rolls ID expression
      DIMENSION laOGFxFlt[1,7] , laBrFldFlt[1,5]
     
      *B603950,5 Add an IF condition to check for "ROLLS.CROLLID" in lcRpExp [Begin]
      IF "ROLLS.CROLLID" $ lcRpExp
      *B603950,5 Add an IF condition to check for "ROLLS.CROLLID" in lcRpExp [End]

        *-- Convert numeric position to string
        lcBrowPos = ALLTRIM(STR(lnRollPost))

        *-- Copy only the Rolls Browse Array to laBrFldFlt.
        FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
          IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
            =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),ALEN(laBrTmpFlt,2))
            EXIT
          ENDIF
        ENDFOR
        laBrFldFlt[1,5] = "laOGFxFlt:1"
    
        *-- Copy only the Rolls Expression to laOGFxFlt.
        =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnRollPost,1),ALEN(laOGFxFlt,2))

        *-- Generate InList expression for Rolls ID.
        lcRollsExp = gfGenFlt('laOGFxFlt',.T.,.T.)
        lcRollsExp = IIF(EMPTY(lcRollsExp),".T.",lcRollsExp)

      *B603950,5 Add an ENDIF condition to check for "ROLLS.CROLLID" in lcRpExp [Begin]
      ENDIF
      *B603950,5 Add an ENDIF condition to check for "ROLLS.CROLLID" in lcRpExp [End]

      *B603950,5 Add an IF condition to check for "ROLLS.CROLLITEM" in lcRpExp [Begin]
      IF "ROLLS.CROLLITEM" $ lcRpExp
      *B603950,5 Add an IF condition to check for "ROLLS.CROLLITEM" in lcRpExp [End]

        *-- Convert numeric position to string
        lcBrowPos = ALLTRIM(STR(lnItemPost))  

        *-- Copy only the Rolls Browse Array to laBrFldFlt.
        FOR lnX = 1 TO ALEN(laBrTmpFlt,1)
          IF "laOGFxFlt:&lcBrowPos" $ laBrTmpFlt[lnX,5]
            =ACOPY(laBrTmpFlt,laBrFldFlt,AELEMENT(laBrTmpFlt,lnX,1),ALEN(laBrTmpFlt,2))
            EXIT
          ENDIF
        ENDFOR
        laBrFldFlt[1,5] = "laOGFxFlt:1"
    
        *-- Copy only the Rolls Expression to laOGFxFlt.
        =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnItemPost,1),ALEN(laOGFxFlt,2))

        *-- Generate InList expression for Rolls ID.
        lcRItemExp = gfGenFlt('laOGFxFlt',.T.,.T.)  
        lcRItemExp = IIF(EMPTY(lcRItemExp),".T.",lcRItemExp)

      *B603950,5 Add an ENDIF condition to check for "ROLLS.CROLLITEM" in lcRpExp [Begin]
      ENDIF
      *B603950,5 Add an ENDIF condition to check for "ROLLS.CROLLITEM" in lcRpExp [End]

      *B603950,5 Add a block of code to remove also Fabric Color Expr. [Begin]
      IF "ROLLS.COLOR" $ lcRpExp

        *-- Copy only the Rolls Expression to laOGFxFlt.
        =ACOPY(laTempExpr,laOGFxFlt,AELEMENT(laTempExpr,lnColorPos,1),ALEN(laOGFxFlt,2))

        *-- Generate InList expression for Rolls Fabric Color.
        lcColorExp = gfGenFlt('laOGFxFlt',.T.,.T.)  
        lcColorExp = IIF(EMPTY(lcColorExp),".T.",lcColorExp)

      ENDIF
      *B603950,5 Add a block of code to remove also Fabric Color Expr. [End]

      *-- Restore Browse Array from the Temp array.
      DIMENSION laOGFxFlt[1] , laBrFldFlt[1]
      =ACOPY(laBrTmpFlt,laBrFldFlt)

      *-- Restore Fixed Filter Original array.
      =ACOPY(laTempExpr,laOGFxFlt)
      
      *-- Remove the new Generated filter from lcRpExp.
      lcRpExp = STRTRAN(lcRpExp,lcRollsExp," .T. ")

      *-- Remove the new Generated filter from lcRpExp.
      lcRpExp = STRTRAN(lcRpExp,lcRItemExp," .T. ")

      *B603950,5 Remove the new Fabric color Expression [Begin]
      *-- Remove the new Generated filter from lcRpExp.
      lcRpExp = STRTRAN(lcRpExp,lcColorExp," .T. ")      
      *B603950,5 Remove the new Fabric color Expression [End]
    ENDIF
  
    *B605212,1 (Begin) In case of Material PO.
    IF lcRpPrntBy = 'P'
    *B605212,1 (End)

      SELECT PoFLn    
      SCAN FOR &lcHidenExp AND &lcRpExp
        lcKey = cRSession+Fabric+Color+cWareCode+Dyelot

        IF SEEK(lcKey,'Rolls')
          SELECT Rolls
 
          *B603950,5 Modify Scan..Loop to hold the Color Exp [Begin]
          *SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot = lcKey FOR &lcRollsExp AND &lcRItemExp
          SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot = lcKey FOR &lcRollsExp AND &lcRItemExp AND &lcColorExp
          *B603950,5 Modify Scan..Loop to hold the Color Exp [End]

            IF SEEK(PoFLn.Fabric+PoFLn.Color,'Fabric')
              m.cFabDesc  = Fabric.Desc  
              m.cFabWidth = Fabric.Width
              m.cUOMBuy   = Fabric.UOMBuy

              *B037166,1 ABD - Fix bug that the Bar Code label show the roll Qty 
              *B037166,1 ABD - Without Decimal, and print Fabric.UOMuse. [Begin]
              m.cUOMUse   = Fabric.UOMUse
              *B037166,1 ABD - [End]
              
            ENDIF
        
            *B603950,5 Allow Warehouse to appear in Report [Begin]
            *IF SEEK(PoFLn.Fabric+SPACE(12)+PoFLn.Color+PoFLn.cWareCode,'WhsLoc')
		    *  m.cLocation = WhsLoc.cLocation
            *ENDIF
            m.cLocation = cWareCode
            *B603950,5 Allow Warehouse to appear in Report [End]
        
            m.cFabric   = PoFLn.Fabric
            m.cColor    = PoFLn.Color
	  	    m.cDyelot   = PoFLn.Dyelot	
            m.cPattern  = PoFLn.Pattern
            m.dTranDate = PoFLn.Date
            m.cVendor   = PoFLn.Vendor
 
            *B603950,5 Replace Quantity Field with Balance Quantity [Begin]
            *m.nYarDage  = nQty
            m.nYarDage  = nQtyBal
            *B603950,5 Replace Quantity Field with Balance Quantity [End]
          
            m.cBarCode   = cRollId

            INSERT INTO (lcWorkFile) FROM MEMVAR 
          ENDSCAN
        ENDIF
      ENDSCAN
    
    *B605212,1 (Begin) In case of Material Manufacturing PO.
    ELSE
      SELECT mmfgordd
      SCAN FOR &lcHidenExp AND &lcRpExp
        lcKey = cRSession+cFabric+Color+cWareCode+Dyelot

        IF SEEK(lcKey,'Rolls')
          SELECT Rolls
          SCAN REST WHILE csession+crollitem+color+cwarecode+dyelot = lcKey FOR &lcRollsExp AND &lcRItemExp AND &lcColorExp
            IF SEEK(mmfgordd.cFabric+mmfgordd.Color,'Fabric')
              m.cFabDesc  = Fabric.Desc  
              m.cFabWidth = Fabric.Width
              m.cUOMBuy   = Fabric.UOMBuy

              *B037166,1 ABD - Fix bug that the Bar Code label show the roll Qty 
              *B037166,1 ABD - Without Decimal, and print Fabric.UOMuse. [Begin]
              m.cUOMUse   = Fabric.UOMUse
              *B037166,1 ABD - [End]

            ENDIF
            m.cLocation = cWareCode
            m.cFabric   = mmfgordd.cFabric
            m.cColor    = mmfgordd.Color
	  	    m.cDyelot   = mmfgordd.Dyelot	
            m.cPattern  = mmfgordd.Pattern
            m.dTranDate = mmfgordd.dRecvDate
            m.nYarDage  = nQtyBal
            m.cBarCode  = cRollId
            INSERT INTO (lcWorkFile) FROM MEMVAR 
          ENDSCAN
        ENDIF
      ENDSCAN
    ENDIF  
    *B605212,1 (End)
     
  ENDIF  
ENDIF
*-- EndIf of user changed filter from OG [End.]
*-- If no records in temp file (empty)
SELECT (lcWorkFile)
*-- If Seek is successful (There's Records)

IF RECCOUNT(lcWorkFile) <> 0
  *B803787,1 ABD Print label not report Frx. [Begin]
  *DO gfDispRe WITH EVALUATE('lcRpName')
  DO gfDispRe WITH (lcRpName),'',.F.,'L'
  *B803787,1 ABD [End]
ELSE     && there is no records in Temp file
  *-- <Message> No records to display.
  *-- <Buttons>          OK
  = gfModalGen('TRM00052B00000','DIALOG' )
  SET DEVICE TO SCREEN	
  RETURN
ENDIF
SET DEVICE TO SCREEN
*-- EndIf of no records in temp file (empty)

*!**************************************************************************
*! Name      : lfwRepWhen
*! Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 08/16/2000
*! Purpose   : OG when function
*!**************************************************************************
*! Called from : OG
*!**************************************************************************
*! Example   : = lfwRepWhen()
*!**************************************************************************
*
FUNCTION lfwRepWhen

*-- Rolls file has index tag on Session when running it from OutPut menu
*-- so we change it in case of running from Transaction
IF TYPE('lcCallFrom') = "C"
  SELECT Rolls
  SET ORDER TO TAG Rolls
ENDIF

*-- Get the Position of the Rolls ID position in OG
lnRollPost = lfItmPos('ROLLS.CROLLID')

*-- Get the Position of the Item position in OG
lnItemPost = lfItmPos('ROLLS.CROLLITEM')

*B603950,5 Add a variable to get the position of the Fabric Color Position [Begin]
*-- Get the Position of the Fabric Color position in OG
lnColorPos = lfItmPos('ROLLS.COLOR')
*B603950,5 Add a variable to get the position of the Fabric Color Position [End]

*-- Make SELECT-SQL (Distinct) for the Rolls file to remove the duplicate records
*-- so when Rolls file is browsed in OG we get only one record for each RollID
*-- per RollItem per Color per Dyelot per Warehouse

*B603950,5 Change SQL (Distinct) value to be on only TRANCD = "1" [Begin]
*SELECT DISTINCT Rolls.cRollId , Rolls.cRollItem , Rolls.Color , Rolls.Dyelot , ;
*                Rolls.cWareCode , Rolls.cSession , PoFLn.cRsession ;
*       FROM Rolls , PoFLn ;
*       WHERE Rolls.cSession = PoFLn.cRsession ;
*       INTO DBF (gcWorkDir + lcTmpRolls)
SELECT DISTINCT Rolls.cRollId , Rolls.cRollItem , Rolls.Color , Rolls.Dyelot , ;
                Rolls.cWareCode , Rolls.TranCD ;
       FROM Rolls ;
       WHERE Rolls.TranCD = '1' ;
       INTO DBF (gcWorkDir + lcTmpRolls)
*B603950,5 Change SQL (Distinct) value to be on only TRANCD = "1" [Begin]

INDEX ON cRollId + cRollItem + Color + Dyelot + cWareCode TAG Rolls
       
DO lpCreatFil   && Create the Work Temp file
*-- End of lfwRepWhen.

*!**************************************************************************
*! Name      : lfItmPos
*! Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 08/16/2000
*! Purpose   : To get the position of the fixed filter in OG
*!**************************************************************************
*! Called from : OG When Function 
*!**************************************************************************
*! Example   : = lfItmPos()
*!**************************************************************************
*
FUNCTION lfItmPos
PARAMETERS lcItmInFlt
PRIVATE lnItmPos

lnItmPos = ASCAN(laOGFxFlt,lcItmInFlt)
IF lnItmPos > 0
  lnItmPos = ASUBSCRIPT(laOGFxFlt,lnItmPos,1)
ENDIF
RETURN lnItmPos
*-- End of lfItmPos.

*!**************************************************************************
*! Name      : lpCreatFil
*! Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 08/16/2000
*! Purpose   : Create work File.
*!**************************************************************************
*! Called from : Report code.
*!**************************************************************************
*! Example   : DO lpCreatFil
*!**************************************************************************
*
PROCEDURE lpCreatFil

*nYarDage  --> Rolls.nQty
*cUOMBuy   --> Fabric.UOMBuy
*cBarCode  --> Rolls.cRollId

*B037166,1 ABD - Fix bug that the Bar Code label show the roll Qty 
*B037166,1 ABD - Without Decimal, and print Fabric.UOMuse. [Begin]
*CREATE TABLE (gcWorkDir + lcWorkFile) ;
*  (cFabric C(7), cFabDesc C(20), cColor C(6), cDyelot C(10),;
*  nYarDage N(8,0), cPattern C(10), cUOMBuy C(3), cVendor C(8), ;
*  dTranDate D(8), cFabWidth C(6), cLocation C(10), cBarCode C(20))
CREATE TABLE (gcWorkDir + lcWorkFile) ;
  (cFabric C(7), cFabDesc C(20), cColor C(6), cDyelot C(10),;
  nYarDage N(12,3), cPattern C(10), cUOMBuy C(3), cVendor C(8), ;
  dTranDate D(8), cFabWidth C(6), cLocation C(10), cBarCode C(20), cUomUse C(3))
*B037166,1 ABD - [End]

SELECT (lcWorkFile)
ZAP
INDEX ON cBarCode+cFabric TAG (lcWorkFile)
*-- End of lpCreatFil.

*!**************************************************************************
*! Name      : lfSRRoll
*! Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 08/16/2000
*! Purpose   : control browsing of Rolls file
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSRRoll()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!**************************************************************************
*
FUNCTION lfSRRoll
PARAMETERS lcParm
DO CASE
  CASE lcParm = 'S'  && Set code
    IF USED(lcTmpRolls)
      USE IN (lcTmpRolls)
    ENDIF

    *-- Close Rolls Master file to open the Temp Rolls file with same name
    IF USED('Rolls')
      USE IN ('Rolls')
    ENDIF
    USE (gcWorkDir + lcTmpRolls) IN 0 ALIAS Rolls ORDER TAG Rolls

    SELECT Rolls
    GO TOP
    *B605212,1 (Begin) Browse proper Rolls id.
    SET RELATION TO Rolls.crollitem + Rolls.color INTO Fabric ADDITIVE

    *B605659,1 RAE [START]
    *SET FILTER TO !EOF('FABRIC') AND IIF(lcRpPrntBy = 'P',!FABRIC.MAKE,FABRIC.MAKE)
    IF TYPE('lcCallFrom') = "C"
      IF TYPE('llFromMan') = "L"
        IF llFromMan 
          SET FILTER TO !EOF('FABRIC') AND FABRIC.MAKE
        ELSE
          SET FILTER TO !EOF('FABRIC') AND !FABRIC.MAKE 
        ENDIF
      ELSE
        SET FILTER TO !EOF('FABRIC')
      ENDIF
    ELSE
      SET FILTER TO !EOF('FABRIC') AND IIF(lcRpPrntBy = 'P',!FABRIC.MAKE,FABRIC.MAKE)
    ENDIF
    *B605659,1 RAE [END]
    
    *B605212,1 (End)
  CASE lcParm = 'R'  && Reset code
    IF USED('Rolls')
      USE IN Rolls
    ENDIF
    *-- If Report is working from Output menu , make order by Rolls in Rolls file
    *-- Else report is working from Transaction , make order by Session 
    =gfOpenFile(gcDataDir+'Rolls',gcDataDir+IIF(lcOGRepID=="MABARC",'Rolls','Session'),'SH')
ENDCASE
*-- End of lfSRRoll.

*!**************************************************************************
*! Name      : lfSRVFab
*! Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 08/16/2000
*! Purpose   : control browsing primary fabric and validate 
*!           : selecting it in inlist function.
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfSRVFab()
*!**************************************************************************
*! Note      : SRV symbol is [S,Set--R,Reset--V,Valid]
*!**************************************************************************
*
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

*!**************************************************************************
*! Name      : lfFabSum
*! Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 08/16/2000
*! Purpose   : sum a specific field for the current fabric in fabric file
*!**************************************************************************
*! Called from : Option Grid,fabric browse calculated fields.
*!*************************************************************
*! Returns     : Calculated field value.
*!*************************************************************
*! Example   : =lfFabSum()
*!*************************************************************
*
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

*!**************************************************************************
*! Name      : lfClearRep
*! Developer : Sameh Saiid Ezzat (SSE)
*: Date      : 08/16/2000
*! Purpose   : Function that we call when Close the option grid.
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfClearRep()
*!**************************************************************************
*
FUNCTION lfClearRep
*-- Close temp. opended files, if it is used.
IF USED(lcWorkFile)
  USE IN (lcWorkFile)
ENDIF

*-- Erase the SQL file
IF USED(lcTmpRolls)
  USE IN (lcTmpRolls)
ENDIF
ERASE (gcWorkDir+lcTmpRolls+'.DBF')
ERASE (gcWorkDir+lcTmpRolls+'.CDX')
*-- End of lfClearRep.


*!**************************************************************************
*! Name      : lfvPrnBy
*! Developer : Adel Mohammed El Gazzar (ADEL)
*: Date      : 10/28/02
*! Purpose   : "Print by"  valid function
*!**************************************************************************
*! Called from : [Option Grid] < Close > button.
*!**************************************************************************
*! Example     : = lfvPrnBy()
*!**************************************************************************
*..B605212,1 
FUNCTION lfvPrnBy

CLEAR READ