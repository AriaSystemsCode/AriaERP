*:************************************************************************
*: Program file  : MFOPTR.Prg
*: Program desc. : Operation Transaction Report (Contractor tracking)
*: System        : Aria Advantage Series VER. 2.7
*: Module        : MF,PO,MA
*: Developer     : AHMED MOHAMMED IBRAHIM
*: Date          : 07/28/98
*:************************************************************************
*: Calls : 
*:    Procedures : ....
*:    Functions  : lfPrint(), lfSelect(), lfCostItems(), lfIncRow()
*:                 lfPriTotal(), lfvCont(), lfwOldVal(), lfGetTit()
*:                 lfwOGWhen(), lfsrvTrans(), lfsrvSty()
*:************************************************************************
*: Passed Parameters  : None
*:************************************************************************
*: Example : DO MFOPTR
*:************************************************************************
*: Modifications
*:B602764,1 AMM 06/16/1999 Add the rate field as it is in 2.6
*:B603348,1 ABD Change the picture upon the width of the filed.
*:B605593,1 KHM 02/28/2002 Fix the problem of displaying no records to
*:B605593,1                display when selecting an operation code even 
*:B605593,1                if there were records for the selected operation.
*:B605593,1                In the Sydreprt and Syrepuvr.
*:B606057,1 AMH 06/06/2002 Fix the bug of printing C/T with incorrect contractors. and
*:B606057,1                optimize the code.
*:************************************************************************
STORE SPACE(0) TO laOldVal
STORE 54  TO lnMaxRow
STORE 1   TO PageNo
xReport    = 'MFOPTR'
R_Title    = 'CONTRACTOR TRACKING REPORT'
lcCursName = gfTempName()
lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSize  = LEN(lcMaj)                   && Length of the major
lcStyTitle = gfItemMask('HI')             && Style Title

SELECT STYLE
SET RELATION TO 'S'+STYLE.SCALE INTO SCALE

* lfSelect() will create the main temporary file of the report
IF lfSelect() 
  SET DEVICE TO PRINT
  * Print the report
  =lfPrint()
  DO ENDREPORT
  SET DEVICE TO SCREEN
ENDIF  


IF USED(lcCursName)
  USE IN (lcCursName)
ENDIF
ERASE (gcWorkDir+lcCursName+'.DBF')
ERASE (gcWorkDir+lcCursName+'.CDX')

*!*************************************************************
*! Name      : lfPrint
*! Developer : Ahmed Mohammed Ibrahim
*! Date      : 08/13/1998
*! Purpose   : Print Operation Management Report
*!*************************************************************
*! Called from : MFOPTR.PRG
*!*************************************************************
*! Calls     : lfCostItems(), lfIncRow(), lfPriTotal()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None 
*!*************************************************************
*! Example   : =lfPrint()
*!*************************************************************
FUNCTION lfPrint

* Report format
*0....*....1....*....2....*....3....*....4....*....5....*....6....*....7....*....8....*....9....*....0....*....1....*....2....*....3..
*VEN920                             CONTRACTOR MANAGEMENT REPORT
*MM/DD/YY
*-------------------------------------------------------------------------------------------------------------------------------------
* OPERATION   : 12  123456789012345678901234567890 
*-------------------------------------------------------------------------------------------------------------------------------------
* CONT./DEPT. : 12345678  123456789012345678901234567890 
*-------------------------------------------------------------------------------------------------------------------------------------
* CutTkt#: 123456     Lot#: 12     Item: 123456789012     XXXXXXXXXXXXXXX
*-------------------------------------------------------------------------------------------------------------------------------------
* COLOR  DESCRIPTION     DATE     TRANS.  Size1 Size2 Size3 Size4 Size5 Size6 Size7 Size8 Total Pcs.
* 123456 123456789012345 XX/XX/XX 1234567  1234  1234  1234  1234  1234  1234  1234  1234      12345 
*-------------------------------------------------------------------------------------------------------------------------------------
*                        Total Budget  :  12345 12345 12345 12345 12345 12345 12345 12345     123456 
*                        Total Receive :  12345 12345 12345 12345 12345 12345 12345 12345     123456
*                        Total Damage  :  12345 12345 12345 12345 12345 12345 12345 12345     123456
*                        Total Cancel  :  12345 12345 12345 12345 12345 12345 12345 12345     123456
*                        Total Open    :  12345 12345 12345 12345 12345 12345 12345 12345     123456
*-------------------------------------------------------------------------------------------------------------------------------------
* Issued Cost Items
*-------------------------------------------------------------------------------------------------------------------------------------
* DATE     ITEM         COLOR  MFG QUANTITY UNIT COST TOT. COST UNT ACT CST TOT ACT CST INVOICE No.
* XX/XX/XX 123456789012 123456  12  999.999 99999.999 99999.999   99999.999  999999.999  1234567890
*-------------------------------------------------------------------------------------------------------------------------------------
* Receiving from previous operations
*-------------------------------------------------------------------------------------------------------------------------------------
* DATE     ITEM         COLOR  QUANTITY UNIT COST TOT. COST From Opr
* XX/XX/XX 123456789012 123456    99999   999.999 99999.999 XXXXXXXXXXXXXXX
*-------------------------------------------------------------------------------------------------------------------------------------

DO Rpt_Hdr WITH xReport,xTitle,R_Width
STORE 5 TO lnRow
DIMENSION laBudget[9], laCancel[9], laDamaged[9], laReceive[9], laOpen[9]
SELECT (lcCursName)
SET RELATION TO Item INTO STYLE ADDITIVE
GO TOP

DO WHILE !EOF()
  * Print operation code and description
  lcOprCode = cOprCode
  lcODesc   = gfCodDes(lcOprCode, 'MFGCODE')
  @ lnRow,01 SAY REPLICATE('-',120)
  @ lnRow+1,01 SAY 'Operation :'+lcOprCode+SPACE(5)+lcODesc
  *B602764,1 AMM Add the rate of the operation
  IF SEEK(cimtyp+ctktno+SPACE(25)+cOprCode ,'Ctktbom')
    @ lnRow+1,45 SAY 'Rate : '
    @ lnRow+1,52 SAY CTktBom.UntCost
  ENDIF
  *B602764,1 AMM end  
  
  =lfIncRow(2)
  DO WHILE cOprCode = lcOprCode .AND. !EOF()
    * Print contractor code and name
    lcContCode = cContCode
    @ lnRow,01 SAY REPLICATE('-',120)
    *B602764,1 AMM Get contactor name
    *@ lnRow+1,01 SAY 'Cont./Dept. :'+lcContCode+SPACE(5)+cContName
    lcContName =SPACE(0)
    IF SEEK(cContCode,'APVENDOR')
      lcContName = APVENDOR.cVenComp
    ENDIF
    @ lnRow+1,01 SAY 'Cont./Dept. :'+lcContCode+SPACE(5)+lcContName
    *B602764,1 AMM end
    =lfIncRow(2)
    DO WHILE cOprCode+cContCode = lcOprCode+lcContCode .AND. !EOF()
      lcCutTkt = cTktNo
      lcLotNo  = cLotNo

      @ lnRow,01 SAY REPLICATE('-',120)
      @ lnRow+1,01 SAY lcTitle+': '+lcCutTkt+SPACE(5)+'Lot#: '+lcLotNo+;
                     SPACE(5)+LEFT(lcStyTitle,lnMajSize)+': '+LEFT(Item,lnMajSize)

      =lfIncRow(2)
      lcScale = ''
      lcScale = SCALE.SZ1+SPACE(1)+SCALE.SZ2+SPACE(1)+SCALE.SZ3+SPACE(1)+;
                SCALE.SZ4+SPACE(1)+SCALE.SZ5+SPACE(1)+SCALE.SZ6+SPACE(1)+;
                SCALE.SZ7+SPACE(1)+SCALE.SZ8
      
      IF lcRepForm = 'D'
        @ lnRow,01 SAY REPLICATE('-',120)
        @ lnRow+1,01 SAY SUBSTR(lcStyTitle,lnMajSize+2)+SPACE(2)+'Description'
        @ lnRow+1,42 SAY 'Date       Trans.'
        @ lnRow+1,61 SAY lcScale
        @ lnRow+1,111 SAY 'Tot. Pcs.'
        @ lnRow+2,01 SAY REPLICATE('-',120)          
        =lfIncRow(3)
      ENDIF  
      * Initialize lot total Budget, Damage, Cancel, Receive and
      * Open quantities.
      STORE 0 TO laBudget, laCancel, laDamaged, laReceive, laOpen
      SCAN WHILE cOprCode + cContCode  + cTktNo   + cLotNo = ;
                 lcOprCode+ lcContCode + lcCutTkt + lcLotNo
        DO CASE
          CASE TranCd = '1'
            lcType = 'Budget'
            lcArrName = 'laBudget'
          CASE TranCd $ '25'
            lcType = 'Receive'
            lcArrName = 'laReceive'
          CASE TranCd = '3'
            lcType = 'Damaged'
            lcArrName = 'laDamaged'              
          CASE TranCd = '4'
            lcType = 'Cancel'
            lcArrName = 'laCancel'              
        ENDCASE
        IF lcRepForm = 'D'
          * Print the style NonMajor title and short description
          @ lnRow,01 SAY SUBSTR(ITEM,lnMajSize+2)+SPACE(2)+Style.Desc
          @ lnRow,42 SAY dTranDate
          @ lnRow,53 SAY lcType
        ENDIF  
        
        * Print Size quantities and calculate totals
        FOR lnCount = 1 TO 8
          lcCount = STR(lnCount,1)
          IF lcRepForm = 'D'
            @ lnRow,62+(lnCount-1)*6 SAY nLotQty&lcCount PICTURE '9999'
          ENDIF  
          
          &lcArrName[lnCount] = &lcArrName[lnCount] + nLotQty&lcCount
          laOpen[lnCount] = laOpen[lnCount] + ;
                            IIF(TranCd='1',nLotQty&lcCount,-nLotQty&lcCount)
        ENDFOR  
        IF lcRepForm = 'D'
          @ lnRow, 111 SAY nLotTotQty PICTURE '999999'
          =lfIncRow(1)
        ENDIF  
        &lcArrName[9] = &lcArrName[9] + nLotTotQty
        laOpen[9] = laOpen[9] + IIF(TranCd='1',nLotTotQty,-nLotTotQty)

      ENDSCAN  

      * Print total Lot Budget, Receive, Cancel, Damage and Open totals.
      @ lnRow,01 SAY REPLICATE('-',120)
      =lfIncRow(1)
      =lfPriTotal('Total Budget  :','laBudget')
      =lfPriTotal('Total Receive :','laReceive')
      =lfPriTotal('Total Damaged :','laDamaged')
      =lfPriTotal('Total Cancel  :','laCancel')
      =lfPriTotal('Total Open    :','laOpen')

      * Display Issued cost items.
      IF llDispCost
        =lfCostItems()
      ENDIF  
    ENDDO
  ENDDO
ENDDO

*!*************************************************************
*! Name      : lfSelect
*! Developer : Ahmed Mohamme Ibrahim
*! Date      : 08/13/1998
*! Purpose   : Select records for report
*!*************************************************************
*! Called from : MFOPTR.PRG
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfSelect()
*!*************************************************************
FUNCTION lfSelect

PRIVATE lcTickFIlt,lcStyFIlt,lcDateFIlt,lcKey,lnOpen
STORE '.T.' TO lcTickFIlt,lcStyFIlt,lcDateFIlt

* Create the temporary file 
SELECT MFGOPRDT
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+2,4]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Flag'
laFileStru[lnFileStru,2] = 'L'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'dDate'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

CREATE TABLE (gcWorkDir+lcCursName) FROM ARRAY laFileStru
INDEX ON cOprCode+cContCode+cTktNo+cLotNo+Item+dDate+TranCd TAG (lcCursName)

* Add the filter of status
IF lcStatus <> "L"
   lcRpExp = '(' +lcRpExp+ ") .AND. ("+lcFile+".Status = '"+lcStatus+"')"
ENDIF

SELECT MFGOPRHD
DO CASE
  CASE lcImTyp = 'M' .OR. lcImTyp = 'T'
    
    *B606057,1 AMH Fill the temporary file for C/Ts [Start]
    *SET RELATION TO cTktNo INTO (lcFile) ADDITIVE
    =lfFilTmpCt()
    *B606057,1 AMH [End]
    
  CASE lcImTyp = 'I'
    
    *B606057,1 AMH Fill the temporary file for POs [Start]
    *SET RELATION TO 'P'+cTktNo INTO (lcFile) ADDITIVE
    =lfFilTmpPo()
    *B606057,1 AMH [End]
    
  CASE lcImTyp = 'T'
    SET RELATION TO cTktNo INTO (lcFile) ADDITIVE
ENDCASE

* Fill the temporary file with required data
*B606057,1 AMH Comment the following lines since we have moved it to a separate function [Start]
*SCAN FOR &lcRpExp
*  IF SEEK(lcImTyp+cTktNo+cOprCode,'MFGOPRDT')
*    SELECT MFGOPRDT
*    SCAN WHILE cImTyp+cTktNo+cOprCode= lcImTyp+MFGOPRHD.cTktNo+MFGOPRHD.cOprCode ;
*         FOR TranCd #'0'
*      SCATTER MEMVAR MEMO
*      m.dDate = DTOS(MfgOprDt.dTranDate)
*      m.Flag  = .F.
*      INSERT INTO (lcCursName) FROM MEMVAR
*    ENDSCAN
*  ENDIF
*ENDSCAN
*B606057,1 AMH [End]

IF EOF(lcCursName)
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN(.F.)
ENDIF
* Return if both Open and Competed lots are required.
IF lcLotsStat = 'B'
  RETURN(.T.)
ENDIF
SELECT (lcCursName)
GO TOP
DO WHILE !EOF()
  * Get Open quantity for this Operation/Lot.
  lcKey = cOprCode+cContCode+cTktNo+cLotNo
  STORE 0 TO lnOpen
  IF SEEK(lcImTyp+cTktNo+cOprCode+cLotNo+'1','MfgOprDt')
    SELECT MfgOprDt
    SUM REST IIF(TranCD='1',nLotTotQty,-nLotTotQty) TO lnOpen ;
        WHILE CtktNo+cOprCode+cLotNo+TranCd=;
              &lcCursName..cTktNo+&lcCursName..cOprCode+&lcCursName..cLotNo+'1'
    SELECT (lcCursName)
  ENDIF  
  * Discard open lots if only completed lots are required.
  * Discard completed lots if only open lots are required.
  llFlag = .T.
  IF (lcLotsStat='O' .AND. lnOpen <= 0) .OR. ;
     (lcLotsStat='C' .AND. lnOpen >  0)     
     llFlag = .F.
  ENDIF   
  REPLACE REST Flag WITH llFlag ;
          WHILE cOprCode+cContCode+cTktNo+cLotNo = lcKey
ENDDO
SET FILTER TO Flag
GO TOP
IF EOF()
  *Message 'There are no records to display...!'
  =gfModalGen('TRM00052B00000','DIALOG')
  RETURN(.F.)
ENDIF


*!*************************************************************
*! Name      : lfCostItems
*! Developer : Ahmed Mohamme Ibrahim
*! Date      : 08/13/1998
*! Purpose   : Print Issued Cost Items
*!*************************************************************
*! Called from : lfPrint()
*!*************************************************************
*! Calls     : lfIncRow()
*!*************************************************************
*! Parameters: None
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   : =lfCostItems()
*!*************************************************************
*-------------------------------------------------------------------------------------------------------------------------------------
* Issued Cost Items
*-------------------------------------------------------------------------------------------------------------------------------------
* DATE     ITEM         COLOR  MFG QUANTITY UNIT COST TOT. COST UNT ACT CST TOT ACT CST INVOICE No.
* XX/XX/XX 123456789012 123456  12  999.999 99999.999 99999.999   99999.999  999999.999  1234567890
*-------------------------------------------------------------------------------------------------------------------------------------
* Receiving from previous operations
*-------------------------------------------------------------------------------------------------------------------------------------
* DATE     ITEM         COLOR  QUANTITY UNIT COST TOT. COST From Opr
* XX/XX/XX 123456789012 123456    99999   999.999 99999.999 XXXXXXXXXXXXXXX
*-------------------------------------------------------------------------------------------------------------------------------------
FUNCTION lfCostItems
PRIVATE lnAlias
lnAlias = SELECT()
* Display Issued Cost Items.
SELECT BOMCOST
IF SEEK(lcImTyp+lcCutTkt)
  LOCATE REST WHILE cTktNo = lcCutTkt ;
              FOR   cOprCode+cLotNo = lcOprCode+lcLotNo
  IF FOUND()            
    @ lnRow,01 SAY REPLICATE('-',120)
    @ lnRow+1,01 SAY 'Issued cost items :'
    @ lnRow+2,01 SAY REPLICATE('-',120)

    * DATE       ITEM                COLOR  MFG    QUANTITY  UNIT COST  TOT. COST  UNT ACT CST  TOT ACT CST INVOICE No.'
    * XX/XX/XXXX 1234567890123456789 XXXXXX XXXXXX 99999.99  99999.999  99999.999    99999.999   999999.999 XXXXXXXX
    *B603348,1 ABD Remark The next Line & Move Them To The Left & Right to Indicate The Layout. [Begin]
    *@ lnRow+3,01 SAY 'Date       Item                Color  MFG     Quantity Unit Cost  Tot. Cost  Unt Act Cst  Tot Act Cst Invoice No.'
    @ lnRow+3,01 SAY 'Date       Item                Color  MFG     Quantity   Unit Cost      Tot. Cost Unt Act Cst    Tot Act Cst Invoice No.'    
    *B603348,1 ABD [ End ]
    
    @ lnRow+4,01 SAY REPLICATE('-',120)
    =lfIncRow(5)
    SCAN REST WHILE cTktNo = lcCutTkt ;
              FOR   cOprCode+cLotNo = lcOprCode+lcLotNo
      @ lnRow,01 SAY dTranDate
      @ lnRow,12 SAY Item
      @ lnRow,32 SAY IClr
      @ lnRow,39 SAY MfgCode
      @ lnRow,47 SAY nTotQty   PICTURE '99999.99'
      *B603348,1 ABD Remark the next lines & move them to the right & change the picture 
      *B603348,1     Instead of the fields. [Begin]
      *@ lnRow,56 SAY nUnitCst  PICTURE '99999.999'
      *@ lnRow,67 SAY nTotCst   PICTURE '99999.999'
      *@ lnRow,80 SAY nUnitACst PICTURE '99999.999'
      *@ lnRow,92 SAY nTotACst  PICTURE '999999.999'
      *@ lnRow,103 SAY cApInvNo
      @ lnRow,56 SAY nUnitCst  PICTURE '9999999.999'
      @ lnRow,68 SAY nTotCst   PICTURE '9999999999.999'
      @ lnRow,83 SAY nUnitACst PICTURE '9999999.999'
      @ lnRow,95 SAY nTotACst  PICTURE '9999999999.999'
      @ lnRow,110 SAY cApInvNo
      *B603348,1 ABD [ End ]
      =lfIncRow(1)
    ENDSCAN
  ENDIF  
ENDIF
* Display cost items received from previous operations.
SELECT MfgOprDt 
SET ORDER TO TAG TrgOpr
IF SEEK(lcImTyp+lcCutTkt+lcOprCode+lcLotNo+'2')
  @ lnRow,01 SAY REPLICATE('-',120)
  @ lnRow+1,01 SAY 'Receiving from previous operations :'
  @ lnRow+2,01 SAY REPLICATE('-',120)
  @ lnRow+3,01 SAY  'Date       Item                Quantity  From Opr.'
  *:                 XX/XX/XXXX 1234567890123456789    99999  XXXXXX
  @ lnRow+4,01 SAY REPLICATE('-',120)
  =lfIncRow(5)
  SCAN REST WHILE cTktNo+cTrgOpr+cTrgLot+TranCd = ;
                  lcCutTkt+lcOprCode+lcLotNo+'2'
    lcODesc   = gfCodDes(cOprCode, 'MFGCODE')
    @ lnRow,01 SAY dTranDate
    @ lnRow,12 SAY Item
    @ lnRow,35 SAY nLotTotQty   PICTURE '99999'
    @ lnRow,42 SAY lcODesc
    =lfIncRow(1)
  ENDSCAN                
ENDIF
SELECT (lnAlias)
RETURN(.T.)

*!*************************************************************
*! Name      : lfIncRow
*! Developer : Ahmed Mohamme Ibrahim
*! Date      : 08/13/1998
*! Purpose   : Increment report line by a specific number and check 
*!             exceeding max. lines to print header
*!*************************************************************
*! Called from : lfPrint(), lfCostItems(), lfPriTotal()
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Parameters: lnIncBy : Number of lines to be increamented.
*!*************************************************************
*! Example   : =lfIncRow(4)
*!*************************************************************
FUNCTION lfIncRow
PARAMETERS lnIncBy

lnRow = lnRow + lnIncBy
IF lnRow >= lnMaxRow
  PageNo = PageNo+1
  DO Rpt_Hdr WITH xReport,xTitle,R_Width
  lnRow = 5
ENDIF
RETURN(.T.)


*!*************************************************************
*! Name      : lfPriTotal
*! Developer : Ahmed Mohamme Ibrahim
*! Date      : 08/13/1998
*! Purpose   : Print lot total
*!*************************************************************
*! Called from : lfPrint()
*!*************************************************************
*! Calls     : lfIncRow()
*!*************************************************************
*! Parameters: lcTitle : Total title
*!             laArray : Array holding total.  
*!*************************************************************
*! Example   : =lfPriTotal('Total Budget  :','laBudget')
*!*************************************************************
FUNCTION lfPriTotal
PARAMETERS lcTit, laArray
@ lnRow,44 SAY lcTit

FOR lnCount = 1 TO 8
  @ lnRow,61+(lnCount-1)*6 SAY &laArray[lnCount] PICTURE '99999'
ENDFOR  
@ lnRow, 111 SAY &laArray[9] PICTURE '999999'

=lfIncRow(1)
RETURN(.T.)

*!*************************************************************
*! Name      : lfvCont
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : Valid function of the contractor setting in the option grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : gfApVnBrow()
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvCont()
*!*************************************************************
FUNCTION lfvCont

PRIVATE lcVar, lcObj
lnAlias = SELECT(0)
lcVar = SYS(18)             && Varible to hold  the name of the memory variable used to create the current GET control
lcObj = ALLTRIM(  ( EVALUATE(SYS(18)) )  )   && Varible to hold the current field value
SELECT APVENDOR
SET ORDER TO TAG VenCode 
*IF Statment to check if we are going to Browse
IF !EMPTY(lcObj) .AND. ('?' $ lcObj )
  IF !SEEK(lcObj , 'APVENDOR'))
    =gfApVnBrow(@lcObj)
    IF !EMPTY(lcObj)
      &lcVar = lcObj      && Update the field
    ELSE
      &lcVar = laOldVal
    ENDIF
  ENDIF
ELSE
  IF !SEEK(lcObj , 'APVENDOR'))
    * "This contractor doesn't exist in the data file !!" 
    =gfModalGen('TRM00001B00000','DIALOG','This contractor')
  ENDIF
ENDIF

SELECT (lnAlias)
*!*************************************************************
*! Name      : lfwOldVal
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : Store the old value.
*!*************************************************************
*! Called from : MFOPMG.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOldVal()
*!*************************************************************
FUNCTION lfwOldVal

laOldVal = EVALUATE(SYS(18))

*!*************************************************************
*! Name      : lfGetTit
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : Get title to be displayed (PO or cuttkt or MFG order)
*!*************************************************************
*! Called from : The option grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : lcTit
*!*************************************************************
*! Example     : = lfGetTit()
*!*************************************************************
FUNCTION lfGetTit

PRIVATE lcTit
DO CASE
  CASE GCACT_APPL='PO'
    lcTit   = 'Purchase Order'
    lcImTyp = 'I'
  CASE GCACT_APPL='MA'
    lcTit   = 'MFG Order'
    lcImTyp = 'T'
  CASE GCACT_APPL='MF'
    lcTit   = ALLTRIM(gfGetMemvar('M_PRDLNLBL',gcAct_Comp))  
    lcImTyp = 'M'
ENDCASE
lcTit  = IIF(RIGHT(lcTit,1) ='#', lcTit,lcTit+'#')
RETURN lcTit

*!*************************************************************
*! Name      : lfwOGWhen
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : When function of the option grid
*!*************************************************************
*! Called from : MFOPTR.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOGWhen()
*!*************************************************************
FUNCTION lfwOGWhen
*R_WIDTH    = 'W'
DO CASE
  CASE GCACT_APPL='PO'
    lcFile  = 'POSHDR'
    lcField = 'PO'
  CASE GCACT_APPL='MA'
    lcFile  = 'MMFGORDH'
    lcField = 'cMFGOrdNo'
  CASE GCACT_APPL='MF'
    lcFile  = 'CUTTKTH'
    lcField = 'CutTkt'
ENDCASE

*!*************************************************************
*! Name      : lfsrvTrans
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : MFOPTR.PRG
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
PRIVATE lnAlias
lnAlias=SELECT(0)
DO CASE
  CASE lcParm = 'S'  && Set code
    SET ORDER TO VENCODE IN APVENDOR
    SELECT POSHDR
    SET RELATION TO Poshdr.vendor INTO Apvendor ADDITIVE
  CASE lcParm = 'R'  && Reset code
    SELECT POSHDR
    SET RELATION TO
ENDCASE
SELECT (lnAlias)

*!*************************************************************
*! Name      : lfsrvSty
*! Developer : AHMED MOHAMMED IBRAHIM
*! Date      : 08/13/98
*! Purpose   : To set relation on or off when running the in range function 
*!             in the option grid.
*!*************************************************************
*! Called from : MFOPTR.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : lcParm
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfsrvSty()
*!*************************************************************
FUNCTION lfsrvSty
PARAMETERS lcParm
IF lcParm = 'S'  && Set code
  SET ORDER TO TAG CSTYLE IN STYLE
ELSE
  SET ORDER TO TAG STYLE IN STYLE
ENDIF

*!*************************************************************
*! Name      : lfFilTmpCt
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/02/2002
*! Purpose   : Fill the temporary file for C/Ts
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFilTmpCt()
*!*************************************************************
*B606057,1 AMH
FUNCTION lfFilTmpCt

*-- To get the selected C/Ts if any.
lcCutFile = ''
llUseCut = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CUTTKT'),1)
IF lnPosition > 0
  lcCutFile = laOGFxFlt[lnPosition,6]
  llUseCut = IIF(!EMPTY(lcCutFile) .AND. USED(lcCutFile) .AND. RECCOUNT(lcCutFile)>0,.T.,.F.)
ENDIF

*-- To get the selected style if any.
lcStylFile = ''
llUseStyle = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.STYLE'),1)
IF lnPosition > 0
  lcStylFile = laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Contractor if any.
lcContFile = ''
llUseContr = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'MFGOPRHD.CCONTCODE'),1)
IF lnPosition > 0
  lcContFile = laOGFxFlt[lnPosition,6]
  llUseContr = IIF(!EMPTY(lcContFile) .AND. USED(lcContFile) .AND. RECCOUNT(lcContFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Operation if any.
lcOperts = ''
llUseOpert = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'MFGOPRHD.COPRCODE'),1)
IF lnPosition > 0
  lcOperts = laOGFxFlt[lnPosition,6]
  llUseOpert = IIF(LEN(lcOperts)>0,.T.,.F.)
ENDIF

*-- To get the selected Division if any.
lcDivs = ''
llUseDiv = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.CDIVISION'),1)
IF lnPosition > 0
  lcDivs = laOGFxFlt[lnPosition,6]
  llUseDiv = IIF(LEN(lcDivs)>0,.T.,.F.)
ENDIF

*-- To get the selected Season if any.
lcSeasons = ''
llUseSeas = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.SEASON'),1)
IF lnPosition > 0
  lcSeasons = laOGFxFlt[lnPosition,6]
  llUseSeas = IIF(LEN(lcSeasons)>0,.T.,.F.)
ENDIF

*-- To get the Completion date.
lcCompDate = '.T.'
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'CUTTKTH.COMPLETE'),1)
IF lnPosition > 0 .AND. !EMPTY(laOGFxFlt[lnPosition,6])
  lnSepPos = AT('|',laOGFxFlt[lnPosition,6])
  IF !EMPTY(CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,lnSepPos-1))) .OR.;
     !EMPTY(CTOD(SUBSTR(laOGFxFlt[lnPosition,6],lnSepPos+1)))
    lcCompDate = 'BETWEEN(CUTTKTH.COMPLETE,{'+STRTRAN(laOGFxFlt[lnPosition,6],'|','},{')+'})'
  ENDIF
ENDIF

IF lcStatus = 'L'
  lcStatus = ''
ENDIF

IF llUseCut
  SELECT CUTTKTH
  lcOrdCut = SET('ORDER')
  SET ORDER TO CUTTKTH
  SELECT MFGOPRDT
  lcOrdOprDt = SET('ORDER')
  SET ORDER TO MFGOPRDT
  SELECT (lcCutFile)
  SCAN
    IF SEEK(CUTTKT,'CUTTKTH') .AND. IIF(llUseStyle,SEEK(CUTTKTH.STYLE,lcStylFile),.T.) .AND.;
       CUTTKTH.STATUS=lcStatus .AND. IIF(llUseDiv,CUTTKTH.CDIVISION$lcDivs,.T.) .AND.;
       IIF(llUseSeas,CUTTKTH.SEASON$lcSeasons,.T.) .AND. &lcCompDate. .AND. SEEK('M'+CUTTKT,'MFGOPRDT')
      SELECT MFGOPRDT
      SCAN REST WHILE cImTyp+cTktNo+cOprCode+cLotNo+TranCd = 'M'+CUTTKTH.CUTTKT;
                  FOR IIF(llUseContr,SEEK(CCONTCODE,lcContFile),.T.) .AND.;
                      IIF(llUseOpert,COPRCODE$lcOperts,.T.) .AND. TranCd #'0'
        SCATTER MEMVAR MEMO
        m.dDate = DTOS(MfgOprDt.dTranDate)
        m.Flag  = .F.
        INSERT INTO (lcCursName) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDSCAN
  SET ORDER TO &lcOrdCut. IN CUTTKTH
  SET ORDER TO &lcOrdOprDt. IN MFGOPRDT
ELSE
  IF llUseStyle
    SELECT CUTTKTH
    lcOrdCut = SET('ORDER')
    SET ORDER TO CUTTKTHS
    SELECT MFGOPRDT
    lcOrdOprDt = SET('ORDER')
    SET ORDER TO MFGOPRDT
    SELECT (lcStylFile)
    SCAN
      IF SEEK(CSTYMAJOR,'CUTTKTH')
        SELECT CUTTKTH
        SCAN REST WHILE Style+Cuttkt = EVALUATE(lcStylFile+'.CSTYMAJOR');
                    FOR STATUS=lcStatus .AND. IIF(llUseDiv,CDIVISION$lcDivs,.T.) .AND.;
                        IIF(llUseSeas,SEASON$lcSeasons,.T.) .AND. &lcCompDate. .AND.;
                        SEEK('M'+CUTTKT,'MFGOPRDT')
          SELECT MFGOPRDT
          SCAN REST WHILE cImTyp+cTktNo+cOprCode+cLotNo+TranCd = 'M'+CUTTKTH.CUTTKT;
                      FOR IIF(llUseContr,SEEK(CCONTCODE,lcContFile),.T.) .AND.;
                          IIF(llUseOpert,COPRCODE$lcOperts,.T.) .AND. TranCd #'0'
            SCATTER MEMVAR MEMO
            m.dDate = DTOS(MfgOprDt.dTranDate)
            m.Flag  = .F.
            INSERT INTO (lcCursName) FROM MEMVAR
          ENDSCAN
        ENDSCAN
      ENDIF
    ENDSCAN
    SET ORDER TO &lcOrdCut. IN CUTTKTH
    SET ORDER TO &lcOrdOprDt. IN MFGOPRDT
  ELSE
    IF llUseContr
      SELECT CUTTKTH
      lcOrdCut = SET('ORDER')
      SET ORDER TO CUTTKTH
      SELECT MFGOPRDT
      lcOrdOprDt = SET('ORDER')
      SET ORDER TO MFGCONT
      SELECT (lcContFile)
      SCAN
        IF SEEK('M'+CVENDCODE,'MFGOPRDT')
          SELECT MFGOPRDT
          SCAN REST WHILE cImTyp+cContCode+cTktNo+cOprCode = 'M'+EVALUAT(lcContFile+'.CVENDCODE');
                      FOR IIF(llUseOpert,COPRCODE$lcOperts,.T.) .AND. TranCd #'0' .AND.;
                          SEEK(CTKTNO,'CUTTKTH') .AND. CUTTKTH.STATUS=lcStatus .AND.;
                          IIF(llUseDiv,CUTTKTH.CDIVISION$lcDivs,.T.) .AND.;
                          IIF(llUseSeas,CUTTKTH.SEASON$lcSeasons,.T.) .AND. &lcCompDate.
            SCATTER MEMVAR MEMO
            m.dDate = DTOS(MfgOprDt.dTranDate)
            m.Flag  = .F.
            INSERT INTO (lcCursName) FROM MEMVAR
          ENDSCAN
        ENDIF
      ENDSCAN
      SET ORDER TO &lcOrdCut. IN CUTTKTH
      SET ORDER TO &lcOrdOprDt. IN MFGOPRDT
    ELSE
      SELECT CUTTKTH
      lcOrdCut = SET('ORDER')
      SET ORDER TO CUTTKTH
      SELECT MFGOPRDT
      lcOrdOprDt = SET('ORDER')
      SET ORDER TO MFGOPRDT
      SELECT MFGOPRHD
      lcOrdOprHd = SET('ORDER')
      SET ORDER TO MFGOPRHD
      =SEEK('M')
      SCAN REST WHILE cImTyp+cTktNo+cOprCode = 'M' FOR IIF(llUseOpert,COPRCODE$lcOperts,.T.)
        IF SEEK('M'+CTKTNO+COPRCODE,'MFGOPRDT')
          SELECT MFGOPRDT
          SCAN REST WHILE cImTyp+cTktNo+cOprCode+cLotNo+TranCd = 'M'+MFGOPRHD.CTKTNO+MFGOPRHD.COPRCODE;
                      FOR TranCd #'0' .AND. SEEK(CTKTNO,'CUTTKTH') .AND.;
                          CUTTKTH.STATUS=lcStatus .AND. IIF(llUseDiv,CUTTKTH.CDIVISION$lcDivs,.T.) .AND.;
                          IIF(llUseSeas,CUTTKTH.SEASON$lcSeasons,.T.) .AND. &lcCompDate.
            SCATTER MEMVAR MEMO
            m.dDate = DTOS(MfgOprDt.dTranDate)
            m.Flag  = .F.
            INSERT INTO (lcCursName) FROM MEMVAR
          ENDSCAN
        ENDIF
      ENDSCAN
      SET ORDER TO &lcOrdCut. IN CUTTKTH
      SET ORDER TO &lcOrdOprDt. IN MFGOPRDT
      SET ORDER TO &lcOrdOprHd. IN MFGOPRHD
    ENDIF
  ENDIF
ENDIF

*!*************************************************************
*! Name      : lfFilTmpPo
*! Developer : AHMED MAHER (AMH)
*! Date      : 07/02/2002
*! Purpose   : Fill the temporary file for POs
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfFilTmpPo()
*!*************************************************************
*B606057,1 AMH
FUNCTION lfFilTmpPo

*-- To get the selected POs if any.
lcPOFile = ''
llUsePO = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,"IIF(.T.,MFGOPRHD.CTKTNO,'')"),1)
IF lnPosition > 0
  lcPOFile = laOGFxFlt[lnPosition,6]
  llUsePO = IIF(!EMPTY(lcPOFile) .AND. USED(lcPOFile) .AND. RECCOUNT(lcPOFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Contractor if any.
lcContFile = ''
llUseContr = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'MFGOPRHD.CCONTCODE'),1)
IF lnPosition > 0
  lcContFile = laOGFxFlt[lnPosition,6]
  llUseContr = IIF(!EMPTY(lcContFile) .AND. USED(lcContFile) .AND. RECCOUNT(lcContFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Operation if any.
lcOperts = ''
llUseOpert = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'MFGOPRHD.COPRCODE'),1)
IF lnPosition > 0
  lcOperts = laOGFxFlt[lnPosition,6]
  llUseOpert = IIF(LEN(lcOperts)>0,.T.,.F.)
ENDIF

*-- To get the selected Division if any.
lcDivs = ''
llUseDiv = .F.
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.CDIVISION'),1)
IF lnPosition > 0
  lcDivs = laOGFxFlt[lnPosition,6]
  llUseDiv = IIF(LEN(lcDivs)>0,.T.,.F.)
ENDIF

*-- To get the Completion date.
lcCompDate = '.T.'
lnPosition = ASUBSCRIPT(laOGFxFlt,ASCAN(laOGFxFlt,'POSHDR.COMPLETE'),1)
IF lnPosition > 0 .AND. !EMPTY(laOGFxFlt[lnPosition,6])
  lnSepPos = AT('|',laOGFxFlt[lnPosition,6])
  IF !EMPTY(CTOD(SUBSTR(laOGFxFlt[lnPosition,6],1,lnSepPos-1))) .OR.;
     !EMPTY(CTOD(SUBSTR(laOGFxFlt[lnPosition,6],lnSepPos+1)))
    lcCompDate = 'BETWEEN(POSHDR.COMPLETE,{'+STRTRAN(laOGFxFlt[lnPosition,6],'|','},{')+'})'
  ENDIF
ENDIF

IF lcStatus = 'L'
  lcStatus = ''
ENDIF

IF llUsePO
  SELECT POSHDR
  lcOrdPO = SET('ORDER')
  SET ORDER TO POSHDR
  SELECT MFGOPRDT
  lcOrdOprDt = SET('ORDER')
  SET ORDER TO MFGOPRDT
  SELECT (lcPOFile)
  SCAN
    IF SEEK('P'+PO,'POSHDR') .AND. POSHDR.STATUS=lcStatus .AND.;
       IIF(llUseDiv,POSHDR.CDIVISION$lcDivs,.T.) .AND.;
       &lcCompDate. .AND. SEEK('I'+POSHDR.PO,'MFGOPRDT')
      SELECT MFGOPRDT
      SCAN REST WHILE cImTyp+cTktNo+cOprCode+cLotNo+TranCd = 'I'+POSHDR.PO;
                  FOR IIF(llUseContr,SEEK(CCONTCODE,lcContFile),.T.) .AND.;
                      IIF(llUseOpert,COPRCODE$lcOperts,.T.) .AND. TranCd #'0'
        SCATTER MEMVAR MEMO
        m.dDate = DTOS(MfgOprDt.dTranDate)
        m.Flag  = .F.
        INSERT INTO (lcCursName) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDSCAN
  SET ORDER TO &lcOrdPO. IN POSHDR
  SET ORDER TO &lcOrdOprDt. IN MFGOPRDT
ELSE
  IF llUseContr
    SELECT POSHDR
    lcOrdPO = SET('ORDER')
    SET ORDER TO POSHDR
    SELECT MFGOPRDT
    lcOrdOprDt = SET('ORDER')
    SET ORDER TO MFGCONT
    SELECT (lcContFile)
    SCAN
      IF SEEK('I'+CVENDCODE,'MFGOPRDT')
        SELECT MFGOPRDT
        SCAN REST WHILE cImTyp+cContCode+cTktNo+cOprCode = 'I'+EVALUAT(lcContFile+'.CVENDCODE');
                    FOR IIF(llUseOpert,COPRCODE$lcOperts,.T.) .AND. TranCd #'0' .AND.;
                        SEEK('P'+CTKTNO,'POSHDR') .AND. POSHDR.STATUS=lcStatus .AND.;
                        IIF(llUseDiv,POSHDR.CDIVISION$lcDivs,.T.) .AND. &lcCompDate.
          SCATTER MEMVAR MEMO
          m.dDate = DTOS(MfgOprDt.dTranDate)
          m.Flag  = .F.
          INSERT INTO (lcCursName) FROM MEMVAR
        ENDSCAN
      ENDIF
    ENDSCAN
    SET ORDER TO &lcOrdPO. IN POSHDR
    SET ORDER TO &lcOrdOprDt. IN MFGOPRDT
  ELSE
    SELECT POSHDR
    lcOrdPO = SET('ORDER')
    SET ORDER TO POSHDR
    SELECT MFGOPRDT
    lcOrdOprDt = SET('ORDER')
    SET ORDER TO MFGOPRDT
    SELECT MFGOPRHD
    lcOrdOprHd = SET('ORDER')
    SET ORDER TO MFGOPRHD
    =SEEK('I')
    SCAN REST WHILE cImTyp+cTktNo+cOprCode = 'I' FOR IIF(llUseOpert,COPRCODE$lcOperts,.T.)
      IF SEEK('I'+CTKTNO+COPRCODE,'MFGOPRDT')
        SELECT MFGOPRDT
        SCAN REST WHILE cImTyp+cTktNo+cOprCode+cLotNo+TranCd = 'I'+MFGOPRHD.CTKTNO+MFGOPRHD.COPRCODE;
                    FOR TranCd #'0' .AND. SEEK('P'+CTKTNO,'POSHDR') .AND.;
                        POSHDR.STATUS=lcStatus .AND. IIF(llUseDiv,POSHDR.CDIVISION$lcDivs,.T.) .AND.;
                        &lcCompDate.
          SCATTER MEMVAR MEMO
          m.dDate = DTOS(MfgOprDt.dTranDate)
          m.Flag  = .F.
          INSERT INTO (lcCursName) FROM MEMVAR
        ENDSCAN
      ENDIF
    ENDSCAN
    SET ORDER TO &lcOrdPO. IN POSHDR
    SET ORDER TO &lcOrdOprDt. IN MFGOPRDT
    SET ORDER TO &lcOrdOprHd. IN MFGOPRHD
  ENDIF
ENDIF