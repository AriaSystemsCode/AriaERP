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
STORE SPACE(0) TO laOldVal
STORE 54  TO lnMaxRow
STORE 1   TO PageNo
loOgScroll.cCRorientation = 'L'

DO CASE
  CASE oAriaApplication.ActiveModuleID ='PO'
    lcImTyp = 'I'
  CASE oAriaApplication.ActiveModuleID  ='MA'
    lcImTyp = 'T'
  CASE oAriaApplication.ActiveModuleID  ='MF'
    lcImTyp = 'M'
ENDCASE

lcMaj      = gfItemMask('PM')             && Get the major of the style
lnMajSize  = LEN(lcMaj)                   && Length of the major
lcStyTitle = gfItemMask('HI')             && Style Title


IF loOgScroll.llOGFltCh && OG Filters changed
  IF lfSelect() AND RECCOUNT(lcCursName) > 0
    SELECT(lcCursName)
    lfCostItems()
    SELECT(lcCursName)
    IF  lcLotsStat = 'B'
      COPY TO oAriaApplication.WorkDir + lcFilName +".DBF" 
      USE oAriaApplication.WorkDir + lcFilName +".DBF"  IN 0 EXCLUSIVE 
      SELECT(lcFilName)
      ZAP 
      INDEX  on cOprCode+cContCode+cTktNo+cLotNo+LEFT(ITEM,lnMajSize)TO (lcFilName)
      SELECT(lcCursName)
      
      SCAN 
        IF !SEEK(cOprCode+cContCode+cTktNo+cLotNo+LEFT(ITEM,lnMajSize),lcFilName )
          STORE 0 TO  m.nlotqty1 , m.nlotqty2, m.nlotqty3,  m.nlotqty4 ,m.nlotqty5 , m.nlotqty6 , m.nlotqty7 ,m.nlotqty8 , m.nlottotqty 
          STORE 0 TO  m.can1 , m.can2, m.can3,  m.can4 ,m.can5 , m.can6 , m.can7 ,m.can8 , m.totcan 
          STORE 0 TO  m.dam1 , m.dam2, m.dam3,  m.dam4 ,m.dam5 , m.dam6 , m.dam7 ,m.dam8 , m.totdam 
          STORE 0 TO  m.rec1 , m.rec2, m.rec3,  m.rec4 ,m.rec5 , m.rec6 , m.rec7 ,m.rec8 , m.totrec 

          SCATTER MEMO MEMVAR  field EXCEPT nlotqty1,nlotqty2,nlotqty3,nlotqty4,nlotqty5,nlotqty6,nlotqty7,nlotqty8,nlottotqty
          DO CASE 
        
          CASE m.trancd = '1'
            m.nlotqty1 = &lcCursName..nlotqty1
            m.nlotqty2 = &lcCursName..nlotqty2
            m.nlotqty3 = &lcCursName..nlotqty3
            m.nlotqty4 = &lcCursName..nlotqty4
            m.nlotqty5 = &lcCursName..nlotqty5
            m.nlotqty6 = &lcCursName..nlotqty6
            m.nlotqty7 = &lcCursName..nlotqty7
            m.nlotqty8 = &lcCursName..nlotqty8
            m.nlottotqty = &lcCursName..nlottotqty

          CASE m.trancd = '4'
            m.can1 = &lcCursName..nlotqty1
            m.can2 = &lcCursName..nlotqty2
            m.can3 = &lcCursName..nlotqty3
            m.can4 = &lcCursName..nlotqty4
            m.can5 = &lcCursName..nlotqty5
            m.can6 = &lcCursName..nlotqty6
            m.can7 = &lcCursName..nlotqty7
            m.can8 = &lcCursName..nlotqty8
            m.totcan = &lcCursName..nlottotqty
          
          CASE m.trancd $ '25'
            m.rec1 = &lcCursName..nlotqty1
            m.rec2 = &lcCursName..nlotqty2
            m.rec3 = &lcCursName..nlotqty3
            m.rec4 = &lcCursName..nlotqty4
            m.rec5 = &lcCursName..nlotqty5
            m.rec6 = &lcCursName..nlotqty6
            m.rec7 = &lcCursName..nlotqty7
            m.rec8 = &lcCursName..nlotqty8
            m.totrec = &lcCursName..nlottotqty
          
          CASE m.trancd = '3'
            m.dam1 = &lcCursName..nlotqty1
            m.dam2 = &lcCursName..nlotqty2
            m.dam3 = &lcCursName..nlotqty3
            m.dam4 = &lcCursName..nlotqty4
            m.dam5 = &lcCursName..nlotqty5
            m.dam6 = &lcCursName..nlotqty6
            m.dam7 = &lcCursName..nlotqty7
            m.dam8 = &lcCursName..nlotqty8
            m.totdam = &lcCursName..nlottotqty
          
          ENDCASE 

  
        INSERT INTO (lcFilName) FROM MEMVAR 
      ELSE
        SELECT(lcFilName)
        DO CASE 
          CASE &lcCursName..trancd = '1'
           
            REPLACE nlotqty1   WITH nlotqty1 + &lcCursName..nlotqty1
            REPLACE nlotqty2   WITH nlotqty2 + &lcCursName..nlotqty2
            REPLACE nlotqty3   WITH nlotqty3 + &lcCursName..nlotqty3
            REPLACE nlotqty4   WITH nlotqty4 + &lcCursName..nlotqty4
            REPLACE nlotqty5   WITH nlotqty5 + &lcCursName..nlotqty5
            REPLACE nlotqty6   WITH nlotqty6 + &lcCursName..nlotqty6
            REPLACE nlotqty7   WITH nlotqty7 + &lcCursName..nlotqty7
            REPLACE nlotqty8   WITH nlotqty8 + &lcCursName..nlotqty8
            REPLACE nlottotqty WITH nlottotqty + &lcCursName..nlottotqty

          CASE &lcCursName..trancd = '4'
           
            REPLACE can1   WITH can1 + &lcCursName..nlotqty1
            REPLACE can2   WITH can2 + &lcCursName..nlotqty2
            REPLACE can3   WITH can3 + &lcCursName..nlotqty3
            REPLACE can4   WITH can4 + &lcCursName..nlotqty4
            REPLACE can5   WITH can5 + &lcCursName..nlotqty5
            REPLACE can6   WITH can6 + &lcCursName..nlotqty6
            REPLACE can7   WITH can7 + &lcCursName..nlotqty7
            REPLACE can8   WITH can8 + &lcCursName..nlotqty8
            REPLACE totcan WITH totcan + &lcCursName..nlottotqty

          
          CASE &lcCursName..trancd $ '25'
           
            REPLACE rec1   WITH rec1 + &lcCursName..nlotqty1
            REPLACE rec2   WITH rec2 + &lcCursName..nlotqty2
            REPLACE rec3   WITH rec3 + &lcCursName..nlotqty3
            REPLACE rec4   WITH rec4 + &lcCursName..nlotqty4
            REPLACE rec5   WITH rec5 + &lcCursName..nlotqty5
            REPLACE rec6   WITH rec6 + &lcCursName..nlotqty6
            REPLACE rec7   WITH rec7 + &lcCursName..nlotqty7
            REPLACE rec8   WITH rec8 + &lcCursName..nlotqty8
            REPLACE totrec WITH totrec + &lcCursName..nlottotqty
          
          CASE &lcCursName..trancd = '3'
            REPLACE dam1   WITH dam1   + &lcCursName..nlotqty1
            REPLACE dam2   WITH dam2   + &lcCursName..nlotqty2
            REPLACE dam3   WITH dam3   + &lcCursName..nlotqty3
            REPLACE dam4   WITH dam4   + &lcCursName..nlotqty4
            REPLACE dam5   WITH dam5   + &lcCursName..nlotqty5
            REPLACE dam6   WITH dam6   + &lcCursName..nlotqty6
            REPLACE dam7   WITH dam7   + &lcCursName..nlotqty7
            REPLACE dam8   WITH dam8   + &lcCursName..nlotqty8
            REPLACE totdam WITH totdam + &lcCursName..nlottotqty
          
          ENDCASE 
        SELECT(lcCursName)
      ENDIF 
      ENDSCAN 
      IF lcRepForm = 'D'
        SELECT(lcCursName)
        COPY TO oAriaApplication.WorkDir + lcCurDet +".DBF" 
      ENDIF 

      USE IN (lcFilName)
    ELSE
      COPY TO oAriaApplication.WorkDir + lcFilName +".DBF" 
      USE oAriaApplication.WorkDir + lcFilName +".DBF"  IN 0 EXCLUSIVE 
      SELECT(lcFilName)
      ZAP 
      INDEX  on cOprCode+cContCode+cTktNo+cLotNo+LEFT(ITEM,lnMajSize)TO (lcFilName)
      SELECT(lcCursName)
      
      SCAN FOR flag 
        IF !SEEK(cOprCode+cContCode+cTktNo+cLotNo+LEFT(ITEM,lnMajSize),lcFilName )
          STORE 0 TO  m.nlotqty1 , m.nlotqty2, m.nlotqty3,  m.nlotqty4 ,m.nlotqty5 , m.nlotqty6 , m.nlotqty7 ,m.nlotqty8 , m.nlottotqty 
          STORE 0 TO  m.can1 , m.can2, m.can3,  m.can4 ,m.can5 , m.can6 , m.can7 ,m.can8 , m.totcan 
          STORE 0 TO  m.dam1 , m.dam2, m.dam3,  m.dam4 ,m.dam5 , m.dam6 , m.dam7 ,m.dam8 , m.totdam 
          STORE 0 TO  m.rec1 , m.rec2, m.rec3,  m.rec4 ,m.rec5 , m.rec6 , m.rec7 ,m.rec8 , m.totrec 

          SCATTER MEMO MEMVAR  field EXCEPT nlotqty1,nlotqty2,nlotqty3,nlotqty4,nlotqty5,nlotqty6,nlotqty7,nlotqty8,nlottotqty
          DO CASE 
        
          CASE m.trancd = '1'
            m.nlotqty1 = &lcCursName..nlotqty1
            m.nlotqty2 = &lcCursName..nlotqty2
            m.nlotqty3 = &lcCursName..nlotqty3
            m.nlotqty4 = &lcCursName..nlotqty4
            m.nlotqty5 = &lcCursName..nlotqty5
            m.nlotqty6 = &lcCursName..nlotqty6
            m.nlotqty7 = &lcCursName..nlotqty7
            m.nlotqty8 = &lcCursName..nlotqty8
            m.nlottotqty = &lcCursName..nlottotqty

          CASE m.trancd = '4'
            m.can1 = &lcCursName..nlotqty1
            m.can2 = &lcCursName..nlotqty2
            m.can3 = &lcCursName..nlotqty3
            m.can4 = &lcCursName..nlotqty4
            m.can5 = &lcCursName..nlotqty5
            m.can6 = &lcCursName..nlotqty6
            m.can7 = &lcCursName..nlotqty7
            m.can8 = &lcCursName..nlotqty8
            m.totcan = &lcCursName..nlottotqty
          
          CASE m.trancd $ '25'
            m.rec1 = &lcCursName..nlotqty1
            m.rec2 = &lcCursName..nlotqty2
            m.rec3 = &lcCursName..nlotqty3
            m.rec4 = &lcCursName..nlotqty4
            m.rec5 = &lcCursName..nlotqty5
            m.rec6 = &lcCursName..nlotqty6
            m.rec7 = &lcCursName..nlotqty7
            m.rec8 = &lcCursName..nlotqty8
            m.totrec = &lcCursName..nlottotqty
          
          CASE m.trancd = '3'
            m.dam1 = &lcCursName..nlotqty1
            m.dam2 = &lcCursName..nlotqty2
            m.dam3 = &lcCursName..nlotqty3
            m.dam4 = &lcCursName..nlotqty4
            m.dam5 = &lcCursName..nlotqty5
            m.dam6 = &lcCursName..nlotqty6
            m.dam7 = &lcCursName..nlotqty7
            m.dam8 = &lcCursName..nlotqty8
            m.totdam = &lcCursName..nlottotqty
          
          ENDCASE 
          INSERT INTO (lcFilName) FROM MEMVAR 
        ELSE
          SELECT(lcFilName)
          DO CASE 
            CASE &lcCursName..trancd = '1'
           
              REPLACE nlotqty1 WITH nlotqty1 + &lcCursName..nlotqty1
              REPLACE nlotqty2 WITH nlotqty2 + &lcCursName..nlotqty2
              REPLACE nlotqty3 WITH nlotqty3 + &lcCursName..nlotqty3
              REPLACE nlotqty4 WITH nlotqty4 + &lcCursName..nlotqty4
              REPLACE nlotqty5 WITH nlotqty5 + &lcCursName..nlotqty5
              REPLACE nlotqty6 WITH nlotqty6 + &lcCursName..nlotqty6
              REPLACE nlotqty7 WITH nlotqty7 + &lcCursName..nlotqty7
              REPLACE nlotqty8 WITH nlotqty8 + &lcCursName..nlotqty8
              REPLACE nlottotqty WITH nlottotqty + &lcCursName..nlottotqty

            CASE &lcCursName..trancd = '4'
           
              REPLACE can1 WITH can1 + &lcCursName..nlotqty1
              REPLACE can2 WITH can2 + &lcCursName..nlotqty2
              REPLACE can3 WITH can3 + &lcCursName..nlotqty3
              REPLACE can4 WITH can4 + &lcCursName..nlotqty4
              REPLACE can5 WITH can5 + &lcCursName..nlotqty5
              REPLACE can6 WITH can6 + &lcCursName..nlotqty6
              REPLACE can7 WITH can7 + &lcCursName..nlotqty7
              REPLACE can8 WITH can8 + &lcCursName..nlotqty8
              REPLACE totcan WITH totcan + &lcCursName..nlottotqty

          
            CASE &lcCursName..trancd $ '25'
            
              REPLACE rec1 WITH rec1 + &lcCursName..nlotqty1
              REPLACE rec2 WITH rec2 + &lcCursName..nlotqty2
              REPLACE rec3 WITH rec3 + &lcCursName..nlotqty3
              REPLACE rec4 WITH rec4 + &lcCursName..nlotqty4
              REPLACE rec5 WITH rec5 + &lcCursName..nlotqty5
              REPLACE rec6 WITH rec6 + &lcCursName..nlotqty6
              REPLACE rec7 WITH rec7 + &lcCursName..nlotqty7
              REPLACE rec8 WITH rec8 + &lcCursName..nlotqty8
              REPLACE totrec WITH totrec + &lcCursName..nlottotqty
          
            CASE &lcCursName..trancd = '3'
              REPLACE dam1 WITH dam1 + &lcCursName..nlotqty1
              REPLACE dam2 WITH dam2 + &lcCursName..nlotqty2
              REPLACE dam3 WITH dam3 + &lcCursName..nlotqty3
              REPLACE dam4 WITH dam4 + &lcCursName..nlotqty4
              REPLACE dam5 WITH dam5 + &lcCursName..nlotqty5
              REPLACE dam6 WITH dam6 + &lcCursName..nlotqty6
              REPLACE dam7 WITH dam7 + &lcCursName..nlotqty7
              REPLACE dam8 WITH dam8 + &lcCursName..nlotqty8
              REPLACE totdam WITH totdam + &lcCursName..nlottotqty
          ENDCASE 
          SELECT(lcCursName)
        ENDIF 
      ENDSCAN 
      IF lcRepForm = 'D'
        SELECT(lcCursName)
        COPY TO oAriaApplication.WorkDir + lcCurDet +".DBF" 
       ENDIF 
       USE IN (lcFilName)
     ENDIF   
     =lfAdjustCRSettings()
     IF RECCOUNT(lcCursName) > 0
       =gfDispRe()
       RETURN
     ENDIF 
   ENDIF  
 ELSE
  IF RECCOUNT(lcCursName) > 0
    =gfDispRe()
    RETURN
  ELSE
    =gfModalGen('TRM00052B40011','ALERT')
    RETURN .F.
  ENDIF 
ENDIF 

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


SELECT BOMCOST
=AFIELDS(laFileStrubom)
lnFileStru = ALEN(laFileStrubom,1)
=gfCrtTmp(lcCurBomCost,@laFileStrubom,'cTktNo + cOprCode+cLotNo ',lcCurBomCost)

SELECT MfgOprDt
=AFIELDS(laFileStruMfgOprDt)
lnFileStru = ALEN(laFileStruMfgOprDt,1)
DIMENSION laFileStruMfgOprDt[lnFileStru + 1,18]
lnFileStru = lnFileStru+1
laFileStruMfgOprDt[lnFileStru,1] = 'COperDesc'
laFileStruMfgOprDt[lnFileStru,2] = 'C'
laFileStruMfgOprDt[lnFileStru,3] = 30
laFileStruMfgOprDt[lnFileStru,4] = 0
STORE ' ' TO  laFileStruMfgOprDt[lnFileStru ,7],laFileStruMfgOprDt[lnFileStru,8],;
            laFileStruMfgOprDt[lnFileStru ,9],laFileStruMfgOprDt[lnFileStru ,10],;
            laFileStruMfgOprDt[lnFileStru ,11],laFileStruMfgOprDt[lnFileStru ,12],;
            laFileStruMfgOprDt[lnFileStru ,13],laFileStruMfgOprDt[lnFileStru ,14],;
            laFileStruMfgOprDt[lnFileStru ,15],laFileStruMfgOprDt[lnFileStru ,16]
STORE 0 TO    laFileStruMfgOprDt[lnFileStru ,17] ,laFileStruMfgOprDt[lnFileStru ,18]

=gfCrtTmp(lcCurMfgOprDt,@laFileStruMfgOprDt,'cTktNo + cOprCode+cLotNo ',lcCurMfgOprDt)



* Create the temporary file 
SELECT MFGOPRDT
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
lcOldFileStru  =lnFileStru 

DIMENSION laFileStru[lnFileStru+ 42,18]
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

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'COperDesc'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'UntCost'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 11
laFileStru[lnFileStru,4] = 3

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'ContName'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 11
laFileStru[lnFileStru,4] = 3

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ1'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ2'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ3'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ4'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ5'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ6'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ7'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'SZ8'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'StyDesc'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 20
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'StyMaj'
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 19
laFileStru[lnFileStru,4] = 0

*---------ADDING FIELDS TO HOLD THE DATA OF THE REC,DAM,CAN
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'rec1'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'rec2'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'rec3'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'rec4'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'rec5'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'rec6'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'rec7'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'rec8'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0


lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'TOTrec'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'DAM1'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'DAM2'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'DAM3'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'DAM4'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'DAM5'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'DAM6'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'DAM7'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'DAM8'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0


lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'TOTDAM'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CAN1'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CAN2'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CAN3'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CAN4'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CAN5'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CAN6'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CAN7'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CAN8'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 5
laFileStru[lnFileStru,4] = 0


lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'TOTCAN'
laFileStru[lnFileStru,2] = 'N'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

 FOR lnLoop = 1  TO  42
    STORE ' ' TO  laFileStru[lcOldFileStru +lnLoop,7],laFileStru[lcOldFileStru+lnLoop,8],;
                laFileStru[lcOldFileStru +lnLoop,9],laFileStru[lcOldFileStru+lnLoop,10],;
                laFileStru[lcOldFileStru+lnLoop,11],laFileStru[lcOldFileStru+lnLoop,12],;
                laFileStru[lcOldFileStru +lnLoop,13],laFileStru[lcOldFileStru+lnLoop,14],;
                laFileStru[lcOldFileStru +lnLoop,15],laFileStru[lcOldFileStru+lnLoop,16]
    STORE 0 TO    laFileStru[lcOldFileStru+lnLoop,17] ,laFileStru[lcOldFileStru+lnLoop,18]

 ENDFOR

=gfCrtTmp(lcCursName,@laFileStru,'cOprCode+cContCode+cTktNo+cLotNo+Item+dDate+TranCd ',lcCursName)


SELECT MFGOPRHD
DO CASE
  CASE lcImTyp = 'M' .OR. lcImTyp = 'T'
   
    =lfFilTmpCt()
    
  CASE lcImTyp = 'I'
 
    =lfFilTmpPo()
    
  CASE lcImTyp = 'T'
  
    SET RELATION TO cTktNo INTO (lcFile) ADDITIVE
ENDCASE


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
LOCATE 
DO WHILE !EOF()
  * Get Open quantity for this Operation/Lot.
  lcKey = cOprCode+cContCode+cTktNo+cLotNo
  STORE 0 TO lnOpen
  IF loDBFMFGOPRDT.SEEK(lcImTyp+cTktNo+cOprCode+cLotNo+'1')
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
SELECT DISTINCT  cTktNo , cOprCode,cLotNo FROM (lcCursName) INTO CURSOR 'TBOM'
* Display Issued Cost Items.
SELECT 'TBOM'
SCAN 
lcCutTkt = cTktNo 
lcOprCode = cOprCode
lcLotNo = cLotNo 
SELECT BOMCOST
IF loDBFBOMCOST.SEEK(lcImTyp+lcCutTkt)
  SCAN REST WHILE cTktNo = lcCutTkt ;
          FOR  cOprCode+cLotNo = lcOprCode+lcLotNo
          
*!*	    @ lnRow,01 SAY REPLICATE('-',120)
*!*	    @ lnRow+1,01 SAY 'Issued cost items :'
*!*	    @ lnRow+2,01 SAY REPLICATE('-',120)
* DATE       ITEM                COLOR  MFG    QUANTITY  UNIT COST  TOT. COST  UNT ACT CST  TOT ACT CST INVOICE No.'
* XX/XX/XXXX 1234567890123456789 XXXXXX XXXXXX 99999.99  99999.999  99999.999    99999.999   999999.999 XXXXXXXX
*B603348,1 ABD Remark The next Line & Move Them To The Left & Right to Indicate The Layout. [Begin]
*@ lnRow+3,01 SAY 'Date       Item                Color  MFG     Quantity Unit Cost  Tot. Cost  Unt Act Cst  Tot Act Cst Invoice No.'
*!*	    @ lnRow+3,01 SAY 'Date       Item                Color  MFG     Quantity   Unit Cost      Tot. Cost Unt Act Cst    Tot Act Cst Invoice No.'    
*B603348,1 ABD [ End ]

*!*	    @ lnRow+4,01 SAY REPLICATE('-',120)
*!*	    =lfIncRow(5)
  
*!*	      @ lnRow,01 SAY dTranDate
*!*	      @ lnRow,12 SAY Item
*!*	      @ lnRow,32 SAY IClr
*!*	      @ lnRow,39 SAY MfgCode
*!*	      @ lnRow,47 SAY nTotQty   PICTURE '99999.99'
      *B603348,1 ABD Remark the next lines & move them to the right & change the picture 
      *B603348,1     Instead of the fields. [Begin]
      *@ lnRow,56 SAY nUnitCst  PICTURE '99999.999'
      *@ lnRow,67 SAY nTotCst   PICTURE '99999.999'
      *@ lnRow,80 SAY nUnitACst PICTURE '99999.999'
      *@ lnRow,92 SAY nTotACst  PICTURE '999999.999'
      *@ lnRow,103 SAY cApInvNo
*!*	      @ lnRow,56 SAY nUnitCst  PICTURE '9999999.999'
*!*	      @ lnRow,68 SAY nTotCst   PICTURE '9999999999.999'
*!*	      @ lnRow,83 SAY nUnitACst PICTURE '9999999.999'
*!*	      @ lnRow,95 SAY nTotACst  PICTURE '9999999999.999'
*!*	      @ lnRow,110 SAY cApInvNo
*!*	      *B603348,1 ABD [ End ]
*!*	      =lfIncRow(1)
    SCATTER MEMO MEMVAR 
    INSERT into (lcCurBomCost) FROM MEMVAR 
  ENDSCAN
ENDIF
* Display cost items received from previous operations.
*lcCurMfgOprDt
SELECT MfgOprDt 
lodbfMfgOprDt.Setorder('trgopr')
*SET ORDER TO TAG TrgOpr
IF lodbfMfgOprDt.SEEK(lcImTyp+lcCutTkt+lcOprCode+lcLotNo+'2')
*!*	  @ lnRow,01 SAY REPLICATE('-',120)
*!*	  @ lnRow+1,01 SAY 'Receiving from previous operations :'
*!*	  @ lnRow+2,01 SAY REPLICATE('-',120)
*!*	  @ lnRow+3,01 SAY  'Date       Item                Quantity  From Opr.'
*!*	  *:                 XX/XX/XXXX 1234567890123456789    99999  XXXXXX
*!*	  @ lnRow+4,01 SAY REPLICATE('-',120)
*!*	  =lfIncRow(5)
  SCAN REST WHILE cTktNo+cTrgOpr+cTrgLot+TranCd = ;
                  lcCutTkt+lcOprCode+lcLotNo+'2'
    SCATTER MEMO MEMVAR      
    m.COperDesc = lfGetDesc(cOprCode)
   *     m.COperDesc = gfCodDes(cOprCode, 'MFGCODE')
    
    INSERT INTO (lcCurMfgOprDt) FROM MEMVAR 
*!*	    @ lnRow,01 SAY dTranDate
*!*	    @ lnRow,12 SAY Item
*!*	    @ lnRow,35 SAY nLotTotQty   PICTURE '99999'
*!*	    @ lnRow,42 SAY lcODesc
*!*	    =lfIncRow(1)
  ENDSCAN                
ENDIF
ENDSCAN 
SELECT(lcCurBomCost)
COPY TO oAriaApplication.WorkDir+lcCurBomC+".dbf"
SELECT(lcCurMfgOprDt)
COPY TO oAriaApplication.WorkDir+lcCurMfgOpr+".dbf"
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
  IF !SEEK(lcObj , 'APVENDOR')
    =gfApVnBrow(@lcObj)
    IF !EMPTY(lcObj)
      &lcVar = lcObj      && Update the field
    ELSE
      &lcVar = laOldVal
    ENDIF
  ENDIF
ELSE
  IF !SEEK(lcObj , 'APVENDOR')
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
  CASE oAriaApplication.ActiveModuleID ='PO'
    lcTit   = 'Purchase Order'
    lcImTyp = 'I'
  CASE oAriaApplication.ActiveModuleID  ='MA'
    lcTit   = 'MFG Order'
    lcImTyp = 'T'
  CASE oAriaApplication.ActiveModuleID  ='MF'
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

loDBFPOSHDR  = CreateObject("RemoteTable",'POSHDR','POSHDR','POSHDR',SET("DATASESSION"))
loDBFStyle  = CreateObject("RemoteTable","Style","Style",'Style',SET("DATASESSION"))
loDBFMFGOPRHD = CreateObject("RemoteTable","MFGOPRHD","MFGOPRHD",'MFGOPRHD',SET("DATASESSION"))
loDBFMFGOPRDT = CreateObject("RemoteTable","MFGOPRDT","MFGOPRDT",'MFGOPRDT',SET("DATASESSION"))
loDBFAPVENDOR = CreateObject("RemoteTable","APVENDOR","VENCODE",'APVENDOR',SET("DATASESSION"))
loDBFCTKTBOM = CreateObject("RemoteTable","CTKTBOM","CTKTYP",'CTKTBOM',SET("DATASESSION"))
loDBFSCALE = CreateObject("RemoteTable","SCALE","SCALE",'SCALE',SET("DATASESSION"))
loDBFBOMCOST = CreateObject("RemoteTable","BOMCOST","POBOMCLS",'BOMCOST',SET("DATASESSION"))
loDBFCodes = CreateObject("RemoteTable","Codes","Codes",'Codes',SET("DATASESSION"))&&,"",.T.)

DO CASE
  CASE GCACT_APPL='PO'
    lcFile  = 'POSHDR'
    lcField = 'PO'
  CASE GCACT_APPL='MA'
    lcFile  = 'MMFGORDH'
    lcField = 'cMFGOrdNo'
  CASE GCACT_APPL='MF'
    lcFile  = 'POSHDR'
    lcField = 'PO'
*!*	    lcFile  = 'CUTTKTH'
*!*	    lcField = 'CutTkt'
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

FUNCTION lfFilTmpCt
*-- To get the selected C/Ts if any.
lcCutFile = ''
llUseCut = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.PO'),1)
IF lnPosition > 0
  lcCutFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseCut = IIF(!EMPTY(lcCutFile) .AND. USED(lcCutFile) .AND. RECCOUNT(lcCutFile)>0,.T.,.F.)
ENDIF

*-- To get the selected style if any.
lcStylFile = ''
llUseStyle = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.STYLE'),1)
IF lnPosition > 0
  lcStylFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseStyle = IIF(!EMPTY(lcStylFile) .AND. USED(lcStylFile) .AND. RECCOUNT(lcStylFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Contractor if any.
lcContFile = ''
llUseContr = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'MFGOPRHD.CCONTCODE'),1)
IF lnPosition > 0
  lcContFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseContr = IIF(!EMPTY(lcContFile) .AND. USED(lcContFile) .AND. RECCOUNT(lcContFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Operation if any.
lcOperts = ''
llUseOpert = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'MFGOPRHD.COPRCODE'),1)
IF lnPosition > 0
  lcOperts = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  lcCoprCodeFile = loOGScroll.gfTempName()	
  llUseOpert = IIF(LEN(lcOperts)>0,.T.,.F.) AND lfConvertToCursor(lcOperts,'COPRCODE',lcCoprCodeFile )
ENDIF

*-- To get the selected Division if any.
lcDivs = ''
llUseDiv = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.CDIVISION'),1)
IF lnPosition > 0
  lcDivs = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  lcDivFile = loogscroll.gfTempName()
  llUseDiv = IIF(LEN(lcDivs)>0,.T.,.F.) and lfConvertToCursor(lcDivs,'CDIVISION',lcDivFile)
ENDIF

*-- To get the selected Season if any.
lcSeasons = ''
llUseSeas = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.SEASON'),1)
IF lnPosition > 0
  lcSeasons = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  lcSeaFile = loogscroll.gfTempName()
  llUseSeas = IIF(LEN(lcSeasons)>0,.T.,.F.) and lfConvertToCursor(lcSeasons,'SEASON',lcSeaFile)
ENDIF

*-- To get the Completion date.
lcCompDate = '.T.'
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.COMPLETE'),1)
IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6])
  lnSepPos = AT('|',LOOGSCROLL.laOGFxFlt[lnPosition,6])
  IF !EMPTY(CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],1,lnSepPos-1))) .OR.;
     !EMPTY(CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],lnSepPos+1)))
     lcCompDate = 'BETWEEN(POSHDR.COMPLETE,CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],1,lnSepPos-1)),CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],lnSepPos+1)))'
  ENDIF
ENDIF

IF lcStatus = 'L'
  lcStatus = ''
ENDIF

IF llUseCut
  SELECT MFGOPRDT
  lcOrdOprDt = ORDER()
  loDBFMFGOPRDT.SETORDER('MFGOPRDT')
  SELECT (lcCutFile)
  SCAN
    IF loDBFPOSHDR.SEEK('PU'+PO) .AND. IIF(llUseStyle,SEEK(POSHDR.STYLE,lcStylFile),.T.) .AND.;
       POSHDR.STATUS = lcStatus .AND. IIF(llUseDiv,SEEK(POSHDR.CDIVISION,lcDivFile),.T.) .AND.;
       IIF(llUseSeas,SEEK(POSHDR.SEASON,lcSeaFile),.T.) .AND. &lcCompDate. .AND. loDBFMFGOPRDT.SEEK('M'+PO,'MFGOPRDT')
      SELECT MFGOPRDT
      SCAN REST WHILE cimtyp + ctktno + coprcode + clotno + trancd = 'M'+POSHDR.PO;
                  FOR IIF(llUseContr,SEEK(CCONTCODE,lcContFile),.T.) .AND.;
                      IIF(llUseOpert,SEEK(COPRCODE,lcCoprCodeFile),.T.) .AND. TranCd #'0'
        SCATTER MEMVAR MEMO
        m.dDate = DTOS(MfgOprDt.dTranDate)
        m.COperDesc = lfGetDesc(cOprCode)
       * m.COperDesc = gfCodDes(COPRCODE, 'MFGCODE')
        m.UntCost = IIF(loDBFCTKTBOM.SEEK(cimtyp+ctktno+SPACE(23)+cOprCode),CTKTBOM.UntCost,0)
        
        m.ContCode = IIF(loDBFAPVENDOR.SEEK(cContCode),APVENDOR.cVenComp,"")
        IF loDBFStyle.Seek(m.Item) AND loDBFSCALE.Seek('S'+STYLE.SCALE)
          m.Sz1 = scale.sz1
          m.Sz2 = scale.sz2
          m.Sz3 = scale.sz3
          m.Sz4 = scale.sz4
          m.Sz5 = scale.sz5
          m.Sz6 = scale.sz6
          m.Sz7 = scale.sz7
          m.Sz8 = scale.sz8
          m.StyDesc = Style.Desc
        ENDIF  
        m.Flag  = .F.
        m.StyMaj = LEFT(Item,lnMajSize)
        INSERT INTO (lcCursName) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDSCAN
  loDBFMFGOPRDT.SETORDER(lcOrdOprDt)
ELSE
  IF llUseStyle

    SELECT POSHDR
    SELECT MFGOPRDT
    lcOrdOprDt = ORDER()
    loDBFMFGOPRDT.SETORDER('MFGOPRDT')
    SELECT (lcStylFile)
    SCAN
    
      IF loDBFPOSHDR.Sqlrun("Select * FROM POSHDR where Style = '" + &lcStylFile..cStyMajor + "' And STATUS =' " + lcStatus +"'",'POSHDR')
        SELECT POSHDR
        SCAN FOR IIF(llUseDiv,CDIVISION$lcDivs,.T.) .AND.;
                        IIF(llUseSeas,SEEK(POSHDR.SEASON,lcSeaFile),.T.) .AND. &lcCompDate. .AND.;
                        loDBFMFGOPRDT.SEEK('M'+PO)
          SELECT MFGOPRDT
          SCAN REST WHILE cImTyp+cTktNo+cOprCode+cLotNo+TranCd = 'M'+POSHDR.PO;
                      FOR IIF(llUseContr,SEEK(CCONTCODE,lcContFile),.T.) .AND.;
                          IIF(llUseOpert,SEEK(COPRCODE,lcCoprCodeFile),.T.) .AND. TranCd #'0'
            SCATTER MEMVAR MEMO
            m.dDate = DTOS(MfgOprDt.dTranDate)
            m.COperDesc = lfGetDesc(cOprCode)
           * m.COperDesc = gfCodDes(COPRCODE, 'MFGCODE')
            m.UntCost = IIF(loDBFCTKTBOM.SEEK(cimtyp+ctktno+SPACE(23)+cOprCode ),CTKTBOM.UntCost,0)
            m.ContCode = IIF(loDBFAPVENDOR.SEEK(cContCode),APVENDOR.cVenComp,"")
            m.Flag  = .F.
            IF loDBFStyle.Seek(m.Item) AND loDBFSCALE.Seek('S'+STYLE.SCALE)
	           m.Sz1 = scale.sz1
	           m.Sz2 = scale.sz2
	           m.Sz3 = scale.sz3
	           m.Sz4 = scale.sz4
	           m.Sz5 = scale.sz5
	           m.Sz6 = scale.sz6
	           m.Sz7 = scale.sz7
	           m.Sz8 = scale.sz8
	           m.StyDesc = Style.Desc
	        ENDIF  
			m.StyMaj = LEFT(Item,lnMajSize)
            INSERT INTO (lcCursName) FROM MEMVAR
          ENDSCAN
        ENDSCAN
      ENDIF
    ENDSCAN
    loDBFMFGOPRDT.SETORDER(lcOrdOprDt)
  ELSE
    IF llUseContr
      SELECT MFGOPRDT
      lcOrdOprDt = ORDER()
      loDBFMFGOPRDT.SETORDER('mfgcont')
      SELECT (lcContFile)
      SCAN
        IF loDBFMFGOPRDT.SEEK('M'+ CVENDCODE)
          SELECT MFGOPRDT
          SCAN REST WHILE cImTyp+cContCode+cTktNo+ cOprCode = 'M'+EVALUAT(lcContFile+'.CVENDCODE');
                      FOR IIF(llUseOpert,SEEK(COPRCODE,lcCoprCodeFile),.T.) .AND. TranCd #'0' .AND.;
                          loDBFPOSHDR.SEEK('PU'+ CTKTNO) .AND. POSHDR.STATUS = lcStatus .AND.;
                          IIF(llUseDiv,SEEK(POSHDR.CDIVISION,lcDivFile),.T.) .AND.;
                          IIF(llUseSeas,SEEK(POSHDR.SEASON,lcSeaFile),.T.) .AND. &lcCompDate.
            SCATTER MEMVAR MEMO
            m.dDate = DTOS(MfgOprDt.dTranDate)
            m.Flag  = .F.
                m.COperDesc = lfGetDesc(cOprCode)
*            m.COperDesc = gfCodDes(COPRCODE, 'MFGCODE')
            m.UntCost = IIF(loDBFCTKTBOM.SEEK(cimtyp+ctktno+SPACE(23)+cOprCode ),CTKTBOM.UntCost,0)
            m.ContCode = IIF(loDBFAPVENDOR.SEEK(cContCode),APVENDOR.cVenComp,"")
            IF loDBFStyle.Seek(m.Item) AND loDBFSCALE.Seek('S'+STYLE.SCALE)
              m.Sz1 = scale.sz1
              m.Sz2 = scale.sz2
         	  m.Sz3 = scale.sz3
	          m.Sz4 = scale.sz4
    	      m.Sz5 = scale.sz5
        	  m.Sz6 = scale.sz6
          	  m.Sz7 = scale.sz7
          	  m.Sz8 = scale.sz8
          	  m.StyDesc = Style.Desc
            ENDIF  
			m.StyMaj = LEFT(Item,lnMajSize)
            INSERT INTO (lcCursName) FROM MEMVAR
          ENDSCAN
        ENDIF
      ENDSCAN
      loDBFMFGOPRDT.SETORDER(lcOrdOprDt)
    ELSE
      SELECT MFGOPRDT
      lcOrdOprDt = ORDER()
      loDBFMFGOPRDT.SETORDER('MFGOPRDT')
      SELECT MFGOPRHD
      lcOrdOprHd = ORDER()
      loDBFMFGOPRHD.SETORDER('MFGOPRHD')
      =loDBFMFGOPRHD.SEEK('M')
      SCAN REST WHILE cImTyp + cTktNo+cOprCode = 'M' FOR IIF(llUseOpert,SEEK(COPRCODE,lcCoprCodeFile),.T.)
        IF loDBFMFGOPRDT.SEEK('M'+CTKTNO+COPRCODE)
          SELECT MFGOPRDT
          SCAN REST WHILE cImTyp+cTktNo+cOprCode+cLotNo+TranCd = 'M'+MFGOPRHD.CTKTNO+MFGOPRHD.COPRCODE;
                      FOR TranCd #'0' .AND. loDBFPOSHDR.SEEK('PU'+ CTKTNO,'POSHDR') .AND.;
                          POSHDR.STATUS = lcStatus .AND. IIF(llUseDiv,SEEK(POSHDR.CDIVISION,lcDivFile),.T.) .AND.;
                          IIF(llUseSeas,SEEK(POSHDR.SEASON,lcSeaFile),.T.) .AND. &lcCompDate. 
            SCATTER MEMVAR MEMO
            m.dDate = DTOS(MfgOprDt.dTranDate)
            m.Flag  = .F.
            m.COperDesc = lfGetDesc(cOprCode)
            *m.COperDesc = gfCodDes(COPRCODE, 'MFGCODE')
	        m.UntCost = IIF(loDBFCTKTBOM.SEEK(cimtyp+ctktno+SPACE(23)+cOprCode),CTKTBOM.UntCost,0)
	        m.ContCode = IIF(loDBFAPVENDOR.SEEK(cContCode),APVENDOR.cVenComp,"")
            IF loDBFStyle.Seek(m.Item) AND loDBFSCALE.Seek('S'+STYLE.SCALE)
 		      m.Sz1 = scale.sz1
        	  m.Sz2 = scale.sz2
	          m.Sz3 = scale.sz3
    	      m.Sz4 = scale.sz4
        	  m.Sz5 = scale.sz5
	          m.Sz6 = scale.sz6
    	      m.Sz7 = scale.sz7
        	  m.Sz8 = scale.sz8
        	  m.StyDesc = Style.Desc
	        ENDIF  
	        m.StyMaj = LEFT(Item,lnMajSize)
            INSERT INTO (lcCursName) FROM MEMVAR
          ENDSCAN
        ENDIF
      ENDSCAN
      loDBFMFGOPRDT.SETORDER(lcOrdOprDt)
      loDBFMFGOPRHD.SETORDER(lcOrdOprHd)
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
FUNCTION lfFilTmpPo

*-- To get the selected POs if any.
lcPOFile = ''
llUsePO = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,"IIF(.T.,MFGOPRHD.CTKTNO,'')"),1)
IF lnPosition > 0
  lcPOFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUsePO = IIF(!EMPTY(lcPOFile) .AND. USED(lcPOFile) .AND. RECCOUNT(lcPOFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Contractor if any.
lcContFile = ''
llUseContr = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'MFGOPRHD.CCONTCODE'),1)
IF lnPosition > 0
  lcContFile = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  llUseContr = IIF(!EMPTY(lcContFile) .AND. USED(lcContFile) .AND. RECCOUNT(lcContFile)>0,.T.,.F.)
ENDIF

*-- To get the selected Operation if any.
lcOperts = ''
llUseOpert = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'MFGOPRHD.COPRCODE'),1)
IF lnPosition > 0
  lcOperts = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  lcCoprCodeFile = loogScroll.gfTempName()
  llUseOpert = IIF(LEN(lcOperts)>0,.T.,.F.) AND lfConvertToCursor(lcOperts,'COPRCODE',lcCoprCodeFile )
ENDIF

*-- To get the selected Division if any.
lcDivs = ''
llUseDiv = .F.
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.CDIVISION'),1)
IF lnPosition > 0
  lcDivs = LOOGSCROLL.laOGFxFlt[lnPosition,6]
  lcDivFile = loogscroll.gfTempName()
  llUseDiv = IIF(LEN(lcDivs)>0,.T.,.F.) and lfConvertToCursor(lcDivs,'CDIVISION',lcDivFile)
ENDIF

*-- To get the Completion date.
lcCompDate = '.T.'
lnPosition = ASUBSCRIPT(LOOGSCROLL.laOGFxFlt,ASCAN(LOOGSCROLL.laOGFxFlt,'POSHDR.COMPLETE'),1)
IF lnPosition > 0 .AND. !EMPTY(LOOGSCROLL.laOGFxFlt[lnPosition,6])
  lnSepPos = AT('|',LOOGSCROLL.laOGFxFlt[lnPosition,6])
  IF !EMPTY(CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],1,lnSepPos-1))) .OR.;
     !EMPTY(CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],lnSepPos+1)))
*    lcCompDate = 'BETWEEN(POSHDR.COMPLETE,{'+STRTRAN(LOOGSCROLL.laOGFxFlt[lnPosition,6],'|','},{')+'})'
    lcCompDate = 'BETWEEN(POSHDR.COMPLETE,CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],1,lnSepPos-1)),CTOD(SUBSTR(LOOGSCROLL.laOGFxFlt[lnPosition,6],lnSepPos+1)))'
  ENDIF
ENDIF

IF lcStatus = 'L'
  lcStatus = ''
ENDIF

IF llUsePO

  SELECT MFGOPRDT
  lcOrdOprDt = ORDER()
  loDBFMFGOPRDT.SETORDER('MFGOPRDT')
  SELECT (lcPOFile)
  SCAN
    IF loDBFPOSHDR.SEEK('PP'+PO) .AND. POSHDR.STATUS = lcStatus .AND.;
       IIF(llUseDiv,SEEK(POSHDR.CDIVISION,lcDivFile),.T.) .AND.;
       &lcCompDate. .AND. loDBFMFGOPRDT.SEEK('I'+POSHDR.PO)
      SELECT MFGOPRDT
      SCAN REST WHILE cImTyp+cTktNo+cOprCode+cLotNo+TranCd = 'I' + POSHDR.PO;
                  FOR IIF(llUseContr,SEEK(CCONTCODE,lcContFile),.T.) .AND.;
                      IIF(llUseOpert,SEEK(COPRCODE,lcCoprCodeFile),.T.) .AND. TranCd #'0'
        SCATTER MEMVAR MEMO
        m.dDate = DTOS(MfgOprDt.dTranDate)
        m.Flag  = .F.
        m.COperDesc = lfGetDesc(cOprCode)
*        m.COperDesc = gfCodDes(COPRCODE, 'MFGCODE')
        m.UntCost = IIF(loDBFCTKTBOM.SEEK(cimtyp+ctktno+SPACE(23)+cOprCode ),CTKTBOM.UntCost,0)
        m.ContCode = IIF(loDBFAPVENDOR.SEEK(cContCode),APVENDOR.cVenComp,"")
        IF loDBFStyle.Seek(m.Item) AND loDBFSCALE.Seek('S'+STYLE.SCALE)
          m.Sz1 = scale.sz1
          m.Sz2 = scale.sz2
          m.Sz3 = scale.sz3
          m.Sz4 = scale.sz4
          m.Sz5 = scale.sz5
          m.Sz6 = scale.sz6
          m.Sz7 = scale.sz7
          m.Sz8 = scale.sz8
          m.StyDesc = Style.Desc
        ENDIF  
		m.StyMaj = LEFT(Item,lnMajSize)
        INSERT INTO (lcCursName) FROM MEMVAR
      ENDSCAN
    ENDIF
  ENDSCAN
  loDBFMFGOPRDT.SETORDER(lcOrdOprDt)
ELSE
  IF llUseContr
    SELECT POSHDR
    lcOrdPO = ORDER()
    loDBFPOSHDR.SETORDER('POSHDR')
    SELECT MFGOPRDT
    lcOrdOprDt = ORDER()
    loDBFMFGOPRDT.SETORDER('MFGCONT')
    SELECT (lcContFile)
    SCAN
      IF loDBFMFGOPRDT.SEEK('I'+CVENDCODE)
        SELECT MFGOPRDT
        SCAN REST WHILE cImTyp+cContCode+cTktNo+cOprCode = 'I'+EVALUAT(lcContFile+'.CVENDCODE');
                    FOR IIF(llUseOpert,SEEK(COPRCODE,lcCoprCodeFile),.T.) .AND. TranCd #'0' .AND.;
                        loDBFPOSHDR.SEEK('PP+CTKTNO) .AND. POSHDR.STATUS = lcStatus .AND.;
                        IIF(llUseDiv,SEEK(POSHDR.CDIVISION,lcDivFile),.T.) .AND. &lcCompDate.
          SCATTER MEMVAR MEMO
          m.dDate = DTOS(MfgOprDt.dTranDate)
          m.COperDesc = lfGetDesc(cOprCode)
*          m.COperDesc = gfCodDes(COPRCODE, 'MFGCODE')
	      m.UntCost = IIF(loDBFCTKTBOM.SEEK(cimtyp+ctktno+SPACE(23)+cOprCode ),CTKTBOM.UntCost,0)
	      m.ContCode = IIF(loDBFAPVENDOR.SEEK(cContCode),APVENDOR.cVenComp,"")
          m.Flag  = .F.
          IF loDBFStyle.Seek(m.Item) AND loDBFSCALE.Seek('S'+STYLE.SCALE)
 	        m.Sz1 = scale.sz1
    	    m.Sz2 = scale.sz2
        	m.Sz3 = scale.sz3
	        m.Sz4 = scale.sz4
    	    m.Sz5 = scale.sz5
        	m.Sz6 = scale.sz6
	        m.Sz7 = scale.sz7
    	    m.Sz8 = scale.sz8
    	    m.StyDesc = Style.Desc
	      ENDIF  
	      m.StyMaj = LEFT(Item,lnMajSize)
          INSERT INTO (lcCursName) FROM MEMVAR
        ENDSCAN
      ENDIF
    ENDSCAN
    loDBFPOSHDR.SETORDER(lcOrdPO)
    loDBFMFGOPRDT.SETORDER(lcOrdOprDt)
    
  ELSE
  
    SELECT POSHDR
    lcOrdPO = ORDER()
    loDBFPOSHDR.SETORDER('POSHDR')
    SELECT MFGOPRDT
    lcOrdOprDt = ORDER()
	loDBFMFGOPRDT.SETORDER('MFGOPRDT')
    SELECT MFGOPRHD
    lcOrdOprHd = ORDER()
    loDBFMFGOPRHD.SETORDER('MFGOPRHD')
    =loDBFMFGOPRHD.SEEK('I')
    SCAN REST WHILE cImTyp+cTktNo+cOprCode = 'I' FOR IIF(llUseOpert,SEEK(COPRCODE,lcCoprCodeFile),.T.)
      IF loDBFMFGOPRDT.SEEK('I'+CTKTNO+COPRCODE)
        SELECT MFGOPRDT
        SCAN REST WHILE cImTyp+cTktNo+cOprCode+cLotNo+TranCd = 'I'+MFGOPRHD.CTKTNO+MFGOPRHD.COPRCODE;
                    FOR TranCd #'0' .AND. loDBFPOSHDR.SEEK('PP'+CTKTNO) .AND.;
                        POSHDR.STATUS=lcStatus .AND. IIF(llUseDiv,SEEK(POSHDR.CDIVISION,lcDivFile),.T.) .AND.;
                        &lcCompDate. 
          SCATTER MEMVAR MEMO
          m.dDate = DTOS(MfgOprDt.dTranDate)
              m.COperDesc = lfGetDesc(cOprCode)
*          m.COperDesc = gfCodDes(COPRCODE, 'MFGCODE')
	        m.UntCost = IIF(loDBFCTKTBOM.SEEK(cimtyp+ctktno+SPACE(23)+cOprCode ),CTKTBOM.UntCost,0)
	        m.ContCode = IIF(loDBFAPVENDOR.SEEK(cContCode),APVENDOR.cVenComp,"")
          m.Flag  = .F.
          IF loDBFStyle.Seek(m.Item) AND loDBFSCALE.Seek('S'+STYLE.SCALE)
            m.Sz1 = scale.sz1
            m.Sz2 = scale.sz2
            m.Sz3 = scale.sz3
            m.Sz4 = scale.sz4
            m.Sz5 = scale.sz5
            m.Sz6 = scale.sz6
            m.Sz7 = scale.sz7
            m.Sz8 = scale.sz8
            m.StyDesc = Style.Desc
          ENDIF  
          m.StyMaj = LEFT(Item,lnMajSize)
          INSERT INTO (lcCursName) FROM MEMVAR
        ENDSCAN
      ENDIF
    ENDSCAN
    loDBFPOSHDR.SETORDER(lcOrdPO)
  	loDBFMFGOPRDT.SETORDER(lcOrdOprDt)
    loDBFMFGOPRHD.SETORDER(lcOrdOprHd)
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfConvertToCursor
*: Developer : MAriam Mazhar (MMT)
*: Date      : 09/14/2005
*! Purpose   : Convert a list of values into a cusrsor
*!*************************************************************
*!
FUNCTION lfConvertToCursor
PARAMETERS lcStrToConv,lcFieldName ,lcNewFile
lcCursorTemp = lcNewFile &&Cursor Hold Selected values
DIMENSION laTempacstru[1,4]
laTempacstru[1,1] = lcFieldName 

DO CASE 
  
CASE   ALLTRIM(lcFieldName) = 'SEASON'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

CASE   ALLTRIM(lcFieldName) = 'CDIVISION'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0
CASE   ALLTRIM(lcFieldName) = 'COPRCODE'
  laTempacstru[1,2]='C'
  laTempacstru[1,3]= 6 
  laTempacstru[1,4]= 0

ENDCASE 
 = gfCrtTmp(lcCursorTemp ,@laTempacstru,lcFieldName ,lcCursorTemp ,.T.)
lcValuesToConvert = lcStrToConv
IF !EMPTY(lcValuesToConvert)
  lnStart=1 
  lnEnd=AT('|',lcValuesToConvert )
  DO WHILE lnEnd <> 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH SUBSTR(lcValuesToConvert,lnStart,lnEnd-1)
    lcValuesToConvert = STUFF(lcValuesToConvert ,lnStart,lnEnd,"") 
    lnEnd=AT('|',lcValuesToConvert )
  ENDDO 
  IF lnEnd = 0
    SELECT(lcCursorTemp ) 
    APPEND BLANK 
    REPLACE &lcFieldName  WITH lcValuesToConvert 
  ENDIF 
ENDIF 
RETURN .T.
*************************************************************
*! Name      : lfAdjustCRSettings
*! Developer : Saeed Mohammed (SMM)
*! Date      : 08/30/2004
*! Purpose   : To set the report data files and parameters
*!*************************************************************
FUNCTION lfAdjustCRSettings

DIMENSION loOgScroll.laCRParams[9,2]

loOgScroll.laCRParams[9,1] = 'rep_Module'

DO CASE
  CASE oAriaApplication.ActiveModuleID ='PO'
    loOgScroll.laCRParams[9,2]  = 'Purchase Order#:'
  CASE oAriaApplication.ActiveModuleID  ='MA'
    loOgScroll.laCRParams[9,2]  = 'MFG Order#:'
  CASE oAriaApplication.ActiveModuleID  ='MF'
    loOgScroll.laCRParams[9,2] = 'Cutting Ticket# :' 
ENDCASE



loOgScroll.laCRParams[1,1] = 'Layout'

IF lcRepForm = 'S'
  lcFormat = 'MFOPTRS'
  loOgScroll.laCRParams[1,2] = 'Summary'
  loOgScroll.lcOGLastForm = lcFormat   
ELSE
  lcFormat = 'MFOPTRD'
  loOgScroll.lcOGLastForm = lcFormat   
  loOgScroll.laCRParams[1,2] = 'Detail'
ENDIF


loOgScroll.laCRParams[2,1] = 'ReportName'
loOgScroll.laCRParams[2,2] = 'Contractor Tracking Report'

loOgScroll.laCRParams[3,1] = 'lcStyTitle'
loOgScroll.laCRParams[3,2] = lcStyTitle

loOgScroll.laCRParams[4,1] = 'lnMajSize '
loOgScroll.laCRParams[4,2] = lnMajSize 

loOgScroll.laCRParams[5,1] = 'lDispCost'
IF llDispCost
  loOgScroll.laCRParams[5,2] = 1
ELSE
  loOgScroll.laCRParams[5,2] = 0
ENDIF 

loOgScroll.laCRParams[6,1] = 'op_title'
loOgScroll.laCRParams[6,2] = xTitle

loOgScroll.laCRParams[7,1] = 'llUseDyes'
loOgScroll.laCRParams[7,2] = IIF(llUseDyes,1,0)

loOgScroll.laCRParams[8,1] = 'lluse_config'
loOgScroll.laCRParams[8,2] = IIF(lluse_config,1,0)

LOCAL lcReportFileName 

lcReportFileName = loOgScroll.lcOGLastForm 

IF FILE(oAriaApplication.ReportHome  + lcReportFileName  + '.RPT')
  lcReportFileName = oAriaApplication.ReportHome + lcReportFileName + '.RPT'
ENDIF   


IF lcRepForm = 'S'
  DIMENSION LOogsCROLL.laCRTables[1]
  
  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir + lcFilName + ".DBF"

  loMainCr = CREATEOBJECT('CrystalRuntime.Application') 
  loMain = CREATEOBJECT('CrystalRuntime.Report') 
  loMain = loMainCr.OpenReport(lcReportFileName)
  loMain.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcFilName + ".DBF")
  loMain.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcFilName + ".DBF",'','')
  loMain.DiscardSavedData()
  loMain.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
  loMain.CaseInsensitiveSQLData = .T.
  loMain.OpenSubreport ('SubCostItm')
  loSub1 = loMain.OpenSubreport ('SubCostItm')
  loSub1.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcCurBomC + ".DBF")  
  loSub1.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcCurBomC + ".DBF",'','')
  loSub1.Database.Verify() && verify database 
  loSub1.DiscardSavedData()
  loSub1.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
  loSub1.CaseInsensitiveSQLData = .T.
  loSub2 = loMain.OpenSubreport ('SubRec')
  loSub2.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcCurMfgOpr + ".DBF")
  loSub2.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcCurMfgOpr  + ".DBF",'','')
  loSub2.Database.Verify() && verify database 
  loSub2.DiscardSavedData()
  loSub2.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
  loSub2.CaseInsensitiveSQLData = .T.
  loMain.Save (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
  COPY FILE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt') TO (lcReportFileName)
  ERASE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
  loMainCr  = NULL
  loMain    = NULL
  loSub2    = NULL 
  loSub1    = NULL 
*!*    loOgScroll.laCRTables[2] = oAriaApplication.WorkDir + lcCurBomC + ".DBF"
*!*    loOgScroll.laCRTables[3] = oAriaApplication.WorkDir + lcCurMfgOpr + ".DBF"
ELSE
  DIMENSION LOogsCROLL.laCRTables[4]

  loOgScroll.laCRTables[1] = oAriaApplication.WorkDir +  lcFilName + ".DBF"
  
  loMainCr = CREATEOBJECT('CrystalRuntime.Application') 
  loMain = CREATEOBJECT('CrystalRuntime.Report') 
  loMain = loMainCr.OpenReport(lcReportFileName)
  loMain.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcFilName + ".DBF")
  loMain.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcFilName + ".DBF",'','')
  loMain.DiscardSavedData()
  loMain.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
  loMain.CaseInsensitiveSQLData = .T.
  loMain.OpenSubreport ('SubCostItm')
  loSub1 = loMain.OpenSubreport ('SubCostItm')
  loSub1.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcCurBomC + ".DBF")  
  loSub1.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcCurBomC + ".DBF",'','')
  loSub1.Database.Verify() && verify database 
  loSub1.DiscardSavedData()
  loSub1.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
  loSub1.CaseInsensitiveSQLData = .T.
  loSub2 = loMain.OpenSubreport ('SubRec')
  loSub2.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcCurMfgOpr + ".DBF")
  loSub2.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcCurMfgOpr  + ".DBF",'','')
  loSub2.Database.Verify() && verify database 
  loSub2.DiscardSavedData()
  loSub2.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
  loSub2.CaseInsensitiveSQLData = .T.
  loSub3 = loMain.OpenSubreport ('ee')
  loSub3.Database.Tables.Item[1].Setlogoninfo ( oAriaApplication.WorkDir + lcCurDet + ".DBF")
  loSub3.Database.Tables.Item[1].SetTableLocation ( oAriaApplication.WorkDir + lcCurDet  + ".DBF",'','')
  loSub3.Database.Verify() && verify database 
  loSub3.DiscardSavedData()
  loSub3.ConvertDateTimeType = 1  && crConvertDateTimeToDate 
  loSub3.CaseInsensitiveSQLData = .T.
  loMain.Save (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
  COPY FILE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt') TO (lcReportFileName)
  ERASE (oAriaApplication.WorkDir+lcTempCrFile+'.rpt')
  loMainCr  = NULL
  loMain    = NULL
  loSub2    = NULL 
  loSub1    = NULL 
  loSub3    = NULL 

*!*    loOgScroll.laCRTables[2] = oAriaApplication.WorkDir + lcCurBomC + ".DBF"
*!*    loOgScroll.laCRTables[3] = oAriaApplication.WorkDir + lcCurMfgOpr + ".DBF"
*!*    loOgScroll.laCRTables[4] = oAriaApplication.WorkDir + lcCurDet + ".DBF"
ENDIF   
*!*************************************************************
*! Name      : lfGetDesc
*! Developer : Mohamed Badran (MAB)
*! Date      : 06/04/99
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
FUNCTION lfGetDesc
PARAMETERS lcMFCode
lcMfName = ""
loDBFCodes.Setorder('Codes')
IF loDBFCodes.SEEK('D'+lcMFCode+'N'+'MFGCODE')
  lcMfName = codes.cdiscrep
ENDIF
RETURN lcMfName
  