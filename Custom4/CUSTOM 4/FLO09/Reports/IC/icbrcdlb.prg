*!*************************************************************
*! Name      : ICBRCDLB.PRG
*! Developer : Hesham Elmasry (HES)
*! Date      : 12/01/2009
*! Tracking  : 
*!*************************************************************
*! Description : Develop Bar-coding System for FBZ for [T20090421.0032]
*! Entity #    : 3
*! Calls       : 
*!*************************************************************
*! Example   : Do ICBRCDLB.PRG
*!*************************************************************
IF !lfColData()
  RETURN 
ENDIF

SELECT(lcRepTmp)
=gfDispRe(lcRpForm,.F.,.F.,'L',.T.)

*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Hesham (HES)
*! Date      : 01/12/2010
*! Purpose   : When Function for Option Grid
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : ....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwRepWhen()
*!*************************************************************
FUNCTION lfwRepWhen

IF !USED('POSHDR')
  = gfOpenTable(oAriaApplication.DataDir + 'POSHDR' ,'POSHDR','SH')
  SELECT POSHDR
  WAIT WINDOW NOWAIT 'Preparing Data ...'  
  gfSeek('')
ENDIF 

IF !USED('POSLN')
  = gfOpenTable(oAriaApplication.DataDir + 'POSLN' ,'POSLN','SH')
ENDIF 

IF !USED('ORDHDR')
  =gfOpenTable(oariaapplication.datadir +'ORDHDR',oariaapplication.datadir +'ORDHDR','SH')
ENDIF 

IF !USED('ORDLINE')
  =gfOpenTable(oariaapplication.datadir +'ORDLINE',oariaapplication.datadir +'ORDLINE','SH')
ENDIF 

IF !USED('STYLE')
  =gfOpenTable(oariaapplication.datadir +'STYLE',oariaapplication.datadir +'STYLE','SH')
ENDIF 

IF !USED('STYDYE')
  =gfOpenTable(oariaapplication.datadir +'STYDYE',oariaapplication.datadir +'STYDYE','SH')
ENDIF 

IF !USED('SCALE')
  =gfOpenTable(oariaapplication.datadir +'SCALE',oariaapplication.datadir +'SCALE','SH')
ENDIF 

IF !USED('CONFGUPC')
  =gfOpenTable(oariaapplication.datadir +'CONFGUPC',oariaapplication.datadir +'CONFGUPC','SH')
ENDIF 

IF !USED('PROFVALU')
  =gfOpenTable(oariaapplication.datadir +'Profvalu',oariaapplication.datadir +'PROFILE','SH')
ENDIF 

IF !USED('CODES')
  =gfOpenTable(oariaapplication.datadir +'CODES',oariaapplication.datadir +'CODES','SH')
ENDIF 
* End of lfwRepWhen

*!**************************************************************
*! Name      : lfvPerc
*! Developer : Hesham (HES)
*! Date      : 01/12/2010
*! Purpose   : Valid the Percentage to Process % in Option Grdid
*!**************************************************************
*! Called from : Option Grid
*!**************************************************************
*! Calls       : ....
*!**************************************************************
*! Passed Parameters : None
*!**************************************************************
*! Return      : None
*!**************************************************************
*! Example     : = lfvPerc()
*!**************************************************************
FUNCTION lfvPerc

IF lnRpPerc > 100 OR lnRpPerc < 0.01
  lnOVal = _screen.ActiveForm.ActiveControl.OldValue 
  MESSAGEBOX('Please Enter a value between 1 - 100',528,'Wrong Input !')
  lnRpPerc = lnOVal
ENDIF 
* End of lfvPerc

*!**************************************************************
*! Name      : lfColData
*! Developer : Hesham (HES)
*! Date      : 01/12/2010
*! Purpose   : Collecting Data
*!**************************************************************
*! Called from : PRG
*!**************************************************************
*! Calls       : ....
*!**************************************************************
*! Passed Parameters : None
*!**************************************************************
*! Return      : None
*!**************************************************************
*! Example     : = lfColdata()
*!**************************************************************
FUNCTION lfColData

IF llOgFltCh
  lfCrtTemp()
ENDIF

PRIVATE lnCnt = 0

IF EMPTY(lnRpPerc)
  lnRpPerc = 100
ENDIF 

lcPoTmp   = lfCheckFilter(1,"POSHDR.PO")
lcSoTmp   = lfCheckFilter(1,"ORDHDR.ORDER")
lcStTmp   = lfCheckFilter(1,"STYLE.CSTYMAJOR")
lnRepPerc = lnRpPerc
llHasDate = .F.

lcRange = lfCheckFilter(1,"TRANS.RANGE")
ldLTrnDat    = CTOD(SUBSTR(lcRange,1,    ATC('|',lcRange)-1))
ldHTrnDat    = CTOD(SUBSTR(lcRange,      ATC('|',lcRange)+1))
IF !EMPTY(ldLTrnDat) AND !EMPTY(ldHTrnDat)
  llHasDate = .T.
ENDIF 

lcFltExp = ''
DO CASE
CASE lcRPTrTyp = 'ST'
  IF !EMPTY(lcStTmp)
    IF !llOgFltCh 
      RETURN 
    ENDIF 
    SELECT(lcStTmp)
    COUNT TO lnCnt FOR !DELETED()
    LOCATE
    IF lnCnt > 0 
      SCAN FOR !EOF()
        SELECT STYDYE
        IF gfSeek(&lcStTmp..STYLE)
          SCAN REST WHILE STYLE = &lcStTmp..STYLE FOR !EMPTY(STYDYE.DYELOT)
            WAIT WINDOW NOWAIT 'Collecting Data for Style : '+ALLTRIM(STYDYE.STYLE)+ ', Configuration : '+ALLTRIM(STYDYE.DYELOT)
            m.QTY1    = ROUND(STK1*(lnRepPerc/100),0)
            m.QTY2    = ROUND(STK2*(lnRepPerc/100),0)
            m.QTY3    = ROUND(STK3*(lnRepPerc/100),0)
            m.QTY4    = ROUND(STK4*(lnRepPerc/100),0)
            m.QTY5    = ROUND(STK5*(lnRepPerc/100),0)
            m.QTY6    = ROUND(STK6*(lnRepPerc/100),0)
            m.QTY7    = ROUND(STK7*(lnRepPerc/100),0)
            m.QTY8    = ROUND(STK8*(lnRepPerc/100),0)    
            m.STYLE   = STYLE      
            m.TRANID  = STYLE    
            m.CONFIG  = DYELOT
            m.TRANTYP = 'ST'
            m.PRICE   = IIF(SEEK(STYLE,'STYLE'),STYLE.PriceA,0)
            m.SCALE   = IIF(SEEK(STYLE,'STYLE'),STYLE.Scale,'')
            m.DESC    = IIF(SEEK(STYLE,'STYLE'),STYLE.DESC,'')
            m.StyMaj  = IIF(SEEK(STYLE,'STYLE'),STYLE.cStyMajor,'')
            SELECT (lcPriTmp)
            APPEND BLANK
            GATHER MEMVAR MEMO
          ENDSCAN
        ENDIF
      ENDSCAN 
      =lfUpdRpTmp()
    ENDIF
  ELSE 
    MESSAGEBOX('Please, Specify Criteria!',576,'Insufficient Data!')
    RETURN .F.
  ENDIF
   
CASE lcRPTrTyp = 'PO'
  IF !EMPTY(lcPoTmp) OR !EMPTY(lcRange)
    IF !llOgFltCh 
      RETURN
    ENDIF     
    IF EMPTY(lcPoTmp)
      SELECT POSHDR
      WAIT WINDOW NOWAIT 'Collecting Data ...'      
      gfseek('')
      COUNT TO lnCnt for !DELETED()
      LOCATE     
    ELSE 
      SELECT(lcPoTmp)
      COUNT TO lnCnt for !DELETED()
      LOCATE 
    ENDIF
    
    IF lnCnt > 0
      SCAN FOR !EOF()
      
        IF !EMPTY(lcPoTmp)
          SELECT POSHDR
          gfSeek("PP"+&lcPoTmp..PO)
          lcPO = &lcPoTmp..PO 
          IF IIF(llHasDate, !BETWEEN(POSHDR.Entered,ldLTrnDat,ldHTrnDat),.F.)
            LOOP
          ENDIF     
        ELSE 
          lcPO = POSHDR.PO          
          IF IIF(llHasDate, !BETWEEN(POSHDR.Entered,ldLTrnDat,ldHTrnDat),.F.)
            LOOP
          ENDIF      
        ENDIF
        
        SELECT POSLN
        gfSeek("PP"+lcPO)
        lcStyle  = ""
        lnLineNo = 0
        lcChekr  = ""
        lnRecNo  = 0
        SCAN FOR !EOF()        
          IF EMPTY(POSLN.DYELOT)
            LOOP
          ENDIF   

          IF STYLE+STR(LineNo,6) $ lcChekr  
            LOOP 
          ELSE 
            lcChekr = STYLE+STR(LineNo,6)+"|"
          ENDIF     
          
          lcStyle  = STYLE
          lnLineNo = LineNo
          
          PRIVATE    lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8         ,;
                     lnsQty1,lnsQty2,lnsQty3,lnsQty4,lnsQty5,lnsQty6,lnsQty7,lnsQty8 ,;
                     lnfQty1,lnfQty2,lnfQty3,lnfQty4,lnfQty5,lnfQty6,lnfQty7,lnfQty8   
                     
          STORE 0 TO lnQty1,lnQty2,lnQty3,lnQty4,lnQty5,lnQty6,lnQty7,lnQty8         ,;
                     lnsQty1,lnsQty2,lnsQty3,lnsQty4,lnsQty5,lnsQty6,lnsQty7,lnsQty8 ,;
                     lnfQty1,lnfQty2,lnfQty3,lnfQty4,lnfQty5,lnfQty6,lnfQty7,lnfQty8                            
                     
          IF lcRpBsdOn = 'P'
            LOCATE FOR STYLE = lcStyle AND LineNo = lnLineNo AND TRANCD = '1'
            IF FOUND()
              FOR lnX = 1 TO 8
                lcY = ALLTRIM(STR(lnX))
                lnfQty&lcY = QTY&lcY
              ENDFOR           
            ENDIF
          ELSE  
            LOCATE FOR STYLE = lcStyle AND LineNo = lnLineNo AND TRANCD = '1'
            IF FOUND()               
              FOR lnX = 1 TO 8
                lcY = ALLTRIM(STR(lnX))
                lnQty&lcY = QTY&lcY
              ENDFOR 
            ENDIF
            lnRecNo = RECNO()
            LOCATE FOR STYLE = lcStyle AND LineNo = lnLineNo AND TRANCD = '2'
            IF FOUND()
              FOR lnX = 1 TO 8
                lcY = ALLTRIM(STR(lnX))
                lnsQty&lcY = QTY&lcY
              ENDFOR 
            ELSE 
              GOTO lnRecNo      
            ENDIF
            FOR lnX = 1 TO 8
              lcY = ALLTRIM(STR(lnX))
              lnfQty&lcY = MAX(lnQty&lcY - lnsQty&lcY,0) && Just for handling the over recieving issue
            ENDFOR                      
          ENDIF   
          WAIT WINDOW NOWAIT 'Collecting Data for Style : '+ALLTRIM(POSLN.STYLE)+ ', Configuration : '+ALLTRIM(POSLN.DYELOT)
          m.QTY1    = ROUND(lnfQty1*(lnRepPerc/100),0)
          m.QTY2    = ROUND(lnfQty2*(lnRepPerc/100),0)
          m.QTY3    = ROUND(lnfQty3*(lnRepPerc/100),0)
          m.QTY4    = ROUND(lnfQty4*(lnRepPerc/100),0)
          m.QTY5    = ROUND(lnfQty5*(lnRepPerc/100),0)
          m.QTY6    = ROUND(lnfQty6*(lnRepPerc/100),0)
          m.QTY7    = ROUND(lnfQty7*(lnRepPerc/100),0)
          m.QTY8    = ROUND(lnfQty8*(lnRepPerc/100),0)            
          m.STYLE   = STYLE
          m.TRANID  = PO
          m.CONFIG  = DYELOT
          m.TRANTYP = 'PO'
          m.SCALE   = SCALE
          m.PRICE   = IIF(SEEK(STYLE,'STYLE'),STYLE.PriceA,0)
          m.DESC    = IIF(SEEK(STYLE,'STYLE'),STYLE.DESC,'')
          m.StyMaj  = IIF(SEEK(STYLE,'STYLE'),STYLE.cStyMajor,'')
          m.LINENO  = LINENO
          SELECT (lcPriTmp)
          APPEND BLANK
          GATHER MEMVAR MEMO
        ENDSCAN
      =lfUpdRpTmp()      
      ENDSCAN
    ENDIF   
  ELSE 
    MESSAGEBOX('Please, Specify Criteria!',576,'Insufficient Data!')
    RETURN .F.
  ENDIF
    
CASE lcRPTrTyp = 'SO'
  IF !EMPTY(lcSoTmp) OR !EMPTY(lcRange)
    IF !llOgFltCh 
      RETURN
    ENDIF     
    IF EMPTY(lcSoTmp)
      SELECT ORDHDR
      gfSeek('')
      LOCATE     
    ELSE 
      SELECT(lcSoTmp)
      COUNT TO lnCnt for !DELETED()
      LOCATE 
    ENDIF
    
    lcOrder = ""
    SCAN FOR !EOF()
    
      IF !EMPTY(lcSoTmp)
        IF lnCnt <= 0
          EXIT
        ENDIF
        SELECT ORDHDR
        LOCATE
        gfSeek("O"+&lcSoTmp..ORDER)
        lcOrder = &lcSoTmp..ORDER
        IF IIF(llHasDate, !BETWEEN(ORDHDR.Entered,ldLTrnDat,ldHTrnDat),.F.)
          LOOP
        ENDIF      
      ELSE 
        lcOrder = ORDHDR.ORDER          
        IF IIF(llHasDate, !BETWEEN(ORDHDR.Entered,ldLTrnDat,ldHTrnDat),.F.)
          LOOP
        ENDIF      
      ENDIF 
      
      SELECT ORDLINE 
      gfSetOrder('ORDLINE')
      gfSeek("O"+lcOrder)
      SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6)="O"+lcOrder FOR !EMPTY(ORDLINE.DYELOT)
        WAIT WINDOW NOWAIT 'Collecting Data for Style : '+ALLTRIM(ORDLINE.STYLE)+ ', Configuration : '+ALLTRIM(ORDLINE.DYELOT)
        m.QTY1    = IIF(lcRpBsdOn = 'P',ROUND(BOOK1*(lnRepPerc/100),0),ROUND(QTY1*(lnRepPerc/100),0))
        m.QTY2    = IIF(lcRpBsdOn = 'P',ROUND(BOOK2*(lnRepPerc/100),0),ROUND(QTY2*(lnRepPerc/100),0))
        m.QTY3    = IIF(lcRpBsdOn = 'P',ROUND(BOOK3*(lnRepPerc/100),0),ROUND(QTY3*(lnRepPerc/100),0))
        m.QTY4    = IIF(lcRpBsdOn = 'P',ROUND(BOOK4*(lnRepPerc/100),0),ROUND(QTY4*(lnRepPerc/100),0))
        m.QTY5    = IIF(lcRpBsdOn = 'P',ROUND(BOOK5*(lnRepPerc/100),0),ROUND(QTY5*(lnRepPerc/100),0))
        m.QTY6    = IIF(lcRpBsdOn = 'P',ROUND(BOOK6*(lnRepPerc/100),0),ROUND(QTY6*(lnRepPerc/100),0))
        m.QTY7    = IIF(lcRpBsdOn = 'P',ROUND(BOOK7*(lnRepPerc/100),0),ROUND(QTY7*(lnRepPerc/100),0))
        m.QTY8    = IIF(lcRpBsdOn = 'P',ROUND(BOOK8*(lnRepPerc/100),0),ROUND(QTY8*(lnRepPerc/100),0))
        m.STYLE   = STYLE
        m.TRANID  = ORDER
        m.CONFIG  = DYELOT
        m.TRANTYP = 'SO'
        m.PRICE   = PRICE
        m.SCALE   = SCALE
        m.DESC    = IIF(SEEK(STYLE,'STYLE'),STYLE.DESC,'')
        m.StyMaj  = IIF(SEEK(STYLE,'STYLE'),STYLE.cStyMajor,'')
        m.LINENO  = LINENO
        SELECT (lcPriTmp)
        APPEND BLANK
        GATHER MEMVAR MEMO
      ENDSCAN
      =lfUpdRpTmp()
    ENDSCAN
  ELSE
    MESSAGEBOX('Please, Specify Criteria!',576,'Insufficient Data!')
    RETURN .F.
  ENDIF
OTHERWISE
ENDCASE

* End of lfColData()

*!**************************************************************
*! Name      : lfCheckFilter
*! Developer : Hesham (HES)
*! Date      : 01/12/2010
*! Purpose   : Get the selected data from Option Grid
*!**************************************************************
*! Called from : PRG
*!**************************************************************
*! Calls       : ....
*!**************************************************************
*! Passed Parameters : None
*!**************************************************************
*! Return      : None
*!**************************************************************
*! Example     : = lfCheckFilter(1,"STYLE.CSTYMAJOR")
*!**************************************************************
FUNCTION lfCheckFilter
LPARAMETERS lnArrayType, lcFilter

LOCAL lcReturn, lnPOS
DO CASE
  CASE lnArrayType = 1
    lnPOS = ASCAN(loOgScroll.laOGFxFlt,lcFilter)
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGFxFlt,lnPos,1)
      lcReturn = loOgScroll.laOGFxFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 2
    lnPOS = ASCAN(loOgScroll.laOGHDFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGHDFlt,lnPos,1)
      lcReturn = loOgScroll.laOGHDFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  CASE lnArrayType = 3  
    lnPOS = ASCAN(loOgScroll.laOGvrFlt,lcFilter) 
    IF lnPos > 0
      lnPOS    = ASUBSCRIPT(loOgScroll.laOGvrFlt,lnPos,1)
      lcReturn = loOgScroll.laOGvrFlt[lnPOS,6]    
    ELSE
      lcReturn = ""     
    ENDIF
  OTHERWISE
    lcReturn = ""
ENDCASE

RETURN lcReturn
* End of lfCheckFilter

*!**************************************************************
*! Name      : lfCrtTemp
*! Developer : Hesham (HES)
*! Date      : 01/12/2010
*! Purpose   : Creating Temp
*!**************************************************************
*! Called from : PRG
*!**************************************************************
*! Calls       : ....
*!**************************************************************
*! Passed Parameters : None
*!**************************************************************
*! Return      : None
*!**************************************************************
*! Example     : =lfCrtTemp()
*!**************************************************************
FUNCTION lfCrtTemp

*-------------\ Primary Temp \--------------
IF USED(lcPriTmp) AND RECCOUNT(lcPriTmp) > 0
  SELECT (lcPriTmp)
  ZAP 
ENDIF
IF !USED(lcPriTmp)
  lnI = 1
  
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'TRANTYP'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 2
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'TRANID'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 19
  laTempStru1[lnI,4] = 0 
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'STYLE'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 19
  laTempStru1[lnI,4] = 0 
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'COLOR'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'SCALE'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 3
  laTempStru1[lnI,4] = 0    
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'PRICE'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0    
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'QTY1'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'QTY2'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'QTY3'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'QTY4'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'QTY5'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'QTY6'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'QTY7'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'QTY8'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'LINENO'
  laTempStru1[lnI,2] = 'N'
  laTempStru1[lnI,3] = 6
  laTempStru1[lnI,4] = 0  
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'CONFIG'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 10
  laTempStru1[lnI,4] = 0  
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'DESC'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 20
  laTempStru1[lnI,4] = 0  
  
  lnI = ALEN(laTempStru1,1)+1
  DIMENSION laTempStru1[lnI,4]
  laTempStru1[lnI,1] = 'STYMAJ'
  laTempStru1[lnI,2] = 'C'
  laTempStru1[lnI,3] = 8
  laTempStru1[lnI,4] = 0    
  
  DECLARE LAIndeces[1,2]
  LAIndeces[1,1] = 'CONFIG'
  LAIndeces[1,2] = 'cCnfg'
  gfCrtTmp(lcPriTmp,@laTempStru1,@LAIndeces,lcPriTmp,.T.)
ENDIF

*-------------\ Report Temp \---------------
IF USED(lcRepTmp) AND RECCOUNT(lcRepTmp) > 0
  SELECT (lcRepTmp)
  ZAP 
ENDIF
IF !USED(lcRepTmp)
  lnI = 1
  
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'STYLE'
  laTempStru2[lnI,2] = 'C'
  laTempStru2[lnI,3] = 8
  laTempStru2[lnI,4] = 0
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'COLOR'
  laTempStru2[lnI,2] = 'C'
  laTempStru2[lnI,3] = 6
  laTempStru2[lnI,4] = 0 
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'FSTYLE'
  laTempStru2[lnI,2] = 'C'
  laTempStru2[lnI,3] = 19
  laTempStru2[lnI,4] = 0        
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'DESC'
  laTempStru2[lnI,2] = 'C'
  laTempStru2[lnI,3] = 20
  laTempStru2[lnI,4] = 0   
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'SIZE'
  laTempStru2[lnI,2] = 'N'
  laTempStru2[lnI,3] = 2
  laTempStru2[lnI,4] = 0
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'CONFIG'
  laTempStru2[lnI,2] = 'C'
  laTempStru2[lnI,3] = 10
  laTempStru2[lnI,4] = 0
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'UPCCODE'
  laTempStru2[lnI,2] = 'C'
  laTempStru2[lnI,3] = 13
  laTempStru2[lnI,4] = 0  
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'SCRCODE'
  laTempStru2[lnI,2] = 'C'
  laTempStru2[lnI,3] = 30
  laTempStru2[lnI,4] = 0    
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'SCRCOLR'
  laTempStru2[lnI,2] = 'C'
  laTempStru2[lnI,3] = 30
  laTempStru2[lnI,4] = 0   
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'ACCESS'
  laTempStru2[lnI,2] = 'C'
  laTempStru2[lnI,3] = 30
  laTempStru2[lnI,4] = 0       
  
  lnI = ALEN(laTempStru2,1)+1
  DIMENSION laTempStru2[lnI,4]
  laTempStru2[lnI,1] = 'PRICE'
  laTempStru2[lnI,2] = 'N'
  laTempStru2[lnI,3] = 6
  laTempStru2[lnI,4] = 0
  
  DECLARE LAIndeces1[1,2]
  LAIndeces1[1,1] = 'FSTYLE+CONFIG+STR(SIZE,2)'
  LAIndeces1[1,2] = 'cSTUPC'
  gfCrtTmp(lcRepTmp,@laTempStru2,@LAIndeces1,lcRepTmp,.T.)
ENDIF 
*-------------\ BarCode Temp \---------------
IF USED(lcBarTmp) AND RECCOUNT(lcBarTmp) > 0
  SELECT (lcBarTmp)
  ZAP 
ENDIF
IF !USED(lcBarTmp)
  lnI = 1
  
  DIMENSION laTempStru3[lnI,4]
  laTempStru3[lnI,1] = 'STYLE'
  laTempStru3[lnI,2] = 'C'
  laTempStru3[lnI,3] = 8
  laTempStru3[lnI,4] = 0
     
  lnI = ALEN(laTempStru3,1)+1
  DIMENSION laTempStru3[lnI,4]
  laTempStru3[lnI,1] = 'SIZE'
  laTempStru3[lnI,2] = 'N'
  laTempStru3[lnI,3] = 2
  laTempStru3[lnI,4] = 0 
  
  lnI = ALEN(laTempStru3,1)+1
  DIMENSION laTempStru3[lnI,4]
  laTempStru3[lnI,1] = 'CONFIG'
  laTempStru3[lnI,2] = 'C'
  laTempStru3[lnI,3] = 10
  laTempStru3[lnI,4] = 0
  
  lnI = ALEN(laTempStru3,1)+1
  DIMENSION laTempStru3[lnI,4]
  laTempStru3[lnI,1] = 'UPCCODE'
  laTempStru3[lnI,2] = 'C'
  laTempStru3[lnI,3] = 13
  laTempStru3[lnI,4] = 0  

  lnI = ALEN(laTempStru3,1)+1
  DIMENSION laTempStru3[lnI,4]
  laTempStru3[lnI,1] = 'UPCFNT'
  laTempStru3[lnI,2] = 'G'
  laTempStru3[lnI,3] = 10
  laTempStru3[lnI,4] = 0
  
  DECLARE LAIndeces2[1,2]
  LAIndeces2[1,1] = 'STYLE+CONFIG+STR(SIZE,2)+UPCCODE'
  LAIndeces2[1,2] = 'UPC'
  gfCrtTmp(lcBarTmp,@laTempStru3,@LAIndeces2,lcBarTmp,.T.)
ENDIF 

SELECT (lcRepTmp)
SET RELATION TO STYLE+CONFIG+STR(SIZE,2)+UPCCODE INTO &lcBarTmp

* End of lfCrtTemp

*!**************************************************************
*! Name      : lfUpdRpTmp
*! Developer : Hesham (HES)
*! Date      : 01/12/2010
*! Purpose   : Updating the Report Temp
*!**************************************************************
*! Called from : PRG
*!**************************************************************
*! Calls       : ....
*!**************************************************************
*! Passed Parameters : None
*!**************************************************************
*! Return      : None
*!**************************************************************
*! Example     : =lfUpdRpTmp()
*!**************************************************************
FUNCTION lfUpdRpTmp

SELECT(lcPriTmp)
SET RELATION TO "S"+SCALE INTO SCALE

LOCAL lcScrCode, lcScrColr, lcAccess 
STORE '' TO lcScrCode, lcScrColr, lcAccess  

SELECT CODES
LOCATE FOR cdiscrep = 'Screen Code'
IF FOUND()
  lcScrCode = cCode_no
ENDIF

LOCATE FOR cdiscrep = 'Screen Color'
IF FOUND()
  lcScrColr = cCode_no
ENDIF

LOCATE FOR cdiscrep = 'Accessory Color'
IF FOUND()
  lcAccess  = cCode_no
ENDIF

SELECT(lcPriTmp)
SCAN FOR !EOF()
DO CASE

  CASE lcRPTrTyp $ 'POSO'
    IF lcRPTrTyp = 'PO'
      lcKey = lcRPTrTyp+"PP"+PADR(&lcPriTmp..TRANID,6)+STR(&lcPriTmp..LINENO,6)
    ELSE
      lcKey = lcRPTrTyp+"O"+PADR(&lcPriTmp..TRANID,6)+STR(&lcPriTmp..LINENO,6)
    ENDIF
    SELECT PROFVALU
    gfSeek(lcKey)
    LOCATE REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = lcKey FOR CPRO_CODE = lcScrCode
    IF FOUND()
      m.SCRCODE = cPro_Value
    ENDIF
    LOCATE WHILE CPRO_TYPE+CKEY+CPRO_CODE = lcKey FOR CPRO_CODE = lcScrColr
    IF FOUND()
      m.SCRCOLR = cPro_Value
    ENDIF  
    LOCATE WHILE CPRO_TYPE+CKEY+CPRO_CODE = lcKey FOR CPRO_CODE = lcAccess
    IF FOUND()
      m.ACCESS = cPro_Value
    ENDIF      
       
  CASE lcRPTrTyp $ 'ST'
    lcKey = lcRPTrTyp+PADR(&lcPriTmp..TRANID,19)
    SELECT PROFVALU
    IF gfSeek(lcKey)
    ELSE 
      lcKey = lcRPTrTyp+PADR(&lcPriTmp..StyMaj,8)+"-******-***"
    ENDIF
    
    lnRecNob = 0
    IF gfSeek(lcKey) 
      lnRecNob = RECNO()
      LOCATE REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = lcKey FOR CPRO_CODE = lcScrCode
      IF FOUND()
        m.SCRCODE = cPro_Value
      ENDIF
    ENDIF
    IF lnRecNob <> 0
      GOTO lnRecNob 
      LOCATE REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = lcKey FOR CPRO_CODE = lcScrColr
      IF FOUND()
        m.SCRCOLR = cPro_Value
      ENDIF 
      GOTO lnRecNob 
      LOCATE REST WHILE CPRO_TYPE+CKEY+CPRO_CODE = lcKey FOR CPRO_CODE = lcAccess
      IF FOUND()
        m.ACCESS = cPro_Value
      ENDIF      
    ELSE 
      m.SCRCOLR = ""
      m.SCRCODE = "" 
      m.ACCESS  = ""    
    ENDIF
  OTHERWISE 
    m.SCRCOLR = ""
    m.SCRCODE = ""
    m.ACCESS  = "" 
  ENDCASE 
  
  FOR lnX = 1 TO 8
    lcX = ALLTRIM(STR(lnX))
    lnQty = &lcPriTmp..Qty&lcX
    m.Size    = PADR(lcX,2)
    m.Price   = &lcPriTmp..Price
    m.Config  = &lcPriTmp..Config
    m.Desc    = &lcPriTmp..Desc
    m.Style   = &lcPriTmp..StyMaj
    m.Color   = SUBSTR(&lcPriTmp..Style,10,6)
    m.FSTYLE  = &lcPriTmp..Style
    SELECT CONFGUPC
    gfSeek(&lcPriTmp..STYLE+&lcPriTmp..CONFIG+PADL(lcX,2))
    m.UPCCODE = BARCODE
    
    IF !EMPTY(m.UPCCODE)
    
      SELECT(lcBarTmp)
      APPEND BLANK 
      APPEND GENERAL UPCFnt CLASS ("IDAuto.BarCode")
      =OLEBarCode(m.UPCCODE)
      GATHER MEMVAR MEMO    
    
      FOR lnI = 1 TO lnQty
        WAIT WINDOW NOWAIT 'Generating Lable for Style : '+ALLTRIM(m.Style)+', Configuration : '+ ;
                                               ALLTRIM(m.Config)+', Size No. '+lcX+', Piece No. '+ALLTRIM(STR(lnI))+ ' '
        SELECT(lcRepTmp)
        APPEND BLANK 
        GATHER MEMVAR MEMO
      ENDFOR 
    ENDIF
  ENDFOR
ENDSCAN
* End of lfUpdRpTmp

*!**************************************************************
*! Name      : OLEBarCode 
*! Developer : Hesham (HES)
*! Date      : 01/12/2010
*! Purpose   : Updating the general field for the report
*!**************************************************************
*! Called from : PRG
*!**************************************************************
*! Calls       : ....
*!**************************************************************
*! Passed Parameters : None
*!**************************************************************
*! Return      : None
*!**************************************************************
*! Example     : =OLEBarCode ()
*!**************************************************************
PROCEDURE OLEBarCode 
PARAMETERS  tcCUPC

lcF_name = UPPER('UPCFnt')
WITH loOgScroll && your form name
  IF TYPE('loOgScroll.'+ lcF_name) <> 'O'
    .ADDOBJECT(lcF_name,"OLEBoundControl")
  ENDIF
  .&lcF_name..CONTROLSOURCE = lcF_name
  .&lcF_name..WIDTH         = 150
  .&lcF_name..HEIGHT        = 150
   APPEND GENERAL &lcF_name. CLASS ("IDAuto.BarCode")
  .&lcF_name..REFRESH
  .&lcF_name..OBJECT.DataToEncode = ALLTRIM(tcCUPC)
  .&lcF_name..OBJECT.Barheight    = 2
  .&lcF_name..OBJECT.SymbologyID  = 13
  .&lcF_name..OBJECT.showtext     = 0
  .&lcF_name..OBJECT.NarrowBarWidth =  0.03
  *E302368,1 HIA adding Orientation to the barcode [Begin]
  * Values are 0,90,180
  .&lcF_name..OBJECT.Orientation = 0
  .&lcF_name..OBJECT.TopMarginCm = 0
  
 .&lcF_name..OBJECT.LeftMarginCm = 0
  .&lcF_name..OBJECT.BACKCOLOR = RGB(255,255,255)
  .&lcF_name..OBJECT.FORECOLOR = RGB(0,0,0)
    
ENDWITH