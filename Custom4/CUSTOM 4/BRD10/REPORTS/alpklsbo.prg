*:***************************************************************************
*: Program file  : ALPKLSBO.PRG
*: Program desc. : CUSTOMIZED PACKING LIST FOR Neiman Marcus Branded Online.
*: Date          :02/10/2019
*: System        : Aria Advantage Series.4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : SARA ROUSHDY
*: Tracking Job Number: (C202249) 
*:***************************************************************************
*: Modifications:
*:***************************************************************************
SET STEP ON 
SELECT (lcPackTmp)
lnTmpFld = AFIELDS(laFieldArr)
DIMENSION laNewTempStru[11,4]
laNewTempStru[1,1] ="QTY"
laNewTempStru[1,2] ='N'
laNewTempStru[1,3]= 7
laNewTempStru[1,4]=0

laNewTempStru[2,1] ="OrderNum"
laNewTempStru[2,2] ='C'
laNewTempStru[2,3]= 30
laNewTempStru[2,4]=0

laNewTempStru[3,1] ="Style"
laNewTempStru[3,2] ='C'
laNewTempStru[3,3]= 60
laNewTempStru[3,4]=0

laNewTempStru[4,1] = 'Pack_No'
laNewTempStru[4,2] = 'C'
laNewTempStru[4,3] = 60
laNewTempStru[4,4] = 0

laNewTempStru[5,1] = 'LineNo'
laNewTempStru[5,2] = 'N'
laNewTempStru[5,3] = 6
laNewTempStru[5,4] = 0


laNewTempStru[6,1] = 'GiftMsg'
laNewTempStru[6,2] = 'C'
laNewTempStru[6,3] = 60
laNewTempStru[6,4] = 0

laNewTempStru[7,1] = 'ReturnMsg'
laNewTempStru[7,2] = 'C'
laNewTempStru[7,3] = 60
laNewTempStru[7,4] = 0


laNewTempStru[8,1] = 'MarketMsg'
laNewTempStru[8,2] = 'C'
laNewTempStru[8,3] = 60
laNewTempStru[8,4] = 0

laNewTempStru[9,1] = 'PackslpMsg'
laNewTempStru[9,2] = 'C'
laNewTempStru[9,3] = 60
laNewTempStru[9,4] = 0

laNewTempStru[10,1] = 'StyleDesc'
laNewTempStru[10,2] = 'C'
laNewTempStru[10,3] = 60
laNewTempStru[10,4] = 0

laNewTempStru[11,1] = 'cUpsDesc'
laNewTempStru[11,2] = 'C'
laNewTempStru[11,3] = 30
laNewTempStru[11,4] = 0

lcUpcTemp = gfTempName()

=gfCrtTmp(lcUpcTemp ,@laNewTempStru,"PACK_NO+STR(LineNo,6)",lcUpcTemp ,.T.)

lcLineTemp =IIF(lcRpSelcBy ="I",(lcInvLnTmp),IIF(lcRpSelcBy ='P',(lcPakLnTmp),(lcOrdLnTmp)))
SELECT(lcLineTemp)

*!*	IF ! UPPER(lcStyleFile) $ UPPER(SET("Relation"))
*!*	  SET RELATION TO Style INTO (lcStyleFile) Addi
*!*	ENDIF  
SELECT (lcPackTmp)
LOCATE 
SCAN FOR (ACCOUNT = "NEM00")
  m.PACK_NO = &lcPackTmp..Pack_NO
  SELECT (lcLineTemp)
  LOCATE 
  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
      LOOP
    ENDIF
    =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
    
    m.QTY = &lcOrdLnTmp..TotBook
    m.OrderNum = ''
    m.Style =  &lcOrdLnTmp..Style
    
	m.LineNo =''
    m.GiftMsg=''
    m.ReturnMsg=''
    M.MarketMsg = ''
    M.PackslpMsg =''
    m.StyleDesc =''
    m.cUpsDesc = ''
    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo))
    ****
    IF 'Marketing message:' $ &lcNotePad..mnotes 
      LNNOTESLINE = MEMLINES(&lcNotePad..mnotes)
      FOR LNF = 1  TO LNNOTESLINE
        IF 'Marketing message:'  $ MLINE(&lcNotePad..mnotes ,LNF)
           M.MarketMsg = ALLTRIM(SUBSTR(MLINE(&lcNotePad..mnotes,LNF),AT('Marketing message:',MLINE(&lcNotePad..mnotes,LNF))+LEN('Marketing message:')))
           EXIT
        ENDIF
      ENDFOR
    ENDIF
        
    IF 'Customer order #:' $ &lcNotePad..mnotes
      LNNOTESLINE = MEMLINES(&lcNotePad..mnotes)
      FOR LNF = 1  TO LNNOTESLINE
        IF 'Customer order #:'  $ MLINE(&lcNotePad..mnotes ,LNF)
          m.OrderNum  = ALLTRIM(SUBSTR(MLINE(&lcNotePad..mnotes,LNF),AT('Customer order #:',MLINE(&lcNotePad..mnotes,LNF))+LEN('Customer order #:')))
          EXIT
        ENDIF
      ENDFOR
    ENDIF
    
   
    
    IF 'Return message:' $ &lcNotePad..mnotes
      LNNOTESLINE = MEMLINES(&lcNotePad..mnotes)
      FOR LNF = 1  TO LNNOTESLINE
        IF 'Return message:'  $ MLINE(&lcNotePad..mnotes ,LNF)
          m.ReturnMsg = ALLTRIM(SUBSTR(MLINE(&lcNotePad..mnotes,LNF),AT('Return message:',MLINE(&lcNotePad..mnotes,LNF))+LEN('Return message:')))
          EXIT
        ENDIF
      ENDFOR
    ENDIF
    
*!*	    IF 'Gift message:|' $ &lcOrdLnTmp..note_mem 
*!*	      LNNOTESLINE = MEMLINES(&lcOrdLnTmp..note_mem)
*!*	      FOR LNF = 1  TO LNNOTESLINE
*!*	        IF 'Gift message:'  $ MLINE(&lcOrdLnTmp..note_mem ,LNF)
*!*	          m.GiftMsg  = ALLTRIM(SUBSTR(MLINE(&lcOrdLnTmp..note_mem,LNF),AT('Gift message:',MLINE(&lcOrdLnTmp..note_mem,LNF))+LEN('Gift message:')))
*!*	           EXIT
*!*	        ENDIF
*!*	      ENDFOR
*!*	    ENDIF
    
    IF 'Gift message : |' $ &lcOrdLnTmp..note_mem
      lnPosSk = ATC('Gift message : |',&lcOrdLnTmp..note_mem)
      lnEndPos = ATC('|',SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk +16))
      m.GiftMsg = SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk+16 ,lnEndPos -1)
    ENDIF
    
    
    IF 'Packing slip message:|' $ &lcOrdLnTmp..note_mem
      lnPosSk = ATC('Packing slip message:|',&lcOrdLnTmp..note_mem)
      lnEndPos = ATC('|',SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk +22))
      m.PackslpMsg  = SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk+22 ,lnEndPos -1)
    ENDIF
    
    
    
       
    IF 'PD|' $ &lcOrdLnTmp..note_mem
      lnPosSk = ATC('PD|',&lcOrdLnTmp..note_mem)
      lnEndPos = ATC('|',SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk +3))
      m.StyleDesc = SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk+3 ,lnEndPos -1)
    ENDIF
    
*!*	    DIMENSION laDRltFld[1,2]
*!*	    laDRltFld = ''
*!*	    laDRltFld[1,1] = 'CUPS'
*!*	    laDRltFld[1,2] = 'lcupsCode'
*!*	    lcupsCode = ''
*!*	    =gfRltFld(&lcPackTmp..ShipVia ,@laDRltFld,'SHIPVIA')
    m.cUpsDesc =gfCodDes(IIF(&lcPackTmp..nRprtTyp=1,&lcPackTmp..Shipvia,IIF(&lcPackTmp..nRprtTyp=2,&lcPackTmp..SHIPVIA,eval(lcOrdhdr+'.Shipvia'))),'SHIPVIA',.T.)
    
    INSERT INTO (LCUPCTEMP) FROM MEMVAR
 ENDSCAN
 ENDSCAN


SELECT (lcPackTmp)
SET FILTER TO account = 'NEM00'
LOCATE 
IF !EOF()
  SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
  SET SKIP TO (lcUpcTemp)
  LOCATE 
  DO gfDispRe WITH EVAL('lcFormName')
  USE IN (lcUpcTemp)
ENDIF

SET FILTER TO 
llALPakLst=.F.

SELECT (lcPackTmp)
*: C202249,1 SAH  02/10/2019 Add custom form for Neiman Marcus Branded Online [Start]
*SET FILTER TO account <> 'NOR12'
SET FILTER TO account <> 'NEM00'
*: C202249,1 SAH  02/10/2019 Add custom form for Neiman Marcus Branded Online [End]
LOCATE 
IF !EOF()
  lcFormName= "ALPKLSA"
  loOgScroll.lcOGLastForm = "ALPKLSA"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSA","",.T.)    && Create criteria form.
  DO gfDispRe WITH EVAL('lcFormName')
  lcFormName= "ALPKLSBO"
  loOgScroll.lcOGLastForm = "ALPKLSBO"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSBO","",.T.)    && Create criteria form.
ENDIF
SET FILTER TO 