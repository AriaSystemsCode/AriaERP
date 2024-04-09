*:***************************************************************************
*: Program file  : ALPKLSOV.PRG
*: Program desc. : CUSTOMIZED PACKING LIST Form FOR OverStock.
*: Date          : 2/10/2019
*: System        : Aria Advantage Series.4XP
*: Module        : AL
*: Developer     : Heba Magdy Selim (HMS)
*: Tracking Job Number: (C202248)[T20190129.0003]
*:***************************************************************************
*: Modifications:
*:***************************************************************************
SET STEP ON 
SELECT (lcPackTmp)
lnTmpFld = AFIELDS(laFieldArr)
DIMENSION laNewTempStru[7,4]
laNewTempStru[1,1] ="QTY"
laNewTempStru[1,2] ='N'
laNewTempStru[1,3]= 7
laNewTempStru[1,4]=0

laNewTempStru[2,1] ="Ship"
laNewTempStru[2,2] ='N'
laNewTempStru[2,3]= 7
laNewTempStru[2,4]=0

laNewTempStru[3,1] ="ItemNo"
laNewTempStru[3,2] ='C'
laNewTempStru[3,3]= 30
laNewTempStru[3,4]=0

laNewTempStru[4,1] ="VendorSKU"
laNewTempStru[4,2] ='C'
laNewTempStru[4,3]= 30
laNewTempStru[4,4]=0


laNewTempStru[5,1] ="PrductDesc"
laNewTempStru[5,2] ='C'
laNewTempStru[5,3]= 60
laNewTempStru[5,4]=0

laNewTempStru[6,1] = 'Pack_No'
laNewTempStru[6,2] = 'C'
laNewTempStru[6,3] = 6
laNewTempStru[6,4] = 0

laNewTempStru[7,1] = 'LineNo'
laNewTempStru[7,2] = 'N'
laNewTempStru[7,3] = 6
laNewTempStru[7,4] = 0

lcUpcTemp = gfTempName()

=gfCrtTmp(lcUpcTemp ,@laNewTempStru,"PACK_NO+STR(LineNo,6)",lcUpcTemp ,.T.)

lcLineTemp =IIF(lcRpSelcBy ="I",(lcInvLnTmp),IIF(lcRpSelcBy ='P',(lcPakLnTmp),(lcOrdLnTmp)))
SELECT(lcLineTemp)

IF ! UPPER(lcStyleFile) $ UPPER(SET("Relation"))
  SET RELATION TO Style INTO (lcStyleFile) Addi
ENDIF  
SELECT (lcPackTmp)
LOCATE 
SCAN FOR (ACCOUNT = "OVE01")
  m.PACK_NO = &lcPackTmp..Pack_NO
  SELECT (lcLineTemp)
  LOCATE 
  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
      LOOP
    ENDIF
    =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
    m.QTY = &lcOrdLnTmp..TotBook
	m.Ship = EVALUATE(lcLineTemp+"."+IIF(lcRpSelcBy $"PI",'Totqty','TotPik'))
	m.ItemNo = ''
	m.VendorSKU = ''
	m.PrductDesc= ''
    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)) &&&lcLineTemp..nordlineno

    IF 'SK|' $ &lcOrdLnTmp..note_mem
      lnPosSk = ATC('SK|',&lcOrdLnTmp..note_mem)
      lnEndPos = ATC('|',SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk +3))
      m.VendorSKU = SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk+3 ,lnEndPos -1)
    ENDIF
    
    
    IF 'BP|' $ &lcOrdLnTmp..note_mem
      lnPosSk = ATC('BP|',&lcOrdLnTmp..note_mem)
      lnEndPos = ATC('|',SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk +3))
      m.ItemNo = SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk+3 ,lnEndPos -1)
    ENDIF
    
    IF '08|' $ &lcOrdLnTmp..note_mem
      lnPosSk = ATC('08|',&lcOrdLnTmp..note_mem)
      lnEndPos = ATC('|',SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk +3))
      m.PrductDesc= SUBSTR(&lcOrdLnTmp..note_mem,lnPosSk+3 ,lnEndPos -1)
    ENDIF
    INSERT INTO (lcUpcTemp) FROM MEMVAR
  ENDSCAN 
ENDSCAN 


SELECT (lcPackTmp)
SET FILTER TO account = 'OVE01'
LOCATE 
IF !EOF()
  SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
  SET SKIP TO (lcUpcTemp)
  LOCATE 
  DO gfDispRe WITH EVAL('lcFormName')
ENDIF

SET FILTER TO 
llALPakLst=.F.

SELECT (lcPackTmp)
SET FILTER TO account <> 'OVE01'
LOCATE 
IF !EOF()
  lcFormName= "ALPKLSA"
  loOgScroll.lcOGLastForm = "ALPKLSA"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSA","",.T.)    && Create criteria form.
  DO gfDispRe WITH EVAL('lcFormName')
  lcFormName= "ALPKLSOV"
  loOgScroll.lcOGLastForm = "ALPKLSOV"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSOV","",.T.)    && Create criteria form.
ENDIF
SET FILTER TO 
*!**************************************************************************
*! Name      : lfOvHeadVar
*! Developer : Heba Magdy Selim (HMS)
*! Date      : 02/11/2019
*! Purpose   : Get Header Info. and add phone# to Ship to array
*!**************************************************************************
FUNCTION lfOvHeadVar
lfHeadVar()
FOR lnX = 1 TO 6 
  IF EMPTY(ALLTRIM(laShipTo[lnX]))
    laShipTo[lnX] = EVAL(lcOrdhdr+'.phone')
    EXIT
  ENDIF
ENDFOR