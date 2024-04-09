*:***************************************************************************
*: Program file  : ALPKLSCO.PRG
*: Program desc. : CUSTOMIZED PACKING LIST Form FOR GMA  (CommerceHub).
*: Date          : 07/28/2020
*: System        : Aria Advantage Series.4XP
*: Module        : AL
*: Developer     : Mariam Mazhar (MMT)
*: Tracking Job Number: (C202372)
*:***************************************************************************
*: Modifications:
*:***************************************************************************
SET STEP ON 
DIMENSION laReturn[7]
laReturn = ''
DIMENSION laDeliv[6]
laDeliv = ''
SELECT (lcPackTmp)
lnTmpFld = AFIELDS(laFieldArr)
DIMENSION laNewTempStru[33,4]
laNewTempStru[1,1] ="QTY"
laNewTempStru[1,2] ='N'
laNewTempStru[1,3]= 7
laNewTempStru[1,4]=0

laNewTempStru[2,1] ="cemail"
laNewTempStru[2,2] ='C'
laNewTempStru[2,3]= 60
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
laNewTempStru[5,3]= 120
laNewTempStru[5,4]=0

laNewTempStru[6,1] = 'Pack_No'
laNewTempStru[6,2] = 'C'
laNewTempStru[6,3] = 6
laNewTempStru[6,4] = 0

laNewTempStru[7,1] = 'LineNo'
laNewTempStru[7,2] = 'N'
laNewTempStru[7,3] = 6
laNewTempStru[7,4] = 0

laNewTempStru[8,1] = 'CstOrder'
laNewTempStru[8,2] = 'C'
laNewTempStru[8,3] = 30
laNewTempStru[8,4] = 0

laNewTempStru[9,1] = 'cOrderDate'
laNewTempStru[9,2] = 'C'
laNewTempStru[9,3] = 30
laNewTempStru[9,4] = 0

laNewTempStru[10,1] = 'cShipRef'
laNewTempStru[10,2] = 'C'
laNewTempStru[10,3] = 30
laNewTempStru[10,4] = 0

laNewTempStru[11,1] = 'GORDERN'
laNewTempStru[11,2] = 'G'
laNewTempStru[11,3] = 10
laNewTempStru[11,4] = 0

laNewTempStru[12,1] = 'GIFTMSG'
laNewTempStru[12,2] = 'M'
laNewTempStru[12,3] = 10
laNewTempStru[12,4] = 0

laNewTempStru[13,1] = 'PayMeth'
laNewTempStru[13,2] = 'M'
laNewTempStru[13,3] = 10
laNewTempStru[13,4] = 0

laNewTempStru[14,1] = 'RegID'
laNewTempStru[14,2] = 'C'
laNewTempStru[14,3] = 30
laNewTempStru[14,4] = 0

laNewTempStru[15,1] = 'LineGIFTMSG'
laNewTempStru[15,2] = 'M'
laNewTempStru[15,3] = 10
laNewTempStru[15,4] = 0

laNewTempStru[16,1] = 'cOrderWare'
laNewTempStru[16,2] = 'C'
laNewTempStru[16,3] = 30
laNewTempStru[16,4] = 0

laNewTempStru[17,1] = 'VendPart'
laNewTempStru[17,2] = 'C'
laNewTempStru[17,3] = 30
laNewTempStru[17,4] = 0

laNewTempStru[18,1] = 'ColrSz'
laNewTempStru[18,2] = 'C'
laNewTempStru[18,3] = 30
laNewTempStru[18,4] = 0

laNewTempStru[19,1] = 'Color'
laNewTempStru[19,2] = 'C'
laNewTempStru[19,3] = 20
laNewTempStru[19,4] = 0


laNewTempStru[20,1] = 'Size'
laNewTempStru[20,2] = 'C'
laNewTempStru[20,3] = 20
laNewTempStru[20,4] = 0

laNewTempStru[21,1] = 'GORDRET'
laNewTempStru[21,2] = 'G'
laNewTempStru[21,3] = 10
laNewTempStru[21,4] = 0

laNewTempStru[22,1] = 'CSPHONE'
laNewTempStru[22,2] = 'C'
laNewTempStru[22,3] = 40
laNewTempStru[22,4] = 0

laNewTempStru[23,1] = 'CSEMAIL'
laNewTempStru[23,2] = 'C'
laNewTempStru[23,3] = 60
laNewTempStru[23,4] = 0

laNewTempStru[24,1] = 'PRICE'
laNewTempStru[24,2] = 'N'
laNewTempStru[24,3] = 10
laNewTempStru[24,4] = 2

laNewTempStru[25,1] = 'CSURL'
laNewTempStru[25,2] = 'C'
laNewTempStru[25,3] = 60
laNewTempStru[25,4] = 0

laNewTempStru[26,1] ="CUSTNAME"
laNewTempStru[26,2] ='C'
laNewTempStru[26,3]= 30
laNewTempStru[26,4]=0	

laNewTempStru[27,1] ="PrsData"
laNewTempStru[27,2] ='C'
laNewTempStru[27,3]= 30
laNewTempStru[27,4]=0	

laNewTempStru[28,1] ="RECEIPTID"
laNewTempStru[28,2] ='C'
laNewTempStru[28,3]= 30
laNewTempStru[28,4]=0	

laNewTempStru[29,1] ="MEMBERID"
laNewTempStru[29,2] ='C'
laNewTempStru[29,3]= 30
laNewTempStru[29,4]=0	

laNewTempStru[30,1] ="LineChrge"
laNewTempStru[30,2] ='N'
laNewTempStru[30,3]= 8
laNewTempStru[30,4]=2	

laNewTempStru[31,1] ="LineTAX"
laNewTempStru[31,2] ='N'
laNewTempStru[31,3]= 8
laNewTempStru[31,4]=2	

laNewTempStru[32,1] ="Linecred"
laNewTempStru[32,2] ='N'
laNewTempStru[32,3]= 8
laNewTempStru[32,4]=2	

laNewTempStru[33,1] = 'ORDMSG'
laNewTempStru[33,2] = 'M'
laNewTempStru[33,3] = 10
laNewTempStru[33,4] = 0
IF lcRpFrmCd = 'HD'
  lnArryDim = ALEN(laNewTempStru,1)
  DIMENSION laNewTempStru[lnArryDim +32,4]
  FOR lnA = 1 TO 8
    lcA = STR(lnA,1)
	  laNewTempStru[lnArryDim +lnA,1] ="ItemNo"+lcA
		laNewTempStru[lnArryDim +lnA,2] ='C'
		laNewTempStru[lnArryDim +lnA,3]= 30
		laNewTempStru[lnArryDim +lnA,4]=0
    
    laNewTempStru[lnArryDim +lnA+8,1] ="VendorSKU"+lcA
		laNewTempStru[lnArryDim +lnA+8,2] ='C'
		laNewTempStru[lnArryDim +lnA+8,3]= 30
		laNewTempStru[lnArryDim +lnA+8,4]=0


		laNewTempStru[lnArryDim +lnA+16,1] ="PrductDesc"+lcA
		laNewTempStru[lnArryDim +lnA+16,2] ='C'
		laNewTempStru[lnArryDim +lnA+16,3]= 60
		laNewTempStru[lnArryDim +lnA+16,4]=0

		laNewTempStru[lnArryDim +lnA+24,1] ="QTY"+lcA
		laNewTempStru[lnArryDim +lnA+24,2] ='N'
		laNewTempStru[lnArryDim +lnA+24,3]= 7
		laNewTempStru[lnArryDim +lnA+24,4]=0			
	ENDFOR 
	
ENDIF

lcUpcTemp = gfTempName()

=gfCrtTmp(lcUpcTemp ,@laNewTempStru,"PACK_NO+STR(LineNo,6)",lcUpcTemp ,.T.)
SET STEP ON 
lcLineTemp =IIF(lcRpSelcBy ="I",(lcInvLnTmp),IIF(lcRpSelcBy ='P',(lcPakLnTmp),(lcOrdLnTmp)))
SELECT(lcLineTemp)

IF ! UPPER(lcStyleFile) $ UPPER(SET("Relation"))
  SET RELATION TO Style INTO (lcStyleFile) Addi
ENDIF  
DO CASE 
  CASE lcRpFrmCd = 'CO'
		SELECT (lcPackTmp)
		LOCATE 
		SCAN &&FOR ACCOUNT = "COST0"
		  m.PACK_NO = &lcPackTmp..Pack_NO
		  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
		  lcOrderNumber= ''
		  lcOrderDate = ''
		  lcShipRef = ''
		  lcGiftMsg = ''
		  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)

		  FOR lnF = 1  TO lnNotesLine 
		    IF 'Customer order number:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderNumber= lcOrderNumber+IIF(EMPTY(lcOrderNumber),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer order number:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer order number:')))  
		    ENDIF
		      
		    IF 'Cust order date:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderDate = lcOrderDate +IIF(EMPTY(lcOrderDate ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Cust order date:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Cust order date:')))  
		    ENDIF
		    IF  'Routing:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcShipRef = lcShipRef +IIF(EMPTY(lcShipRef ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Routing:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Routing:')))  
		    ENDIF
		     
		    IF 'Gift Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Gift Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Gift Message:')))  
		    ENDIF
		  ENDFOR 
		  m.CstOrder = lcOrderNumber
		  m.cOrderDate = lcOrderDate 
		  m.cShipRef  = lcShipRef  
		  m.GIFTMSG = lcGiftMsg 
		  SELECT (lcLineTemp)
		  LOCATE 
		  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
		    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
		      LOOP
		    ENDIF
		    IF 'ORDLINE' $ DBF(lcOrdLnTmp)
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,'ORDLINE')
		    ELSE 
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
		    ENDIF
		    m.cemail = &lcCustomer..cemail_add 
		    m.ItemNo = &lcOrdLnTmp..STYLE
		    m.PrductDesc = &lcOrdLnTmp..DESC1
		    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)) &&&lcLineTemp..nordlineno
		    FOR lnQ =1  to &lcScaleFile..Cnt
		      lcQ = STR(lnQ,1)
		  	  m.QTY = EVALUATE(lcOrdLnTmp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+lcQ)
		      IF m.Qty = 0
		         LOOP 
		      ENDIF
		      * Lines Notes
		      lnNotesLine = MEMLINES(&lcOrdLnTmp..note_mem)
*!*				    lcClor = ''
*!*				    lcSize = ''
			    lcNSku = ''
			    lcVSKU = ''
			    lcDesc = ''
			    FOR lnF = 1  TO lnNotesLine 
			      IF 'SK|' $ MLINE(&lcOrdLnTmp..note_mem,lnF)
			        lcNSku = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
			      ENDIF
			      
			      IF 'VN|' $ MLINE(&lcOrdLnTmp..note_mem,lnF)
			        lcVSKU = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC( 'VN|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
			      ENDIF
			      
			      IF 'DESC|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
			        lcDesc = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5))-1)
			      ENDIF
			      
*!*				      IF  "Merchant Color:"  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
*!*				        lcClor = lcClor +IIF(EMPTY(lcClor),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),AT("Merchant Color:",MLINE(&lcOrdLnTmp..note_mem,lnF))+LEN("Merchant Color:")))  
*!*				      ENDIF
*!*				       
*!*				      IF "Merchant Size:" $ MLINE(&lcOrdLnTmp..note_mem,lnF)
*!*				        lcSize = lcSize +IIF(EMPTY(lcSize),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),AT("Merchant Size:",MLINE(&lcOrdLnTmp..note_mem,lnF))+LEN("Merchant Size:")))  
*!*				      ENDIF
			    ENDFOR 
		      * Lines Notes
		   	  m.VendorSKU = lfGetSku(&lcPackTmp..Account, &lcOrdLnTmp..STYLE,lnQ)
		   	 
		   	  IF EMPTY(ALLTRIM(m.VendorSKU))
		   	    m.VendorSKU = lcNSku 
		   	  ENDIF
		   	  IF EMPTY(ALLTRIM(m.ItemNo))
 		   	    m.ItemNo = lcVSKU 
		   	  ENDIF
		   	  IF EMPTY(ALLTRIM(m.PrductDesc))
		   	    m.PrductDesc = lcDesc 
		   	  ENDIF
		   	  SELECT (lcUpcTemp)
		   	  APPEND BLANK
		   	  GATHER MEMO MEMVAR  
		      *INSERT INTO (lcUpcTemp) FROM MEMVAR
		      
		      lfGetBarCode(EVAL(lcOrdhdr+'.CUSTPO') ,'GORDERN')
		    ENDFOR 
		  ENDSCAN 
		  
		ENDSCAN 
		SELECT (lcPackTmp)
		SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
		SET SKIP TO (lcUpcTemp)
		loogScroll.cCROrientation = 'L'

		&&SET FILTER TO ACCOUNT = "COST0"
		LOCATE 
		IF !EOF()

		  LOCATE 
		  lcFormName= "ALPKLSCO"
		  loOgScroll.lcOGLastForm = "ALPKLSCO"
		  loOgScroll.lcRpFrxMod = "Graphics"
		  lcOGTmpForm   = loogscroll.gfTempName()
		  = gfCrtFrm("ALPKLSCO","",.T.)    && Create criteria form.
		  DO gfDispRe WITH EVAL('lcFormName')
		ENDIF

		SET FILTER TO 
		llALPakLst=.F.
		
  CASE lcRpFrmCd = 'BB' OR lcRpFrmCd = 'BA'
		SELECT (lcPackTmp)
		LOCATE 
		SCAN &&FOR ACCOUNT = "BEDB0"
		  m.PACK_NO = &lcPackTmp..Pack_NO
		  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
		  lcOrderNumber= ''
		  lcOrderDate = ''
		  lcShipRef = ''
		  lcGiftMsg = ''
		  lcPayMeth = ''
		  lccOrderWare = ''
		  LCORDMSG = ''
		  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)

		  FOR lnF = 1  TO lnNotesLine 
		    IF 'Customer order number:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderNumber= lcOrderNumber+IIF(EMPTY(lcOrderNumber),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer order number:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer order number:')))  
		    ENDIF
		      
		    IF 'Cust order date:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderDate = lcOrderDate +IIF(EMPTY(lcOrderDate ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Cust order date:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Cust order date:')))  
		    ENDIF
		    IF  'Routing:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcShipRef = lcShipRef +IIF(EMPTY(lcShipRef ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Routing:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Routing:')))  
		    ENDIF
		     
		    IF 'Gift Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Gift Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Gift Message:')))  
		    ENDIF
		    IF 'Order packingSlip Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      LCORDMSG = LCORDMSG +IIF(EMPTY(LCORDMSG ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Order packingSlip Message:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Order packingSlip Message:' )))  
		    ENDIF
		    IF 'Payment method  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcPayMeth = lcPayMeth +IIF(EMPTY(lcPayMeth ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Payment method  :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Payment method  :')))      
		    ENDIF
		    IF 'Warehouse storage location number  :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lccOrderWare = lccOrderWare +IIF(EMPTY(lccOrderWare ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Warehouse storage location number  :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Warehouse storage location number  :')))      
		    ENDIF
		  ENDFOR 
		  M.ORDMSG =LCORDMSG
		  m.PayMeth = lcPayMeth
		  m.CstOrder = lcOrderNumber
		  m.cOrderDate = lcOrderDate 
		  m.cShipRef  = lcShipRef  
		  m.GIFTMSG = lcGiftMsg
		  m.cOrderWare  = lccOrderWare  
		  SELECT (lcLineTemp)
		  LOCATE 
		  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
		    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
		      LOOP
		    ENDIF
		    IF 'ORDLINE' $ DBF(lcOrdLnTmp)
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,'ORDLINE')
		    ELSE 
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
		    ENDIF
		    lcLineGIFTMSG = ''
		    lcGiftReg = '' &&
		    lnNotesLine = MEMLINES(&lcOrdLnTmp..note_mem)
		    lcClor = ''
		    lcSize = ''
		    lcNSku = ''
		    lcVSKU = ''
		    lcDesc = ''
		    lcUPC = ''
		    FOR lnF = 1  TO lnNotesLine 
		      IF 'Line Gift  Registry:'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcGiftReg = lcGiftReg +IIF(EMPTY(lcGiftReg),'',CHR(13)+CHR(10))+; 
		        SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line Gift  Registry:',MLINE(&lcOrdLnTmp..note_mem,lnF))+20,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line Gift  Registry:',MLINE(&lcOrdLnTmp..note_mem,lnF))+20))-1)
		      ENDIF
		      IF 'Line packingSlip Message:'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcLineGIFTMSG = lcLineGIFTMSG +IIF(EMPTY(lcLineGIFTMSG ),'',CHR(13)+CHR(10))+;
		        SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line packingSlip Message:',MLINE(&lcOrdLnTmp..note_mem,lnF))+25,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line packingSlip Message:',MLINE(&lcOrdLnTmp..note_mem,lnF))+25))-1)
		      ENDIF
*!*			      IF 'Line packingSlip Message:'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
*!*			        lcLineGIFTMSG = lcLineGIFTMSG +IIF(EMPTY(lcLineGIFTMSG ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),AT('Line packingSlip Message:' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+LEN('Line packingSlip Message:' )))  
*!*			      ENDIF
		      IF  'CO|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcClor = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		       
		      IF 'SZ|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcSize = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		      
	        IF 'SK|' $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcNSku = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		      
		      IF 'VN|' $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcVSKU = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC( 'VN|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		      
          IF 'DESC|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcDesc = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5))-1)
	        ENDIF
	        
	        IF 'UP|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcUPC= SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('UP|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('UP|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
	        ENDIF
		    ENDFOR 
		    
		    m.RegID = lcGiftReg 
		    m.LineGIFTMSG = lcLineGIFTMSG 
		    m.ColrSz = ALLTRIM(lcClor)+"/"+ALLTRIM(lcSize)
		    
		    m.cemail = &lcCustomer..cemail_add 
		    m.ItemNo = &lcOrdLnTmp..STYLE
		    m.PrductDesc = &lcOrdLnTmp..DESC1
	      IF EMPTY(ALLTRIM(m.PrductDesc))
	   	    m.PrductDesc = lcDesc 
	   	  ENDIF
		    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)) &&&lcLineTemp..nordlineno
		    FOR lnQ =1  to &lcScaleFile..Cnt
		      lcQ = STR(lnQ,1)
		  	  m.QTY = EVALUATE(lcOrdLnTmp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+lcQ)
		      IF m.Qty = 0
		         LOOP 
		      ENDIF
		   	  m.VendorSKU = lfGetSku(&lcPackTmp..Account, &lcOrdLnTmp..STYLE,lnQ)
		   	  IF EMPTY(ALLTRIM(m.VendorSKU))
		   	    m.VendorSKU = lcNSku 
		   	  ENDIF
		   	  
		      m.VendPart = lfGetUPC(&lcOrdLnTmp..STYLE,lnQ)
		     
		      IF EMPTY(ALLTRIM(m.VendPart))
		   	    m.VendPart = lcUPC 
		   	  ENDIF
		   	  
		      IF EMPTY(ALLTRIM(m.ItemNo))
		   	    m.ItemNo= lcVSKU 
		   	  ENDIF
		      
		     * INSERT INTO (lcUpcTemp) FROM MEMVAR
		      SELECT (lcUpcTemp)
		   	  APPEND BLANK
		   	  GATHER MEMO MEMVAR 
		     ENDFOR 
		  ENDSCAN 
		  
		ENDSCAN 

		loogScroll.cCROrientation = 'P'
		SELECT (lcPackTmp)
		SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
		SET SKIP TO (lcUpcTemp)		
		*SET FILTER TO ACCOUNT = "BEDB0"
		LOCATE 
		IF !EOF()
			lcFormName= "ALPKLS"+lcRpFrmCd
			loOgScroll.lcOGLastForm = "ALPKLS"+lcRpFrmCd
			loOgScroll.lcRpFrxMod = "Graphics"
			lcOGTmpForm   = loogscroll.gfTempName()
			= gfCrtFrm("ALPKLS"+lcRpFrmCd,"",.T.)    && Create criteria form.
		*!*	  SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
		*!*	  SET SKIP TO (lcUpcTemp)
		*!*	  LOCATE 
		  DO gfDispRe WITH EVAL('lcFormName')
		ENDIF
		SET FILTER TO 
		llALPakLst=.F.
  *Belk
  CASE lcRpFrmCd = 'BK'
  SET STEP ON 
		SELECT (lcPackTmp)
		LOCATE 
		SCAN &&FOR ACCOUNT = "COST0"
		  m.PACK_NO = &lcPackTmp..Pack_NO
		  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
		  lcOrderNumber= ''
		  lcOrderDate = ''
		  lcShipRef = ''
		  lcGiftMsg = ''
		  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)

		  FOR lnF = 1  TO lnNotesLine 
		    IF 'Customer order number:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderNumber= lcOrderNumber+IIF(EMPTY(lcOrderNumber),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer order number:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer order number:')))  
		    ENDIF
		      
		    IF 'Cust order date:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderDate = lcOrderDate +IIF(EMPTY(lcOrderDate ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Cust order date:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Cust order date:')))  
		    ENDIF
		    IF  'Routing:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcShipRef = lcShipRef +IIF(EMPTY(lcShipRef ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Routing:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Routing:')))  
		    ENDIF
		     
		    IF 'Order packingSlip Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Order packingSlip Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Order packingSlip Message:')))  
		    ENDIF
		  ENDFOR 
		  m.CstOrder = lcOrderNumber
		  m.cOrderDate = lcOrderDate 
		  m.cShipRef  = lcShipRef  
		  m.GIFTMSG = lcGiftMsg 
		  SELECT (lcLineTemp)
		  LOCATE 
		  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
		    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
		      LOOP
		    ENDIF
		    IF 'ORDLINE' $ DBF(lcOrdLnTmp)
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,'ORDLINE')
		    ELSE 
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
		    ENDIF
		    lcCB = ''
		    lcClor = ''
		    lcSize = ''
 		    lcLineGIFTMSG  = ''
		    FOR lnF = 1  TO lnNotesLine 
		      
		      IF  'CO|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcClor = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		       
		      IF 'SZ|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcSize = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		      
		      IF 'DESC|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcDesc = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5))-1)
	        ENDIF
	        
		      IF 'Line packingSlip Message:'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcLineGIFTMSG = lcLineGIFTMSG +IIF(EMPTY(lcLineGIFTMSG ),'',CHR(13)+CHR(10))+;
		        SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line packingSlip Message:',MLINE(&lcOrdLnTmp..note_mem,lnF))+25,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line packingSlip Message:',MLINE(&lcOrdLnTmp..note_mem,lnF))+25))-1)
		      ENDIF
		      
		     		     
		      IF 'CB|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcCB = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CB|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CB|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
	        ENDIF
		    ENDFOR 
		    
		    m.LineGIFTMSG = lcLineGIFTMSG 
		    m.Color  = lcClor 
		    m.Size  = lcSize 
		    m.cemail = &lcCustomer..cemail_add 
		    m.ItemNo = &lcOrdLnTmp..STYLE
		    
		    m.PrductDesc = &lcOrdLnTmp..DESC1
		    
		    IF EMPTY(ALLTRIM(m.PrductDesc))
	   	    m.PrductDesc = lcDesc 
	   	  ENDIF
	   	  
		    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)) &&&lcLineTemp..nordlineno
		    FOR lnQ =1  to &lcScaleFile..Cnt
		      lcQ = STR(lnQ,1)
		  	  m.QTY = EVALUATE(lcOrdLnTmp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+lcQ)
		      IF m.Qty = 0
		         LOOP 
		      ENDIF
		   	  m.VendorSKU = lcCB &&lfGetSku(&lcPackTmp..Account, &lcOrdLnTmp..STYLE,lnQ)
		      *INSERT INTO (lcUpcTemp) FROM MEMVAR
		      SELECT (lcUpcTemp)
		   	  APPEND BLANK
		   	  GATHER MEMO MEMVAR 
		      lfGetBarCode('0888'+SUBSTR(ALLTRIM(m.CstOrder),1,8)+'0000000000' ,'GORDERN')
		      lfGetBarCode(SUBSTR(ALLTRIM(&lcPackTmp..Store),1,4)+SUBSTR(ALLTRIM(m.CstOrder),1,8)+'0000000000' ,'GORDRET')
		      
		    ENDFOR 
		  ENDSCAN 
		  
		ENDSCAN 
		SELECT (lcPackTmp)
		SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
		SET SKIP TO (lcUpcTemp)
		loogScroll.cCROrientation = 'P'

		&&SET FILTER TO ACCOUNT = "COST0"
		LOCATE 
		IF !EOF()
  	  LOCATE 
		  lcFormName= "ALPKLSBK"
		  loOgScroll.lcOGLastForm = "ALPKLSBK"
		  loOgScroll.lcRpFrxMod = "Graphics"
		  lcOGTmpForm   = loogscroll.gfTempName()
		  = gfCrtFrm("ALPKLSBK","",.T.)    && Create criteria form.
		  DO gfDispRe WITH EVAL('lcFormName')
		ENDIF

		SET FILTER TO 
		llALPakLst=.F.
&& HD
 CASE lcRpFrmCd = 'HD'
		SELECT (lcPackTmp)
		LOCATE 
		SCAN &&FOR ACCOUNT = "COST0"
		  m.PACK_NO = &lcPackTmp..Pack_NO
		  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
		  lcOrderNumber= ''
		  lcOrderDate = ''
		  lcShipRef = ''
		  lcGiftMsg = ''
		  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)
      lccOrderWare =''
      lcCustName =''
		  FOR lnF = 1  TO lnNotesLine 
		    IF 'Customer order number:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderNumber= lcOrderNumber+IIF(EMPTY(lcOrderNumber),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer order number:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer order number:')))  
		    ENDIF
		      
		    IF 'Cust order date:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderDate = lcOrderDate +IIF(EMPTY(lcOrderDate ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Cust order date:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Cust order date:')))  
		    ENDIF
		    IF  'Routing:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcShipRef = lcShipRef +IIF(EMPTY(lcShipRef ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Routing:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Routing:')))  
		    ENDIF
		     
		    IF 'Gift Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Gift Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Gift Message:')))  
		    ENDIF
		    IF 'Warehouse storage location number  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lccOrderWare = lccOrderWare +IIF(EMPTY(lccOrderWare ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Warehouse storage location number  :'  ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Warehouse storage location number  :')))      
		    ENDIF	
		    IF 'Customer Name :' $ MLINE(&lcTempNotePad..mnotes,lnF)
  		    lcCustName = lcCustName +IIF(EMPTY(lccOrderWare ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer Name :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer Name :')))      
		    ENDIF 
*!*			    IF 'Order packingSlip Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
*!*			      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Order packingSlip Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Order packingSlip Message:')))  
*!*			    ENDIF	    
		  ENDFOR 
		  m.CUSTNAME =lcCustName
		  m.CstOrder = lcOrderNumber
		  m.cOrderDate = lcOrderDate 
		  m.cShipRef  = lcShipRef  
		  m.GIFTMSG = lcGiftMsg 
		  M.cOrderWare = lccOrderWare 
		  SELECT (lcLineTemp)
		  LOCATE 
		  lnX = 0
		  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
		    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
		      LOOP
		    ENDIF
		    
		    IF 'ORDLINE' $ DBF(lcOrdLnTmp)
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,'ORDLINE')
		    ELSE 
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
		    ENDIF
  	    m.cemail = &lcCustomer..cemail_add 
		    m.ItemNo = &lcOrdLnTmp..STYLE
		    m.PrductDesc = &lcOrdLnTmp..DESC1
		    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)) &&&lcLineTemp..nordlineno
		    
		    lcNSku = ''
		    lcVSKU = ''
		    lcDesc = ''
		    lcUPC = ''
		    lcPI = ''
		    FOR lnF = 1  TO lnNotesLine 

		   
		     
		      
	        IF 'SK|' $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcNSku = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		      
		      IF 'VN|' $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcVSKU = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC( 'VN|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		      
          IF 'DESC|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcDesc = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5))-1)
	        ENDIF
	        
	        IF 'UP|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcUPC= SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('UP|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('UP|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
	        ENDIF
	        IF  'PI|' $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcPI= SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('PI|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('PI|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
	        ENDIF
		    ENDFOR 
		    
*!*			    m.LineGIFTMSG = lcLineGIFTMSG 
*!*			    m.Color  = lcClor 
*!*			    m.Size  = lcSize 
		    m.cemail = &lcCustomer..cemail_add 
		    m.ItemNo = lcPI
		    
		    m.PrductDesc = &lcOrdLnTmp..DESC1
		    
		    IF EMPTY(ALLTRIM(m.PrductDesc))
	   	    m.PrductDesc = lcDesc 
	   	  ENDIF
		    
		    FOR lnQ =1  to &lcScaleFile..Cnt
		      lcQ = STR(lnQ,1)
		  	  m.QTY = EVALUATE(lcOrdLnTmp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+lcQ)
		      IF m.Qty = 0
		         LOOP 
		      ENDIF
		      lcX = STR(lnX,1)
		   	  m.VendorSKU = &lcOrdLnTmp..STYLE &&lfGetSku(&lcPackTmp..Account, &lcOrdLnTmp..STYLE,lnQ)
		   	  IF EMPTY(ALLTRIM(m.VendorSKU))
		   	    m.VendorSKU =lcVSKU 
		   	  ENDIF
		   	  IF lnX > 0
		   	    REPLACE VendorSKU&lcX. WITH  m.VendorSKU IN (lcUpcTemp)
 		   	    REPLACE ItemNo&lcX.  WITH   m.ItemNo IN (lcUpcTemp)
 		   	    REPLACE PrductDesc&lcX.  WITH m.PrductDesc IN (lcUpcTemp)
 		   	    REPLACE QTY&lcX. WITH  m.QTY IN (lcUpcTemp)
		   	  ELSE 
  		     * INSERT INTO (lcUpcTemp) FROM MEMVAR
  		      SELECT (lcUpcTemp)
		   	    APPEND BLANK
		   	    GATHER MEMO MEMVAR 
  		    ENDIF  
		      lnX = lnX +  1
		      IF lnX > 8
		        lnX = 0
		      ENDIF
		      IF !EOF(lcUpcTemp) AND  !BOF(lcUpcTemp)
		        lfGetBarCode(EVAL(lcOrdhdr+'.CUSTPO') ,'GORDERN')
		      ENDIF  
		    ENDFOR 
		  ENDSCAN 
		  
		ENDSCAN 
		SELECT (lcPackTmp)
		SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
		SET SKIP TO (lcUpcTemp)
		loogScroll.cCROrientation = 'L'

		&&SET FILTER TO ACCOUNT = "COST0"
		LOCATE 
		IF !EOF()

		  LOCATE 
		  lcFormName= "ALPKLSHD"
		  loOgScroll.lcOGLastForm = "ALPKLSHD"
		  loOgScroll.lcRpFrxMod = "Graphics"
		  lcOGTmpForm   = loogscroll.gfTempName()
		  = gfCrtFrm("ALPKLSHD","",.T.)    && Create criteria form.
		  DO gfDispRe WITH EVAL('lcFormName')
		ENDIF

		SET FILTER TO 
		llALPakLst=.F.
	*HOT TOPIC 
	 CASE lcRpFrmCd = 'HT' OR lcRpFrmCd = 'BL'
		SELECT (lcPackTmp)
		LOCATE 
		SCAN &&FOR ACCOUNT = "COST0"
		  m.PACK_NO = &lcPackTmp..Pack_NO
		  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
		  lcOrderNumber= ''
		  lcOrderDate = ''
		  lcShipRef = ''
		  lcGiftMsg = ''
		  lcPayMeth =  ''
		  lcEmailCS = ''
		  lcCSPhone = ''
		  lcCSURL = ''
		  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)

		  FOR lnF = 1  TO lnNotesLine 
		    IF 'Customer order number:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderNumber= lcOrderNumber+IIF(EMPTY(lcOrderNumber),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer order number:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer order number:')))  
		    ENDIF
		      
		    IF 'Cust order date:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderDate = lcOrderDate +IIF(EMPTY(lcOrderDate ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Cust order date:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Cust order date:')))  
		    ENDIF
		    IF  'Routing:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcShipRef = lcShipRef +IIF(EMPTY(lcShipRef ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Routing:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Routing:')))  
		    ENDIF
		     
		    IF 'Order packingSlip Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Order packingSlip Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Order packingSlip Message:')))  
		    ENDIF
		    IF 'Payment method  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcPayMeth = lcPayMeth +IIF(EMPTY(lcPayMeth ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Payment method  :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Payment method  :')))      
		    ENDIF
		    IF 'Electronic Mail  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcEmailCS  = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Electronic Mail  :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Electronic Mail  :')))      
		    ENDIF
		    IF  'Customer Service Phone :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcCSPhone = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Customer Service Phone :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Customer Service Phone :')))      
		    ENDIF

        IF 'Customer Service URL :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcCSURL = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Customer Service URL :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Customer Service URL :' )))      
		    ENDIF

		  ENDFOR 
		  m.CSURL = lcCSURL 
		  m.CSEMAIL = lcEmailCS  
		  m.CSPHONE = lcCSPhone 
      m.PayMeth = lcPayMeth 
		  m.CstOrder = lcOrderNumber
		  m.cOrderDate = lcOrderDate 
		  m.cShipRef  = lcShipRef  
		  m.GIFTMSG = lcGiftMsg 
		  SELECT (lcLineTemp)
		  LOCATE 
		  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
		    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
		      LOOP
		    ENDIF
		    IF 'ORDLINE' $ DBF(lcOrdLnTmp)
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,'ORDLINE')
		    ELSE 
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
		    ENDIF
		    lcVnSK = ''
		    lcMSKU = ''
		    lcSize = ''
 		    lcLineGIFTMSG  = ''
 		    lcClor = ''
 		    lcSize = ''
 		    lcPrice = ''
		    FOR lnF = 1  TO lnNotesLine 
		      
		      IF  'VN|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcVnSK = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|'   ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
*!*			       
		      IF 'SK|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcMSKU = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
*!*			      
		      IF 'DESC|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcDesc = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5))-1)
	        ENDIF
          IF  'CO|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcClor = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		       
		      IF 'SZ|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcSize = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF	        
*!*			      IF 'Line packingSlip Message:'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
*!*			        lcLineGIFTMSG = lcLineGIFTMSG +IIF(EMPTY(lcLineGIFTMSG ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),AT('Line packingSlip Message:' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+LEN('Line packingSlip Message:')))  
*!*			      ENDIF
		      
		     		     
		      IF 'Retail Price|'    $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcPrice = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Retail Price|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+13,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Retail Price|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+13))-1)
	        ENDIF
		    ENDFOR 
		    
*!*			    m.LineGIFTMSG = lcLineGIFTMSG 
		    m.Color  = lcClor 
		    m.Size  = lcSize 
		    m.PRICE= VAL(ALLTRIM(lcPrice))
*!*			    m.cemail = &lcCustomer..cemail_add 
		    m.ItemNo = lcMSKU 
		    
		    m.PrductDesc = &lcOrdLnTmp..DESC1
		    
		    IF EMPTY(ALLTRIM(m.PrductDesc))
	   	    m.PrductDesc = lcDesc 
	   	  ENDIF
	   	  
		    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)) &&&lcLineTemp..nordlineno
		    FOR lnQ =1  to &lcScaleFile..Cnt
		      lcQ = STR(lnQ,1)
		  	  m.QTY = EVALUATE(lcOrdLnTmp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+lcQ)
		      IF m.Qty = 0
		         LOOP 
		      ENDIF
		   	  m.VendorSKU = &lcOrdLnTmp..STYLE
		   	  IF EMPTY( m.VendorSKU)
		   	    m.VendorSKU = lcVnSK 
		   	  ENDIF
		   	  &&lcCB &&lfGetSku(&lcPackTmp..Account, &lcOrdLnTmp..STYLE,lnQ)
		      *INSERT INTO (lcUpcTemp) FROM MEMVAR
		       SELECT (lcUpcTemp)
		   	   APPEND BLANK
		   	   GATHER MEMO MEMVAR 
		     * lfGetBarCode('0888'+SUBSTR(ALLTRIM(m.CstOrder),1,8)+'0000000000' ,'GORDERN')
		      *lfGetBarCode(SUBSTR(ALLTRIM(&lcPackTmp..Store),1,4)+SUBSTR(ALLTRIM(m.CstOrder),1,8)+'0000000000' ,'GORDRET')
		      
		    ENDFOR 
		  ENDSCAN 
		  
		ENDSCAN 
		SELECT (lcPackTmp)
		SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
		SET SKIP TO (lcUpcTemp)
		loogScroll.cCROrientation = 'P'

		&&SET FILTER TO ACCOUNT = "COST0"
		LOCATE 
		IF !EOF()
  	  LOCATE 
		  lcFormName= "ALPKLS"+lcRpFrmCd
		  loOgScroll.lcOGLastForm =  "ALPKLS"+lcRpFrmCd
		  loOgScroll.lcRpFrxMod = "Graphics"
		  lcOGTmpForm   = loogscroll.gfTempName()
		  = gfCrtFrm("ALPKLS"+lcRpFrmCd,"",.T.)    && Create criteria form.
		  DO gfDispRe WITH EVAL('lcFormName')
		ENDIF

		SET FILTER TO 
		llALPakLst=.F.	
 ** JC Penny
 CASE lcRpFrmCd = 'JC'
		SELECT (lcPackTmp)
		LOCATE 
		SCAN &&FOR ACCOUNT = "COST0"
		  m.PACK_NO = &lcPackTmp..Pack_NO
		  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
		  lcOrderNumber= ''
		  lcOrderDate = ''
		  lcShipRef = ''
		  lcGiftMsg = ''
		  lcPayMeth =  ''
		  lcEmailCS = ''
		  lcCSPhone = ''
		  lcCSURL = ''
		  
		  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)
			lcCustName  = ''
		  FOR lnF = 1  TO lnNotesLine 
		    IF 'Customer order number:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderNumber= lcOrderNumber+IIF(EMPTY(lcOrderNumber),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer order number:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer order number:')))  
		    ENDIF
		      
		    IF 'Cust order date:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderDate = lcOrderDate +IIF(EMPTY(lcOrderDate ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Cust order date:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Cust order date:')))  
		    ENDIF
		    IF  'Routing:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcShipRef = lcShipRef +IIF(EMPTY(lcShipRef ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Routing:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Routing:')))  
		    ENDIF
		     
		    IF 'Order packingSlip Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Order packingSlip Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Order packingSlip Message:')))  
		    ENDIF
		    IF 'Payment method  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcPayMeth = lcPayMeth +IIF(EMPTY(lcPayMeth ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Payment method  :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Payment method  :')))      
		    ENDIF
*!*			    IF 'Electronic Mail  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
*!*			      lcEmailCS  = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Electronic Mail  :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Electronic Mail  :')))      
*!*			    ENDIF
*!*			    IF 'CS Phone :' $ MLINE(&lcTempNotePad..mnotes,lnF)
*!*			      lcCSPhone = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('CS Phone :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('CS Phone :')))      
*!*			    ENDIF

*!*	        IF 'Customer Service URL :' $ MLINE(&lcTempNotePad..mnotes,lnF)
*!*			      lcCSURL = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Customer Service URL :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Customer Service URL :' )))      
*!*			    ENDIF
        IF 'Customer Name :' $ MLINE(&lcTempNotePad..mnotes,lnF)
  		    lcCustName = lcCustName +IIF(EMPTY(lcCustName ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer Name :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer Name :')))      
		    ENDIF 	
		  ENDFOR 
		  m.CustName = lcCustName 
		  m.CSURL = lcCSURL 
		  m.CSEMAIL = lcEmailCS  
		  m.CSPHONE = lcCSPhone 
      m.PayMeth = lcPayMeth 
		  m.CstOrder = lcOrderNumber
		  m.cOrderDate = lcOrderDate 
		  m.cShipRef  = lcShipRef  
		  m.GIFTMSG = lcGiftMsg 
		  SELECT (lcLineTemp)
		  LOCATE 
		  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
		    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
		      LOOP
		    ENDIF
		    IF 'ORDLINE' $ DBF(lcOrdLnTmp)
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,'ORDLINE')
		    ELSE 
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
		    ENDIF
		    lcVnSK = ''
		    lcMSKU = ''
		    lcSize = ''
 		    lcLineGIFTMSG  = ''
 		    lcClor = ''
 		    lcSize = ''
 		    lcPrice = ''
 		    lcCB = ''
 		    lcPrsData = ''
		    FOR lnF = 1  TO lnNotesLine 
		      
		      IF  'VN|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcVnSK = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|'   ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
*!*			       
		      IF 'SK|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcMSKU = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
*!*			      
		      IF 'DESC|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcDesc = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5))-1)
	        ENDIF
          IF  'CO|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcClor = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		       
		      IF 'SZ|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcSize = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF	        
*!*			      IF 'Line packingSlip Message:'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
*!*			        lcLineGIFTMSG = lcLineGIFTMSG +IIF(EMPTY(lcLineGIFTMSG ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),AT('Line packingSlip Message:' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+LEN('Line packingSlip Message:')))  
*!*			      ENDIF
	       IF 'CB|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcCB = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CB|'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CB|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
	        ENDIF	      
		     		     
		      IF 'Personalization Data:'     $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcPrsData = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Personalization Data:' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+21,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Personalization Data:'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+21))-1)
	        ENDIF
		    ENDFOR 
		    m.PrsData = lcPrsData 
*!*			    m.LineGIFTMSG = lcLineGIFTMSG 
		    m.Color  = IIF(!EMPTY(lcClor),ALLTRIM(lcClor)+",",'')
		    m.Size  = IIF(!EMPTY(lcSize),ALLTRIM(lcSize)+",",'')
		    m.PRICE= VAL(ALLTRIM(lcPrice))
*!*			    m.cemail = &lcCustomer..cemail_add 
		    m.ItemNo = lcMSKU 
		    m.VendPart = lcCB 
		    m.PrductDesc = &lcOrdLnTmp..DESC1
		    
		    IF EMPTY(ALLTRIM(m.PrductDesc))
	   	    m.PrductDesc = lcDesc 
	   	  ENDIF
	   	  
		    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)) &&&lcLineTemp..nordlineno
		    FOR lnQ =1  to &lcScaleFile..Cnt
		      lcQ = STR(lnQ,1)
		  	  m.QTY = EVALUATE(lcOrdLnTmp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+lcQ)
		      IF m.Qty = 0
		         LOOP 
		      ENDIF
		   	  m.VendorSKU = &lcOrdLnTmp..STYLE
		   	  IF EMPTY( m.VendorSKU)
		   	    m.VendorSKU = lcVnSK 
		   	  ENDIF
		   	  &&lcCB &&lfGetSku(&lcPackTmp..Account, &lcOrdLnTmp..STYLE,lnQ)
		      *INSERT INTO (lcUpcTemp) FROM MEMVAR
		       SELECT (lcUpcTemp)
		   	   APPEND BLANK
		   	   GATHER MEMO MEMVAR 
		     * lfGetBarCode('0888'+SUBSTR(ALLTRIM(m.CstOrder),1,8)+'0000000000' ,'GORDERN')
		      *lfGetBarCode(SUBSTR(ALLTRIM(&lcPackTmp..Store),1,4)+SUBSTR(ALLTRIM(m.CstOrder),1,8)+'0000000000' ,'GORDRET')
		      
		    ENDFOR 
		  ENDSCAN 
		  
		ENDSCAN 
		SELECT (lcPackTmp)
		SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
		SET SKIP TO (lcUpcTemp)
		loogScroll.cCROrientation = 'P'

		&&SET FILTER TO ACCOUNT = "COST0"
		LOCATE 
		IF !EOF()
  	  LOCATE 
		  lcFormName= "ALPKLSJC"
		  loOgScroll.lcOGLastForm = "ALPKLSJC"
		  loOgScroll.lcRpFrxMod = "Graphics"
		  lcOGTmpForm   = loogscroll.gfTempName()
		  = gfCrtFrm("ALPKLSJC","",.T.)    && Create criteria form.
		  DO gfDispRe WITH EVAL('lcFormName')
		ENDIF

		SET FILTER TO 
		llALPakLst=.F.
		****** MACY's
	 CASE lcRpFrmCd = 'MC' OR lcRpFrmCd ='BD'
		SELECT (lcPackTmp)
		LOCATE 
		SCAN &&FOR ACCOUNT = "COST0"
		  m.PACK_NO = &lcPackTmp..Pack_NO
		  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
		  lcOrderNumber= ''
		  lcOrderDate = ''
		  lcShipRef = ''
		  lcGiftMsg = ''
		  lcPayMeth =  ''
		  lcEmailCS = ''
		  lcCSPhone = ''
		  lcCSURL = ''
		  lcRECEIPTID = ''
		  lcGiftReg = ''
		  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)

		  FOR lnF = 1  TO lnNotesLine 
		    
		    IF 'Gift Registry:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcGiftReg = lcGiftReg +IIF(EMPTY(lcGiftReg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Gift Registry:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Gift Registry:')))  
		    ENDIF
		    
		    IF 'Receipt ID :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcRECEIPTID = lcRECEIPTID +IIF(EMPTY(lcRECEIPTID ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Receipt ID :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Receipt ID :')))  
		    ENDIF
		    
		    IF 'Customer order number:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderNumber= lcOrderNumber+IIF(EMPTY(lcOrderNumber),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer order number:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer order number:')))  
		    ENDIF
		      
		    IF 'Cust order date:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderDate = lcOrderDate +IIF(EMPTY(lcOrderDate ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Cust order date:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Cust order date:')))  
		    ENDIF
		    IF  'Routing:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcShipRef = lcShipRef +IIF(EMPTY(lcShipRef ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Routing:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Routing:')))  
		    ENDIF
		     
		    IF 'Gift Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Gift Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Gift Message:')))  
		    ENDIF
		    &&
*!*			     IF 'Order packingSlip Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
*!*			      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Order packingSlip Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Order packingSlip Message:')))  
*!*			    ENDIF
		    IF 'Payment method  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcPayMeth = lcPayMeth +IIF(EMPTY(lcPayMeth ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Payment method  :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Payment method  :')))      
		    ENDIF
		    IF 'Electronic Mail  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcEmailCS  = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Electronic Mail  :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Electronic Mail  :')))      
		    ENDIF
		    IF  'Customer Service Phone :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcCSPhone = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer Service Phone :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer Service Phone :')))      
		    ENDIF

            IF 'Customer Service URL :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcCSURL = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Customer Service URL :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Customer Service URL :' )))      
		    ENDIF

		  ENDFOR 
		  m.RegID = lcGiftReg 
		  m.RECEIPTID = lcRECEIPTID 
		  m.CSURL = lcCSURL 
		  m.CSEMAIL = lcEmailCS  
		  m.CSPHONE = lcCSPhone 
          m.PayMeth = lcPayMeth 
		  m.CstOrder = lcOrderNumber
		  m.cOrderDate = lcOrderDate 
		  m.cShipRef  = lcShipRef  
		  m.GIFTMSG = lcGiftMsg 
		  SELECT (lcLineTemp)
		  LOCATE 
		  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
		    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
		      LOOP
		    ENDIF
		    IF 'ORDLINE' $ DBF(lcOrdLnTmp)
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,'ORDLINE')
		    ELSE 
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
		    ENDIF
		    lcVnSK = ''
		    lcMSKU = ''
		    lcSize = ''
 		    lcLineGIFTMSG  = ''
 		    lcClor = ''
 		    lcSize = ''
 		    lcPrice = ''
 		    lcLine = ''
		    FOR lnF = 1  TO lnNotesLine 
		      
		      IF  'VN|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcVnSK = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|'   ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
*!*			       
		      IF 'UP|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcMSKU = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('UP|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('UP|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
*!*			      
		      IF 'DESC|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	            lcDesc = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5))-1)
	          ENDIF
              IF  'CO|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcClor = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		       
		      IF 'SZ|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcSize = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF	        
		      IF 'LN|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		         lcLine = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('LN|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('LN|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		      
		     		     
		      IF 'Retail Price|'    $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcPrice = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Retail Price|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+13,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Retail Price|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+13))-1)
	        ENDIF
		    ENDFOR 
		    
*!*			    m.LineGIFTMSG = lcLineGIFTMSG 
		    m.Color  = lcClor 
		    m.Size  = lcSize 
		    m.PRICE= VAL(ALLTRIM(lcPrice))
*!*			    m.cemail = &lcCustomer..cemail_add 
		    m.ItemNo = lcMSKU 
		    m.VendPart =lcLine 
		    m.PrductDesc = &lcOrdLnTmp..DESC1
		    
		    IF EMPTY(ALLTRIM(m.PrductDesc))
	   	    m.PrductDesc = lcDesc 
	   	  ENDIF
	   	  
		    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)) &&&lcLineTemp..nordlineno
		    FOR lnQ =1  to &lcScaleFile..Cnt
		      lcQ = STR(lnQ,1)
		  	  m.QTY = EVALUATE(lcOrdLnTmp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+lcQ)
		      IF m.Qty = 0
		         LOOP 
		      ENDIF
		   	  m.VendorSKU = &lcOrdLnTmp..STYLE
		   	  IF EMPTY( m.VendorSKU)
		   	    m.VendorSKU = lcVnSK 
		   	  ENDIF
		   	  &&lcCB &&lfGetSku(&lcPackTmp..Account, &lcOrdLnTmp..STYLE,lnQ)
		      *INSERT INTO (lcUpcTemp) FROM MEMVAR
		       SELECT (lcUpcTemp)
		   	   APPEND BLANK
		   	   GATHER MEMO MEMVAR 
		      lfGetBarCode(m.RegID ,'GORDERN')
		      *lfGetBarCode(SUBSTR(ALLTRIM(&lcPackTmp..Store),1,4)+SUBSTR(ALLTRIM(m.CstOrder),1,8)+'0000000000' ,'GORDRET')
		      
		    ENDFOR 
		  ENDSCAN 
		  
		ENDSCAN 
		SELECT (lcPackTmp)
		SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
		SET SKIP TO (lcUpcTemp)
		loogScroll.cCROrientation = 'P'

		&&SET FILTER TO ACCOUNT = "COST0"
		LOCATE 
		IF !EOF()
  	  LOCATE 
		  lcFormName= "ALPKLS"+lcRpFrmCd
		  loOgScroll.lcOGLastForm = "ALPKLS"+lcRpFrmCd
		  loOgScroll.lcRpFrxMod = "Graphics"
		  lcOGTmpForm   = loogscroll.gfTempName()
		  = gfCrtFrm("ALPKLS"+lcRpFrmCd,"",.T.)    && Create criteria form.
		  DO gfDispRe WITH EVAL('lcFormName')
		ENDIF

		SET FILTER TO 
		llALPakLst=.F.
		*** QVC
	 CASE lcRpFrmCd = 'QV'
		SELECT (lcPackTmp)
		LOCATE 
		SCAN &&FOR ACCOUNT = "COST0"
		  m.PACK_NO = &lcPackTmp..Pack_NO
		  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
		  lcOrderNumber= ''
		  lcOrderDate = ''
		  lcShipRef = ''
		  lcGiftMsg = ''
		  lcPayMeth =  ''
		  lcEmailCS = ''
		  lcCSPhone = ''
		  lcCSURL = ''
		  LCMEMBERID = ''
		  
		  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)

		  FOR lnF = 1  TO lnNotesLine 
		    IF  "Customer membership ID :"  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      LCMEMBERID = LCMEMBERID +IIF(EMPTY(LCMEMBERID ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( "Customer membership ID :",MLINE(&lcTempNotePad..mnotes,lnF))+LEN( "Customer membership ID :")))  
		    ENDIF
		      
		    IF 'Customer order number:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderNumber= lcOrderNumber+IIF(EMPTY(lcOrderNumber),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Customer order number:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Customer order number:')))  
		    ENDIF
		      
		    IF 'Cust order date:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcOrderDate = lcOrderDate +IIF(EMPTY(lcOrderDate ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Cust order date:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Cust order date:')))  
		    ENDIF
		    IF  'Routing:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcShipRef = lcShipRef +IIF(EMPTY(lcShipRef ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Routing:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Routing:')))  
		    ENDIF
		     
		    IF 'Order packingSlip Message:' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Order packingSlip Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Order packingSlip Message:')))  
		    ENDIF
		    IF 'Payment method  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcPayMeth = lcPayMeth +IIF(EMPTY(lcPayMeth ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Payment method  :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Payment method  :')))      
		    ENDIF
		    IF 'Electronic Mail  :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcEmailCS  = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Electronic Mail  :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Electronic Mail  :')))      
		    ENDIF
		    IF  'Customer service Telephone :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcCSPhone = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Customer service Telephone :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Customer service Telephone :')))      
		    ENDIF

        IF 'Customer Service URL :' $ MLINE(&lcTempNotePad..mnotes,lnF)
		      lcCSURL = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Customer Service URL :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Customer Service URL :' )))      
		    ENDIF

		  ENDFOR 
		  IF EMPTY(ALLTRIM(LCMEMBERID))
		    LCMEMBERID = lcOrderNumber
		  ENDIF
		  m.MEMBERID = LCMEMBERID
		  m.CSURL = lcCSURL 
		  m.CSEMAIL = lcEmailCS  
		  m.CSPHONE = lcCSPhone 
      m.PayMeth = lcPayMeth 
		  m.CstOrder = lcOrderNumber
		  m.cOrderDate = lcOrderDate 
		  m.cShipRef  = lcShipRef  
		  m.GIFTMSG = lcGiftMsg 
		  SELECT (lcLineTemp)
		  LOCATE 
		  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
		    IF SEEK(&lcPackTmp..Pack_NO+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcUpcTemp,lcUpcTemp)
		      LOOP
		    ENDIF
		    IF 'ORDLINE' $ DBF(lcOrdLnTmp)
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,'ORDLINE')
		    ELSE 
		      =SEEK('O'+&lcPackTmp..ORDER+STR(IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)),6),lcOrdLnTmp,lcOrdLnTmp)
		    ENDIF
		    lcVnSK = ''
		    lcMSKU = ''
		    lcSize = ''
 		    lcLineGIFTMSG  = ''
 		    lcClor = ''
 		    lcSize = ''
 		    lcPrice = ''
 		    lcCB = ''
 		    lcLineTAX = ''
 		    lcLineChrge = ''
 		    LCLinecred = ''
 		    lcDesc = ''
		    FOR lnF = 1  TO lnNotesLine 
		      
		      IF  'VN|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcVnSK = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('VN|'   ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
*!*			       
		      IF 'SK|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcMSKU = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SK|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
*!*			      
		      IF 'DESC|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcDesc = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('DESC|',MLINE(&lcOrdLnTmp..note_mem,lnF))+5))-1)
	        ENDIF
          IF  'CO|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcClor = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CO|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		       
		      IF 'SZ|'  $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcSize = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|',MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('SZ|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF	
		              
		      IF 'Line-item shipping charge:'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcLineChrge = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line-item shipping charge:',MLINE(&lcOrdLnTmp..note_mem,lnF))+26,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line-item shipping charge:' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+26))-1)
		      ENDIF
		      
		      IF  'CB|'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
		        lcCB = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CB|'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('CB|'   ,MLINE(&lcOrdLnTmp..note_mem,lnF))+3))-1)
		      ENDIF
		     		     
		      IF 'Retail Price|'    $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcPrice = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Retail Price|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+13,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Retail Price|' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+13))-1)
	        ENDIF
	        IF 'Line Tax:'     $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          lcLineTAX = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line Tax:' ,MLINE(&lcOrdLnTmp..note_mem,lnF))+9,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line Tax:'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+9))-1)
	        ENDIF
	        IF 'Line Credit:'   $ MLINE(&lcOrdLnTmp..note_mem,lnF)
	          LCLinecred = SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line Credit:'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+12,ATC('|',SUBSTR(MLINE(&lcOrdLnTmp..note_mem,lnF),ATC('Line Credit:'  ,MLINE(&lcOrdLnTmp..note_mem,lnF))+12))-1)
	        ENDIF
		    ENDFOR 
		    m.regId = lcMSKU
		    M.Linecred =  VAL(LCLinecred)
		    M.LineTAX = VAL(lcLineTAX ) 
		    m.LineChrge = VAL(lcLineChrge)
*!*			    m.LineGIFTMSG = lcLineGIFTMSG 
		    m.Color  = lcClor 
		    m.Size  = lcSize 
		    m.PRICE= VAL(ALLTRIM(lcPrice))
*!*			    m.cemail = &lcCustomer..cemail_add 
		    m.ItemNo = lcCB 
		    
		    m.PrductDesc = ALLTRIM(&lcOrdLnTmp..DESC1)
		    
		    IF !EMPTY(ALLTRIM( lcDesc))
	   	    m.PrductDesc = m.PrductDesc + " " + ALLTRIM(lcDesc )
	   	  ENDIF
	   	  
		    m.LineNO = IIF(lcRpSelcBy="I",&lcLineTemp..nOrdline,IIF(lcRpSelcBy="P",&lcLineTemp..nordlineno,&lcLineTemp..LineNo)) &&&lcLineTemp..nordlineno
		    FOR lnQ =1  to &lcScaleFile..Cnt
		      lcQ = STR(lnQ,1)
		  	  m.QTY = EVALUATE(lcOrdLnTmp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+lcQ)
		      IF m.Qty = 0
		         LOOP 
		      ENDIF
		   	  m.VendorSKU = &lcOrdLnTmp..STYLE
		   	  IF EMPTY( m.VendorSKU)
		   	    m.VendorSKU = lcVnSK 
		   	  ENDIF
		   	  &&lcCB &&lfGetSku(&lcPackTmp..Account, &lcOrdLnTmp..STYLE,lnQ)
		    *  INSERT INTO (lcUpcTemp) FROM MEMVAR
		      SELECT (lcUpcTemp)
		   	  APPEND BLANK
		   	  GATHER MEMO MEMVAR 
		      lfGetBarCode(m.CstOrder ,'GORDERN')
		      *lfGetBarCode(SUBSTR(ALLTRIM(&lcPackTmp..Store),1,4)+SUBSTR(ALLTRIM(m.CstOrder),1,8)+'0000000000' ,'GORDRET')
		      
		    ENDFOR 
		  ENDSCAN 
		  
		ENDSCAN 
		SELECT (lcPackTmp)
		SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
		SET SKIP TO (lcUpcTemp)
		loogScroll.cCROrientation = 'P'

		&&SET FILTER TO ACCOUNT = "COST0"
		LOCATE 
		IF !EOF()
  	  LOCATE 
		  lcFormName= "ALPKLSQV"
		  loOgScroll.lcOGLastForm = "ALPKLSQV"
		  loOgScroll.lcRpFrxMod = "Graphics"
		  lcOGTmpForm   = loogscroll.gfTempName()
		  = gfCrtFrm("ALPKLSQV","",.T.)    && Create criteria form.
		  DO gfDispRe WITH EVAL('lcFormName')
		ENDIF

		SET FILTER TO 
		llALPakLst=.F.						
ENDCASE
*'CB|' 
*!*  SELECT (lcPackTmp)
*!*  SET FILTER TO account <> 'OVE01'
*!*  LOCATE 
*!*  IF !EOF()
*!*    lcFormName= "ALPKLSA"
*!*    loOgScroll.lcOGLastForm = "ALPKLSA"
*!*    loOgScroll.lcRpFrxMod = "Graphics"
*!*    lcOGTmpForm   = loogscroll.gfTempName()
*!*    = gfCrtFrm("ALPKLSA","",.T.)    && Create criteria form.
*!*    DO gfDispRe WITH EVAL('lcFormName')
*!*    lcFormName= "ALPKLSOV"
*!*    loOgScroll.lcOGLastForm = "ALPKLSOV"
*!*    loOgScroll.lcRpFrxMod = "Graphics"
*!*    lcOGTmpForm   = loogscroll.gfTempName()
*!*    = gfCrtFrm("ALPKLSOV","",.T.)    && Create criteria form.
*!*  ENDIF
*!*  SET FILTER TO 
*!**************************************************************************
*! Name      : lfOvHeadVar
*! Developer : Heba Magdy Selim (HMS)
*! Date      : 02/11/2019
*! Purpose   : Get Header Info. and add phone# to Ship to array
*!**************************************************************************
FUNCTION lfOvHeadVar
lcSolTName =''
lfHeadVar()
DIMENSION laSoldTo[6,1]
=AINS(laSoldTo,1)
laSoldTo[1,1] = lcSolTName 
=lfAdrShift('laSoldTo')
laSoldTo[6] = ''
laShipTo[6] = ''
laSoldTo[5] = ''
laShipTo[5] = ''

SET STEP ON 
IF lcRpFrmCd = 'JC' 
  DIMENSION laDeliv[6]
  laDeliv = ''
  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)
  FOR lnF = 1  TO lnNotesLine 
    IF 'Delivery Agent'  $ MLINE(&lcTempNotePad..mnotes,lnF)
      DO CASE 
        CASE 'Delivery Agent Name :'   $ MLINE(&lcTempNotePad..mnotes,lnF)
          laDeliv [1] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Delivery Agent Name :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Delivery Agent Name :')))  
        CASE 'Delivery Agent Address1 :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laDeliv [2] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Delivery Agent Address1 :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Delivery Agent Address1 :')))  
        CASE 'Delivery Agent Address2 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laDeliv [3] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Delivery Agent Address2 :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Delivery Agent Address2 :')))  
        CASE 'Delivery Agent City :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laDeliv [4] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Delivery Agent City :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Delivery Agent City :')))  
        CASE 'Delivery Agent State :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laDeliv [5] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Delivery Agent State :'   ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Delivery Agent State :')))   
        CASE 'Delivery Agent Postal Code :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laDeliv [6] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Delivery Agent Postal Code :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Delivery Agent Postal Code :')))   
      ENDCASE
    ENDIF
  ENDFOR
  IF !EMPTY(laDeliv [4])  
    laDeliv [5] = laDeliv [4] + ", "+laDeliv [5]+", "+laDeliv [6]
  ENDIF
  laDeliv [6] = ''  
  lfAdrShift('laDeliv')
ENDIF
IF lcRpFrmCd ='QV' 
  DIMENSION laReturn[6]
  laReturn= ''
  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)
  FOR lnF = 1  TO lnNotesLine 
   DO CASE 
      CASE 'Ship From  :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
        laReturn[1] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From  :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From  :' )))  
      CASE 'Ship From Add 1 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
        laReturn[2] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From Add 1 :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From Add 1 :')))  
      CASE 'Ship From Add 2 :' $ MLINE(&lcTempNotePad..mnotes,lnF)
        laReturn[3] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From Add 2 :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From Add 2 :')))  
      CASE 'Ship From City :' $ MLINE(&lcTempNotePad..mnotes,lnF)
        laReturn[4] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From City :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From City :' )))  
      CASE 'Ship From State :' $ MLINE(&lcTempNotePad..mnotes,lnF)
        laReturn[5] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From State :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From State :')))   
      CASE 'Ship From postal code :' $ MLINE(&lcTempNotePad..mnotes,lnF)
        laReturn[6] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From postal code :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From postal code :' )))   
    ENDCASE
  ENDFOR
  IF !EMPTY(laReturn[4])  
  laReturn[5] = laReturn[4] + ", "+laReturn[5]+", "+laReturn[6]
  ENDIF
  laReturn[6] = ''  
  lfAdrShift('laReturn')   
ENDIF 



IF .T. &&lcRpFrmCd = 'BK' OR lcRpFrmCd = 'HT' 
	DIMENSION laShipTo[10]
	DIMENSION laSoldTo[10]
	DIMENSION laReturn[7]
	
	laShipTo  = ''
	laSoldTo  = ''
	laReturn = ''
	
  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)
  FOR lnF = 1  TO lnNotesLine 
    IF 'Ship'  $ MLINE(&lcTempNotePad..mnotes,lnF) OR 'Buyer Name or Department :'  $ MLINE(&lcTempNotePad..mnotes,lnF) OR 'Telephone :' $ MLINE(&lcTempNotePad..mnotes,lnF) 
      DO CASE 
        CASE 'Ship To :'   $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[1] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship To :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship To :')))  
        CASE 'Buyer Name or Department :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[2] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Buyer Name or Department :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Buyer Name or Department :')))  
        CASE 'Ship To Address1 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[3] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship To Address1 :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship To Address1 :')))  
        CASE 'Ship To Address2 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[4] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship To Address2 :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship To Address2 :')))  
        CASE 'Ship To City :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[5] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship To City :'   ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship To City :')))   
        CASE 'Ship To State : ' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[6] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship To State : ',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship To State : ')))   
        CASE 'Ship To Postal Code :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[7] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Ship To Postal Code :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Ship To Postal Code :')))   
        CASE 'Ship To Country:'   $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[8] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship To Country:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship To Country:')))   
       CASE 'Telephone :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[9] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Telephone :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Telephone :')))             
          laShipTo[9] = TRANSFORM(STRTRAN(laShipTo[9],'-',''),'@R ' +"XXX-XXX-XXXX")
       CASE 'Ship-to Email  :'   $ MLINE(&lcTempNotePad..mnotes,lnF) 
          laShipTo[10] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship-to Email  :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship-to Email  :')))             
      ENDCASE
    ENDIF
    IF 'Bill To'  $ MLINE(&lcTempNotePad..mnotes,lnF)
      DO CASE 
        CASE 'Bill To :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[1] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Bill To :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Bill To :')))  
        CASE 'Bill To Address1 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[2] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Bill To Address1 :'  ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Bill To Address1 :')))  
        CASE 'Bill To Address2 :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[3] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Bill To Address2 :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Bill To Address2 :')))  
        CASE 'Bill To City :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[4] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Bill To City :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Bill To City :')))  
        CASE 'Bill To State :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[5] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Bill To State :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Bill To State :')))   
        CASE 'Bill To Postal Code :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[6] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Bill To Postal Code :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Bill To Postal Code :')))   
        CASE 'Bill To Country:'   $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[7] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Bill To Country:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Bill To Country:')))   
        CASE 'Bill-to Email   :'   $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[8] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Bill-to Email   :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Bill-to Email   :')))             
      ENDCASE
    ENDIF
    IF (lcRpFrmCd ='HT' OR lcRpFrmCd ='BL') AND 'Ship From' $ MLINE(&lcTempNotePad..mnotes,lnF)
     DO CASE 
        CASE 'Ship From  :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laReturn[1] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From  :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From  :' )))  
        CASE 'Ship From Add 1 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laReturn[2] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From Add 1 :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From Add 1 :')))  
        CASE 'Ship From Add 2 :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laReturn[3] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From Add 2 :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From Add 2 :')))  
        CASE 'Ship From City :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laReturn[4] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From City :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From City :' )))  
        CASE 'Ship From State :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laReturn[5] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From State :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From State :')))   
        CASE 'Ship From postal code :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laReturn[6] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship From postal code :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship From postal code :' )))   
      ENDCASE
    ENDIF
  ENDFOR 
  
  
                                                                    
  IF !EMPTY(laShipTo[5])  
    laShipTo[5] = laShipTo[5] +", "+ laShipTo[6]+", "+laShipTo[7]
  ENDIF
  laShipTo[7] = ''
  laShipTo[6] = ''
  IF !EMPTY( laSoldTo[4])  
    laSoldTo[4] = laSoldTo[4] + ", "+laSoldTo[5]+", "+laSoldTo[6]
  ENDIF
  laSoldTo[5] =   ''
  laSoldTo[6] =   ''
  IF !EMPTY( laReturn[4])  
   laReturn[5] = laReturn[4] + ", "+laReturn[5]+", "+laReturn[6]
  ENDIF
  laReturn[7] = ''
  laReturn[6] = ''  
  lfAdrShift('laSoldTo')
  lfAdrShift('laShipTo')
  lfAdrShift('laReturn')
ENDIF  
IF !(lcRpFrmCd = 'BK' OR lcRpFrmCd = 'HT' OR lcRpFrmCd ='BL' OR lcRpFrmCd ='CO')
  DIMENSION laShipTo[7]

	laShipTo[7] = ''

  IF !EMPTY(IIF(EMPTY(EVAL(lcOrdhdr+'.phone')),EVAL(lcCustomer+'.phone1'),EVAL(lcOrdhdr+'.phone')))
		FOR lnX = 1 TO 7
		  IF EMPTY(ALLTRIM(laShipTo[lnX]))
		    laShipTo[lnX] = TRANSFORM(IIF(EMPTY(EVAL(lcOrdhdr+'.phone')),EVAL(lcCustomer+'.phone1'),EVAL(lcOrdhdr+'.phone')),'@R ' +"XXX-XXX-XXXX")
		    EXIT
		  ENDIF
		ENDFOR
	ENDIF
ENDIF
IF (lcRpFrmCd <> 'CO' AND  lcRpFrmCd <> 'BK' AND lcRpFrmCd <> 'HT' AND lcRpFrmCd <>'BL')
  DIMENSION laSoldTo[7]
  laSoldTo[7] = ''
  IF !EMPTY(EVAL(lcCustomer+'.phone1'))
 	FOR lnX = 1 TO 7
	  IF EMPTY(ALLTRIM(laSoldTo[lnX]))
	    laSoldTo[lnX] = TRANSFORM(EVAL(lcCustomer+'.phone1'),'@R ' +"XXX-XXX-XXXX")
	    EXIT
	  ENDIF
	ENDFOR
ENDIF
ENDIF
*!*	IF !EMPTY(EVAL(lcCustomer+'.cemail_add')) AND  lcRpFrmCd <> 'BK' AND lcRpFrmCd <>'HD' AND lcRpFrmCd <>'HT'  AND lcRpFrmCd <>'CO' AND lcRpFrmCd <> 'BL'
*!*		FOR lnX = 1 TO 7
*!*		  IF EMPTY(ALLTRIM(laSoldTo[lnX]))
*!*		    laSoldTo[lnX] = EVAL(lcCustomer+'.cemail_add')
*!*		    EXIT
*!*		  ENDIF  
*!*		ENDFOR
*!*	  FOR lnX = 1 TO 7
*!*		  IF EMPTY(ALLTRIM(laShipTo[lnX]))
*!*		    laShipTo[lnX] = EVAL(lcCustomer+'.cemail_add')
*!*		    EXIT
*!*		  ENDIF
*!*	  ENDFOR	
*!*	ENDIF
IF lcRpFrmCd = 'CO' OR lcRpFrmCd = 'HT' OR lcRpFrmCd ='BL'
  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)
  FOR lnF = 1  TO lnNotesLine 
    IF 'Ship-to Email  :'   $ MLINE(&lcTempNotePad..mnotes,lnF) AND EMPTY(laShipTo[6])
      laShipTo[7] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Ship-to Email  :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Ship-to Email  :')))                 
    ENDIF  
    IF 'Bill-to Email   :' $ MLINE(&lcTempNotePad..mnotes,lnF) AND EMPTY(laSoldTo[6])
      laSoldTo[7] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Bill-to Email   :'  ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Bill-to Email   :' )))                 
    ENDIF    
  ENDFOR
ENDIF
=lfAdrShift('laShipTo')
=lfAdrShift('laSoldTo')
*!**************************************************************************
*! Name      : lfGetBarCode
*! Developer : SAH [T20180612.0015]
*! Date      : 12-03-2018
*! Purpose   : Get Barcode for Deal NO
*!**************************************************************************
*: C202232,1
FUNCTION lfGetBarCode
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18   [Start] 
*PARAMETERS  tcCUPC
PARAMETERS  tcCUPC,lcFldName
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18   [End] 

SELECT(lcUpcTemp) 
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18   [Start] 
*lcF_name = UPPER('UPCBAR')
lcF_name = UPPER(lcFldName)
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18   [End] 

WITH loogScroll && your form name
  IF TYPE('loogScroll.'+ lcF_name) <> 'O'
    .ADDOBJECT(lcF_name,"OLEBoundControl")
  ENDIF
  .&lcF_name..CONTROLSOURCE = lcF_name 
 *  .&lcF_name..WIDTH         =10 &&600&&200
 * .&lcF_name..HEIGHT        = 10&&300
*!*     .&lcF_name..WIDTH         = 300&&200
*!*    .&lcF_name..HEIGHT        = 150
   SELECT(lcUpcTemp) 
   APPEND GENERAL &lcF_name. CLASS ("IDAuto.BarCode")
  .&lcF_name..REFRESH
  .&lcF_name..OBJECT.DataToEncode = tcCUPC
  .&lcF_name..OBJECT.SymbologyID  = 13
  .&lcF_name..OBJECT.showtext     = 0
  .&lcF_name..OBJECT.Orientation = 0
  .&lcF_name..OBJECT.TopMarginCm = 0
  .&lcF_name..OBJECT.BACKCOLOR = RGB(255,255,255)
  .&lcF_name..OBJECT.FORECOLOR = RGB(0,0,0)
  .&lcF_name..OBJECT.CodabarStartCharacter ='*'
  .&lcF_name..OBJECT.CodabarStopCharacter ='*'
*  .&lcF_name..OBJECT.XDimensionMILS  = 0.09
  .&lcF_name..OBJECT.BarHeight = 1
  .&lcF_name..OBJECT.Wide2NarrowRatio =0.003
  .&lcF_name..OBJECT.LeftMarginCM = 0.25
  .&lcF_name..OBJECT.AddCheckDigit = .F.
  .&lcF_name..OBJECT.narrowbarwidth = 0.0129
  .&lcF_name..Code128CharSet = 0
ENDWITH
FUNCTION lfGetSku
LPARAMETERS lcAccount, lcSTYLE,lnQ
lcQtySz = STR(lnQ,1)
IF !USED('SPCK_LIN')
  =gfOpenTable('SPCK_LIN','SPCKLINS')
ENDIF
SELECT SPCK_LIN
=gfSetOrder('SPCKLINS')
IF gfSeek('S'+lcAccount+lcSTYLE)
  LOCATE REST WHILE TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+lcAccount+lcSTYLE FOR Qty&lcQtySz. = 1 
  IF FOUND()
    RETURN SPCK_LIN.PACK_ID 
  ENDIF
ELSE
 RETURN ''
ENDIF

FUNCTION lfGetUPC
LPARAMETERS  lcSTYLE,lnQ
lcQtySz = STR(lnQ,1)
IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC','STYLEUPC')
ENDIF
SELECT STYLEUPC
=gfSetOrder('STYLEUPC')
IF gfSeek(lcSTYLE+lcQtySz)
  RETURN STYLEUPC.CUPCNUM1+STYLEUPC.CUPCNUM2+STYLEUPC.CUPCNUM3
ELSE
 RETURN ''
ENDIF