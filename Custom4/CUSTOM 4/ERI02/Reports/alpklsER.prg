*:***************************************************************************
*: Program file  : ALPKLSER.PRG
*: Program desc. : CUSTOMIZED PACKING LIST Form FOR ERI02.
*: Date          : 11/05/2015
*: System        : Aria Advantage Series.4XP
*: Module        : SALES ORDER ALLOCATION (AL)
*: Developer     : Mariam Mazhar (MMT)
*: Tracking Job Number: (C201730) {T20150910.0062}
*:***************************************************************************
*: Modifications:
*: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015]
*: C202232,2 MMT 12/19/2018 Read Addresses from Order notes in custom form EJ for BloomingDale's [T20180612.0015]
*: C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [P20191121.0001]	
*: C611982,2 MMT 12/18/2019 Custom Retail Packing List for SAK18	 [P20191121.0001]	
*:***************************************************************************
SET STEP ON 
SELECT (lcPackTmp)
lnTmpFld = AFIELDS(laFieldArr)
*: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][Start]
*DIMENSION laNewTempStru[4,4]
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
*DIMENSION laNewTempStru[10,4]
DIMENSION laNewTempStru[23,4]
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 

*: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][End]
laNewTempStru[1,1] ="TranNo"
laNewTempStru[1,2] ='C'
laNewTempStru[1,3]= 6
laNewTempStru[1,4]=0

laNewTempStru[2,1] ="UPC"
laNewTempStru[2,2] ='C'
laNewTempStru[2,3]= 13
laNewTempStru[2,4]=0

laNewTempStru[3,1] ="Desc"
laNewTempStru[3,2] ='C'
laNewTempStru[3,3]= 60
laNewTempStru[3,4]=0

laNewTempStru[4,1] ="QTY"
laNewTempStru[4,2] ='N'
laNewTempStru[4,3]= 7
laNewTempStru[4,4]=0

*: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][Start]
laNewTempStru[5,1] ="Line_No"
laNewTempStru[5,2] ='N'
laNewTempStru[5,3]= 6
laNewTempStru[5,4]=0

laNewTempStru[6,1] = 'upcBar'
laNewTempStru[6,2] = 'G'
laNewTempStru[6,3] = 1
laNewTempStru[6,4] = 0

laNewTempStru[7,1] = 'ReservNO'
laNewTempStru[7,2] = 'C'
laNewTempStru[7,3] = 30
laNewTempStru[7,4] = 0

laNewTempStru[8,1] = 'DealNO'
laNewTempStru[8,2] = 'C'
laNewTempStru[8,3] = 30
laNewTempStru[8,4] = 0

laNewTempStru[9,1] = 'GiftMsg'
laNewTempStru[9,2] = 'M'
laNewTempStru[9,3] = 1
laNewTempStru[9,4] = 0

laNewTempStru[10,1] ="QTYORD"
laNewTempStru[10,2] ='N'
laNewTempStru[10,3]= 7
laNewTempStru[10,4]=0
*: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][End]
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
laNewTempStru[11,1] ="STYLE"
laNewTempStru[11,2] ='C'
laNewTempStru[11,3]= 19
laNewTempStru[11,4]=0

laNewTempStru[12,1] ="Price"
laNewTempStru[12,2] ='N'
laNewTempStru[12,3]= 12
laNewTempStru[12,4]= 2

laNewTempStru[13,1] ="ORDERNUM"
laNewTempStru[13,2] ='C'
laNewTempStru[13,3]= 13
laNewTempStru[13,4]= 0

laNewTempStru[14,1] ="PAYMETH"
laNewTempStru[14,2] ='C'
laNewTempStru[14,3]= 60
laNewTempStru[14,4]= 0

laNewTempStru[15,1] ="CUSTPO"
laNewTempStru[15,2] ='C'
laNewTempStru[15,3]= 15
laNewTempStru[15,4]= 0

laNewTempStru[16,1] ="COMPLETE"
laNewTempStru[16,2] ='D'
laNewTempStru[16,3]= 8
laNewTempStru[16,4]= 0


laNewTempStru[17,1] ="SHIPVIA"
laNewTempStru[17,2] ='C'
laNewTempStru[17,3]= 30
laNewTempStru[17,4]= 0

laNewTempStru[18,1] ="Freight"
laNewTempStru[18,2] ='N'
laNewTempStru[18,3]= 13
laNewTempStru[18,4]= 2


laNewTempStru[19,1] ="tax_amt"
laNewTempStru[19,2] ='N'
laNewTempStru[19,3]= 13
laNewTempStru[19,4]= 2

laNewTempStru[20,1] ="SHPSERIAL"
laNewTempStru[20,2] ='C'
laNewTempStru[20,3]= 60
laNewTempStru[20,4]= 0


laNewTempStru[21,1] ="GSHPSERIAL"
laNewTempStru[21,2] ='G'
laNewTempStru[21,3]= 10
laNewTempStru[21,4]= 0

laNewTempStru[22,1] ="GORDERN"
laNewTempStru[22,2] ='G'
laNewTempStru[22,3]= 10
laNewTempStru[22,4]= 0

laNewTempStru[23,1] ="llLastLine"
laNewTempStru[23,2] ='L'
laNewTempStru[23,3]= 1
laNewTempStru[23,4]= 0
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 


*lcUpcTemp = loogscroll.gfTempName()
=gfCrtTmp(lcUpcTemp ,@laNewTempStru,"TranNo",lcUpcTemp ,.T.)
IF !USED('STYLEUPC')
  =gfOpenTable('STYLEUPC',"STYLEUPC")
ENDIF
lcLineTemp =IIF(lcRpSelcBy ="I",(lcInvLnTmp),IIF(lcRpSelcBy ='P',(lcPakLnTmp),(lcOrdLnTmp)))
SELECT(lcLineTemp)
IF ! UPPER(lcStyleFile) $ UPPER(SET("Relation"))
  SET RELATION TO Style INTO (lcStyleFile) Addi
ENDIF  
SELECT (lcPackTmp)
LOCATE 
SET STEP ON 
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
*SCAN FOR (ACCOUNT = "NOR12" OR ACCOUNT = 'BLO77')
SCAN FOR (ACCOUNT = "NOR12" OR ACCOUNT = 'BLO77' OR ACCOUNT = 'SAK18')
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 

  m.TranNO = &lcPackTmp..Pack_NO
  SELECT (lcLineTemp)
  LOCATE 
  *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
  lnCntLine = 0
  *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 
  SCAN FOR IIF(lcRpSelcBy="I",INVOICE,IIF(lcRpSelcBy="P",PACK_No,cordtype+order+store))=IIF(lcRpSelcBy="I",&lcPackTmp..INVOICE,IIF(lcRpSelcBy="P",&lcPackTmp..PACK_NO,"O"+&lcPackTmp..order+&lcPackTmp..store))
    FOR lnCnt= 1 TO &lcScaleFile..cnt
      IF EVALUATE(lcLineTemp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+STR(lnCnt,1))>0
        IF gfSeek(&lcLineTemp..Style+STR(lnCnt,1),'StyleUPC','StyleUPC')
          m.UPC = StyleUPC.CUPCNUM1+StyleUPC.CUPCNUM2+StyleUPC.CUPCNUM3
          m.Qty =EVALUATE(lcLineTemp+"."+IIF(lcRpSelcBy $"PI",'qty','Pik')+STR(lnCnt,1))
        
          *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
          *m.Desc = ALLTRIM(&lcSTYLEfile..Desc)+","+ ALLTRIM(gfCodDes(PADR(SUBSTR(&lcLineTemp..Style ,lnClrStart ,lnClrLen   ),6), 'COLOR     '))+","+EVALUATE(lcScaleFile+".SZ"+STR(lnCnt,1))
          IF &lcPackTmp..ACCOUNT = 'SAK18'
            =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
            IF 'Format Of Packing Slip : gift order' $  &lcTempNotePad..mnotes
              REPLACE lstandctn WITH .T. IN (lcPackTmp)  
            ELSE 
              REPLACE lstandctn WITH .F. IN (lcPackTmp)  
            ENDIF
              
            =SEEK('O'+&lcPackTmp..ORDER+STR(&lcLineTemp..nordlineno,6),lcOrdLnTmp,lcOrdLnTmp)
            m.Desc = ALLTRIM(&lcOrdLnTmp..Desc1)
            m.Desc = ALLTRIM(&lcSTYLEfile..Desc)+","+ ALLTRIM(gfCodDes(PADR(SUBSTR(&lcLineTemp..Style ,lnClrStart ,lnClrLen   ),6), 'COLOR     '))+","+EVALUATE(lcScaleFile+".SZ"+STR(lnCnt,1))
            m.Style = SUBSTR(&lcLineTemp..Style,1,lnMajorPic)
          ELSE 
            m.Desc = ALLTRIM(&lcSTYLEfile..Desc)+","+ ALLTRIM(gfCodDes(PADR(SUBSTR(&lcLineTemp..Style ,lnClrStart ,lnClrLen   ),6), 'COLOR     '))+","+EVALUATE(lcScaleFile+".SZ"+STR(lnCnt,1))
            *: C611982,2 MMT 12/18/2019 Custom Retail Packing List for SAK18	 [P20191121.0001]	[Start]
            m.Style = ''
            *: C611982,2 MMT 12/18/2019 Custom Retail Packing List for SAK18	 [P20191121.0001]	[End]
          ENDIF  
          
         *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 

          *: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][Start]
          *Insert into (lcUpcTemp)Values(m.TranNo,m.UPC,m.Desc,m.Qty)
          *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
          *Insert into (lcUpcTemp)(TranNo,UPC,Desc,Qty) Values(m.TranNo,m.UPC,m.Desc,m.Qty)
          *IF &lcPackTmp..ACCOUNT = 'BLO77'
          Insert into (lcUpcTemp)(TranNo,UPC,Desc,Qty,Style) Values(m.TranNo,m.UPC,m.Desc,m.Qty,m.Style)
          IF &lcPackTmp..ACCOUNT = 'SAK18'
            lnCntLine = lnCntLine + 1 
            IF MOD(lnCntLine ,7)=0
              REPLACE llLastLine WITH .T. IN (lcUpcTemp)
            ENDIF
          ENDIF  
          IF &lcPackTmp..ACCOUNT = 'BLO77' OR &lcPackTmp..ACCOUNT = 'SAK18'
            REPLACE  &lcUpcTemp..Price WITH EVALUATE(lcOrdLnTmp+'.Price')
         *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 

            =SEEK('O'+&lcPackTmp..ORDER+STR(&lcLineTemp..nordlineno,6),lcOrdLnTmp,lcOrdLnTmp)
            REPLACE QtyOrd WITH EVALUATE(lcOrdLnTmp+'.Qty'+STR(lnCnt,1)) IN (lcUpcTemp)
            REPLACE Line_No WITH &lcLineTemp..Line_No IN (lcUpcTemp)
            
            =loOrdHdr.SEEK('O' + &lcPackTmp..Order)
            *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
            IF &lcPackTmp..ACCOUNT = 'SAK18'
              REPLACE  &lcUpcTemp..CUSTPO WITH &lcTempOrdhdr..CUSTPO  IN (lcUpcTemp)
              REPLACE  &lcUpcTemp..COMPLETE WITH &lcTempOrdhdr..COMPLETE IN (lcUpcTemp)
              REPLACE  &lcUpcTemp..SHIPVIA WITH  gfCodDes(&lcTempOrdhdr..SHIPVIA  , 'SHIPVIA')IN (lcUpcTemp)
            ENDIF
            
            IF &lcPackTmp..ACCOUNT = 'SAK18' 
                =SEEK('O'+&lcPackTmp..ORDER+STR(&lcLineTemp..nordlineno,6),lcOrdLnTmp,lcOrdLnTmp)
                lcGiftMsg = ''
                lnFreight = 0 
                lnTaxAmt = 0
                lnNotesLine = MEMLINES(&lcOrdLnTmp..note_mem)
                lnPrice = 0
                FOR lnF = 1  TO lnNotesLine 
                  DIMENSION laNotesArr[1]
                  =gfSubStr(MLINE(&lcOrdLnTmp..note_mem,lnF),@laNotesArr,"|")
                  IF !EMPTY(laNotesArr[1])
                     FOR lnArNot = 1 TO ALEN(laNotesArr,1)
        	             IF "Type of Comment :"+ALLTRIM(m.UPC)  $ laNotesArr[lnArNot] AND &lcPackTmp..lstandctn 
                           lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(laNotesArr[lnArNot],AT('Type of Comment :'+ALLTRIM(m.UPC) ,laNotesArr[lnArNot])+LEN("Type of Comment :"+ALLTRIM(m.UPC))))  
*!*	                         ELSE
*!*							   lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(laNotesArr[lnArNot],AT('Type of Comment :',laNotesArr[lnArNot])+LEN("Type of Comment :")))                           
                         ENDIF
 	                     IF "Handling Charges :"+ALLTRIM(m.UPC)   $ laNotesArr[lnArNot]
	                       lnFreight =  VAL(ALLTRIM(SUBSTR(laNotesArr[lnArNot],AT("Handling Charges :"+ALLTRIM(m.UPC) ,laNotesArr[lnArNot])+LEN("Handling Charges :"+ALLTRIM(m.UPC) ))))
		                 ENDIF
		                 IF "Sales Tax :"+ALLTRIM(m.UPC)   $ laNotesArr[lnArNot]
		                   lnTaxAmt =  VAL(ALLTRIM(SUBSTR(laNotesArr[lnArNot],AT("Sales Tax :"+ALLTRIM(m.UPC) ,laNotesArr[lnArNot])+LEN("Sales Tax :"+ALLTRIM(m.UPC) )))  )
		                 ENDIF  
		                 IF "SuggestedRetail Price :"+ALLTRIM(m.UPC)   $ laNotesArr[lnArNot]
		                   lnPrice =  VAL(ALLTRIM(SUBSTR(laNotesArr[lnArNot],AT("SuggestedRetail Price :"+ALLTRIM(m.UPC) ,laNotesArr[lnArNot])+LEN("SuggestedRetail Price :"+ALLTRIM(m.UPC) )))  )
		                 ENDIF 
                      ENDFOR  
                   ENDIF 
   		  	    ENDFOR 
   		  	    * 
                REPLACE GiftMsg WITH lcGiftMsg IN (lcUpcTemp)
                REPLACE  &lcUpcTemp..Freight  WITH lnFreight ,;
                         &lcUpcTemp..tax_amt  WITH lnTaxAmt ,;
                         Price WITH lnPrice  IN (lcUpcTemp)
                      
	  		 ENDIF
             *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 


            m.ReservNO = ''
            m.DealNO = ''
            IF 'Order #:' $ &lcTempOrdhdr..Note1
              lnColPos = ATC(':',&lcTempOrdhdr..Note1)
              IF lnColPos > 0
                m.ReservNO = SUBSTR(&lcTempOrdhdr..Note1,lnColPos+1) 
                REPLACE ReservNO WITH m.ReservNO IN (lcUpcTemp)
              ENDIF
            ENDIf  
            IF 'Promotional:' $ &lcTempOrdhdr..Note2
              lnColPos = ATC(':',&lcTempOrdhdr..Note2)
              IF lnColPos > 0
                m.DealNO = SUBSTR(&lcTempOrdhdr..Note2,lnColPos+1) 
                REPLACE DealNO  WITH m.DealNO  IN (lcUpcTemp)
                IF !EMPTY(m.DealNO)
                  SELECT (lcUpcTemp)
                  *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
                  *  lfGetBarCode(m.DealNO)        
                  lfGetBarCode(m.DealNO,'UPCBAR')
                 *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 

                ENDIF
              ENDIF
            ENDIf 
            =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
            
              *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 

              lcOrderNumber= ''
              lcPayMethod = ''
              lcShipRef = ''
              lnFreight = 0
              lnTaxAmt = 0
                            lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)

                            FOR lnF = 1  TO lnNotesLine 

                IF &lcPackTmp..ACCOUNT = 'SAK18'
                  IF 'Order Number :' $ MLINE(&lcTempNotePad..mnotes,lnF)
                    lcOrderNumber= lcOrderNumber+IIF(EMPTY(lcOrderNumber),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Order Number :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Order Number :')))  
                  ENDIF
                  IF "Special Payment Reference Number :"  $ MLINE(&lcTempNotePad..mnotes,lnF)
                    lcPayMethod = lcPayMethod +IIF(EMPTY(lcPayMethod ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT("Special Payment Reference Number :",MLINE(&lcTempNotePad..mnotes,lnF))+LEN("Special Payment Reference Number :")))  
                  ENDIF
                  IF  "Transaction Code :"  $ MLINE(&lcTempNotePad..mnotes,lnF)
                    lcShipRef = lcShipRef +IIF(EMPTY(lcShipRef ),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT("Transaction Code :",MLINE(&lcTempNotePad..mnotes,lnF))+LEN("Transaction Code :")))  
                  ENDIF
                  &&"Shipping Label Serial Number :"
                ENDIF  
                REPLACE  &lcUpcTemp..ORDERNUM WITH lcOrderNumber IN (lcUpcTemp)
                REPLACE  &lcUpcTemp..PAYMETH  WITH lcPayMethod  IN (lcUpcTemp)
                REPLACE  &lcUpcTemp..SHPSERIAL WITH lcShipRef   IN (lcUpcTemp)
               
                
                lfGetBarCode(&lcUpcTemp..SHPSERIAL,'GSHPSERIAL')
                lfGetBarCode(&lcUpcTemp..ORDERNUM ,'GORDERN')
              ENDFOR 
              *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 
            
              IF 'Gift Message:' $ &lcTempNotePad..mnotes AND &lcPackTmp..ACCOUNT <> 'SAK18'
              
              lcGiftMsg = ''
              lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)
			      

              FOR lnF = 1  TO lnNotesLine 
  			        IF 'Gift Message:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
                  lcGiftMsg = lcGiftMsg +IIF(EMPTY(lcGiftMsg),'',CHR(13)+CHR(10))+ ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Gift Message:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Gift Message:')))  
                  *EXIT 
                ENDIF
           

      			  ENDFOR  
              
              REPLACE GiftMsg WITH lcGiftMsg  IN (lcUpcTemp)
            ENDIF
          ENDIF          
        ENDIF
      ENDIF
      *: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][End]
    ENDFOR 
  ENDSCAN 
  *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
  IF &lcPackTmp..Account = 'SAK18'
    SELECT (lcUpcTemp) 
    SET ORDER TO (lcUpcTemp) DESCENDING 
    =SEEK(&lcPackTmp..Pack_NO)
    REPLACE llLastLine WITH .T.  
    SET ORDER TO (lcUpcTemp) ASCENDING  
    SELECT (lcPackTmp)
  ENDIF
  *C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 
ENDSCAN 


SELECT (lcPackTmp)
SET FILTER TO account = 'NOR12'
LOCATE 
IF !EOF()
  SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
  SET SKIP TO (lcUpcTemp)
  LOCATE 
  DO gfDispRe WITH EVAL('lcFormName')
ENDIF
llALPakLst=.F.

*: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][Start]
SELECT (lcPackTmp)
SET FILTER TO account = 'BLO77'
LOCATE 
IF !EOF()
  IF ! UPPER(lcUpcTemp) $ UPPER(SET("Relation"))
    SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
    SET SKIP TO (lcUpcTemp)
    LOCATE
  ENDIF  
  lcFormName= "ALPKLSEJ"
  loOgScroll.lcOGLastForm = "ALPKLSEJ"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSEJ","",.T.)    && Create criteria form.
  DO gfDispRe WITH EVAL('lcFormName')
  lcFormName= "ALPKLSER"
  loOgScroll.lcOGLastForm = "ALPKLSER"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSER","",.T.)    && Create criteria form.
ENDIF
SET FILTER TO 
llALPakLst=.F.
*: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][END]		  

*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
SELECT (lcPackTmp)
SET FILTER TO account = 'SAK18'
LOCATE 
IF !EOF()
  IF ! UPPER(lcUpcTemp) $ UPPER(SET("Relation"))
    SET RELATION TO PACK_NO INTO (lcUpcTemp) ADDITIVE 
    SET SKIP TO (lcUpcTemp)
    LOCATE
  ENDIF  
  lcFormName= "ALPKLSSK"
  loOgScroll.lcOGLastForm = "ALPKLSSK"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSSK","",.T.)    && Create criteria form.
  DO gfDispRe WITH EVAL('lcFormName')
  lcFormName= "ALPKLSER"
  loOgScroll.lcOGLastForm = "ALPKLSER"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSER","",.T.)    && Create criteria form.
ENDIF
SET FILTER TO 
llALPakLst=.F.
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 




SELECT (lcPackTmp)
*: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][Start]
*SET FILTER TO account <> 'NOR12'
*: C611982,2 MMT 12/18/2019 Custom Retail Packing List for SAK18	 [P20191121.0001]	[Start]
*SET FILTER TO account <> 'BLO77' AND account <> 'NOR12'
SET FILTER TO account <> 'BLO77' AND account <> 'NOR12' AND account <> 'SAK18'
*: C611982,2 MMT 12/18/2019 Custom Retail Packing List for SAK18	 [P20191121.0001]	[End]
*: C202232,1 SAH 12/4/2018 Add custom form EJ for BloomingDale's [T20180612.0015][End]
LOCATE 
IF !EOF()
  lcFormName= "ALPKLSA"
  loOgScroll.lcOGLastForm = "ALPKLSA"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSA","",.T.)    && Create criteria form.
  DO gfDispRe WITH EVAL('lcFormName')
  lcFormName= "ALPKLSER"
  loOgScroll.lcOGLastForm = "ALPKLSER"
  loOgScroll.lcRpFrxMod = "Graphics"
  lcOGTmpForm   = loogscroll.gfTempName()
  = gfCrtFrm("ALPKLSER","",.T.)    && Create criteria form.
ENDIF
SET FILTER TO 

*!**************************************************************************
*! Name      : lfGetBarCode
*! Developer : SAH [T20180612.0015]
*! Date      : 12-03-2018
*! Purpose   : Get Barcode for Deal NO
*!**************************************************************************
*: C202232,1
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
*FUNCTION lfGetBarCode
FUNCTION lfOLDGetBarCode
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 

PARAMETERS  tcCUPC
SELECT(lcUpcTemp) 
lcF_name = UPPER('UPCBAR')
WITH loogScroll && your form name
  IF TYPE('loogScroll.'+ lcF_name) <> 'O'
    .ADDOBJECT(lcF_name,"OLEBoundControl")
  ENDIF
  .&lcF_name..CONTROLSOURCE = lcF_name 
  .&lcF_name..WIDTH         = 600&&200
  .&lcF_name..HEIGHT        = 300
   SELECT(lcUpcTemp) 
   APPEND GENERAL &lcF_name. CLASS ("IDAuto.BarCode")
  .&lcF_name..REFRESH
  .&lcF_name..OBJECT.DataToEncode = tcCUPC
  .&lcF_name..OBJECT.SymbologyID  = 16
  .&lcF_name..OBJECT.showtext     = 1
  .&lcF_name..OBJECT.Orientation = 0
  .&lcF_name..OBJECT.TopMarginCm = 0
  .&lcF_name..OBJECT.BACKCOLOR = RGB(255,255,255)
  .&lcF_name..OBJECT.FORECOLOR = RGB(0,0,0)
  .&lcF_name..OBJECT.CodabarStartCharacter ='*'
  .&lcF_name..OBJECT.CodabarStopCharacter ='*'
  *.&lcF_name..OBJECT.XDimensionMILS  = 35.43307&&0.09
  .&lcF_name..OBJECT.BarHeight = 1
  .&lcF_name..OBJECT.Wide2NarrowRatio = 0.003
  .&lcF_name..OBJECT.LeftMarginCM = 0.25
  .&lcF_name..OBJECT.AddCheckDigit = .F.
  .&lcF_name..OBJECT.narrowbarwidth = .05
  
ENDWITH
*: C202232,2 MMT 12/19/2018 Read Addresses from Order notes in custom form EJ for BloomingDale's [T20180612.0015][Start]
*!**************************************************************************
*! Name      : lfGetPLAddress
*! Developer : Mariam Mazhar
*! Date      : 12-20-2018
*! Purpose   : Get Addresses from notes
*!**************************************************************************
FUNCTION lfGetPLAddress
*XX
SET STEP ON 
IF &lcPackTmp..Account ='SAK18'
  DIMENSION laShipTo[8],laSoldTo[8]
  laSoldTo = ''
  laShipTo = ''
  =loNotePad.SEEK("B" + &lcPackTmp..ORDER)
  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)
  FOR lnF = 1  TO lnNotesLine 
    IF 'Shipping'  $ MLINE(&lcTempNotePad..mnotes,lnF)
      DO CASE 
        CASE 'Shipping Name :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[1] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping Name :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping Name :')))  
        CASE 'Shipping Name 2 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[2] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping Name 2 :'  ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping Name 2 :' )))  
        CASE 'Shipping Address 1 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[3] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping Address 1 :'  ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping Address 1 :' )))  
        CASE 'Shipping Address 2 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[4] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping Address 2 :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping Address 2 :' )))  
        CASE 'Shipping City :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[5] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping City :'  ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping City :' )))   
        CASE 'Shipping State :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[6] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping State :'  ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping State :' )))   
        CASE 'Shipping Postal :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[7] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT( 'Shipping Postal :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN( 'Shipping Postal :')))   
        CASE 'Shipping Country :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[8] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping Country :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping Country :' )))   
      ENDCASE
    ENDIF
    IF 'Billing'  $ MLINE(&lcTempNotePad..mnotes,lnF)
      DO CASE 
        CASE 'Billing Name :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[1] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing Name :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing Name :')))  
        CASE 'Billing Name 2 :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[2] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing Name 2 :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing Name 2 :')))  
        CASE 'Billing Address1:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[3] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing Address1:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing Address1:' )))  
        CASE 'Billing Address2:' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[4] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing Address2:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing Address2:')))  
        CASE 'Billing City :' $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[5] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing City :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing City :')))   
        CASE 'Billing State :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[6] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing State :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing State :')))   
        CASE 'Billing Postal Code :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[7] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing Postal Code :',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing Postal Code :')))   
        CASE 'Billing Country :'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[8] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing Country :' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing Country :')))   
      ENDCASE
    ENDIF
  ENDFOR 
  laShipTo[5] = laShipTo[5] +", "+ laShipTo[6]+", "+laShipTo[7]
  laShipTo[7] = ''
  laShipTo[6] = ''
  laSoldTo[5] = laSoldTo[5] + ", "+laSoldTo[6]+", "+laSoldTo[7]
  laSoldTo[7] = ''
  laSoldTo[6] = ''
  lfAdrShift('laSoldTo')
  lfAdrShift('laShipTo')
  
ELSE
*XX
=loNotePad.SEEK("B" + &lcPackTmp..ORDER)
IF 'Shipping Address:' $ &lcTempNotePad..mnotes OR 'Billing Address:' $ &lcTempNotePad..mnotes
  lnNotesLine = MEMLINES(&lcTempNotePad..mnotes)
  FOR lnF = 1  TO lnNotesLine 
    IF 'Shipping '  $ MLINE(&lcTempNotePad..mnotes,lnF)
      DO CASE 
        CASE 'Shipping Address:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[1] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping Address:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping Address:')))  
        CASE 'Shipping Address1:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[2] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping Address1:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping Address1:' )))  
        CASE 'Shipping Address2:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[3] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping Address2:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping Address2:' )))  
        CASE 'Shipping CITY:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[4] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping CITY:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping CITY:' )))   
        CASE 'Shipping State:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[5] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping State:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping State:' )))   
        CASE 'Shipping Postal:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laShipTo[6] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Shipping Postal:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Shipping Postal:' )))   
      ENDCASE
   ENDIF
   IF 'Billing '  $ MLINE(&lcTempNotePad..mnotes,lnF)
      DO CASE 
        CASE 'Billing Address:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[1] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing Address:',MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing Address:'))) 
        *: C611982,2 MMT 12/18/2019 Custom Retail Packing List for SAK18	 [P20191121.0001]	[Start]  
        *CASE Billing Address2:  $ MLINE(&lcTempNotePad..mnotes,lnF)
        CASE 'Billing Address2:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
        *: C611982,2 MMT 12/18/2019 Custom Retail Packing List for SAK18	 [P20191121.0001]	[End]
          laSoldTo[2] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing Address2:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing Address2:' )))  
        CASE 'Billing CITY:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[3] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing CITY:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing CITY:' )))   
        CASE 'Billing State:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[4] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing State:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing State:' )))   
        CASE 'Billing Postal:'  $ MLINE(&lcTempNotePad..mnotes,lnF)
          laSoldTo[5] = ALLTRIM(SUBSTR(MLINE(&lcTempNotePad..mnotes,lnF),AT('Billing Postal:' ,MLINE(&lcTempNotePad..mnotes,lnF))+LEN('Billing Postal:' )))   
      ENDCASE
   ENDIF   
  ENDFOR 
  laShipTo[4] = laShipTo[4] + laShipTo[5]+laShipTo[6]
  laShipTo[5] = ''
  laShipTo[6] = ''
  laSoldTo[3] = laSoldTo[3] + laSoldTo[4]+laSoldTo[5]
  laSoldTo[4] = ''
  laSoldTo[5] = ''
  laSoldTo[6] = ''
ENDIF
*: C202232,2 MMT 12/19/2018 Read Addresses from Order notes in custom form EJ for BloomingDale's [T20180612.0015][End]
*XX
ENDIF
*XXX

*!**************************************************************************
*! Name      : lfGetBarCode
*! Developer : SAH [T20180612.0015]
*! Date      : 12-03-2018
*! Purpose   : Get Barcode for Deal NO
*!**************************************************************************
*: C202232,1
FUNCTION lfGetBarCode
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
*PARAMETERS  tcCUPC
PARAMETERS  tcCUPC,lcFldName
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 

SELECT(lcUpcTemp) 
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [Start] 
*lcF_name = UPPER('UPCBAR')
lcF_name = UPPER(lcFldName)
*C611982,1 ES 11/25/2019 Custom Retail Packing List for SAK18	 [End] 

WITH loogScroll && your form name
  IF TYPE('loogScroll.'+ lcF_name) <> 'O'
    .ADDOBJECT(lcF_name,"OLEBoundControl")
  ENDIF
  .&lcF_name..CONTROLSOURCE = lcF_name 
 *  .&lcF_name..WIDTH         =10 &&600&&200
 * .&lcF_name..HEIGHT        = 10&&300
*!*	   .&lcF_name..WIDTH         = 300&&200
*!*	  .&lcF_name..HEIGHT        = 150
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


*!*************************************************************
*! Name      : lfAdrShift
*! Developer : Haytham El_Sheltawi
*! Date      : 01/15/1998
*! Purpose   : Function to Shift the Address array if there is any
*!             empty lines in the address
*!*************************************************************
*! Called from : ARPINVA.PRG , lfSolSpAdr()
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : The Address Array name
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfAdrShift

PARAMETERS lcArrayNam

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)

  *IF The current Array element is of type character and empty
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") = "C" .AND.;
     EMPTY(&lcArrayNam.[lnCount])

    =ADEL(&lcArrayNam , lnCount)
    lnCount = lnCount - 1
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop

*FOR Loop to loop the Address Array
FOR lnCount = 1 TO ALEN(&lcArrayNam)
  *IF The current Array element is not of type character
  IF TYPE(lcArrayNam + "[" + STR(lnCount , 1) + "]") <> "C"
    &lcArrayNam.[lnCount] = ''
  ENDIF    && End of IF
ENDFOR    && End of FOR Loop
