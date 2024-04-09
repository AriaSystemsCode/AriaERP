*!**************************************************************************
*: Name      : TAJMAIN.PRG
*: Developer : TMI - TAREK MOHAMMED IBRAHIM
*: Date      : 07/28/2004
*: Purpose   : Main program for Taj imports customer
*: Issue     : *C116794,1
*:**************************************************************************
*: Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*:             lcFunPars -> Process function parameters, sent as a string.
*:**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'
*--Run the function.
llRetValue = EVAL(lcFunToRun)
RETURN llRetValue

*:**************************************************************************
*:* Name        : lfOPSTYDYE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/28/2004
*:* Purpose     : Open stydye file if not used
*:***************************************************************************
*:* Called from : mfgenct.prg
*:***************************************************************************
*C # 116794,1
FUNCTION lfOPSTYDYE
IF !USED('STYDYE')
  =gfOpenFile(gcDataDir+'STYDYE','STYDYE','SH')
ENDIF 
*-- end of lfOPSTYDYE.

*:**************************************************************************
*:* Name        : lfUPCTQTY
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/28/2004
*:* Purpose     : Deduct stock value from paurchased qty for TAG IMPORTS
*:***************************************************************************
*:* Called from : mfgenct.prg
*:***************************************************************************
*C # 116794,1
FUNCTION lfUPCTQTY

=SEEK(lcStyle+lcWareCode+lcDyelot,'STYDYE')
*C116794,3  TMI [Start] Check Open To Sell qty
*lnCutQty1 = MAX(lnCutQty1 - STYDYE.STK1,0)
*lnCutQty2 = MAX(lnCutQty2 - STYDYE.STK2,0)
*lnCutQty3 = MAX(lnCutQty3 - STYDYE.STK3,0)
*lnCutQty4 = MAX(lnCutQty4 - STYDYE.STK4,0)
*lnCutQty5 = MAX(lnCutQty5 - STYDYE.STK5,0)
*lnCutQty6 = MAX(lnCutQty6 - STYDYE.STK6,0)
*lnCutQty7 = MAX(lnCutQty7 - STYDYE.STK7,0)
*lnCutQty8 = MAX(lnCutQty8 - STYDYE.STK8,0)
*- I based on that the customer will take all sales orders , considering OTS , the target is to make 
*- OTS not less than zero
lnOTS1 = STYDYE.STK1 + STYDYE.WIP1 - STYDYE.ORD1
lnOTS2 = STYDYE.STK2 + STYDYE.WIP2 - STYDYE.ORD2
lnOTS3 = STYDYE.STK3 + STYDYE.WIP3 - STYDYE.ORD3
lnOTS4 = STYDYE.STK4 + STYDYE.WIP4 - STYDYE.ORD4
lnOTS5 = STYDYE.STK5 + STYDYE.WIP5 - STYDYE.ORD5
lnOTS6 = STYDYE.STK6 + STYDYE.WIP6 - STYDYE.ORD6
lnOTS7 = STYDYE.STK7 + STYDYE.WIP7 - STYDYE.ORD7
lnOTS8 = STYDYE.STK8 + STYDYE.WIP8 - STYDYE.ORD8
lnCutQty1 = IIF(lnOTS1<0 , MIN(lnCutQty1,-lnOTS1) , 0)
lnCutQty2 = IIF(lnOTS2<0 , MIN(lnCutQty2,-lnOTS2) , 0)
lnCutQty3 = IIF(lnOTS3<0 , MIN(lnCutQty3,-lnOTS3) , 0)
lnCutQty4 = IIF(lnOTS4<0 , MIN(lnCutQty4,-lnOTS4) , 0)
lnCutQty5 = IIF(lnOTS5<0 , MIN(lnCutQty5,-lnOTS5) , 0)
lnCutQty6 = IIF(lnOTS6<0 , MIN(lnCutQty6,-lnOTS6) , 0)
lnCutQty7 = IIF(lnOTS7<0 , MIN(lnCutQty7,-lnOTS7) , 0)
lnCutQty8 = IIF(lnOTS8<0 , MIN(lnCutQty8,-lnOTS8) , 0)
*C116794,3  TMI [End  ] 
*-- end of lfUPCTQTY.