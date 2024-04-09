*:***************************************************************************
* Application  : Aria27
* Module       : IC
* Porpouse     : Fix program to update prices in Spck_lin file for GMA
* Developed By : TMI - Tarek Mohamed Ibrahim             
* Date         : 01/15/2004
* Ref #        : *B121235,1  
*:***************************************************************************
*--Calling the option grid
lcExpr = gfOpGrid('ICFXPCK' , .T.)

*--IF close button pressed , exit
IF EMPTY(lcExpr) .OR. lcExpr = '.T.'
  *B121235,3  TMI [Start] "no packs processed " message
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Packs processed.')
  *B121235,3  TMI [End  ] 
  RETURN
ENDIF

IF !USED('SPCK_HDR')
  *- TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION
  USE (gcDataDir+'SPCK_HDR') IN 0 
ENDIF
SET ORDER TO SPCK_HDRVR IN SPCK_HDR

IF !USED('SPCK_LIN')
  *- TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
  USE (gcDataDir+'SPCK_LIN') IN 0 
  *B121235,3  TMI [Start] Ask for alias "SPCKLIN2" separatly
ENDIF
IF !USED('SPCKLIN2')
  *B121235,3  TMI [End  ]   
  USE (gcDataDir+'SPCK_LIN') AGAIN IN 0 ALIAS SPCKLIN2
ENDIF
SET ORDER TO SPCK_LINVR IN SPCK_LIN
SET ORDER TO SPCK_LINVR IN SPCKLIN2

lnCnt = 0
SELECT SPCK_HDR
SCAN FOR &lcExpr
  SELECT SPCK_HDR
  IF SEEK(TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'SPCK_LIN')    
    lnCnt = lnCnt + 1
    lcKey = TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION

    lnPckQty  = 0
    lnPckClPr = 0

    IF SPCK_HDR.LPCKPRPIEC
      SELECT SPCK_LIN
      SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcKey
        REPLACE NPCK_PRICE WITH SPCK_HDR.NPKSLPRICE
        lnPckQty  = lnPckQty  + TOTQTY
        lnPckClPr = lnPckClPr + NPCK_PRICE*TOTQTY
      ENDSCAN
      SELECT SPCK_HDR
      REPLACE nPckQty  WITH lnPckQty ;
              nPckClPr WITH lnPckClPr
    
    ELSE

      SELECT SPCKLIN2
      SEEK lcKey
      lnTotQty = 0 
      SUM TOTQTY TO lnTotQty REST WHILE TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcKey 
      
      IF lnTotQty>0    
        SELECT SPCK_LIN
        SCAN REST WHILE TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE = lcKey
          REPLACE NPCK_PRICE WITH ROUND(SPCK_HDR.NPKSLPRICE/lnTotQty,2)
          lnPckQty  = lnPckQty  + TOTQTY
          lnPckClPr = lnPckClPr + NPCK_PRICE*TOTQTY
        ENDSCAN
        SELECT SPCK_HDR
        REPLACE nPckQty  WITH lnPckQty ;
                nPckClPr WITH lnPckClPr
      ENDIF
    ENDIF
  ENDIF
ENDSCAN

=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(lnCnt>0,LTRIM(STR(lnCnt)),'No')+' Pack(s) processed.')

*B121235,3  TMI [Start] -Close opened files
IF USED('SPCK_HDR')
  USE IN 'SPCK_HDR'
ENDIF

IF USED('SPCK_LIN')
  USE IN 'SPCK_LIN'
ENDIF

IF USED('SPCKLIN2')
  USE IN 'SPCKLIN2'
ENDIF
*B121235,3  TMI [End  ] 

*:**************************************************************************
*:* Name        : lfSRPackId
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/15/2004
*! Purpose   : Go top in the Spck_Hdr file when browse.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSRPackId()
*:***************************************************************************
FUNCTION lfSRPackId
PARAMETERS OpGrdParm
SELECT SPCK_HDR   

DO CASE
  CASE  OpGrdParm='S'           
    SELECT SPCK_HDR
    SET ORDER TO SPCK_HDRVR
    GO TOP
ENDCASE
