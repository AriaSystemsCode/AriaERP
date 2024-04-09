*:***************************************************************************
* Application  : Aria27
* Module       : SO
* Porpouse     : Fix program to update prices in ORDLINE file for GMA
* Developed By : TMI - Tarek Mohamed Ibrahim             
* Date         : 01/15/2004
* Ref #        : *B121235,1  
*:***************************************************************************
*--Calling the option grid
lcExpr = gfOpGrid('SOFXPCK' , .T.)

*--IF close button pressed , exit
IF EMPTY(lcExpr) .OR. lcExpr = '.T.'
  *B121235,3  TMI [Start] "No orders processed " message
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No Orders processed.')
  *B121235,3  TMI [End  ] 
  RETURN
ENDIF

IF !USED('SPCK_LIN')
  *- TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE
  USE (gcDataDir+'SPCK_LIN') IN 0 
ENDIF
SET ORDER TO SPCK_LINVR IN SPCK_LIN

*B121235,3  TMI [Start] Open Spck_hdr file
IF !USED('SPCK_HDR')
  *- TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION
  USE (gcDataDir+'SPCK_HDR') IN 0 
ENDIF
SET ORDER TO SPCK_HDRVR IN SPCK_HDR
*B121235,3  TMI [End  ] 

IF !USED('ORDHDR')
  USE (gcDataDir+'ORDHDR') IN 0 
ENDIF
SET ORDER TO ORDHDR IN ORDHDR

IF !USED('ORDLINE')
  USE (gcDataDir+'ORDLINE') IN 0 
ENDIF
SET ORDER TO PACK_ID IN ORDLINE


IF !USED('INVHDR')
  USE (gcDataDir+'INVHDR') IN 0 
ENDIF
SET ORDER TO INVHDR IN INVHDR

IF !USED('INVLINE')
  USE (gcDataDir+'INVLINE') IN 0 
ENDIF
SET ORDER TO INVLINEO IN INVLINE

SELECT ORDHDR
GO TOP

lnCnt = 0
SELECT ORDHDR
SCAN FOR &lcExpr
  SELECT ORDLINE
  llCnt = .F.   
  =SEEK(ORDHDR.CORDTYPE+ORDHDR.ORDER,'ORDLINE')
  SCAN REST WHILE CORDTYPE+ORDER+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STR(LINENO,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER
    IF !EMPTY(PACK_ID) AND SEEK('P'+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION+STYLE,'SPCK_LIN')
      llCnt = .T.
      *B121235,3  TMI [Start] calculate style price after discount correctly
      *REPLACE GROS_PRICE WITH SPCK_LIN.NPCK_PRICE ;
      *        PRICE      WITH ROUND(SPCK_LIN.NPCK_PRICE*DISC_PCNT,2) ;
      *        NPKSLPRICE WITH SPCK_LIN.NPCK_PRICE               
      &&- Key:TYPE+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION
      =SEEK('P'+ACCOUNT+PACK_ID+CPKCOLOR+CPCKSIZE+CPKVERSION,'SPCK_HDR') 
      *B121235,4  TMI [Start] Let the pack price as it is,Do not update it from spck_hdr file
      *                       just update the style price based on pack price
      *REPLACE GROS_PRICE WITH SPCK_LIN.NPCK_PRICE ;
      *        PRICE      WITH ROUND(SPCK_LIN.NPCK_PRICE*(1-DISC_PCNT/100),2) ;
      *        NPKSLPRICE WITH SPCK_HDR.NPKSLPRICE
      REPLACE GROS_PRICE WITH IIF(SPCK_HDR.LPCKPRPIEC,NPKSLPRICE,;
                               IIF(SPCK_HDR.NPCKQTY>0,NPKSLPRICE/SPCK_HDR.NPCKQTY,GROS_PRICE)) ;
              PRICE      WITH ROUND(GROS_PRICE*(1-DISC_PCNT/100),2) 
      *B121235,4  TMI [End  ] 
      *B121235,3  TMI [End  ] 
      
      IF SEEK(ORDLINE.ORDER+STR(ORDLINE.LINENO,6),'INVLINE')
        SELECT INVLINE
        *B121235,3  TMI [Start] calculate style price after discount correctly
        *REPLACE GROS_PRICE WITH SPCK_LIN.NPCK_PRICE ;
        *        PRICE      WITH ROUND(SPCK_LIN.NPCK_PRICE*DISC_PCNT,2) ;
        *        NPKSLPRICE WITH SPCK_LIN.NPCK_PRICE
        *B121235,4  TMI [Start] Let the pack price as it is,Do not update it from spck_hdr file
        *REPLACE GROS_PRICE WITH SPCK_LIN.NPCK_PRICE ;
        *        PRICE      WITH ROUND(SPCK_LIN.NPCK_PRICE*(1-DISC_PCNT/100),2) ;
        *        NPKSLPRICE WITH SPCK_HDR.NPKSLPRICE
        REPLACE GROS_PRICE WITH IIF(SPCK_HDR.LPCKPRPIEC,NPKSLPRICE,;
                                 IIF(SPCK_HDR.NPCKQTY>0,NPKSLPRICE/SPCK_HDR.NPCKQTY,GROS_PRICE)) ;
                PRICE      WITH ROUND(GROS_PRICE*(1-DISC_PCNT/100),2) 
        *B121235,4  TMI [End  ] 
        *B121235,3  TMI [End  ] 
      ENDIF
    ENDIF
  ENDSCAN
  lnCnt = lnCnt + IIF(llCnt,1,0)            

ENDSCAN  

=gfModalGen('INM00000B00000',.F.,.F.,.F.,IIF(lnCnt>0,LTRIM(STR(lnCnt)),'No ')+' Order(s) processed.')


*B121235,3  TMI [Start] -*- Close opened files
IF USED('SPCK_LIN')
  USE IN 'SPCK_LIN'
ENDIF

IF USED('SPCK_HDR')
  USE IN 'SPCK_HDR'
ENDIF

IF USED('ORDHDR')
  USE IN 'ORDHDR'
ENDIF

IF USED('ORDLINE')
  USE IN 'ORDLINE'
ENDIF

IF USED('INVHDR')
  USE IN 'INVHDR'
ENDIF

IF USED('INVLINE')
  USE IN 'INVLINE'
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
FUNCTION lfSRORDER
PARAMETERS OpGrdParm

*-- end of lfSRORDER.