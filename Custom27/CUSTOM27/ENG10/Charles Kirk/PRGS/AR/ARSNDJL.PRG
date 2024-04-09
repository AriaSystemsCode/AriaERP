*:****************************************************************************
*: Program file        : ARSNDJL.PRG
*: Program description : Export Invoices to John Lewis           
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : AR2 - Accounts reciveble
*: Developer Name      : TMI TAREK MOHAMED IBRAHIM
*: Tracking Job Number : C200573
*:****************************************************************************
*: Modification
*: C037287,1 MMM 11/30/2003 Some Modifications based on the customer request.
*:****************************************************************************

*C037287,1 MMM 11/30/2003 Adding Variable to hold the M_ANA Value [Start]
lcAna = gfGetMemVar('M_ANA')
*ash1
lcAna2 = gfGetMemVar('M_ANA2')
*C037287,1 MMM [End]

*--Defining syrepuvr variables
*ash1
lnTerDiscR = 0
DECLARE laTRltFld[1,2]
laTRltFld[1,1] = 'NTERDISCR'
laTRltFld[1,2] = 'lnTerDiscR'


PRIVATE lcSlctdAcc,lcOldValue
STORE '' TO lcRpSndFil,lcSlctdAcc,lcTmpInv,lcOldValue

*-- Create a cursor that is used in filling the finally output text file
lcTmpInv = gfTempName()
CREATE TABLE &gcDataDir.&lcTmpInv. (ACCOUNT C(5),INVOICE C(6))
INDEX ON ACCOUNT+INVOICE TAG &lcTmpInv

*--Calling the option grid
lcExpr = gfOpGrid('ARSNDJL' , .T.)

*--IF close button pressed , exit
IF EMPTY(lcExpr)
  RETURN
ENDIF

*-- Export invoices to ASCII file
=lfExpToAsc()

*-- Start exporting invoices


*:**************************************************************************
*:* Name        : lfwRpWhen() 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : When OG Function
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfwRpWhen

*-- if it is the default case
IF lnOgSeting = 1
  SHOW GET pbRun DISABLE
ENDIF

*-- end of lfwRpWhen.

*:**************************************************************************
*:* Name        : lfvSndFile
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : Valid fn for selecting a file to export data to 
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
*:* Example     :  = lfvSndFile()
*:***************************************************************************
FUNCTION lfvSndFile
DO WHILE .T.
  IF EMPTY(lcRpSndFil)
    EXIT
  ENDIF
  
  IF lcOldValue # lcRpSndFil
    lcRpSndFil = ALLT(lcRpSndFil)
    lcRpSndFil = lcRpSndFil + IIF('.' $ lcRpSndFil , '' , '.TXT' )
    IF FILE(lcRpSndFil)
      lcRpSndFil = LOCFILE(lcRpSndFil)
      *-- Message 00356 :File ð already exists. Overwrite?
      *-- Button  02011 :\!\<OK;\?\<Cancel
      IF gfModalGen('QRM00356B02011','DIALOG',lcRpSndFil) = 1
        EXIT
      ENDIF
    ELSE
      lnHndl = FCREATE(lcRpSndFil)
      IF lnHndl>0
        =FCLOSE(lnHndl)
        lcRpSndFil = LOCFILE(lcRpSndFil)
        ERASE (lcRpSndFil)
        EXIT
      ENDIF  
    ENDIF
    lcRpSndFil = GETFILE('TXT','File to create')  
  ELSE
    EXIT
  ENDIF
ENDDO

=lfvChkRnBt() 
*-- end of lfvSndFile.

*:**************************************************************************
*:* Name        : lfvChkRnBt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : Check Run button to be enabled
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvChkRnBt()
*:***************************************************************************
FUNCTION lfvChkRnBt
PRIVATE lnSlct,lcStat,lnAccPos,lnAccRec 
lnSlct = SELECT()

lnAccPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT'),1)

STORE 0 TO lnAccRec
IF USED(laOgFxFlt[lnAccPos,6])
  SELECT (laOgFxFlt[lnAccPos,6])
  COUNT FOR !DELETED() AND !EMPTY(ACCOUNT) TO lnAccRec
ENDIF  

lcStat = IIF(lnAccRec>0 AND !EMPTY(lcRpSndFil),'ENABLE','DISABLE')
SHOW GET pbRun &lcStat

SELECT (lnSlct)
*-- end of lfvChkRnBt.

*:**************************************************************************
*:* Name        : lfChkTmpInv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : Rebuild lcTmpInv file data
*:* Note        : This file is used in export process.
*:***************************************************************************
*:* Called from : called lfvChkRnBt
*:***************************************************************************
FUNCTION lfChkTmpInv
PRIVATE lnInvPos,lnAccPos

lnInvPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'INVHDR.INVOICE'),1)
lnAccPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT'),1)

*-- Rebuild "lcTmpInv" records  

*- If Account is selected only then take all availabel invoices ,else include only selected invoices 
SELECT &lcTmpInv
ZAP
IF USED(laOgFxFlt[lnAccPos,6])
  SELECT (laOgFxFlt[lnAccPos,6])
  SCAN
    INSERT INTO (lcTmpInv) VALUES (EVAL(laOgFxFlt[lnAccPos,6]+'.ACCOUNT'),'ALL')
  ENDSCAN
ENDIF

IF USED(laOgFxFlt[lnInvPos,6])
  SELECT (laOgFxFlt[lnInvPos,6])
  SCAN
    =SEEK(EVAL(laOgFxFlt[lnInvPos,6]+'.INVOICE'),'INVHDR')
    IF SEEK(INVHDR.ACCOUNT+'ALL',lcTmpInv)
      REPLACE &lcTmpInv..INVOICE WITH INVHDR.INVOICE
    ELSE
      INSERT INTO (lcTmpInv) VALUES (INVHDR.ACCOUNT,INVHDR.INVOICE)
    ENDIF
  ENDSCAN   
ENDIF

*-- end of lfChkTmpInv.

*:**************************************************************************
*:* Name        : lfvSlctdAcc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : Set/rest/valid funtion for Accounts Browse
*                 Get selected accounts to used as a filter for selected Invoices
*                 also clear selected invoices if not for the new selected accounts
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvSlctdAcc()
*:***************************************************************************
FUNCTION lfvSlctdAc
PARAMETERS lcSR
PRIVATE lnAlias,lnAccPos,lnInvPos
lnAlias = SELECT()

DO CASE
  CASE lcSR = 'S'
    GO TOP IN CUSTOMER

  CASE lcSR = 'R'
    *-- Get the selected accounts in a memory variable to be used in browse filter of invoices
    lcSlctdAcc = ''
  
    lnAccPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT'),1)
    SELECT (laOgFxFlt[lnAccPos,6])
    GO TOP
    SCAN
      lcSlctdAcc = lcSlctdAcc + EVAL(laOgFxFlt[lnAccPos,6]+'.ACCOUNT') + ' | '
    ENDSCAN
    
    *-- Clear invoices not for the selected accounts     
    lnInvPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'INVHDR.INVOICE'),1)
    IF USED(laOgFxFlt[lnInvPos,6])
      SELECT (laOgFxFlt[lnInvPos,6])
      GO TOP
      SCAN
        =SEEK( EVAL(laOgFxFlt[lnInvPos,6]+'.INVOICE') , 'INVHDR' )
        IF !SEEK(INVHDR.ACCOUNT,(laOgFxFlt[lnAccPos,6]))
          DELETE
        ENDIF
      ENDSCAN
    ENDIF    
    
    *-- Recheck Run btn status
    =lfvChkRnBt()
    =lfChkTmpInv()
    
ENDCASE

SELECT (lnAlias)

*-- end of lfvSlctdAcc.

*:**************************************************************************
*:* Name        : lfsrvInv
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : Set-reset-valid funtion for invoice browse
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfsrvInv
PARAMETERS lcSRV
IF lcSRV = 'R'
  *-- Recheck Run btn status
  =lfvChkRnBt()  
  =lfChkTmpInv()
ENDIF
*-- end of lfsrvInv.

*:**************************************************************************
*:* Name        : lfExpToAsc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : Export data to selected ASCII file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfExpToAsc()
*:***************************************************************************
FUNCTION lfExpToAsc
PRIVATE lnHndl,lcWrtStr,lnI,lcSz,lcTmpSnd,lcSvErr,llError,lcSKU,lnAmt,lnVat,lcWhereExp,llLnsAdded,;
        lnClrPos,lnClrLen

*- Variables description
*- lnHndl       : File handle variable for the temp created output file
*- lcTmpSnd     : The name of the temp text file to create
*- lcWrtStr     : Used in adding lines to the temp output text file
*- lnI          : loop variable
*- lcSz         : represents sizes in a loop
*- lcSvErr      : Save error handler 
*- llError      : Dummy variable check if error happened while copying the text file      
*- lcSKU        : Get the style  sku
*- lnAmt        : Save the amount of InvLine.totqty*InvLine.price for all processed invoices
*- lnVat        : Save the amount of InvLine.qty*InvLine.price for only sizes applied into Vat
*- lcWhereExp   : Used in select statement
*- llLnsAdded   : Check if any line is added to the temp created text file.


STORE .F. TO llLnsAdded

*-- Create a temp file for output and finally if created the user is asked to save or not
lcTmpSnd = gcWorkDir+gfTempName()
lcWrtStr = ''
lnHndl = FCREATE(lcTmpSnd)
IF lnHndl <= 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not create the file to be sent.')
  RETURN
ENDIF

*-- The text file is created  , continue filling it
=gfOpenFile(gcDataDir+'INVHDR','INVHDRA','SH')
=gfOpenFile(gcDataDir+'INVLINE','INVLINE','SH')
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN','SPCKLINS','SH')  && Key :: TYPE+ACCOUNT+STYLE+PACK_ID
=gfOpenFile(gcDataDir+'SPCK_HDR','SKU_STYLE','SH')  && Key :: TYPE+ACCOUNT+STYLE+PACK_ID
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'SkuTmpl','SkuTmpl','SH')
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')

=gfOpenFile(gcDataDir+'ORDHDR','ORDHDR','SH')
=gfOpenFile(gcDataDir+'PIKTKT','PIKTKT','SH')
=gfOpenFile(gcDataDir+'CODES','CCODE_NO','SH')

*- Define variables hold color len and position
STORE 0  TO lnClrPos,lnClrLen
=lfGetClrD()

*- Create Vat Tax file
lcVatRt = gfTempName()
CREATE DBF (gcWorkDir+lcVatRt) (VATRT N(7,3),AMT N(14,2),VAT N(7,3))
INDEX ON VATRT TAG &lcVatRt
*ash1
*APPEND BLANK                && Add a record for vat rate 0

STORE 0 TO lnAmt,lnVat
SELECT &lcTmpInv
GO TOP

SCAN

  =SEEK(&lcTmpInv..ACCOUNT+IIF(&lcTmpInv..INVOICE='ALL','',&lcTmpInv..INVOICE),'INVHDR')

  SELECT INVHDR  
  SCAN REST WHILE ACCOUNT+INVOICE = &lcTmpInv..ACCOUNT+IIF(&lcTmpInv..INVOICE='ALL','',;
                                                           &lcTmpInv..INVOICE) FOR !LINVSNTJL
    =SEEK('O'+INVHDR.ORDER,'ORDHDR')
    =SEEK(INVHDR.PIKTKT,'PIKTKT')
    
    *C037287,1 MMM 11/30/2003 adding Variable to hold the CUSTOMER.storelocno value and Update field 
    *                         3 ,4 with ana code value ,field 9 with CUSTOMER.storelocno and leave
    *                         field 10,11 empty [Start] 
    lcStore = INVHDR.Store
    lcSlct = select()
    SELECT CUSTOMER
    IF EMPTY(lcStore)
      =SEEK('M'+&lcTmpInv..ACCOUNT + lcStore)
    ELSE 
      =SEEK('S'+&lcTmpInv..ACCOUNT + lcStore)
    ENDIF
    lcStrLocNo = CUSTOMER.storelocno
    SELECT(lcSlct)
    *ash1
    =gfRltFld(InvHdr.cTermCode,@laTRltFld,'CTERMCODE') 
    *lcWrtStr = ["HDR",]+;
               ["502863","502863",," "," "," "," ","502863","502863","502863"," "," "," "," "," "," ",]+;
               INVHDR.INVOICE+[,]+;
               lfGtDt(INVHDR.INVDATE)+[," "," "," ",]+;               
               ["]+ALLT(INVHDR.CUSTPO)+[",]+;
               lfGtDt(ORDHDR.COMPLETE)+[," "," ",]+;
               ["]+ALLT(INVHDR.PIKTKT)+[",]+;
               *lfGtDt(PIKTKT.DATE)+[," "," "," "," "," "," "," "]
               
    lcWrtStr = ["HDR",]+;
               ["502863","]+lcana+[","]+lcana2+["," "," "," ","502863","]+lcStrLocNo+["," "," "," "," "," "," "," "," ",]+;
               INVHDR.INVOICE+[,]+;
               lfGtDt(INVHDR.INVDATE)+[," "," "," ",]+;               
               ["]+ALLT(INVHDR.CUSTPO)+[",]+;
               lfGtDt(ORDHDR.COMPLETE)+[," "," ",]+;
               ["]+ALLT(INVHDR.PIKTKT)+[",]+;
               lfGtDt(PIKTKT.DATE)+[," "," "," "," "," "," "," "] 
               
    *C037287,1 MMM [End]                    
               
    =FPUTS(lnHndl,lcWrtStr)
    
    =SEEK(INVHDR.INVOICE,'INVLINE')
    
    SELECT INVLINE
    SCAN REST WHILE INVOICE+STR(LINENO,6) = INVHDR.INVOICE FOR INVLINE.TOTQTY > 0
      =SEEK(INVLINE.STYLE,'STYLE')
      =SEEK('S'+STYLE.SCALE,'SCALE')
      
      DIMENSION laTaxRt[1]
      laTaxRt[1] = 0
      IF !EMPTY(STYLE.CTAXCODE)
        lcWhereExp = 'NCTAXCODE  '+STYLE.CTAXCODE +SPACE(30)+ 'NTAXRATE  '
        SELECT VAL(CRLTD_VLU) ;
          FROM CODES ;
          WHERE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = lcWhereExp ;
          INTO ARRAY laTaxRt      
      ENDIF
      
      IF !SEEK(laTaxRt[1],lcVatRt)
        INSERT INTO &lcVatRt VALUES (laTaxRt[1],INVLINE.TOTQTY*INVLINE.PRICE,0)
      ELSE        
        REPLACE &lcVatRt..AMT WITH &lcVatRt..AMT + INVLINE.TOTQTY*INVLINE.PRICE       
      ENDIF
      
      FOR lnI = 1 TO SCALE.CNT
        lcSz = STR(lnI,1)
        IF INVLINE.QTY&lcSz > 0
          lcSKU = lfGtSku(lcSz)
          lcWrtStr = ["ILD",]+;
                     ["]+lcSKU+[",]+;
                     ["]+lcSKU+[",]+;
                     ["]+ALLT(STYLE.CVENSTY)+[",]+;
                     ALLT(STR(INVLINE.QTY&lcSz))+[," "," ",]+;
                     ["]+ALLTRIM(INVLINE.DESC1)+["," ",]+;                     
                     ALLTRIM(STR(INVLine.Price,12,2))+[,]+;
                     ALLTRIM(STR(INVLine.Price*INVLINE.QTY&lcSz,12,2))+[,]+;
                     ["]+IIF(lnI < STYLE.NTAXBREAK .OR. laTaxRt[1] = 0 , 'Z' , 'S' )+[",]+;
                     STR(IIF(lnI < STYLE.NTAXBREAK , 0 , laTaxRt[1]),7,3)+[,]+;
                     ALLTRIM(STR(INVLINE.GROS_Price,12,2))+[,]+;
                     ALLTRIM(STR(INVLINE.DISC_PCNT,6,2))+[," "]
          =FPUTS(lnHndl,lcWrtStr)
          =SEEK(IIF(lnI < STYLE.NTAXBREAK,0,laTaxRt[1]),lcVatRt)
          *ash1
          IF EOF(lcVatRt) AND lnI < STYLE.NTAXBREAK
            INSERT INTO &lcVatRt VALUES (0,0,0)
          ENDIF
          REPLACE &lcVatRt..VAT WITH &lcVatRt..VAT + INVLine.Price*INVLINE.QTY&lcSz

          llLnsAdded = .T.
                    
        ENDIF
      ENDFOR      
    ENDSCAN
    
    *C200573,4  TMI [Start] Initialize variables lnAmt,lnVat for each invoice
    STORE 0 TO lnAmt,lnVat    
    *C200573,4  TMI [End  ] 
    
    *- Write Vat summary lines for each invoice for each Vat
    SELECT &lcVatRt
    GO TOP
    SCAN 
      *ash1
      *lcWrtStr = ["STL",]+;
                 ["]+IIF(&lcVatRt..VATRT # 0 , 'S' , 'Z' ) + [",]+;
                 STR(&lcVatRt..VATRT,7,3)+[,]+;
                 STR(&lcVatRt..VAT,14,2)+[," "," "," "," "," "," ",]+;
                 STR( (&lcVatRt..VAT/100)*&lcVatRt..VATRT ,14,2)+[,]+;
                 STR( (&lcVatRt..VAT/100)*&lcVatRt..VATRT ,14,2)+[," "]
       lcWrtStr = ["STL",]+;
                  ["]+IIF(&lcVatRt..VATRT # 0 , 'S' , 'Z' ) + [",]+;
                  STR(&lcVatRt..VATRT,7,3)+[,]+;
                  STR(&lcVatRt..VAT,14,2)+[," "," "," "," "," "," ",]+;
                  STR( (&lcVatRt..VAT/100)*&lcVatRt..VATRT*(100-lnTerDiscR)/100 ,14,2)+[,]+;
                  STR( (&lcVatRt..VAT/100)*&lcVatRt..VATRT*(100-lnTerDiscR)/100 ,14,2)+[," "]                 
      =FPUTS(lnHndl,lcWrtStr)
      
      lnAmt = lnAmt + &lcVatRt..AMT
      *ash1
      *lnVat = lnVat + (&lcVatRt..VAT/100)*&lcVatRt..VATRT
      lnVat = lnVat + (&lcVatRt..VAT/100*(100-lnTerDiscR)/100)*&lcVatRt..VATRT

    ENDSCAN
    
    *- Clear the Var rates file to refill
    SELECT &lcVatRt
    ZAP

    *C200573,4  TMI [Start] Write this line for each ivoice as per tony    
    lcWrtStr = ["TLF",]+;
           STR(lnAmt,14,2)+[," "," "," "," "," "," ",]+;
           STR(lnVat,14,2)+[,]+;
           STR(lnAmt+lnVat,14,2)+[," "]  
    =FPUTS(lnHndl,lcWrtStr)          
    *C200573,4  TMI [End  ] 

  ENDSCAN
ENDSCAN

*- Write the trailer of all the selected invoices
*C200573,4  TMI [Start] Write this line for each ivoice as per tony
*lcWrtStr = ["TLF",]+;
           STR(lnAmt,14,2)+[," "," "," "," "," "," ",]+;
           STR(lnVat,14,2)+[,]+;
           STR(lnAmt+lnVat,14,2)+[," "]  
*=FPUTS(lnHndl,lcWrtStr)          
*C200573,4  TMI [End  ] 

*-- Close the file
=FCLOSE(lnHndl)

*-- Remove temp vat file
IF USED(lcVatRt)
  USE IN &lcVatRt  
ENDIF
ERASE(gcWorkDir+lcVatRt+'.DBF')
ERASE(gcWorkDir+lcVatRt+'.CDX')

*- Ask for saving
*-- Button  32000 :\<Yes;\?\<No
IF llLnsAdded .AND. ;
  gfModalGen('QRM00000B32000',.F.,.F.,.F.,'File has been created, do you want to save it?') = 1
  lcSvErr = ON('ERROR')
  llError = .F.
  ON ERROR llError = .T.  
  COPY FILE &lcTmpSnd TO &lcRpSndFil
  ON ERROR &lcSvErr
  IF !llError            && file is saved correctly , go now to update the field INVHDR.LINVSNTJL 
    SELECT &lcTmpInv.
    SCAN
      =SEEK(&lcTmpInv..ACCOUNT+IIF(&lcTmpInv..INVOICE='ALL','',&lcTmpInv..INVOICE),'INVHDR')
      SELECT INVHDR
      SCAN REST WHILE ACCOUNT+INVOICE = &lcTmpInv..ACCOUNT+IIF(&lcTmpInv..INVOICE='ALL','',;
                                                               &lcTmpInv..INVOICE)
        REPLACE INVHDR.LINVSNTJL WITH .T.
      ENDSCAN
    ENDSCAN
    =gfModalGen('QRM00000B00000',.F.,.F.,.F.,;
                               'File saved successfully.')
    
  ELSE
    =gfModalGen('QRM00000B00000',.F.,.F.,.F.,;
                               'Some error is encountered while saving the file, '+;
                               'please retry the process or change the file name.')
  ENDIF
ELSE
  IF !llLnsAdded
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No invoices processed, cannot create the output file.')
  ENDIF  
ENDIF
USE IN &lcTmpInv.
ERASE (gcDataDir+lcTmpInv+'.DBF')
ERASE (gcDataDir+lcTmpInv+'.CDX')

*-- end of lfExpToAsc.

*:**************************************************************************
*:* Name        : lfGtDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : Get date in a string type in the format dd/mm/yy
*:***************************************************************************
*:* Called from : lfExpToAsc
*:***************************************************************************
*:* Parameters : Date value
*:***************************************************************************
*:* Return      : String contains the date in the format dd/mm/yy
*:***************************************************************************
FUNCTION lfGtDt
PARAMETERS ldDat
IF !EMPTY(ldDat)
  RETURN PADL(DAY(ldDat),2,'0')+'/'+;
         PADL(MONTH(ldDat),2,'0')+'/'+;
         RIGHT(STR(YEAR(ldDat)),2)
ELSE
  RETURN ''
ENDIF         
         
*-- end of lfGtDt.

*:**************************************************************************
*:* Name        : lfvOldVal
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : Old value function
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfvOldVal
lcOldValue = EVAL(SYS(18))
*-- end of lfvOldVal.

*:**************************************************************************
*:* Name        : lfGtSku
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 7/22/2003
*:* Purpose     : Get Sku of the current style account from spck_lin file
*:***************************************************************************
*: Parametes    : Size # that with qty>0 ,passed as Character
*:* Called from : lfExpToAsc
*:***************************************************************************
FUNCTION lfGtSku
PARAMETERS lcSz
*TMI , it is wrong to make the parameter lcSz be private
*PRIVATE lcSz,lnSlct,lcSku,lnDime
PRIVATE lnSlct,lcSku,lnDime
lnSlct = SELECT()

lcSku = ''
*C200575,7  TMI [Start] seek by the new add field Ordline.packid
*IF SEEK('S'+INVLINE.ACCOUNT+SUBSTR(INVLINE.STYLE,1,lnClrPos+lnClrLen-1),'SPCK_HDR') .AND. ;
   SEEK('S'+INVLINE.ACCOUNT+INVLINE.STYLE,'SPCK_LIN')
SET ORDER TO SPCK_HDR IN SPCK_HDR
SET ORDER TO SPCK_LIN IN SPCK_LIN
IF !USED('ORDLINE')
  =gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
ENDIF
=SEEK('O'+INVLINE.ORDER+STR(INVLINE.LINENO,6),'ORDLINE')
IF SEEK('S'+INVLINE.ACCOUNT+ORDLINE.PACKID,'SPCK_HDR') .AND. ;
   SEEK('S'+INVLINE.ACCOUNT+ALLT(ORDLINE.PACKID),'SPCK_LIN')
  *C200575,7  TMI [End  ]    
  lnDime = LEN(ALLT(SPCK_HDR.PACK_ID))
  
  SELECT SPCK_LIN
  *TMI , use FOR instead of WHILE
  *LOCATE REST WHILE TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+INVLINE.ACCOUNT+INVLINE.STYLE ;
              FOR   QTY&lcSz > 0
  lcSvOrd = ORDER()            
  SET ORDER TO 
  *LOCATE FOR TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+INVLINE.ACCOUNT+INVLINE.STYLE ;
              .AND.   QTY&lcSz > 0
  LOCATE FOR TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+INVLINE.ACCOUNT+INVLINE.STYLE+ALLTRIM(ORDLINE.PACKID) ;
              .AND.   QTY&lcSz > 0              
  IF FOUND()
    lcSku = ALLT(SUBSTR(SPCK_LIN.PACK_ID,lnDime+1))
  ENDIF            
  SET ORDER TO &lcSvOrd IN SPCK_LIN
ENDIF

SELECT (lnSlct)
RETURN lcSku
*-- end of lfGtSku.

*:**************************************************************************
*:* Name        : lfGetClrD                                   
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/28/2003
*:* Purpose     : Get color width for David luck
*:***************************************************************************
FUNCTION lfGetClrD
DECLARE laItemSeg[1]
lcOldSelect=select()

=gfItemMask(@laItemSeg)
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    lnClrPos = laItemSeg[lnCount,4]
    EXIT
  ENDIF
ENDFOR
SELECT(lcOldSelect)
*--end function lfGetClrD
