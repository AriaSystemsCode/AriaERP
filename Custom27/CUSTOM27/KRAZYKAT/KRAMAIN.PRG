*!**************************************************************************
*! Name      : KRAMAIN.PRG
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 11/01//1999
*! Purpose   : KRAZY KAT Custom Process Program.
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Returns   : Logical value.
*!**************************************************************************
*! Modifications
*C102789,1 TMI 02/02/2003 Add the option of having the creating the cartons 
*C102789,1                type and add that type in the pack header and line
*:**************************************************************************
PARAMETER lcEvntFun,lcFunPars

lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue


*!***************************************************************
*! Name      : lfSeqNo
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 11/01/1999
*! Purpose   : Get the sequence no.
*!***************************************************************
*! Program   : 'ARBKADJ'    -> Debit & Credit Adj.
*! Event     : 'SeqNo'      -> Get the sequence no.
*!***************************************************************
*!
FUNCTION lfSeqNo

IF TYPE('llKraNote') <> 'U' AND llKraNote
  lcToSnd  = IIF(MCHOICE = "D" , 'DEBIT' , 'CRMEMO')
ENDIF  


*!***************************************************************
*! Name      : lfChrgDate
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 11/01/1999
*! Purpose   : Update charge back date in the temp file..
*!***************************************************************
*! Program   : 'ARBKADJ'    -> Debit & Credit Adj.
*! Event     : 'ChrgDate'      -> Get the sequence no.
*!***************************************************************
*!
FUNCTION lfChrgDate

IF TYPE('llKraNote') <> 'U' AND llKraNote
  PRIVATE lnAlias

  lnAlias = SELECT()
  m.Chgbk_Date = ldBatchDt
  *--Update the notepad
  IF !EMPTY(mNotes)
    lcNoteDesc= "Notes For Tran. Number : " + Tran
    SELECT NotePad
    APPEND BLANK
    DO CASE
     CASE MCHOICE = 'D' AND llChrgBack
       *-- 'CHARGEBACK'
       REPLACE TYPE WITH 'Q'
     CASE MCHOICE = 'D'
       *-- 'DEBIT'
       REPLACE TYPE WITH 'D'
     CASE MCHOICE = 'C'
       *-- 'CREDIT'
       REPLACE TYPE WITH 'K'
   ENDCASE  
   REPLACE KEY    WITH &lcAdjTemp..TRAN      ,;
           CDESC  WITH lcNoteDesc            ,;
          MNOTES WITH &lcAdjTemp..mNotes
  ENDIF
  SELECT(lnAlias)
ENDIF  

*!***************************************************************
*! Name      : lfNotes
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 11/01/1999
*! Purpose   : Enter Notepad.
*!***************************************************************
*! Program   : 'ARBKADJ'    -> Debit & Credit Adj.
*! Event     : 'Notes'   -> Enter Notepad.
*!***************************************************************
*!
FUNCTION lfNotes

IF TYPE('llKraNote') <> 'U' AND llKraNote
  lcSaveBmp   = gcBmpHome + "Sav.BMP"
  lcCancelBmp = gcBmpHome + "Can.BMP"
  IF (gfModalGen('TRM00000B00006',.F.,.F.,.F.,'Edit any notes for this adjustment?') = 2)
    RETURN
  ENDIF
  lcNoteDesc = 'ChargeBack notes'
  REPLACE &lcToEdit..mEdtNotes WITH &lcAdjTemp..mNotes
  =lfGoToEnd()
  DO (gcScrDir+gcAct_Appl+"\KRAnote.SPX")
  *--End of lfNotes.
ENDIF  

*:************************************************************************
*: Program file  : lfGoEnd
*: Program desc. : Go to the last line in the edit reigon.
*: For screen    : 
*:         System: A27.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:*************************************************************
FUNCTION lfGoToEnd

IF TYPE('llKraNote') <> 'U' AND llKraNote
  _CUROBJ = OBJNUM(&lcToEdit..mEdtNotes)
  KEYBOARD "{SHIFT+CTRL+END}"+"{END}"  
  *--End of lfGoEnd.
ENDIF  

*!***************************************************************
*! Name      : lfvSaveNot
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 11/01/1999
*! Purpose   : save notes in tran. tmp file.
*!***************************************************************
*!
FUNCTION lfvSaveNot

IF TYPE('llKraNote') <> 'U' AND llKraNote
  PRIVATE lnAlias
  lnAlias = SELECT()
  SELECT (lcAdjTemp)
  REPLACE MNOTES WITH &lcToEdit..mEdtNotes
  SELECT(lnAlias)
  CLEAR READ
ENDIF  



*:************************************************************************
*: Program file  : lfAddField
*: Program desc. : Add a new field .
*: For screen    : 
*:         System: A27.
*:         Module: Aria Apparel Series.
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:*************************************************************
FUNCTION lfAddField

IF TYPE('llKraNote') <> 'U' AND llKraNote
  DIMENSION laFileStru[ALEN(laFileStru,1)+1,4]
  *--Notes field in case of charge back fOR KRAZI KAT.
  laFileStru[ALEN(laFileStru,1),1] = 'mNotes'
  laFileStru[ALEN(laFileStru,1),2] = 'M'
  laFileStru[ALEN(laFileStru,1),3] = 10
  laFileStru[ALEN(laFileStru,1),4] = 0
  *-- end of 
ENDIF  

*:*************************************************************
*: Name      : lfPRNTRUN
*: Developer : AHMED MAHER
*: Date      : 04/22/2002
*: Purpose   : To do not display the printer screen
*:*************************************************************
*: Example   : = lfPRNTRUN()
*:*************************************************************
*! Due to C102599,1
*:*************************************************************
FUNCTION lfPRNTRUN

IF TYPE("lcFormName") = "C" .AND. (lcFormName = "POPENDPO")
  RETURN .T.
ELSE
  RETURN .F.
ENDIF

*:*************************************************************
*: Name      : lfOpenFile
*: Developer : AHMED MAHER
*: Date      : 05/02/2002
*: Purpose   : Open custom table to browse shipment records form
*:*************************************************************
*: Example   : = lfOpenFile()
*:*************************************************************
*! Due to B605932,1
*:*************************************************************
FUNCTION lfOpenFile

=gfOpenFile(gcDataDir+'ITLCSHIP','SHIPNUM','SH')

*:*************************************************************
*: Name      : lfMakRel
*: Developer : AHMED MAHER
*: Date      : 05/02/2002
*: Purpose   : Make relation with custom table itlcship
*:*************************************************************
*: Example   : = lfMakRel()
*:*************************************************************
*! Due to B605932,1
*:*************************************************************
FUNCTION lfMakRel

SET RELATION TO EVAL(lcPOTran+'.shipno') INTO ITLCSHIP ADDITIVE

*:*************************************************************
*: Name      : lfBrowse
*: Developer : AHMED MAHER
*: Date      : 05/02/2002
*: Purpose   : Browse shipment record from custom table itlcship
*:*************************************************************
*: Example   : = lfBrowse()
*:*************************************************************
*! Due to B605932,1
*:*************************************************************
FUNCTION lfBrowse

DO CASE
  CASE lnOrdTrans=3
    IF llQtyPSize
      lcBrD2fld= [LineNo :4 :H='Line#' :R,]+;
                 [Style   :30 :H=lcStyHdr :R,]+;
                 IIF(llDyelot,[Dyelot :H= 'Dyelot' :R :20,],[])+;    
                 [Qty1  :H='Size1':R,]+;
                 [Qty2  :H='Size2':R,]+;
                 [Qty3  :H='Size3':R,]+;
                 [Qty4  :H='Size4':R,]+;
                 [Qty5  :H='Size5':R,]+;
                 [Qty6  :H='Size6':R,]+;
                 [Qty7  :H='Size7':R,]+;
                 [Qty8  :H='Size8':R,]+;
                 [Date  :H='Date' :R,]+;
                 [ITLCSHIP.DEtADATE :H='E.T.A.'  :R,]+;
                 [ShipNo:H='Ship No.':R,]+;
                 [ITLCSHIP.CMASTERNUM:H='MWB #':R,]+;
                 [ITLCSHIP.MASTERDATE:H='MWB date':R,]+;
                 [ITLCSHIP.CCONTNNUM:H='Container':R,]+;
                 [Reference :H='Reference':R]
    ELSE
       lcBrD2fld= [LineNo :4 :H='Line#' :R,]+;
                  [Style   :30 :H=lcStyHdr :R,]+;
                  IIF(llDyelot,[Dyelot :H= 'Dyelot' :R :20,],[])+; 
                  [Date   :H='Date' :R,]+;
                  [TotQty :H='Qty'  :R:P='9999999',]+;
                  [nELanCost1 :H='Landed Cost' :R:P='999999.999',]+;                 
                  [lnAmt=(TotQty*nELanCost1) :H='Amount'   :R:P='9999999.99',]+;
                  [ITLCSHIP.DEtADATE :H='E.T.A.'  :R,]+;
                  [ShipNo:H='Ship No.':R,]+;
                  [ITLCSHIP.CMASTERNUM:H='MWB #':R,]+;
                  [ITLCSHIP.MASTERDATE:H='MWB date':R,]+;
                  [ITLCSHIP.CCONTNNUM:H='Container':R,]+;
                  [Reference :H='Reference':R]
    ENDIF
  CASE lnOrdTrans=2
    IF llQtyPSize
      lcBrD2fld= [Style   :30 :H=lcStyHdr :R,]+;
                 [Qty1  :H='Size1':R,]+;
                 [Qty2  :H='Size2':R,]+;
                 [Qty3  :H='Size3':R,]+;
                 [Qty4  :H='Size4':R,]+;
                 [Qty5  :H='Size5':R,]+;
                 [Qty6  :H='Size6':R,]+;
                 [Qty7  :H='Size7':R,]+;
                 [Qty8  :H='Size8':R,]+;
                 [ITLCSHIP.DEtADATE :H='E.T.A.'  :R,]+;
                 [ShipNo:H='Ship No.':R,]+;
                 [ITLCSHIP.CMASTERNUM:H='MWB #':R,]+;
                 [ITLCSHIP.MASTERDATE:H='MWB date':R,]+;
                 [ITLCSHIP.CCONTNNUM:H='Container':R,]+;
                 [cWareCode :H='Ship To' :R,]+;
                 [Reference :H='Reference':R]
    ELSE
      lcBrD2fld= [lcInTrTTl=IIF(TranCd='6','Inter Location','Shipment') :20 :H='In-Transit',]+;
                 [Style :30 :H=lcStyHdr  :R,]+;
                 [ITLCSHIP.DEtADATE :H='E.T.A.'  :R,]+;
                 [ShipNo:H='Ship No.':R,]+;
                 [ITLCSHIP.CMASTERNUM:H='MWB #':R,]+;
                 [ITLCSHIP.MASTERDATE:H='MWB date':R,]+;
                 [ITLCSHIP.CCONTNNUM:H='Container':R,]+;
                 [cWareCode :H='Ship To' :R,]+;
                 [TotQty  :H='Qty'  :R:P='9999999',]+;
                 [Reference :H='Reference':R]
    ENDIF
ENDCASE



*:**************************************************************************
*:* Name        : lfPOCRTNPD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/29/2003
*:* Purpose     : Define the pad for Cartons dimensions type option
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfPOCRTNPD()
*:***************************************************************************
*C102789,1
FUNCTION lfALCRTNPD
PRIVATE lnBarNo
lnBarNo = CNTBAR('_OPTPOP') + 1
DEFINE BAR lnBarNo   OF _OPTPOP PROMPT "\-" SKIP FOR .T.
*DEFINE BAR lnBarNo+1 OF _OPTPOP PROMPT '\<Update Carton Dimensions type field'  ;
                SKIP FOR (lnActFolder<>1) .OR. (laScrMode[1] .OR. laScrMode[2])
DEFINE BAR lnBarNo+1 OF _OPTPOP PROMPT '\<Update Carton Dimensions type field'  ;
                SKIP FOR (laScrMode[1] .OR. laScrMode[2])
                
ON SELECTION BAR lnBarNo+1 OF _OPTPOP DO gfDoTriger WITH "ALPLIST",PADR("CRTNTYPE",10)
*-- end of lfPOCRTNPD.

*:**************************************************************************
*:* Name        : lfCRTNTYPE
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/29/2003
*:* Purpose     : Open the screen that updates the carton dimension field in Pack_lin file
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCRTNTYPE()
*:***************************************************************************
*C102789,1
FUNCTION lfCRTNTYPE
PRIVATE laCodes,lnCrtntype,laDfCod,lcSlctdCod
lnCrtntype = 1
DIMENSION laCodes[1,10],laCrtntype[1,2],laDfCod[1]
laCodes[1,1] = 'CCRTNVLTYP'
laCodes[1,2] = 'laCrtntype'
laCodes[1,3] = 'lnCrtntype'
laCodes[1,4] = ''
laCodes[1,5] = .F.
laCodes[1,6] = .F.
laCodes[1,10] = 'CCRTNVLTYP'

laCrtntype = ''
=gfwCodePop(@laCodes,'CCRTNVLTYP','L')

*--Get the "Default code value" position in the array "laCrtntype"
SELECT CCODE_NO FROM CODES WHERE CDEFCODE+CRLTFIELD+CFLD_NAME = 'DNCCRTNVLTYP' INTO ARRAY laDfCod
IF !EMPTY(laDfCod[1])
  DO CASE 
    CASE lnActFolder = 1
      IF SUBSTR(gcbasewind,4) = "ALPLIST"
        lcSlctdCod = IIF(!EMPTY(laData[17]),laData[17],laDfCod[1])
      ELSE
        lcFileNamA = lcTmpBrow+"A"
        lcSlctdCod = IIF(!EMPTY(&lcFileNamA..lcRPCart),&lcFileNamA..lcRPCart,laDfCod[1])
      ENDIF
    CASE lnActFolder = 2
      IF SUBSTR(gcbasewind,4) = "ALPLIST"
        lcSlctdCod = IIF(!EMPTY(&lcTmpPck..CCRTNVLTYP),&lcTmpPck..CCRTNVLTYP,laDfCod[1])
      ELSE
        lcSlctdCod = IIF(!EMPTY(&lcTmpPkLin..CCRTNVLTYP),&lcTmpPkLin..CCRTNVLTYP,laDfCod[1])
      ENDIF
    CASE lnActFolder = 3
      lcSlctdCod = IIF(!EMPTY(&lcCtnHdr..CCRTNVLTYP),&lcCtnHdr..CCRTNVLTYP,laDfCod[1])
  ENDCASE
  lnCrtntype = ASUBSCRIPT( laCrtntype , ASCAN(laCrtntype,lcSlctdCod) , 1 )
  DO (gcScrDir+gcWinAppl+'\ALCRTNTY.SPX')
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Carton Dimensions type'+;
        ' codes has not been set up. Please, set up and try again.')  
  RETURN 
ENDIF
*-- end of lfCRTNTYPE.

*:**************************************************************************
*:* Name        : lfvCrtnOk
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/29/2003
*:* Purpose     : Ok button for ALCRTNTY.SCX SCREEN
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvCrtnOk()
*:***************************************************************************
*C102789,1
FUNCTION lfvCrtnOk
PRIVATE lnPos, lcCrtNum
DO CASE
  CASE lnActFolder = 1
    IF SUBSTR(gcbasewind,4) = "ALPLIST"
      laData[17] = laCrtntype[lnCrtntype,2]
      lnPos = ASCAN(laUsrField,'CCRTNVLTYP')
      IF lnPos>0
        laUsrField[lnPos,6] = laData[17]
      ENDIF
    ELSE
      lcFileNamA = lcTmpBrow+"A"
      REPLACE &lcFileNamA..lcRPCart WITH laCrtntype[lnCrtntype,2]
    ENDIF
  CASE lnActFolder = 2
    IF SUBSTR(gcbasewind,4) = "ALPLIST"
      IF SEEK(&lcTmpPck..STYLE,lcPckLin)
        PRIVATE lcAlias 
        lcAlias = ALIAS()
        SELECT (lcPckLin)
        REPLACE cCrtnVlTyp WITH laCrtntype[lnCrtntype,2]
        SELECT (lcCtnDtl)
        SCAN 
          IF Style = &lcPckLin..Style
            IF SEEK(STR(Cart_No,4),lcCtnHdr) 
              REPLACE &lcCtnHdr..cCrtnVlTyp WITH laCrtntype[lnCrtntype,2]        
            ENDIF
          ENDIF
        ENDSCAN
        SELECT (lcAlias)      
      ENDIF
    ELSE
      lnRcNo = RECNO(lcTmpPkLin)
      lcCrtNum = STR(CartonNo,4)
      REPLACE ALL &lcTmpPkLin..cCrtnVlTyp WITH laCrtntype[lnCrtntype,2] FOR STR(CartonNo,4)=lcCrtNum
      GOTO lnRcNo IN &lcTmpPkLin
    ENDIF

  CASE lnActFolder = 3
    PRIVATE lcAlias 
    lcAlias = ALIAS()
    SELECT (lcCtnHdr)
    REPLACE cCrtnVlTyp WITH laCrtntype[lnCrtntype,2]
    SELECT (lcAlias)
ENDCASE
CLEAR READ
*-- end of lfvCrtnOk.

*:**************************************************************************
*:* Name        : lfADDTPFLD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 02/02/2003
*:* Purpose     : Add the field "CCRTNVLTYP" to the field list 
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfADDTPFLD()
*:***************************************************************************
*C102789,1
FUNCTION lfADDTPFLD
lcScFields = lcScFields + ',CCRTNVLTYP'
*-- end of lfADDTPFLD.

*:**************************************************************************
*:* Name        : lfSAVLINE
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/15/2003
*:* Purpose     : Save Carton type in pack_lin file from manual packing list program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSAVLINE()
*:***************************************************************************
*C102789,4
FUNCTION lfSAVLINE
SELECT (lcCtnHdr)
SCAN
  IF SEEK(laData[1]+STR(Cart_No,4),'Pack_Lin')
    SELECT Pack_Lin
    SCAN WHILE pack_no+STR(no_cart,4)+style = laData[1]+STR(&lcCtnHdr..Cart_No,4)
      REPLACE cCrtnVlTyp WITH IIF(EMPTY(&lcCtnHdr..cCrtnVlTyp),PACK_HDR.cCrtnVlTyp,&lcCtnHdr..cCrtnVlTyp)
    ENDSCAN
  ENDIF
ENDSCAN

*:**************************************************************************
*:* Name        : lfGetVol
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/15/2003
*:* Purpose     : Retrieve Carton type from pack_lin file from manual packing list program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetVol()
*:***************************************************************************
*C102789,4
FUNCTION lfGetVol
REPLACE &lcPckLin..cCrtnVlTyp WITH Pack_Lin.cCrtnVlTyp 
REPLACE &lcCtnHdr..cCrtnVlTyp WITH Pack_Lin.cCrtnVlTyp 

*:**************************************************************************
*:* Name        : lfVolApp
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/15/2003
*:* Purpose     : Save Carton type in lcCtnHdr file from Apply button in manual packing list program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfVolApp()
*:***************************************************************************
*C102789,4
FUNCTION lfVolApp
IF SEEK(STR(lnPackCtn,4),lcCtnHdr)
  REPLACE &lcCtnHdr..cCrtnVlTyp WITH &lcPckLin..cCrtnVlTyp
ENDIF    

*:**************************************************************************
*:* Name        : lfAddFld
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/15/2003
*:* Purpose     : Add cCrtnVlTyp in lcCtnHdr, lcPckLin files in manual packing list program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfAddFld()
*:***************************************************************************
*C102789,4
FUNCTION lfAddFld
lnFileStru = ALEN(laFileStru,1)
DIMENSION laFileStru[lnFileStru+1,4]
laFileStru[lnFileStru+1,1] = 'cCrtnVlTyp'
laFileStru[lnFileStru+1,2] = 'C'
laFileStru[lnFileStru+1,3] = 6
laFileStru[lnFileStru+1,4] = 0

*:**************************************************************************
*:* Name        : lfREMALL
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/15/2003
*:* Purpose     : Replace 'ALL' from carton type popup with 'N/A' in automatic packing list program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfREMALL()
*:***************************************************************************
*C102789,4
FUNCTION lfREMALL
lnLen = ALEN(LAOGPOPARY)
lnPos = ASCAN(LAOGPOPARY,'All')
LAOGPOPARY[lnPos]='N/A'

*:**************************************************************************
*:* Name        : lfSAVCRT 
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/15/2003
*:* Purpose     : Save carton type in pack_hdr & pack_lin files in automatic packing list program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSAVCRT ()
*:***************************************************************************
*C102789,4
FUNCTION lfSAVCRT 
lcFileNamA = lcCtnDtl+"A"
REPLACE PACK_HDR.cCrtnVlTyp WITH &lcFileNamA..lcRPCart
PRIVATE lcAlias 
lcAlias = ALIAS()
SELECT PACK_LIN
=SEEK(PACK_HDR.PACK_NO)
SCAN WHILE PACK_NO = PACK_HDR.PACK_NO
  REPLACE cCrtnVlTyp WITH &lcFileNamA..lcRPCart 
ENDSCAN
SELECT (lcAlias)

*:**************************************************************************
*:* Name        : lfINITVAR   
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/15/2003
*:* Purpose     : Create cursor to hold the carton type value in automatic packing list program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfINITVAR()
*:***************************************************************************
*C102789,4
FUNCTION lfINITVAR   
IF SUBSTR(gcbasewind,4) = "ALAUTP"
  lcFileNamA = lcCtnDtl+"A"
ELSE
  lcFileNamA = lcTmpBrow+"A"
ENDIF

CREATE CURSOR (lcFileNamA) (lcRPCart C(6))
APPEND BLANK


*:**************************************************************************
*:* Name        : lfDEFVAR 
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/15/2003
*:* Purpose     : Valid function for carton type option in automatic packing list program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDEFVAR()
*:***************************************************************************
*C102789,4
FUNCTION lfDEFVAR 
lcFileNamA = lcCtnDtl+"A"
REPLACE &lcFileNamA..lcRPCart WITH lcRPCart 

*:**************************************************************************
*:* Name        : lfActPad
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/16/2003
*:* Purpose     : Add carton type option to pack and scan program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfActPad()
*:***************************************************************************
*C102789,4
FUNCTION lfActPad

*B606886,1 ABD - Remark the next line and add new bar number.. [Begin]
*DEFINE PAD   _OPTIONS OF _MSYSMENU PROMPT 'O\<ptions' KEY ALT+P , SPACE(1)
*DEFINE POPUP _OPTPOP MARGIN 
*ON PAD _OPTIONS OF _MSYSMENU ACTIVATE POPUP _OPTPOP
*DEFINE BAR 1 OF _OPTPOP PROMPT '\<Update Carton Dimensions type field'  SKIP FOR (laScrMode[1] .OR. laScrMode[2])
*ON SELECTION BAR 1 OF _OPTPOP DO gfDoTriger WITH "ALPKSCN",PADR("CRTNTYPE",10)

DEFINE BAR 100 OF _OPTPOP PROMPT '\<Update Carton Dimensions type field'  SKIP FOR (laScrMode[1] .OR. laScrMode[2])
ON SELECTION BAR 100 OF _OPTPOP DO gfDoTriger WITH "ALPKSCN",PADR("CRTNTYPE",10)
*B606886,1 ABD - [End]

*:**************************************************************************
*:* Name        : lfCrtTmp
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/16/2003
*:* Purpose     : Add carton type to the tmp file in pack and scan program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrtTmp()
*:***************************************************************************
*C102789,4
FUNCTION lfCrtTmp
 CREATE TABLE (gcWorkDir+lcTmpPkLin) ;
              (CartonNo N(4),cUpc C(12),Style C(19),cSize C(5), Size C(2),;
                TotQty N(5),Weight N(10,2) ,OldCrtNo N(4), OrdLineNo N(6), CCRTNVLTYP C(6))

*:**************************************************************************
*:* Name        : lfSavLn
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/16/2003
*:* Purpose     : Save carton type in pack_lin file for pack and scan program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSavLn()
*:***************************************************************************
FUNCTION lfSavLn
lcFileNamA = lcTmpBrow+"A"
REPLACE CCRTNVLTYP WITH IIF(EMPTY(&lcTmpPkLin..CCRTNVLTYP),&lcFileNamA..lcRPCart,&lcTmpPkLin..CCRTNVLTYP)

*:**************************************************************************
*:* Name        : lfGetDtLn
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/16/2003
*:* Purpose     : Get data from pack_lin file for pack and scan program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetDtLn()
*:***************************************************************************
FUNCTION lfGetDtLn
IF !EOF(lcTmpPkLin) 
  REPLACE &lcTmpPkLin..CCRTNVLTYP WITH Pack_Lin.CCRTNVLTYP 
ENDIF

*:**************************************************************************
*:* Name        : lfSavHdr
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/16/2003
*:* Purpose     : Save carton type in pack_hdr file for pack and scan program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfSavHdr()
*:***************************************************************************
*C102789,4
FUNCTION lfSavHdr
lcFileNamA = lcTmpBrow+"A"
REPLACE CCRTNVLTYP WITH &lcFileNamA..lcRPCart 		

*:**************************************************************************
*:* Name        : lfGetData
*:* Developer   : ASH - Ashraf Sherif
*:* Date        : 02/16/2003
*:* Purpose     : Get data from pack_hdr file for pack and scan program.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfGetData()
*:***************************************************************************
*C102789,4
FUNCTION lfGetData
lcFileNamA = lcTmpBrow+"A"
REPLACE &lcFileNamA..lcRPCart WITH PACK_HDR.CCRTNVLTYP 
