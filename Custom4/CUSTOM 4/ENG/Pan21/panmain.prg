*!**************************************************************************
*! Name      : PANMAIN.PRG
*! Developer : Tarek Mohammed Ibrhim
*! Date      : 11/20/2008
*! Purpose   : Custom triggers for DCC customer ( DIR03 )
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*Modifications
*B608850,1 TMI 04/27/2009 Allow to view the order charges in the view mode
*B609283,1 MMT 06/03/2010 Fix bug of not updating order charges in case of EDI Tmp Order[T20100512.0008]
*B609695,1 MAB 10/12/2011 Error adding New Order Charges at DCC [T20111004.0012]
*B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [T20111004.0012]
*B609695,3 MMT 12/15/2011 Display generic charges+Account Chanrges[T20111004.0012]
*B609695,4 MMT 06/18/2012 Menu of order charges is not displayed in Order screen[T20120522.0002]
*!**************************************************************************

PARAMETER loFormSet,lcEvntFun,lcFunPars
lcFunPars  = IIF(TYPE('lcFunPars') = 'C',lcFunPars,'')
lcFunToRun = 'lf'+ALLT(lcEvntFun)+'('+lcFunPars+')'

*--Run the function.
llRetValue = EVAL(lcFunToRun)

RETURN llRetValue

*:**************************************************************************
*:* Name        : lfAddMPad
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/20/2008
*:* Purpose     : Add the custom options to the Invoice Sales order screen 
*:***************************************************************************
*:* Called from : ARIINV.SCX mOptionsMenuPad method
*:***************************************************************************
*C201070 TMI ( The aria27 attach. in *C201071 )
FUNCTION lfAddMPad
LOCAL lnBarNo

*-- Count Options BARS to add the new BAR at the end of the Popup
lnBarNo = CNTBAR('_INQURYPOP') + 1

*&&>>> - make sure that these are called only in Add mode
DEFINE BAR lnBarNo OF _INQURYPOP PROMPT '\<Generate Consolidated Invoices' ;
       SKIP FOR !EMPTY(_Screen.ActiveForm.keyOrder.Keytextbox.Value)    &&>>> what is the case if consolidated and we in the first line
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfDispScr IN PANMAIN WITH .T.

DEFINE BAR lnBarNo+1 OF _INQURYPOP PROMPT 'Generate In\<voices from Ship Date' ;
       SKIP FOR !EMPTY(_Screen.ActiveForm.keyOrder.Keytextbox.Value)
ON SELECTION BAR lnBarNo+1 OF _INQURYPOP DO lfDispScr IN PANMAIN WITH .F.
*-- END OF FUNCTION lfAddMPad.

*:**************************************************************************
*:* Name        : lfDispScr
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/20/2008
*:* Purpose     :  Calling the 'Generate Consolidated Invoices'   and
*!              :              'Generate Invoices from ship Date' Screens.
*:***************************************************************************
*C201070 
FUNCTION lfDispScr
PARAMETER llConsInv
LOCAL lnSlct
lnSlct = SELECT(0)

DO FORM (oAriaApplication.ScreenHome + 'AR\ARCOINV.SCX') WITH _Screen.ActiveForm.Parent,llConsInv

SELECT(lnSlct)
RETURN
*-- END OF FUNCTION lfDispScr

*:**************************************************************************
*:* Name        : lfADORDOP    && Add Order screen Options
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/20/2008
*:* Purpose     : Add the DCC custom options to the SOORD.SCX screen
*:***************************************************************************
*:* Called from : SOORD.SCX
*:***************************************************************************
*C201070 
FUNCTION lfADORDOP
PRIVATE lnBarNo
lnBarNo = CNTBAR('_INQURYPOP') + 1
*B608850,1 TMI 04/27/2009 [Start] allow to view the order charges in the view mode
*DEFINE BAR lnBarNo   OF _INQURYPOP PROMPT '\<View Order Charges' ;
       SKIP FOR _Screen.ActiveForm.Parent.ActiveMode <> 'E' .OR. (_Screen.ActiveForm.Parent.ActiveMode = 'E' .AND. _Screen.ActiveForm.cboStatus.Value='C')
DEFINE BAR lnBarNo   OF _INQURYPOP PROMPT '\<View Order Charges' ;
       SKIP FOR _Screen.ActiveForm.Parent.ActiveMode $ 'SA' 
*B608850,1 TMI 04/27/2009 [End  ]
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfOpChrgOrdSc IN PANMAIN.PRG
*B609695,4 MMT 06/18/2012 Menu of order charges is not displayed in Order screen[Start]
loFormSet.NextOptionBar = loFormSet.NextOptionBar + 1
*B609695,4 MMT 06/18/2012 Menu of order charges is not displayed in Order screen[End]
*-- end of lfADORDOP.

*:**************************************************************************
*:* Name        : lfOpChrgOrdSc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/20/2008
*:* Purpose     : Calls the SOORDCHG screen
*:***************************************************************************
*C201070 
FUNCTION lfOpChrgOrdSc

DO FORM (oAriaApplication.ScreenHome + 'SO\SOORDCHG.SCX') WITH _Screen.ActiveForm.Parent

*-- end of lfOpChrgOrdSc.

*:**************************************************************************
*:* Name        : lfBRWSVAR
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/20/2008
*:* Purpose     : Currently to prevent corrupting due to a trigger already found in the screen and is called with no conditions
*:***************************************************************************
*:* Called from : SOORD.SCX
*:***************************************************************************
*C201070 
FUNCTION lfBRWSVAR

*-- end of lfBRWSVAR.

*:**************************************************************************
*:* Name        : lfUnLock
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/23/2008
*:* Purpose     : Lock and Unlock a table
*:***************************************************************************
*C201070 
FUNCTION lfTableLock
LPARAMETERS lcTable,llLock
PRIVATE lnSlct
lnSlct = SELECT(0)

SELECT &lcTable
=RLOCK()
IF llLock
  REPLACE lLok_Stat WITH .T.         ,;
          cLok_User WITH oAriaApplication.User_ID   ,;
          dLok_Date WITH DATE()      ,;
          cLok_Time WITH gfGetTime()
ELSE 
  REPLACE lLok_Stat WITH .F. ,;
          cLok_User WITH ''  ,;
          dLok_Date WITH {}  ,;
          cLok_Time WITH ''
ENDIF        
UNLOCK       
 
SELECT(lnSlct)

*--End of Function lfUnLock.

*:**************************************************************************
*:* Name        : lfINVUDFUP    && invoice UDF updates
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2008
*:* Purpose     : Update the user field INVHDR.PACK_DATE
*:***************************************************************************
*:* Called from : ARIINV.SCX savefiles method
*:***************************************************************************
*C201070 
FUNCTION lfINVUDFUP
LOCAL lnSlct
lnSlct = SELECT(0)

SELECT (loFormSet.lcInvHdr)
LOCATE FOR INVOICE = INVHDR.INVOICE
IF FOUND()
  SELECT INVHDR
  REPLACE PACK_DATE WITH EVALUATE(loFormSet.lcInvhdr+'.PACK_DATE')
ENDIF

SELECT (lnSlct)
*-- end of lfINVUDFUP.

*:**************************************************************************
*:* Name        : lfPCKDTBRW
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2008
*:* Purpose     : Add the field PACK_DATE	 to the browse for DIR03
*:***************************************************************************
*:* Called from : ARDINV.SCX lfInvBrow
*:***************************************************************************
*C201070 
FUNCTION lfPCKDTBRW

lcBrFields = lcBrFields + ",PACK_DATE :H='Packed Date'"

*-- end of lfPCKDTBRW.

*:**************************************************************************
*:* Name        : lfCHGACC
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/09/2005,11/24/2008
*:* Purpose     : Valid function for charged account field
*:***************************************************************************
*:* Called from : SMCODES screen , validate the field CHGACC, that is related to CORDCHG
*:***************************************************************************
*C201070 
FUNCTION lfCHGACC
LOCAL lnSlct,lcControl,lcCurVar
PRIVATE xAccount

lnSlct = SELECT(0)

IF !USED('CUSTOMER')
  =gfOpenTable(oAriaApplication.DataDir+'CUSTOMER','CUSTOMER','SH')
ENDIF

lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.

IF !EMPTY(lcCurVar.Value) .AND. !SEEK('M'+lcCurVar.Value,'CUSTOMER')
  xAccount = lcCurVar.Value
  SELECT CUSTOMER
  PRIVATE lcBrFields, lcBrowTitle  
  DO CUSBROWM WITH xAccount
  loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = xAccount
  = lfOGShowGet(lcCurVar.Parent.nRowIndex)
ENDIF
  
SELECT (lnSlct)
*-- end of lfCHGACC.


*:**************************************************************************
*:* Name        : lfCHGAMNT
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2008
*:* Purpose     : Valid function for charged amount field
*:***************************************************************************
*:* Called from : SMCODES screen , validate the field NORDCHG, that is related to CORDCHG
*:***************************************************************************
*C201070 
FUNCTION lfCHGAMNT
LOCAL lnSlct,lcControl,lcCurVar
lnSlct = SELECT(0)

lcControl  = loOGScroll.FocusControl
lcCurVar   = loOGScroll.&lcControl.
IF lcCurVar.Value < 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Charge amount can not be negative')
  loOgScroll.laOGFxFlt[lcCurVar.Parent.nRowIndex,6] = lcCurVar.OldValue
  lfOGShowGet(lcCurVar.Parent.nRowIndex)
ENDIF
  
SELECT (lnSlct)

*-- end of lfCHGAMNT.
*:**************************************************************************
*:* Name        : lfMRCHDSCD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/09/2005,11/27/2008
*:* Purpose     : update codes file with a code in "Non-merchandise charges" for each new code in "Order charges"
*:***************************************************************************
*:* Called from : SAVE in SMCODES.PRG 
*:***************************************************************************
*C201070 
FUNCTION lfMRCHDSCD
LOCAL lnSlct,lcSvOrd,lcVldFld,laStru,lcLinesToUpdate
lnSlct = SELECT (0)

lcSvOrd = ORDER(loFormSet.lcTmpCodes)
SELECT (loFormSet.lcTmpCodes)        
SET ORDER TO CCODE_NO  && KEY :: CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM

LOCAL lcWhere,lcTmpCurs
lcTmpCurs = gfTempName()
lcWhere = "CFLD_NAME = 'CTAXCODE  ' .AND. "+;
          "CRLTD_NAM = 'NTAXRATE  ' .AND. "+;
          "'0.0' $ CRLTD_VLU AND VAL(CRLTD_VLU ) = 0"
IF !lfSQLStatement("SELECT CCODE_NO FROM CODES WHERE &lcWhere", lcTmpCurs, '', '', lfIsNative('CODES') = 1)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No tax code with tax rate 0 was found, unable to continue saving process')
  SET ORDER TO &lcSvOrd IN (loFormSet.lcTmpCodes)
  SELECT (lnSlct)  
  RETURN .F.
ENDIF

SELECT (loFormSet.lcTmpCodes)        
DIMENSION laStru[1,18]
=AFIELDS(laStru)
lcLinesToUpdate = gfTempName()
CREATE CURSOR &lcLinesToUpdate FROM ARRAY laStru
SELECT(loFormSet.lcTmpCodes)
SCAN FOR CSTATUS='S'
  SCATTER MEMVAR MEMO
  SELECT &lcLinesToUpdate
  APPEND BLANK
  GATHER MEMVAR MEMO
ENDSCAN


SELECT &lcLinesToUpdate
SCAN 
  IF !SEEK('N'+'CCHRGCODE '+&lcLinesToUpdate..CCODE_NO,loFormSet.lcTmpCodes)
    INSERT INTO (loFormSet.lcTmpCodes) ;
           (cdefcode,cfld_name,ccode_no, ;
            cdiscrep,crltfield, ;
            cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
            cadd_user,dadd_date,cadd_time,cStatus) ;
    VALUES ('N','CCHRGCODE ',&lcLinesToUpdate..CCODE_NO, ;
            &lcLinesToUpdate..CCODE_NO ,"N","",;
            "","", ;
            oAriaApplication.User_ID,DATE(),gfGetTime(),"S")
  ENDIF
 
  SELECT (loFormSet.lcTmpCodes)
  LOCATE FOR CDEFCODE+CFLD_NAME+CCODE_NO = 'N'+'CCHRGCODE '+&lcLinesToUpdate..CCODE_NO AND ;
             CRLTFIELD = 'Y' AND ;
             CRLTD_NAM = 'CTAXCODE'
  IF !FOUND()
  INSERT INTO (loFormSet.lcTmpCodes) ;
         (cdefcode,cfld_name,ccode_no, ;
          cdiscrep,crltfield, ;
          cRltd_Nam,cRltd_Typ,cRltd_Vlu,;
          cadd_user,dadd_date,cadd_time,cStatus) ;
  VALUES ('N','CCHRGCODE ',&lcLinesToUpdate..CCODE_NO, ;
          "","Y","CTAXCODE  ",;
          "C",&lcTmpCurs..CCODE_NO, ;
          oAriaApplication.User_ID,DATE(),gfGetTime(),"S")
ENDIF
ENDSCAN

SET ORDER TO &lcSvOrd IN (loFormSet.lcTmpCodes)
SELECT (lnSlct)  
*-- end of lfMRCHDSCD.


*:**************************************************************************
*:* Name        : lfCHRGORD   
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 11/24/2008
*:* Purpose     : Add a charge to the order , Update the ORDCHG sql file
*:***************************************************************************
*:* Called from : SOUPDATE.PRG lfSavScr
*:***************************************************************************
*C201070 
FUNCTION lfCHRGORD
LPARAMETERS loOrdChg
IF TYPE('loOrdChg') = 'O'
  loFormSet = loOrdChg.loParentForm
  loList = loOrdChg.ARIAFORM1.lstCharges
ENDIF
PRIVATE lcOrdChg
LOCAL lnSlct,lcSvOrd,lcAccount
lnSlct = SELECT(0)

*B609695,1 MAB 10/12/2010 Error adding New Order Charges at DCC [Start]
LOCAL lcOrdHdr
lcOrdHdr = loformset.OFORMENVIRONMENT.lcOrdHdr
*B609695,1 MAB 10/12/2010 Error adding New Order Charges at DCC [End]
*B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [Start]
IF TYPE('lcActiveMode') = 'U'
  LOCAL lcOrderNo
  lcOrderNo = &lcOrdHdr..Order
ENDIF  
*B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [ENd]
*B609283,1 MMT 06/03/2010 Fix bug of not updating order charges in case of EDI Tmp Order[Start]
*lcAccount = loFormSet.Ariaform1.keyAccount.Keytextbox.Value
lcAccount =  &lcOrdHdr..Account
*B609283,1 MMT 06/03/2010 Fix bug of not updating order charges in case of EDI Tmp Order[End]
=gfSEEK('M'+lcAccount,'CUSTOMER')   

IF !USED('ORDCHG')
  =gfOpenTable(oAriaApplication.DataDir+'ORDCHG','ORDCHG','SH')
ENDIF
*B609283,1 MMT 06/03/2010 Fix bug of not updating order charges in case of EDI Tmp Order[Start]
*IF IIF(TYPE('loOrdChg')<>'O',loFormSet.ActiveMode <> 'A',.F.)
*B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [Start]
*IF IIF(TYPE('loOrdChg')<>'O',; 
   IIF(lcActiveMode='A' OR  (lcActiveMode ='E' AND &lcOrdHdr..lfromweb=.T. AND &lcOrdHdr..Status<> 'B' AND &lcOrdHdr..cOrdType='T'),.F.,.T.) ,.F.)
IF IIF(TYPE('loOrdChg')<>'O',; 
   IIF(TYPE('lcActiveMode')<>'U',IIF(lcActiveMode='A' OR  (lcActiveMode ='E' AND &lcOrdHdr..lfromweb=.T. AND &lcOrdHdr..Status<> 'B' AND &lcOrdHdr..cOrdType='T'),.F.,.T.),loFormSet.ActiveMode = 'E'),.F.)
*B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [ENd]
*B609283,1 MMT 06/03/2010 Fix bug of not updating order charges in case of EDI Tmp Order[End]
*B609695,3 MMT 12/15/2011 Display generic charges+Account Chanrges[Start]
*!*    SELECT 'ORDCHG'
*!*    =gfTableUpdate()
*B609695,3 MMT 12/15/2011 Display generic charges+Account Chanrges[END]
  =gfCloseTable('ORDCHG')
  
ELSE
  
  *- check Charge Carriage user field
  *B609283,1 MMT 06/03/2010 Fix bug of not updating order charges in case of EDI Tmp Order[Start]
  *lnBookAmt = loFormSet.Ariaform1.Ariapageframe1.Page3.cntSummary.txtBookedAmnt.Value
  lnBookAmt = &lcOrdHdr..BookAmt 
  *B609283,1 MMT 06/03/2010 Fix bug of not updating order charges in case of EDI Tmp Order[End]
  *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [Start]
  *IF CUSTOMER.lChrgchg .AND. 0 < CUSTOMER.nMinOrdChg .AND. lnBookAmt <= CUSTOMER.nMinOrdChg
  IF IIF(TYPE('lcActiveMode') = 'U',CUSTOMER.lChrgchg,CUSTOMER.lChrgchg .AND. 0 < CUSTOMER.nMinOrdChg .AND. lnBookAmt <= CUSTOMER.nMinOrdChg)  
  *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [End]
    
    lcOrdChg = gfTempName()
    CREATE TABLE (oAriaApplication.WorkDir+lcOrdChg) ;
        (CORDCHG C(6),CDISCREP C(30),NORDCHG N(8,2),CHGACC C(5),STNAME C(30),CUPDATE C(1))
    INDEX ON CORDCHG TAG &lcOrdChg
  
    SELECT CODES
    lcSvOrd = ORDER('CODES')
    =gfSetOrder('CCODE_NO')     && key :: CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM
    
    IF gfSEEK('N'+'CORDCHG   ','CODES')
      DO WHILE CFLD_NAME = 'CORDCHG   '
        m.CORDCHG = CODES.CCODE_NO  
        STORE '' TO m.CHGACC,m.CDISCREP
        m.NORDCHG = 0
        SCAN REST WHILE CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'NCORDCHG   '+m.CORDCHG
          IF CRLTFIELD = 'N'
            M.CDISCREP = CODES.CDISCREP 
          ELSE
            lcRlFld = 'm.'+ALLTRIM(CODES.CRLTD_NAM)
            &lcRlFld = IIF(CODES.CRLTD_TYP = 'N' , VAL(CODES.CRLTD_VLU) , CODES.CRLTD_VLU )
          ENDIF
        ENDSCAN      
      IF (EMPTY(m.CHGACC) .OR. m.CHGACC = lcAccount) .AND.;
          !SEEK(m.CORDCHG,lcOrdChg)
          INSERT INTO &lcOrdChg FROM MEMVAR
        ENDIF
      ENDDO
    ENDIF
      
    *- if there are Order Charges in the Codes file that are specific to that customer – then only 
    *- those should be shown – if not order charges are specific to the customer then all those that 
    *- are not assigned to a customer should be displayed
    SELECT &lcOrdChg
   *B609695,3 MMT 12/15/2011 Display generic charges+Account Chanrges[Start]    
*!*	    LOCATE
*!*	    LOCATE FOR !EMPTY(&lcOrdChg..CHGACC)
*!*	    IF FOUND()
*!*	      DELETE FOR EMPTY(&lcOrdChg..CHGACC)
*!*	    ENDIF  
    *B609695,3 MMT 12/15/2011 Display generic charges+Account Chanrges[END] 
    LOCATE
    IF RECCOUNT(lcOrdChg)>0
      *- Pick one of the order charges  

      IF !EMPTY(lfLocBrow())
        SELECT ORDCHG     
        *B609283,1 MMT 06/03/2010 Fix bug of not updating order charges in case of EDI Tmp Order[Start] 
        *IF loFormSet.ActiveMode = 'A'
        *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [Start]        
        *IF (lcActiveMode = 'A') OR (lcActiveMode ='E' AND &lcOrdHdr..lfromweb=.T. AND &lcOrdHdr..Status<> 'B' AND &lcOrdHdr..cOrdType='T')               
        IF IIF(TYPE('lcActiveMode') <> 'U',(lcActiveMode = 'A') OR (lcActiveMode ='E' AND &lcOrdHdr..lfromweb=.T. AND &lcOrdHdr..Status<> 'B' AND &lcOrdHdr..cOrdType='T') ,loFormSet.ActiveMode = 'E')
        *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [End]
		*B609283,1 MMT 06/03/2010 Fix bug of not updating order charges in case of EDI Tmp Order[End]
          *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [Start]
          SELECT (lcOrdChg)
          SCAN 
          SELECT ORDCHG     
          *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [ENd]

            IF !gfSEEK(lcOrderNo+&lcOrdChg..CORDCHG,'ORDCHG')
              lcReplace = 'ORDER    WITH "'+lcOrderNo+'" '+;
                          'CORDCHG  WITH "'+&lcOrdChg..CORDCHG+'" '+;
                          'NORDCHG  WITH '+STR(&lcOrdChg..NORDCHG,8,3)+' '+;
                          'INVOICED WITH .F. '+;
                          'INVOICE  WITH "" '
              =gfAppend()
              =gfReplace(lcReplace)
              =gfTableUpdate()               
            ENDIF
            *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [Start]
            SELECT (lcOrdChg)
          ENDSCAN
          
          IF TYPE('loList')='O'
            SELECT (lcOrdChg)
            SCAN 
              llUpdated = .F.
              FOR lnI = 1 TO loList.ListCount
                IF loList.List(lnI,1)=&lcOrdChg..CORDCHG
                  loList.List(lnI,2) = STR(&lcOrdChg..NORDCHG,8,2)
                  llUpdated = .T.
                  EXIT
                ENDIF
              ENDFOR
              IF !llUpdated
                loList.AddItem(&lcOrdChg..CORDCHG+'-'+gfCodDes(&lcOrdChg..CORDCHG,'CORDCHG'))
                loList.AddListItem(STR(&lcOrdChg..NORDCHG,8,2),loList.ListCount,2)
              ENDIF
            ENDSCAN 
          ENDIF
          *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [ENd]
        
        ELSE
          
          && Here called from the SOORDCHG screen , when editing a sales order an needs to update the charges
          && the updates apply only on the local screen
          IF TYPE('loList')='O'
            *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [Start]
            SELECT (lcOrdChg)
            SCAN 
            *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [END]
              llUpdated = .F.
              FOR lnI = 1 TO loList.ListCount
                IF loList.List(lnI,1)=&lcOrdChg..CORDCHG
                  loList.List(lnI,2) = STR(&lcOrdChg..NORDCHG,8,2)
                  llUpdated = .T.
                  EXIT
                ENDIF
              ENDFOR
              IF !llUpdated
                loList.AddItem(&lcOrdChg..CORDCHG+'-'+gfCodDes(&lcOrdChg..CORDCHG,'CORDCHG'))
                loList.AddListItem(STR(&lcOrdChg..NORDCHG,8,2),loList.ListCount,2)
              ENDIF
            *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [Start]
            ENDSCAN 
            *B609695,2 MMT 11/30/2011 Error adding New Order Charges at DCC [END]
            
          ENDIF
        
        ENDIF
        
      ENDIF
    ELSE
      =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No charge codes applied to this order.')    
    ENDIF
    
    USE IN &lcOrdChg
    ERASE (oAriaApplication.WorkDir+lcOrdChg+'.DBF')
    ERASE (oAriaApplication.WorkDir+lcOrdChg+'.CDX')
  
    SELECT CODES
    =gfSetOrder(lcSvOrd) 
  
  ELSE
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,'No charges applied to this order.')
  ENDIF
  
ENDIF
  
SELECT (lnSlct)
*-- end of lfCHRGORD.

*:**************************************************************************
*:* Name        : lfLocBrow
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 12/13/2004, then 11/25/2008
*:* Purpose     : A local browse one can edit some fields in it
*:***************************************************************************
*C201070 
FUNCTION lfLocBrow
LOCAL lnSlct,llOk
lnSlct = SELECT(0)

DO FORM (oAriaApplication.ScreenHome + 'SO\SOSLORCH.SCX') TO llOk

SELECT(lnSlct)
RETURN llOk

*-- end of lfLocBrow.



*:**************************************************************************
*:* Name        : lfINVCHARG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 01/12/2008
*:* Purpose     : Update invoice charges for DIR03
*:***************************************************************************
*:* Called from : ARINV.PRG
*:***************************************************************************
*C201070 
FUNCTION lfINVCHARG
LOCAL lnSlct
lnSlct = SELECT()

IF !USED('ORDCHG')
  =gfOpenTable(oAriaApplication.DataDir+'ORDCHG','ORDCHG','SH')
ENDIF

PRIVATE lcReplace,lcChargesFile

lcChargesFile = loFormSet.AriaForm1.Ariapageframe1.Page4.cntInvoicesummary.ChargesFile
lcHdrFile = loFormSet.lcInvHdr
*C201070 TMI 15/12/2008 [Start] if this is a consolidated customer then change the save way
SELECT &lcHdrFile
SCAN FOR CONSOL <> 'Y'
  
  IF gfSEEK(&lcHdrFile..ORDER,'ORDCHG') .AND. !ORDCHG.INVOICED
    LOCAL llUpdate
    SELECT ORDCHG
    SCAN REST WHILE ORDER+CORDCHG = &lcHdrFile..ORDER
      *IF SEEK(&lcHdrFile..ORDER+&lcHdrFile..STORE+&lcHdrFile..PIKTKT+ORDCHG.CORDCHG,lcChargesFile)
      SELECT &lcChargesFile
      LOCATE FOR ORDER+PIKTKT+CCHRGCODE = &lcHdrFile..ORDER+&lcHdrFile..PIKTKT+ORDCHG.CORDCHG
      IF FOUND()
        llUpdate = .T.
        lcReplace = 'INVOICE  WITH "'+&lcHdrFile..INVOICE+'" '+;
                    'INVOICED WITH .T.'
        SELECT ORDCHG
        =gfReplace(lcReplace)
      ENDIF
    ENDSCAN
    
    IF llUpdate
      SELECT ORDCHG
      =gfTableUpdate()
    ENDIF
    
  ENDIF
  
ENDSCAN

*-- end of lfINVCHARG.


*:**************************************************************************
*:* Name        : lfVDORDCHG
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/12/2008
*:* Purpose     : Update the ORDCHG file by removing the Inv# and assign .F. to Invoiced field
*:***************************************************************************
*:* Called from : ARDINV.SCX DELETE method
*:***************************************************************************
*C201070 
FUNCTION lfVDORDCHG
LOCAL lnSlct,lcSvOrder,lcInvoice
PRIVATE lcReplace
lnSlct = SELECT()
lcSvOrder = ''
IF !USED('ORDCHG')
  =gfOpenTable(oAriaApplication.DataDir+'ORDCHG','INVOICE','SH')
ELSE
  SELECT ORDCHG
  lcSvOrder = ORDER()
  =gfSetOrder('INVOICE')  
ENDIF

*- Order charges are added only to the first invoice ( in case if the order is shipped via more than one invoice )
lcInvoice = loFormSet.AriaForm1.keyInvoice.Keytextbox.Value
IF gfSEEK(lcInvoice,'ORDCHG') .AND. ORDCHG.INVOICED
  SELECT ORDCHG
  SET ORDER TO 
  LOCATE
  SCAN FOR INVOICE = lcInvoice
    lcReplace = 'INVOICE  WITH "" '+;
                'INVOICED WITH .F.'
    =gfReplace(lcReplace)
  ENDSCAN
ENDIF
SELECT ORDCHG
=gfTableUpdate()

IF !EMPTY(lcSvOrder)
  SELECT ORDCHG
  =gfSetOrder(lcSvOrder)
ELSE
  =gfCloseTable('ORDCHG')
ENDIF

SELECT (lnSlct)

*-- end of lfVDORDCHG.

*:**************************************************************************
*:* Name        : lfCHNGMOD
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 03/12/2008
*:* Purpose     : close the ORDCHG file when mode changes in SOORD.SCX screen
*:***************************************************************************
*:* Called from : SOORD.SCX changemode method
*:***************************************************************************
FUNCTION lfCHNGMOD
IF USED('ORDCHG')
  =gfCloseTable('ORDCHG')
ENDIF

*-- end of lfCHNGMOD.


*!*************************************************************
*! Name      : lfGetOrder
*! Developer : Nader NABIL (NNA)
*! Date      : 09/02/2005
*! Purpose   : This Function copied from Ariinv Program and I made
*!             some Changes in it to be suitable to Get my order 
*!             information
*!*************************************************************
*! Calls     : gfRltFld,gfChkRate,gfModalGen
*!*************************************************************
*! Parameters: lcOrderNo  : Order Number
*!             lcStoreNo  : Store
*!             llPicked   : Picked lines only
*!*************************************************************
*! Returns   : None
*!*************************************************************
*! Example   :  =lfGetOrder()
*!*************************************************************
FUNCTION lfGetOrder
PARAMETERS lcOrderNo,lcStoreNo,llPicked ,lcPikTkt
PRIVATE ldDueDate,lcStore,lcCustLink,lcCustSales,lcSaleRep1,lcSaleRep2,;
        lcCurrCode,lnExRate,lnCurrUnit,llPacked,lcLinesCond,lnTaxQty,;
        lnTaxRate,llBackOrd

PRIVATE lnAllCartn
lnAllCartn = 0
PRIVATE lnSyUserRec , lcLok_User
IF OrdHdr.lLok_Stat
  *-- Display the message "Record is in use by user AAAA"
  lnSyUserRec   = IIF(RECNO('SyUUser')>RECCOUNT('SyUUser'),0,RECNO('SyUUser'))
  lcLok_User = ALLTRIM(PROPER(LOOKUP(SyUUser.cUsr_name,OrdHdr.cLok_User,SyUUser.cUser_id,'cUser_id')))
  lcLok_User = IIF(EMPTY(lcLok_User),gcUser_ID,lcLok_User)
  IF lnSyUserRec > 0
    GO lnSyUserRec IN SyUUser
  ENDIF  
          
  *-- Record is in use by user ????    
  IF gfModalGen("INM40182B00000","ALERT",'Order #' + OrdHdr.Order + '|' + lcLok_User) = 1
    RETURN(.F.)
  ENDIF
ENDIF
IF OrdHdr.Status='C'
  *--Message : 40109
  *--Order# 999999 has been shipped complete.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40109B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF OrdHdr.Status = 'X'
  *--Message : 40110
  *--Order# 999999 has been cancelled! Cannot ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40110B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF OrdHdr.Status = 'B'
  *--Message : 40155
  *--Order# 999999 is Bid. Cannot ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40155B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF !SEEK('M'+OrdHdr.Account,'Customer')
  *--Message : 40112
  *--Account XXXXX not found. Cannot Ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40112B00000','ALERT','Account '+OrdHdr.Account)
  RETURN(.F.)
ENDIF
IF Customer.Status <> 'A'
  *--Message : 40113
  *--Account XXXXX is Non-Active. Invoicing is not allowed.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40113B00000','ALERT','Account '+OrdHdr.Account)
  RETURN(.F.)
ENDIF
IF !EMPTY(lcStoreNo) .AND. !SEEK('S'+OrdHdr.Account+lcStoreNo,'Customer')
  *--Message : 40112
  *--Store XXXXX not found. Cannot Ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40112B00000','ALERT','Store '+ALLTRIM(lcStoreNo))
  RETURN(.F.)
ENDIF
IF Customer.Status <> 'A'
  *--Message : 40113
  *--Store XXXXX is Non-Active. Invoicing is not allowed.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40113B00000','ALERT','Store '+ALLTRIM(lcStoreNo))
  RETURN(.F.)
ENDIF
IF !SEEK('O'+lcOrderNo,'ordline')
  *--Message : 40114
  *--The lines for order# 999999 were not found.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40114B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF OrdHdr.Bulk='Y' AND laSetups[21,2]='N'
  *--Message : 40151
  *--Order# 999999 is a Bulk order. Cannot Ship.
  *--Button : 00000
  *--Ok
  =gfModalGen('TRM40151B00000','ALERT',lcOrderNo)
  RETURN(.F.)
ENDIF
IF OrdHdr.Status = 'H' 
  IF laSetups[22,2]='Y'
    *--Message : 40111
    *--Order# 999999 is On Hold.
    *--Button : 40003
    *--Proceed Cancel
    IF gfModalGen('QRM40111B40003','ALERT',lcOrderNo+'| ')=2
      RETURN(.F.)
    ENDIF
  ELSE
    *--Message : 40111
    *--Order# 999999 is On Hold. Cannot Ship.
    *--Button : 00000
    *--Ok
    =gfModalGen('TRM40111B00000','ALERT',lcOrderNo+'| Cannot Ship.')
    RETURN(.F.)
  ENDIF
ENDIF
IF !OrdHdr.lLok_Stat
  PRIVATE lcCurAlias
  lcCurAlias = ALIAS()
  SELECT OrdHdr
  =RLOCK()
  REPLACE lLok_Stat WITH .T.         ,;
          cLok_User WITH gcUser_ID   ,;
          dLok_Date WITH DATE()      ,;
          cLok_Time WITH gfGetTime()
  UNLOCK
  IF !EMPTY(lcCurAlias)
    SELECT (lcCurAlias)
  ENDIF  
ENDIF
IF ldDefInvDate < ORDHDR.START
  *--Message : 40116
  *--Order# 999999 start date is not until 99/99/9999.
  *--Button : 00000
  *--Ok
  =gfModalGen('INM40116B00000','ALERT',lcOrderNo+'|'+DTOC(ORDHDR.START))
ENDIF
IF 'AL' $ gcCmpModules
  IF !USED('ORDERPCK')
    =gfOpenFile(gcDataDir+'PACK_HDR',gcDataDir+'Orderpck','SH') 
  ENDIF
  IF !USED('PACK_LIN')
    =gfOpenFile(gcDataDir+'PACK_LIN',gcDataDir+'PACK_LIN','SH')
  ENDIF
ENDIF
*-- if the function was not called from the scope screen
IF !USED('STYLE')
  =gfOpenFile(gcDataDir+'STYLE',gcDataDir+'STYLE','SH') 
ENDIF
IF !USED('STYDYE')
  =gfOpenFile(gcDataDir+'STYDYE',gcDataDir+'STYDYE','SH')
ENDIF
IF !USED('SCALE')
  =gfOpenFile(gcDataDir+'SCALE',gcDataDir+'SCALE','SH')
ENDIF  
SELECT STYLE
SET RELATION TO 'S'+SCALE INTO SCALE
SELECT (lcInvLine)
SET RELATION TO STYLE INTO STYLE
SET RELATION TO Style+cWareCode+Dyelot INTO STYDYE ADDITIVE
IF llPicked
  *-- Pick ticket filter   
  lcLinesCond = "Picked .AND. TotPik > 0 AND PIKTKT = lcPiktkt"
ENDIF
*-- get the Term code related fields               
=gfRltFld(OrdHdr.cTermCode,@laTRltFld,'CTERMCODE')
lnEOMDay = IIF(TYPE('lnEOMDay') <> 'N' .OR. lnEOMDay = 0,20,lnEOMDay-1)
IF llIsEngland
  ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                  CTOD('01'+SUBSTR(DTOC(GOMONTH(ldDefInvDate,1)),3))-1+lnTDaysDue)
ELSE                
  ldDueDate = IIF(lcTEOM <> 'Y',ldDefInvDate+lnTDaysDue,;
                  GOMONTH(CTOD(SUBSTR(DTOC(ldDefInvDate),1,3)+'10'+;
                  SUBSTR(DTOC(ldDefInvDate),6,5)),IIF(DAY(ldDefInvDate) > lnEOMDay,2,1))+lnTDaysDue)
ENDIF
lcCurrCode= IIF(EMPTY(OrdHdr.cCurrCode),gcBaseCurr,OrdHdr.cCurrCode)
STORE 1 TO lnExRate, lnCurrUnit
IF lcCurrCode <> gcBaseCurr
  lnExRate=gfChkRate('lnCurrUnit',lcCurrCode,ldDefInvDate,.T.,.F.,.F.,llEditExRt)
  IF lnExRate = 0
    IF llEditExRt
      *--Message : 00262
      *--A valid xxx to xxx exchange rate could not be found on xxx.
      *--Button : 00000
      *--Ok
      =gfModalGen('INM00262B00000','ALERT',ALLTRIM(lcCurrCode)+'|'+ALLTRIM(gcBaseCurr)+'|'+DTOC(ldDefInvDate))
    ELSE
      lcCurrCode = gcBaseCurr
      STORE 1 TO lnExRate, lnCurrUnit
    ENDIF
  ENDIF
ENDIF
=SEEK('M'+OrdHdr.Account,'Customer')
IF EMPTY(OrdHdr.Store)  
  lcPhone=Customer.Phone1
ENDIF  
llBackOrd = INLIST(IIF(EMPTY(Customer.cBackOrd),laSetups[23,2],Customer.cBackOrd),'A','I')
SELECT OrdLine
SET ORDER TO TAG Ordlinst
=SEEK('O'+lcOrderNo+ALLTRIM(lcStoreNo))
DO WHILE cOrdType+Order+Store+Style+STR(LineNo,6) = 'O'+lcOrderNo+ALLTRIM(lcStoreNo)
  lcStore = Store
  =IIF(EMPTY(Store),SEEK('M'+Account,'Customer'),SEEK('S'+Account+Store,'Customer'))
  lcSaleRep1 = Ordhdr.Rep1
  lnComm1    = OrdHdr.Comm1
  lcSaleRep2 = Ordhdr.Rep2
  lnComm2    = OrdHdr.Comm2
  IF OrdHdr.Multi='Y' AND EMPTY(OrdHdr.Rep1) AND EMPTY(OrdHDr.Rep2)
    lcSaleRep1 = Customer.SalesRep
    lnComm1    = Customer.Comm
    lcSaleRep2 = Customer.Rep2
    lnComm2    = Customer.Comm2
  ENDIF
  *-- Scan only for the Piktkt that Passed From Function lfCons of lfUnCons
  SCAN REST FOR   EVAL(lcLinesCond) ;
            WHILE cOrdType+Order+Store+Style+STR(LineNo,6)='O'+lcOrderNo+lcStore
    llPacked = .F.
    IF 'AL' $ gcCmpModules AND ;
      SEEK(OrdLine.Order+OrdLine.Store+OrdLine.PikTkt,'Pack_Hdr') AND ;
      SEEK(PACK_HDR.Pack_No,'Pack_Lin')
      IF !SEEK(OrdLine.PikTkt+OrdLine.Order,lcPackLine)
        =lfPackLin(OrdLine.Order,OrdLine.PikTkt)
      ENDIF
      llPacked = SEEK(OrdLine.PikTkt+OrdLine.Order+STR(OrdLine.LineNo,6),lcPackLine)
      SELECT OrdLine
    ENDIF
    SCATTER MEMVAR MEMO
    m.Gros_Price = IIF(m.Gros_Price=0,m.Price,m.Gros_Price)
    =SEEK(m.Style,'Style')
    STORE 0 TO lnTaxQty,lnTaxRate
    IF llIsEngland .AND. laSetups[9,2]='Y' .AND. ;
       !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
      =gfRltFld(Style.cTaxCode,@laEngStyTax,'CTAXCODE') 
     ENDIF
    m.lTaxable = Style.lTaxable
    IF llPacked
      m.Pik1 = &lcPackLine..Qty1
      m.Pik2 = &lcPackLine..Qty2
      m.Pik3 = &lcPackLine..Qty3
      m.Pik4 = &lcPackLine..Qty4
      m.Pik5 = &lcPackLine..Qty5
      m.Pik6 = &lcPackLine..Qty6
      m.Pik7 = &lcPackLine..Qty7
      m.Pik8 = &lcPackLine..Qty8
      m.TotPik = &lcPackLine..TotQty

      IF !EMPTY(m.Prepak)  
        =SEEK('P'+m.Scale+m.Prepak,'Scale')
        IF Scale.pp1/Scale.PPTot*m.TotPik <> m.Pik1 OR ;
           Scale.pp2/Scale.PPTot*m.TotPik <> m.Pik2 OR ;
           Scale.pp3/Scale.PPTot*m.TotPik <> m.Pik3 OR ;
           Scale.pp4/Scale.PPTot*m.TotPik <> m.Pik4 OR ;
           Scale.pp5/Scale.PPTot*m.TotPik <> m.Pik5 OR ;
           Scale.pp6/Scale.PPTot*m.TotPik <> m.Pik6 OR ;
           Scale.pp7/Scale.PPTot*m.TotPik <> m.Pik7 OR ;
           Scale.pp8/Scale.PPTot*m.TotPik <> m.Pik8
          m.Prepak = ''
          m.PPQty  = 0
        ELSE
          m.PPQty = IIF(Scale.PPTot=0,0,m.TotPik /Scale.PPTot)
        ENDIF
        =SEEK('S'+m.Scale,'Scale')
      ENDIF
    ENDIF
    INSERT INTO (lcInvLine);
    (Order,Account,LineNo,Store,PikTkt,Style,Dyelot,Note_Mem,Comm1,Comm2,Book1,Book2,;
     Book3,Book4,Book5,Book6,Book7,Book8,TotBook,Flag,Pik1,Pik2,Pik3,Pik4,Pik5,;
     Pik6,Pik7,Pik8,TotPik,Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty,;
     Price,Pack_Id,Gros_Price,Disc_Pcnt,lPacked,lBackOrd,nTaxRate,Group,Prepak,;
     Desc1,Season,PPQty,Scale,cWareCode,Consol,cDivision,cCurrCode,lTaxable,cDyeFlag,cwarecode,;
     Gl_Sales);
     VALUES (m.Order,m.Account,m.LineNo,m.Store,m.PikTkt,m.Style,m.Dyelot,;
     m.Note_Mem,m.Comm1,m.Comm2,m.Qty1,m.Qty2,m.Qty3,m.Qty4,m.Qty5,m.Qty6,;
     m.Qty7,m.Qty8,m.TotQty,IIF(llPicked,'B',' '),m.Pik1,m.Pik2,m.Pik3,m.Pik4,;
     m.Pik5,m.Pik6,m.Pik7,m.Pik8,m.TotPik,IIF(llPicked,m.Pik1,m.Qty1),;
     IIF(llPicked,m.Pik2,m.Qty2),IIF(llPicked,m.Pik3,m.Qty3),IIF(llPicked,m.Pik4,m.Qty4),;
     IIF(llPicked,m.Pik5,m.Qty5),IIF(llPicked,m.Pik6,m.Qty6),IIF(llPicked,m.Pik7,m.Qty7),;
     IIF(llPicked,m.Pik8,m.Qty8),IIF(llPicked,m.TotPik,m.TotQty),m.Price,m.Pack_Id,;
     m.Gros_Price,m.Disc_Pcnt,llPacked,llBackOrd,lnTaxRate,m.Group,m.Prepak,;
     m.Desc1,m.Season,m.PPQty,m.Scale,m.cWareCode,'N',OrdHdr.cDivision,OrdHdr.cCurrCode,;
     m.lTaxable,Style.cDye_Flg,m.cwarecode,OrdLine.Gl_Sales)


    SELECT (lcInvLine)
    IF INLIST(laSetups[7,2],'F','L') 
      *-- Funtion to Update The Stk Qty.
      = lfUpStkQty ()
    ENDIF
    =SEEK(m.Style,'Style')
    IF llIsEngland .AND. laSetups[9,2]='Y' .AND. ;
      !Customer.lVatExem  .AND. Style.nTaxBreak <> 0
      FOR lnCount = Style.nTaxBreak TO 8
        lnTaxQty = lnTaxQty + EVAL(lcInvLine+'.Qty'+STR(lnCount,1))
      ENDFOR
    ENDIF
    SCATTER MEMVAR FIELDS Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8,TotQty
    SELECT (lcInvHdr)
    llInvPck = !EMPTY(m.PikTkt) AND SEEK(m.Order+m.Store+m.PikTkt,'Pack_Hdr')
   
    IF !SEEK(m.Account+m.Order+m.Store+m.PikTkt)
      APPEND BLANK
      REPLACE cSelect    WITH '»' ,;
              Order      WITH m.Order ,;
              Store      WITH m.Store ,;
              PikTkt     WITH m.PikTkt ,;
              CustPo     WITH IIF(EMPTY(m.CustPo),OrdHdr.CustPo,m.CustPo),;
              Dist_Ctr   WITH Customer.Dist_Ctr ,;
              Account    WITH OrdHdr.Account ,;
              cTermCode  WITH OrdHdr.cTermCode  ,;
              SpcInst    WITH OrdHdr.SpcInst ,;
              lUpsIns    WITH (OrdHdr.CINSUR='Y') ,;
              Rep1       WITH lcSaleRep1 ,;
              Comm1      WITH lnComm1    ,;
              Rep2       WITH lcSaleRep2 ,;
              Comm2      WITH lnComm2    ,;
              Note1      WITH OrdHdr.Note1 ,;
              Note2      WITH OrdHdr.Note2 ,;
              lCompUps   WITH .T. ,;
              Consol     WITH 'N'
                    
      REPLACE LastLine   WITH OrdHdr.LastLine   
      IF llInvPck AND !EMPTY(Pack_Hdr.Ship_Date)
        IF llIsEngland
          ldDueDate = IIF(lcTEOM <> 'Y',Pack_Hdr.Ship_Date+lnTDaysDue,;
                          CTOD('01'+SUBSTR(DTOC(GOMONTH(Pack_Hdr.Ship_Date,1)),3))-1+lnTDaysDue)
        ELSE                
          ldDueDate = IIF(lcTEOM <> 'Y',Pack_Hdr.Ship_Date+lnTDaysDue,;
                          GOMONTH(CTOD(SUBSTR(DTOC(Pack_Hdr.Ship_Date),1,3)+'10'+;
                          SUBSTR(DTOC(Pack_Hdr.Ship_Date),6,5)),IIF(DAY(Pack_Hdr.Ship_Date) > lnEOMDay,2,1))+lnTDaysDue)
        ENDIF      
      ENDIF
      REPLACE DiscPcnt   WITH OrdHdr.Disc                                       ,;
              InvDate    WITH ldDefInvDate                                      ,;
              ShipDate   WITH IIF(llInvPck AND !EMPTY(Pack_Hdr.Ship_Date)        ;
                              ,Pack_Hdr.Ship_Date,ldDefInvDate)   ,;
              dPostDate  WITH ldDefPstDate                                      ,;
              DueDate    WITH ldDueDate                                         ,;
              Dept       WITH OrdHdr.Dept                                       ,; 
              cFacCode   WITH OrdHdr.cFacCode                                   ,;
              Approval   WITH OrdHdr.Approval                                   ,;
              Appramt    WITH OrdHdr.ApprAmt                                    ,;
              Season     WITH OrdHdr.Season                                     ,;
              cDivision  WITH OrdHdr.cDivision                                  ,;
              UpsZone    WITH Customer.UpsZone                                  ,;
              Phone      WITH IIF(EMPTY(Ordhdr.Store),lcPhone,Customer.Phone1)  ,;
              cWareCode  WITH IIF(llPicked .AND. SEEK(m.Order+m.PikTkt,'PikTkt'),;
                                  PikTkt.cWareCode,OrdHdr.cWareCode)            ,;
              Trde_Disc  WITH lnTerDiscR                                        ,;
              Tax_Rate   WITH IIF(laSetups[9,2] <> 'Y',0,                        ;
                              IIF(llIsCanada,laSetups[10,2],Customer.nTaxRate)) ,;
              nPstRate   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                               Customer.nTaxRate,0) ,;
              cTaxRule   WITH IIF(laSetups[9,2]='Y' AND llIsCanada,;
                              Customer.cTaxRule,'') ,;
              Cod_Flag   WITH IIF(lcTCod='Y','Y','N'),;
              Status     WITH OrdHdr.Status ,;
              cCurrCode  WITH lcCurrCode ,;
              nExRate    WITH lnExRate   ,;
              nCurrUnit  WITH lnCurrUnit ,;
              dAdd_Date  WITH gdSysDate  ,;
              cAdd_Time  WITH TIME()     ,;
              cAdd_User  WITH gcUser_id
      REPLACE nHstRate   WITH IIF(laSetups[9,2] <> 'Y' OR !llIsCanada,0,laSetups[26,2])
      IF llInvPck
        REPLACE Weight    WITH Pack_Hdr.Tot_Wght ,;
                nCartons  WITH Pack_Hdr.Tot_Cart ,;
                Cartons   WITH Pack_Hdr.Tot_Cart
        REPLACE BOL_No WITH     Pack_Hdr.Bill_ladg    
      
      ENDIF
    ENDIF
    lnCartons  = nCartons + IIF(Style.Qty_Ctn>0,m.TotQty/Style.Qty_Ctn,0)
    lnAllCartn = IIF(CEILING(lnCartons)=0,1,CEILING(lnCartons))
    REPLACE Ordered   WITH Ordered  + &lcInvLine..TotBook ,;
            Ship      WITH Ship     + m.TotQty ,;
            ShipAmt   WITH ShipAmt  + m.TotQty*m.Price ,;
            Discount  WITH -ShipAmt * DiscPcnt/100,;
            Weight    WITH Weight   + IIF(llInvPck,0,m.TotQty*Style.nStyWeight) ,;
            nCartons  WITH IIF(llInvPck,nCartons,lnCartons) ,;
            Cartons   WITH IIF(llInvPck,Cartons,lnAllCartn) ,;
            Picked    WITH Picked  + m.TotPik ,;
            ShipVia   WITH IIF(Customer.nBrkWeight <> 0 AND Weight > Customer.nBrkWeight,Customer.cAltShpVia,;
                           IIF(llInvPck,IIF(ALLTRIM(Pack_Hdr.ShipVia)='*',Customer.ShipVia,Pack_Hdr.ShipVia),IIF(ALLTRIM(OrdHdr.ShipVia)='*',Customer.ShipVia,OrdHdr.ShipVia))),;
            nMerchTax WITH nMerchTax + lnTaxQty * m.Price * lnTaxRate/100 ,;
            Tax_Amt   WITH nMerchTax*(100-DiscPcnt)/100*(100-Trde_Disc)/100  ,;
            Cod_Amt   WITH IIF(Cod_Flag='Y' AND llIsEngland,ShipAmt+Tax_Amt+Discount,0) ,; 
            TotalChg  WITH ShipAmt+Tax_Amt+Discount,;
            nTaxDue   WITH nTaxDue + IIF(m.lTaxable,m.TotQty*m.Price,0)
    IF llIsEngland
      REPLACE NTrueShip 	WITH NTrueShip + (m.TotQty*m.Price) ,;
              NTrueDscnt	WITH -NTrueShip * DiscPcnt/100 		,;
              NTrueChrg     WITH NTrueShip + nMerchTax + NTrueDscnt
    ENDIF
    IF llRevue
      REPLACE Complete WITH OrdHdr.Complete ,;
              ShipDesc WITH gfCodDes(ShipVia,'SHIPVIA')
    ENDIF          
  ENDSCAN
ENDDO

RETURN

*-- End of Function lfGetOrder.

*:**************************************************************************
*:* Name        : lfORDCHGUP
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/27/2009
*:* Purpose     : call the lfUpddirChg from the lfGetOrder
*:***************************************************************************
*:* Called from : ARIINV.SCX lfgetorder()
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfORDCHGUP()
*:***************************************************************************
FUNCTION lfORDCHGUP

*- if the lfGetOrder method is called from the ARCOINV.SCX screen then do not call the screen again
IF TYPE('loARCOINV') = 'O'
  RETURN
ENDIF 

DO FORM (oAriaApplication.ScreenHome + 'AR\ARCOINV.SCX') WITH loFormSet,.F.,.T.

*- Update the charges from the temp charge file
SELECT (loFormSet.lcInvHdr)
LOCATE
SCAN FOR CONSOL = 'N'
  loFormSet.AriaForm1.Ariapageframe1.Page4.cntInvoicesummary.merchandisetax = nMerchTax
  loFormSet.AriaForm1.Ariapageframe1.Page4.Activate
ENDSCAN          
SELECT (loFormSet.lcInvHdr)
LOCATE

*-- end of lfORDCHGUP.