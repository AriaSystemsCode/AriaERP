*!**************************************************************************
*! Name      : DIRMAIN.PRG
*! Developer : Tarek Mohammed Ibrhim
*! Date      : 11/20/2008
*! Purpose   : Custom triggers for DCC customer ( DIR03 )
*!**************************************************************************
*! Parameters: lcEvntFun -> Process event function name without 'lf..'  .
*!             lcFunPars -> Process function parameters, sent as a string.
*!**************************************************************************
*! Modifications:
*C201126,1 MMT 04/01/2009  Add trigger to Sales order screen to validate contarct ref. field [T20070214.0006]
*C201141,1 HES 04/26/2009, quote Processing Enbroidery orders for DCC [T20080808.0019]
*B609035,1 TMI 10/12/2009  Define the variable ldStartDate based on the setup option M_CHKPRIAG [T20080812.0001]
*B609054,1 HES 10/21/2009, ordchg table missing standard aria fields [T20090819.0010]
*C201220,1 HES 13/02/2010, Recalculate Style PO Quantities- Quotation required [T20091217.0006]
*B609209,1 MMT 04/15/2010, Fix bug of missing lines while importing SO From excel(DCC)[T20100323.0011]
*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [T20091130.0004]
*B609264,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [T20100513.0013]
*B609316,1 MMT 06/29/2010 CODPRICE trigger should be executed in ADD or edit mode of Sales order screen[T20100614.0001]
*B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[T20091130.0004]
*B609478,1 TMI 12/12/2010 fix problem of not creating a po line when the scale mapping contains more than one scale [T20091130.0004]
*B609478,5 TMI 01/14/2011 fix the 'Invalid subscript reference problem'
*B609478,7 TMI 03/21/2011 Fix the problem that if there are several scales are mapped to the same scale then it takes only the first one
*B609478,8 TMI 04/03/2011 Amend the function "lfCpyArrToTmp" to avoide suspected problem [T20091130.0004] 
*B609478,8                add the user ID to the start of the file ACTDATA.MEM to avoid the case that two users logged in at the same time creating POs
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003]
*B609743,1 MMT 11/27/2011 Custom Customer Price filter on STYLE Browse is not working fine in edit mode[T20111108.0002]
*C201438,1 MMT 12/12/2011 Add Trigger to default POSHDR.CFRCSTGRP to the code defualt value[T20110117.0005]
*C201472,1 MMT 04/02/2012 Custom program to allocate received qty[T20120314.0001]
*C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[T20120314.0001]
*B610153,1 SAB 11/11/2012 Fix problem of not showing Order Charges on Sales Order Option [T20121102.0002]
*B610224,1 HIA 01/31/2013 DCC R12 slowing the system  [T20130111.0006]
*C201636,1 MMT 09/07/2014 Consolidate invoice by contract reference[T20140819.0035]
*C201643,1 MMT 11/30/2014 Add Triggers for [T20141118.0011]
*C201643,2 MMT 12/11/2014 Fix issue#1 on project[T20141118.0011]
*C201645,1 MMT 12/18/2014 Add triggers to change the style field back color if projection field is 0[T20141121.0002]
*B610928,1 MMT 01/18/2015 PO manage PO's style component screen show wrong scale[T20150115.0021]
*B610937,1 MMT 01/26/2015 PO manage Style compenents screen shows  incorrect component stock[T20150123.0001]
*C201649,1 MMT 01/27/2015 Add Trigger to check CRM approved order with same con. ref.[T20141113.0005]
*B610943,1 MMT 02/05/2015 Automatic allocation allocates non existing stock[T20150128.0019]
*B610944,1 MMT 02/05/2015 Automatic allocation gives error while updating PICKPACK [T20150102.0002]
*B610995,1 MMT 04/26/2015 Automatic allocation gives error while updating PICKPACK [T20150424.0001]
*B610995,2 MMT 04/27/2015 Automatic allocation gives error while updating PICKPACK [T20150424.0001]
*B611002,1 MMT 05/13/2015 Error while updating Pickpack Table at DCC[T20150512.0006]
*B611005,1 MMT 05/14/2015 error while receiving PO at DCC[T20150512.0009]
*C201681,1 MMT 05/28/2015 Add Issue None button to issue component screen[T20150514.0012]
*C201695,1 MMT 07/07/2015 Create Picking Ticket for SO linked to the PO being received[T20150317.0018]
*B611041,1 MMT 08/24/2015 Incorrect style cost Sheet is selected in style comp. custom[T20150820.0001]
*B611053,1 MMT 09/14/2015 Include Status B in Checking unathorized CRM orders[T20150909.0020]
*C201723,1 MMT 10/25/2015 Add trigger to Automatic allocation to check pickpack table[T20150908.0008]
*B611112,1 MMT 02/10/2016 Fix error while using PO receiving screen[T20160204.0003]
*C201790,1 MMT 03/09/2016 Send email after adding PO[P20160119.0001 - Entity#7]
*C201790,2 MMT 03/15/2016 Error while saving PO[P20160119.0001 - Entity#7]
*B611129,1 MMT 03/23/2016 Custom Auto create PO phase#7 send email even if vendor has no email setup[T20160314.0015]
*C201809,1 MMT 04/21/2016 Calculate Style price in Return auth. screen from CSTPRICE[T20160324.0006]  
*C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006]  
*C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001]
*C201930,1 Sara.O 01/30/2017 Update Price with Uplift %[P20170117.0001]
*C201930,2 MMT 02/13/2017 Update Price with Uplift %[P20170117.0001]
*C201930,3 MMT 02/20/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1]
*C201930,4 MMT 02/21/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][Start]
*C202185,1 MMT 06/27/2018 Add trigger to credit memo saving program to update reference[T20180619.0002]
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
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfDispScr IN DIRMAIN WITH .T.

DEFINE BAR lnBarNo+1 OF _INQURYPOP PROMPT 'Generate In\<voices from Ship Date' ;
       SKIP FOR !EMPTY(_Screen.ActiveForm.keyOrder.Keytextbox.Value)
ON SELECTION BAR lnBarNo+1 OF _INQURYPOP DO lfDispScr IN DIRMAIN WITH .F.
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
DEFINE BAR lnBarNo   OF _INQURYPOP PROMPT '\<View Order Charges' ;
       SKIP FOR _Screen.ActiveForm.Parent.ActiveMode <> 'E' .OR. (_Screen.ActiveForm.Parent.ActiveMode = 'E' .AND. _Screen.ActiveForm.cboStatus.Value='C')
ON SELECTION BAR lnBarNo OF _INQURYPOP DO lfOpChrgOrdSc IN DIRMAIN.PRG

*B610153,1 SAB 11/11/2012 Fix problem of not showing Order Charges on Sales Order Option [Start]
loFormSet.NextOptionBar = loFormSet.NextOptionBar + 1
*B610153,1 SAB 11/11/2012 Fix problem of not showing Order Charges on Sales Order Option [End]

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
*:* Purpose     : Add the field PACK_DATE   to the browse for DIR03
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

lcAccount = loFormSet.Ariaform1.keyAccount.Keytextbox.Value
=gfSEEK('M'+lcAccount,'CUSTOMER')   

IF !USED('ORDCHG')
  =gfOpenTable(oAriaApplication.DataDir+'ORDCHG','ORDCHG','SH')
ENDIF

IF IIF(TYPE('loOrdChg')<>'O',loFormSet.ActiveMode <> 'A',.F.)
  
  SELECT 'ORDCHG'
  =gfTableUpdate()
  =gfCloseTable('ORDCHG')
  
ELSE
  
  *- check Charge Carriage user field
  lnBookAmt = loFormSet.Ariaform1.Ariapageframe1.Page3.cntSummary.txtBookedAmnt.Value
  IF CUSTOMER.lChrgchg .AND. 0 < CUSTOMER.nMinOrdChg .AND. lnBookAmt <= CUSTOMER.nMinOrdChg
    
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
          
          *B609054,1 HES 10/21/2009, ordchg table missing standard aria fields [Start]
          gfAdd_Info(lcOrdChg,loFormSet)
          *B609054,1 HES 10/21/2009, ordchg table missing standard aria fields [End]
          
        ENDIF
      ENDDO
    ENDIF
      
    *- if there are Order Charges in the Codes file that are specific to that customer – then only 
    *- those should be shown – if not order charges are specific to the customer then all those that 
    *- are not assigned to a customer should be displayed
    SELECT &lcOrdChg
    LOCATE
    LOCATE FOR !EMPTY(&lcOrdChg..CHGACC)
    IF FOUND()
      DELETE FOR EMPTY(&lcOrdChg..CHGACC)
    ENDIF  
    
    LOCATE
    IF RECCOUNT(lcOrdChg)>0
      *- Pick one of the order charges  
      
      IF !EMPTY(lfLocBrow())
        SELECT ORDCHG      
        IF loFormSet.ActiveMode = 'A'        
          IF !gfSEEK(lcOrderNo+&lcOrdChg..CORDCHG,'ORDCHG')
            lcReplace = 'ORDER    WITH "'+lcOrderNo+'" '+;
                        'CORDCHG  WITH "'+&lcOrdChg..CORDCHG+'" '+;
                        'NORDCHG  WITH '+STR(&lcOrdChg..NORDCHG,8,3)+' '+;
                        'INVOICED WITH .F. '+;
                        'INVOICE  WITH "" '
            =gfAppend()
            
            *B609054,1 HES 10/21/2009, ordchg table missing standard aria fields [Start]
            gfAdd_Info('ORDCHG',loFormSet)
            *B609054,1 HES 10/21/2009, ordchg table missing standard aria fields [End]
            
            =gfReplace(lcReplace)
            =gfTableUpdate()               
          ENDIF
        
        ELSE
          
          && Here called from the SOORDCHG screen , when editing a sales order an needs to update the charges
          && the updates apply only on the local screen
          IF TYPE('loList')='O'
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
        
        *B609054,1 HES 10/21/2009, ordchg table missing standard aria fields [Start]
        gfAdd_Info('ORDCHG')
        *B609054,1 HES 10/21/2009, ordchg table missing standard aria fields [End]
        
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
                
    *B609054,1 HES 10/21/2009, ordchg table missing standard aria fields [Start]
    gfAdd_Info(ORDCHG)
    *B609054,1 HES 10/21/2009, ordchg table missing standard aria fields [End]
    
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
*!**************************************************************************
*! Name       : lfEDITPRICE
*! Developer  : (HES)Hesham Elmasry
*! Date       : 03/02/2009
*! Purpose    : Custom trigger for DCC customer for restricting the edit
*!            :   of prices
*!**************************************************************************
*! Parameters :
*!**************************************************************************
FUNCTION lfEDITPRICE

LOCAL llIfAuthrzd, lcUserID
lcUserID = oAriaApplication.User_ID
IF !USED('SYUUSER')
  =gfOpenTable(oAriaApplication.DataDir+'SYUUSER','CUSER_ID','SH')
ENDIF 
SELECT SYUUSER
gfSetorder('Cuser_ID')
gfSeek(lcUserID)
lcUsrLevl = Cusr_Levl   && Retrieving the user's privilege
llIfAuthrzd  = gfUserPriv('SO','SOORDPRI','EDITPRICE') && Check if the user has an authorization or not
IF !llIfAuthrzd AND Cusr_Levl <> 'A'
 *-* Get an Authintication for an Authorized user
  DO FORM(oAriaApplication.sCreenHome + 'SO\SOVLDUSR') TO llIfVld
  IF !llIfVld 
    RETURN .F.
  ENDIF 
ENDIF 
*-- end of lfEDITPRICE.
*!**************************************************************************
*! Name      : lfFLTPRCSTL
*! Developer : (HES)Hesham Elmasry
*! Date      : 03/02/2009
*! Purpose   : Custom trigger for DCC customer for linking the styles 
*!           :  to the price table
*!**************************************************************************
*! Parameters: 
*!**************************************************************************\
FUNCTION lfFLTPRCSTL

*-* Check if the feature (Link Styles to Price Table) allowed or not
llIfLinked = gfGetMemVar('M_FLTRSTYL',oAriaApplication.ActiveCompanyID)
IF !llIfLinked
  RETURN
ENDIF

LOCAL lcI,lnX
IF !USED('CSTPRICE')
  gfOpenTable(oAriaApplication.dataDir+'CSTPRICE','CSTPRICE','SH')
ENDIF

SELECT Style  && Avoid any previouse filteration for the last account
SET FILTER TO 

SELECT CSTPRICE
gfSetorder('CSTPRICE')
gfSeek('')

SELECT Customer
gfsetorder('CUSTOMER')
*B609743,1 MMT 11/27/2011 Custom Customer Price filter on STYLE Browse is not working fine in edit mode[Start]
lcCustomerCode = Customer.Account
lcCustomerKey = EVALUATE(KEY())
=gfSeek('M'+lcCustomerCode)
*B609743,1 MMT 11/27/2011 Custom Customer Price filter on STYLE Browse is not working fine in edit mode[End]
lcI = ''
lnX = 0
lcstyfltr = "INLIST(STYLE"
PUBLIC lcstyfltr1 
lcstyfltr1 = '|'
lcstycount = 0
FOR lnI = 1 TO 15
  *-* Retrieve the price codes for this customer
  lcPrcCode = Customer.priccode&lcI
  IF !EMPTY(lcPrcCode)
    lnI = lnI + 1
  
    IF lnI < 10
      lcI = '0' + ALLTRIM(STR(lnI))
    ELSE
     lcI = STR(lnI)
    ENDIF
   
    *-* Select the Styles related to this code and insert it in the cursor (lcTempStyl)
    SELECT CSTPRICE
    gfSetorder('CSTPRICE')
    SEEK(lcPrcCode)
    SCAN REST WHILE priccode = lcPrcCode 
      IF lcstycount > 23
        lcstyfltr = lcstyfltr + ") OR INLIST(STYLE"
        lcstycount= 0
      ENDIF 

      lcstyfltr = lcstyfltr + ",'"+CSTPRICE.stydv+"'"
      lcstyfltr1 = lcstyfltr1 + CSTPRICE.stydv + '|'
      lcstycount=lcstycount+1
      
    ENDSCAN
  ENDIF
ENDFOR
lcstyfltr =lcstyfltr +")"

IF lcstycount > 0
  SELECT STYLE
  gfSetorder('STYLE')
  TRY 
    SET FILTER TO &lcstyfltr
  CATCH 
    SET FILTER TO STYLE $ lcstyfltr1
  ENDTRY 
  LOCATE 
ENDIF 
*B609743,1 MMT 11/27/2011 Custom Customer Price filter on STYLE Browse is not working fine in edit mode[Start]
IF !EMPTY(lcCustomerKey)
  =gfSeek(lcCustomerKey ,'Customer','Customer')
ENDIF
*B609743,1 MMT 11/27/2011 Custom Customer Price filter on STYLE Browse is not working fine in edit mode[End]
*-- end of lfFLTPRCSTL.

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
      REPLACE NTrueShip   WITH NTrueShip + (m.TotQty*m.Price) ,;
              NTrueDscnt  WITH -NTrueShip * DiscPcnt/100     ,;
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

*!**************************************************************************
*! Name       : lfNorAlFEG
*! Developer  : (HES)Hesham Elmasry
*! Date       : 03/02/2009
*! Purpose    : Fill the empty group with a special charachter
*! Type       : Custom
*!**************************************************************************
FUNCTION lfNorAlFEG

*B610943,1 MMT 02/05/2015 Automatic allocation allocates non existing stock[T20150128.0019][Start]
IF TYPE('llRpPikAll') ='L' AND llRpPikAll AND loFormSet.lnRpPikSep =100
  RETURN 
ENDIF
*B610943,1 MMT 02/05/2015 Automatic allocation allocates non existing stock[T20150128.0019][End]

llIfTotOrdr = gfGetMemVar('M_MULALLOC')
IF !llIfTotOrdr 
  RETURN
ENDIF 

lcSpCh = CHR(193) && Special Charachter for the empty group
loFormSet.lcCustSpCh = lcSpCh

REPLACE GROUP WITH lcSpCh FOR EMPTY(GROUP)
LOCATE

*-- end of lfNorAlFEG.

*!**************************************************************************
*! Name       : lfNorAlEFG
*! Developer  : (HES)Hesham Elmasry
*! Date       : 03/02/2009
*! Purpose    : Empty the group of the custom special charachter as it was
*! Type       : Custom
*!**************************************************************************
FUNCTION lfNorAlEFG

llIfTotOrdr = gfGetMemVar('M_MULALLOC')
IF !llIfTotOrdr 
  RETURN
ENDIF 

GO TOP 
REPLACE GROUP WITH '' FOR Group = loFormSet.lcCustSpCh 

*-- end of lfNorAlEFG.

*!**************************************************************************
*! Name       : lfNorPrDCV
*! Developer  : (HES)Hesham Elmasry
*! Date       : 03/02/2009
*! Purpose    : Initializing the appropriate formset's properties and the calculation flag
*! Type       : Custom
*!**************************************************************************
FUNCTION lfNorPrDCV

llIfTotOrdr = gfGetMemVar('M_MULALLOC')
IF !llIfTotOrdr 
  RETURN
ENDIF 

lcGroup = Group
PRIVATE lcOrder
lcOrder = Order

loFormSet.llIfStanCalc = .F. && Flag to calculate the total Available, Required for this group 
loFormSet.lnAvlFAlo = 0 && variable to hold the total Available for this group
loFormSet.lnTotReq = 0  && variable to hold the total required for this group
llGrpFrRec = .T. && To avoid skipping the next condition (BUG)
LOCATE FOR Group = lcGroup AND order = lcOrder

*-- end of lfNorPrDCV.

*!**************************************************************************
*! Name       : lfNorPrVMin
*! Developer  : (HES)Hesham Elmasry
*! Date       : 03/02/2009
*! Purpose    : Valid the Total (AVL & REQ) and check if it fullfilled with Min%
*! Type       : Custom
*!**************************************************************************
FUNCTION lfNorPrVMin

llIfTotOrdr = gfGetMemVar('M_MULALLOC')
IF !llIfTotOrdr 
  RETURN
ENDIF 
*C201643,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][Start]
IF TYPE('llRpPikAll') ='L' AND llRpPikAll AND loFormSet.lnRpPikSep =100
  RETURN 
ENDIF
*C201643,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][End]
*#INCLUDE R:\Aria4XP\Screens\AL\ALAUTAL.h
#DEFINE LANG_AUTOALLOC_COLROTITL1 			  'Only These '
#DEFINE LANG_AUTOALLOC_COLROTITL2 			  's.'
#DEFINE LANG_AutoAlloc_Scope				  'Scope'
#DEFINE LANG_AutoAlloc_Allocate				  'Allocate selected records'
#DEFINE LANG_AutoAlloc_release				  'Release selected records'
#DEFINE LANG_AutoAlloc_GenPick				  'Generate picking tickets'
#DEFINE LANG_AutoAlloc_SepertOpt			  '\-'
#DEFINE LANG_AutoAlloc_ScopeOpt				  '\<Scope'
#DEFINE LANG_AutoAlloc_AllocateOpt			  '\<Allocate selected records'
#DEFINE LANG_AutoAlloc_releaseOpt			  '\<Release selected records'
#DEFINE LANG_AutoAlloc_GenerateOp			  '\<Generate picking tickets'
#DEFINE LANG_AutoAlloc_Option	        	  'O\<ptions'
#DEFINE LANG_AutoAlloc_LabelOrder 			  'Order'
#DEFINE LANG_AutoAlloc_LabelAccount			  'Account'
#DEFINE LANG_AutoAlloc_LabelStore			  'Store'
#DEFINE LANG_AutoAlloc_LabelConfg			  'Configuration'
#DEFINE LANG_AutoAlloc_LabelDyelot			  'Dyelot'
#DEFINE LANG_AutoAlloc_LabelGroup			  'G'
#DEFINE LANG_AutoAlloc_LabelAvialable		  'Avail.'
#DEFINE LANG_AutoAlloc_LabelOpen			  'Open'
#DEFINE	LANG_AutoAlloc_LabelOpnAmnt		 	  'Opn Amnt.'
#DEFINE LANG_AutoAlloc_LabelPiktkt			  'Pick Tkt'
#DEFINE LANG_AutoAlloc_LabelPicked			  'Picked'
#DEFINE LANG_AutoAlloc_LabelPikAmnt			  'Pik Amnt.'
#DEFINE LANG_AutoAlloc_Season				  ' season'
#DEFINE LANG_AutoAlloc_Division	    		  ' division'
#DEFINE LANG_AutoAlloc_Group				  ' group'
#DEFINE LANG_AutoAlloc_FabricCode			  ' fabric code'
#DEFINE LANG_AutoAlloc_Pattern 				  ' pattern'
#DEFINE LANG_AutoAlloc_Configuration		  'Configuration:'
#DEFINE LANG_AutoAlloc_Dyelot		    	  'Dyelot       :'
#DEFINE LANG_AutoAlloc_CT   		   	      'C/T            :'
#DEFINE LANG_AutoAlloc_PO		    	      'P/O            :'
#DEFINE LANG_AutoAlloc_MsgAskAssign           '. Do you wish to assign it'
#DEFINE LANG_AutoAlloc_MsgSeason              '|Season'
#DEFINE LANG_AutoAlloc_MsgDivision            '|Division'
#DEFINE LANG_AutoAlloc_MsgAskForce1           'Do you want to force allocation?'
#DEFINE LANG_AutoAlloc_MsgAskForce2           'The allocated quantity|the available quantity'
#DEFINE LANG_AutoAlloc_LableProgress          'Preparing data files'
#DEFINE LANG_AutoAlloc_LableAssign            'Assigning piktkt number for...Order/Style : '
#DEFINE LANG_AutoAlloc_LableReleas            'Releasing...Order/Style : '
#DEFINE LANG_AutoAlloc_WaitMsgExcl            'Excluding'
#DEFINE LANG_AutoAlloc_WaitMsgSelc            'Selecting'
#DEFINE LANG_AutoAlloc_WaitMsgPO              ' purchase order '
#DEFINE LANG_AutoAlloc_WaitMsgCT              ' cutting ticket '
#DEFINE LANG_AutoAlloc_ProgrsAllo             'Allocating...'
#DEFINE LANG_AutoAlloc_ProgrsOrdNum           'Order number: '
#DEFINE LANG_AUTOALLOC_OPTNCONFG              'Exclude styles with Confg'
#DEFINE LANG_AUTOALLOC_OPTNDYELOT             'Exclude styles with dyelot'
#DEFINE LANG_AUTOALLOC_SQLMSG1                ' While opening '
#DEFINE LANG_AUTOALLOC_SQLMSG2                ' file.'
#DEFINE LANG_AUTOALLOC_MsgPickSep		      'Pick separates min%|0|100'
#DEFINE LANG_AUTOALLOC_MsgPickCoord		      'Pick coordinate groups min%|0|100'
#DEFINE LANG_AUTOALLOC_MsgCutOffUnt		      'Cut-off units'
#DEFINE LANG_AUTOALLOC_ButtSelect		      '\<Select'
#DEFINE LANG_AUTOALLOC_ButtUnSelect		      'Un\<select'
#DEFINE LANG_AutoAlloc_LabelReasone           'Reject Reason'
#DEFINE LANG_AutoAlloc_LabelReasMASetup       'This is a dyelot style but Use dyelot setup in MA is NO'
#DEFINE LANG_AutoAlloc_LabelReasAvlQty        'There is no available Qty'
#DEFINE LANG_AutoAlloc_LabelResaAvlQtCov1     'There is no available to cover the pick separates Min%'
#DEFINE LANG_AutoAlloc_LabelResaAvlQtCov2     'There is no available to cover the pick coordinate Min%'
#DEFINE LANG_AutoAlloc_LabelReasDyeNtAssgn1   'Dyelot '
#DEFINE LANG_AutoAlloc_LabelReasCnfgNtAssgn1  'Configuration '
#DEFINE LANG_AutoAlloc_LabelReasDyeNtAssgn2   ' is not assigned to location '
#DEFINE LANG_AutoAlloc_LabelResaStyleNtAssgn  'This style is not assigned to location '
#DEFINE LANG_AutoAlloc_LabelReasDyelotAssign  'No Dyelots assigned to this style'
#DEFINE LANG_AutoAlloc_LabelReasConfigAssign  'No configurations assigned to this style'
#DEFINE LANG_AutoAlloc_LabelReasSameFabDye    'Styles should be allocated within the same fabric dyelot. But the fabric dyelot is different.'
#DEFINE LANG_AutoAlloc_LabelReasNOFabDye      'There are no dyelots for Fabric/Color : '
#DEFINE LANG_AutoAlloc_ExcludingOrder         'Excluding order '
#DEFINE LANG_AutoAlloc_SelectingOrder         'Selecting order '
#DEFINE LANG_AutoAlloc_Excluding              'Excluding '
#DEFINE LANG_AutoAlloc_Selecting              'Selecting '
#DEFINE LANG_AutoAlloc_CannotChange           '. Cannot change.'
*#INCLUDE R:\Aria4XP\Screens\AL\ALAUTAL.h

IF !EOF()
  SKIP
ENDIF 

IF EOF()
  SKIP -1 
ELSE 
  SKIP -2 && because the current group will not be the group we work with
ENDIF 

lcGroup = Group 
PRIVATE lcOrder
lcOrder = Order

IF lcGroup = loFormSet.lcCustSpCh
  loFormSet.lnMinPer = loFormSet.lnRpPikSep
ELSE 
  loFormSet.lnMinPer = loFormSet.lnRpPikCor
ENDIF  
llAloGoup = loFormSet.lnAvlFAlo <> 0 .AND. (loFormSet.lnAvlFAlo/loFormSet.lnTotReq >= loFormSet.lnMinPer/100)
IF llAloGoup  && If this group valid to start allocation for it
  loFormSet.llIfStanCalc = .T.
  LOCATE FOR Group = lcGroup AND order = lcOrder
  llHasNP = .F.
  SCAN REST WHILE llGrpFrRec .AND. Order + Store + Group = lcGroupStr FOR IIF(lnAlcCase = 1, .T. , lnSel <> 0);
                                                                                       AND !Picked AND TotAvl <> 0
    lnCurent = lnCurent + 1
      
    *IF We are going to Allocate this group
    IF llAloGoup
      lcPikWare = IIF(EMPTY(loFormSet.lcRpPkFWrh) , cWareCode , loFormSet.lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
      loFormSet.llCalWip = (lcCurStyWr = Style + cWareCode)
      *-* Force the allocation for these styles in this group because the group allocation approved
      llAloGoup = lfTmpAlo(.F. , lcPikWare , .F. , loFormSet.lnRpCutUnt , loFormSet.lnRpPikCor,loFormSet)
      lcCurStyWr = Style + cWareCode
      llHasNP = .T.
    ENDIF    && End of IF
  ENDSCAN    && End of SCAN Loop
  
  *IF We are going to Allocate this group
  IF llAloGoup AND llHasNP = .T.
    lnCurent = IIF(llGrpFrRec , lnCurent , lnCurent + 1)
    GO lnRecNumb
    *SCAN Loop to scan the temp. Order lines file FOR the same Order
    *and Store and Group
    SCAN REST WHILE Order + Store + Group = lcGroupStr FOR IIF(lnAlcCase = 1, .T. , lnSel <> 0) AND !Picked;
                                                                                                AND TotAvl <> 0
      lcPikWare = IIF(EMPTY(loFormSet.lcRpPkFWrh) , cWareCode , loFormSet.lcRpPkFWrh)    && Variable to hold the Warehouse to allocate from
      =lfAloQty(lcPikWare,.F.,loFormSet)
    ENDSCAN    && End of SCAN Loop
  ENDIF    && End of IF
  SKIP -1
  *! B610224,1 HIA 01/31/2013 DCC R12 slowing the system  [T20130111.0006][Start]
  loFormSet.oPross.lblSecondLabel.Caption = LANG_AutoAlloc_ProgrsOrdNum + Order
  loFormSet.oPross.CurrentProgress(lnCurent)
  *! B610224,1 HIA 01/31/2013 DCC R12 slowing the system  [T20130111.0006][End]
  RETURN .F.
ELSE 
  IF Group = loFormSet.lcCustSpCh
    lcGroup = Group
    PRIVATE lcOrder
    lcOrder = Order
    
    lnRecNo = RECNO()
    REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelResaAvlQtCov1) FOR GROUP = lcGroup;
                                            AND order = lcOrder AND IIF(lnAlcCase = 1, .T. , lnSel <> 0) AND !picked
    loFormSet.llReject = .T.
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo 
    ENDIF
  ELSE
    lcGroup = Group
    lnRecNo = RECNO()
    REPLACE cReason WITH IIF(!EMPTY(cReason),cReason ,LANG_AutoAlloc_LabelResaAvlQtCov2) FOR GROUP = lcGroup;
                                           AND order = lcOrder AND IIF(lnAlcCase = 1, .T. , lnSel <> 0) AND !picked
    loFormSet.llReject = .T.
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo 
    ENDIF
  ENDIF
  RETURN .F.  
ENDIF 

*-- end of lfNorPrVMin.

*!**************************************************************************
*! Name       : lfTmpAlCalc
*! Developer  : (HES)Hesham Elmasry
*! Date       : 03/02/2009
*! Purpose    : Calculate the accumulative AVl & REQ for a specific group
*! Type       : Custom
*!**************************************************************************
FUNCTION lfTmpAlCalc

llIfTotOrdr = gfGetMemVar('M_MULALLOC')
IF !llIfTotOrdr 
  RETURN
ENDIF 
*C201643,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][Start]
IF TYPE('llRpPikAll') ='L' AND llRpPikAll AND loFormSet.lnRpPikSep =100
  RETURN .T.
ENDIF
*C201643,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][End]
IF !loFormSet.llIfStanCalc
  IF !Picked AND !'*****' $ PikTkt
    lnTotReq = IIF(loFormSet.lcRpScpMod $ 'KP',TotCut,TotQty) - TotExcCut && calculates the total rerquired
    loFormSet.lnAvlFAlo = loFormSet.lnAvlFAlo + lnAvlFAlo && variable to hold the total Available for this group
    loFormSet.lnTotReq = loFormSet.lnTotReq + lnTotReq  && variable to hold the total required for this group
  ELSE 
    lnTotReq = IIF(loFormSet.lcRpScpMod $ 'KP',TotCut,TotQty) - TotExcCut && calculates the total rerquired
    loFormSet.lnAvlFAlo = loFormSet.lnAvlFAlo + TotPik && variable to hold the total Available for this group
    loFormSet.lnTotReq = loFormSet.lnTotReq + lnTotReq  && variable to hold the total required for this group
  ENDIF 
  RETURN .F.
ENDIF 

*-- end of lfTmpAlCalc.

*!**************************************************************************
*! Name       : lfTmpAlReTr
*! Developer  : (HES)Hesham Elmasry
*! Date       : 03/02/2009
*! Purpose    : Approve the Calculation
*! Type       : Custom
*!**************************************************************************
FUNCTION lfTmpAlReTr

llIfTotOrdr = gfGetMemVar('M_MULALLOC')
IF !llIfTotOrdr 
  RETURN
ENDIF  

IF loFormSet.llIfStanCalc
  llReturn = .T. && Because when the (loFormSet.llIfCustAlo = .T.) the allocation already approved for this style
ENDIF 

*-- end of lfTmpAlReTr.

*!**************************************************************************
*! Name       : lfGloProps
*! Developer  : (HES)Hesham Elmasry
*! Date       : 03/02/2009
*! Purpose    : Add appropriate properties for formset at runtime for Grouping Allocation
*! Type       : Custom
*!**************************************************************************
FUNCTION lfGloProps

llIfTotOrdr = gfGetMemVar('M_MULALLOC')
IF !llIfTotOrdr 
  RETURN
ENDIF 

IF TYPE('loFormSet.lnAvlFAlo') = 'U' && Available for allocation for the entir group
  loFormSet.AddProperty('lnAvlFAlo',0)
ENDIF 

IF TYPE('loFormSet.lnTotReq') = 'U'  && Total required for the entir group
  loFormSet.AddProperty('lnTotReq',0)
ENDIF 

IF TYPE('loFormSet.llIfStanCalc') = 'U'  && .F. if we do our custom calculation
  loFormSet.AddProperty('llIfStanCalc',.F.)
ENDIF 

IF TYPE('loFormSet.lcCustSpCh') = 'U'  && Special charachter for the separates group
  loFormSet.AddProperty('lcCustSpCh','')
ENDIF 

IF TYPE('loFormSet.lnMinPer') = 'U'  && Holds the Min% for the separates or the coordinates
  loFormSet.AddProperty('lnMinPer','')
ENDIF 

IF TYPE('loFormSet.lnOldCor') = 'U'  && Holds the old value Coordinate Min% 
  loFormSet.AddProperty('lnOldCor','')
ENDIF 

*-- end of lfGloProps.
*C201126,1 MMT 04/01/2009 Add trigger to Sales order screen to validate contarct ref. field [Start]
*!**************************************************************************
*! Name       : lfSODIRSV
*! Developer  : Mariam MAzhar [MMT]
*! Date       : 04/01/2009
*! Purpose    : validate contarct ref. field  in SO Screen
*!**************************************************************************
FUNCTION lfSODIRSV
lcOrdHdr =loFormSet.oFormEnvironment.lcOrdHdr 
= SEEK('M'+&lcOrdHdr..Account,'CUSTOMER')
IF CUSTOMER.LCMPCSTPO
   IF EMPTY(&lcOrdHdr..CCONTREF)
      = GFMODALGEN('INM00000B00000',.F.,.F.,.F.,'Can not save unless the Contract Ref. field on the Order Header has been entered.')
      loFormSet.AriaForm1.ariapageframe1.ActivePage = 3
      loFormSet.AriaForm1.ariapageframe1.page3.refresh
      loFormSet.AriaForm1.ariapageframe1.page3.txtContRef.SetFocus()
      LLCSAVE = .F.
      RETURN .F.
   ELSE
     REPLACE CustPO WITH &lcOrdHdr..CCONTREF IN (lcOrdHdr)
   ENDIF
ENDIF
*C201126,1 MMT 04/01/2009 Add trigger to Sales order screen to validate contarct ref. field [End]


*!**************************************************************************
*! Name       : lfSOPROSH
*! Developer  : Mostafa Eid [MOS]
*! Date       : 04/14/2009
*! Purpose    : IMPORT SPREADSHEET FROM SO SCREEN 
*!**************************************************************************
*C201136
FUNCTION lfSOPROSH
DEFINE BAR loFormSet.NextOptionBar OF _INQURYPOP PROMPT 'I\<mport From SpreadSheet'  SKIP FOR  gfFormIsActive(&lcHostFormName) AND ((TYPE('_screen.ActiveForm.ariapageframe1') = "O" AND  _screen.ActiveForm.ariapageframe1.ActivePage<>2) .OR. (_Screen.ActiveForm.Parent.ActiveMode <> 'A'))
ON SELECTION BAR loFormSet.NextOptionBar OF _INQURYPOP  DO lfOpnOG WITH _Screen.ActiveForm.Parent
loFormSet.NextOptionBar = loFormSet.NextOptionBar + 1
*loFormSet = _Screen.ActiveForm.Parent
*------- endof 
*!**************************************************************************
*! Name       : lfSOPROSH
*! Developer  : Mostafa Eid [MOS]
*! Date       : 04/14/2009
*! Purpose    : IMPORT SPREADSHEET FROM SO SCREEN 
*!**************************************************************************
*C201136
FUNCTION lfOpnOG
PARAMETERS loFormSet
LOCAL lcOldDataSession 
lcOldDataSession = loFormSet.dataSessionID
lcExpr = gfOpGrid('SOPROSH',.T. )

*!**************************************************************************
*! Name       : lfMSG
*! Developer  : Mostafa Eid [MOS]
*! Date       : 04/14/2009
*! Purpose    : CHECK IF USER INPUT ADRESSES IN SPRAEDSHEET  
*!**************************************************************************
*C201136
FUNCTION lfMSG
LOCAL lcMsg
IF llUseAdd = 'Y'
 lcMsg = "Please Make Sure That address Details have been entered in the spradsheet"
 IF !EMPTY(lcMsg)
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
 ENDIF
ENDIF   

*!**************************************************************************
*! Name       : lfwRepWhen()
*! Developer  : Mostafa Eid [MOS]
*! Date       : 04/14/2009
*! Purpose    :CALLED FROM OG
*!**************************************************************************
*C201136
FUNCTION lfwRepWhen
lcImpXls = gfGetMemVar('M_SOIMPDIR',oAriaApplication.ActiveCompanyID) 
*!*************************************************************
*! Name      : lfGetXls
*! Developer : Mostafa Eid 
*! Date      : 03/23/2009
*! Purpose   : Importing Excel Sheet 
*!*************************************************************
*! Example   : =lfGetXlsFile()
*!*************************************************************
*C201136
FUNCTION lfGetXls

lcImpDir= gfGetMemVar('M_SOIMPDIR',oAriaApplication.ActiveCompanyID)
LOCAL lcSaveDir
IF EMPTY(lcImpXls) OR "?" $ lcImpXls
  lcSaveDir = FULLPATH("")
  CD (lcImpDir)
  lcImpXls = GETFILE("XLS") 
  lcFileName = JUSTFNAME(lcImpXls)
  CD (lcSaveDir)
ENDIF 
RETURN lcFileName 
*!**************************************************************************
*! Name       : lfProcess()
*! Developer  : Mostafa Eid [MOS]
*! Date       : 04/14/2009
*! Purpose    : caLLED FROM OG
*!**************************************************************************
*C201136
FUNCTION lfProcess
STORE 0 TO lnClrLen , lnClrPos 
lcFileName = JUSTFNAME(lcImpXls)
lnRecNo = 1
lcXlsHis= gfGetMemVar('M_SOSAVDIR',oAriaApplication.ActiveCompanyID)

LOCAL lcMsg
IF !FILE(lcImpXls)
  lcMsg = "Please Select Valid Spreaddsheet"
  IF !EMPTY(lcMsg)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
  RETURN .F.
 ENDIF
ENDIF 

*--- Get Temp Tables From So SCreen 
lnOgDataSess = SET("Datasession")
SET DATASESSION TO loFormSet.dataSessionID

IF !USED('ALTSHIP')
  =gfOpenTable(oAriaApplication.DataDir+'ALTSHIP','ALTSHIP','SH')
ENDIF

lcordhdr  = loformset.oformenvironment.lcordhdr
lcordline = loformset.oformenvironment.lcordline

SELECT(lcordhdr)
lcRpAccnt = &lcordhdr..account
lcstore = &lcordhdr..store
lcTimes = gfGetTime()

*- IMPORT PROCESS
SELECT 0
lcSvErr = ON('ERROR')
llErr = .F.
ON ERROR llErr = .T.
IMPORT FROM (lcImpXls) TYPE XL8
ON ERROR &lcSvErr
IF llErr
  MESSAGEBOX('Spread Sheet is being used , Please Close !')
  RETURN .F.
ENDIF
lcAlias = DBF()
USE 

lcTmpXls  = gfTempName()
lcWorkXLDir = (oAriaApplication.WorkDir + lcTmpXls +'.dbf')
IF FILE(lcWorkXLDir) 
  USE IN (lcWorkXLDir)
  DELETE FILE (lcWorkXLDir)
ENDIF 

RENAME (lcAlias) TO (lcWorkXLDir) 
USE (lcWorkXLDir)
GO 7
lcEmployee = ALLTRIM(&lcTmpXls..K)
GO 5
Lccontref  = ALLTRIM(&lcTmpXls..K)

IF !EMPTY(lcEmployee)
  SELECT CONTACT   
  IF gfSEEK('C'+ PADR(ALLTRIM(lcRpAccnt),8)+PADR(ALLTRIM(lcstore),8),'CONTACT')
    LOCATE  REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT ='C'+ PADR(ALLTRIM(lcRpAccnt),8)+PADR(ALLTRIM(lcstore),8) ;
    FOR CCNTCTCODE = PADR(lcEmployee,12)
      IF !FOUND()   
        lcMsg = "Employee Does Not Exist"
        =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
        SET DATASESSION TO lnOgDataSess   
        RETURN .F.     
      ENDIF  
  ENDIF   
ENDIF    
 
 IF llUseAdd = 'Y' 
  SELECT(lcTmpXls) 
  LOCATE 
  GO 2
  lcName = ALLTRIM(&lcTmpXls..B)
  GO 5
  LcAddress1 = ALLTRIM(&lcTmpXls..A)
  GO 6
  LcAddress2 = ALLTRIM(&lcTmpXls..A)
  GO 7
  LcAddress3 = ALLTRIM(&lcTmpXls..A)
  LcAddress4 = ALLTRIM(&lcTmpXls..E)

  SELECT (lcOrdhdr )
  replace alt_shpto  WITH .Y. ;
          stname     WITH lcName ;
          caddress1  WITH LcAddress1 ;
          caddress2  WITH LcAddress2 ;
          caddress3  WITH LcAddress3 ;
          caddress4  WITH LcAddress4 ;
                
  SELECT ALTSHIP        
  =gfSEEK(lcRpAccnt,'ALTSHIP','ALTSHIP')   
  GO BOTTOM
  lnCode = IIF(TYPE(ALTSHIP.Caddcode)='N',ALTSHIP.Caddcode,VAL(ALTSHIP.Caddcode))
  *lcNexSeq = STR(PADL(INT(lcCode + 1),6,'0'))

  IF EMPTY(lnCode)
   lcNewSeQ = "000001" 
  ELSE  
   lnNewSeQ = &lnCode + 1
   lcNewSeQ = PADL(ALLTRIM(STR(lnNewSeQ)),6,'0') 
  ENDIF 
  
  =gfCloseTable('ALTSHIP')
  
  IF !USED('ALTSHIP')
    =gfOpenTable(oAriaApplication.DataDir+'ALTSHIP','ALTSHIP','SH')
  ENDIF

  SELECT ALTSHIP
  gfappend()
  gfREPLACE([ACCOUNT   WITH lcRpAccnt])
  gfREPLACE([caddcode  WITH lcNewSeQ])
  gfREPLACE([BTNAME    WITH lcName ])
  gfREPLACE([caddress1 WITH LcAddress1 ])
  gfREPLACE([caddress2 WITH LcAddress2 ])
  gfREPLACE([caddress3 WITH LcAddress3 ])
  gfREPLACE([caddress4 WITH LcAddress4])
  gfREPLACE([dadd_date WITH oAriaApplication.SystemDate ])
  gfREPLACE([cadd_user WITH OAriaApplication.user_id ])
  gfREPLACE([cadd_time WITH lcTimes ])
  gftableupdate()
ENDIF 
  =gfCloseTable('ALTSHIP')

*-- PROCESSING The SpraedSheet [BEGIN]

SELECT (lcTmpXls)

GO 12
SCAN REST FOR !EMPTY(&lcTmpXls..a) AND !("FIT"$UPPER(&lcTmpXls..D)) AND !EMPTY(&lcTmpXls..e)
WAIT WINDOW " Processsing The Spread Sheet Is In Progress ..... " NOWAIT  
   SCATTER MEMO TO laQtyList
  *-- style code 
  lcstyleCode = PADR(&lcTmpXls..a,12)
  *-- color code  
  SELECT codes 
  SET ORDER TO CCODE_NO   
  =SEEK("N"+"COLOR",'Codes','CCODE_NO')
  LOCATE REST WHILE CDEFCODE+CFLD_NAME = "N"+"COLOR" FOR CDISCREP = LTRIM(PADR(&lcTmpXls..c,30))
  =lfGetClrD()
  lcColCode =   PADR(Codes.CcODE_no,lnClrLen)
  
  *-get scale 
  lcScal = &lcTmpXls..e
  lcfit  = &lcTmpXls..d
  lnx  = 5
  
  SELECT SCALE
  gfSetOrder("SCALE")
  LOCATE FOR SCALE = ALLTRIM(lcScal)AND SCALE.CDIM1 = PADR(lcfit,5)
  *-- Start scan the spraedsheet  
  SCAN REST WHILE TYPE+ SCALE = "S"+ALLTRIM(lcScal) FOR  SCALE.CDIM1 = PADR(lcfit,5)
    *B609209,1 MMT 04/15/2010, Fix bug of missing lines while importing SO From excel(DCC)[Start]
    lnScaleRec = RECNO('Scale')
    *B609209,1 MMT 04/15/2010, Fix bug of missing lines while importing SO From excel(DCC)[End]   
    lnScalCount = SCALE.CNT
    *--get scale and full style code 
    lcFullscale = scale.scale 
    lcstyle = lcstylecode+'-'+lcColCode+lcFullscale
    lnGrssPric = ""
    
    SELECT style
    gfsetorder("STYLE")
    LOCATE FOR style = lcstyle 
    lcStyleDesc = style.desc1
    *------- 
    SELECT (lcordline)
    SET ORDER TO 
    gfSetOrder("ORDLINE")
    APPEND BLANK  
    FOR lnI = 1 TO 8   
      lcI = STR(lnI,1)
      lnx = lnx + 1
      LnValue = IIF(TYPE('laQtyList[lnx]')='N',laQtyList[lnx],VAL(laQtyList[lnx]))
      IF lnI <= lnScalCount
         
         REPLACE &lcordline..QTY&lcI    WITH LnValue ;
                 &lcordline..BOOK&lcI   With LnValue 
      ELSE 
        EXIT 
      ENDIF 
    ENDFOR  
        
     SUM(qty1+qty2+qty3+qty4+qty5+qty6+qty7+qty8)         RECORD  RECNO()   to  lntotqty
     SUM(book1+book2+book3+book4+book5+book6+book7+book8) RECORD  RECNO()   to  lntotbook
     
     IF lntotqty = 0
       DELETE 
       EXIT 
     ENDIF 
     
     Replace style                WITH lcstyle ; 
             cordtype             WITH 'O' ;
             ACCOUNT              WITH lcRpAccnt;
             CWARECODE            WITH &lcordhdr..Cwarecode;
             STORE                WITH &lcordhdr..store;
             LINENO               WITH lnRecNo ;
             style                WITH lcStyle ;
             season               WITH &lcordhdr..Season;
             SCALE                WITH lcFullscale;
             start                WITH &lcordhdr..Start;
             complete             WITH &lcordhdr..Complete ;
             cadd_user            WITH OAriaApplication.user_id;
             dadd_date            WITH oAriaApplication.SystemDate  ;
             totqty               WITH lntotqty ;
             totbook              WITH lntotbook;
             flag                 WITH 'N'       ;
             EMPLOYEE             WITH lcEmployee;
             cadd_time            WITH lcTimes    ;             
             gl_sales             WITH &lcordhdr..link_code  ;
             desc1                with lcStyleDesc
            lnRecNo = lnRecNo + 1
    
     =gfSEEK('M'+lcRpAccnt,'CUSTOMER')
     IF !EMPTY(customer.priccode)
       =lfCodPrice()
      IF EMPTY(lnGrssPric)
         PriceLevel = CUSTOMER.PRICELVL 
         STORE 0 TO lcGros_Price,lcPrice,lcDisc_Pcnt , lcGetPrice
         lnTotQty = 1
         lcContract = ''
         lcStore = (&lcTmpXls..E)
         =lfSTYPRICE(lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt,"lcContract", lnTotQty)
         SELECT (lcOrdLine) 
         REPLACE PRICE      WITH lcGetPrice ;
                 gros_price WITH lcGetPrice
       ENDIF   
     ELSE 
         
         PriceLevel = CUSTOMER.PRICELVL 
         STORE 0 TO lcGros_Price,lcPrice,lcDisc_Pcnt , lcGetPrice
         lnTotQty = 1
         lcContract = ''
         lcStore = (&lcTmpXls..E)
         =lfSTYPRICE(lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt,"lcContract", lnTotQty)
         SELECT (lcOrdLine) 
         REPLACE PRICE      WITH lcGetPrice   ;
                 gros_price WITH lcGetPrice   
     ENDIF   
   *!---- end of pricing code  
    *B609209,1 MMT 04/15/2010, Fix bug of missing lines while importing SO From excel(DCC)[Start]
    SELECT Scale 
    IF BETWEEN(lnScaleRec,1,RECCOUNT())
      GO RECORD lnScaleRec
    ENDIF
    *B609209,1 MMT 04/15/2010, Fix bug of missing lines while importing SO From excel(DCC)[End]   
  ENDSCAN 
ENDSCAN 

=lfUpdHdr() && update order header 

SELECT(lcOrdhdr)
REPLACE &lcOrdhdr..Sheet WITH UPPER(lcRpAccnt)+"-"+UPPER(ALLTRIM(lcFileName));
         ccontref        WITH Lccontref ;
         custpo          with Lccontref ;
         cadd_user       WITH OAriaApplication.user_id;
         dadd_date       WITH oAriaApplication.SystemDate  ;
         cadd_time       WITH lcTimes   

*-- PROCESSING The SpraedSheet [END]

*no need for these two variables
*!*    llCusmsg  = .T.
*!*    llselMode = .T. && used in identify this ticket
*no need for these two variables

llselMode = .T.
SELECT STYLE 
DO lfSavScr IN (oAriaApplication.ApplicationHome + 'SO\SOUPDATE.FXP') WITH .F.,'A',lcOrdHdr,lcOrdLine,'','','',loformset
SET DATASESSION TO lnOgDataSess   

*!*************************************************************
*! Name      : lfStyPrice 
*: Developer : Mostafa Eid(mos)
*: Date      : 03/04/2009
*! Purpose   : get style price 
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Example   : =lfStyPrice ()
*!*************************************************************
*C201136
FUNCTION lfStyPrice 
PARAMETERS lcStyle,lcStore,lcGros_Price,lcPrice,lcDisc_Pcnt,lcContract, lnTotQty

lcStyle = PADR(lcStyle,19)
=gfseek(lcStyle,'STYLE','STYLE')
  *-- Get Style Gross Price
  lcGros_Price = gfGetprice(lcStyle,PriceLevel,lnTotQty,&lcOrdHDr..cCurrCode)
  IF lcGros_Price = 0
    IF PriceLevel = 'Q'
      DO CASE
        CASE Style.nAtQtyC > 0 AND lnTotQty > Style.nAtQtyC
          lcLevel = 'C'
        CASE Style.nAtQtyB > 0 AND lnTotQty > Style.nAtQtyB
          lcLevel = 'B'
        OTHERWISE
          lcLevel = 'A'
      ENDCASE
    ELSE
      lcLevel=IIF(INLIST(PriceLevel,'A','B','C'),PriceLevel,'A')
    ENDIF
      IF lcGros_Price <> 0
       lcGros_Price = lfCheckPri(lcStyle,lcLevel,&lcOrdHDr..cCurrCode)
      ENDIF 
  ENDIF  

  *-- get style discount
   lcDiscCode  = IIF(SEEK(lcStyle+&lcOrdHDr..cWareCode+SPACE(10),'StyDye'),StyDye.cDiscCode,'')
   m.Disc_Pcnt = 0 
  IF !EMPTY(lcDiscCode)
    *-- Get discount type, start date, end date, and discount percent
    DECLARE laDisRltFld[4,2]
    STORE '' TO lcDisType
    STORE {} TO ldstartDte, ldEndDate
    STORE 0  TO lnDisc_Pcnt
    laDisRltFld[1,1] = 'CCOSTAFECT'
    laDisRltFld[1,2] = 'lcDisType'
    laDisRltFld[2,1] = 'START'
    laDisRltFld[2,2] = 'ldstartDte'
    laDisRltFld[3,1] = 'DENDATE'
    laDisRltFld[3,2] = 'ldEndDate'
    laDisRltFld[4,1] = 'DISCPCNT'
    laDisRltFld[4,2] = 'lnDisc_Pcnt'
    =gfRltFld(lcDiscCode, @laDisRltFld, 'CDISCCODE')
    IF ALLTRIM(lcDisType) <> 'R' .AND. BETWEEN(&lcOrdHDr..Entered,ldstartDte,ldEndDate)
      lcDisc_Pcnt = lnDisc_Pcnt
    ENDIF
  ENDIF  
lcGetPrice = lcGros_Price*(100-lcDisc_Pcnt)/100

*!*************************************************************
*! Name      : lfGetClrD
*! Developer : mos
*! Date      : 03/04/2009
*! Purpose   : To get color position also color length
*!*************************************************************
*! Called from : ICSLCAT1.PRG
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            : None
*!*************************************************************
*! Example   : =lfGetClrD
*!*************************************************************
*C201136
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
*:***************************************************************************
*:* Example     :  = lfCodPrice()
*:***************************************************************************
*C201136
FUNCTION lfCodPrice

*B609316,1 MMT 06/29/2010 CODPRICE trigger should be executed in ADD or edit mode of Sales order screen[Start]
IF !(loformset.ActiveMode $ 'EA')
  RETURN 
ENDIF 
*B609316,1 MMT 06/29/2010 CODPRICE trigger should be executed in ADD or edit mode of Sales order screen[End]

lnSlct = SELECT()
=lfOpenTbls()

lcPricCursr = gfTempName()
STORE 0  TO lnClrPos,lnClrLen
=lfGetClrD()    

*B609035,1 TMI 10/12/2009 12:53:55 PM [Start] Define the lcOrdline variable , also default the lnGrssPric to the ordline Gros_Price
lcordline = loformset.oformenvironment.lcordline
lnGrssPric = &lcordline..GROS_PRICE
*B609035,1 TMI 10/12/2009 12:53:55 PM [End  ] 

STORE '' TO  lcPricCode
*-- loop here on sizes to gather them into price-groups
*- Get the price code
lcCurrCode = "GBP"
lcPricCode = lfPRICCODE()

*- Get the currency code
IF SEEK(lcPricCode+lcCurrCode+&lcordline..Style,'CSTPRICE')
  CREATE CURSOR &lcPricCursr (SIZES C(8),Gros_Price N(12,2),COMMDV N(12,2),;
         QTY1 N(6),QTY2 N(6),QTY3 N(6),QTY4 N(6),QTY5 N(6),QTY6 N(6),QTY7 N(6),QTY8 N(6),TOTQTY N(6))
  INDEX ON Gros_Price TAG Gros_Price
  LOCAL lnI,lcI
  FOR lnI = 1 TO 8
    lcI = STR(lnI,1)
    IF !EMPTY(&lcordline..QTY&lcI)
      *- use the special size price , if it is 0 use the special scale price ( FOR DIR03 ) 
      lnGrssPric = IIF(CSTPRICE.PRICE&lcI<>0,CSTPRICE.PRICE&lcI,;
                   IIF(CSTPRICE.PRICEDV<>0  ,CSTPRICE.PRICEDV,  ;
                                             &lcordline..GROS_PRICE))
      lnStyComm  = IIF(STYLE.COMMISSION,CSTPRICE.COMMDV,0)
    
      *- Add a line per price to the temp ordline file
      IF !SEEK(lnGrssPric,lcPricCursr)
        INSERT INTO &lcPricCursr (SIZES,Gros_Price,COMMDV,QTY&lcI,TOTQTY) VALUES (lcI,lnGrssPric,lnStyComm,&lcordline..QTY&lcI,&lcordline..QTY&lcI)
      ELSE
        SELECT &lcPricCursr 
        REPLACE QTY&lcI WITH &lcordline..QTY&lcI ;
                TOTQTY  WITH TOTQTY + QTY&lcI       ;
                SIZES   WITH ALLTRIM(SIZES)+lcI
      ENDIF
    ENDIF
  ENDFOR
  
   *- save the current line data to memo variables and Delete it.
  lnLnCnt = 1
  SELECT &lcordline
  SCATTER MEMVAR MEMO FIELDS EXCEPT QTY*
  
  SELECT &lcPricCursr
  SCAN
    SCATTER FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,GROS_PRICE MEMVAR
    SELECT &lcordline
    
    && if only one price group then just update the line , otherwise add extra lines for groups other than the first one
    IF lnLnCnt>1  
      APPEND BLANK
      SELECT &lcOrdHdr     
      REPLACE LASTLINE WITH LASTLINE + 1
      m.LINENO = &lcOrdHdr..LASTLINE
      SELECT &lcordline
    ENDIF
    
    GATHER MEMVAR MEMO
    REPLACE PRICE  WITH GROS_PRICE*(100-DISC_PCNT)/100 ;
            COWNER WITH &lcPricCursr..SIZES ;
            BOOK1  WITH QTY1 ;
            BOOK2  WITH QTY2 ;
            BOOK3  WITH QTY3 ;
            BOOK4  WITH QTY4 ;
            BOOK5  WITH QTY5 ;
            BOOK6  WITH QTY6 ;
            BOOK7  WITH QTY7 ;
            BOOK8  WITH QTY8 ;
            TOTBOOK WITH BOOK1+BOOK2+BOOK3+BOOK4+BOOK5+BOOK6+BOOK7+BOOK8 ;
            CSTSZPRICE WITH &lcPricCursr..SIZES
    lnLnCnt = lnLnCnt + 1
   
   ENDSCAN
  
  *- Release the temp cursor
  USE IN &lcPricCursr
 
ENDIF
SELECT (lnSlct)
RETURN  lnGrssPric   
*-- end of lfCodPrice.

*:**************************************************************************
*:* Name        : lfOpenTbls
*:* Developer   : MOS
*:* Date        : 05/11/2009
*:* Purpose     : open needed tables
*:***************************************************************************
*C201136
FUNCTION lfOpenTbls

IF !USED('SycCurr')
  =gfOpenTable(oAriaApplication.SysPath+'SycCurr',oAriaApplication.SysPath+'cCurrCode','SH')
ENDIF
IF !USED('CSTPRICH')
  =gfOpenTable(oAriaApplication.DataDir+'CSTPRICH','CSTPRICH','SH')
ENDIF
IF !USED('CSTPRICE')
  =gfOpenTable(oAriaApplication.DataDir+'CSTPRICE','CSTPRICE','SH')
ENDIF
IF !USED('STYLE')
  =gfOpenTable(oAriaApplication.DataDir+'STYLE','STYLE','SH')
ENDIF
IF !USED('SCALE')
  =gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
ENDIF
*-- end of lfOpenTbls.

*:**************************************************************************
*:* Name        : lfPRICCODE
*:* Developer   : MOS
*:* Date        : 05/11/2009
*:* Purpose     : get Customer Price code 
*:***************************************************************************
*C201136
FUNCTION lfPRICCODE
LOCAL lcSlct,lcPricCode,lcSvKey
lcSlct = SELECT()

SELECT CUSTOMER
*B609035,1 TMI 10/12/2009 12:32:36 PM [Start] Define the customer id From the screen variable,also the lcOrdLine variable
*lcAccount = lcRpAccnt
lcAccount = IIF(TYPE('lcRpAccnt')='C',lcRpAccnt,loFormSet.Ariaform1.keyAccount.Keytextbox.Value)
lcordline = loformset.oformenvironment.lcordline
*B609035,1 TMI 10/12/2009 12:32:38 PM [End  ] 

lcSvKey = EVALUATE(KEY())
=gfSEEK('M'+lcAccount,'CUSTOMER')

*B609035,1 TMI 10/12/2009 11:34:38 AM [Start] Define the variable ldStartDate 
LOCAL lcDateTyp,ldStartDate
lcDateTyp = PADR(gfGetMemVar('M_CHKPRIAG'),1)
DO CASE
  CASE lcDateTyp = 'E'   && Entered
    ldStartDate = loFormSet.AriaForm1.Ariapageframe1.Page1.txtEntered.Text1.Value

  CASE lcDateTyp = 'C'   && Complete
    ldStartDate = loFormSet.AriaForm1.Ariapageframe1.Page1.txtComplete.Text1.Value

  OTHERWISE     && = 'S'   && Start
    ldStartDate = loFormSet.AriaForm1.Ariapageframe1.Page1.txtStart.Text1.Value

ENDCASE
*B609035,1 TMI 10/12/2009 11:34:42 AM [End  ] 

LOCAL lnI,lcI,lcNoDtPCod

*-- Loop through all price codes
*-   pick the first price code with suitable date,
*-   if no one, pick the first one with no valid dates , otherwise return chr(255)
lcNoDtPCod = CHR(255)
  
*- Download needed lines to seek in from cstprice and cstprich sql files
SELECT CSTPRICE
=gfSetOrder('CSTYCODE')
=gfSEEK(&lcordline..STYLE,'CSTPRICE')
=gfSetOrder('CSTPRICE')

PRIVATE lcStyClr
lcStyClr = PADR(SUBSTR(&lcordline..STYLE,1,lnClrPos+lnClrLen-1),19) 
SELECT CSTPRICH
=gfSetOrder('STYLE')
=gfSEEK(lcStyClr,'CSTPRICH')
=gfSetOrder('CSTPRICH')
  
FOR lnI = 1 TO 15
  lcI = IIF(lnI = 1 , '' , PADL(lnI,2,'0') ) 
    
  IF !EMPTY(CUSTOMER.PRICCODE&lcI).AND. SEEK(CUSTOMER.PRICCODE&lcI+lcCurrCode+&lcordline..STYLE,'CSTPRICE') ;
                                  .AND. SEEK(CUSTOMER.PRICCODE&lcI+lcCurrCode+lcStyClr,'CSTPRICH')
    IF EMPTY(CSTPRICH.DVLDPRTO)
      *- Get no valid date price code
      lcNoDtPCod = UPPER(CUSTOMER.PRICCODE&lcI)
    ELSE
      *- Compare valid  prices for Banach based on setup ( Entered, Start or Complete Dates)
      IF BETWEEN(ldStartDate,CSTPRICH.DVLDPRFR,CSTPRICH.DVLDPRTO)
        EXIT
      ENDIF

    ENDIF
  ENDIF    
ENDFOR

lcPRICCODE = IIF(lnI < 16 , UPPER(CUSTOMER.PRICCODE&lcI) , lcNoDtPCod )

*- restore customer record
=gfSeek(lcSvKey,'CUSTOMER')

SELECT (lcSlct)
RETURN lcPRICCODE
*-- end of lfPRICCODE.
*:**************************************************************************
*:* Name        : lfUpdHdr
*:* Developer   : MOS
*:* Date        : 05/11/2009
*:* Purpose     : Update the lcOrdHdr file
*:***************************************************************************
*C201136
FUNCTION lfUpdHdr

LOCAL lnLastLine,lnOpenQty,lnOpenPrice,lnBookQty,lnBookAmt
SELECT &lcordline
LOCATE
CALCULATE SUM(TOTQTY),SUM(TOTQTY*PRICE),SUM(TOTBOOK),SUM(TOTBOOK*PRICE) TO lnOpenQty,lnOpenPrice,lnBookQty,lnBookAmt
GO BOTTOM
SELECT (lcOrdhdr)
REPLACE OPEN     WITH lnOpenQty  ;
        OPENAMT  WITH lnOpenPrice ;
        BOOK     WITH lnBookQty ;
        BOOKAMT  WITH lnBookAmt

*:**************************************************************************
*:* Name        : lfCustMsg  
*:* Developer   : mos 
*:* Date        : 05/11/2009
*:* Purpose     : display so saving cust msg
*:* called from : SOUPDATE  
*:***************************************************************************
*C201136
*!*    FUNCTION lfCustMsg  
*!*    lcMsg = 'Sales Order  '+ lcOrderNo + '  has been Created For spread sheet '+ lcFileName 
*!*    =gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
*!*    lcXlsHis= IIF(RIGHTC(ALLTRIM(lcXlsHis),1)="\",ALLTRIM(lcXlsHis),ALLTRIM(lcXlsHis)+"\")
*!*    lcEndName = ALLTRIM(lcXlsHis)+UPPER(lcRpAccnt)+"-"+UPPER(ALLTRIM(lcFileName))
*!*    IF FILE(lcEndName)
*!*      IF gfModalGen('QRM00000B42002',.F.,.F.,.F.,'Spread Sheet With the Name  '+lcRpAccnt+"-"+ALLTRIM(lcFileName)+'  already Exists, Replace ?') = 1 
*!*        DELETE FILE (lcEndName)
*!*        RENAME (lcimpXls) TO (lcEndName)  
*!*      ELSE 
*!*      RETURN .F. 
*!*      ENDIF    
*!*    ELSE 
*!*      RENAME (lcimpXls) TO (lcEndName)
*!*    ENDIF 

*-- end of lfUpdHdr.
*!**************************************************************************
*! Name       : lfSOADEMPCOL
*! Developer  : Mostafa Eid [MOS]
*! Date       : 04/14/2009
*! Purpose    : adding employee column to so screen
*! tracking # : C201131  
*!**************************************************************************
FUNCTION lfSOADEMPCOL

 IF loFormset.Activemode <> 'S' AND loFormSet.llDetails
   SELECT (lcOrderFile)
   loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1.ColumnCount =   loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1.ColumnCount + 1
   lnLast = loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1.ColumnCount 
   lcOrderFile = loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1.recordsource
   loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1.Columns(lnLast).controlsource= lcOrderFile+'.employee'
   *loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1.Columns(lnLast).WIDTH= 30
   loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1.Columns(lnLast).header1.caption = 'Employee #' 
   loFormSet.AriaForm1.Ariapageframe1.Page2.Ariagrid1.SETALL('ReadOnly',.T.,'COLUMN')
 
 ENDIF 
 
*:**************************************************************************
*:* Name        : lfSHWCSTNT
*:* Developer   : mos - mostafa eid 
*:* Date        : 12/5/2009
*:* Purpose     : Show Customer Order Notes at Sales Order Entry 
*! tracking #   : C201131  
*!**************************************************************************
FUNCTION lfSHOWNTS
IF gfModalGen('QRM00349B32000','DIALOG') = 1   
  LOCAL lnSlct,loNotePad
  lnSlct = SELECT(0)
  =NOTEPAD('B',IIF(OrdHdr.cOrdType='T','T','')+OrdHdr.ORder)
  SELECT (lnSlct)
ENDIF   
*-- end of lfSHWCSTNT.

*C201141,1 HES 04/26/2009, quote Processing Enbroidery orders for DCC [Start]
*!**************************************************************************
*! Name       : lfMfPrRcData
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/06/2009
*! Purpose    : Preparing the required data for creating the PO and the cost sheet
*! Tracking # : C201141
*!**************************************************************************
FUNCTION lfMfPrRcData

IF FILE('C:\X.TXT')
set step on 
ENDIF

IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) AND loFormSet.ActiveMode <> 'E'

  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
  lcStyCpTmp = gfTempName()
  =lfCrStCpTmp()
  llForm = .F.
  lnMajorlen=LEN(gfItemMask("PM","","0001"))
  STORE  0 TO lnClrLen ,lnClrPos
  DECLARE laItemSeg[1]
  PRIVATE lnCount 
  =gfItemMask(@laItemSeg)
  lnClrLen = 0
  FOR lnCount = 1 TO ALEN(laItemSeg,1)
    IF laItemSeg[lnCount,1]='C'
      lnClrLen = LEN(laItemSeg[lnCount,3])
      EXIT
    ENDIF
  ENDFOR
  lnSclLen=19-(lnMajorlen+lnClrLen+1)  
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

  loFormSet.AddProperty('llIfAut',.F.)
  loFormSet.AddProperty('lnPerce',0)
  loFormSet.AddProperty('lnReqIssu',0)

  =gfOpenTable(oariaapplication.datadir +'STYDYE',oariaapplication.datadir +'STYDYE','SH')
  =gfOpenTable(oariaapplication.datadir +'BOM',oariaapplication.datadir +'multibom','SH')
  
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
  =gfOpenTable(oariaapplication.datadir +'BOMHEADR',oariaapplication.datadir +'bomheadr','SH')
  
*!*    SELECT BOM
*!*    gfSetorder('mbomitem')
*!*    gfseek('')
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
  
  LOCAL lcPoItem,lnctktno,lnitemqty,lcstyle,lnTotstk,lnTotAlo,lnAvl,lnRestQty,lnUnitQty,lnPCGros_Price,llSAlStrs,;
        lcCompWarHos,lcCstSht_id,lcMajorStyle,lnAvl1,lnAvl2,lnAvl3,lnAvl4,lnAvl5,lnAvl6,lnAvl7,lnAvl8,llIAlStrs,;
        lnQtty1,lnQtty2,lnQtty3,lnQtty4,lnQtty5,lnQtty6,lnQtty7,lnQtty8,lcDefCSht,lnPoTotQty,lnBomItmAmnt,lcPriItm,;
        lnAct1,lnAct2,lnAct3,lnAct4,lnAct5,lnAct6,lnAct7,lnAct8,lnCntr,lcSClrPart,lcSSclPart,lcIClrPart,lcISclPart
        
  STORE 0 TO lnctktno,lnitemqty,lnTotstk,lnTotAlo,lnAvl,lnRestQty,lnUnitQty,lnPCGros_Price,lnAvl1,lnAvl2,lnAvl3,lnAvl4
  STORE 0 TO lnAvl5,lnAvl6,lnAvl7,lnAvl8,lnQtty1,lnQtty2,lnQtty3,lnQtty4,lnQtty5,lnQtty6,lnQtty7,lnQtty8,lnPoTotQty
  STORE 0 TO lnAct1,lnAct2,lnAct3,lnAct4,lnAct5,lnAct6,lnAct7,lnAct8,lnCntr,lnCheck,lnAllQtys,lnAllReqQtys,lnBomItmAmnt
  
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*    DIMENSION laRecData[1,30]
  DIMENSION laRecData[1,46]
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
  
  lnCntr = 1

  SELECT(loFormSet.Ariaform1.mainWorkOrder.cPoLine)
  
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
  *=lfSetIndex(loFormSet.Ariaform1.mainWorkOrder.cPosLn,'POSLN','cbusdocu+cstytype+po+cinvtype+style+STR(lineno,6)+trancd')
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

  LOCATE
  *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
  *SCAN FOR !EOF()
  SCAN FOR !EOF() AND !EMPTY(cCstSht_Id)
  *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
    lcCurStyl = STYLE
    
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
    lcSClrPart = SUBSTR(lcCurStyl,lnMajorlen+2,lnClrLen)
    lcSSclPart = SUBSTR(lcCurStyl,lnMajorlen+lnClrLen+2,lnSclLen)  
    lnPoTotQty = TotQty
    lcStyMaj = SUBSTR(lcCurStyl,1,lnMajorlen)
    
    SELECT BOMHEADR
    gfSeek('0001'+lcStyMaj)
    *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
    *lcDefCSht = cCstSht_Id
    lcDefCSht = EVALUATE(loFormSet.Ariaform1.mainWorkOrder.cPoLine+'.cCstSht_Id')
    *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
    SELECT BOM
    gfSeek('0001'+PADR(lcStyMaj,19)+'I'+lcDefCSht)
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
      
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
    *SELECT (loformset.lcbomline)
    SELECT BOM
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
    *B609478,1 TMI 12/11/2010 [Start] Add more lines based on the scale mappings    
    *** I noticed that the written code gets the scale of the parent style, then searchs this in the MSZCROSREF field
    *** and gets the related scale of the component style, and deals only with one scale from the component
    *** this is ok if the user selected only one scale from the component style, in the other case we need to repeat this
    *** action per scales used from the component, so I added the more lines 
    SELECT BOM
    SCAN FOR citmmajor = lcStyMaj AND typ = "2" AND recno()>0 
      SCATTER MEMVAR MEMO
      lnCurrRec = RECNO()
      =ALINES(laLines,M.MSZCROSREF)
      *- add the scale of the component first to sort the array by it
      FOR lnCnt = 1 TO ALEN(laLines)        
        laLines[lnCnt] = SUBSTR(laLines[lnCnt],7,3)+laLines[lnCnt]
      ENDFOR
      =ASORT(laLines)
      *- return the array back to its original value
      FOR lnCnt = 1 TO ALEN(laLines)        
        laLines[lnCnt] = SUBSTR(laLines[lnCnt],4)
      ENDFOR
      lcScl = SUBSTR(laLines[1],7,3)
      llNew = .F.
      *B609478,4 TMI 12/19/2010 [Start] keep in each line only one scale of the child 
      *          reason for this : as the program is designed to deal only with one scale of the chiled 
      *          then if there are unneeded scales of the chiled this causes duplication
      *          example of this when 
      *FOR lnCnt =  2 TO ALEN(laLines)        
        *IF lcSSclPart = LEFT(laLines[lnCnt],3)
      REPLACE MSZCROSREF WITH ''
      FOR lnCnt =  1 TO ALEN(laLines)        
        IF lnCnt>1 AND lcSSclPart = LEFT(laLines[lnCnt],3)
          *B609478,4 TMI 12/19/2010 [End  ] 
          IF lcScl <> SUBSTR(laLines[lnCnt],7,3)
            SELECT BOM
            APPEND BLANK     && added lines take recno() < 0
            M.MSZCROSREF = ''
            GATH MEMVAR MEMO        
            lcScl = SUBSTR(laLines[lnCnt],7,3)
            llNew = .T.
          ENDIF
          *B609478,4 TMI 12/19/2010 [Start] 
          *IF llNew
          *  REPLACE MSZCROSREF WITH MSZCROSREF + laLines[lnCnt] + CHR(13)
          *ENDIF        
          *B609478,4 TMI 12/19/2010 [End  ] 
        ENDIF
        *B609478,4 TMI 12/19/2010 [Start] 
        REPLACE MSZCROSREF WITH MSZCROSREF + laLines[lnCnt] + CHR(13)
        *B609478,4 TMI 12/19/2010 [End  ] 
      ENDFOR
      GOTO       lnCurrRec 
    ENDSCAN
    SELECT BOM
    *B609478,1 TMI 12/11/2010 [End  ]         
    LOCATE    
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
    *SCAN FOR STYLE = lcCurStyl AND ccatgtyp = 'S'  && To handle the issue of raising a PO with a style has a style component with multi colors           
    SCAN FOR citmmajor = lcStyMaj AND typ = "2"
      *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
      
      *B609478,4 TMI 12/19/2010 [Start] 
      =ALINES(laLines,BOM.MSZCROSREF)
      *B609478,7 tmi [Start] not just checking the first element of the array
      *IF !lcSSclPart $ LEFT(laLines[1],3)
      FOR lnI = 1 TO ALEN(laLines,1)
        laLines[lnI] = LEFT(laLines[lnI],3)
      ENDFOR      
      IF ASCAN(laLines,lcSSclPart) = 0
        *B609478,7 tmi [End]
        LOOP
      ENDIF
      *B609478,4 TMI 12/19/2010 [End  ] 
      
      DIMENSION laSzCrsRef[8]
      laSzCrsRef = 0
  	*B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
      lcPItm = Item
      lcMClrPart = SUBSTR(citmmask,lnMajorlen+2,lnClrLen)
      lcMSclPart = SUBSTR(citmmask,lnMajorlen+lnClrLen+2,lnSclLen)            
      llSAlStrs  = '*' $ lcMSclPart AND '*' $ lcMClrPart      
      
      lcIClrPart = SUBSTR(lcPItm,lnMajorlen+2,lnClrLen)
      lcISclPart = SUBSTR(lcPItm,lnMajorlen+lnClrLen+2,lnSclLen) 
      lcMajItm   = SUBSTR(lcPItm,1,lnMajorlen)
      llIAlStrs  = '*' $ lcISclPart AND '*' $ lcIClrPart
      *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
      lcItemScale = ''
      IF !EMPTY(mszcrosref)
        lnScalePos = ATC(lcSSclPart,mszcrosref)
        IF lnScalePos > 0 
          lcScalStr = SUBSTR(mszcrosref,lnScalePos +LEN(lcSSclPart)-1)
          lnItemScalePOs = ATC('~',lcScalStr )
          IF lnItemScalePOs> 0 
            lcScalStr = SUBSTR(lcScalStr ,lnItemScalePOs+1)
            lnItemScaleEnd = ATC(',',lcScalStr )
            lcItemScale = SUBSTR(lcScalStr ,1,lnItemScaleEnd-1)
          ENDIF
        ENDIF
      ENDIF
      IF !EMPTY(lcItemScale) AND SEEK('S'+lcItemScale ,'Scale','Scale')
        *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
        lcScaleStyle =lcSSclPart 
		*B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
        lcSSclPart =lcItemScale 
        *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
        lcCrosRefFld =mszcrosref
        DIMENSION laSizesArr[1]
        laSizesArr = ''
        =gfSubStr(lcCrosRefFld ,@laSizesArr ,CHR(13))
        FOR lnS = 1 TO 8
          LNPOSINARR = 0
          for lnW = 1 to alen(laSizesArr,1)
            if ALLTRIM(lcItemScale)+','+STR(lnS,1) $ laSizesArr[lnW]
              LNPOSINARR  = lnW
              exit
            ENDif
          ENDFOR
          *LNPOSINARR= ASCAN(laSizesArr,ALLTRIM(lcItemScale)+STR(lnS,1))
          IF LNPOSINARR >0
            lnChlScl = ATC('~',laSizesArr[LNPOSINARR])
            IF lnChlScl > 0
              lcSclPosStr = SUBSTR(laSizesArr[LNPOSINARR],1,lnChlScl -1)
              lnCommPos = ATC(',',lcSclPosStr )
              laSzCrsRef [lnS] = VAL(SUBSTR(lcSclPosStr ,lnCommPos +1))
            ENDIF  
          ENDIF
        ENDFOR
	   *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
      ENDIF
      *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
      IF llIAlStrs AND llSAlStrs
        lcPriItm = lcMajItm + "-" + lcSClrPart + lcSSclPart
      ELSE 
        IF llSAlStrs AND '*' $ lcIClrPart AND !'*' $ lcISclPart 
          lcPriItm = lcMajItm + "-" + lcSSclPart + lcISclPart
        ELSE 
          IF llSAlStrs AND !'*' $ lcIClrPart AND '*' $ lcISclPart 
            lcPriItm = lcMajItm + "-" + lcIClrPart + lcSSclPart
          ELSE 
            lcPriItm = lcPItm
          ENDIF 
        ENDIF  
      ENDIF 
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
    *B610928,1 MMT 01/18/2015 PO manage PO's style component screen show wrong scale[T20150115.0021][Start]
    IF '*' $ lcPriItm 
       lcTempClrPart = SUBSTR(lcPriItm ,lnMajorlen+2,lnClrLen)
       lcTempSclPart = SUBSTR(lcPriItm ,lnMajorlen+lnClrLen+2,lnSclLen) 
       IF  '*' $ lcTempClrPart      
         lcPriItm = lcMajItm + "-" + lcSClrPart + lcTempSclPart 
       ELSE 
         lcPriItm = lcMajItm + "-" + lcTempClrPart + lcSSclPart
       ENDIF
    ENDIF
    *B610928,1 MMT 01/18/2015 PO manage PO's style component screen show wrong scale[T20150115.0021][End]
      *B609478,1 TMI 12/11/2010 [Start] reset the lcSSclPart value      
      IF TYPE('lcScaleStyle')='C'
        lcSSclPart = lcScaleStyle 
        RELEASE lcScaleStyle
      ENDIF
      *B609478,1 TMI 12/11/2010 [End  ] 
      
      STORE 0 TO lnctktno,lnitemqty,lnTotstk,lnTotAlo,lnAvl,lnRestQty,lnUnitQty,lnPCGros_Price,lnAvl1,lnAvl2,lnAvl3
      STORE 0 TO lnAvl4,lnAvl5,lnAvl6,lnAvl7,lnAvl8,lnAct1,lnAct2,lnAct3,lnAct4,lnAct5,lnAct6,lnAct7,lnAct8,lnItmPos
      STORE 0 TO lnItmActPos 
      
      lnItmPos = ASCAN(laRecData,lcPriItm)
      IF lnItmPos > 0
        lnItmActPos = ASUBSCRIPT(laRecData,lnItmPos,1)
        lnCntr = lnItmActPos
        llExist = .T.
        lcAls = ALIAS()
        lcItm = lcPriItm
        SELECT(lcStyCpTmp)
        SEEK(lcItm)
        SELECT(lcAls)
      ELSE 
        IF !EMPTY(laRecData[1])
          lnCurCntr = ALEN(laRecData,1) + 1
        ELSE 
          lnCurCntr = 1
        ENDIF         
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
        llExist = .F.
*!*          DIMENSION laRecData[lnCurCntr,30]
        DIMENSION laRecData[lnCurCntr,46]
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]        
        lnCntr = lnCurCntr         
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*          FOR lnx = 1 TO 30
        FOR lnx = 1 TO 46
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
          laRecData[lnCurCntr,lnX] = 0
        ENDFOR 
      ENDIF 
      
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
      
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
      lnBomUntQty = nBomTotQty
      lnBomUntCst = UntCost
*!*        m.CLRDESC = gfCodDes(PADR(SUBSTR(STYLE,lnMajorlen+2,lnClrLen),6),'COLOR')
      m.CLRDESC = gfCodDes(PADR(SUBSTR(lcPriItm,lnMajorlen+2,lnClrLen),6),'COLOR')
      SELECT BOM      
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]      
      
      laRecData[lnCntr,32] = m.CLRDESC
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]      
      
      lcPoItem = lcPriItm
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
      m.StyComp = lcPriItm
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
      laRecData[lnCntr,1] = lcPoItem
      
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*        lnctktno = ctktno
*!*        laRecData[lnCntr,19] = lnctktno
*!*        lnitemqty = itemqty       
      lnitemqty = lnBomUntQty * lnPoTotQty
      lnBomItmAmnt = lnitemqty * lnBomUntCst
*!*        lcStyle = style 
*!*        m.Style = Style      
      lcStyle = lcCurStyl
      m.Style = lcCurStyl    
      
      lcAls = ALIAS()
      SELECT STYLE        
      SEEK(lcPriItm) 
      laRecData[lnCntr,33] = DESC
      SELECT(lcAls)
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
      
      IF lnitemqty > 0 AND !EMPTY(lcPoItem) && to prevent the infinit loop
      
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*          lnIndx = AT('-',lcPoItem)
*!*          lcMajorStyle = SUBSTR(lcPoItem,1,lnIndx-1)
        lcMajorStyle = SUBSTR(lcPoItem,1,lnMajorlen)        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
        
        lcMajorStyle = PADR(lcMajorStyle,19)
         
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start] 
*!*          lnUnitQty = unitQty
*!*          laRecData[lnCntr,25] = lnUnitQty 
*!*          lnPCGros_Price = Itemamt/Itemqty
*!*          laRecData[lnCntr,26] = Itemamt
*!*          laRecData[lnCntr,23] = lnPCGros_Price
        lnUnitQty = lnBomUntQty
        laRecData[lnCntr,25] = lnUnitQty 
        lnPCGros_Price = lnBomItmAmnt/lnitemqty
        laRecData[lnCntr,26] = lnBomItmAmnt
        laRecData[lnCntr,23] = lnPCGros_Price
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
                
        SELECT BOM
        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*          gfSetorder('mbomitem')
        
*!*          LOCATE FOR cItmMajor = lcMajorStyle
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
        
        lcCstSht_id = cCstSht_id
        laRecData[lnCntr,20] = lcCstSht_id 
          
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start] 
*!*          SELECT(loFormSet.lcPosLn)
        SELECT(loFormSet.Ariaform1.mainWorkOrder.cPoLine)
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
        lnQtty1  = IIF(llExist,lnQtty1,0) + qty1*lnUnitQty
        lnQtty2  = IIF(llExist,lnQtty2,0) + qty2*lnUnitQty
        lnQtty3  = IIF(llExist,lnQtty3,0) + qty3*lnUnitQty
        lnQtty4  = IIF(llExist,lnQtty4,0) + qty4*lnUnitQty
        lnQtty5  = IIF(llExist,lnQtty5,0) + qty5*lnUnitQty
        lnQtty6  = IIF(llExist,lnQtty6,0) + qty6*lnUnitQty
        lnQtty7  = IIF(llExist,lnQtty7,0) + qty7*lnUnitQty
        lnQtty8  = IIF(llExist,lnQtty8,0) + qty8*lnUnitQty
        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
        m.POREQ1 = lnQtty1        
        m.POREQ2 = lnQtty2
        m.POREQ3 = lnQtty3
        m.POREQ4 = lnQtty4
        m.POREQ5 = lnQtty5
        m.POREQ6 = lnQtty6
        m.POREQ7 = lnQtty7
        m.POREQ8 = lnQtty8
       *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
        IF !EMPTY(BOM.mszcrosref)
          FOR lnZ = 1 TO 8
            lcZ = STR(lnZ,1)
            IF laSzCrsRef[lnZ]<>0
              lcSizeP = STR(laSzCrsRef[lnZ],1)
              m.POREQ&lcZ. = lnQtty&lcSizeP.
            else
            m.POREQ&lcZ. = 0 
            ENDIF
          ENDFOR
        ENDIF
		*B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
        m.POTotREQ = m.POREQ1+m.POREQ2+m.POREQ3+m.POREQ4+m.POREQ5+m.POREQ6+m.POREQ7+m.POREQ8
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
      
        SELECT STYDYE
        SET ORDER TO STYDYE
        *B610937,1 MMT 01/26/2015 PO manage Style compenents screen shows  incorrect component stock[T20150123.0001][Start]
        *SEEK(lcPoItem)
        =SEEK(PADR(lcPoItem,19)+Eval(loFormSet.Ariaform1.mainWorkOrder.cPoLine+'.CWareCode'),'STYDYE','STYDYE')
        *B610937,1 MMT 01/26/2015 PO manage Style compenents screen shows  incorrect component stock[T20150123.0001][End]
        lnTotstk = Totstk
        lnTotAlo = Totalo
      
        lnAvl1 = stk1 - alo1
        laRecData[lnCntr,2] = lnAvl1
        lnAvl2 = stk2 - alo2
        laRecData[lnCntr,3] = lnAvl2
        lnAvl3 = stk3 - alo3
        laRecData[lnCntr,4] = lnAvl3
        lnAvl4 = stk4 - alo4
        laRecData[lnCntr,5] = lnAvl4
        lnAvl5 = stk5 - alo5
        laRecData[lnCntr,6] = lnAvl5
        lnAvl6 = stk6 - alo6
        laRecData[lnCntr,7] = lnAvl6
        lnAvl7 = stk7 - alo7
        laRecData[lnCntr,8] = lnAvl7
        lnAvl8 = stk8 - alo8
        laRecData[lnCntr,9] = lnAvl8
        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
        m.AVLSTK1 = lnAvl1
        m.AVLSTK2 = lnAvl2        
        m.AVLSTK3 = lnAvl3
        m.AVLSTK4 = lnAvl4        
        m.AVLSTK5 = lnAvl5        
        m.AVLSTK6 = lnAvl6        
        m.AVLSTK7 = lnAvl7        
        m.AVLSTK8 = lnAvl8        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
       
        SELECT STYLE
        SET ORDER TO STYLE
        SEEK(lcPoItem)
        lcCompWarHos = CDefWare
        laRecData[lnCntr,21] = lcCompWarHos
        lcCompVendor = Vendor
        laRecData[lnCntr,22] = lcCompVendor
        
        =gfSEEK('S'+STYLE.SCALE,'SCALE')        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
        =gfOpenTable(oariaapplication.datadir +'SCALEHD',oariaapplication.datadir +'EXTSCALE','SH')
        =gfSEEK(LEFT(STYLE.SCALE,2),'SCALEHD')
        laRecData[lnCntr,45] = SCALE.CNT
        IF SCALEHD.Nnoofdim = 2
          laRecData[lnCntr,34] = SCALE.Cdim1
        ELSE 
          laRecData[lnCntr,34] = ''
        ENDIF 
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
        *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
        FOR lnM =1 TO 8
          lcMn = STR(lnM,1)
          m.SZ&lcMn. = ''
        ENDFOR
        *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]

        lcM = ''
        FOR lny = 1 TO SCALE.CNT
          lcM = lcM + ALLTRIM(STR(lny))
          
          *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
          lcy = ALLTRIM(STR(lnY))
          m.SZ&lcY = ALLTRIM(Scale.SZ&lcY)
          laRecData[lnCntr,34+lny] = m.SZ&lcY
          *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
          
        ENDFOR 
        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
        lnL = SCALE.CNT+1 
        FOR lnN = lnL TO 8
          lcN = ALLTRIM(STR(lnN))
          laRecData[lnCntr,34+lnN] = ''
        ENDFOR 
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
        
        laRecData[lnCntr,29] = lcM
        laRecData[lnCntr,30] = SCALE.SCALE
        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
        
*!*          lnAct1 = IIF((lnQtty1 - lnAvl1) > 0,(lnQtty1 - lnAvl1),0)
*!*          laRecData[lnCntr,10] = laRecData[lnCntr,10] + IIF(SCALE.CNT >= 1 , lnAct1 , 0 )

*!*          lnAct2 = IIF((lnQtty2 - lnAvl2) > 0,(lnQtty2 - lnAvl2),0)
*!*          laRecData[lnCntr,11] = laRecData[lnCntr,11] + IIF(SCALE.CNT >= 2 , lnAct2 , 0 )
*!*          
*!*          lnAct3 = IIF((lnQtty3 - lnAvl3) > 0,(lnQtty3 - lnAvl3),0)
*!*          laRecData[lnCntr,12] = laRecData[lnCntr,12] + IIF(SCALE.CNT >= 3 , lnAct3 , 0 )
*!*          
*!*          lnAct4 = IIF((lnQtty4 - lnAvl4) > 0,(lnQtty4 - lnAvl4),0)
*!*          laRecData[lnCntr,13] = laRecData[lnCntr,13] + IIF(SCALE.CNT >= 4 , lnAct4 , 0 )
*!*          
*!*          lnAct5 = IIF((lnQtty5 - lnAvl5) > 0,(lnQtty5 - lnAvl5),0)
*!*          laRecData[lnCntr,14] = laRecData[lnCntr,14] + IIF(SCALE.CNT >= 5 , lnAct5 , 0 )
*!*          
*!*          lnAct6 = IIF((lnQtty6 - lnAvl6) > 0,(lnQtty6 - lnAvl6),0)
*!*          laRecData[lnCntr,15] = laRecData[lnCntr,15] + IIF(SCALE.CNT >= 6 , lnAct6 , 0 )
*!*          
*!*          lnAct7 = IIF((lnQtty7 - lnAvl7) > 0,(lnQtty7 - lnAvl7),0)
*!*          laRecData[lnCntr,16] = laRecData[lnCntr,16] + IIF(SCALE.CNT >= 7 , lnAct7 , 0 )
*!*          
*!*          lnAct8 = IIF((lnQtty8 - lnAvl8) > 0,(lnQtty8 - lnAvl8),0)
*!*          laRecData[lnCntr,17] = laRecData[lnCntr,17] + IIF(SCALE.CNT >= 8 , lnAct8 , 0 )

        lnAct1 = IIF((lnQtty1 - lnAvl1) > 0,(lnQtty1 - lnAvl1),0)
        laRecData[lnCntr,10] = IIF(SCALE.CNT >= 1 , lnAct1 , 0 )
        
        lnAct2 = IIF((lnQtty2 - lnAvl2) > 0,(lnQtty2 - lnAvl2),0)
        laRecData[lnCntr,11] = IIF(SCALE.CNT >= 2 , lnAct2 , 0 )
        
        lnAct3 = IIF((lnQtty3 - lnAvl3) > 0,(lnQtty3 - lnAvl3),0)
        laRecData[lnCntr,12] = IIF(SCALE.CNT >= 3 , lnAct3 , 0 )
        
        lnAct4 = IIF((lnQtty4 - lnAvl4) > 0,(lnQtty4 - lnAvl4),0)
        laRecData[lnCntr,13] = IIF(SCALE.CNT >= 4 , lnAct4 , 0 )
        
        lnAct5 = IIF((lnQtty5 - lnAvl5) > 0,(lnQtty5 - lnAvl5),0)
        laRecData[lnCntr,14] = IIF(SCALE.CNT >= 5 , lnAct5 , 0 )
        
        lnAct6 = IIF((lnQtty6 - lnAvl6) > 0,(lnQtty6 - lnAvl6),0)
        laRecData[lnCntr,15] = IIF(SCALE.CNT >= 6 , lnAct6 , 0 )
        
        lnAct7 = IIF((lnQtty7 - lnAvl7) > 0,(lnQtty7 - lnAvl7),0)
        laRecData[lnCntr,16] = IIF(SCALE.CNT >= 7 , lnAct7 , 0 )
        
        lnAct8 = IIF((lnQtty8 - lnAvl8) > 0,(lnQtty8 - lnAvl8),0)
        laRecData[lnCntr,17] = IIF(SCALE.CNT >= 8 , lnAct8 , 0 )
        
        m.BLNORD1 = laRecData[lnCntr,10]        
        m.BLNORD2 = laRecData[lnCntr,11]
        m.BLNORD3 = laRecData[lnCntr,12]
        m.BLNORD4 = laRecData[lnCntr,13]
        m.BLNORD5 = laRecData[lnCntr,14]
        m.BLNORD6 = laRecData[lnCntr,15]
        m.BLNORD7 = laRecData[lnCntr,16]
        m.BLNORD8 = laRecData[lnCntr,17]
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
        
        lnavl = lnAvl1 + lnAvl2 + lnAvl3 + lnAvl4 + lnAvl5 + lnAvl6 + lnAvl7 + lnAvl8
        laRecData[lnCntr,18] = lnavl
        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
        m.AvlTotSt = lnavl
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
        
        lnReqQnts = laRecData[lnCntr,10]+ laRecData[lnCntr,11]+ laRecData[lnCntr,12]+ laRecData[lnCntr,13]+ ;
                    laRecData[lnCntr,14]+ laRecData[lnCntr,15]+ laRecData[lnCntr,16]+ laRecData[lnCntr,17]                  
        
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]            
*!*          laRecData[lnCntr,24] = laRecData[lnCntr,24] + lnReqQnts
        laRecData[lnCntr,24] = lnReqQnts
        m.BlnTotOrd = laRecData[lnCntr,24]
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]        
        
        lnCheck = lnCheck + 1
      ENDIF
      lnCntr = lnCntr + 1
      
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
      SELECT(lcStyCpTmp)
      IF !llExist 
        APPEND BLANK 
      ENDIF
      GATHER MEMVAR MEMO
      *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
      
    ENDSCAN
  ENDSCAN
  
  lnCntr = lnCntr - 1  
  
  IF TYPE('loFormSet.llContinue') = 'U' 
    loFormSet.AddProperty('llContinue',.F.)
  ENDIF 
  
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start  ]
  IF TYPE('loFormSet.llCreatPO') = 'U' 
    loFormSet.AddProperty('llCreatPO',.F.)
  ELSE 
    loFormSet.llCreatPO = .F.
  ENDIF   
  
  
  *C201681,1 MMT 05/28/2015 Add Issue None button to issue component screen[T20150514.0012][Start]
  IF TYPE('loFormSet.lIssueNone') = 'U' 
    loFormSet.AddProperty('lIssueNone',.F.)
  ELSE 
    loFormSet.lIssueNone = .F.
  ENDIF 
  *C201681,1 MMT 05/28/2015 Add Issue None button to issue component screen[T20150514.0012][End]
  
  
  llIfIssue = .F.
  lnAllQtys = 0
  lnAllReqQtys = 0
  FOR lnA = 1 TO lnCntr
    lnAllQtys = lnAllQtys + laRecData[lnA,18]
    lnAllReqQtys = lnAllReqQtys + laRecData[lnA,24]
  ENDFOR  
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
  IF lnAllQtys > 0 OR lnAllReqQtys > 0
    llForm = .T.
    DO FORM (oAriaApplication.ScreenHome + 'POADJCOMP.SCX') WITH lcStyCpTmp, loFormSet
  ENDIF
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start  ] 
  
  IF !llForm   
    RETURN
  ENDIF 

  *C201681,1 MMT 05/28/2015 Add Issue None button to issue component screen[T20150514.0012][Start]
  IF loFormSet.lIssueNone AND llForm
    RETURN .T.
  ENDIF
  *C201681,1 MMT 05/28/2015 Add Issue None button to issue component screen[T20150514.0012][End]
        
  IF !loFormSet.llContinue AND llForm
    RETURN .F.
  ELSE 
    lcCuTb = ALIAS()
    SELECT(lcStyCpTmp)
    lcOrd = ORDER()
    SET ORDER TO 
    lnContr = 0
    SCAN FOR !EOF()
      lnContr = lnContr + 1
      IF lnContr <= lnCntr 
        lfUpdArray(lnContr) 
      ENDIF 
    ENDSCAN 
    SET ORDER TO &lcOrd
    SELECT(lcCuTb)
  ENDIF 
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
  
  lnAllQtys = 0
  lnAllReqQtys = 0
  FOR lnA = 1 TO lnCntr
    lnAllQtys = lnAllQtys + laRecData[lnA,18]
    lnAllReqQtys = lnAllReqQtys + laRecData[lnA,24]
  ENDFOR
  
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
  laRecData[1,46] = lnAllQtys
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
  
  laRecData[1,27] = lnAllReqQtys 
  laRecData[1,28] = lnCntr
  
  IF lnCheck > 0
    *B609478,8 TMI 04/03/2011 [Start] add the user ID to the start of the file ACTDATA.MEM to avoid the case that two users logged in at the same time creating POs
    *SAVE TO (oAriaApplication.DataDir+"ActData.mem") ALL LIKE laRecData
    SAVE TO (oAriaApplication.DataDir+oAriaApplication.User_id+"ActData.mem") ALL LIKE laRecData
    *B609478,8 TMI 04/03/2011 [End  ] 
  ENDIF 

*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]      
*!*    IF lnAllQtys > 0 
*!*      loformset.llIfAut = .T.
*!*      WAIT WINDOW NOWAIT 'Issuing the Available Quantities of Style Component ('+ SUBSTR(lcPoItem,1,12) +')..'
*!*      =lfvIssCstItm('','',loFormSet) && Issue the available Qtys
*!*      
*!*      lnSlct = SELECT(0)
*!*      SELECT STYDYE
*!*      =TABLEUPDATE(.T.,.T.)
*!*      SELECT(lnSlct)
*!*    ENDIF
*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

ENDIF
* End of lfMfPrRcData

*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!**************************************************************************
*! Name       : lfMfIssuData
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/06/2009
*! Purpose    : Issuing the available qtys and print the issue sheet
*! Tracking # : C201230
*!**************************************************************************
FUNCTION lfMfIssuData

PARAMETERS loFormSet, llIfAut 

IF TYPE('loFormSet.llIfAut') = 'U' 
  loFormSet.AddProperty('llIfAut',.F.)
ENDIF

*B609478,8 TMI 04/03/2011 [Start] add the user ID to the start of the file ACTDATA.MEM to avoid the case that two users logged in at the same time creating POs
*IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) AND FILE(oAriaApplication.DataDir+"ActData.mem")
IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) AND FILE(oAriaApplication.DataDir+oAriaApplication.User_id+"ActData.mem")
*B609478,8 TMI 04/03/2011 [End  ] 

  loFormSet.llIfAut = llIfAut

  *B609478,8 TMI 04/03/2011 [Start] add the user ID to the start of the file ACTDATA.MEM to avoid the case that two users logged in at the same time creating POs
  *RESTORE FROM oAriaApplication.DataDir+"ActData.mem" ADDITIVE
  RESTORE FROM oAriaApplication.DataDir+oAriaApplication.User_id+"ActData.mem" ADDITIVE
  *B609478,8 TMI 04/03/2011 [End  ] 
  
  IF laRecData[1,46] > 0 
    loFormSet.llIfAut = .T.
    WAIT WINDOW NOWAIT 'Issuing the Available Quantities of Style Component ('+ SUBSTR(laRecData[1,1],1,12) +')..'
    =lfvIssCstItm('','',loFormSet) && Issue the available Qtys
       
    lnSlct = SELECT(0)
    SELECT STYDYE
    LOCAL lnBuffering
    lnBuffering = CURSORGETPROP("Buffering",'STYDYE')
    =CURSORSETPROP("Buffering",3,'STYDYE')
    =TABLEUPDATE(.T.,.T.)    
    =CURSORSETPROP("Buffering",lnBuffering,'STYDYE')
    SELECT(lnSlct)
    
    IF TYPE('oPoStyRef.llCreatPO') = 'U' 
      oPoStyRef.AddProperty('llCreatPO',.T.)
    ELSE 
      oPoStyRef.llCreatPO = .T.
    ENDIF
    
    IF laRecData[1,27] = 0
    
      FOR lnM = 1 TO ALEN(laRecData,1)
        laRecData[lnM,31] = '' 
        laRecData[lnM,43] = {}
        laRecData[lnM,44] = ''           
      ENDFOR     
     
      lcIssTmp = gfTempName()
      =lfCpyArrToTmp(@laRecData,.F.)
      SELECT(lcIssTmp)
      LOCATE 
      SUM IssueTot TO lnTotAllIss
      LOCATE 
      IF lnTotAllIss > 0
        REPORT FORM oAriaApplication.ReportHome + 'POISSHT.FRX' PREVIEW 
      ENDIF    
    ENDIF 
  ELSE 
    IF laRecData[1,27] > 0
      IF TYPE('oPoStyRef.llCreatPO') = 'U' 
        oPoStyRef.AddProperty('llCreatPO',.T.)
      ELSE 
        oPoStyRef.llCreatPO = .T.
      ENDIF       
    ENDIF 
  ENDIF  
ENDIF 
* End of lfMfIssuData

*!**************************************************************************
*! Name       : lfUpdArray
*! Developer  : (HES)Hesham Elmasry
*! Date       : 04/07/2010
*! Purpose    : Preparing the Array holds the required data
*! Tracking # : C201230
*!**************************************************************************
FUNCTION lfUpdArray
PARAMETERS lnCntr

laRecData[lnCntr,2]  = Issue1
laRecData[lnCntr,3]  = Issue2
laRecData[lnCntr,4]  = Issue3
laRecData[lnCntr,5]  = Issue4
laRecData[lnCntr,6]  = Issue5
laRecData[lnCntr,7]  = Issue6
laRecData[lnCntr,8]  = Issue7
laRecData[lnCntr,9]  = Issue8
laRecData[lnCntr,18] = IssueTot

laRecData[lnCntr,10] = BlnOrd1
laRecData[lnCntr,11] = BlnOrd2
laRecData[lnCntr,12] = BlnOrd3
laRecData[lnCntr,13] = BlnOrd4
laRecData[lnCntr,14] = BlnOrd5
laRecData[lnCntr,15] = BlnOrd6
laRecData[lnCntr,16] = BlnOrd7
laRecData[lnCntr,17] = BlnOrd8
laRecData[lnCntr,24] = BlnTotOrd

* End of lfUpdArray
*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

*!**************************************************************************
*! Name       : lfMfUpdTemp
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/06/2009
*! Purpose    : Preparing the temp file used for the Automatic issue
*! Tracking # : C201141
*!**************************************************************************
FUNCTION lfMfUpdTemp
PARAMETERS loFormset

*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*  IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID)
IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) OR ;
   gfGetMemVar('M_ISSURECI',oAriaApplication.ActiveCompanyID)
*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

  SELECT (loParentForm.lcIssLtFile)
  LOCATE
  SCAN FOR cCatgTyp = 'S' AND !EOF()
    replace lSelect WITH .T.
  ENDSCAN 
ENDIF 
* End of lfMfUpdTemp
*!**************************************************************************
*! Name       : lfMfcsAutIss
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/06/2009
*! Purpose    : Making automatic issue
*! Tracking # : C201141
*!**************************************************************************
FUNCTION lfMfcsAutIss
PARAMETERS loFormset 

*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*  IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID)
IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) OR ;
   gfGetMemVar('M_ISSURECI',oAriaApplication.ActiveCompanyID)
*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

  =lfvOkIssLt(loFormSet)
  llCustReturn = .F. && returned in lfmfissltinit() from MFCSSH to prevent the screen from bieng appear
ENDIF 
* End of lfMfcsAutIss
*!**************************************************************************
*! Name       : lfCalCsShScr
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/06/2009
*! Purpose    : Calling the Cost Sheet screen for the a specific PO
*! Tracking # : C201141
*!**************************************************************************
FUNCTION lfCalCsShScr

IF gfGetMemVar('M_ISSURECI',oAriaApplication.ActiveCompanyID)
  
  SELECT(loFormset.lcPoshdr)
  lnIssuPoNo = IssuPoNo
  
  IF !EMPTY(lnIssuPoNo)

    *-- Call the Cost Sheet screen to let the user issue the bought required Qntys for the style component
    
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
    lcParameter = "'" + lnIssuPoNo + "'" + ',.F.' 
    IF TYPE('loFormset.llRecPO') = 'U' 
      loFormset.AddProperty('llRecPO',.T.)
    ELSE 
      loFormset.llRecPO = .T.
    ENDIF         
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
    
    lcScrName   = "AWRPOCSSH"
    lcSvProc = SET("Procedure")
    SET PROCEDURE TO (oAriaApplication.ApplicationHome+"MFCSSH.FXP")
    WAIT WINDOW NOWAIT 'Calling Cost Sheet Screen to Complete Issuing Process...' 
    oAriaApplication.DoProgram(lcScrName, lcParameter, .T., 'PO')
    
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
    IF TYPE('loFormset.llRecPO') = 'U' 
      loFormset.AddProperty('llRecPO',.F.)
    ELSE 
      loFormset.llRecPO = .F.
    ENDIF
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]      
    
    SET PROCEDURE TO &lcSvProc
  ENDIF
ENDIF
* End of lfCalCsShScr

*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!**************************************************************************
*! Name       : lfMFRISSUDAT
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/06/2009
*! Purpose    : Automatic issuing the remain pieced missed for the Parent PO
*! Tracking # : C201230
*!**************************************************************************
FUNCTION lfMFRISSUDAT
PARAMETERS loFormSet 

IF gfGetMemVar('M_ISSURECI',oAriaApplication.ActiveCompanyID) AND oPoRecRef.llRecPO

  IF TYPE('loFormSet.llIfAut') = 'U' 
    loFormSet.AddProperty('llIfAut',.T.)
  ELSE 
    loFormSet.llIfAut = .T.  
  ENDIF
  
  loFormset.ChangeMode("V")
  WAIT WINDOW NOWAIT 'Issuing the Whole Quantities Booked into the Linked PO..'
  =lfvIssCstItm('','',loFormSet) && Issue the Whole Qtys

  lnSlct = SELECT(0)
  SELECT STYDYE
  LOCAL lnBuffering
  lnBuffering = CURSORGETPROP("Buffering",'STYDYE')
  =CURSORSETPROP("Buffering",3,'STYDYE')
  =TABLEUPDATE(.T.,.T.)    
  =CURSORSETPROP("Buffering",lnBuffering,'STYDYE')
  SELECT(lnSlct) 

  lcIssTmp = gfTempName()
  DIMENSION laRecLIData[1,54]
  
  IF lfPrepIssArry(@laRecLIData)
  
    =lfCpyArrToTmp(@laRecLIData,.T.)
    loFormSet.activemode = "S" 
    SELECT(lcIssTmp)
    LOCATE 
    SUM IssueTot TO lnTotAllIss
    LOCATE 
    IF lnTotAllIss > 0
      REPORT FORM oAriaApplication.ReportHome + 'POISSHT.FRX' PREVIEW  
    ENDIF 
    
  ENDIF
   
ENDIF  

*!**************************************************************************
*! Name       : lfPrepIssArry
*! Developer  : (HES)Hesham Elmasry
*! Date       : 04/22/2010
*! Purpose    : Preparing the issue temp for the recieving step. 
*! Tracking # : C201230
*!**************************************************************************
FUNCTION lfPrepIssArry
PARAMETERS laRecLIData

lnMajorlen=LEN(gfItemMask("PM","","0001"))
STORE  0 TO lnClrLen ,lnClrPos
DECLARE laItemSeg[1]
PRIVATE lnCount 
=gfItemMask(@laItemSeg)
lnClrLen = 0
 FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
  EXIT
  ENDIF
ENDFOR
lnSclLen=19-(lnMajorlen+lnClrLen+1)  

*B609478,5 TMI 01/14/2011 [Start] add the user id to the name of these temp file
*!*	IF FILE(oAriaApplication.DataDir+"ActOrgData.mem") AND FILE(oAriaApplication.DataDir+"Budgtdata.mem")

*!*	  RESTORE FROM oAriaApplication.DataDir+"ActOrgData.mem" ADDITIVE
*!*	  ERASE oAriaApplication.DataDir+"ActOrgData.mem"

*!*	  RESTORE FROM oAriaApplication.DataDir+"Budgtdata.mem" ADDITIVE
*!*	  ERASE oAriaApplication.DataDir+"Budgtdata.mem"
IF FILE(oAriaApplication.DataDir+oAriaApplication.User_id+"ActOrgData.mem") AND FILE(oAriaApplication.DataDir+oAriaApplication.User_id+"Budgtdata.mem")

  RESTORE FROM oAriaApplication.DataDir+oAriaApplication.User_id+"ActOrgData.mem" ADDITIVE
  ERASE oAriaApplication.DataDir+oAriaApplication.User_id+"ActOrgData.mem"
  
  RESTORE FROM oAriaApplication.DataDir+oAriaApplication.User_id+"Budgtdata.mem" ADDITIVE
  ERASE oAriaApplication.DataDir+oAriaApplication.User_id+"Budgtdata.mem"
*B609478,5 TMI 01/14/2011 [End  ]   
ELSE 
  RETURN .F.
ENDIF

lnB = 1 
SELECT(loFormSet.lccTktBom)
SCAN FOR cCatgTyp = 'S'
  DIMENSION laRecLIData[lnB,54]
  lnOrgPos = ASCAN(laOrgData,Item)
  IF lnOrgPos > 0 
    lnOrgPos = ASUBSCRIPT(laOrgData,lnOrgPos,1)
  ENDIF 
  
  lnBudPos = ASCAN(laRcvIssData,Item)
  IF lnBudPos > 0 
    lnBudPos = ASUBSCRIPT(laRcvIssData,lnBudPos ,1)
  ENDIF
  
  *B609478,5 TMI 01/14/2011 [Start] if the either the arrays not filled then return back 
  IF lnBudPos = 0 OR lnOrgPos = 0 
    RETURN .F.
  ENDIF
  *B609478,5 TMI 01/14/2011 [End  ] 
  
  && Already Issued = Required for style - (recieved + To Issue + Balance to Come)
  lnTest = 0
  
  && 'Y' = Already exists in the new PO
  && 'N' = not exists in the new PO as all req qty already issued, and we just collect these data for the issue sheet already issued line.
  IF laRcvIssData[lnBudPos,34] = 'Y'  
    FOR lnX = 1 TO 8
      lnOrgRec = laOrgData[lnOrgPos,lnX] - ;
      (IIF(TYPE('laRcvIssData[lnBudPos,lnX+9]')  = 'N',laRcvIssData[lnBudPos,lnX+9] ,0)+ ;
      IIF(TYPE('laRcvIssData[lnBudPos,lnX+17]') = 'N',laRcvIssData[lnBudPos,lnX+17],0)+ ;
      IIF(TYPE('laRcvIssData[lnBudPos,lnX+25]') = 'N',laRcvIssData[lnBudPos,lnX+25],0))
      IF TYPE('laRcvIssData[lnBudPos,lnX+9]') = 'N'
        laRcvIssData[lnBudPos,lnX+9] = laRcvIssData[lnBudPos,lnX+9] + lnOrgRec 
      ENDIF 
    ENDFOR 
  ELSE
    FOR lnX = 1 TO 8
      IF TYPE('laRcvIssData[lnBudPos,lnX+9]') = 'N'
        laRcvIssData[lnBudPos,lnX+9] = laRcvIssData[lnBudPos,lnX+9]  
      ENDIF 
    ENDFOR     
  ENDIF  
  *NEW
  
  laRecLIData[lnB,1] = Item
  lcItem = Item
  laRecLIData[lnB,32] = gfCodDes(PADR(SUBSTR(Item,lnMajorlen+2,lnClrLen),6),'COLOR')  
  lcStyle = Style
  IF !USED('SCALE')
    =gfOpenTable(oAriaApplication.DataDir+'SCALE','SCALE','SH')
  ENDIF

  SELECT STYLE
  SET ORDER TO STYLE
  SEEK(lcItem)
  =gfSEEK('S'+STYLE.SCALE,'SCALE')        
  IF !USED('SCALEHD')
    =gfOpenTable(oariaapplication.datadir +'SCALEHD',oariaapplication.datadir +'EXTSCALE','SH')
  ENDIF 
  =gfSEEK(LEFT(STYLE.SCALE,2),'SCALEHD')
  laRecLIData[lnB,45] = SCALE.CNT
  IF SCALEHD.Nnoofdim = 2
    laRecLIData[lnB,34] = SCALE.Cdim1
  ELSE 
    laRecLIData[lnB,34] = ''
  ENDIF 
       
  FOR lny = 1 TO SCALE.CNT
    lcy = ALLTRIM(STR(lnY))
    laRecLIData[lnB,34+lny] = ALLTRIM(Scale.SZ&lcY)     
  ENDFOR 

  lnL = SCALE.CNT+1
  FOR lnN = lnL TO 8
    lcN = ALLTRIM(STR(lnN))
    laRecLIData[lnB,34+lnN] = ''
  ENDFOR
  
  SELECT STYLE
  SEEK(laRecLIData[lnB,1])
  laRecLIData[lnB,33] = DESC
  SELECT(loFormSet.lccTktBom)
  laRecLIData[lnB,31] = CutTkt
  laRecLIData[lnB,22] = ''
  laRecLIData[lnB,43] = {}
  laRecLIData[lnB,44] = ''
    
  FOR lnO = 1 TO 8
    lcO = ALLTRIM(STR(lnO))
    laRecLIData[lnB,9+lnO]  = IIF(TYPE('laRcvIssData[lnBudPos,lnO+25]') = 'N',laRcvIssData[lnBudPos,lnO+25],0)
    && Preparing already Issued 
    laRecLIData[lnB,46+lnO] = IIF(TYPE('laRcvIssData[lnBudPos,lnO+9]') =  'N', laRcvIssData[lnBudPos,lnO+9],0)    
    laRecLIData[lnB,1+lnO]  = IIF(TYPE('laRcvIssData[lnBudPos,lnO+17]') = 'N',laRcvIssData[lnBudPos,lnO+17],0)
  ENDFOR 
  
  lnB = lnB + 1
ENDSCAN

RETURN .T.

* End of lfPrepIssArry
*!*  *!**************************************************************************
*!*  *! Name       : lfPORUPDMEM
*!*  *! Developer  : (HES)Hesham Elmasry
*!*  *! Date       : 04/21/2010
*!*  *! Purpose    : Update the memo field cCstSht_id with the right value 
*!*  *! Tracking # : 
*!*  *!**************************************************************************
*!*  FUNCTION lfPORUPDMEM

*!*  m.cCstSht_id = BOM.cCstSht_id
*!*  lnRecDetNo = 0
*!*  * End of lfPORUPDMEM()
*!**************************************************************************
*! Name       : lfPreIsShDat
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/10/2010
*! Purpose    : Preparing the Recieving Issue sheet data 
*! Tracking # : C201230
*!**************************************************************************
FUNCTION lfPreIsShDat
PARAMETERS loFormSet

*B609478,5 TMI 01/14/2011 [Start] add oAriaApplication.User_id to the name of the temp files
*IF gfGetMemVar('M_ISSURECI',oAriaApplication.ActiveCompanyID) AND !FILE(oAriaApplication.DataDir+"BudgtData.mem")
IF gfGetMemVar('M_ISSURECI',oAriaApplication.ActiveCompanyID) AND !FILE(oAriaApplication.DataDir+oAriaApplication.User_id+"BudgtData.mem")
  *B609478,5 TMI 01/14/2011 [End  ] 


  SELECT (loFormSet.lcPoshdr)
  lcParPo = IssuPoNo

  lcSqlStatment = "Select * from POSLN where PO = '"+lcParPo+"'"
  lcCursor = gfTempName()
  lnConnectionHandlar1 = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcCursor,'POSLN',;
                  oAriaApplication.ActiveCompanyConStr,3,.F.,SET("Datasession"))                                
                  
  lcSqlStatment = "Select * from BOMLINE where cTktNo = '"+lcParPo+"'"
  lcBomLne = gfTempName()
  lnConnectionHandlar2 = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,lcBomLne,'BOMLINE',;
                         oAriaApplication.ActiveCompanyConStr,3,.F.,SET("Datasession")) 
                                    
  IF lnConnectionHandlar1 = 1 AND lnConnectionHandlar2 = 1 
    lnDim = 1
    *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
    DIMENSION laRcvIssData[1, 34]
    *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
    SELECT (lcCursor)
    SCAN FOR TranCd = '1'
      lcStyle  = STYLE
      lnUntQty = 0
      
      SELECT (lcBomLne)
      *B609478,1 TMI 12/11/2010 [Start] use the scan as the BOMLINE may contains more than one line in the component
      *LOCATE FOR cCatgTyp = 'S' AND STYLE = lcStyle
      *IF FOUND()
      SCAN FOR cCatgTyp = 'S' AND STYLE = &lcCursor..STYLE
        *B609478,1 TMI 12/11/2010 [End  ] 
        lcItem   = ITEM
        lnUntQty = UnitQty
        *B609478,1 TMI 12/11/2010 [Start] comment this      
        *ELSE 
        *  RETURN 
        *ENDIF      
        *B609478,1 TMI 12/11/2010 [End  ] 
      
        *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
*       DIMENSION laRcvIssData[1,34]
        *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
        SELECT (loFormSet.lcPosLn)
        lnRecNomb = RECNO()
    
        LOCATE FOR TranCd = '1' AND STYLE = lcItem
        IF FOUND()
          DIMENSION laRcvIssData[lnDim,34]
          lnRecNo = RECNO()
          lcStyle = style
          laRcvIssData[lnDim,1] = lcStyle 
          FOR lnB = 1 TO 8
            lcB = ALLTRIM(STR(lnB))
            laRcvIssData[lnDim,1+lnB] = Qty&lcB
          ENDFOR 
          SUM Qty1,Qty2,Qty3,Qty4,Qty5,Qty6,Qty7,Qty8 FOR style = lcStyle AND trancd = '2' TO laRcvIssData[lnDim,10] ,;
             laRcvIssData[lnDim,11],laRcvIssData[lnDim,12],laRcvIssData[lnDim,13],laRcvIssData[lnDim,14],laRcvIssData[lnDim,15] ,;
             laRcvIssData[lnDim,16],laRcvIssData[lnDim,17]
             FOR lnC = 1 TO 8
              lcC = ALLTRIM(STR(lnC))
              laRcvIssData[lnDim,lnC+17] = laRcvIssData[lnDim,lnC+1] - laRcvIssData[lnDim,lnC+9]
              laRcvIssData[lnDim,lnC+25] = 0
            ENDFOR 
            laRcvIssData[lnDim,34] = 'Y'
            GOTO lnRecNo 
        ELSE    
          GOTO lnRecNomb                        
          DIMENSION laRcvIssData[lnDim,34]
          SELECT (lcCursor)
          FOR lnC = 1 TO 8
            lcC = ALLTRIM(STR(lnC))
            laRcvIssData[lnDim,1] = lcItem
            laRcvIssData[lnDim,1+lnC]  = 0
            laRcvIssData[lnDim,9+lnC]  = Qty&lcC*lnUntQty
            laRcvIssData[lnDim,lnC+17] = 0
            laRcvIssData[lnDim,lnC+25] = 0
          ENDFOR 
          laRcvIssData[lnDim,34] = 'N'
        ENDIF   
        lnDim = lnDim + 1
      
      ENDSCAN 
      
      *B609478,1 TMI 12/11/2010 [Start] close the above added scan
    ENDSCAN
    *B609478,1 TMI 12/11/2010 [End  ] 
    
  ENDIF   
  
  *B609478,5 TMI 01/14/2011 [Start] add oAriaApplication.User_id to the name of the temp file
  *SAVE TO (oAriaApplication.DataDir+"BudgtData.mem") ALL LIKE laRcvIssData    
  SAVE TO (oAriaApplication.DataDir+oAriaApplication.User_id+"BudgtData.mem") ALL LIKE laRcvIssData
  *B609478,5 TMI 01/14/2011 [End  ] 
  SELECT (loFormSet.lcPosLn)
ENDIF
* End of lfPreIsShDat()
*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

*!**************************************************************************
*! Name       : lfCrPOndCoSh
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/06/2009
*! Purpose    : Create PO and Cost Sheet for the required Qtys of the style Component
*! Tracking # : C201141
*!**************************************************************************
FUNCTION lfCrPOndCoSh


IF TYPE('loFormSet.llCreatPO') = 'U' 
  loFormSet.AddProperty('llCreatPO',.F.)
ENDIF  

*B609478,8 TMI 04/03/2011 [Start] add the user ID to the start of the file ACTDATA.MEM to avoid the case that two users logged in at the same time creating POs
*IF FILE(oAriaApplication.DataDir+"ActData.mem") AND gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) AND loformset.llCreatPO
  *RESTORE FROM oAriaApplication.DataDir+"ActData.mem" ADDITIVE 
  *ERASE oAriaApplication.DataDir+"ActData.mem"
IF FILE(oAriaApplication.DataDir+oAriaApplication.User_id+"ActData.mem") AND gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) AND loformset.llCreatPO
  RESTORE FROM oAriaApplication.DataDir+oAriaApplication.User_id+"ActData.mem" ADDITIVE 
  ERASE oAriaApplication.DataDir+oAriaApplication.User_id+"ActData.mem"
  *B609478,8 TMI 04/03/2011 [End  ] 
  
  *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
  IF !USED('Style_A')
    =gfOpenTable('Style','STYLE','SH','Style_A')
  ENDIF
  *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]

  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*    IF TYPE('laRecData[1,27]') = 'N' AND laRecData[1,27] > 0 AND !EMPTY(laRecData[1,22])
  IF TYPE('laRecData[1,27]') = 'N' AND laRecData[1,27] > 0 
  *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
  
    FOR lnX = 1 TO laRecData[1,28]
      IF laRecData[lnX,10] > 0 OR laRecData[lnX,11] > 0 OR laRecData[lnX,12] > 0 OR laRecData[lnX,13] > 0 ;
                               OR laRecData[lnX,14] > 0 OR laRecData[lnX,15] > 0 OR laRecData[lnX,16] > 0 ;
                               OR laRecData[lnX,17] > 0 
        && Making an automatic PO for this component with the Rest Quantity
        lnTotQuanty = laRecData[lnX,24] 
        loformset.cWorkOrderType = 'P'
        loformset.lcInvType = '0001'
        loformset.activemode = 'A'
        loformset.ariaForm1.mainworkorder.cworkordtyp = 'PP'
        loformset.ariaForm1.mainworkorder.lgenornum = .F.
        
        SELECT(loformset.ariaForm1.mainworkorder.cposln)
        LOCATE 
        SCATTER MEMVAR MEMO
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
        laRecData[lnX,31] = PO
        
        lnMajorlen = LEN(gfItemMask("PM","","0001"))
        SELECT BOM
        gfSeek('0001'+SUBSTR(laRecData[lnX,1],1,lnMajorlen))
        *B611041,1 MMT 08/24/2015 Incorrect style cost Sheet is selected in style comp. custom[T20150820.0001][Start]
        IF !USED('BOMHEADR_AB')
          =gfOpenTable('BOMHEADR','BOMHEADR','SH','BOMHEADR_AB')
        ENDIF
        SELECT 'BOMHEADR_AB'
        =gfSeek('0001'+SUBSTR(laRecData[lnX,1],1,lnMajorlen))
        LOCATE REST WHILE CINVTYPE+CITMMAJOR+CCSTSHTTYP+CCSTSHT_ID ='0001'+SUBSTR(laRecData[lnX,1],1,lnMajorlen) FOR LDEFCSTSHT                    
        IF FOUND()
        *B611041,1 MMT 08/24/2015 Incorrect style cost Sheet is selected in style comp. custom[T20150820.0001][End]
        laRecData[lnX,20] = Ccstsht_id
        *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
        *B611041,1 MMT 08/24/2015 Incorrect style cost Sheet is selected in style comp. custom[T20150820.0001][Start]
        ELSE
          SELECT 'BOMHEADR_AB'
          =gfSeek('0001'+SUBSTR(laRecData[lnX,1],1,lnMajorlen))
          laRecData[lnX,20] = Ccstsht_id
        ENDIF
        *B611041,1 MMT 08/24/2015 Incorrect style cost Sheet is selected in style comp. custom[T20150820.0001][END]
        SELECT(loformset.ariaForm1.mainworkorder.cpoline) && Preparing the POSLN temp
        APPEND BLANK
        GATHER MEMVAR MEMO
        REPLACE Ccstsht_id WITH laRecData[lnX,20] ;
                Gros_price WITH laRecData[lnX,23] ;
                Nfcost1    WITH laRecData[lnX,23] ;
                Nicost1    WITH laRecData[lnX,23] ;
                Qty1       WITH laRecData[lnX,10] ;
                Qty2       WITH laRecData[lnX,11] ;
                Qty3       WITH laRecData[lnX,12] ;
                Qty4       WITH laRecData[lnX,13] ;
                Qty5       WITH laRecData[lnX,14] ;
                Qty6       WITH laRecData[lnX,15] ;
                Qty7       WITH laRecData[lnX,16] ;
                Qty8       WITH laRecData[lnX,17] ;
                Style      WITH laRecData[lnX,1]  ;
                CwareCode  WITH laRecData[lnX,21] ;
                Vendor     WITH laRecData[lnX,22] ;
                Scale      WITH laRecData[lnX,30] ;
                Totqty     WITH lnTotQuanty       ;
                LineNo     WITH lnX
                
       *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
       =gfSeek(SUBSTR(laRecData[lnX,1] ,1,19),'Style_A')
       REPLACE CVENSTY WITH Style_A.cVenSty,;
           Scale WITH Style_A.Scale
       *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
                
      ENDIF  
    ENDFOR  
    
    SELECT(loformset.ariaForm1.mainworkorder.cposhdr)   && Preparing the POSHDR temp
    
    SCATTER MEMVAR MEMO
    APPEND BLANK 
    GATHER MEMVAR MEMO
    
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
    laRecData[1,19] = PO
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
    
    REPLACE Nfcost1   WITH laRecData[1,23]*laRecData[1,27];
            Nicost1   WITH laRecData[1,23]*laRecData[1,27];
            Style     WITH laRecData[1,1]                 ;
            Open      WITH laRecData[1,27]                ;
            NStyorder WITH laRecData[1,27]                ;
            Pototal   WITH laRecData[1,23]*laRecData[1,27];
            PO        WITH ''                             ;&& To be ready for the new Po #
            CwareCode WITH laRecData[1,21]                ;
            Vendor    WITH laRecData[1,22]                ;
            ISSUPONO  WITH laRecData[1,19]                ;
            Available with oAriaApplication.SystemDate + 90
    
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]            
*!*      SELECT(loformset.ariaForm1.mainworkorder.cMPoshdr) && preparing the temp to get data from cposhdr to be updated in SQL
*!*      APPEND BLANK

*!*      *-- Recalling the MSAVEPO function for the style component(s)
*!*      WAIT WINDOW NOWAIT 'Creating PO for the Unavailable Quantities of the Style Component(s)...' 
*!*      loformset.ariaForm1.mainworkorder.msavepo(.F.,.T.) && Second parameter .T. to preventing the appearing of PO message
*!*        
*!*      SELECT(loformset.ariaForm1.mainworkorder.cpoline)
*!*      GO BOTTOM 
*!*      lcNewPo = PO
*!*      lcCompStyle = SUBSTR(STYLE,1,18)
*!*      MESSAGEBOX('PO No. ('+ lcNewPo +') for Style Compoment ('+ lcCompStyle +') has been Created.',576)

    lcNewVendor = ALLTRIM(laRecData[1,22])    
    *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
    IF !USED('Apvendor_A')
      =gfOpenTable('Apvendor','VENCODE','SH','Apvendor_A')
    ENDIF
    =gfSeek(SUBSTR(laRecData[1,22],1,8),'Apvendor_A')
    SELECT((loformset.ariaForm1.mainworkorder.cposhdr))
    REPLACE Contact WITH  Apvendor_A.cvencont,;
        Phone   WITH  Apvendor_A.cphoneno,;
        cTermCode WITH Apvendor_A.cTermCode  IN (loformset.ariaForm1.mainworkorder.cposhdr)
    *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]      

    
    lcActivCompany = oAriaApplication.ActiveCompanyId
*B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
*!*	    lcNewPo = gfSequence("PO", lcActivCompany)
*!*	    IF !USED('SEQUENCE')
*!*	      =gfOpenTable(oAriaApplication.DataDir+'SEQUENCE','CSEQ_TYPE','SH')
*!*	    ENDIF 
*!*	    SELECT SEQUENCE
*!*	    gfSeek('PO        ')
*!*	    IF FOUND()
*!*	      lnNSeq = Nseq_no - 1
*!*	      REPLACE Nseq_no WITH lnNSeq
*!*	    ENDIF 
    llOpenedSeq = .F.
    IF !USED('SEQUENCE')
      =gfOpenTable(oAriaApplication.DataDir+'SEQUENCE','CSEQ_TYPE','SH')
      llOpenedSeq = .T.
    ENDIF 
    =gfSeek('PO        ','SEQUENCE','CSEQ_TYPE')
    lcNewPo  = PADL(SEQUENCE.Nseq_no ,6,"0")
    IF llOpenedSeq 
      =gfCloseTable('SEQUENCE')
    ENDIF 
    *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
    *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][Start]
    lcComponentPONum = lcNewPo 
    *C201827,1 MMT 05/30/2016 P20160510.0001 - Issue#4 - Point#9 Add PO# to the component PO notes[P20160510.0001][End]

    laRecData[1,44] = lcNewPo
    lcCompStyle = SUBSTR(laRecData[1,1],1,18)
    IF !EMPTY(lcNewVendor)
      lnRes = MESSAGEBOX('PO No. ('+ lcNewPo +') is being raised for Compoment ('+ lcCompStyle +') for Vendor ['+ lcNewVendor +']'+CHR(13)+' Do you wish to change this vendor?',548)
      IF lnRes = 6
        *B609478,5 TMI 01/16/2011 [Start] be sure to select a vendor
        laRecData[1,22] = ''
        DO WHILE EMPTY(laRecData[1,22])
          *B609478,5 TMI 01/16/2011 [End  ] 
          laRecData[1,22] = lfChngVndr()
          *B609478,5 TMI 01/16/2011 [Start] 
        ENDDO
        *B609478,5 TMI 01/16/2011 [End  ] 
		*B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
	    IF !USED('Apvendor_A')
	      =gfOpenTable('Apvendor','VENCODE','SH','Apvendor_A')
	    ENDIF
		=gfSeek(SUBSTR(laRecData[1,22],1,8),'Apvendor_A')	    
		*B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]   
        SELECT(loformset.ariaForm1.mainworkorder.cposhdr)
        REPLACE Vendor WITH laRecData[1,22]
		*B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]      
    	REPLACE Contact WITH  Apvendor_A.cvencont,;
          	  Phone   WITH  Apvendor_A.cphoneno,;
		      cTermCode WITH Apvendor_A.cTermCode  IN (loformset.ariaForm1.mainworkorder.cposhdr)
	    *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]      
      ENDIF 
    ELSE 
      MESSAGEBOX("There is no Default Vendor for this Style Component, Press OK to choose one.",528)
      *B609478,5 TMI 01/16/2011 [Start] be sure to select a vendor
      laRecData[1,22] = ''
      DO WHILE EMPTY(laRecData[1,22])
        *B609478,5 TMI 01/16/2011 [End  ] 
        laRecData[1,22] = lfChngVndr()
        *B609478,5 TMI 01/16/2011 [Start] 
      ENDDO
      *B609478,5 TMI 01/16/2011 [End  ] 
      lnRes = MESSAGEBOX('PO No. ('+ lcNewPo +') is being raised for Compoment ('+ lcCompStyle +') for Vendor ['+ laRecData[1,22] +']'+CHR(13)+' Do you wish to change this vendor?',548)
      IF lnRes = 6
        laRecData[1,22] = lfChngVndr()
        SELECT(loformset.ariaForm1.mainworkorder.cposhdr)
        REPLACE Vendor WITH laRecData[1,22]
      ELSE 
        SELECT(loformset.ariaForm1.mainworkorder.cposhdr)
        REPLACE Vendor WITH laRecData[1,22]             
      ENDIF        
    ENDIF 
    
    *B609478,3 TMI 12/16/2010 [Start] Update all lines in the POSLN with the new vendor
    SELECT(loformset.ariaForm1.mainworkorder.cpoline)
    REPLACE VENDOR WITH laRecData[1,22] ALL
    *B609478,3 TMI 12/16/2010 [End  ] 

    SELECT(loformset.ariaForm1.mainworkorder.cMPoshdr) && preparing the temp to get data from cposhdr to be updated in SQL
    APPEND BLANK

    *-- Recalling the MSAVEPO function for the style component(s)
    WAIT WINDOW NOWAIT 'Creating PO No. ('+ lcNewPo +') for the Unavailable Quantities of the Style Component(s)...' 
    loformset.ariaForm1.mainworkorder.msavepo(.F.,.T.) && Second parameter .T. to preventing the appearing of PO message    
        
    FOR lnH = 1 TO ALEN(laRecData,1)
      laRecData[lnH,43] = oAriaApplication.SystemDate + 90
      laRecData[lnH,44] = lcNewPo
      laRecData[lnH,22] = laRecData[1,22]
    ENDFOR 
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
    
    loformset.laNewMpos[1] = EVALUATE(loformset.dataenvironment.initialselectedalias+'.PO')
    IF EVAL(loformset.ariaForm1.mainworkorder.cPosHdr+'.Status')='H' AND loformset.ariaForm1.mainworkorder.cGenPoCstSht $ 'AIT'  
      loformset.ariaForm1.mainworkorder.llGenCostSht = .T.
      WAIT WINDOW NOWAIT 'Generating Cost Sheet for the New PO...'
      loformset.ariaForm1.mainworkorder.mGenCostSheet()
      loformset.ariaForm1.mainworkorder.llGenCostSht = .F.
      loformset.activemode = 'S'
    ENDIF

    *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
    IF gfModalGen('QRM00349B00006','DIALOG')=1
      loformset.callnotepad() 
      IF !EMPTY(loformset.formhastoolbar)
        oAriaApplication.oToolBar.Init(loformset)
      ENDIF
    ENDIF 
    loformset.mPrintTrx()
    SET DEFAULT TO (oAriaApplication.DefaultPath)
	*B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
    
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
    lcIssTmp = gfTempName()
    =lfCpyArrToTmp(@laRecData,.F.)
    loformset.activemode = 'S'
    SELECT(lcIssTmp)
    LOCATE 
    SUM IssueTot TO lnTotAllIss
    LOCATE 
    IF lnTotAllIss > 0
      REPORT FORM oAriaApplication.ReportHome + 'POISSHT.FRX' PREVIEW 
    ENDIF
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
  ELSE 
    
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*      IF EMPTY(laRecData[1,22])
*!*        MESSAGEBOX("There is no Default Vendor for this Style Component, So we can't create a PO with Empty Vendor.",528)
*!*        loformset.activemode = 'S'
*!*      ENDIF
    *C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]
  ENDIF 
ENDIF
* End of lfCrPOndCoSh

*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!**************************************************************************
*! Name       : lfCpyArrToTmp
*! Developer  : (HES)Hesham Elmasry
*! Date       : 04/14/2010
*! Purpose    : Create a temp file from the used Array to be used in the Issue Sheet
*! Tracking # : C201230
*!**************************************************************************
FUNCTION lfCpyArrToTmp
PARAMETERS laRecData, llRec

=lfCrIsShtTmp()
lcStyPo = ''
FOR lnT = 1 TO ALEN(laRecData,1)
  *B611112,1 MMT 02/10/2016 Fix error while using PO receiving screen[T20160204.0003][Start]
  IF TYPE('laRecData[lnT,1]') <> 'C'
    LOOP
  ENDIF
  *B611112,1 MMT 02/10/2016 Fix error while using PO receiving screen[T20160204.0003][End]
  SELECT(lcIssTmp)
  APPEND BLANK
  *B609478,8 TMI 04/03/2011 [Start] update the field "BlnTotOrd" from the BlnOrd1,...,BlnOrd8 not from the array
  *REPLACE StyComp   WITH laRecData[lnT,1]  ;
          STYLEPO   WITH IIF(EMPTY(laRecData[lnT,31]),lcStyPo,laRecData[lnT,31]) ;
          ClrDesc   WITH laRecData[lnT,32] ;
          CpShDsc   WITH laRecData[lnT,33] ;
          Fit       WITH laRecData[lnT,34] ;
          Vendor    WITH laRecData[lnT,22] ;
          Avail     WITH laRecData[lnT,43] ;
          NPO       WITH laRecData[lnT,44] ;
          SclCnt    WITH laRecData[lnT,45] ;
          SZ1       WITH laRecData[lnT,35] ;
          SZ2       WITH laRecData[lnT,36] ;
          SZ3       WITH laRecData[lnT,37] ;
          SZ4       WITH laRecData[lnT,38] ;
          SZ5       WITH laRecData[lnT,39] ;
          SZ6       WITH laRecData[lnT,40] ;
          SZ7       WITH laRecData[lnT,41] ;
          SZ8       WITH laRecData[lnT,42] ;
          BlnOrd1   WITH IIF(!EMPTY(laRecData[lnT,10]),laRecData[lnT,10],0) ;
          BlnOrd2   WITH IIF(!EMPTY(laRecData[lnT,11]),laRecData[lnT,11],0) ;
          BlnOrd3   WITH IIF(!EMPTY(laRecData[lnT,12]),laRecData[lnT,12],0) ;
          BlnOrd4   WITH IIF(!EMPTY(laRecData[lnT,13]),laRecData[lnT,13],0) ;
          BlnOrd5   WITH IIF(!EMPTY(laRecData[lnT,14]),laRecData[lnT,14],0) ;
          BlnOrd6   WITH IIF(!EMPTY(laRecData[lnT,15]),laRecData[lnT,15],0) ;
          BlnOrd7   WITH IIF(!EMPTY(laRecData[lnT,16]),laRecData[lnT,16],0) ;
          BlnOrd8   WITH IIF(!EMPTY(laRecData[lnT,17]),laRecData[lnT,17],0) ;
          BlnTotOrd WITH laRecData[lnT,10]+laRecData[lnT,11]+laRecData[lnT,12]+laRecData[lnT,13]+laRecData[lnT,14]+laRecData[lnT,15]+laRecData[lnT,16]+laRecData[lnT,17];
          Issue1    WITH IIF(!EMPTY(laRecData[lnT,2]),laRecData[lnT,2],0) ;
          Issue2    WITH IIF(!EMPTY(laRecData[lnT,3]),laRecData[lnT,3],0) ;
          Issue3    WITH IIF(!EMPTY(laRecData[lnT,4]),laRecData[lnT,4],0) ;
          Issue4    WITH IIF(!EMPTY(laRecData[lnT,5]),laRecData[lnT,5],0) ;
          Issue5    WITH IIF(!EMPTY(laRecData[lnT,6]),laRecData[lnT,6],0) ;
          Issue6    WITH IIF(!EMPTY(laRecData[lnT,7]),laRecData[lnT,7],0) ;
          Issue7    WITH IIF(!EMPTY(laRecData[lnT,8]),laRecData[lnT,8],0) ;
          Issue8    WITH IIF(!EMPTY(laRecData[lnT,9]),laRecData[lnT,9],0) ;
          IssueTot  WITH laRecData[lnT,2]+laRecData[lnT,3]+laRecData[lnT,4]+laRecData[lnT,5]+laRecData[lnT,6]+laRecData[lnT,7]+laRecData[lnT,8]+laRecData[lnT,9] 
  REPLACE StyComp   WITH laRecData[lnT,1]  ;
          STYLEPO   WITH IIF(EMPTY(laRecData[lnT,31]),lcStyPo,laRecData[lnT,31]) ;
          ClrDesc   WITH laRecData[lnT,32] ;
          CpShDsc   WITH laRecData[lnT,33] ;
          Fit       WITH laRecData[lnT,34] ;
          Vendor    WITH laRecData[lnT,22] ;
          Avail     WITH laRecData[lnT,43] ;
          NPO       WITH laRecData[lnT,44] ;
          SclCnt    WITH laRecData[lnT,45] ;
          SZ1       WITH laRecData[lnT,35] ;
          SZ2       WITH laRecData[lnT,36] ;
          SZ3       WITH laRecData[lnT,37] ;
          SZ4       WITH laRecData[lnT,38] ;
          SZ5       WITH laRecData[lnT,39] ;
          SZ6       WITH laRecData[lnT,40] ;
          SZ7       WITH laRecData[lnT,41] ;
          SZ8       WITH laRecData[lnT,42] ;
          BlnOrd1   WITH IIF(!EMPTY(laRecData[lnT,10]),laRecData[lnT,10],0) ;
          BlnOrd2   WITH IIF(!EMPTY(laRecData[lnT,11]),laRecData[lnT,11],0) ;
          BlnOrd3   WITH IIF(!EMPTY(laRecData[lnT,12]),laRecData[lnT,12],0) ;
          BlnOrd4   WITH IIF(!EMPTY(laRecData[lnT,13]),laRecData[lnT,13],0) ;
          BlnOrd5   WITH IIF(!EMPTY(laRecData[lnT,14]),laRecData[lnT,14],0) ;
          BlnOrd6   WITH IIF(!EMPTY(laRecData[lnT,15]),laRecData[lnT,15],0) ;
          BlnOrd7   WITH IIF(!EMPTY(laRecData[lnT,16]),laRecData[lnT,16],0) ;
          BlnOrd8   WITH IIF(!EMPTY(laRecData[lnT,17]),laRecData[lnT,17],0) ;
          BlnTotOrd WITH BlnOrd1+BlnOrd2+BlnOrd3+BlnOrd4+BlnOrd5+BlnOrd6+BlnOrd7+BlnOrd8;
          Issue1    WITH IIF(!EMPTY(laRecData[lnT,2]),laRecData[lnT,2],0) ;
          Issue2    WITH IIF(!EMPTY(laRecData[lnT,3]),laRecData[lnT,3],0) ;
          Issue3    WITH IIF(!EMPTY(laRecData[lnT,4]),laRecData[lnT,4],0) ;
          Issue4    WITH IIF(!EMPTY(laRecData[lnT,5]),laRecData[lnT,5],0) ;
          Issue5    WITH IIF(!EMPTY(laRecData[lnT,6]),laRecData[lnT,6],0) ;
          Issue6    WITH IIF(!EMPTY(laRecData[lnT,7]),laRecData[lnT,7],0) ;
          Issue7    WITH IIF(!EMPTY(laRecData[lnT,8]),laRecData[lnT,8],0) ;
          Issue8    WITH IIF(!EMPTY(laRecData[lnT,9]),laRecData[lnT,9],0) ;
          IssueTot  WITH Issue1+Issue2+Issue3+Issue4+Issue5+Issue6+Issue7+Issue8
  *B609478,8 TMI 04/03/2011 [End  ] 
  IF llRec
    *B609478,8 TMI 04/03/2011 [Start] update the TotOldIss from the fields not from the array
    *REPLACE OldIss1   WITH IIF(!EMPTY(laRecData[lnT,47]),laRecData[lnT,47],0) ;
            OldIss2   WITH IIF(!EMPTY(laRecData[lnT,48]),laRecData[lnT,48],0) ;
            OldIss3   WITH IIF(!EMPTY(laRecData[lnT,49]),laRecData[lnT,49],0) ;
            OldIss4   WITH IIF(!EMPTY(laRecData[lnT,50]),laRecData[lnT,50],0) ;
            OldIss5   WITH IIF(!EMPTY(laRecData[lnT,51]),laRecData[lnT,51],0) ;
            OldIss6   WITH IIF(!EMPTY(laRecData[lnT,52]),laRecData[lnT,52],0) ;
            OldIss7   WITH IIF(!EMPTY(laRecData[lnT,53]),laRecData[lnT,53],0) ;
            OldIss8   WITH IIF(!EMPTY(laRecData[lnT,54]),laRecData[lnT,54],0) ;
            TotOldIss WITH laRecData[lnT,47]+laRecData[lnT,48]+laRecData[lnT,49]+laRecData[lnT,50]+laRecData[lnT,51]+laRecData[lnT,52]+laRecData[lnT,53]+laRecData[lnT,54]
    REPLACE OldIss1   WITH IIF(!EMPTY(laRecData[lnT,47]),laRecData[lnT,47],0) ;
            OldIss2   WITH IIF(!EMPTY(laRecData[lnT,48]),laRecData[lnT,48],0) ;
            OldIss3   WITH IIF(!EMPTY(laRecData[lnT,49]),laRecData[lnT,49],0) ;
            OldIss4   WITH IIF(!EMPTY(laRecData[lnT,50]),laRecData[lnT,50],0) ;
            OldIss5   WITH IIF(!EMPTY(laRecData[lnT,51]),laRecData[lnT,51],0) ;
            OldIss6   WITH IIF(!EMPTY(laRecData[lnT,52]),laRecData[lnT,52],0) ;
            OldIss7   WITH IIF(!EMPTY(laRecData[lnT,53]),laRecData[lnT,53],0) ;
            OldIss8   WITH IIF(!EMPTY(laRecData[lnT,54]),laRecData[lnT,54],0) ;
            TotOldIss WITH OldIss1+OldIss2+OldIss3+OldIss4+OldIss5+OldIss6+OldIss7+OldIss8
    *B609478,8 TMI 04/03/2011 [End  ]    
  ENDIF 
ENDFOR 
* End OF lfCpyArrToTmp

*!**************************************************************************
*! Name       : lfChngVndr
*! Developer  : (HES)Hesham Elmasry
*! Date       : 04/13/2010
*! Purpose    : Browse for a vendor to be used for the new raised PO
*! Tracking # : C201230
*!**************************************************************************
FUNCTION lfChngVndr

IF !USED('APVENDOR')
  =gfOpenTable(oAriaApplication.DataDir+'APVENDOR','VENCODE','SH')
  gfSeek('')
ENDIF 

SELECT APVENDOR
LOCATE 

DIMENSION laTemp[1]
laTemp     = ''
lcFile_Ttl = "Vendors"  
lcBrFields = "cVendCode :R :H= 'Vendor',    " +;
             "cVenComp  :R :H= 'Name',      " +;
             "cVenOurAc :R :H= 'Our Account'"
lcSavBrFld = lcBrFields
lcSavTitle = lcFile_Ttl             
 =ARIABROW(' ', 'Vendors', .F., .F., .F., .F., .F., .T., ;
                'CVENDCODE,CPHONENO,CVENCOMP', 'laTemp', .F., .F., .F., ;
                .F., .F., .F., .F., .F., ;
                .F., .F.)
lcBrFields = lcSavBrFld
lcFile_Ttl = lcSavTitle
RETURN laTemp[1]
* End of lfChngVndr()
*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

*!**************************************************************************
*! Name       : lfPrcPrvApp
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/06/2009
*! Purpose    : Prevent the screen from appearing (continuing the work of lfMfcsAutIssu Function)
*! Tracking # : C201141
*!**************************************************************************
FUNCTION lfPrcPrvApp
PARAMETERS loformset

*B609478,3 TMI 12/18/2010 [Start] define the property llIfAut
*B609478,6 TMI 01/24/2011 [Start] it is not correct to add the property "llIfAut" here to loformset as it is not used in this method
*                                 the correct is to add it to "loparentform"
*IF TYPE('loFormSet.llIfAut') = 'U' 
*  loFormSet.AddProperty('llIfAut',.F.)
*ENDIF
IF TYPE('loparentform.llIfAut') = 'U' 
  loparentform.AddProperty('llIfAut',.F.)
ENDIF
*B609478,6 TMI 01/24/2011 [End  ] 
*B609478,3 TMI 12/18/2010 [End  ] define the property llIfAut

*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!*  IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID)
IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) OR gfGetMemVar('M_ISSURECI',oAriaApplication.ActiveCompanyID)
*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

  IF loparentform.llIfAut
    IF !lfMfIssLtInit(loformset.lcOprCode,loformset.lcLotNo,loformset)
      llPRCPRVAPP = .F.
      RETURN
    ENDIF 
  ENDIF 
ENDIF
* End of lfPrcPrvApp 
*!**************************************************************************
*! Name       : lfPreIssData
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/06/2009
*! Purpose    : Update the file with just the required Qntys and the 100% precent 
*! Tracking # : C201141
*! Type       : Custom
*!**************************************************************************
FUNCTION lfPreIssData
PARAMETERS loformset

IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) OR ;
   gfGetMemVar('M_ISSURECI',oAriaApplication.ActiveCompanyID)
  loFormSet.lcOprCode = ''
  loFormSet.lcLotNo = ''
  
  lcOrder = ORDER()
  SET ORDER TO 
    
  *B609478,8 TMI 04/03/2011 [Start] add the user ID to the start of the file ACTDATA.MEM to avoid the case that two users logged in at the same time creating POs
  *IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) AND FILE(oAriaApplication.DataDir+"ActData.mem")
    *RESTORE FROM oAriaApplication.DataDir+"ActData.mem" ADDITIVE
  IF gfGetMemVar('M_ISSURAIS',oAriaApplication.ActiveCompanyID) AND FILE(oAriaApplication.DataDir+oAriaApplication.User_id+"ActData.mem")
    RESTORE FROM oAriaApplication.DataDir+oAriaApplication.User_id+"ActData.mem" ADDITIVE
    *B609478,8 TMI 04/03/2011 [End  ] 
    lnX = 1
    LOCATE
    
    SCAN FOR ccatgtyp = 'S'
      REPLACE Item     WITH laRecData[lnX,1]  ;
              pieces   WITH laRecData[lnX,18] ;
              req_qty  WITH laRecData[lnX,18] ;
              untqty   WITH laRecData[lnX,25] ;
              nissue   WITH laRecData[lnX,18] ;
              untcost  WITH laRecData[lnX,23] ;
              Req_Qty1 WITH laRecData[lnx,2]  ;
              Req_Qty2 WITH laRecData[lnx,3]  ;
              Req_Qty3 WITH laRecData[lnx,4]  ;
              Req_Qty4 WITH laRecData[lnx,5]  ;
              Req_Qty5 WITH laRecData[lnx,6]  ;
              Req_Qty6 WITH laRecData[lnx,7]  ;
              Req_Qty7 WITH laRecData[lnx,8]  ;
              Req_Qty8 WITH laRecData[lnx,9]  ;
              nIssPcnt WITH 100
  
      IF pieces <= 0
        REPLACE lSelect  WITH .F. ;
                nIssPcnt WITH 0
      ENDIF
      lnX = lnX + 1
      *B611005,1 MMT 05/14/2015 error while receiving PO at DCC[T20150512.0009][Start]
      IF lnX > ALEN(laRecData,1)
        EXIT 
      ENDIF
      *B611005,1 MMT 05/14/2015 error while receiving PO at DCC[T20150512.0009][Start]
    ENDSCAN 
    SET ORDER TO (lcOrder) 
  ELSE 
    *B609478,5 TMI 01/14/2011 [Start] add oAriaApplication.User_id to the name of the temp file
    *IF FILE(oAriaApplication.DataDir+"Budgtdata.mem")
    *  RESTORE FROM oAriaApplication.DataDir+"Budgtdata.mem" ADDITIVE    
    IF FILE(oAriaApplication.DataDir+oAriaApplication.User_id+"Budgtdata.mem")
      RESTORE FROM oAriaApplication.DataDir+oAriaApplication.User_id+"Budgtdata.mem" ADDITIVE    
      *B609478,5 TMI 01/14/2011 [End  ]       
    ELSE
      RETURN
    ENDIF
    
    lnX = 1
    LOCATE 
    SCAN FOR ccatgtyp = 'S'
      
      lnStyPos = ASCAN(laRcvIssData,Item)
      IF lnStyPos > 0
        lnStyPos = ASUBSCRIPT(laRcvIssData,lnStyPos ,1)
      ENDIF       
     
      DIMENSION laOrgData[lnX,10]
      lnTotOrg = 0
      FOR lnT = 1 TO 8
        lcT = ALLTRIM(STR(lnT))
        laOrgData[lnX,lnT] = Req_Qty&lcT
        lnTotOrg = lnTotOrg + Req_Qty&lcT
      ENDFOR 
      laOrgData[lnX,9] = lnTotOrg
      laOrgData[lnX,10] = Item
      
     REPLACE  Req_Qty1 WITH IIF(TYPE('laRcvIssData[lnStyPos ,18]') = 'N',laRcvIssData[lnStyPos ,18],0) + used_qty1 ;
              Req_Qty2 WITH IIF(TYPE('laRcvIssData[lnStyPos ,19]') = 'N',laRcvIssData[lnStyPos ,19],0) + used_qty2 ;
              Req_Qty3 WITH IIF(TYPE('laRcvIssData[lnStyPos ,20]') = 'N',laRcvIssData[lnStyPos ,20],0) + used_qty3 ;
              Req_Qty4 WITH IIF(TYPE('laRcvIssData[lnStyPos ,21]') = 'N',laRcvIssData[lnStyPos ,21],0) + used_qty4 ;
              Req_Qty5 WITH IIF(TYPE('laRcvIssData[lnStyPos ,22]') = 'N',laRcvIssData[lnStyPos ,22],0) + used_qty5 ;
              Req_Qty6 WITH IIF(TYPE('laRcvIssData[lnStyPos ,23]') = 'N',laRcvIssData[lnStyPos ,23],0) + used_qty6 ;
              Req_Qty7 WITH IIF(TYPE('laRcvIssData[lnStyPos ,24]') = 'N',laRcvIssData[lnStyPos ,24],0) + used_qty7 ;
              Req_Qty8 WITH IIF(TYPE('laRcvIssData[lnStyPos ,25]') = 'N',laRcvIssData[lnStyPos ,25],0) + used_qty8 ;
              req_qty  WITH Req_Qty1+Req_Qty2+Req_Qty3+Req_Qty4+Req_Qty5+Req_Qty6+Req_Qty7+Req_Qty8 ;
              nIssPcnt WITH 100
      
      IF pieces <= 0
        REPLACE lSelect  WITH .F. ;
                nIssPcnt WITH 0
      ENDIF
      lnX = lnX + 1
    ENDSCAN 
    *B609478,5 TMI 01/14/2011 [Start] 
    *SAVE TO (oAriaApplication.DataDir+"ActOrgData.mem") ALL LIKE laOrgData    
    SAVE TO (oAriaApplication.DataDir+oAriaApplication.User_id+"ActOrgData.mem") ALL LIKE laOrgData
    *B609478,5 TMI 01/14/2011 [End  ] 
    
    SET ORDER TO (lcOrder)     
  ENDIF
ENDIF        
* End of lfPreIssData

*!**************************************************************************
*! Name       : lfPrRecdata
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/09/2010
*! Purpose    : Get the updated data by the user in the Manual process
*! Tracking # : C201230
*! Type       : Custom
*!**************************************************************************
FUNCTION lfPrRecdata
PARAMETERS loFormSet

IF gfGetMemVar('M_ISSURECI',oAriaApplication.ActiveCompanyID)

  SELECT(loParentForm.lcTmpLine)
  lcStyle = Style
  *B609478,5 TMI 01/14/2011 [Start] add oAriaApplication.User_id to the name of the temp file
  *IF FILE(oAriaApplication.DataDir+"Budgtdata.mem")
  *  RESTORE FROM oAriaApplication.DataDir+"Budgtdata.mem" ADDITIVE
  *  ERASE oAriaApplication.DataDir+"Budgtdata.mem"
  IF FILE(oAriaApplication.DataDir+oAriaApplication.User_id+"Budgtdata.mem")
    RESTORE FROM oAriaApplication.DataDir+oAriaApplication.User_id+"Budgtdata.mem" ADDITIVE
    ERASE oAriaApplication.DataDir+oAriaApplication.User_id+"Budgtdata.mem"
    *B609478,5 TMI 01/14/2011 [End  ] 
  ELSE 
    RETURN 
  ENDIF
  *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[Start]
  llexist = .F.
  IF VARTYPE(larcvissdata)='U'
    RETURN
  ENDIF
  *B609273,1 MMT 10/26/2010 Fix bug of wrong vendor information in created comp. PO[End]
  lnStyPos = ASCAN(laRcvIssData,lcStyle)
  llExist = .F.
  IF lnStyPos > 0
    lnStyPos = ASUBSCRIPT(laRcvIssData,lnStyPos ,1)
    llExist = .T.
  ENDIF 
  IF !llExist 
    RETURN
  ENDIF 
  SELECT (loFormSet.lcLineQty)
  LOCATE FOR cType = '4'
  FOR lnM = 1 TO 8
    lcM = ALLTRIM(STR(lnM))
    laRcvIssData[lnStyPos,lnM+17] = NQty&lcM
  ENDFOR 
  LOCATE FOR cType = '8'
  FOR lnT = 1 TO 8
    lcT = ALLTRIM(STR(lnT))
    laRcvIssData[lnStyPos,lnT+25] = NQty&lcT
  ENDFOR 
  *B609478,5 TMI 01/14/2011 [Start] add oAriaApplication.User_id to the name of the temp file
  *SAVE TO (oAriaApplication.DataDir+"BudgtData.mem") ALL LIKE laRcvIssData  
  SAVE TO (oAriaApplication.DataDir+oAriaApplication.User_id+"BudgtData.mem") ALL LIKE laRcvIssData
  *B609478,5 TMI 01/14/2011 [End  ] 
ENDIF
* End of lfPrRecdata()
*C201141,1 HES 04/26/2009, quote Processing Enbroidery orders for DCC [End]

*TMI 05/24/2009 [Start] do not copy this trigger from its original source, call it from BN4MAIN.PRG instead
*!*    *!***************************************************************************
*!*    *!* Name        : lfAlSavAut
*!*    *!* Developer   : MOS
*!*    *!* Date        : 05/16/2009
*!*    *!* Module      : Allocation (AL)
*!*    *!* Purpose     : Save Data to Automatic Allocation screen 
*!*    *!***************************************************************************
*!*    *C201136
*!*    FUNCTION lfAlSavAut
*!*    LOCAL lnSlct,lnDataSession

*!*    lcAlocOrd = PADR(gfGetMemvar('M_ALOCORD'),1)
*!*    DO CASE
*!*    CASE lcAlocOrd = 'Y'
*!*      *- go to alloaction
*!*    CASE lcAlocOrd = 'N'
*!*      RETURN
*!*    CASE lcAlocOrd = 'I'
*!*      IF gfModalGen('INM00000B32000',.F.,.F.,.F.,'Do you want to pick the created Sales Order?') <> 1
*!*        RETURN
*!*      ENDIF
*!*    OTHERWISE
*!*      RETURN  
*!*    ENDCASE

*!*    lnSlct = SELECT(0)
*!*    PRIVATE llCalledFromSoord
*!*    llCalledFromSoord = .T.
*!*    oAriaApplication.DoProgram("AWRALORDAL",'"'+OrdHdr.Order+'"',.F.,'AL')
*!*    TRY  
*!*    loFormSet.mOptionsMenuPad()
*!*    CATCH
*!*    ENDTRY 
*!*    SELECT (lnSlct )
*!*    *--------------- END OF lfAlSavAut
*TMI 05/24/2009 [End  ] 

*:**************************************************************************
*:* Name        : lfSPSHSAV     && spread sheet save
*:* Developer   : MOS - MOSTAFA EID
*:* Date        : 05/21/2009
*:* Purpose     : Update the ORDER DATA got from a spreaqd sheet for DCC
*:***************************************************************************
*:* Called from : SOUPDATE.PRG
*:***************************************************************************
*C201136,1
FUNCTION lfSPSHSAV

lcMsg = 'Sales Order  '+ lcOrderNo + '  has been Created For spread sheet '+ lcFileName 
=gfModalGen('INM00000B00000',.F.,.F.,.F.,lcMsg)
lcXlsHis= IIF(RIGHTC(ALLTRIM(lcXlsHis),1)="\",ALLTRIM(lcXlsHis),ALLTRIM(lcXlsHis)+"\")
lcEndName = ALLTRIM(lcXlsHis)+UPPER(lcRpAccnt)+"-"+UPPER(ALLTRIM(lcFileName))
IF FILE(lcEndName)
  IF gfModalGen('QRM00000B42002',.F.,.F.,.F.,'Spread Sheet With the Name  '+lcRpAccnt+"-"+ALLTRIM(lcFileName)+'  already Exists, Replace ?') = 1 
    DELETE FILE (lcEndName)
    RENAME (lcimpXls) TO (lcEndName)    
  ENDIF    
ELSE 
  RENAME (lcimpXls) TO (lcEndName)
ENDIF 
  
*- calling the notepad
=lfSHOWNTS()

SELECT ORDHDR
=TABLEUPDATE(.T.) 
*TMI 05/24/2009 [Start] call lfAlSavAut trigger from BN4MAIN.fxp to do allocation 
*=lfAlSavAut()
IF FILE('BN4MAIN.FXP')
  DO lfAlSavAut IN BN4MAIN.FXP
ENDIF
*TMI 05/24/2009 [End  ] 
loformset.ChangeMode('S')
loformset.llUpdate = .F.   
*!*    SET REPROCESS TO lnRePro  
*!*    *RETURN .F.

*-- end of lfSPSHSAV.

*=====================================================================================
*C201220,1 HES 13/02/2010, Recalculate Style PO Quantities- Quotation required [Start]
*!**************************************************************************
*! Name       : lfReclcQty
*! Developer  : (HES)Hesham Elmasry
*! Date       : 02/13/2010
*! Purpose    : Create the custom Option fpr the Recalculating Qtys Screen
*! Tracking # : C201220
*!**************************************************************************
FUNCTION lfReclcQty

PRIVATE llFound

&& Property holds the validation result for the selected fabric
IF TYPE('loFormSet.llVldFbrc') = 'U'
  loFormSet.AddProperty('llVldFbrc',.F.)
ENDIF
IF TYPE('loFormSet.lcPrFbStys') = 'U'
  loFormSet.AddProperty('lcPrFbStys','')
ENDIF
IF TYPE('loFormSet.lcPriFabrc') = 'U'
  loFormSet.AddProperty('lcPriFabrc','')
ENDIF

&& Creating the Option Menue
STORE .F. TO llFound
FOR lnCount = 1 TO CNTPAD('_MSYSMENU')    && Number of pads
  IF PRMPAD('_MSYSMENU', GETPAD('_MSYSMENU', lnCount)) = 'Options'
    llFound = .T.
    EXIT
  ENDIF
ENDFOR
IF !llFound
  DEFINE PAD _Option OF (loFormSet.cHostFormName) PROMPT 'O\<ptions' KEY ALT+P , ' '
  ON PAD _Option OF (loFormSet.cHostFormName) ACTIVATE POPUP _OPTIONPOP
  DEFINE POPUP _OPTIONPOP MARGIN SHADOW
ENDIF

&& Creating the cutom Option
lnBarNo = CNTBAR('_OPTIONPOP') + 1 
DEFINE BAR lnBarNo  OF _OPTIONPOP PROMPT 'Recalculate PO Quantities' SKIP FOR !(_screen.ActiveForm.Parent.llVldFbrc)
ON SELECTION BAR lnBarNo  OF _OPTIONPOP DO lfRrcQty

* End of lfReclcQty()

*!**************************************************************************
*! Name       : lfRrcQty
*! Developer  : (HES)Hesham Elmasry
*! Date       : 02/13/2010
*! Purpose    : Open the Recalculating Qtys Screen
*! Tracking # : C201220
*!**************************************************************************
FUNCTION lfRrcQty 

loFormSet = _screen.ActiveForm.parent

IF !USED('STYLE') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'STYLE',oAriaApplication.DATADIR+'STYLE','SH')
ENDIF

IF !USED('SCALE') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'SCALE',oAriaApplication.DATADIR+'SCALE','SH')
ENDIF

IF !USED('APVENDOR') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'APVENDOR',oAriaApplication.DATADIR+'VENCODE','SH')
ENDIF

IF !USED('ORDLINE') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'ORDLINE',oAriaApplication.DATADIR+'ORDLINE','SH')
ENDIF

IF !USED('CUSTOMER') 
  =gfOpenTABLE(oAriaApplication.DATADIR+'CUSTOMER',oAriaApplication.DATADIR+'CUSTOMER','SH')
ENDIF

loFormSet.lcPrFbStys = gfTempName()
=lfCrPStTmp(loFormSet.lcPrFbStys)

lnMajorlen=LEN(gfItemMask("PM","","0001"))
STORE  0 TO lnClrLen ,lnClrPos
DECLARE laItemSeg[1]
PRIVATE lnCount 
=gfItemMask(@laItemSeg)
lnClrLen = 0
FOR lnCount = 1 TO ALEN(laItemSeg,1)
  IF laItemSeg[lnCount,1]='C'
    lnClrLen = LEN(laItemSeg[lnCount,3])
    EXIT
  ENDIF
ENDFOR

lnSclLen=19-(lnMajorlen+lnClrLen+1)
llIfExtend = gfGetMemVar('M_USEEXSSC',oAriaApplication.ActiveCompanyID)

STORE 0 TO m.QTY1,    m.QTY2,    m.QTY3,    m.QTY4,    m.QTY5,    m.QTY6,    m.QTY7,    m.QTY8,    m.Total, ;
           m.ORGQTY1, m.ORGQTY2, m.ORGQTY3, m.ORGQTY4, m.ORGQTY5, m.ORGQTY6, m.ORGQTY7, m.ORGQTY8, m.ORGTotal

SELECT(loFormSet.lcTktSheet)
LOCATE FOR ITEM = loFormSet.lcPriFabrc
m.FREQ    = REQ_QTY
m.FISSUED = ISSUE_QTY
m.APPLIED = ISSUE_QTY
m.USED    = USED_QTY
m.FBRDESC = DESC
m.DIFRNC  = USED_QTY - REQ_QTY
IF !EMPTY(loFormSet.lcPriFabrc)
  SELECT (loFormSet.lcBomLine)
  SCAN FOR !EOF()
    IF loFormSet.lcPriFabrc = ITEM AND lfvFbSt(STYLE,ITEM)
      SELECT (loFormSet.lcBomLine)      
      m.PO      = CTKTNO
      m.STYLE   = STYLE
      m.FABRIC  = loFormSet.lcPriFabrc
      m.CLRDESC = gfCodDes(PADR(SUBSTR(STYLE,lnMajorlen+2,lnClrLen),6),'COLOR')
      m.REQ     = STYQTY*UNITQTY
      m.UNTQTY  = UNITQTY
      
      SELECT(loFormSet.lcPosln)
      LOCATE FOR STYLE = m.STYLE
      IF FOUND()
        FOR lnX =1 TO 8
          lcX = ALLTRIM(STR(lnX))
          m.ORGQTY&lcX = QTY&lcX
          m.PRCQTY&lcX = QTY&lcX/TOTQTY
        ENDFOR 
        m.OrgTotal = TOTQTY
        m.Lineno   = LINENO
      ENDIF 
      
      SELECT STYLE
      IF gfSeek(m.STYLE)      
        FOR lnX =1 TO 8
          lcX = ALLTRIM(STR(lnX))
          m.SZ&lcX = SCALE.SZ&lcX
        ENDFOR
        m.CNT = SCALE.CNT
      ENDIF 
      
      SELECT(loFormSet.lcPrFbStys)
      APPEND BLANK 
      GATHER MEMVAR MEMO
    ENDIF 
  ENDSCAN 
ENDIF

SELECT(loFormSet.lcPrFbStys)
LOCATE
IF llIfExtend AND lnSclLen > 0
  lcStyle = ""
  lnCntr = 0
  llExtRec = .F.
  lcOrder = ORDER()
  SET ORDER TO 
  SCAN FOR !EOF()
    lcCurStyle = SUBSTR(STYLE,1,18)
    IF lcStyle <> lcCurStyle
      lnCntr = lnCntr + 1
      REPLACE EXTFLG WITH lnCntr 
    ELSE 
      REPLACE EXTFLG WITH lnCntr
      llExtRec = .T.
    ENDIF 
    lcStyle = lcCurStyle
  ENDSCAN 
  IF !llExtRec
    REPLACE EXTFLG WITH 0 ALL
  ENDIF 
  SET ORDER TO &lcOrder 
  && Handle the Perce. if Extended lines exist.
  SELECT(loFormSet.lcPrFbStys)
  SUM EXTFLG TO lnTotFlg 
  IF lnTotFlg > 1
    =lfHndlExtnd()
  ENDIF
ELSE 
  REPLACE EXTFLG WITH 0 ALL
ENDIF 
&& Perform Calculations
=lfReqCalc(0,0)
SELECT(loFormSet.lcPrFbStys)
DO FORM (oAriaApplication.ScreenHome+'POCLCQTY.SCX') WITH loFormSet
* End of lfRrcQty()

*!**************************************************************************
*! Name       : lfHndlExtnd
*! Developer  : (HES)Hesham Elmasry
*! Date       : 02/25/2010
*! Purpose    : Handle Style Percentage if Extended.
*! Tracking # : C201220
*!**************************************************************************
FUNCTION lfHndlExtnd

LOCAL lnExCnt, lnExtTot
STORE 0 TO lnExCnt, lnExtTot

SELECT(loFormSet.lcPrFbStys)
SCAN FOR !EOF()
  lnRecNo = RECNO()
  lnFlg = EXTFLG
  COUNT TO lnExCnt FOR EXTFLG = lnFlg 
  IF lnExCnt > 1
    SET FILTER TO EXTFLG = lnFlg 
    SUM OrgTotal TO lnExtTot
    SCAN FOR !EOF()
      FOR lnY = 1 TO 8
        lcY = ALLTRIM(STR(lnY))
        REPLACE PRCQTY&lcY WITH ORGQTY&lcY/lnExtTot
      ENDFOR 
    ENDSCAN 
    SET FILTER TO 
    LOCATE FOR EXTFLG = lnFlg + 1
    SKIP -1
  ELSE 
    IF BETWEEN(lnRecNo,1,RECCOUNT())
      GOTO lnRecNo 
    ENDIF
  ENDIF 
ENDSCAN 
* End of lfHndlExtnd()

*!**************************************************************************
*! Name       : lfReqCalc
*! Developer  : (HES)Hesham Elmasry
*! Date       : 02/25/2010
*! Purpose    : Make required calculations
*! Tracking # : C201220
*!**************************************************************************
FUNCTION lfReqCalc
PARAMETERS lnUsed, lnRec

LOCAL lnStyTot, lnCurrTot
STORE 0 TO lnStyTot, lnCurrTot

SELECT(loFormSet.lcPrFbStys)
LOCATE
lnRecCnt = RECCOUNT()
IF lnRecCnt = 1
  lnStyTot = INT(IIF(lnUsed = 0,USED,lnUsed)/IIF(UNTQTY<>0,UNTQTY,1))
  m.STUSED = USED
  =lfCalcRec(lnStyTot,lnStyTot)
ELSE
  SELECT(loFormSet.lcPrFbStys)
  SUM EXTFLG TO lnTotFlg 
  IF llIfExtend AND lnSclLen > 0 AND lnTotFlg > 1
    lnSpFlg = 0
    IF lnRec <> 0
      SELECT(loFormSet.lcPrFbStys)
      SUM EXTFLG TO lnTotFlg       
      IF BETWEEN(lnRec,1,RECCOUNT())
        GOTO lnRec
      ENDIF 
      lnSpFlg = EXTFLG
      llSpec = .T.
    ENDIF     
    IF lnRec = 0   
      SCAN FOR !EOF()
        lnRecrd = RECNO()
        lnFlag = EXTFLG
        SUM OrgTotal TO lnStyTotls FOR EXTFLG = lnFlag
        LOCATE FOR EXTFLG = lnFlag
        SCAN REST WHILE EXTFLG = lnFlag 
          lnStyTot = INT(USED/IIF(UNTQTY<>0,UNTQTY,1))
          m.STUSED = USED
          lnStyPer   = OrgTotal / IIF(lnStyTotls<>0,lnStyTotls,1)
          lnStExOrSh = DIFRNC * lnStyPer
          lnStyReq   = OrgTotal * UNTQTY
          lnSTUSED   = ROUND(lnStyReq + lnStExOrSh,2)
          lnOStyTot  = ROUND(lnSTUSED / IIF(UNTQTY<>0,UNTQTY,1),0)        
          =lfCalcRec(lnStyTot,lnOStyTot)        
        ENDSCAN 
        LOCATE FOR EXTFLG = lnFlag + 1
        SKIP -1  
      ENDSCAN       
    ELSE
      IF lnUsed > 0
        SUM OrgTotal TO lnStyTotls FOR EXTFLG = lnSpFlg
        LOCATE FOR EXTFLG = lnSpFlg
        SCAN REST WHILE EXTFLG = lnSpFlg
          lnStyTot = INT(lnUsed/IIF(UNTQTY<>0,UNTQTY,1))
          m.STUSED = lnUsed
          lnStyPer   = OrgTotal / IIF(lnStyTotls<>0,lnStyTotls,1)
          REPLACE DIFRNC WITH lnUsed - FREQ
          lnStExOrSh = DIFRNC * lnStyPer
          lnStyReq   = OrgTotal * UNTQTY
          lnSTUSED   = ROUND(lnStyReq + lnStExOrSh,2)
          lnOStyTot  = ROUND(lnSTUSED / IIF(UNTQTY<>0,UNTQTY,1),0)        
          =lfCalcRec(lnStyTot,lnOStyTot)      
        ENDSCAN   
        IF BETWEEN(lnRec,1,RECCOUNT())
          GOTO lnRec     
        ENDIF
      ELSE 
        loFormSet.AriaForm1.txtUse.Value = loFormSet.AriaForm1.txtUse.OldValue          
      ENDIF
    ENDIF 
    =lfHndExtPrv()
  ELSE    
    SELECT(loFormSet.lcPrFbStys)
    GO BOTTOM 
    lnLstLn = RECNO()
    LOCATE
    SUM OrgTotal TO lnStyTotls
    llSpec = .F.
    DIMENSION laVldAll[RECCOUNT(),3]
    SCAN FOR !EOF()
      IF lnRec <> 0
        SELECT(loFormSet.lcPrFbStys)
        GOTO lnRec
        llSpec = .T.
      ENDIF
      lnReNo = RECNO()
      lnTotRec = RECCOUNT()
      lnFUssed = FISSUED
      lnStyPer   = OrgTotal / IIF(lnStyTotls<>0,lnStyTotls,1)
      lnStExOrSh = DIFRNC * lnStyPer
      lnStyReq   = OrgTotal * UNTQTY
      m.STUSED   = ROUND(lnStyReq + lnStExOrSh,2)
      IF !llSpec
        lnStyTot   = CEILING((IIF(lnUsed = 0,m.STUSED,lnUsed)) / IIF(UNTQTY<>0,UNTQTY,1))
      ELSE 
        lnStyTot   = ROUND((IIF(lnUsed = 0,m.STUSED,lnUsed)) / IIF(UNTQTY<>0,UNTQTY,1),2)
      ENDIF 
      laVldAll[lnReNo,1] = UNTQTY
      laVldAll[lnReNo,2] = lnStyTot
      laVldAll[lnReNo,3] = lnStyTot * UNTQTY
      IF lnReNo = lnLstLn AND !llSpec
        lnAccum = 0
        FOR lnB = 1 TO lnTotRec
          IF lnB <> lnLstLn 
            lnAccum = lnAccum + laVldAll[lnB,3]
          ENDIF 
        ENDFOR 
        lnStyTot = (lnFUssed - lnAccum) / laVldAll[lnLstLn,1]
      ENDIF 
      =lfCalcRec(lnStyTot,lnStyTot)
      IF llSpec
        EXIT 
      ENDIF 
    ENDSCAN
  ENDIF
ENDIF
* End of lfReqCalc

*!**************************************************************************
*! Name       : lfCalcRec
*! Developer  : (HES)Hesham Elmasry
*! Date       : 02/25/2010
*! Purpose    : Calculate the current record
*! Tracking # : C201220
*!**************************************************************************
FUNCTION lfCalcRec
PARAMETERS lnFinTot,lnOrgStTot

PRIVATE lnCurrTot
STORE 0 TO lnCurrTot

SELECT(loFormSet.lcPrFbStys)
FOR lnX = 1 TO 8
  lcX = ALLTRIM(STR(lnX))
  lnInt = INT(PRCQTY&lcX*lnFinTot)
  lnFra = (PRCQTY&lcX*lnFinTot) - lnInt
  IF lnFra < 0.5
    m.QTY&lcX = INT(PRCQTY&lcX*lnFinTot)
  ELSE 
    m.QTY&lcX = ROUND(PRCQTY&lcX*lnFinTot,0)
  ENDIF 
  lnCurrTot = lnCurrTot + m.QTY&lcX
ENDFOR 
IF lnCurrTot <> lnOrgStTot
  lnDif = lnOrgStTot - lnCurrTot 
  FOR lnI = 1 TO 8
    lnVr = 9 - lnI
    lcVr = ALLTRIM(STR(lnVr))
    IF m.QTY&lcVr <> 0
        m.QTY&lcVr  = INT(m.QTY&lcVr + lnDif)
        EXIT
    ENDIF 
  ENDFOR 
ENDIF 
m.TOTQTY = m.QTY1+m.QTY2+m.QTY3+m.QTY4+m.QTY5+m.QTY6+m.QTY7+m.QTY8
GATHER MEMVAR FIELDS QTY1,QTY2,QTY3,QTY4,QTY5,QTY6,QTY7,QTY8,TOTQTY,STUSED
* End of lfCalcRec

*!**************************************************************************
*! Name       : lfHndExtPrv
*! Developer  : (HES)Hesham Elmasry
*! Date       : 03/07/2010
*! Purpose    : 
*! Tracking # : C201220
*!**************************************************************************
FUNCTION lfHndExtPrv

SELECT(loFormSet.lcPrFbStys)
SCAN FOR !EOF()
  lnEFlg = EXTFLG
  COUNT TO lnTotFlglns FOR EXTFLG = lnEFlg
  IF lnTotFlglns > 1
    SUM REQ TO lnTotFlgReq FOR EXTFLG = lnEFlg
    LOCATE FOR EXTFLG = lnEFlg
    lnCntr = 0
    SCAN REST WHILE EXTFLG = lnEFlg
      lnCntr = lnCntr + 1 
      IF lnCntr = 1
        REPLACE REQ WITH lnTotFlgReq
      ELSE 
        BLANK FIELDS REQ, STUSED
      ENDIF   
    ENDSCAN  
  ENDIF 
  LOCATE FOR EXTFLG = lnEFlg + 1 
  SKIP -1  
ENDSCAN 
* End of lfHndExtPrv()

*!**************************************************************************
*! Name       : lfvFbSt
*! Developer  : (HES)Hesham Elmasry
*! Date       : 02/13/2010
*! Purpose    : Validate if the style has this Fabric as a Primary Fabric
*! Tracking # : C201220
*!**************************************************************************
FUNCTION lfvFbSt
PARAMETERS lcStyle, lcItem 

SELECT STYLE
IF SEEK(lcStyle)
  IF STYLE.FABRIC $ lcItem
    RETURN .T.
  ELSE 
    RETURN .F.
  ENDIF 
ENDIF 
* End of lfvFbSt()

*!**************************************************************************
*! Name       : lfVLDFBRC
*! Developer  : (HES)Hesham Elmasry
*! Date       : 02/13/2010
*! Purpose    : validate the selected Fabric from PO Cost Sheet Screen to 
*!               handle the appearing of the custom Option.
*! Tracking # : C201220
*!**************************************************************************
FUNCTION lfVLDFBRC

loFormSet = _Screen.ActiveForm.Parent

*B609264,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [Start]
IF TYPE('loFormSet.llVldFbrc') = 'U'
  loFormSet.AddProperty('llVldFbrc',.F.)
ENDIF
*B609264,1 HES 05/23/2010 PO - Error adding new lines to PO Cost Sheet [End  ]

SELECT(loFormSet.lcTktSheet)

IF cCatgTyp = 'F'
  lcItem = ITEM
  lnReq  = REQ_QTY 
  lnUsed = USED_QTY
  SELECT (loFormSet.lcBomLine) 
  LOCATE FOR ITEM = lcItem
  IF FOUND()
    lcStyle = STYLE
    SELECT STYLE
    IF SEEK(lcStyle)
      IF STYLE.FABRIC $ lcItem AND lnUsed > 0 AND ROUND(lnReq,0) <> ROUND(lnUsed,0) 
        loFormSet.llVldFbrc = .T.
        loFormSet.lcPriFabrc = lcItem
      ELSE 
        loFormSet.llVldFbrc = .F.
      ENDIF 
    ELSE 
      loFormSet.llVldFbrc = .F.
    ENDIF 
  ELSE 
    loFormSet.llVldFbrc = .F.
  ENDIF 
ELSE 
  loFormSet.llVldFbrc = .F.
ENDIF 
* End of lfVLDFBRC()

*!**************************************************************************
*! Name       : lfCrPStTmp
*! Developer  : (HES)Hesham Elmasry
*! Date       : 02/14/2010
*! Purpose    : Create the primary styles temp
*! Tracking # : C201220
*!**************************************************************************
FUNCTION lfCrPStTmp
PARAMETERS lcPriStyTmp

*-------------\ Primary Temp \---------------
IF USED(lcPriStyTmp) AND RECCOUNT(lcPriStyTmp) > 0
  SELECT (lcPriStyTmp)
  ZAP 
ENDIF
IF !USED(lcPriStyTmp)
  lnI = 1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'STYLE'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 19
  laTempStru[lnI,4] = 0
     
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'FABRIC'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 19
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PO'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'FBRDESC'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 20
  laTempStru[lnI,4] = 0  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'CLRDESC'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 12
  laTempStru[lnI,4] = 0
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'FREQ'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 12
  laTempStru[lnI,4] = 3  

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'FISSUED'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 13
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'DIFRNC'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 12
  laTempStru[lnI,4] = 3  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'REQ'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 12
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'APPLIED'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 12
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'USED'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 13
  laTempStru[lnI,4] = 3  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'STUSED'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 13
  laTempStru[lnI,4] = 3    
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'UNTQTY'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 7
  laTempStru[lnI,4] = 3  

  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ORGQTY1'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ORGQTY2'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ORGQTY3'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ORGQTY4'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ORGQTY5'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ORGQTY6'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ORGQTY7'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ORGQTY8'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'ORGTOTAL'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'QTY1'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'QTY2'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'QTY3'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'QTY4'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'QTY5'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'QTY6'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'QTY7'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'QTY8'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3   
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'TOTQTY'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3   
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRCQTY1'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRCQTY2'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRCQTY3'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRCQTY4'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRCQTY5'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRCQTY6'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRCQTY7'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'PRCQTY8'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 3     
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SZ1'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0      
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SZ2'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0 
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SZ3'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0 
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SZ4'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0 
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SZ5'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0      
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SZ6'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0 
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SZ7'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0 
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'SZ8'
  laTempStru[lnI,2] = 'C'
  laTempStru[lnI,3] = 6
  laTempStru[lnI,4] = 0       
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'CNT'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 1
  laTempStru[lnI,4] = 0  
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'EXTFLG'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 11
  laTempStru[lnI,4] = 0   
  
  lnI = ALEN(laTempStru,1)+1
  DIMENSION laTempStru[lnI,4]
  laTempStru[lnI,1] = 'Lineno'
  laTempStru[lnI,2] = 'N'
  laTempStru[lnI,3] = 8
  laTempStru[lnI,4] = 0     
  
  DECLARE LAIndeces[1,2]
  LAIndeces[1,1] = 'PO+ALLTRIM(STR(EXTFLG))+STYLE+FABRIC'
  LAIndeces[1,2] = 'PRIFBRC'
  gfCrtTmp(lcPriStyTmp,@laTempStru,@LAIndeces,lcPriStyTmp,.T.)
ENDIF
* End of lfCrPStTmp()

*C201220,1 HES 13/02/2010, Recalculate Style PO Quantities- Quotation required [End]
*===================================================================================

*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [Start]
*!**********************************************************************************
*! Name       : lfCrStCpTmp
*! Developer  : (HES)Hesham Elmasry
*! Date       : 04/01/2010
*! Purpose    : Create the primary styles Component temp
*! Tracking # : 
*!**********************************************************************************
FUNCTION lfCrStCpTmp

*-------------\ Primary Temp \---------------
lcStyCpTmp = gfTempName() 
IF USED(lcStyCpTmp) AND RECCOUNT(lcStyCpTmp) > 0
  SELECT (lcStyCpTmp)
  ZAP 
ENDIF
IF !USED(lcStyCpTmp)
  lnI = 1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'STYLE'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 19
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'STYCOMP'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 19
  laTpCmpStru[lnI,4] = 0  
     
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'CLRDESC'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 12
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ1'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0      
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ2'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ3'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ4'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ5'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0      
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ6'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ7'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ8'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0         
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'POREQ1'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'POREQ2'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'POREQ3'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'POREQ4'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   
  

  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'POREQ5'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'POREQ6'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'POREQ7'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'POREQ8'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'POTotREQ'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0     
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'AvlStk1'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'AvlStk2'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'AvlStk3'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'AvlStk4'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   

  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'AvlStk5'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'AvlStk6'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'AvlStk7'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'AvlStk8'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'AvlTotSt'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0      
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd1'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd2'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd3'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd4'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   

  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd5'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd6'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd7'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd8'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnTotOrd'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0     
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue1'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue2'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue3'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue4'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   

  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue5'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue6'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue7'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue8'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'IssueTot'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0    
  
  DECLARE LACmpIndc[1,2]
  LACmpIndc[1,1] = 'STYCOMP'
  LACmpIndc[1,2] = 'STYCOMP'
  gfCrtTmp(lcStyCpTmp,@laTpCmpStru,@LACmpIndc,lcStyCpTmp,.T.)
ENDIF

*!**********************************************************************************
*! Name       : lfCrIsShtTmp
*! Developer  : (HES)Hesham Elmasry
*! Date       : 04/01/2010
*! Purpose    : Create the temp for Issue Sheet
*! Tracking # : C201230
*!**********************************************************************************
FUNCTION lfCrIsShtTmp

*-------------\ Primary Temp \---------------
IF USED(lcIssTmp) AND RECCOUNT(lcIssTmp) > 0
  SELECT (lcIssTmp)
  ZAP 
ENDIF
IF !USED(lcIssTmp)
  lnI = 1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'STYLEPO'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'STYCOMP'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 19
  laTpCmpStru[lnI,4] = 0  
     
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'CLRDESC'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 12
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'CPSHDSC'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 20
  laTpCmpStru[lnI,4] = 0  
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'FIT'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 5
  laTpCmpStru[lnI,4] = 0    
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'VENDOR'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 8
  laTpCmpStru[lnI,4] = 0     
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Avail'
  laTpCmpStru[lnI,2] = 'D'
  laTpCmpStru[lnI,3] = 8
  laTpCmpStru[lnI,4] = 0      
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'NPO'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0     
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SCLCNT'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 1
  laTpCmpStru[lnI,4] = 0    
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ1'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0      
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ2'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ3'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ4'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ5'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0      
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ6'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ7'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0 
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'SZ8'
  laTpCmpStru[lnI,2] = 'C'
  laTpCmpStru[lnI,3] = 6
  laTpCmpStru[lnI,4] = 0           
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd1'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd2'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd3'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd4'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   

  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd5'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd6'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd7'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnOrd8'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'BlnTotOrd'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0     
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue1'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue2'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue3'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue4'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   

  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue5'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue6'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue7'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'Issue8'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'IssueTot'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'OldIss1'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'OldIss2'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'OldIss3'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'OldIss4'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0   

  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'OldIss5'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'OldIss6'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'OldIss7'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'OldIss8'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0     
  
  lnI = ALEN(laTpCmpStru,1)+1
  DIMENSION laTpCmpStru[lnI,4]
  laTpCmpStru[lnI,1] = 'TotOldIss'
  laTpCmpStru[lnI,2] = 'N'
  laTpCmpStru[lnI,3] = 11
  laTpCmpStru[lnI,4] = 0    
  
  DECLARE LACmpIndc[1,2]
  LACmpIndc[1,1] = 'STYCOMP'
  LACmpIndc[1,2] = 'STYCOMP'
  gfCrtTmp(lcIssTmp,@laTpCmpStru,@LACmpIndc,lcIssTmp,.T.)
ENDIF
*C201230,1 HES 04/26/2010, Develop - specs - amend the DCC embroidery PO program [End  ]

*! B609271,1 HES 04/26/2009, Problem in POSTY.SCX [Start]
*!**********************************************************************************
*! Name       : lfMFRELPUP
*! Developer  : (HES)Hesham Elmasry
*! Date       : 05/25/2010
*! Purpose    : Create the temp for Issue Sheet
*! Tracking # : B609271
*!**********************************************************************************
FUNCTION lfMFRELPUP

IF TYPE('oPostyRef')='O'
  RELEASE oPostyRef
ENDIF 
* End of lfMFRELPUP()
*! B609271,1 HES 04/26/2009, Problem in POSTY.SCX [Start]




*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][Start]
*!**********************************************************************************
*! Name       : LFUPDPCKPACK
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 05/11/2011
*! Purpose    : Add Piktkt record to PICKPACK Table
*! Tracking # : C201334
*!**********************************************************************************
FUNCTION LFUPDPCKPACK
lcOldSel = SELECT()
*mt
llUpdatePik = .F.
*mt
*B610944,1 MMT 02/05/2015 Automatic allocation gives error while updating PICKPACK [T20150102.0002][Start]
TRY
*B610944,1 MMT 02/05/2015 Automatic allocation gives error while updating PICKPACK [T20150102.0002][End]
IF !USED('PICKPACK')
  =gfOpenTable('PICKPACK','PICKPACK')
ENDIF
IF !gfSeek(PIKTKT.PIKTKT,'PICKPACK')
  SELECT 'PICKPACK'
  APPEND BLANK 
  REPLACE PIKTKT WITH PIKTKT.PIKTKT,;
          PIKDATE WITH PIKTKT.Date,;
          CPSTATUS WITH 'Not Issued',;
          Order  WITH PIKTKT.ORDER,;
          cAdd_user WITH PIKTKT.cAdd_user,;
          cAdd_TIME WITH PIKTKT.cAdd_TIME,;
          cAdd_ver  WITH PIKTKT.cAdd_ver,;
          DADD_date WITH PIKTKT.DADD_date,;
          Account  WITH PIKTKT.Account,;
          STORE    WITH PIKTKT.STORE,;
          LCOMPPICK WITH .F.
  =gfReplace('')        
  =gfTableUpdate()
  *mt
  llUpdatePik = .T.
  *mt
ENDIF
=gfCloseTable('PICKPACK')
*B610944,1 MMT 02/05/2015 Automatic allocation gives error while updating PICKPACK [T20150102.0002][Start]
CATCH 
  LOCAL lnConnHandler ,lnConnectionHandlar1
  lnConnHandler = 0
  lnConnectionHandlar1= 0
  lcSqlStatment ="Select * from PICKPACK Where PIKTKT='"+PIKTKT.PIKTKT+"'"
  lnConnectionHandlar1 = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,"TmpPICKPACK",'PICKPACK',;
                  oAriaApplication.ActiveCompanyConStr,3,.F.,SET("Datasession"))                                
  IF lnConnectionHandlar1 >= 1 
    IF RECCOUNT('TmpPICKPACK')==0
      ldPikDate = PIKTKT.Date
      ldAddDate = PIKTKT.DADD_date
      lcSqlStatment ="INSERT INTO PICKPACK (PIKTKT ,PIKDATE,CPSTATUS,[Order],cAdd_user,cAdd_TIME,cAdd_ver,DADD_date,Account,STORE,LCOMPPICK) Values"+;
                                          "('"+PIKTKT.PIKTKT+"',?ldPikDate,'Not Issued','"+PIKTKT.ORDER+;
                                          "','"+PIKTKT.cAdd_user+"','"+PIKTKT.cAdd_TIME+"','"+PIKTKT.cAdd_ver+"',?ldAddDate,'"+PIKTKT.Account+"','"+PIKTKT.STORE+"',0)"
      lnConnectionHandlar1 = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,"TmpPICKPACK",'PICKPACK',;
                  oAriaApplication.ActiveCompanyConStr,3,.F.,SET("Datasession"),.T.,@lnConnHandler) 
      IF lnConnectionHandlar1 > 0
        *B610995,2 MMT 04/27/2015 Automatic allocation gives error while updating PICKPACK [T20150424.0001][Start]
        *SQLCOMMIT(lnConnHandler)
        TRY
          SQLCOMMIT(lnConnHandler)
          *mt
          llUpdatePik = .T.
          *mt
        CATCH        
        ENDTRY
        *B610995,2 MMT 04/27/2015 Automatic allocation gives error while updating PICKPACK [T20150424.0001][End]
      ELSE
        *B610995,1 MMT 04/26/2015 Automatic allocation gives error while updating PICKPACK [T20150424.0001][Start]
        *=oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnResult,.T.)    
        *B611002,1 MMT 05/13/2015 Error while updating Pickpack Table at DCC[T20150512.0006][Start]    
        *=oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnConnectionHandlar1,.T.)
        *B611002,1 MMT 05/13/2015 Error while updating Pickpack Table at DCC[T20150512.0006][End]
        *B610995,1 MMT 04/26/2015 Automatic allocation gives error while updating PICKPACK [T20150424.0001][End]
        *B610995,2 MMT 04/27/2015 Automatic allocation gives error while updating PICKPACK [T20150424.0001][Start]
        *SQLROLLBACK(lnConnHandler)
        TRY 
          SQLROLLBACK(lnConnHandler)
        CATCH
        ENDTRY 
        *B610995,1 MMT 04/27/2015 Automatic allocation gives error while updating PICKPACK [T20150424.0001][end]
      ENDIF                                                    
    ENDIF
  ENDIF
ENDTRY 
*B610944,1 MMT 02/05/2015 Automatic allocation gives error while updating PICKPACK [T20150102.0002][End]
*mt
IF  !llUpdatePik 
  LOCAL lnConnHandler2 ,lnConnectionHandlar1
  lnConnHandler2 = 0
  lnConnectionHandlar11= 0
  lcSqlStatment ="Select * from PICKPACK Where PIKTKT='"+PIKTKT.PIKTKT+"'"
  lnConnectionHandlar11 = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,"TmpPICKPACK",'PICKPACK',;
                  oAriaApplication.ActiveCompanyConStr,3,.F.,SET("Datasession"))                                
  IF lnConnectionHandlar11 >= 1 
    IF RECCOUNT('TmpPICKPACK')==0
      ldPikDate = PIKTKT.Date
      ldAddDate = PIKTKT.DADD_date
      lcSqlStatment ="INSERT INTO PICKPACK (PIKTKT ,PIKDATE,CPSTATUS,[Order],cAdd_user,cAdd_TIME,cAdd_ver,DADD_date,Account,STORE,LCOMPPICK) Values"+;
                                          "('"+PIKTKT.PIKTKT+"',?ldPikDate,'Not Issued','"+PIKTKT.ORDER+;
                                          "','"+PIKTKT.cAdd_user+"','"+PIKTKT.cAdd_TIME+"','"+PIKTKT.cAdd_ver+"',?ldAddDate,'"+PIKTKT.Account+"','"+PIKTKT.STORE+"',0)"
      DO WHILE (!llUpdatePik)                                    
        lnConnectionHandlar11 = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,"TmpPICKPACK",'PICKPACK',;
                    oAriaApplication.ActiveCompanyConStr,3,.F.,SET("Datasession"),.T.,@lnConnHandler2) 
        IF lnConnectionHandlar11 > 0
          TRY
            SQLCOMMIT(lnConnHandler2)
            llUpdatePik = .T.
          CATCH        
          ENDTRY
        ELSE
          TRY 
            SQLROLLBACK(lnConnHandler2)
          CATCH
          ENDTRY 
        ENDIF                                                    
      ENDDO     
    ENDIF
  ENDIF
ENDIF
*mt

SELECT (lcOldSel)
*!**********************************************************************************
*! Name       : lfCANPCKPACK
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 05/11/2011
*! Purpose    : Cancel Piktkt record in PICKPACK Table
*! Tracking # : C201334
*!**********************************************************************************
FUNCTION lfCANPCKPACK
lcOldSel = SELECT()
IF !USED('PICKPACK')
  =gfOpenTable('PICKPACK','PICKPACK')
ENDIF
IF gfSeek(PIKTKT.PIKTKT,'PICKPACK')
  SELECT PICKPACK
  REPLACE CPSTATUS  WITH 'Cancelled'
  =gfAdd_Info('PICKPACK')
  =gfReplace("")
  =gfTableUpdate()
ENDIF
=gfCloseTable('PICKPACK')
SELECT (lcOldSel)
*: C201334,1 MMT 05/11/2011 Custom Pick and Pack monitor screen[T20110401.0003][End]

*C201438,1 MMT 12/12/2011 Add Trigger to default POSHDR.CFRCSTGRP to the code defualt value[Start]
*!**********************************************************************************
*! Name       : lfPOWIPGRP
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 12/12/2011
*! Purpose    : Get CFRCSTGRP  default value and update it in poshdr
*! Tracking # : C201438,1 
*!**********************************************************************************
FUNCTION lfPOWIPGRP  

IF loFormSet.ActiveMode <> 'A'
  RETURN 
ENDIF
LOCAL lcDefaultValue
lcDefaultValue = ''
lnCurAlias = SELECT()
IF !USED('CODES_A')
  =gfOpenTable('CODES','CCODE_NO','SH','CODES_A')
ENDIF
IF gfSEEK('D'+'CFRCSTGRP','CODES_A','CCODE_NO')
  lcDefaultValue = CODES_A.CCODE_NO
ENDIF
SELECT(loFormSet.ariaForm1.mainworkorder.cPosHdr)
REPLACE CFRCSTGRP WITH lcDefaultValue
SELECT(lnCurAlias)
*C201438,1 MMT 12/12/2011 Add Trigger to default POSHDR.CFRCSTGRP to the code defualt value[END]
*C201472,1 MMT 04/02/2012 Custom program to allocate received qty[T20120314.0001][Start]
*!**********************************************************************************
*! Name       : lfAUTALLOC
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 04/02/2012
*! Purpose    : Allocate SOs from Received Qty 
*! Tracking # : C201472,1
*!**********************************************************************************
FUNCTION lfAUTALLOC

llAutoAlloc = gfGetMemVar('M_AUTOALLO',oAriaApplication.ActiveCompanyID)
IF !llAutoAlloc
  RETURN
ENDIF 

*C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[Start]
llUseCtPick =.F.
llUseOrdline =.F.
llUseOrdHdr =.F.
llUseStyle =.F.
llUseStyDye =.F.
llUseScale =.F.
*C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[END]
IF !USED('CUTPICK')
  gfOpenTable('CUTPICK','CUTPKORD')
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[Start]
  llUseCtPick =.T.
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[END]
ENDIF

IF !USED('ORDLINE')
  gfOpenTable('ORDLINE','ORDLINE')
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[Start]
  llUseOrdline =.T.
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[END]
ENDIF
IF !USED('ORDHDR')
  gfOpenTable('ORDHDR','ORDHDR')
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[Start]
  llUseOrdHdr =.T.
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[END]
ENDIF
IF !USED('STYLE')
  gfOpenTable('STYLE','STYLE')
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[Start]
  llUseStyle =.T.
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[END]
ENDIF
IF !USED('STYDYE')
  gfOpenTable('STYDYE','STYDYE')
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[Start]
  llUseStyDye =.T.
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[END]
ENDIF
IF !USED('SCALE')
  gfOpenTable('SCALE','SCALE')
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[Start]
  llUseScale =.T.
  *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[END]
ENDIF

*C201695,1 MMT 07/07/2015 Create Picking Ticket for SO linked to the PO being received[T20150317.0018][Start]
IF !USED('PIKTKT')
  =gfOpenTable('PIKTKT','PIKTKT')
ENDIF

IF !USED('WareHous')
  =gfOpenTable('WareHous','WAREHOUS')
ENDIF

IF 'AS' $ oAriaApplication.CompanyInstalledModules
  =gfOpenFile(oAriaApplication.DataDir+'EDIACPRT',oAriaApplication.DataDir+'ACCFACT','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDIPD',oAriaApplication.DataDir+'PARTTRANS','SH')
  =gfOpenFile(oAriaApplication.DataDir+'EDITRANS',oAriaApplication.DataDir+'TYPEKEY','SH')
ENDIF

oAlObj = CREATEOBJECT("AL")
*C201695,1 MMT 07/07/2015 Create Picking Ticket for SO linked to the PO being received[T20150317.0018][End]

SELECT(loFormSet.lcTmpLine)
lcTempLineCur = loFormSet.lcTmpLine
SCAN FOR trancd ='2' AND TOTQTY > 0
  lcLineNum = STR(LineNo,6)
  lcPoNum = PO
  IF !gfSeek('2'+lcPoNum+lcLineNum,'CUTPICK')
    LOOP 
  ENDIF
  SELECT CUTPICK
  DIMENSION laOrderAvl[1,2]
  laOrderAvl = ''
  SCAN REST WHILE TRANCD+CTKTNO+CTKTLINENO+ORDER+STYLE+CORDLINE  = '2'+lcPoNum+lcLineNum                                                                      
    SELECT Ordline 
    =gfSeek('O'+CUTPICK.ORDER+CUTPICK.CORDLINE)
    IF (!EMPTY(Ordline.PIKTKT) AND Ordline.PIKTKT <> REPLICATE('*',6)) OR TotQty = 0
      LOOP
    ENDIF
    IF PIK1 = Qty1 AND PIK2 = Qty2 AND PIK3 = Qty3 AND PIK4 = Qty4 AND PIK5 = Qty5 AND PIK6 = Qty6 AND PIK7 = Qty7 AND PIK8 = Qty8
      LOOP
    ENDIF
    =gfSeek('O'+CUTPICK.ORDER,'ORDHDR','ORDHDR')
    IF EMPTY(laOrderAvl[1,1])
      laOrderAvl[1,1] = CUTPICK.Order
      laOrderAvl[1,2] = Ordhdr.Complete
    ELSE
      DIMENSION laOrderAvl[ALEN(laOrderAvl,1)+1,2]
      laOrderAvl[ALEN(laOrderAvl,1),1] = CUTPICK.Order
      laOrderAvl[ALEN(laOrderAvl,1),2] = Ordhdr.Complete
    ENDIF
  ENDSCAN
  IF !EMPTY(laOrderAvl[1,1])
    ASORT(laOrderAvl,2)
    FOR lnArCnt = 1 TO ALEN(laOrderAvl,1)
       =SEEK('2'+lcPoNum+lcLineNum+laOrderAvl[lnArCnt,1],'CUTPICK')
       =gfSeek('O'+laOrderAvl[lnArCnt,1]+CUTPICK.CORDLINE,'ORDLINE')
       =gfSeek(Ordline.STYLE,'STYLE')
       =gfSeek('S'+STYLE.SCALE,'SCALE')       
      FOR  lnCntSca = 1 TO Scale.cnt
        lcCntSca  = STR(lnCntSca ,1)
        IF &lcTempLineCur..Qty&lcCntSca. > 0 AND Ordline.PIK&lcCntSca. < ordline.Qty&lcCntSca. 
          lnQtyToUpdate = MIN((ordline.Qty&lcCntSca. - Ordline.PIK&lcCntSca.),Cutpick.Qty&lcCntSca.,&lcTempLineCur..Qty&lcCntSca.)
          IF lnQtyToUpdate > 0 
            REPLACE Ordline.PIK&lcCntSca. WITH Ordline.PIK&lcCntSca.+lnQtyToUpdate ,;
                    Ordline.TOTPIK  WITH Ordline.TOTPIK +lnQtyToUpdate  IN ORDLINE
            IF EMPTY(ORDLINE.PIKTKT)
              REPLACE PIKTKT WITH REPLICATE('*',6),;
                      picked WITH .T.,;
                      pikdate WITH oAriaApplication.SystemDate IN ORDLINE
               
              *C201695,1 MMT 07/07/2015 Create Picking Ticket for SO linked to the PO being received[T20150317.0018][Start]
              lcNewPikTkt = oAlObj.lfGetPkTkt(ORDLINE.Order, ORDHDR.cDivision, ORDLINE.Store, ORDLINE.cWareCode)
              REPLACE PIKTKT WITH lcNewPikTkt  IN ORDLINE
              IF !SEEK(ORDLINE.Order + lcNewPikTkt , 'PIKTKT')
                INSERT INTO PIKTKT;
                (Piktkt, Account, STORE, ORDER, DATE, cWareCode, CustPo, STATUS);
                 VALUES;
               (lcNewPikTkt , ORDLINE.Account, ORDLINE.Store, ORDLINE.Order, oAriaApplication.SystemDate, ORDLINE.cWarecode,IIF(ORDHDR.MultiPO,Ordline.CustPo,ORDHDR.CustPo), 'O')
                =gfAdd_Info('PIKTKT')
                LFUPDPCKPACK()
                IF 'AS' $ oAriaApplication.CompanyInstalledModules AND SEEK(ORDLINE.cWareCode,'WareHous','WareHous') AND  SEEK('W'+WareHous.cThrdPLPr,'EDIACPRT','ACCFACT') AND ;
                    SEEK(EDIACPRT.cPartCode+'940','EDIPD','PARTTRANS')
                   SELECT EDITRANS
			        IF !SEEK('940'+PADR(lcNewPikTkt ,40)+'W'+WareHous.cThrdPLPr,'EDITRANS','TYPEKEY')
			          INSERT INTO 'EDITRANS' (CEDITRNTYP,KEY,TYPE,CPARTNER,lInterComp) VALUES ;
			            ('940',lcNewPikTkt ,'W',WareHous.cThrdPLPr,EDIACPRT.lInterComp)
			        ENDIF
			        REPLACE cStatus WITH 'N'
			        =gfAdd_Info('EDITRANS')
     		    ENDIF
              ENDIF
              *C201695,1 MMT 07/07/2015 Create Picking Ticket for SO linked to the PO being received[T20150317.0018][End]
            ENDIF
            REPLACE STYLE.ALO&lcCntSca. WITH STYLE.ALO&lcCntSca.+ lnQtyToUpdate ,;
            		style.totalo  WITH style.totalo+ lnQtyToUpdate	IN STYLE
            *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[Start]
            IF TYPE('lcTempItem')='C' AND !EMPTY(lcTempItem) AND USED(lcTempItem) AND SEEK(Ordline.STYLE,lcTempItem)
              REPLACE style.WIP&lcCntSca. WITH &lcTempItem..WIP&lcCntSca.,;
                      STYLE.TOTWIP WITH STYLE.WIP1+STYLE.WIP2+STYLE.WIP3+STYLE.WIP4+STYLE.WIP5+STYLE.WIP6+STYLE.WIP7+STYLE.WIP8 IN STYLE
                      
              REPLACE style.intrans&lcCntSca. WITH &lcTempItem..intrans&lcCntSca.,;
                      STYLE.totintrn WITH STYLE.intrans1+STYLE.intrans2+STYLE.intrans3+STYLE.intrans4+STYLE.intrans5+STYLE.intrans6+STYLE.intrans7+STYLE.intrans8 IN STYLE
            ENDIF          
            *C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[END] 		
            		
            =gfSeek(ORDLINE.STYLE+Ordline.CWARECODE,'STYDYE')		
            REPLACE STYDYE.ALO&lcCntSca. WITH STYDYE.ALO&lcCntSca.+ lnQtyToUpdate ,;
            		STYDYE.totalo  WITH STYDYE.totalo+ lnQtyToUpdate	IN STYDYE
            REPLACE Qty&lcCntSca.  WITH Qty&lcCntSca. - lnQtyToUpdate,;
                    TOTQTY         WITH TOTQTY - lnQtyToUpdate IN (lcTempLineCur)
          ENDIF
        ENDIF
      ENDFOR  
    ENDFOR
  ENDIF
ENDSCAN
SELECT Style 
=gfTableUpdate()
SELECT STYDYE
=gfTableUpdate()
SELECT ORDLINE
=gfTableUpdate()

*C201695,1 MMT 07/07/2015 Create Picking Ticket for SO linked to the PO being received[T20150317.0018][Start]
SELECT PIKTKT
=gfTableUpdate()
*C201695,1 MMT 07/07/2015 Create Picking Ticket for SO linked to the PO being received[T20150317.0018][End]

*C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[Start]
IF llUseScale 
  =gfCloseTable('Scale')
ENDIF
IF llUseStyDye 
  =gfCloseTable('STYDYE')
ENDIF
IF llUseStyle 
  =gfCloseTable('STYLE')
ENDIF
IF llUseOrdHdr
  =gfCloseTable('ORDHDR')
ENDIF
IF llUseOrdline
  =gfCloseTable('ORDLINE')
ENDIF
IF llUseCtPick 
  =gfCloseTable('CUTPICK')
ENDIF
*C201472,2 MMT 04/05/2012 WIP is not updated correctly in styles file[END]
*C201472,1 MMT 04/02/2012 Custom program to allocate received qty[T20120314.0001][End]

*C201636,1 MMT 09/07/2014 Consolidate invoice by contract reference[T20140819.0035][Start]
*!**********************************************************************************
*! Name       : lfCHANGCTRLN
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 09/07/2014
*! Purpose    : Change Dist_CTR field length to be 30 chars
*! Tracking # : C201636,1
*!**********************************************************************************
FUNCTION lfCHANGCTRLN
IF ASCAN(laFileStru,'DIST_CTR') > 0 
  lnPosDist = ASUBSCRIPT(laFileStru, ASCAN(laFileStru,'DIST_CTR'), 1)
  laFileStru[lnPosDist,3] = 30
ENDIF
*!**********************************************************************************
*! Name       : lfCHCKCONFLG
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 09/07/2014
*! Purpose    : Change Consolidate by flag value
*! Tracking # : C201636,1
*!**********************************************************************************
FUNCTION lfCHCKCONFLG
IF TYPE('lcAccount')<> 'C'
  lcAccount =  EVALUATE(loFormset.lcInvHdr+'.ACCOUNT')
ENDIF
llConsDist = SEEK('M'+lcAccount,'Customer') AND Customer.lconbycorf
LLCONSBYDC = llConsDist
llConsStore = .F.
*!**********************************************************************************
*! Name       : lfCHNGTMPIDX
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 09/07/2014
*! Purpose    : Change index based on customer flag
*! Tracking # : C201636,1
*!**********************************************************************************
FUNCTION lfCHNGTMPIDX
IF TYPE('lcAccount')<> 'C'
  lcAccount = EVALUATE(loFormset.lcInvHdr+'.ACCOUNT')
ENDIF
IF SEEK('M'+lcAccount,'Customer') AND (Customer.lconbycorf OR Customer.ConByDc $ 'SY')
  SET ORDER TO TAG ConsDist IN (loFormSet.lcInvhdr)
ENDIF
*!**********************************************************************************
*! Name       : lfALTCOLSZ
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 09/07/2014
*! Purpose    : Change dist_ctr field size in temp. files
*! Tracking # : C201636,1
*!**********************************************************************************
FUNCTION lfALTCOLSZ
IF TYPE('lcAccount')<> 'C'
  lcAccount = ORDHDR.ACCOUNT
ENDIF
IF SEEK('M'+lcAccount,'Customer') AND (Customer.lconbycorf) 
  llconsbystore = .F.
  lcOldHDOrder = ORDER(loFormset.lcInvHdr)
  lcOldCHOrder = ORDER(loFormset.lcConsInvH) 
  lcOldDTOrder = ORDER(loFormset.lcInvLine)
  lcOldCDOrder = ORDER(loFormset.lcConsInvD)
  ALTER table (loFormset.lcInvHdr) alter COLUMN Dist_Ctr Char(30)
  ALTER table (loFormset.lcConsInvH) alter COLUMN Dist_Ctr Char(30)
  ALTER table (loFormset.lcInvLine) alter COLUMN Dist_Ctr Char(30)
  ALTER table (loFormset.lcConsInvD) alter COLUMN Dist_Ctr Char(30)
  SET ORDER TO (lcOldHDOrder) IN (loFormset.lcInvHdr) 
  SET ORDER TO (lcOldCHOrder) IN (loFormset.lcConsInvH) 
  SET ORDER TO (lcOldDTOrder) IN (loFormset.lcInvLine)
  SET ORDER TO (lcOldCDOrder) IN (loFormset.lcConsInvD)
  IF !LLPICKED
    LCLINESCOND = LCLINESCOND +  " AND (Picked .AND. TotPik > 0 AND PIKTKT <> '******')"
  ENDIF
ENDIF
*!**********************************************************************************
*! Name       : lfADJDISTVAL
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 09/07/2014
*! Purpose    : Adjust dist_ctr memory field value
*! Tracking # : C201636,1
*!**********************************************************************************
FUNCTION lfADJDISTVAL
 IF SEEK('M'+m.Account,'Customer') AND (Customer.lconbycorf) 
   LCDIST_CTR = orDhdr.ccontref
 ENDIF  
*!**********************************************************************************
*! Name       : lfADJGRDCOL
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 09/07/2014
*! Purpose    : Adjust grid column control source
*! Tracking # : C201636,1
*!**********************************************************************************
FUNCTION lfADJGRDCOL
loFormset.AriaForm1.AriaPageFrame1.Page1.MultiSelectionGrid.grdMultiSelectionGrid.DistCtr.ControlSource  = "IIF(EVALUATE(Thisformset.lcinvhdr+'.Flag') = 'C','*****',IIF(LEN(Thisformset.lcinvhdr+'.Dist_Ctr')>8,'',Dist_Ctr))" 
loFormset.AriaForm1.AriaPageFrame1.Page1.MultiSelectionGrid.grdMultiSelectionGrid.Order.ControlSource    = "IIF(EVALUATE(Thisformset.lcinvhdr+'.Flag') $ 'O','*****',Order)"
*!**********************************************************************************
*! Name       : lfCLRDISTCTR
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 09/07/2014
*! Purpose    : Clear Dist_ctr memory field value
*! Tracking # : C201636,1
*!**********************************************************************************
FUNCTION lfCLRDISTCTR
IF SEEK('M'+m.Account,'Customer') AND (Customer.lconbycorf) 
   m.Dist_ctr = ''
ENDIF
*C201636,1 MMT 09/07/2014 Consolidate invoice by contract reference[T20140819.0035][End]


*C201643,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][Start]
*!**********************************************************************************
*! Name       : lfALOCPART  
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 11/30/2014
*! Purpose    : Check if allocate partially conditions are satisfied or not
*! Tracking # : C201643,1
*!**********************************************************************************
FUNCTION lfALOCPART  
*XX
*IF !llReturnValue AND llRpPikAll AND loFormSet.lnRpPikSep =100
IF !llReturnValue AND IIF(TYPE('llRpPikAll')='L',llRpPikAll,.T.) AND loFormSet.lnRpPikSep =100
*XX
  RETURN .T.
ELSE
  RETURN .F.  
ENDIF 
*!**********************************************************************************
*! Name       : lfDEFINEVAR
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 11/30/2014
*! Purpose    : Define new variable for option in OG
*! Tracking # : C201643,1
*!**********************************************************************************
FUNCTION lfDEFINEVAR
PUBLIC llRpPikAll 
llRpPikAll = .F.
*!**********************************************************************************
*! Name       : lfDONOTPIK
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 11/30/2014
*! Purpose    : Change flag in temp file to stop picking.
*! Tracking # : C201643,1
*!**********************************************************************************
FUNCTION lfDONOTPIK
IF llRpPikAll AND loFormSet.lnRpPikSep =100 
  IF !USED('Ordline_AL')
  =gfOpenTable("Ordline",'Ordline','SH','Ordline_AL')
ENDIF
*C201643,2 MMT 12/11/2014 Fix issue# 1on project[T20141118.0011][Start]
IF !USED('PIKTKT_AL')
  =gfOpenTable("PIKTKT",'PIKTKT','SH','PIKTKT_AL')
ENDIF
*C201643,2 MMT 12/11/2014 Fix issue# 1on project[T20141118.0011][End]
  SELECT (loFormSet.lcTmpOrdLn)
  LOCATE 
  SCAN 
  IF lnSel=1 AND   (IIF(Qty1>0, Qty1>Pik1,.F.) OR ;
  IIF(Qty2>0, Qty2>Pik2,.F.) OR ;
  IIF(Qty3>0, Qty3>Pik3,.F.) OR ;
  IIF(Qty4>0, Qty4>Pik4,.F.) OR ;
  IIF(Qty5>0, Qty5>Pik5,.F.) OR ;
  IIF(Qty6>0, Qty6>Pik6,.F.) OR ;
  IIF(Qty7>0, Qty7>Pik7,.F.) OR ;
  IIF(Qty8>0, Qty8>Pik8,.F.)) &&(TOTQTY > TOTPIK)
    REPLACE nProcNo WITH 5
  ENDIF 
  IF lnSel=1 AND TOTPIK =0 AND picked
    REPLACE picked WITH .f.,;
            pikdate WITH {},;
            TotPik WITH 0,; 
            PIKTKT WITH ''
     IF gfSeek(EVALUATE(loFormSet.lcTmpOrdLn+'.cOrdtype')+EVALUATE(loFormSet.lcTmpOrdLn+'.Order')+STR(EVALUATE(loFormSet.lcTmpOrdLn+'.LineNo'),6),'Ordline_AL') AND !EMPTY(Ordline_AL.PIKTKT)
       SELECT ORDLINE_AL
       REPLACE picked WITH .f.,;
	           pikdate WITH {},;
    	       TotPik WITH 0,; 
        	   PIKTKT WITH ''
       =gfReplace('') 	   
       =gfTableUpdate()  	   
 	 ENDIF	            
  ENDIF 
  ENDSCAN   
  SELECT distinct Order,Account,Store FROM (loFormSet.lcTmpOrdLn) WHERE lnSel=1  AND nProcNo = 5 INTO CURSOR 'DontPick'
  SELECT 'DontPick'
  LOCATE 
  SCAN 
    SELECT (loFormSet.lcTmpOrdLn)
    SCAN FOR Order = DontPick.Order  AND Account = DontPick.Account AND Store=DontPick.Store AND lnSel=1  AND nProcNo <> 5
       REPLACE nProcNo WITH 5
       *C201643,2 MMT 12/11/2014 Fix issue# 1on project[T20141118.0011][Start]
       IF !EMPTY(PIKTKT) AND PIKTKT <> '******' AND gfSeek(EVALUATE(loFormSet.lcTmpOrdLn+'.PIKTKT'),'PIKTKT_AL','PIKTKT')
         REPLACE PIKDATE WITH PIKTKT_AL.Date
         IF gfSeek(EVALUATE(loFormSet.lcTmpOrdLn+'.cOrdtype')+EVALUATE(loFormSet.lcTmpOrdLn+'.Order')+STR(EVALUATE(loFormSet.lcTmpOrdLn+'.LineNo'),6),'Ordline_AL') AND !EMPTY(Ordline_AL.PIKTKT)
           SELECT ORDLINE_AL
		   REPLACE  PIKDATE WITH PIKTKT_AL.Date
          =gfReplace('') 	   
          =gfTableUpdate()  	   
     	 ENDIF	     
       ENDIF
       *C201643,2 MMT 12/11/2014 Fix issue# 1on project[T20141118.0011][End]
    ENDSCAN
    SELECT 'DontPick'
  ENDSCAN 
  SELECT (loFormSet.lcTmpOrdLn)
ENDIF
*C201643,1 MMT 11/30/2014 Add Triggers for [T20141118.0011][End]
*C201645,1 MMT 12/18/2014 Add triggers to change the style field back color if projection field is 0[T20141121.0002][Start]
*!**********************************************************************************
*! Name       : lfSTYBKCLR  
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 12/18/2014
*! Purpose    : Change style back color if projection is 0 in the Sales order screen
*! Tracking # : C201645,1
*!**********************************************************************************
FUNCTION lfSTYBKCLR  
IF !USED('STYLE_COLOR')
  =gfOpenTable("Style","Style",'SH','STYLE_COLOR')
ENDIF
IF gfSeek(loFormSet.ariaForm1.ariapageframe1.page2.ariaeditregion1.keyStyle.Value,'STYLE_COLOR','STYLE') AND STYLE_COLOR.NSTYFRSPRJ = 0
 loFormSet.ariaForm1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.BackColor = 255
 loFormSet.ariaForm1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.DisabledBackColor=255
ELSE
  If !Isnull(oAriaApplication.setupedGotFocusBackColor ) And ;
      !Empty(oAriaApplication.setupedGotFocusBackColor) And ;
     Type('eval(oAriaApplication.setupedGotFocusBackColor)')='N'
    loFormSet.ariaForm1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.BackColor = Val(oAriaApplication.setupedGotFocusBackColor)
  ELSE
    loFormSet.ariaForm1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.BackColor = 16777215
  ENDIF
  loFormSet.ariaForm1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.DisabledBackColor=16777215
ENDIF
*!**********************************************************************************
*! Name       : lfSTYRSBKCLR
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 12/18/2014
*! Purpose    : reset the style back color
*! Tracking # : C201645,1
*!**********************************************************************************
FUNCTION lfSTYRSBKCLR
If !Isnull(oAriaApplication.setupedGotFocusBackColor ) And ;
    !Empty(oAriaApplication.setupedGotFocusBackColor) And ;
    Type('eval(oAriaApplication.setupedGotFocusBackColor)')='N'
  loFormSet.ariaForm1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.BackColor = Val(oAriaApplication.setupedGotFocusBackColor)
ELSE
  loFormSet.ariaForm1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.BackColor = 16777215
ENDIF
loFormSet.ariaForm1.ariapageframe1.page2.ariaeditregion1.keyStyle.txtItem.DisabledBackColor=16777215
*!**********************************************************************************
*! Name       : lfALSTYBKCLR
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 12/18/2014
*! Purpose    : change the style back color if projection is 0 in Allocation by order screen
*! Tracking # : C201645,1
*!**********************************************************************************
FUNCTION lfALSTYBKCLR

IF !USED('STYLE_COLOR')
  =gfOpenTable("Style","Style",'SH','STYLE_COLOR')
ENDIF
IF gfSeek(loFormSet.AriaForm1.kbStyle.Value,'STYLE_COLOR','STYLE') AND STYLE_COLOR.NSTYFRSPRJ = 0
  loFormSet.AriaForm1.kbStyle.txtItem.BackColor = 255
  loFormSet.AriaForm1.kbStyle.txtItem.DisabledBackColor=255
ELSE
  If !Isnull(oAriaApplication.setupedGotFocusBackColor ) And ;
      !Empty(oAriaApplication.setupedGotFocusBackColor) And ;
     Type('eval(oAriaApplication.setupedGotFocusBackColor)')='N'
    loFormSet.AriaForm1.kbStyle.txtItem.BackColor = Val(oAriaApplication.setupedGotFocusBackColor)
  ELSE
    loFormSet.AriaForm1.kbStyle.txtItem.BackColor = 16777215
  ENDIF
  loFormSet.AriaForm1.kbStyle.txtItem.DisabledBackColor=16777215
ENDIF  
*!**********************************************************************************
*! Name       : lfPORSBKCLR
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 12/18/2014
*! Purpose    : Rest the style back color PO screen
*! Tracking # : C201645,1
*!**********************************************************************************
FUNCTION lfPORSBKCLR
If !Isnull(oAriaApplication.setupedGotFocusBackColor ) And ;
    !Empty(oAriaApplication.setupedGotFocusBackColor) And ;
    Type('eval(oAriaApplication.setupedGotFocusBackColor)')='N'
  loFormSet.ariaForm1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbStyle.txtItem.BackColor = Val(oAriaApplication.setupedGotFocusBackColor)
ELSE
  loFormSet.ariaForm1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbStyle.txtItem.BackColor = 16777215
ENDIF
loFormSet.ariaForm1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbStyle.txtItem.DisabledBackColor=16777215
*!**********************************************************************************
*! Name       : lfPOSTYBKCLR
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 12/18/2014
*! Purpose    : change the style back color if projection = 0 in the PO screen
*! Tracking # : C201645,1
*!**********************************************************************************
FUNCTION lfPOSTYBKCLR
IF !USED('STYLE_COLOR')
  =gfOpenTable("Style","Style",'SH','STYLE_COLOR')
ENDIF
IF gfSeek(loFormSet.ariaForm1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbStyle.Value,'STYLE_COLOR','STYLE') AND STYLE_COLOR.NSTYFRSPRJ = 0
 loFormSet.ariaForm1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbStyle.txtItem.BackColor = 255
 loFormSet.ariaForm1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbStyle.txtItem.DisabledBackColor=255
ELSE
  If !Isnull(oAriaApplication.setupedGotFocusBackColor ) And ;
      !Empty(oAriaApplication.setupedGotFocusBackColor) And ;
     Type('eval(oAriaApplication.setupedGotFocusBackColor)')='N'
    loFormSet.ariaForm1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbStyle.txtItem.BackColor = Val(oAriaApplication.setupedGotFocusBackColor)
  ELSE
    loFormSet.ariaForm1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbStyle.txtItem.BackColor = 16777215
  ENDIF
  loFormSet.ariaForm1.pgfPOStyle.page2.CntDetailFolder.editregion1.kbStyle.txtItem.DisabledBackColor=16777215
ENDIF
*!**********************************************************************************
*! Name       : lfRARSBKCLR
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 12/18/2014
*! Purpose    : change the style back color if projection = 0 in the Return Autherization screen
*! Tracking # : C201645,1
*!**********************************************************************************
FUNCTION lfRARSBKCLR
If !Isnull(oAriaApplication.setupedGotFocusBackColor ) And ;
    !Empty(oAriaApplication.setupedGotFocusBackColor) And ;
    Type('eval(oAriaApplication.setupedGotFocusBackColor)')='N'
  loFormSet.ariaForm1.pageFrame.page2.cntStyle.txtItem.BackColor = Val(oAriaApplication.setupedGotFocusBackColor)
ELSE
  loFormSet.ariaForm1.pageFrame.page2.cntStyle.txtItem.BackColor = 16777215
ENDIF
loFormSet.ariaForm1.pageFrame.page2.cntStyle.txtItem.DisabledBackColor=16777215
*!**********************************************************************************
*! Name       : lfRASTYBKCLR
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 12/18/2014
*! Purpose    : Reset the style back color in the Return Autherization screen
*! Tracking # : C201645,1
*!**********************************************************************************
FUNCTION lfRASTYBKCLR
IF !USED('STYLE_COLOR')
  =gfOpenTable("Style","Style",'SH','STYLE_COLOR')
ENDIF
IF gfSeek(loFormSet.ariaForm1.pageFrame.page2.cntStyle.Value,'STYLE_COLOR','STYLE') AND STYLE_COLOR.NSTYFRSPRJ = 0
 loFormSet.ariaForm1.pageFrame.page2.cntStyle.txtItem.BackColor = 255
 loFormSet.ariaForm1.pageFrame.page2.cntStyle.txtItem.DisabledBackColor=255
ELSE
  If !Isnull(oAriaApplication.setupedGotFocusBackColor ) And ;
      !Empty(oAriaApplication.setupedGotFocusBackColor) And ;
     Type('eval(oAriaApplication.setupedGotFocusBackColor)')='N'
    loFormSet.ariaForm1.pageFrame.page2.cntStyle.txtItem.BackColor = Val(oAriaApplication.setupedGotFocusBackColor)
  ELSE
    loFormSet.ariaForm1.pageFrame.page2.cntStyle.txtItem.BackColor = 16777215
  ENDIF
  loFormSet.ariaForm1.pageFrame.page2.cntStyle.txtItem.DisabledBackColor=16777215
ENDIF
*C201645,1 MMT 12/18/2014 Add triggers to change the style field back color if projection field is 0[T20141121.0002][End]
*C201649,1 MMT 01/27/2015 Add Trigger to check CRM approved order with same con. ref.[T20141113.0005][Start]
*!**********************************************************************************
*! Name       : lfCHKCRMORD
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 01/27/2015
*! Purpose    : Check if there is an unapproved CRM order with con. ref.
*! Tracking # : C201649,1 
*!**********************************************************************************
FUNCTION lfCHKCRMORD
lnOldSelected  = SELECT(0)
IF !USED('Customer_CRM')
  =gfOpenTable("CUSTOMER","CUSTOMER",'SH','Customer_CRM')
ENDIF
lcOrderAccount = SUBSTR(loFormSet.ariaForm1.keyAccount.keytextbox.Value ,1,5)
lcOrderStore = SUBSTR(loFormSet.ariaForm1.keyStore.keytextbox.Value ,1,8)
lcOrderConRef = SUBSTR(loFormSet.ariaForm1.ariapageframe1.page3.txtContRef.Value ,1,30)
IF gfSeek("M"+lcOrderAccount,'Customer_CRM','Customer') AND Customer_CRM.lchkcrmord
  llOrderFound = .F.
  *B611053,1 MMT 09/14/2015 Include Status B in Checking unathorized CRM orders[T20150909.0020][Start]
  *lcSqlStat = "Select * from ordhdr where cordtype = 'T' and Account = '"+lcOrderAccount+"' AND ccontref ='"+lcOrderConRef+"' AND Status = 'U' "+ IIF(!EMPTY(ALLTRIM(lcOrderStore))," AND Store ='"+lcOrderStore+"'",'')  
  lcSqlStat = "Select * from ordhdr where cordtype = 'T' and Account = '"+lcOrderAccount+"' AND ccontref ='"+lcOrderConRef+"' AND Status IN ('U','B') "+ IIF(!EMPTY(ALLTRIM(lcOrderStore))," AND Store ='"+lcOrderStore+"'",'')
  *B611053,1 MMT 09/14/2015 Include Status B in Checking unathorized CRM orders[T20150909.0020][End]
  lnCrmRemoteResult = oAriaApplication.RemoteCompanyData.SQLRun(lcSqlStat ,"OrdhrCrm","ORDHDR",oAriaApplication.ActiveCompanyConStr,3,'BROWSE',SET("Datasession"))
  IF lnCrmRemoteResult >= 1 AND USED("OrdhrCrm") AND RECCOUNT("OrdhrCrm") > 0
    llOrderFound = .T.
  ENDIF
  IF llOrderFound 
    IF gfModalGen('QRM00000B32000',.F.,.F.,.F.,'This order already exists as an unapproved CRM order, are you sure you wish to continue ?') = 1   
      SELECT (lnOldSelected)
      RETURN .T.
    ELSE
      SELECT (lnOldSelected)
      RETURN .f. 
    ENDIF
  ELSE
    SELECT (lnOldSelected)
    RETURN .T.
  ENDIF
ELSE
  SELECT (lnOldSelected)
  RETURN .T.  
ENDIF
*C201649,1 MMT 01/27/2015 Add Trigger to check CRM approved order with same con. ref.[T20141113.0005][End]

*C201723,1 MMT 10/25/2015 Add trigger to Automatic allocation to check pickpack table[T20150908.0008][Start]
*!**********************************************************************************
*! Name       : lfCHKPIKPACK
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 10/25/2015
*! Purpose    : Check if there is Picking ticket without record in PICKPACK Table
*! Tracking # : C201723,1 
*!**********************************************************************************
FUNCTION lfCHKPIKPACK
=gfOpenTable('PIKTKT','PIKTKT','SH','PIKTKUP')
SELECT PIKTKUP
=gfSeek('')
SCAN FOR Status ='O'
  LOCAL lnConnHandler ,lnConnectionHandlar1
  lnConnHandler = 0
  lnConnectionHandlar1= 0
  lcSqlStatment ="Select * from PICKPACK Where PIKTKT='"+PIKTKUP.PIKTKT+"'"
  lnConnectionHandlar1 = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,"TmpPICKPACK",'PICKPACK',;
                  oAriaApplication.ActiveCompanyConStr,3,.F.,SET("Datasession"))                                
  IF lnConnectionHandlar1 >= 1 
    IF RECCOUNT('TmpPICKPACK')==0
      ldPikDate = PIKTKUP.Date
      ldAddDate = PIKTKUP.DADD_date
      lcSqlStatment ="INSERT INTO PICKPACK (PIKTKT ,PIKDATE,CPSTATUS,[Order],cAdd_user,cAdd_TIME,cAdd_ver,DADD_date,Account,STORE,LCOMPPICK) Values"+;
                                          "('"+PIKTKUP.PIKTKT+"',?ldPikDate,'Not Issued','"+PIKTKUP.ORDER+;
                                          "','"+PIKTKUP.cAdd_user+"','"+PIKTKUP.cAdd_TIME+"','"+PIKTKUP.cAdd_ver+"',?ldAddDate,'"+PIKTKUP.Account+"','"+PIKTKUP.STORE+"',0)"
      lnConnectionHandlar1 = oAriaApplication.RemoteCompanyData.sqlrun(lcSqlStatment,"TmpPICKPACK",'PICKPACK',;
                  oAriaApplication.ActiveCompanyConStr,3,.F.,SET("Datasession"),.T.,@lnConnHandler) 
      IF lnConnectionHandlar1 > 0
        TRY
          SQLCOMMIT(lnConnHandler)
        CATCH        
        ENDTRY
       ELSE
         TRY 
          SQLROLLBACK(lnConnHandler)
        CATCH
        ENDTRY 
       ENDIF                                                    
    ENDIF
  ENDIF
ENDSCAN 
=gfCloseTable('PIKTKUP')
*C201723,1 MMT 10/25/2015 Add trigger to Automatic allocation to check pickpack table[T20150908.0008][END]
*C201790,1 MMT 03/09/2016 Send email after adding PO[P20160119.0001 - Entity#7][Start]
*!**********************************************************************************
*! Name       : lfSNDEMAIL
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 03/09/2016
*! Purpose    : Send email from PO screen
*! Tracking # : C201790,1 
*!**********************************************************************************
FUNCTION lfSNDEMAIL
IF !EMPTY(loFormSet.laNewMpos[1])
  loFormSet.ChangeMode('S')
  loFormSet.ariaForm1.cntPoHeader.kbPONo.keytextbox.Value = loFormSet.laNewMpos[1]  
  loFormSet.ariaForm1.cntPoHeader.kbPONo.keytextbox.Valid()
  lnDataSessionID = SET("Datasession")
  SET DATASESSION TO loFormSet.DataSessionID
  IF USED('REP_OBJECT')
    USE IN 'REP_OBJECT'
  ENDIF
  loFormSet.cAssociatedReport = 'POSTYP'
  *C201790,2 MMT 03/15/2016 Error while saving PO[P20160119.0001 - Entity#7][Start]
  *loPOForm.GetAssociatedReport()  
  loFormSet.GetAssociatedReport()
  *C201790,2 MMT 03/15/2016 Error while saving PO[P20160119.0001 - Entity#7][End]
  SET DATASESSION TO lnDataSessionID
  loFormSet.oToolbar.CMDMAIL.Click() 
  loFormSet.ChangeMode('S')     
  loFormSet.cAssociatedReport = ''    
ENDIF    
*!**********************************************************************************
*! Name       : lfADJMAIL
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 03/09/2016
*! Purpose    : Adjust email template and To email
*! Tracking # : C201790,1 
*!**********************************************************************************
FUNCTION lfADJMAIL
IF !EMPTY(loFormSet.laNewMpos[1]) AND TYPE('loMailFormObj') ='O'
  *B611129,1 MMT 03/23/2016 Custom Auto create PO phase#7 send email even if vendor has no email setup[T20160314.0015][Start]
*!*	  loMailFormObj.oHost.llDontSaveLocalCopy = .T.
*!*	  IF !EMPTY(loMailFormObj.cboTemplate.RowSource)
*!*	    loMailFormObj.cboTemplate.Value ="PO Auto Create Template"
*!*	    loMailFormObj.cboTemplate.Valid()
*!*	  ENDIF
  *B611129,1 MMT 03/23/2016 Custom Auto create PO phase#7 send email even if vendor has no email setup[T20160314.0015][END]
  IF !USED('POSHDR_Eml')
    =gfOpenTable('POSHDR','POSHDR','SH','POSHDR_Eml')
  ENDIF
  =gfSeek('PP'+loFormSet.laNewMpos[1],'POSHDR_Eml','POSHDR')
  lcVendor = POSHDR_Eml.Vendor
  lcStatementSelect = "SELECT Email_addresses From  [EMAIL_FORMS] WHERE partner_TYPEs ='V' AND partner_ID ='"+ lcVendor+"' AND [ID] ='POSTYP'"
  lcEmailCursor = gfTempName()
  lnConnectionHandlar1 = oAriaApplication.RemoteCompanyData.sqlrun(lcStatementSelect ,lcEmailCursor,'EMAIL_FORMS',;
                  oAriaApplication.ActiveCompanyConStr,3,.F.,SET("Datasession"))   
  IF lnConnectionHandlar1 > 0                                                 
    SELECT (lcEmailCursor)
    LOCATE 
    IF !EMPTY(ALLTRIM(&lcEmailCursor..Email_addresses))
      *B611129,1 MMT 03/23/2016 Custom Auto create PO phase#7 send email even if vendor has no email setup[T20160314.0015][Start]
      loMailFormObj.oHost.llDontSaveLocalCopy = .T.
      IF !EMPTY(loMailFormObj.cboTemplate.RowSource)
        loMailFormObj.cboTemplate.Value ="PO Auto Create Template"
        loMailFormObj.cboTemplate.Valid()
      ENDIF
      *B611129,1 MMT 03/23/2016 Custom Auto create PO phase#7 send email even if vendor has no email setup[T20160314.0015][End]
      loMailFormObj.txtSendToList.Value = &lcEmailCursor..Email_addresses
    *B611129,1 MMT 03/23/2016 Custom Auto create PO phase#7 send email even if vendor has no email setup[T20160314.0015][Start]
      loMailFormObj.cmdOK.Click  
    ELSE
      RETURN 
    *B611129,1 MMT 03/23/2016 Custom Auto create PO phase#7 send email even if vendor has no email setup[T20160314.0015][End]  
    ENDIF
  ENDIF  
  *B611129,1 MMT 03/23/2016 Custom Auto create PO phase#7 send email even if vendor has no email setup[T20160314.0015][Start]
  *loMailFormObj.cmdOK.Click  
  *B611129,1 MMT 03/23/2016 Custom Auto create PO phase#7 send email even if vendor has no email setup[T20160314.0015][End]
ENDIF  
*C201790,1 MMT 03/09/2016 Send email after adding PO[P20160119.0001 - Entity#7][End]

*C201809,1 MMT 04/21/2016 Calculate Style price in Return auth. screen from CSTPRICE[T20160324.0006][Start]  
*!**********************************************************************************
*! Name       : lfCALCPRCE
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 04/21/2016 
*! Purpose    : Get Style price from CSTPRICE
*! Tracking # : C201809,1 
*!**********************************************************************************
FUNCTION lfCALCPRCE
IF !EMPTY(RETAUTH.INVOICE)
  RETURN 
ENDIF

lnCustPrice = 0
IF !USED('Customer_A')
  =gfOpenTable('Customer','Customer','SH','Customer_A')
ENDIF

IF !USED('SCALE_A')
  =gfOpenTable('SCALE','SCALE','SH','SCALE_A')
ENDIF

IF !USED('STYLE_A')
  =gfOpenTable('STYLE','STYLE','SH','STYLE_A')
ENDIF

IF !USED('CSTPRICE')
  gfOpenTable(oAriaApplication.dataDir+'CSTPRICE','CSTPRICE','SH')
ENDIF
=gfSeek('M'+RETAUTH.ACCOUNT ,'Customer_A','Customer')
lcPriceCode = Customer_A.PRICCODE
lcTmpLnFile = loFormSet.Prg.LCTMPRETLN
SELECT (loFormSet.Prg.LCTMPRETLN)	

*C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]	 
*SCAN FOR CSTATUS = "A" AND !DELETED() AND EMPTY(cOwner)
lnCurreNO = RECNO(lcTmpLnFile)
TRY
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE1 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE2 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE3 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE4 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE5 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE6 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE7 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE8 L
loFormSet.Prg.LFBRLINE()
CATCH
ENDTRY
IF BETWEEN(lnCurreNO ,1,RECCOUNT(lcTmpLnFile))
  GO RECORD lnCurreNO  IN (lcTmpLnFile)
ENDIF
*****
REPLACE LSIZE1 WITH .F.,;
        LSIZE2 WITH .F.,;
        LSIZE3 WITH .F.,;
        LSIZE4 WITH .F.,;               
        LSIZE5 WITH .F.,;
        LSIZE6 WITH .F.,;
        LSIZE7 WITH .F.,;
        LSIZE8 WITH .F.
*C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
  SCATTER MEMO MEMVAR 

  lnCurreNO = RECNO(lcTmpLnFile)
  LCSTYLE = STYLE
  =gfSeek(LCSTYLE,'STYLE_A','STYLE')
  =gfSeek('S'+STYLE_A.Scale,'SCALE_A','SCALE')
  *STORE 0 TO lnPric1,lnPric2,lnPric3,lnPric4,lnPric5,lnPric6,lnPric7,lnPric8
  DIMENSION laPrices[8]
  laPrices = 0
  STORE .F. TO llSize1,llSize2,llSize3,llSize4,llSize5,llSize6,llSize7,llSize8
  SELECT CSTPRICE
  =gfSEEK(lcPriceCode)
  LOCATE REST WHILE priccode = lcPriceCode FOR STYDV = LCSTYLE
  IF FOUND()
    FOR lnB = 1 TO SCALE_A.Cnt
      lcB = STR(lnB,1)
      llSize&lcB. = .T.
      laPrices[lnB] = IIF(CSTPRICE.PRICE&lcB.<>0,CSTPRICE.PRICE&lcB.,IIF(CSTPRICE.PRICEDV<>0  ,CSTPRICE.PRICEDV,0))
      IF RETAUTH.CCURRCODE <> CSTPRICE.ccurrcod
        STORE '/' TO lcExSign,lcUntSin
        lcExSign = gfGetExSin(@lcUntSin, CSTPRICE.ccurrcod)
        lnUnit = 1
        LNEXRATE = GFCHKRATE('lnUnit' , CSTPRICE.ccurrcod, RetAuth.radate, .F.) 
        lnExRate   = IIF(LNEXRATE=0,1,LNEXRATE)
        lnCurrUnit = IIF(lnUnit =0,1,lnUnit)
        lnBaseAmt =  laPrices[lnB] &lcExSign lnExRate &lcUntSin lnCurrUnit
        IF RETAUTH.CCURRCODE <> oAriaApplication.BaseCurrency
          STORE '/' TO lcExSign,lcUntSin
          lcExSign = gfGetExSin(@lcUntSin, RETAUTH.CCURRCODE)
          lnExRate   = IIF(RETAUTH.nExRate=0,1,RETAUTH.nExRate)
          lnCurrUnit = IIF(RETAUTH.nCurrUnit=0,1,RETAUTH.nCurrUnit)
          lcExSign = IIF(lcExSign ='*','/','*')
          lcUntSin = IIF(lcUntSin ='*','/','*')
          laPrices[lnB] = lfGetPriceAfterDiscount(lnBaseAmt &lcExSign lnExRate &lcUntSin lnCurrUnit)
        ELSE
          laPrices[lnB] = lfGetPriceAfterDiscount(lnBaseAmt )
        ENDIF
      ELSE
        laPrices[lnB] = lfGetPriceAfterDiscount(laPrices[lnB]) 
      ENDIF
    ENDFOR  
    IF laPrices[1] <> 0 OR laPrices[2] <> 0 OR laPrices[3] <> 0 OR laPrices[4] <> 0 OR laPrices[5] <> 0  OR laPrices[6] <> 0 OR laPrices[7] <> 0 OR laPrices[8] <> 0  
      lnFirstPrice = laPrices[1]
      llAllSamePrice = .T.
      FOR lnX = 2 To  Scale_A.cnt
        IF laPrices[lnX] <> lnFirstPrice
          llAllSamePrice = .F.
          EXIT
        ENDIF
      ENDFOR 
      IF llAllSamePrice 
        REPLACE PRICE  WITH lnFirstPrice ,;
		        AMOUNT  WITH TOTQTY * PRICE       IN (lcTmpLnFile)
    		*C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
        REPLACE LSIZE1 WITH .T.,;
		        LSIZE2 WITH .T.,;
		        LSIZE3 WITH .T.,;
		        LSIZE4 WITH .T.,;               
		        LSIZE5 WITH .T.,;
		        LSIZE6 WITH .T.,;
		        LSIZE7 WITH .T.,;
		        LSIZE8 WITH .T. IN (lcTmpLnFile)
 	      *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
      ELSE
        * Need to group each list of sizes have the same price 
        lnFirstPrice = 0
        lcIncSize = ""
        DIMENSION laPrcGroup[1,2]
        laPrcGroup =''
        FOR lnA =  1 TO SCALE_A.cnt
          IF STR(lnA,1) $ lcIncSize
            LOOP 
          ENDIF
          DIMENSION laPrcGroup[ALEN(laPrcGroup,1)+1,2]
          laPrcGroup[ALEN(laPrcGroup,1),1] = STR(lnA,1)
		      laPrcGroup[ALEN(laPrcGroup,1),2] = laPrices[lnA]
		      lcIncSize = lcIncSize + STR(lnA,1)
          FOR lnB =  1 TO SCALE_A.cnt
            IF STR(lnB,1) $ lcIncSize
              LOOP 
            ENDIF
            IF laPrices[lnA] = laPrices[lnB]
              laPrcGroup[ALEN(laPrcGroup,1),1] = laPrcGroup[ALEN(laPrcGroup,1),1] +","+  STR(lnB,1)
              lcIncSize = lcIncSize + STR(lnB,1)
            ENDIF
          ENDFOR 
        ENDFOR 
        SELECT (loFormSet.Prg.LCTMPRETLN)	
       
        llFirstLine = .T.
        FOR lnV = 1 TO 8
          lcV = STR(lnV,1)
          REPLACE QTY&lcV. WITH 0,;
                  NOPNQTY&lcV. WITH 0
        ENDFOR  
        REPLACE TotQty WITH 0,;
                NTOTOPNQTY WITH 0
        lnQtyTotal = 0        
        FOR lnCn = 1 TO ALEN(laPrcGroup,1)
          IF EMPTY(laPrcGroup [lnCn,1])
            LOOP
          ENDIF
          DIMENSION laSizesArr[1]
          laSizesArr = '' 
          =gfSubstr(laPrcGroup [lnCn,1],@laSizesArr ,',')
          llAddLineNew = .T.
          FOR lnInC =  1 TO ALEN(laSizesArr ,1)
            IF EMPTY(laSizesArr[lnInC])
              LOOP
            ENDIF
            lcSZ = ALLTRIM(laSizesArr[lnInC])
            IF llFirstLine  
              REPLACE QTY&lcSZ WITH m.QTY&lcSZ,;
                      NOPNQTY&lcSZ. WITH   m.QTY&lcSZ,;
                      TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
                      NTOTOPNQTY WITH NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8,;
                      Price WITH laPrcGroup [lnCn,2],;
                      AMOUNT  WITH TOTQTY * PRICE       
              lnQtyTotal = lnQtyTotal + TOTQTY 
              *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
              REPLACE LSIZE&lcSZ WITH .T.
              *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
            ELSE
              *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
              *IF llAddLineNew AND m.QTY&lcSZ > 0
               IF llAddLineNew
              *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
                loFormSet.Prg.LNRA_LINNO = loFormSet.Prg.LNRA_LINNO + 1
                loFormSet.Prg.LNNOOFLINES = loFormSet.Prg.LNNOOFLINES + 1
                m.CRA_LINNO =  ALLTRIM(STR(loFormSet.Prg.LNRA_LINNO))
                *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
                *m.cOwner = 'Aria'
                *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
                INSERT INTO (loFormSet.Prg.LCTMPRETLN) FROM MEMVAR 
                llAddLineNew = .F.
                FOR lnV = 1 TO 8
  		          lcV = STR(lnV,1)
		            REPLACE QTY&lcV. WITH 0,;
		                NOPNQTY&lcV. WITH 0
  		        ENDFOR  
		            REPLACE TotQty WITH 0,;
		                NTOTOPNQTY WITH 0
              ENDIF  
		          REPLACE QTY&lcSZ WITH m.QTY&lcSZ,;
                      NOPNQTY&lcSZ. WITH m.QTY&lcSZ,;
                      TotQty WITH Qty1+Qty2+Qty3+Qty4+Qty5+Qty6+Qty7+Qty8,;
                      NTOTOPNQTY WITH NOPNQTY1+NOPNQTY2+NOPNQTY3+NOPNQTY4+NOPNQTY5+NOPNQTY6+NOPNQTY7+NOPNQTY8,;
                      Price WITH laPrcGroup [lnCn,2],;
                      AMOUNT  WITH TOTQTY * PRICE  
              *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
              REPLACE LSIZE&lcSZ WITH .T.
              *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
            ENDIF
          ENDFOR 
          *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
          *IF lnQtyTotal > 0
          *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
          llFirstLine = .F.
          *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
          *ENDIF 
          *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
        ENDFOR 
      ENDIF 
    ENDIF  
  ELSE
    lnCustPrice  = lfGetPriceAfterDiscount(loFormSet.Prg.LFGETPRICE(&lcTmpLnFile..STYLE,loFormSet.Prg.LCCUSTPLVL,&lcTmpLnFile..TOTQTY) )
    REPLACE PRICE  WITH lnCustPrice ,;
            AMOUNT  WITH TOTQTY * PRICE IN (lcTmpLnFile)
    *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
    REPLACE LSIZE1 WITH .T.,;
            LSIZE2 WITH .T.,;
            LSIZE3 WITH .T.,;
            LSIZE4 WITH .T.,;               
            LSIZE5 WITH .T.,;
            LSIZE6 WITH .T.,;
            LSIZE7 WITH .T.,;
            LSIZE8 WITH .T. IN (lcTmpLnFile)
    *C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
  ENDIF
  IF BETWEEN(lnCurreNO ,1,RECCOUNT(lcTmpLnFile))
    GO RECORD lnCurreNO  IN (lcTmpLnFile)
  ENDIF
*C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
*ENDSCAN
*!*	SELECT (lcTmpLnFile)
*!*	LOCATE
*!*	REPLACE cOwner WITH '' ALL 
*!*	LOCATE
*!*	DELETE FOR TotQty = 0 
*C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
*!**********************************************************************************
*! Name       : lfGetPriceAfterDiscount
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 04/21/2016 
*! Purpose    : Get Style price discount
*! Tracking # : C201809,1 
*!**********************************************************************************
FUNCTION lfGetPriceAfterDiscount
LPARAMETERS lnPrice
lnNewPrice =lnPrice 
IF !EMPTY(STYLE_A.CDISCCODE)
  *-- Get the discount percentage for this discount code from the codes file.
  LCDISCCODE = STYLE_A.CDISCCODE
  DECLARE LASTYDISC[1,2]
  lnDiscPCnt = 0
  LASTYDISC[1,1] = 'DISCPCNT'
  LASTYDISC[1,2] = 'lnDiscPCnt'
  =GFRLTFLD(LCDISCCODE , @LASTYDISC , "CDISCCODE")
  *-- If the discount percentage greater than zero...
  IF LNDISCPCNT > 0 && Private
    *LCTMPSTR = ALLTRIM(STR(THIS.LNDISCPCNT)) +"|" + ALLTRIM(STR(THIS.LNPRICE,10,3)) + ;
        "|" + ALLTRIM(STR(THIS.LNPRICE * THIS.LNDISCPCNT /100,10,3)) + "|" + ;
        ALLTRIM(STR(THIS.LNPRICE - (THIS.LNPRICE * THIS.LNDISCPCNT / 100),10,3))
    *** There is discount percentage on this style : {lnDiscPcnt} ***
    *** Gross Price is : {lnPrice}
    *** Discount amount is : {lnPrice * lnDiscPcnt /100}
    *** Net Price is : {lnPrice - (lnPrice * lnDiscPcnt / 100)}
    *=GFMODALGEN("INM46030B00000" , "DIALOG" , LCTMPSTR)
    *-- Get a discount on the total gross price, & inform the user.
     lnNewPrice  = lnNewPrice  - (lnNewPrice  * LNDISCPCNT / 100)
   ENDIF && This.lnDiscPCnt > 0
ENDIF &&
RETURN lnNewPrice  
*C201809,1 MMT 04/21/2016 Calculate Style price in Return auth. screen from CSTPRICE[T20160324.0006][End]  

*C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][Start]   
*!**********************************************************************************
*! Name       : lfADDCSFLD
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 05/26/2016 
*! Purpose    : Add flags to Enable/Disable Qty fields 
*! Tracking # : C201809,2
*!**********************************************************************************
FUNCTION lfADDCSFLD
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE1 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE2 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE3 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE4 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE5 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE6 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE7 L
ALTER table (OARIAAPPLICATION.WORKDIR + loFormSet.Prg.LCTMPRETLN) ADD COLUMN  LSIZE8 L
*!**********************************************************************************
*! Name       : lfREFFSIZ
*! Developer  : Mariam Mazhar{MMT}
*! Date       : 05/26/2016 
*! Purpose    : Enable/Disable Qty fields 
*! Tracking # : C201809,2
*!**********************************************************************************
FUNCTION lfREFFSIZ
TRY
  IF loFormSet.ActiveMode $ 'EA' AND !EMPTY(EVALUATE(loFormSet.Prg.LCTMPRETLN+'.Style')) AND EVALUATE(loFormSet.Prg.LCTMPRETLN+'.cStatus')='A'
    FOR lnY = 1 TO 8
      lcY = STR(lnY,1)
      loFormSet.Prg.LOFORM.PAGEFRAME.PAGE2.CNTSIZES.txtQty&lcY..Enabled = EVALUATE(loFormSet.Prg.LCTMPRETLN+'.lSize'+lcY)
    ENDFOR
  ENDIF 
CATCH
ENDTRY
*C201809,2 MMT 05/26/2016 Calculate Style price in Return auth. screen from CSTPRICE while adding lines[T20160324.0006][End]   
*C201930,1 Sara.O 01/30/2017 Update Price with Uplift %[P20170117.0001][Start]
*!**********************************************************************************
*! Name       : lfUPDPRCUPLF
*! Developer  : Sara Osama
*! Date       : 01/30/2017 
*! Purpose    : Update Price with Uplift %
*! Tracking # : C201930
*!**********************************************************************************
FUNCTION lfUPDPRCUPLF

*C201930,2 MMT 02/13/2017 Update Price with Uplift %[P20170117.0001][Start]
*!*	lcLineFile = loFormSet.oFormEnvironment.lcOrdLine
*!*	IF !EMPTY(&lcLineFile..store)
*!*		IF !USED('TEMPCUSTOMER')
*!*		  =gfOpenTable('CUSTOMER','CUSTOMER','SH','TEMPCUSTOMER')
*!*	    ENDIF 
*!*		IF gfSEEK('S'+&lcLineFile..Account+&lcLineFile..Store,'TEMPCUSTOMER') 
*!*		    *x
*!*			*IF TEMPCUSTOMER.nbrkweight > 0
*!*			IF TEMPCUSTOMER.nbrkweight > 0 AND ALLTRIM(&lcLineFile..cedt_ver) <> "BRW"
*!*			*X
*!*				Replace Gros_price WITH Gros_price +( Gros_price *(TEMPCUSTOMER.nbrkweight/100)) IN (lcLineFile)
*!*				Replace Price WITH Gros_price - ( Gros_price *(disc_pcnt/100)) IN (lcLineFile)
*!*				*X
*!*				REPLACE cedt_ver WITH "BRW" IN (lcLineFile)
*!*				*X
*!*	   	        loFormSet.AriaForm1.AriaPageframe1.Page2.Ariaeditregion1.txtGrossPrice.Refresh()
*!*				loFormSet.AriaForm1.AriaPageframe1.Page2.Ariaeditregion1.txtNetPrice.Refresh()
*!*			ENDIF 
*!*		ENDIF 
*!*	ENDIF
IF loFormSet.ActiveMode $ 'VS'
  RETURN
ENDIF
lcLineFile = loFormSet.oFormEnvironment.lcOrdLine
lcHdrFile = loFormSet.oFormEnvironment.lcOrdHdr
IF !EMPTY(&lcLineFile..store)
  IF !USED('TEMPCUSTOMER')
	 =gfOpenTable('CUSTOMER','CUSTOMER','SH','TEMPCUSTOMER')
  ENDIF 
  IF gfSEEK('S'+&lcLineFile..Account+&lcLineFile..Store,'TEMPCUSTOMER') 
    *C201930,3 MMT 02/20/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][Start]
*!*	    IF !EMPTY(ALLTRIM(cOwner)) 
*!*		  Replace Gros_price WITH VAL(ALLTRIM(cOwner)) IN (lcLineFile)
    IF !EMPTY(ALLTRIM(clok_time)) 
	  Replace Gros_price WITH VAL(ALLTRIM(clok_time)) IN (lcLineFile)
	*C201930,4 MMT 02/21/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][Start]
	ELSE
	  REPLACE clok_time WITH STR(Gros_price ,8,2) IN (lcLineFile)
	*C201930,4 MMT 02/21/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][End]  
	*C201930,3 MMT 02/20/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][End]
	ENDIF
	REPLACE bookamt WITH bookAmt - (&lcLineFile..Price * &lcLineFile..TotBook) in (lcHdrFile)
	REPLACE OpenAmt WITH OpenAmt  - (&lcLineFile..Price * &lcLineFile..TotQty) in (lcHdrFile)
	*C201930,4 MMT 02/21/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][Start]
	*REPLACE cOwner WITH STR(Gros_price ,12,2) IN (lcLineFile)
	*C201930,4 MMT 02/21/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][Start]
	*REPLACE clok_time WITH STR(Gros_price ,12,2) IN (lcLineFile)
	*C201930,4 MMT 02/21/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][End]
	*C201930,4 MMT 02/21/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][End]
	Replace Gros_price WITH Gros_price +( Gros_price *(TEMPCUSTOMER.nbrkweight/100)) IN (lcLineFile)
	REPLACE cedt_ver WITH 'BKW' IN (lcLineFile)
    Replace Price WITH Gros_price - ( Gros_price *(disc_pcnt/100)) IN (lcLineFile)
	REPLACE bookamt WITH bookAmt + (&lcLineFile..Price * &lcLineFile..TotBook) in (lcHdrFile)
	REPLACE OpenAmt WITH OpenAmt + (&lcLineFile..Price * &lcLineFile..TotQty) in (lcHdrFile)
    loFormSet.AriaForm1.AriaPageframe1.Page2.Ariaeditregion1.txtGrossPrice.Refresh()
    loFormSet.AriaForm1.AriaPageframe1.Page2.Ariaeditregion1.txtNetPrice.Refresh()
  ENDIF 
ENDIF
*C201930,2 MMT 02/13/2017 Update Price with Uplift %[P20170117.0001][End]
*!**********************************************************************************
*! Name       : lfUPDPRUPLFQ
*! Developer  : Sara Osama
*! Date       : 01/31/2017 
*! Purpose    : Update Price with Uplift % in Quick order
*! Tracking # : C201930
*!**********************************************************************************
FUNCTION lfUPDPRUPLFQ
lcLineFile = loFormSet.oFormEnvironment.lcOrdLine
lcHdrFile = loFormSet.oFormEnvironment.lcOrdHdr
IF !EMPTY(&lcHdrFile..store)
	IF !USED('TEMPCUSTOMERQ')
	  =gfOpenTable('CUSTOMER','CUSTOMER','SH','TEMPCUSTOMERQ')
	ENDIF 
	IF gfSEEK('S'+&lcHdrFile..Account+&lcHdrFile..Store,'TEMPCUSTOMERQ') 
		IF TEMPCUSTOMERQ.nbrkweight > 0
			m.Gros_price = m.Gros_price +( m.Gros_price *(TEMPCUSTOMERQ.nbrkweight/100)) 
		ENDIF 
	ENDIF 
ENDIF
*C201930,1 Sara.O 01/30/2017 Update Price with Uplift %[P20170117.0001][End]
*C201930,2 MMT 02/13/2017 Update Price with Uplift %[P20170117.0001][Start]
*!**********************************************************************************
*! Name       : lfUPDORDSTR
*! Developer  : MMT
*! Date       : 02/13/2017 
*! Purpose    : Update Price with Uplift % in Store Validation
*! Tracking # : C201930
*!**********************************************************************************
FUNCTION lfUPDORDSTR
lcLineFile = loFormSet.oFormEnvironment.lcOrdLine
lcHdrFile = loFormSet.oFormEnvironment.lcOrdHdr
*C201930,3 MMT 02/20/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][Start]
IF !USED(lcLineFile)
  RETURN
ENDIF
*C201930,3 MMT 02/20/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][End]
SELECT (lcLineFile)
lnRecNumL = RECNO(lcLineFile)
LOCATE 
IF !EOF()
  IF !USED('TEMPCUSTOMER')
	=gfOpenTable('CUSTOMER','CUSTOMER','SH','TEMPCUSTOMER')
  ENDIF 
  IF gfSEEK('S'+&lcHdrFile..Account+&lcHdrFile..Store,'TEMPCUSTOMER') 
    SELECT (lcLineFile)
    *C201930,3 MMT 02/20/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][Start]
    *SCAN FOR !EMPTY(&lcLineFile..cOwner)    
    SCAN FOR !EMPTY(&lcLineFile..clok_time)
    *C201930,3 MMT 02/20/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][End]
      REPLACE bookamt WITH bookAmt - (&lcLineFile..Price * &lcLineFile..TotBook) in (lcHdrFile)
	  REPLACE OpenAmt WITH OpenAmt  - (&lcLineFile..Price * &lcLineFile..TotQty) in (lcHdrFile)
      *C201930,4 MMT 02/21/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][Start]
      *Replace Gros_price WITH VAL(&lcLineFile..cOwner) +(VAL(&lcLineFile..cOwner) *(TEMPCUSTOMER.nbrkweight/100)) IN (lcLineFile)
      Replace Gros_price WITH VAL(&lcLineFile..clok_time) +(VAL(&lcLineFile..clok_time) *(TEMPCUSTOMER.nbrkweight/100)) IN (lcLineFile)
      *C201930,4 MMT 02/21/2017 Update Price with Uplift % in SO screen does not work[P20170117.0001-Issue#1][End]
      REPLACE cedt_ver WITH 'BKW' IN (lcLineFile)
      Replace Price WITH Gros_price - ( Gros_price *(disc_pcnt/100)) IN (lcLineFile)

 	  REPLACE bookamt WITH bookAmt + (&lcLineFile..Price * &lcLineFile..TotBook) in (lcHdrFile)
	  REPLACE OpenAmt WITH OpenAmt + (&lcLineFile..Price * &lcLineFile..TotQty) in (lcHdrFile)
    ENDSCAN
  ENDIF 
  IF BETWEEN(lnRecNumL ,1,RECCOUNT(lcLineFile))
    GO RECORD lnRecNumL  IN (lcLineFile)
    loFormSet.ariaform1.ariapageframe1.page2.Ariagrid1.AfterRowColChange()
  ENDIF   
ENDIF
*C201930,2 MMT 02/13/2017 Update Price with Uplift %[P20170117.0001][End]

*C202185,1 MMT 06/27/2018 Add trigger to credit memo saving program to update reference[T20180619.0002][Start]
*!**********************************************************************************
*! Name       : lfUPCRDTREF
*! Developer  : Mariam Mazhar (MMT)
*! Date       : 06/27/2019 
*! Purpose    : Update Credit.Reference field with SO#
*! Tracking # : C202185
*!**********************************************************************************
FUNCTION lfUPCRDTREF
SELECT (oBusObj.CREDIT.lcCursorUpdate)
REPLACE REFERENCE WITH 'R/A# '+&lcCrMemHdr..RaNo+IIF(!Empty(&lcCrMemHdr..Order)," - SO#"+&lcCrMemHdr..Order,'')
*C202185,1 MMT 06/27/2018 Add trigger to credit memo saving program to update reference[T20180619.0002][End]