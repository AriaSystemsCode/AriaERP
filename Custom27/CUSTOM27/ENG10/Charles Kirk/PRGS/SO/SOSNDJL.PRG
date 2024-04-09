*:****************************************************************************
*: Program file        : SOSNDJL.PRG
*: Program description : Export Sales Orders to  John Lewis           
*: For System          : Aria Advantage Series - Version 2.7
*: For Module          : System Manager - (SO2)
*: Developer Name      : TMI TAREK MOHAMED IBRAHIM
*: Tracking Job Number : C200574
*:****************************************************************************
*modifications
*B607492,1  TMI [Start] seek by the new added field ORDLINE.PACKID instead of style, since a style may have more than one pack id
*:***************************************************************************

*--Defining syrepuvr variables


PRIVATE lcSlctdAcc,lcOldValue
STORE '' TO lcRpSndFil,lcSlctdAcc,lcTmpOrd,lcOldValue

*-- Create a cursor that is used in filling the finally output text file
lcTmpOrd = gfTempName()
CREATE TABLE &gcDataDir.&lcTmpOrd. (ACCOUNT C(5),ORDER C(6))
INDEX ON ACCOUNT+ORDER TAG &lcTmpOrd

*--Calling the option grid
lcExpr = gfOpGrid('SOSNDJL' , .T.)

*--IF close button pressed , exit
IF EMPTY(lcExpr)
  RETURN
ENDIF

*-- Export data to ASCII file
=lfExpToAsc()

*-- Start exporting orders


*:**************************************************************************
*:* Name        : lfwRpWhen() 
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 06/17/2001
*:* Purpose     : When OG Function
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Passed Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfwRpWhen() ()
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
*:* Date        : 07/09/2003
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
    lcRpSndFil = CHRTRAN(lcRpSndFil,' ','')
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
*:* Date        : 07/09/2003
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
PRIVATE lcStat,lnOrdPos,lnAccPos,lnOrdRec,lnAccRec 

lnOrdPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDHDR.ORDER'),1)
lnAccPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT'),1)

STORE 0 TO lnOrdRec,lnAccRec
*IF USED(laOgFxFlt[lnOrdPos,6])
*  SELECT (laOgFxFlt[lnOrdPos,6])
*  COUNT FOR !DELETED() AND !EMPTY(ORDER) TO lnOrdRec
*ENDIF
IF USED(laOgFxFlt[lnAccPos,6])
  SELECT (laOgFxFlt[lnAccPos,6])
  COUNT FOR !DELETED() AND !EMPTY(ACCOUNT) TO lnAccRec
ENDIF  
*IF lnOrdRec>0 AND lnAccRec>0 AND !EMPTY(lcRpSndFil)
IF lnAccRec>0 AND !EMPTY(lcRpSndFil)
  SHOW GET pbRun ENABLE
*  SELECT (laOgFxFlt[lnOrdPos,6])
*  GO TOP
*  COPY TO &gcDataDir.&lcTmpOrd
ELSE  
  SHOW GET pbRun DISABLE
ENDIF
*-- end of lfvChkRnBt.

*:**************************************************************************
*:* Name        : lfChkTmpOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/13/2003
*:* Purpose     : Check lcTmpOrd file
*:***************************************************************************
*:* Called from : called lfvChkRnBt
*:***************************************************************************
FUNCTION lfChkTmpOrd
PRIVATE lnOrdPos,lnAccPos,lcSvOrd
lcSvOrd = ORDER('ORDHDR')
SET ORDER TO ORDHDR IN ORDHDR

lnOrdPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDHDR.ORDER'),1)
lnAccPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT'),1)

*-- Rebuild "lcTmpOrd" records  
SELECT &lcTmpOrd
ZAP
IF USED(laOgFxFlt[lnAccPos,6])
  SELECT (laOgFxFlt[lnAccPos,6])
  SCAN
    INSERT INTO (lcTmpOrd) VALUES (EVAL(laOgFxFlt[lnAccPos,6]+'.ACCOUNT'),'ALL')
  ENDSCAN
ENDIF
IF USED(laOgFxFlt[lnOrdPos,6])  
  SELECT (laOgFxFlt[lnOrdPos,6])
  SCAN
    =SEEK('O'+EVAL(laOgFxFlt[lnOrdPos,6]+'.ORDER'),'ORDHDR')
    IF SEEK(ORDHDR.ACCOUNT+'ALL',lcTmpOrd)
      REPLACE &lcTmpOrd..ORDER WITH ORDHDR.ORDER
    ELSE
      INSERT INTO (lcTmpOrd) VALUES (ORDHDR.ACCOUNT,ORDHDR.ORDER)
    ENDIF
  ENDSCAN   
ENDIF

SET ORDER TO &lcSvOrd IN ORDHDR
*-- end of lfChkTmpOrd.

*:**************************************************************************
*:* Name        : lfvSlctdAcc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/2003
*:* Purpose     : Set/rest/valid funtion for Accounts Browse
*                 Get selected accounts to used as a filter for selected orders
*                 also clear selected orders if not for the new selected accounts
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
PRIVATE lnAlias,lnAccPos,lnOrdPos
lnAlias = SELECT()

DO CASE
  CASE lcSR = 'S'
    GO TOP IN CUSTOMER

  CASE lcSR = 'R'
    *-- Get the selected accounts in a memory variable to be used in browse filter of orders
    lcSlctdAcc = ''
  
    lnAccPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'CUSTOMER.ACCOUNT'),1)
    SELECT (laOgFxFlt[lnAccPos,6])
    GO TOP
    SCAN
      lcSlctdAcc = lcSlctdAcc + EVAL(laOgFxFlt[lnAccPos,6]+'.ACCOUNT') + ' | '
    ENDSCAN
    
    *-- Clear orders not for the selected accounts 
    *lcDelStat = SET('DELETED')
    *SET DELETED OFF
    
    lnOrdPos = ASUBSCRIPT(laOgFxFlt,ASCAN(laOgFxFlt,'ORDHDR.ORDER'),1)
    IF USED(laOgFxFlt[lnOrdPos,6])
      SELECT (laOgFxFlt[lnOrdPos,6])
      GO TOP
      SCAN
        =SEEK( 'O'+EVAL(laOgFxFlt[lnOrdPos,6]+'.ORDER') , 'ORDHDR' )
        IF !SEEK(ORDHDR.ACCOUNT,(laOgFxFlt[lnAccPos,6]))
          DELETE
        ENDIF
      ENDSCAN
    ENDIF    
    *SET DELETED &lcDelStat    
    
    *-- Recheck Run btn status
    =lfvChkRnBt()
    =lfChkTmpOrd()
    
ENDCASE

SELECT (lnAlias)

*-- end of lfvSlctdAcc.

*:**************************************************************************
*:* Name        : lfsrvOrd
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/2003
*:* Purpose     : Set-reset-valid funtion for Order browse
*:***************************************************************************
*:* Called from : OG
*:***************************************************************************
FUNCTION lfsrvOrd
PARAMETERS lcSRV
IF lcSRV = 'R'
  *-- Recheck Run btn status
  =lfvChkRnBt()  
  =lfChkTmpOrd()
ENDIF
*-- end of lfsrvOrd.

*:**************************************************************************
*:* Name        : lfExpToAsc
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/2003
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
PRIVATE lnOrdPos,lnHndl,lcWrtStr,lnI,lcSz,lcTmpSnd,lcSvErr,llDummy,lnClrPos,lnClrLen

*-- Create a temp file for output and finally if created the user is asked to save or not
lcTmpSnd = gcWorkDir+gfTempName()
lcWrtStr = ''
lnHndl = FCREATE(lcTmpSnd)
IF lnHndl <= 0
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,'Can not create the file to be sent.')
  RETURN
ENDIF

*-- The text file is created  , continue filling it
=gfOpenFile(gcDataDir+'ORDHDR','ORDACCT','SH')
=gfOpenFile(gcDataDir+'ORDLINE','ORDLINE','SH')
=gfOpenFile(gcDataDir+'STYLE','STYLE','SH')
=gfOpenFile(gcDataDir+'SPCK_LIN','SPCKLINS','SH')  && Key :: TYPE+ACCOUNT+STYLE+PACK_ID
=gfOpenFile(gcDataDir+'SPCK_HDR','SKU_STYLE','SH')  && Key :: TYPE+ACCOUNT+STYLE+PACK_ID
=gfOpenFile(gcDataDir+'SCALE','SCALE','SH')
=gfOpenFile(gcDataDir+'SkuTmpl','SkuTmpl','SH')
=gfOpenFile(gcDataDir+'CUSTOMER','CUSTOMER','SH')

*- Define variables hold color len and position
STORE 0  TO lnClrPos,lnClrLen
=lfGetClrD()

*B607492,1  TMI [Start] Change the order in SPCK_HDR,_LIN FILES
SET ORDER TO SPCK_HDR IN SPCK_HDR
SET ORDER TO SPCK_LIN IN SPCK_LIN
*B607492,1  TMI [End  ] 

SELECT &lcTmpOrd
GO TOP
SCAN
  SELECT ORDHDR  &&Key is ORDACCT :: ACCOUNT+CORDTYPE+ORDER
  =SEEK(&lcTmpOrd..ACCOUNT+'O'+IIF(&lcTmpOrd..ORDER='ALL','',&lcTmpOrd..ORDER),'ORDHDR')
  SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcTmpOrd..ACCOUNT+'O'+;
                                           IIF(&lcTmpOrd..ORDER='ALL','',&lcTmpOrd..ORDER) ;
                                        FOR !LORDSNTJL
    *C200574,4  TMI [Start] Update field 5 as per Tony
    lcWrtStr = ["HDR",]+;
               [502863,]+;
               ["]+ALLT(ORDHDR.CUSTPO)+[",]+;
               lfGtDt(ORDHDR.ENTERED)+[,]+;
               IIF(EMPTY(ORDHDR.CPRTACK),[3100],[3130])+[,]+;
               ["]+ORDHDR.CPRTACK+[",]+;
               lfGtDt(ORDHDR.COMPLETE)               
    *C200574,4  TMI [End  ]            
    =FPUTS(lnHndl,lcWrtStr)
    
    =SEEK(ORDHDR.CORDTYPE+ORDHDR.ORDER,'ORDLINE')
    SELECT ORDLINE
    SCAN REST WHILE CORDTYPE+ORDER+STR(LINENO,6) = ORDHDR.CORDTYPE+ORDHDR.ORDER FOR ORDLINE.TOTQTY > 0
      =SEEK(ORDLINE.STYLE,'STYLE')
      =SEEK('S'+STYLE.SCALE,'SCALE')
      FOR lnI = 1 TO SCALE.CNT
        lcSz = STR(lnI,1)
        IF ORDLINE.QTY&lcSz > 0
          lcWrtStr = ["OAD",]+;
                     ["]+lfGtSku(lcSz)+[",]+;
                     [" ",]+;
                     ["]+ALLT(STYLE.CVENSTY)+[",]+;
                     [1,]+;
                     ALLT(STR(ORDLINE.QTY&lcSz))+[,]+;
                     [" ",]+;
                     ALLTRIM(STR(OrdLine.Price,12,2))
                     
          =FPUTS(lnHndl,lcWrtStr)
        ENDIF
      ENDFOR
    ENDSCAN
  ENDSCAN
ENDSCAN

=FCLOSE(lnHndl)

*-- Button  32000 :\<Yes;\?\<No
IF gfModalGen('QRM00000B32000',.F.,.F.,.F.,'File has been created, do you want to save it?') = 1
  lcSvErr = ON('ERROR')
  llDummy = .F.
  ON ERROR llDummy = .T.  
  *COPY FILE &lcTmpSnd TO &lcRpSndFil   &&Using macro subst. cause error if file name has spaces.
  COPY FILE (lcTmpSnd) TO (lcRpSndFil) 
  ON ERROR &lcSvErr
  IF !llDummy   && file is saved correctly , go now to update the field ORDHDR.LORDSNTJL 
    SELECT &lcTmpOrd.
    SCAN
      =SEEK(&lcTmpOrd..ACCOUNT+'O'+IIF(&lcTmpOrd..ORDER='ALL','',&lcTmpOrd..ORDER),'ORDHDR')
      SELECT ORDHDR
      SCAN REST WHILE ACCOUNT+CORDTYPE+ORDER = &lcTmpOrd..ACCOUNT+'O'+;
                                               IIF(&lcTmpOrd..ORDER='ALL','',&lcTmpOrd..ORDER)
        REPLACE ORDHDR.LORDSNTJL WITH .T.
      ENDSCAN
    ENDSCAN
    =gfModalGen('QRM00000B00000',.F.,.F.,.F.,;
                               'File saved successfully.')
    
  ELSE
    =gfModalGen('QRM00000B00000',.F.,.F.,.F.,;
                               'Some error is encountered while saving the file, '+;
                               'please retry the process or change the file name.')
  ENDIF
ENDIF
USE IN &lcTmpOrd.
ERASE (gcDataDir+lcTmpOrd+'.DBF')
ERASE (gcDataDir+lcTmpOrd+'.CDX')

*-- end of lfExpToAsc.

*:**************************************************************************
*:* Name        : lfGtDt
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/2003
*:* Purpose     : Get date in a string type in the format yy/mm/dd
*:***************************************************************************
*:* Called from : lfExpToAsc
*:***************************************************************************
*:* Parameters : Date value
*:***************************************************************************
*:* Return      : String contains the date in the format yy/mm/dd
*:***************************************************************************
FUNCTION lfGtDt
PARAMETERS ldDat
IF !EMPTY(ldDat)
  RETURN RIGHT(STR(YEAR(ldDat)),2)+'/'+;
         PADL(MONTH(ldDat),2,'0')+'/'+;
         PADL(DAY(ldDat),2,'0')       
ELSE
  RETURN ''
ENDIF         
*-- end of lfGtDt.

*:**************************************************************************
*:* Name        : lfvOldVal
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 07/10/03
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
*:* Date        : 07/10/2003
*:* Purpose     : Get Sku of the current style account from spck_lin file
*:***************************************************************************
*: Parametes    : Size # that with qty>0 ,passed as Character
*:* Called from : lfExpToAsc
*:***************************************************************************
FUNCTION lfGtSku
PARAMETERS lcSz
*tmi , it is wrong to make a parameter a private
*PRIVATE lcSz,lnSlct,lcSku,lnDime 
PRIVATE lnSlct,lcSku,lnDime
lnSlct = SELECT()

lcSku = ''
*B607492,1  TMI [Start] seek by the new added field ORDLINE.PACKID instead of style, since a style may have more than one pack id
*IF SEEK('S'+ORDLINE.ACCOUNT+SUBSTR(ORDLINE.STYLE,1,lnClrPos+lnClrLen-1),'SPCK_HDR') .AND. ;
   SEEK('S'+ORDLINE.ACCOUNT+ORDLINE.STYLE,'SPCK_LIN')
IF SEEK('S'+ORDLINE.ACCOUNT+ORDLINE.PACKID,'SPCK_HDR') .AND. ;
   SEEK('S'+ORDLINE.ACCOUNT+ALLTRIM(ORDLINE.PACKID),'SPCK_LIN')   
   *B607492,1  TMI [End  ] 
  lnDime = LEN(ALLT(SPCK_HDR.PACK_ID))
  
  SELECT SPCK_LIN
  *TMI , do not use 'WHILE' clause , use FOR instead
  *LOCATE REST WHILE TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+ORDLINE.ACCOUNT+ORDLINE.STYLE ;
              FOR   QTY&lcSz > 0
  lcSvOrd = ORDER()            
  SET ORDER TO
  *ash Changes as per tony's email on 02/18/04 
  *LOCATE REST FOR TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+ORDLINE.ACCOUNT+ORDLINE.STYLE ;
              .AND.  QTY&lcSz > 0             
  LOCATE REST FOR TYPE+ACCOUNT+STYLE+PACK_ID = 'S'+ORDLINE.ACCOUNT+ORDLINE.STYLE+alltrim(ordline.packid) .AND. QTY&lcSz > 0
 
               
  IF FOUND()
    lcSku = ALLT(SUBSTR(SPCK_LIN.PACK_ID,lnDime+1))
  ENDIF        
  SET ORDER TO &lcSvOrd IN SPCK_LIN    
ENDIF

SELECT (lnSlct)
RETURN lcSku
*-- end of lfGtSku.
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
