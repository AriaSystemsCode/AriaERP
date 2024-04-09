*!*************************************************************
*! Name      : lflfVendCont
*! Developer : NAD
*! Date      : 07/01/2000
*! Purpose   : Vendor Contacts Screen
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : 
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfVendCont()
*!*************************************************************
*!B606434,1 ALB 09/25/2002 Fix the contact screen
*!
FUNCTION lfVendCont
lcOAlias=Alias()
lcDefCont=laData[17]
ON KEY LABEL ALT+B ACTIVATE WINDOW (lcBrwTitl)
lcClose   = gcBmpHome + "close1.bmp"
lcNew     = gcBmpHome + "new.bmp"
lcRemove  = gcBmpHome + "rem.bmp"
lcModify  = gcBmpHome + "MODIFY.bmp" 
lcBsWind=gfTempName()
lcBrwCont=gfTempName()
lcPushB=gfTempName()
lcMainTtl='Vendor Contacts'
lcBrwTitl='Contacts'
DECLARE laRole[1],laTarget[1]      && Array to hold Roles from codes file, and Selected roles
=lfCrTmpRol()  && Create the temp file lcContRole 

=gfOpenFile(gcDataDir+'CONTACT',gcDataDir+'CONTACT','SH')
IF !EMPTY(laData[17]) AND !SEEK ('V'+PADR(laData[1],8)+SPACE(8)+PADR(laData[17],30) ,'CONTACT')
  SELECT Contact
  APPEND BLANK
  REPLACE cContType      WITH 'V'
  REPLACE CCont_Id WITH laData[1]      
  REPLACE Contact WITH laData[17]
ENDIF
IF (laScrMode[3] .OR. laScrMode[4])  AND  !USED(lcContFile)   
  lcOldVend=laData[1] 
  SELECT * ,.F. AS lDefCont FROM CONTACT WHERE cContType+cCont_Id+Store+Contact = ;
        'V'+PADR(laData[1],8)+SPACE(8) INTO DBF (gcWorkDir+lcContFile)        
        INDEX ON cContType+cCont_Id+Store+Contact TAG (lcContFile)     

ENDIF 

IF USED(lcContFile)
  SELECT (lcContFile)    
  REPLACE ALL lDefCont WITH .F.
  IF SEEK ('V'+PADR(laData[1],8)+SPACE(8)+PADR(laData[17],30))  
    REPLACE lDefCont WITH .T.
	  ENDIF
  GO TOP IN  (lcContFile)   
ENDIF         

*B606434,1 ALB Fix the contact screen [Begin]
SELECT cCode_No+' - '+cDiscrep FROM Codes ;
   WHERE cDefCode+cRltField+cFld_Name = 'NNCROLE     ' ;
   ORDER BY cCode_No INTO ARRAY laRole
IF _TALLY = 0
  laRole[1] = 'N/A' + SPACE(27)
ENDIF
*B606434,1 ALB Fix the contact screen [End]

DO (gcScrDir +"APVCont.SPX")


SELECT (lcOAlias)

*!*************************************************************
*! Name      : lfBrowCont
*! Developer : NAD
*! Date      : 07/12/2000
*! Purpose   : Browse Vendor Contacts
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : lfShowBrow
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfBrowCont()
*!*************************************************************
FUNCTION lfBrowCont

 
SELECT IIF(laScrMode[1] .OR. laScrMode[2],'Contact',lcContFile)
lnMarker = RECNO()
GO TOP

BROWSE FIELDS ;
       cMarker =IIF(RECNO()=lnMarker ,'>',' '):H=' ':R:1:W=.F.,;
       Contact    :H='Contact'  :22:R,;
       cContTtl   :H='Title' :22:R,;
       Phone      :P= gfPhoneTem() :H='Phone' :R,;
       Fax        :P= gfPhoneTem() :H='Fax' :R,;
       cContSalut :H='Salutation' :R,;
       cEmail_Add :H='Email':R;
       WINDOW (lcBrwCont)  ;
       IN WINDOW (lcBsWind);
       NOMENU            ;         
       NOAPPEND          ;
       NODELETE          ;         
       NOWAIT            ;
       SAVE              ;
       NOCLEAR           ;
       WHEN lfShowBrow() ;
	   TITLE lcBrwTitl   ;    
	   KEY 'V'+PADR(laData[1],8)+SPACE(8)     

=lfShowBrow()

*!*************************************************************
*! Name      : lfShowBrow
*! Developer : NAD
*! Date      : 07/12/2000
*! Purpose   : Refresh Vendor Contacts.
*! Ref       : E301436,1  
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfShowBrow()
*!*************************************************************
FUNCTION lfShowBrow

lnMarker = RECNO(IIF(laScrMode[1] .OR. laScrMode[2],'Contact',lcContFile))
llDefCont=.F.
SHOW WINDOW (lcBrwTitl) REFRESH SAME
=lfDefCont()

*!*************************************************************
*! Name      : lfDefCont
*! Developer : NAD
*! Date      : 07/12/2000
*! Purpose   : Get The Vendor Default Contact.
*! Ref       : E301436,1  
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  : None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfDefCont()
*!*************************************************************
FUNCTION lfDefCont

DO CASE
 CASE  ALIAS() ='CONTACT'
   IF Contact<> laData[17]  
     llDefCont=.F.
   ELSE
     IF !EMPTY(laData[17]) 
       llDefCont=.T.
     ENDIF
   ENDIF    
 CASE ALIAS() = lcContFile  
   llDefCont= lDefCont    
ENDCASE
SHOW GET llDefCont 

  
*!*************************************************************
*! Name      : lfvContact
*! Developer : NAD
*! Date      : 07/12/2000
*! Purpose   : Add/Modify Vendor Contact
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : arcuscon.SPX
*!*************************************************************
*! Passed Parameters  :  llNewCont .T. New Contact
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvContact()
*!*************************************************************
FUNCTION lfvContact
PARAMETERS llNewCont

SELECT IIF(laScrMode[2],'Contact',lcContFile)
IF llNewCont  
  SCATTER MEMVAR FIELDS Contact,cContTtl,Phone,Fax,cContSalut,mNote,cEmail_Add MEMO BLANK 
ELSE  
  SCATTER MEMVAR FIELDS Contact,cContTtl,Phone,Fax,cContSalut,mNote,cEmail_Add MEMO  
ENDIF

*B606434,1 ALB Fix the contact screen [Begin]
lcRoleStat = IIF(EMPTY(m.Contact) ,'DISABLE','ENABLE')
llRole = .F.  && variable to fix the bug that when Modify contact and click role btn the screen disappear
lcAlias = ALIAS()
IF !USED('CONTROLE')
  =gfOpenFile(gcDataDir+'CONTROLE',gcDataDir+'CONTROLE','SH')
ENDIF
SELECT (lcAlias)
=lfvRole(.T.)
*B606434,1 ALB Fix the contact screen [end]

lcContStat = IIF(laScrMode[2],'DISABLE','ENABLE')
lcFildStat = IIF(EMPTY(m.Contact) .OR. laScrMode[2],'DISABLE','ENABLE')
lcZoomStat = IIF(EMPTY(m.Contact) ,'DISABLE','ENABLE')
lcNoModify = IIF(laScrMode[2]," NOMODIFY","")
DO (gcScrDir+"arcuscon.SPX")
IF !laScrMode[2] .AND. !EMPTY(Contact)
  SHOW GET pbContact ENABLE
  SHOW GET pbCRemove ENABLE
ENDIF
SHOW WINDOW (lcBrwTitl) REFRESH SAME
*!*************************************************************
*! Name      : lfvRemCon
*! Developer : NAD
*! Date      : 07/12/20000
*! Purpose   : Remove Vendor Contact
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : gfModalGen
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvRemCon()
*!*************************************************************
FUNCTION lfvRemCon

*Message : 40054
*Are you sure you want to delete this contact?
*Button : 40000
*Yes No

IF gfModalGen('QRM40054B40000','ALERT','Contact') = 1

  *B606434,1 ALB Fix the contact screen [BEGIN]
  lckey = CCONTTYPE+CCONT_ID+STORE+CONTACT
  SELECT (lcContRole)
  =SEEK(lcKey)
  REPLACE CMARK WITH 'D' REST ;
  WHILE  CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = lcKey
  *B606434,1 ALB Fix the contact screen [END]
  SELECT (lcContFile)
  DELETE
  IF lDefCont 
    lcDefCont=""
    llDefCont=.F.
    SHOW GET llDefCont
  ENDIF  
  IF !SEEK('V'+PADR(laData[1],8)+SPACE(8))
    SHOW GET pbCRemove DISABLE
    SHOW GET pbContact DISABLE
  ENDIF
  SHOW WINDOW (lcBrwTitl) REFRESH SAME
ENDIF

*!*************************************************************
*! Name      : lfvContId
*! Developer : NAD
*! Date      : 07/12/2000
*! Purpose   : Validate Vendor Contact
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvContId()
*!*************************************************************
FUNCTION lfvContId
PRIVATE lcOldContact
IF EMPTY(m.Contact)
  RETURN
ENDIF
lcOldContact =  &lcContFile..Contact
IF SEEK('V'+PADR(laData[1],8)+SPACE(8)+m.Contact,lcContFile)
  SCATTER MEMVAR FIELDS Contact,cContTtl,Phone,Fax,cContSalut,mNote,cEmail_Add MEMO 
  
ELSE
  =SEEK('V'+PADR(laData[1],8)+SPACE(8)+lcOldContact,lcContFile)
ENDIF
SHOW GET m.cContTtl   ENABLE
SHOW GET m.Phone      ENABLE
SHOW GET m.Fax        ENABLE
SHOW GET m.cContSalut ENABLE
SHOW GET m.mNote      ENABLE
SHOW GET pbZoom       ENABLE
SHOW GET pbOkCont     ENABLE
SHOW GET m.cEmail_Add ENABLE

*!*************************************************************
*! Name      : lfvOkCont
*! Developer : NAD
*! Date      : 07/12/20000
*! Purpose   : Update Vendor Contact
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvOkCont()
*!*************************************************************
FUNCTION lfvOkCont

SELECT (lcContFile)
IF llNewCont .AND. !SEEK('V'+PADR(laData[1],8)+SPACE(8)+m.Contact,lcContFile)
  INSERT INTO (lcContFile) (cContType,cCont_Id,Store) ;
  VALUES ('V',laData[1],Space(8))
ENDIF
IF !laScrMode[2]
  GATHER MEMVAR MEMO
ENDIF
CLEAR READ

*!*************************************************************
*! Name      : lfIniCont
*! Developer : NAD
*! Date      : 07/11/2000
*! Purpose   : Control the status of the push buttoms
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfIniCont()
*!*************************************************************
FUNCTION lfIniCont
SHOW GET llDefCont DISABLED
SHOW GET ibContact ENABLE
lnRecNo = RECNO()
IF SEEK('V'+PADR(laData[1],8)+SPACE(8))
  SHOW GET pbContact ENABLE
  IF laScrMode[3] .OR. laScrMode[4]
    SHOW GET pbCRemove ENABLE     
  ENDIF
ENDIF
IF BETWEEN(lnRecNo,1,RECCOUNT())
  GOTO lnRecNo
ENDIF
IF laScrMode[3] .OR. laScrMode[4]
  SHOW GET pbCNew ENABLE
  SHOW GET llDefCont ENABLED
ENDIF
=lfDefCont()   
*!*************************************************************
*! Name      : lfContCnl
*! Developer : NAD
*! Date      : 07/11/2000
*! Purpose   : Clear the vendor contact screen
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfContCnl()
*!*************************************************************
FUNCTION lfvCncl
llContChng=.T.
IF  laScrMode[3] OR laScrMode[4] 
  LOCATE FOR ldefCont=.T.
  IF FOUND()
    lCDefCont=Contact
  ELSE
    lcDefCont =''
  ENDIF
ENDIF  
lcOldCont=laData[17]
laData[17]=lcDefCont
CLEAR READ
  



*!*************************************************************
*! Name      : lfTrap
*! Developer : NAD
*! Date      : 07/11/2000
*! Purpose   : Trap the Tab ,BackTab ,Esc Keys
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfTrap()
*!*************************************************************
FUNCTION lfTrap

IF WONTOP() = lcBrwTitl 
  ON KEY LABEL ESCAPE  DO lpEsc
  ON KEY LABEL TAB     DO lpTab
  ON KEY LABEL BACKTAB DO lpBackTab	
ENDIF
RETURN .F.
*!*************************************************************
*! Name      : lfClrTrap
*! Developer : NAD
*! Date      : 07/11/2000
*! Purpose   : Clear the  Trap
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfClrTrap()
*!*************************************************************
FUNCTION lfClrTrap

ON KEY LABEL ESCAPE
ON KEY LABEL TAB
ON KEY LABEL BACKTAB
ON KEY LABEL ENTER


*!*************************************************************
*! Name      : lpEsc
*! Developer : NAD
*! Date      : 07/11/2000
*! Purpose   : the Esc Key
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpEsc()
*!*************************************************************

PROCEDURE lpEsc

ON KEY LABEL ESCAPE

IF WONTOP()=lcBrwTitl
  ACTIVATE WINDOW (lcPushB)
ENDIF  
CLEAR READ

*!*************************************************************
*! Name      : lpTab
*! Developer : NAD
*! Date      : 07/11/2000
*! Purpose   : the Tab Key
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpTab()
*!*************************************************************

PROCEDURE lpTab
ON KEY LABEL TAB
IF WONTOP() = lcBrwTitl  
  ACTIVATE WINDOW (lcPushB)
  _CUROBJ = OBJNUM(pbCNew)  
ENDIF
*!*************************************************************
*! Name      : lpBackTab
*! Developer : NAD
*! Date      : 07/11/2000
*! Purpose   : The BackTab Key
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lpBackTab()
*!*************************************************************

PROCEDURE lpBackTab
ON KEY LABEL BACKTAB
IF WONTOP()=lcBrwTitl
  ACTIVATE WINDOW (lcPushB)
  _CUROBJ = OBJNUM(pbCCLose) 
ENDIF

*!*************************************************************
*! Name      : lfvDefCont
*! Developer : NAD
*! Date      : 07/11/2000
*! Purpose   : Update the default contact in the tmp file
*! Ref       : E301436,1 
*!*************************************************************
*! Calls     : None
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  None
*!*************************************************************
*! Example            :  =lfvDefCont()
*!*************************************************************
FUNCTION lfvDefCont
SELECT (lcContFile)
*B606434,1 ALB Fix the contact screen [Begin]
lnRecno = IIF(!EOF(),RECNO(),0)
*B606434,1 ALB Fix the contact screen [end]

lcCont= Contact
lcVendor=cCont_Id
REPLACE lDefCont WITH llDefCont
REPLACE  lDefCont WITH .F. FOR cContType+cCont_Id+Store+Contact <> 'V'+lcVendor+SPACE(8)+lcCont 
lcDefCont=lcCont
*B606434,1 ALB Fix the contact screen [Begin]
IF lnRecno != 0
  GOTO lnRecNo
ENDIF
*B606434,1 ALB Fix the contact screen [end]

*:**************************************************************************
*:* Name        : lfvRole
*:* Developer   : Albert Raif
*:* Date        : 09/25/2002
*:* Purpose     : Valid function to add/remove roles to a contact
*:***************************************************************************
*:* Called from : Contact screen
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvRole()
*:* Refer to    : B606434,1
*:***************************************************************************
FUNCTION lfvRole
PARAMETERS llFromCont
PRIVATE lcAlias,lnI,lnTrgt,lnRolPos,llOk

*--if user try to click role btn when he modified the Contact , then prevent him
IF llRole
  SHOW GET pbRole     DISABLE  && DISABLE Role button
  llRole = .F.
  RETURN
ENDIF

lcAlias = ALIAS()
IF llFromCont
  *--Open CONTROLE file
  IF !USED('CONTROLE')
    =gfOpenFile(gcDataDir+'CONTROLE',gcDataDir+'CONTROLE','SH')
  ENDIF

  
  *--Clear selected roles
  IF ASCAN(laRole,'\') > 0
    FOR lnI = 1 TO ALEN(laRole,1)
      IF LEFT(laRole[lnI],1) = '\'
        laRole[lnI] = SUBSTR(laRole[lnI],2)
      ENDIF
    ENDFOR
  ENDIF
  
  *--Fill the temp file
*  IF !laScrMode[2]
*    SELECT CONTROLE
*    =SEEK('V'+PADR(laData[1],8)+SPACE(8)+m.CONTACT,'CONTROLE')
*    SCAN REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = 'V'+PADR(laData[1],8)+SPACE(8)+m.CONTACT    
*      IF !SEEK('V'+PADR(laData[1],8)+SPACE(8)+m.CONTACT+CONTROLE.CROLE,lcContRole)
*        INSERT INTO (lcContRole) (CCONTTYPE,CCONT_ID,STORE,CONTACT,CROLE) VALUES ;
*                       ('V',PADR(laData[1],8),SPACE(8),m.CONTACT,CONTROLE.CROLE)
*      ENDIF
*    ENDSCAN
*  ENDIF
  
  *--Empty Target array
  DIMENSION laTarget[1]
  laTarget[1] = SPACE(0)
  
  *--Then ReFill it.
  lnTrgt = 0
  SELECT IIF(laScrMode[2],'CONTROLE',lcContRole)
  =SEEK('V'+PADR(laData[1],8)+SPACE(8)+m.CONTACT)
  SCAN REST WHILE CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = ;
                    'V'+PADR(laData[1],8)+SPACE(8)+m.CONTACT FOR IIF(laScrMode[2],.T.,CMARK # 'D')
    lnRolPos = ASCAN(laRole,CROLE+' - ')
    IF lnRolPos > 0
      lnTrgt = lnTrgt + 1
      DIMENSION laTarget[lnTrgt]
      laTarget[lnTrgt] = laRole[lnRolPos]
      laRole[lnRolPos] = '\'+laRole[lnRolPos]
    ENDIF    
  ENDSCAN
  SELECT (lcAlias)
  RETURN
ENDIF

*-- Call mover function.
=gfMover(@laRole,@laTarget,'Select Roles', !laScrMode[2] ,'')

IF laScrMode[2]
  SELECT (lcAlias)
  RETURN .F.
ENDIF
SHOW GET m.Contact DISABLE

*--Mark all records of this contact as deleted
SELECT (lcContRole)
=SEEK('V'+PADR(laData[1],8)+SPACE(8)+m.CONTACT,lcContRole)
REPLACE CMARK WITH 'D' REST ;
WHILE  CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE = 'V'+PADR(laData[1],8)+SPACE(8)+m.CONTACT

*--Update lcContRole from the laTarget Array
IF IIF(ALEN(laTarget,1)=1,!EMPTY(laTarget[1]),.T.)
  FOR lnI = 1 TO ALEN(laTarget,1)
    IF SEEK('V'+PADR(laData[1],8)+SPACE(8)+m.CONTACT+PADR(laTarget[lnI],6),lcContRole)
      REPLACE CMARK WITH ' '
    ELSE
      INSERT INTO (lcContRole) VALUES ;
      ('V',PADR(laData[1],8),SPACE(8),m.CONTACT,PADR(laTarget[lnI],6),' ')
    ENDIF
  ENDFOR
ENDIF
SELECT (lcAlias)
*-- end of lfvRole.
*:**************************************************************************
*:* Name        : lfCrTmpRol
*:* Developer   : Albert Raif
*:* Date        : 09/25/2002
*:* Purpose     : Create temp file for Roles
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfCrTmpRol()
*:***************************************************************************
FUNCTION lfCrTmpRol

IF !USED(lcContRole)
  CREATE TABLE (gcWorkDir+lcContRole) ;
  (CCONTTYPE C(1),CCONT_ID C(8),STORE C(8),CONTACT C(30),CROLE C(6),CMARK C(1))
  *-- CMARK :  "D" ==> Delete ; " " ==> New ; "O" ==> Old
  INDEX ON CCONTTYPE+CCONT_ID+STORE+CONTACT+CROLE TAG (lcContRole)
ENDIF

IF !USED('CONTROLE')
  =gfOpenFile(gcDataDir+'CONTROLE',gcDataDir+'CONTROLE','SH')
ENDIF

SELECT CCONTTYPE,CCONT_ID,STORE,CONTACT,CROLE FROM CONTROLE ;
       WHERE cContType+cCont_Id+Store+Contact = ;
       'V'+PADR(laData[1],8)+SPACE(8) INTO ARRAY laContRole
IF _TALLY > 0
  SELECT (lcContRole)
  FOR lnCount = 1 TO ALEN(laContRole,1)
    APPEND BLANK
    REPLACE CCONTTYPE WITH laContRole[lnCount,1];
            CCONT_ID  WITH laContRole[lnCount,2];
            STORE     WITH laContRole[lnCount,3];
            CONTACT   WITH laContRole[lnCount,4];
            CROLE     WITH laContRole[lnCount,5];
            CMARK     WITH 'O'
  ENDFOR
ENDIF
*-- end of lfCrTmpRol.
