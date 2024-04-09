*:************************************************************************
*: Program file  : SOUSRTS.PRG                              
*: Program desc. : Automatic generating user task list (E#301878)
*:         Module: Aria Apparel Series.
*:      Developer: Hend Ghanem (HBG)
*:************************************************************************
*:To Check :
*: Date for SU module.
*: Select SQL.
*:E#301878,4 TMI Continuo working on this entery.
*:B#606115,1 SSH Fix duplicate records in profile values and some spelling
*:B#606115,1 SSH mistake in the option grid and the 
*:B606208,1 RAE New modifications in Automatic generating user task list.
*:              Modi(1) - Select the profile Values from  ARPRFCOD file instead  of
*:                        ROFILE file code.
*:              Modi(2) - Add new  text object to the  option  grid  to be used  to 
*:                        update field SYSCHDUL.CSUBJECT.
*:              Modi(3) - Before generate  tasks display browse with all tasks that 
*:                        will be  generated to the user.  This browse will be like 
*:                        in list  browse and all the  records will be  selected by 
*:                        default and the user can select or unselect.    (the same 
*:                        functionality as Aria standard in list)
*:              Modi(4) - If the user did not enter one of the non mandatory fields
*:                        do not reset the option grid.
*:B606453,1 SSH 09/03/2002 Fix the following Issues:
*:                         - I see that the contacts are duplicated multiple times, 
*:                           it appears that it depends on the number of roles that they have.
*:                         - I do not see all of the customers that have an email address; 
*:                           it appears that only the ones that have a role defined 
*:                           appear to be selected (I did not select any role)
*:B606510,1 SSH 10/01/2002 Fix the following Issues:
*:                         1-  The browse that we get with all the generated tasks has to have
*:                             additional information:Account#, Account name, Reason 
*:                             description(instead of reason code), contact email address.
*:                         2-  The system should validate that all the email addresses for all
*:                             tasks generated are unique and that there is no duplication.
*:                             One email address should be used only once which means that if the
*:                             option grid is set to filter on contacts with non empty email 
*:                             addresses, it should not create additional tasks for the same 
*:                             email address.
*:                         3-  Default "Scheduled Activity date" and "Entered Date" to today's 
*:                              date. 
*:                         4 - I tried to use the Role filter, I put "Time Sheet" 
*:                             (which is a role that we have defined in our system), 
*:                             I checked that the account BRATEXT BRA14 has at least two 
*:                             people that have several roles including the above. 
*:                             The system did not select any contacts from that account.
*:                             role filter is not working.ThanksOmar 
*:                         5-  contact that have no email address. Should not be selected
*:                                 it seems that this option is also not working properly.
*:B119217,1 ALB 08/27/2003 Fix bug when creating the temp file
*:*****************************************************************************************

STORE "" TO lcOldValue , lcTmpCust   , lcTmpStore , lcOgPrvRun , lcRpExp   , lcTmpStr ,;
            lcTmpAcc   , lcWorkFile , laRpSource , lcEmlInde

*-- Filter Variables
STORE "" TO lcRpStatus , lcRpTskTyp , lcRpContTp , lcRpUsrID
STORE "" TO lcPrfExp   , lcProfil   , lcRolExp   , lcRolfil  , lcResone ,;
            lcOFlExp   , lcNFlExp

*-- OG Variables
ldRPMatLDt = gdSysdate
ldRPMatHDt = gdSysdate
ldRpEntDat = gdSysdate
ldRpSchDat = gdSysdate
DIMENSION laRpSource[1] , laRpTarget[1] , laRpTskSr[1]  , laRpTskTr[1]  , laRpContSr[1] ,;
          laRpContTr[1] , laRpProfSr[1] , laRpProfTr[1] , laRpPrfVSr[1] , laRpPrfVTr[1]
STORE 0  TO lnRpStatu,lnRpTskTyp,lnRpContTp,lnDumDate
STORE "" TO laRpTarget , laRpTskSr  , laRpTskTr  , laRpContSr , laRpContTr  , laRpProfSr,;
            laRpProfTr , laRpPrfVSr , laRpPrfVTr

*B606208,1 RAE [Start]
STORE '' TO lcRpText   , lcTempSel , lcRoleCode , lcRegion , lcShipVia , lcSpecInst ,;
            lcCustClas , lcTerms   , lcDivision , lcTmpDbf , lcOldReson
DIMENSION laOldValu1[1]
DIMENSION laOldValu2[1]
DIMENSION laOldValu3[1]
DIMENSION laOldValu4[1]
*B606208,1 RAE [End]

=lfOpnFiles()
*B606208,4
*DO WHILE EMPTY(laRpTskTr)  OR ;
         EMPTY(ldRpSchDat) OR ;
         EMPTY(lcRpContTp) OR ;
         EMPTY(lcResone)

DO WHILE EMPTY(laRpTskTr)  OR ;
         EMPTY(ldRpSchDat) OR ;
         EMPTY(lcRpContTp) OR ;
         EMPTY(lcResone)   OR ;
         EMPTY(lcRpUsrID)
*B606208,4
*B606208,1 RAE - Modi(4) llFrstTim to indicate the first time of running the Option Grid. [Start]
llFrstTim = IIF(TYPE('llFrstTim') = "U" , .T. , llFrstTim)
*B606208,1 RAE [End]

  lcRpExp = gfOpGrid('SOUSRTS' , .T.)
  *B606208,1 RAE - Modi(4) [Start]
  llFrstTim = .F.  
  *B606208,1 RAE [End]
  IF TYPE('lcRpExp') # 'C'
    RETURN
  ENDIF
  lcMsg = ''
  DO CASE
    CASE EMPTY(laRpTskTr)
      lcMsg = 'At least one task type must be selected.'
    CASE EMPTY(ldRpSchDat)
      *:B#606115,1 SSH Fix spelling mistake.
      *lcMsg = 'Schedule Activity Date Must be entered.'
      lcMsg = 'Schedule Activity Date must be entered.'
      *:B#606115,1 SSH (END)
    CASE EMPTY(lcRpContTp)
      lcMsg = 'At least one Contact type must be selected.'
    CASE EMPTY(lcResone)
      lcMsg = 'A Task Reason must be selected.'
    *B606208,4
    CASE EMPTY(lcRpUsrID)
      lcMsg = 'A User ID must be selected.'
    *B606208,4
  ENDCASE
  
  IF !EMPTY(lcMsg)
    =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,lcMsg)
  ENDIF
ENDDO
 	
IF TYPE('lcRpExp') = 'C'
  *-- Get Flter Expression
  =lfGetFltEx()

  *-- Creat temp files
  =lfCrtTemp()

  *-- Collect Data
  =lfCollect()

  *-- Update User Task List
  =lfUpdUsrTs()
ENDIF

*!**************************************************************************
*! Name      : lfCrtFltEx
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Creat Flter Expression
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfCrtFltEx()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
FUNCTION lfCrtFltEx

*B606208,1 RAE - Modi(4) [Start]
FOR lnInd = 1 TO ALEN(laOgFxFlt,1)
  DO CASE
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CONTROLE.CROLE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Role Code
      lcRoleCode = laOgFxFlt[lnInd,6]

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.REGION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Region Code
      lcRegion = laOgFxFlt[lnInd,6]

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.SHIPVIA' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Ship Via
      lcShipVia = laOgFxFlt[lnInd,6]
      
    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.SPCINST' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Special Instructions
      lcSpecInst = laOgFxFlt[lnInd,6]

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.CLASS' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Customer Class
      lcCustClas = laOgFxFlt[lnInd,6]

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.CTERMCODE' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Terms
      lcTerms = laOgFxFlt[lnInd,6]

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.CDIVISION' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Divisions
      lcDivision = laOgFxFlt[lnInd,6]      

    CASE ALLTRIM(laOgFxFlt[lnInd,1]) = 'SYSCHDUL.CTRANRESON' .AND. !EMPTY(ALLTRIM(laOgFxFlt[lnInd,6]))
      *--- Reason Code
      lcOldReson = laOgFxFlt[lnInd,6]
  ENDCASE
ENDFOR
*B606208,1 RAE - Modi(4) [End]
*-- Get the Profile code filter expression
*lnFound   = ASCAN(laOGFxFlt,'PROFILE.CPRO_CODE')
*lnPrfPos  = ASUBSCRIPT(laOGFxFlt,lnFound,1)
*lcProfil  = STRTRAN(laOGFXFlt[lnPrfPos,6],"|",'","')
*lcPrfExp  = STRTRAN(laOGFXFlt[lnPrfPos,6],"|",",")

*-- Get the Role code filter expression
lnFound   = ASCAN(laOGFxFlt,'CONTROLE.CROLE')
lnPrfPos  = ASUBSCRIPT(laOGFxFlt,lnFound,1)
lcRolfil  = STRTRAN(laOGFXFlt[lnPrfPos,6],"|",'","')
lcRolExp  = STRTRAN(laOGFXFlt[lnPrfPos,6],"|",",")

*-- Remove Resone from the filter
lnFound   = ASCAN(laOGFxFlt,'SYSCHDUL.CTRANRESON')
lnPrfPos  = ASUBSCRIPT(laOGFxFlt,lnFound,1)
lcResone  = laOGFXFlt[lnPrfPos,6]
lcFltExp  = IIF(ATC('AND  SYSCHDUL.CTRANRESON',lcRpExp) <> 0,'  AND  SYSCHDUL.CTRANRESON = "'+ lcResone +'"',;
            'SYSCHDUL.CTRANRESON = "'+ lcResone +'"')
lcRpExp   = STRTRAN(lcRpExp,lcFltExp,'')



*-- Store Filter 
lnFound   = ASCAN(laOGFxFlt,'CUSTOMER.STORE')
lnStrPos  = ASUBSCRIPT(laOGFxFlt,lnFound,1)
lcTmpStr = laOGFxFlt[lnStrPos,6]
lcStores = ""
IF USED(lcTmpStr)
  GO TOP IN (lcTmpStr)
ENDIF               
IF USED(lcTmpStr) AND !EOF(lcTmpStr)
  lnFound   = ASCAN(laOGFxFlt,'CUSTOMER.ACCOUNT')
  lnAccPos  = ASUBSCRIPT(laOGFxFlt,lnFound,1)
  lcTmpACC = laOGFxFlt[lnAccPos,6]
  lcAccounts = ""
  lcSAcc  = ""
  IF USED(lcTmpAcc)
    SELECT (lcTmpAcc)
    llFirst = .T.
    SCAN
      IF llFirst 
        llFirst = .F.
        lcAccounts = '"M' + &lcTmpAcc..Account + '"'
        lcSAcc     = '"S' + &lcTmpAcc..Account + '"'
      ELSE
        lcAccounts = lcAccounts + ',"' + 'M' + &lcTmpAcc..Account + '"'
        lcSAcc     = lcSAcc     + ',"' + 'S' + &lcTmpAcc..Account + '"'
      ENDIF
    ENDSCAN
  ENDIF
  lcOFlExp = 'INLIST(CUSTOMER.TYPE+CUSTOMER.ACCOUNT+CUSTOMER.STORE,' + lcAccounts + ')'
  lcNFlExp = 'INLIST(CUSTOMER.TYPE+CUSTOMER.ACCOUNT+CUSTOMER.STORE,' + lcSAcc + ')'
ENDIF

*!**************************************************************************
*! Name      : lfGetFltEx
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Creat Flter Expression
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfCrtFltEx()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
FUNCTION lfGetFltEx

*-- Remove the Profile code filter expression
*lcFltExp  = IIF(ATC('AND',lcRpExp) <> 0,'  AND  INLIST(PROFILE.CPRO_CODE,"'+ lcProfil +'")',;
*            'INLIST(PROFILE.CPRO_CODE,"'+ lcProfil +'")')
*lcRpExp   = STRTRAN(lcRpExp,lcFltExp,'')

*-- Remove the Role code filter expression

*---606510
*lcFltExp  = IIF(ATC('AND',lcRpExp) <> 0,'  AND  INLIST(CONTROLE.CROLE,"'+ lcRolfil +'")',;
            'INLIST(CONTROLE.CROLE,"'+ lcRolfil  +'")')

lcFltExp  = IIF(ATC('AND  INLIST(CONTROLE.CROLE',lcRpExp) <> 0,'  AND  INLIST(CONTROLE.CROLE,"'+ lcRolfil +'")',;
            'INLIST(CONTROLE.CROLE,"'+ lcRolfil  +'")')

*--- 606510
lcRpExp   = STRTRAN(lcRpExp,lcFltExp,'')

*-- Remove Resone from the filter
*---606510
*lcFltExp  = IIF(ATC('AND',lcRpExp) <> 0,'  AND  SYSCHDUL.CTRANRESON = "'+ lcResone +'"',;
            'SYSCHDUL.CTRANRESON = "'+ lcResone +'"')

lcFltExp  = IIF(ATC('AND  SYSCHDUL.CTRANRESON',lcRpExp) <> 0,'  AND  SYSCHDUL.CTRANRESON = "'+ lcResone +'"',;
            'SYSCHDUL.CTRANRESON = "'+ lcResone +'"')

*---606510
lcRpExp   = STRTRAN(lcRpExp,lcFltExp,'')

*-- Store Filter 
IF USED(lcTmpStr)   
  GO TOP IN (lcTmpStr)
ENDIF               
IF USED(lcTmpStr) AND !EOF(lcTmpStr)
  lcRpExp   = STRTRAN(lcRpExp,lcOFlExp,lcNFlExp)
ENDIF

*-- Get the Customer filter expression
lcRpExp  = lcRpExp + IIF(!EMPTY(lcRpExp),'  AND  ','') + 'STATUS $ lcRpStatus'

*!**************************************************************************
*! Name      : lfCrtTemp
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Creat temp files
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfCrtTemp()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
FUNCTION lfCrtTemp

lcTmpCust  = gfTempName()
lcWorkFile = gfTempName()
lcEmlInde  = gfTempName()
*B606208,1 RAE - Modi(3) Temp name of index on (lcWorkFile) file. [Start]
lcTempSel = gfTempName()
lcTmpDbf  = gfTempName()
*B606208,1 RAE [End]
SELECT CUSTOMER
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru, 1)
*B606208,1 RAE - Modi(3) Add new 6 field to lcTmpCust and lcWorkFile [Start]
*DIMENSION laFileStru(lnFileStru + 7, 4)
*B119217,1 ALB Fix bug when creating the temp file [BEGIN]
*DIMENSION laFileStru(lnFileStru + 13, 4)
DIMENSION laFileStru(lnFileStru + 12, 4)
*B119217,1 ALB Fix bug when creating the temp file [END]

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cTranType'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cUser_id'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cSelcKey'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 7
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cSubject'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 30
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'dtrandate'
laFileStru[lnFileStru ,2] = 'D'
laFileStru[lnFileStru ,3] = 8
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'ctrantime'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 11
laFileStru[lnFileStru ,4] = 0
*B606208,1 RAE [End]
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cRole'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Contact'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 30
laFileStru[lnFileStru ,4] = 0
lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Phone'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 16
laFileStru[lnFileStru ,4] = 0

*B119217,1 ALB Fix bug when creating the temp file [BEGIN]
*lnFileStru = lnFileStru + 1
*laFileStru[lnFileStru ,1] = 'CEMAIL_ADD'
*laFileStru[lnFileStru ,2] = 'C'
*laFileStru[lnFileStru ,3] = 60
*laFileStru[lnFileStru ,4] = 0
*B119217,1 ALB Fix bug when creating the temp file [END]

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'mNotes'         
laFileStru[lnFileStru ,2] = 'M'
laFileStru[lnFileStru ,3] = 10
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'cTranReson'
laFileStru[lnFileStru ,2] = 'C'
laFileStru[lnFileStru ,3] = 6
laFileStru[lnFileStru ,4] = 0

lnFileStru = lnFileStru + 1
laFileStru[lnFileStru ,1] = 'Delete'
laFileStru[lnFileStru ,2] = 'L'
laFileStru[lnFileStru ,3] = 1
laFileStru[lnFileStru ,4] = 0

CREATE DBF (lcTmpCust) FROM ARRAY laFileStru
INDEX ON Type+Account+Store TAG (lcTmpCust)
CREATE DBF (lcWorkFile) FROM ARRAY laFileStru
INDEX ON Type+Account+Store TAG (lcWorkFile)
INDEX ON cTranType+CEMAIL_ADD TAG(lcEmlInde)
*!**************************************************************************
*! Name      : lfCollect
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Collect Data
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfCollect()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
FUNCTION lfCollect
PRIVATE lnI

SELECT CUSTOMER
SET RELATION TO Salesrep INTO SALESREP ADDITIVE
SET RELATION TO "C"+Account+Store INTO CONTROLE ADDITIVE
IF 'SU'$ gcCmpModules
  SET RELATION TO ACCOUNT INTO CUSTPROF ADDITIVE
ENDIF
LOCATE

SCAN FOR &lcRpExp
  WAIT WINDOW "Generate task for customer# "+CUSTOMER.Account NOWAIT
  SCATTER MEMVAR MEMO
  INSERT INTO (lcTmpCust) FROM MEMVAR  
ENDSCAN

*-- Get profile Codes
lcPrfExp = ''
IF !EMPTY(laRpProfTr[1])
  FOR lnI = 1 TO ALEN(laRpProfTr,1)
    lcPrfExp = lcPrfExp + LEFT(laRpProfTr[lnI],6) + ' | '
  ENDFOR
ENDIF

IF !EMPTY(lcPrfExp)
  lcPrfVals = ''
  IF !EMPTY(laRpPrfVTr[1])
    FOR lnI = 1 TO ALEN(laRpPrfVTr,1)
      lcPrfVals = lcPrfVals + SUBSTR(laRpPrfVTr[lnI],10) + ' | '
    ENDFOR
  ENDIF
  
  SELECT (lcTmpCust)
  SCAN     
    SELECT PROFILE
    _TALLY = 0    
    =SEEK('C' + PADR(&lcTmpCust..Account,8)+PADR(&lcTmpCust..Store,8) , 'PROFILE' )
    SCAN REST WHILE CCONTTYPE+CCONT_ID+STORE+CPRO_CODE+CPRO_VALUE = ;
                    'C' + PADR(&lcTmpCust..Account,8)+PADR(&lcTmpCust..Store,8)
      WAIT WINDOW "Validate customer# "+PADR(&lcTmpCust..Account,8)+" profile." NOWAIT
      IF ALLTRIM(PROFILE.cPro_code) $ lcPrfExp 
        IF EMPTY(lcPrfVals)
          _TALLY = 1
          EXIT
        ELSE
          IF ALLTRIM(PROFILE.CPRO_VALUE) $ lcPrfVals
            _TALLY = 1
            EXIT
          ENDIF
        ENDIF
      ENDIF
    ENDSCAN
    IF _TALLY = 0
      SELECT (lcTmpCust)
      REPLACE &lcTmpCust..Delete WITH .T.
    ENDIF
  ENDSCAN  
  DELETE FOR &lcTmpCust..Delete 
ENDIF
*B606208,1 RAE - Modi(3)  [Start]
FOR lnI = 1 TO ALEN(laRpTskTr,1)
*B606208,1 RAE [End]
SELECT (lcTmpCust)
SCAN
  *:B606453,1 SSH 09/03/2002 Generate taska for contact even if there is not any roll.
  *IF SEEK('C'+PADR(Account,8)+Store,'CONTROLE')
  *:B606453,1 SSH 09/03/2002 Generate taska for contact even if there is not any roll.
    SCATTER MEMVAR MEMO
    *:B606453,1 SSH 09/03/2002 Generate taska at contact level not roll level.    
    *SCAN REST WHILE cContType+cCont_Id+Store+Contact+cRole =;
                   'C'+PADR(&lcTmpCust..Account,8)+&lcTmpCust..Store;
                    FOR IIF(!EMPTY(lcRolExp), cRole $ lcRolExp , .T. )
    *:B606453,1 SSH 09/03/2002 Generate taska at contact level not roll level.    
    IF SEEK('C'+PADR(Account,8)+Store,'CONTACT')
      SELECT CONTACT
      SCAN REST WHILE cconttype+ccont_id+store+contact = 'C'+PADR(Account,8)+Store
        SELECT CONTROLE
        PRIVATE llAddRec
        LOCATE FOR cContType+cCont_Id+Store+Contact+cRole =;
                   'C'+PADR(&lcTmpCust..Account,8)+&lcTmpCust..Store+CONTACT.Contact;
                   .AND. IIF(!EMPTY(lcRolExp), cRole $ lcRolExp , .T. )
        llAddRec = (FOUND() .OR. EMPTY(lcRolExp))
        *:B606453,1 SSH
        IF llAddRec
          *--- 606510
          *--- cTranType+CEMAIL_ADD
          SELECT (lcWorkFile)
          LOCATE FOR cTranType+CEMAIL_ADD = SUBSTR(laRpTskTr[lnI],1,1)+CONTACT.CEMAIL_ADD
          IF !FOUND()
            *--- 606510
            INSERT INTO (lcWorkFile) FROM MEMVAR
            SELECT (lcWorkFile)
            *:B606453,1 SSH 09/03/2002 Remove contact.
            *REPLACE &lcWorkFile..cRole   WITH CONTROLE.cRole,;
                     &lcWorkFile..Contact WITH CONTROLE.Contact
            REPLACE cRole   WITH CONTROLE.cRole
            *:B606453,1 SSH 
            *B606208,1 RAE Modi(3) [Start]
            REPLACE cUser_id   WITH IIF(EMPTY(lcRpUsrID),gcUser_ID,lcRpUsrID) ,;
                    cTranType  WITH laRpTskTr[lnI] ,;
                    cSubject   WITH lcRpText ,;
                    dtrandate  WITH ldRpSchDat ,;
                    ctrantime  WITH LEFT(TIME(),5)+' AM'
            *B606208,1 RAE [End]              
            *:B606453,1 SSH 09/03/2002 Remove contact.
            *IF SEEK('C'+PADR(Account,8)+Store+Contact,'CONTACT')
            *:B606453,1 SSH 09/03/2002 Remove contact.
              IF (EMPTY(CONTACT.Fax) AND 'F' $ lcRpContTp) AND;
                 (EMPTY(CONTACT.CEMAIL_ADD) AND 'E' $ lcRpContTp)
                REPLACE Delete     WITH .T.
              ELSE   
                REPLACE Phone      WITH CONTACT.Phone,;
                        Fax        WITH CONTACT.Fax,;
                        mNotes     WITH CONTACT.mNote,;        
                        CEMAIL_ADD WITH CONTACT.CEMAIL_ADD,;
                        cTranReson WITH lcResone,;
                        Contact    WITH CONTACT.Contact
              ENDIF
            FLUSH
          *:B606453,1 SSH 09/03/2002 Commented ou.
          ENDIF
          *:B606453,1 SSH 
        ENDIF
      ENDSCAN
    ENDIF
    *:B606453,1 SSH 09/03/2002 Generate taska at contact level not roll level.
    *ENDSCAN
    *:B606453,1 SSH 09/03/2002 Generate taska at contact level not roll level.
  *:B606453,1 SSH 09/03/2002 Generate taska for contact even if there is not any roll.
  *ENDIF
  *:B606453,1 SSH 09/03/2002.
ENDSCAN
ENDFOR
SELECT (lcWorkFile)
DELETE FOR &lcWorkFile..Delete 

*!**************************************************************************
*! Name      : lfUpdUsrTs
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Update user task list
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfUpdUsrTs()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
FUNCTION lfUpdUsrTs
llUpdat  = .F.
*B606208,1 RAE Modi(3) [Start]
*DIMENSION laConttype[ALEN(laRpTskTr,1)]
*FOR lnI = 1 TO ALEN(laRpTskTr,1)
*  DO CASE
*    CASE laRpTskTr[lnI] = 'Todo' 
*      laConttype[lnI] = 'T'
*    CASE laRpTskTr[lnI] = 'Call' 
*      laConttype[lnI] = 'C'
*    CASE laRpTskTr[lnI] = 'Appointment'         
*      laConttype[lnI] = 'A'
*  ENDCASE
*ENDFOR
SELECT (lcWorkFile)
INDEX ON cSelcKey TAG (lcTempSel)
REPLACE ALL cSelcKey WITH cTranType+PADR(STR(RECNO(),6),6)
SELECT * FROM (lcWorkFile) INTO DBF (lcTmpDbf)
LOCATE

*:B606453,1 SSH 09/03/2002 Initia variable
llContinuo = .F.
*:B606453,1 SSH 09/03/2002 

IF RECCOUNT() <> 0
  *--- 606510
  *lcBrowFld = "cTranType = IIF(CTRANTYPE='A','Appointment',IIF(CTRANTYPE='T','To Do','Call')) :R :H='Activity',;
               dtrandate :R :H='Date', ctrantime :R :H='Time', CUSER_ID :R :H='User',;
               CONTACT :R :H='Contact', cTranReson :R :H='Reason'"
  SELECT (lcWorkFile)
  lcBrowFld = [cTranType = IIF(CTRANTYPE='A','Appointment',IIF(CTRANTYPE='T','To Do','Call')) :R :H='Activity']
  lcBrowFld = lcBrowFld + [, Account :R :H='Account', lcName = IIF(EMPTY(Store),btName,stName) :R :H='Name']
  
  DO CASE
    CASE 'E' == ALLTRIM(lcRpContTp)
      lcBrowFld = lcBrowFld + [, CONTACT :R :H='Contact', CEMAIL_ADD :R :H='Email address']
      DELETE ALL FOR EMPTY(CEMAIL_ADD)
      PACK
    CASE 'F' == ALLTRIM(lcRpContTp)
      lcBrowFld = lcBrowFld + [, CONTACT :R :H='Contact', Fax :R :H='Fax']
      DELETE ALL FOR EMPTY(FAX)
      PACK
    CASE 'F' $ ALLTRIM(lcRpContTp) .AND.  'E' $ ALLTRIM(lcRpContTp)
      lcBrowFld = lcBrowFld + [, CONTACT :R :H='Contact', CEMAIL_ADD :R :H='Email address', Fax :R :H='Fax']
      DELETE ALL FOR EMPTY(FAX) .OR. EMPTY(CEMAIL_ADD)
      PACK
  ENDCASE

  lcBrowFld = lcBrowFld + [, cDesc=gfCodDes(cTranReson,'CTRANRESON') :H='Reason' :R]  
  lcBrowFld = lcBrowFld + [, dtrandate :R :H='Date', ctrantime :R :H='Time', CUSER_ID :R :H='User' , Store :R :H='Store']
  *--- 606510
  SELECT (lcWorkFile)
  SET ORDER TO (lcTempSel)
  *:B606453,1 SSH 09/03/2002 Change varianle name.
  *lcval=gfrange(lcBrowFld,lcWorkFile,"cSelcKey","FOR !EMPTY(Account)","","","@! XXXXXXXXXXXXXXXXXXX")
  llContinuo = gfrange(lcBrowFld,lcWorkFile,"cSelcKey","FOR !EMPTY(Account)","","","@! XXXXXXXXXXXXXXXXXXX")
  *:B606453,1 SSH 09/03/2002.
ENDIF
*:B606453,1 SSH 09/03/2002 Use another variable.
*IF lcval
IF llContinuo
*:B606453,1 SSH 09/03/2002
  SELECT (lcTmpDbf)
  INDEX ON cSelcKey TAG (lcTmpDbf)
  SET ORDER TO (lcTmpDbf)
  
  SELECT (lcWorkFile)
  SET RELATION TO cSelcKey INTO (lcTmpDbf) ADDITIVE
  SCAN
    SELECT (lcTmpDbf)
    *FOR lnI = 1 TO ALEN(laConttype,1)
    *B606208,1 RAE [End]
    SCAN FOR cSelcKey = &lcWorkFile..cSelcKey
      llUpdat = .T.
      SELECT SYSCHDUL
      lcSeqNo = gfSequence('CSEQNUMBER')
      APPEND BLANK
      =gfAdd_Info('SYSCHDUL')
      ldRpEntDat = IIF(EMPTY(ldRpEntDat),gdSysDate,ldRpEntDat)

      *B606208,1 RAE - Modi(2) Update SYSCHDUL.cSubject with the new text object in 
      *                        Option Grid and SYSCHDUL.ctrantype with laRpTskTr.[Start]
      *REPLACE cSeqNumber WITH lcSeqNo,;
              cuser_id   WITH IIF(EMPTY(lcRpUsrID),gcUser_ID,lcRpUsrID),;
              cconttype  WITH 'C',;
              ccont_id   WITH &lcWorkFile..Account,;
              store      WITH &lcWorkFile..store,;
              contact    WITH &lcWorkFile..contact,;
              CSUBJECT   WITH gfCodDes(&lcWorkFile..cRole,'CROLE'),;
              cphone     WITH &lcWorkFile..phone,;
              cpriority  WITH &lcWorkFile..priority,;
              ctrantype  WITH laConttype[lnI],;
              dtrandate  WITH ldRpSchDat,;
              ctrantime  WITH LEFT(TIME(),5)+' AM',;
              ctranreson WITH &lcWorkFile..ctranreson,;
              mNotes     WITH &lcWorkFile..mNotes,;
              CCOMPLETED WITH 'N',;         
              ccomp_id   WITH gcAct_Comp,;
              DADD_DATE  WITH ldRpEntDat,;
              cowner     WITH gcUser_Id
     
      REPLACE cSeqNumber WITH lcSeqNo,;
              cuser_id   WITH IIF(EMPTY(lcRpUsrID),gcUser_ID,lcRpUsrID),;
              cconttype  WITH 'C',;
              ccont_id   WITH &lcTmpDbf..Account,;
              store      WITH &lcTmpDbf..store,;
              contact    WITH &lcTmpDbf..contact,;
              CSUBJECT   WITH lcRpText,;
              cphone     WITH &lcTmpDbf..phone,;
              cpriority  WITH &lcTmpDbf..priority,;
              ctrantype  WITH &lcTmpDbf..cTranType,;
              dtrandate  WITH ldRpSchDat,;
              ctrantime  WITH LEFT(TIME(),5)+' AM',;
              ctranreson WITH &lcTmpDbf..ctranreson,;
              mNotes     WITH &lcTmpDbf..mNotes,;
              CCOMPLETED WITH 'N',;         
              ccomp_id   WITH gcAct_Comp,;
              DADD_DATE  WITH ldRpEntDat,;
              cowner     WITH gcUser_Id
      *B606208,1 RAE [End]
    ENDSCAN
  ENDSCAN
ENDIF
*ENDFOR

IF llUpdat
  *:B#606115,1 SSH Fix spelling mistake.
  *=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Generating User task list Completed successfully.')
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'Generating User Task List completed successfully.')
  *:B#606115,1 SSH (END)
ELSE
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'No tasks generated.')
ENDIF

*!**************************************************************************
*! Name      : lfsCustmr
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Directs Customer File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsCustmr()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
FUNCTION lfsCustmr

PARAMETERS lcParm

GO TOP  IN Customer

*-- End of lfsCustmr.

*!**************************************************************************
*! Name      : lfsRep
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Directs SALESREP File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsRep()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
FUNCTION lfsRep

PARAMETERS lcParm

GO TOP IN SALESREP

*-- End of lfsCustmr.
*:************************************************************************
*: Program file  : lfvUserId
*: Program desc. : Browse function for avilable Users.
*:         Module: Aria Apparel Series.
*:      Developer: HEND GHANEM (HBG)
*:************************************************************************
FUNCTION lfvUserId

PRIVATE lcTitle, lcBrFields, lnCurAlias

lnCurAlias   = SELECT(0)
lcUser_Id = EVALUATE(SYS(18))
lcUser = STRTRAN(lcUser_Id," ","")
IF EMPTY(lcUser)
  lcUser_Id = ""
ENDIF
IF !EMPTY(lcOldValue) AND lcUser_Id = lcOldValue
  RETURN
ENDIF
lcFldLocNam = SYS(18)
SELECT SYUUSER
GO TOP
IF EOF()
  SELECT (lnCurAlias)
  lcUser_Id = PADR(ALLTRIM(lcUser_Id),10)
ELSE
  lcBrFields = "cUser_Id : H='User ID',cUsr_Name : H='User Name'"
  lcSetExact = SET('EXACT')
  SET EXACT ON
  lcTitle    = 'Users'
  DIMENSION latemp[1]
  IF !EMPTY(lcUser_Id) AND !SEEK(ALLTRIM(lcUser_Id))
    lcUser_Id = IIF(ARIABROW('',lcTitle,;
                     gnBrFSRow1, gnBrFSCol1, gnBrFSRow2, gnBrFSCol2,'','','SYUUSER.cUser_Id','latemp'),;
                     SYUUSER.cUser_Id, SPACE(10))
    &lcFldLocNam = lcUser_Id                   
  ENDIF
  SET EXACT &lcSetExact
ENDIF

*=lfpbRun()

SELECT (lnCurAlias)

*!*************************************************************
*! Name      : lfOldValue
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Function to store old value of the current filed.
*!*************************************************************
FUNCTION lfOldValue
lcOldValue = EVALUATE(SYS(18))
RETURN


*!*************************************************************
*! Name      : lfvDateRng
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Showes date range screen
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : lfObjState,lfSelcObjs,gfGetMemVar
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvDateRng()
*!*************************************************************

FUNCTION lfvDateRng

PRIVATE ldFrom,ldTo

ldFrom = ldRPMatLDt
ldTo   = ldRPMatHDt

lcTitle = 'Mainten expiration date'
DO DateRng.Spx

ldRPMatLDt = ldFrom
ldRPMatHDt = ldTo
*-- end of lfvDateRng.

*!*************************************************************
*! Name      : lfvpbOk
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Validate date range screen's OK button
*!*************************************************************
*! Called from : Option Grid
*!*************************************************************
*! Calls       : .....
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfvpbOk()
*!*************************************************************

FUNCTION lfvpbOk

IF ldFrom > ldTo
   WAIT WINDOW ["From" value must be less than or equal to "To" value] NOWAIT
  _CUROBJ = OBJNUM(ldFrom)
ELSE
  CLEAR READ
ENDIF

*!*************************************************************
*! Name      : lfwOgWhen
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Load Settings before Report starts (When Func.)
*!*************************************************************
*! Called from : option grid of ALSTYAL.PRG
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Return      : None
*!*************************************************************
*! Example     : = lfwOgWhen()
*!*************************************************************

FUNCTION lfwOgWhen
IF lnOgSeting = 1  && if it the default case

  **--- Customer status mover
  DECLARE laRpSource[4],laRpTarget[4]
  STORE 'Potential' TO laRpSource[1] , laRpTarget[1]
  STORE 'Hold'      TO laRpSource[2] , laRpTarget[2]
  STORE 'Active'    TO laRpSource[3] , laRpTarget[3]
  STORE 'Canceled'  TO laRpSource[4] , laRpTarget[4]
  lcRpStatus = 'PHACX'
  
  **--- Task types Mover
  *B606208,1 RAE - Modi(4) [Start]
  *DECLARE laRpTskSr[3],laRpTskTr[1]
  DECLARE laRpTskSr[3]
  *B606208,1 RAE [End]
  STORE 'Todo'        TO laRpTskSr[1] 
  STORE 'Call'        TO laRpTskSr[2]  
  STORE 'Appointment' TO laRpTskSr[3] 
  lcRpTskTyp = 'T'

  *B606208,1 RAE - Modi(4) Save the selected values from the Task type to be 
  *                        appeared next time.[Start]
  IF !llFrstTim
    DECLARE laRpTskTr[ALEN(laRpTskTr,1)]
    FOR lnI = 1 TO ALEN(laRpTskTr,1)
      laRpTskTr[lnI] = laOldValu1[lnI]
    ENDFOR
  ENDIF
  *B606208,1 RAE [End]
  
  **--- Comtract With mover
  *B606208,1 RAE - Modi(4) [Start]
  *DECLARE laRpContSr[2],laRpContTr[1]
  *B606208,1 RAE [End]
  DECLARE laRpContSr[2]
  STORE 'Email Address' TO laRpContSr[1] , laRpContTr[1]
  STORE 'Fax #'         TO laRpContSr[2]  
  lcRpContTp = 'E'
  
  *B606208,1 RAE - Modi(4) Save the selected values from the Contact type to be 
  *                        appeared next time.[Start]
  IF !llFrstTim
    DECLARE laRpContTr[ALEN(laRpContTr,1)]
    FOR lnI = 1 TO ALEN(laRpContTr,1)
      laRpContTr[lnI] = laOldValu2[lnI]
    ENDFOR
  ENDIF
  *B606208,1 RAE [End]

  **--- Fill the Profile array
  SELECT CCODE_NO + ' - ' + CDISCREP  FROM CODES WHERE CDEFCODE+CRLTFIELD+CFLD_NAME='NNCPRO_CODE ';
      ORDER BY CCODE_NO INTO ARRAY laRpProfSr

  *B606208,1 RAE - Modi(4) Save the selected values from the Profile mover.[Start]
  *DIME laRpProfTr[1],laRpPrfVSr[1],laRpPrfVTr[1]
  *STORE '' TO laRpProfTr,laRpPrfVSr,laRpPrfVTr
  DIME laRpPrfVSr[1]  
  STORE '' TO laRpPrfVSr
  IF !llFrstTim
    DECLARE laRpProfTr[ALEN(laRpProfTr,1)]
    FOR lnI = 1 TO ALEN(laRpProfTr,1)
      laRpProfTr[lnI] = laOldValu3[lnI]
    ENDFOR
  ENDIF
  *-- Save the selected values from the Profile values mover.
  IF !llFrstTim
    DECLARE laRpPrfVTr[ALEN(laRpPrfVTr,1)]
    FOR lnI = 1 TO ALEN(laRpPrfVTr,1)
      laRpPrfVTr[lnI] = laOldValu4[lnI]
    ENDFOR
    
    FOR lnInd = 1 TO ALEN(laOgFxFlt,1)
      IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'CONTROLE.CROLE'
        laOgFxFlt[lnInd,6] = lcRoleCode
      ENDIF
      IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.REGION'
        laOgFxFlt[lnInd,6] = lcRegion
      ENDIF
      IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.SHIPVIA'
        laOgFxFlt[lnInd,6] = lcShipVia
      ENDIF
      IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.SPCINST'
        laOgFxFlt[lnInd,6] = lcSpecInst
      ENDIF
      IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.CLASS'
        laOgFxFlt[lnInd,6] = lcCustClas
      ENDIF
      IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.CTERMCODE'
        laOgFxFlt[lnInd,6] = lcTerms
      ENDIF
      IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'CUSTOMER.CDIVISION'
        laOgFxFlt[lnInd,6] = lcDivision
      ENDIF
      IF ALLTRIM(laOgFxFlt[lnInd,1]) = 'SYSCHDUL.CTRANRESON'
        laOgFxFlt[lnInd,6] = lcOldReson
      ENDIF
    ENDFOR
  ENDIF
  *B606208,1 RAE - Modi(4) [End]
ENDIF

**-- Temp file for store In Range
CREATE DBF (lcTmpStore) (TYPE C(1),Account C(5),Store C(8),STNAME C(30),PHONE1 C(16),;
                           CADDRESS6 C(20),NETBAL N(14,2))

*-- end of lfwOgWhen.
*!*************************************************************
*! Name      : lfvOStatus  
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : - Evaluate Status expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOStatus()
*!*************************************************************
FUNCTION lfvOStatus
PRIVATE lcOldStat

lcOldStat = lcRpStatus  && Save old status value.

= gfMover(@laRpSource,@laRpTarget,'Select Customer status',.T.,'')  && call mover function.

lcRpStatus = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTarget[1])
  FOR lnI = 1 TO ALEN(laRpTarget,1)
    lcRpStatus = lcRpStatus + IIF(laRpTarget[lnI] = 'Potential','P',;
                              IIF(laRpTarget[lnI] = 'Hold','H',;
                              IIF(laRpTarget[lnI] = 'Active','A' , ;
                              IIF(laRpTarget[lnI] = 'Canceled','X',''))))
  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpStatus = IIF(EMPTY(lcRpStatus),'PHAXC', ALLTRIM(lcRpStatus))

*!*************************************************************
*! Name      : lfvOTasks 
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : - Evaluate Tasl types expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOTasks()
*!*************************************************************
FUNCTION lfvOTasks
PRIVATE lcOldStat

lcOldStat = lcRpTskTyp && Save old status value.
= gfMover(@laRpTskSr,@laRpTskTr,'Select Task Type',.T.,'')  && call mover function.

lcRpTskTyp = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpTskTr[1])
  FOR lnI = 1 TO ALEN(laRpTskTr,1)
    lcRpTskTyp = lcRpTskTyp+  IIF(laRpTskTr[lnI] = 'Todo','T',;
                              IIF(laRpTskTr[lnI] = 'Call','C',;
                              IIF(laRpTskTr[lnI] = 'Appointment','A' ,'')))                              

  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpTskTyp = IIF(EMPTY(lcRpTskTyp),'T', ALLTRIM(lcRpTskTyp))

*B606208,1 RAE - Modi(4) Save the selected values from Task type mover. [Start]
DIMENSION laOldValu1[ALEN(laRpTskTr,1)]
FOR lnI = 1 TO ALEN(laRpTskTr,1)
  laOldValu1[lnI] = laRpTskTr[lnI]
ENDFOR  
*B606208,1 RAE [End]
*=lfpbRun()

*!*************************************************************
*! Name      : lfvOContr 
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : - Evaluate Contact types expression.
*!*************************************************************
*! Calls     : 
*!             Procedures : ....
*!             Functions  : ....
*!*************************************************************
*! Called from : Report code
*!*************************************************************
*! Passed Parameters  : String have Pipes,Number of Pieps.
*!*************************************************************
*! Returns            : InList Expression like ["AS","BS","CS"]
*!*************************************************************
*! Example   : = lfvOContr()
*!*************************************************************
FUNCTION lfvOContr
PRIVATE lcOldStat

lcOldStat = lcRpContTp && Save old status value.

= gfMover(@laRpContSr,@laRpContTr,'Select Contact Types',.T.,'')  && call mover function.

lcRpContTp = ' '
*-- Loop to make Status expression.
IF !EMPTY(laRpContTr[1])
  FOR lnI = 1 TO ALEN(laRpContTr,1)
    lcRpContTp = lcRpContTp + IIF(laRpContTr[lnI] = 'Email Address','E',;
                              IIF(laRpContTr[lnI] = 'Fax #','F',''))                              

  ENDFOR  && end Loop to make Status expression.
ENDIF

lcRpContTp = IIF(EMPTY(lcRpContTp),'E', ALLTRIM(lcRpContTp))

*B606208,1 RAE  - Modi(4) Save the selected values from Contact type mover. [Start]
DIMENSION laOldValu2[ALEN(laRpContTr,1)]
FOR lnI = 1 TO ALEN(laRpContTr,1)
  laOldValu2[lnI] = laRpContTr[lnI]
ENDFOR  
*B606208,1 RAE [End]

*!**************************************************************************
*! Name      : lfsCustmr
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Directs Customer File to Go Top in the InRange
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfsCustmr()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
FUNCTION lfsStore
PARAMETERS lcParm

lcTmpAcc = laOGFxFlt[1,6]
lcAccounts = ""
IF USED(lcTmpAcc)
  SELECT (lcTmpAcc)
  llFirst = .T.
  SCAN
    IF llFirst 
      llFirst = .F.
      lcAccounts = &lcTmpAcc..Account
    ELSE
      lcAccounts = lcAccounts + "," + &lcTmpAcc..Account
    ENDIF
  ENDSCAN
ENDIF

SELECT CUSTOMER.TYPE,CUSTOMER.Account,CUSTOMER.Store,CUSTOMER.STNAME,CUSTOMER.PHONE1,;
       CUSTOMER.CADDRESS6,CUSTOMER.NETBAL FROM CUSTOMER WHERE;
     CUSTOMER.type+CUSTOMER.account+CUSTOMER.store = 'S' AND CUSTOMER.account $ lcAccounts;
     INTO DBF (lcTmpStore)

SELECT (lcTmpStore)
INDEX ON Type+Account+Store TAG (lcTmpStore)                           
LOCATE

*!**************************************************************************
*! Name      : lfOpnFiles
*! Developer : Hend Ghanem (HBG)
*! Date      : 09/04/2002
*! Purpose   : Open the needed files
*!**************************************************************************
*! Called from : Option Grid
*!**************************************************************************
*! Example   : =lfOpnFiles()
*!**************************************************************************
*! Note      : 
*!**************************************************************************
FUNCTION lfOpnFiles

= gfOpenFile(gcDataDir+'CUSTOMER',gcDataDir+'CUSTOMER','SH')
= gfOpenFile(gcDataDir+'CONTACT',gcDataDir+'CONTACT','SH')
= gfOpenFile(gcDataDir+'CONTROLE',gcDataDir+'CONTROLE','SH')
= gfOpenFile(gcDataDir+'PROFILE',gcDataDir+'PROFILE','SH')
= gfOpenFile(gcDataDir+'SALESREP',gcDataDir+'SALESREP','SH')
= gfOpenFile(gcSysHome+'SYUUSER',gcSysHome+'CUSER_ID','SH')
= gfOpenFile(gcDataDir+'CODES',gcDataDir+'IDRLTFNAME','SH')
= gfOpenFile(gcSysHome+'SYSCHDUL',gcSysHome+'COPRUSR','SH')
*B606208,1 RAE - Modi(1) Open ARPRFCOD file. [Start]
=gfOpenFile(gcDataDir+'ARPRFCOD',gcDataDir+'PROFILE','SH')
*B606208,1 RAE [End]
IF 'SU'$ gcCmpModules
  = gfOpenFile(gcDataDir+'CUSTPROF',gcDataDir+'CUST_ID','SH')
ENDIF
  
*:**************************************************************************
*:* Name        : lfvProfile
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/22/2002
*:* Purpose     : Profile valid fn.
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvProfile()
*:***************************************************************************
FUNCTION lfvProfile
PRIVATE lnDim,lcOld,I
lcOld =''
FOR I = 1 TO ALEN(laRpProfTr,1)
  lcOld = lcOld  + LEFT(laRpProfTr[I],6)
ENDFOR
=gfMover(@laRpProfSr,@laRpProfTr,'Select Profile',.T.,'')  && call mover function.

DIME laRpPrfVSr[1]
STORE '' TO laRpPrfVSr
lcAlias = Alias()
*B606208,1 RAE Modi(1) [Start]
*SELECT PROFILE
SELECT ARPRFCOD
*B606208,1 RAE [End]
lnDim = 1

*:B#606115,1 SSH Fix duplicate records in profile values. 
*SCAN FOR ASCAN(laRpProfTr,PROFILE.CPRO_CODE ) > 0

*B606208,1 RAE Modi(1) [Start]
*SCAN FOR ASCAN(laRpProfTr,PROFILE.CPRO_CODE ) > 0 .AND. ;
         ASCAN(laRpPrfVSr,PROFILE.CPRO_CODE + ' - '+PROFILE.CPRO_VALUE) = 0
*:B#606115,1 SSH (END)
SCAN FOR ASCAN(laRpPrfVSr,ARPRFCOD.CPRO_CODE + ' - '+ARPRFCOD.CPRO_VALUE) = 0 .AND. ;
         ASCAN(laRpProfTr,ARPRFCOD.CPRO_CODE ) > 0
*B606208,1 RAE [End]

  DIME laRpPrfVSr[lnDim]
  *B606208,1 RAE - Modi(1) Select Profile values from ARPRFCOD instead of from PROFILE [start]
  *laRpPrfVSr[lnDim] = PROFILE.CPRO_CODE + ' - ' + PROFILE.CPRO_VALUE
  laRpPrfVSr[lnDim] = ARPRFCOD.CPRO_CODE + ' - ' + ARPRFCOD.CPRO_VALUE
  *B606208,1 RAE [End]
  lnDim = lnDim + 1
ENDSCAN
SELECT (lcAlias)

lcDelted = ''
FOR I = 1 TO LEN(lcOld)/6
  IF ASCAN(laRpProfTr,SUBSTR(lcOld,(I-1)*6+1,6)) = 0
    lcDelted = SUBSTR(lcOld,(I-1)*6+1,6)
    lnLoc = ASCAN(laRpPrfVTr,lcDelted+' - ' )
    DO WHILE lnLoc > 0
      =ADEL(laRpPrfVTr, lnLoc )
      DIME laRpPrfVTr[IIF(ALEN(laRpPrfVTr,1)=1,1,ALEN(laRpPrfVTr,1)-1)]
      lnLoc = ASCAN(laRpPrfVTr,lcDelted+' - ' )
    ENDDO
  ENDIF
ENDFOR

*B606208,1 RAE - Modi(4) Save the selected values from Profile mover. [Start]
DIMENSION laOldValu3[ALEN(laRpProfTr,1)]
FOR lnI = 1 TO ALEN(laRpProfTr,1)
  laOldValu3[lnI] = laRpProfTr[lnI]
ENDFOR  
*B606208,1 RAE [End]

*-- end of lfvProfile.
*:**************************************************************************
*:* Name        : lfvPrfVlus
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/22/2002
*:* Purpose     : Profile values mover
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfvPrfVlus()
*:***************************************************************************
FUNCTION lfvPrfVlus

IF !EMPTY(laRpPrfVSr[1])
  *= gfMover(@laRpPrfVSr,@laRpPrfVTr,'Select Profile Values',.T.,'')  && call mover function.
  = gfMover(@laRpPrfVSr,@laRpPrfVTr,'Select Profile values',.T.,'')  && call mover function.
ELSE
  *:B#606115,1 SSH Fix  spelling mistake.
  *=gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'No Profiles Selected Or Profile has no Profile Values.')
  =gfModalGen("TRM00000B00000","DIALOG",.F.,.F.,'No Profiles selected or Profile has no Profile values.')
  *:B#606115,1 SSH (END)
ENDIF   

*B606208,1 RAE - Modi(4) Save the selected values from Profile values mover. [Start]
DIMENSION laOldValu4[ALEN(laRpPrfVTr,1)]
FOR lnI = 1 TO ALEN(laRpPrfVTr,1)
  laOldValu4[lnI] = laRpPrfVTr[lnI]
ENDFOR  
*B606208,1 RAE [End]

*-- end of lfvPrfVlus.

*:**************************************************************************
*:* Name        : lfDummy
*:* Developer   : TMI - TAREK MOHAMED IBRAHIM
*:* Date        : 04/22/2002
*:* Purpose     : Fill laReason , laRea2 arrays
*:***************************************************************************
*:* Called from : 
*:***************************************************************************
*:* Parameters : None
*:***************************************************************************
*:* Return      : None
*:***************************************************************************
*:* Example     :  = lfDummy()
*:***************************************************************************
FUNCTION lfDummy

**--- Fill the Reason array
SELECT CCODE_NO + ' - ' + CDISCREP  FROM CODES WHERE CDEFCODE+CRLTFIELD+CFLD_NAME='NNCRESULTCD ' ;
    ORDER BY CCODE_NO INTO ARRAY laReason
DIME laRea2[ALEN(laReason,1)]
FOR I = 1 TO ALEN(laReason,1)
  laRea2[I] = PADR(laReason[I],6)
ENDFOR
laRpReason = laRea2[1] 
*-- end of lfDummy.