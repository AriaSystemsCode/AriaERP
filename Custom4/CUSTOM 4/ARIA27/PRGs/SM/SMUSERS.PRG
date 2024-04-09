*:************************************************************************
*:
*: Procedure file: SMUSERS.PRG 
*:                 (Users)
*:
*:         System: ARIA ADVANTAGE SYRESE
*:         Module: SYSTEM MANAGER
*:         Author: Reham Aly Alallamy
*:      Copyright (c) 
*:************************************************************************
* Last modified:  /  /
*
*B600116,1 MAN 05/27/95 Fixing the Passward if you ' tab ' or ' enter '
*B600116,1 MAN 05/27/95 without editing the passward it was deleting it
*
*E300268,1 Reham 08/14/95 1- Add new field CEXTPRG in the SYUUSER.DBF
*E300268,1 Reham 08/14/95 2- Add new record in the SYDSCOBJ.DBF for the
*E300268,1                   added field & give it array elem. no. 29
*E300268,1 Reham 08/14/95 3- Add new object in the user default screen
*E300268,1                   to validate the new external program field.
*E300275,1 Reham On 08/23/95
*E300275,1 We Have to have the ability to blank the default bitmap.
*E300683,4 AHMED 06/08/97 Add prgs directory path to the calling of programs
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*B603002,1 Hesham 06/21/99
*B603002,1 Allow assigning group security to a user in the user screen.
*B802353,1 M.H_NY 06/16/99 Increase the NRECNO field to No.
*B802375,1 MAN 07/19/99 Allow Alphanumeric entry for the user ID
*B603248,1 MAN 10/27/1999
*B603248,1 User Password can not take digits
*C200249,7 AMH 01/29/2002 Add prompte days field for MBI.
*B607468,1 ALB 08/19/2003 Do not allow to add user ID named "ARIA"
*C123615,1 TMI 11/08/2004 Add "Option" pad to Users system screen via a trigger for nik nak to update user field "cemail"
*C124836,1 MHM 11/25/2004 Custom auto create material PO from Sales Order
*C123847,1 TMI 01/04/2005 Add "Option" pad to Users system screen via a trigger for DIR03 to update user field "SYUUSER.AUTHENTL"
*C127341,5 TMI 08/07/2005 Add a standard "Option" pad to Users system screen to update user field "SYUUSER.LAPRV" for approving shipments
*E302251,1 AMH 09/05/2005 Add Email and report template for user information.
************************************************************************************************************

EXTERNAL ARRAY laData,laScrMode,laDefProc

*B603002,1 Hesham (Start)
*DECLARE laKeyField[1,4],laWndObj[4,3],laCompInfo[3,1],;
        laModInfo[2,1],laWrkInfo[2,1],laPrntInfo[2,1]


DECLARE laKeyField[1,4],laWndObj[4,3],laCompInfo[3,1],;
        laModInfo[2,1],laWrkInfo[2,1],laPrntInfo[2,1],laGroups[1,2]
*B603002,1 Hesham (End)
*E300268,1 Reham On 08/22/95   *** Begin ***
*E300268,1 This array to hold the names of the external programs in the menu.
DECLARE laExtPrg[1,1]

laExtPrg = " "

lcNone   = "None"
*E300268,1 Reham On 08/22/95   *** End   ***

laKeyField[1]   = 'laData[1]'
laKeyField[1,2] =.T.
laKeyField[1,3] = 'CUSER_ID'
laKeyField[1,4] = 1

laDefProc [7] = .F.
laDefProc [9] = .F.


lcPassWrd  = SPACE(8)
lcOld_ID   = ''
lcOld_Mod  = ''
lcOld_Pr   = ''
lcOld_wrk  = ''
lcOldColor = ''
lcOldFil   = ''
lcComNam   = ''
lcModNam   = ''
lcPrintNam = ''
lcwrk_name = ''
lctUsrDef  = "User Defaults"
lctUsrInf  = "User Log Information"
lcPrv_Tmp1 = " "
lcPrv_Tmp2 = " "
lcCopUsTmp = " "
lcGrp_Tmp  = " "
lcLastData = ''
lcPrvStat  = " "
lcDefStat  = " "
lcOTypHead = " "
lcCompMdl  = " "

*E300268,1 Reham On 08/14/95   *** Begin ***
lcOldExtP = " "     && *E300268,1 Var. to hold the old external program.
lcExtPrg  = " "     && *E300268,1 Var. to hold the external program on the screen.
*E300268,1 Reham On 08/14/95   *** End   ***

lnLastKey  = 0

llUpdPriv  = .F.
llBrowse   = .F.

rbUsrLvl   = 2
rbAudit    = 1
rbHours    = 1

lcOld_Tmp = ''

IF !gfSetup()    
  RETURN
ENDIF  

*B603002,1 Hesham (Start)
puUGroup = 1
*B603002,1 Hesham (End)

laWndObj[1,1]  = gcBaseWind
laWndObj[1,2]  = 'LADATA[1]'
laWndObj[1,3]  = 'PBLOGINFO'

laWndObj[2,1]  = 'GWCCONTRL1'
laWndObj[2,2]  = 'PBTOP'
laWndObj[2,3]  = 'PBCLS'

laWndObj[3,1]  = 'CWDSMUSRDF'
laWndObj[3,2]  = 'LADATA[12]'
laWndObj[3,3]  = 'PBDFCLOSE'

laWndObj[4,1]  = 'CWDSMLGINF'
laWndObj[4,2]  = 'laData[20]'
laWndObj[4,3]  = 'PBINFCLOSE'

IF !WEXIST(lcBaseWind)
  SCATTER  FIELDS &lcScFields TO laData BLANK
  
  *C200249,7 AMH Add prompte days field for MBI [Start]
  IF ASCAN(laEvntTrig , PADR('TODOMBI',10)) <> 0
    SCATTER FIELDS NPROMDAYS MEMVAR BLANK
  ENDIF
  *C200249,7 AMH [End]
  
  *E302251,1 AMH Add Email and report template for user information [Start]
  SCATTER FIELDS CEMAIL_ADD,SYUUSER.CTMP_CODE MEMVAR BLANK
  *E302251,1 AMH [End]
  
  llUpdPriv  = .F.
  
  *** Get name for the user privilege temp. file. ***
  lcPrv_Tmp1 = gfTempName()
  lcPrv_Tmp2 = gfTempName()
  lcCopUsTmp = gfTempName()
  lcGrp_Tmp  = gfTempName()

  *** Create the user privilege temp. file. ***
  SELECT SYUUSRPR
  DIMENSION laUserPrvl[1,4]
  =AFIELDS(laUserPrvl)

  CREATE DBF (gcWorkDir+lcGrp_Tmp) FROM ARRAY laUserPrvl

  DIMENSION laUserPrvl[ALEN(laUserPrvl,1)+3,4]

  laUserPrvl[ALEN(laUserPrvl,1)-2,1] = 'NRECNO'
  laUserPrvl[ALEN(laUserPrvl,1)-2,2] = 'N'
  *B802353,1 Begin.  
  *laUserPrvl[ALEN(laUserPrvl,1)-2,3] = 3
  laUserPrvl[ALEN(laUserPrvl,1)-2,3] = 6
  *B802353,1 End.
  laUserPrvl[ALEN(laUserPrvl,1)-2,4] = 0

  laUserPrvl[ALEN(laUserPrvl,1)-1,1] = 'CSTATUS'
  laUserPrvl[ALEN(laUserPrvl,1)-1,2] = 'C'
  laUserPrvl[ALEN(laUserPrvl,1)-1,3] = 1
  laUserPrvl[ALEN(laUserPrvl,1)-1,4] = 0

  laUserPrvl[ALEN(laUserPrvl,1),1] = 'LEXIST'
  laUserPrvl[ALEN(laUserPrvl,1),2] = 'L'
  laUserPrvl[ALEN(laUserPrvl,1),3] = 1
  laUserPrvl[ALEN(laUserPrvl,1),4] = 0

  CREATE DBF (gcWorkDir+lcPrv_Tmp1) FROM ARRAY laUserPrvl
  
  CREATE DBF (gcWorkDir+lcPrv_Tmp2) FROM ARRAY laUserPrvl
  
  CREATE DBF (gcWorkDir+lcCopUsTmp) FROM ARRAY laUserPrvl
  
  SELECT (lcPrv_Tmp2)
  INDEX ON ALLTRIM(CAPP_ID)+ALLTRIM(CPROSS_ID)+CPROCTYPE+ALLTRIM(CCOMP_ID) TAG CAPP_ID
  SET ORDER TO TAG CAPP_ID

  *E300268,1 Reham On 08/14/95   *** Begin ***
  SELECT SYCMENU
  SELECT  DISTINCT CPROSS_ID ;
    FROM  (gcSysHome+"SYCMENU") ;
    WHERE cProcType="E" ;
    INTO  ARRAY laExtPrg
  DIMENSION laExtPrg[ALEN(laExtPrg,1)+1,1]
  =AINS(laExtPrg,1)
  laExtPrg[1,1] = lcNone
  *E300268,1 Reham On 08/14/95   *** End   ***
  
  lcPrvStat = "DISABLE"
  lcDefStat = "DISABLE"
  
  *B603002,1 Hesham (Start)
  =gfOpenFile(gcSysHome+"SYUGROUP")
  SELECT CGROUP_NAM,CGROUP_ID FROM SYUGROUP INTO ARRAY laGroups
  IF _TALLY > 0
    DIMENSION laGroups[ALEN(laGroups,1)+1,ALEN(laGroups,2)]
    =AINS(laGroups,1)
  ENDIF
  laGroups[1,1] = "None"
  laGroups[1,2] = "    "
  =gfCloseFile("SYUGROUP")
  *B603002,1 Hesham (End)
ELSE
  rbHours    = IIF(laScrMode[1] OR laScrMode[4],1,VAL(laData[8])+1)
  rbAudit    = IIF(laScrMode[1] OR laScrMode[4],1,AT(laData[9],"NAPB"))
  rbUsrLvl   = IIF(laScrMode[1] OR laScrMode[4],2,AT(laData[7],"AO"))
  lcPrvStat  = IIF(laScrMode[1],"DISABLE",IIF(laData[7]="A","DISABLE","ENABLE"))
  lcDefStat  = IIF(_DOS,"DISABLE",;
                   IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE","ENABLE"))
  
  *E300268,1 Reham On 08/14/95   *** Begin ***
  lcExtPrg   = IIF(!EMPTY(laData[29]),laData[29],lcNone)
  lcSavExact = SET("EXACT")
  SET EXACT ON
  puExt = IIF(ASCAN(laExtPrg,laData[29])>0,ASCAN(laExtPrg,laData[29]),1)
  SET EXACT &lcSavExact
  *E300268,1 Reham On 08/14/95   *** End   ***
ENDIF  

*C127341,11 TMI [Start] create a menu pad as custom
IF ASCAN(laEvntTrig,'APRUSRPR')<>0 .AND. gfGetMemVar('M_APRVSHIP')
  =gfDoTriger('SMUSERS','APRUSRPR')
ENDIF
*C127341,11 TMI [End  ] 

*C123615,1  TMI [Start] Add "Option" pad to Users system screen via a trigger for nik nak to update user field "cemail"
*C124836,1 MHM 11/25/2004 Custom auto create material PO from Sales Order[Start]
IF ASCAN(laEvntTrig,'SMEMAIL') <> 0
  =gfDoTriger('SMUSERS','SMEMAIL')
ENDIF
*C124836,1 MHM 11/25/2004 [End]

*C123615,1  TMI [End  ] 
*C123847,1  TMI [Start] Add "Option" pad to Users system screen via a trigger for DIR03 to update user field "SYUUSER.AUTHENTL"
IF ASCAN(laEvntTrig,'USROPTN') <> 0
  =gfDoTriger('SMUSERS','USROPTN')
ENDIF
*C123847,1  TMI [End  ]
SELECT SYUUSER
*E300683,4 Call *.SPR from screens directory
* DO SMUsers.SPR 
DO (gcScrDir + gcWinAppl + '\SMUsers.SPR')
*E300683,4 end   

IF glQuitting

  *C123615,1  TMI [Start] Release Added option pad
  *C124836,1 MHM 11/25/2004 Release pad [Start]
  IF ASCAN(laEvntTrig,'RELPAD') <> 0
    =gfDoTriger('SMUSERS','RELPAD')
  ENDIF
  *C124836,1 MHM [End]  
  *C123615,1  TMI [End  ] 
  
  *C123847,1  TMI [Start] Release defined pad
  IF ASCAN(laEvntTrig,'RELAUPAD') <> 0
    =gfDoTriger('SMUSERS','RELAUPAD')
  ENDIF
  *C123847,1  TMI [End  ]
  
  *C127341,11 TMI [Start] use the trigger "RELGPSPD" to releae the pad for GPS00
  IF ASCAN(laEvntTrig,'RELGPSPD') <> 0
    =gfDoTriger('SMUSERS','RELGPSPD')
  ENDIF
  *C127341,11 TMI [End  ]   
  
  IF WEXIST("CWDSMUSRDF")
    RELEASE WINDOW CWDSMUSRDF
  ENDIF
  
  IF WEXIST("CWDSMLGINF")
    RELEASE WINDOW CWDSMLGINF
  ENDIF
  
  IF USED(lcPrv_Tmp1)
    USE IN ALIAS(lcPrv_Tmp1)
  ENDIF
  ERASE (gcWorkDir+lcPrv_Tmp1+".DBF")
  ERASE (gcWorkDir+lcPrv_Tmp1+".FPT")

  IF USED(lcPrv_Tmp2)
    USE IN ALIAS(lcPrv_Tmp2)
  ENDIF
  ERASE (gcWorkDir+lcPrv_Tmp2+".DBF")
  ERASE (gcWorkDir+lcPrv_Tmp2+".CDX")
  ERASE (gcWorkDir+lcPrv_Tmp2+".FPT")
  
  IF USED(lcCopUsTmp)
    USE IN ALIAS(lcCopUsTmp)
  ENDIF
  ERASE (gcWorkDir+lcCopUsTmp+".DBF")
  ERASE (gcWorkDir+lcCopUsTmp+".FPT")

  IF USED(lcGrp_Tmp)
    USE IN ALIAS(lcGrp_Tmp)
  ENDIF
  ERASE (gcWorkDir+lcGrp_Tmp+".DBF")
  ERASE (gcWorkDir+lcGrp_Tmp+".FPT")
ENDIF  

*!**************************************************************************
*!
*!      Function: lpShow
*!
*!**************************************************************************
*
FUNCTION lpShow

EXTERNAL ARRAY laData,laScrMode

lcLastData = " "

lcPassWrd  = SPACE(8)
lcPassWrd  = IIF((laScrMode[2] OR laScrMode[3]) AND !EMPTY(laData[3]),REPLICAT("*",8),lcPassWrd)

rbHours    = IIF(laScrMode[1]  OR laScrMode[4],1,VAL(laData[8])+1)
rbAudit    = IIF(laScrMode[1]  OR laScrMode[4],1,AT(laData[9],"NAPB"))
rbUsrLvl   = IIF(laScrMode[1]  OR laScrMode[4],2,AT(laData[7],"AO"))
puUGroup   = 1
lcComNam   = IIF(EMPTY(laData[12]),'',;
                  LOOKUP(sycComp.cCom_name ,ALLTRIM(laData[12]),;
                         sycComp.cComp_id,'cComp_id'))

lcCompMdl  =  IIF(EMPTY(laData[12]),'',;
                  ALLTRIM(LOOKUP(syccomp.mcomp_mdl ,ALLTRIM(laData[12]),;
                         sycComp.cComp_id,'cComp_id')))

lcModNam   = IIF(EMPTY(laData[24]),'',;
                  LOOKUP(sydAppl.cApp_name ,ALLTRIM(laData[24]),;
                         sydAppl.cApp_id,'cApp_id'))

lcPrintNam = IIF(EMPTY(laData[13]),'',;
                  LOOKUP(sycPrint.cPrint_nam,ALLTRIM(laData[13]),;
                         sycPrint.cPrint_id,'cPrint_id'))
lcwrk_name = IIF(EMPTY(laData[14]),'',;
                  LOOKUP(sycWrkst.cWrk_name,ALLTRIM(laData[14]),;
                         sycWrkst.cWrkst_id,'cWrkst_id'))

laData[7] = SUBSTR("AO",rbUsrLvl,1)
laData[8] = STR(rbHours-1,1)
laData[9] = SUBSTR("NAPB",rbAudit,1)

SHOW GET lcPassWrd 

lcDefStat  = IIF(_DOS,"DISABLE",;
                 IIF(laScrMode[1] .OR. laScrMode[2],"DISABLE","ENABLE"))
SHOW GET pbDefBmp   &lcDefStat
SHOW GET laData[27] &lcDefStat

*E300268,1 Reham On 08/14/95   *** Begin ***
*E300268,1 Restore the external program name.
lcExtPrg   = IIF(!EMPTY(laData[29]),laData[29],lcNone)
lcSavExact = SET("EXACT")
SET EXACT ON
puExt = IIF(ASCAN(laExtPrg,laData[29])>0,ASCAN(laExtPrg,laData[29]),1)
SET EXACT &lcSavExact
*E300268,1 Reham On 08/14/95   *** End   ***

IF laScrMode[1]
  rbUsrLvl   = 2
  rbAudit    = 1
  *B603002,1 Hesham (Start)
  puUGroup  = 1
  *B603002,1 Hesham (End)
ENDIF

IF !laScrMode[1]
  *B603002,1 Hesham (Start)
  lnGrpPos = ASCAN(laGroups,laData[28])
  IF lnGrpPos > 0
    puUGroup = ASUBSCRIPT(laGroups,lnGrpPos,1)
  ENDIF
  *B603002,1 Hesham (End)
  IF laData[7] = "A"
    SHOW GET pbUsrPrv DISABLE
  *B603002,1 Hesham (Start)
    SHOW GET puUGroup DISABLE
  *B603002,1 Hesham (End)
    
  ELSE
    SHOW GET pbUsrPrv ENABLE
  *B603002,1 Hesham (Start)
    SHOW GET puUGroup ENABLE
  *B603002,1 Hesham (End)
  ENDIF
  
  *C123847,1  TMI [Start] Set mark to the option bar for "Allow Auth. of over entitlement" for DIR03
  IF ASCAN(laEvntTrig,'UPDOPBAR') <> 0
    =gfDoTriger('SMUSERS','UPDOPBAR')
  ENDIF
  *C123847,1  TMI [End  ] 
ELSE
  SHOW GET pbUsrPrv DISABLE
  *B603002,1 Hesham (Start)
    SHOW GET puUGroup DISABLE
  *B603002,1 Hesham (End)
ENDIF

*** Get privilege for current user. ***
IF laScrMode[2]
  lcUsr_grup = SYUUSER.cUsr_grup
  
  *B603002,1 Hesham (Start)
*  lnGrpPos = ASCAN(laGroups,laData[28])
*  IF lnGrpPos > 0
*    puUGroup = ASUBSCRIPT(laGroups,lnGrpPos,1)
*  ENDIF
  *B603002,1 Hesham (End)
  
  IF !EMPTY(lcUsr_grup)
    SELECT * , RECNO() AS 'nRecNo' , "S" AS 'cStatus' , .T. AS 'lExist' ;
           FROM (gcSysHome+"SYUUSRPR") ;
           INTO DBF (gcWorkDir+lcPrv_Tmp1) ;
           WHERE ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(laData[1]) +"U" .OR. ;
                 ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(lcUsr_grup)+"G"
  ELSE
    SELECT * , RECNO() AS 'nRecNo' , "S" AS 'cStatus' , .T. AS 'lExist' ;
           FROM (gcSysHome+"SYUUSRPR") ;
           INTO DBF (gcWorkDir+lcPrv_Tmp1) ;
           WHERE ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(laData[1])+"U"
  ENDIF

  *B603002,1 Hesham (Start)
    SHOW GET puUGroup DISABLE
  *B603002,1 Hesham (End)
  
  *C200249,7 AMH Add prompte days field for MBI [Start]
  IF ASCAN(laEvntTrig , PADR('TODOMBI',10)) <> 0
    SCATTER FIELDS NPROMDAYS MEMVAR
  ENDIF
  *C200249,7 AMH [End]
  
  *E302251,1 AMH Add Email and report template for user information [Start]
  SCATTER FIELDS CEMAIL_ADD,SYUUSER.CTMP_CODE MEMVAR
  *E302251,1 AMH [End]
  
ENDIF

*C200249,7 AMH Add prompte days field for MBI [Start]
IF ASCAN(laEvntTrig , PADR('TODOMBI',10)) <> 0
  IF laScrMode[2]
    SHOW GET m.nPromDays DISABLE
  ELSE
    SHOW GET m.nPromDays ENABLE
  ENDIF
ENDIF
*C200249,7 AMH [End]

*E302251,1 AMH Add Email and report template for user information [Start]
IF laScrMode[1] .OR. laScrMode[2]
  SHOW GET m.cEmail_Add DISABLE
  SHOW GET m.cTmp_Code  DISABLE
ELSE
  SHOW GET m.cEmail_Add ENABLE
  SHOW GET m.cTmp_Code  ENABLE
ENDIF
*E302251,1 AMH [End]

*C127341,10 TMI [Start] check temp file for admn privilage for GPS00
IF ASCAN(laEvntTrig,'CHKUSR')<>0
  =gfDoTriger('SMUSERS','CHKUSR')
ENDIF
*C127341,10 TMI [End  ] 

SELECT SYUUSER

*!**************************************************************************
*!
*!      Function: lfvData_1
*!
*!**************************************************************************
*
FUNCTION lfvData_1

EXTERNAL ARRAY laData 

IF llBrowse .OR. (!EMPTY(laData[1]) .AND. LASTKEY() = 13) 
  IF ALLTRIM(laData[1]) = "?" .OR. llBrowse 
    =gfSeekRec()
  ELSE
    llChar = .T.
    *B607468,1 ALB Do not allow to add user ID named "ARIA" [BEGIN]
    *B802375,1 Allow Alphanumeric entry for the user ID
    *llChar = lfAlpha(ALLTRIM(laData[1])) 
    *IF !llChar AND (ALLTRIM(laData[1]) = "ARIA")
    IF (ALLTRIM(laData[1]) = "ARIA")
      *** User ID should not have any numbers. ***
      *** <  Ok  > ***
      *=gfModalGen("TRM00232B00000","DIALOG")
      =gfModalGen("TRM00000B00000","DIALOG",.F.,.f.,"Can't add a user named 'ARIA' , please select another user name.")
    *B607468,1 ALB Do not allow to add user ID named "ARIA" [END]
      laData[1] = SPACE(10)
      _CUROBJ   = OBJNUM(laData[1])
      RETURN
    ELSE
      =gfSeeKRec()
    ENDIF
  ENDIF
ENDIF
llBrowse = .F.

*C200249,7 AMH Add prompte days field for MBI [Start]
IF ASCAN(laEvntTrig , PADR('TODOMBI',10)) <> 0
  SCATTER FIELDS NPROMDAYS MEMVAR
ENDIF
*C200249,7 AMH [End]

*E302251,1 AMH Add Email and report template for user information [Start]
SCATTER FIELDS CEMAIL_ADD,SYUUSER.CTMP_CODE MEMVAR
*E302251,1 AMH [End]

*!**************************************************************************
*!
*!      Function: lfwData_3
*!
*!**************************************************************************
*
FUNCTION lfwData_3

SET CONFIRM Off

DO CASE 
  CASE _DOS
    lnYpos    = 3
    lnXpos    = 14
    lnDypos   = 3
    lnDxPos   = 14   
    @ lnDypos,lnDxPos SAY SPACE(8) COLOR &gcReadClr
  CASE _WINDOWS
    lnYpos    = 5.177
    lnXpos    = 12.270
    lnDypos   = 5.177
    lnDxPos   = 12.270
    @ lnDypos,lnDxPos+0.300 SAY REPLICATE(CHR(219),8) COLOR N/N;
         FONT "FOXFONT",9 STYLE "N"
ENDCASE 

lcPass    =' '
lcPassWrd =''

DO WHILE LEN(lcPassWrd) < 8 
  DO CASE 
    CASE _DOS
      @ lnYpos,lnXpos GET lcPass PICTURE "@!A" COLOR ,rg+/N
      READ COLOR ,rg+/N
    CASE _WINDOWS
    *B603248,1 MAN (Start)
    *B603248,1 Make User Password take digits
*      @ lnYpos,lnXpos GET lcPass  SIZE 1.000,1 PICTURE "@!A" COLOR ,N/N
      @ lnYpos,lnXpos GET lcPass  SIZE 1.000,1 PICTURE "@!" COLOR ,N/N;
      FONT "FOXFONT",9 STYLE "N"
      READ COLOR ,N/N
    *B603248,1 MAN (End)      
  ENDCASE       
  
  *B603248,1 MAN (Start)
  *B603248,1 Make User Password take digits
*  IF INLIST(LASTKEY(),13,9,15)  OR ASC(lcPass) < 65 OR  ASC(lcPass) > 90 
  IF INLIST(LASTKEY(),13,9,15)  OR ASC(lcPass) < 48 OR  ASC(lcPass) > 90   
  *B603248,1 MAN (End)
    EXIT
  ENDIF  
  lcPassWrd = lcPassWrd+lcPass
  lcPass = ' '

  DO CASE
    CASE _DOS
      lnXpos = lnXpos + 1
      @ lnDypos ,lnDxPos SAY REPLICAT('!',LEN(lcPassWrd)) COLOR &gcReadClr
    CASE _WINDOWS  
      lnXpos = lnXpos + 1.34
      @ lnDypos ,lnDxPos SAY REPLICAT('!',LEN(lcPassWrd)) ;
         FONT "FOXFONT",9 STYLE "N" COLOR &gcReadClr
  ENDCASE       
ENDDO    

IF INLIST(LASTKEY(),13,9,15) 
  IF !EMPTY(lcPassWrd) 
    laData[3] = SYS(2007,ALLTRIM(lcPassWrd))
    lcPassWrd = REPLICAT("*",8)
 ELSE
*B600116,1 MAN Removing the password deletion in case of Tab, Enter 
*  laData[3] =''
*  lcPassWrd = SPACE(8)
    IF !EMPTY(laData[3])
      lcPassWrd = REPLICAT("*",8)
    ENDIF
*END MAN    
  ENDIF  
ELSE
  IF EMPTY(laData[3])
    lcPassWrd = SPACE(8)
  ELSE
    lcPassWrd = REPLICAT("*",8)
  ENDIF
ENDIF

SET CONFIRM ON
lnLastKey=LASTKEY()
KEYBOARD CHR(13)


*!**************************************************************************
*!
*!      Function: lfvData_3
*!
*!**************************************************************************
*
FUNCTION lfvData_3

IF lnLastKey = 5 .OR. lnLastKey = 18  .OR. lnLastKey = 15
  _CUROBJ = _CUROBJ -1
ENDIF  

*!**************************************************************************
*!
*!      Function: lfvData_10
*!
*!**************************************************************************
*
FUNCTION lfvData_10

IF !EMPTY(laData[10]) .AND. LASTKEY() = 13
  IF !BETWEEN(VAL(laData[10]),0,128)
    *** Typeahead range should be between 0 and 128. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00233B00000","DIALOG")
    laData[10] = lcOTypHead
    _CUROBJ    = OBJNUM(laData[10])
  ENDIF
ELSE
  laData[10] = lcOTypHead
ENDIF
SHOW GET laData[10]

*!**************************************************************************
*!
*!      Function: lfvHours
*!
*!**************************************************************************
*
FUNCTION lfvHours

EXTERNAL ARRAY laData
laData[8] =STR(rbHours-1,1)

*!**************************************************************************
*!
*!      Function: lfvAudit
*!
*!**************************************************************************
*
FUNCTION lfvAudit

EXTERNAL ARRAY laData
laData[9]=SUBSTR("NAPB",rbAudit,1)

*!**************************************************************************
*!
*!      Function: lfvUsrLvl
*!
*!**************************************************************************
*
FUNCTION lfvUsrLvl

EXTERNAL ARRAY laData
laData[7]=SUBSTR("AO",rbUsrLvl,1)
IF laData[7] = "A"
  SHOW GET pbUsrPrv DISABLE
*B603002,1 Hesham (Start)
  laData[28] = laGroups[1,2]
  puUGroup = 1
  SHOW GET puUGroup DISABLE
*B603002,1 Hesham (End)  
ELSE
  SHOW GET pbUsrPrv ENABLE
 *B603002,1 Hesham (Start)    
  SHOW GET puUGroup ENABLE
 *B603002,1 Hesham (End)  
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_15
*!
*!**************************************************************************
*
FUNCTION lfvData_15

EXTERNAL ARRAY laData
IF laData[15] > laData[16] .AND. (!EMPTY(laData[15]) .AND. !EMPTY(laData[16]))
  =gfModalGen('TRM00107B00000','DIALOG')
  _CUROBJ = OBJNUM(laData[15])
ENDIF

*!**************************************************************************
*!
*!      Function: lfvData_16
*!
*!**************************************************************************
*
FUNCTION lfvData_16

EXTERNAL ARRAY laData
IF laData[16] < laData[15] .AND. (!EMPTY(laData[15]) .AND. !EMPTY(laData[16]))
  =gfModalGen('TRM00107B00000','DIALOG')
  _CUROBJ = OBJNUM(laData[16])
ENDIF
 
*!**************************************************************************
*!
*!      Function: lfvTime
*!
*!**************************************************************************
*
FUNCTION lfvTime
PARAMETER lnIndex    && index of the element in the array

EXTERNAL ARRAY laData
** The following condition checks if the entered time is valid, i.e;
** the hours < 24, minutes < 60, and seconds < 60

IF VAL(SUBSTR(laData[lnIndex],1,2)) <= 24;
   AND VAL(SUBSTR(laData[lnIndex],4,2)) <= 60;
   AND VAL(SUBSTR(laData[lnIndex],7,2)) <= 60

  ** The following condition checks if the entered time is more than;
  ** 24 hours by minutes or seconds
   
  IF VAL(SUBSTR(laData[lnIndex],1,2)) = 24 AND;
     (VAL(SUBSTR(laData[lnIndex],4,2)) > 0 OR;
      VAL(SUBSTR(laData[lnIndex],6,2)) > 0) 
  
    RETURN .F.
  ELSE
    RETURN .T.
  ENDIF    
ELSE
  RETURN .F.
ENDIF         

*!********************************************************************
*!
*!              Procedure: lpDelScr
*!
*!********************************************************************
*
PROCEDURE lpDelScr
EXTERNAL ARRAY laScrMode

lcSavAlias=SELECT(0)
 
*** Delete all priv. for this user
SELECT (lcPrv_Tmp1)
*B603002,1 Hesham (Start)
*REPLACE ALL cStatus WITH 'D' FOR cUser_Id = laData[1]
REPLACE ALL cStatus WITH 'D' FOR cUser_Id = laData[1] AND CGRPORUSER = "U"
*B603002,1 Hesham (End)
DELETE ALL FOR cStatus = 'D'
=gfTmp2Mast("SYUUSRPR",lcPrv_Tmp1,'Deleting user privilege information...')  

*** Delete the user record in the chek message file
SELECT syuCkMsg
IF SEEK (laData[1])
  DELETE
ENDIF  

*** Delete the user record from the user file
SELECT(lcSavAlias)
SCATTER FIELDS &lcScFields MEMO TO laData BLANK

*C200249,7 AMH Add prompte days field for MBI [Start]
IF ASCAN(laEvntTrig , PADR('TODOMBI',10)) <> 0
  SCATTER FIELDS NPROMDAYS MEMVAR BLANK
ENDIF
*C200249,7 AMH [End]

*E302251,1 AMH Add Email and report template for user information [Start]
SCATTER FIELDS CEMAIL_ADD,SYUUSER.CTMP_CODE MEMVAR BLANK
*E302251,1 AMH [End]

GATHER FROM laData FIELDS &lcScFields MEMO 

*C200249,7 AMH Add prompte days field for MBI [Start]
IF ASCAN(laEvntTrig , PADR('TODOMBI',10)) <> 0
  GATHER FIELDS NPROMDAYS MEMVAR
ENDIF
*C200249,7 AMH [End]

*E302251,1 AMH Add Email and report template for user information [Start]
GATHER FIELDS CEMAIL_ADD,CTMP_CODE MEMVAR
*E302251,1 AMH [End]

DELETE

laScrMode    = .F.
laScrMode[1] = .T.

*!********************************************************************
*!
*!              Procedure: lpSavScr
*!
*!********************************************************************
*
PROCEDURE lpSavScr

EXTERNAL ARRAY laScrMode,laData
IF EMPTY(laData[2])
  *** You cannot save this user without name. ***
  *** <   Ok   > ***
  = gfModalGen("TRM00154B00000","DIALOG","user")
  llCSave = .F.    && Flag to say that no save happen.
  _CUROBJ = OBJNUM(laData[2])
  RETURN
ENDIF

lcSavAlias =SELECT(0)
IF laData[7] <> "A" 
  *E301077,78 Hesham  (Start)
  =gfOpenFile(gcSysHome+'SYCINST')
  *E301077,78 Hesham  (End)
  IF !EMPTY(laData[12])
    SELECT (lcPrv_Tmp1)
    LOCATE FOR INLIST(cUser_ID,laData[1],laData[28]) .AND. ;
               cComp_ID = laData[12]
    
    IF !FOUND()
      *** This user has no rights in the default company. ***
      *** < Clear default > - < Modify >
      lnOption = gfModalGen("QRM00159B00027","DIALOG",;
                 laData[12]+"-"+ALLTRIM(lcComNam))
      IF lnOption = 1
        laData[12] = ""
      ELSE
        llCSave = .F.    && Flag to say that no save happen.
        _CUROBJ = OBJNUM(laData[12])
        SELECT SYUUSER
        RETURN
      ENDIF
    ENDIF
    
    IF !EMPTY(laData[24])
      LOCATE FOR INLIST(cUser_ID,laData[1],laData[28]) .AND. ;
                 cApp_ID = laData[24]
      
      IF !FOUND()
        *** This user has no rights in this default module. ***
        *** < Clear default > - < Modify >
        lnOption = gfModalGen("QRM00162B00027","DIALOG",;
                   laData[24]+"-"+ALLTRIM(lcModNam))
        IF lnOption = 1
          laData[24] = "  "
        ELSE
          llCSave = .F.    && Flag to say that no save happen.
          _CUROBJ = OBJNUM(laData[24])
          SELECT SYUUSER
          RETURN
        ENDIF
      ENDIF
    ENDIF  
  
  
  ELSE
  
    SELECT (lcPrv_Tmp1)
    LOCATE FOR INLIST(cUser_ID,laData[1],laData[28]) .AND. ;
               cComp_ID = sycInst.cInsdfcom
    
    IF !FOUND()
      *** This user has no default company, So he will login  with ***
      *** the system default company that he has no rights in it.  ***
      *** You have to enter a default company for this user or add ***
      *** privileges in the system default company...
      *** <  Ok  >  ***
      
      *B603002,1 HESHAM (Start)
      *=gfModalGen("QRM00235B00000","DIALOG",sycinst.cinsdfcom+"-"+;
                  ALLTRIM(LOOKUP(sycComp.cCom_name,;
                  ALLTRIM(sycinst.cinsdfcom),;
                  sycComp.cComp_id,'cComp_id')))

      lnOption =gfModalGen("QRM00235B00006","DIALOG",sycinst.cinsdfcom+"-"+;
                  ALLTRIM(LOOKUP(sycComp.cCom_name,;
                  ALLTRIM(sycinst.cinsdfcom),;
                  sycComp.cComp_id,'cComp_id')))

      *llCSave = .F.    && Flag to say that no save happen.
      *SELECT SYUUSER
      *RETURN
      IF lnOption = 2
        llCSave = .F.    && Flag to say that no save happen.
        SELECT SYUUSER
        RETURN
      ENDIF
      *B603002,1 HESHAM (End)      
    ENDIF
  ENDIF
  SELECT SYUUSER
ENDIF

*C123615,1  TMI [Start] Update the array element related to the field "cEmail" in the file syuuser for niknak

*C124836,1 MHM 11/25/2004 update mail for User[Start]
IF ASCAN(laEvntTrig,'UPDEMAIL') <> 0
  =gfDoTriger('SMUSERS','UPDEMAIL')
ENDIF
*C124836,1 MHM 11/25/2004 [End]
*C123615,1  TMI [End  ] 

IF laScrMode[4]
  APPEND BLANK
ENDIF
GATHER FROM laData FIELDS &lcScFields MEMO 

*C200249,7 AMH Add prompte days field for MBI [Start]
IF ASCAN(laEvntTrig , PADR('TODOMBI',10)) <> 0
  GATHER FIELDS NPROMDAYS MEMVAR
ENDIF
*C200249,7 AMH [End]
*C127341,11 TMI [Start] Update privilages for GPS00 
IF ASCAN(laEvntTrig,'SAVAPRV')<>0
  =gfDoTriger('SMUSERS','SAVAPRV')
ENDIF
 
*C127341,11 TMI [End  ] 

*E302251,1 AMH Add Email and report template for user information [Start]
GATHER FIELDS CEMAIL_ADD,CTMP_CODE MEMVAR
*E302251,1 AMH [End]

=gfAdd_Info()

*** If modefying for the current user update the user;
*** default module and company

IF laData[1] = gcUser_ID
  gcUser_Com = laData[12]
  gcUser_Mdl = laData[24]
ENDIF

IF laData[7] = "A"
  SELECT (lcPrv_Tmp1)
  *B603002,1 Hesham (Start)
  *REPLACE ALL cStatus WITH "D"
   REPLACE ALL cStatus WITH 'D' FOR cUser_Id = laData[1] AND CGRPORUSER = "U"
   *DELETE ALL   
   DELETE ALL FOR cUser_Id = laData[1] AND CGRPORUSER = "U"   
  *B603002,1 Hesham (End)  
  SELECT SYUUSRPR
  =gfTmp2Mast("SYUUSRPR",lcPrv_Tmp1,'Saving user privileges...')  
ELSE
  IF llUpdPriv  
    llUpdPriv  = .F.
  
    SELECT (lcPrv_Tmp1)
    IF laData[7] = "A"
     *B603002,1 Hesham (Start)
     *REPLACE ALL cStatus WITH "D" FOR cUser_Id = laData[1]
      REPLACE ALL cStatus WITH 'D' FOR cUser_Id = laData[1] AND CGRPORUSER = "U"
      *DELETE ALL   
      DELETE ALL FOR cUser_Id = laData[1] AND CGRPORUSER = "U"   
     *B603002,1 Hesham (End)  
    ELSE
     *B603002,1 Hesham (Start)
     *DELETE ALL FOR cStatus = "D" .OR. cUser_Id <> laData[1]
      DELETE ALL FOR (cStatus = "D" AND cUser_Id = laData[1] AND CGRPORUSER = "U") .OR. CGRPORUSER = "G"
     *B603002,1 Hesham (Start)
    ENDIF

    SELECT SYUUSRPR
    =gfTmp2Mast("SYUUSRPR",lcPrv_Tmp1,'Saving user privileges...')  
  ENDIF  
ENDIF

SELECT SYUUSER

IF laScrMode[4]
  *** Add New Record for this user in the chek messag file
  INSERT INTO &gcSysHome.syuCkMsg (cUser_Id) VALUES (laData[1])
  lcUsr_Resr  = gfTempName()
  
  *E300613,1 YMA 06/25/97 Create the resource file using CREATE TABLE 
  *E300613,1 YMA 06/25/97 instead of the SQL, because the length of 
  *E300613,1 YMA 06/25/97 the "Type" field has been changed in the 
  *E300613,1 YMA 06/25/97 dictionary, so we cannot use the "SYCRESRC"
  *E300613,1 YMA 06/25/97 file anymore.
  CREATE TABLE (gcWorkDir+lcUsr_Resr) (Type C(12), ID C(12), Name C(24), ReadOnly L, CkVal N(6), Data M, Updated D)
  *SELECT * FROM (gcSysHome+"SYCRESRC") INTO DBF (gcRsrcHome+lcUsr_Resr) 
  *E300613,1 YMA 06/25/97 End.
    
  REPLACE SYUUSER.cUsr_Resr WITH lcUsr_Resr
  IF USED(lcUsr_Resr)
    SELECT (lcUsr_Resr)  
    USE
  ENDIF
ENDIF

SELECT (lcSavAlias)
=lfRefresh()

*!**************************************************************************
*!
*!      Function lfwCompId
*!
*!**************************************************************************
*
Function lfwCompId

lcOld_ID = laData[12]

*!**************************************************************************
*!
*!      Function lfvCompId
*!
*!**************************************************************************
*
Function lfvCompId

IF llBrowse
  laData[12] = "?"
ENDIF

IF laData[12] = lcOld_ID
  RETURN
ENDIF
  
IF EMPTY(laData[12]) .AND. LASTKEY() = 13
  lcComNam = ""
  =lfRefresh()
  RETURN
ENDIF
IF LASTKEY() = 13 .OR. llBrowse

  IF SEEK(laData[12],"SYCCOMP")
    IF laData[7] <> "A" 
      SELECT (lcPrv_Tmp1)
      LOCATE FOR INLIST(cUser_ID,laData[1],laData[28]) .AND. ;
                 cComp_ID = laData[12] .AND. cStatus <> "D"
      IF !FOUND()
        *** This user has no rights in the default company. ***
        *** <  Ok  > ***
        = gfModalGen("TRM00159B00000","DIALOG",;
                     laData[12]+"-"+ALLTRIM(sycComp.cCom_name))
        laData[12] = lcOld_ID
      ELSE
        lcComNam  = sycComp.cCom_name
        lcCompMdl = ALLTRIM(syccomp.mcomp_mdl)
      ENDIF
    ELSE
      lcComNam  = sycComp.cCom_name    
      lcCompMdl = ALLTRIM(syccomp.mcomp_mdl)
    ENDIF  
  ELSE
    SELECT sycComp

    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF

    lcSaveBrow    = lcBrFields
    lcBrFields    = "cComp_id :H='Company ID',cCom_name :H='Company Name'"
    lcOld_ttl     = lcFile_ttl
    lcFile_ttl    = "Company information"
    laCompInfo[1] = laData[12]
    laCompInfo[2] = lcComNam
    laCompInfo[3] = lcCompMdl

    =gfBrows(.F.,"cComp_id,cCom_name,mcomp_mdl","laCompInfo")

    lcBrFields    = lcSaveBrow 
    lcFile_ttl    = lcOld_ttl

    IF laData[12] = laCompInfo[1] 
      laData[12]  = lcOld_ID
    ELSE
      laData[12] = laCompInfo[1]

      IF laData[7] <> "A"  

        SELECT (lcPrv_Tmp1)
        LOCATE FOR INLIST(cUser_ID,laData[1],laData[28]) .AND. ;
                   cComp_ID = laData[12] .AND. cStatus <> "D"
        IF !FOUND()
          *** This user has no rights in the default company. ***
          *** <  Ok  > ***
          = gfModalGen("TRM00159B00000","DIALOG",;
                       laData[12]+"-"+ALLTRIM(laCompInfo[2]))
          laData[12] = lcOld_ID
        ELSE
          laData[12] = laCompInfo[1]
          lcComNam   = laCompInfo[2]
          lcCompMdl  = ALLTRIM(laCompInfo[3])
        ENDIF
      ELSE
        laData[12] = laCompInfo[1]
        lcComNam   = laCompInfo[2]
        lcCompMdl  = ALLTRIM(laCompInfo[3])
      ENDIF  
    ENDIF
  ENDIF
  IF !EMPTY(laData[24])
    IF !(laData[24] $ lcCompMdl) .AND. laData[24]<> "SM"
      = gfModalGen("INM00151B00000","DIALOG",;
        laData[24]+"-"+lcModNam+"|"+lcComNam)

      laData[24] = "  "
      lcModNam   = ""
      SHOW GET laData[24]
    ENDIF
  ENDIF  

ELSE
  laData[12] = lcOld_ID
ENDIF

SELECT SYUUSER
SHOW GET laData[12]
=lfRefresh()
llBrowse = .F.

*!**************************************************************************
*!
*!      Function lfwMod_Id
*!
*!**************************************************************************
*
Function lfwMod_Id

lcOld_Mod = laData[24]

*!**************************************************************************
*!
*!      Function lfvMod_Id
*!
*!**************************************************************************
*
Function lfvMod_Id

IF llBrowse
  laData[24] = "?"
ENDIF

IF laData[24] = lcOld_Mod
  RETURN
ENDIF  
  
IF EMPTY(laData[24]) .AND. LASTKEY() = 13
  lcModNam = ""
  =lfRefresh()
  RETURN
ENDIF

IF LASTKEY() = 13 .OR. llBrowse
  IF laData[24] = "SY"
    *** You cannot choose the main system as a default module. ***
    *** <  Ok  > ***
    =gfModalGen("TRM00163B00000","DIALOG")
    laData[24] = lcOld_Mod
    SHOW GET laData[24]
    =lfRefresh()
    RETURN
  ENDIF

  IF SEEK(laData[24],"SYDAPPL")
    IF !EMPTY(laData[12]) 
      IF !(laData[24] $ lcCompMdl) .AND. laData[24]<> "SM"
        = gfModalGen("INM00151B00000","DIALOG",;
          laData[24]+"-"+ALLTRIM(sydAppl.cApp_name)+"|"+lcComNam)

        laData[24] = lcOld_Mod
        SHOW GET laData[24]
        =lfRefresh()
        RETURN
      ENDIF
    ENDIF  

    IF laData[7] <> "A" 
      SELECT (lcPrv_Tmp1)
      LOCATE FOR INLIST(cUser_ID,laData[1],laData[28]) .AND. ;
                 cApp_ID = laData[24] .AND. cStatus <> "D"
      IF !FOUND()
        *** This user has no rights in this default module. ***
        *** <  Ok  > ***
        = gfModalGen("TRM00162B00000","DIALOG",;
                     laData[24]+"-"+ALLTRIM(sydAppl.cApp_name))
        laData[24] = lcOld_Mod
      ELSE
        lcModNam = sydAppl.cApp_name
      ENDIF
    ELSE
      lcModNam = sydAppl.cApp_name
    ENDIF  
  ELSE
    SELECT sydAppl

    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF

    lcSaveBrow    = lcBrFields
    lcBrFields    = "cApp_id :H='Module ID',cApp_name :H='Module Name'"
    lcOld_ttl     = lcFile_ttl
    lcFile_ttl    = "Modules Dictionary"
    laModInfo[1]  = laData[24]
    laModInfo[2]  = lcModNam

    =gfBrows(.F.,"cApp_id,cApp_name","laModInfo")

    lcBrFields    = lcSaveBrow 
    lcFile_ttl    = lcOld_ttl

    IF laData[24] = laModInfo[1] 
      laData[24]  = lcOld_Mod
    ELSE
      laData[24]  = laModInfo[1]
      IF laData[24] = "SY"
        *** You cannot choose the main system as a default module. ***
        *** <  Ok  > ***
        =gfModalGen("TRM00163B00000","DIALOG")
        laData[24] = lcOld_Mod
        SHOW GET laData[24]
        =lfRefresh()
        RETURN
      ENDIF

      IF !EMPTY(laData[12]) 
        IF !(laData[24] $ lcCompMdl) .AND. laData[24]<> "SM"
          = gfModalGen("INM00151B00000","DIALOG",;
            laData[24]+"-"+ALLTRIM(sydAppl.cApp_name)+"|"+lcComNam)

          laData[24] = lcOld_Mod
          SHOW GET laData[24]
          =lfRefresh()
          RETURN
        ENDIF
      ENDIF  


      IF laData[7] <> "A"
        SELECT (lcPrv_Tmp1)
        LOCATE FOR INLIST(cUser_ID,laData[1],laData[28]) .AND. ;
                   cApp_ID = laData[24] .AND. cStatus <> "D"
        IF !FOUND()
          *** This user has no rights in this default module. ***
          *** <  Ok  > ***
          = gfModalGen("TRM00162B00000","DIALOG",;
                       laData[24]+"-"+ALLTRIM(laModInfo[2]))
          laData[24] = lcOld_Mod
        ELSE
          lcModNam   = laModInfo[2]
        ENDIF
      ELSE
        lcModNam   = laModInfo[2]      
      ENDIF  
    ENDIF
  ENDIF
ELSE
  laData[24] = lcOld_Mod
ENDIF

SELECT SYUUSER
SHOW GET laData[24]
=lfRefresh()
llBrowse = .F.

*!**************************************************************************
*!
*!      Function lfwPrnt_Id
*!
*!**************************************************************************
*
Function lfwPrnt_Id

lcOld_Pr = laData[13]

*!**************************************************************************
*!
*!      Function lfvPrnt_Id
*!
*!**************************************************************************
*
Function lfvPrnt_Id

IF llBrowse
  laData[13] = "?"
ENDIF

IF laData[13] = lcOld_Pr 
  RETURN
ENDIF

IF EMPTY(laData[13]) .AND. LASTKEY() = 13
  lcPrintNam = ""
  =lfRefresh()
  RETURN
ENDIF

IF LASTKEY() = 13 .OR. llBrowse
  IF SEEK(ALLTRIM(laData[13]),"SYCPRINT")
    lcPrintNam = sycPrint.cPrint_nam
  ELSE
    SELECT sycPrint

    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF

    lcSaveBrow  = lcBrFields
    lcBrFields  = "cPrint_id :H='Printer ID',cPrint_nam :H='Printer Name'"
    lcOld_ttl   = lcFile_ttl
    lcFile_ttl  = "Printer information"
    laPrntInfo[1] = laData[13]
    laPrntInfo[2] = lcPrintNam

    =gfBrows(.F.,"cPrint_id,cPrint_nam","laPrntInfo")

    lcBrFields = lcSaveBrow 
    lcFile_ttl = lcOld_ttl 
    IF laData[13] = laPrntInfo[1] 
      laData[13] = lcOld_Pr
    ELSE
      laData[13] = laPrntInfo[1]
      lcPrintNam = laPrntInfo[2]
    ENDIF
  ENDIF  
ELSE
  laData[13] = lcOld_Pr
ENDIF

SELECT SYUUSER
SHOW GET laData[13]
=lfRefresh()
llBrowse = .F.

*!**************************************************************************
*!
*!      Function lfwWrkName
*!
*!**************************************************************************
*
FUNCTION lfwWrkName

lcOld_wrk = laData[14]

*!**************************************************************************
*!
*!      Function lfvWrkName
*!
*!**************************************************************************
*
FUNCTION lfvWrkName

IF llBrowse
  laData[14] = "?"
ENDIF

IF laData[14] = lcOld_wrk 
  RETURN
ENDIF

IF EMPTY(laData[14]) .AND. LASTKEY() = 13
  lcwrk_name = " "
  =lfRefresh()
  RETURN
ENDIF

IF LASTKEY() = 13 .OR. llBrowse
  IF SEEK(ALLTRIM(laData[14]),"SYCWRKST")
    lcwrk_name = sycWrkst.cWrk_name
  ELSE
    SELECT sycWrkst

    lcSaveBrow  = lcBrFields
    lcBrFields  = "cWrkst_ID :H='Workstation ID',cWrk_name :H='Workstation Name'"
    lcOLd_ttl   = lcFile_ttl 
    lcFile_ttl  = "Workstation information"
    laWrkInfo[1] = laData[14]
    laWrkInfo[2] = lcWrk_Name

    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF

    =gfBrows(.F.,"cWrkst_ID,cWrk_name","laWrkInfo")

    lcBrFields = lcSaveBrow 
    lcFile_ttl = lcOld_ttl
    IF laData[14] = laWrkInfo[1] 
      laData[14] = lcOld_wrk
    ELSE
      laData[14] = laWrkInfo[1]
      lcwrk_name = laWrkInfo[2]
    ENDIF
  ENDIF  
ELSE
  laData[14] = lcOld_wrk
ENDIF

SELECT SYUUSER

SHOW GET laData[14]
=lfRefresh()
llBrowse = .F.

*!**************************************************************************
*!
*!      Function lfvOk
*!
*!**************************************************************************
*
FUNCTION lfvOk

*!********************************************************************
*!
*!              Function: lfvUsrPrv
*!
*!********************************************************************
*
FUNCTION lfvUsrPrv

SELECT SYUUSER
lnRecUser = RECNO("SYUUSER")

lcUsr_grup = IIF(laScrMode[4]," ",SYUUSER.cUsr_grup)

IF lcLastData <> laData[1]
  IF !EMPTY(lcUsr_grup)
    SELECT * , RECNO() AS 'nRecNo' , "S" AS 'cStatus' , .T. AS 'lExist' ;
           FROM (gcSysHome+"SYUUSRPR") ;
           INTO DBF (gcWorkDir+lcPrv_Tmp1) ;
           WHERE ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(laData[1]) +"U" .OR. ;
                 ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(lcUsr_grup)+"G"
  ELSE
    SELECT * , RECNO() AS 'nRecNo' , "S" AS 'cStatus' , .T. AS 'lExist' ;
           FROM (gcSysHome+"SYUUSRPR") ;
           INTO DBF (gcWorkDir+lcPrv_Tmp1) ;
           WHERE ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(laData[1]) + "U"
  ENDIF
  
  SELECT (lcPrv_Tmp2)
  ZAP
  APPEND FROM (gcWorkDir+lcPrv_Tmp1)
  
  llUpdPriv  = .F.
  
  *E300683,4 Call *.PRG from screens directory
  *=SMprvl('User',laData[1],laData[2])
  DO (gcAppHome + gcWinAppl + '\SMprvl') WITH 'User',laData[1],laData[2]
  *E300683,4 end   
  lcLastData = laData[1]
ELSE
  *E300683,4 Call *.PRG from screens directory
  *=SMprvl('User',laData[1],laData[2])   
  DO (gcAppHome + gcWinAppl + '\SMprvl') WITH 'User',laData[1],laData[2]
  *E300683,4 end   
ENDIF

SELECT SYUUSER
IF lnRecUser > 0 .AND. lnRecUser <=RECCOUNT("SYUUSER")
  GO lnRecUser
ENDIF

*!**************************************************************************
*!
*!      Function lfvDefBmp
*!
*!**************************************************************************
*
FUNCTION lfvDefBmp

lcOldFil   = laData[27]

laData[27] = gfChngWall(.T.)

*E300275,1 Reham On 08/23/95   *** Begin ***
*E300275,1 We Have to have the ability to blank the default bitmap.
*laData[27] = IIF(EMPTY(laData[27]),lcOldFil,laData[27])
*E300275,1 Reham On 08/23/95   *** End   ***

SHOW GET laData[27]

*!*************************************************************
*! Name      : lfvExtPrg
*! Developer : Reham Alallamy
*! Date      : 08/14/1995
*! Purpose   : *E300268,1 Validate the external program field.
*!*************************************************************
*! Calls     : 
*!             Procedures : 
*!             Functions  : 
*!*************************************************************
*! Passed Parameters  :  None
*!*************************************************************
*! Returns            :  
*!*************************************************************
*! Example            :  
*!*************************************************************
*
FUNCTION lfvExtPrg

*E300268,1 Reham On 08/14/95   *** Begin ***
*E300268,1 Validate the external program field that added in the
*E300268,1 user default screen.

lcOldExtP = laData[29]

DO CASE
  CASE _DOS     .OR. _UNIX
    laData[29] = gfActPop(13,23,17,37,'laExtPrg',1,1,@lcExtPrg)
    laData[29] = IIF(laData[29]=lcNone," ",laData[29])
    =lfRefresh()
  CASE _WINDOWS .OR. _MAC
    laData[29] = IIF(puExt = 1 , " " , laExtPrg[puExt,1])
    lcExtPrg   = laExtPrg[puExt,1]
    SHOW GET puExt
ENDCASE

IF lcOldExtP <> laData[29]
  =gfUpdate()
ENDIF

*E300268,1 Reham On 08/14/95   *** End   ***

*B603002,1 Hesham (Start)
FUNCTION lfvUsrGroup
IF laData[28] <> laGroups[puUGroup,2]
   laData[28] = laGroups[puUGroup,2]
  lcUsr_grup = laData[28]
  IF !EMPTY(lcUsr_grup)
     SELECT * , RECNO() AS 'nRecNo' , "S" AS 'cStatus' , .T. AS 'lExist' ;
            FROM (gcSysHome+"SYUUSRPR") ;
             INTO DBF (gcWorkDir+lcPrv_Tmp1) ;
             WHERE ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(laData[1]) +"U" .OR. ;
                   ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(lcUsr_grup)+"G"
  ELSE
      SELECT * , RECNO() AS 'nRecNo' , "S" AS 'cStatus' , .T. AS 'lExist' ;
             FROM (gcSysHome+"SYUUSRPR") ;
             INTO DBF (gcWorkDir+lcPrv_Tmp1) ;
             WHERE ALLTRIM(CUSER_ID)+CGRPORUSER = ALLTRIM(laData[1])+"U"
  ENDIF
ENDIF
*B603002,1 Hesham (End)

*!**************************************************************************
*!
*!      Function lfwTmpCode
*!
*!**************************************************************************
*E302251,1 AMH
Function lfwTmpCode

lcOld_Tmp = m.cTmp_Code

*!**************************************************************************
*!
*!      Function: lfvTmpCode
*!
*!**************************************************************************
*E302251,1 AMH
FUNCTION lfvTmpCode

IF llBrowse
  m.cTmp_Code = "?"
ENDIF

IF m.cTmp_Code = lcOld_Tmp
  RETURN
ENDIF
  
IF EMPTY(m.cTmp_Code) .AND. LASTKEY() = 13
  =lfRefresh()
  RETURN
ENDIF

IF !USED("PMRPRTM")
  =gfOpenFile(gcDatadir+'PMRPRTM','PMRPRTM','SH')
ENDIF

IF LASTKEY() = 13 .OR. llBrowse
  IF !SEEK(m.cTmp_Code,"PMRPRTM")
    SELECT PMRPRTM
    
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
    
    DECLARE laTemp[1]
    lcSaveBrow = lcBrFields
    lcBrFields = "cTmp_Code :H='Code',cTmp_Dsc :H='Description'"
    lcOld_ttl  = lcFile_ttl
    lcFile_ttl = "Report Template"
    laTemp[1]  = m.cTmp_Code
    
    =gfBrows(.F.,"cTmp_Code","laTemp")
    
    lcBrFields    = lcSaveBrow
    lcFile_ttl    = lcOld_ttl
    
    IF m.cTmp_Code = laTemp[1]
      cTmp_Code   = lcOld_Tmp
    ELSE
      cTmp_Code   = laTemp[1]
    ENDIF
  ENDIF
ELSE
  cTmp_Code = lcOld_Tmp
ENDIF

SELECT SYUUSER
llBrowse = .F.