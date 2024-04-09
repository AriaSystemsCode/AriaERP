*:************************************************************************
*:
*: Procedure file: SMUPRVL.RPR
*:
*:         System: ARIA BUSINESS SYSTEM
*:         Module: System Manager
*:         Author: Reham Alallami
*:      Copyright (c)
*:  Last modified:01/09/95
*:
*:  Procs & Funcs: 
*:
*:************************************************************************

IF EMPTY(lcUser_ID)
  *** You have to select a user or group ID to print its privilege. ***
  =gfModalGen("TRM00236B00000","DIALOG",IIF(lcUsr_Grp="U","user","group"))
  RETURN
ENDIF

lnCount = 1
lnTotal = RECCOUNT("SYCMENU")+RECCOUNT("SYUUSRPR")

IF lcUsr_Grp = "U"
  SELECT SYUUSER
  IF SEEK(lcUser_Id)
    IF SYUUSER.cUsr_levl = "A"
      *** This user has adminstrator level. ***
      *** So he has rights to all the system...
      =gfModalGen("TRM00237B00000","DIALOG")
      RETURN
    ENDIF
    
    IF !EMPTY(SYUUSER.cUsr_grup)
      lcGroup  = SYUUSER.cUsr_grup
      lcOrGrp  = "(SYUUSRPR.CUSER_ID+SYUUSRPR.CGRPORUSER = PADR(lcUser_Id,10)+'U' ;
                  OR SYUUSRPR.CUSER_ID+SYUUSRPR.CGRPORUSER = PADR(lcGroup,10)+'G')"
    ELSE
      lcGroup = ""
      lcOrGrp = "SYUUSRPR.CUSER_ID+SYUUSRPR.CGRPORUSER = PADR(lcUser_Id,10)+'U'"
    ENDIF
  ELSE
    lcGroup = ""
    lcOrGrp = "SYUUSRPR.CUSER_ID+SYUUSRPR.CGRPORUSER = PADR(lcUser_Id,10)+'U'"
  ENDIF
ELSE
  lcGroup = ""
  lcOrGrp = "SYUUSRPR.CUSER_ID+SYUUSRPR.CGRPORUSER = PADR(lcUser_Id,10)+'G'"
ENDIF

SELECT DISTINCT ;
       lcUser_ID AS User , CCOMP_ID AS Company ,SYUUSRPR.CAPP_ID AS Module,;
       SPACE(VAL(CPOP_LEVL)*2)+STUFF(CSUB_PRPT,ATC('\',CSUB_PRPT),2,"") ;
       AS Processes, ;
       IIF(CSUB_TYP="C",IIF(LADDREC,"Yes"," No"),"N/A")  AS Add, ;
       IIF(CSUB_TYP="C",IIF(LEDITREC,"Yes"," No"),"N/A") AS Edit, ;
       IIF(CSUB_TYP="C",IIF(LEDITREC,"Yes"," No"),"N/A") AS Delete, ;
       CPAD_POS,SYCMENU.CPROSS_ID,SYCMENU.CMSTR_NAM ;
 FROM  (gcSysHome+"SYUUSRPR"),(gcSysHome+"SYCMENU") ;
 WHERE ;
       SYUUSRPR.CAPP_ID            = SYCMENU.CAPP_ID     .AND. ;
       &lcOrGrp .AND. ;
       (SYUUSRPR.CPROSS_ID         = SYCMENU.CPROSS_ID   .OR.  ;
        LEFT(SYCMENU.CPROSS_ID,3)  = LEFT(SYCMENU.CMSTR_NAM,3)) .AND. ;
       lfRpThermo(lnTotal,"lnCount") ;
 ORDER BY CCOMP_ID, SYUUSRPR.CAPP_ID, CPAD_POS, CPOP_POS ;
 INTO DBF (gcWorkDir+lcTmPrvRp1)

=lfTrapEsc(.F.)        
IF !llOGEscPrs  
  IF lnCount < lnTotal
    =lfClosThrm(lnTotal,lnCount,.F.,INT(ABS(lnTotal-_tally)))
  ENDIF
  IF _TALLY = 0
    *** There is no recoeds to display. ***
    =gfModalGen("INM00052B00000","DIALOG")
    RETURN
  ENDIF

  lnCount = 1
  lnTotal = RECCOUNT("SYCMENU")+RECCOUNT("SYUUSRPR")+RECCOUNT(lcTmPrvRp1)

  SELECT DISTINCT ;
         User , Company , Module , Processes , Add , Edit, ;
         Delete , &lcTmPrvRp1..CPAD_POS , &lcTmPrvRp1..CPROSS_ID, ;
         &lcTmPrvRp1..CMSTR_NAM , RECNO() AS SEQ ;
   FROM  (gcSysHome+"SYUUSRPR"),(gcSysHome+"SYCMENU"),(gcWorkDir+lcTmPrvRp1) ;
   WHERE ;
         SYUUSRPR.CAPP_ID            = SYCMENU.CAPP_ID        .AND. ;
         &lcOrGrp .AND. ;
         SYUUSRPR.CPROSS_ID          = SYCMENU.CPROSS_ID      .AND. ;
         (SYUUSRPR.CPROSS_ID         = &lcTmPrvRp1..CPROSS_ID .OR.  ;
          (LEFT(SYCMENU.CMSTR_NAM,LEN(ALLTRIM(&lcTmPrvRp1..CPROSS_ID))) ;
           == &lcTmPrvRp1..CPROSS_ID   .AND. ;
           SYUUSRPR.CCOMP_ID         = &lcTmPrvRp1..COMPANY)) .AND. ;
         lfRpThermo(lnTotal,"lnCount") ;
   ORDER BY SEQ ;
   INTO DBF (gcWorkDir+lcTmPrvRp2)

  =lfTrapEsc(.F.)        
  IF !llOGEscPrs  
    IF lnCount < lnTotal
      =lfClosThrm(lnTotal,lnCount,.F.,INT(ABS(lnTotal-_tally)))
    ENDIF
    IF _TALLY = 0
      *** There is no recoeds to display. ***
      =gfModalGen("INM00052B00000","DIALOG")
      RETURN
    ENDIF
    SELECT (lcTmPrvRp2)
    DO gfDispRe WITH EVAL('lcRpForm')
  ELSE
    IF lnCount < lnTotal
      lnCount = lnTotal - 1
      =lfClosThrm(lnTotal,lnCount)
    ENDIF
  ENDIF
ELSE
  IF lnCount < lnTotal
    lnCount = lnTotal - 1
    =lfClosThrm(lnTotal,lnCount)
  ENDIF
ENDIF

IF USED(lcTmPrvRp1)
  USE IN ALIAS(lcTmPrvRp1)
ENDIF
ERASE (gcWorkDir+lcTmPrvRp1+".DBF")

IF USED(lcTmPrvRp2)
  USE IN ALIAS(lcTmPrvRp2)
ENDIF
ERASE (gcWorkDir+lcTmPrvRp2+".DBF")

*!************************************************************************
*!
*!      FUNCTION lfClearRep
*!
*!************************************************************************
*
FUNCTION lfClearRep

IF USED(lcRpTargt)
  USE IN ALIAS(lcRpTargt)
ENDIF

IF USED(lcTmPrvRp1)
  USE IN ALIAS(lcTmPrvRp1)
ENDIF
ERASE (gcWorkDir+lcTmPrvRp1+".DBF")

IF USED(lcTmPrvRp2)
  USE IN ALIAS(lcTmPrvRp2)
ENDIF
ERASE (gcWorkDir+lcTmPrvRp2+".DBF")

*!************************************************************************
*!
*!      FUNCTION lfvUsr_Grp
*!
*!************************************************************************
*
FUNCTION lfvUsr_Grp

IF lcUsr_Grp = "U"
  =GFVLFLD('SYUUSER','CUser_ID',VARREAD(),'',.F.,.F., ;
          [CUser_ID:H="User ID",CUsr_name:H="User name"],'1',.F.,.T.)
ELSE
  =GFVLFLD('SYUGROUP','CGROUP_ID',VARREAD(),'',.F.,.F., ;
           [CGROUP_ID:H="Group ID",CGROUP_NAM:H="Group name"],'1',.F.,.T.)
ENDIF
