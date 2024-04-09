*!**************************************************************************
*! Program file        : BLOCKFIX.PRG
*! Program description : FIX PROGRAM
*! For screen          : NONE
*! For System          : Aria Advantage Series - Version 2.7
*! For Module          : 
*! Developer Name      : Walid Abou El-Magd (WAM) Due to E301316,1
*! Last modi           : 
*!**************************************************************************
*! Calls               : 
*!**************************************************************************
*! Passed Parameters   : None.
*!**************************************************************************
*! Example             : 
*!**************************************************************************
*! Modification        :
*!**************************************************************************
*! Note : This is a modified copy of maintain dictionary program
*!        SMMTDIC.PRG , But we use only the option <reorganize>
*!        all files in all companies to be sure that all files
*!        which have memo fields (i.e have a .FPT) file
*!        use a standerd block size 64 instead of the old one 33.
*!        By this way we will be sure that our utility to fix corrupted
*!        files either in the last version of maintain dictionary program
*!        Or in the last version of ARIA27 error handler will be able
*!        to fix any corrupted file using its standered block size.
*!**************************************************************************
*!E301316,1
*!**************************************************************************
PARAMETER X
DECLARE laApp_nam[1,1],laApp_fl[1,1],laSelected [1,2],;
        laCompany[1,1],laComPath[1,1],laComMdls[1,1]
        
STORE ' ' TO laApp_nam,laApp_fl,laCompany,laComPath,laComMdls

lnBarNo     = 1
lnSelected  = 0
lcModules   = " "
lcMod_ID    = " "
lcOldMdl    = " "
lcCompany   = " "
lcComp_ID   = " "
lcOldComp   = " "
lcComMdls   = " "
lcSubsVar   = ' '
lcTargDir   = ' '
lcFilePath  = ' '

lcTUnAble   = ' '

llAllComp   = .F.
llTaged     = .F.
llNoMdls    = .F.
llYes2All   = .F.
STORE .F. TO llOpen1,llOpen2,llOpen3,llOpen4,llOpen5,llOpen6,llOpen7,llOpen8

IF !("FFIX4_25.FLL" $ SET('LIBR'))
  SET LIBR TO FULLPATH('')+"FFIX4_25.FLL" ADDI
  llOpen8=.T.
ENDIF

IF !USED('syuusrpr')
  SELECT 0
  USE (gcSysHome+"syuusrpr") ORDER 1
  llOpen1=.T.
ENDIF

IF !USED('SYCCOMP')
  SELECT 0
  USE (gcSysHome+"SYCCOMP") ORDER TAG CCOMP_ID
  llOpen2=.T.
ENDIF

IF !USED('SYDAPPL')
  SELECT 0
  USE (gcSysHome+"SYDAPPL") ORDER TAG CAPP_ID
  llOpen3=.T.
ENDIF

IF !USED('SYDFILES')
  SELECT 0
  USE (gcSysHome+"SYDFILES") 
  llOpen4=.T.
ENDIF

IF !USED('SYDFLFLD')
  SELECT 0
  USE (gcSysHome+"SYDFLFLD")
  llOpen5=.T.
ENDIF


IF !USED('SYDINDEX')
  SELECT 0
  USE (gcSysHome+"SYDINDEX") 
  llOpen6=.T.
ENDIF

IF !USED('SYDFIELD')
  SELECT 0
  USE (gcSysHome+"SYDFIELD")
  llOpen7=.T.
ENDIF

*** Collect all companies
*-- [x,1]company name  [x,2]directory of data  [x,3]Modules for this company.  
SELECT ccomp_id+" - "+cCom_Name,PADR(gfGetDataDir(ALLT(cCom_dDir)),LEN(cCom_dDir)),ALLT(syccomp.mcomp_mdl);
  FROM (gcSyshome+"syccomp") ;
  INTO ARRAY laCompany ;
  ORDER BY 1
  


IF _TALLY > 0
  DECLARE laCompany [_TALLY+1,3] 

  =AINS('laCompany',1)
  laCompany [1,1] = "     All Companies"
  laCompany [1,2] = " "
  laCompany [1,3] = " "
  lcComp_ID       = SUBSTR(laCompany[2,1],1,2)
  lcCompany       = laCompany[2,1]
  puCompany       = 2
  lcFilePath      = ALLTRIM(LOWER(laCompany[2,2]))
  lcComMdls       = ALLTRIM(laCompany[2,3])
  =gfSubStr(lcComMdls,@laComMdls,"|")
ENDIF

*** Collect all modules
SELECT cApp_name,cApp_ID ;
  FROM (gcSyshome+"sydappl");
  INTO ARRAY laApp_nam ;
  ORDER BY 2;
  WHERE cApp_ID <> "SY" .AND. cApp_ID <> "SM" .AND.;
        cApp_ID $ lcComMdls
  
IF _TALLY > 0
  *** Add more bar to select all modules
  DECLARE laApp_nam [_TALLY+2,2] 
  =AINS('laApp_nam',1)
  laApp_nam [1,1] = "Company Settings "
  laApp_nam [1,2] = "__"
  lcModules       = laApp_nam [1,1]
  lcMod_ID        =  '  '
  
  =AINS('laApp_nam',1)
  laApp_nam [1,1] = "     All Modules"
  laApp_nam [1,2] = "  "
  lcModules       = laApp_nam [1,1]
  lcMod_ID        = laApp_nam [1,2]
  
  
  puApp_nam       = 1
  llNoMdls        = .F.
ELSE
  *** Add more bar to select all modules
  DECLARE laApp_nam [_TALLY+2,2] 
  =AINS('laApp_nam',1)
  laApp_nam [1,1] = "Company Settings "
  laApp_nam [1,2] = "__"
  lcModules       = laApp_nam [1,1]
  lcMod_ID        = '  '
  
  =AINS('laApp_nam',1)
  laApp_nam [1,1] = "No modules are installed for this company"
  laApp_nam [1,2] = "  "
  lcModules       = laApp_nam [1,1]
  lcMod_ID        = laApp_nam [1,2]
  llNoMdls        = .T.
ENDIF  

*** Collect Files for selected module
IF !EMPTY(lcComMdls)
   SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl  ;
      FROM (gcSyshome+"sydfiles") ;
      INTO ARRAY laApp_fl ;
      WHERE LEFT(cFile_nam,2) <> "SY" .AND. lfCompMdl(ALLTRIM(mfile_app));
      ORDER BY 1
ENDIF      
   
*********

DO lpRnAsFix

IF llOpen1
  USE IN syuusrpr
ENDIF

IF llOpen2
  USE IN SYCCOMP
ENDIF

IF llOpen3
  USE IN SYDAPPL
ENDIF
IF llOpen4
  USE IN SYDFILES
ENDIF

IF llOpen5
  USE IN SYDFLFLD
ENDIF

IF llOpen6
  USE IN SYDINDEX
ENDIF

IF llOpen7
  USE IN SYDFIELD
ENDIF

IF llOpen8
  RELEASE LIBRARY FULLPATH("")+"FFIX4_25.FLL" 
ENDIF
=gfThermo(100 ,100,'','')

*!*******************************************************************
*!
*!      Function: lfvSysOrMd
*!
*!*******************************************************************
* Collect files of ether the system files or the selected module
* or all modules
FUNCTION lfvSysOrMd

DECLARE laApp_fl [1,1]
laApp_fl  =" "

*** Select only system files
IF cbSysOrMd = 1
  SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
    FROM (gcSyshome+"sydFiles") ;
    INTO ARRAY laApp_fl ;
    WHERE LEFT(cFile_nam,2) = 'SY' AND ;
         ("SY" $ mFile_app .OR. "SM" $ mFile_app) ;
    ORDER BY 1
    
  SHOW GET ibModules DISABLE
  SHOW GET ibCompany DISABLE
  SHOW GET puApp_nam DISABLE
  SHOW GET puCompany DISABLE
*** Select module(s) files
ELSE
  *** Files of one module 
  IF !EMPTY(lcMod_ID)
     IF lcMod_id = '__'
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (gcSyshome+"sydfiles") ;
         INTO ARRAY laApp_fl ;
        WHERE 'SY' $ mFile_app ;
        AND LEFT(cFile_nam,2) <>'SY';
        ORDER BY 1
     
     ELSE
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (gcSyshome+"sydfiles") ;
         INTO ARRAY laApp_fl ;
        WHERE lcMod_ID $ mFile_app ;
        ORDER BY 1
      ENDIF  
  *** All files of all modules
  ELSE
    *** If all companies was selected select all files of all modules
    IF EMPTY(lcComp_ID)
      SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
        FROM (gcSyshome+"sydfiles") ;
        INTO ARRAY laApp_fl ;
       WHERE LEFT(cFile_nam,2) <> "SY" ;
       ORDER BY 1

    *** If company was selected select all files of all installed modules 
    *** to the selected cmompany
    ELSE
      SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (gcSyshome+"sydfiles") ;
         INTO ARRAY laApp_fl ;
        WHERE LEFT(cFile_nam,2) <> "SY" .AND. lfCompMdl(ALLTRIM(mfile_app));
        ORDER BY 1
    ENDIF   
  ENDIF   

  SHOW GET ibModules ENABLE
  SHOW GET ibCompany ENABLE
  SHOW GET puApp_nam ENABLE
  SHOW GET puCompany ENABLE
ENDIF

SHOW GET lsApp_fl

llTaged     = .F.                        && Tag None
SHOW GET  pbTag,1 PROMPT "Tag \<All" 
*WALID
IF TYPE('llExtCall')='U' 
  _CUROBJ = OBJNUM(lsApp_fl)
ENDIF
*!*******************************************************************
*!
*!      Function: lfvModules
*!
*!*******************************************************************
FUNCTION lfvModules

DO CASE
  CASE _DOS
    lcMod_ID = gfActPop(3,23,12,68,'laApp_nam',2,1,@lcModules)
  CASE _WINDOWS
    lcMod_ID = laApp_nam[puApp_nam,2]
ENDCASE    

*** If the user select new module
IF lcMod_ID  <> lcOldMdl
  IF _DOS
    =lfRefresh()
  ENDIF  
  laApp_fl = " "
  
  *** If the user select the first option (All modules)
  IF !EMPTY(lcMod_ID)
     IF lcMod_id = '__'
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (gcSyshome+"sydfiles") ;
         INTO ARRAY laApp_fl ;
        WHERE 'SY' $ mFile_app ;
        AND LEFT(cFile_nam,2) <>'SY';
        ORDER BY 1
     
     ELSE
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (gcSyshome+"sydfiles") ;
         INTO ARRAY laApp_fl ;
        WHERE lcMod_ID $ mFile_app ;
        ORDER BY 1
      ENDIF  
  *** If the user select on module
  ELSE
    *** If the user was selecting all companies option
    *** get all the files 
    IF EMPTY(lcComp_ID)
       SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (gcSyshome+"sydfiles") ;
         INTO ARRAY laApp_fl ;
        WHERE LEFT(cFile_nam,2) <> "SY" ;
        ORDER BY 1
    *** If one company was selected get all the files of the installed
    *** modules
    ELSE
      SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
         FROM (gcSyshome+"sydfiles") ;
         INTO ARRAY laApp_fl ;
        WHERE LEFT(cFile_nam,2) <> "SY" .AND. lfCompMdl(ALLTRIM(mfile_app));
        ORDER BY 1
    ENDIF    
  ENDIF    
  
  SHOW GET lsApp_fl
  llTaged     = .F.                        && Tag None
  SHOW GET  pbTag,1 PROMPT "Tag \<All" 
  
ENDIF  

_CUROBJ = OBJNUM(lsApp_fl)

*!*******************************************************************
*!
*!      Function: lfCompMdl
*!
*!*******************************************************************
* This function is beeing called from the select SQL to determin if 
* one file from the file dectionary belong to a module installed to 
* a spacific company to be colleted or not
FUNCTION lfCompMdl
PARAMETERS lcFileMdl

llRetFlag = .F.

FOR lnCount = 1 TO ALEN(laComMdls,1)
  *B601949,1 Hesham (Start)
  *B601949,1 view all files of the installed modules for the selected comp.
  *B601949,1 beside the system settings files
*  IF laComMdls[lnCount] $ lcFileMdl
  IF (laComMdls[lnCount] $ lcFileMdl) OR ('SY' $ lcFileMdl)
  *B601949,1 Hesham (End)  
    llRetFlag = .T.
    EXIT
  ENDIF
ENDFOR

RETURN llRetFlag

*!*******************************************************************
*!
*!      Function: lfvCompany
*!
*!*******************************************************************
FUNCTION lfvCompany

*** Get the company ID from the selected bar of the popup
DO CASE 
  CASE _DOS
    lcFilePath = ALLTRIM(LOWER(gfActPop(4,23,12,53,'laCompany',2,1,@lcCompany)))
    lcComp_ID  = SUBSTR(lcCompany,1,2)
    lnAryElem  = BAR()
  CASE _WINDOWS
    lcFilePath = laCompany[puCompany,2]
    lcComp_ID  = SUBSTR(laCompany[puCompany,1],1,2)
    lnAryElem  = puCompany
ENDCASE  

*** If the user select new company
IF lcComp_ID  <> lcOldComp

  *** If the user select all companies option 
  IF EMPTY(lcComp_ID)
    llAllComp  =.T.
    lcComMdls  = " "
    laComMdls  = " "

    SELECT cApp_name,cApp_ID ;
      FROM (gcSyshome+"sydappl");
      INTO ARRAY laApp_nam ;
     WHERE cApp_ID <> "SY" .AND. cApp_ID <> "SM"

    *** Add new bar " all modules "
    IF _TALLY > 0
       *** Add more bar to select all modules
       DECLARE laApp_nam [_TALLY+2,2] 
       =AINS('laApp_nam',1)
       laApp_nam [1,1] = "Company Settings "
       laApp_nam [1,2] = "__"
       lcModules       = laApp_nam [1,1]
       lcMod_ID        = '  '
       
       =AINS('laApp_nam',1)
       laApp_nam [1,1] = "     All Modules"
       laApp_nam [1,2] = " "
       IF llNoMdls
         lcModules     = laApp_nam [1,1]
         lcMod_ID      = laApp_nam [1,2]
         puApp_nam     = 1
       ENDIF  
       llNoMdls        = .F.
    ELSE
       *** Add more bar to select all modules
       DECLARE laApp_nam [_TALLY+2,2] 
       =AINS('laApp_nam',1)
       laApp_nam [1,1] = "Company Settings "
       laApp_nam [1,2] = "__"
       lcModules       = laApp_nam [1,1]
       lcMod_ID        = '  '
       
       =AINS('laApp_nam',1)
       laApp_nam [1,1] = "No modules are installed for this company"
       laApp_nam [1,2] = " "
       llNoMdls        = .T.
    ENDIF  

    SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
       FROM (gcSyshome+"sydfiles") ;
       INTO ARRAY laApp_fl ;
       WHERE LEFT(cFile_nam,2) <> "SY" ;
       ORDER BY 1


  ***********************************
  *** If the user select one company
  ELSE
    llAllComp   =.F.
    lcComMdls   = ALLTRIM(laCompany[lnAryElem,3])
    =gfSubStr(lcComMdls,@laComMdls,"|")

    SELECT cApp_name,cApp_ID ;
      FROM  (gcSyshome+"sydappl"),(gcSyshome+"SYCCOMP");
      INTO  ARRAY laApp_nam ;
      WHERE syccomp.ccomp_id=lcComp_ID .AND. ;
            sydappl.capp_id $ ALLTRIM(syccomp.mcomp_mdl) .AND. ;
            cApp_ID <> "SY" .AND. cApp_ID <> "SM"

     *** Add new bar " All Modules"
    IF _TALLY > 0
      *** Add more bar to select all modules
      DECLARE laApp_nam [_TALLY+2,2] 
      =AINS('laApp_nam',1)
      laApp_nam [1,1] = "Company Settings "
      laApp_nam [1,2] = "__"
      lcModules       = laApp_nam [1,1]
      lcMod_ID        = '  '
      
      =AINS('laApp_nam',1)
      laApp_nam [1,1] = "     All Modules"
      laApp_nam [1,2] = " "

      IF llNoMdls
        lcModules     = laApp_nam [1,1]
        lcMod_ID      = laApp_nam [1,2]
        puApp_nam     = 1
      ENDIF  

      llNoMdls        = .F.
    ELSE
      *** Add more bar to select all modules
      DECLARE laApp_nam [_TALLY+2,2] 
      =AINS('laApp_nam',1)
      laApp_nam [1,1] = "Company Settings "
      laApp_nam [1,2] = "__"
      lcModules       = laApp_nam [1,1]
      lcMod_ID        = '  '
      
      =AINS('laApp_nam',1)
      laApp_nam [1,1] = "No modules are installed for this company"
      laApp_nam [1,2] = " "
      lcModules       = laApp_nam [1,1]
      lcMod_ID        = laApp_nam [1,2]
      puApp_nam       = 1
      llNoMdls        = .T. 
      laApp_fl = " "
      *MAN 
      RETURN .F.
    ENDIF  
    
    *** If the selected module is not installed to the new selected
    *** company select the first one and collect it's files
    IF !llNoMdls 
      IF !EMPTY(lcMod_ID)
        IF ASCAN(laApp_nam,lcMod_ID) = 0
          lcModules       = laApp_nam [2,1]
          lcMod_ID        = laApp_nam [2,2]

          laApp_fl = " "
       IF lcMod_id = '__'
         SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
           FROM (gcSyshome+"sydfiles") ;
           INTO ARRAY laApp_fl ;
          WHERE 'SY' $ mFile_app ;
          AND LEFT(cFile_nam,2) <>'SY';
          ORDER BY 1
       ELSE           
          SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
            FROM (gcSyshome+"sydfiles") ;
            INTO ARRAY laApp_fl ;
           WHERE lcMod_ID $ mFile_app ;
           ORDER BY 1
        ENDIF  
        ENDIF
      ELSE
        SELECT " "+cFile_nam+" - "+cfile_ttl,lsys_data,cupgrdlvl ;
           FROM (gcSyshome+"sydfiles") ;
           INTO ARRAY laApp_fl ;
           WHERE LEFT(cFile_nam,2) <> "SY" .AND. lfCompMdl(ALLTRIM(mfile_app));
           ORDER BY 1
      ENDIF
    ENDIF
  ENDIF
  
  IF _DOS 
    =lfRefresh()
  ELSE
    SHOW GET puApp_nam   
  ENDIF  

  llTaged     = .F.                        && Tag None
  SHOW GET lsApp_fl
  SHOW GET  pbTag,1 PROMPT "Tag \<All" 
ENDIF

*!*******************************************************************
*!
*!      Function: lfvApp_fl
*!
*!*******************************************************************
*
FUNCTION lfvApp_fl

IF LEFT(laApp_fl[lsApp_fl,1],1) = ""
   laApp_fl[lsApp_fl,1] =" "+SUBSTR(laApp_fl[lsApp_fl,1],2) 
ELSE
   laApp_fl[lsApp_fl,1] =""+SUBSTR(laApp_fl[lsApp_fl,1],2) 
ENDIF
SHOW GET lsApp_fl

*!*******************************************************************
*!
*!      Function: lfvTag
*!
*!*******************************************************************
*
FUNCTION lfvTag  

IF !EMPTY(laApp_fl)
  DO CASE
    CASE !llTaged                              && Tag All Files
      llTaged     = .T.
      SHOW GET  pbTag,1 PROMPT "Tag \<None" 
 
      FOR lcFlNo = 1 TO ALEN(laApp_fl,1)
        IF LEFT(laApp_fl[lcFlNo,1],1)<> ""
          laApp_fl[lcFlNo,1] =""+SUBSTR(laApp_fl[lcFlNo,1],2) 
        ENDIF
      ENDFOR

    CASE llTaged 
      llTaged     = .F.                        && Tag None
      SHOW GET  pbTag,1 PROMPT "Tag \<All" 
      FOR lcFlNo = 1 TO ALEN(laApp_fl,1)
        IF LEFT(laApp_fl[lcFlNo,1],1) = ""
          laApp_fl[lcFlNo,1] =" "+SUBSTR(laApp_fl[lcFlNo,1],2) 
        ENDIF
      ENDFOR
  ENDCASE 
  SHOW GET lsApp_fl  

ENDIF     

*!*******************************************************************
*!
*!      Function: lfDoAction
*!
*!*******************************************************************
*
FUNCTION lfDoAction
PARAMETERS lcAction

DECLARE lax[1]
lax=" "


IF lcAction # "CHECK"
  =lfvAselect()                 && Collect Selected files
ENDIF


IF !EMPTY(laSelected [1,1])   && Exit if None 

  DECLARE laFileStrc[1,1],laFileAStr[1,1]

  llOpenRep  = .F.               && Open Report flage
  lcFilHandl = ''
  lcTempNam  = ''
  
  
  lcFixMess  = ''
  
  lnRepLine  = 0
  llYes2All  = .F.
  llUpdated = .f.
  DO CASE
    CASE lcAction = "VERIFY"
      lcTherMesg  = "Verifying file structure..."
    CASE lcAction = "UPDATE" 
      lcTherMesg  = "Updating file structure..."
    CASE lcAction = "INDEX"
      lcTherMesg  = "Building index for file..."
    CASE lcAction = "REORGANIZ"
      lcTherMesg  = "Reorganizing file ..."
    CASE lcAction = "CLEARLOCKS"
      lcTherMesg  = "Clear locks..."
      *E301316,1 add new case [Start]
    CASE lcAction = "CHECK"
      lcTherMesg  = "Check for corruption..."
      *E301316,1 add new case [End..]
  ENDCASE    

  *** Verify files for all companies
  IF llAllComp .AND. cbSysOrMd = 2
    *** Check all selected files
    lnLastComp = ALEN(laCompany,1)

    *** Loop all or one company
    FOR lnCompNo = 2 TO lnLastComp
      lcFilePath = ALLTRIM(laCompany[lnCompNo,2])
      lnSelected = ALEN(laSelected,1)

      *** Check if there is one module selecet if this module is not installed to
      *** the company in the loop skip this company
      IF !EMPTY(lcMod_ID) .AND. !(lcMod_ID $ ALLTRIM(laCompany[lnCompNo,3])) AND lcMod_ID<>'__'
        LOOP
      ENDIF

      *** Loop all selected files
      FOR lnCount = 1 TO lnSelected
        lcFileNam = UPPER(ALLTRIM(laSelected[lnCount,1]))

        *B601592,1 Change this line [Begin]
        *lcTargDir = UPPER(IIF(UPPER(LEFT(lcFileNam ,2))='SY',gcSysHome,lcFilePath))
        lcTargDir = UPPER(IIF(UPPER(LEFT(lcFileNam ,2)) = 'SY', ALLTRIM(gcSysHome) , ALLTRIM(lcFilePath)))
        *B601592,1 Change this line [End]

        *E301316,1 Adjust the next check [Start] 
        *IF LEFT(lcFileNam,2) $ ALLTRIM(laCompany[lnCompNo,3]) OR laSelected[lnCount,3]='S'
        IF !USED('sydfiles')
          =gfOpenFile(gcSysHome+'sydfiles','Cfile_nam')
        ELSE
          SELECT sydfiles
          SET ORDER TO TAG Cfile_nam
        ENDIF
        =SEEK(lcFileNam,'sydfiles')
               
        =gfSubStr(sydfiles.Mfile_app,@laX,',')
        PRIVATE llFound
        llFound=.F.
        FOR lnI=1 TO ALEN(laX,1)
          IF laX[lnI] $ ALLTRIM(laCompany[lnCompNo,3])
            llFound=.T.
            EXIT
          ENDIF
        ENDFOR
        IF llFound
        *E301316,1 Adjust the next check [End..] 
        
          *** If the to be cheked file belong to one of the installed modules
          *** for this company, then verify it
          lcSubsVar = ALLTRIM(laSelected[lnCount,2])
          =gfThermo(lnSelected ,lnCount-.01,lcTherMesg,lcSubsVar)
          DO CASE
            CASE lcAction = "VERIFY"
              =lfVer_Upd(lcFileNam)
            CASE lcAction = "UPDATE" 
              llUpdated = lfVer_Upd(lcFileNam,.T.)  
              IF !llUpdated
                EXIT
              ENDIF
            CASE lcAction = "INDEX"
              =lfIndex(lcFileNam)
            CASE lcAction = "REORGANIZ"
              =lfReOrg(lcFileNam)
            CASE lcAction ="CLEARLOCKS"
              =lfClrLock(lcFileNam)
            
            *E301316,1 add new case [Start]
            CASE lcAction = "CHECK"
              laFixInfo[1]=lcTargDir+lcFileNam
              =lfFixDbf()
            *E301316,1 add new case [End..]
            
          ENDCASE
          =gfThermo(lnSelected ,lnCount,lcTherMesg,lcSubsVar)
        ELSE
          *** If not skip this files but keep the thermometer running
          lcSubsVar = " "
          =gfThermo(lnSelected ,lnCount,lcTherMesg,lcSubsVar)
        ENDIF
      ENDFOR
    ENDFOR
  *** Verify files of one company
  ELSE
    lnSelected = ALEN(laSelected,1)

    *** Loop all selected files
    FOR lnCount = 1 TO lnSelected
      lcFileNam = UPPER(ALLTRIM(laSelected[lnCount,1]))

      *B601592,1 Change this line [Begin]
      *lcTargDir = UPPER(IIF(UPPER(LEFT(lcFileNam ,2))='SY',gcSysHome,lcFilePath))
      lcTargDir = UPPER(IIF(UPPER(LEFT(lcFileNam ,2)) = 'SY', ALLTRIM(gcSysHome) , ALLTRIM(lcFilePath)))
      *B601592,1 Change this line [End]
      
      lcSubsVar = ALLTRIM(laSelected[lnCount,2])
      =gfThermo(lnSelected ,lnCount-.01,lcTherMesg,lcSubsVar)
      DO CASE
        CASE lcAction = "VERIFY"
          =lfVer_Upd(lcFileNam)
        CASE lcAction = "UPDATE" 
          llUpdated = lfVer_Upd(lcFileNam,.T.)  
          IF !llUpdated
            EXIT
          ENDIF
        CASE lcAction = "INDEX"
          =lfIndex(lcFileNam)
        CASE lcAction = "REORGANIZ"
          =lfReOrg(lcFileNam)
        CASE lcAction ="CLEARLOCKS"
          =lfClrLock(lcFileNam)
        *E301316,1 add new case [Start]
        CASE lcAction = "CHECK"
          laFixInfo[1]=lcTargDir+lcFileNam
          =lfFixDbf()
        *E301316,1 add new case [End..]

      ENDCASE
      =gfThermo(lnSelected ,lnCount,lcTherMesg,lcSubsVar)
    ENDFOR
  ENDIF

  *** If there is a report created dispaly it to the user
  IF llOpenRep
    =FFLUSH(lcFilHandl)

    DO WHILE !FCLOSE(lcFilHandl)

    ENDDO
    CREATE CURSOR TMPSTR (mStrRep M(10))
    APPEND BLANK
    APPEND MEMO mStrRep FROM (gcWorkDir+"structur.txt")
    *E300683,4 Call *.SPR from screens directory
    * DO SMSTRREP.SPR
    *walid
    IF TYPE('llExtCall')='U'  
      DO (gcScrDir + gcWinAppl + '\SMSTRREP.SPR')
    ELSE
      DO (gcScrDir + 'SM\SMSTRREP.SPR')
    ENDIF
    *E300683,4 end  
    USE IN TMPSTR
  ELSE
   *E300766,1 Hesham (Start)
   *E300766,1 Change The message to be readable by the user  
   *=gfModalGen ('INM00005B00000','Dialog')   
    DO CASE
      CASE lcAction = "VERIFY"
        =gfModalGen ('INM00005B00000','Dialog')
      CASE lcAction = "UPDATE" and llUpdated
        =gfModalGen ('INM00279B00000','Dialog')
      CASE lcAction = "INDEX"
        =gfModalGen ('INM00280B00000','Dialog')
      CASE lcAction = "REORGANIZ"
        *=gfModalGen ('INM00281B00000','Dialog')
      CASE lcAction ="CLEARLOCKS"
        =gfModalGen ('INM00282B00000','Dialog')
      *E301316,1 add new message [Start]
      CASE lcAction = "CHECK"
        *-- No corrupted file(s) found.
        =gfModalGen ('INM00363B00000','Dialog')
      *E301316,1 add new case [End..]


    ENDCASE
   *E300766,1 Hesham (End)    
  ENDIF
ELSE

  =gfModalGen ('INM00018B00000','Dialog')  
ENDIF

*!*******************************************************************
*!
*!      Function: lfVer_Upd
*!
*!*******************************************************************
*
* Function to verify or update data and system files
*
FUNCTION lfVer_Upd
PARAMETERS lcFileNam,llUpdate

DECLARE laFileStrc[1,1],laFileAStr[1,1]

llUpdate   = IIF(TYPE('llUpdate')='U',.F.,llUpdate)
lcTmpFNm   = ''
llFileCorr = .F.               && Flage If file Corrupted
llRetFlag  = .T.


*** Dictionary file strc.
*B601701,1 Hesham (Start)
*B601701,1 change the update structure function and the update index
*B601701,1 to check for the complete name of the file with 8 char.
*SELECT sydflfld.cfld_name,sydfield.cdata_typ,;
       sydfield.nfld_wdth,sydfield.nfld_dec,sydflfld.nfld_pos;
       FROM  (gcSyshome+"sydflfld"),(gcSyshome+"sydfield");
       ORDER BY sydflfld.nfld_pos;
       GROUP BY sydField.cFld_Name;
       WHERE UPPER(sydflfld.cfile_nam) =UPPER(lcFileNam);
       AND sydfield.cfld_name = sydflfld.cfld_name;
       INTO ARRAY laFileStrc

SELECT ALLT(sydflfld.cfld_name),sydfield.cdata_typ,;
       sydfield.nfld_wdth,sydfield.nfld_dec,sydflfld.nfld_pos;
       FROM  (gcSyshome+"sydflfld"),(gcSyshome+"sydfield");
       ORDER BY sydflfld.nfld_pos;
       GROUP BY sydField.cFld_Name;
       WHERE UPPER(sydflfld.cfile_nam) =UPPER(PADR(lcFileNam,8));
       AND sydfield.cfld_name = sydflfld.cfld_name;
       INTO ARRAY laFileStrc
*B601701,1 Hesham (End)
=gfAdel(@laFileStrc,5,2)       && An additional colum holding the position 
                               && of each field has to be removed from the
                               && Array befor verification or building.
     

PRIVATE lnCount 
FOR lnCount = 1 TO ALEN(laFileStrc,1)
  laFileStrc[lnCount,1] = ALLTRIM(laFileStrc[lnCount,1])
ENDFOR
*** Check if file exist or build new one
IF FILE(lcTargDir+lcFileNam+".DBF")

  *** File was found in target directory
  IF USED(lcFileNam)
    llOpened = .T.
    SELECT(lcFileNam)
  ELSE  
    llOpened = .F.
    SELECT 0
    USE (lcTargDir+lcFileNam) 
  ENDIF

  *** Actual file structure
  =AFIELDS('laFileAStr')

  IF !EMPTY(laFileStrc[1])            && Found data for this file
    *** Check all fields information
    FOR lnFieldNo = 1 TO ALEN(laFileStrc)     &&All Fields
      IF TYPE ('laFileAStr[lnFieldNo]') <> 'U'
*        IF laFileStrc[lnFieldNo]<>laFileAStr[lnFieldNo]
        IF !(PADR(laFileStrc[lnFieldNo],10)==PADR(laFileAStr[lnFieldNo],10))
          IF llUpdate
            llFileCorr = .T.
            EXIT
          ELSE
            *** Report corrupted field
            =lfReport(1)              && Wrong field
          ENDIF  
        ENDIF
      ELSE 
        IF llUpdate
          llFileCorr = .T.
          EXIT
        ELSE
          *** Report not existing field 
          =lfReport(1)
        ENDIF
      ENDIF
    ENDFOR       
    IF ALEN(laFileStrc)<>ALEN(laFileAStr) AND !llFileCorr
      llFileCorr = .T.
      =lfReport(4)
    ENDIF  
    IF llFileCorr AND llUpdate
      IF !llYes2All
        lnOption = gfModalGen('QRM00004B00004','Dialog',lcSubsVar)  
        DO CASE
          CASE lnOption = 1
            *** Fix file structure
            =lfUpdate(lcFileNam)
          CASE lnOption = 3
            llYes2All  = .T.
            =lfUpdate(lcFileNam)
          CASE lnOption = 4
            llRetFlag  = .F.
            =gfThermo(100 ,100,'','')
        ENDCASE
      ELSE
        *** Fix file structure
        =lfUpdate(lcFileNam)
      ENDIF  

    ENDIF
  ELSE
    *** File infornation is not stored in dictionary  
    =lfReport(2)                    
  ENDIF    

  IF !llOpened 
     USE
  ENDIF  

*** File was not found in target directory
ELSE
  IF llUpdate
    *** Build new file 
    =lfUpdate(lcFileNam,.T.)
  ENDIF
  =lfReport(3)
ENDIF

RETURN llRetFlag

*!*******************************************************************
*!
*!      Function: lfUpdate
*!
*!*******************************************************************
*
* This function will build a new data files from the dectionary 
* and their indexes
*
FUNCTION lfUpdate
PARAMETERS lcFileNam,llBldFile

llBldFile = IIF(TYPE('llBldFile')='U',.F.,llBldFile)
lcTempNam = gfTempName()

IF !EMPTY(laFileStrc[1])
  IF llBldFile 
    CREATE DBF  (lcTargDir+lcFileNam) FROM ARRAY laFileStrc
  ELSE
    CREATE DBF  (lcTargDir+lcTempNam) FROM ARRAY laFileStrc
    APPEND FROM (lcTargDir+lcFileNam)
    USE

    IF USED(lcFileNam)
      USE IN (lcFileNam)
    ENDIF  

    ERASE  (lcTargDir+lcFileNam+".DBF")
    ERASE  (lcTargDir+lcFileNam+".FPT")
    ERASE  (lcTargDir+lcFileNam+".CDX")
            
    RENAME (lcTargDir+lcTempNam+".DBF") TO (lcTargDir+lcFileNam+".DBF")

    IF FILE(lcTargDir+lcTempNam+'.FPT')	
      RENAME (lcTargDir+lcTempNam+".FPT") TO (lcTargDir+lcFileNam+".FPT")
    ENDIF 

    SELECT 0
    USE (lcTargDir+lcFileNam)
  ENDIF

  =lfIndex(lcFileNam) 

ELSE 
  WAIT lcTUnAble+lcFileNam WINDOW
ENDIF  

*!*******************************************************************
*!
*!      Function: lfIndex
*!
*!*******************************************************************
* This function will creat or fix the index for one file
*
FUNCTION lfIndex
PARAMETERS lcFileNam
DECLARE laFileCDX[1,1]

lcSavAlias = SELECT(0)
*B601701,1 Hesham (Start)
*B601701,1 change the update structure function and the update index
*B601701,1 to check for the complete name of the file with 8 char.
*SELECT ALLTRIM(sydindex.cindx_exp),ALLTRIM(sydindex.cfile_tag),sydindex.lascend,;
       sydindex.lunique;
       FROM (gcSyshome+"sydindex");
       WHERE UPPER(sydindex.cfile_nam) = lcFileNam;
       INTO ARRAY laFileCDX

SELECT ALLTRIM(sydindex.cindx_exp),ALLTRIM(sydindex.cfile_tag),sydindex.lascend,;
       sydindex.lunique;
       FROM (gcSyshome+"sydindex");
       WHERE UPPER(sydindex.cfile_nam) = PADR(lcFileNam,8);
       INTO ARRAY laFileCDX
*B601701,1 Hesham (End)
IF !USED(lcFileNam)
  SELECT 0
ELSE
  SELECT (lcFileNam)
ENDIF  
IF FILE(ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)+'.DBF')
  USE (ALLTRIM(lcTargDir)+ALLTRIM(lcFileNam)) EXCL

  IF !EMPTY(laFileCDX[1])
      FOR lnTagNo = 1 TO ALEN(laFileCDX,1)
        lcAscend  =IIF(laFileCDX[lnTagNo,3],'ASCENDING','DESCENDING')
        lcUnique  =IIF(laFileCDX[lnTagNo,4],'UNIQUE','')
        INDEX ON &laFileCDX[lnTagNo,1] TAG &laFileCDX[lnTagNo,2]; 
              ADDITIVE &lcAscend &lcUnique
      ENDFOR            
  ENDIF
  USE
ELSE
  =lfReport(2)                      
ENDIF  

SELECT (lcSavAlias)

*!*******************************************************************
*!
*!      Function: lfReOrg
*!
*!*******************************************************************
*!
FUNCTION lfReOrg

PARAMETERS lcFileNam

SELECT SYDFILES 
SET ORDER TO TAG CFILE_NAM

lcMastTag = LOOKUP(cfile_tag,lcFileNam,CFILE_NAM,'CFILE_NAM')

IF !USED(lcFileNam)
  SELECT 0
ELSE
  SELECT (lcFileNam)
ENDIF  
IF !FILE(lcTargDir+lcFileNam+".FPT")
  RETURN
ENDIF
IF FIXDBF(lcTargDir+lcFileNam+".DBF",0,64)=0
  RETURN
ENDIF

*-- use gfopenfile to be able to see our errorhandler so if any
*-- error in file openning error handler will fix it
*walid check for the physical existance of the file
IF FILE(lcTargDir+lcFileNam+".DBF") AND FIXDBF(lcTargDir+lcFileNam+".DBF",0,33)=0
  USE (lcTargDir+lcFileNam) EXCL

  IF !EMPTY(lcMastTag)
    SET ORDER TO TAG &lcMastTag
  ENDIF
  IF TYPE('lcExt') # "C"
    lcTempName = gfTempName()
    COPY TO (lcTargDir+lcTempName+".DBF") CDX FOR .T.
    USE 

    ERASE (lcTargDir+lcFileNam+".DBF") 
    ERASE (lcTargDir+lcFileNam+".CDX")
    ERASE (lcTargDir+lcFileNam+".FPT")

    IF FILE (lcTargDir+lcTempName+".DBF")
      RENAME (lcTargDir+lcTempName+".DBF") TO (lcTargDir+lcFileNam+".DBF")
    ENDIF

    IF FILE (lcTargDir+lcTempName+".CDX")
      RENAME (lcTargDir+lcTempName+".CDX") TO (lcTargDir+lcFileNam+".CDX")
    ENDIF

    IF FILE (lcTargDir+lcTempName+".FPT")
      RENAME (lcTargDir+lcTempName+".FPT") TO (lcTargDir+lcFileNam+".FPT")
    ENDIF
  ENDIF
ENDIF

*!*******************************************************************
*!
*!      Function: lfClrLock
*!
*!*******************************************************************
*
FUNCTION lfClrLock
PARAMETERS lcFileNam

IF FILE(lcTargDir+lcFileNam+".DBF")
  IF !USED(lcFileNam)
    SELECT 0
  ELSE
    SELECT (lcFileNam)
  ENDIF  
  USE (lcTargDir+lcFileNam) EXCL
 *B500644,1 added condition to chek wether the locking fields
 *B500644,1 exist or not before replacing because some files does not
 *B500644,1 have the locking fields "ex. Resource file"
 IF TYPE('llok_stat')='L' AND TYPE('dlok_date')='D';
    AND TYPE('clok_user')='C' AND TYPE('clok_time')='C'
    REPLACE ALL llok_stat WITH .F. ;
                dlok_date WITH {} ;
                clok_user WITH "" ;
                clok_time WITH "" ;
            FOR llok_stat
  ENDIF
  SELECT (lcFileNam)
  USE
ENDIF

*!*******************************************************************
*!
*!      Function: lfReport
*!
*!*******************************************************************
*
FUNCTION lfReport
PARAMETERS lnAction
DECLARE laFieldTyp[4,1]
laFieldTyp[1]='name  '
laFieldTyp[2]='type  '
laFieldTyp[3]='width '
laFieldTyp[4]='dec.  '

IF !llOpenRep  
  llOpenRep = .T.
  lcFilHandl = FCREAT('&gcWorkDir.structur.txt')
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  *E301316,1 [Start] 
  *=FPUTS(lcFilHandl,"*                  FILE VERIFICATION STATS REPORT                  *")
  =FPUTS(lcFilHandl,"*                  FILE VERIFICATION STATUS REPORT                  *")
  *E301316,1 [End..] 
  =FPUTS(lcFilHandl,REPLICATE('*',68))
  =FPUTS(lcFilHandl,' ')
  =FPUTS(lcFilHandl,' ')
ENDIF

IF lcTempNam <> lcFileNam
   lcTempNam =  lcFileNam
   IF lnRepLine > 0
     =FPUTS(lcFilHandl,REPLICATE('Ä',68))
   ENDIF
   lnRepLine   = lnRepLine + 1
  =FPUTS(lcFilHandl,STR(lnRepLine,2)+". File &lcTargDir&lcFileNam :")
ENDIF

DO CASE 
  CASE lnAction = 1
    IF TYPE ('laFileAStr[lnFieldNo]') <> 'U'
      lcCurrFld=laFileAStr[ASUBSCRIPT('laFileAStr',lnFieldNo,1),1]
      IF lcTmpFNm <> lcCurrFld
        lcTmpFNm = lcCurrFld
        =FPUTS(lcFilHandl,;
        "   * Field &lcCurrFld"+SPACE(10-LEN(lcCurrFld))+":")
      ENDIF 

      =FPUTS(lcFilHandl,;
        "       - Wrong "+laFieldTyp[ASUBSCRIPT('laFileStrc',lnFieldNo,2)]+;
        IIF(TYPE('laFileAStr[lnFieldNo]')='N',;
        STR(laFileAStr[lnFieldNo],2),ALLTRIM(laFileAStr[lnFieldNo]))+;
        " it should be "+;
        IIF(TYPE('laFileStrc[lnFieldNo]')='N',;
        STR(laFileStrc[lnFieldNo],2),ALLTRIM(laFileStrc[lnFieldNo])))
      ELSE
        lcCurrFld=laFileStrc[ASUBSCRIPT('laFileStrc',lnFieldNo,1),1] 

        IF lcTmpFNm <> lcCurrFld
          lcTmpFNm = lcCurrFld
          =FPUTS(lcFilHandl,;
          "   * Field &lcCurrFld"+SPACE(10-LEN(lcCurrFld))+":")
          =FPUTS(lcFilHandl,"       - Field dose not Exist.")
        ENDIF 

      ENDIF  

  CASE lnAction = 2
    =FPUTS(lcFilHandl,"   * File's information is not stored in the dictionary.")
  CASE lnAction = 3
    =FPUTS(lcFilHandl,"   * File dose not Exist.")
  CASE lnAction = 4
    =FPUTS(lcFilHandl,"   * File Mismatch structure. or have removed fields")
  *E301316,1 Add new case[Start] 
  CASE lnAction = 5
    =FPUTS(lcFilHandl,&lcFixMess)
  *E301316,1 Add new case[End..]     
ENDCASE    


*!*******************************************************************
*!
*!      Function: lfvAselect
*!
*!*******************************************************************
*
* Fill array with marked options from multyselect popup
*
FUNCTION lfvAselect
DECLARE laSelected [1,3]
laSelected = '' 
lnAryLen   = 0

FOR lnCount = 1 TO ALEN(laApp_fl,1)
  IF LEFT(laApp_fl[lnCount,1],1)=""
    lnAryLen = lnAryLen + 1 
    DECLARE laSelected [lnAryLen,3]
    laSelected [lnAryLen,1] = SUBSTR(laApp_fl[lnCount,1],2,9)
    laSelected [lnAryLen,2] = SUBSTR(laApp_fl[lnCount,1],13 )
    laSelected [lnAryLen,3] = laApp_fl[lnCount,3]
  ENDIF
ENDFOR

*!*******************************************************************
*!
*!      Function: lfImport
*!
*!*******************************************************************
*
FUNCTION lfImport
PRIVATE ALL LIKE l*

*** Let the user point to any file to import it's structure to the
*** dectionary or to reupdate it's information if it is olredy 
*** exist in the dectionary
lcPathFile= getfile("DBF","Select file to import it's structure ...")

*** If no selection return without doing anyting
IF EMPTY(lcPathFile)
  RETURN
ELSE
  *** Cutt off the path and extention
  lcFileName = SUBSTR(SUBSTR(lcPathFile,RAT('\',lcPathFile)+1),1,RAT('.',SUBSTR(lcPathFile,RAT('\',lcPathFile)+1))-1)
ENDIF

*E301077,78 Hesham (Start)
=gfOpenFile(gcSysHome+'SYDFILES','CFILE_NAM')
=gfOpenFile(gcSysHome+'SYDFLFLD','CFILE_NAM')
=gfOpenFile(gcSysHome+'SYDFIELD','CFLD_NAME')
=gfOpenFile(gcSysHome+'SYDINDEX','CFILE_NAM')

*IF !USED("SYDFILES")
*  SELECT 0
*  USE(gcSysHome+"SYDFILES")
*ELSE
*  SELECT SYDFILES 
*ENDIF
*SET ORDER TO TAG CFILE_NAM

*IF !USED("SYDFIELD")
*  SELECT 0
*  USE (gcSysHome+"SYDFIELD")
*ELSE
*  SELECT SYDFIELD 
*ENDIF
*SET ORDER TO TAG CFLD_NAME

*IF !USED("SYDFLFLD")
*  SELECT 0
*  USE (gcSysHome+"SYDFLFLD")
*ELSE
*  SELECT SYDFLFLD
*ENDIF
*SET ORDER TO TAG CFILE_NAM

*IF !USED("SYDINDEX")
*  SELECT 0
*  USE(gcSysHome+"SYDINDEX")
*ELSE
*  SELECT SYDINDEX
*ENDIF  
*SET ORDER TO TAG CFILE_NAM
*E301077,78 Hesham (End)


*** Open the file to check it's structure
IF !USED(lcFileName)
  SELECT 0 
  USE(lcPathFile) 
ELSE
  SELECT (lcFileName)
ENDIF  

*** Get the file structre
DECLARE laFileStrc[1,4]
DECLARE laTagesExp[1,2]
laFileStrc = " "
laTagesExp = " "
=AFIELDS('laFileStrc')

*** Collect all tages in an array
lnTagNo = 1
DO WHILE !EMPTY(TAG(lnTagNo))
  IF !EMPTY(laTagesExp[1,1])
    DECLARE laTagesExp[ALEN('laTagesExp',1)+1,2]
  ENDIF
  laTagesExp[lnTagNo,1] = TAG(lnTagNo)
  laTagesExp[lnTagNo,2] = SYS(14,lnTagNo)
  lnTagNo = lnTagNo + 1
ENDDO  


*** Check if the file exist or not and update it's data
SELECT SYDFILES
*** If the file is not in the dectionary add new record
IF !SEEK(lcFileName)
  APPEND BLANK
ENDIF

REPLACE cfile_nam  WITH  lcFileName                              ;
        nfld_no    WITH  ALEN('laFileStrc',1)                    ;
        nindx_no   WITH lnTagNo-1                                ;
        cfile_tag  WITH IIF(EMPTY(cfile_tag),laTagesExp[1,1],cfile_tag)   ;
        cfile_ttl  WITH IIF(EMPTY(cfile_ttl),lcFileName,cfile_ttl);
        cupgrdlvl  WITH IIF(EMPTY(cupgrdlvl),"A",cupgrdlvl)      ;
        cadd_user  WITH gcUser_ID                                ;
        cadd_time  WITH gfGetTime()                              ;
        dadd_date  WITH DATE()                                   ;
        llok_stat  WITH .F.



*** Update file field data and field dectionary data
SELECT SYDFLFLD

IF SEEK(lcFileName)
   REPLACE ALL cfile_nam WITH " " FOR cfile_nam = lcFileName
   DELETE  ALL FOR EMPTY(cfile_nam)
ENDIF

SET DELETE OFF
FOR lnFldNo = 1 TO ALEN('laFileStrc',1)

  IF SEEK(" ")  .AND. DELETED()
    RECALL
  ELSE
    APPEND BLANK
  ENDIF 

  *** Update the file field data
  REPLACE cfile_nam WITH lcFileName                           ; 
          cfld_name WITH laFileStrc[lnFldNo,1]                ; 
          nfld_pos  WITH lnFldNo                              ;
          cupgrdlvl WITH IIF(LEFT(lcFileName,2)="SY","S","A") ;
          cadd_user WITH gcUser_ID                            ;
          cadd_time WITH gfGetTime()                          ;
          dadd_date WITH DATE()                               ;
          llok_stat WITH .F. 
ENDFOR
SET DELETE ON

*** Update the field dictionaty with new information about each field
SELECT SYDFIELD
FOR lnFldNo = 1 TO ALEN('laFileStrc',1)
  IF !SEEK(laFileStrc[lnFldNo,1])
    APPEND BLANK 
  ENDIF

  REPLACE cfld_name WITH  laFileStrc[lnFldNo,1]   ;
          cdata_typ WITH  laFileStrc[lnFldNo,2]   ;
          nfld_wdth WITH  laFileStrc[lnFldNo,3]   ;
          nfld_dec  WITH  laFileStrc[lnFldNo,4]   ;
          cupgrdlvl WITH  IIF(EMPTY(cupgrdlvl),IIF(LEFT(lcFileName,2)="SY","S","A"),cupgrdlvl);
          cadd_user WITH  gcUser_ID               ;
          cadd_time WITH  gfGetTime()             ;
          dadd_date WITH  DATE()                  ;
          llok_stat WITH  .F.
 
ENDFOR


*** Update index name data
SELECT (lcFileName)
lcCdxName = FULLPATH(CDX(1))
lcCdxName = SUBSTR(lcCdxName,RAT('\',lcCdxName)+1)

SELECT SYDINDEX

FOR lnTagNo = 1 TO ALEN(laTagesExp,1)
 IF !SEEK(lcFileName+laTagesExp[lnTagNo,1]) 
   APPEND BLANK
 ENDIF

 REPLACE cindx_nam WITH  lcCdxName             ;
         cfile_nam WITH  lcFileName            ; 
         cfile_tag WITH  laTagesExp[lnTagNo,1] ;
         cindx_exp WITH  laTagesExp[lnTagNo,2] ;
         cupgrdlvl WITH   IIF(EMPTY(cupgrdlvl),IIF(LEFT(lcFileName,2)="SY","S","A"),cupgrdlvl);
         cadd_user WITH  gcUser_ID             ;
         cadd_time WITH  gfGetTime()           ;
         dadd_date WITH  DATE()                ;
         llok_stat WITH  .F.
ENDFOR 

*         lascend   WITH  
*         lunique   WITH  


USE IN (lcFileName)
USE IN SYDFILES
USE IN SYDFIELD
USE IN SYDINDEX
USE IN SYDFLFLD
*!**************************************************************************
*! Name      : lfvFix
*! Developer : Walid Abou El-Magd
*! Date      : 10/01/1999
*! Purpose   : 
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  :  None
*!**************************************************************************
*! Returns   : 
*!           : 
*!**************************************************************************
*! Example   :  
*!**************************************************************************
*! Notes     : 
*!**************************************************************************
*!E301316,1 
FUNCTION lfvFix

=lfvAselect()                 && Collect Selected files
IF !EMPTY(laSelected [1,1])   && Exit if None 
  *-- Save trapping setting .
  lcHldTab = ON('KEY','TAB')
  lcHldBtb = ON('KEY','BACKTAB')    
  lcHldEnt = ON('KEY','ENTER')
  ON KEY LABEL TAB    
  ON KEY LABEL BACKTAB
  ON KEY LABEL ENTER
  *-- Call  screen.
  IF TYPE('llExtCall')='U' 
    DO (gcScrDir+gcWinAppl+"\SMFIX.SPX")
  ELSE
    DO (gcScrDir+"SM\SMFIX.SPX")
  ENDIF
  *-- Restore trapping setting .
  ON KEY LABEL TAB     &lcHldTab
  ON KEY LABEL BACKTAB &lcHldBtb
  ON KEY LABEL ENTER   &lcHldEnt
ELSE
  =gfModalGen ('INM00018B00000','Dialog')  
ENDIF
*!**************************************************************************
*! Name      : lfvProceed
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : 
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  :  None
*!**************************************************************************
*! Returns   : 
*!**************************************************************************
*! Example   :  
*!**************************************************************************
*! Notes     : 
*!**************************************************************************
*!E301316,1
FUNCTION lfvProceed
*-- Start define the parameters from the screen
SHOW GET pbClose DISABLE
DIMENSION laFixInfo[10]
laFixInfo=" "
laFixInfo[1]=" "

*-- Set the what to do parameter according to its control variable lnFix 
DO CASE
  CASE lnFix=1
    laFixInfo[2]="A" &&-- Ask
  CASE lnFix=2 
    laFixInfo[2]="D" &&-- Don't Fix  
  CASE lnFix=3 
    laFixInfo[2]="F" &&-- AutoFix  
ENDCASE  

*-- Set the detail parameter according to its control variable lnLog 
DO CASE
  CASE lnLog=1
    laFixInfo[3]="T" &&-- Detail
  CASE lnLog=2 
    laFixInfo[3]="F" &&-- Summary  
ENDCASE  

*-- Set the backup parameter according to its control variable lnBackup 
DO CASE
  CASE lnBackup=1
    laFixInfo[4]="A" &&-- Ask
  CASE lnBackup=2
    laFixInfo[4]="T" &&-- Automatically make backup  
  CASE lnBackup=3
    laFixInfo[4]="F" &&-- Don't make backup  
ENDCASE  

*-- Set the last backup parameter according to its control variable lnLastBacp 
DO CASE
  CASE lnLastBacp=1
    laFixInfo[5]="A" &&-- Ask
  CASE lnLastBacp=2
    laFixInfo[5]="T" &&-- Auto-Overwrite  
  CASE lnLastBacp=3
    laFixInfo[5]="F" &&-- Make new  
ENDCASE  

=lfDoAction("CHECK")
SHOW GET pbClose ENABLE

*!**************************************************************************
*! Name      : lfFixDbf
*! Developer : Walid Abou El-Magd
*! Date      : 07/01/1999
*! Purpose   : Call and analysis the return values from gfFixDbf()
*!**************************************************************************
*! Calls     : None
*!**************************************************************************
*! Passed Parameters  :  None
*!**************************************************************************
*! Returns   : 
*!           : 
*!**************************************************************************
*! Example   :  
*!**************************************************************************
*! Notes     : 
*!**************************************************************************
*!E301316,1

FUNCTION lfFixDbf
=gfFixDbf(@laFixInfo)
lcFixMess=SPACE(0)
DO CASE
  CASE UPPER(laFixInfo[6])="NO PROBLEMS FOUND"
    RETURN
  CASE UPPER(laFixInfo[6])="FILE ACCESS DENIED"
    lcFixMess = "'   * File Access Denied'"
    =lfReport(5)
    RETURN        
  CASE UPPER(laFixInfo[6])="FILE NOT EXIST"
    lcFixMess = "'* File Not Exist'"
    =lfReport(5)
    RETURN          
  CASE UPPER(laFixInfo[6])="PROBLEMS FOUND"
    lcFixMess = "   * Problems Found"+'|'
    lcFixMess =lcFixMess+IIF(!EMPTY(laFixInfo[8]),'   * Backup :  '+laFixInfo[8]+'|',SPACE(0))+IIF(laFixInfo[3]="T",laFixInfo[9],SPACE(0))+"   * Current Status :"+laFixInfo[10]+'|'
    lcFixMess = PADR(lcFixMess,LEN(lcFixMess)-1)
    lcFixMess = STRTRAN(lcFixMess,'|','"+CHR(13)+"' )
    lcFixMess = '"'+lcFixMess+'"'
    =lfReport(5)
ENDCASE

PROCEDURE lpRnAsFix
PRIVATE lnI
puCompany = 1
cbSysOrMd = 2
FOR lnI=2 to ALEN(laCompany,1)
  puCompany=lnI
  *MAN Remove =lfvCompany() and add IF lfvCompany()   instead
  IF lfvCompany()
    =lfvTag()
    =lfDoAction("REORGANIZ")
  ENDIF  
ENDFOR
RETURN