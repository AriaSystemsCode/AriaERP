PARAMETERS lcAllParams, lcModules, lcActKey
ON ERROR DO lpErrHand WITH LINENO()

*lcSysDir = "c:\aria27\sysfiles\" 
*lcModules = "IC,MF,PS,SO,SP,HR.PW,SM" 
*lcActKey = "002913-2421828"

PRIVATE lcSysDir
IF TYPE("lcAllParams") != "C" OR EMPTY(lcAllParams)
  = CreateOutPutFile(.F.)
  RETURN
ENDIF

*-- User pass the first parameter only.
IF TYPE("lcModules") != "C" OR EMPTY(lcModules)
  *-- Analyize first parameter, and extract it to 3 parameters
  IF ATC(";",lcAllParams) = 0
    = CreateOutPutFile(.F.)
    RETURN
  ELSE
    lcAllParams = ";" + lcAllParams + ";"
    LOCAL lnParam, lcParam1, lcParam2, lcParam3, lcParam, lnStart, lnEnd
    FOR lnParam = 1 TO OCCURS(";",lcAllParams) - 1
      lnStart  = ATC(";",lcAllParams,lnParam)
      lnEnd    = ATC(";",lcAllParams,lnParam+1)
      lcParam  = "lcParam" + ALLTRIM(STR(lnParam))
      &lcParam. = SUBSTR(lcAllParams,lnStart+1,lnEnd - lnStart - 1)
    ENDFOR
    lcSysDir  = lcParam1
    lcModules = lcParam2
    lcActKey  = lcParam3
  ENDIF
ELSE
  = CreateOutPutFile(.F.)
  RETURN
ENDIF

llValid = TYPE("lcSysDir") = "C" AND !EMPTY(lcSysDir) AND;
          TYPE("lcActKey") = "C" AND !EMPTY(lcActKey) AND;
          OCCUR("-", lcActKey)=1
IF llValid
  llValid  = LEN(SUBSTR(lcActKey,ATC("-",lcActKey)+1))>=5 AND;
             LEN(RIGHT(lcActKey,ATC("-",lcActKey)-1))>=5
  IF llValid
    SET EXCL OFF
    SET FULLPATH ON
    SET SAFETY OFF
    SET CPDialog OFF
    SET DELETE ON
    
    lcSysDir  = ALLTRIM(lcSysDir)
    gcSysHome = lcSysDir+IIF(RIGHT(lcSysDir,1)="\", "", "\")
    lcRetMod  = ""
    lnUsers   = ""
    lcPForm   = ""
    llValid   = lfvAct_Key(lcActKey,@lcRetMod,@lnUsers,@lcPForm)
  ENDIF
ENDIF
= CreateOutPutFile(llValid)

* -----------------------------------------------------

FUNCTION CreateOutPutFile
LPARAMETERS llValid

lnFHand = FCREATE("R.R")
= FPUTS(lnFHand, IIF(llValid, "V", "N"))
= FCLOSE(lnFHand)

* -----------------------------------------------------

Function lfvAct_Key
PARAMETERS lcAct_Key,lcRetModules,lnNoUsers,lcPlatForm

DIMENSION laModulIns[1,2]
STORE '' TO laModulIns,lcInsModules,lcInsPlat,lcRetModules,lcPlatForm
STORE .F. TO llInstUsd,llApplUsd

*-- YMA
IF FILE(gcSysHome+'sydappl.dbf') 
*IF !USED('sydappl')
*-- YMA
  SELECT 0
  USE (gcSysHome+'sydappl')
  llApplUsd = .T.
ENDIF
lcAct_Key = ALLTRIM(lcAct_Key)
lcKey     = SUBSTR(lcAct_Key,2)
lcKey     = STUFF(lcKey,LEN(lcKey),1,'')
lcKey     = STRTRAN(lcKey,'-')
lcKey     = SUBSTR(lcKey,1,LEN(lcKey)-1)
llValdKey = .F.

*-- YMA
IF llApplUsd
*-- YMA
  SELECT cApp_ID,SYS(2007,cApp_ID);
    FROM (gcSysHome+'sydappl');
    INTO ARRAY laModulIns;
   ORDER BY 2;
   WHERE CAPP_ID <> 'SY'
*-- YMA
ENDIF
*-- YMA

*-- YMA
DO WHILE !EMPTY(lcModules)
  lcOneModule  = SUBSTR(lcModules, 1, 3)
  lcModules    = STRTRAN(lcModules, lcOneModule, "")
  lcOneModule  = STRTRAN(lcOneModule, ",", "")
  
  IF ASCAN(laModulIns, lcOneModule) = 0
    lnALen       = ALEN(laModulIns,1)+1
    IF !(lnALen = 1 AND EMPTY(laModulIns[1,1]))
      DIMENSION laModulIns[lnALen,2]
    ENDIF
    laModulIns[lnALen,1] = lcOneModule
    laModulIns[lnALen,2] = SYS(2007, lcOneModule)
  ENDIF
ENDDO    
ASORT(laModulIns,2,ALEN(laModulIns,1),0)
*-- YMA
 
FOR lnCount = 1 TO ALEN(laModulIns,1)  
  lcInsModules = lcInsModules+laModulIns[lnCount,1]
  lcRetModules = lcRetModules+IIF(lnCount=1,'',',')+laModulIns[lnCount,1]
ENDFOR
lcPlatForm = RIGHT(lcAct_Key,1)
lnPlatForm = IIF(ASC(lcPlatForm)>=65,ASC(lcPlatForm)-55,VAL(lcPlatForm))
lcBinary   = ''
lnPrimary  = lnPlatForm
lcPlatForm = ''
DO WHILE lnPrimary > 0
  lcBinary   = STR(MOD(lnPrimary,2),1)+lcBinary
  lcPlatForm = lcPlatForm+IIF(MOD(lnPrimary,2)=1,SUBSTR('DMUW',LEN(lcBinary),1),'')
  lnPrimary  = INT(lnPrimary/2)
ENDDO
lcInsPlat    = lcPlatForm
lcNoUsers    = SUBSTR(lcAct_Key,1,1)+SUBSTR(lcAct_Key,LEN(lcAct_Key)-1,1)
lnLenAct     = CEILING((LEN(STRTRAN(lcKey,'-')))/2)
lcHidChar    = ''
lcOldAct_Key = ''
FOR lnCount = 1 TO lnLenAct
  *-- YMA
  IF ((lnCount-1)*2)+1 <= LEN(lcKey)
  *-- YMA
    lcHidChar    = lcHidChar    + SUBSTR(lcKey,((lnCount-1)*2)+1,1)
  *-- YMA
  ENDIF
  IF ((lnCount-1)*2)+2 <= LEN(lcKey)
  *-- YMA
    lcOldAct_Key = lcOldAct_Key + SUBSTR(lcKey,((lnCount-1)*2)+2,1)
  *-- YMA
  ENDIF
  *-- YMA
ENDFOR
FOR lnCount = VAL(lcNoUsers)*10 TO (VAL(lcNoUsers)*10)+9
  FOR lnTrilVer = 1 TO 2
    llTrilVer   = lnTrilVer = 2
    lcKeyConted = PADR(ALLTRIM(SYS(2007,PADR(lcInsModules+lcInsPlat+IIF(llTrilVer,'T','')+STR(lnCount),80))),LEN(lcHidChar))
    lcKeyAct    = ''
    FOR lnHidLen = 1 TO LEN(lcHidChar)
      lcKeyAct = lcKeyAct+SUBSTR(lcHidChar,lnHidLen,1) + SUBSTR(lcKeyConted,lnHidLen,1)
    ENDFOR
    lcKeyAct = LEFT(lcNoUsers,1)+lcKeyAct+RIGHT(lcNoUsers,1)
    IF lcKeyConted == lcOldAct_Key
      llValdKey = .T.
      EXIT
    ENDIF
  ENDFOR
  IF llValdKey = .T.
    EXIT
  ENDIF
ENDFOR 

IF !llValdKey 
  lcRetModules = ''
  lcPlatForm   = ''
  lnNoUsers    = 0
  llAllDone    = .F. 
ELSE
  lnNoUsers    = lnCount
  llAllDone    = .T.     
  lcPlatForm   = lcPlatForm + IIF(llTrilVer,'T','')
ENDIF

IF USED('SYCINST') AND llInstUsd 
  USE IN SYCINST
ENDIF
IF USED('SYDAPPL') AND llApplUsd
  USE IN SYDAPPL
ENDIF

RETURN llAllDone

* -----------------------------------------------------

PROCEDURE lpErrHand
PARAMETERS lnLine

ON ERROR
lcErrNum  = ALLTRIM(STR(ERROR()))
lcLine    = ALLTRIM(STR(lnLine))
lcMessage = MESSAGE()

MessageBox("Error Number  : " + lcErrNum  + CHR(13) +;
           "Line Number   : " + lcLine    + CHR(13) +;
           "Error Message : " + lcMessage + CHR(13) ,;
           16, "Setup Error")

ON ERROR DO lpErrHand WITH LINENO()

* -----------------------------------------------------














Function olfvAct_Key
PARAMETERS lcAct_Key,lcRetModules,lnNoUsers,lcPlatForm
DIMENSION laModulIns[1,2]
STORE '' TO laModulIns,lcInsModules,lcInsPlat,lcRetModules,lcPlatForm
STORE .F. TO llInstUsd,llApplUsd
*IF !USED('SYCINST')
*  SELECT 0
*  USE (gcSysHome+'SYCINST')
*  llInstUsd = .T.
*ENDIF
*GO TOP IN SYCINST
*B600506,1 take care if the SYDAPPL file was opend by the function
*B600506,1 IF yes then at the end of the function close the file
IF FILE(gcSysHome+'sydappl.dbf') 
*IF !USED('sydappl')
  SELECT 0
  USE (gcSysHome+'sydappl')
  llApplUsd= .T.
ENDIF
lcAct_Key = ALLTRIM(lcAct_Key)
lcKey=SUBSTR(lcAct_Key,2)
lcKey=STUFF(lcKey,LEN(lcKey),1,'')
lcKey=STRTRAN(lcKey,'-')
lcKey=SUBSTR(lcKey,1,LEN(lcKey)-1)
llValdKey = .F.

IF llApplUsd
  SELECT cApp_ID,SYS(2007,cApp_ID);
    FROM (gcSysHome+'sydappl');
    INTO ARRAY laModulIns;
    ORDER BY 2;
    WHERE CAPP_ID<>'SY'
ENDIF

DO WHILE !EMPTY(lcModules)
  lcOneModule  = SUBSTR(lcModules, 1, 3)
  lcModules    = STRTRAN(lcModules, lcOneModule, "")
  lcOneModule  = STRTRAN(lcOneModule, ",", "")
  
  IF ASCAN(laModulIns, lcOneModule) = 0
    lnALen       = ALEN(laModulIns,1)+1
    DIMENSION laModulIns[lnALen,2]
    laModulIns[lnALen,1] = lcOneModule
    laModulIns[lnALen,2] = SYS(2007, lcOneModule)
  ENDIF
ENDDO    
ASORT(laModulIns,2,ALEN(laModulIns,1),0)

FOR lnCount = 1 TO ALEN(laModulIns,1)  
  lcInsModules = lcInsModules+laModulIns[lnCount,1]
  lcRetModules = lcRetModules+IIF(lnCount=1,'',',')+laModulIns[lnCount,1]
ENDFOR
*lcInsPlat = lcInsPlat+IIF(SYCINST.lInsUsDos,'D','') 
*lcInsPlat = lcInsPlat+IIF(SYCINST.lInsUsMac,'M','') 
*lcInsPlat = lcInsPlat+IIF(SYCINST.lInsUsUnx,'U','') 
*lcInsPlat = lcInsPlat+IIF(SYCINST.lInsUsWin,'W','') 
lcPlatForm=RIGHT(lcAct_Key,1)
lnPlatForm =IIF(ASC(lcPlatForm)>=65,ASC(lcPlatForm)-55,VAL(lcPlatForm))
lcBinary=''
lnPrimary=lnPlatForm
lcPlatForm=''
DO WHILE lnPrimary>0
  lcBinary=STR(MOD(lnPrimary,2),1)+lcBinary
  lcPlatForm=lcPlatForm+IIF(MOD(lnPrimary,2)=1,SUBSTR('DMUW',LEN(lcBinary),1),'')
  lnPrimary=INT(lnPrimary/2)
ENDDO
lcInsPlat=lcPlatForm
*lcAct_Key=SUBSTR(lcAct_Key,1,LEN(lcAct_Key)-1)
*lcNoUsers = SUBSTR(lcAct_Key,1,1)+RIGHT(lcAct_Key,1)
lcNoUsers = SUBSTR(lcAct_Key,1,1)+SUBSTR(lcAct_Key,LEN(lcAct_Key)-1,1)
*B601500,1 KHM 12/26/96 (Begin) Adding the ceiling function to get
*B601500,1 KHM           the nearest greater number 
*lnLenAct=(LEN(STRTRAN(lcKey,'-')))/2
lnLenAct=CEILING((LEN(STRTRAN(lcKey,'-')))/2)
*B601500,1 KHM (End)
lcHidChar =''
lcOldAct_Key = ''
FOR lnCount = 1 TO lnLenAct
  lcHidChar    = lcHidChar+SUBSTR(lcKey,((lnCount-1)*2)+1,1)
  lcOldAct_Key = lcOldAct_Key+SUBSTR(lcKey,((lnCount-1)*2)+2,1)
ENDFOR
FOR lnCount = VAL(lcNoUsers)*10 TO (VAL(lcNoUsers)*10)+9
  *E300579,1 Hesham El-Sheltawi (Start)
  *E300579,1 Check if the Activation Key contain that
  *E300579,1 the runing version is a Demo Version
  FOR lnTrilVer = 1 TO 2
    llTrilVer = lnTrilVer = 2
  *E300579,1 Hesham El-Sheltawi (End)      

  *E300579,1 Hesham El-Sheltawi (Start)
  *E300579,1 Check if the Activation Key contain that
  *E300579,1 the runing version is a Demo Version
*    lcKeyConted = SYS(2007,PADR(lcInsModules+lcInsPlat+STR(lnCount),80))
    lcKeyConted = SYS(2007,PADR(lcInsModules+lcInsPlat+IIF(llTrilVer,'T','')+STR(lnCount),80))
  *E300579,1 Hesham El-Sheltawi (End)        

  lcKeyAct=''
  FOR lnHidLen = 1 TO LEN(lcHidChar)
    lcKeyAct = lcKeyAct+SUBSTR(lcHidChar,lnHidLen,1)+ SUBSTR(lcKeyConted,lnHidLen,1)
  ENDFOR
  lcKeyAct=LEFT(lcNoUsers,1)+lcKeyAct+RIGHT(lcNoUsers,1)
  *B600506,1 check for the validation key with ignoring the inserted
  *B600506,1 random numbers
  *B601500,1 KHM 12/26/96 (Begin) Check if its the valid key or not
  *IF lcKeyConted=lcOldAct_Key
  IF lcKeyConted==lcOldAct_Key
  *B601500,1 KHM (End)
    llValdKey = .T.
    EXIT
  ENDIF
*  IF lcKeyAct=STRTRAN(lcAct_Key,'-')
*    llValdKey = .T.
*    EXIT
*  ENDIF         
  *E300579,1 Hesham El-Sheltawi (Start)
  *E300579,1 Check if the Activation Key contain that
  *E300579,1 the runing version is a Demo Version
  ENDFOR
  IF llValdKey = .T.
    EXIT
  ENDIF
  *E300579,1 Hesham El-Sheltawi (End)  

ENDFOR 

*E300581,1 Hesham El-Sheltawi (Start)
*E300581,1 To be removed Latter So that the activation key Work
*  lnNoUsers = lnCount
*  llAllDone  = .T.     
* return .t.
*E300581,1 Hesham El-Sheltawi (End)
IF !llValdKey 
  lcRetModules = ''
  lcPlatForm=''
  lnNoUsers = 0
  llAllDone  = .F. 
ELSE
  lnNoUsers = lnCount
  llAllDone  = .T.     
  *E300579,1 Hesham El-Sheltawi (Start)
  *E300579,1 Check if the Activation Key contain that
  *E300579,1 the runing version is a Demo Version
  lcPlatForm = lcPlatForm + IIF(llTrilVer,'T','')
  *E300579,1 Hesham El-Sheltawi (End)  
ENDIF
*B600506,1 take care if the SYDAPPL file was opend by the function
*B600506,1 IF yes then at the end of the function close the file
IF USED('SYCINST') AND llInstUsd 
  USE IN SYCINST
ENDIF
IF USED('SYDAPPL') AND llApplUsd
  USE IN SYDAPPL
ENDIF
RETURN llAllDone
