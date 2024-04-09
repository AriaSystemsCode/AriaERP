*:*********************************************************************************
*: Program file  : SMSETREP.PRG
*: Program desc. : Print Company Setup Report
*:        System : Aria4 XP.
*:        Module : SM (System Manager).
*:     Developer : Saber A Razek (SAB)
*:     Entry     : E303336
*:*********************************************************************************

lcStTime   = TIME()                     && Variable to hold the start Time.

IF USED('SYDFIELD_A')
  USE IN SYDFIELD_A  
ENDIF
=gfOpenFile(oAriaApplication.SysPath+'SYDFIELD','CFLD_NAME', 'SH', 'SYDFIELD_A')  && CFLD_NAME

*- if new selection.
IF llClearFn OR !(lcRpCmpExp == lcLstCmpEx) OR !(lcRpModExp == lcLstModEx)

  llClearFn  = .F.
  lcLastTag  = ''     && to fill index field with its corresponding data.

  lcLstCmpEx = lcRpCmpExp
  lcLstModEx = lcRpModExp

  *-- if you have previous data clear workfile then recreate it. [begin]
  IF !USED(lcWorkFile) OR (RECCOUNT(lcWorkFile) > 0)
    IF USED(lcWorkFile)
      USE IN (lcWorkFile)
    ENDIF  
    =lfWorkFile()
  ENDIF
  *-- if you have previous data clear workfile then recreate it. [end]

  *-- Fill temp. file with data [Begin]
  *-- lnComp     : Counter to companies.
  *-- lnMod      : Counter to modules. 
  *-- lnInSource : Position of item in source array.
  *-- lcCmpArray : Company array used in loop (Source or Target)
  *-- lcModArray : Module array used in loop (Source or Target)
  *-- lcCmpDir   : Company directory.
  *-- lcKeyCode  : Moule code.
  *-- llOpnSetup : Flag .T. if we open setups file.
  *-- lcSetupAls : Hold Temp. Name of alias for setups file.
  *-- lcPGSetAls : Hold Temp. Name of alias for (APSETUP or GLSETUP ) file.
  PRIVATE lnComp,lnMod,lnInSource,lcCmpArray,lcModArray,lcCmpDir,lcKeyCode,;
          llOpnSetup,lcSetupAls,lcPGSetAls

  lcSetupAls = gfTempName()
  lcPGSetAls = gfTempName()

  lcCmpArray = IIF(EMPTY(laRpTarCmp),'laRpSorCmp','laRpTarCmp')
  
  *- Loop company array.
  FOR lnComp = 1 TO ALEN(&lcCmpArray,1)

    lnInSource  = IIF(EMPTY(laRpTarCmp),lnComp,ASCAN(laRpSorCmp,"\"+laRpTarCmp[lnComp],1))
    IF lnInSource = 0
      lnInSource  = ASCAN(laRpSorCmp,laRpTarCmp[lnComp],1)
    ENDIF
    
    *-- Company dependent Memory variables. [Begin]
    m.cComp_Id  = PADR(laRpCmpCod[lnInSource,1],2)
    m.cCom_Name = ALLTRIM(SUBSTR(laRpCmpCod[lnInSource,1],6))
    *-- Company dependent Memory variables. [End]
    lcCmpDir    = LOWER(ALLTRIM(laRpCmpCod[lnInSource,2]))

    IF USED(lcSetupAls)
      USE IN (lcSetupAls)
    ENDIF
    
    llOpnSetup = .F.
    lcModArray = IIF(EMPTY(laRpTarMod),'laRpSorMod','laRpTarMod')
    
    *-- Loop Module array.
    FOR lnMod = 1 TO ALEN(&lcModArray,1)
  
      lnInSource  = IIF(EMPTY(laRpTarMod),lnMod,ASCAN(laRpSorMod,"\"+laRpTarMod[lnMod],1))
      IF lnInSource = 0
        lnInSource = ASCAN(laRpSorMod,laRpTarMod[lnMod],1)      
      ENDIF

      *-- Module dependent Memory variables. [Begin]
      STORE laRpModCod[lnInSource,1] TO lcKeyCode,m.cApp_Id
      m.cApp_Name = ALLTRIM(laRpModCod[lnInSource,2])
      *-- Module dependent Memory variables. [End]

      *- If codes come from setups file.
      IF laRpModCod[lnInSource,3] = 'F'
        IF EMPTY(m.cApp_Id) 
          lcKeyCode = 'ÿÿ'    && Code for main system.
        ENDIF
    
        *- If you do not open this file before.
        IF !llOpnSetup
          *-- if you find setups file
          IF FILE(lcCmpDir+'SETUPS.DBF')
            llOpnSetup = .T.
            USE (lcCmpDir+'SETUPS') ORDER TAG Nvarpos AGAIN ALIAS (lcSetupAls) IN 0 SHARED          
          ELSE  && you did not find file due to data error.          
            EXIT
          ENDIF  
        ENDIF    && end if you do not open this file before.

        *-- if This module has been setuped.
        IF SEEK(lcKeyCode,lcSetupAls)
          
          SELECT (lcSetupAls)
          SCAN REST WHILE capp_id+cfld_name = lcKeyCode
          
            m.cFld_Head = cFld_Head
            IF !EMPTY(ALLTRIM(mVEntries)) AND !EMPTY(mData_Def)

              DIMENSION laEntDesc[1],laEntVal[1]
              lcLeftStr  = SUBSTR(ALLTRIM(mVEntries),1,ATC('~',ALLTRIM(mVEntries))-1)
              lcRightStr = SUBSTR(ALLTRIM(mVEntries),ATC('~',ALLTRIM(mVEntries))+1)

              =gfSubStr(lcLeftStr , @laEntDesc ,'|')
              =gfSubStr(lcRightStr, @laEntVal  ,'|')

              lnUserSet   = ASCAN(laEntVal,ALLTRIM(mData_Def),1)
              IF lnUserSet > 0  
                m.cSetting  = laEntDesc[lnUserSet]
              ELSE
                m.cSetting  = ALLTRIM(mData_Def)  
              ENDIF
            ELSE
              m.cSetting  = ALLTRIM(mData_Def)  
            ENDIF

            IF ('L' $ cData_Typ) AND ('.' $ m.cSetting)
              IF EVALUATE(m.cSetting)
                m.cSetting = 'Yes' 
              ELSE
                m.cSetting = 'No' 
              ENDIF
            ENDIF
            INSERT INTO (lcWorkFile) FROM MEMVAR

          ENDSCAN
          
        ELSE  && else no setups has been done for this module ..!          
          m.cFld_Head = [No setups has been done for this module ..!]
          m.cSetting = ''
    
          INSERT INTO (lcWorkFile) FROM MEMVAR    
        ENDIF  && end if This module has been setuped.
  
      ELSE   && else Code is either 'AP' for AP module or 'GL' for GL module.
        
        PRIVATE lnCurrAls      && save current work alias.
        lnCurrAls = SELECT(0)

        IF FILE(lcCmpDir+ lcKeyCode+'SETUP.DBF')        
          USE (lcCmpDir+lcKeyCode+'SETUP') AGAIN ALIAS (lcPGSetAls) IN 0 SHARED
          SELECT (lcPGSetAls)
          GO TOP        
        ELSE          
          m.cFld_Head = [No setups has been done for this module ..!]
          m.cSetting = ''
          INSERT INTO (lcWorkFile) FROM MEMVAR
          EXIT        
        ENDIF
        
        SET ORDER TO Cfld_name IN SYDFIELD_A
        
        *- Loop setups record to get its fields.
        FOR lnI = 1 TO FCOUNT() - 8

          m.cFld_Head = IIF(SEEK(ALLTRIM(FIELD(lnI)),'SYDFIELD_A'),SYDFIELD_A.cFld_Head,'')
  
          DO CASE  
            CASE TYPE(FIELD(lnI)) = 'L'
              m.cSetting = IIF(EVALUATE(FIELD(lnI)),'Yes','No')

            CASE TYPE(FIELD(lnI)) = 'C'
              m.cSetting = EVALUATE(FIELD(lnI))

            CASE TYPE(FIELD(lnI)) = 'N'
              m.cSetting = ALLTRIM(STR(EVALUATE(FIELD(lnI))))            
          ENDCASE
          
          INSERT INTO (lcWorkFile) FROM MEMVAR

        ENDFOR    && end loop setups record to get its fields.

        USE IN (lcPGSetAls)
        SELECT (lnCurrAls)    && restore previous work alias.
       
      ENDIF  && end if codes come from setups file.
    
    ENDFOR   && end loop Module array.

  ENDFOR     && end loop company array.
  *-- Fill temp. file with data [End]

ENDIF        && end if new selection.

*-- no records was selected.
IF RECCOUNT(lcWorkFile) = 0
  *-- Message : There are no records to display...!
  *--                < Ok > 
  =gfModalGen('TRM00052B40011','ALERT')
  RETURN
ENDIF

SELECT (lcWorkFile)
*-- ReIndex work file if first time collect data or user change sort By.
IF !(lcRpIndTag == lcLastTag)
  lcLastTag = lcRpIndTag
  REPLACE ALL cTempKey WITH EVALUATE(lcRpIndTag)
ENDIF

lcEdTime   = TIME()
lnInterval = lfCollTime(lcStTime,lcEdTime)
WAIT WINDOW 'Selected ' + ALLTRIM(STR(RECCOUNT(lcWorkFile))) + ' Record(s) in ' + ALLTRIM(STR(lnInterval,6,2)) + ' Second'+ IIF(lnInterval>1,'s...','...')  NOWAIT

DO gfDispRe WITH EVAL('lcRpForm')
*- End of report code...


*!*************************************************************
*! Name      : lfwRepWhen
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : The when function of the option grid
*!*************************************************************
FUNCTION lfwRepWhen

*- If your data does have values.
IF EMPTY(laRpCmpCod)

  SET ORDER TO Ccomp_id IN SYCCOMP
  SET ORDER TO Capp_id  IN SYDAPPL
  SET ORDER TO Modvar   IN SYCCONFG

  DECLARE laRpCmpCod[1,2],laRpModCod[1,3]
  STORE '' TO lcRpCmpExp,lcRpModExp

  *-- Fill Company and Modules arrays [Begin]

  *-- Collect all companies
  SELECT ccomp_id+" - "+cCom_Name,cCom_dDir ;
    FROM SYCCOMP                            ;
    INTO ARRAY laRpCmpCod                   ;
    ORDER BY 1
  
  DECLARE laRpSorCmp[ALEN(laRpCmpCod,1),1],laRpTarCmp[ALEN(laRpCmpCod,1),1]
  FOR lnI = 1 TO ALEN(laRpCmpCod,1)
    STORE laRpCmpCod[lnI,1] TO laRpSorCmp[lnI,1],laRpTarCmp[lnI,1]
  ENDFOR

  =lfCmpExpr()

  SELECT DISTINCT Sycconfg.capp_id, Sydappl.capp_name, 'F' AS cCond;
    FROM Sycconfg, Sydappl;
    WHERE Sydappl.capp_id = Sycconfg.capp_id;
    ORDER BY Sycconfg.capp_id, Sydappl.capp_name ;
    INTO ARRAY laRpModCod
  
  *-- Add hard coded rows to array.   [begin]
  lnHardCode = 3
  DIMENSION laHardCode[lnHardCode,3]
  laHardCode[1,1] = 'AP'
  laHardCode[1,2] = 'Accounts Payable'
  laHardCode[1,3] = 'H'

  laHardCode[2,1] = 'GL'
  laHardCode[2,2] = 'General Ledger'
  laHardCode[2,3] = 'H'
  
  laHardCode[3,1] = '  '
  laHardCode[3,2] = 'Main System'
  laHardCode[3,3] = 'F'
  
  DIMENSION laRpModCod[ALEN(laRpModCod,1)+lnHardCode,ALEN(laRpModCod,2)]
  lnJ = 1
  FOR lnI = ALEN(laRpModCod,1) - (lnHardCode-1) TO ALEN(laRpModCod,1)
    laRpModCod[lnI,1] = laHardCode[lnJ,1]
    laRpModCod[lnI,2] = laHardCode[lnJ,2]
    laRpModCod[lnI,3] = laHardCode[lnJ,3]
    lnJ = lnJ + 1
  ENDFOR
  =ASORT(laRpModCod)
  *- Add hard coded rows to array.   [end  ]
  
  DECLARE laRpSorMod[ALEN(laRpModCod,1),1],laRpTarMod[ALEN(laRpModCod,1),1]
  FOR lnI = 1 TO ALEN(laRpModCod,1)
    STORE laRpModCod[lnI,2] TO laRpSorMod[lnI,1],laRpTarMod[lnI,1]
  ENDFOR

  =lfModExpr()

  *-- Fill Company and Modules arrays [End]
 
  =lfWorkFile()   && Create temporary cursor.

ENDIF
*- End of lfwRepWhen.


*!*************************************************************
*! Name      : lfWorkFile
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Create work cursor.
*!*************************************************************
FUNCTION lfWorkFile

CREATE CURSOR (lcWorkFile) (cTempKey C(4) , cComp_Id C(2),cCom_Name C(30),cApp_Id C(2),;
                            cApp_Name C(60),cFld_Head C(60) , cSetting C(60))
                            
SELECT (lcWorkFile)
INDEX ON cTempKey TAG (lcWorkFile)

ENDFUNC
*- End of lfWorkFile.


*!*************************************************************
*! Name      : lfvCompany
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Call mover function then Evaluate Company expression 
*!*************************************************************
FUNCTION lfvCompany

= gfMover(@laRpSorCmp,@laRpTarCmp,'Select Company',.T.,'')  && call mover function.
= lfCmpExpr()

ENDFUNC
*- End of lfvCompany.


*!*************************************************************
*! Name      : lfCmpExpr
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Evaluate Company expression.
*!*************************************************************
FUNCTION lfCmpExpr

PRIVATE laTarget

IF EMPTY(laRpTarCmp)
  = ACOPY(laRpSorCmp,laTarget)
ELSE
  = ACOPY(laRpTarCmp,laTarget)
ENDIF
  
= ASORT(laTarget)
lcRpCmpExp = ''

FOR lnI = 1 TO ALEN(laTarget,1)
  lcRpCmpExp = IIF(EMPTY(lcRpCmpExp),PADR(laTarget[lnI],2),;
                    lcRpCmpExp + ','+PADR(laTarget[lnI],2))
ENDFOR

ENDFUNC
*- End of lfCmpExpr.


*!*************************************************************
*! Name      : lfvModule
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Call mover function then Evaluate Module expression 
*!*************************************************************
FUNCTION lfvModule

= gfMover(@laRpSorMod,@laRpTarMod,'Select Company',.T.,'')  && call mover function.
= lfModExpr()

ENDFUNC
*- End of lfvModule.


*!*************************************************************
*! Name      : lfModExpr
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Evaluate Module expression.
*!*************************************************************
FUNCTION lfModExpr

PRIVATE laTarget
IF EMPTY(laRpTarMod)
  = ACOPY(laRpSorMod,laTarget)
ELSE
  = ACOPY(laRpTarMod,laTarget)
ENDIF  

= ASORT(laTarget)
lcRpModExp = ''
FOR lnI = 1 TO ALEN(laTarget,1)
  lnInSource = ASCAN(laRpSorMod,laTarget[lnI],1)
  lcRpModExp = IIF(EMPTY(lcRpModExp),PADR(laTarget[lnI],2), lcRpModExp + ','+PADR(laTarget[lnI],2))  
ENDFOR

ENDFUNC
*- End of lfModExpr.


*!*************************************************************
*! Name      : lfvSortBy
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Evaluate Groups, Index , and Groups header expressions.
*!*************************************************************
FUNCTION lfvSortBy

IF lcRpSortBy = 'C'
  lcRpIndTag = [CCOMP_ID+CAPP_ID]
  
  lcRpGroup1 = [CCOMP_ID]
  lcRpGroup2 = [CAPP_ID]
  
  lcRpGrpHd1 = ['Company : ' +  cCom_Name]
  lcRpGrpHd2 = ['Module : ' + cApp_Name]
  
ELSE
  lcRpIndTag = [CAPP_ID+CCOMP_ID]

  lcRpGroup1 = [CAPP_ID]
  lcRpGroup2 = [CCOMP_ID]

  lcRpGrpHd1 = ['Module : ' + cApp_Name]
  lcRpGrpHd2 = ['Company : ' +  cCom_Name]

ENDIF

ENDFUNC
*- End of lfvSortBy.


*!*************************************************************
*! Name      : lfCollTime
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Calcualte spent time in data collection.
*!*************************************************************
FUNCTION lfCollTime

PARAMETERS lcStart,lcEnd
lnStHour  = IIF(VAL(LEFT(lcStart,2)) = 0,VAL(LEFT(lcStart,2))+24,VAL(LEFT(lcStart,2)))
lnEndHour = IIF(VAL(LEFT(lcEnd,2))   = 0,VAL(LEFT(lcEnd,2))  +24,VAL(LEFT(lcEnd,2)))
lnStart = 3600 * lnStHour  + 60 * VAL(SUBSTR(lcStart,4,2)) + VAL(RIGHT(lcStart,2))
lnEnd   = 3600 * lnEndHour + 60 * VAL(SUBSTR(lcEnd,4,2))   + VAL(RIGHT(lcEnd,2))
RETURN (lnEnd - lnStart)

ENDFUNC
*- End of lfCollTime.


*!*************************************************************
*! Name      : lfPreGrd
*! Developer : Saber A Rzek (SAB)
*! Date      : 12/30/2012
*! Purpose   : Function called to determine if we continue showing OG or not.
*!*************************************************************
FUNCTION lfPreGrd

USE (gcSyshome+"syccomp") AGAIN ALIAS XSYS_COMP IN 0
GO TOP IN XSYS_COMP
IF EOF('XSYS_COMP')
  *-- Message : 'There are no companies installed in system'
  *--                           <Ok>
  =gfModalGen('INM00339B00000','DIALOG')
  llDoOpGrid = .F.
ENDIF
USE IN XSYS_COMP

USE (gcSyshome+"SYCCONFG") AGAIN ALIAS XSYS_COFIG IN 0
GO TOP IN XSYS_COFIG
IF EOF('XSYS_COFIG')
  *-- Message : 'Setup has not been done for any module in system'
  *--                           <Ok>
  =gfModalGen('INM00340B00000','DIALOG')
  llDoOpGrid = .F.
ENDIF
USE IN XSYS_COFIG
RETURN

ENDFUNC
*- End of lfPrOpGd.
