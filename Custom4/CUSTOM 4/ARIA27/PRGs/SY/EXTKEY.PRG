PARAMETERS lcDbfName,lcFrstField,lcFrstRet,lcOldVal,;
           lcSecField,lcSecRet,lcBrowsFld,lcOrdName,;
           lcKeyToUse,llCanFilter
PRIVATE lcCurDbf,lcFrstType,lcSecType,lcTmpBrFld,lnCurRec,lcWinTitle,;
        lcFilter

lcCurDbf=SELECT()
lnCurRec=IIF(RECNO()>RECCOUNT(),0,RECNO())

llMyUse=.F.           
****** Validate the database to use name and the first field name           
IF EMPTY(lcDbfName) OR EMPTY(lcFrstField) ;
  OR TYPE('lcDbfName')<>'C' OR TYPE('lcFrstField')<>'C'
    &lcFrstRet = lcOldVal
    RETURN
ENDIF  
********************************************************

**** Validate the variable name to return value in it
IF TYPE('lcFrstRet')<>'C' OR EVAL(lcFrstRet)='U'
  &lcFrstRet = lcOldVal
  RETURN
ENDIF
********************************************************
lcFrstType = lfGetType(lcFrstRet)
lcSecType  =IIF(TYPE('lcSecRet')='C',lfGetType(lcSecRet),'U')
lcTmpBrFld=lcBrFields
IF !USED('SYDFILES')
  llMyUse=.T.
  SELECT SELECT(1)
  USE &gcSysHome.SYDFILES ORDER 1
ELSE
  SELECT SYDFILES
  lcOldOrd=ORDER()
  lcFilter=FILTER()
  lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
  SET ORDER TO IIF(EMPTY(lcOrdName),1,&lcOrdName)
  SET FILTER TO
ENDIF  
lcWinTitle=lcDbfName
IF SEEK(IIF(ATC('.',lcDbfName)>0,ALLTRIM(lcDbfName),ALLTRIM(lcDbfName)+'.DBF'))
  lcWinTitle=IIF(!EMPTY(cfile_ttl),cFile_ttl,lcWinTitle)
ENDIF  

IF llMyUse
  USE IN SYDFILES
ELSE
  SELECT SYDFILES  
  IF !EMPTY(lcOldOrd)
    SET ORDER TO &lcOldOrd
  ELSE
    SET ORDER TO
  ENDIF
  IF !EMPTY(lcFilter)
    SET FILTER TO &lcFilter
  ELSE
    SET FILTER TO
  ENDIF
  IF lnRecNo<>0
    GO lnRecNo
  ENDIF
ENDIF

llMyUse=.F.
IF !USED(lcDbfName)
  llMyUse=.T.
  lcPath=IIF(SUBSTR(UPPER(lcDbfName),1,2)='SY',gcSysHome,gcData_Dir)  
  SELECT SELECT(1)
  USE &lcPath.&lcDbfName ORDER IIF(EMPTY(lcOrdName),1,&lcOrdName)
ELSE
  SELECT(lcDbfName)
  lcOldOrd=ORDER()
  lcFilter=FILTER()
  lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())
  SET ORDER TO IIF(EMPTY(lcOrdName),1,&lcOrdName)
  SET FILTER TO
ENDIF
DECLARE laFieldBr[IIF(lcSecType='U',1,2)]
lcScatField=lcFrstField+IIF(EMPTY(lcSecField),'',','+lcSecField)
lcBrFields=IIF(EMPTY(lcBrowsFld),.F.,lcBrowsFld)
IF SEEK(&lcFrstRet)
  SCATTER FIELDS &lcScatField TO laFieldBr MEMO
ELSE  
    IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
      GO RECNO(0)
    ELSE
      GO TOP
    ENDIF
  =gfBrows(lckeyToUse,lcScatField,'laFieldBr',lcWinTitle,llCanFilter)
ENDIF
lcBrFields=lcTmpBrFld
SELECT(lcCurDbf)
IF lnCurRec<>0
  GO lnCurRec
ENDIF
IF !EMPTY(laFieldBr[1])
  IF lcFrstType='F'
    REPLACE &lcFrstRet WITH laFieldBr[1]
  ELSE
    IF lcFrstType='V'
     &lcFrstRet=laFieldBr[1]  
    ENDIF 
  ENDIF
  IF lcSecType='F'
    REPLACE &lcSecRet WITH laFieldBr[2]
  ELSE
    IF lcSecType='V'
      &lcSecRet=laFieldBr[1]  
    ENDIF 
  ENDIF
ELSE
  IF lcFrstType='F'
    REPLACE &lcFrstRet WITH lcOldVal
  ELSE
    IF lcFrstType='V'
     &lcFrstRet=lcOldVal
    ENDIF 
  ENDIF
ENDIF

IF llMyUse
  USE IN &lcDbfName
ELSE
  SELECT(lcDbfName)  
  IF !EMPTY(lcOldOrd)
    SET ORDER TO &lcOldOrd
  ELSE
    SET ORDER TO
  ENDIF
  IF !EMPTY(lcFilter)
    SET FILTER TO &lcFilter
  ELSE
    SET FILTER TO
  ENDIF
  IF lnRecNo<>0
    GO lnRecNo
  ENDIF
ENDIF
SELECT(lcCurDbf)
SHOW GET &lcFrstRet
IF lcSecType<>'U'
  SHOW GET &lcSecRet
ENDIF  

*!**************************************************************
*!
*!               FUNCTION lfGetType
*!
*!**************************************************************
*  FUNCTION to check a name if it is an array element or a field
*  or variable
FUNCTION lfGetType
PARAMETERS lcWhchType
PRIVAT lcSelect,lcType
lcSelect=SELECT()
SELECT SELECT(1)
lcType=''
IF TYPE(lcWhchType)<>'U'
 lcType='V'
ENDIF
SELECT(lcSelect)
RETURN IIF(EMPTY(lcType),IIF(TYPE(lcWhchType)='U','U','F'),'V')
