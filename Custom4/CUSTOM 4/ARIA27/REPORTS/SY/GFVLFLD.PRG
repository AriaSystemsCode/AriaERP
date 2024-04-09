*:************************************************************************
*: Program file  : GFVLFLD.PRG
*: Program desc. : 
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : 
*:************************************************************************
*B602308,1 MAB 12/13/1998 Fix order of SydFiles file.
*E301077,72 Select sydfiles before seeking.

PARAMETERS lcDbfName,lcFrstField,lcFrstRet,lcOldVal,;
           lcSecField,lcSecRet,lcBrowsFld,lcOrdName,;
           lcKeyToUse,llCanFilter
PRIVATE lcCurDbf,lcFrstType,lcSecType,lcTmpBrFld,lnCurRec,lcWinTitle,;
        lcFilter,laScrMode
        

IF lcFrstField="CACCTCODE"
  IF EMPTY(STRTRAN(EVAL(lcFrstRet),'-'))
    IF ATC('-',EVAL(lcFrstRet)) > 0
      STORE "" TO (lcFrstRet)
    ENDIF  
    RETURN
  ENDIF
ENDIF  
        
IF EMPTY(EVAL(lcFrstRet))
  RETURN
ENDIF
DIMENSION laScrMode[5]        
STORE .F. TO laScrMode
lcCurDbf=SELECT()
lnCurRec=IIF(RECNO()>RECCOUNT(),0,RECNO())

llMyUse=.F.           
****** Validate the database to use name and the first field name           
lcFileName=IIF(TYPE('lcFileName')='U','',lcFileName)
IF EMPTY(lcDbfName) OR EMPTY(lcFrstField) ;
  OR TYPE('lcDbfName')<>'C' OR TYPE('lcFrstField')<>'C'
    &lcFrstRet = lcOldVal
    RETURN
ENDIF  
********************************************************

**** Validate the variable name to return value in it
IF TYPE('lcFrstRet')<>'C' OR TYPE(lcFrstRet)='U'
  &lcFrstRet = lcOldVal
  RETURN
ENDIF
********************************************************
lcFrstType = lfGetType(lcFrstRet)
lcSecType  =IIF(TYPE('lcSecRet')='C',lfGetType(lcSecRet),'U')
lcTmpBrFld=lcBrFields
IF !USED('SYDFILES')
  llMyUse=.T.
  
  *B602308,1 It is more readable to set order to tag name instead of 1
  *SELECT SELECT(1)
  *USE &gcSysHome.SYDFILES ORDER 1
  USE &gcSysHome.SYDFILES ORDER Cfile_nam IN 0 SHARED
  *B602308,1 end
  
  *E301077,72 SYDFILES must be selected.[Begin
  SELECT SYDFILES
  *E301077,72 SYDFILES must be selected.[End

ELSE

  SELECT SYDFILES
  lcOldOrd=ORDER()
  lcFilter=FILTER()
  lnRecNo=IIF(RECNO()>RECCOUNT(),0,RECNO())

  *B602308,1 order of sydfiles is Cfile_nam tag.
  *SET ORDER TO IIF(EMPTY(lcOrdName),1,&lcOrdName)
  SET ORDER TO TAG Cfile_nam
  *B602308,1 end
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
  lcPath=IIF(SUBSTR(UPPER(lcDbfName),1,2)='SY',gcSysHome,gcDataDir)  
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
lcBrFields=IIF(TYPE('lcBrowsFld')<>'C',gfDbfField(lcDbfName),lcBrowsFld)
IF !EMPTY(ALLTRIM(&lcFrstRet))
  IF SEEK(&lcFrstRet)
    SCATTER FIELDS &lcScatField TO laFieldBr MEMO
  ELSE  
    IF LASTKEY()=13
       IF RECNO(0) >0 .AND. RECNO(0) <= RECCOUNT()
         GO RECNO(0)
       ELSE
         GO TOP
       ENDIF
      =gfBrows(lckeyToUse,lcScatField,'laFieldBr',lcWinTitle,llCanFilter)
    ENDIF  
  ENDIF
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
