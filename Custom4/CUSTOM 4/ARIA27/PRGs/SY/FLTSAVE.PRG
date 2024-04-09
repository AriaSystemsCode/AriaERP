*:************************************************************************
*: Program file  : FLTSAVE.PRG
*: Program desc. : Main system
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
PRIVATE lcSelect,llCanSave
STORE .F. TO llCanSave,cbaccbyall
STORE lcUsedFlt TO lcFltID
STORE '' TO lcflt_dis
lcSelect=SELECT()
SELECT SYUFILTR
IF !EMPTY(lcUsedFlt)
  IF SEEK(lcFilter+lcUsedFlt)
    llCanSave= (cUser_ID=gcUser_ID)
    lcflt_dis=cFltr_Dis
    cbAccByall = lAllUsers
  ENDIF
ENDIF

DO fltSave.SPR

SELECT(lcSelect)

FUNCTION lfShow
IF EMPTY(lcFltID)
  SHOW GET pbDelete DISABLE
ENDIF
*IF llCanSave
*  SHOW GETS ENABLE ONLY
*ELSE
*  SHOW GETS DISABLE ONLY
*  SHOW GET pbSaveAs ENABLE
*ENDIF


FUNCTION lfvSave
PRIVATE lcSelect
IF !EMPTY(lcFltID)
  lcSelect=SELECT()
  SELECT SYUFILTR
  IF SEEK(lcFilter+lcFltID)
     IF lcFltID<>lcUsedFlt
       WAIT 'FILTER ALREADY EXIST' WINDOW
     ENDIF     
  ELSE
    lcDeleteStat=SET('DELETE')
    SET DELETE OFF
    IF !SEEK('')
      APPEND BLANK   
    ELSE
      RECALL  
    ENDIF  
    SET DELETE &lcDeleteStat
  ENDIF
  DIMENSION laTmpFlt[1,7]
  STORE '' TO laTmpFlt
  SELECT mlField_nm,cleftdes,clefttype,lisNot,coperator,mrfield_nm,crightdes;
         FROM TemCursor INTO ARRAY laTmpFlt
  SELECT syuFiltr
  DIMENSION laTmpFlt[RECCOUNT('TemCursor'),7]
  REPLACE cflter_ID WITH lcFltID,;
          cFltr_Typ WITH lcFilter,;
          cFltr_Dis WITH lcflt_dis,;
          cUser_ID  WITH gcUser_ID,;
          lAllUsers WITH cbaccbyall,;
          cComp_ID  WITH gcAct_Comp,;
          cFile_Nam WITH lcSeekName
  SAVE  TO MEMO mFltr_Exp ALL LIKE laTmpFlt
  lcUsedFlt=lcFltID
  SELECT(lcSelect)
  CLEAR READ
ELSE
  WAIT 'You must Enter a filter ID first' WINDOW  NOWAIT
  _CUROBJ=OBJNUM(lcFltID)
ENDIF  







FUNCTION lfvDelete
PRIVATE lcSelect
IF !EMPTY(lcFltID)
  lcSelect=SELECT()
  SELECT SYUFILTR
  IF SEEK(lcFilter+lcFltID)
     IF lcFltID<>lcCurFilter
       WAIT 'The filter in active is not the Deleted one' WINDOW
     ENDIF
     SCATTER MEMVAR MEMO BLANK
     GATHER MEMVAR MEMO
     DELETE  
  ENDIF
  lcUsedFlt=''
  SELECT(lcSelect)
  CLEAR READ  
ENDIF

