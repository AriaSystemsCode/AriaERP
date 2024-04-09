*:************************************************************************
*: Program file  : LFFIND.PRG
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
*B601437,1 Hesham El-Sheltawi 26/11/96
*B601437,1 in find function if there is a browse key then seek for the key 
*B601437,1 before issuing the locate command and make the locate command
*B601437,1 locate rest
*B601456,1 Hesham El-Sheltawi 12/05/96
*B601456,1 set the array of the screen mode to be in the edit mode
*B601456,1 Just to make the expression builder can be editable
*B601660,1 Hesham El-Sheltawi 13/03/97
*B601660,1 Make the Global Browse Use one read Level
*B601660,1 Change the validation of the push button Expr
*B601660,1 that branch to the expression screen to Terminate the read
*B601660,1 of the find
*B606457,1 TMI 11/05/2002 Fix a bug that a error message when using the find screen from style screen
*B129341,1 EIH 05/09/2005 Fix a bug that we search about upper case and don't set upper to the field which we search in.
*:***************************************************************************

EXTERNAL ARRAY laData,laKeyField,laField_H,laField_N
PRIVATE lcCurFile,lcPrgToRun,rbValExp
STORE 1 TO rbValExp,lnCount
lvSought= " "
lcPrgToRun = ''
=lfDefFieldArr()

IF TYPE('lcOldExpr') $ "UL"
  PUBLIC lcOldExpr
  lcOldExpr = ""
ENDIF  

*B601660,1 Hesham El-Sheltawi (Start)
*B601660,1 Execute the Find Screen In a do while 
*B601660,1 So when Executing any other screen or function
*B601660,1 it return to run the Find screen again and keep 
*B601660,1 the read level as it is
*DO lwFind.SPR
DO WHILE .T.
  CLEAR TYPEAHEAD
  DO lwFind.SPR
  IF !EMPTY(lcPrgToRun)
    &lcPrgToRun
    lcPrgToRun = ''
  ELSE
    EXIT
  ENDIF
  CLEAR TYPEAHEAD  
ENDDO
IF WEXIST('lwFind')
  RELEASE WINDOW lwFind
ENDIF
*B601660,1 Hesham El-Sheltawi (End)


*!**************************************************************************
*!
*!      Function: lfShowExp
*!
*!**************************************************************************
*
FUNCTION lfShowExp
IF RDLEVEL() > 4
  SHOW GET rbValExp,2 DISABLE
  SHOW GET puExp DISABLE
ENDIF
*B601660,1 Hesham El-Sheltawi (Start)
=lfvValExp()
*B601660,1 Hesham El-Sheltawi (End)

*!**************************************************************************
*!
*!      Function: lfvValExp
*!
*!**************************************************************************
*
FUNCTION lfvValExp
DO CASE
CASE rbValExp = 1
  SHOW GET puExp DISABLE
  lvExpres = " "
  SHOW GET lvExpres DISABLE
  SHOW GET puField_H ENABLE
  SHOW GET lvSought ENABLE
  SHOW GET puOperat ENABLE
CASE rbValExp = 2
  SHOW GET puExp ENABLE
  SHOW GET lvExpres ENABLE
  SHOW GET puField_H DISABLE
  lvSought = " "
  SHOW GET lvSought DISABLE
  SHOW GET puOperat DISABLE
ENDCASE
*B601660,1 Hesham El-Sheltawi (Start)
*SHOW GETS
*B601660,1 Hesham El-Sheltawi (End)


*!**************************************************************************
*!
*!      Function : lfvSought
*!
*!**************************************************************************
*
FUNCTION lfvSought
IF !EMPTY(lvSought)
  IF "'"$lvSought
    SET CURSOR OFF
    WAIT WINDOW "Can't have single quote in value" NOWAIT
    SET CURSOR ON
    RETURN .F.
  ENDIF
  IF TYPE('&puField_H') = "L"
    IF AT('.T.',UPPER(lvSought))=0 AND;
       AT('.F.',UPPER(lvSought))=0 AND ;
       NOT EMPTY(lvSought)
      WAIT WINDOW "Logical values must have periods around them" NOWAIT
      RETURN .F.
    ENDIF
  ENDIF
ENDIF  

*!**************************************************************************
*!
*!      Function : lfvExp
*!
*!**************************************************************************
*
FUNCTION lfvExp
*B601456,1 Hesham El-Sheltawi (Start)
*B601456,1 set the array of the screen mode to be in the edit mode
*B601456,1 Just to make the expression builder can be editable
DIME laScrMode[5]
PRIVATE laModes
=ACOPY(laScrMode,laModes)
laScrMode[3] = .T.
*B601456,1 Hesham El-Sheltawi (End)
IF llEmpty
  lvExpres=GetExpr() 
ELSE
  lvExpres=GetExpr(lvExpres,ALIAS()," ","laField_H",@laField_N)
ENDIF  
*B601456,1 Hesham El-Sheltawi (Start)
=ACOPY(laModes,laScrMode)
*B601456,1 Hesham El-Sheltawi (End)
SHOW GET lvExpres
SHOW GETS 

*!**************************************************************************
*!
*!      Function : lfvExpres
*!
*!**************************************************************************
*
FUNCTION lfvExpres
IF !EMPTY(lvExpres)
  IF(TYPE(lvExpres)="U")
    WAIT WINDOW "Not a valid expression"  NOWAIT
  ENDIF  
ENDIF           

*!**************************************************************************
*!
*!      Function : lfvAction
*!
*!**************************************************************************
*
*FUNCTION lfvAction
*REGIONAL lvHold, lvType, lvOldRec
*IF BOF() OR EOF()
*  lvOldRec=0
*ELSE  
*  lvOldRec = RECNO()
*ENDIF

*DO CASE
*  CASE pbAction = 1
*  DO CASE
*    CASE rbValExp = 1
*      lvHold = UPPER(ALLTRIM(lvSought))
*      IF EMPTY(lvHold) OR EMPTY(puField_H)
*        WAIT WINDOW "Missing field or value" NOWAIT
*        RETURN .F.
*        _CUROBJ=OBJNUM(lvSought)
*      ELSE
*        IF OCCURS('"',lvHold)>0 .OR. OCCURS("'",lvHold)>0
*          WAIT WINDOW "Can't have double quotes in value" NOWAIT
*          RETURN .F.
*          _CUROBJ=OBJNUM(lvSought)
*        ENDIF         
*        lvHold = laField_N[ASCAN(laField_H,puField_H)]+ALLTRIM(puOperat)+'"'+lvHold+'"'
*        lnType = ASCAN(laField_H,puField_H)      
*        lcChk  = "&laField_N["+ALLTRIM(STR(lnType,LEN(STR(lnType)),0))+"]"
*        lvType = TYPE(laField_N[lnType])
*        DO CASE
*          CASE lvType = "C" OR lvType = "M" 
*            lvHold = "ALLTRIM("+SUBSTR(lvHold,1,AT(puOperat,lvHold)-1)+")"+;
*                      SUBSTR(lvHold,AT(puOperat,lvHold),;
*                      LEN(lvHold)-AT(puOperat,lvHold)+1)           
*          CASE lvType = "D"
*            IF OCCURS('"',lvHold)>0
*              lvHold = STRTRAN(lvHold,'"','CTOD("',1,1)
*              lvHold = STRTRAN(lvHold,'"','")',2,1)
*            ENDIF
*          CASE lvType = "N"
*            lvHold = STRTRAN(lvHold,'"','',1)
*          CASE lvType = "L"
*            lvHold = STRTRAN(lvHold,'"','',1)
*        ENDCASE
*        GO TOP
*        LOCATE FOR &lvHold
*	    DO PostLoc
*      ENDIF
*    CASE rbValExp = 2
*      IF TYPE(lvExpres)<>"L" .OR. EMPTY(lvExpres)
*        WAIT WINDOW "No expression" NOWAIT
*      ELSE
*        LOCATE FOR &lvExpres
*        DO PostLoc
*      ENDIF
*  ENDCASE
*  lcOldExpr = lvExpres
*  CASE pbAction = 2
*  CLEAR READ
*ENDCASE

*!**************************************************************************
*!
*!      Procedure: PostLoc
*!
*!**************************************************************************
*
PROCEDURE PostLoc
IF !FOUND()
  SET CURSOR OFF
  WAIT WINDOW "Not found" NOWAIT
  SET CURSOR ON
  IF lvOldRec <> 0  
    GO lvOldRec
  ENDIF  
  _CUROBJ=IIF(rbValExp = 1,OBJNUM(lvSought),OBJNUM(lvExpres))
ELSE
  CLEAR READ 
ENDIF

*!**************************************************************************
*!
*!      Function : lfvFind
*!
*!**************************************************************************
*
FUNCTION lfvFind
REGIONAL lvHold, lvType, lvOldRec
IF BOF() OR EOF()
  lvOldRec=0
ELSE  
  lvOldRec = RECNO()
ENDIF
DO CASE
  CASE rbValExp = 1
    lvHold = UPPER(ALLTRIM(lvSought))
    IF EMPTY(lvHold) OR EMPTY(puField_H)
      WAIT WINDOW "Missing field or value" NOWAIT
      RETURN .F.
      _CUROBJ=OBJNUM(lvSought)
    ELSE
      IF OCCURS('"',lvHold)>0 .OR. OCCURS("'",lvHold)>0
        WAIT WINDOW "Can't have double quotes in value" NOWAIT
        RETURN .F.
        _CUROBJ=OBJNUM(lvSought)
      ENDIF         
      lvHold = laField_N[ASCAN(laField_H,puField_H)]+ALLTRIM(puOperat)+'"'+lvHold+'"'
      lnType = ASCAN(laField_H,puField_H)      
      lcChk  = "&laField_N["+ALLTRIM(STR(lnType,LEN(STR(lnType)),0))+"]"
      lvType = TYPE(laField_N[lnType])
      DO CASE
        CASE lvType = "C" OR lvType = "M" 
          *B129341,1 EIH 05/09/2005 Fix a bug that we search about upper case and don't set upper to 
          *B129341,1 EIH			The field which we search in [Start].
          
          *lvHold = "ALLTRIM("+SUBSTR(lvHold,1,AT(puOperat,lvHold)-1)+")"+;
                    SUBSTR(lvHold,AT(puOperat,lvHold),;
                    LEN(lvHold)-AT(puOperat,lvHold)+1)           
          lvHold = "UPPER(ALLTRIM("+SUBSTR(lvHold,1,AT(puOperat,lvHold)-1)+"))"+;
                    SUBSTR(lvHold,AT(puOperat,lvHold),;
                    LEN(lvHold)-AT(puOperat,lvHold)+1)           
          
          *B129341,1 EIH 05/09/2005 [End]

        CASE lvType = "D"
          IF OCCURS('"',lvHold)>0
            lvHold = STRTRAN(lvHold,'"','CTOD("',1,1)
            lvHold = STRTRAN(lvHold,'"','")',2,1)
          ENDIF
        CASE lvType = "N"
          lvHold = STRTRAN(lvHold,'"','',1)
        CASE lvType = "L"
          lvHold = STRTRAN(lvHold,'"','',1)
      ENDCASE
     *B601456,1 Hesham El-Sheltawi (Start)
      IF TYPE(lvHold) = 'U'
        WAIT 'Not Found' WINDOW NOWAIT
        RETURN
      ENDIF
      *B601456,1 Hesham El-Sheltawi (End)
      
      GO TOP
      *B601437,1 Hesham El-Sheltawi (Start)
      lcOldExpr = lvHold
      IF TYPE('lcForExp')='C' AND !EMPTY(lcForExp)
        IF UPPER(LEFT(lcForExp,4)) = 'FOR '
          *B606457,1 TMI [Start] the lcForExp contains "Rest" word at its end , so let it at the end of the expression 
          *lvHold = SUBSTR(lcForExp,5)+ ' AND '+lvHold
          lvHold = lvHold + ' AND ' + SUBSTR(lcForExp,5)
          *B606457,1 TMI [End  ] 
        ELSE  
          lvHold = lcForExp + ' AND ' + lvHold
        ENDIF  
      ENDIF
      IF TYPE('lcFltrExp') = 'C' AND !EMPTY(lcFltrExp)
        PRIVATE lcKeyToSeek 
        lcKeyToSeek = lcFltrExp
        DO CASE
          CASE  ATC("',",lcFltrExp) > 0
            lcKeyToSeek = SUBSTR(lcFltrExp,1,ATC("',",lcFltrExp))
          CASE  ATC('",',lcFltrExp) > 0
            lcKeyToSeek = SUBSTR(lcFltrExp,1,ATC('",',lcFltrExp))
        ENDCASE  
        =SEEK(EVAL(lcKeyToSeek))
        *B606457,1 TMI [START] using LOCATE command without REST word, since it is included in the string lvHold
        *LOCATE REST FOR &lvHold      
        LOCATE FOR &lvHold      
        *B606457,1 TMI [END  ] using LOCATE command without REST word, since it is included in the string lvHold
      ELSE
        LOCATE FOR &lvHold
      ENDIF
*      LOCATE FOR &lvHold
      *B601437,1 Hesham El-Sheltawi (End)      
      DO PostLoc
    ENDIF

  CASE rbValExp = 2
    IF TYPE(lvExpres)<>"L" .OR. EMPTY(lvExpres)
      WAIT WINDOW "No expression" NOWAIT
    ELSE
      *B601437,1 Hesham El-Sheltawi (Start)
      IF TYPE('lcForExp')='C' AND !EMPTY(lcForExp)
        IF UPPER(LEFT(lcForExp,4)) = 'FOR '
          lvExpres = SUBSTR(lcForExp,5)+ ' AND '+lvExpres
        ELSE  
          lvExpres = lcForExp + ' AND ' + lvExpres
        ENDIF  
      ENDIF
      IF TYPE('lcFltrExp') = 'C' AND !EMPTY(lcFltrExp)
        PRIVATE lcKeyToSeek 
        lcKeyToSeek = lcFltrExp
        DO CASE
          CASE  ATC("',",lcFltrExp) > 0
            lcKeyToSeek = SUBSTR(lcFltrExp,1,ATC("',",lcFltrExp))
          CASE  ATC('",',lcFltrExp) > 0
            lcKeyToSeek = SUBSTR(lcFltrExp,1,ATC('",',lcFltrExp))
        ENDCASE  
        =SEEK(EVAL(lcKeyToSeek))
        LOCATE REST FOR &lvExpres
      ELSE
        LOCATE FOR &lvExpres
      ENDIF
*     LOCATE FOR &lvExpres
      *B601437,1 Hesham El-Sheltawi (End)      
      DO PostLoc
    ENDIF
  lcOldExpr = lvExpres    
ENDCASE


*!**************************************************************************
*!
*!      Function : lfvCancel
*!
*!**************************************************************************
*
FUNCTION lfvCancel
CLEAR READ




FUNCTION LFDUMM
IF lcBaseFile <> ALIAS() 
  IF SEEK(UPPER(ALIAS())+".DBF","SYDFLFLD")
    IF !EMPTY(lcBrFields)
      lcCurFile = ALIAS()+".DBF"
      SELECT  DISTINCT IIF(EMPTY(sydField.cFld_Head),sydField.cFld_Name,;
              sydField.cFld_Head),sydFlFld.nFld_Pos ;
              FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
              WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
              ALLTRIM(sydFlFld.cFile_Nam) = lcCurFile .AND.;
              ALLTRIM(sydFlFld.cFld_Name) $ UPPER(lcBrFields) ;
              ORDER BY nFld_Pos;
              INTO ARRAY laField_H

      SELECT  DISTINCT sydField.cFld_Name,sydFlFld.nFld_Pos ;
              FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
              WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
              ALLTRIM(sydFlFld.cFile_Nam) = lcCurFile .AND.;
              ALLTRIM(sydFlFld.cFld_Name) $ UPPER(lcBrFields) ;
              ORDER BY nFld_Pos;
              INTO ARRAY laField_N
    ELSE 
      SELECT  DISTINCT IIF(EMPTY(sydField.cFld_Head),sydField.cFld_Name,;
              sydField.cFld_Head),sydFlFld.nFld_Pos ;
              FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
              WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
              sydFlFld.cFile_Nam = lcCurFile;
              ORDER BY nFld_Pos;
              INTO ARRAY laField_H

       SELECT DISTINCT sydField.cFld_Name,sydFlFld.nFld_Pos ;
              FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
              WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
              sydFlFld.cFile_Nam = lcCurFile;
              ORDER BY nFld_Pos;
              INTO ARRAY laField_N
     ENDIF
     =gfADel(@laField_H,2,2)  
     =gfADel(@laField_N,2,2)           
  ELSE
    DIMENSION laField_N[FCOUNT()],laField_H[FCOUNT()]
    FOR lnCount=1 TO FCOUNT()
      laField_N[lnCount]=FIELD(lnCount)
      laField_H[lnCount]=FIELD(lnCount)
    ENDFOR
  ENDIF       
  llSelBrow=.T.
  puField_H=laField_H[1,1]
ENDIF
