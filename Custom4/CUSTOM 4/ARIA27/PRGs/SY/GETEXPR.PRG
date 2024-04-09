*:************************************************************************
*: Program file  : GETEXPR.PRG
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
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it

PARAMETER lcExp,lcFile,lcExpPrmpt,la_Field,laField,llAddAlias,lcType,laFiles,la_Var
EXTERNAL ARRAY LASCRMODE,LAFILES,LA_VAR 

lcMesage=IIF(EMPTY(lcExpPrmpt),"",lcExpPrmpt)
lcType=IIF(TYPE('lcType')='C',lcType+'NLCD','NLCD')
llCanVald = .T.
IF TYPE('laScrMode')='L'
  llCanVald = laScrMode[3] OR laScrMode[4]
ENDIF
lcEdit = IIF(llCanVald,""," NOMODIFY")
DECLARE laFile[1,1],laVar[1,1],laMath[32,1],laDate[18,1],;
        laStr[35,1],lalogic[19,1]

=lfAryBuld()
lnRow = 4 + IIF(EMPTY(lcExp),0,INT(LEN(ALLTRIM(lcExp))/58))
lnCol = 2 + IIF(EMPTY(lcExp),0,MOD(LEN(ALLTRIM(lcExp)),58))
lsField=0
***** build arrays holding fields and fields logical names
IF !(TYPE('laFiles') $ 'UL')
  DECLARE laFld_H[1,2],laField[1,2]  
ENDIF  
 =lfGetFileFld(lcFile,IIF(TYPE('laFiles') $ 'UL',.F.,.T.))
  

IF TYPE('lcExpPrmpt') $ "UL"
  lcExpPrmpt = ''
ENDIF

IF TYPE('lcFile')="U" 
  laFile = ALIAS()
  IF !EMPTY(laFile)
    DIMENSION laField[FCOUNT()]
    FOR lnCount = 1 TO FCOUNT()
      laField[lnCount] = FIELD(lnCount)
    ENDFOR
  ENDIF  
ENDIF
IF !EMPTY(laField) AND !('U' $ lcType) 
  FOR lnCount = 1 TO ALEN(laField)
    DO CASE
      CASE LEFT(laField[lnCount],1)="M" OR LEFT(laField[lnCount],1)="C"
        &laField[lnCount] = " "
      CASE LEFT(laField[lnCount],1)="N"
        &laField[lnCount] = 1
      CASE LEFT(laField[lnCount],1)="D"
        &laField[lnCount] = DATE()
      OTHERWISE
        &laField[lnCount] = .F.
    ENDCASE
  ENDFOR
ELSE
   IF TYPE('laFiles') $ 'UL' AND EMPTY(laField)
     laField = " "
   ENDIF  
ENDIF

mvExp = IIF(EMPTY(lcExp),"",ALLTRIM(lcExp))
IF _DOS
  laFile = SPACE(INT(31-LEN(ALLTRIM(laFile)))/2)+ALLTRIM(laFile)+;
           SPACE(INT(31-LEN(ALLTRIM(laFile)))/2)
ENDIF           

DO GETEXPR.SPR

lcExp = IIF(EMPTY(lcExp),"",lcExp) 
RETURN lcExp

FUNCTION lfShow
IF !llCanVald 
  SHOW GET pbVerfy DISABLE
  SHOW GET pbCancel DISABLE
ENDIF


****************************************************************************
*
*      FUNCTION : lfvMath
*
****************************************************************************
FUNCTION lfvMath
IF llCanVald
  IF puMath <> 1
    DO lpWriteExp WITH laMath[puMath]
    puMath = 1
    SHOW GET puMath
  ENDIF
ELSE
  puMath = 1
  SHOW GET puMath
ENDIF  

****************************************************************************
*
*      FUNCTION : lfvString
*
****************************************************************************
FUNCTION lfvString
IF llCanVald
  IF puStr <> 1
    DO lpWriteExp WITH laStr[puStr]
    puStr = 1
    SHOW GET puStr
  ENDIF
ELSE
  puStr = 1
  SHOW GET puStr
ENDIF  

****************************************************************************
*
*      FUNCTION : lfvLogic
*
****************************************************************************
FUNCTION lfvLogic
IF llCanVald
  IF puLogic <> 1 
    DO lpWriteExp WITH laLogic[puLogic]
    puLogic = 1 
    SHOW GET puLogic
  ENDIF
ELSE
  puLogic = 1 
  SHOW GET puLogic
ENDIF  

****************************************************************************
*
*      FUNCTION : lfvDate
*
****************************************************************************
FUNCTION lfvDate
IF llCanVald
  IF puDate <> 1 
    DO lpWriteExp WITH laDate[puDate]
    puDate = 1
    SHOW GET puDate
  ENDIF
ELSE
  puDate = 1
  SHOW GET puDate
ENDIF  


****************************************************************************
*
*      FUNCTION : lfvfiles
*
****************************************************************************
FUNCTION lfvfiles
lcFile = laFiles[puFile,2]
=lfGetFileFld(lcFile,.T.)
lcFile = laFiles[puFile,1]
SHOW GETS

****************************************************************************
*
*      FUNCTION : lfvVariabe
*
**************************************************************************** 
FUNCTION lfvVariabe 
IF lsVariable > 0
  DO lpWriteExp WITH (la_Var[lsVariable,2])
ENDIF

****************************************************************************
*
*      FUNCTION : lfvMvExp
*
****************************************************************************
FUNCTION lfvMvExp
ON KEY LABEL ENTER
lnRow = ROW()
lnCol = COL()

****************************************************************************
*
*      FUNCTION : lfvCtrl
*
****************************************************************************
FUNCTION lfvCtrl
PARAMETERS lnCtrl

DO CASE
  CASE lnCtrl = 3
    CLEAR READ
  CASE lnCtrl = 1
    WAIT IIF(!(TYPE(mvExp) $ lcType), "Not a valid expression",;
             "Valid expression ") WINDOW NOWAIT
   OTHERWISE
     IF !EMPTY(mvExp)
       IF !(TYPE(mvExp) $ lcType)
         WAIT  "Not a valid expression" WINDOW NOWAIT
       ELSE
         lcExp = mvExp         && Added by RAM on 06/14/94
         CLEAR READ
       ENDIF 
     ELSE
       lcExp = " "             && Added by RAM on 06/14/94
       CLEAR READ
     ENDIF           
*     lcExp = mvExp            && Remarked by RAM on 06/14/94
ENDCASE

****************************************************************************
*
*      FUNCTION : lfvField
*
****************************************************************************
FUNCTION lfvField

IF lsField > 0
  DO lpWriteExp WITH (IIF(llAddAlias,ALLTRIM(lcFile)+'.','')+laField[lsField])
ENDIF

****************************************************************************
*
*      PROCEDURE : lpWriteExp
*
****************************************************************************
PROCEDURE lpWriteExp
PARAMETER lcExpElm
*mvExp = STUFF(mvExp,(lnRow-4)*58+lnCol-1, 0,ALLTRIM(lcExpElm)+" ")
lnHiglight=ATC('(exp',lcExpElm)
lnHiglight=IIF(lnHiglight>0,lnHiglight+4,0)
lnRight = LEN(lcExpElm)-lnHiglight
lcCntrol = ''
IF lnHiglight>0
  FOR lnCount= 1 TO lnRight
    lcCntrol=lcCntrol+[+"{leftarrow}"]
  ENDFOR
  FOR lnCount= 1 TO 4
    lcCntrol=lcCntrol+[+"{SHIFT+leftarrow}"]
  ENDFOR 
  
ENDIF  
IF !EMPTY(lcCntrol)
  KEYBOARD CHR(24)+lcExpElm +&lcCntrol
ELSE
  KEYBOARD CHR(24)+lcExpElm 
ENDIF  
SHOW GET mvExp
_CUROBJ = OBJNUM(mvExp)
*KEYBOARD "{CTRL+RIGHTARROW}"

****************************************************************************
*
*      FUNCTION :lfAryBuld
*
****************************************************************************
FUNCTION lfAryBuld

laMath[1] = 'Math'
laMath[2] = '*'
laMath[3] = '/'
laMath[4] = '+'
laMath[5] = '-'
laMath[6] = 'ABS(expN)'
laMath[7] = 'MAX(,)'
laMath[8] = '^'
laMath[9] = 'MIN(,)'
laMath[10] = 'MOD(,)'
laMath[11] = 'PAYMENT(,,)'
laMath[12] = 'PI()'
laMath[13] = 'PV(,,)'
laMath[14] = 'RAND()'
laMath[15] = 'ACOS(expN)'
laMath[16] = 'ASIN(expN)'
laMath[17] = 'ATAN(expN)'
laMath[18] = 'ATN2(expN)'
laMath[19] = 'CEILING(expN)'
laMath[20] = 'EXP(expN)'
laMath[21] = 'FLOOR(expN)'
laMath[22] = 'FV(,,)'
laMath[23] = 'INT(expN)'
laMath[24] = 'LOG(expN)'
laMath[25] = 'LOG10(expN)'
laMath[26] = 'ROUND(expN)'
laMath[27] = 'RTOD(expN)'
laMath[28] = 'SIGN(expN)'
laMath[29] = 'SIN(expN)'
laMath[30] = 'SQRT(expN)'
laMath[31] = 'TAN(expN)'
laMath[32] = 'VAL(expC)'

laStr[1] = 'String'
laStr[2] = '-'
laStr[3] = '""'
laStr[4] = '+'
laStr[5] = '-'
laStr[6] = 'ASC(expC)'
laStr[7] = 'ALLTRIM(expC)'
laStr[8] = 'LEFT(expC,)'
laStr[9] = 'LEN(expC)'
laStr[10] = 'LOWER(expC)'
laStr[11] = 'UPPER(expC)'
laStr[12] = 'LTRIM(expC)'
laStr[13] = 'RTRIM(expC)'
laStr[14] = 'PADC(expC,)'
laStr[15] = 'PADL(expC,)'
laStr[16] = 'PADR(expC,)'
laStr[17] = 'RIGHT(expC,)'
laStr[18] = 'TYPE(expC)'
laStr[19] = 'PROPER(expC)'
laStr[20] = 'SOUNDEX(expC)'
laStr[21] = 'CHR(expN)'
laStr[22] = 'SPACE(expN)'
laStr[23] = 'STR(expN,,)'
laStr[24] = 'AT(,,)'
laStr[25] = 'ATC(,,)'
laStr[26] = 'MAX(,)'
laStr[27] = 'MIN(,)'
laStr[28] = 'OCCURS(,)'
laStr[29] = 'RAT(,,)'
laStr[30] = 'REPLICATE(,)'
laStr[31] = 'CHRTRAN(,,)'
laStr[32] = 'STRTRAN(,,)'
laStr[33] = 'STUFF()'
laStr[34] = 'SUBSTR()'
laStr[35] = 'TRANSFORM(,)'

lalogic[1] = 'Logical'
lalogic[2] = '=='
lalogic[3] = '()'
lalogic[4] = '<'
lalogic[5] = '>'
lalogic[6] = '='
lalogic[7] = '<>'
lalogic[8] = '<='
lalogic[9] = '>='
lalogic[10] = '.T.'
lalogic[11] = '.F.'
lalogic[12] = 'NOT'
lalogic[13] = 'AND'
lalogic[14] = 'OR'
lalogic[15] = 'BETWEEN(,,)'
lalogic[16] = 'EMPTY(expR)'
lalogic[17] = 'IIF(,,)'
lalogic[18] = 'INLIST(,,)'

laDate[1] = 'Date'
laDate[2] = 'CDOW(expD)'
laDate[3] = 'CMONTH(expD)'
laDate[4] = 'CTOD(expC)'
laDate[5] = 'DATE(expD)'
laDate[6] = 'DAY(expD)'
laDate[7] = 'DMY(expD)'
laDate[8] = 'DOW(expD)'
laDate[9] = 'DTOS(expD)'
laDate[10] = 'GOMONTH(expD)'
laDate[11] = 'MDY(expD)'
laDate[12] = 'MONTH(expD)'
laDate[13] = 'YEAR(expD)'
laDate[14] = 'MAX(,)'
laDate[15] = 'MIN(,)'
laDate[16] = 'SECONDS()'
laDate[17] = 'TIME()'



FUNCTION lfGetFileFld
PARAMETERS lcFile,llHaveToBld
IF TYPE('la_Field') $ "UL"  OR llHaveToBld
  la_Field = "laFld_H"
  laField  = "laField"
  DECLARE laFld_H[1,2],laField[1,2]
  *E301077,78 Hesham (Start)
  llflfld = gfSysOpen(gcSysHome+'sydflfld')
  llfield = gfSysOpen(gcSysHome+'sydfield')
  llfiles = gfSysOpen(gcSysHome+'sydfiles')  
  *E301077,78 Hesham (End)
  
  SELECT  DISTINCT IIF(EMPTY(sydField.cFld_Head),sydField.cFld_Name,;
          sydField.cFld_Head),sydFlFld.nFld_Pos ;
          FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
          WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
          sydFlFld.cFile_Nam = lcFile ;
          ORDER BY nFld_Pos;
          INTO ARRAY laFld_H

  SELECT DISTINCT sydField.cFld_Name,sydFlFld.nFld_Pos ;
         FROM &gcSysHome.sydField,&gcSysHome.sydFlFld;
         WHERE sydField.cFld_Name=sydFlFld.cFld_Name .AND.;
         sydFlFld.cFile_Nam = lcFile ;
         ORDER BY nFld_Pos;
         INTO ARRAY laField

    =gfADel(@laFld_H,2,2)       
    =gfADel(@laField,2,2)       
  lcTit= LOOKUP(sydFiles.cFile_ttl,lcFile,;
                  sydFiles.cFile_Nam,'cFile_Nam')
  laFile = ALLTRIM(IIF(EMPTY(lcTit),lcFile,lcTit))
  *E301077,78 Hesham (Start)
  USE IN IIF(llflfld,'sydflfld',0)
  USE IN IIF(llfield,'sydfield',0)
  USE IN IIF(llfiles,'sydfiles',0)    
  *E301077,78 Hesham (End)
ELSE
  laFile = ALIAS()         
ENDIF        

