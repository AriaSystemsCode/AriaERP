*:************************************************************************
*: Program file  : sycmset.PRG
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
*: Passed Parameters  : lcOGModle  && Module ID to change the setting for
*:                                  && if empty then we are changing the
*:                                  && Company settings
*:                      lcOGCmpPath && Company data path
*:                      lcOGCmpName && Company ID
*:************************************************************************
*B601012,1 RENEE 04/16/96. Call lfVldSets() that checks for the validity 
*B601012,1                 of the entered values before closing.
*E300633,1 Hesham 04/08/97 Make the Program to change the setups of any
*E300633,1 module or the company setups
*B601802,1 Hesham El-Sheltawi 06/25/97
*B601802,1 in the view mode if we use the scrolling down or up the system
*B601802,1 go in an inf. loop, so check if the mode is !view mode before
*B601802,1 forcing the screen to go to an object of the grid
*B601911,1 AMM 02/12/98 Enable all objects if 'Reset'ing
*B601985,1 AMM 03/22/98 Fixed the bug of rounding the decimal numbers in the 
*B601985,1 AMM          setting grid when saving, and excluding one decimal 
*B601985,1 AMM          from diplaying numerics with decimals
*B601985,1 AMM          ReDimensioned array laOGObjType in SYCMSET2.SCX 
*B601985,1 AMM          Setup code to add 2 more columns.
*E300872,9 AMM 05/12/98 Use gfTraceKey when editing company\module settings
*E301058,4 AMM 11/09/98 Pass two new parameters to gfTracekey .
*E301073,1 AMM 11/29/98 Fix the bug of calling Codes file as a system file 
*E301073,1 AMM          not a dbf file
*E301077,78 Hesham 02/25/1999
*E301077,78 Controlling opening and clossing files.
*E301077,78 to make the program open the files when ever it need it
*E300789,1 Hesham 03/04/99
*E300789,1 remove field ccomp_id from files ACCOD,FISHD,FSPRD,FSHLD,CODES
*E300789,1 Change Any seeking in these files
*B602889,1 AMM 05/11/99 Perform an exact search on variable names to avoid
*B602889,1 				conflicts resulting from variables with close names
*E301433,1 RAMY 08/10/2000 Add calling the function LFVY2K() that valid the entered year in a date field
*E301469,1 AMH 11/14/2000 Purge DataBase.
*B803949,1 HBG 18/02/2001 make the text like field in Option Grid scrolling
*E301937,1 ADEL 07/31/02 Add a new "Material WIP link code" setup to MA to differenciate
*E301937,1               between Style WIP link code and Material WIP link code.
*E301949,1 TMI 08/05/2002 Enable/Disable "Open Ext.S.Scl screen" Depending if Ext S. Scl is enabled
*B037860,1 MHM 23/03/2004 The title option of the ‘History Company code’ should be ‘Active company 
*B037860,1                code ‘ in case you have selected history company ID at company information screen
*:************************************************************************
*
*E300633,1 Hesham El-Sheltawi (Start)
*E300633,1 change the parameters of the program
*PARAMETERS lcOGCmpName,lcTempName
PARAMETERS lcOGModle,lcOGCmpPath,lcOGCmpName &&,lcTempName
lcOGCmpPath = ALLT(lcOGCmpPath)
*E300633,1 (End)
*B601985,1 AMM increase size of array laogobjtype.
*DIMENSION laScrMode[5],laOGSeting[1,2],laOGObjType[1,3]
DIMENSION laScrMode[5],laOGSeting[1,2],laOGObjType[1,5]
*B601985,1 AMM end
STORE '' TO lcOGOldEsc, lnOGBarNo,lcOGoldKey 
STORE .F. to glFirsTime,LLFromObj
*E300633,1 Hesham El-Sheltawi (Start)
*E300633,1 create temproray file to hold the setups of the module or
*E300633,1 the company
PRIVATE lcTempName
lcTempName   = gfTempName()
lcOGFileName = SELECT()
*E301077,78 Hesham (Start)
llSetOpen = gfSysOpen(lcOGCmpPath+'SETUPS')
*E301077,78 Hesham (End)
SELECT *,RECNO() AS NRECNO,'S' AS CSTATUS ;
    FROM (lcOGCmpPath+'SETUPS') ;
    WHERE CAPP_ID = lcOGModle ;
    INTO DBF (gcWorkDir+lcTempName)
INDEX ON CAPP_ID+CFLD_NAME TAG MODVAR
INDEX ON CAPP_ID+STR(NVARPOS) TAG NVARPOS
*E300633,1 (End)
*laScrMode[3] = .T.
*******
lnObjNum=1
lnObjMax=7
lnMaxObj=0
lnScrolRow=IIF(_DOS,2,2)
lnOGStObj=0
lncount=1
lncolsize=25
DIMENSION laObjScrP[1,5]
lcListColor=SCHEME(6,3)
lcListColor=STRTRAN(SUBSTR(lcListColor,1,ATC('/',lcListColor)-1),'+','')+;
            SUBSTR(lcListColor,ATC('/',lcListColor))
lcXorColor=SCHEME(6,3)
lcXorColor=STRTRAN(SUBSTR(lcXorColor,ATC('/',lcXorColor)+1),'*','')+'/'+;
           STRTRAN(SUBSTR(lcXorColor,1,ATC('/',lcXorColor)-1),'+','')

******

IF !_DOS
  lcOGoldKey=SET('KEYCOMP')
  SET KEYCOMP TO DOS 
ENDIF  
lnOGSeting=1
*
lcOGTmpWin=''

lcOGOldEsc = SET('ESCAPE')
SET ESCAPE OFF
*****************************
*E301077,78 Hesham (Start)
llCnfgOpen = gfOpenFile(gcSysHome+'SYCCONFG','NVARPOS')
*IF !USED('SYCCONFG')
*  SELECT 0
*  USE &gcSysHome.SYCCONFG
*ELSE
*  SELECT SYCCONFG
*ENDIF
*SET ORDER TO TAG NVARPOS
*E301077,78 Hesham (End)

lcOGCmpID=PADR(ALLTRIM(lcOGCmpName),2)
lcOGCol1=SCHEME(5,2)
lcOGCol2=SCHEME(5,3)
lcOGCol3=SCHEME(5,2)
lcOGReadW='lfOGRWhen()'
*B601012,1 Add a condition to the READ valid so as not to exit
*B601012,1 the screen if at least one entry in the grid is not valid
*lcOGreadV='lfOGRValid()'
lcOGreadV = 'llOgTrmnat .AND. lfOGRValid()'
*B601012,1 end.
lcOGReadS='lfOGRShow()'
lcOGFltr=''
llOGFltCh = .T.
lcOGSeting=0
lcOGEntSet=''
lcOGEntVal=''
lcOGOldVal=''
PRIVATE ALL LIKE ??OG* 
DIMENSION laScrObj[1,5],laOGObjPos[1,5]
DIMENSION InvB[1],InvE[1],laOGHdFlt[1,7],laOGFxFlt[1,7],laOGVrFlt[1,7],;
          laOGFieldH[1,1],laOGFieldN[1,1],laOGWinObj[1],laSelFile[1,2],laOpenBy[1],;
          laOGObjCnt[1]
STORE 1 TO lnWinDisp,invb,inve,inv,puSeting,puSort,lnOGVarNo
STORE .T. TO llFirstTime,lcActWind,llOGVarFlt
STORE '' TO laOGHdFlt,laOGFxFlt,laOGVrFlt,laOGFieldH,laOGFieldN,laSelFile
lcOGExact=SET('EXACT')
SET EXACT OFF
STORE .F. TO laOpenBy,llOgTrmnat
lcElmSep='|'
lcValSep='~'
lnOGMax=7
lnOGFixFl=0
lnOGVarFl=0
lcOGWinTitl =''
lcOgTmExact=SET('EXACT')
SET EXACT OFF
*B037860,1 MHM 23/03/2004 Define new variable[Start]
STORE .F. TO  llOgHist 
*B037860,1 MHM 23/03/2004 [End]

*E300633,1 Hesham El-Sheltawi (Start)
*E300633,1 Change the relation between the sycconfg and the company setup file
*E300633,1 to use the variable Name
SELECT (lcTempName)
SET ORDER TO TAG MODVAR
SELECT SYCCONFG
SET ORDER TO TAG MODVAR
*SET RELATION TO lcOGCmpName+STR(NVARPOS) INTO &lcTempName
*SET RELATION TO STR(NVARPOS) INTO &lcTempName
SET RELATION TO lcOGModle+CFLD_NAME INTO &lcTempName
*E300633,1 (End)
GO TOP
=SEEK(lcOGModle)
SCAN WHILE CAPP_ID+CFLD_NAME = lcOGModle
  IF EOF(lcTempName)
    DIME laOGVarData[1]
    SCATTER MEMO TO laOGVarData 
    SELECT (lcTempName)
    APPEND BLANK
    GATHER FROM laOGVarData MEMO
    REPLACE NRECNO WITH 0,;
            CSTATUS WITH 'A'
    SELECT SYCCONFG
  ELSE
    REPLACE &lcTempName..NVARPOS WITH SYCCONFG.NVARPOS  
  ENDIF    
  lcOGVarName=STRTRAN(ALLTRIM(cfld_name),CHR(13)+CHR(10),'')
  lcOGDefTyp = cDefa_Typ
  IF EOF(lcTempName)
    lcOGValue=STRTRAN(mData_Def,CHR(13)+CHR(10),'')
  ELSE
    lcOGValue=STRTRAN(&lcTempName..mData_Def,CHR(13)+CHR(10),'')  
    lcOGDefTyp = &lcTempName..cDefa_Typ
  ENDIF  
  DO CASE
    CASE lcOGDefTyp='V'
      &lcOGVarName=lfTrnsStr(lcOGValue,cData_Typ)
    CASE lcOGDefTyp='E'
     &lcOGVarName=&lcOGValue
  ENDCASE
ENDSCAN
COUNT FOR CAPP_ID+CFLD_NAME = lcOGModle TO lnOGVariable 
SELECT (lcTempName)
SET ORDER TO TAG NVARPOS

SELECT SYCCONFG
SET ORDER TO TAG NVARPOS
SET RELATION TO lcOGModle+STR(NVARPOS) INTO &lcTempName
**
*IF EMPTY(lcOGModle)
IF CHR(255) $ lcOGModle
  lcOGWinTitl = 'Company '+lcOGCmpID+' Settings'
ELSE
  lcOGWinTitl = lcOGModle+' Setup'
ENDIF  

lnOGFixFl = 0
lnOGVarFl = 0

STORE 1 TO lnWinDisp,invb,inve,inv
llOGOK = .F.
IF (lnOGVariable+lnOGFixFl+lnOGVarFl)>0
  DO SYCMSET.spr
ENDIF 
*E300633,1 Hesham El-Sheltawi (Start)
*E300633,1 if the user select OK then save changes to
*E300633,1 the company setup file
IF llOGOK
  lcSetAlias = gfTempName()
  USE (lcOGCmpPath+'SETUPS') AGAIN ALIAS &lcSetAlias IN 0
  SELECT (lcSetAlias)
  SET ORDER TO TAG NVARPOS

  *E301469,1 AMH Update setups of History company[Start]
  IF !GfGetMemVar('LLHIST',laData[1]) AND !EMPTY(GfGetMemVar('M_COMP_ID',laData[1]))
    lcHistTemp = gfTempName()
    lcHstAlias = gfTempName()
    lnActAlias =  SELECT(0)
    SELECT (lcTempName)
    COPY TO (gcWorkDir+lcHistTemp)
    USE (gcWorkDir+lcHistTemp) IN 0
    SELECT (lcHistTemp)
    INDEX ON CAPP_ID+CFLD_NAME TAG MODVAR
    SET ORDER TO TAG MODVAR
    =SEEK(CHR(255)+CHR(255)+'LLHIST')
    REPLACE CSTATUS WITH 'S'
    =SEEK(CHR(255)+CHR(255)+'M_COMP_ID')
    REPLACE CSTATUS WITH 'S'
    LOCATE
    SELECT SYCCOMP
    =SEEK(ALLTRIM(GfGetMemVar('M_COMP_ID',laData[1])))
    lcHistComp = ALLTRIM(SYCCOMP.ccom_ddir)
    USE (lcHistComp+'SETUPS') AGAIN ALIAS &lcHstAlias IN 0
    SELECT (lcHstAlias)    
    SET ORDER TO TAG NVARPOS
    SELECT (lcHistTemp)
    SCAN
      IF SEEK(capp_id+STR(nvarpos),lcHstAlias)
        REPLACE nRecNo WITH RECNO(lcHstAlias)
      ELSE
        REPLACE CSTATUS WITH 'A'
      ENDIF
    ENDSCAN
    LOCATE
    SELECT (lcHstAlias)
    LOCATE
    =gfTmp2Mast(lcHstAlias,lcHistTemp,'Saving History settings...')  
    USE IN (lcHstAlias)
    IF USED(lcHistTemp)
      USE IN (lcHistTemp)
    ENDIF
    ERASE (gcWorkDir+lcHistTemp+'.DBF')
    ERASE (gcWorkDir+lcHistTemp+'.FPT')
    ERASE (gcWorkDir+lcHistTemp+'.CDX')
    SELECT SYCCOMP
    =SEEK(ALLTRIM(laData[1]))
    SELECT (lnActAlias)
  ENDIF 	
  *E301469,1 AMH [End]

  =gfTmp2Mast(lcSetAlias,lcTempName,'Saving company settings...')  
  USE IN (lcSetAlias)
ENDIF 
IF USED(lcTempName)
  USE IN (lcTempName)
ENDIF
ERASE (gcWorkDir+lcTempName+'.DBF')
ERASE (gcWorkDir+lcTempName+'.FPT')
ERASE (gcWorkDir+lcTempName+'.CDX')
*E300633,1 Hesham El-Sheltawi (End)
llFirstTime=.F.
SET EXACT &lcOGExact  
SET ESCAPE &lcOGOldEsc
*E301077,78 Hesham (Start)
=IIF(llCnfgOpen,gfSysClose('SYCCONFG'),'')
USE IN IIF(llSetOpen,'SETUPS',0)
*E301077,78 Hesham (End)
IF !_DOS
  SET KEYCOMP TO &lcOGoldKey
ENDIF  
SELECT(lcOGFileName)

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfOGShow
IF llFirstTime
  llFirstTime=.f.
  =lfActvateWin(1)
ENDIF
IF !laScrMode[2]
  SHOW GET pbOGReset ENABLE
  SHOW GET pbClose ENABLE
  *E301949,1 TMI [Start] Enable/Disable "Open Ext.S.Scl screen" 
  =lfOGShowGet('M_EXSS_SCR', gfGetMemVar('M_USEEXSSC') )
  *E301949,1 TMI [End  ] 
ELSE  
  SHOW GET pbOGReset DISABLE
  SHOW GET pbClose DISABLE
ENDIF
IF !EMPTY(lcOGReadS)
 =&lcOGReadS
ENDIF

FUNCTION lfOGShowGet
PARAMETERS lcVarName,llShow
lcWOutput=WOUTPUT()
ACTIVATE WINDOW gwdGridobj SAME
DO CASE
  CASE TYPE('lcVarName')='C'
    *B602889,1 Perform an exact search on the variable name to avoid
    *B602889,1 conflicts resulting from variables with close names
    *lnVarPos=ASCAN(laOGObjType,UPPER(lcVarName))
    lcSExact = SET('EXACT')
    SET EXACT ON
    lnVarPos=ASCAN(laOGObjType,UPPER(lcVarName))
    SET EXACT &lcSExact
    *B602889,1 end

    IF lnVarPos>0
     lnVarPos=ASUBSCRIPT(laOGObjType,lnVarPos,1)
    ELSE
      RETURN 
    ENDIF
  CASE TYPE('lcVarName')='L'
   lnVarPos=0
ENDCASE
IF lnVarPos>0
  IF TYPE('laOGObjType[lnVarPos,3]')='C'
    IF _DOS
      @(lnVarPos*2)-2,laObjScrP[lfObjPos('(LCOGVARNAME)'),2]  SAY &laOGObjType[lnVarPos,3] SIZE 1,25
    ELSE
      &laOGObjType[lnVarPos,2]=&laOGObjType[lnVarPos,3]    
    ENDIF
  ENDIF
  IF PARAMETERS()>1
    laOGObjCnt[lnVarPos]=llShow
  ENDIF
  IF laOGObjCnt[lnVarPos]
      SHOW OBJECT lnVarPos ENABLE
  ELSE
      SHOW OBJECT lnVarPos DISABLE
  ENDIF  
ELSE
  PRIVATE lnCount
  lnVarPos=lfObjPos('(LCOGVARNAME)')
  FOR lnCount=1 TO lnMaxObj
    IF TYPE('laOGObjType[lnCount,3]')='C'
      IF _DOS
        @(lnCount*2)-2,laObjScrP[lnVarPos,2]  SAY &laOGObjType[lnCount,3] SIZE 1,25
      ELSE  
        &laOGObjType[lnCount,2]=&laOGObjType[lnCount,3]
      ENDIF  
    ENDIF  
    IF PARAMETERS()>1
      laOGObjCnt[lnCount]=llShow
    ENDIF    
    IF laOGObjCnt[lnCount]
       SHOW OBJECT lnCount ENABLE
    ELSE
      SHOW OBJECT lnCount DISABLE
    ENDIF    
  ENDFOR
ENDIF
IF !EMPTY(lcWOutput)
  ACTIVATE WINDOW (lcWOutput)
ENDIF
*!********************************************************************
*!
*!              Function: lfDefWin
*!
*!********************************************************************
*
FUNCTION lfDefWin
PARAMETER lnOGBar
IF llFirstTime
  WAIT 'Generating Settings Grid...' WINDOW NOWAIT
ENDIF  
    lcDefa = ' '
IF  lnOGBar<=lnOGVariable AND lnOGVariable>0
   =lfDefVariable(lnOGBar)
ENDIF


*!********************************************************************
*!
*!              Function: lfActWin
*!
*!********************************************************************
*
FUNCTION lfActWin
=lfActvateWin(1)


*!********************************************************************
*!
*!              Function: lfActvateWin
*!
*!********************************************************************
*
FUNCTION lfActvateWin
PARAMETERS lnWinNo
       FOR lnOGCount= 1 TO ALEN(laOGObjCnt,1)
         IF laOGObjCnt[lnOGCount]              
           SHOW OBJECT lnOGCount ENABLE
         ELSE
           SHOW OBJECT lnOGCount DISABLE
         ENDIF
      ENDFOR                
RETURN
lnOGStart=IIF(lnWinNo>1,laOGWinObj[lnWinNo-1]+1,1)
lnOGEnd  =IIF(lnWinNo=CEILING((lnOGVariable+lnOGFixFl+lnOGVarFl)/lnOGMax),;
              ALEN(laOGObjCnt,1),laOGWinObj[lnWinNo])
llOGCanAcc=.F.              
FOR lnOGCount= lnOGStart TO lnOGEnd
  IF laOGObjCnt[lnOGCount]              
    llOGCanAcc=.T.
    EXIT
  ENDIF
ENDFOR           
IF llOGCanAcc     
  lnWinDisp=lnWinNO
  FOR ln=1 TO CEILING((lnOGVariable+lnOGFixFl+lnOGVarFl)/lnOGMax)
    IF ln=lnWinNo
       lnOGStart=IIF(lnWinNo>1,laOGWinObj[lnWinNo-1]+1,1)
       lnOGEnd  =IIF(lnWinNo=CEILING((lnOGVariable+lnOGFixFl+lnOGVarFl)/lnOGMax),;
                       ALEN(laOGObjCnt,1),laOGWinObj[lnWinNo])
       FOR lnOGCount= lnOGStart TO lnOGEnd
         IF laOGObjCnt[lnOGCount]              
           SHOW OBJECT lnOGCount ENABLE
         ELSE
           SHOW OBJECT lnOGCount DISABLE
         ENDIF
       ENDFOR                
      SHOW WINDOW (laOGWinName[ln]) TOP
    ELSE
      SHOW GETS WINDOW (laOGWinName[ln]) DISABLE ONLY
    ENDIF  
  ENDFOR  
  =lfModiScrol()
ENDIF  




*!********************************************************************
*!
*!              Function: lfDefVariable
*!
*!********************************************************************
*
FUNCTION lfDefVariable
PARAMETERS lnOGObjNum
lnOGObjPos=MOD(lnOGObjNum,lnOGMax)
lnOGObjPos=IIF(lnOGObjPos=0,lnOGMax,lnOGObjPos)
lnOGChar=lnOGObjPos
lnOGObjPos=(lnOGObjPos-1)*4+1
lnOGNumObj=0
STORE '' TO lcValid,lcWhen,lcDefa
SELECT SYCCONFG
LOCATE FOR CAPP_ID+STR(nVarPos)=lcOGModle+STR(lnOGObjNum)
IF FOUND()

  lcProm=cFld_Head
  
  *B037860,1 MHM 23/03/2004 Check for history company or not to get new description[Start]
  IF ALLTRIM(SYCCONFG.cfld_name) = 'LLHIST'
    llOgHist = LLHIST  
  ENDIF
  IF ALLTRIM(SYCCONFG.cFld_Head) = "History Company Code" .AND. llOgHist
    lcProm = "Active Company Code"
  ENDIF
  *B037860,1 MHM 23/03/2004 [End]

  lcOper='Is '
  lcOGVarName=UPPER(STRTRAN(ALLTRIM(cfld_name),CHR(13)+CHR(10),''))
*  IF !EMPTY(SET('PROC')) AND ALIAS()<>'SYDFIELD'
    *E301433,1 Ramy Change this line to add the calling of the LFVY2K() function to the valid 
    *               string to of the date objects
    *lcValid=IIF(EMPTY(MVAld_Str),'','Valid '+ALLTRIM(MVAld_Str))
    lcValid=IIF(EMPTY(MVAld_Str) , IIF(cData_Typ = 'D' , 'Valid lfvY2k()' , '') , IIF(cData_Typ = 'D' , 'Valid lfvY2k() .AND. ' , 'Valid ') + ALLTRIM(MVAld_Str))
    *E301433,1 Ramy [end]
    lcValid=STRTRAN(lcValid,'{{HPOS}}',ALLTRIM(STR(lnOGObjPos)))  
    lcWhen=IIF(ATC('WHEN ',lcValid)>0,SUBSTR(lcValid,ATC('WHEN ',lcValid)),'')
    lcValid=STRTRAN(lcValid,lcWhen,'')
*  ENDIF  
  lcWhen=IIF(!EMPTY(lcWhen),'WHEN '+'lfWhen() AND '+STRTRAN(UPPER(lcWhen),'WHEN '),'WHEN lfWhen()')
  laOGObjType[lnCount,1]=lcOGVarName
  laOGObjType[lnCount,2]=lcOGVarName
  laogobjtype[lnCount,3]=.f.
  *B601985,1 AMM store the field width and decimal point in the array
  laOGObjType[lnCount,4]=nFld_wdth
  laOGObjType[lnCount,5]=nFld_Dec
  *B601985,1 AMM end
  *lcOGVarName
  lcDefa=''
  DO CASE
    CASE cData_Typ = 'D'
      lcPict = [{  /  /  }]
      *lcValid+' '+lcWhen
         lnOGNumObj=1
         lnColSize=10
    CASE cData_Typ = 'L'
      IF COBJ_TYPE='C'
          lcOGPrmpt=IIF(EMPTY(mObjPrmpt),IIF(EMPTY(cfld_Head),lcOGVarName,cfld_Head),mObjPrmpt)      
          lcPict="@*C "+lcOGPrmpt
          lcOper=' '
          lnColSize=20
          *+lcValid+' '+lcWhen
       ELSE   
          lcRowPos=IIF(_WINDOWS,'1',ALLTRIM(STR((lnOGObjNum*2)-2)))
          lcColPos=IIF(_WINDOWS,'1',ALLTRIM(STR(laObjScrP[lfObjPos('(LCOGVARNAME)'),2])))       
 
           lcOGValid=IIF(_WINDOWS,'lfVldWinPop("','lfVldEntPop("');
                      +lcOGVarName+'",'+'"L'+'"'+','+'"E'+'"'+','+'"';
                      +IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.');
                      +'"'+','+;
                      lcRowPos+','+lcColPos+;
                      ','+lcRowPos+;
                      ','+lcColPos+')'
  
                      
         lcOGValid=IIF(!EMPTY(lcValid),lcOGValid+' AND '+STRTRAN(UPPER(lcValid),'VALID ',''),lcOGValid)
         lnColSize=25
         IF !_WINDOWS           
           lcDefa=PADR(lfGetDef(lcOGVarName,'E',;
                                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                                )),24)+IIF(_DOS,'','')
           lcOGValid=STRTRAN(lcOGValid,CHR(13)+CHR(10),'')
           laOGObjType[lnCount,3]=[PADR(lfGetDef(']+lcOGVarName+[','E',']+;
                                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                                )+['),24)+'']
           lcOGVarName='INV'
           laOGObjType[lnCount,2]=lcOGVarName 

           lcPict="@*IVN "
           lcValid='VALID '+lcOGValid
           ELSE
             IF _WINDOWS
                 IF !EMPTY(INVB[1])
                   DIMENSION INVB[ALEN(INVB,1)+1]
                 ENDIF
                 lnOgBarNo=1
                lcPict="@^ " + lfGetPopPrm(lcOGVarName,'E',;
                                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                                ),.f.,'L')
                laOGObjType[lnCount,3]=[lfGetDef(']+lcOGVarName+[','E',']+;
                                IIF(!EMPTY(ALLTRIM(MvEntries)),ALLTRIM(MvEntries),'Yes'+lcElmSep+'No'+lcValSep+'.T.'+lcElmSep+'.F.';
                                )+[')]
                
                lcDefa=lnOgBarNo
                lcValid='VALID '+lcOGValid                 
                STORE lnOgBarNo TO INVB[ALEN(INVB,1)]                
                lcOGVarName='INVB['+ALLTRIM(STR(ALEN(INVB,1)))+']'                                
                laOGObjType[lnCount,2]=lcOGVarName 
             ENDIF             
          ENDIF   
        ENDIF  
         lnOGNumObj=1 &&IIF(_WINDOWS,2,1)
    CASE cData_Typ = 'N'
      IF COBJ_TYPE='P'
         lcOGPrmpt=ALLTRIM(IIF(EMPTY(mObjPrmpt),IIF(EMPTY(cfld_Head),lcOGVarName,cfld_Head),mObjPrmpt))      
          lcPict="@*HN "+lcOGPrmpt
          lnColSize=25
*          len(STRTRAN(STRTRAN(STRTRAN(lcOGPrmpt,'\<'),'\!'),'\?'))+1
          *+lcValid+' '+lcWhen
      ELSE
       DO CASE
         CASE lVldEntry OR !EMPTY(mVEntries)
          lcCodes_Fld=cCodes_Fld
          lcRowPos=IIF(_WINDOWS,'1',ALLTRIM(STR((lnOGObjNum*2)-2)))
          lcColPos=IIF(_WINDOWS,'1',ALLTRIM(STR(laObjScrP[lfObjPos('(LCOGVARNAME)'),2])))
          lcOGValid=IIF(_WINDOWS,'lfVldWinPop("','lfVldEntPop("');
                     +lcOGVarName+'",'+'"N'+'"'+','+'"'+IIF(lVldEntry,'C','E')+'"'+','+'"';
                      +IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+'"'+','+;
                      lcRowPos+','+lcColPos+;
                      ','+lcRowPos+;
                      ','+lcColPos+')'
                                            
         lcOGValid=IIF(!EMPTY(lcValid),lcOGValid+' AND '+STRTRAN(UPPER(lcValid),'VALID ',''),lcOGValid)           
         lcOGValid=STRTRAN(lcOGValid,CHR(13)+CHR(10),'')         
         lcValid='Valid '+lcOGValid
         lnColSize=25
         IF !_WINDOWS  
         lcPict="@*IVN "
         lcDefa=PADR(lfGetDef(lcOGVarName,IIF(lVldEntry,'C','E'),;
                                  IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))),24)+IIF(_DOS,'','')
          lcCodes_Fld=cCodes_Fld
            laOGObjType[lnCount,3]=[PADR(lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
                                  IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+['),24)+'']
           lcOGVarName='INV'
           laOGObjType[lnCount,2]=lcOGVarName 
           ELSE
                 IF !EMPTY(INVB[1])
                   DIMENSION INVB[ALEN(INVB,1)+1]
                 ENDIF
                 lcPict="@^ " + lfGetPopPrm(lcOGVarName,IIF(lVldEntry,'C','E'),;
                IIF(lVldEntry,lcCodes_Fld,ALLTRIM(mvEntries)),.F.,'C')
               laOGObjType[lnCount,3]=[lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
                                  IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+[')]                
                lcOGVarName='INVB['+ALLTRIM(STR(ALEN(INVB,1)))+']'                
                laOGObjType[lnCount,2]=lcOGVarName                 
                lcDefa=lnOgBarNo                
                 STORE lnOgBarNo TO INVB[ALEN(INVB,1)]              
          ENDIF 
         lnOGNumObj=1 &&IIF(_WINDOWS,2,1)
         CASE !EMPTY(MVAld_Str)
           lcPict=lfOGGetPct(cData_Typ)
           lnColSize=lfGetSize(25)           
           lnOGNumObj=1           
         OTHERWISE
           lcPict=lfOGGetPct(cData_Typ)
           lnColSize=lfGetSize(25)           
       ENDCASE      
       ENDIF
         lnOGNumObj=1         
    CASE cData_Typ = 'C'
       
       *B803949,1 HBG 18/02/2001 If the field is button keep te width of it as it is[Begin]
       IF COBJ_TYPE = 'P'
         lnRow = (1+(4*0.077))
       ELSE
         lnRow = 1
       ENDIF
       *B803949,1 [End]
  
       DO CASE
         CASE lVldEntry OR !EMPTY(mVEntries)
          lcRowPos=IIF(_WINDOWS,'1',ALLTRIM(STR((lnOGObjNum*2)-2)))
          lcColPos=IIF(_WINDOWS,'1',ALLTRIM(STR(laObjScrP[lfObjPos('(LCOGVARNAME)'),2])))
         lcCodes_Fld=cCodes_Fld
           lcOGValid=IIF(_WINDOWS,'lfVldWinPop("','lfVldEntPop("');
                     +lcOGVarName+'",'+'"C'+'"'+','+'"'+IIF(lVldEntry,'C','E')+'"'+','+'"';
                      +IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+'"'+','+;
                      lcRowPos+','+lcColPos+;
                      ','+lcRowPos+;
                      ','+lcColPos+')'
                      
                      
         lcOGValid=IIF(!EMPTY(lcValid),lcOGValid+' AND '+STRTRAN(UPPER(lcValid),'VALID ',''),lcOGValid)           
         lcValid='Valid '+lcOGValid
         lnColSize=25

         IF !_WINDOWS  
            lcDefa=PADR(lfGetDef(lcOGVarName,IIF(lVldEntry,'C','E'),;
                                      IIF(lVldEntry,lcCodes_Fld,ALLTRIM(mvEntries))),24)+IIF(_DOS,'','')

            laOGObjType[lnCount,3]=[PADR(lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
                                  IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+['),24)+'']
           lcOGVarName='INV'
           laOGObjType[lnCount,2]=lcOGVarName 
         lcPict="@*IVN "
           ELSE
             IF !EMPTY(INVB[1])
               DIMENSION INVB[ALEN(INVB,1)+1]
             ENDIF
                 lcPict="@^ " + lfGetPopPrm(lcOGVarName,IIF(lVldEntry,'C','E'),;
                IIF(lVldEntry,lcCodes_Fld,ALLTRIM(mvEntries)),.F.,'C')
               laOGObjType[lnCount,3]=[lfGetDef(']+lcOGVarName+[',']+IIF(lVldEntry,'C','E')+[',']+;
                                  IIF(lVldEntry,lcCodes_Fld,ALLTRIM(MvEntries))+[')]                
                lcOGVarName='INVB['+ALLTRIM(STR(ALEN(INVB,1)))+']'                
                lcValid='Valid '+lcOGValid
                lcDefa=lnOgBarNo
                laOGObjType[lnCount,2]=lcOGVarName              
                STORE lnOGBarNo TO INVB[ALEN(INVB,1)]                            
          ENDIF 
         lnOGNumObj=1 &&IIF(_WINDOWS,2,1)
         CASE !EMPTY(MVAld_Str)
           lcPict=lfOGGetPct(cData_Typ)
           lnColSize=lfGetSize(25)           
           lnOGNumObj=1           
         OTHERWISE
           lcPict=lfOGGetPct(cData_Typ)
           lnColSize=lfGetSize(25)           
       ENDCASE
    CASE cData_Typ='M'
       lcPict=lfOGGetPct(cData_Typ)
       lnColSize=lfGetSize(25)       
       lnOGNumObj=1       
  ENDCASE 
  lnOGNumObj=1        
ENDIF     
RETURN lnOGNumObj

FUNCTION lfOGReset
PRIVATE lcOldAlias
lcOldAlias=SELECT()
SELECT SYCCONFG
SET ORDER TO TAG NVARPOS
GO TOP
SCAN 
  lcOGVarName=STRTRAN(ALLTRIM(cfld_name),CHR(13)+CHR(10),'')
  lcOGValue=STRTRAN(mData_Def,CHR(13)+CHR(10),'')
  *E301469,1 AMH Dont reset the history company code [Start]
  IF lcOGVarName = 'M_COMP_ID'
    LOOP
  ENDIF
  *E301469,1 AMH [End]
  DO CASE
    CASE cDefa_Typ='V'
      &lcOGVarName=lfTrnsStr(lcOGValue,cData_Typ)
    CASE cDefa_Typ='E'
     &lcOGVarName=&lcOGValue
  ENDCASE
ENDSCAN
llCUpDate = .T.
SELECT (lcOldAlias)
*B601911,1 Enable all objects if 'Reset'ing
*=lfOGShowGet()
=lfOGShowGet(.F., .T.)
*B601911,1 end
=lfOGShow()

FUNCTION lfvOGCancel
llOGTrmnat = .T.
llOGOK = .F.
RETURN 

FUNCTION lfvOGClose

*E301469,1 AMH 11/14/2000 Restor the edit mode when save [Start]
IF GfGetMemVar('LLHIST',laData[1])
  *laScrMode    = .F.
  *laScrMode[3] = .T.
ENDIF
*E301469,1 AMH [End]
llOgTrmnat=.t.
IF !laScrMode[2]
  *B601012,1 Call lfVldSets() that checks for the validity of the
  *B601012,1 entered values before closing.
  IF lfVldSets()
    llOGOK = .T.
  *B601012,1 end.
    SELECT (lcTempName)
    FOR lnCount = 1 TO lnOGVariables

*SICO    
*      IF SEEK(lcOGCmpID+STR(lnCount))
      IF SEEK(lcOGModle+STR(lnCount))
*SICO      
        *B601985,1 Call the function getstring with 2 new parameters 
        *B601985,1 to get the right decimal number. 
        *REPLACE cDefa_typ WITH 'V',;
                MDATA_DEF WITH getString(EVAL(laOGObjType[lnCount,1]),TYPE(laOGObjType[lnCount,1])),;
                CSTATUS WITH IIF(CSTATUS='S','M',CSTATUS)
		REPLACE cDefa_typ WITH 'V',;
                MDATA_DEF WITH getString(EVAL(laOGObjType[lnCount,1]),TYPE(laOGObjType[lnCount,1]),;
                laOGObjType[lnCount,4],laOGObjType[lnCount,5]),;
                CSTATUS WITH IIF(CSTATUS='S','M',CSTATUS)                
        
        *E300872,9 Use gfTraceKey when editing company\module settings
        *E301058,4 AMM Pass two new parameters to gfTrace to call it with the chosen company with its modules installed
        *=gfTraceKey('SETUPS', lcOGModle+STR(lnCount),cStatus)
        IF SEEK(lcOGCmpName,'SYCCOMP')
          lcInstMdl = SYCCOMP.mModlSet
        ELSE
          lcInstMdl = gcComp_Mdl
        ENDIF
        =gfTraceKey('SETUPS', lcOGModle+STR(lnCount),cStatus,lcOGCmpPath,lcInstMdl)
        *E301058,4 AMM end
        *E300872,9 end
        
        *B601985,1 end
      ENDIF
    ENDFOR
  *B601012,1 If entries are not valid, do not terminate.
  ELSE
    llOgTrmnat=.F.
  ENDIF  
  *B601012,1 end.
ELSE
  llCUpDate = .F.
ENDIF

*!*************************************************************************
*!
*!              Function: lfGetPic
*!
*!*************************************************************************
*  this function to return the picture of a field using the currunt record
*  fields of the cursor file
FUNCTION lfOGGetPct
PARAMETERS cLeftType
IF EMPTY(cPict_str)
  DO CASE 
    CASE  cLeftType='C' 
       RETURN REPLICATE('X',nFld_Wdth)
    CASE  cLeftType='N' 
      *B601985,1 Adjust the picture as the decimal point chr was excluded 
      *B601985,1 from the fixed number not the decimal number)
      *RETURN REPLICATE('9',NFld_WDTH-NFld_DEC)+IIF(NFld_DEC>0,'.','');
             +IIF(NFld_DEC>0,REPLICATE('9',NFld_DEC-1),'')
      RETURN IIF(NFld_DEC>0, REPLICATE('9',NFld_WDTH-NFld_DEC-1)+'.'+;
                		     REPLICATE('9',NFld_DEC),;
                			 REPLICATE('9',NFld_WDTH))
      *B601985,1 end

    CASE  cLeftType='M'
       RETURN REPLICATE('X',165)
  ENDCASE
ENDIF
llATSign= ATC('@',CPICT_STR)>0
llExclSign = ATC('!',CPICT_STR)>0
lcOGFunc=STRTRAN(CPICT_STR,'@')
lcOGFunc=STRTRAN(lcOGFunc,'!')
IF !EMPTY(CPICT_STR)
  lcOGFunc=EVAL(ALLTRIM(CPICT_STR))
ENDIF  

lcOGFunc=IIF(TYPE('lcOGFunc')='C',lcOGFunc,CPICT_STR)
*IIF(cLeftType $ 'CM',IIF(llAtSign,'@','')+;
                         IIF(llExclSign AND !llAtSign,'@! ',IIF(llExclSign,'@! ',''));
                          +ALLTRIM(lcOGFunc),'')
*lcPict=IIF(!INLIST(ALLTRIM(&lcOGArrName[lnOGArrPos,5]),'In List','Between'),EVAL(CPICT_STR),ALLTRIM(CPICT_STR))

lcPict=IIF(TYPE(CPICT_STR)<>'C',lcOGFunc,EVAL(CPICT_STR))
RETURN lcPict


*!*************************************************************************
*!
*!              Function: lfGetSize
*!
*!*************************************************************************
*  this function to return the size of a get field using the current record
* of the cursor file information
FUNCTION lfGetSize
PARAMETERS lnSize
IF EMPTY(NFld_WDTH)
  RETURN lnSize
ENDIF
RETURN MIN(IIF(_DOS OR _UNIX,nFld_Wdth,nFld_Wdth*1.33),lnSize)



*!********************************************************************
*!
*!              Function: lfVldEntPop
*!
*!********************************************************************
*
FUNCTION lfVldEntPop
PARAMETERS lcOgVarName,lcOGDataTyp,lcOGPopType,lcOGValidEnt,lnOGRow,lnOGCol,lnOGSR,lnOGSC,lcOGEmpVl
lcOGEmpHD=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,'',;
           SUBSTR(lcOGEmpVl,ATC(lcElmSep,lcOGEmpVl)+1)),'')
lcOGEmpVl=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,lcOGEmpVl,;
           SUBSTR(lcOGEmpVl,1,ATC(lcElmSep,lcOGEmpVl)-1)),'All')
lcOGEmpHD=IIF(lcOGDataTyp<>'C',lfTrnsStr(lcOGEmpHD,lcOGDataTyp),lcOGEmpHD)           
lnOGSR=IIF(TYPE('lnOGSR')<>'N',lnOGRow,lnOGSR)
lnOGSC=IIF(TYPE('lnOGSC')<>'N',lnOGCol,lnOGSC)
lcOGOldVal=&lcOGVarName
DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]      
STORE '' TO laOGVldEnt,laOGVldVal
DO CASE
  CASE lcOGPopType='C' 
    lcOGSelect=SELECT()
    lcOGFieldName = PADR(UPPER(lcOGValidEnt),10)
    DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]
    *E301073,1 AMM Codes file is now dbf file not a system file
    *SELECT cDiscrep FROM &gcSysHome.syccodes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &gcSysHome.syccodes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldVal    
    *E300789,1 Hesham (Start)  
    *SELECT cDiscrep FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    

    SELECT cDiscrep FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    SELECT cCode_No FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    

    *E300789,1 Hesham (End)
    *E301073,1 AMM end
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt),1],laOGVldVal[ALEN(laOGVldVal),1]  
    IF !EMPTY(lcOGEmpVl)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]      
      =AINS(laOgVldEnt,1)
      =AINS(laOgVldVal,1)  
      laOGVldEnt[1,1]=lcOGEmpVl
      laOGVldVal[1,1]=lcOGEmpHD
    ENDIF  
  CASE lcOGPopType = 'F'
    DIMENSION laOGVldFl[1,1]
    =gfSubStr(lcOGValidEnt,@laOGVldFl)
    lcOGFlPath=IIF(LEFT(laOGVldFl[1],2)='SY',gcSysHome,gcDataDir)      
    lcOGSelect=SELECT()
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    lcOGCond=IIF(ALEN(laOGVldFl,1)>3,'Where '+laOGVldFl[4],'')
    SELECT &laOGVldFl[2] FROM &lcOGFlPath.&laOGVldFl[1] ;
      INTO ARRAY laOGVldEnt;
      &lcOGCond
    SELECT &laOGVldFl[3] FROM &lcOGFlPath.&laOGVldFl[1];
      INTO ARRAY laOGVldVal;
      &lcOGCond          
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt),1],laOGVldVal[ALEN(laOGVldVal),1]      
    IF !EMPTY(lcOGEmpVl)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
      =AINS(laOgVldEnt,1)
      =AINS(laOgVldVal,1)  
      laOGVldEnt[1]=lcOGEmpVl
      laOGVldVal[1]=lcOGEmpHD    
    ENDIF  
  CASE lcOGPopType='E'
    IF !EMPTY(lcOGValidEnt)  
      DIMENSION laOGVldEnt[1,1]
      lcOGVldEnt=SUBSTR(lcOGValidEnt,1,ATC(lcValSep,lcOGValidEnt)-1)
        =gfSubStr(lcOGVldEnt,@laOGVldEnt,lcElmSep)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1),1]      
      DIMENSION laOGVldVal[1,1]
       =gfSubStr(SUBSTR(lcOGValidEnt,ATC(lcValSep,lcOGValidEnt)+1),@laOGVldVal,lcElmSep)
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]       
        FOR lnOGCount= 1 TO ALEN(laOGVldVal,1)
          laOGVldVal[lnOGCount,1]=lfTrnsStr(laOGVldVal[lnOGCount,1],lcOGDataTyp)
        ENDFOR
    ENDIF
ENDCASE
IF ALEN(laOGVldEnt,1)>1
*   lcWin=IIF(EMPTY(WPARENT()),WONTOP(),WPARENT())

* lnWlrow=WLROW(WPARENT(lcWinName))+IIF(WLROW(lcWinName)<0,WLROW(lcWinName),-WLROW(lcWinName))    
  lnWlrow=IIF(EMPTY(WPARENT(WPARENT())),1,-WLROW(WPARENT())+WLROW()+5)
  lnWlRow=IIF(lnogRow+lnWlRow+4>WROWS('GWDGRID'),lnWlRow-4,lnWlRow)
   DEFINE POPUP puOGTarget FROM lnOGRow+lnWlRow,lnOGCol TO lnOGRow+lnWlRow+IIF(_DOS,4,7),lnOGCol+30 SCROLL SHADOW IN WINDOW gwdGrid
*   IN WINDOW (lcOGWinowN)
   =lfFillPop('PUOGTARGET','laOGVldEnt')
   lnOGBarNo=0
   lnOGBarNo = ASCAN(laOGVldVal,&lcOGVarName,1)
   ON SELECTION POPUP PUOGTARGET DO lfVldEntVal with 'PUOGTARGET',PROMPT(),Bar(),lcOGVarName,lnOGRow,lnOGCol
   ACTIVATE POPUP PUOGTARGET  BAR IIF(lnOGBarNo>0,lnOGBarNo,1)   
   DEACTIVATE POPUP PUOGTARGET
   RELEASE POPUP PUOGTARGET
   IF !EMPTY(PROMPT())
     IF _DOS
       @ lnOGSR,lnOGSC SAY PADR(SUBSTR(PROMPT(),1,24),24)+IIF(_DOS,'','');
       COLOR (SCHEME(5,4))  
       SHOW OBJECT _CUROBJ      
     ELSE
       @ lnOGSR,lnOGSC SAY PADR(SUBSTR(PROMPT(),1,24),24);
       FONT "MS Sans Serif", 8;
       STYLE 'B';
       SIZE 1.144,20,0      
       SHOW OBJECT _CUROBJ
     ENDIF  
   ENDIF  
ENDIF
*IF lcOGOldVal=&lcOGVarName
*  RETURN .F.
*ENDIF

*!********************************************************************
*!
*!              Function: lfVldEntVal
*!
*!********************************************************************
*
FUNCTION lfVldEntVal
PARAMETERS lcOGPopName,lcOGSelection,lnOGBarNo,lcOGVarName,lnOGRow,lnOGCol
&lcOGVarName=laOGVldVal[lnOGBarNo]
DEACTIVATE POPUP PUOGTARGET
RELEASE POPUP PUOGTARGET




*!*************************************************************************
*!
*!              Function: lfFillPop
*!
*!*************************************************************************
*
FUNCTION lfFillPop
PARAMETERS lcPopName,lcArrName
RELEASE BAR ALL OF (lcpopname)
FOR lnCount=1 to ALEN(&lcArrName,1)
  IF !EMPTY(&lcArrName[lnCount,1])
    DEFINE BAR lnCount OF (lcPopName) PROMPT &lcArrName[lnCount,1]
  ENDIF
ENDFOR













*!*************************************************************************
*!
*!              Function: lfGetDef
*!
*!*************************************************************************
*
FUNCTION lfGetDef
PARAMETERS lcOGVarName,lcOGPopType,lcOGValEnt,lcOGEmpVl

lcOGEmpHD=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,'',;
           SUBSTR(lcOGEmpVl,ATC(lcElmSep,lcOGEmpVl)+1)),'')
lcOGEmpVl=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,lcOGEmpVl,;
           SUBSTR(lcOGEmpVl,1,ATC(lcElmSep,lcOGEmpVl)-1)),'All')
lcOGEmpHD=IIF(TYPE(lcOGVarName)<>'C',lfTrnsStr(lcOGEmpHD,TYPE(lcOGVarName)),lcOGEmpHD)
DO CASE
  CASE lcOGPopType='C'
    lcOGSelect=SELECT()
    lcOGFieldName = PADR(UPPER(lcOGValEnt),10)
    DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]
    *E301073,1 AMM Codes file is now dbf file not a system file
    *SELECT cDiscrep FROM &gcSysHome.syccodes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &gcSysHome.syccodes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldVal    
    *E300789,1 Hesham (Start)  
    *SELECT cDiscrep FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    

    SELECT cDiscrep FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    SELECT cCode_No FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    

    *E300789,1 Hesham (End)
    *E301073,1 AMM end
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt),1],laOGVldVal[ALEN(laOGVldVal),1]  
    IF !EMPTY(lcOGEmpVl)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]      
      =AINS(laOgVldEnt,1)
      =AINS(laOgVldVal,1)  
      laOGVldEnt[1,1]=lcOGEmpVl
      laOGVldVal[1,1]=lcOGEmpHD  
    ENDIF  
  CASE lcOGPopType = 'F'
    DIMENSION laOGVldFl[1,1]
    =gfSubStr(lcOGValEnt,@laOGVldFl)
    lcOGFlPath=IIF(LEFT(laOGVldFl[1],2)='SY',gcSysHome,gcDataDir)      
    lcOGSelect=SELECT()
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    lcOGCond=IIF(ALEN(laOGVldFl,1)>3,'Where '+laOGVldFl[4],'')
    SELECT &laOGVldFl[2] FROM &lcOGFlPath.&laOGVldFl[1] ;
      INTO ARRAY laOGVldEnt;
      &lcOGCond
    SELECT &laOGVldFl[3] FROM &lcOGFlPath.&laOGVldFl[1];
      INTO ARRAY laOGVldVal;
      &lcOGCond          
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)],laOGVldVal[ALEN(laOGVldVal)]  
    IF !EMPTY(lcOGEmpVl)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1],laOGVldVal[ALEN(laOGVldVal)+1]          
      =AINS(laOgVldEnt,1)
      =AINS(laOgVldVal,1)  
      laOGVldEnt[1]=lcOGEmpVl
      laOGVldVal[1]=lcOGEmpHD    
    ENDIF  
  CASE lcOGPopType='E'
    IF !EMPTY(lcOGValEnt)  
      DIMENSION laOGVldEnt[1,1]
      lcOGVldEnt=SUBSTR(lcOGValEnt,1,ATC(lcValSep,lcOGValEnt)-1)
        =gfSubStr(lcOGVldEnt,@laOGVldEnt,lcElmSep)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1),1]      
      DIMENSION laOGVldVal[1,1]
       =gfSubStr(SUBSTR(lcOGValEnt,ATC(lcValSep,lcOGValEnt)+1),@laOGVldVal,lcElmSep)
 *       DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]             
       lcDataTyp=TYPE(lcOGVarName)
 *      IF lcDataTyp $ 'CN'
 *        =AINS(laOgVldEnt,1)
 *        =AINS(laOgVldVal,1)  
 *        laOGVldEnt[1]=lcOGEmpVl
 *        laOGVldVal[1]=lcOGEmpHD          
 *      ENDIF  
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]       
        FOR lnOGCount=1 TO ALEN(laOGVldVal,1)
          laOGVldVal[lnOGCount,1]=IIF(TYPE('laOGVldVal[lnOGCount,1]')='C',lfTrnsStr(laOGVldVal[lnOGCount,1],lcDataTyp),;
                                       laOGVldVal[lnOGCount,1])
        ENDFOR
      ENDIF
ENDCASE

lnOGBarNo=0 
lnOGBarNo = ASCAN(laOGVldVal,&lcOGVarName,1)
lcOGToRet=IIF(lnOGBarNo>0,laOGVldEnt[lnOGBarNo],'')
RETURN IIF(_DOS,lcOGToRet,lnOGBarNo)


*!********************************************************************
*!
*!              Function: lfChangeMsg
*!
*!********************************************************************
*
FUNCTION lfChangeMsg
PARAMETERS lcOGMsg1,lcOGMsg2,llOGMsg,lnOGObjPos
@ laOGObjPos[lnOGObjPos+1,1],laOGObjPos[lnOGObjPos+1,2] SAY IIF(llOGMsg,lcOGMsg1,lcOGMsg2) ;
  SIZE 1,20
RETURN  




************************************************
*!********************************************************************
*!
*!              Function: lfInitValue
*!
*!********************************************************************
*
FUNCTION lfInitValue
PARAMETERS lcOGArrName,lcOGDir
FOR lnOGCount = 1 TO  ALEN(&lcOGArrName,1)
  IF &lcOGArrName[lnOGCount,1]<>'.OR.'
    IF &lcOGArrName[lnOGCount,7] = 'V' AND ;
      !INLIST(&lcOGArrName[lnOGCount,5],'In List','Between')
      DO CASE
        CASE &lcOGArrName[lnOGCount,3]='D'
          &lcOGArrName[lnOGCount,6] = IIF(lcOGDir='C',CTOD(&lcOGArrName[lnOGCount,6]),;
                                          DTOC(&lcOGArrName[lnOGCount,6]))        
        CASE &lcOGArrName[lnOGCount,3]='N'        
          &lcOGArrName[lnOGCount,6] = IIF(lcOGDir='C',VAL(&lcOGArrName[lnOGCount,6]),;
                                         ALLTRIM(STR(&lcOGArrName[lnOGCount,6])))          
        CASE &lcOGArrName[lnOGCount,3]='L'
          &lcOGArrName[lnOGCount,6] = IIF(lcOGDir='C',IIF(&lcOGArrName[lnOGCount,6]='.T.',.T.,.F.),;
                                         IIF(!(&lcOGArrName[lnOGCount,6]),'.F.','.T.'))
      ENDCASE
    ENDIF
  ENDIF   
ENDFOR



*!********************************************************************
*!
*!              Function: lfGetObjVal
*!
*!********************************************************************
*
FUNCTION lfGetObjVal
PARAMETERS lcValueVar,lcVarNam1,lcVarNam2,lcDataType
DIMENSION lcTemName[2]
STORE '' TO lcTemName
=gfSubStr(&lcValueVar,@lcTemName,lcElmSep)
IF ALEN(lcTemName,1)<2
 DIMENSION lcTemName[2]
 STORE '' TO lcTemName[2]
ENDIF
&lcVarNam1=lfTrnsStr(lcTemName[1],lcDataType)
&lcVarNam2=lfTrnsStr(lcTemName[2],lcDataType)


*!********************************************************************
*!
*!              Function: lfTrnsStr
*!
*!********************************************************************
*
FUNCTION lfTrnsStr
PARAMETERS lcValueStr,lcDataType,lcDirection
DO CASE
  CASE lcDataType $ 'CM'
     RETURN lcValueStr
  CASE lcDataType = 'N'
    *B601985,1  Set the decimal setting to the decimal digit of the number 
    *B601985,1  passed as a parameter.
    *RETURN VAL(lcValueStr)
    IF AT('.',lcValueStr) > 0
      lnSetDec = SET('DECIMALS')
      SET DECIMAL TO INT(LEN(lcValueStr)-AT('.',lcValueStr))
      lnValueStr = VAL(lcValueStr)
      SET DECIMALS TO lnSetDec
      RETURN lnValueStr
    ELSE
      RETURN VAL(lcValueStr)
    ENDIF
    *B601985,1 end
  CASE lcDataType='D'
     RETURN CTOD(lcValueStr)
  CASE lcDataType = 'L'
     RETURN IIF(UPPER(ALLTRIM(lcValueStr))='.F.',.F.,.T.)
ENDCASE



*!********************************************************************
*!
*!              Function: lfSaveUDefV
*!
*!********************************************************************
*
*FUNCTION lfSaveUDefV
*PARAMETERS lcOGArrName
*PRIVATE lcOGSelect
*lcOGSelect=SELECT()
*SELECT SYREPUVR
*DIMENSION &lcOGArrName[1,2] 
*STORE '' TO &lcOGArrName
*SELECT SYURPFLT
*SAVE  TO MEMO MUSRDVAR ALL LIKE L?RP*
*SELECT(lcOGSelect)


*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfwDown
IF !MDOWN()
  RETURN .F.
ENDIF

*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfVDown
_CUROBJ=_CUROBJ-1
KEYBOARD "{SPACEBAR}"


***************************************************
**    This function must be in the GQB 

***************************************************



*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfGetLastObj
PARAMETERS lnOGWinNo
lnOGStart=IIF(lnOGWinNo>1,laOGWinObj[lnOGWinNo-1]+1,1)
lnOGEnd  =IIF(lnOGWinNo=CEILING((lnOGVariable+lnOGFixFl+lnOGVarFl)/lnOGMax),;
              ALEN(laOGObjCnt,1),laOGWinObj[lnOGWinNo])
lnOGCanAcc=0              
FOR lnOGCount= lnOGEnd TO lnOGStart STEP -1
  IF laOGObjCnt[lnOGCount]              
    lnOGCanAcc=lnOGCount
    EXIT
  ENDIF
ENDFOR 
RETURN lnOGCanAcc          


*!********************************************************************
*!
*!              Function: lfGetPopPrm
*!
*!********************************************************************
*
FUNCTION lfGetPopPrm
PARAMETERS lcOgVarName,lcOGPopType,lcOGValidEnt,lcOGEmpVl,lcOGDataTyp,lcOGPopN
lcOGEmpHD=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,'',;
           SUBSTR(lcOGEmpVl,ATC(lcElmSep,lcOGEmpVl)+1)),'')
lcOGEmpVl=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,lcOGEmpVl,;
           SUBSTR(lcOGEmpVl,1,ATC(lcElmSep,lcOGEmpVl)-1)),'All')
*lcOGEmpHD=IIF(lcOGDataTyp<>'C',lfTrnsStr(lcOGEmpHD,lcOGDataTyp),lcOGEmpHD)           
lcOGOldVal=&lcOGVarName
DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]      
STORE '' TO laOGVldEnt,laOGVldVal
DO CASE
  CASE lcOGPopType='C' 
    lcOGSelect=SELECT()
    lcOGFieldName = PADR(UPPER(lcOGValidEnt),10)
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    *E301073,1 AMM Codes file is now dbf file not a system file
    *SELECT cDiscrep FROM &gcSysHome.syccodes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &gcSysHome.syccodes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldVal    
    *E300789,1 Hesham (Start)
    *SELECT cDiscrep FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    

    SELECT cDiscrep FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    SELECT cCode_No FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    

    *E300789,1 Hesham (End)
    *E301073,1 AMM end 
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
    =AINS(laOgVldEnt,1)
    =AINS(laOgVldVal,1)  
    laOGVldEnt[1]=lcOGEmpVl
    laOGVldVal[1]=lcOGEmpHD
  CASE lcOGPopType = 'F'
    DIMENSION laOGVldFl[1,1]
    =gfSubStr(lcOGValidEnt,@laOGVldFl)
    lcOGFlPath=IIF(LEFT(laOGVldFl[1],2)='SY',gcSysHome,gcDataDir)      
    lcOGSelect=SELECT()
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    lcOGCond=IIF(ALEN(laOGVldFl,1)>3,'Where '+laOGVldFl[4],'')
    SELECT &laOGVldFl[2] FROM &lcOGFlPath.&laOGVldFl[1] ;
      INTO ARRAY laOGVldEnt;
      &lcOGCond
    SELECT &laOGVldFl[3] FROM &lcOGFlPath.&laOGVldFl[1];
      INTO ARRAY laOGVldVal;
      &lcOGCond          
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
    =AINS(laOgVldEnt,1)
    =AINS(laOgVldVal,1)  
    laOGVldEnt[1]=lcOGEmpVl
    laOGVldVal[1]=lcOGEmpHD    
  CASE lcOGPopType='E'
    IF !EMPTY(lcOGValidEnt)  
      DIMENSION laOGVldEnt[1,1]
      lcOGVldEnt=SUBSTR(lcOGValidEnt,1,ATC(lcValSep,lcOGValidEnt)-1)
        =gfSubStr(lcOGVldEnt,@laOGVldEnt,lcElmSep)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1),1]      
      DIMENSION laOGVldVal[1,1]
       =gfSubStr(SUBSTR(lcOGValidEnt,ATC(lcValSep,lcOGValidEnt)+1),@laOGVldVal,lcElmSep)
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]       
    ENDIF
ENDCASE
IF !EMPTY(laOGVldEnt[1,1])
  lcOGPrompt=''
  FOR lnOGCount = 1 TO ALEN(laOGVldEnt,1)
    lcOGPrompt=lcOGPrompt+IIF(lnOGCount=1,'',';')+laOGVldEnt[lnOGCount,1]
  ENDFOR
ENDIF
 lnOGBarNo=0
 lnOGBarNo = ASCAN(laOGVldVal,GetString(&lcOGVarName,TYPE(lcOGVarName)),1)
 IF TYPE('lcOGPopN')<>'L'
   &lcOGPopN = MAX(lnOGBarNo,1)
 ENDIF
RETURN lcOGPrompt


*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfVldWinPop
PARAMETERS lcOgVarName,lcOGDataTyp,lcOGPopType,lcOGValidEnt,lnOGRow,lnOGCol,lnOGSR,lnOGSC,lcOGEmpVl
lcOGEmpHD=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,'',;
           SUBSTR(lcOGEmpVl,ATC(lcElmSep,lcOGEmpVl)+1)),'')
lcOGEmpVl=IIF(TYPE('lcOGEmpVl')='C',IIF(ATC(lcElmSep,lcOGEmpVl)=0,lcOGEmpVl,;
           SUBSTR(lcOGEmpVl,1,ATC(lcElmSep,lcOGEmpVl)-1)),'All')
lcOGEmpHD=IIF(lcOGDataTyp<>'C',lfTrnsStr(lcOGEmpHD,lcOGDataTyp),lcOGEmpHD)           
lnOGSR=IIF(TYPE('lnOGSR')<>'N',lnOGRow,lnOGSR)
lnOGSC=IIF(TYPE('lnOGSC')<>'N',lnOGCol,lnOGSC)
lcOGOldVal=&lcOGVarName
DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]      
STORE '' TO laOGVldEnt,laOGVldVal
DO CASE
  CASE lcOGPopType='C' 
    lcOGSelect=SELECT()
    lcOGFieldName = PADR(UPPER(lcOGValidEnt),10)
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    *E301073,1 AMM Codes file is now dbf file not a system file
    *SELECT cDiscrep FROM &gcSysHome.syccodes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &gcSysHome.syccodes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
      INTO ARRAY laOGVldVal    
    *E300789,1 Hesham (Start)
    *SELECT cDiscrep FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    *SELECT cCode_No FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND CCOMP_ID=gcAct_comp;
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    

    SELECT cDiscrep FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    SELECT cCode_No FROM &lcOGCmpPath.codes ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    

    *E300789,1 Hesham (End)
    *E301073,1 AMM end
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
    =AINS(laOgVldEnt,1)
    =AINS(laOgVldVal,1)  
    laOGVldEnt[1]=lcOGEmpVl
    laOGVldVal[1]=lcOGEmpHD
  CASE lcOGPopType = 'F'
    DIMENSION laOGVldFl[1,1]
    =gfSubStr(lcOGValidEnt,@laOGVldFl)
    lcOGFlPath=IIF(LEFT(laOGVldFl[1],2)='SY',gcSysHome,gcDataDir)      
    lcOGSelect=SELECT()
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    lcOGCond=IIF(ALEN(laOGVldFl,1)>3,'Where '+laOGVldFl[4],'')
    SELECT &laOGVldFl[2] FROM &lcOGFlPath.&laOGVldFl[1] ;
      INTO ARRAY laOGVldEnt;
      &lcOGCond
    SELECT &laOGVldFl[3] FROM &lcOGFlPath.&laOGVldFl[1];
      INTO ARRAY laOGVldVal;
      &lcOGCond          
    SELECT(lcOGSelect)  
    DIMENSION laOGVldEnt[ALEN(laOGVldEnt)+1,1],laOGVldVal[ALEN(laOGVldVal)+1,1]  
    =AINS(laOgVldEnt,1)
    =AINS(laOgVldVal,1)  
    laOGVldEnt[1]=lcOGEmpVl
    laOGVldVal[1]=lcOGEmpHD    
  CASE lcOGPopType='E'
    IF !EMPTY(lcOGValidEnt)  
      DIMENSION laOGVldEnt[1,1]
      lcOGVldEnt=SUBSTR(lcOGValidEnt,1,ATC(lcValSep,lcOGValidEnt)-1)
        =gfSubStr(lcOGVldEnt,@laOGVldEnt,lcElmSep)
      DIMENSION laOGVldEnt[ALEN(laOGVldEnt,1),1]      
      DIMENSION laOGVldVal[1,1]
       =gfSubStr(SUBSTR(lcOGValidEnt,ATC(lcValSep,lcOGValidEnt)+1),@laOGVldVal,lcElmSep)
        DIMENSION laOGVldVal[ALEN(laOGVldVal,1),1]       
        FOR lnOGCount= 1 TO ALEN(laOGVldVal,1)
          laOGVldVal[lnOGCount,1]=lfTrnsStr(laOGVldVal[lnOGCount,1],lcOGDataTyp)
        ENDFOR
    ENDIF
ENDCASE
 
  lnOGBarNo=0
 lnOGBarNo = ASCAN(laOGVldVal,&lcOGVarName,1)
 IF LASTKEY()=13
  IF lnOGBarNo=EVAL(VARREAD())
    RETURN .F.
  ELSE
    &lcOGVarName=laOGVldVal[EVAL(VARREAD()),1]  
  ENDIF
 ELSE
   lcOGVar = VARREAD()
   &lcOGVar= MAX(lnOGBarNo,1)
   _CUROBJ=_CUROBJ+1
   RETURN .F.
ENDIF  



*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
*FUNCTION lfSaveTDefV
*PARAMETERS lcOGArrName
*PRIVATE lcOGSelect
*lcOGSelect=SELECT()
*SELECT SYREPUVR
*DIMENSION &lcOGArrName[1,2] 
*STORE '' TO &lcOGArrName
*SELECT cfld_name,'' ;
*  FROM &gcSysHome.SYREPUVR;
*  WHERE cComp_ID = PADR(lcOGCmpID,2);
*   AND cExpType = 'V';
*   AND lAskRunT = .T.;
*   INTO ARRAY &lcOGArrName   

*IF lnOGVariable>0
*  DIMENSION &lcOGArrName[lnOGVariable,2]    
*  FOR lnOGCount= 1 TO lnOGVariable
*    lcOGVarName = &lcOGArrName[lnOGCount,1]
*    lcOGValue= &lcOGVarName
*    &lcOGArrName[lnOGCount,2] = lcOGValue
*  ENDFOR  
*ENDIF  
*SELECT(lcOGSelect)



*!********************************************************************
*!
*!              Function: 
*!
*!********************************************************************
*
FUNCTION lfLoadTDefV
lcOGArrName = 'laOGUDefVr'
IF !EMPTY(mUsrDVar)
  RESTORE FROM MEMO mUsrDVar ADDI
  FOR lnOGCount=1 TO ALEN(&lcOGArrName,1)
    IF !EMPTY(&lcOGArrName[lnOGCount,1])
      lcOGVarName=&lcOGArrName[lnOGCount,1]
      lcOGValue=&lcOGArrName[lnOGCount,2]
      &lcOGVarName=lcOGValue
    ENDIF
  ENDFOR
  lnOGVariable=ALEN(laOGUdefVr,1)
ELSE
  lnOGVariable=0  
ENDIF  

**************************************************************************
**  FUNCTION getString
**************************************************************************

FUNCTION getString
*B601985,1 Add two parameters to convert numbers to strings with its 
*B601985,1 decimal point.
*PARAMETERS lcValue,lcDataType
PARAMETERS lcValue,lcDataType, lnNum, lnDec
*B601985,1  end
DO CASE
  CASE lcDataType $ 'CM'
      RETURN lcValue
  CASE lcDataType = 'N'
      *B601985,1 If lnNum, and lnDec are passed, use them to get the correct
      *B601985,1 format of the numeric field
      *RETURN ALLTRIM(STR(lcValue))
      IF TYPE('lnNum')='N' AND TYPE('lnDec')='N'
        RETURN ALLTRIM(STR(lcValue,lnNum,lnDec))
      ELSE
        RETURN ALLTRIM(STR(lcValue))
      ENDIF
      *B601985,1 end
  CASE lcDataType = 'L'
    RETURN IIF(lcValue=.T.,'.T.','.F.')
  CASE lcDataType = 'D'    
    RETURN CTOD(lcValue)
ENDCASE  

************************ SCROLL CONTROLING
****************************************

FUNCTION lfWhen
IF BETWEEN(_CUROBJ-lnOGStObj,lnObjNum,lnObjMax) 
  LLFromObj= .F.
  RETURN  laOGObjCnt[_CUROBJ-lnOGStObj]
ELSE
  IF INLIST(LASTKEY(),5,4,19,24) AND !LLFromObj
      DO CASE
      CASE lnObjMax<_CUROBJ-lnOGStObj
        IF laOGObjCnt[_CUROBJ-lnOGStObj]=.T.               
*          MOVE WINDOW gwdgridobj BY -2,0
          lnObjMax=_CUROBJ-lnOGStObj        
          lnObjNum=lnObjMax-6
          MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0                    
          =lfModiHScrol()   
       ENDIF  
       RETURN  laOGObjCnt[_CUROBJ-lnOGStObj]
      CASE lnObjNum>_CUROBJ-lnOGStObj
        IF laOGObjCnt[_CUROBJ-lnOGStObj]=.T.                     
*          MOVE WINDOW gwdgridobj BY 2,0  
          lnObjNum=_CUROBJ-lnOGStObj
          lnObjMax=lnObjNum+6
          MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0                    
          =lfModiHScrol()   
        ENDIF  
       RETURN  laOGObjCnt[_CUROBJ-lnOGStObj]
    ENDCASE
  ELSE
    RETURN .F.  
  ENDIF 
ENDIF

FUNCTION lfPageUp
IF LASTKEY()=18 
  lnOldObjN=lnObjNum
  lnObjNum=IIF((lnObjNum-6)<=1,1,lnObjNum-6)
  IF lnObjNum<>lnOldObjN
    MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0  
    lnObjMax=lnObjNum+6
    =lfModiHScrol()   
   *B601802,1 Hesham El_Sheltawi (Start)
   *B601802,1 check if the mode is !view mode before
   *B601802,1 forcing the screen to go to an object of the grid
   IF !laScrMode[2]
     _CUROBJ=lnObjNum+lnOGStObj
   ENDIF 
   *B601802,1 Hesham El_Sheltawi (End) 
  ENDIF
ELSE
   LLFromObj= .T.    
ENDIF  
RETURN .F.

FUNCTION lfPageDn
IF LASTKEY()=3 AND lnMaxObj>7
  lnOldObjN=lnObjNum
  lnObjNum=IIF((lnObjNum+6)>=lnMaxObj-6,lnMaxObj-6,lnObjNum+6)
  IF lnObjNum<>lnOldObjN  
    MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0  
    lnObjMax=lnObjNum+6
    =lfModiHScrol()   
   *B601802,1 Hesham El_Sheltawi (Start)
   *B601802,1 check if the mode is !view mode before
   *B601802,1 forcing the screen to go to an object of the grid
    IF !laScrMode[2]
      _CUROBJ=lnObjNum+lnOGStObj
    ENDIF 
    *B601802,1 Hesham El_Sheltawi (End)
  ENDIF  
ELSE
 LLFromObj= .T.      
ENDIF  
RETURN .F.
*!********************************************************************
*!
*!              Function: lfScrollup
*!
*!********************************************************************
*
FUNCTION lfScrollUP
lnObjNo1=lfObjPos('IBSCRUP')
IF !MDOWN() OR !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
DO WHILE MDOWN() AND lnObjNum>1 AND BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  lnObjNum = lnObjNum-1
  lnObjMax=lnObjNum+6
   MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0  
   =lfModiHScrol()   
ENDDO
*B601802,1 Hesham El_Sheltawi (Start)
*B601802,1 check if the mode is !view mode before
*B601802,1 forcing the screen to go to an object of the grid
IF !laScrMode[2]
  _CUROBJ=lnObjNum+lnOGStObj
ENDIF 
*B601802,1 Hesham El_Sheltawi (End)


*!********************************************************************
*!
*!              Function: lfScrollDn
*!
*!********************************************************************
*
FUNCTION lfScrollDn
lnObjNo1=lfObjPos('IBSCRDN')
IF !MDOWN() OR !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
DO WHILE MDOWN() AND lnObjNum+6<lnMaxObj AND BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL(),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4])
  lnObjNum = lnObjNum+1
  lnObjMax=lnObjNum+6
   MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0  
   =lfModiHScrol()
ENDDO
*B601802,1 Hesham El_Sheltawi (Start)
*B601802,1 check if the mode is !view mode before
*B601802,1 forcing the screen to go to an object of the grid
IF !laScrMode[2]
  _CUROBJ=lnObjNum+lnOGStObj
ENDIF 
*B601802,1 Hesham El_Sheltawi (End)



*!********************************************************************
*!
*!              Function: lfScrollVer
*!
*!********************************************************************
*
FUNCTION lfScrollVer
lnObjNo1=lfObjPos('IBSCRHBAR')
IF !MDOWN() !(BETWEEN(MROW(),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]);
   AND BETWEEN(MCOL("gwdOGscrl"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]))
  RETURN .F.
ENDIF
lnCurCol=MROW("gwdOGscrl")
lnWCurPos=MAX(CEILING((MROW("gwdOGscrl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                 ((lnMaxObj-6)))),1)

DO CASE 
  CASE IIF(_DOS,lnCurCol<>lnScrolRow,lnWCurPos<>lnObjNum)
    lnSign=IIF(lnWCurPos>lnObjNum,1,-1)                 
    DO WHILE MDOWN() 
      IF BETWEEN(MCOL("gwdOGscrl"),laObjScrP[lnObjNo1,2],laObjScrP[lnObjNo1,4]) AND;
         BETWEEN(MROW("gwdOGscrl"),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3])
        lnWCurPos=MAX(CEILING((MROW("gwdOGscrl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                  ((lnMaxObj-6)))),1)
        lnSign2=IIF(lnWCurPos>lnObjNum,1,-1)                             
       IF lnSign=lnSign2
         lnObjNum=lnObjNum+(lnSign*IIF(ABS(lnWCurPos-lnObjNum)>1,1,ABS(lnWCurPos-lnObjNum)))
         lnObjMax=lnObjNum+6                
         MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0
         =lfModiHScrol()        
        ENDIF  
     ENDIF  
       =INKEY(.01,'HM')
   ENDDO
 OTHERWISE
    lnTmpCol=lnScrolROW
        lnWCurPos=MAX(CEILING((MROW("gwdOGscrl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                  ((lnMaxObj-6)))),1)
    =lfVerCurs()                                
    DO WHILE MDOWN() 
      IF (BETWEEN(MROW("gwdOGscrl"),laObjScrP[lnObjNo1,1],laObjScrP[lnObjNo1,3]))
        lnWCurPos=MAX(CEILING((MROW("gwdOGscrl")-laObjScrP[lnObjNo1,1])/((laObjScrP[lnObjNo1,3]-laObjScrP[lnObjNo1,1])/ ;
                  ((lnMaxObj-6)))),1)
         =lfVerCurs()                                
         =INKEY(.01,'HM')         
     ENDIF  
       =INKEY(.01,'HM')     
   ENDDO    
     IF lnObjNum<>lnWcurPos
       lnObjNum=lnWCurPos
       lnObjMax=lnObjNum+6       
       lnScrolRow=lnTmpCol
       MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0
       =lfModiHScrol()    
     ENDIF      
ENDCASE   
*B601802,1 Hesham El_Sheltawi (Start)
*B601802,1 check if the mode is !view mode before
*B601802,1 forcing the screen to go to an object of the grid
IF !laScrMode[2]
  _CUROBJ=lnObjNum+lnOGStObj
ENDIF 
*B601802,1 Hesham El_Sheltawi (End)



*********************************************************************

*!********************************************************************
*!
*!              Function: lfModiHScrol
*!
*!********************************************************************
*
FUNCTION lfModiHScrol
  lnObjPos=lfObjPos('IBSCRHBAR')
  DO CASE
    CASE _DOS   
      lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnMaxObj-7))*(lnObjNum-1)+laObjScrP[lnObjPos,1]))
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdOGscrl SAME
         @ lnPos,laObjScrP[lnObjPos,2] SAY 'Û' COLOR &lcListColor
         @ lnScrolRow,laObjScrP[lnObjPos,2] SAY '' COLOR &lcXorColor
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
     =INKEY(.01,'HM')        
    CASE _WINDOWS
     lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],MIN(laObjScrP[lnObjPos,3]-1.00,;
                 FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnMaxObj-7)-1)*(lnObjNum-1)+laObjScrP[lnObjPos,1])))
                 
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdOGscrl SAME
         @ lnPos,laObjScrP[lnObjPos,2] CLEAR TO lnPos+1.000+2*0.077,laObjScrP[lnObjPos,4] &&-0.088         

         
        @ lnScrolRow,laObjScrP[lnObjPos,2] TO lnScrolRow+1.00,laObjScrP[lnObjPos,2];
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(255,255,255,255,255,255)		           	
                  
         @ lnScrolRow,laObjScrP[lnObjPos,2] TO lnScrolRow,laObjScrP[lnObjPos,4]-0.500+3*0.077;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(255,255,255,255,255,255)		           	         
         
         @ lnScrolRow+1.00,laObjScrP[lnObjPos,2] TO lnScrolRow+1.00,laObjScrP[lnObjPos,4]-0.500+6*0.077;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(128,128,128,128,128,128)
         
         @ lnScrolRow,laObjScrP[lnObjPos,4]-0.500+3*0.077 TO lnScrolRow+1.00,laObjScrP[lnObjPos,4]-0.500+3*0.077;
         PEN 2, 8 ;
         STYLE "1";
         COLOR RGB(128,128,128,128,128,128)		  
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
  ENDCASE        


*!*************************************************************************
*!
*!              Function: lfObjPos
*!
*!*************************************************************************
*  This function to return an object position in the array that defined in
* the screen layout 'setup' to hold the position of the screen objects and
* the names to and this function is to return the row # that have the data
* of this object
FUNCTION lfObjPos
PARAMETERS lcOGObjNam
PRIVATE lnCount
llOGFound=.f.
FOR lnCount=1 TO ALEN(laObjScrP,1)
  IF UPPER(laObjScrP[lnCount,5])=UPPER(lcOGObjNam)
    llOGFound=.t.
    EXIT  
  ENDIF
ENDFOR
RETURN IIF(llOGFound,lnCount,0)

         
         
         
*!********************************************************************
*!
*!              Function: lfVerCurs
*!
*!********************************************************************
*
FUNCTION lfVerCurs
  lnObjPos=lfObjPos('IBSCRHBAR')
  DO CASE
    CASE _DOS   
      lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnMaxObj-7))*(lnObjNum-1)+laObjScrP[lnObjPos,1]))

       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdOGscrl SAME
         @ lnPos,laObjScrP[lnObjPos,2] SAY 'Û' COLOR &lcListColor
         @ lnScrolRow,laObjScrP[lnObjPos,2] SAY '' COLOR &lcXorColor
       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF

     =INKEY(.01,'HM')        
    CASE _WINDOWS
     lnPos=lnScrolRow
      lnScrolRow=MAX(laObjScrP[lnObjPos,1],MIN(laObjScrP[lnObjPos,3]-1.00,;
                 FLOOR((laObjScrP[lnObjPos,3]-laObjScrP[lnObjPos,1]-1)/ ;
                 ((lnMaxObj-7)-1)*(lnWCurPos-1)+laObjScrP[lnObjPos,1])))
                 
     IF lnPos<>lnScrolRow
       STORE WOUTPUT() TO currwind
         ACTIVATE WINDOW gwdOGscrl SAME

         @ lnPos,laObjScrP[lnObjPos,2] TO lnPos+1.00+2*0.077,laObjScrP[lnObjPos,4]-0.500+6*0.077;
         	PEN 1, 1 ;
         	STYLE "T";
         	COLOR RGB(192,192,192,,,,)         	
         	

         @ lnScrolRow,laObjScrP[lnObjPos,2] TO lnScrolRow+1.00+2*0.077,laObjScrP[lnObjPos,4]-0.500+6*0.077;
         	PEN 1, 1 ;
         	STYLE "T";
         	COLOR RGB(255,255,255,,,,)         	    	
         	
         	


       IF NOT EMPTY(currwind)
         ACTIVATE WINDOW (currwind) SAME 
       ENDIF
    ENDIF 
  ENDCASE        
         

FUNCTION lfOGSetRelat
FOR lnCount = 1 TO ALEN(laSelFile,1)
  SELECT (laSelFile[lnCount,1])
  IF !EMPTY(laSelFile[lnCount,3])
    IF ATC(' DESC',laSelFile[lnCount,3])>0
       SET ORDER TO TAG &laSelFile[lnCount,3]
    ELSE
       SET ORDER TO TAG &laSelFile[lnCount,3] ASCENDING  
    ENDIF   
  ELSE
*    SET ORDER TO 
  ENDIF  
  SET FILTER TO &laSelFile[lnCount,6]
ENDFOR
FOR lnCount = 1 TO ALEN(laSelFile,1)
  SELECT (laSelFile[lnCount,1])
  lcRelation=STRTRAN(laSelFile[lnCount,4],CHR(255),',')
  IF !EMPTY(lcRelation)
    SET RELATION TO &lcRelation ADDI
  ELSE
    SET RELATION TO  
  ENDIF  
ENDFOR
FOR lnCount = 1 TO ALEN(laSelFile,1)
  SELECT (laSelFile[lnCount,1])
  IF !EMPTY(laSelFile[lnCount,5])
    SET SKIP TO (STRTRAN(laSelFile[lnCount,5],CHR(255)))
  ELSE
    SET SKIP TO  
  ENDIF  
ENDFOR
SELECT (laSelFile[1,1])
GO  TOP

FUNCTION lfOGClrRelat
FOR lnCount = 1 TO ALEN(laSelFile,1)
  SELECT (laSelFile[lnCount,1])
*  SET ORDER TO 
  SET FILTER TO
  SET RELATION TO 
  SET SKIP TO
ENDFOR


*!**************************************************************************
*! Name      : lfvGetLink
*! Developer : Adel Mohammed El Gazzar (ADEL)
*! Date      : 12/19/2001
*! Purpose   : Validate function for "Material WIP link code"  new stup
*!**************************************************************************
*! Passed Parameters : 
*!**************************************************************************
*! Notes : 
*!**************************************************************************
*--E301937,1
FUNCTION lfvGetLink

PRIVATE lnAlias
lnAlias = SELECT()
IF !USED('GL_Link')
  =SEEK(laData[1],'SycComp')  
  =gfOpenFile(ALLTRIM(SycComp.cCom_ddir)+'GL_Link',ALLTRIM(SycComp.cCom_ddir)+'GL_Link','SH')
ENDIF
IF !EMPTY(M_WIPLINK) AND !SEEK("05"+M_WIPLINK, "GL_Link"))
  IF !gfGLBrowse('05',@M_WIPLINK ) 
    M_WIPLINK  = ""
  ENDIF
ENDIF
SELECT(lnAlias)