*:************************************************************************
*: Program file  : sycmwz.PRG   N#000185
*: Program desc. : Company Setup option grid
*: For screen    :
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: Ahmed Salah Shalaby - (SSH) 
*:************************************************************************
*: Calls : 
*:         Procedures :
*:         Functions  : 
*:************************************************************************
*: Passed Parameters  : lcOGModle   && Module ID to change the setting for
*:                                  && if empty then we are changing the
*:                                  && Company settings
*:                      lcOGCmpPath && Company data path
*:                      lcOGCmpName && Company ID
*:************************************************************************
PARAMETERS lcOGModle,lcOGCmpPath,lcOGCmpName &&,lcTempName

lcOGCmpPath = ALLT(lcOGCmpPath)
DIMENSION laScrMode[5],laOGSeting[1,2],laOGObjType[1,5]
STORE '' TO lcOGOldEsc, lnOGBarNo,lcOGoldKey
STORE .F. to glFirsTime,LLFromObj

PRIVATE lcTempName
lcComeFrom   = ''
lcTempName   = gfTempName()
lcOGFileName = SELECT(0)
lcWzDir = FULLPATH("")+"WIZFILES"
SELECT *,RECNO() AS NRECNO,'S' AS CSTATUS ;
    FROM (gcSysHome+'SYWSETP') ;
    WHERE CAPP_ID = lcOGModle ;
    INTO DBF (gcWorkDir+lcTempName)
INDEX ON CAPP_ID+CFLD_NAME TAG MODVAR
INDEX ON CAPP_ID+STR(NVARPOS) TAG NVARPOS

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
lcOGTmpWin=''
lcOGOldEsc = SET('ESCAPE')
SET ESCAPE OFF
*****************************

llCnfgOpen = gfOpenFile(gcsyshome+'SYWCONFG','NVARPOS')

lcOGCmpID=PADR(ALLTRIM(lcOGCmpName),2)
lcOGCol1=SCHEME(5,2)
lcOGCol2=SCHEME(5,3)
lcOGCol3=SCHEME(5,2)
lcOGReadW='lfOGRWhen()'
lcOGreadV = 'llOgTrmnat .AND. lfOGRValid()'
*---lfOGRShow() SSH
lcOGReadS=''
*lcOGReadS='lfOGRShow()'
IF lcOGModle = "ÿÿ"
  lcOGReadS='lfInitGl() .AND. lfvMulCurr()'
ELSE
  lcOGReadS = IIF((lcOGModle = "IC"),"",'lfOGRShow()')
ENDIF
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
SELECT (lcTempName)
SET ORDER TO TAG MODVAR
SELECT SYWCONFG
SET ORDER TO TAG MODVAR
SET RELATION TO lcOGModle+CFLD_NAME INTO &lcTempName
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
    SELECT SYWCONFG
  ELSE
    REPLACE &lcTempName..NVARPOS WITH SYWCONFG.NVARPOS  
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

SELECT SYWCONFG
SET ORDER TO TAG NVARPOS
SET RELATION TO lcOGModle+STR(NVARPOS) INTO &lcTempName
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
  DO Sycmwz.spr
ENDIF 
IF llOGOK
  lcSetAlias = gfTempName()
  USE (gcSysHome+'SYWSETP') AGAIN ALIAS &lcSetAlias IN 0
  SELECT (lcSetAlias)
  SET ORDER TO TAG NVARPOS
  =gfTmp2Mast(lcSetAlias,lcTempName,'Saving company settings...')  
  USE IN (lcSetAlias)
ENDIF 
IF USED(lcTempName)
  USE IN (lcTempName)
ENDIF
ERASE (gcWorkDir+lcTempName+'.DBF')
ERASE (gcWorkDir+lcTempName+'.FPT')
ERASE (gcWorkDir+lcTempName+'.CDX')
llFirstTime=.F.
SET EXACT &lcOGExact  
SET ESCAPE &lcOGOldEsc
=IIF(llCnfgOpen,gfSysClose('SYWCONFG'),'')
USE IN SYWSETP
SET KEYCOMP TO &lcOGoldKey
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
    lcSExact = SET('EXACT')
    SET EXACT ON
    lnVarPos=ASCAN(laOGObjType,UPPER(lcVarName))
    SET EXACT &lcSExact
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
SELECT SYWCONFG
LOCATE FOR CAPP_ID+STR(nVarPos)=lcOGModle+STR(lnOGObjNum)
IF FOUND()
  lcProm=cFld_Head
  lcOper='Is '
  lcOGVarName=UPPER(STRTRAN(ALLTRIM(cfld_name),CHR(13)+CHR(10),''))
  lcValid=IIF(EMPTY(MVAld_Str) , IIF(cData_Typ = 'D' , 'Valid lfvY2k()' , '') , IIF(cData_Typ = 'D' , 'Valid lfvY2k() .AND. ' , 'Valid ') + ALLTRIM(MVAld_Str))
  lcValid=STRTRAN(lcValid,'{{HPOS}}',ALLTRIM(STR(lnOGObjPos)))  
  lcWhen=IIF(ATC('WHEN ',lcValid)>0,SUBSTR(lcValid,ATC('WHEN ',lcValid)),'')
  lcValid=STRTRAN(lcValid,lcWhen,'')
  lcWhen=IIF(!EMPTY(lcWhen),'WHEN '+'lfWhen() AND '+STRTRAN(UPPER(lcWhen),'WHEN '),'WHEN lfWhen()')
  laOGObjType[lnCount,1]=lcOGVarName
  laOGObjType[lnCount,2]=lcOGVarName
  laogobjtype[lnCount,3]=.f.
  laOGObjType[lnCount,4]=nFld_wdth
  laOGObjType[lnCount,5]=nFld_Dec
  lcDefa=''
  DO CASE
    CASE cData_Typ = 'D'
      lcPict = [{  /  /  }]
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
SELECT SYWCONFG
SET ORDER TO TAG NVARPOS
GO TOP
SCAN 
  lcOGVarName=STRTRAN(ALLTRIM(cfld_name),CHR(13)+CHR(10),'')
  lcOGValue=STRTRAN(mData_Def,CHR(13)+CHR(10),'')
  DO CASE
    CASE cDefa_Typ='V'
      &lcOGVarName=lfTrnsStr(lcOGValue,cData_Typ)
    CASE cDefa_Typ='E'
     &lcOGVarName=&lcOGValue
  ENDCASE
ENDSCAN
llCUpDate = .T.
SELECT (lcOldAlias)
=lfOGShowGet(.F., .T.)
=lfOGShow()

FUNCTION lfvOGCancel

llOGTrmnat = .T.
llOGOK = .F.
RETURN 

FUNCTION lfvOGClose

llOgTrmnat=.T.
IF !laScrMode[2]
  IF lcOGModle<>"SM" .OR. lfVldSets()
    llOGOK = .T.
    SELECT (lcTempName)
    FOR lnCount = 1 TO lnOGVariables
      IF SEEK(lcOGModle+STR(lnCount))
		REPLACE cDefa_typ WITH 'V',;
                MDATA_DEF WITH getString(EVAL(laOGObjType[lnCount,1]),TYPE(laOGObjType[lnCount,1]),;
                laOGObjType[lnCount,4],laOGObjType[lnCount,5]),;
                CSTATUS WITH IIF(CSTATUS='S','M',CSTATUS)
        IF SEEK(lcOGCmpName,'SYWCOMP')
          lcInstMdl = SYWCOMP.mModlSet
        ELSE
          lcInstMdl = gcComp_Mdl
        ENDIF
      ENDIF
    ENDFOR
  ELSE
    llOgTrmnat=.F.
  ENDIF  
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
      RETURN IIF(NFld_DEC>0, REPLICATE('9',NFld_WDTH-NFld_DEC-1)+'.'+;
                		     REPLICATE('9',NFld_DEC),;
                			 REPLICATE('9',NFld_WDTH))
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
    SELECT cDiscrep FROM (gcsyshome+'SYWCODE') ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    SELECT cCode_No FROM (gcsyshome+'SYWCODE') ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    

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
  lnWlrow=IIF(EMPTY(WPARENT(WPARENT())),1,-WLROW(WPARENT())+WLROW()+5)
  lnWlRow=IIF(lnogRow+lnWlRow+4>WROWS('GWDGRID'),lnWlRow-4,lnWlRow)
   DEFINE POPUP puOGTarget FROM lnOGRow+lnWlRow,lnOGCol TO lnOGRow+lnWlRow+IIF(_DOS,4,7),lnOGCol+30 SCROLL SHADOW IN WINDOW gwdGrid
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
    SELECT cDiscrep FROM (gcsyshome+'SYWCODE') ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    SELECT cCode_No FROM (gcsyshome+'SYWCODE') ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    
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
      lcDataTyp=TYPE(lcOGVarName)
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
    IF AT('.',lcValueStr) > 0
      lnSetDec = SET('DECIMALS')
      SET DECIMAL TO INT(LEN(lcValueStr)-AT('.',lcValueStr))
      lnValueStr = VAL(lcValueStr)
      SET DECIMALS TO lnSetDec
      RETURN lnValueStr
    ELSE
      RETURN VAL(lcValueStr)
    ENDIF
  CASE lcDataType='D'
     RETURN CTOD(lcValueStr)
  CASE lcDataType = 'L'
     RETURN IIF(UPPER(ALLTRIM(lcValueStr))='.F.',.F.,.T.)
ENDCASE

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
lcOGOldVal=&lcOGVarName
DIMENSION laOGVldEnt[1,1],laOGVldVal[1,1]      
STORE '' TO laOGVldEnt,laOGVldVal
DO CASE
  CASE lcOGPopType='C' 
    lcOGSelect=SELECT()
    lcOGFieldName = PADR(UPPER(lcOGValidEnt),10)
    DIMENSION laOGVldEnt[1],laOGVldVal[1]
    SELECT cDiscrep FROM (gcsyshome+'SYWCODE') ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    SELECT cCode_No FROM (gcsyshome+'SYWCODE') ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    
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
    SELECT cDiscrep FROM (gcsyshome+'SYWCODE') ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldEnt
    SELECT cCode_No FROM (gcsyshome+'SYWCODE') ;
      WHERE cfld_Name = lcOGFieldName AND cdefcode='N';
            .AND. crltField = 'N';
      INTO ARRAY laOGVldVal    
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
PARAMETERS lcValue,lcDataType, lnNum, lnDec

DO CASE
  CASE lcDataType $ 'CM'
      RETURN lcValue
  CASE lcDataType = 'N'
      IF TYPE('lnNum')='N' AND TYPE('lnDec')='N'
        RETURN ALLTRIM(STR(lcValue,lnNum,lnDec))
      ELSE
        RETURN ALLTRIM(STR(lcValue))
      ENDIF
  CASE lcDataType = 'L'
    RETURN IIF(lcValue=.T.,'.T.','.F.')
  CASE lcDataType = 'D'    
    RETURN CTOD(lcValue)
ENDCASE  

************************ SCROLL CONTROLING
FUNCTION lfWhen

lcDesc = lfGetHelp(laOGObjType[_CUROBJ-lnOGStObj,1])
SHOW GET lcDesc ENABLE

IF BETWEEN(_CUROBJ-lnOGStObj,lnObjNum,lnObjMax) 
  LLFromObj= .F.
  RETURN  laOGObjCnt[_CUROBJ-lnOGStObj]
ELSE
  IF INLIST(LASTKEY(),5,4,19,24) AND !LLFromObj
      DO CASE
      CASE lnObjMax<_CUROBJ-lnOGStObj
        IF laOGObjCnt[_CUROBJ-lnOGStObj]=.T.               
          lnObjMax=_CUROBJ-lnOGStObj        
          lnObjNum=lnObjMax-6
          MOVE WINDOW gwdgridobj TO IIF((lnObjNum-1)*2=0,0,-(lnObjNum-1)*2-IIF(_DOS,0,1*0.077)),0                    
          =lfModiHScrol()   
       ENDIF  
       RETURN  laOGObjCnt[_CUROBJ-lnOGStObj]
      CASE lnObjNum>_CUROBJ-lnOGStObj
        IF laOGObjCnt[_CUROBJ-lnOGStObj]=.T.                     
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
    IF !laScrMode[2]
      _CUROBJ=lnObjNum+lnOGStObj
    ENDIF 
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
    IF !laScrMode[2]
      _CUROBJ=lnObjNum+lnOGStObj
    ENDIF  
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
IF !laScrMode[2]
  _CUROBJ=lnObjNum+lnOGStObj
ENDIF

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
IF !laScrMode[2]
  _CUROBJ=lnObjNum+lnOGStObj
ENDIF


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
IF !laScrMode[2]
  _CUROBJ=lnObjNum+lnOGStObj
ENDIF  

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
  SET FILTER TO
  SET RELATION TO 
  SET SKIP TO
ENDFOR

*!*************************************************************
*! Name      : lfGetHelp  (N000185)
*! Developer : Ahmed Salah Shalaby - (SSH)
*! Date      : 01/11/2000
*! Purpose   : Function to get help.
*!*************************************************************
*! Calls     : Procedures : None
*!             Functions  : lfAddNew
*!*************************************************************
*! Passed Parameters : None.
*!*************************************************************
*! Returns           : None.
*!*************************************************************
*! Example     : = lfGetHelp()
*!*************************************************************
*!
FUNCTION lfGetHelp
PARAMETER lcHlpVName,lcOgModul

PRIVATE lnOldAls , lcOldInd , lcToRet ,lcOldPos
lcToRet = ''
lnOldAls = SELECT(0)
SELECT SYWHELP
lcOgModul = IIF(TYPE("lcOgModul")<>'C',PADR(lcOGModle,3),lcOgModul)
IF SEEK(lcOgModul+IIF(EMPTY(lcHlpVName),VARREAD(),lcHlpVName))
  lcToRet = mHelp
ENDIF
SELECT(lnOldAls)
RETURN(lcToRet)

FUNCTION lfWButton
PARAMETER lcActoin

DO CASE
  CASE  lcActoin = "U"
    lcNextActn='U'
    *--- Screno + ObjName
    lcDesc = lfGetHelp(PADR("BB",12),PADR("RE",3))
  CASE  lcActoin = "R"
    lcNextActn='R'
    lcDesc = lfGetHelp(PADR("BB",12),PADR("RS",3))
  CASE  lcActoin = "N"
    lcNextActn='N'
    lcDesc = lfGetHelp(PADR("BB",12),PADR("NX",3))
  CASE  lcActoin = "B"
    lcNextActn='B'
    lcDesc = lfGetHelp(PADR("BB",12),PADR("BR",3))
  CASE  lcActoin = "C"
    lcNextActn='C'
    lcDesc = lfGetHelp(PADR("BB",12),PADR("CA",3))
ENDCASE
SHOW GET lcDesc ENABLE


FUNCTION lfInitGl

IF M_LINK_GL = "N"
  M_GL_VERS  = ' '
  M_SYS_DIR  = ' '
  M_GL_CO    = ' '
  M_POST_DET = ' '
  M_DIV_LINK = ' '
  M_GL_COST  = "N"
  * disable the remainder items when deleting the link   
  =lfOGShowGet('M_GL_VERS',.F.)
  =lfOGShowGet('M_SYS_DIR',.F.)
  =lfOGShowGet('M_GL_CO',.F.)
  =lfOGShowGet('M_POST_DET',.F.)
  =lfOGShowGet('M_DIV_LINK',.F.)
ELSE
  M_GL_VERS='A'
  M_GL_COST  = "Y"
  = lfvGLVers(.T.)
  =lfOGShowGet('M_GL_VERS',.T.)
  =lfOGShowGet('M_POST_DET',.T.)
  =lfOGShowGet('M_DIV_LINK',.T.)
ENDIF
PRIVATE lnOldAls , lcOldInde
lnOldAls = SELECT(0)
SELECT sywseqnc
lcOldInde = ORDER("sywseqnc")
lcGlSeqKey = EVAL(KEY())
SET ORDER TO Cscrnam
*--- cscrnam+cparam1
llGlEnDis = (ASCAN(laAllComp,'GL') > 0) .AND. SEEK("SYCMWZ    "+"GL") .AND. lInstall
IF !llGlEnDis
  =lfOGShowGet('M_LINK_GL',llGlEnDis)
  M_GL_VERS  = 'A'
  M_SYS_DIR  = ' '
  M_GL_CO    = ' '
  M_POST_DET = ' '
  M_DIV_LINK = ' '
  M_LINK_GL = "N"
  M_GL_COST = "N"
  * disable the remainder items when deleting the link   
  =lfOGShowGet('M_GL_VERS',llGlEnDis)
  =lfOGShowGet('M_SYS_DIR',llGlEnDis)
  =lfOGShowGet('M_GL_CO',llGlEnDis)
  =lfOGShowGet('M_POST_DET',llGlEnDis)
  =lfOGShowGet('M_DIV_LINK',llGlEnDis)
  =lfOGShowGet('M_LINK_GL',llGlEnDis)
ENDIF
SET ORDER TO (lcOldInde)
=SEEK(lcGlSeqKey)
SELECT (lnOldAls)

*!*************************************************************
*! Name      : lfvGLVers
*! Developer : Ahmed Mohammed
*! Date      : 06/16/1997
*! Purpose   : Called from SMCMSET.PRG, Checks for the validity
*!             of the entered values in the setups grid
*!*************************************************************
*! Calls     : 
*!             Procedures : None
*!             Functions  : lfOGShowGet(),gfModalGen()
*!*************************************************************
*! Parameters: llDefa
*!*************************************************************
*! Returns   : 
*!*************************************************************
*! Example   :  
*!*************************************************************
FUNCTION lfvGLVers
PARAMETERS llDefa

PRIVATE llOpen, llDntLnk
llOpen = .F.
llDntLnk = .F.
DO CASE
  CASE M_GL_VERS='S'      && SBT version
    M_SYS_DIR =' '
    M_GL_CO  ='  '
    =lfOGShowGet('M_SYS_DIR',.T.)
    =lfOGShowGet('M_GL_CO',.F.)
  CASE M_GL_VERS='A'    && Aria
    M_SYS_DIR = gcSysHome    
    M_GL_Co = laDATA[1]
    IF !USED('SycComp')
      =gfOpenFile(gcSysHome + 'SycComp',' ','SH')
      llOpen = .T.
    ELSE
      SELECT SycComp    
    ENDIF
    SET ORDER TO TAG cComp_Id
    = SEEK(M_GL_Co)
    M_SYS_DIR = ' '
    M_GL_Co   = '  '
    =lfOGShowGet('M_GL_VERS',.T.) 
    =lfOGShowGet('M_SYS_DIR',.F.)
    =lfOGShowGet('M_GL_CO',.F.)
    IF llOpen
      USE IN SYCCOMP
    ENDIF
  CASE M_GL_VERS = 'O'   && Other
    M_SYS_DIR    = ' '
    M_GL_CO     = '  '
    =lfOGShowGet('M_SYS_DIR',.F.)
    =lfOGShowGet('M_GL_CO',.F.)
ENDCASE  


*!**************************************************************************
*!
*!      Function: lfvMulCurr
*!
*!**************************************************************************
*
FUNCTION lfvMulCurr

IF LLMULCURR
  llMultCurr = .T.
  LLEXCHRATE = .T.
  LLSTYPRICE = .T.
  LLEDITEXRA = .F.
  LNEXRATDAY = 30
ELSE
  llMultCurr = .F.
  LLEXCHRATE = .F.
  LLSTYPRICE = .F.
  LLEDITEXRA = .F.
  LNEXRATDAY = 0
ENDIF
=lfOGShowGet('LLMULCURR',.T.)
=lfOGShowGet('LLEXCHRATE',llMultCurr)
=lfOGShowGet('LLSTYPRICE',llMultCurr)
=lfOGShowGet('LLEDITEXRA',llMultCurr)
=lfOGShowGet('LNEXRATDAY',llMultCurr)
LNEXRATACC = SPACE(24)
=lfOGShowGet('LNEXRATACC',llMultCurr)
