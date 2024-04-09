*:************************************************************************
*: Program file  : FLTLOAD.PRG
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
*:B607001,1 AMH Fix bug of variable mVald_Str not found.
*:************************************************************************
DIMENSION laFltToLod[1]
STORE 0 TO lsFltToLod
lcSelect=SELECT()

SELECT cFlter_ID+'³'+cFltr_Dis ;
       FROM &gcSysHome.SYUFILTR;
       INTO ARRAY laFltToLod ;
    WHERE  (cUser_ID=gcUser_ID ;
           OR   lAllUsers=.T. );
           AND cFile_nam = lcSeekName;
           AND cComp_ID  = gcAct_Comp
      
           
DO FLTLOAD.SPR

SELECT(lcSelect)

FUNCTION lfShow
IF RECCOUNT('TEMCURSOR')>0
  SHOW GET PBAPPEND ENABLE
ELSE
  SHOW GET PBAPPEND DISABLE  
ENDIF

FUNCTION lfvLoad
PRIVATE lcSelect,laTmpFlt
DIMENSION laTmpFlt[1,7]
STORE '' TO laTmpFlt
lcSelect=SELECT()
IF lsFltToLod=0
  WAIT 'You Must Select a Filter To load first' WINDOW
  RETURN 
ENDIF
lcFltToLod=lcFilter+SUBSTR(laFltToLod[lsFltToLod],1,;
                 ATC('³',laFltToLod[lsFltToLod])-1)
                 
SELECT syuFiltr

IF SEEK(lcFltToLod)
  RESTORE FROM MEMO mFltr_Exp ADDITIVE
  IF !EMPTY(laTmpFlt[1,1]) 
    SELECT TemCursor
  IF VARREAD()='PBLOAD'    
    USE
    
    *B607001,1 AMH Fix bug of variable mVald_Str not found [Start]
    *CREATE CURSOR TEMCursor ( mHeader M,MLFIELD_NM M,LISNOT L,COPERATOR C(20),;
                                 CLEFTTYPE C(1),CLEFTDES C(1),MRFIELD_NM M,;
                                 MRIGHTHEAD M,CRIGHTTYPE C(1);
                                 ,CRIGHTDES C(1),NWIDTH N(5,0),NDECIMAL N(5,0),cvald_str M;
                                 ,cpict_str C(35),lvldEntry L,mventries M,nfieldPos N(5,0))
    CREATE CURSOR TEMCursor ( mHeader M,MLFIELD_NM M,LISNOT L,COPERATOR C(20),;
                                 CLEFTTYPE C(1),CLEFTDES C(1),MRFIELD_NM M,;
                                 MRIGHTHEAD M,CRIGHTTYPE C(1);
                                 ,CRIGHTDES C(1),NWIDTH N(5,0),NDECIMAL N(5,0),mvald_str M;
                                 ,cpict_str C(35),lvldEntry L,mventries M,nfieldPos N(5,0))
    *B607001,1 AMH [End]
    
    =lfFillFile('laTmpFlt')                               
    lcUsedFlt=SUBSTR(lcFltToLod,2)
  ELSE
    =lfFillFile('laTmpFlt',RECCOUNT('TEMCURSOR'))                                   
    lcUsedFlt=''
  ENDIF    
    =lfpopfill('puquery')
    lsquery=1
  ENDIF  
ENDIF
SELECT(lcSelect)
CLEAR READ

FUNCTION lfvAppend
=lfvLoad()
