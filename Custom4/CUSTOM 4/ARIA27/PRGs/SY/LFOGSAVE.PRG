*:************************************************************************
*: Program file  : LFOGSAVE.PRG
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
*B800551,1 Hesham El-Sheltawi 04/10/96
*B800551,1 Fix the saving of the prefrences to make the option grid
*B800551,1 feel any set as default prefrence to load it
*E301734,1 ADEL 09/30/01   - Add check box "Accessible by all users" to save settings screen..
*:************************************************************************
STORE .F. TO llOGRepFlt,llOGSetDef,llOGAccByAll
STORE '' TO lcOGFltDes,lcOGRepFlt

lcOGSelect=SELECT()

IF !USED('SYURPFLT')
  SELECT 0
  USE &gcSysHome.SYURPFLT ORDER 1
  llOGRepFlt=.T.
ELSE
  SELECT SYURPFLT
ENDIF
*lcOGRepFlt=IIF(lcOGSeting='Default','',lcOGSeting)
*B800551,1 Hesham (Start)
*B800551,1 loading the report prefrence without checking the
*B800551,1 setting of the develop env. variable
*lcOGRepFlt=IIF(!llOGSysData AND EMPTY(GETENV('DEVELOP')),lcOGFlt_ID,'')
lcOGRepFlt=IIF(!llOGSysData,lcOGFlt_ID,'')
*B800551,1 Hesham (End)
IF !EMPTY(lcOGRepFlt)
  IF SEEK(ALLTRIM(lcOGManRep)+gcUser_ID+lcOGRepFlt)
    lcOGFltDes=cFltr_Des
    llOGSetDef=lDefault
  *E301734,1 (Begin) If this setting is set by anothor user and set for all users, load its info.
  ELSE
    IF SEEK(ALLTRIM(lcOGManRep))
      LOCATE REST WHILE ALLTRIM(crep_id)+cuser_id+cfltr_id = ALLTRIM(lcOGManRep);
                  FOR   cfltr_id = lcOGRepFlt AND lAccByAll
      IF FOUND()
        lcOGFltDes=cFltr_Des
      ENDIF            
    ENDIF 
  ENDIF
  *E301734,1 (End)
ENDIF
*E301734,1 (Begin) Refresh "Access by all users" check box.
llNewId = .F.
llAccAlUsr= IIF(!EMPTY(lcOGRepFlt),lfIsAcc(1),.F.)
llAccAllU = IIF(!EMPTY(lcOGRepFlt),IIF(lAccByAll OR llAccAlUsr,1,0),0)
*E301734,1 (End)

DO syOGSave.spr

SELECT(lcOGSelect)
IF _DOS
  SHOW GETS OFF
ELSE
  SHOW GETS ONLY
ENDIF  




FUNCTION lfvSave

IF (!EMPTY(lcOGRepFlt) AND !EMPTY(lcOGFltDes)) 

*  IF llOGSysData AND EMPTY(GETENV('DEVELOP'))
    *E301734,1 (Begin) See if new ID.
    llNewId = (cfltr_id <> lcOGRepFlt)
    *E301734,1 (End)
    
    llCanSeek=SEEK(ALLTRIM(lcOGManRep)+PADR(gcUser_ID,LEN(CUSER_ID))+PADR(lcOGRepFlt,4))
*  ELSE
*    LOCATE FOR crep_id=lcOGManRep AND cFltr_id=lcOGRepFlt AND lSys_Data=.T.
*    llCanSeek=FOUND()
*  ENDIF  
  
  IF llCanSeek
    ** CONFIRM OVERWRITING
    IF gfModalGen("INM00254B00006",'ALERT') = 2
      _CUROBJ = OBJNUM(lcOGRepFlt)
      RETURN
    ENDIF
    IF llOGSetDef
      REPLACE ALL lDefault WITH .F.;
          FOR CREP_ID = lcOGManRep;
            AND CUSER_ID = gcUser_ID 
    ENDIF      
    =SEEK(ALLTRIM(lcOGManRep)+PADR(gcUser_ID,LEN(CUSER_ID))+PADR(lcOGRepFlt,4))    
  ELSE
    IF llOGSetDef
      REPLACE ALL lDefault WITH .F.;
          FOR CREP_ID = lcOGManRep;
            AND CUSER_ID = gcUser_ID 
    ENDIF    
    APPEND BLANK
  ENDIF
*B800551,1 Hesham (Start)  
*B800551,1 Save the user name everytime even if set develop is on
  REPLACE CREP_ID WITH lcOGManRep;
          cUser_ID WITH gcUser_ID;
          cFltr_ID WITH lcOGRepFlt;
          lDefault WITH llOGSetDef;
          lAccByAll WITH llOGAccByAll;
          cfltr_des WITH lcOGFltDes;
          lSelect   WITH llOGFilter;
          lSys_Data WITH .F.

   *E301734,1 (Begin) Update 'Accessible by all users' check box.
   lcKey = EVAL(KEY())
   DO CASE
     CASE llAccAlUsr AND llAccAllU = 0
       *--Was Access by all and no longer : delete for all except current user.
       =lfIsAcc(3)
     CASE llAccAllU = 1
       *--Was Access by all and still, OR Wasn't Access by all and becomes: update all.
       =lfIsAcc(2)
   ENDCASE
   =SEEK(lcKey)
   lcOGSeting = RECNO()
   REPLACE lAccByAll WITH IIF(!llCanSeek AND llAccAlUsr AND !llNewId ,.F.,IIF(llAccAllU = 1,.T.,.F.)) 
   *E301734,1 (End)
   
*    REPLACE CREP_ID WITH lcOGManRep;
            cUser_ID WITH IIF(!EMPTY(GETENV('DEVELOP')),'',gcUser_ID);
            cFltr_ID WITH lcOGRepFlt;
            lDefault WITH llOGSetDef;
            lAccByAll WITH llOGAccByAll;
            cfltr_des WITH lcOGFltDes;
            lSelect   WITH llOGFilter;
            lSys_Data WITH IIF(!EMPTY(GETENV('DEVELOP')),.T.,.F.)
*B800551,1 Hesham (End)            
    lcOGSeting = RECNO()           
    DIMENSION laOGUDefVr[1,2]        
    STORE '' TO laOGUDefVr
    =lfSaveUDefV('laOGUDefVr')           
    IF !EMPTY(laOGUDefVr[1,1])
      SAVE TO MEMO mUsrDVar ALL LIKE laOGUDefVr
    ENDIF
    IF !EMPTY(laOGHDFLT[1,1])
      SAVE TO MEMO mRepHdFlt ALL LIKE laOGHDFLT
    ENDIF
    IF !EMPTY(laOGFxFLT[1,1])  
      =lfInitValue('laOGFxFlt','T')    
      SAVE TO MEMO mRepFxFlt ALL LIKE laOGFxFlt
      =lfInitValue('laOGFxFlt','C')      
    ENDIF  
    IF !EMPTY(laOGVrFLT[1,1])  
      =lfInitValue('laOGVrFlt','T')
      SAVE TO MEMO mRepVrFlt ALL LIKE laOGVrFlt  
     =lfInitValue('laOGVrFlt','C') 
    ENDIF  
    CLEAR READ      
    =lfGetSetting() 
    lnogSeting = ASCAN(lAOGSeting,lcOGSeting)    
    lnogSeting = IIF(lnogSeting<>0,ASUBSCRIPT(laOGSeting,lnogSeting,1),1)
    =lfwChangeset()
  ENDIF


FUNCTION lfOGSaveShow
IF (!EMPTY(lcOGRepFlt) AND !EMPTY(lcOGFltDes)) 
  SHOW GET pbSave ENABLE
ELSE
  SHOW GET pbSave DISABLE
ENDIF

*:************************************************************************
*: Program file  : lfIsAcc.PRG
*: Program desc. : Is there any settinfg for teh current report "Access fro all users"?
*:         System: Aria advantage series
*:         Module: Main system 
*:      Developer: Adel Mohammed El Gazzar (ADEL)
*:************************************************************************
*E301734,1
FUNCTION lfIsAcc
PARAMETERS lnUpdate

PRIVATE lcKey
lcKey = EVAL(KEY())
DO CASE
  CASE lnUpdate = 1
    IF SEEK(ALLTRIM(lcOGManRep))
       LOCATE REST WHILE ALLTRIM(crep_id)+cuser_id+cfltr_id = ALLTRIM(lcOGManRep);
                   FOR   cfltr_id = lcOGRepFlt AND lAccByAll
    ENDIF 
    llFound = FOUND()
    =SEEK(lcKey)  
    RETURN llFound
  CASE lnUpdate = 2
    IF SEEK(ALLTRIM(lcOGManRep))
      SCAN WHILE ALLTRIM(crep_id)+cuser_id+cfltr_id = ALLTRIM(lcOGManRep);
                      FOR   cfltr_id = lcOGRepFlt
        lcOGSeting = RECNO()           
        DIMENSION laOGUDefVr[1,2]        
        STORE '' TO laOGUDefVr
        =lfSaveUDefV('laOGUDefVr')           
        IF !EMPTY(laOGUDefVr[1,1])
          SAVE TO MEMO mUsrDVar ALL LIKE laOGUDefVr
        ENDIF
        IF !EMPTY(laOGHDFLT[1,1])
          SAVE TO MEMO mRepHdFlt ALL LIKE laOGHDFLT
        ENDIF
        IF !EMPTY(laOGFxFLT[1,1])  
          =lfInitValue('laOGFxFlt','T')    
          SAVE TO MEMO mRepFxFlt ALL LIKE laOGFxFlt
          =lfInitValue('laOGFxFlt','C')      
        ENDIF  
        IF !EMPTY(laOGVrFLT[1,1])  
          =lfInitValue('laOGVrFlt','T')
          SAVE TO MEMO mRepVrFlt ALL LIKE laOGVrFlt  
          =lfInitValue('laOGVrFlt','C') 
        ENDIF
        REPLACE lAccByAll WITH IIF(lAccByAll,IIF(llAccAllU = 1,.T.,.F.),lAccByAll)
      ENDSCAN  
    ENDIF
  CASE lnUpdate = 3
    IF SEEK(ALLTRIM(lcOGManRep))
      DELETE REST WHILE ALLTRIM(crep_id)+cuser_id+cfltr_id = ALLTRIM(lcOGManRep);
                      FOR   cfltr_id = lcOGRepFlt AND CUSER_ID <> gcUser_ID 
    ENDIF                  
ENDCASE
=SEEK(lcKey)

FUNCTION lfCheclAcc

IF (llAccAlUsr AND llAccAllU = 0) AND gfModalGen("QRM00381B00006","ALERT") = 2
  llAccAllU = 1
  SHOW GET llAccAllU
ENDIF