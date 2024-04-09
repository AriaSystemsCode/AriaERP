PRIVATE ALL LIKE L*
DIMENSION laScrMode[5]
SET DELETE ON
SET TALK OFF

LWasModi  = .F.
LAddMod   = .F.
cbAllMsg  = .F.
lCanModi  = .T.
laScrMode = .F.
lcTmpfile=gfTempName()

lcRetTo=SELECT(0)
    SELECT * FROM &gcSysHome.syumsg;
           WHERE CUser_ID=gcUser_ID AND (LSend_Imed=.F. OR;
           LWas_Read=.F.);
           INTO TABLE &gcWorkDir.&lcTmpFile ORDER BY cUser_ID,cTopic;
           ,DSend_Date,cSend_Time
           

     
    DIMENSION laTopic[1],laUserId[1],lnRecPos[1],lnMastPos[1]
    
    lnMastPos=0
            
    SELECT RECNO() FROM &gcSysHome.syumsg;
           INTO ARRAY lnMastPos;
           WHERE CUser_ID=gcUser_ID AND (LSend_Imed=.F. OR;
           LWas_Read=.F.);
            ORDER BY cUser_ID,cTopic;
           ,DSend_Date,cSend_Time    

    SELECT "SYuMsg"       

              
    lnRecPos=0
    laTopic=""
    laUserID=""
    lcWhome="laUserID"
    
    SELECT CTopic+space(25-LEN(CTopic))+'['+;
        IIF(LSend_Imed=.T.,'X',' ')+']'+SPACE(2)+;
        DSend_Date+Space(2)+SUBSTR(CSend_Time,1,LEN(cSend_Time)-6)+RIGHT(cSend_Time,3);
        +SPACE(4)+;
        "["+IIF(LWas_Read=.T.,"X"," ")+"]";
        FROM &gcWorkdir.&lcTmpFile ORDER BY CTopic;
        INTO ARRAY laTopic
        
    SELECT RECNO() FROM &gcWorkDir.&lcTmpFile INTO ARRAY lnRecPos    

   SELECT CUser_ID+CUsr_Name FROM &gcsyshome.syuuser;
   INTO ARRAY  laUserID;
   WHERE CUser_ID<>gcUser_ID
   
    SELECT("SYUUSER")
    SET ORDER TO 1
 SEEK gcUser_ID
 LCuse_N=CUsr_Name
 lcComp_N=" "
 lcPhone=SPACE(16)
IF USED("sycComp") 
  SELECT sycComp
  SET ORDER TO 1
  IF SEEK(gcAct_Comp)
    lcComp_N=cCom_Name
    lcPhone=cCom_Phon  
  ENDIF  
ELSE 

SELECT 0
USE &gcSysHome.sycComp.DBF ORDER 1  excl
IF SEEK(gcAct_Comp)
  lcComp_N=cCom_Name
  lcPhone=cCom_Phon  
ENDIF  
USE IN ("sycComp")
ENDIF 
 
 SELECT(lcTmpFile)
   
 DO message1.spr       


   IF !EMPTY(lcTmpFile)
    USE IN (lcTmpFile)
    lcTmpFile = lcTmpFile + ".DBF"
    ERASE &gcWorkDir.&lcTmpFile
    lcTmpFile = LEFT(lcTmpFile,ATC(".",lcTmpFile)-1)+".CDX"
    ERASE &gcWorkDir.&lcTmpFile
    lcTmpFile = LEFT(lcTmpFile,ATC(".",lcTmpFile)-1)+".FPT"
    ERASE &gcWorkDir.&lcTmpFile
  ENDIF
  
SELECT(lcRetTo)  
  
FUNCTION lfWlsTop
IF !EMPTY(laTopic)
 IF TYPE("lnRecPos[1]")='N' 
   IF lnRecPos[lsTopic]>0
     GOTO lnRecPos[lsTopic]
   ENDIF
 ENDIF 
 laTopic[lsTopic]=CTopic+space(25-LEN(CTopic))+'['+;
        IIF(LSend_Imed=.T.,'X',' ')+']'+SPACE(2)+;
        DSend_Date+Space(2)+SUBSTR(CSend_Time,1,LEN(cSend_Time)-6)+RIGHT(cSend_Time,3);
        +SPACE(4)+;
        "["+IIF(LWas_Read=.T.,"X"," ")+"]"
  rbToTyp=AT(UPPER(CSnd_To_T),"CVO")
IF !LAddMod  
  FOR lnSear=1 TO ALEN(laUserId,1)
    puWhomToS=IIF(cTo_Usr_ID $ laUserID[lnSear],lnSear,puWhomToS)
  ENDFOR 
ENDIF  
IF cbAllMsg
  IF lWas_Read
    SHOW GET cTopic DISABLE
    SHOW GET pbrmv DISABLE
    SHOW GET puWhomToS DISABLE
    SHOW GET lSend_Imed DISABLE
    SHOW GET pbZoomM DISABLE
    SHOW GET lsTopic
    SHOW GET MMsgText
    lCanModi     = .F.
    laScrMode    = .F.
    laScrMode[1] = .T.
  ELSE
    SHOW GET cTopic ENABLE
    SHOW GET pbrmv ENABLE
    SHOW GET puWhomToS ENABLE
    SHOW GET lSend_Imed ENABLE
    SHOW GET pbZoomM ENABLE      
    SHOW GET lsTopic
    SHOW GET MMsgText
    lCanModi     =.T.
    laScrMode    = .F.
    laScrMode[3] = .T.
  ENDIF
ELSE
  SHOW GETS 
  lCanModi     =.T.
  laScrMode    = .F.
  laScrMode[3] = .T.
ENDIF  

ELSE  
*  IF LExt_Call
*    SHOW GET rbToTyp ENABLE
*    IF AT(UPPER(CSnd_To_T),"CVU")>0  
*      SHOW GET CContact DISABLE
*      SHOW GET CCompany DISABLE
*      SHOW GET CPhone_Num DISABLE
*    ELSE
*      SHOW GET CContact ENABLE
*      SHOW GET CCompany ENABLE
*      SHOW GET CPhone_Num ENABLE
*    ENDIF
*  ELSE
*    SHOW GET rbToTyp DISABLE
*    SHOW GET CContact DISABLE
*    SHOW GET CCompany DISABLE
*    SHOW GET CPhone_Num DISABLE     
*  ENDIF  
ENDIF  
IF lWasModi
 SHOW GET pbCncl ENABLE
 SHOW GET cbAllMsg DISABLE
ENDIF
RETURN .T.

FUNCTION lfvlsTop
LWasModi=!cbAllMsg
IF !cbAllMsg
  SHOW GET pbCncl ENABLE
  SHOW GET cbAllMsg DISABLE
ENDIF



  
FUNCTION lfvpbOk


SELECT "SYUMSG"
SET ORDER TO 1

IF lWasModi
  IF !cbAllMsg
    DELETE FOR cUser_Id=gcUser_ID AND (LSend_Imed=.F. OR;
              lWas_Read=.F.)
   ELSE
    DELETE FOR cUser_Id =gcUser_ID AND lWas_Read=.F.              
   ENDIF            
 IF !EMPTY(lnRecPos[1] )
   FOR lnCount=1 TO ALEN(lnMastPos,1)  
   IF TYPE("lnMastPos[lnCount]")="N"
     IF lnMastPos[lnCount]=0
       SELECT &lcTmpFile            
       GOTO lnRecPos[lnCount]
       SCATTER MEMVAR MEMO
       IF lSend_Imed
         =SEEK(cTo_Usr_ID,"syuckmsg")
         SELECT "SYUCKMSG"
         REPLACE lIs_Order WITH .T.
       ENDIF
       SELECT "SYUMSG"    
       IF !SEEK(" ")
       APPEND BLANK
       ELSE
       RECALL
       ENDIF
       GATHER MEMVAR MEMO
     ELSE
       SELECT &lcTmpFile            
       GOTO lnRecPos[lnCount]
       SCATTER MEMVAR MEMO
       IF !lWas_Read AND lSend_Imed
         =SEEK(cTo_Usr_ID,"syuckmsg")
         SELECT "SYUCKMSG"
         REPLACE lIs_Order WITH .T.
       ENDIF
       SELECT "SYUMSG"    
       GOTO lnMastPos[lnCount]
       RECALL
       IF !lWas_Read
         IF !EMPTY(cSend_Time)
           GATHER MEMVAR MEMO
         ENDIF
       ENDIF
     ENDIF
   ENDIF  
   ENDFOR
   DO PACKING WITH "SYUMSG"
  ENDIF
ELSE
DO lfvpbCncl
ENDIF
RETURN .T.  

FUNCTION lfvpbCncl
  CLEAR READ
 RETURN .T. 




FUNCTION lfvpbRmv
LWasModi=.T.
IF !EMPTY(laTopic)
  =ADEL(laTopic,lstopic)
  =ADEL(lnRecPos,lsTopic)
  =ADEL(lnMastPos,lsTopic)
IF ALEN(laTopic,1)>1  
  DIMENSION laTopic[ALEN(laTopic,1)-1],lnRecPos[ALEN(lnRecPos,1)-1];
            lnMastPos[ALEN(lnMastPos,1)-1] 
ELSE
  laTopic[1]=" "
  SHOW GET pbRmv DISABLE 
  SCATTER MEMVAR MEMO BLANK
  GATHER MEMVAR
  SHOW GETS
ENDIF  
  DELETE
  IF lsTopic>1
   lsTopic=lsTopic-1
   GOTO lnRecPos[lsTopic]  
  ENDIF
ENDIF
 SHOW GET pbCncl ENABLE
do lfWlsTop
RETURN

FUNCTION lfvpbAdd
 LAddMod=.T.
  LWasModi=.T.
 APPEND BLANK



     SELECT(lcTmpFile)
     
     REPLACE CContact WITH lcUse_N;
     CSnd_To_T WITH "U";
     CTo_Usr_ID WITH ALLTRIM(LEFT(&lcWhome[puWhomToS],4));
     DSend_Date WITH DTOC(Date());
     CSend_Time WITH gfGetTime();
     cUser_ID WITH gcUser_ID;
     cCompany WITH lcComp_N;
     cPhone_Num WITH lcPhone
     
IF !EMPTY(laTopic[1])
     DIMENSION laTopic[ALEN(laTopic,1)+1],lnRecPos[ALEN(lnRecPos,1)+1];
               ,lnMastPos[ALEN(lnMastPos,1)+1]
ENDIF      
     laTopic[ALEN(laTopic,1)]=CTopic+space(25-LEN(CTopic))+'['+;
        IIF(LSend_Imed=.T.,'X',' ')+']'+SPACE(5)+;
        DSend_Date+Space(2)+CSend_Time+SPACE(10)+;
        "["+IIF(LWas_Read=.T.,"X"," ")+"]"
     lnRecPos[ALEN(lnRecPos,1)]=RECNO()
     lnMastPos[ALEN(lnMastPos,1)]=0
     lsTopic=ALEN(laTopic,1)
     SHOW GET pbCncl ENABLE
     SHOW GET pbRmv ENABLE
     DO lfwlsTop
 _CUROBJ=OBJNUM(CTOPIC)
  LAddMod=.T.






FUNCTION lfvTopic
 IF LAddMod
   IF !EMPTY(cTopic)
     SELECT(lcTmpFile)
     laTopic[ALEN(laTopic,1)]=CTopic+space(25-LEN(CTopic))+'['+;
        IIF(LSend_Imed=.T.,'X',' ')+']'+SPACE(5)+;
        DSend_Date+Space(2)+CSend_Time+SPACE(10)+;
        "["+IIF(LWas_Read=.T.,"X"," ")+"]"
        DO lfwlsTop
        DO U:\PROX\GETMEMO.SPR
        SHOW GET MMsgText
*        _CUROBJ=OBJNUM(MMsgText)
   ENDIF
 LAddMod=.F.  
 LWasModi=.T.
 ELSE
  laTopic[lsTopic]=cTopic+space(25-LEN(cTopic))+''+;
   IIF(LSend_Imed=.T.,'X',' ')+']'+SPACE(5)+;
   DSend_Date+SPACE(2)+cSend_Time+SPACE(10)+;
   '['+IIF(lWas_Read=.T.,"X"," ")+" "   
   LWASmODI=!cbAllMsg
 DO lfwlsTop 
ENDIF


FUNCTION lfvcbSImd
 LWasModi=.T.
 do lfWlsTop
 
FUNCTION lfvWhome  
LWasModi=.T.
SHOW GET cbAllMsg DISABLE
REPLACE CTo_Usr_ID WITH ALLTRIM(LEFT(&lcWhome[puWhomToS],4))


FUNCTION lfvZoom
 DO GetMemo.SPR
 IF lWasModi
   SHOW GET pbCncl Enable
   SHOW GET cbAllMsg DISABLE
 ENDIF 
 SHOW GET MMsgText
 

FUNCTION lfshowZoom
IF lCanModi
  SHOW GETS ENABLE ONLY
ELSE
  SHOW GET MMsgText DISABLE
  SHOW GET pbOk ENABLE  
ENDIF 

 
FUNCTION lfvExt
REPLACE LExt_Call With !LEXt_Call
DO ExtCall.Spr
IF !cbAllMsg
  IF lWasModi
    SHOW GET cbAllMsg DISABLE
  ENDIF
ENDIF
SHOW GET LExt_Call

FUNCTION lfvrbToTyp
DO CASE
  CASE rbToTyp=1

  CASE rbToTyp=2
  
  CASE rbToTyp=3 
    REPLACE CSnd_To_T WITH "U";
    cContact WITH lcUse_N;
    cCompany WITH lcComp_N;
    cPhone_Num WITH lcPhone;
    LExt_Call WITH .F.
    SHOW GETS

  CASE rbToTyp=4
    REPLACE CSnd_To_T WITH "O";
    cContact WITH " ";
    cCompany WITH " ";
    cPhone_Num WITH " ";
    LExt_Call WITH .T.
    SHOW GETS  
 
ENDCASE
LWasModi=.T.


FUNCTION lfShowExt
rbToTyp= AT(UPPER(cSnd_To_T),"CVUO")
IF !lCanModi
  SHOW GETS DISABLE ONLY
  SHOW GET pbOk ENABLE
ELSE 
DO CASE
  CASE rbToTyp=1
    SHOW GET CContact DISABLE
    SHOW GET CCompany DISABLE
    SHOW GET CPhone_Num DISABLE
    SHOW GET rbToTyp ENABLE
    
  CASE rbToTyp=3
    REPLACE cContact WITH lcUse_N;
    cCompany WITH lcComp_N;
    cPhone_Num WITH lcPhone;
    LExt_Call WITH .F.    
    SHOW GET cContact DISABLE
    SHOW GET cCompany DISABLE
    SHOW GET cPhone_Num DISABLE
    SHOW GET rbToTyp ENABLE

  CASE rbToTyp=4
    SHOW GET rbToTyp ENABLE
    SHOW GET cContact ENABLE
    SHOW GET cCompany ENABLE
    SHOW GET cPhone_Num ENABLE
ENDCASE
ENDIF


FUNCTION lfvpbOpt
DO MSGOPT.SPR
IF !cbAllMsg
  IF lWasModi
   SHOW GET cbAllMsg DISABLE
   SHOW GET pbCncl ENABLE
  ENDIF
ENDIF  

 
FUNCTION lfvcbAllM
lWasModi=.F.
IF cbAllMsg
  IF !EMPTY(lcTmpFile)
    USE IN (lcTmpFile)
    lcTmpFile = lcTmpFile + ".DBF"
    ERASE &gcWorkDir.&lcTmpFile
    lcTmpFile = LEFT(lcTmpFile,ATC(".",lcTmpFile)-1)+".CDX"
    ERASE &gcWorkDir.&lcTmpFile
    lcTmpFile = LEFT(lcTmpFile,ATC(".",lcTmpFile)-1)+".FPT"
    ERASE &gcWorkDir.&lcTmpFile
  ENDIF
SELECT "SYUMSG"  
*RECALL FOR cUser_Id=gcUser_ID AND (LSend_Imed=.F. OR;
*              lWas_Read=.F.)   
              
    lcTmpfile=gfTempName()
    SELECT * FROM &gcSysHome.syumsg;
           WHERE CUser_ID=gcUser_ID;
           INTO TABLE &gcWorkDir.&lcTmpFile ORDER BY cUser_ID,CTopic,;
           dSend_Date,cSend_Time
           

    DIMENSION laTopic[1],lnRecPos[1],lnMastPos[1]
    lnMastPos=0
    SELECT RECNO() FROM &gcSysHome.syumsg;
           INTO ARRAY lnMastPos;
           WHERE CUser_ID=gcUser_ID;
            ORDER BY cUser_ID,CTopic,;
           dSend_Date,cSend_Time    
           
    SELECT "SYUMSG"  


    laTopic=""
    SELECT CTopic+space(25-LEN(CTopic))+'['+;
        IIF(LSend_Imed=.T.,'X',' ')+']'+SPACE(2)+;
        DSend_Date+Space(2)+SUBSTR(CSend_Time,1,LEN(cSend_Time)-6)+RIGHT(cSend_Time,3);
        +SPACE(4)+;
        "["+IIF(LWas_Read=.T.,"X"," ")+"]";
        FROM &gcWorkdir.&lcTmpFile ORDER BY cUser_ID,CTopic;
        ,dSend_Date,cSend_Time;
        INTO ARRAY laTopic

  SELECT "SYUMSG"  
    SELECT RECNO() FROM &gcWorkDir.&lcTmpFile INTO ARRAY lnRecPos    
    SELECT &lcTmpFile
    DO lfwlsTop
ELSE
  lsTopic=1
  IF !EMPTY(lcTmpFile)
    USE IN (lcTmpFile)
    lcTmpFile = lcTmpFile + ".DBF"
    ERASE &gcWorkDir.&lcTmpFile
    lcTmpFile = LEFT(lcTmpFile,ATC(".",lcTmpFile)-1)+".CDX"
    ERASE &gcWorkDir.&lcTmpFile
    lcTmpFile = LEFT(lcTmpFile,ATC(".",lcTmpFile)-1)+".FPT"
    ERASE &gcWorkDir.&lcTmpFile
  ENDIF
  

    lcTmpfile=gfTempName()
*    SELECT "syumsg"
*    RECALL FOR cUser_Id =gcUser_ID AND lWas_Read=.F.
    SELECT * FROM &gcSysHome.syumsg;
           WHERE CUser_ID=gcUser_ID AND (LSend_Imed=.F. OR;
           LWas_Read=.F.);
           INTO TABLE &gcWorkDir.&lcTmpFile ORDER BY  cUser_ID,CTopic;
        ,dSend_Date,cSend_Time



    DIMENSION laTopic[1],lnRecPos[1],lnMastPos[1]
    lnMastPos=0    

    SELECT RECNO() FROM &gcSysHome.syumsg;
    	   INTO ARRAY lnMastPos;
           WHERE CUser_ID=gcUser_ID AND (LSend_Imed=.F. OR;
           LWas_Read=.F.);
           ORDER BY  cUser_ID,CTopic;
        ,dSend_Date,cSend_Time
*    SELECT "SYUMSG"  
*    DELETE FOR cUser_Id=gcUser_ID AND (LSend_Imed=.F. OR;
*              lWas_Read=.F.)
              
    laTopic=""
    SELECT CTopic+space(25-LEN(CTopic))+'['+;
        IIF(LSend_Imed=.T.,'X',' ')+']'+SPACE(2)+;
        DSend_Date+Space(2)+SUBSTR(CSend_Time,1,LEN(cSend_Time)-6)+RIGHT(cSend_Time,3);
        +SPACE(4)+;
        "["+IIF(LWas_Read=.T.,"X"," ")+"]";
        FROM &gcWorkdir.&lcTmpFile ORDER BY cUser_ID,CTopic;
        ,dSend_Date,cSend_Time;
        INTO ARRAY laTopic
        
    SELECT RECNO() FROM &gcWorkDir.&lcTmpFile INTO ARRAY lnRecPos    
    SELECT &lcTmpFile
*    SHOW GET lsTopic

  SHOW GETS ENABLE ONLY
  SHOW GET LGroup DISABLE
  SHOW GET MMsgText DISABLE
  SHOW GET pbCncl DISABLE
ENDIF 
 
 
RETURN .T.
 
FUNCTION lfShowOP
IF !lCanModi
  SHOW GETS DISABLE ONLY
  SHOW GET pbOk ENABLE
ELSE
  SHOW GETS ENABLE ONLY
ENDIF


PROCEDURE PACKING
PARAMETER lcAlias_N
lcSavAlias= SELECT(0)
SELECT &lcAlias_N
 GOTO TOP
 SET DELETE OFF
DO WHILE !EOF()
 IF DELETED()
   SCATTER MEMVAR MEMO BLANK
   GATHER MEMVAR MEMO
  ENDIF
  SKIP
ENDDO
SET DELETE ON
SELECT (lcSavAlias )


FUNCTION LFVCBGROUP 



FUNCTION LFVMSGTSND 

