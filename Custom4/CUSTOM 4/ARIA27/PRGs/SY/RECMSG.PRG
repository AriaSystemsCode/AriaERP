SET DELETE ON
SET TALK OFF
PRIVATE ALL LIKE L*
lcRetTo=SELECT(0)
*gcSysHome=GETENV("SYSHOME")
*gcWorkDir="F:\PROX\WORK\"
*gcUser_ID="HS"
*gcAct_Comp="AS"
LWasModi=.F.
LAddMod=.F.
cbAllMsg=.F.
lCanModi=.F.
lcSendBy=" "
*SET PROCEDURE TO f:\prox\prgs\aria2.prg
lcTmpfile=gfTempName()

  
            
    SELECT * FROM &gcSysHome.syumsg;
           WHERE CTo_Usr_ID=gcUser_ID AND LSend_Imed=.T. ;
           INTO TABLE &gcWorkDir.&lcTmpFile ORDER BY cUser_ID,cTopic;
           ,DSend_Date,cSend_Time
  SELECT "SyuCkMsg"
  SEEK gcUser_ID
  REPLACE lIs_Order With .F.         
  SELECT "SYuMsg"       
    REPLACE lWas_Read WITH .T.;
            DRead_Date WITH DATE();
            cRead_Time WITH gfGetTime();
            FOR cTo_Usr_Id=gcUser_ID; 
            And lSend_Imed=.T.
            
     
    DIMENSION laTopic[1],laUserId[1],lnRecPos[1],lnMastPos[1]
    
    lnMastPos=0
            
    SELECT RECNO() FROM &gcSysHome.syumsg;
           INTO ARRAY lnMastPos;
           WHERE CTo_Usr_ID=gcUser_ID AND LSend_Imed=.T.;
           ORDER BY cUser_ID,cTopic;
          ,DSend_Date,cSend_Time    

            
              
    lnRecPos=0
    laTopic=""
    laUserID=""
    lcWhome="laUserID"
 
SELECT SUBSTR(TRIM(CContact),1,15)+SPACE(17-LEN(SUBSTR(TRIM(CCONTACT),1,15)));
      +SUBSTR(TRIM(CCompany),1,15)+SPACE(17-LEN(SUBSTR(TRIM(CCOMPANY),1,15)));
      +SUBSTR(TRIM(CPhone_Num),1,10)+SPACE(11-LEN(SUBSTR(TRIM(CPhone_Num),1,10)));
       +dSend_Date+"  "+;
       cSend_Time;               
       FROM &gcWorkDir.&lcTmpFile ORDER BY cUser_ID,cTopic;
           ,dSend_Date,cSend_Time;
           INTO ARRAY laTopic

    SELECT RECNO() FROM &gcWorkDir.&lcTmpFile INTO ARRAY lnRecPos;
    ORDER BY cUser_ID,cTopic,dSend_Date,cSend_Time    


   SELECT CUser_ID+CUsr_Name FROM &gcsyshome.syuuser;
   INTO ARRAY  laUserID;
   WHERE CUser_ID<>gcUser_ID

   
    SELECT("SYUUSER")
    SET ORDER TO 1
 SEEK gcUser_ID
 LCuse_N=CUsr_Name
 
 DIMENSION laComInfo[1]
 laComInfo=" "
IF USED("sycComp") 
 SELECT cCom_Name,cCom_Phon FROM &gcSysHome.sycComp;
 INTO ARRAY laComInfo;
 WHERE cComp_ID=gcAct_Comp
ELSE
  SELECT cCom_Name,cCom_Phon FROM &gcSysHome.sycComp;
 INTO ARRAY laComInfo;
 WHERE cComp_ID=gcAct_Comp
 
 USE IN ("sycComp")
 lcComp_N=laComInfo[1,1]
 lcPhone=laComInfo[1,2]
 RELEASE laComInfo
ENDIF 
 
 SELECT(lcTmpFile)

 DO message2.spr       


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

           
  FOR lnSear=1 TO ALEN(laUserId,1)
    lcSendBy =IIF(cUser_ID $ laUserID[lnSear],;
                 SUBSTR(laUserID[lnSear],4,Len(laUserID[lnSear])-4);
                ,lcSendBy)
  ENDFOR 

ENDIF  
IF lWasModi
 SHOW GET pbCncl ENABLE
ENDIF
SHOW GETS 
RETURN .T.


  
FUNCTION lfvpbOk


SELECT "SYUMSG"
SET ORDER TO 1

IF lWasModi
 DELETE FOR cTo_Usr_Id=gcUser_ID AND LSend_Imed=.T.
 IF !EMPTY(lnRecPos[1] )
   FOR lnCount=1 TO ALEN(lnMastPos,1)  
   IF TYPE("lnMastPos[lnCount]")="N"
     IF lnMastPos[lnCount]=0
       SELECT &lcTmpFile            
       GOTO lnRecPos[lnCount]
       SCATTER MEMVAR MEMO
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
       SELECT "SYUMSG"    
       GOTO lnMastPos[lnCount]
       RECALL
       GATHER MEMVAR MEMO
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
  SELECT "SYUMSG"
   RECALL FOR cTo_Usr_ID=gcUser_ID AND lSend_Imed=.T.
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

FUNCTION lfvZoom
 DO GetMemo.SPR
 SHOW GET MMsgText

FUNCTION lfshowZoom
IF lCanModi
  SHOW GETS ENABLE ONLY
ELSE
  SHOW GET MMsgText DISABLE
  SHOW GET pbOk ENABLE  
ENDIF 
 
 
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

 
 
FUNCTION lfShowOP

  SHOW GETS DISABLE ONLY
  SHOW GET pbOk ENABLE



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

