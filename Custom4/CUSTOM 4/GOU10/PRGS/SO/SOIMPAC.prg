*:***************************************************************************
*: Program file  : SOIMPAC.prg
*: Program desc. : Custom Program to Read store information for Gordini.
*: System        : Aria Advantage Series.(Aria4XP)
*: Module        : Sales Order (SO)
*: Developer     : Mariam Mazhar(MMT)
*! Date          : 04/10/2011
*! Entry No.     : C201323.122,c201324.EXE- [T20101109.0013]
*!***************************************************************************
*: Modifications:
*: C201324,2 MMT 07/07/2011 Update Customer Codes fields with the Default Code Value[T20101109.0013]
*: C201371,1 SAB 07/28/2011 Error when open "Import Account/Store info" screen after "Sales order Status/Ship Status" [T20101109.0013]
*********************************************************************************************
lcExpr = gfOpGrid('SOIMPACC' , .T.)
*!**************************************************************************
*! Name      : lfvPath
*! Developer : Mariam Mazhar (MMT)
*! Date      : 01/25/2010
*! Purpose   : Validate Path/File Name
*!************************************************************************** 
FUNCTION lfvPath
IF !EMPTY(lcRpPath) AND '?' $ lcRpPath  
  lcRpPath = GETFILE('XLS','Select the File Location and Name')
ENDIF 
RETURN .T.
*!**************************************************************************
*! Name      : lfwOGWhen
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/10/2011
*! Purpose   : When Function of the OG
*!************************************************************************** 
*: C201371,1 SAB 07/28/2011 Error when open "Import Account/Store info" screen after "Sales order Status/Ship Status" [Start]
*FUNCTION lfwOGWhen
FUNCTION lfOGWhen
*: C201371,1 SAB 07/28/2011 Error when open "Import Account/Store info" screen after "Sales order Status/Ship Status" [End]
IF EMPTY(lcRpPath)
  IF FILE(oariaApplication.DataDir+'ImportFl'+'.MEM')
    RESTORE FROM oariaApplication.DataDir+'ImportFl'+'.MEM' ADDITIVE
  ENDIF
ENDIF

*!**************************************************************************
*! Name      : lfCreatExp
*! Developer : Mariam Mazhar (MMT)
*! Date      : 04/10/2011
*! Purpose   : Import function
*!************************************************************************** 
FUNCTION lfCreatExp
IF EMPTY(lcRpPath) 
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Name or Path")
  RETURN .F.
ENDIF 
IF !EMPTY(lcRpPath) 
  lcDir = JUSTPATH(lcRpPath)
  IF !DIRECTORY(lcDir)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File Path")
    RETURN .F.
  ENDIF   
  IF !FILE(lcRpPath)
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File")
    RETURN .F.
  ENDIF
  IF UPPER(JUSTEXT(lcRpPath)) <> 'XLS'
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File")
    RETURN .F.
  ENDIF  
ENDIF 
SAVE TO oariaApplication.DataDir+'ImportFl'+'.MEM' ALL LIKE lcRpPath*

XL_File = lcRpPath
oXL = CreateObject('Excel.Application')
oXL.Workbooks.Open(XL_File)
lnSheetCount = oXL.Worksheets.Count
Dimension laSheet[lnSheetCount]
For lnCntSheet = 1 to lnSheetCount 
  laSheet[lnCntSheet] = oXL.Worksheets.Item(lnCntSheet).Name
Next
oXL.Workbooks.Close()
oXL.Quit
oXL = Null
Release oXL
DBF_File = JUSTSTEM(XL_File) + '.DBF'
DIMENSION laDbfsLoc[ALen(laSheet),2]
STORE '' TO laDbfsLoc
lcDefaPth = FULLPATH('')
SET DEFAULT TO (oAriaApplication.workdir)
For lnCnt = 1 to ALen(laSheet)  
 lcTmpName = loogscroll.gfTempName()
 IMPORT  from (XL_File) type XL8 Sheet laSheet[lnCnt] 
 GO RECORD 1
 FOR lnD = 1 TO FCOUNT()
   IF  ALLTRIM(UPPER(EVALUATE(FIELD(lnD)))) = "CARHARTT_BILLTO_ID"
     laDbfsLoc[lnCnt,2] = 'BILLACCT'
     EXIT 
   ENDIF
   IF  ALLTRIM(UPPER(EVALUATE(FIELD(lnD)))) = "CARHARTT_SHIPTOID"
     laDbfsLoc[lnCnt,2] = 'SHIPLOC'
     EXIT      
   ENDIF
   IF  ALLTRIM(UPPER(EVALUATE(FIELD(lnD)))) = "REF_CARHARTT_APPLYSTO" 
     laDbfsLoc[lnCnt,2] = 'INSTRUCT'
     EXIT      
   ENDIF
 ENDFOR  
 USE  
 Rename (DBF_File) to (ALLTRIM(lcTmpName) + '.DBF')
 laDbfsLoc[lnCnt,1] = ADDBS(oAriaApplication.workdir)+ALLTRIM(lcTmpName) + '.DBF'
NEXT 
SET DEFAULT TO  (lcDefaPth)
lcMapAcc = loogscroll.gfTempName()
DIMENSION laMapAccStur[2,4]
laMapAccStur[1,1] = 'Bill_To_ID'
laMapAccStur[1,2] = 'C'
laMapAccStur[1,3] = 5
laMapAccStur[1,4] = 0

laMapAccStur[2,1] = 'ACCOUNT'
laMapAccStur[2,2] = 'C'
laMapAccStur[2,3] = 5
laMapAccStur[2,4] = 0

=gfCrtTmp(lcMapAcc ,@laMapAccStur,'Bill_To_ID',lcMapAcc ,.f.)

IF !USED('Customer')
  =gfOpenTable('Customer','Customer')
ENDIF
IF !EMPTY(laDbfsLoc[1,2])
  lnBillLoc = ASCAN(laDbfsLoc,'BILLACCT')
  IF lnBillLoc =0
    =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File")
    RETURN .F.
  ELSE  
    lnBillLoc = ASUBSCRIPT(laDbfsLoc,lnBillLoc,1)
  ENDIF 
  IF FILE(laDbfsLoc[lnBillLoc,1])
    USE (laDbfsLoc[lnBillLoc,1]) IN 0 SHARED 
    *Import Main Account Records
    *
    lcKeyAcc = SPACE(3)
    lnAccCnt = 0
    lcFileToSelect =JUSTSTEM(laDbfsLoc[lnBillLoc,1])
    SELECT(lcFileToSelect)
    INDEX on ALLTRIM(C) TAG (lcFileToSelect)
    LOCATE 
    SCAN FOR RECNO()>1 AND !SEEK(ALLTRIM(C),lcMapAcc)
	    IF lcKeyAcc <> SUBSTR(ALLTRIM(C),1,3)
        lnAccCnt = 0
      ENDIF
      SELECT CUSTOMER 
      =gfSeek('M')
      LOCATE REST WHILE TYPE+ACCOUNT+STORE = 'M' FOR ALLTRIM(usr_dfnd1) = ALLTRIM(&lcFileToSelect..C)
      IF !FOUND()
        lcKeyAcc = SUBSTR(ALLTRIM(&lcFileToSelect..C),1,3)
        lcNewAccount = SUBSTR(ALLTRIM(&lcFileToSelect..C),1,3)+PADL(ALLTRIM(STR(lnAccCnt)),2,'0')
      
        DO WHILE gfSEEK('M'+lcNewAccount,'Customer')
          lcNewAccount = SUBSTR(ALLTRIM(&lcFileToSelect..C),1,3)+PADL(ALLTRIM(STR(lnAccCnt)),2,'0')
          lnAccCnt = lnAccCnt + 1
        ENDDO
        SELECT Customer 
        APPEND BLANK 

      ELSE
        lcNewAccount = Customer.Account
        SELECT Customer 
      ENDIF


      REPLACE TYPE       WITH 'M',;
      		  BILLTO    WITH 'M',;
      		  sTATUS     WITH 'A',;
            Account    WITH lcNewAccount,;
            BtName     WITH &lcFileToSelect..F,;
            caddress12 WITH &lcFileToSelect..G,;
            caddress22 WITH &lcFileToSelect..H,;
            caddress32 WITH &lcFileToSelect..I,;             
            caddress42 WITH &lcFileToSelect..J,;
            caddress52 WITH &lcFileToSelect..K,;
            caddress62 WITH &lcFileToSelect..L,;
            stname     WITH &lcFileToSelect..F,;
            caddress1  WITH &lcFileToSelect..G,;
            caddress2  WITH &lcFileToSelect..H,;
            caddress3  WITH &lcFileToSelect..I,;             
            caddress4  WITH &lcFileToSelect..J,;
            caddress5  WITH &lcFileToSelect..K,;
            caddress6  WITH &lcFileToSelect..L,;
            ccont_code WITH &lcFileToSelect..L,;
            ccont_cod2 WITH &lcFileToSelect..L,;
            ccurrcode  WITH &lcFileToSelect..M,;
            phone1     WITH &lcFileToSelect..D,;
            fax        WITH &lcFileToSelect..E,;
            usr_dfnd1 WIth &lcFileToSelect..C
       *: C201324,2 MMT 07/07/2011 Update Customer Codes fields with the Default Code Value[Start]
        REPLACE CLASS     WITH lfDefCode('CLASS'),;
								ctermcode WITH lfDefCode('CTERMCODE'),;
								cdivision WITH lfDefCode('CDIVISION'),;
								region    WITH lfDefCode('REGION'),;
								shipvia   WITH lfDefCode('SHIPVIA'),;
								spcinst   WITH lfDefCode('SPCINST')
       *: C201324,2 MMT 07/07/2011 Update Customer Codes fields with the Default Code Value[End]       
       =gfAdd_Info('Customer')       
       =gfReplace("")       
       SELECT (lcMapAcc)               
       APPEND BLANK 
       REPLACE Bill_To_ID WITH ALLTRIM(&lcFileToSelect..C),;
               ACCOUNT WITH lcNewAccount 
       
    ENDSCAN
    USE IN (lcFileToSelect)
    SELECT Customer 
    =gfTableUpdate()
    *Ship To locations (Stores)
		lnShpLoc = ASCAN(laDbfsLoc,'SHIPLOC')
    IF lnShpLoc <> 0
      lnShpLoc = ASUBSCRIPT(laDbfsLoc,lnShpLoc ,1)
      IF FILE(laDbfsLoc[lnShpLoc ,1])
        USE (laDbfsLoc[lnShpLoc ,1]) IN 0 SHARED 
        lcLocFile = JUSTSTEM(laDbfsLoc[lnShpLoc ,1])
        SELECT(lcLocFile)
        INDEX on  C TAG (lcLocFile)
        LOCATE 
        SCAN FOR RECNO()>1 AND SEEK(ALLTRIM(&lcLocFile..C),lcMapAcc) AND gfSEEK('M'+&lcMapAcc..Account,'Customer') 
          SELECT Customer
          IF gfSEEK('M'+&lcMapAcc..Account,'Customer') 
            SCATTER MEMO MEMVAR 
          ELSE
            LOOP
          ENDIF
          IF !gfSeek('S'+&lcMapAcc..Account+ALLTRIM(&lcLocFile..D),'Customer')
            APPEND BLANK 
            GATHER MEMO MEMVAR 
          ENDIF  
          REPLACE TYPE WITH 'S',;
          				BILLTO WITH 'M',;
          				Store  WITH &lcLocFile..D,;
							    caddress1 WITH &lcLocFile..I+&lcLocFile..J,;
			            caddress2 WITH &lcLocFile..K,;
			            caddress3 WITH &lcLocFile..L,;             
			            caddress4 WITH &lcLocFile..M,;
			            caddress5 WITH &lcLocFile..N,;
			            caddress6 WITH &lcLocFile..O,;
			            stname     WITH &lcLocFile..H
        =gfAdd_Info('Customer')       
        =gfReplace("")       
        ENDSCAN 
        USE IN (lcLocFile)
		    SELECT Customer 
		    =gfTableUpdate()
      ENDIF  
    ENDIF    
    *INSTRUCT
    *Instructions
		lnShpInst = ASCAN(laDbfsLoc,'INSTRUCT')
    IF lnShpInst<> 0
      lnShpInst= ASUBSCRIPT(laDbfsLoc,lnShpInst,1)
      IF FILE(laDbfsLoc[lnShpInst,1])
        USE (laDbfsLoc[lnShpInst,1]) IN 0 SHARED 
        lcInstFile = JUSTSTEM(laDbfsLoc[lnShpInst,1])
        SELECT(lcInstFile)
        INDEX ON D TAG (lcInstFile)
        LOCATE 
        IF !USED('NotePad')
          =gfOpenTable('notepad','notepad')
        ENDIF
        SELECT(lcInstFile)
        SCAN FOR RECNO()>1 
          lcDashPos = ATC('-',ALLTRIM(D))
          IF lcDashPos > 0
            lcAccount = SUBSTR(ALLTRIM(D),1,lcDashPos-1)
            lcStore  = SUBSTR(ALLTRIM(D),lcDashPos+1)
          ELSE
            lcAccount = SUBSTR(ALLTRIM(D),1,5)
            lcStore  = ''
					ENDIF
					*SEEK(ALLTRIM(C),lcMapAcc) AND gfSEEK('M'+&lcMapAcc..Account,'Customer') AND !gfSeek('S'+&lcMapAcc..Account+ALLTRIM(D),'Customer')  
					IF !Seek(lcAccount,lcMapAcc) OR !gfSEEK('M'+&lcMapAcc..Account,'Customer') OR (!EMPTY(lcStore) AND !gfSeek('S'+&lcMapAcc..Account+ALLTRIM(lcStore),'Customer'))
					  LOOP 
					ENDIF
*					IF !EMPTY(lcStore) 
					  IF !gfSeek('A'+&lcMapAcc..Account+ALLTRIM(lcStore),'NOTEPAD')
					    SELECT NOTEPAD
					    APPEND BLANK 
					    REPLACE TYPE WITH 'A',;
					    				KEY  WITH &lcMapAcc..Account+ALLTRIM(lcStore),;
					    				cDesc WITH "Notes For Account Number : "+&lcMapAcc..Account+ALLTRIM(lcStore),;
					    				mnotes WITH &lcInstFile..B+":"+CHR(13)+CHR(10)+&lcInstFile..E
			        =gfAdd_Info('NOTEPAD')       
      			  =gfReplace("")       
					  ELSE
  					  SELECT NOTEPAD
					    REPLACE mnotes WITH mnotes+CHR(13)+CHR(10) +CHR(13)+CHR(10)+&lcInstFile..B+":"+CHR(13)+CHR(10)+&lcInstFile..E
					  ENDIF
        ENDSCAN 
        USE IN (lcInstFile)
        SELECT NOTEPAD
        =gfTableUpdate('')
			ENDIF        
		ENDIF    

    lcMessage = ALLTRIM(STR(RECCOUNT(lcMapAcc)))+' Accounts are Imported successfully'
    = gfModalGen('INM00000B00000','F','ALERT',' ',lcMessage)
		SELECT(lcMapAcc)
  	lcFileAcc = gftempname()
  	EXPORT TO (oAriaapplication.workdir+lcFileAcc+'.xls') TYPE  XLS
  	IF FILE(oAriaapplication.workdir+lcFileAcc+'.xls')
	    loRun = CreateObject("WScript.Shell")
	    loRun.Run(oAriaapplication.workdir+lcFileAcc+'.xls', 3)
  	  loRun = NULL
	  ENDIF	
	ELSE
	  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File")
	  RETURN .F.
	ENDIF
ELSE
  =gfModalGen('INM00000B00000',.F.,.F.,.F.,"Invalid File")
  RETURN .F.
ENDIF

*: C201324,2 MMT 07/07/2011 Update Customer Codes fields with the Default Code Value[Start]
*!**************************************************************************
*! Name      : lfDefCode
*! Developer : Mariam Mazhar (MMT)
*! Date      : 07/07/2011
*! Purpose   : Get Default Code
*!************************************************************************** 
FUNCTION lfDefCode
LPARAMETERS lcCodeFld
lcOldSel = SELECT()
IF !USED('CODES_A')
  =gfOpenTable('codes','CCODE_NO','SH','CODES_A')
ENDIF
lcReturnValue = ''
IF gfSeek('D'+PADR(lcCodeFld,10),'CODES_A')
  lcReturnValue = CODES_A.CCODE_NO
ENDIF
SELECT(lcOldSel)

RETURN lcReturnValue 
*: C201324,2 MMT 07/07/2011 Update Customer Codes fields with the Default Code Value[Start]