*:************************************************************************
*:  Program File: Aria4xp/PRGS/SM/SMDATLG.PRG
*:  Module      : System Manager
*:  System      : Aria4xp
*:  Developer   : ES - Esraa
*:  Date        : 07/05/2020
*:************************************************************************
*E612181,1 Es 07/06/2020  Aria 5 - Data Maintenance Log [P20190703.0001]
*B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003]
*:************************************************************************
*E612181,1 Es 07/06/2020  Aria 5 - Data Maintenance Log [P20190703.0001]
*E612181,1 MMT 09/27/2020  Aria 5 - Data Maintenance Log - Use Audtrail table [P20190703.0001]
DO FORM (oAriaApplication.ScreenHome+"\SM\SMDATLG.SCX")
RETURN

*!*************************************************************
*! Name      : lfCreateTemp
*! Date      : 07/06/2020
*! Purpose   : Create Temp
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfCreateTemp
PARAMETERS loformset ,lcChangeID

SET STEP ON 
*!*	loformset.ariaform1.cbostatus.AddItem('Pending')
*!*	loformset.ariaform1.cbostatus.AddItem('Approved')
*!*	loformset.ariaform1.cbostatus.ListIndex=1

*Create Temp File (loformset.lcTempdatachangeheader) with the Same stucture of DataChangeHeader
SELECT datachangeheader
=AFIELDS(laHeaderStru)
FOR i=1 TO ALEN(laHeaderStru,1)
	IF LEN(laHeaderStru(i,1)) >10
		*lcTmpVarHeader=  STRTRAN(lcVarHeader, laHeaderStru(i,1), SUBSTR(laHeaderStru(i,1),1,10))
		laHeaderStru(i,1)=SUBSTR(laHeaderStru(i,1),1,10)
	ENDIF
ENDFOR
loformset.lcTempdatachangeheader = gfTempName()
=gfCrtTmp(loformset.lcTempdatachangeheader,@laHeaderStru,"CAUDTRALID+CEVNTOBJID")


*Create Temp File (loformset.lcTempdatachangelines) with the Same stucture of datachangelines
DIMENSION laLinesStru[4,4]

laLinesStru[1,1] = 'Key'
laLinesStru[1,2] = 'C'
laLinesStru[1,3] = 40
laLinesStru[1,4] = 0

laLinesStru[2,1] = 'OldValue'
laLinesStru[2,2] = 'C'
laLinesStru[2,3] = 30
laLinesStru[2,4] = 0

laLinesStru[3,1] = 'NewValue'
laLinesStru[3,2] = 'C'
laLinesStru[3,3] = 30
laLinesStru[3,4] = 0

laLinesStru[4,1] = 'Field_name'
laLinesStru[4,2] = 'C'
laLinesStru[4,3] = 30
laLinesStru[4,4] = 0

loformset.lcTempdatachangelines = gfTempName()
=gfCrtTmp(loformset.lcTempdatachangelines,@laLinesStru,"Key+Field_name")



*Get Header Data
SELECT datachangeheader
=gfSQLRun("Select * From Audtrail where CAUDTRALID = '"+lcChangeID+"' AND [Key1]= 'Data Maintenance'","datachangeheader",.T.,'TmpAud')
IF USED('TmpAud') AND RECCOUNT('TmpAud') > 0
*IF gfseek(lcChangeID)
   
	lcTableName=ALLTRIM(TmpAud.CEVNTOBJID)
	
*B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][Start]
IF !USED('SYDFILES_T')
	=gfOpenTable('SYDFILES','CFILE_NAM','SH','SYDFILES_T')
ENDIF
= GFSEEK(lcTableName,'SYDFILES_T')
llSQLFile = (SYDFILES_T.CVER = 'A40')
IF llSQLFile 
  lnFldResult = oAriaApplication.RemoteSystemData.Execute( ;
   "SELECT SYDFIELD.* FROM SYDFIELD,SYDFLFLD WHERE SYDFIELD.cfld_name == SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam == '" + ;
   ALLTRIM(UPPER(lcTableName)) + "' AND (SYDFIELD.CVER='A40' OR EMPTY(SYDFIELD.CVER))", ;
   '', ;
   "sydfieldtmp", ;
   "", ;
   oAriaApplication.cAria4Sysfiles, ;
   3, ;
   "", ;
   loFormSet.DatasessionID)
ELSE
	lnFldResult = oAriaApplication.remotesystemdata.execute("SELECT SYDFIELD.* FROM SYDFIELD,SYDFLFLD WHERE SYDFIELD.cfld_name == SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam == '"+;
	             lcTableName+"' AND (SYDFIELD.CVER='A27' OR EMPTY(SYDFIELD.CVER))",'',;
  "sydfieldtmp","",oAriaApplication.SystemConnectionString,3,'',loFormSet.DatasessionID )
ENDIF
*B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][End]
	
	loformset.ariaform1.kbChangeID.keytextbox.Value=ALLTRIM(TmpAud.CAUDTRALID)
	loformset.ariaform1.kbfileName.keytextbox.Value=ALLTRIM(TmpAud.CEVNTOBJID)
	loformset.ariaform1.kbuserName.keytextbox.Value=ALLTRIM(TmpAud.cadd_user)

*	loformset.ariaform1.cbostatus.ListIndex=IIF(UPPER(ALLTRIM(cstatus))='P',1,2)
	loformset.ariaform1.txtticket.Value=ALLTRIM(TmpAud.ticketno)
	loformset.ariaform1.DtpDateOfChange.Value=TmpAud.dadd_date
	loformset.ariaform1.txtreason.Value=ALLTRIM(TmpAud.changereason)
    lcIndex = ''
	lcVar= ''
	SELECT SYDINDEX_T
	IF ALLTRIM(UPPER(lcTableName)) == 'STYLE'
      SELECT SYDINDEX_T
	  =GFSEEK(lcTableName)
	  LOCATE REST WHILE cfile_nam=lcTableName FOR lunique=.T.
	  IF FOUND()
		lcVar=SYDINDEX_T.cindx_exp
		lcIndex=SYDINDEX_T.cfile_tag
	  ENDIF
	ENDIF

	IF EMPTY(lcVar)
	    SELECT SYDINDEX_T
		=GFSEEK(lcTableName)
		LOCATE REST WHILE cfile_nam=lcTableName FOR cfile_tag=lcTableName
		IF FOUND()
			lcVar=SYDINDEX_T.cindx_exp
			lcIndex=SYDINDEX_T.cfile_tag
		ENDIF
	ENDIF
    
    *XXX
*!*		IF EMPTY(lcVar)
*!*		    SELECT SYDINDEX_T
*!*			IF GFSEEK(lcTableName)
*!*				lcVar=SYDINDEX_T.cindx_exp
*!*				lcIndex=SYDINDEX_T.cfile_tag
*!*			ENDIF
*!*		ENDIF
	IF EMPTY(lcVar)
	    SELECT SYDINDEX_T
		IF GFSEEK(lcTableName)
		  LOCATE REST WHILE cfile_nam=lcTableName FOR !'(' $ SYDINDEX_T.cindx_exp
	      IF FOUND()
			lcVar=SYDINDEX_T.cindx_exp
			lcIndex = SYDINDEX_T.cfile_tag
  		  ELSE
		    IF GFSEEK(lcTableName)
		   	  lcVar=SYDINDEX_T.cindx_exp
			  lcIndex =SYDINDEX_T.cfile_tag
  		    ENDIF
		  ENDIF
		ENDIF
	ENDIF
	*XXX
	

	IF !USED(lcTableName)
		=gfOpenTable(lcTableName,ALLTRIM(lcIndex),'SH',lcTableName)
	ENDIF


*Create Temp File (loformset.lcTempFile) with the Same stucture of TableName
	SELECT(lcTableName)
	=AFIELDS(laTableStru)
	lnTableStru = ALEN(laTableStru,1)
	DIMENSION laTableStru[lnTableStru+2,18]
	laTableStru[lnTableStru+1,1] = 'recStatus'
	laTableStru[lnTableStru+1,2] = 'C'
	laTableStru[lnTableStru+1,3] = 1
	laTableStru[lnTableStru+1,4] = 0


	laTableStru[lnTableStru+2,1] = 'ckey'
	laTableStru[lnTableStru+2,2] = 'C'
	laTableStru[lnTableStru+2,3] = 40
	laTableStru[lnTableStru+2,4] = 0

	STORE '' TO laTableStru[lnTableStru+1,7],laTableStru[lnTableStru+1,8],laTableStru[lnTableStru+1,9],;
		laTableStru[lnTableStru+1,10],laTableStru[lnTableStru+1,11],laTableStru[lnTableStru+1,12],;
		laTableStru[lnTableStru+1,13],laTableStru[lnTableStru+1,14],laTableStru[lnTableStru+1,15],;
		laTableStru[lnTableStru+1,16]


	STORE '' TO laTableStru[lnTableStru+2,7],laTableStru[lnTableStru+2,8],laTableStru[lnTableStru+2,9],;
		laTableStru[lnTableStru+2,10],laTableStru[lnTableStru+2,11],laTableStru[lnTableStru+2,12],;
		laTableStru[lnTableStru+2,13],laTableStru[lnTableStru+2,14],laTableStru[lnTableStru+2,15],;
		laTableStru[lnTableStru+2,16]

	loformset.lcTempFile= gfTempName()
	=gfCrtTmp(loformset.lcTempFile,@laTableStru,lcVar)


	SELECT (loformset.lcTempFile)
	INDEX on ckey TAG ckeyIndex
	SET ORDER to ckeyIndex
	AFIELDS(laTempFileColumns)

    SELECT 'TmpAud'
    LOCATE 
    SCAN
      IF .f. && gfSeek(ALLTRIM(TmpAud.Key),lcTableName) 
        SELECT(lcTableName)
    	SCATTER MEMVAR MEMO
    	m.cKey = TmpAud.Key
    	m.recStatus = IIF(TmpAud.cEvent_ID='DELETE', 'D', IIF(TmpAud.cEvent_ID='UPDATE', 'M','N')) 
    	INSERT INTO (loformset.lcTempFile) FROM MEMVAR
    	IF TmpAud.cEvent_ID='UPDATE'
    	  *B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][Start]
*!*	    	  =gfSQLRun("select * from audtrail where audtrail.cevntobjid = '"+TmpAud.cevntobjid+"' and [key] = '"+TmpAud.Key+;
*!*	    	            "' and caudtralid <> '"+TmpAud.caudtralid +"' AND (dadd_date < '"+DTOC(TmpAud.dadd_date) +;
*!*	    	            "' OR (dadd_date = '"+DTOC(TmpAud.dadd_date) +;
*!*	    	            "' AND convert(Time,cadd_time, 8)  < convert(Time,'"+;
*!*	    	            TmpAud.cadd_time+"', 8))) order by dadd_date,convert(Time, cadd_time, 8) Desc","datachangeheader",.T.,'TmpAudOld')
*!*	    	  IF USED('TmpAudOld') AND RECCOUNT('TmpAudOld') > 0
          IF !EMPTY(ALLTRIM(TmpAud.MFLDFDATA))
    	  *B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][End]
*!*	    	    SELECT TmpAudOld
*!*	    	    LOCATE 
    	    m.Key = TmpAud.Key
    	    DIMENSION laOldDataInf[1]
            =gfSubstr(ALLTRIM(TmpAud.MFLDFDATA),@laOldDataInf,CHR(13)+CHR(10))     	    

    	    DIMENSION laDataInf[1]
            =gfSubstr(ALLTRIM(TmpAud.mneededinf ),@laDataInf,CHR(13)+CHR(10))     	    
            
            FOR lnx =1 TO ALEN(laDataInf,2)     
               IF TYPE('laOldDataInf[1,lnX]') <>'U' AND  !(laDataInf[1,lnX] == laOldDataInf[1,lnX])
                 lnEqPos = ATC("=", laDataInf[1,lnX] ,1)
                 m.Field_name = SUBSTR(laDataInf[1,lnX] ,1,lnEqPos-1)
                 m.OldValue = SUBSTR(laOldDataInf[1,lnX] ,lnEqPos+1)
                 m.NewValue = SUBSTR(laDataInf[1,lnX] ,lnEqPos+1)
                 INSERT INTO (loformset.lcTempdatachangelines) FROM MEMVAR 
               ENDIF
            ENDFOR 
       ENDIF     
    ENDIF
    ELSE
      *IF TmpAud.cEvent_ID='DELETE'
        m.cKey = TmpAud.Key
        DIMENSION laDataInf[1]
        =gfSubstr(ALLTRIM(TmpAud.mneededinf ),@laDataInf,CHR(13)+CHR(10)) 
        m.recStatus =IIF(TmpAud.cEvent_ID='DELETE', 'D', IIF(TmpAud.cEvent_ID='UPDATE', 'M','N')) 
        FOR lnx =1 TO ALEN(laDataInf,2)     
           lnEqPos = ATC("=", laDataInf[1,lnX] ,1)
           SELECT sydfieldtmp
           LOCATE FOR ALLTRIM(cfld_head) ==  ALLTRIM(SUBSTR(laDataInf[1,lnX] ,1,lnEqPos-1))
           IF FOUND()
             lcField_name = sydfieldtmp.CFLD_NAME
             m.&lcField_name. = IIF(sydfieldtmp.cdata_typ='D',CTOD(SUBSTR(laDataInf[1,lnX] ,lnEqPos+1)),;
                                IIF(sydfieldtmp.cdata_typ='N',VAL(SUBSTR(laDataInf[1,lnX] ,lnEqPos+1)),;
                                IIF(sydfieldtmp.cdata_typ='L',(SUBSTR(laDataInf[1,lnX] ,lnEqPos+1)='True'),SUBSTR(laDataInf[1,lnX] ,lnEqPos+1))))
             
           ENDIF
        ENDFOR 
        INSERT INTO (loformset.lcTempFile) FROM MEMVAR 
         IF !EMPTY(ALLTRIM(TmpAud.MFLDFDATA))
    	  *B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][End]
*!*	    	    SELECT TmpAudOld
*!*	    	    LOCATE 
    	    m.Key = TmpAud.Key
    	    DIMENSION laOldDataInf[1]
            =gfSubstr(ALLTRIM(TmpAud.MFLDFDATA),@laOldDataInf,CHR(13)+CHR(10))     	    

    	    DIMENSION laDataInf[1]
            =gfSubstr(ALLTRIM(TmpAud.mneededinf ),@laDataInf,CHR(13)+CHR(10))     	    
            
            FOR lnx =1 TO ALEN(laDataInf,2)     
               IF TYPE('laOldDataInf[1,lnX]') <>'U' AND  !(laDataInf[1,lnX] == laOldDataInf[1,lnX])
                 lnEqPos = ATC("=", laDataInf[1,lnX] ,1)
                 m.Field_name = SUBSTR(laDataInf[1,lnX] ,1,lnEqPos-1)
                 m.OldValue = SUBSTR(laOldDataInf[1,lnX] ,lnEqPos+1)
                 m.NewValue = SUBSTR(laDataInf[1,lnX] ,lnEqPos+1)
                 INSERT INTO (loformset.lcTempdatachangelines) FROM MEMVAR 
               ENDIF
            ENDFOR 
       ENDIF 
      *ENDIF
    ENDIF
ENDSCAN      
*Get Lines Data
     
    
*!*	*New
*!*		SELECT datachangelines
*!*		=GFSEEK(lcChangeID)
*!*		LOCATE REST WHILE CHANGEID+HEADEROID+CFIELD_NAME=lcChangeID FOR  changetype='A'
*!*		IF FOUND()
*!*			SELECT (loformset.lcTempFile)
*!*			APPEND BLANK
*!*			replace recStatus  WITH 'A'

*!*			SELECT datachangelines
*!*			SCAN FOR changetype='A'
*!*				lcfield_name=DataChangeLines.cfield_name
*!*				lcOldValue=DataChangeLines.oldvalue
*!*				lcNewValue=DataChangeLines.newvalue
*!*				lcStatus ='New'

*!*				SELECT (loformset.lcTempFile)
*!*				lctype=TYPE("&lcfield_name")
*!*				IF lctype!='C'
*!*					lcNewValue=IIF(lctype='N',VAL(lcNewValue),IIF(lctype='D',CTOD(lcNewValue),IIF(lctype='L',IIF(UPPER(lcNewValue)='FALSE',.F.,.T.),lcNewValue)))
*!*				ENDIF
*!*				replace &lcfield_name WITH  lcNewValue
*!*			ENDSCAN
*!*		ENDIF


*!*	*Modified
*!*		SELECT datachangelines
*!*		=GFSEEK(lcChangeID)
*!*		LOCATE REST WHILE CHANGEID+HEADEROID+CFIELD_NAME=lcChangeID FOR  changetype='M'
*!*		IF FOUND()
*!*			SELECT datachangelines
*!*			SCAN FOR changetype='M'
*!*				lcfield_name=DataChangeLines.cfield_name
*!*				lcOldValue=DataChangeLines.oldvalue
*!*				lcNewValue=DataChangeLines.newvalue
*!*				lckey=ALLTRIM(datachangelines.ckey)
*!*				lcStatus ='Modified'
*!*				IF lcOldValue<>lcNewValue OR CSTATUS  ='M'
*!*					SELECT SYDFIELD
*!*					IF GFSEEK(UPPER(ALLTRIM(lcfield_name)))
*!*						lcfld_head=SYDFIELD.cfld_head
*!*					ENDIF
*!*					SELECT datachangelines
*!*					SCATTER MEMVAR MEMO
*!*					INSERT INTO (loformset.lcTempdatachangelines) FROM MEMVAR
*!*					SELECT (loformset.lcTempdatachangelines)
*!*					replace cfield_nam WITH datachangelines.cfield_name;
*!*						Field_name WITH lcfld_head;
*!*						ckey WITH lckey
*!*				ENDIF

*!*				SELECT (loformset.lcTempFile)
*!*				lctype=TYPE("&lcfield_name")
*!*				IF lctype!='C'
*!*					lcOldValue=IIF(lctype='N',VAL(lcOldValue),IIF(lctype='D',CTOD(lcOldValue),IIF(lctype='L',IIF(UPPER(lcOldValue)='FALSE',.F.,.T.),lcOldValue)))
*!*				ENDIF


*!*				IF !gfseek(lckey)
*!*					APPEND BLANK
*!*					replace recStatus  WITH 'M' ;
*!*						ckey WITH lckey;
*!*						&lcfield_name WITH  lcOldValue
*!*				ELSE
*!*					replace &lcfield_name WITH  lcOldValue
*!*				ENDIF
*!*			ENDSCAN
*!*		ENDIF


*!*	*Delete
*!*		SELECT datachangelines
*!*		=GFSEEK(lcChangeID)
*!*		LOCATE REST WHILE CHANGEID+HEADEROID+CFIELD_NAME=lcChangeID FOR  changetype='D'
*!*		IF FOUND()
*!*			SELECT datachangelines
*!*			SCAN FOR changetype='D'
*!*				lcfield_name=DataChangeLines.cfield_name
*!*				lcOldValue=DataChangeLines.oldvalue
*!*				lcNewValue=DataChangeLines.newvalue
*!*				lckey =ALLTRIM(DataChangeLines.ckey)
*!*				lcStatus ='Deleted'
*!*				SELECT (loformset.lcTempFile)
*!*				lctype=TYPE("&lcfield_name")
*!*				IF lctype!='C'
*!*					lcOldValue=IIF(lctype='N',VAL(lcOldValue),IIF(lctype='D',CTOD(lcOldValue),IIF(lctype='L',IIF(UPPER(lcOldValue)='FALSE',.F.,.T.),lcOldValue)))
*!*				ENDIF

*!*				IF !gfseek(lckey)
*!*					APPEND BLANK
*!*					replace recStatus  WITH 'D'  ;
*!*						ckey WITH lckey;
*!*						&lcfield_name WITH  lcOldValue
*!*				ELSE
*!*					replace &lcfield_name WITH  lcOldValue
*!*				ENDIF
*!*			ENDSCAN
*!*		ENDIF

SET STEP ON 
    SELECT (loformset.lcTempFile)
    SET RELATION TO cKey INTO   (loformset.lcTempdatachangelines)
    LOCATE 
    
	WITH loformset.ariaform1.grdFileData
		.RecordSource = ""
		.RecordSource = loformset.lcTempFile
		.ColumnCount =  ALEN(laTempFileColumns,1)-1
		.column1.header1.Caption='Status'

*ALEN(laTempFileColumns,1)-2 => (Don't Display *(Recstatus & ckey) columns
		FOR i=1 TO ALEN(laTempFileColumns,1)-2
			SELECT (loformset.lcTempFile)
			LOCATE
			.column1.ControlSource="IIF(recStatus='D', 'Deleted', IIF(recStatus='M', 'Modified','New') )"
			.column1.DynamicBackColor = "IIF(recStatus='D', RGB(255,0,0), IIF(recStatus='M',RGB(255,255,0), RGB(0,255,0)) )"

			lcindex=ALLTRIM(STR(i+1))
			.Column&lcindex..Header1.Caption = laTempFileColumns(i,1)
			.column&lcindex..ControlSource =loformset.lcTempFile+"."+laTempFileColumns(i,1)+""
		ENDFOR
		.SETALL('ReadOnly',.T.,'COLUMN')
		.Enabled = .T.
		.refresh()
	ENDWITH
	WITH loformset.ariaform1.grdDataChangeLines
	   	 .RecordSource = ""
    		.RecordSource = loformset.lcTempdatachangelines

			.column1.ControlSource = loformset.lcTempdatachangelines+".Field_name"
			.column2.ControlSource = loformset.lcTempdatachangelines+".oldvalue"
			.column3.ControlSource = loformset.lcTempdatachangelines+".newvalue"

		.SETALL('ReadOnly',.T.,'COLUMN')
		.Enabled = .T.
		.refresh()
	ENDWITH




	loformset.ariaform1.grdfileData.AfterRowColChange()
    loformset.ChangeMode('V')


ENDIF
*!*************************************************************
*! Name      : lfvChangeID
*! Date      : 07/06/2020
*! Purpose   : Select Changed ID
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvChangeID
PARAMETERS loformset

SET STEP ON 

DIMENSION laTmpVal[1]
laTmpVal= ''
IF !USED('DATA_HEADER')
 =gfOpenTable('AUDTRAIL','CHNGHEADER','SH','DATA_HEADER')
ENDIF
SELECT DATA_HEADER
IF loformset.llfrombrowse OR '?' $ ALLTRIM(loformset.ariaform1.kbChangeID.keytextbox.Value)
  SELECT DATA_HEADER
   
	  
	*B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][Start]
	*=gfseek('Data Maintenance') 
	*SELECT DISTINCT CAUDTRALID,CAdd_user,dAdd_Date,CEVNTOBJID FROM DATA_HEADER INTO CURSOR 'DATA_HEADER_1' ORDER BY CAUDTRALID
	=gfSQLRun("SELECT DISTINCT CAUDTRALID,CAdd_user,dAdd_Date,CEVNTOBJID From Audtrail where [Key1]= 'Data Maintenance'","datachangeheader",.T.,'DATA_HEADER_1')
	SELECT DATA_HEADER_1
	*B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][End]
	lcBrfields ="CAUDTRALID:20:H='Change ID',CAdd_user:30:H='Created by',;
	               dAdd_Date:30:H='Create Date',CEVNTOBJID:30:H='File Name'"

    *B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][Start]
    *=ariabrow("",'DATA_HEADER', 20,.F.,20,.F.,.F.,.F.,"CAUDTRALID,CAdd_user,dAdd_Date,CEVNTOBJID",'laTmpVal',.F.,.F.,.F.)
	=ariabrow("",'DATA_HEADER_1', 20,.F.,20,.F.,.F.,.F.,"CAUDTRALID,CAdd_user,dAdd_Date,CEVNTOBJID",'laTmpVal',.F.,.F.,.F.)
    *B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][End]
ELSE
   llBrowChange = .T.
   IF !EMPTY(ALLTRIM(loformset.ariaform1.kbChangeID.keytextbox.Value))
     =gfSQLRun("Select * From Audtrail where CAUDTRALID = '"+ALLTRIM(loformset.ariaform1.kbChangeID.keytextbox.Value)+"' AND [Key1]= 'Data Maintenance'","DATA_HEADER",.T.,'TmpAud')
     IF USED('TmpAud') AND RECCOUNT('TmpAud') > 0
       llBrowChange = .F.
     ENDIF
   ENDIF  
	IF EMPTY(ALLTRIM(loformset.ariaform1.kbChangeID.keytextbox.Value)) OR  llBrowChange &&!GFSEEK(UPPER(ALLTRIM(loformset.ariaform1.kbChangeID.keytextbox.Value)))
		SELECT DataChangeHeader
        =gfseek('Data Maintenance')
		
		lcBrfields ="CAUDTRALID:20:H='Change ID',CAdd_user:30:H='Created by',;
	               dAdd_Date:30:H='Create Date',CEVNTOBJID:30:H='File Name'"

		=ariabrow("",'DATA_HEADER', 20,.F.,20,.F.,.F.,.F.,"CAUDTRALID,CAdd_user,dAdd_Date,CEVNTOBJID",'laTmpVal',.F.,.F.,.F.)

	ELSE
		laTmpVal[1]=ALLTRIM(loformset.ariaform1.kbChangeID.keytextbox.Value)
	ENDIF
ENDIF

IF !EMPTY(ALLTRIM(laTmpVal[1]))
	lcChangeID =ALLTRIM(laTmpVal[1])
	=lfCreateTemp(loformset ,lcChangeID )
ENDIF

*!*************************************************************
*! Name      : lfOpenTables
*! Date      : 07/06/2020
*! Purpose   : Open needed Tables
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfOpenTables

PUBLIC lcVarHeader,lcVarLines,lcIndexHeader,lcIndexLines
lcVarHeader=''
lcVarLines =''
lcIndexHeader=''
lcIndexLines=''


SET MULTILOCKS ON
IF !USED('SYDINDEX_T')
	=gfOpenTable('SYDINDEX','CFILE','SH','SYDINDEX_T')
ENDIF


SELECT SYDINDEX_T
IF GFSEEK(UPPER('datachangeheader'))
	lcVarHeader=ALLTRIM(SYDINDEX_T.cindx_exp)
	lcIndexHeader=ALLTRIM(SYDINDEX_T.cfile_tag)
ENDIF

*!*	IF GFSEEK(UPPER('datachangelines'))
*!*		lcVarLines=ALLTRIM(SYDINDEX_T.cindx_exp)
*!*		lcIndexLines=ALLTRIM(SYDINDEX_T.cfile_tag)
*!*	ENDIF


IF !USED('DATACHANGEHEADER')
    *MMT
	*=gfOpenTable('DATACHANGEHEADER',UPPER(lcIndexHeader),'SH','DATACHANGEHEADER')
	 =gfOpenTable('AUDTRAIL','AUDTRAIL','SH','DATACHANGEHEADER')
	*MMT
ENDIF


*!*	IF !USED('DATACHANGELINES')
*!*		=gfOpenTable('DATACHANGELINES','CHNGELINES','SH','DATACHANGELINES')
*!*	ENDIF
 
SELECT DATACHANGEHEADER
*B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][Start]
*=gfseek('')
*B612538,1 MMT 03/30/2022 Data Maintenance log screen repeats the changes IDs[T20211008.0003][End]
*!*	SELECT DATACHANGELINES
*!*	=gfseek('')

IF !USED('SYDFIELD')
	=gfOpenTable('SYDFIELD','cFld_Name','SH','SYDFIELD')
ENDIF
