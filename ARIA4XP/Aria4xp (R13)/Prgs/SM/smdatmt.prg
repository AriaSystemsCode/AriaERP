*:************************************************************************
*:  Program File: Aria4xp/Aria5ERP/PRGS/SM/SMDATMT.PRG
*:  Module      : System Manager
*:  System      : Aria5
*:  Developer   : ES - Esraa
*:  Date        : 05/18/2020
*:************************************************************************
*E612138,1 Es 05/18/2020  Aria 5 - Command prompt [P20190703.0001]
*E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001]
*B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003]
*:************************************************************************
#INCLUDE R:\Aria4xp\PRGs\SM\smdatmt.h
*E612138,1 Es 05/18/2020  Aria 5 - Command prompt [P20190703.0001][Start]
PUBLIC laScopExpr
DECLARE laScopExpr[1]
STORE "" TO laScopExpr
PUBLIC  lcVar, lcIndexName,lcbrowflds,lcbrowTable,lcTag,lcTableName,llSQLFile
DO FORM (oAriaApplication.ScreenHome+"\SM\SMDATMT.SCX")
RETURN

*!*************************************************************
*! Name      : lfOGWhen
*! Date      : 05/20/2020
*! Purpose   : When function for the Option Grid
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfOGWhen
laOGObjCnt[2]=.F.
= lfOGShowGet('laOGFxFlt[2,6]')  && Enable / Disable Object .

IF !USED('SYDFILES')
	=gfOpenTable('SYDFILES','CFILE_NAM','SH','SYDFILES')
ENDIF

IF !USED('SYDFIELD')
	=gfOpenTable('SYDFIELD','cFld_Name','SH','SYDFIELD')
ENDIF

IF !USED('sydflfld')
	=gfOpenTable('sydflfld','CFILE_NAM','SH','sydflfld')
ENDIF

IF !USED('Syrepuvr')
	=gfOpenTable('Syrepuvr','CREP_ID','SH','Syrepuvr')
ENDIF


*E612138,1 Es 05/18/2020  Aria 5 - Command prompt [P20190703.0001][End]




*!*************************************************************
*! Name      : lfvScope
*! Date      : 05/18/2020
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
FUNCTION lfvScope
PARAMETERS loformset

*E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][Start]
loformset.llCallScop = .F.  
*E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][End]
lcDataSessI = SET("Datasession" )
lcExpr = gfOpGrid('SMDATMTC' , .T.)
SET DATASESSION TO lcDataSessI
SET STEP ON 
IF lcExpr <> ".F." AND !EMPTY(lcTableName)

	=lfCrTmpTable(loformset)
	loformset.ChangeMode('V')
ELSE
    *E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][Start]
    loformset.llCallScop = .T.  
    loformset.ChangeMode('S')
	RETURN
	*E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][END]
ENDIF

*!*************************************************************
*! Name      : lfCrTmpTable
*! Date      : 05/18/2020
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
FUNCTION lfCrTmpTable
PARAMETERS loformset

SET STEP ON 
IF !USED(ALLTRIM(lcTableName))
	=gfOpenTable(ALLTRIM(lcTableName),ALLTRIM(lcIndexName),'SH',ALLTRIM(lcTableName))
ENDIF

loformset.lcFileName =ALLTRIM(lcTableName)
loformset.lcFileIndex =ALLTRIM(lcIndexName)

lnPos = ASCAN(laScopExpr,"lcFileData")
IF lnPos > 0
	lnPos = ASUBSCRIPT(laScopExpr,lnPos ,1)
	lcTableData =IIF(!EMPTY(laScopExpr[lnPos ,6]),laScopExpr[lnPos ,6],'')

	lcTableName=ALLTRIM(lcTableName)
	loformset.lcTempFile= gfTempName()
	SELECT(lcTableName)
	=AFIELDS(laFileStru)
	lnFileStru = ALEN(laFileStru,1)
	DIMENSION laFileStru[lnFileStru +2,18]
	laFileStru[lnFileStru +1,1] = 'llSel'
	laFileStru[lnFileStru +1,2] = 'L'
	laFileStru[lnFileStru +1,3] = 1
	laFileStru[lnFileStru +1,4] = 0
	laFileStru[lnFileStru +2,1] = 'RecStatus'
	laFileStru[lnFileStru +2,2] = 'C'
	laFileStru[lnFileStru +2,3] = 1
	laFileStru[lnFileStru +2,4] = 0
	
	FOR lnLoop = 1 TO 2
	 STORE ' ' TO  laFileStru[lnFileStru+lnLoop,7],laFileStru[lnFileStru+lnLoop,8],;
                laFileStru[lnFileStru+lnLoop,9],laFileStru[lnFileStru+lnLoop,10],;
                laFileStru[lnFileStru+lnLoop,11],laFileStru[lnFileStru+lnLoop,12],;
                laFileStru[lnFileStru+lnLoop,13],laFileStru[lnFileStru+lnLoop,14],;
                laFileStru[lnFileStru+lnLoop,15],laFileStru[lnFileStru+lnLoop,16]
      STORE 0 TO    laFileStru[lnFileStru+lnLoop,17] ,laFileStru[lnFileStru+lnLoop,18]
    ENDFOR   
	=gfCrtTmp(loformset.lcTempFile,@laFileStru,lcTag)

    *E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][Start]
    IF !EMPTY(lcTableData)
    *E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][End]
	SELECT (lcTableData)
	SCAN

		lckeyexpr=&lcTableData..keyexp

		SELECT(lcTableName)
		IF GFSEEK(lckeyexpr)
			SCATTER MEMVAR MEMO
			INSERT INTO (loformset.lcTempFile) FROM MEMVAR
		ENDIF
	ENDSCAN
	*E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][Start]
	ELSE
 	  *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
	  SELECT(lcTableName)
    IF GFSEEK('',lcTableName)
      lnCntRec = 0
      SCAN 
        lnCntRec = lnCntRec +  1
		SCATTER MEMVAR MEMO
		INSERT INTO (loformset.lcTempFile) FROM MEMVAR
	  ENDSCAN		
	ENDIF	  
  *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
	ENDIF
	SELECT(lcTableName)
	loformset.lcKeyFields = KEY()
    *E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][End]

	
ENDIF

*!*************************************************************
*! Name      : lfvFileName
*! Date      : 05/18/2020
*!*************************************************************
*! Passed Parameters : cfile_nam as 'File ID' and cfile_ttl as 'File Name' )
*!*************************************************************
*! Return      : None
FUNCTION lfvFileName

IF !USED('SYDFILES_T')
	=gfOpenTable('SYDFILES','CFILE_NAM','SH','SYDFILES_T')
ENDIF

SET STEP on

SELECT SYDFILES_T
IF EMPTY(ALLTRIM(laOGFxFlt[1,6])) OR !GFSEEK(UPPER(ALLTRIM(laOGFxFlt[1,6])))
	laOGObjCnt[2]=.F.
	= lfOGShowGet('laOGFxFlt[2,6]')  && Enable / Disable Object .

	DIMENSION latemp[2]
	latemp   = ''
	SELECT  SYDFILES_T
	LOCATE
	=ARIABROW('',LANG_ARIA_TABLES,20,.F.,20,.F.,.F.,.F.,'cfile_nam,cfile_ttl' ,'laTemp',.F.,.F.,.F.)



	IF !EMPTY(ALLTRIM(laTemp[1]))
		lcfile=UPPER(ALLTRIM(laTemp[1]))
		laOGFxFlt[1,6]=ALLTRIM(laTemp[1])
		laOGObjCnt[2]=.T.
		= lfOGShowGet('laOGFxFlt[2,6]')  && Enable / Disable Object .
	ENDIF

ELSE
	lcfile=UPPER(ALLTRIM(laOGFxFlt[1,6]))
	laOGObjCnt[2]=.T.
	= lfOGShowGet('laOGFxFlt[2,6]')  && Enable / Disable Object .
ENDIF


IF !USED('SYDFLFLD_T')
	=gfOpenTable('SYDFLFLD','CFILE_NAM','SH','SYDFLFLD_T')
ENDIF



IF !USED('SYDFIELD_T')
	=gfOpenTable('SYDFIELD','cFld_Name','SH','SYDFIELD_T')
ENDIF



lcVar=''
lcIndexName=''
lcbrowflds=''
lcbrowTable=''


IF !USED('SYDINDEX_T')
	=gfOpenTable('SYDINDEX','CFILE','SH','SYDINDEX_T')
ENDIF
IF ALLTRIM(UPPER(lcfile)) == 'STYLE'
  SELECT SYDINDEX_T
  =GFSEEK(lcfile)
  LOCATE REST WHILE cfile_nam=lcfile FOR lunique=.T.
  IF FOUND()
	lcVar=SYDINDEX_T.cindx_exp
	lcIndexName=SYDINDEX_T.cfile_tag
  ENDIF
ENDIF

IF EMPTY(lcVar)
    SELECT SYDINDEX_T
	=GFSEEK(lcfile)
	LOCATE REST WHILE cfile_nam=lcfile FOR cfile_tag=lcfile
	IF FOUND()
		lcVar=SYDINDEX_T.cindx_exp
		lcIndexName=SYDINDEX_T.cfile_tag
	ENDIF
ENDIF

IF EMPTY(lcVar)
    SELECT SYDINDEX_T
	IF GFSEEK(lcfile)
	  LOCATE REST WHILE cfile_nam=lcfile FOR !'(' $ SYDINDEX_T.cindx_exp
     IF FOUND()
		lcVar=SYDINDEX_T.cindx_exp
		lcIndexName=SYDINDEX_T.cfile_tag
	ELSE
	   IF GFSEEK(lcfile)
	   		lcVar=SYDINDEX_T.cindx_exp
		lcIndexName=SYDINDEX_T.cfile_tag
  
	   ENDIF
	   	ENDIF
	ENDIF
ENDIF



*xxx

*!*	select SYDFLFLD_T
*!*	SELECT distinct cfld_name FROM SYDFLFLD_T WHERE  CFILE_NAM==lcfile INTO ARRAY lafieldsName

lcbrwFields=""
DIMENSION lafieldsName[1]
lafieldsName[1]=""

SELECT SYDFILES_T
IF  GFSEEK(lcfile)
	IF !EMPTY(mbrow_fld)
		ALINES(lafieldsName,mbrow_fld, "|")
	ELSE
		ALINES(lafieldsName,lcVar, "+")
	ENDIF
	llSQLFile = (SYDFILES_T.CVER = 'A40')
ENDIF

FOR i=1 TO ALEN(lafieldsName)
	lnstart= AT("(", lafieldsName[i])
	IF lnstart!=0
		lnend= AT(")",lafieldsName[i])
		lcfield=SUBSTR(lafieldsName[i],(lnstart+1),(lnend-lnstart))
		IF AT(",",lcfield)
			ALINES(lafild,lcfield, ",")
			lcfield=lafild[1]
		ENDIF
		lafieldsName[i]=lcfield
	ENDIF

	SELECT SYDFIELD_T
	IF GFSEEK(lafieldsName[i])
		lcfld_head=SYDFIELD_T.cfld_head
		*B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
		IF UPPER(ALLTRIM(lafieldsName[i]))='CVERSION'
		  lcbrwFields=lcbrwFields+"lCVERSION=ALLTRIM("+lafieldsName[i]+") :R :H='"+lcfld_head+"',"
		ELSE
		*B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
  		lcbrwFields=lcbrwFields+lafieldsName[i]+" :R :H='"+lcfld_head+"',"
		*B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
		ENDIF
		*B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
	ENDIF

ENDFOR

lcbrwFields=SUBSTR(lcbrwFields,1,LEN(lcbrwFields)-1)


lcbrowflds=lcbrwFields
lcbrowTable=lcfile
lcTableName=lcbrowTable
lcTag=lcVar

IF !USED(ALLTRIM(lcfile))
	=gfOpenTable(ALLTRIM(lcfile),ALLTRIM(lcIndexName),'SH',ALLTRIM(lcfile))
ENDIF
*E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][Start]
*!*	lnPos = ASCAN(loOGScroll.laOGFxFlt,"lcFileData")
*!*	IF lnPos > 0
*!*	  lnPos = ASUBSCRIPT(loOGScroll.laOGFxFlt,lnPos ,1)
*!*	  lcTableData =IIF(!EMPTY(loOGScroll.laOGFxFlt[lnPos ,6]),loOGScroll.laOGFxFlt[lnPos ,6],'')
*!*	  IF !EMPTY(lcTableData) AND USED(lcTableData)
*!*	    SELECT (lcTableData)
*!*	    ZAP
*!*	  ENDIF
*!*	ENDIF	
*E612173,1 MMT 07/01/2020  Aria 5 - Command prompt - Iteration#2 [P20190703.0001][End]

=lfCloseFiles()



*!*************************************************************
*! Name      : lfOpenFiles
*! Date      : 05/31/2020
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
FUNCTION lfOpenFiles

IF !USED('SYDFILES')
	=gfOpenTable('SYDFILES','CFILE_NAM','SH','SYDFILES')
ENDIF

IF !USED('SYDFIELD')
	=gfOpenTable('SYDFIELD','cFld_Name','SH','SYDFIELD')
ENDIF

IF !USED('sydflfld')
	=gfOpenTable('sydflfld','CFILE_NAM','SH','sydflfld')
ENDIF

IF !USED('Syrepuvr')
	=gfOpenTable('Syrepuvr','CREP_ID','SH','Syrepuvr')
ENDIF

IF !USED('sydindex')
	=gfOpenTable('sydindex','CFILE','SH','sydindex')
ENDIF

*!*************************************************************
*! Name      : lfCloseFiles
*! Date      : 05/31/2020
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
FUNCTION lfCloseFiles

IF USED('SYDFILES')
	SELECT SYDFILES
	USE
ENDIF

IF USED('SYDFIELD')
	SELECT SYDFIELD
	USE
ENDIF


IF USED('sydflfld')
	SELECT sydflfld
	USE
ENDIF

IF USED('Syrepuvr')
	SELECT Syrepuvr
	USE
ENDIF

IF USED('sydindex')
	SELECT sydindex
	USE
ENDIF



*!*************************************************************
*! Name      : lfCloseTempFiles
*! Date      : 06/03/2020
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
FUNCTION lfCloseTempFiles
IF USED('SYDFILES_T')
	SELECT SYDFILES_T
	USE
ENDIF

IF USED('SYDFLFLD_T')
	SELECT SYDFLFLD_T
	USE
ENDIF


IF USED('SYDFLFLD_T')
	SELECT SYDFLFLD_T
	USE
ENDIF


IF USED('SYDINDEX_T')
	SELECT SYDINDEX_T
	USE
ENDIF
*!*************************************************************
*! Name      : lfAddNewRecord
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Add new record
*!*************************************************************
FUNCTION lfAddNewRecord
PARAMETERS  loFormset
SELECT(loformset.lcTempFile)
APPEND BLANK 
REPLACE RecStatus WITH 'A'
loformset.ariaform1.grdData.grdMultiSelectionGrid.Refresh()
*!*************************************************************
*! Name      : lfAddCntSrc
*! Date      : 05/18/2020
*!*************************************************************
*! Passed Parameters : lcTable:Table Name
*!*************************************************************
*! Return      : None
FUNCTION lfAddCntSrc
PARAMETERS  loformset

=lfOpenFiles()

SELECT SYDFILES
=GFSEEK(lcTableName)
lcTable=cfile_ttl

SELECT sydflfld
SELECT distinct cfld_name FROM sydflfld WHERE  CFILE_NAM==UPPER(ALLTRIM(lcTableName)) INTO ARRAY lafields


WITH loformset.ariaform1.grdData.grdMultiSelectionGrid
	.RecordSource = ""
	.RecordSource = loformset.lcTempFile
	.ColumnCount = 1
	.ColumnCount =ALEN(lafields)+1
	.Column1.CurrentControl = 'Ariacheckbox1'
	.Column1.Sparse = .F.
	.column1.ControlSource =loformset.lcTempFile+"."+"llSel"
	.column1.Header1.Caption  =''
	
	FOR i=1 TO ALEN(lafields)
		SELECT (loformset.lcTempFile)
		LOCATE
		lcindex=ALLTRIM(STR(i+1))
		SELECT SYDFIELD
		IF GFSEEK(lafields[i])
			.Column&lcindex..Header1.Caption = SYDFIELD.cfld_head
			*B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
			*IF SYDFIELD.lvldentry 
			IF SYDFIELD.lvldentry AND SYDFIELD.cdata_typ ='C' AND sydfield.nfld_wdth = 6 AND ALLTRIM(lafields[i]) <> 'CSITEID'
			*B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
*!*				*XX
*!*				 .column&lcindex..ControlSource =loformset.lcTempFile+"."+ALLTRIM(lafields[i])+""
*!*				 *XX
			  lcOnErr = ON("Error")
			  ON ERROR er = 0 
   			*B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
    		.column&lcindex..ControlSource =loformset.lcTempFile+"."+ALLTRIM(lafields[i])+""
	    	*B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]

		    .column&lcindex..width =200
			  .column&lcindex..AddObject("AriaCod"+lcindex,"AriaCodes")
			   
			  .column&lcindex..AriaCod&lcindex..codesfield= ALLTRIM(lafields[i])
			  .column&lcindex..AriaCod&lcindex..Visible = .T.
			   *BINDEVENT(.column&lcindex,'Click',loFormSet,'CodeClick',1)
			   BINDEVENT(.column&lcindex..AriaCod&lcindex,'Valid',loFormSet,'AssignCodeValue',1)
				   BINDEVENT(.column&lcindex..AriaCod&lcindex,'VALUE_ASSIGN',loFormSet,'BefAssignValue',0)
*!*	      		   BINDEVENT(.column&lcindex..AriaCod&lcindex,'VALUE_ASSIGN',loFormSet,'AftAssignValue',1)
			  .column&lcindex..AriaCod&lcindex..INIT()
			  .column&lcindex..AriaCod&lcindex..ColumnWidths = '200,0'
			  .column&lcindex..CurrentControl = "AriaCod"+lcindex
			  .column&lcindex..Sparse = .F.
			  .column&lcindex..AriaCod&lcindex..Enabled = IIF(loformset.ActiveMode= 'E'  ,.T.,.F.)
			  ON ERROR &lcOnErr.
			 ELSE
			   DO case 
			     CASE SYDFIELD.cdata_typ ='N'
			       .column&lcindex..ControlSource =loformset.lcTempFile+"."+ALLTRIM(lafields[i])+""
			       .column&lcindex..Format = REPLICATE('9',sydfield.nfld_wdth)+IIF(sydfield.nfld_dec > 0,"."+REPLICATE('9',sydfield.nfld_dec),'')
			  	   .column&lcindex..InputMask = REPLICATE('9',sydfield.nfld_wdth)+IIF(sydfield.nfld_dec > 0,"."+REPLICATE('9',sydfield.nfld_dec),'')
                 CASE SYDFIELD.cdata_typ ='C'
                   .column&lcindex..ControlSource =loformset.lcTempFile+"."+ALLTRIM(lafields[i])+""
			       .column&lcindex..Format = REPLICATE('X',sydfield.nfld_wdth)
			  	   .column&lcindex..InputMask = REPLICATE('X',sydfield.nfld_wdth)
			  	 CASE SYDFIELD.cdata_typ ='D'
 			  	   .column&lcindex..ControlSource =loformset.lcTempFile+"."+ALLTRIM(lafields[i])+""
 			  	   *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
 			  	   TRY
  			  	 *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
  			  	   .column&lcindex..AddObject("DtPicker"+lcindex,"DtPicker")
			  	   *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
			  	   CATCH
			  	   ENDTRY
 			  	   *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
			  	   .column&lcindex..DtPicker&lcindex..Visible = .T.
*			  	   .column&lcindex..DtPicker&lcindex..Visible = .T.
			  	   .column&lcindex..CurrentControl = "DtPicker"+lcindex
  				   .column&lcindex..Sparse = .F.
	  			   .column&lcindex..DtPicker&lcindex..Enabled = IIF(loformset.ActiveMode= 'E'  ,.T.,.F.)
		  		   .column&lcindex..Width =125
			  	   .column&lcindex..DtPicker&lcindex..ControlSource = loformset.lcTempFile+"."+ALLTRIM(lafields[i])+""
				   
	  	     CASE SYDFIELD.cdata_typ ='L'

             .column&lcindex..ControlSource =loformset.lcTempFile+"."+ALLTRIM(lafields[i])+""
             *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
             TRY
             *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
			  	     .column&lcindex..AddObject("Ariacheckbox"+lcindex,"Ariacheckbox")
			  	   *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
 		  	     CATCH
			  	   ENDTRY 
			  	   *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
			  	   .column&lcindex..Ariacheckbox&lcindex..Visible = .T.
			  	   .column&lcindex..Ariacheckbox&lcindex..Caption = ''	
   		  	 	 .column&lcindex..CurrentControl = "Ariacheckbox"+lcindex
  				   .column&lcindex..Sparse = .F.
	  			   .column&lcindex..Ariacheckbox&lcindex..Enabled =IIF(loformset.ActiveMode= 'E'  ,.T.,.F.)
		  		   .column&lcindex..Width =25
				     *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
  				   TRY 
	  			   *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
		    		   .column&lcindex..Ariacheckbox&lcindex..ControlSource =loformset.lcTempFile+"."+ALLTRIM(lafields[i])+""
				     *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
				     CATCH
				     ENDTRY
				     *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
				   
			   ENDCASE 

			ENDIF
		ENDIF
	ENDFOR
	.SETALL('ReadOnly',.T.,'COLUMN')
	
	*.Enabled = IIF(loformset.ActiveMode= 'E'  ,.T.,.F.)
	.SETALL('Enabled', IIF(loformset.ActiveMode= 'E'  ,.T.,.F.),'COLUMN')
    .SetAll("DynamicBackColor", "IIF("+loformset.lcTempFile+".RecStatus ='D',RGB(255,0,0),IIF("+;
			                                     loformset.lcTempFile+".RecStatus='A',RGB(0,255,0),RGB(255,255,255))","Column")  
	.refresh()
	.AfterRowColChange()
ENDWITH

loformset.ariaform1.txtFileName.Value=lcTableName
loformset.ariaform1.txtDescription.Value=lcTable

*!*************************************************************
*! Name      : lfApplySel
*! Date      : 05/18/2020
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
FUNCTION lfApplySel

=ACOPY(loOGScroll.laOGFxFlt , laScopExpr)

=lfCloseTempFiles()
*!*************************************************************
*! Name      : RefreshFile
*! Date      : 06/25/2020
*! Purpose   : Refresh File option
*!*************************************************************
FUNCTION RefreshFile
RETURN lcbrowTable
*!*************************************************************
*! Name      : lfvDeleteRec
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Delete record
*!*************************************************************
FUNCTION lfvDeleteRec
LPARAMETERS loformset
lnSelect = SELECT(0)
SELECT(loformset.lcTempFile)
lnCurrRec = RECNO()
LOCATE 
SCAN FOR llSel
  REPLACE  RecStatus WITH 'D'
ENDSCAN 
loformset.ariaform1.grdData.grdMultiSelectionGrid.Refresh()
IF BETWEEN(lnCurrRec ,1,RECCOUNT())
  GO RECORD  lnCurrRec  
ENDIF
SELECT(lnSelect)
*!*************************************************************
*! Name      : lfvInvert
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Valid function of push button Invert
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvInvert
LPARAMETERS loformset
SELECT(loformset.lcTempFile)
lnRecNOCurr = RECNO()

REPLACE ALL LLSEL WITH !LLSEL

GO lnRecNOCurr

lfvpbSel(loformset)

loformset.lnUnSelRec = loformset.lnSelRec
loformset.lnSelRec   = RECCOUNT() - loformset.lnDelRec - loformset.lnSelRec

*there is no selected records
IF loformset.lnSelRec = 0
  loformset.llenableselectall = .T.
  loformset.llenableselectnone = .F.
  loformset.llenablerel = .F.
ELSE
  loformset.llenableselectnone = .T.
  loformset.llenablerel = .T.

  *--All the records were selected
  IF loformset.lnUnSelRec = 0
    loformset.llenableselectall = .F.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvpbSel
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/20205
*! Purpose   : Function to arange the push button select prompt
*!*************************************************************
*! Called from : lfvSelect() , lfvInvert() , The Browse [lcPickBrow]
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : .T.
*!*************************************************************
FUNCTION lfvpbSel
LPARAMETERS loformset
SELECT(loformset.lcTempFile)
IF LLSEL
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcCaptionSel = LANG_unSelect
loformset.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_unSelect,this.loFormSet.GetHeaderText("LANG_unSelect ",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ELSE
  *N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcCaptionSel = LANG_Select
loformset.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_SELECT ,this.loFormSet.GetHeaderText("LANG_SELECT",this.loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

ENDIF

RETURN .T.
*!*************************************************************
*! Name      : lfvSelect
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Valid function of push button Select
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : lfvpbSel()
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelect
LPARAMETERS loformset
SELECT(loformset.lcTempFile)
REPLACE LLSEL WITH !LLSEL
lfvpbSel(loformset)

loformset.lnSelRec   = IIF(llSel , loformset.lnSelRec + 1 , loformset.lnSelRec - 1)
loformset.lnUnSelRec = IIF(llSel , loformset.lnUnSelRec - 1 , loformset.lnUnSelRec + 1)

*No records was selected
IF loformset.lnSelRec = 0
  loformset.llenableinvert = .T.
  loformset.llenableselect = .T.
  loformset.llenableselectall = .T.
  loformset.llenableselectnone = .F.
  loformset.llenablerel = .F.
ELSE    && Else
  loformset.llenableselectnone = .T.
  loformset.llenablerel = .T.

  *-- All the records were selected
  IF loformset.lnUnSelRec = 0
    loFormSet.llenableselectall = .F.
  ELSE
    loFormSet.llenableselectall = .T.
  ENDIF
ENDIF
*!*************************************************************
*! Name      : lfvSelAll
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Valid function of push button Select all
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelAll
LPARAMETERS loformset
SELECT(loformset.lcTempFile)
lnRecCurrn = RECNO()

REPLACE ALL LLSEL WITH .T.
loformset.lnSelRec   = RECCOUNT() - loformset.lnDelRec
loformset.lnUnSelRec = 0
GO lnRecCurrn

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcCaptionSel = LANG_unSelect
loformset.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_unSelect,loFormSet.GetHeaderText("LANG_unSelect",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]


loFormSet.llenableselectall = .F.
loFormSet.llenableselectnone = .T.
loFormSet.llenablerel = .T.

*!*************************************************************
*! Name      : lfvSelNon
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Valid function of push button Select none
*!*************************************************************
*! Called from : Scrren ALRELPIK
*!*************************************************************
*! Calls       : None
*!*************************************************************
*! Passed Parameters : None
*!*************************************************************
*! Return      : None
*!*************************************************************
*
FUNCTION lfvSelNon
LPARAMETERS loformset
SELECT(loformset.lcTempFile)

lnRecCurr = RECNO()

REPLACE ALL LLSEL WITH .F.

loformset.lnSelRec   = 0

loformset.lnUnSelRec = RECCOUNT() - loformset.lnDelRec

GO lnRecCurr

*N000682,1 11/20/2012 MMT Globlization changes[Start]
*This.lcCaptionSel = LANG_Select
loformset.lcCaptionSel = IIF(oAriaApplication.oActivelang.cLang_ID = "EN",LANG_Select,loFormSet.GetHeaderText("LANG_Select",loFormSet.HeaderAlias))
*N000682,1 11/20/2012 MMT Globlization changes[End]

loFormSet.llEnableSelectAll  = .T.
loFormSet.llEnableSelectNone = .F.
loFormSet.llEnableRel = .F.
*!*************************************************************
*! Name      : lfChangeMode
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Change Mode
*!*************************************************************
FUNCTION lfChangeMode
LPARAMETERS lcModeToChange,loFormSet
DO CASE 
CASE lcModeToChange = 'S'
  loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.RecordSource =''
  IF !loFormSet.llCallScop
   IF !loFormSet.llFrstTime
 	 lfvScope(loFormSet) 
     loFormSet.llCallScop = .T.
   ENDIF
  ENDIF
  loFormSet.llFrstTime = .F.
  loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.ReadOnly = .T.
  loFormSet.AriaForm1.cmdAdd.Enabled = .F.
  loFormSet.AriaForm1.cmdRemove.Enabled = .F.  
CASE lcModeToChange = 'V'  
  =lfAddCntSrc (loformset)
  loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.ReadOnly = .T.
  loFormSet.AriaForm1.cmdAdd.Enabled = .F.
  loFormSet.AriaForm1.cmdRemove.Enabled = .F. 
  FOR lnA = 1 TO loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.ColumnCount
    lcField = loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.Columns(lnA).ControlSource
    lnDot =  ATC('.',lcField )
    lcFieldName =ALLTRIM(SUBSTR(lcField ,lnDot+1))
    IF UPPER(lcFieldName ) $ UPPER(loformset.lcKeyFields)
      loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.Columns(lnA).ReadOnly = .T.
      loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.Columns(lnA).Header1.FontBold = .T.
    ENDIF
  ENDFOR 
  
CASE lcModeToChange = 'E'  
  lfAddCntSrc (loFormSet)   
  loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.ReadOnly = .F.
  loFormSet.AriaForm1.cmdAdd.Enabled = .T.
  loFormSet.AriaForm1.cmdRemove.Enabled = .T.

  FOR lnA = 1 TO loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.ColumnCount
    lcField = loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.Columns(lnA).ControlSource
    lnDot =  ATC('.',lcField )
    lcFieldName =ALLTRIM(SUBSTR(lcField ,lnDot+1))
    IF UPPER(lcFieldName ) $ UPPER(loformset.lcKeyFields)
      loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.Columns(lnA).ReadOnly = .T.
      loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.Columns(lnA).Header1.FontBold = .T.    
    ENDIF
  ENDFOR     
ENDCASE 
loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.Column1.CurrentControl = 'Ariacheckbox1'
loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.Column1.Sparse = .F. 
loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.SetAll("DynamicBackColor", "IIF("+loformset.lcTempFile+".RecStatus ='D',RGB(255,0,0),IIF("+;
			                                     loformset.lcTempFile+".RecStatus='A',RGB(0,255,0),RGB(255,255,255)))","Column")  
loFormSet.AriaForm1.grdData.grdMultiSelectionGrid.cOLUMN1.SetFocus ()			                                     
*!*************************************************************
*! Name      : lfAssignValue
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Assign Value of AriaCodes
*!*************************************************************
FUNCTION lfAssignValue
LPARAMETERS loFormSet
loFormSet.AriaForm1.txtFileName.SetFocus()

*!*************************************************************
*! Name      : lfInitForm
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Form Initialization
*!*************************************************************
FUNCTION lfInitForm
LPARAMETERS loFormSet
IF !USED('DATACHANGEHEADER')
  =gfOpenTable('AUDTRAIL','CHNGHEADER','SH','DATACHANGEHEADER')
ENDIF
*!*	IF !USED('DATACHANGELINES')
*!*	  =gfOpenTable('DATACHANGELINES','CHNGELINES')
*!*	ENDIF
*!*************************************************************
*! Name      : lfInitFormSave
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Child Form Initialization
*!*************************************************************
FUNCTION lfInitFormSave
LPARAMETERS lChildFormSet
IF !USED('DATACHANGEHEADER')
   =gfOpenTable('AUDTRAIL','CHNGHEADER','SH','DATACHANGEHEADER')
ENDIF
*!*************************************************************
*! Name      : lfvParentChangeID
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Parent Change ID Validation
*!*************************************************************
FUNCTION lfvParentChangeID
LPARAMETERS loChilFormSet
SET STEP ON 
IF (!EMPTY(loChilFormSet.AriaForm1.KbParntChangeID.keytextbox.Value) AND !gfSeek(loChilFormSet.AriaForm1.KbParntChangeID.keytextbox.Value,'DATACHANGEHEADER','CHNGHEADER')) OR;
    loChilFormSet.AriaForm1.KbParntChangeID.selectedfrombrowse 
  SELECT DATACHANGEHEADER
  DIMENSION laSelecPO[1]
  *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
  *gfSeek("'"+LANG_DATA_MAIN+"'")
  *gfSeek(LANG_DATA_MAIN)
  =gfSQLRun("SELECT DISTINCT CAUDTRALID,CAdd_user,dAdd_Date,CEVNTOBJID,CSTATUS From Audtrail where [Key1]= '"+LANG_DATA_MAIN+"' ORDER BY CAUDTRALID","DATACHANGEHEADER",.T.,'DATACHANGEHEADER_A')
  SELECT 'DATACHANGEHEADER_A'
  *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
  lcBrFields  = "caudtralid:R :H='"+LANG_CHANGEID+"', CEVNTOBJID:R :H = '"+LANG_FILE_NAME+"',lcStat = IIF(CSTATUS ='P','"+;
                LANG_PENDING+"',''):R :H= '"+LANG_STATUS+"',CAdd_User:R :H= '"+LANG_CREATED_BY+"',dAdd_Date:R :H= '"+LANG_CREATED_ON+"'"  			   	


 
  =AriaBrow("",LANG_CHANGES , gnbrfsrow1, gnbrfscol1,;
            gnbrfsrow2, gnbrfscol2, '','',;
            "caudtralid",'laSelecPO')
  loChilFormSet.AriaForm1.KbParntChangeID.selectedfrombrowse  = .F.
  IF !EMPTY(laSelecPO[1])
    loChilFormSet.AriaForm1.KbParntChangeID.keytextbox.Value = laSelecPO[1]
  ELSE
    loChilFormSet.AriaForm1.KbParntChangeID.keytextbox.Value = ''
  ENDIF 
ENDIF 
*!*************************************************************
*! Name      : lfCheckDataChanged
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Check if data changed or not
*!*************************************************************
FUNCTION lfCheckDataChanged
LPARAMETERS loFormSet

lcbrowTable = loformset.lcFileName
llRecordModi = .F.
lnAlias = SELECT(0)
SELECT (loformset.lcTempFile)
lnRecNo = RECNO()
LOCATE FOR !EMPTY(RecStatus)
IF FOUND()
  llRecordModi = .T.
ELSE
  LOCATE 
  SCAN 
    lcKeyValue =  EVALUATE(loformset.lcKeyFields)
    =gfSeek(lcKeyValue,loformset.lcFileName,loformset.lcFileIndex)
    SELECT (lcbrowTable)
    lnFld = AFIELDS(laFileStr)
    FOR lnX=1 TO lnFld 
      IF EVALUATE(lcbrowTable+"."+laFileStr[lnX,1]) <> EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1])
         llRecordModi = .T.
         EXIT 
      ENDIF
    ENDFOR
    IF llRecordModi
      EXIT
    ENDIF 
  ENDSCAN 
ENDIF
IF BETWEEN(lnRecNo ,1,RECCOUNT())
  GO RECORD lnRecNo 
ENDIF
SELECT(lnAlias)
IF !llRecordModi 
  =gfModalGen('TRM54069B00000','Dialog')
ENDIF
RETURN llRecordModi 
*!*************************************************************
*! Name      : lfSaveChange
*! Developer : Mariam Mazhar[MMT]
*! Date      : 07/01/2020
*! Purpose   : Saving function
*!*************************************************************
FUNCTION lfSaveChange
PARAMETERS loFormSet

lcbrowTable = loformset.lcFileName
SELECT (loformset.lcTempFile)
LOCATE 
SCAN FOR EMPTY(RecStatus)
    lcKeyValue =  EVALUATE(loformset.lcKeyFields)
    =gfSeek(lcKeyValue,loformset.lcFileName,loformset.lcFileIndex)
    SELECT (lcbrowTable)
    lnFld = AFIELDS(laFileStr)
    FOR lnX=1 TO lnFld 
      IF EVALUATE(lcbrowTable+"."+laFileStr[lnX,1]) <> EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1])
         REPLACE RecStatus WITH 'M' IN (loformset.lcTempFile)
         EXIT 
      ENDIF
    ENDFOR
    SELECT (loformset.lcTempFile)
    IF !EMPTY(RecStatus)
      LOOP 
    ENDIF 
ENDSCAN 
SET STEP ON 

lcAudTrlID = gfSequence('cAudTralID')
*m.ParentChangeID  = loformset.lcParentChangeID 
*!*	m.TicketNo  = loformset.lcTicketNo
*!*	m.ChangeReason = loformset.lcChangeReason
*!*	m.ChangeID = gfSequence('CHANGEID')
*!*	m.CAPOBJNAM =lfGetTableEntity(lcbrowTable)

*!*	m.CAUDTRALID =lcAudTrlID
*!*	*lcChangeID = m.ChangeID 
*!*	m.CEVNTOBJID = lcbrowTable 
*!*	*!*	m.cStatus ='A'&&IIF(oAriaApplication.User_Level ='O','P','A')
*!*	*!*	m.ApprovedBy = oAriaApplication.User_ID && IIF(oAriaApplication.User_Level ='O','',oAriaApplication.User_ID)
*!*	*!*	m.dapprovaldate= oAriaApplication.SystemDate&& IIF(oAriaApplication.User_Level ='O',{},oAriaApplication.SystemDate)
*!*	*!*	m.capprovaltime  = TIME()&&&IIF(oAriaApplication.User_Level ='O','',TIME())
*!*	m.cAdd_User  = oAriaApplication.User_ID 
*!*	m.dAdd_Date  = DATE()   
*!*	m.cAdd_Time  = gfGetTime()
*!*	*!*	INSERT INTO DataChangeHeader FROM MEMVAR
*!*	*!*	SELECT DataChangeHeader 
*!*	*!*	*!*	=gfAdd_Info('DataChangeHeader')
*!*	*!*	=gfReplace('')   
*!*	*!*	=gfTableUpdate()
*!*	m.Key1 = 'Data Maintenance'
*!*	SELECT DataChangeHeader 
*!*	=GFSEEK(lcChangeID ,'DATACHANGEHEADER','CHNGHEADER')
*!*	m.HeaderOid = DATACHANGEHEADER.REC_NO
m.CAPOBJNAM =lfGetTableEntity(lcbrowTable)
SELECT (loformset.lcTempFile)
LOCATE 
SCAN FOR !EMPTY(RecStatus)
  SCATTER MEMO MEMVAR
  m.Key = EVALUATE(loformset.lcKeyFields)
  llRecFound =gfSeek(m.Key,loformset.lcFileName,loformset.lcFileIndex)
  SELECT(lcbrowTable)
  lnFld = AFIELDS(laFileStr)
  DO CASE 
    CASE EVALUATE(loformset.lcTempFile+".RecStatus") ='A'
*!*	      FOR lnX=1 TO lnFld 
*!*	        DO CASE 
*!*	          CASE laFileStr[lnX,2] ='C' OR laFileStr[lnX,2] ='M'
*!*	            m.NewValue = EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1])
*!*	      
*!*	          CASE laFileStr[lnX,2] ='L'
*!*	            m.NewValue = IIF(EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1])=.F., 'False', 'True')
*!*	            
*!*	          CASE laFileStr[lnX,2] ='N'  
*!*	            m.NewValue = STR(EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1]),laFileStr[lnX,3],laFileStr[lnX,4])
*!*	                    
*!*	    	  CASE laFileStr[lnX,2] ='D'
*!*	              m.NewValue = DTOC(EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1]))
*!*	        ENDCASE  
*!*	        m.Cfield_name = laFileStr[lnX,1]
*!*	        m.OldValue = ''
*!*	        INSERT INTO DataChangeLines FROM MEMVAR
*!*	        SELECT DataChangeLines 
*!*	        =gfReplace('')
*!*	      ENDFOR
      * IF oAriaApplication.User_Level ='A'
      m.cevent_id = 'INSERT'
      SELECT(lcbrowTable)
      APPEND BLANK
      GATHER MEMO MEMVAR 
      =gfReplace('')
      lcInform = lfGetAudInfo(lcbrowTable)
      * ENDIF   
    CASE EVALUATE(loformset.lcTempFile+".RecStatus") = 'D' AND llRecFound 
*!*	       FOR lnX=1 TO lnFld 
*!*	        m.Cfield_name = laFileStr[lnX,1]
*!*	       * m.OldValue = EVALUATE(lcbrowTable+"."+laFileStr[lnX,1])
*!*	          DO CASE 
*!*	            CASE laFileStr[lnX,2] ='C' OR laFileStr[lnX,2] ='M'
*!*	              m.OldValue = EVALUATE(lcbrowTable+"."+laFileStr[lnX,1])
*!*	      
*!*	            CASE laFileStr[lnX,2] ='L'
*!*	               m.OldValue = IIF(EVALUATE(lcbrowTable+"."+laFileStr[lnX,1])=.F., 'False', 'True')
*!*	            
*!*	            CASE laFileStr[lnX,2] ='N'  
*!*	              m.OldValue = STR(EVALUATE(lcbrowTable+"."+laFileStr[lnX,1]),laFileStr[lnX,3],laFileStr[lnX,4])
*!*	                    
*!*				CASE laFileStr[lnX,2] ='D'
*!*	              m.OldValue = DTOC(EVALUATE(lcbrowTable+"."+laFileStr[lnX,1]))
*!*	          ENDCASE  
*!*	          m.NewValue =  ''
*!*	          INSERT INTO DataChangeLines FROM MEMVAR
*!*	          SELECT DataChangeLines 
*!*	          =gfReplace('')
*!*	       ENDFOR
   
       
     *  IF oAriaApplication.User_Level ='A'
         m.cevent_id = 'DELETE'
         SELECT(lcbrowTable)
         lcInform = lfGetAudInfo(lcbrowTable)
         *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
         SELECT(lcbrowTable)
         *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
    		 =gfDelete()
    *   ENDIF             
    CASE EVALUATE(loformset.lcTempFile+".RecStatus") = 'M' AND llRecFound 
      *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
      lcOldInform = lfGetAudInfo(lcbrowTable)
      *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
      FOR lnX=1 TO lnFld 
        *IF EVALUATE(lcbrowTable+"."+laFileStr[lnX,1]) <> EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1])
*!*	          m.Cfield_name = laFileStr[lnX,1]
*!*	          DO CASE 
*!*	            CASE laFileStr[lnX,2] ='C' OR laFileStr[lnX,2] ='M'
*!*	              m.OldValue = EVALUATE(lcbrowTable+"."+laFileStr[lnX,1])
*!*	              m.NewValue =  EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1])
*!*	            CASE laFileStr[lnX,2] ='L'
*!*	               m.OldValue = IIF(EVALUATE(lcbrowTable+"."+laFileStr[lnX,1])=.F., 'False', 'True')
*!*	               m.NewValue = IIF(EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1])=.F., 'False', 'True')
*!*	            CASE laFileStr[lnX,2] ='N'  
*!*	              m.OldValue = STR(EVALUATE(lcbrowTable+"."+laFileStr[lnX,1]),laFileStr[lnX,3],laFileStr[lnX,4])
*!*	              m.NewValue =  STR(EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1]) ,laFileStr[lnX,3],laFileStr[lnX,4])         
*!*				CASE laFileStr[lnX,2] ='D'
*!*	              m.OldValue = DTOC(EVALUATE(lcbrowTable+"."+laFileStr[lnX,1]))
*!*	              m.NewValue =  DTOC(EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1]))
*!*	          ENDCASE    
*!*	          INSERT INTO DataChangeLines FROM MEMVAR
*!*	          SELECT DataChangeLines 
*!*	          =gfReplace('')  
         * IF oAriaApplication.User_Level ='A'
         lcUpVal = EVALUATE(loformset.lcTempFile+"."+laFileStr[lnX,1])
         SELECT(lcbrowTable)
         =gfReplace(""+laFileStr[lnX,1]+" WITH lcUpVal")
        *  ENDIF        
       * ENDIF
      ENDFOR
      lcInform = lfGetAudInfo(lcbrowTable)
      m.cevent_id = 'UPDATE'
  ENDCASE 
  *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
  m.ParentChangeID  = loformset.lcParentChangeID 
  *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
  m.TicketNo  = loformset.lcTicketNo
  m.ChangeReason = loformset.lcChangeReason
  m.CAUDTRALID =lcAudTrlID
  m.CEVNTOBJID = lcbrowTable 
  m.cAdd_User  = oAriaApplication.User_ID 
  m.dAdd_Date  = DATE()   
  m.cAdd_Time  = gfGetTime()
  m.Key1 = LANG_DATA_MAIN
  m.Rec_no = ''
  INSERT INTO DataChangeHeader FROM MEMVAR
  SELECT DataChangeHeader 
  
*!*	=gfAdd_Info('DataChangeHeader')
 * =gfReplace('')
  =gfReplace("mNeededInf WITH lcInform")
  *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][Start]
  IF  EVALUATE(loformset.lcTempFile+".RecStatus") = 'M'
    =gfReplace("MFLDFDATA WITH lcOldInform")
  ENDIF
  *B612539,1 MMT 03/30/2022 Data Maintenance screen does not save parent change ID, takes time to load huge tables data, and does not delete the records that user wants to delete[T20211008.0003][End]
ENDSCAN
SELECT DataChangeHeader 
=gfTableUpdate() 
*IF oAriaApplication.User_Level ='A'
SELECT(lcbrowTable)
=gfTableUpdate() 
*ENDIF
*!*	SELECT DataChangeLines 
*!*	=gfTableUpdate()
=gfModalGen('INM54070B00000','Dialog',lcAudTrlID)

*!*************************************************************
*! Name      : lfGetAudInfo
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/24/2020
*! Purpose   : Get Information of record to be saved in Audtrail table
*!*************************************************************
FUNCTION lfGetAudInfo
LPARAMETERS lcDbfName 

IF llSQLFile 
  lnFldResult = oAriaApplication.RemoteSystemData.Execute( ;
   "SELECT SYDFIELD.* FROM SYDFIELD,SYDFLFLD WHERE SYDFIELD.cfld_name == SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam == '" + ;
   ALLTRIM(UPPER(lcDbfName)) + "' AND (SYDFIELD.CVER='A40' OR EMPTY(SYDFIELD.CVER))", ;
   '', ;
   "sydfieldtmp", ;
   "", ;
   oAriaApplication.cAria4Sysfiles, ;
   3, ;
   "", ;
   loFormSet.DatasessionID)
ELSE
	lnFldResult = oAriaApplication.remotesystemdata.execute("SELECT SYDFIELD.* FROM SYDFIELD,SYDFLFLD WHERE SYDFIELD.cfld_name == SYDFLFLD.cfld_name AND SYDFLFLD.cfile_nam == '"+;
	             lcDbfName+"' AND (SYDFIELD.CVER='A27' OR EMPTY(SYDFIELD.CVER))",'',;
  "sydfieldtmp","",oAriaApplication.SystemConnectionString,3,'',loFormSet.DatasessionID )
ENDIF
*! E038142,2 MAH 09/23/2004 [END]

lcInform = ""
IF lnFldResult = 1
  SELECT SYDFIELDTMP 
  LOCATE 
	*-- scan through the fields to get the values
  SCAN REST 
    *IF !(ALLTRIM(sydFieldtmp.cFld_Name) $ "CADD_USER,CADD_TIME,DADD_DATE,LLOK_STAT"+;
        	                         ",CLOK_USER,DLOK_DATE,CLOK_TIME")
      lValue = EVALUATE(lcDbfName+"."+ALLTRIM(sydFieldtmp.cFld_Name))
      lcValue = ""
      *-- convert the values of the fields to a character expression
      DO CASE
        CASE TYPE('lValue') = "C"
          lcValue = ALLTRIM(lValue)
        CASE TYPE('lValue') = "N"
          lcValue =  ALLTRIM(STR(lValue,sydFieldtmp.nfld_wdth,nfld_dec))
        CASE TYPE('lValue') = "L"
          lcValue = IIF(lValue,"True","False")
        CASE TYPE('lValue') = "Y"
        CASE TYPE('lValue') $ "DT"
          lcValue = ALLTRIM(DTOC(lValue))
      ENDCASE  
    	lcInform = lcInform + ALLTRIM(sydfieldtmp.cFld_Head)+" = " +lcValue+CHR(13)+CHR(10)
    *ENDIF           
  ENDSCAN
    	
 ENDIF
 RETURN (lcInform )
*!*************************************************************
*! Name      : lfGetTableEntity
*! Developer : Mariam Mazhar[MMT]
*! Date      : 09/24/2020
*! Purpose   : Get Table related entity
*!*************************************************************
 FUNCTION lfGetTableEntity
 PARAMETERS lcFileName
 IF FILE(oAriaApplication.DefaultPath+"TableEntity.xml")
   XMLTOCURSOR(oAriaApplication.DefaultPath+"TableEntity.xml",'TableEntity',512)
   IF USED('TableEntity')
     SELECT TableEntity
     INDEX on cfile_nam TAG 'TableEnt'
   ENDIF
 ENDIF
 lcEntity = ''
 lcScreenName = ''
 IF  USED('TableEntity') AND SEEK(PADR(lcFileName,30),'TableEntity','TableEnt')
   lcEntity = ALLTRIM(TableEntity.a5objnam)
 ENDIF
 IF !USED('SYDOBJCT_ENT')
   =gfOpenTable('SYDOBJCT','CAPOBJNAM','SH','SYDOBJCT_ENT')
 ENDIF
 IF !EMPTY(lcEntity)
   SELECT SYDOBJCT_ENT
   LOCATE FOR STRTRAN(UPPER(SYDOBJCT_ENT.a5objnam ),' ','')== UPPER(lcEntity)
   IF FOUND()
     lcScreenName = SUBSTR(SYDOBJCT_ENT.cbasewind,4)
   ENDIF
 ELSE
   SELECT SYDOBJCT_ENT
   LOCATE FOR ALLTRIM(UPPER(SYDOBJCT_ENT.cbasefile))== UPPER(ALLTRIM(lcFileName))
   IF FOUND()
     lcScreenName = SUBSTR(SYDOBJCT_ENT.cbasewind,4)
   ELSE
     SELECT SYDOBJCT_ENT
     LOCATE FOR UPPER(ALLTRIM(lcFileName)) $  ALLTRIM(UPPER(SYDOBJCT_ENT.mprgfiles))
     IF FOUND()
       lcScreenName = SUBSTR(SYDOBJCT_ENT.cbasewind,4)
     ENDIF   
   ENDIF
 ENDIF
 RETURN (lcScreenName)
 
*!*************************************************************
*! Name      : gfehan
*! Developer : Mariam Mazhar[MMT]
*! Date      : 11/24/2021
*! Purpose   : Overwrite the global error handler
*!*************************************************************
 FUNCTION gfehan
 PARAMETERS lnError, lcmsg, lckode, lcmodul, lnline, lcprint, ;
    lckonsol, lcdevice, keypress, curr_dbf, def_drive, ;
    run_memry, prnt_devic, cur_direc, top_win, llocked, ;
    lcmissing, curs_set,lcLpmHit
*!*************************************************************
*! Name      : GFMODALGEN5    
*! Developer : Mariam Mazhar[MMT]
*! Date      : 11/24/2021
*! Purpose   : overwrite Aria5 message function in case of error
*!*************************************************************
FUNCTION GFMODALGEN5    
LPARAMETER LCDLGID,LCDLGTYP,LCVARSSTR,LCDLGVALID,LCDLGMESSG
IF !SUBSTR(ALLTRIM(UPPER(MESSAGE())),1,30) $ ALLTRIM(UPPER(LCDLGMESSG)) 
SET STEP ON 
 DO GFMODALGEN5  WITH LCDLGID,LCDLGTYP,LCVARSSTR,LCDLGVALID,LCDLGMESSG IN (ADDBS(oAriaApplication.Ref5.applicationhome)+'SY\Ariaglb5.fxp') 
ENDIF
