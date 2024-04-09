*:***************************************************************************
*: Program file  : MFPRCTCW.PRG
*: Program desc. : Custom Cutting ticket form for Carole Wren
*: TRACK NO      : C201311.122,C201312.exe{T20101109.0011}        
*: System        : Aria4XP
*: Module        : MF
*: Developer     : Mariam Mazhar(MMT)
*:***************************************************************************
* Modifications:
* C201312,1 MMT 04/28/2011 Convert Date format Custom UDF to be char(10){T20101109.0011}        
* C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{T20101109.0011}        
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023]
* B609680,1 MMT 10/03/2011 Error in custom cutting ticket form CW if Print BOM is NO[T20110921.0011]
*:***************************************************************************

IF loOGScroll.llOgFltCh
	IF !USED('Customer')
	  =gfOpenTable('Customer','Customer')
	ENDIF
	* C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
	IF !USED('APVENDOR')
	  =gfOpenTable('APVENDOR','VENCODE')
	ENDIF
	* C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        
	lcMainF = loOGScroll.gfTempName()
	lcHdrStmnt = "Select Distinct(poshdr.po) as PO,poshdr.ctkttype,poshdr.style as hdrstyle,poshdr.pattern ,poshdr.entered,poshdr.cdivision"
    * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
	*lcHdrStmnt = lcHdrStmnt +  ",poshdr.CCODEACCT,poshdr.CCOMENTN1,poshdr.CCOMENTN2,poshdr.CMILL  ,poshdr.DEXPDEL1  ,poshdr.DEXPDEL2  ,poshdr.DEXPDEL3,poshdr.CTONUMBER "	
	* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]
	*lcHdrStmnt = lcHdrStmnt +  ",poshdr.CCODEACCT,poshdr.CCOMENTN1,poshdr.CCOMENTN2,poshdr.CMILL  ,poshdr.DEXPDEL1  ,poshdr.DEXPDEL2  ,poshdr.DEXPDEL3,poshdr.CTONUMBER,poshdr.CCODEFACT"	
	lcHdrStmnt = lcHdrStmnt +  ",poshdr.CCODEACCT,poshdr.CCOMENTN1,poshdr.CCOMENTN2,poshdr.CMILL  ,poshdr.FABETD1  ,poshdr.FABETD2  ,poshdr.FABETD3,poshdr.CTONUMBER,poshdr.CCODEFACT"
	* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][END]
    * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        
	lcHdrStmnt = lcHdrStmnt + ",poshdr.status,poshdr.complete from poshdr(index=poshdr) inner join posln (index=posln) "
	lcHdrStmnt = lcHdrStmnt + "on poshdr.cbusdocu+poshdr.cstytype+poshdr.po = posln.cbusdocu+posln.cstytype+posln.po"
	lcHdrStmnt = lcHdrStmnt + " where posln.trancd = 1 and poshdr.cbusdocu = 'P' and poshdr.cstytype = 'U'"
	IF !EMPTY(loOGScroll.lcRpSqlExp)
	  lcHdrStmnt = lcHdrStmnt + " AND " + loOGScroll.lcRpSqlExp
	ENDIF 
	StatCount =  LEN(lcStatus)
	IF StatCount <> 7
	  lcStatusFlt = " POSHDR.STATUS IN ("
	  lnCnt = 1
	  FOR I = 1 TO StatCount
	    lcStatusFlt = IIF(lnCnt = 1,lcStatusFlt + " '" + SUBSTR(lcStatus,I,1) + "' ",lcStatusFlt + ",'" + SUBSTR(lcStatus,I,1) + "' ")
	    lnCnt = 2
	  ENDFOR 
	  lcStatusFlt = lcStatusFlt + ")"
	  IF !EMPTY(lcStatusFlt)
	    lcHdrStmnt = lcHdrStmnt + " AND " + lcStatusFlt    
	  ENDIF 
	ENDIF 
	lnMainResult = loOGScroll.oRDA.SqlRun(lcHdrStmnt,lcTempMain,,oAriaApplication.ActiveCompanyConStr,3,"BROWSE",SET("Datasession" ))
	 
	IF lnMainResult > 0 .AND. lnCPResult > 0
	  SELECT &lcTempMain
	  lnBuffering = CURSORGETPROP("Buffering","&lcTempMain")
	  =CURSORSETPROP("Buffering",3,"&lcTempMain")
	  INDEX ON PO TAG &lcTempMain
	  SET ORDER TO TAG &lcTempMain
	  *-make a sql file carrying selected POs
	  lcSqlPOs = loOgScroll.gfSQLTempName('','PO C(6)',lcTempMain,'PO')
	  SELECT CutPICKF
	  lnBuffering = CURSORGETPROP("Buffering","CutPICKF")
	  =CURSORSETPROP("Buffering",3,"CutPICKF")
	  SELECT CutPICKF
	  INDEX on TranCd+CtktNo+Style+CTKTLINENO TAG CUTPICKF
	  SET ORDER TO TAG CutPICKF
	ENDIF 
	lfCrtTmpF()
    loPosLn.SetOrder('POSLNW')
     * C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]
	 lcColorSep =''
	 STORE 0 TO lnScaPosGl ,lnScaLnGl
     DECLARE laItemSeg[1]
	 =gfItemMask(@laItemSeg)
	 FOR lnCount = 1 TO ALEN(laItemSeg,1)
	   IF laItemSeg[lnCount,1]='C'
		 lcColorSep =laItemSeg[lnCount,6]
	   ENDIF
	   IF laItemSeg[lnCount,1]='S'
	     lnScaLnGl  = LEN(laItemSeg[lnCount,3])
	     lnScaPosGl = laItemSeg[lnCount,4]
 	     *EXIT
	   ENDIF
	 ENDFOR
	 IF !EMPTY(ALLTRIM(lcColorSep))
  	   lnScaPosGl = lnScaPosGl - LEN(ALLTRIM(lcColorSep))-1
  	 ELSE
	   lnScaPosGl = lnScaPosGl - 1 
	 ENDIF
     * C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][End]    
   * B609680,1 MMT 10/03/2011 Error in custom cutting ticket form CW if Print BOM is NO[START]
   IF TYPE('loFabric') <> 'O'
     loFabric = CreateObject('RemoteTable','ITEM','STYLE','FABRIC_x',SET("Datasession"))
   ENDIF
   * B609680,1 MMT 10/03/2011 Error in custom cutting ticket form CW if Print BOM is NO[END]
   
	SELECT &lcTempMain
	SCAN 
	  lcPo = &lcTempMain..PO
	  lcWareCode = SPACE(8)
	  lcStyle    = SPACE(20)
	  IF loPosLn.SEEK('P'+'U'+lcPo,'POSLNW')
	    SELECT POSLN
	    SCAN FOR posln.TranCd = '1'
	      SCATTER MEMVAR
	      m.NoteFlag = 'N'
	      m.Notes    = ''
	      INSERT INTO (lcMainF) FROM MEMVAR   
	      	* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]     
*!*		      REPLACE &lcMainF..cTktType   WITH &lcTempMain..CtktType ,; 
*!*			        		&lcMainF..HPattern   WITH &lcTempMain..Pattern  ,; 
*!*									&lcMainF..HdrStyle   WITH &lcTempMain..HdrStyle ,; 
*!*									&lcMainF..HEntered   WITH &lcTempMain..Entered  ,; 
*!*									&lcMainF..Status     WITH &lcTempMain..Status   ,; 
*!*									&lcMainF..cDivision  WITH &lcTempMain..cDivision ,; 
*!*									&lcMainF..HComplete  WITH &lcTempMain..Complete,;
*!*									&lcMainF..BtName     WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
*!*									&lcMainF..CCOMENTN1  WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
*!*									&lcMainF..CCOMENTN2  WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
*!*									&lcMainF..CMILL      WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
*!*									&lcMainF..DEXPDEL1   WITH IIF(ISNULL(&lcTempMain..DEXPDEL1) ,"",&lcTempMain..DEXPDEL1),;
*!*									&lcMainF..DEXPDEL2   WITH IIF(ISNULL(&lcTempMain..DEXPDEL2) ,"",&lcTempMain..DEXPDEL2),;
*!*									&lcMainF..DEXPDEL3   WITH IIF(ISNULL(&lcTempMain..DEXPDEL3) ,"",&lcTempMain..DEXPDEL3),;
*!*									&lcMainF..CTONUMBER  WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
*!*								  &lcMainF..CFABRIC    WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric ,''),;
*!*						  	  &lcMainF..CFABDESC   WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric ),'STYLE'),FABRIC_x.DESC,'')IN (lcMainF)
	      REPLACE &lcMainF..cTktType   WITH &lcTempMain..CtktType ,; 
		        		&lcMainF..HPattern   WITH &lcTempMain..Pattern  ,; 
								&lcMainF..HdrStyle   WITH &lcTempMain..HdrStyle,; 
								&lcMainF..HEntered   WITH &lcTempMain..Entered  ,; 
								&lcMainF..Status     WITH &lcTempMain..Status   ,; 
								&lcMainF..cDivision  WITH &lcTempMain..cDivision ,; 
								&lcMainF..HComplete  WITH &lcTempMain..Complete,;
								&lcMainF..BtName     WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
								&lcMainF..CCOMENTN1  WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
								&lcMainF..CCOMENTN2  WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
								&lcMainF..CMILL      WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
								&lcMainF..FABETD1   WITH IIF(ISNULL(&lcTempMain..FABETD1) ,"",&lcTempMain..FABETD1),;
								&lcMainF..FABETD2   WITH IIF(ISNULL(&lcTempMain..FABETD2) ,"",&lcTempMain..FABETD2),;
								&lcMainF..FABETD3   WITH IIF(ISNULL(&lcTempMain..FABETD3) ,"",&lcTempMain..FABETD3),;
								&lcMainF..CTONUMBER  WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
	     		 			    &lcMainF..CFABRIC    WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric ,''),;
						  	    &lcMainF..CFABDESC   WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric ),'STYLE'),FABRIC_x.DESC,''),;
						  	    &lcMainF..OStyle    WITH &lcMainF..STYLE,;
						  	    &lcMainF..STYLE      WITH SUBSTR(&lcMainF..STYLE,1,lnScaPosGl)  IN (lcMainF)
		  * C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][End]
      	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
		  REPLACE &lcMainF..FactName WITH IIF(!ISNULL(&lcTempMain..CCODEFACT) AND !EMPTY(&lcTempMain..CCODEFACT) AND  gfSeek(&lcTempMain..CCODEFACT,'apvendor'),apvendor.cvencomp,'') IN (lcMainF)
   	  	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        
		  
	      IF lcRpSize = 'R' AND !EMPTY(m.PREPAK)
	        =SEEK('P'+m.Scale+m.PREPAK,'Scale','Scale')
	        FOR lnD = 1 TO 8
	          lcD = STR(lnD,1)
	          REPLACE &lcMainF..Qty&lcD. WITH Scale.pp&lcD. 
	        ENDFOR
	      ENDIF 
	      *-- Get last style in the cuttkt to put the notes lines directly after it.
	      IF lcStyle < Style
	        lcStyle = Style
	      ENDIF
	      *-- We will print notes with each warehouse.
	      IF cWareCode # lcWareCode .AND. !EMPTY(lcWareCode)
	        m.cWareCode = lcWareCode
	        lcWareCode  = cWareCode
	        m.Style     = lcStyle
	        m.Dyelot    = CHR(255)
	        M.QTY1      = 0
	        M.TOTQTY    = 0
	        *-- Get the style note.
	        IF llRPrtSn .AND. SEEK('F'+&lcTempMain..HdrStyle,'NotePad')
	          m.NoteFlag = 'S'
	          m.Notes    = Notepad.MNotes
	          
	          INSERT INTO (lcMainF) FROM MEMVAR  
  			  * C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]		          
*!*		          REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
*!*		                  &lcMainF..HPattern   WITH &lcTempMain..Pattern ,; 
*!*		                  &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
*!*		                  &lcMainF..HEntered   WITH &lcTempMain..Entered ,; 
*!*		                  &lcMainF..Status    WITH &lcTempMain..Status ,; 
*!*		                  &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
*!*		                  &lcMainF..HComplete  WITH &lcTempMain..Complete ,; 
*!*		                  &lcMainF..NoteFlag  WITH m.NoteFlag ,; 
*!*		                  &lcMainF..Notes    WITH m.Notes ,;
*!*		                  &lcMainF..BtName    WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
*!*											&lcMainF..CCOMENTN1 WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
*!*											&lcMainF..CCOMENTN2 WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
*!*											&lcMainF..CMILL     WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
*!*											&lcMainF..DEXPDEL1  WITH IIF(ISNULL(&lcTempMain..DEXPDEL1) ,'',&lcTempMain..DEXPDEL1),;
*!*											&lcMainF..DEXPDEL2  WITH IIF(ISNULL(&lcTempMain..DEXPDEL2) ,'',&lcTempMain..DEXPDEL2),;
*!*											&lcMainF..DEXPDEL3  WITH IIF(ISNULL(&lcTempMain..DEXPDEL3) ,'',&lcTempMain..DEXPDEL3),;
*!*											&lcMainF..CTONUMBER WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
*!*										  &lcMainF..CFABRIC   WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric,''),;
*!*								  	  &lcMainF..CFABDESC  WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric),'STYLE'),FABRIC_x.DESC,'')IN (lcMainF)
	          REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
	                  &lcMainF..HPattern  WITH &lcTempMain..Pattern ,; 
	                  &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
	                  &lcMainF..HEntered  WITH &lcTempMain..Entered ,; 
	                  &lcMainF..Status    WITH &lcTempMain..Status ,; 
	                  &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
	                  &lcMainF..HComplete WITH &lcTempMain..Complete ,; 
	                  &lcMainF..NoteFlag  WITH m.NoteFlag ,; 
	                  &lcMainF..Notes     WITH m.Notes ,;
	                  &lcMainF..BtName    WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
					  &lcMainF..CCOMENTN1 WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
					  &lcMainF..CCOMENTN2 WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
					  &lcMainF..CMILL     WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
					  &lcMainF..FABETD1  WITH IIF(ISNULL(&lcTempMain..FABETD1) ,'',&lcTempMain..FABETD1),;
					  &lcMainF..FABETD2  WITH IIF(ISNULL(&lcTempMain..FABETD2) ,'',&lcTempMain..FABETD2),;
					  &lcMainF..FABETD3  WITH IIF(ISNULL(&lcTempMain..FABETD3) ,'',&lcTempMain..FABETD3),;
					  &lcMainF..CTONUMBER WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
					  &lcMainF..CFABRIC   WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric,''),;
					  &lcMainF..CFABDESC  WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric),'STYLE'),FABRIC_x.DESC,''),;
					  &lcMainF..OStyle    WITH &lcMainF..STYLE,;
					  &lcMainF..STYLE      WITH SUBSTR(&lcMainF..STYLE,1,lnScaPosGl)IN (lcMainF)

		  * C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][End]
 	  	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
		  REPLACE &lcMainF..FactName WITH IIF(!ISNULL(&lcTempMain..CCODEFACT) AND !EMPTY(&lcTempMain..CCODEFACT) AND  gfSeek(&lcTempMain..CCODEFACT,'apvendor'),apvendor.cvencomp,'') IN (lcMainF)
   	  	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        
							  	  
	            
	        ENDIF
	        *-- Get the cutting ticket note.
	        IF llRPrtCtn .AND. SEEK('I'+m.PO,'NotePad')
	          m.NoteFlag = 'T'
	          m.Notes    = Notepad.MNotes
	          INSERT INTO (lcMainF) FROM MEMVAR            
	          	* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]
*!*		          REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
*!*		                  &lcMainF..HPattern  WITH &lcTempMain..Pattern ,; 
*!*		                  &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
*!*		                  &lcMainF..HEntered  WITH &lcTempMain..Entered ,; 
*!*		                  &lcMainF..Status    WITH &lcTempMain..Status ,; 
*!*		                  &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
*!*		                  &lcMainF..HComplete WITH &lcTempMain..Complete,;
*!*		                  &lcMainF..BtName    WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
*!*											&lcMainF..CCOMENTN1 WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
*!*											&lcMainF..CCOMENTN2 WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
*!*											&lcMainF..CMILL     WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
*!*											&lcMainF..DEXPDEL1  WITH IIF(ISNULL(&lcTempMain..DEXPDEL1) ,'',&lcTempMain..DEXPDEL1),;
*!*											&lcMainF..DEXPDEL2  WITH IIF(ISNULL(&lcTempMain..DEXPDEL2) ,'',&lcTempMain..DEXPDEL2),;
*!*											&lcMainF..DEXPDEL3  WITH IIF(ISNULL(&lcTempMain..DEXPDEL3) ,'',&lcTempMain..DEXPDEL3),;
*!*											&lcMainF..CTONUMBER WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
*!*										  &lcMainF..CFABRIC   WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric,''),;
*!*								  	  &lcMainF..CFABDESC  WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric),'STYLE'),FABRIC_x.DESC,'')IN (lcMainF)
	          REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
	                  &lcMainF..HPattern  WITH &lcTempMain..Pattern ,; 
	                  &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
	                  &lcMainF..HEntered  WITH &lcTempMain..Entered ,; 
	                  &lcMainF..Status    WITH &lcTempMain..Status ,; 
	                  &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
	                  &lcMainF..HComplete WITH &lcTempMain..Complete,;
	                  &lcMainF..BtName    WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
					  &lcMainF..CCOMENTN1 WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
 					  &lcMainF..CCOMENTN2 WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
					  &lcMainF..CMILL     WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
					  &lcMainF..FABETD1  WITH IIF(ISNULL(&lcTempMain..FABETD1) ,'',&lcTempMain..FABETD1),;
					  &lcMainF..FABETD2  WITH IIF(ISNULL(&lcTempMain..FABETD2) ,'',&lcTempMain..FABETD2),;
					  &lcMainF..FABETD3  WITH IIF(ISNULL(&lcTempMain..FABETD3) ,'',&lcTempMain..FABETD3),;
					  &lcMainF..CTONUMBER WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
					  &lcMainF..CFABRIC   WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric,''),;
					  &lcMainF..CFABDESC  WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric),'STYLE'),FABRIC_x.DESC,''),;
					  &lcMainF..OStyle    WITH &lcMainF..STYLE,;
					  &lcMainF..STYLE      WITH SUBSTR(&lcMainF..STYLE,1,lnScaPosGl)IN (lcMainF)
		  	  * C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][End]
 		  	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
			  REPLACE &lcMainF..FactName WITH IIF(!ISNULL(&lcTempMain..CCODEFACT) AND !EMPTY(&lcTempMain..CCODEFACT) AND  gfSeek(&lcTempMain..CCODEFACT,'apvendor'),apvendor.cvencomp,'') IN (lcMainF)
  		  	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        
							  	  
			      IF lcRpSize = 'R' AND !EMPTY(m.PREPAK)
			        =SEEK('P'+m.Scale+m.PREPAK,'Scale','Scale')
			        FOR lnD = 1 TO 8
			          lcD = STR(lnD,1)
			          REPLACE &lcMainF..Qty&lcD. WITH Scale.pp&lcD. 
			        ENDFOR
			      ENDIF 
							  	  
	         
	        ENDIF
	      ENDIF
	      IF EMPTY(lcWareCode)
	        lcWareCode = cWareCode
	        lcStyle    = Style
	      ENDIF
		ENDSCAN
	    m.cWareCode = lcWareCode
	    m.Style     = lcStyle
	    m.Dyelot    = CHR(255)
	    *-- Print notes for last warehouse.
	    IF llRPrtSn .AND. SEEK('F'+&lcTempMain..HdrStyle,'NotePad')
	      m.NoteFlag = 'S'
	      m.Notes    = Notepad.MNotes
	      INSERT INTO (lcMainF) FROM MEMVAR 
	* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]	             
*!*		      REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
*!*		              &lcMainF..HPattern  WITH &lcTempMain..Pattern ,; 
*!*		              &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
*!*		              &lcMainF..HEntered  WITH &lcTempMain..Entered ,; 
*!*		              &lcMainF..Status    WITH &lcTempMain..Status ,; 
*!*		              &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
*!*		              &lcMainF..HComplete WITH &lcTempMain..Complete ,;
*!*		              &lcMainF..BtName    WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
*!*									&lcMainF..CCOMENTN1 WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
*!*									&lcMainF..CCOMENTN2 WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
*!*									&lcMainF..CMILL     WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
*!*									&lcMainF..DEXPDEL1  WITH IIF(ISNULL(&lcTempMain..DEXPDEL1) ,'',&lcTempMain..DEXPDEL1),;
*!*									&lcMainF..DEXPDEL2  WITH IIF(ISNULL(&lcTempMain..DEXPDEL2) ,'',&lcTempMain..DEXPDEL2),;
*!*									&lcMainF..DEXPDEL3  WITH IIF(ISNULL(&lcTempMain..DEXPDEL3) ,'',&lcTempMain..DEXPDEL3),;
*!*		  						&lcMainF..CTONUMBER WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
*!*								  &lcMainF..CFABRIC   WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric ,''),;
*!*						  	  &lcMainF..CFABDESC  WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric),'STYLE'),FABRIC_x.DESC,'') IN(lcMainF)
	      REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
	              &lcMainF..HPattern  WITH &lcTempMain..Pattern ,; 
	              &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
	              &lcMainF..HEntered  WITH &lcTempMain..Entered ,; 
	              &lcMainF..Status    WITH &lcTempMain..Status ,; 
	              &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
	              &lcMainF..HComplete WITH &lcTempMain..Complete ,;
	              &lcMainF..BtName    WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
  				  &lcMainF..CCOMENTN1 WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
				  &lcMainF..CCOMENTN2 WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
				  &lcMainF..CMILL     WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
				  &lcMainF..FABETD1  WITH IIF(ISNULL(&lcTempMain..FABETD1) ,'',&lcTempMain..FABETD1),;
				  &lcMainF..FABETD2  WITH IIF(ISNULL(&lcTempMain..FABETD2) ,'',&lcTempMain..FABETD2),;
				  &lcMainF..FABETD3  WITH IIF(ISNULL(&lcTempMain..FABETD3) ,'',&lcTempMain..FABETD3),;
	  			  &lcMainF..CTONUMBER WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
				  &lcMainF..CFABRIC   WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric ,''),;
				  &lcMainF..CFABDESC  WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric),'STYLE'),FABRIC_x.DESC,''),;
				  &lcMainF..OStyle    WITH &lcMainF..STYLE,;
				  &lcMainF..STYLE     WITH SUBSTR(&lcMainF..STYLE,1,lnScaPosGl) IN(lcMainF)
	* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][End]
  	  	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
		  REPLACE &lcMainF..FactName WITH IIF(!ISNULL(&lcTempMain..CCODEFACT) AND !EMPTY(&lcTempMain..CCODEFACT) AND  gfSeek(&lcTempMain..CCODEFACT,'apvendor'),apvendor.cvencomp,'') IN (lcMainF)
  	  	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        
					  	  
	      IF lcRpSize = 'R' AND !EMPTY(m.PREPAK)
	        =SEEK('P'+m.Scale+m.PREPAK,'Scale','Scale')
	        FOR lnD = 1 TO 8
	          lcD = STR(lnD,1)
	          REPLACE &lcMainF..Qty&lcD. WITH Scale.pp&lcD. 
	        ENDFOR
	      ENDIF 
					  	  
	      
	    ENDIF
	    IF llRPrtCtn .AND. SEEK('I'+m.PO,'NotePad')
		    m.NoteFlag = 'T'
		    m.Notes    = Notepad.MNotes
		    INSERT INTO (lcMainF) FROM MEMVAR        
		    	* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]
*!*			    REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
*!*		              &lcMainF..HPattern  WITH &lcTempMain..Pattern ,; 
*!*		              &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
*!*		              &lcMainF..HEntered  WITH &lcTempMain..Entered ,; 
*!*		              &lcMainF..Status    WITH &lcTempMain..Status ,; 
*!*		              &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
*!*		              &lcMainF..HComplete WITH &lcTempMain..Complete ,;
*!*		              &lcMainF..BtName    WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
*!*									&lcMainF..CCOMENTN1 WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
*!*									&lcMainF..CCOMENTN2 WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
*!*									&lcMainF..CMILL  	 WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
*!*									&lcMainF..DEXPDEL1  WITH IIF(ISNULL(&lcTempMain..DEXPDEL1) ,'',&lcTempMain..DEXPDEL1),;
*!*									&lcMainF..DEXPDEL2  WITH IIF(ISNULL(&lcTempMain..DEXPDEL2) ,'',&lcTempMain..DEXPDEL2) ,;
*!*									&lcMainF..DEXPDEL3  WITH IIF(ISNULL(&lcTempMain..DEXPDEL3) ,'',&lcTempMain..DEXPDEL3) ,;
*!*									&lcMainF..CTONUMBER WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
*!*								  &lcMainF..CFABRIC   WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric,''),;
*!*						  	  &lcMainF..CFABDESC  WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric),'STYLE'),FABRIC_x.DESC,'') IN (lcMainF)
		    REPLACE &lcMainF..cTktType  WITH &lcTempMain..CtktType ,; 
	              &lcMainF..HPattern  WITH &lcTempMain..Pattern ,; 
	              &lcMainF..HdrStyle  WITH &lcTempMain..HdrStyle ,; 
	              &lcMainF..HEntered  WITH &lcTempMain..Entered ,; 
	              &lcMainF..Status    WITH &lcTempMain..Status ,; 
	              &lcMainF..cDivision WITH &lcTempMain..cDivision ,; 
	              &lcMainF..HComplete WITH &lcTempMain..Complete ,;
	              &lcMainF..BtName    WITH IIF(!ISNULL(&lcTempMain..CCODEACCT) AND !EMPTY(&lcTempMain..CCODEACCT) AND gfSeek('M'+&lcTempMain..CCODEACCT,'CUSTOMER'),Customer.btname,''),;
			      &lcMainF..CCOMENTN1 WITH IIF(ISNULL(&lcTempMain..CCOMENTN1),'',&lcTempMain..CCOMENTN1),;
				  &lcMainF..CCOMENTN2 WITH IIF(ISNULL(&lcTempMain..CCOMENTN2),'',&lcTempMain..CCOMENTN2),;
				  &lcMainF..CMILL  	 WITH IIF(ISNULL(&lcTempMain..CMILL),'',&lcTempMain..CMILL),;
				  &lcMainF..FABETD1  WITH IIF(ISNULL(&lcTempMain..FABETD1) ,'',&lcTempMain..FABETD1),;
				  &lcMainF..FABETD2  WITH IIF(ISNULL(&lcTempMain..FABETD2) ,'',&lcTempMain..FABETD2) ,;
				  &lcMainF..FABETD3  WITH IIF(ISNULL(&lcTempMain..FABETD3) ,'',&lcTempMain..FABETD3) ,;
				  &lcMainF..CTONUMBER WITH IIF(ISNULL(&lcTempMain..CTONUMBER),'',&lcTempMain..CTONUMBER),;
				  &lcMainF..CFABRIC   WITH IIF(SEEK(ALLTRIM(&lcMainF..HdrStyle),'STYLE','STYLE'),style.fabric,''),;
				  &lcMainF..CFABDESC  WITH IIF(!EMPTY(&lcMainF..CFABRIC) AND loFabric.Seek('0002'+ALLTRIM(style.fabric),'STYLE'),FABRIC_x.DESC,''),;
				  &lcMainF..OStyle    WITH &lcMainF..STYLE,;
				  &lcMainF..STYLE     WITH SUBSTR(&lcMainF..STYLE,1,lnScaPosGl) IN (lcMainF)
  		  * C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][End]
   	  	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
		  REPLACE &lcMainF..FactName WITH IIF(!ISNULL(&lcTempMain..CCODEFACT) AND !EMPTY(&lcTempMain..CCODEFACT) AND  gfSeek(&lcTempMain..CCODEFACT,'apvendor'),apvendor.cvencomp,'') IN (lcMainF)
   	  	  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        
					  	  
	      IF lcRpSize = 'R' AND !EMPTY(m.PREPAK)
	        =SEEK('P'+m.Scale+m.PREPAK,'Scale','Scale')
	        FOR lnD = 1 TO 8
	          lcD = STR(lnD,1)
	          REPLACE &lcMainF..Qty&lcD. WITH Scale.pp&lcD. 
	        ENDFOR
	      ENDIF 
					  	  
	      
	    ENDIF
	    GO TOP IN (lcMainF)
	  ENDIF
	ENDSCAN  && End loop of CUTTKTH file.
	SELECT (lcMainF)
	SET RELATION TO "1"+PO+IIF(&lcMainF..NoteFlag='N',style,SPACE(20))+STR(LINENO ,6) INTO CutpickF ADDITIVE
	* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][START]
	*SET RELATION TO STYLE INTO STYLE ADDITIVE  
	SET RELATION TO OStyle  INTO STYLE ADDITIVE  
	* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][END]
	IF llRPrtAlo
	  SET SKIP TO CUTPICKF
	ENDIF
	IF llRpPic
	  SELECT &lcMainF
	  SET RELATION TO 'S'+ &lcMainF..Hdrstyle INTO Objlink ADDITIVE
	ENDIF   
ENDIF

*!*************************************************************
*! Name      : lfCrtTmpF
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/13/2011
*! Purpose   : Create Temp. File
*!*************************************************************
FUNCTION lfCrtTmpF
SELECT POSLN
=AFIELDS(laFileStru)
lnFileStru = ALEN(laFileStru,1)
* C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
*DIMENSION laFileStru[lnFileStru+22,18]
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][START]
*DIMENSION laFileStru[lnFileStru+23,18]
DIMENSION laFileStru[lnFileStru+24,18]
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][End]
* C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cTktType' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'HPattern' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'HdrStyle' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 19
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'HEntered' 
laFileStru[lnFileStru,2] = 'D'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'HComplete' 
laFileStru[lnFileStru,2] = 'D'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Desc' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 20
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Desc1' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 60
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'OComplete' 
laFileStru[lnFileStru,2] = 'D'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'NoteFlag' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Notes' 
laFileStru[lnFileStru,2] = 'M'
laFileStru[lnFileStru,3] = 0
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'cDivision' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 6
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'Status' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 1
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'BTNAME' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CCOMENTN1' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CCOMENTN2' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CMILL' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]
*laFileStru[lnFileStru,1] = 'DEXPDEL1' 
laFileStru[lnFileStru,1] = 'FABETD1' 
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][End]
* C201312,1 MMT 04/28/2011 Convert Date format Custom UDF to be char(10){Start}        
*!*	laFileStru[lnFileStru,2] = 'D'
*!*	laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
* C201312,1 MMT 04/28/2011 Convert Date format Custom UDF to be char(10){End}        
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]
*laFileStru[lnFileStru,1] = 'DEXPDEL2' 
laFileStru[lnFileStru,1] = 'FABETD2' 
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][END]
* C201312,1 MMT 04/28/2011 Convert Date format Custom UDF to be char(10){Start}        
*!*	laFileStru[lnFileStru,2] = 'D'
*!*	laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
* C201312,1 MMT 04/28/2011 Convert Date format Custom UDF to be char(10){End}        
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][Start]
*laFileStru[lnFileStru,1] = 'DEXPDEL3' 
laFileStru[lnFileStru,1] = 'FABETD3' 
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][End]
* C201312,1 MMT 04/28/2011 Convert Date format Custom UDF to be char(10){Start}        
*!*	laFileStru[lnFileStru,2] = 'D'
*!*	laFileStru[lnFileStru,3] = 8
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
* C201312,1 MMT 04/28/2011 Convert Date format Custom UDF to be char(10){End}        
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CTONUMBER' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 10
laFileStru[lnFileStru,4] = 0
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CFABRIC' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 19
laFileStru[lnFileStru,4] = 0

lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'CFABDESC' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0
* C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'FactName' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 30
laFileStru[lnFileStru,4] = 0
* C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        

* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][START]
lnFileStru = lnFileStru+1
laFileStru[lnFileStru,1] = 'OStyle' 
laFileStru[lnFileStru,2] = 'C'
laFileStru[lnFileStru,3] = 19
laFileStru[lnFileStru,4] = 0
* C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][END]

FOR  lnLen = 7 TO 18
  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{Start}        
  *FOR lnCount = 0 TO 22
  * C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][START]
  *FOR lnCount = 0 TO 23
  FOR lnCount = 0 TO 24
  * C201343,2 MMT 06/26/2011 Changes in Cut ticket form CW[T20110620.0023][END]
  * C201343,1 MMT 05/23/2011 Display the Vendor Name in Cut tkt form CW{End}        
    STORE SPACE(1) TO laFileStru[lnFileStru - lnCount,lnLen]
  ENDFOR 
ENDFOR

gfCrtTmp(lcMainF,@laFileStru,"PO+cWareCode+Style+Dyelot+NoteFlag",lcMainF,.F.)
*!*************************************************************
*! Name      : lfGetLastLine
*: Developer : MAriam Mazhar (MMT)
*: Date      : 03/13/2011
*! Purpose   : get Last line in PO/Warehouse
*!*************************************************************
FUNCTION lfGetLastLine

lnLastLineNo = 0
lcOldAl = SELECT(0)
lnCurrRec = RECNO(lcMainF)
lcPO = EVAL(lcMainF+'.PO')
lcWare = EVAL(lcMainF+'.cWareCode')
SELECT (lcMainF)
SCAN FOR PO = lcPO  AND cwarecode = lcWare AND NOTEFLAG ='N'
  IF lnLastLineNo < LINENO
    lnLastLineNo = LINENO
  ENDIF
ENDSCAN  
IF BETWEEN(lnCurrRec,1,RECCOUNT(lcMainF))
  GO RECORD lnCurrRec IN (lcMainF)
ENDIF
SELECT(lcOldAl)
RETURN ''