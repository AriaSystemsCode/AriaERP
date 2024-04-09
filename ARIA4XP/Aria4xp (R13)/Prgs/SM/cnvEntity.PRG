*E612215,1 MMT 10/08/2020 Convert System entities to Entity and attachment tables
*B612501,1 MMT 11/29/2021 Modify Aria5 Desktop to open Global search screen filtered by search value entered by the user in search textbox
LPARAMETERS lcA27Sys
SET STEP ON 
ON ERROR 
LOCAL lnConnHandler   ,lnEntInserEnt 
lnConnHandler = 0            
lnEntInserEnt = 0
oAriaApplication.User_ID = 'Admin'
oAriaApplication.ref4.User_ID = 'Admin'
oAriaApplication.ref5.User_ID = 'Admin'
oxml =  CREATEOBJECT("MSXML2.DOMDocument")
USE ADDBS(lcA27Sys)+'SYCCOMP.DBF' SHAR ALIAS 'SYCCOMP_A' IN 0
SELECT SYCCOMP_A
lcOldComp = oAriaApplication.ActiveCompanyId
SCAN FOR lRunfroma4
  WAIT WINDOW 'Updating Entity table of company:'+SYCCOMP_A.ccomp_id NOWAIT
  SELECT SYCCOMP_A
  IF EMPTY(ALLTRIM(CCONSERVER)) OR EMPTY(ALLTRIM(CCONDBNAME))
    LOOP
  ENDIF
  oAriaApplication.ref4.activecompanyid = SYCCOMP_A.ccomp_id
  oAriaApplication.GetCompanyInformation(SYCCOMP_A.ccomp_id)
  
  IF !USED('NOTEPAD')
     =gfOpenTable('NOTEPAD', 'NOTEPAD' ,'SH')
  ENDIF
  lnEntStatusSel = oAriaApplication.RemoteCompanyData.execute("Select * from [EntityStatus]",'',;
                                                                  "EntityStatus","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
                                                                  
  lnEntSetDSel = oAriaApplication.RemoteCompanyData.execute("Select *, CAST(ExtraData as text) as 'ExtraData1' from [EntityTypeSettings] ",'',;
                                                                "EntitySetting","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
                                                                
                                                                

  SELECT "EntitySetting"
  LOCATE
  SCAN
    *B612501,1 MMT 11/29/2021 Modify Aria5 Desktop to open Global search screen filtered by search value entered by the user in search textbox[Start]
    DIMENSION laFields[1,4]
    laFields = ''
    oxml.Loadxml(aLLTRIM(EntitySetting.ExtraData1))
    loRoot = oxml.childNodes(0)
*!*	    	lcDataXML = '<DataValues>'&&+CHR(13)+CHR(10)
    	FOR lnX = 0 TO loRoot.childNodes(0).childNodes.Length-1
    	  lcFldName = ''
    	  lcDatType =''
    	  lnWidth = ''
    	  lnDecimals = 0
    	  FOR lnZ =  0 TO loRoot.childNodes(0).childNodes(lnX).childNodes.Length-1
    	    IF ALLTRIM(UPPER(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).baseName)) =='NAME'
    	      lcFldName =  ALLTRIM(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).text)
    	      LOOP
    	    ENDIF
    	    IF ALLTRIM(UPPER(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).baseName)) =='DATATYPE'
    	      lcDatType =  upper(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).text)
    	      LOOP 
    	    ENDIF
    	    IF ALLTRIM(UPPER(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).baseName)) =='WIDTH'
    	      lnWidth =  VAL(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).text)
    	      LOOP 
    	    ENDIF
    	    IF ALLTRIM(UPPER(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).baseName)) =='DECIMALS'
    	      lnDecimals =  VAL(loRoot.childNodes(0).childNodes(lnX).childNodes(lnZ).text)
    	      LOOP 
    	    ENDIF
    	  ENDFOR 
  		  IF !EMPTY(lcFldName)
  		    IF EMPTY(laFields[1,1])
  		      laFields[1,1] = lcFldName 
  		      laFields[1,2] = IIF(lcDatType ='NUMERIC','N',IIF(lcDatType ='BIT','L',IIF(lcDatType ='DATE','D','C')))
  		      laFields[1,3] = lnWidth 
  		      laFields[1,4] = lnDecimals 
  		    ELSE
  		      DIMENSION  laFields[ALEN( laFields,1)+1,4]
  		      laFields[ALEN( laFields,1),1] = lcFldName 
  		      laFields[ALEN( laFields,1),2] = IIF(lcDatType ='NUMERIC','N',IIF(lcDatType ='BIT','L',IIF(lcDatType ='DATE','D','C')))
  		      laFields[ALEN( laFields,1),3] = lnWidth 
  		      laFields[ALEN( laFields,1),4] = lnDecimals 
  		    ENDIF
  		    
  		  ENDIF
	  	ENDFOR
	  	
	  	
	  	  lnConnHandler1= 0
	  	 lnEntSelectEnt = oAriaApplication.RemoteCompanyData.execute("DELETE from EntityAttachment Where ENTITY in (Select OID FROM ENTITY  WHERE [TYPE]='"+EntitySetting.EntityTypeOID+"')" ,'',;
                                                                  "EntityOID","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"),.T., @lnConnHandler1)
                                                                  
        IF lnEntSelectEnt > 0
  			  SQLCOMMIT(lnConnHandler1)
  			ELSE
*  			  =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnEntInserEnt,.T.)
  			  SQLROLLBACK(lnConnHandler1)
  			  RETURN
  			ENDIF                                                                  
	  	*B612501,1 MMT 11/29/2021 Modify Aria5 Desktop to open Global search screen filtered by search value entered by the user in search textbox[End]
	  	
	  	
	  	  lnConnHandler= 0
	  	 lnEntSelectEnt = oAriaApplication.RemoteCompanyData.execute("DELETE from ENTITY WHERE [TYPE]='"+EntitySetting.EntityTypeOID+"'" ,'',;
                                                                  "EntityOID","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"),.T., @lnConnHandler)
                                                                  
        IF lnEntSelectEnt > 0
  			  SQLCOMMIT(lnConnHandler)
  			ELSE
*  			  =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnEntInserEnt,.T.)
  			  SQLROLLBACK(lnConnHandler)
  			  RETURN
  			ENDIF                                                                  
*B612501,1 MMT 11/29/2021 Modify Aria5 Desktop to open Global search screen filtered by search value entered by the user in search textbox[End]
    lcMasterFile = ALLTRIM(EntitySetting.mastertable)
    =gfOpenTable(ALLTRIM(EntitySetting.mastertable),ALLTRIM(EntitySetting.mastertableindex),'SH')
    SELECT(lcMasterFile) 
    lcKeyFld = KEY() 
    lcSeekExpress =''
    lcScanExp = ''
    IF !EMPTY(ALLTRIM(EntitySetting.indexexpressiondefaultvalue))
      DIMENSION laIndexFldV [1]
      laIndexFldV = ''
      lcSeekExpress = '' 
      =gfSubStr(ALLTRIM(EntitySetting.indexexpressiondefaultvalue),@laIndexFldV ,',')
      IF !Empty(laIndexFldV[1])
        FOR lnIns = 1 TO ALEN(laIndexFldV,1)
          IF TYPE(lcMasterFile+"."+ALLTRIM(laIndexFldV[lnIns]))='U'
            lcSeekExpress = lcSeekExpress +IIF(EMPTY(lcSeekExpress),'',"+")+ ALLTRIM(laIndexFldV[lnIns]) 
          ENDIF
        ENDFOR
      ENDIF
      IF !EMPTY(lcSeekExpress)
        SELECT(lcMasterFile)
        =gfSeek(&lcSeekExpress.)
        lcScanExp = "REST WHILE "+ lcKeyFld + " = "+lcSeekExpress
      ELSE
       SELECT(lcMasterFile) 
       = gfSeek('')  
      ENDIF  
    ELSE
     SELECT(lcMasterFile) 
     = gfSeek('')  
    ENDIF 
    
    SELECT(lcMasterFile) 
    SCAN &lcScanExp.
        
*!*	      oxml.Loadxml(aLLTRIM(EntitySetting.ExtraData1))
*!*	      loRoot = oxml.childNodes(0)
    	lcDataXML = '<DataValues>'&&+CHR(13)+CHR(10)
    	FOR lnX = 1 TO ALEN(laFields,1)
    	  
  		  IF !EMPTY(laFields[lnX ,1])
  		    lcValue = ''
  		    DO CASE 
  		      CASE UPPER(laFields[lnX ,2])  = 'N'
  		        lcValue = STR(EVALUATE(lcMasterFile+"."+ALLTRIM(laFields[lnX ,1])),laFields[lnX ,3],laFields[lnX ,4])
  		      CASE UPPER(ALLTRIM(laFields[lnX ,2]))  = 'L'
  		        lcValue = IIF(EVALUATE(lcMasterFile+"."+ALLTRIM(laFields[lnX ,1])),"1","0")
  		      CASE UPPER(ALLTRIM(laFields[lnX ,2]))  = 'D'
  		        lcValue = lfCodeSpChr(DTOC(EVALUATE(lcMasterFile+"."+ALLTRIM(laFields[lnX ,1]))))
  		      OTHERWISE 
  		        lcValue = lfCodeSpChr(EVALUATE(lcMasterFile+"."+ALLTRIM(laFields[lnX ,1]))  )
  		    ENDCASE 
  		    lcDataXML = lcDataXML +  "<"+ALLTRIM(laFields[lnX ,1])+">"+ lcValue + "</"+ALLTRIM(laFields[lnX ,1])+">"&&+CHR(13)+CHR(10)
  		  ENDIF
	  	ENDFoR 
  		lcDataXML = lcDataXML +  '</DataValues>'
  		lcDataXML= STRTRAN(lcDataXML,"'","''")
  		FOR lnS = 0 TO 8
    	  IF CHR(lnS) $ lcDataXML
     		lcDataXML= STRTRAN(lcDataXML,CHR(lnS),"CHR("+STR(lnS,1)+")")
  	  	  ENDIF
  		ENDFOR
        FOR lnS = 11 TO 12
    	  IF CHR(lnS) $ lcDataXML
     		lcDataXML= STRTRAN(lcDataXML,CHR(lnS),"CHR("+STR(lnS,2)+")")
  	  	  ENDIF
  		ENDFOR
        FOR lnS = 14 TO 31
    	  IF CHR(lnS) $ lcDataXML
     		lcDataXML= STRTRAN(lcDataXML,CHR(lnS),"CHR("+STR(lnS,2)+")")
  	  	  ENDIF
  		ENDFOR

  		*B612501,1 MMT 11/29/2021 Modify Aria5 Desktop to open Global search screen filtered by search value entered by the user in search textbox[Start]
  		lcNotes = ''
  		IF !EMPTY(ALLTRIM(EntitySetting.entityNotes)) AND !ISNULL(EntitySetting.entityNotes)
   		  SELECT (lcMasterFile)
  		  IF gfSeek(EVALUATE(STRTRAN(ALLTRIM(EntitySetting.entityNotes),',','+')),'NOTEPAD','NOTEPAD')
  		    lcNotes = NOTEPAD.mnotes
  		  ENDIF
  		ENDIF  
  		*B612501,1 MMT 11/29/2021 Modify Aria5 Desktop to open Global search screen filtered by search value entered by the user in search textbox[End]
  		
  		lcStatusID = ''
  		IF TYPE(lcMasterFile+".STATUS") <>'U' OR TYPE(lcMasterFile+".cbatstat") <>'U' OR TYPE(lcMasterFile+".cinvstat") <>'U' OR TYPE(lcMasterFile+".CTRNSTAT") <>'U'
  		  lcStatusFld = IIF(TYPE(lcMasterFile+".STATUS") <>'U','STATUS',IIF(TYPE(lcMasterFile+".cbatstat") <>'U',"cbatstat",IIF(TYPE(lcMasterFile+".CTRNSTAT") <>'U','CTRNSTAT',"cinvstat")))
    		*lnEntStatusSel = oAriaApplication.RemoteCompanyData.execute("Select * from [EntityStatus]  WHERE [EntityType]= '"+;
  		                                                             EntitySetting.EntityTypeOID+"' AND SUBSTRING(ID,1,1)='"+EVALUATE(lcMasterFile+"."+lcStatusFld)+"'",'',;
                                                                  "EntityStatus","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
        llStatusFound = .F.
        SELECT EntityStatus
        LOCATE FOR EntityType= EntitySetting.EntityTypeOID AND SUBSTR(ID,1,1)=EVALUATE(lcMasterFile+"."+lcStatusFld)
        IF FOUND() &&lnEntStatusSel > 0
          *SELECT EntityStatus
          *LOCATE
          lcStatusID = EntityStatus.Oid
			llStatusFound = .T.
        ENDIF
      ENDIF  
*!*	      lnEntSelectEnt = oAriaApplication.RemoteCompanyData.execute("Select OID from ENTITY WHERE ID = '"+STRTRAN(EVALUATE(lcMasterFile+"."+IIF(',' $ EntitySetting.SearchField,SUBSTR(EntitySetting.SearchField,1,;
*!*	  		               ATC(',',EntitySetting.SearchField)-1),ALLTRIM(EntitySetting.SearchField))),"'","''")+"' AND [TYPE]='"+EntitySetting.EntityTypeOID+"'" ,'',;
*!*	                                                                  "EntityOID","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
*!*	    
  	  IF .T. && lnEntSelectEnt > 0 AND RECCOUNT('EntityOID')=0
  			lcInsertStat = "INSERT INTO ENTITY ([TYPE],[id],[description],[status],[StatusId],[extradata],[typeId],[Notes]) Values ('"+EntitySetting.EntityTypeOID+"','"+;
  			               STRTRAN(EVALUATE(lcMasterFile+"."+IIF(',' $ EntitySetting.SearchField,SUBSTR(EntitySetting.SearchField,1,;
  			               ATC(',',EntitySetting.SearchField)-1),ALLTRIM(EntitySetting.SearchField))),"'","''")+"','"+IIF(!EMPTY(ALLTRIM(EntitySetting.entitydescField)),;
  			               STRTRAN(EVALUATE(lcMasterFile+"."+ALLTRIM(EntitySetting.entitydescField)),"'","''"),'')+;
  			               "',"+IIF(llStatusFound ,"'"+lcStatusID+"'",'NULL') +",'"+ ;
  			               IIF(llStatusFound ,EntityStatus.Description,'')+"','"+lcDataXML+"','"+EntitySetting.entityname+"','"+;
  			               STRTRAN(lcNotes,"'","''")+"')"
  		
  			*
  			*lcInsertStat = STRTRAN(lcInsertStat ,"'","''")
  	    lnEntInserEnt = oAriaApplication.RemoteCompanyData.execute(lcInsertStat ,'',;
  	                                                                "Entity","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"),.T., @lnConnHandler) 
  	                                                                		               
  	    
  	 	 IF lnEntInserEnt > 0
  			  SQLCOMMIT(lnConnHandler)
  			ELSE
*  			  =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnEntInserEnt,.T.)
  			  SQLROLLBACK(lnConnHandler)
  			  RETURN
  			ENDIF
*!*	 			IF USED('EntityStatus')
*!*	  		  USE IN EntityStatus
*!*			  ENDIF	
    	ELSE
     
    	 lcUpdateStat = "UPDATE ENTITY SET [Notes] ='"+STRTRAN(lcNotes,"'","''")+"', [status]="+IIF(llStatusFound ,"'"+ALLTRIM(lcStatusID)+"'",'NULL')+;
    	                ",[StatusId]='"+IIF(llStatusFound ,ALLTRIM(EntityStatus.Description),'')+"',[extradata]='"+lcDataXML+"'  WHERE [TYPE]= '"+;
  		                                                             EntitySetting.EntityTypeOID+;
  		                                                             "' AND ID='"+;
  		                                                              STRTRAN(EVALUATE(lcMasterFile+"."+IIF(',' $ EntitySetting.SearchField,SUBSTR(EntitySetting.SearchField,1,;
  		               ATC(',',EntitySetting.SearchField)-1),ALLTRIM(EntitySetting.SearchField))),"'","''")+"'"
  		               
*!*	  			LOCAL lnConnHandler   ,lnEntInserEnt 
*!*	  			lnConnHandler = 0            
*!*	  			lnEntInserEnt = 0
  	    lnEntInserEnt = oAriaApplication.RemoteCompanyData.execute(lcUpdateStat ,'',;
  	                                                                "Entity","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"),.T., @lnConnHandler) 
  	                                                                		               
  	    
  	 	 IF lnEntInserEnt > 0
  			  SQLCOMMIT(lnConnHandler)
  			ELSE
*  			  =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnEntInserEnt,.T.)
  			  SQLROLLBACK(lnConnHandler)
  			  RETURN
  			ENDIF
*!*	  				IF USED('EntityStatus')
*!*	  		  USE IN EntityStatus
*!*			  ENDIF	
  		ENDIF  
      lnEntSelectEnt = oAriaApplication.RemoteCompanyData.execute("Select OID from ENTITY WHERE ID = '"+STRTRAN(EVALUATE(lcMasterFile+"."+IIF(',' $ EntitySetting.SearchField,SUBSTR(EntitySetting.SearchField,1,;
  		               ATC(',',EntitySetting.SearchField)-1),ALLTRIM(EntitySetting.SearchField))),"'","''")+"' AND [TYPE]='"+EntitySetting.EntityTypeOID+"'" ,'',;
                                                                  "EntityOID","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
    
  	  IF lnEntSelectEnt > 0 AND RECCOUNT('EntityOID') >0 AND !EMPTY(EntitySetting.EntityObjLink) AND !ISNULL(EntitySetting.EntityObjLink)
  	    lcEntID = EntityOID.OID
  	    IF !USED('OBJECTLINK')
  	      =gfOpenTable('OBJLINK','OBJLNKTY','SH','OBJECTLINK')
  	    ENDIF
  	    IF !USED('OBJECTS_LINK')
  	      =gfOpenTable('OBJECTS','OBJECTID','SH','OBJECTS_LINK')
  	    ENDIF
  	    SELECT(lcMasterFile)
  	    lcObjKey = EVALUATE(STRTRAN(EntitySetting.EntityObjLink,',','+'))
  	    IF gfseek(lcObjKey,'OBJECTLINK','OBJLNKTY' )
  	      SELECT 'OBJECTLINK'
  	      SCAN REST WHILE COBJLNKTYP+COBJLINK =lcObjKey 
  	        IF gfSeek(OBJECTLINK.cobject_id,'OBJECTS_LINK') AND FILE(ALLTRIM(OBJECTS_LINK.mimgpath))
  	          lnAttach = oAriaApplication.RemoteCompanyData.execute("Select OID from [Attachment] WHERE  [name]='"+ALLTRIM(OBJECTS_LINK.mimgpath)+"'" ,'',;
                                                                  "Attachment","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
            
  	          IF lnAttach > 0  AND RECCOUNT('Attachment') = 0
*!*	  		        	lnConnHandler = 0            
*!*	  		        	lnEntInserEnt = 0
  	            lnEntInserEnt = oAriaApplication.RemoteCompanyData.execute("INSERT INTO [Attachment]([name],[attachment]) VALUES ("+;
  	                                                 "'"+ALLTRIM(OBJECTS_LINK.mimgpath)+"',CAST('"+ STRCONV(FILETOSTR(ALLTRIM(OBJECTS_LINK.mimgpath)),15)+"' as varbinary(max)))" ,'',;
  	                                                                "Entity","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"),.T., @lnConnHandler) 
  	                                                                		               
  	    
  				 	   IF lnEntInserEnt > 0
  						    SQLCOMMIT(lnConnHandler)
  					  	ELSE
  						   * =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnEntInserEnt,.T.)
  						    SQLROLLBACK(lnConnHandler)
  						    SELECT 'OBJECTLINK'
  					  	  LOOP 
  					  	ENDIF 
  			  		ENDIF  	
              lnAttach = oAriaApplication.RemoteCompanyData.execute("Select OID from [Attachment] WHERE [name]='"+ALLTRIM(OBJECTS_LINK.mimgpath)+"'" ,'',;
                                                                  "Attachment","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
            
  	          IF lnAttach > 0  AND RECCOUNT('Attachment') > 0
*!*	  	            lnConnHandler   = 0
  	            lnAttachDel  = oAriaApplication.RemoteCompanyData.execute("Select * from [EntityAttachment] WHERE entity='"+lcEntID+"' and entityid= '"+EVALUATE(lcMasterFile+"."+IIF(',' $ EntitySetting.SearchField,SUBSTR(EntitySetting.SearchField,1,;
  			               ATC(',',EntitySetting.SearchField)-1),ALLTRIM(EntitySetting.SearchField)))+"' AND [Attachment]='"+Attachment.OID+"'" ,'',;
                                                                  "AttachmentDel","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
                IF lnAttachDel  > 0 AND RECCOUNT("AttachmentDel") = 0                                                 
  	            lnEntInserEnt = oAriaApplication.RemoteCompanyData.execute("INSERT INTO [EntityAttachment]([entity],[entityid],[Attachment],[AttachmentName]"+;
  	                           ",[IsDefault]) VALUES ('"+lcEntID+"','"+STRTRAN(EVALUATE(lcMasterFile+"."+IIF(',' $ EntitySetting.SearchField,SUBSTR(EntitySetting.SearchField,1,;
  			               ATC(',',EntitySetting.SearchField)-1),ALLTRIM(EntitySetting.SearchField))),"'","''")+"','"+Attachment.OID+"','"+OBJECTLINK.COBJLINK+"','"+IIF(OBJECTLINK.cdef_obj='D','1','0')+"')" ,'',;
  	                                                                "EntityATT","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"),.T., @lnConnHandler) 
  	    
      				 	 IF lnEntInserEnt > 0
      						  SQLCOMMIT(lnConnHandler)
      						ELSE
      						 * =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnEntInserEnt ,.T.)
      						  SQLROLLBACK(lnConnHandler)
      						  SELECT 'OBJECTLINK'
      						  LOOP 
      						ENDIF 
    						ENDIF                                                                
              ENDIF
            ENDIF
	        ENDSCAN 
	            *B612501,1 MMT 11/29/2021 Modify Aria5 Desktop to open Global search screen filtered by search value entered by the user in search textbox[Start]
	    ELSE
	      IF ALLTRIM(LOWER(EntitySetting.EntityName))=='style'
	        lcStyMajor = &lcMasterFile..cStyMajor
	        lcObjKey = EVALUATE(STRTRAN(STRTRAN(UPPER(EntitySetting.EntityObjLink),',','+'),'STYLE','CSTYMAJOR'))
	        IF gfseek(lcObjKey,'OBJECTLINK','OBJLNKTY' )
  	          SELECT 'OBJECTLINK'
  	          SCAN REST WHILE COBJLNKTYP+COBJLINK =lcObjKey 
  	            IF gfSeek(OBJECTLINK.cobject_id,'OBJECTS_LINK') AND FILE(ALLTRIM(OBJECTS_LINK.mimgpath))
  	              lnAttach = oAriaApplication.RemoteCompanyData.execute("Select OID from [Attachment] WHERE  [name]='"+ALLTRIM(OBJECTS_LINK.mimgpath)+"'" ,'',;
                                                                  "Attachment","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
            
		          IF lnAttach > 0  AND RECCOUNT('Attachment') = 0
      	            lnEntInserEnt = oAriaApplication.RemoteCompanyData.execute("INSERT INTO [Attachment]([name],[attachment]) VALUES ("+;
  	                                                 "'"+ALLTRIM(OBJECTS_LINK.mimgpath)+"',CAST('"+ STRCONV(FILETOSTR(ALLTRIM(OBJECTS_LINK.mimgpath)),15)+"' as varbinary(max)))" ,'',;
  	                                                                "Entity","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"),.T., @lnConnHandler) 
  	                                                                		               
  	    
  				 	   IF lnEntInserEnt > 0
  						    SQLCOMMIT(lnConnHandler)
  					  	ELSE
  						   * =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnEntInserEnt,.T.)
  						    SQLROLLBACK(lnConnHandler)
  						    SELECT 'OBJECTLINK'
  					  	  LOOP 
  					  	ENDIF 
  			  		ENDIF  	
              lnAttach = oAriaApplication.RemoteCompanyData.execute("Select OID from [Attachment] WHERE [name]='"+ALLTRIM(OBJECTS_LINK.mimgpath)+"'" ,'',;
                                                                  "Attachment","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
            
  	          IF lnAttach > 0  AND RECCOUNT('Attachment') > 0
*!*	  	            lnConnHandler   = 0
  	            lnAttachDel  = oAriaApplication.RemoteCompanyData.execute("Select * from [EntityAttachment] WHERE entity='"+lcEntID+"' and entityid= '"+EVALUATE(lcMasterFile+"."+IIF(',' $ EntitySetting.SearchField,SUBSTR(EntitySetting.SearchField,1,;
  			               ATC(',',EntitySetting.SearchField)-1),ALLTRIM(EntitySetting.SearchField)))+"' AND [Attachment]='"+Attachment.OID+"'" ,'',;
                                                                  "AttachmentDel","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"))
                IF lnAttachDel  > 0 AND RECCOUNT("AttachmentDel") = 0                                                 
  	            lnEntInserEnt = oAriaApplication.RemoteCompanyData.execute("INSERT INTO [EntityAttachment]([entity],[entityid],[Attachment],[AttachmentName]"+;
  	                           ",[IsDefault]) VALUES ('"+lcEntID+"','"+STRTRAN(EVALUATE(lcMasterFile+"."+IIF(',' $ EntitySetting.SearchField,SUBSTR(EntitySetting.SearchField,1,;
  			               ATC(',',EntitySetting.SearchField)-1),ALLTRIM(EntitySetting.SearchField))),"'","''")+"','"+Attachment.OID+"','"+OBJECTLINK.COBJLINK+"','"+IIF(OBJECTLINK.cdef_obj='D','1','0')+"')" ,'',;
  	                                                                "EntityATT","",oAriaApplication.activecompanyconstr,3,"",SET("Datasession"),.T., @lnConnHandler) 
  	    
      				 	 IF lnEntInserEnt > 0
      						  SQLCOMMIT(lnConnHandler)
      						ELSE
      						 * =oAriaApplication.RemoteCompanyData.CheckRetResult("Execute",lnEntInserEnt ,.T.)
      						  SQLROLLBACK(lnConnHandler)
      						  SELECT 'OBJECTLINK'
      						  LOOP 
      						ENDIF 
    						ENDIF                                                                
              ENDIF
            ENDIF
	        ENDSCAN 
	        ENDIF
	      ENDIF
	    *B612501,1 MMT 11/29/2021 Modify Aria5 Desktop to open Global search screen filtered by search value entered by the user in search textbox[End]
	      ENDIF
	    ENDIF
    ENDSCAN 
    IF USED(lcMasterFile)
     =gfCloseTable(lcMasterFile)
    ENDIF
 
    IF USED('OBJECTLINK')
     =gfCloseTable('OBJECTLINK')
    ENDIF
    IF USED('OBJECTS_LINK')
      =gfCloseTable('OBJECTS_LINK')
    ENDIF
  ENDSCAN 
  IF USED('NOTEPAD')
     =gfCLoseTable('NOTEPAD')
  ENDIF
ENDSCAN
oAriaApplication.activecompanyid = lcOldComp
oAriaApplication.GetCompanyInformation(lcOldComp)
USE IN 'SYCCOMP_A'

FUNCTION lfCodeSpChr
LPARAMETERS lvValue

IF TYPE('lvValue') = 'C' AND !EMPTY(lvValue)
  lvValue = STRTRAN(lvValue, '&', '&amp;')
  lvValue = STRTRAN(lvValue, '>', '&gt;')
  lvValue = STRTRAN(lvValue, '<', '&lt;')
  lvValue = STRTRAN(lvValue, '"', '&quot;')
  lvValue = STRTRAN(lvValue, "'", '&apos;')
ENDIF

RETURN lvValue