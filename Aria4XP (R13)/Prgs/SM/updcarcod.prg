*:************************************************************************
*:  Program File: \ARIA4XP\PRGS\SM\UPDCARCOD.Prg
*:  Module      : System Manager
*:  System      : Aria 4XP
*:  Developer   : ES - Esraa Ahmed
*:  Date        : 03/05/2020
*:************************************************************************
Parameters lcsyspath
SET STEP ON
lcclient_id=oariaapplication.readxml()
lcsql = "use [system.master] select carrier_id from CLIENTS_CARRIER_T WHERE client_id='"+lcclient_id+"'"
lnresult  = oariaapplication.remotesystemdata.sqlrun(lcsql ,'lcTmpCarrier','', oariaapplication.systemmasterconnectionstring, 3, 'SAVE', Set("Datasession"))

IF lnresult  >0 AND RECCOUNT('lcTmpCarrier') >0
	IF  !Used('SYCCOMP_X')
		SYCCOMP_X =gfTempName()
		USE (lcsyspath+"SYCCOMP")  AGAIN IN 0 ALIAS &SYCCOMP_X ORDER CCOMP_ID
	ENDIF

	IF FILE(oariaapplication.defaultpath+'CarrierCodes.xml')
		Xmltocursor(oariaapplication.defaultpath+'CarrierCodes.xml','LCTEMPXML',512)
		lnnum=0
		SELECT  lctmpcarrier
		lcfltexp =" UPPER(carrier_id) ='UPS' OR  UPPER(carrier_id) ='FEDEX' OR  UPPER(carrier_id) ='USPS'"
		SCAN  For &lcfltexp.
			SELECT  &syccomp_x
			SCAN
*Get DBFS Path
				lcDBFSPath =ALLTRIM(&syccomp_x..ccom_ddir)
*Get DBFS Path
				codes_x =gfTempName()
				USE (lcDBFSPath +'codes')  AGAIN IN 0 ALIAS &codes_x ORDER CCODE_NO
				SELECT  lctempxml
				lcfltexp =" ALLTRIM(UPPER(CarrierID)) =ALLTRIM(UPPER(lcTmpCarrier.carrier_id)) "
				SCAN  For &lcfltexp.
					lcname=Iif(Upper(Alltrim(cfld_name))='S','CUPS','CCRTNVLTYP')
					SELECT  &codes_x
					IF  Upper(lctmpcarrier.carrier_id)='UPS'
						IF   gfseek('N'+PADR(ALLTRIM(lcname),10)+PADR(ALLTRIM(lctempxml.ccode_no),6),Codes_X)
							Replace &codes_x..cdiscrep WITH lctempxml.cdiscrep;
							&codes_x..cedit_user WITH  'AriaCarUPD'
						ELSE
							APPEND BLANK
							Replace cdefcode With 'N';
							lrltfields WITH .F. ;
							crltfield WITH 'N';
							cfld_name With lcname ;
							ccode_no With lctempxml.ccode_no ;
							cdiscrep With lctempxml.cdiscrep;
							dadd_date With oariaapplication.systemdate;
							cAdd_user  WITH  'AriaCarUPD'
						ENDIF
					ELSE
						IF gfseek('N'+PADR(ALLTRIM(lcname),10),Codes_X)
*Locate Rest While UPPER(cdiscrep)=UPPER(lctempxml.cdiscrep)
							Locate Rest While CDEFCODE+CFLD_NAME+CCODE_NO+CDISCREP+CRLTD_NAM = 'N'+PADR(ALLTRIM(lcname),10) FOR ALLTRIM(UPPER(cdiscrep))=ALLTRIM(UPPER(lctempxml.cdiscrep))
							Replace &codes_x..cdiscrep WITH lctempxml.cdiscrep;
							&codes_x..cedit_user WITH  'AriaCarUPD'
						ELSE
							lccod=Iif(Alltrim(Upper(cfld_name))='S','Fdx_','Pac_')
							Do While .T.
								lcfoundstring=lccod+Alltrim(Str(lnnum))
								Locate Rest While cdefcode+cfld_name+ccode_no+cdiscrep+crltd_nam = "N"+PADR(ALLTRIM(lcname),10) For Upper(Alltrim(ccode_no)) = Upper(Alltrim(lcfoundstring))
								IF !Found()
									Exit
								ENDIF
								lnnum=lnnum+1
							ENDDO
							APPEND BLANK
							Replace cdefcode With 'N' ;
							lrltfields WITH .F. ;
							crltfield WITH 'N';
							cfld_name With lcname ;
							ccode_no With lcfoundstring;
							cdiscrep With lctempxml.cdiscrep;
							dadd_date With oariaapplication.systemdate;
							cAdd_user  WITH  'AriaCarUPD'

						ENDIF
					ENDIF
				ENDSCAN
*!*				SELECT &Codes_X
*!*					=gftableupdate('Codes_X')
				IF  Used(Codes_X)
					SELECT &Codes_X
					USE
				ENDIF
			ENDSCAN
		ENDSCAN
	ENDIF
ENDIF