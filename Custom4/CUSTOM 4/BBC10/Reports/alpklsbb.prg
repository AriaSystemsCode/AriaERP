PUBLIC ARRAY laPckTempp[9]
Store '' To  lcTmpPck , lcTmpStyl

Function lfGetCustNotes
Lparameters lcAccnt
Store '' To lcCustNotes
=gfOpenTable('NOTEPAD','NOTEPAD','SH','lcTmpNOT')

If Type('lcAccnt')='C' .And. !Empty(lcAccnt) .And. GFSEEK("A"+Alltrim(lcAccnt),'lcTmpNOT','NOTEPAD')
	lcCustNotes= lcTmpNOT.mnotes
Endif

Return lcCustNotes


Function lfGetOrdtNotes
Lparameters lcOrd
Store '' To lcOrdNotes
=gfOpenTable('NOTEPAD','NOTEPAD','SH','lcTmpNOT')

If Type('lcOrd')='C' .And. !Empty(lcOrd) .And. GFSEEK("B"+Alltrim(lcOrd),'lcTmpNOT','NOTEPAD')
	lcOrdNotes= lcTmpNOT.mnotes
Endif

Return lcOrdNotes


Function lfGetSku
Lparameters lcPack_id , lcAcnt
lcTmpSPCK_HDR = gfTempName()
lcTmpSPCK_LIN= gfTempName()
lcSkuTmpl= gfTempName()
lcCustomer= gfTempName()
=gfOpenTable('SPCK_HDR','SKU_STYLE','SH',lcTmpSPCK_HDR)
=gfOpenTable('SPCK_LIN','SPCK_LIN','SH',lcTmpSPCK_LIN)
=gfOpenTable('SKUTMPL','SKUTMPL','SH',lcSkuTmpl)
=gfOpenTable('CUSTOMER','CUSTOMER','SH',lcCustomer)


lcAcnt = Alltrim(lcAcnt)
lcTmpPck = Alltrim(Getwordnum(lcPack_id,2,":"))

= GFSEEK("M"+lcAcnt,lcCustomer,'CUSTOMER')
lcSkuTmplet=Iif(!Empty(&lcCustomer..SkuTmpl),&lcCustomer..SkuTmpl,'DEF')
If Seek('S'+lcSkuTmplet,lcSkuTmpl,'SKUTMPL')
	lnDime1 = &lcSkuTmpl..Len1+&lcSkuTmpl..Len2+&lcSkuTmpl..Len3
	lnDime2 = &lcSkuTmpl..Len4
Else
	lnDime1 = 8  &&Default
	lnDime2 = 8  &&Default
Endif

If Type('lcTmpPck')='C' .And. !Empty(lcTmpPck) .And. GFSEEK("P"+lcAcnt+lcTmpPck,lcTmpSPCK_LIN,'SPCK_LIN')
	laPckTempp[1]= "Pack ID : "+&lcTmpSPCK_LIN..pack_id
	For x=2 To 9
		laPckTempp[x]=''
	Endfor
	Return
Else
	If Type('lcTmpPck')='C' .And. !Empty(lcTmpPck) .And. GFSEEK("S"+lcAcnt+lcTmpPck,lcTmpSPCK_LIN,'SPCK_LIN')
		lcTmpStyl = &lcTmpSPCK_LIN..Style
	Endif
Endif

If Type('lcTmpStyl')='C' .And. !Empty(lcTmpStyl) .And. GFSEEK("S"+lcAcnt+lcTmpStyl,lcTmpSPCK_HDR,'SKU_STYLE')
	laPckTempp[1] = Alltrim(&lcTmpSPCK_HDR..sku)

	lnDime1 = Min(lnDime1,Len(Alltrim(laPckTempp[1])))
	lcTmpAls= Alias()
	Select (lcTmpSPCK_LIN)
	Set Order To SPCKLINS
	= Seek('S'+lcAcnt+lcTmpStyl,lcTmpSPCK_LIN)
	Scan While Type+Account+Style = 'S'+lcAcnt+lcTmpStyl &&.AND. lnI < 9
		For lnX=2 To 9
			Z=Str(lnX-1,1)
			If QTY&Z > 0
				laPckTempp[lnX]=Substr(&lcTmpSPCK_LIN..pack_id,lnDime1+1,lnDime2)
				Exit
			Endif
		Endfor
* lnI = lnI + 1
	Endscan
	laPckTempp[1] ="SKU : "+laPckTempp[1]
	Select (lcTmpAls)
Else
	For x=1 To 9
		laPckTempp[x]=''
	Endfor

ENDIF


RETURN


