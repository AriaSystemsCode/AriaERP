  �   !                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              �DRIVER=winspool
DEVICE=HP LaserJet 2100
OUTPUT=LPT2:,IP_192.168.2.201
ORIENTATION=0
PAPERSIZE=259
ASCII=0
COPIES=1
DEFAULTSOURCE=256
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=1
COLLATE=1
                     L  $  winspool  HP LaserJet 2100  LPT2:,IP_192.168.2.201                                      �HP LaserJet 2100                 � 4C� �4d   ,  ,  A4                                                            ����                DINU"   4  L5a                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Courier New                    ldet='1'                       detail11                                                      Times New Roman                ldet='1'                       detail21                                                      Times New Roman                ldet='1'                       HEADER3                        y "CARTON# : "+'000'+ALLTRIM(MANUF_ID)+' '+ALLTRIM(UCC9)+' '+ALLTRIM(UCC_CHECK)                                                    Times New Roman                ldet='1'                       HEADER4                        y "CUST PO: "+ALLTRIM(CUSTPO)                                                                                                      Times New Roman                ldet='1'                                                      ldet='1'                                                      ldet='1'                       HEADER1                                                       Times New Roman                ldet='1'                       HEADER2                                                       Times New Roman                ldet='1'                       detail12                                                      Times New Roman                ldet='1'                       detail13                                                      Times New Roman                ldet='1'                       detail14                                                      Times New Roman                ldet='1'                       detail15                                                      Times New Roman                ldet='1'                       detail16                                                      Times New Roman                ldet='1'                       detail17                                                      Times New Roman                ldet='1'                       detail18                                                      Times New Roman                ldet='1'                       detail19                                                      Times New Roman                ldet='1'                       	detail110                                                     Times New Roman                ldet='1'                       	detail111                                                     Times New Roman                ldet='1'                       	detail112                                                     Times New Roman                ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                       detail22                                                      Times New Roman                ldet='1'                       detail23                                                      Times New Roman                ldet='1'                       detail24                                                      Times New Roman                ldet='1'                       detail25                                                      Times New Roman                ldet='1'                       detail26                                                      Times New Roman                ldet='1'                       detail27                                                      Times New Roman                ldet='1'                       detail28                                                      Times New Roman                ldet='1'                       detail29                                                      Times New Roman                ldet='1'                       	detail210                                                     Times New Roman                ldet='1'                       	detail211                                                     Times New Roman                ldet='1'                       	detail212                                                     Times New Roman                ldet='1'                       footer1                                                       Times New Roman                lIIF(TYPE('LPCRTBOL')='L' AND LPCRTBOL = .T., "CARTON# : "+STR(CART_NO_B,4)+' OF '+STR(TOT_CART,4) , footer2)                      ""                             Times New Roman                ldet='1'                       "carton contents "                                            Times New Roman                ldet='1'                       Courier New                    Times New Roman                Times New Roman                Times New Roman                dataenvironment                _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
                                  
@PROCEDURE Init
  FOR lnItemRow = 1 TO MIN(20 , ALEN(laLblInfo , 1))

      lladdrec          = .F.
      IF Empty(laLblInfo[lnItemRow,3]) and Mod(laLblInfo[lnItemRow,2],1)= 0
         lnLinesCounter = lnLinesCounter+1
         lcLineno		    = ALLTRIM(STR(lnLinesCounter)) 
         MDSKU&lcLineno     = laLblInfo[lnItemRow,4]
         MDQty&lcLineno     = laLblInfo[lnItemRow,2]
         MDSizDes&lcLineno  = laLblInfo[lnItemRow,3]
         MDStyle&lcLineno   = laLblInfo[lnItemRow,4]
         lcPackid  			= laLblInfo[lnItemRow,4]
         MDPACK&lcLineno   			= laLblInfo[lnItemRow,4]
         lladdrec = .T.
         llPackstyles = .F.
      ELSE
         IF Empty(laLblInfo[lnItemRow,3]) and Mod(laLblInfo[lnItemRow,2],1)# 0
            llPackstyles = .T.
            LOOP
         ENDIF
      Endif
      IF (EMPTY(laLblInfo[lnItemRow,9]) OR laLblInfo[lnItemRow,9] # lcPackid or llPackstyles)  and !lladdrec 
         lnLinesCounter = lnLinesCounter+1
         lcLineno		    = ALLTRIM(STR(lnLinesCounter)) 
         MDSKU&lcLineno     = laLblInfo[lnItemRow,1]
         MDQty&lcLineno     = laLblInfo[lnItemRow,2]
         MDSizDes&lcLineno  = laLblInfo[lnItemRow,3]
         MDStyle&lcLineno   = laLblInfo[lnItemRow,4]
         lladdrec = .T.
      ENDIF
      
*!*	      IF lladdrec 
*!*	         lnLinesCounter = lnLinesCounter+1 
*!*	      ENDIF
      IF ALEN(laLblInfo,2) => 8
         INPACKQTY&lcLineno  = laLblInfo[lnItemRow,8]
      ENDIF 
      MDSTYMAJ&lcLineno  = LEFT(laLblInfo[lnItemRow,4] , lnMajorLen)
      IF llUseColor
         MDCOLOR&lcLineno = SUBSTR(laLblInfo[lnItemRow,4] , lnColorPos , lnColorLen)
      ENDIF
      MDSTYUPC&lcLineno  = laLblInfo[lnItemRow, 5]
      MKDSTYUPC&lcLineno = IIF(!EMPTY(ALLTRIM(MDSTYUPC&lcLineno)),PADL(ALLTRIM(MDSTYUPC&lcLineno),14,'0'),' ')
      IF ALEN(laLblInfo, 2) => 7
         MDPCS&lcLineno  = laLblInfo[lnItemRow, 7]
         MDPQTY&lcLineno   = laLblInfo[lnItemRow,7]
      ENDIF
*!*	      IF lnCount = 1 
*!*	         ShortSKU = LFREAD(7,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU1,'Detail','850','PO1')
*!*	         LongSku  = LFREAD(9,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU1,'Detail','850','PO1')
*!*	         ITEMDESC = LFREAD(2,ALLTRIM(Asn_Ship.CUSTPO),'Header','850','PO1')
*!*	      Else
*!*	         ShortSKU = IIF(ShortSku = LFREAD(7,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU&lcLineno,'Detail','850','PO1'),ShortSku,"PREPACK")
*!*	         LongSku  = IIF(LongSku  = LFREAD(9,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU&lcLineno,'Detail','850','PO1'),LongSku ,Left(LongSku,Len(LongSku)-3) +"***")
*!*	      EndIf  
  ENDFOR

ENDPROC
             g���    N  N                        �_   %   �        ,   �          �  U  " ��  ���(�C�C�� ��F��
� T� �-��/ %�CC �  �� �� CC �  �� �G� 	���� T� �� ��� T� �CC� Z���/ MDSKU&lcLineno     = laLblInfo[lnItemRow,4]
/ MDQty&lcLineno     = laLblInfo[lnItemRow,2]
/ MDSizDes&lcLineno  = laLblInfo[lnItemRow,3]
/ MDStyle&lcLineno   = laLblInfo[lnItemRow,4]
 T� �C �  �� ��1 MDPACK&lcLineno   			= laLblInfo[lnItemRow,4]
 T� �a�� T� �-�� ���/ %�CC �  �� �� CC �  �� �G� 	���� T� �a�� .� � �: %�CC �  �	� �� C �  �	� � � � � � 
	��� T� �� ��� T� �CC� Z���/ MDSKU&lcLineno     = laLblInfo[lnItemRow,1]
/ MDQty&lcLineno     = laLblInfo[lnItemRow,2]
/ MDSizDes&lcLineno  = laLblInfo[lnItemRow,3]
/ MDStyle&lcLineno   = laLblInfo[lnItemRow,4]
 T� �a�� � %�C�� �����R�0 INPACKQTY&lcLineno  = laLblInfo[lnItemRow,8]
 �B MDSTYMAJ&lcLineno  = LEFT(laLblInfo[lnItemRow,4] , lnMajorLen)
 %�� ����O MDCOLOR&lcLineno = SUBSTR(laLblInfo[lnItemRow,4] , lnColorPos , lnColorLen)
 �0 MDSTYUPC&lcLineno  = laLblInfo[lnItemRow, 5]
l MKDSTYUPC&lcLineno = IIF(!EMPTY(ALLTRIM(MDSTYUPC&lcLineno)),PADL(ALLTRIM(MDSTYUPC&lcLineno),14,'0'),' ')
 %�C�� ������- MDPCS&lcLineno  = laLblInfo[lnItemRow, 7]
. MDPQTY&lcLineno   = laLblInfo[lnItemRow,7]
 � �� U 	 LNITEMROW	 LALBLINFO LLADDREC LNLINESCOUNTER LCLINENO LCPACKID LLPACKSTYLES
 LLUSECOLOR Init,     ��1 !� �����Q� � � �� A A A ������ A uA !� �A �q��A I 2                       5
      )   N                           �DRIVER=winspool
DEVICE=HP LaserJet 2100
OUTPUT=LPT2:,IP_192.168.2.201
ORIENTATION=0
PAPERSIZE=259
ASCII=0
COPIES=1
DEFAULTSOURCE=256
PRINTQUALITY=300
COLOR=1
YRESOLUTION=300
TTOPTION=1
COLLATE=1
                     L  $  winspool  HP LaserJet 2100  LPT2:,IP_192.168.2.201                                      �HP LaserJet 2100                 � 4C� �4d   ,  ,  A4                                                            ����                DINU"   4  L5a                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Courier New                    ldet='1'                       detail11                                                      Times New Roman                ldet='1'                       detail21                                                      Times New Roman                ldet='1'                       HEADER3                        y "CARTON# : "+'000'+ALLTRIM(MANUF_ID)+' '+ALLTRIM(UCC9)+' '+ALLTRIM(UCC_CHECK)                                                    Times New Roman                ldet='1'                       HEADER4                        y "CUST PO: "+ALLTRIM(CUSTPO)                                                                                                      Times New Roman                ldet='1'                                                      ldet='1'                                                      ldet='1'                       HEADER1                                                       Times New Roman                ldet='1'                       HEADER2                                                       Times New Roman                ldet='1'                       detail12                                                      Times New Roman                ldet='1'                       detail13                                                      Times New Roman                ldet='1'                       detail14                                                      Times New Roman                ldet='1'                       detail15                                                      Times New Roman                ldet='1'                       detail16                                                      Times New Roman                ldet='1'                       detail17                                                      Times New Roman                ldet='1'                       detail18                                                      Times New Roman                ldet='1'                       detail19                                                      Times New Roman                ldet='1'                       	detail110                                                     Times New Roman                ldet='1'                       	detail111                                                     Times New Roman                ldet='1'                       	detail112                                                     Times New Roman                ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                                                      ldet='1'                       detail22                                                      Times New Roman                ldet='1'                       detail23                                                      Times New Roman                ldet='1'                       detail24                                                      Times New Roman                ldet='1'                       detail25                                                      Times New Roman                ldet='1'                       detail26                                                      Times New Roman                ldet='1'                       detail27                                                      Times New Roman                ldet='1'                       detail28                                                      Times New Roman                ldet='1'                       detail29                                                      Times New Roman                ldet='1'                       	detail210                                                     Times New Roman                ldet='1'                       	detail211                                                     Times New Roman                ldet='1'                       	detail212                                                     Times New Roman                ldet='1'                       footer1                                                       Times New Roman                lIIF(TYPE('LPCRTBOL')='L' AND LPCRTBOL = .T., "CARTON# : "+STR(CART_NO_B,4)+' OF '+STR(TOT_CART,4) , footer2)                      ""                             Times New Roman                ldet='1'                       "carton contents "                                            Times New Roman                ldet='1'                       Courier New                    Times New Roman                Times New Roman                Times New Roman                dataenvironment                _Top = 220
Left = 1
Width = 520
Height = 200
DataSource = .NULL.
Name = "Dataenvironment"
                                  
@PROCEDURE Init
  FOR lnItemRow = 1 TO MIN(20 , ALEN(laLblInfo , 1))

      lladdrec          = .F.
      IF Empty(laLblInfo[lnItemRow,3]) and Mod(laLblInfo[lnItemRow,2],1)= 0
         lnLinesCounter = lnLinesCounter+1
         lcLineno		    = ALLTRIM(STR(lnLinesCounter)) 
         MDSKU&lcLineno     = laLblInfo[lnItemRow,4]
         MDQty&lcLineno     = laLblInfo[lnItemRow,2]
         MDSizDes&lcLineno  = laLblInfo[lnItemRow,3]
         MDStyle&lcLineno   = laLblInfo[lnItemRow,4]
         lcPackid  			= laLblInfo[lnItemRow,4]
         MDPACK&lcLineno   			= laLblInfo[lnItemRow,4]
         lladdrec = .T.
         llPackstyles = .F.
      ELSE
         IF Empty(laLblInfo[lnItemRow,3]) and Mod(laLblInfo[lnItemRow,2],1)# 0
            llPackstyles = .T.
            LOOP
         ENDIF
      Endif
      IF (EMPTY(laLblInfo[lnItemRow,9]) OR laLblInfo[lnItemRow,9] # lcPackid or llPackstyles)  and !lladdrec 
         lnLinesCounter = lnLinesCounter+1
         lcLineno		    = ALLTRIM(STR(lnLinesCounter)) 
         MDSKU&lcLineno     = laLblInfo[lnItemRow,1]
         MDQty&lcLineno     = laLblInfo[lnItemRow,2]
         MDSizDes&lcLineno  = laLblInfo[lnItemRow,3]
         MDStyle&lcLineno   = laLblInfo[lnItemRow,4]
         lladdrec = .T.
      ENDIF
      
*!*	      IF lladdrec 
*!*	         lnLinesCounter = lnLinesCounter+1 
*!*	      ENDIF
      IF ALEN(laLblInfo,2) => 8
         INPACKQTY&lcLineno  = laLblInfo[lnItemRow,8]
      ENDIF 
      MDSTYMAJ&lcLineno  = LEFT(laLblInfo[lnItemRow,4] , lnMajorLen)
      IF llUseColor
         MDCOLOR&lcLineno = SUBSTR(laLblInfo[lnItemRow,4] , lnColorPos , lnColorLen)
      ENDIF
      MDSTYUPC&lcLineno  = laLblInfo[lnItemRow, 5]
      MKDSTYUPC&lcLineno = IIF(!EMPTY(ALLTRIM(MDSTYUPC&lcLineno)),PADL(ALLTRIM(MDSTYUPC&lcLineno),14,'0'),' ')
      IF ALEN(laLblInfo, 2) => 7
         MDPCS&lcLineno  = laLblInfo[lnItemRow, 7]
         MDPQTY&lcLineno   = laLblInfo[lnItemRow,7]
      ENDIF
*!*	      IF lnCount = 1 
*!*	         ShortSKU = LFREAD(7,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU1,'Detail','850','PO1')
*!*	         LongSku  = LFREAD(9,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU1,'Detail','850','PO1')
*!*	         ITEMDESC = LFREAD(2,ALLTRIM(Asn_Ship.CUSTPO),'Header','850','PO1')
*!*	      Else
*!*	         ShortSKU = IIF(ShortSku = LFREAD(7,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU&lcLineno,'Detail','850','PO1'),ShortSku,"PREPACK")
*!*	         LongSku  = IIF(LongSku  = LFREAD(9,ALLTRIM(Asn_Ship.CUSTPO)+MDSKU&lcLineno,'Detail','850','PO1'),LongSku ,Left(LongSku,Len(LongSku)-3) +"***")
*!*	      EndIf  
  ENDFOR

ENDPROC
             g���    N  N                        �_   %   �        ,   �          �  U  " ��  ���(�C�C�� ��F��
� T� �-��/ %�CC �  �� �� CC �  �� �G� 	���� T� �� ��� T� �CC� Z���/ MDSKU&lcLineno     = laLblInfo[lnItemRow,4]
/ MDQty&lcLineno     = laLblInfo[lnItemRow,2]
/ MDSizDes&lcLineno  = laLblInfo[lnItemRow,3]
/ MDStyle&lcLineno   = laLblInfo[lnItemRow,4]
 T� �C �  �� ��1 MDPACK&lcLineno   			= laLblInfo[lnItemRow,4]
 T� �a�� T� �-�� ���/ %�CC �  �� �� CC �  �� �G� 	���� T� �a�� .� � �: %�CC �  �	� �� C �  �	� � � � � � 
	��� T� �� ��� T� �CC� Z���/ MDSKU&lcLineno     = laLblInfo[lnItemRow,1]
/ MDQty&lcLineno     = laLblInfo[lnItemRow,2]
/ MDSizDes&lcLineno  = laLblInfo[lnItemRow,3]
/ MDStyle&lcLineno   = laLblInfo[lnItemRow,4]
 T� �a�� � %�C�� �����R�0 INPACKQTY&lcLineno  = laLblInfo[lnItemRow,8]
 �B MDSTYMAJ&lcLineno  = LEFT(laLblInfo[lnItemRow,4] , lnMajorLen)
 %�� ����O MDCOLOR&lcLineno = SUBSTR(laLblInfo[lnItemRow,4] , lnColorPos , lnColorLen)
 �0 MDSTYUPC&lcLineno  = laLblInfo[lnItemRow, 5]
l MKDSTYUPC&lcLineno = IIF(!EMPTY(ALLTRIM(MDSTYUPC&lcLineno)),PADL(ALLTRIM(MDSTYUPC&lcLineno),14,'0'),' ')
 %�C�� ������- MDPCS&lcLineno  = laLblInfo[lnItemRow, 7]
. MDPQTY&lcLineno   = laLblInfo[lnItemRow,7]
 � �� U 	 LNITEMROW	 LALBLINFO LLADDREC LNLINESCOUNTER LCLINENO LCPACKID LLPACKSTYLES
 LLUSECOLOR Init,     ��1 !� �����Q� � � �� A A A ������ A uA !� �A �q��A I 2                       5
      )   N                     