����  �k  �j  %                    �|    I  �  �   �  6M&:     4�  � � � T� �F�	 MEMOWIDTHz�� G$(����� 5� � %�F� \�  �� ���� T� �F� FULLPATHz�� GQ �� %�� � F� �  � .FRX2	� � � F� �  � .LBX2	�# � � F� � � \�  � .LBX2	�# � � F� � � \�  � .FRX2	��H�O T�  �FF� �  � .FRX2� F� �  � .LBX2�
 � �  � � � � \�  8�� ���O T�  �FF�	 �  � .FRX2� F�	 �  � .LBX2�
 �	 �  � �	 � � \�  8�� � SET FULL &lcFullSet
 �4 T� �FF� \�  �� � F�  F� \�  ��`� �  8�� %�F�
 � � .FRX2��[�& ERASE &gcWorkDir.&lcOGTmpForm..FRX
( ERASE &gcWorkDir.&lcOGTmpForm..FRT  
 � %�F�
 � � .LBX2����& ERASE &gcWorkDir.&lcOGTmpForm..LBX
( ERASE &gcWorkDir.&lcOGTmpForm..LBT  
 �, %�FF�  �� .FRX2� FF�  �� .LBX2��� %�FF�  �� .FRX2����t SELECT * FROM &lcFormName..FRX WHERE PLATFORM=lcOGPlatForm AND objtype<>10 INTO DBF &gcWorkDir.&lcOGTmpForm..FRX
 ��t SELECT * FROM &lcFormName..LBX WHERE PLATFORM=lcOGPlatForm AND objtype<>10 INTO DBF &gcWorkDir.&lcOGTmpForm..LBX
 � �"� J��  �(� � � B� � %�� ����- �F� 
� F� ����	� F� � ��	��O >� ��F� �� ��F� �� ��F� �� ��F� ��F� ����� F� � ��	�� 3� � %�F� ����� Q�� �� � %�F� ����� Q�� �� � G$(�� �� U LCFORMNAMELCARRAYNAMLLCHKCHANGLNMEMOWID LCFILENAMELCFULLSET GCMULTIINSGCCREPHOMEGCACT_APPLGCREPHOME GCWORKDIR LCOGTMPFORLCOGFORMV LFCANDISP OBJTYPE   OBJCODE   VPOS      LFOGVPOS  HPOS      LFOGHPOS  HEIGHT    LFOGHSIZE WIDTH     LFOGWSIZE FOR       �  4�  � T� �F�� �  ���) T� �F� � � F�� � ��� � 8��? RETURN IIF(lnArrPos=0,'',ALLTRIM(&lcArrayName[lnArrPos,6]))
 U LNRECNO MELNARRPOS MLAOGOBJPOS�  4�  � T�� �� ��! %�F� #VPOSITION � �� ��� �" T� �F� F� #VPOSITION � ΐ��( T� �FF� F� #VPOSITION � ��
`��� T�� �F� ��� � B��� �� U LNOBJPOS EYPOS POS MVPOS BJPOSCOMMENT  SLCCLAUS MELCSIZE  T �  4�  � T�� �� ��! %�F� #HPOSITION � �� ��� �" T� �F� F� #HPOSITION � ΐ��( T� �FF� F� #HPOSITION � ��
`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EXPOS POS MHPOS BJPOSCOMMENT  SLCCLAUS MELCSIZE  T LCOGPLATFOFONTFACE OFONTSIZE L�  4�  � T�� �� �� T� �F� #HSIZE � ��� %�� � ��� � T� �F� F� #HSIZE � ΐ��$ T� �FF� F� #HSIZE � ��`��� T�� �F� ��� � B��� �� U LNOBJPOS EHEIGHT S MLNCLAUS OSCOMMENT  SLCCLAUS MELCSIZE  T �  4�  � T�� �� �� T� �F� #WSIZE � ��� %�� � ��� � T� �F� F� #WSIZE � ΐ��$ T� �FF� F� #WSIZE � ��`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EWIDTH  S MLNCLAUS OSCOMMENT  SLCCLAUS MELCSIZE  T LCOGPLATFOFONTFACE OFONTSIZE L| T�  ��  �� T� �e�� %�F� �
��� � %�F� #OBJDISP� �� ��� �G T�  �FF� F� #OBJDISP� ΐF� #OBJDISPF� F� #OBJDISP� ΐ��	`�� � � %�F�  �
��l� T� �F� #OBJDISPF� j��� �� ���(�� ��h�$ T�  �FF� F� #OBJDISP� � �`���G T�  �FF�  F� #OBJDISP�  ΐF� #OBJDISPF�  F� #OBJDISP�  ΐ��	`�� T� �F�  ��� %�� 
��d� !� � �� �	 B�� �� U LCCONDITIOLLCANDISP COMMENT OSLNNOCOND OLNCOUNT ME( 4�  � 5� %�F� (�  �� ��2 � B�F� ,�  ��� �� T� �� �� T� �� �� �� ���(�F�  @��� ��z �� �F�  � �`� (��� � T� �� ��� �F�  � �`� )��� � T� �� ��� �F�  � �`� ,��� %�� � ��� T� �� �� !� � � �� �	 B�� �� U LCSTRING OLNCOUTS   LNCOMPOS SLNCOUNT  OLFMODISTYL   LFOGVPOS   �  LFOGHPOS   �  LFOGHSIZE  	  LFOGWSIZE  �	  LFCANDISP  C  LFGETCOM   �  � �� s qqa ��� �B tA A�a�A �a�A ��C� CA � B A � ��D I � � A � � A � 8 q A��: q � !�A � 8 q � !��A � 8 q � ��AA � 8 q � ��A�A � 8 � � �rA A �qAr� � A A A A � 8 q Q q� � � � �� ���� A A A A A � 1             .   �  �   3   �  �   <   w  �   F   �  �   P   �  �   [   }   �   o   $        �.  �2  �  �.  -M&:>   � 4�  � � � � �- T� �FF�	 lcRprtLblf� UL� � R� � 8�� G*(� G(&�, T� �FF� llPrntToFilef� L� � � /8��% %�� � �2	� � � PRINTER	��� � T� �/�� �6 T� �FF�
 lcCriteriaf� C� F� �
	� � � �  8�� 5� � � �	 �
 � T�	 �/��: T�  �FF� lcOGTmpFormf� C� F� �� �  � � � 8�� T�
 �F� DEFAULTz�� %�F� \�  �� ���� T� �F� FULLPATHz�� GQ �� %�� � F� �  � .FRX2	� � � F� �  � .LBX2	�# � � F� � � \�  � .FRX2	�# � � F� � � \�  � .LBX2	��h�O T�  �FF� �  � .FRX2� F� �  � .LBX2�
 � �  � � � � \�  8�� ���O T�  �FF� �  � .FRX2� F� �  � .LBX2�
 � �  � � � � \�  8�� � SET FULL &lcFullSet
 � J��  �(� � � T� �FFRFQ� � � FR8�� T� �F�
 lcCriteria� ��& T� �F� WHILE � �F� FOR � ��� %�F� WHILE F� j�� ����@ T� �F� � F� �F� FOR � ��`� F� F� WHILE � �`8�� � %�F� FOR F� j�� ���T T� �F� � F� F� FOR � ��`�* F� F� FOR � ��F� � �F� FOR � �`8�� � T� �FQ� �� %�F� �
� F� �
���� %�F� ���`� #)� � T� �� �  � �� LOCATE &lcLoctCond
 T� �F6�� � %�� � ����	 #�� �� � %�� 
����$ ��F� INM00052B00000� DIALOG� �� B� � T� �F� ERROR��� {�  �@ T� �F�2� � � PRINTER	� � lfChckPrtStat() AND � �  8�� ��b��� �� � PRINTER��Q� %�� ����u T� �� \\SPOOLER\P=FF� ^�� \S=F�  �� \Q=F�! �F��� � \C=FF�^�� �  8F�" � � \B=�# � � \N8�� �� T� ��$ �� � %��2��?� SET PRINTER TO &lcPrintSet
 �M� G*(� �& �� � SCREEN� � � FILE���� SET PRINTER TO &gcOutFile
 � T�% �F� CURSORz�� G]� %��2� � 
	��
�* %�� � PRINTER� F�a� READY	��@� %��� BOTH,BEFORE��<� T�& ���� T��� FORMFEED�� � T���& �� � � %�� � SCREEN��� T� ��(�� T�	��  �� T�(��  �� � 5� � �' � T� �FQ�� T� ����RlcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND &lcPrntError IIF(INKEY()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1)])+ ' '+lcOGFOR
 T����� T�( �F��a�� T�) �� ��, s,��( �������(������������ t,���( �� z,��( ���0 �� ������� Printing Page 1���������5 ��������� Press [ESC] to abort��������� t,)��( �� %��* ���
� %�� � L��;
�A LABEL FORM (lcOGRprtNam) TO PRINTER NOCONSOLE &lcCriteria    
 ��
�G REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
 � �<� %�� � L���
�7 LABEL FORM (lcOGRprtNam) TO PRINTER &lcCriteria    
 �8�Q REPORT FORM (lcOGRprtNam) TO PRINTER NOCONSOLE PLAIN NOEJECT  &lcCriteria    
 � � <,��( �� %�� � SCREEN��n� T�(�� �� �* %�� � PRINTER� F�a� READY	���� %���
 BOTH,AFTER���� T�& ���� T��� FORMFEED�� � T���& �� � � %�� 
���
 ��F�+ �� � �g�F T� �FF� ��	 � ALL� � 8�  FF� �� �  � � �  8�  � �� ��e�c� �� � SCREEN����% %��, � WINDOW� �, � MAC���� %��* ��+� %�� � L����/ LABEL FORM &lcOGRprtNam PREVIEW &lcCriteria
 �'�0 REPORT FORM &lcOGRprtNam PREVIEW &lcCriteria
 � ��� %�� � L����> LABEL  FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
 ���> REPORT FORM &lcOGRprtNam  PREVIEW PLAIN &lcCriteria       
 � � ��� T� ��(�� T�	��  �� T�(��  ��LlcCriteria=IIF(EMPTY(lcOGScope),'ALL',lcOGScope)+' '+IIF(EMPTY(lcOGWhile),[ WHILE &lcPrntError IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ],lcOGWhile+[ AND IIF(inkey()<>27,IIF(_PAGENO<>lnPPage,lfdispPage(),.T.),gfModalGen("QRM00114B00023","ALERT") = 1) ])+ ' '+lcOGFor      
 T����� T�( �F��a�� T�) �� ��] s,��( �������(������@�� MS SANS SERIF����A�� B�B����������������� t,���( �� z,��( ���0 ��������� Printing Page 1���������5 ��������� Press [ESC] to abort��������� t,)��( �� %��* ��� %�� � L����< LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria
 ��F REPORT FORM &lcOGRprtNam TO PRINTER NOCONSOLE NOEJECT  &lcCriteria
 � ��� %�� � L��Q�2 LABEL FORM &lcOGRprtNam TO PRINTER &lcCriteria
 ���B REPORT FORM &lcOGRprtNam TO PRINTER PLAIN NOEJECT  &lcCriteria
 � � <,��( �� %�� 
����
 ��F�+ �� � T�(�� �� � �� � PRINTER����J T�- �F�5� F� ARIAPRN7�* FF� ARIAPRN7� LIB� �  � F� ARIAPRN788��# %��, � DOS� �, � UNIX���� T�. �� F�/ � .TXT�� G*(��. �� %��� BOTH,BEFORE���� T�& ���� T��� FORMFEED�� � T���& �� � v�G� %��* ���� %�� � L��8�= LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
 ���G REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
 � �C� %�� � L����A LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
 �?�Q REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
 � � w� %��� BOTH,BEFORE���� T�& ���� T��� FORMFEED�� � T���& �� � G*(� G*(��$ �� T�0 �/�� T�1 �F� ERROR��� 1� llError=.T.� T�2 �F� SAFEz�� G.� T�3 ��(�� T�(��  �� +�F�a� OFFLINE��i�+ %�F� QRM00054B00015� ALERT� ���e� T�0 �e�� !� � � %�F�. 2� �0 /	��6� T�4 ��$ �� ����2�- ��- � ???�
 �- � ??�	 �- � ?��A� T�5 �F�/ �� T�6 �FZ��' CREATE CURS &lcTmpCur (mprnfile M) 
 � �7 ��. �� G*(� %�� ����u T� �� \\SPOOLER\P=FF� ^�� \S=F�  �� \Q=F�! �F��� � \C=FF�^�� �  8F�" � � \B=�# � � \N8��& SET PRINTER TO &lcPrintSet        
 ��� G*(��4 �� � G* � T�8 �F� MEMOWz�� T�9 �F� CONSz�� T�: ���� T�; ���� T�< ���� T�= ���� G
� G$(��A��
 T��e�� T������ T��� �� T����� ��> ���(������( &lcPrntTool MPRNFILE FUNCTION 'V253'
 �� G*� G*(� SET CONS &lcConsSet
 G$(��8 �� T���: �� T���; �� T���< �� T���= �� USE IN &lcTmpCur

 F��6 �� ��- � COPY���� G*(��4 �� ��> ���(������ !COPY &gcOutFile &gcLoclPRT
 �� ��- � LIB����. %�F� TXTSPOOL.FLL2� F�
 PRTWIN.EXE2	���� Gb(� TXTSPOOL.FLL�% T�A �� IBM Proprinter on F�4 ��� T�B �F �A �C �� ��> ���(����]� ��F �. � ARIA SYSTEMS�? �� �� ��F �B �C �� <�� TXTSPOOL.FLL� � ��- � TYPE���� T�D �F� HEADINGz�� T�8 �F� MEMOWz�� T�9 �F� CONSz�� T�: ���� T�< ���� T�= ���� G$(�����
 T��/�� T������ T��� �� G� G
� T� ��(�� T�	��  �� T�(��  �� G*(��$ �� ��> ���(������ O(!��. �� �� G*(� SET HEADING &lcHeadSet
 SET CONS &lcConsSet
 G$(��8 �� T�(�� �� T���: �� T���< �� T���= �� 2�2� ��> ���(����.� ��. �(��$ �� �� � � ON ERROR &lcOnErr
 SET SAFE &lcSetSafe    
 T�(��3 ��
  ��. �� ��� %��� NONE���� � v��� %��* ���� %�� � L���= LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
 ��� %��� NONE��g�G REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
 ���? REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE &lcCriteria 
 � � ��� %�� � L���A LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
 ��� %��� NONE��~�Q REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
 ���I REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE PLAIN &lcCriteria     
 � � � w� � �� � FILE��c� %��* ���� %�� � L��T�= LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria 
 ���G REPORT FORM &lcOGRprtNam TO PRINTER  NOCONSOLE NOEJECT &lcCriteria 
 � �_� %�� � L���A LABEL FORM &lcOGRprtNam TO PRINTER NOCONSOLE &lcCriteria     
 �[�Q REPORT FORM &lcOGRprtNam TO PRINTER  NOEJECT NOCONSOLE PLAIN &lcCriteria     
 � � � � G*(� SET CURSOR &lcSetCur
 ON ERROR &lcOldErrHnd
 SET DEFAULT TO &lcSavDFDir
 <�
 � G(&� UE LCOGRPRTNALCCRITERIALLENDJOB  LCRPRTLBL LLPRNTTOFIGCDEVICE ILCTEMPPR ALNOGCOUNT LNOGTOTAL LLQUITING LCSAVDFDIRLCOGTMPFORGCWORKDIR LCFULLSET GCMULTIINSGCCREPHOMEGCACT_APPLGCREPHOME LCOGWHILE LCOGFOR   LNDSRECNO LCOGSCOPE LFGETSCOPELLWHILEFRSLLCANDISP LCLOCTCONDGFMODALGENLCOLDERRHNLCPRNTERROGLPRNT_LANLCPRINTSETGNPRNT_NO GCSERV_NAMGCQUEU_NAMGLBANER   GCBANER_H GCPRNT_PORLCSETCUR  LCADDV E ILCOGSETESCLCTEMPWIN LNPPAGE   GLHEADER  GFENDPRN  LCOGPLATFOLCPRNTTOOLGCOUTFILE GFTEMPNAMELLERROR ORLCONERR   LCSETSAFE LCPDSETUP GCLOCLPRT LCTMPCUR LLCWORKA MEMPRNFILE  LNMEMOWID LCCONSSET LLWRAPSTATLNINDENT  LNRMARGIN LNLMARGIN LNNOCOPY  TXTSPOOL LFLL       LCWINPRT  LCORGPRNT PRTWIN    LCHEADSET �  4�  � T� �e�� T� ��  �� �� ���(�F�  @��� � %�F�  � �`� '"��a � T� �� 
�� �1 T� �� F� � FF�  � �`j� F�  � �`8�� ��	 B�� �� U LCOGCRITERLLTOUP RIALCOGUPCRITLNOGCOUNT � 4�  � 5� � T� �F� EXACTz�� G � T� ��  ��, &lcOGCriteria=lfCondUpper(&lcOGCriteria)
n &lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
 lcCriteria = &lcOGCriteria
X lcOGScope = lcOGScope + IIF(&lcOGCriteria<>STRTRAN(&lcOGCriteria,' ALL '),'ALL ','')
8 &lcOGCriteria = STRTRAN(&lcOGCriteria,' ALL ' , ' ')
n &lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
Z lcOGScope = lcOGScope + IIF(&lcOGCriteria<>STRTRAN(&lcOGCriteria,' REST '),'REST ','')
; &lcOGCriteria = STRTRAN(&lcOGCriteria , ' REST ' , ' ')
n &lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
* IF ATC(' RECORD ',&lcOGCriteria)>0�{�L lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria,ATC(' RECORD ',&lcOGCriteria)+8))
 T� �� �  �� T� �F� �F�  � ��`��! T� �� � RECORD � �  ��� lcOGRec = SUBSTR(&lcOGCriteria,ATC(' RECORD ',&lcOGCriteria),(ATC(' RECORD ',&lcOGCriteria)+LEN(lcOGrcCont)+8)-ATC(' RECORD ',&lcOGCriteria))
4 &lcOGCriteria = STRTRAN(&lcOGCriteria,lcOGRec)  
 �n &lcOGCriteria = IIF(LEFT(&lcOGCriteria,1)=' ','',' ')+&lcOGCriteria+IIF(RIGHT(&lcOGCriteria,1)=' ','',' ')
( IF ATC(' NEXT ',&lcOGCriteria)>0�z�J lcOGRec = ALLTRIM(SUBSTR(&lcOGCriteria,ATC(' NEXT ',&lcOGCriteria)+6))
 T� �� �  �� T� �F� �F�  � ��`��+ T� �� FF� �
� � NEXT � � �  8��� lcOGRec = SUBSTR(&lcOGCriteria,ATC(' NEXT ',&lcOGCriteria),(ATC(' NEXT ',&lcOGCriteria)+LEN(lcOGrcCont)+6)-ATC(' NEXT ',&lcOGCriteria))
4 &lcOGCriteria = STRTRAN(&lcOGCriteria,lcOGRec)  
 � SET EXACT &lcOGExact
	 B�� �� U LCOGCRITERLCOGEXACT LCOGSCOPE LCOGREC T LCOGRCCONT� 4�  � 5� � T� �F� EXACTz�� G � T� ��  �� T� �F �  � ��/ T� �� F�  F�  � ALL��
 � ALL � �  8��1 T� �� F�  F�  � REST�� � REST � �  8�� %�F� RECORD �  �� ��(�$ T� �FF�  F� RECORD �  ��`��� T� �F� �F�  � ��`��! T� �� � RECORD � �  �� � %�F� NEXT �  �� ����" T� �FF�  F� NEXT �  ��`��� T� �F� �F�  � ��`��+ T� �� FF� �
� � NEXT � � �  8�� � SET EXACT &lcOGExact
	 B�� �� U LCOGCRITERLCOGEXACT LCOGSCOPE LCCRITERIALFCONDUPPELCOGREC ERLCOGRCCONT�  4�  � � �� WAIT SUBSTR(SPACE(LEN(lcMessage))+lcMessage+SPACE(LEN(lcMessage)),MOD(&lnRpCount,LEN(lcMessage)*2),LEN(lcMessage)) WINDOW NOWAIT 
! &lnRpCount=&lnRpCount+1      
 B�e�� U LNRPTOTAL LNRPCOUNT LCMESSAGE ?  4�  � �1 B�FF��� /� F�  ���� F� ���	
8�� U LNMROW AL LNMCOL NT �  5�  � T�  �F��� t,��� ��P �F�2� � � �8��F�2� �� �8���� Printing Page FF�^���������� T� ���� %�F�  �
��� � t,��  �� � U LCOUTWIN  LCTEMPWIN LNPPAGE E   B�F� PRINTERz� ON�� U  LFCONDUPPE �"  LFGETSCOPE w#  LFGETCOPE1 O*  LFRPTHERMO t,  LFQUITREPO U-  LFDISPPAGE �-  LFCHCKPRTS k.  Pq�d q �Q� A ct� �crqa ��� �A sA ��a�A �AA �� Q A aa� A � A � AA A V� � �� U� � A � �� a A b�A Ra E��� AA � A A r� � � A � � � #� � �� � Q� � "� qA � "q� A B � s� A ��� AA � A A � � A � g� qQ� "�� A � "�� �A B � � � � �� � �� � Q� � "�� aA � "!� !A B � � � A � A ��6�� �� AA � A � � "�� qA � "� A B A �� AA � A c � � Q11a � � ��� A A A �� � �� � qQ � c � Ua� � A b A1� � � � a � � � � � i�A f a q� � � � � C� W� c�A H�aQ!c�A � AA QaA1� � � � � � � a a � � � � c� A b �q� � � � � � c!A B B Q�� � � BA � � "�� Cq� �A C � "� D� �A C B A A Q� "�� qA � "� A B A A a ���q q 9 q � � ��� A A � 8 q q Aa � �����������!�	AA ���!���AA �� 8 q q Aa � !��A�A �!��A �� 8 � Sq 8 � 8 q � � � � A 4 �1             q  �d  �  |  �f  �  �  }o  �  �  �s  �  �  Mu    �  �v    �  �w  &      �  K  z   W  \M&:k   < 4�  � � DIMENSION &lcArrayName[1]
 STORE .F. TO &lcArrayName
 T� �F�	 MEMOWIDTHz�� G$(����� 5� � %�F� \�  �� ��s� T� �F� FULLPATHz�� GQ �9 %�� �) F� �  � .FRX2� F� � � \�  2	���: T�  �FF� �  � .FRX2�
 � �  � � � � \�  8�� �X�: T�  �FF� �  � .FRX2�
 � �  � � � � \�  8�� � SET FULL &lcFullSet
 �4 T� �FF� \�  �� � F�  F� \�  ��`� �  8�� %�F� �
���� F� � USE &lcFormName..FRX  
 � %�F� ������ SELECT VPOS,HPOS,lfCanDisp() FROM &lcFormName..FRX WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7 AND PLATFORM = lcOGPlatForm ORDER BY VPOS,HPOS INTO ARRAY &lcArrayName
  =gfADel(@&lcArrayName,1,2)  
. =gfADel(@&lcArrayName,1,2)                
 � %�F� ���� Q�� �� � %�F�	 ���*� Q��	 �� � G$(�� �� U
 LCFORMNAMELCARRAYNAMLNMEMOWID LCFILENAMELCFULLSET GCMULTIINSGCCREPHOMEGCACT_APPLGCREPHOME LCOGTMPFOR�  4�  � T� �F�� �  ���) T� �F� � � F�� � ��� � 8��? RETURN IIF(lnArrPos=0,'',ALLTRIM(&lcArrayName[lnArrPos,6]))
 U LNRECNO MELNARRPOS MLAOGOBJPOS�  4�  � T�� �� ��! %�F� #VPOSITION � �� ��� �" T� �F� F� #VPOSITION � ΐ��( T� �FF� F� #VPOSITION � ��
`��� T�� �F� ��� � B��� �� U LNOBJPOS EYPOS POS MVPOS BJPOSCOMMENT  SLCCLAUS T LCSIZE  NS�  4�  � T�� �� ��! %�F� #HPOSITION � �� ��� �" T� �F� F� #HPOSITION � ΐ��( T� �FF� F� #HPOSITION � ��
`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EXPOS POS MHPOS BJPOSCOMMENT  SLCCLAUS T LCSIZE  NSLCOGPLATFOFONTFACE OFONTSIZE  �  4�  � T�� �� �� T� �F� #HSIZE � ��� %�� � ��� � T� �F� F� #HSIZE � ΐ��$ T� �FF� F� #HSIZE � ��`��� T�� �F� ��� � B��� �� U LNOBJPOS EHEIGHT S MLNCLAUS OSCOMMENT  SLCCLAUS T LCSIZE  NS�  4�  � T�� �� �� T� �F� #WSIZE � ��� %�� � ��� � T� �F� F� #WSIZE � ΐ��$ T� �FF� F� #WSIZE � ��`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EWIDTH  S MLNCLAUS OSCOMMENT  SLCCLAUS T LCSIZE  NSLCOGPLATFOFONTFACE OFONTSIZE  | T�  ��  �� T� �e�� %�F� �
��� � %�F� #OBJDISP� �� ��� �G T�  �FF� F� #OBJDISP� ΐF� #OBJDISPF� F� #OBJDISP� ΐ��	`�� � � %�F�  �
��l� T� �F� #OBJDISPF� j��� �� ���(�� ��h�$ T�  �FF� F� #OBJDISP� � �`���G T�  �FF�  F� #OBJDISP�  ΐF� #OBJDISPF�  F� #OBJDISP�  ΐ��	`�� T� �F�  ��� %�� 
��d� !� � �� �	 B�� �� U LCCONDITIOLLCANDISP COMMENT OSLNNOCOND OLNCOUNT T ( 4�  � 5� %�F� (�  �� ��2 � B�F� ,�  ��� �� T� �� �� T� �� �� �� ���(�F�  @��� ��z �� �F�  � �`� (��� � T� �� ��� �F�  � �`� )��� � T� �� ��� �F�  � �`� ,��� %�� � ��� T� �� �� !� � � �� �	 B�� �� U LCSTRING OLNCOUTS   LNCOMPOS SLNCOUNT  OLFMODISTYL �  LFOGVPOS E j  LFOGHPOS r N  LFOGHSIZE �  LFOGWSIZE .�  LFCANDISP ��  LFGETCOM  �
  � ���� r qqa ��� �A sA As �A � �A � � A � � A � 8 q A��: q � !�A � 8 q � !��A � 8 q � ��AA � 8 q � ��A�A � 8 � � �rA A �qAr� � A A A A � 8 q Q q� � � � �� ���� A A A A A � 1             "   �	  D   '   =  R   0     b   :   �  s   D   q  �   O   $  �   c   �  �       �	  �
  S   &
  OM&:�   � 4�  � � 5� � � T� �F�	 MEMOWIDTHz�� G$(����� T� �FZ��2 DIMENSION &lcArrayName[1,ALEN(&lcArrayName,2)]
 STORE '' TO &lcArrayName
 5� � %�F� \�  �� ���� T� �F� FULLPATHz�� GQ �9 %�� �) F�	 �  � .FRX2� F�	 �
 � \�  2	��A�: T�  �FF�	 �  � .FRX2�
 �	 �  � �	 �
 � \�  8�� ���: T�  �FF� �  � .FRX2�
 � �  � � �
 � \�  8�� � SET FULL &lcFullSet
 �4 T� �FF� \�  �� � F�  F� \�  ��`� �  8�� %�F� �
��� F� � USE &lcFormName..FRX  
 � -�� � �� %�F6
��6� T� �� DOS�� � %�F� ���\�# %�� � DOS� � � UNIX��S�� SELECT EXPR,lfOGVPos() ,lfOGHPos(), lfOGHSize(),lfOGWSize(), STYLE,objtype,objcode FROM &lcFormName..FRX WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7 AND PLATFORM = lcOGPlatForm ORDER BY VPOS,HPOS INTO ARRAY &lcArrayName
 �X�� SELECT EXPR,lfOGVPos() ,lfOGHPos(), lfOGHSize(),lfOGWSize(), FONTFACE,FONTSTYLE,FONTSIZE,objtype,objcode FROM &lcFormName..FRX WHERE OBJTYPE IN(8,5,7) AND OBJCODE BETWEEN 0 AND 7 AND PLATFORM = lcOGPlatForm ORDER BY VPOS,HPOS INTO ARRAY &lcArrayName
 � � %�F� ���z� Q�� �� � %�F� ����� Q�� �� �
 F�� �� G$(�� �� U LCFORMNAMELCARRAYNAMLCERRHAND LLERRHAPENLNMEMOWID LCOGSELECTLCFILENAMELCFULLSET GCMULTIINSGCCREPHOMEGCACT_APPLGCREPHOME PLATFORM ELCOGPLATFOLCOGTMPFOR�  4�  � T�� �� ��! %�F� #VPOSITION � �� ��� �" T� �F� F� #VPOSITION � ΐ��( T� �FF� F� #VPOSITION � ��
`��� T�� �F� ��� � B��� �� U LNOBJPOS EYPOS AYNAMVPOS HAND COMMENT ENLCCLAUS D LCSIZE  CT�  4�  � T�� �� ��! %�F� #HPOSITION � �� ��� �" T� �F� F� #HPOSITION � ΐ��( T� �FF� F� #HPOSITION � ��
`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EXPOS AYNAMHPOS HAND COMMENT ENLCCLAUS D LCSIZE  CTLCOGPLATFOFONTFACE OFONTSIZE S�  4�  � T�� �� �� T� �F� #HSIZE � ��� %�� � ��� � T� �F� F� #HSIZE � ΐ��$ T� �FF� F� #HSIZE � ��`��� T�� �F� ��� � B��� �� U LNOBJPOS EHEIGHT NAMLNCLAUS D COMMENT ENLCCLAUS D LCSIZE  CT�  4�  � T�� �� �� T� �F� #WSIZE � ��� %�� � ��� � T� �F� F� #WSIZE � ΐ��$ T� �FF� F� #WSIZE � ��`��� T�� �F� ���X T�� �F�� F� � WINDOWS� � � MAC� F�F� �� �$�	8��d�
Z@� �8�W�� � B��� �� U	 LNOBJPOS EWIDTH  NAMLNCLAUS D COMMENT ENLCCLAUS D LCSIZE  CTLCOGPLATFOFONTFACE OFONTSIZE SLFOGVPOS HSl  LFOGHPOS WSP  LFOGHSIZE D�  LFOGWSIZE C�  � � �� � !�r qqa ��� �A sA As �A � � A � 3x� �A A � � A � � A � � 9 q � !�A � 8 q � !��A � 8 q � ��AA � 8 q � ��A�A � 1             +   �  c   4   �  s   >     �   H     �         B  z   N  �K&:�   t 4�  � � � 5� � T� �FF��� /� � 8�� 5� � T� �/�� T� �e�� %�� ��"� T� �FZ�� %�F� SYDREPRT�
��� � F� �# USE &gcSysHome.SYDREPRT ORDER 1
 T� �e�� �� � F� �
 G((� 1� � T� ��  �� ��FF�  �ǚ��  -��
 � A40�+��	 F�  ����. T� �FFF� �
� FF� ����
 FF�  ���8��� %�� ��T�
 ��F� �� �
 F�� �� T� �F� FULLPATHz�� GQ �I %�� � F� � � .FXP2	�# � � F� � � \� � .FXP2	��9�q T� �FF� � � .FXP2�
 � � �G FF� � � \� � .FXP2� � � � \� � � F� �?� \� 88�� ���q T� �FF� � � .FXP2�
 � � �G FF� � � \� � .FXP2� � � � \� � � F� �?� \� 88�� � SET FULL &lcFullSet
 T� �� �� T� �F� CENTURYz�� G� DO &lcSetRpPrg
 SET CENTURY &lcSetCent
 �m� 5� � � � � � � � ����# � ������� �������& J��  �(� � � � �  �! �" � J��  �(� � � � � �c �# �������$ �������% �������& �������' �������( ������� �) ������� �* ������� T�+ �� N�� %�F� SYDREPRT�
���� F� �# USE &gcSysHome.SYDREPRT ORDER 1
 T� �e�� ��� F� �
 G((� 1� � T� ��  �� ��FF�  �ǚ��  -��
 � A40�+��	 F�  ����. T� �FFF� �
� FF� ����
 FF�  ���8��� T� �F� FULLPATHz�� GQ �I %�� � F� � � .FXP2	�# � � F� � � \� � .FXP2	����q T� �FF� � � .FXP2�
 � � �G FF� � � \� � .FXP2� � � � \� � � F� �?� \� 88�� �u�q T� �FF� � � .FXP2�
 � � �G FF� � � \� � .FXP2� � � � \� � � F� �?� \� 88�� � SET FULL &lcFullSet
 T� �F�	 PROCEDUREz�� T�, �F� ERROR��� 1� llDumErr = .T.� G+(�F� ��� ON ERROR &lcOldErr

 ��F�- �� %�� ��:� ��F �  / � �. �� %�� ��6� Q� � � � G+(�� �� %�F�
 laPrgTempsf� U��i� ��/ ���(�F�� ����e� %�FF �/ �� �
��a� %�FF �/ �� ����� Q�F �/ �� �� �  ��0 F �/ �� � .DBF��  ��0 F �/ �� � .FPT��  ��0 F �/ �� � .CDX��  ��0 F �/ �� � .TXT��  ��0 F �/ �� � .TMP�� � �� � � U1 LCRPPRG   LLDOREPORTLCREPDIR  LCSETCENT LLREPFILE LLDOOPGRIDLCREPALIASSYDREPRT  LCSETRPPRGCREP_ID   CVER RG   CMAINREPIDLAPPARELRPGFSETDEVICLCFULLSET GCMULTIINSGCCREPHOMEGCACT_APPLGCREPHOME QCOMPANY GGCCOM_NAMELCBRFIELDSLCBASEFILELAFIELD_H LAFIELD_N LCFILE_TTLLCSETPROCDLAPRGTEMPSLCRPFORM  LCRPFILES LCRPEXPR  LCRPTARGT LCRPORDER LCRPFIELDSLCRPEXP   LARPMATYPELARPSTYPE LARPCTYPE LARPETYPE LARPITYPE LARPTTYPE LARPCOL   LATEMPCOL R_WIDTH   LCOLDERR DLFPREGRD GGFOPGRID DLNCOUNT CDGCWORKDIR �  4�  � � � 5� �: T� �FF�	 lcMessagef� C� � � � Collecting Data8�� &lnRpCount = &lnRpCount + 1
 T� �F� ESCAPEz�� G�  IF &lnRpCount<=lnRpTotal�� �1 =gfThermo(lnRpTotal,&lnRpCount,lcMessage,' ')
 � SET ESCAPE &lcOldEsc
 U LNRPTOTAL LNRPCOUNT LCMESSAGE LCOLDESC  �  4�  � � � �( T� �FF� lnStepf� N� � � �8�� T� �F� �� �� � 8��: T� �FF�	 lcMessagef� C� � � � Collecting Data8�� �� �� �(��  ���� ��� � ��F �   �  � �  � �� �� %�F�	 gwdThermo���� � <,� � � U LNRPTOTAL LNRPCOUNT LCMESSAGE LNSTEP C  LNCOUNTER GFTHERMO  GWDTHERMO !  R,:�� Collecting data... �� U    U  LFRPTHERMO �
  LFCLOSTHRM �  LFWAITMSG  �  LFCLEARREP   �� t �s � � � � �q 1� � q � A � �� � A � ua �� B u� da "�� �� 3b�411� �q 1� � q � A � �xa �� B u�Qa� b� � 1� � A C � ���q1A �����A A A D 8 � q ��Qa A �9 1�����A �� A 9 �8 1             a   �&    l   )  $  w   ^+  7  y   g,  A      0   G      ?   @f+:�     U    B� U  LFPREGRD OC%   �1 A 1                �     x:\build42\reports\gfcrtfrm.prg c:\windows\temp\ GFCRTFRM.FXP x:\build42\reports\gfdispre.prg GFDISPRE.FXP x:\build42\reports\gffrmcnt.prg GFFRMCNT.FXP x:\build42\reports\gfinstar.prg GFINSTAR.FXP x:\build42\reports\ic\icreport.prg ICREPORT.FXP x:\build42\reports\ic\lfpregrd.prg LFPREGRD.FXP  *   H      1    H  (C      ^    (C  �P      �    �P  �[      �    �[  Yj      �    Yj  �j        