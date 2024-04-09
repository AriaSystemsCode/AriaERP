*-- Call form to run debit & credit adjustment program
*oAriaApplication.DoProgram('AWRARDCADJ',"'D',' '",.F.,"AR")
DO FORM (oAriaApplication.ScreenHome+"ARDCADJ.SCX") WITH "D",' '
