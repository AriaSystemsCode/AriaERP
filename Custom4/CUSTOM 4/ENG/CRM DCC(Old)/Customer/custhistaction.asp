<%@ Language=VBScript %>
<%Response.Buffer=True%>
<html>
<head>
<meta NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>CRM - Customer History</TITLE>
</head>
<body topmargin="0" leftmargin="0" marginheight="0" marginwidth="0" bgcolor="#aecae6">
<%
IF trim(request("seqnum"))="" Then
				response.redirect "custhistory.asp"
Else
				valueofseq=trim(request("seqnum"))
				seqsplit=Split(valueofseq,",")
				Set conn=server.CreateObject("ADODB.connection")
				Set conn2=server.CreateObject("ADODB.connection")
				Set rs=server.CreateObject("ADODB.recordset")
				conn.Open Application("SystemConnectionString")
				conn2.Open Application("DataConnectionString")
				Set rs2=server.CreateObject("ADODB.recordset")
				sqls="select * from contact where ccont_id='"&Session("customerid")&"'"
				RS2.open sqls,conn2,1,3

				sqlstat="select  * from syschdul where cseqnumber='"&request("seqnum")&"'"
				rs.open sqlstat,conn ,1,3
				If trim(cstr(request("modify")))<>"" then
				Set RS1=Server.CreateObject("ADODB.Recordset")
				sqlt="select distinct * from codes where cfld_name='CRESULTCD' AND CDEFCODE='N'"
				RS1.Open sqlt,conn2,1,3
				RS1.MoveFirst %>
									<table border="0" width="100%" cellspacing="0" cellpadding="0">
										<tr>
											<td>
									            <p align="center"><font color="navy" size="5"><b>History</b></font>
									            </p>
									            <p align="center"><BR>
									            </p>
											</td>
										</tr>
										<tr>
											<td width=100%>
									<table  border="1" bgcolor="#6495d0" bordercolor="#aecae6" width=750>
										<tr>

											<td align="left" width="19%">
									            <b><font color="navy" size="2" face="arial">Activity</font></b>
											</td>
											<td align="left" width="19%">
									            <b><font color="navy" size="2" face="arial">Date</font></b>
											</td>
											<td align="left" width="19%">
									            <b><font color="navy" size="2" face="arial">User</font></b>
											</td>
											<td align="left" width="19%">
									            <b><font color="navy" size="2" face="arial">Contact</font></b>
											</td>
											<td align="left" width="19%">
									            <b><font color="navy" size="2" face="arial">Result</font></b>
											</td>
											<td align="left" width="19%">
									            <b><font color="navy" size="2" face="arial">Subject</font></b>
											</td>
										</tr>
										
				                        <form action="custhistory.asp?SEQ=<%=rs("cseqnumber")%>" method=post id=form1 name=form1>
											
														<tr>
														
																
																<td align="left" valign="center" width="19%" bgcolor="ivory"><font color="navy" size="2" face="Arial">
																<%
														    If rs("ctrantype")="C" Then 
																	Response.Write "<input name=txtact disabled value=Call>"
																End If
																If rs("ctrantype")="A" Then 
																	Response.Write "<input name=txtact disabled value=Appointment>"
																End If
																If rs("ctrantype")="T" Then 
																	Response.Write "<input name=txtact disabled value=To do>"
																End If
																%></font>
																</td>
																<td align="left" valign="center" width="19%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><input name=dttran2 value="<%If trim(rs("dtrandate"))="12:00:00 AM" Then response.write " " Else response.write trim(rs("dtrandate"))%>"></font></td>
																<td align="left" valign="center" width="19%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><input  name=txtuserid2 value="<%Response.Write rs("cuser_id") %>" size="20"></font></td>
																<td align="left" valign="center" width="19%" bgcolor="ivory"><font color="navy" size="2" face="Arial">
																<select size="1" name=txtcontact2 >
																<option value="<%=rs("contact")%>" <%If trim(request("lstcontact"))=trim(RS("contact")) then response.write "selected"%>><%=RS("contact")%></option>
																<%RS2.MoveFirst
																Do while not RS2.eof%>
																	<%IF trim(rs("contact"))<>trim(rs2("contact")) Then%>
																			<option value="<%=RS2("contact")%>" <%If trim(request("lstcontact"))=trim(RS2("contact")) then response.write "selected"%>><%=RS2("contact")%></option>
																			<%RS2.Movenext
																		Else
																				RS2.Movenext
																		End If
																Loop%>  
																</select>
																<td align="left" valign="center" width="19%" bgcolor="ivory"><font color="navy" size="2" face="Arial">
																<%
															Set dbrs1=server.CreateObject("ADODB.recordset")
															If trim(rs("cresultcd"))<>"" Then
																sqls="select distinct * from codes where cfld_name='CRESULTCD' AND CCODE_NO='"&rs("CRESULTCD")&"'AND CDEFCODE='N'"
																dbrs1.open sqls,conn2,3,3
															ENd IF
					%>
				  <select id=lstresult name=lstresult  size="1">
				  <%If trim(rs("cresultcd"))<>"" Then%>
					  <option name="<%=dbrs1("ccode_no")%>" <%If trim(dbrs1("cdiscrep"))=trim(request("lstresult")) Then response.write "selected" %>><%=dbrs1("Cdiscrep")%>
				  <%End IF%>
				   <%Do while not rs1.eof
								If  trim(rs1("cdiscrep"))=trim(dbrs1("cdiscrep")) Then
									rs1.MoveNext 
								Else%>
									<option value="<%=rs1("ccode_no")%>" <%If trim(rs1("cdiscrep"))=trim(request("lstresult")) Then response.write "selected" %>><%=rs1("Cdiscrep")%>
									<%rs1.movenext
								End If
						Loop%>
				  </select>
					</font>
					</td>
					<td align="left" valign="center" width="19%" bgcolor="ivory"><font color="navy" size="2" face="Arial"><input name=txtsubject2 value="<%Response.Write rs("csubject")%>"></font></td>
					</tr>
					</table>
					<table align=center border=0>
					<tr ><td ><input type=submit value="Modify" id=modify name=modify></td>
					 </form>

		<%ElseIf trim(cstr(request("remove")))<>"" Then
								for i=0 to ubound(seqsplit)
									sqlS1="delete from syschdul Where cseqnumber='"&trim(seqsplit(i))&"'"
									Set RSTemp=Server.CreateObject("ADODB.Recordset")
									RSTemp.Open sqls1,conn,1,3
								next
								response.redirect "custhistory.asp"
		End If%>	

<%End IF%>
</body>
</html>

































