<%@ Page Language="C#" AutoEventWireup="true" CodeFile="Default.aspx.cs" Inherits="WebPages_Default" %>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head runat="server">
    <title>Untitled Page</title>
<script language="javascript" type="text/javascript">
<!--

function Button2_onclick() {
var args = window; 
var modal = window.showModalDialog("configure.aspx", args, "dialogWidth=800px;dialogHeight=600px;scrollbars=yes;resizable=no;status=no;help=no"); 

}

// -->
</script>
</head>
<body>
    <form id="form1" runat="server">
    <div>
      &nbsp;
      <asp:TextBox ID="TextBox2" runat="server"></asp:TextBox>
      <asp:TextBox ID="TextBox1" runat="server"></asp:TextBox>
      <asp:Button ID="Button1" runat="server" OnClick="Button1_Click1" Text="Button" />
      <asp:TextBox ID="TextBox3" runat="server"></asp:TextBox><br />
      <br />
      <asp:GridView ID="GridView1" runat="server">
      </asp:GridView>
      <br />
      <asp:Button ID="Button2" runat="server" OnClick="Button2_Click" Text="Button" /></div>
    </form>
</body>
</html>
