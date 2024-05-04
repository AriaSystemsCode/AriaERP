 <%@ Control Language="C#" AutoEventWireup="true" EnableViewState="true" CodeBehind="Fields.ascx.cs" Inherits="Aria5SystemAdmin.Web.UserControls.SysFiles.Fields" %>
<%@ Register assembly="DevExpress.Web.Bootstrap.v17.1, Version=17.1.3.0, Culture=neutral, PublicKeyToken=b88d1754d700e49a" namespace="DevExpress.Web.Bootstrap" tagprefix="dx" %>
<div  style="float: left; width:47%; height: 408px;">
    <form>
        <div>
    &nbsp;&nbsp;Alias:&nbsp;&nbsp;&nbsp;&nbsp;<br />
&nbsp;<asp:TextBox ID="TXT_Alias" runat="server" Width="152px" Height="16px" style="margin-left: 0px"></asp:TextBox>
        &nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            </div>
        <div>
            <%--        <p style="height: 0px; width: 65px">--%>&nbsp;Field&nbsp;:&nbsp;&nbsp;<dx:ASPxComboBox ID="combo_Fields"  ValueType="System.String" runat="server" OnInit="combo_Fields_Init">
        </dx:ASPxComboBox>
            
            </div>

        <div>
            Expression&nbsp;:&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;<br />
&nbsp;<asp:TextBox ID="TXT_Expression" runat="server" Width="152px" Height="16px" style="margin-left: 0px"></asp:TextBox>
            </div>
        <%-- </p>--%><%--<p style="height: 14px; width: 261px">--%>
        <div>
 
<%-- </p>--%>
            </div>
 &nbsp;<div>
            <%--<dx:ASPxButton HorizontalAlign="Center" CssClass="AlignButtonCenter" runat="server" Text="Add To List" ID="AddToList" Width="80px" OnClick="AddToList_Click" UseSubmitBehavior="false"/>--%>
            <dx:BootstrapButton ID="AddToList" runat="server"  DropDownStyle="DropDown" OnClick="AddToList_Click" Text="Add To List">
            </dx:BootstrapButton>
 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <dx:BootstrapButton ID="Clear" runat="server"  DropDownStyle="DropDown" OnClick="Clear_Click" Text="Clear" Width="80px">
            </dx:BootstrapButton>
 </div>
    </form>
</div>

<div style="float:right;width:50%;">
    List Of Fields <asp:ListBox ID="LB_Fields" runat="server" AutoPostBack="true" Height="170px" Width="236px" OnInit="LB_Fields_Init" OnSelectedIndexChanged="LB_Fields_SelectedIndexChanged"></asp:ListBox>

    <br />
    <br />
    <br />
    <br />
    <br />
    <br />
    <br />
    <br />
            <dx:BootstrapButton ID="RemoveFromList" runat="server"  DropDownStyle="DropDown" OnClick="RemoveFromList_Click" Text="Remove From List">
            </dx:BootstrapButton>

</div>
 



 


 

 



 


