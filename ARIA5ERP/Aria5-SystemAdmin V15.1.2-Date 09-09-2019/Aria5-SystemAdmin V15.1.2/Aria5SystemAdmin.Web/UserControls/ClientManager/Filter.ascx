 <%@ Control Language="C#" AutoEventWireup="true" EnableViewState="true" CodeBehind="Filter.ascx.cs"  Inherits="Aria5SystemAdmin.Web.UserControls.ClientManager.Filter"%>
<%@ Register assembly="DevExpress.Web.Bootstrap.v17.1, Version=17.1.3.0, Culture=neutral, PublicKeyToken=b88d1754d700e49a" namespace="DevExpress.Web.Bootstrap" tagprefix="dx" %>
<div  style="float: left; width:47%; height: 408px;">
    <form>
        <div>
    &nbsp;&nbsp;<asp:Label ID="Name" runat="server" Text="Name"></asp:Label>
        :&nbsp;&nbsp;&nbsp;&nbsp;<br />
&nbsp;<asp:TextBox ID="TXT_Name" runat="server" Width="152px" Height="16px" style="margin-left: 0px"></asp:TextBox>
        &nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            </div>
        <div>
<%--        <p style="height: 0px; width: 65px">--%>
            
                &nbsp;<asp:Label ID="Type" runat="server" Text="Type"></asp:Label>
            &nbsp;&nbsp;:&nbsp;&nbsp;<dx:ASPxComboBox ID="combo_type"  ValueType="System.String" runat="server" OnInit="combo_type_Init">
        </dx:ASPxComboBox>
            
            </div>

 <%--<p style="height: 16px; width: 267px">--%>
        <div>
     <asp:Label ID="Label3" runat="server" Text="Data Type"></asp:Label>
&nbsp;&nbsp;:&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;
        <dx:ASPxComboBox ID="Combo_datatype" ValueType="System.String" runat="server" OnInit="Combo_datatype_Init">
        </dx:ASPxComboBox>
            </div>
    <%-- </p>--%>
 <%--<p style="height: 14px; width: 261px">--%>
        <div>
     <asp:Label ID="Label4" runat="server" Text="NOT"></asp:Label>
&nbsp;&nbsp;&nbsp;&nbsp;:&nbsp;&nbsp;
       <%-- <p style="height: 14px; width: 261px">--%>
            &nbsp;<asp:CheckBox ID="CB_Not" runat="server" />
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
 <%--</p>--%>
            </div>
        <div>
<%-- <p style="height: 18px; width: 261px">--%>
&nbsp;<asp:Label ID="Label5" runat="server" Text="Operator"></asp:Label>
&nbsp;&nbsp;&nbsp;&nbsp;:&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
          
        <dx:ASPxComboBox ID="combo_sysoperator" ValueType="System.String" runat="server" OnInit="combo_sysoperator_Init">
        </dx:ASPxComboBox>
            </div>
        <div>
 <%--<p style="height: 20px; width: 261px">--%>
     <asp:Label ID="Label6" runat="server" Text="Value"></asp:Label>
            &nbsp;&nbsp;&nbsp;:&nbsp;<br />
&nbsp;<asp:TextBox ID="TXT_Value" runat="server" Width="153px" Height="16px"></asp:TextBox>
            </div>
<%-- </p>--%>
<%-- <p style="height: 20px; width: 261px">--%>
        <div>
 <asp:Label ID="Label7" runat="server" Text="Value Type"></asp:Label>
&nbsp; :&nbsp;
 
<%-- </p>--%>
        <dx:ASPxComboBox ID="Combo_valueType" ValueType="System.String" runat="server" OnInit="Combo_valueType_Init">
        </dx:ASPxComboBox>
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
 <asp:ListBox ID="LB_Filters" runat="server" AutoPostBack="true" Height="170px" Width="236px" OnInit="LB_Filters_Init" OnSelectedIndexChanged="LB_Filters_SelectedIndexChanged"></asp:ListBox>

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
 



 


 

 



 


