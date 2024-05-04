 <%@ Control Language="C#" AutoEventWireup="true" EnableViewState="true" CodeBehind="Files.ascx.cs" Inherits="Aria5SystemAdmin.Web.UserControls.SysFiles.Files"%>
<%@ Register assembly="DevExpress.Web.Bootstrap.v17.1, Version=17.1.3.0, Culture=neutral, PublicKeyToken=b88d1754d700e49a" namespace="DevExpress.Web.Bootstrap" tagprefix="dx" %>
 <div  style="float: left; width:47%; height: 408px;">
     <form>
         <div>
            </div>
        <div>
            <%--        <p style="height: 0px; width: 65px">--%>&nbsp;File&nbsp;&nbsp;:&nbsp;&nbsp;<dx:ASPxComboBox ID="combo_Files"  ValueType="System.String" runat="server" OnInit="combo_Files_Init">
        </dx:ASPxComboBox>
            
            </div>
        <div>
            Index&nbsp;&nbsp;:&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;
        <dx:ASPxComboBox ID="Combo_Indeces" ValueType="System.String" runat="server" OnInit="Combo_Indeces_Init">
        </dx:ASPxComboBox>
            </div>
        <%-- </p>--%><%--<p style="height: 14px; width: 261px">--%>
        <div>
 
<%-- </p>--%>
            &nbsp;Alias:&nbsp;&nbsp;&nbsp;&nbsp;<br />
&nbsp;<asp:TextBox ID="TXT_Name" runat="server" Width="152px" Height="16px" style="margin-left: 0px"></asp:TextBox>
            &nbsp;</div>
 &nbsp;<br />
         <div>
            <%--<dx:ASPxButton HorizontalAlign="Center" CssClass="AlignButtonCenter" runat="server" Text="Add To List" ID="AddToList" Width="80px" OnClick="AddToList_Click" UseSubmitBehavior="false"/>--%>
            <dx:BootstrapButton ID="AddToList" runat="server"  DropDownStyle="DropDown" OnClick="AddToList_Click" Text="Add To List" Width="106px">
            </dx:BootstrapButton>
 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <dx:BootstrapButton ID="Clear" runat="server"  DropDownStyle="DropDown" OnClick="Clear_Click" Text="Clear" Width="89px">
            </dx:BootstrapButton>
 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            </div>
    </form>
</div>

<div style="float:right;width:50%;">
    List Of Files
 <asp:ListBox ID="LB_Files" runat="server" AutoPostBack="true" Height="170px" Width="236px" OnInit="LB_Files_Init" OnSelectedIndexChanged="LB_Filters_SelectedIndexChanged"></asp:ListBox>

    <br />
    <br />
            <%--<dx:BootstrapButton ID="RemoveFromList" CssClasses-Control="btn btn-default dxbs-button" runat="server" DropDownStyle="DropDown" OnClick="RemoveFromList_Click" Text="Remove From List">
             
            </dx:BootstrapButton>--%>
    <Button  ID="RemoveFromList" CssClasses-Control="btn btn-default dxbs-button" runat="server"OnClick="RemoveFromList_Click" Text="Remove From List">
             
            </Button>

</div>
 

 


 

 



 


