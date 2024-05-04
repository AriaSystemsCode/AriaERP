<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="ExportUserControl.ascx.cs"
    Inherits="AriaDevExpress.Web.UserControls.SysFiles.ExportUserControl" %>
<table width="100%" cellspacing="15">
    <tr>
        <td width="30%">
        </td>
        <td>
            Application
        </td>
        <td>
            <dx:ASPxComboBox ID="applicationDropDownlist" runat="server">
            </dx:ASPxComboBox>
        </td>
        <td>
            Client:
        </td>
        <td>
            <dx:ASPxComboBox ID="ClientDropDownlist" runat="server">
            </dx:ASPxComboBox>
        </td>
        <td width="30%">
        </td>
    </tr>
    <tr>
        <td width="30%">
        </td>
        <td>
            Database:
        </td>
        <td>
            <dx:ASPxComboBox ID="DataBaseDropDownlist" runat="server">
            </dx:ASPxComboBox>
        </td>
        <td colspan="2">
        </td>
        <td width="30%">
        </td>
    </tr>
    <tr>
        <td width="30%">
        </td>
        <td colspan="4">
            <dx:ASPxListBox ID="TablesListBox" Width="100%" SelectionMode="CheckColumn" Height="200px"
                runat="server">
                <Items>
                    <dx:ListEditItem Text="Fields" Value="SYDFIELD" />
                    <dx:ListEditItem Text="Tables" Value="SYDFILES" />
                    <dx:ListEditItem Text="Table Fields" Value="Sydflfld" />
                    <dx:ListEditItem Text="Table Index" Value="SYDINDEX" />
                    <dx:ListEditItem Text="Report" Value="sydreprt" />
                    <dx:ListEditItem Text="Report Variable" Value="syrepuvr" />
                    <dx:ListEditItem Text="Menu" Value="SycMenu" />
                    <dx:ListEditItem Text="Object" Value="SYDOBJCT" />
                </Items>
            </dx:ASPxListBox>
        </td>
        <td width="30%">
        </td>
    </tr>
    <tr>
        <td width="30%">
        </td>
        <td colspan="4" align="center">
            <dx:ASPxButton ID="btnExport" runat="server" Text="Export" OnClick="btnExport_Click">
            </dx:ASPxButton>
        </td>
        <td width="30%">
        </td>
    </tr>
</table>
