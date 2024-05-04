<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="EmailSetting.ascx.cs" Inherits="AriaDevExpress.Web.UserControls.OneTouchAway.EmailTemplate" %>
 <div style="width: 100%; text-align: center; font-size: x-large; padding: 10px 0 10px 0;">
       Email Settings
    </div>
    <center>
        <table cellpadding="5">
            <tr>
                <td>
                    From Email
                </td>
                <td>
                    <asp:TextBox ID="txtFrom" Columns="30" runat="server"></asp:TextBox>
                </td>
            </tr>
            <tr>
                <td>
                    From Name
                </td>
                <td>
                    <asp:TextBox ID="txtFromName" Columns="30" runat="server"></asp:TextBox>
                </td>
            </tr>
            <tr>
                <td>
                    SMTP Host
                </td>
                <td>
                    <asp:TextBox ID="txtHost" Columns="30" runat="server"></asp:TextBox>
                </td>
            </tr>
            <tr>
                <td>
                    SMTP Username
                </td>
                <td>
                    <asp:TextBox ID="txtUser" Columns="30" runat="server"></asp:TextBox>
                </td>
            </tr>
            <tr>
                <td>
                    SMTP Password
                </td>
                <td>
                    <asp:TextBox ID="txtPass" Columns="30" runat="server"></asp:TextBox>
                </td>
            </tr>
            <tr>
                <td>
                    SMTP Port
                </td>
                <td>
                    <asp:TextBox ID="txtPort" runat="server" Columns="10"></asp:TextBox>
                </td>
            </tr>
            <tr>
                <td>
                    SMTP SSL
                </td>
                <td>
                    <asp:CheckBox ID="chkSSL" runat="server" />
                </td>
            </tr>
            <tr>
                <td colspan="2" align="center">
                    <asp:Button ID="btnSave" runat="server" Text="Save" OnClick="btnSave_Click" Width="69px" />
                </td>
            </tr>
        </table>
    </center>

