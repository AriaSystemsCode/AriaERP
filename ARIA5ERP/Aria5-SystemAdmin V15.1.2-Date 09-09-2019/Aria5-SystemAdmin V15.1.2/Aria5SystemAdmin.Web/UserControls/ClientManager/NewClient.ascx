<%@ Control Language="C#" AutoEventWireup="true" CodeBehind="NewClient.ascx.cs" Inherits="Aria5SystemAdmin.Web.UserControls.ClientManager.NewClient" %>


<style type="text/css">
    #ClientInfo input,
    #ClientCompanies input,
    #ClientUsers input,
    #SQLInfo input {
        width: 300px;
    }

    .SqlTitle {
        font-size: larger;
        font-weight: bold;
    }
</style>
<div>
    <div style="width: 100%;">
        <dx:ASPxLabel ID="lblServiceStatus" runat="server" Style="text-align: right; float: right"></dx:ASPxLabel>
    </div>
    <dx:ASPxPageControl ID="MainContentASPxPageControl" EnableCallBacks="false" runat="server" ClientInstanceName="Tabs" ClientVisible="true" EnableClientSideAPI="true" Width="100%" ActiveTabIndex="1">
        <ClientSideEvents ActiveTabChanged="function(s,e){ TabChanged();}" />
        <TabPages>
            <dx:TabPage Text="Client Info">
                <ContentCollection>
                    <dx:ContentControl>
                        <table id="ClientInfo" cellspacing="15">
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Client Code" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtClientCode" MaxLength="5" ClientInstanceName="txtClientCode" runat="server">
                                        <ClientSideEvents TextChanged="function(s,e){ClientCodeChanged();}" />
                                    </dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Client Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtClientName" MaxLength="50" runat="server"></dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Client Phone" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtClientPhone" runat="server"></dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="ActivationKey Path" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtActivationKeyPath" runat="server"></dx:ASPxTextBox>
                                </td>
                            </tr>
                        </table>
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
            <dx:TabPage Text="Companies">
                <ContentCollection>
                    <dx:ContentControl>
                        <table id="ClientCompanies" cellspacing="15">
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Company Code" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtNewCompanyCode" runat="server" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Company Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtNewCompanyName" runat="server" />
                                </td>
                            </tr>
                            <tr>
                                <td colspan="2" align="center">
                                    <dx:ASPxButton ID="btnAddCompany" UseSubmitBehavior="false" OnClick="btnAddCompany_Click" runat="server" Text="Add Company" />
                                </td>
                            </tr>
                            <tr>
                                <td colspan="2"></td>
                            </tr>
                            <tr>
                                <td colspan="2"></td>
                            </tr>
                            <tr>
                                <td colspan="2" align="center">
                                    <dx:ASPxListBox ID="lstCompanies" Width="100%" runat="server" ClientEnabled="true" ClientInstanceName="lstComapnies" SelectionMode="CheckColumn">
                                        <ClientSideEvents SelectedIndexChanged="function(s, e) { UpdateButtonState(); }" />
                                    </dx:ASPxListBox>
                                </td>
                            </tr>
                            <tr>
                                <td colspan="2" align="center">
                                    <dx:ASPxButton ID="btnDeleteCompany" UseSubmitBehavior="false" OnClick="btnDeleteCompany_Click" runat="server" ClientEnabled="true" ClientInstanceName="btnDeleteCompany" Text="Delete" />
                                </td>
                            </tr>
                        </table>
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
            <dx:TabPage Text="Users">
                <ContentCollection>
                    <dx:ContentControl>
                        <table id="ClientUsers" cellspacing="15">
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="No Of Users" />
                                </td>
                                <td>
                                    <dx:ASPxSpinEdit runat="server" ID="txtUsersNo" MinValue="0" AllowUserInput="true" MaxLength="2" NumberType="Integer" AllowNull="false" />
                                </td>
                            </tr>
                            <tr>
                                <td colspan="2" align="center">
                                    <dx:ASPxButton ID="btnGenerateUsers" UseSubmitBehavior="false" runat="server" OnClick="btnGenerateUsers_Click" Text="Generate" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="User Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtNewUserName" runat="server" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="User Password" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox ID="txtNewUserPassword" runat="server" />
                                </td>
                            </tr>
                            <tr>
                                <td colspan="2" align="center">
                                    <dx:ASPxButton ID="btnAddUser" UseSubmitBehavior="false" OnClick="btnAddUser_Click" runat="server" Text="Add User" />
                                </td>
                            </tr>
                            <tr>
                                <td colspan="2"></td>
                            </tr>
                            <tr>
                                <td colspan="2"></td>
                            </tr>
                            <tr>
                                <td colspan="2" align="center">
                                    <dx:ASPxListBox ID="lstUsers" Width="100%" runat="server" ClientEnabled="true" ClientInstanceName="lstUsers" SelectionMode="CheckColumn">
                                        <ClientSideEvents SelectedIndexChanged="function(s, e) { UpdateButtonState(); }" />
                                    </dx:ASPxListBox>
                                </td>
                            </tr>
                            <tr>
                                <td colspan="2" align="center">
                                    <dx:ASPxButton ID="btnDeleteUser" UseSubmitBehavior="false" OnClick="btnDeleteUser_Click" runat="server" ClientEnabled="true" ClientInstanceName="btnDeleteUser" Text="Delete" />
                                </td>
                            </tr>
                        </table>
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
            <dx:TabPage Text="SQL Info">
                <ContentCollection>
                    <dx:ContentControl>
                        <table id="SQLInfo" cellspacing="15">
                            <tr>
                                <td colspan="2">
                                    <dx:ASPxLabel runat="server" Text="Aria4XP SQL" CssClass="SqlTitle" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Client User Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtClientUserNameAria4" ID="txtClientUserNameAria4">
                                        <ClientSideEvents TextChanged="function(s,e){ClientUserNameChanged();}" />
                                    </dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Client Password" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtClientPasswordAria4" ID="txtClientPasswordAria4">
                                        <ClientSideEvents TextChanged="function(s,e){ClientPasswordChanged();}" />
                                    </dx:ASPxTextBox>
                                </td>
                            </tr>
                            <tr>
                                <td></td>
                            </tr>
                            <tr>
                                <td colspan="2">
                                    <dx:ASPxLabel runat="server" Text="Client Master SQL" CssClass="SqlTitle" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="DataBase Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtDataBaseNameClientMaster" ID="txtDataBaseNameClientMaster" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Client User Name" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtClientUserNameClientMaster" ID="txtClientUserNameClientMaster" />
                                </td>
                            </tr>
                            <tr>
                                <td>
                                    <dx:ASPxLabel runat="server" Text="Client Password" />
                                </td>
                                <td>
                                    <dx:ASPxTextBox runat="server" ClientInstanceName="txtClientPasswordClientMaster" ID="txtClientPasswordClientMaster" />
                                </td>
                            </tr>
                        </table>
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
            <dx:TabPage Text="Client Applications">
                <ContentCollection>
                    <dx:ContentControl>
                        <dx:ASPxCheckBoxList ID="chkClientApps" DataSourceID="ClientAppsLinqDataSource" TextField="Value" ValueField="Key" ItemSpacing="6" Width="100%" runat="server">
                            <Items>
                            </Items>
                        </dx:ASPxCheckBoxList>
                        <asp:LinqDataSource ID="ClientAppsLinqDataSource" runat="server" OnSelecting="ClientAppsLInqDataSource_Selecting" />
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
            <dx:TabPage Text="Client Roles">
                <ContentCollection>
                    <dx:ContentControl>
                        <dx:ASPxCheckBoxList ID="chkClientRoles" DataSourceID="ClientRolesLinqDataSource" TextField="Value" ValueField="Key" ItemSpacing="6" Width="100%" runat="server">
                            <Items>
                            </Items>
                        </dx:ASPxCheckBoxList>
                        <asp:LinqDataSource ID="ClientRolesLinqDataSource" runat="server" OnSelecting="ClientRolesLinqDataSource_Selecting" />
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
            <dx:TabPage Text="Actions">
                <ContentCollection>
                    <dx:ContentControl>
                        <dx:ASPxCheckBoxList ID="chkSelectedActions" ItemSpacing="6" Width="100%" runat="server">
                            <Items>
                                <dx:ListEditItem Selected="true" Text="Create Client Aria Folder" Value="CreateClientAriaFolder" />
                                <dx:ListEditItem Selected="true" Text="Create Client Sql Folder" Value="CreateClientSQLFolder" />
                                <dx:ListEditItem Selected="true" Text="Copy Aria Source to Client Folder" Value="Copy_AriaSource_ClientFolder" />
                                <dx:ListEditItem Selected="true" Text="Create Client SQL User" Value="CreateClientUser" />
                                <dx:ListEditItem Selected="true" Text="Copy Aria.Master DB to Client SQL Folder" Value="Copy_AriaMaster_ClientSQLFolder" />
                                <dx:ListEditItem Selected="true" Text="Attach Client.Master DB to SQL Server" Value="AttachClientMasterDB" />
                                <dx:ListEditItem Selected="true" Text="Create Aria4XP Companies SQL Databases" Value="CreateAllA4Databases" />
                                <dx:ListEditItem Selected="true" Text="Add Aria4 Companies Rows to Syccompany" Value="AddAllSycCompanyInfo" />
                                <dx:ListEditItem Selected="true" Text="Create Companies dbfs Files using default 99 company" Value="CreateCompanyDbfsFiles" />
                                <dx:ListEditItem Selected="true" Text="Update Sycinst with Current Paths" Value="UpdateSycinst" />
                                <dx:ListEditItem Selected="true" Text="Create Client Active Directory Group" Value="CreateClientGroup" />
                                <dx:ListEditItem Selected="true" Text="Create Client Active Directory Users" Value="CreateClientUsers" />
                                <dx:ListEditItem Selected="true" Text="Adjust Client Shared Folder Security" Value="AdjustClientFolderSecurity" />
                                <dx:ListEditItem Selected="true" Text="Share Client Shared Folder" Value="ShareClientSharedFolder" />
                                <dx:ListEditItem Selected="true" Text="Create Client Settings XML" Value="CreateClientSettingsXML" />
                                <dx:ListEditItem Selected="true" Text="Add Client Row to System.Master Clients Table" Value="AddSystemMasterClientRow" />
                                <dx:ListEditItem Selected="true" Text="Add AriaUser Rows to System.Master" Value="AddSystemMasterAriaUsersRows" />
                                <dx:ListEditItem Selected="true" Text="Delete Old Activiation Key File" Value="DeleteActKey" />
                                <dx:ListEditItem Selected="true" Text="Copy New Activiation Key File" Value="CopyActKey" />
                                <dx:ListEditItem Selected="true" Text="Update Client Selected SydApp" Value="UpdateSydAppl" />
                                <dx:ListEditItem Selected="true" Text="Update Client Selected AriaRoles" Value="AddSystemMasterAriaUsersRoles" />
                                <dx:ListEditItem Selected="true" Text="Update Aria Client Prodcut rows to System.Master" Value="AddSystemMasterClientProductRows" />
                            </Items>
                        </dx:ASPxCheckBoxList>
                        <table id="Actions">
                            <tr></tr>
                        </table>
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
            <dx:TabPage Text="Log" Name="LogTab">
                <ContentCollection>
                    <dx:ContentControl>
                        Request ID:<dx:ASPxTextBox ID="txtRequestID" Style="display: inline-table; width: 100px;" runat="server"></dx:ASPxTextBox>
                        <dx:ASPxButton ID="btnRefreshRequestMemo" UseSubmitBehavior="false" Style="display: inline-table;" runat="server" Text="Refresh" OnClick="btnRefreshRequestMemo_Click"></dx:ASPxButton>
                        <br />
                        <br />
                        <dx:ASPxMemo runat="server" ID="ResultMemo" AutoResizeWithContainer="true" Font-Bold="true" Font-Size="12px" ReadOnly="true" Width="95%" Height="300px"></dx:ASPxMemo>
                    </dx:ContentControl>
                </ContentCollection>
            </dx:TabPage>
        </TabPages>
    </dx:ASPxPageControl>
    <table width="100%">
        <tr align="center">
            <td>
                <dx:ASPxButton ID="btnPrev" ClientInstanceName="btnPrev" Style="display: inline-table;" EnableClientSideAPI="true" AutoPostBack="false" runat="server" Text="Previous">
                    <ClientSideEvents Click="function(s, e) { Prev();}" />
                </dx:ASPxButton>
                &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                        <dx:ASPxButton ID="btnNext" ClientInstanceName="btnNext" Style="display: inline-table;" EnableClientSideAPI="true" AutoPostBack="false" runat="server" Text="Next">
                            <ClientSideEvents Click="function(s, e) { Next();}" />
                        </dx:ASPxButton>

            </td>
        </tr>
        <tr>
            <td>&nbsp;</td>
        </tr>
        <tr>
            <td>&nbsp;</td>
        </tr>
        <tr>
            <td>&nbsp;</td>
        </tr>
        <tr>
            <td>&nbsp;</td>
        </tr>
        <tr>
            <td align="center">
                <dx:ASPxButton ID="btnCreateClient" UseSubmitBehavior="false" ClientInstanceName="btnCreateClient" Width="150px" Height="65px" OnClick="btnCreateClient_Click" EnableClientSideAPI="true" runat="server" Text="Create Client" />
            </td>
        </tr>
    </table>
</div>
<br />
<dx:ASPxLabel ID="lblStatus" Style="padding-left: 50px;" Font-Bold="true" Font-Size="12px" runat="server" Width="100%" />
