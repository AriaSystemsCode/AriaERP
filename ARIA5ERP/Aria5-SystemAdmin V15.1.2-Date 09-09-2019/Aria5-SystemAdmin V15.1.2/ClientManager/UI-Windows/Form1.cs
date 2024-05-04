using System;
using System.Linq;
using System.Windows.Forms;

namespace UI_Windows
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void btnStart_Click(object sender, EventArgs e)
        {
            Core.Utilites.Settings Settings = new Core.Utilites.Settings();

            //ClientService.ClientServiceClient client = new ClientService.ClientServiceClient();

            Settings.ClientCode = txtClientCode.Text;
            Settings.ClientName = "My Test Name";
            Settings.AriaSourceDirectoryPath = @"\\303842-VM4\Distribution\"; //txtAriaSourcePath.Text;
            Settings.AriaMasterSourceDirectoryPath = @"\\303842-VM4\SQL_DATABASES\Client.Master\";// txtAriaMasterSourcePath.Text;
            Settings.SharedDirectoryPath = @"\\303842-VM4\shared\";// txtSharedPath.Text;
            Settings.SqlDBDirectoryPath = @"\\303842-VM4\SQL_DATABASES\";// txtSqlDBPath.Text;
            Settings.SharedDirectoryLocalPath = @"D:\Shared\";
            Settings.SharedServerName = "303842-VM4";
            Settings.Aria4MappedPath = "X:\\Aria4xp\\";
            Settings.ClientPhone = "01114370437";
            Settings.ActivationKeyFilePath = @"\\303842-VM4\Shared\Distribution\Aria4XP\SYSFILES\act_key.bin";
            Settings.ClientSelectedRoles = new string[] { "ARIA4XP", "AriaEDI", "ARIAERP" }.ToList();
            string password = Settings.ClientCode.Substring(0, 3).ToLower() + "_" + Settings.ClientPhone;
            Settings.ClientUsers = new Core.Utilites.Credential[] { new Core.Utilites.Credential(Settings.ClientCode + "1", password), new Core.Utilites.Credential(Settings.ClientCode + "2", password) }.ToList();
            Settings.Companies = new Core.Utilites.Company[] { new Core.Utilites.Company() { Code = "01", Name = "My 01 Test" }, new Core.Utilites.Company() { Code = "02", Name = "My 02 Test" } }.ToList();
            Settings.DomainAdminUserName = "crm";
            Settings.DomainAdminPassword = "aria_1234";
            Settings.DomainName = "Aria";

            Settings.SelectedApps = new string[] { "AL", "IC", "SM", "HR" }.ToList();

            Settings.SystemMasterSqlInfo = new Core.Utilites.SQLInfo();
            Settings.ClientMasterSqlInfo = new Core.Utilites.SQLInfo();
            Settings.Aria4SqlInfo = new Core.Utilites.SQLInfo();

            Settings.Aria4SqlInfo.Admin = new Core.Utilites.Credential("crm", "crm");// new Core.Utilites.Credential(txtSqlUserName.Text, txtSqlPassword.Text);
            Settings.Aria4SqlInfo.Client = new Core.Utilites.Credential(Settings.ClientCode, Settings.ClientCode.Substring(0, 3).ToLower());
            Settings.Aria4SqlInfo.ServerName = @"303842-VM4\ARIASQLSERVER";// txtServerName.Text;
            //Settings.Aria4SqlInfo.ServerName = @".\SqlExpress";// txtServerName.Text;

            Settings.ClientMasterSqlInfo.Admin = new Core.Utilites.Credential("crm", "crm");// new Core.Utilites.Credential(txtSqlUserName.Text, txtSqlPassword.Text);
            Settings.ClientMasterSqlInfo.Client = new Core.Utilites.Credential(Settings.ClientCode, Settings.ClientCode.Substring(0, 3).ToLower());
            Settings.ClientMasterSqlInfo.ServerName = @"303842-VM4\ARIASQLSERVER";// txtServerName.Text;
            //Settings.ClientMasterSqlInfo.ServerName = @".\SqlExpress";// txtServerName.Text;

            Settings.SystemMasterSqlInfo.Admin = new Core.Utilites.Credential("crm", "crm");// new Core.Utilites.Credential(txtSqlUserName.Text, txtSqlPassword.Text);
            Settings.SystemMasterSqlInfo.Client = new Core.Utilites.Credential(Settings.ClientCode, Settings.ClientCode.Substring(0, 3).ToLower());
            Settings.SystemMasterSqlInfo.ServerName = @"303842-VM4\ARIASQLSERVER";// txtServerName.Text;
            //Settings.SystemMasterSqlInfo.ServerName = @".\SqlExpress";// txtServerName.Text;

            Settings.SystemMasterSqlInfo.DataBaseName = "System.Master";// "System.Master";
            Settings.ClientMasterSqlInfo.DataBaseName = Settings.ClientCode + ".Master";

            #region Needed Actions
            //Settings.NeededActions = new List<Core.Utilites.Actions>();
            //Settings.NeededActions.Add(Core.Utilites.Actions.CreateClientAriaFolder);
            //Settings.NeededActions.Add(Core.Utilites.Actions.CreateClientSQLFolder);
            //Settings.NeededActions.Add(Core.Utilites.Actions.Copy_AriaSource_ClientFolder);
            //Settings.NeededActions.Add(Core.Utilites.Actions.CreateClientUser);
            //Settings.NeededActions.Add(Core.Utilites.Actions.Copy_AriaMaster_ClientSQLFolder);
            //Settings.NeededActions.Add(Core.Utilites.Actions.AttachClientMasterDB);
            //Settings.NeededActions.Add(Core.Utilites.Actions.CreateAllA4Databases);
            Settings.NeededActions.Add(Core.Utilites.Actions.AddAllSycCompanyInfo);
            //Settings.NeededActions.Add(Core.Utilites.Actions.CreateCompanyDbfsFiles);
            //Settings.NeededActions.Add(Core.Utilites.Actions.UpdateSycinst);
            //Settings.NeededActions.Add(Core.Utilites.Actions.CreateClientGroup);
            //Settings.NeededActions.Add(Core.Utilites.Actions.CreateClientUsers);
            //Settings.NeededActions.Add(Core.Utilites.Actions.AdjustClientFolderSecurity);
            //Settings.NeededActions.Add(Core.Utilites.Actions.ShareClientSharedFolder);
            //Settings.NeededActions.Add(Core.Utilites.Actions.CreateClientSettingsXML);
            //Settings.NeededActions.Add(Core.Utilites.Actions.AddSystemMasterClientRow);
            //Settings.NeededActions.Add(Core.Utilites.Actions.AddSystemMasterAriaUsersRows);
            //Settings.NeededActions.Add(Core.Utilites.Actions.AddSystemMasterAriaUsersRoles);
            //Settings.NeededActions.Add(Core.Utilites.Actions.AddSystemMasterClientProductRows);
            //Settings.NeededActions.Add(Core.Utilites.Actions.DeleteActKey);
            ////Settings.NeededActions.Add(Core.Utilites.Actions.CopyActKey);
            //Settings.NeededActions.Add(Core.Utilites.Actions.UpdateSydAppl);

            #endregion

            Core.Business.Main main = new Core.Business.Main();

            main.CreateClient(Settings);

            //var response = await client.CreateClientAsync(Settings);
            //string msg = "";
            //if (response.Content != null)
            //    response.Content.Aggregate((x, y) => msg = x + " , " + y);
            //MessageBox.Show(msg);

            //LogService.LogServiceClient logClient = new LogService.LogServiceClient();

            //var queryResponse = logClient.QueryRequest(response.Content.FirstOrDefault());
            //if (queryResponse.Content != null)
            //    MessageBox.Show(queryResponse.Content.Aggregate((x, y) => msg = x + " , " + y));
        }

        private void button1_Click(object sender, EventArgs e)
        {

        }
    }
}
