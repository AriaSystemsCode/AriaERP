using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.UI;
using System.Web.UI.WebControls;
using DevExpress.Web;

namespace Aria5SystemAdmin.Web.UserControls.ClientManager
{
    public partial class NewClient : System.Web.UI.UserControl
    {
        string displayFormat = "{0}   -   {1}";
        bool ServiceOnline;
        new bool IsPostBack;
        protected void Page_Init(object sender, EventArgs e)
        {
           IsPostBack = Request.Form["__EVENTTARGET"] != "";
           if (IsPostBack)
           {
               lstUsers.Items.AddRange(UsersSource);
               lstCompanies.Items.AddRange(CompaniesSource);
           }
        }
        protected void Page_Load(object sender, EventArgs e)
        {
            lblStatus.Text = "";
            if (!IsPostBack)
            {
                ClearSessions();
                Add99TestCompany();
                CompaniesSource = lstCompanies.Items;
            }
 
            ServiceOnline = CheckIfServiceOnline();
            if (ServiceOnline)
            {
                lblServiceStatus.Text = "Service Online";
                lblServiceStatus.ForeColor = System.Drawing.Color.Green;
            }
            else
            {
                string endpoint = new ClientService.ClientServiceClient().Endpoint.ListenUri.ToString();
                lblServiceStatus.Text = "Service Offline";
                lblServiceStatus.ForeColor = System.Drawing.Color.Red;
                lblServiceStatus.ToolTip = string.Format("Service at {0} couldn't be reached!", endpoint);
            }
            btnCreateClient.Enabled = ServiceOnline;
        }

        private void ClearSessions()
        {
            UsersSource = null;
            CompaniesSource = null;
        }

        protected void Page_PreRender(object sender, EventArgs e)
        {
            UsersSource = lstUsers.Items;
            CompaniesSource = lstCompanies.Items;
        }

        private bool CheckIfServiceOnline()
        {
            try
            {
                ClientService.ClientServiceClient client = new ClientService.ClientServiceClient();
                //if (client.ChannelFactory.State == System.ServiceModel.CommunicationState.Created)
                //{
                //    return true;
                //}
                //else
                //{
                    client.Open();
               // }
                return true;
            }
            catch
            {
                return false;
            }
        }

        private void Add99TestCompany()
        {
            txtNewCompanyCode.Text = "99";
            txtNewCompanyName.Text = "Test Company";
            btnAddCompany_Click(null, null);
        }

        protected void btnAddCompany_Click(object sender, EventArgs e)
        {
            if (txtNewCompanyCode.Text == "")
            {
                lblStatus.Text = "Empty Company Code!";
                return;
            }
            if (txtNewCompanyName.Text == "")
            {
                lblStatus.Text = "Empty Company Name!";
                return;
            }
            if (lstCompanies.Items.FindByValue(txtNewCompanyCode.Text) != null)
            {
                lblStatus.Text = string.Format("Company {0} already Exist!", txtNewCompanyCode.Text);
                return;
            }
            string listText = string.Format(displayFormat, txtNewCompanyCode.Text, txtNewCompanyName.Text);
            lstCompanies.Items.Add(new DevExpress.Web.ListEditItem(listText, txtNewCompanyCode.Text));
            txtNewCompanyName.Text = txtNewCompanyCode.Text = "";
        }

        protected void btnDeleteCompany_Click(object sender, EventArgs e)
        {
            if (lstCompanies.SelectedItems.Count > 0)
            {
                while (lstCompanies.SelectedItems.Count > 0)
                    lstCompanies.Items.Remove(lstCompanies.SelectedItems[0]);
            }
        }

        protected void btnGenerateUsers_Click(object sender, EventArgs e)
        {
            int UsersNo = 0;
            if (!int.TryParse(Convert.ToString(txtUsersNo.Value), out UsersNo))
            {
                lblStatus.Text = "Please enter Correct No Of Users!";
                return;
            }
            if (UsersNo < 1)
            {
                lblStatus.Text = "Minmum no of users should be 1!";
                return;
            }
            if (txtClientCode.Text == "")
            {
                lblStatus.Text = "Client Code is Required!";
                return;
            }
            if (txtClientPhone.Text == "")
            {
                lblStatus.Text = "Client Phone is Required!";
                return;
            }
            string defaultPassword = txtClientCode.Text.Substring(0, 3).ToLower() + "_" + txtClientPhone.Text;

            for (int x = 0; x < UsersNo; x++)
            {
                int UserIndex = 1;
                string userNameFormat = "{0}_U{1}";
                while (lstUsers.Items.FindByValue(string.Format(userNameFormat, txtClientCode.Text, UserIndex)) != null)
                    UserIndex++;
                string userName = string.Format(userNameFormat, txtClientCode.Text, UserIndex);

                string listText = string.Format(displayFormat, userName, defaultPassword);
                lstUsers.Items.Add(new ListEditItem(listText, userName));

                txtUsersNo.Number = 0;
            }
        }

        protected void btnAddUser_Click(object sender, EventArgs e)
        {
            if (txtNewUserName.Text == "")
            {
                lblStatus.Text = "Empty User Name!";
                return;
            }
            if (txtNewUserPassword.Text == "")
            {
                lblStatus.Text = "Empty User Password!";
                return;
            }
            if (lstUsers.Items.FindByValue(txtNewUserName.Text) != null)
            {
                lblStatus.Text = string.Format("User {0} already Exist!", txtNewUserName.Text);
                return;
            }

            string listText = string.Format(displayFormat, txtNewUserName.Text, txtNewUserPassword.Text);
            lstUsers.Items.Add(new ListEditItem(listText, txtNewUserName.Text));
            txtNewUserName.Text = txtNewUserPassword.Text = "";
        }

        protected void btnDeleteUser_Click(object sender, EventArgs e)
        {
            if (lstUsers.SelectedItems.Count > 0)
            {
                while (lstUsers.SelectedItems.Count > 0)
                    lstUsers.Items.Remove(lstUsers.SelectedItems[0]);
            }
        }

        protected void ClientAppsLInqDataSource_Selecting(object sender, LinqDataSourceSelectEventArgs e)
        {
            if (ServiceOnline)
            {
                Settings settings = Settings.Load();
                if (!string.IsNullOrWhiteSpace(settings.AriaSourcePath))
                {
                    DataService.DataServiceClient client = new DataService.DataServiceClient();
                   //var appstest = Core.Utilites.DataHelper.GetSydApp("D:\\");
                    var apps = client.GetSydApp(settings.AriaSourcePath);
                    e.Result = apps;
                }
            }
            else
                e.Result = new List<string>();
        }

        protected void ClientRolesLinqDataSource_Selecting(object sender, LinqDataSourceSelectEventArgs e)
        {
            if (ServiceOnline)
            {
                Settings settings = Settings.Load();
                if (settings.SystemMaster != null &&
                    settings.SystemMaster.Admin != null &&
                    !string.IsNullOrWhiteSpace(settings.SystemMaster.ServerName) &&
                    !string.IsNullOrWhiteSpace(settings.SystemMaster.DataBaseName) &&
                    !string.IsNullOrWhiteSpace(settings.SystemMaster.Admin.UserName) &&
                    !string.IsNullOrWhiteSpace(settings.SystemMaster.Admin.Password)
                    )
                {
                    DataService.DataServiceClient client = new DataService.DataServiceClient();
                    var roles = client.GetRoles(settings.SystemMaster);
                    e.Result = roles;
                }
            }
            else
                e.Result = new List<string>();
        }

        protected void btnCreateClient_Click(object sender, EventArgs e)
        {
            #region Validation

            if (txtClientCode.Text == "")
            {
                lblStatus.Text = "Client Code is Required!";
                return;
            }
            if (txtClientPhone.Text == "")
            {
                lblStatus.Text = "Client Phone is Required!";
                return;
            }

            if (txtClientName.Text == "")
            {
                lblStatus.Text = "Client Name is Required!";
                return;
            }
            if (lstCompanies.Items.Count == 0)
            {
                lblStatus.Text = "Client Companies is Required!";
                return;
            }

            if (lstUsers.Items.Count == 0)
            {
                lblStatus.Text = "Client Users is Required!";
                return;
            }

            if (txtClientUserNameAria4.Text == "")
            {
                lblStatus.Text = "Client Aria4 SQL UserName is Required!";
                return;
            }

            if (txtClientPasswordAria4.Text == "")
            {
                lblStatus.Text = "Client Aria4 SQL Password is Required!";
                return;
            }

            if (txtDataBaseNameClientMaster.Text == "")
            {
                lblStatus.Text = "Client ClientMaster SQL DatabaseName is Required!";
                return;
            }

            if (txtClientUserNameClientMaster.Text == "")
            {
                lblStatus.Text = "Client ClientMaster SQL UserName is Required!";
                return;
            }

            if (txtClientPasswordClientMaster.Text == "")
            {
                lblStatus.Text = "Client ClientMaster SQL Password is Required!";
                return;
            }

            if (chkSelectedActions.SelectedItems.Count == 0)
            {
                lblStatus.Text = "Client Actins is Required!";
                return;
            }
            #endregion

            Core.Utilites.Settings settings = new Core.Utilites.Settings();
            Settings ConfigSettings = Settings.Load();

            settings.Aria4MappedPath = ConfigSettings.AriaMappedPath;
            settings.AriaMasterSourceDirectoryPath = ConfigSettings.AriaMasterPath;
            settings.AriaSourceDirectoryPath = ConfigSettings.AriaSourcePath;
            settings.SharedDirectoryPath = ConfigSettings.AriaSharedPath;
            settings.SharedDirectoryLocalPath = ConfigSettings.AriaSharedLocalPath;
            settings.SharedServerName = ConfigSettings.AriaSharedServerName;
            settings.SqlDBDirectoryPath = ConfigSettings.AriaSQLDBPath;
            settings.DomainName = ConfigSettings.DomainName;
            settings.DomainAdminUserName = ConfigSettings.DomainAdminUserName;
            settings.DomainAdminPassword = ConfigSettings.DomainAdminUserPassword;
            settings.Aria4SqlInfo = ConfigSettings.Aria4;
            settings.SystemMasterSqlInfo = ConfigSettings.SystemMaster;
            settings.ClientMasterSqlInfo = ConfigSettings.ClientMaster;

            settings.ClientMasterSqlInfo.Client = new Core.Utilites.Credential(txtClientUserNameClientMaster.Text, txtClientPasswordClientMaster.Text);
            settings.ClientMasterSqlInfo.DataBaseName = txtDataBaseNameClientMaster.Text;

            settings.Aria4SqlInfo.Client = new Core.Utilites.Credential(txtClientUserNameAria4.Text, txtClientPasswordAria4.Text);

            settings.ClientCode = txtClientCode.Text;
            settings.ClientName = txtClientName.Text;
            settings.ClientPhone = txtClientPhone.Text;

            settings.ActivationKeyFilePath = txtActivationKeyPath.Text;
            settings.Companies = new List<Core.Utilites.Company>();
            foreach (ListEditItem item in lstCompanies.Items)
            {
                var company = new Core.Utilites.Company();

                company.Code = Convert.ToString(item.Value);
                company.Name = item.Text.Replace(string.Format(displayFormat, company.Code, ""), "");
                settings.Companies.Add(company);
            }

            settings.ClientUsers = new List<Core.Utilites.Credential>();
            foreach (ListEditItem item in lstUsers.Items)
            {
                string userName = Convert.ToString(item.Value);
                string password = item.Text.Replace(string.Format(displayFormat, userName, ""), "");
                var user = new Core.Utilites.Credential(userName, password);
                settings.ClientUsers.Add(user);
            }

            settings.SelectedApps = new List<string>();
            foreach (ListEditItem item in chkClientApps.SelectedItems)
            {
                settings.SelectedApps.Add(Convert.ToString(item.Value));
            }

            settings.ClientSelectedRoles = new List<string>();
            foreach (ListEditItem item in chkClientRoles.SelectedItems)
            {
                settings.ClientSelectedRoles.Add(Convert.ToString(item.Value));
            }

            settings.NeededActions = new List<Core.Utilites.Actions>();
            foreach (ListEditItem item in chkSelectedActions.SelectedItems)
            {
                string enumString = Convert.ToString(item.Value);
                Core.Utilites.Actions outValue;
                if (Enum.TryParse<Core.Utilites.Actions>(enumString, true, out outValue))
                {
                    settings.NeededActions.Add(outValue);
                }
            }
            //Core.Business.DB xx = new Core.Business.DB(settings);
            //xx.AddSystemMasterClientRow();
            ClientService.ClientServiceClient clientService = new ClientService.ClientServiceClient();
            var response = clientService.CreateClient(settings);
            string requestid = response.Content[0];
            AddLogLine(string.Format("Your Request has been receieved and assigned Id:{0}", requestid));
            txtRequestID.Text = requestid;
            MainContentASPxPageControl.ActiveTabPage = MainContentASPxPageControl.TabPages.FindByName("LogTab");

        }

        protected void AddLogLine(string text)
        {
            ResultMemo.Text += text + Environment.NewLine;
        }

        protected void btnRefreshRequestMemo_Click(object sender, EventArgs e)
        {
            if (txtRequestID.Text != "")
            {
                string requestID = txtRequestID.Text;
                LogService.LogServiceClient logService = new LogService.LogServiceClient();
                var response = logService.QueryRequest(requestID);
                AddLogLine("");
                AddLogLine("");
                foreach (var line in response.Content)
                    AddLogLine(line);
                AddLogLine(string.Format("Current Request Status : {0}", response.Status.ToString()));
            }
        }

        private ListEditItemCollection UsersSource
        {
            get
            {
                if (Session["UsersSource"] == null)
                    Session["UsersSource"] = new ListEditItemCollection();

                return Session["UsersSource"] as ListEditItemCollection;
            }
            set
            {
                Session["UsersSource"] = value;
            }
        }

        private ListEditItemCollection CompaniesSource
        {
            get
            {
                if (Session["CompaniesSource"] == null)
                    Session["CompaniesSource"] = new ListEditItemCollection();

                return Session["CompaniesSource"] as ListEditItemCollection;
            }
            set
            {
                Session["CompaniesSource"] = value;
            }
        }

        private ListEditItemCollection ClientAppsSource
        {
            get
            {
                if (Session["ClientAppsSource"] == null)
                    Session["ClientAppsSource"] = new ListEditItemCollection();

                return Session["ClientAppsSource"] as ListEditItemCollection;
            }
            set
            {
                Session["ClientAppsSource"] = value;
            }
        }


    }
}