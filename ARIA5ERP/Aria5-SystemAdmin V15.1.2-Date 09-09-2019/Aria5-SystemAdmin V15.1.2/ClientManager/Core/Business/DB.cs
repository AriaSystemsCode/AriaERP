using Core.Utilites;
using System;
using System.Collections.Generic;
using System.Data;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Data.Odbc;

namespace Core.Business
{
    public class DB
    {
        public DB(Settings settings)
        {
            this.Settings = settings;
        }

        public Settings Settings { get; set; }

        public void CreateClientUser()
        {
            string scriptPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().CodeBase.Replace("file:///", "")).EndsWithBackSlash();
            scriptPath += @"Resources\SqlScripts\CreateUser.sql";
            Utilites.SqlHelper.ExecuteScriptFile(Settings.Aria4SqlInfo.GetMasterConnection(), scriptPath, Settings.Aria4SqlInfo.Client.UserName, Settings.Aria4SqlInfo.Client.Password);
        }

        public void AttachClientMasterDB()
        {
            string mdfFilePath = Settings.ClientSQLDirectory.EndsWithBackSlash() + string.Format("{0}.mdf", Settings.ClientMasterSqlInfo.DataBaseName);
            string ldfFilePath = Settings.ClientSQLDirectory.EndsWithBackSlash() + string.Format("{0}_log.ldf", Settings.ClientMasterSqlInfo.DataBaseName);

            string scriptPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().CodeBase.Replace("file:///", "")).EndsWithBackSlash();
            scriptPath += @"Resources\SqlScripts\AttachClientMaster.sql";
            Utilites.SqlHelper.ExecuteScriptFile(Settings.Aria4SqlInfo.GetMasterConnection(), scriptPath, Settings.ClientMasterSqlInfo.DataBaseName, mdfFilePath, ldfFilePath);
        }

        public void CreateAllA4Databases()
        {
            foreach (Company company in Settings.Companies)
            {
                CreateA4CompanyDatabase(company);
            }
        }

        public void AddAllSycCompanyInfo()
        {
            foreach (Company company in Settings.Companies)
            {
                AddSycCompanyInfo(company);
            }
        }

        //Create new Aria4 Sql DB in client Sql folder and add Client sql user as dbowner in it
        public void CreateA4CompanyDatabase(Company company)
        {
            string dbName = Settings.ClientAria4DBName(company.Code);
            string physicalDBPath = Settings.ClientSQLDirectory.EndsWithBackSlash() + dbName;
            string scriptPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().CodeBase.Replace("file:///", "")).EndsWithBackSlash();
            scriptPath += @"Resources\SqlScripts\CreateDatabase.sql";
            Utilites.SqlHelper.ExecuteScriptFile(Settings.Aria4SqlInfo.GetMasterConnection(), scriptPath, dbName, physicalDBPath, Settings.Aria4SqlInfo.Client.UserName);
        }

        //insert record in Syccomp for paramter Company
        public void AddSycCompanyInfo(Company company)
        {
            using (var command = Settings.SysFilesConnection.CreateCommand())
            {
                command.CommandText = string.Format("select ccomp_id from syccomp where ccomp_id = '{0}'", company.Code);
                DataTable installedcompanies = new DataTable();
                OdbcDataAdapter da = new OdbcDataAdapter(command);
                da.Fill(installedcompanies);
                string sqlStatment = "";
                if (installedcompanies.Rows.Count > 0)
                {
                    sqlStatment = "UPDATE syccomp SET ";
                    sqlStatment += "Ccom_name = '{1}'" + ",";
                    sqlStatment += "Ccom_ddir = '{2}'" + ",";
                    sqlStatment += "Ccondriver = 'SQL'" + ",";
                    sqlStatment += "cconserver = '{3}'" + ",";
                    sqlStatment += "ccondbname = '{4}'" + ",";
                    sqlStatment += "cconuserid = '{5}'" + ",";
                    sqlStatment += "cconpaswrd = '{6}'" + ",";
                    sqlStatment += "mcomp_mdl = '{7}'" + ",";
                    sqlStatment += "mmodlset = '{8}'" + ",";
                    sqlStatment += "Lrunfroma4=1";
                    sqlStatment += " WHERE ccomp_id='{0}'";
                }
                else
                {
                    sqlStatment = "insert INTO syccomp (ccomp_id,Ccom_name,Ccom_ddir,Ccondriver,cconserver,ccondbname,cconuserid,cconpaswrd,mcomp_mdl,mmodlset,Lrunfroma4)";
                    sqlStatment += " VALUES ('{0}','{1}','{2}','SQL','{3}','{4}','{5}','{6}','{7}','{8}',1)";
                  
                }
                sqlStatment = string.Format(sqlStatment,
                                                  company.Code,
                                                  company.Name,
                                                  string.Format("{0}DBFS\\{1}", Settings.Aria4MappedPath.EndsWithBackSlash(), company.Code.EndsWithBackSlash()),
                                                  Settings.Aria4SqlInfo.ServerName,
                                                  Settings.ClientAria4DBName(company.Code),
                                                  Settings.Aria4SqlInfo.Client.UserName,
                                                  Settings.Aria4SqlInfo.Client.Password,
                                                  Settings.SelectedApps.Count > 0 ? Settings.SelectedApps.Aggregate((x, y) => x + "|" + y) : "",
                                                  Settings.SelectedApps.Count > 0 ? Settings.SelectedApps.Aggregate((x, y) => x + "," + y) : "");
                command.CommandText = sqlStatment;
                command.Connection.Open();
                command.ExecuteNonQuery();
                command.Connection.Close();
            }
        }

        public void AddSystemMasterClientRow()
        {
            Data.SystemMaster db = new Data.SystemMaster(Settings.SystemMasterSqlInfo);
            if (db.CLIENTS.Any(x => x.CCLIENTID == Settings.ClientCode.ToUpper())) return;
            Data.Client client = new Data.Client();
            client.CCLIENTID = Settings.ClientCode.ToUpper();
            client.CCLIENTNAME = Settings.ClientName;
            //ATA replace the data path with shared folder path instead the client data path direct 4/5/2017 [start]
            client.CDATAPATH = Settings.SharedDirectoryPath.Replace("shared\\","").EndsWithBackSlash() + Settings.ClientCode + "SH\\"; 
           // client.CDATAPATH = Settings.ClientSharedDirectory.ToUpper().EndsWithBackSlash();
            //ATA replace the data path with shared folder path instead the client data path direct 4/5/2017 [End]            
            client.LLOCKSYS = false;
            client.CCONSERVER = Settings.ClientMasterSqlInfo.ServerName;
            client.CCONDBNAME = Settings.ClientMasterSqlInfo.DataBaseName;
            client.CCONUSERID = Settings.ClientMasterSqlInfo.Admin.UserName;
            client.CCONPASWRD = Settings.ClientMasterSqlInfo.Admin.Password;
            //ATA replace the data path with shared folder path instead the client data path direct 4/5/2017 [start]
            client.ARIA27SYS = Settings.SharedDirectoryPath.Replace("shared\\", "").EndsWithBackSlash() + Settings.ClientCode + "SH\\" + "ARIA4XP\\SYSFILES\\";
            client.ARIA40SYS = Settings.SharedDirectoryPath.Replace("shared\\", "").EndsWithBackSlash() + Settings.ClientCode + "SH\\" + "ARIA4XP\\SYSFILES\\";
            //client.ARIA27SYS = Settings.ClientSharedDirectory.EndsWithBackSlash() + "ARIA4XP\\SYSFILES\\";
            //client.ARIA40SYS = Settings.ClientSharedDirectory.EndsWithBackSlash() + "ARIA4XP\\SYSFILES\\";
            //ATA replace the Sysfiles path with shared folder path instead the client data path direct 4/5/2017 [ENd]

            db.CLIENTS.Add(client);
            db.SaveChanges();
        }

        public void AddSystemMasterAriaUsersRows()
        {
            foreach (Credential user in Settings.ClientUsers)
            {
                AddSystemMasterAriaUserRow(user);
            }
        }

        public void AddSystemMasterAriaUserRow(Credential User)
        {
            Data.SystemMaster db = new Data.SystemMaster(Settings.SystemMasterSqlInfo);
            if (db.AriaUsers.Any(x => x.UserId == User.UserName)) return;
            Data.AriaUser AriaUser = new Data.AriaUser();
            AriaUser.UserId = User.UserName;
            AriaUser.MenuId = 17;
            AriaUser.ClientID = Settings.ClientCode;
            AriaUser.Description = User.Password;
            db.AriaUsers.Add(AriaUser);
            db.SaveChanges();
        }

        public void AddSystemMasterAriaUsersRoles()
        {
            foreach (string role in Settings.ClientSelectedRoles)
            {
                foreach (Credential user in Settings.ClientUsers)
                {
                    AddSystemMasterAriaUserRoleRow(user, role);
                }
            }
        }

        public void AddSystemMasterAriaClientProductRows()
        {
            foreach (string role in Settings.ClientSelectedRoles)
            {
                AddSystemMasterAriaClientProductRow(role.Trim());
            }
        }

        public void AddSystemMasterAriaUserRoleRow(Credential User, string RoleID)
        {
            Data.SystemMaster db = new Data.SystemMaster(Settings.SystemMasterSqlInfo);
            if (db.AriaUserRoles.Any(x => x.UserId == User.UserName && x.RoleId == RoleID)) return;
            Data.AriaUserRole userRole = new Data.AriaUserRole();
            userRole.UserId = User.UserName;
            userRole.RoleId = RoleID;
            db.AriaUserRoles.Add(userRole);
            db.SaveChanges();
        }

        public void AddSystemMasterAriaClientProductRow(string RoleID)
        {
            string productSettingsPath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().CodeBase.Replace("file:///", "")).EndsWithBackSlash();
            productSettingsPath += string.Format(@"Resources\ProductSettings\{0}.xml", RoleID);
            if (!File.Exists(productSettingsPath)) return;
            Data.SystemMaster db = new Data.SystemMaster(Settings.SystemMasterSqlInfo);
            if (db.AriaClientProducts.Any(x => x.ClientId == Settings.ClientCode && x.ProductId == RoleID)) return;
            Data.AriaClientProduct clientProduct = new Data.AriaClientProduct();
            clientProduct.ClientId = Settings.ClientCode;
            clientProduct.ProductId = RoleID;
            clientProduct.ProductSettings = File.ReadAllText(productSettingsPath);
            db.AriaClientProducts.Add(clientProduct);
            db.SaveChanges();
        }

        public void UpdateSydAppl()
        {
            using (var cmd = Settings.SysFilesConnection.CreateCommand())
            {
                var apps = Settings.SelectedApps.Select(x => "'" + x + "'").ToList().Aggregate((x, y) => x + "," + y);
                cmd.CommandText = string.Format("DELETE FROM sydappl where capp_id not in({0})", apps);
                cmd.Connection.Open();
                cmd.ExecuteNonQuery();
                cmd.Connection.Close();
            }
        }

        public void UpdateSycinst()
        {
            using (var cmd = Settings.SysFilesConnection.CreateCommand())
            {
                string pathPattern = Settings.Aria4MappedPath + "{0}\\";
                cmd.CommandText = "Update SycInst set ";
                cmd.CommandText += string.Format("ccompath ='" + pathPattern + "'", "DBFS") + ",";
                cmd.CommandText += string.Format("cservpath ='" + pathPattern + "'", "SPACK") + ",";
                cmd.CommandText += string.Format("cinsysfdr ='" + pathPattern + "'", "SYSFILES") + ",";
                cmd.CommandText += string.Format("cimagdir ='" + pathPattern + "'", "IMAGES") + ",";
                cmd.CommandText += string.Format("cscrdir ='" + pathPattern + "'", "SCREENS") + ",";
                cmd.CommandText += string.Format("cinsallcmp ='" + pathPattern + "'", "DBFS") + ",";
                cmd.CommandText += string.Format("cinsrsrdr ='" + pathPattern + "'", "RESOURCE") + ",";
                cmd.CommandText += string.Format("cinsdospd ='" + pathPattern + "'", "DOSAPPS") + ",";
                cmd.CommandText += string.Format("cinsdosrd ='" + pathPattern + "'", "REPORTS") + ",";
                cmd.CommandText += string.Format("cinsdoswd ='" + pathPattern + "'", "WORK") + ",";
                cmd.CommandText += string.Format("cinswinpd ='" + pathPattern + "'", "PRGS") + ",";
                cmd.CommandText += string.Format("cinswinrd ='" + pathPattern + "'", "REPORTS") + ",";
                cmd.CommandText += string.Format("cinswinwd ='" + pathPattern + "'", "WORK") + ",";
                cmd.CommandText += string.Format("cinswinbm ='" + pathPattern + "'", "BMPS") + ",";
                cmd.CommandText += string.Format("cclassdir ='" + pathPattern + "'", "CLASSES") + ",";
                cmd.CommandText += string.Format("cedipath  ='" + pathPattern + "'", "EDI") + ",";
                cmd.CommandText += string.Format("ca4sysdir  ='" + pathPattern + "'", "SYSFILES");
                cmd.Connection.Open();
                cmd.ExecuteNonQuery();
                cmd.Connection.Close();
            }
        }
    }
}