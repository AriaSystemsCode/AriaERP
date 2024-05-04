using System;
using System.Collections.Generic;
using System.DirectoryServices;
using System.DirectoryServices.ActiveDirectory;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Core.Utilites;
using System.Management;
using System.IO;
using System.Security.AccessControl;
using System.Security.Principal;

namespace Core.Business
{
    public class AD
    {
        public AD(Settings settings)
        {
            this.Settings = settings;
            directoryEntry = new DirectoryEntry(
                "LDAP://" + Settings.DomainName,
                Settings.DomainAdminUserName,
                Settings.DomainAdminPassword);

            directorySearcher = new DirectorySearcher(directoryEntry);
        }

        public DirectoryEntry directoryEntry { get; set; }

        public DirectorySearcher directorySearcher { get; set; }

        public Settings Settings { get; set; }

        private bool CheckUserExist(string userName)
        {
            return GetUser(userName).Count > 0;
        }

        private bool CheckGroupExist(string groupName)
        {
            return GetGroup(groupName).Count > 0;
        }

        private SearchResultCollection GetGroup(string groupName)
        {
            string query = string.Format("(&(objectCategory=group)(name={0}))", groupName);
            var result = Search(query);
            return result;
        }

        private SearchResultCollection GetUser(string userName)
        {
            string query = string.Format("(&(objectCategory=person)(objectClass=user)(name={0}))", userName);
            var result = Search(query);
            return result;
        }

        private SearchResultCollection Search(string query)
        {
            directorySearcher.Filter = query;
            var sResultSet = directorySearcher.FindAll();
            return sResultSet;
        }


        private string GetProperty(SearchResult searchResult, string PropertyName)
        {
            if (searchResult.Properties.Contains(PropertyName))
            {
                return searchResult.Properties[PropertyName][0].ToString();
            }
            else
            {
                return string.Empty;
            }
        }

        public void CreateClientGroup()
        {
            string GroupName = Settings.ClientName;
            if (CheckGroupExist(GroupName)) return;
            DirectoryEntry saasClientsOU = directoryEntry.Children.Find("OU=SAAS Clients");
            DirectoryEntry saasClientsGroup = saasClientsOU.Children.Find("CN=SAAS Clients", "group");
            using (var newGroup = saasClientsOU.Children.Add(string.Format("CN={0}", GroupName), "group"))
            {
                AdGroupType gt = AdGroupType.GlobalGrp | AdGroupType.SecurityGrp;
                int typeNum = (int)gt;
                newGroup.Properties["sAMAccountName"].Add(GroupName);
                newGroup.Properties["description"].Add(GroupName);
                newGroup.Properties["groupType"].Add(typeNum);
                newGroup.CommitChanges();//Create Group under OU SAAS Clients

                saasClientsGroup.Invoke("Add", new object[] { newGroup.Path.ToString() });
                saasClientsGroup.CommitChanges();//Add Group to Saas Clients Group members
            }
        }

        public void CreateClientUsers()
        {

            foreach (Credential user in Settings.ClientUsers)
            {
                CreateUser(user.UserName, user.Password, Settings.ClientName);
            }
        }

        public void CreateSupportUser()
        {
            string userName = string.Format("{0}_A1", Settings.ClientCode);
            string password = Settings.ClientCode.Substring(0, 3).ToLower() + "_" + Settings.ClientPhone;
            CreateUser(userName, password, Settings.ClientName);
        }

        private void CreateUser(string UserName, string Password, string GroupName)
        {
            DirectoryEntry saasClientsOU = directoryEntry.Children.Find("OU=SAAS Clients");
            if (CheckUserExist(UserName)) return;
            using (var newUser = saasClientsOU.Children.Add(string.Format("CN={0}", UserName), "user"))
            {
                newUser.Properties["sAMAccountName"].Add(UserName);
                newUser.Properties["homedrive"].Add("X:");
                newUser.Properties["homedirectory"].Add(Settings.SharedServerName.EndsWithBackSlash() + Settings.ClientCode + "SH");
                newUser.Properties["displayName"].Add(UserName);
                newUser.Properties["description"].Add(Password);
                ActiveDs.IADsUser iADsUser = (ActiveDs.IADsUser)newUser.NativeObject;
                TSUSEREXLib.IADsTSUserEx TerminalUser = (TSUSEREXLib.IADsTSUserEx)iADsUser;
                TerminalUser.TerminalServicesInitialProgram = @"D:\shared\Aria27\AAS_WIN.EXE"; //For Example
                TerminalUser.TerminalServicesWorkDirectory = @"D:\shared\Aria27\";
                newUser.CommitChanges();//Create User

                newUser.Invoke("SetPassword", new object[] { Password });
                newUser.CommitChanges();//Set User Password

                int ADS_UF_NORMAL_ACCOUNT = 0x0200;//Normal Account
                int ADS_UF_PASSWD_CANT_CHANGE = 0x000000040;//User cannot change password
                int ADS_UF_DONT_EXPIRE_PASSWD = 0x00010000;//Password Never Expires
                int combinedFlag = ADS_UF_NORMAL_ACCOUNT | ADS_UF_DONT_EXPIRE_PASSWD | ADS_UF_PASSWD_CANT_CHANGE;
                newUser.Properties["userAccountControl"].Value = combinedFlag;
                newUser.CommitChanges(); // Enable User Account

                DirectoryEntry clientGroup = saasClientsOU.Children.Find("CN=" + GroupName, "group");
                clientGroup.Invoke("Add", new object[] { newUser.Path.ToString() });
                clientGroup.CommitChanges();//Add Created User to Client Group
            }
        }

        public void ShareClientSharedFolder()
        {
            string UncPath =Settings.ClientSharedDirectory.TrimEnd('\\');
            string Localpath = Settings.ClientSharedDirectoryLocalPath.TrimEnd('\\');
            string ServerName = Settings.SharedServerName;
            string ShareName = Settings.ClientCode + "SH";
            string ShareDesc = Settings.ClientCode;
            ManagementBaseObject outParams;
            ManagementPath p = new ManagementPath(string.Format("\\\\{0}\\root" + "\\cimv2:Win32_Share", ServerName));
            ManagementScope scope = new ManagementScope(string.Format("\\\\{0}\\root\\cimv2", ServerName));
            scope.Connect();
            ManagementClass wmiShare = new ManagementClass(scope, p, new ObjectGetOptions());
            ManagementBaseObject inParams = wmiShare.GetMethodParameters("Create");
            inParams["Path"] = Localpath;
            inParams["Name"] = ShareName;
            inParams["Type"] = 0x0; // Disk Drive
            inParams["Description"] = ShareDesc;
            inParams["MaximumAllowed"] = null;	//setting a property
            inParams["Password"] = null;
            inParams["Access"] = null; // Make Everyone has full control access.
            outParams = wmiShare.InvokeMethod("Create", inParams, null);
            uint returnValue = (uint)(outParams.Properties["ReturnValue"].Value);
            bool success = returnValue == 0 || returnValue == 22;
            if (!success)
                throw new Exception(string.Format("Couldn't share folder {0} , return value is {1}:{2}", UncPath, returnValue, ((ShareResult)returnValue).ToString()));
            else
            {
                //user selection
                SecurityIdentifier userSID = new SecurityIdentifier(WellKnownSidType.WorldSid, null);

                //SID
                byte[] utenteSIDArray = new byte[userSID.BinaryLength];
                userSID.GetBinaryForm(utenteSIDArray, 0);

                //Trustee
                ManagementObject userTrustee = new ManagementClass(new ManagementPath("Win32_Trustee"), null);
                userTrustee["Name"] = "Everyone";
                userTrustee["SID"] = utenteSIDArray;

                //ACE
                ManagementObject userACE = new ManagementClass(new ManagementPath("Win32_Ace"), null);
                userACE["AccessMask"] = 2032127;                                 //Full access
                userACE["AceFlags"] = AceFlags.ObjectInherit | AceFlags.ContainerInherit;
                userACE["AceType"] = AceType.AccessAllowed;
                userACE["Trustee"] = userTrustee;

                ManagementObject userSecurityDescriptor = new ManagementClass(new ManagementPath("Win32_SecurityDescriptor"), null);
                userSecurityDescriptor["ControlFlags"] = 4; //SE_DACL_PRESENT 
                userSecurityDescriptor["DACL"] = new object[] { userACE };

                ManagementPath p2 = new ManagementPath(p.Path + string.Format(".Name='{0}'", ShareName));
                ManagementObject share = new ManagementObject(scope, p2, new ObjectGetOptions());
                share.InvokeMethod("SetShareInfo", new object[] { Int32.MaxValue, ShareName, userSecurityDescriptor });
            }
        }

        public void AdjustClientFolderSecurity()
        {
            string[] fullControl = new string[]{
                string.Format("{0}\\{1}", Settings.DomainName, Settings.ClientName),
                string.Format("{0}\\{1}", Settings.DomainName, "SAAS Clients"),
                string.Format("{0}\\{1}",Settings.DomainName,"Aria_IT")};

            string[] ModifyControl = new string[]{
                string.Format("{0}\\{1}", Settings.DomainName, "Aria_Distribution"),
                string.Format("{0}\\{1}",Settings.DomainName,"Aria_Support")};

            SecurityIdentifier everyOneUser = new SecurityIdentifier(WellKnownSidType.WorldSid, null);

            DirectoryInfo myDirectoryInfo = new DirectoryInfo(Settings.ClientSharedDirectory);
            DirectorySecurity myDirectorySecurity = myDirectoryInfo.GetAccessControl();
            foreach (string fullcontrolEnty in fullControl)
                myDirectorySecurity.AddAccessRule(new FileSystemAccessRule(fullcontrolEnty, FileSystemRights.FullControl, InheritanceFlags.ContainerInherit | InheritanceFlags.ObjectInherit, PropagationFlags.None, AccessControlType.Allow));
            foreach (string modifycontrolEntry in ModifyControl)
                myDirectorySecurity.AddAccessRule(new FileSystemAccessRule(modifycontrolEntry, FileSystemRights.Modify, InheritanceFlags.ContainerInherit | InheritanceFlags.ObjectInherit, PropagationFlags.None, AccessControlType.Allow));

            myDirectorySecurity.AddAccessRule(new FileSystemAccessRule(everyOneUser, FileSystemRights.ReadAndExecute, InheritanceFlags.ContainerInherit | InheritanceFlags.ObjectInherit, PropagationFlags.None, AccessControlType.Allow));
            myDirectoryInfo.SetAccessControl(myDirectorySecurity);
        }
    }

    public enum AdGroupType : uint
    {
        UnivGrp = 0x08,
        DomLocalGrp = 0x04,
        GlobalGrp = 0x02,
        SecurityGrp = 0x80000000
    }

    public enum ShareResult : uint
    {
        Success = 0, 	//Success
        AccessDenied = 2, 	//Access denied
        UnknownFailure = 8, 	//Unknown failure
        InvalidName = 9, 	//Invalid name
        InvalidLevel = 10, 	//Invalid level
        InvalidParameter = 21, 	//Invalid parameter
        DuplicateShare = 22, 	//Duplicate share
        RedirectedPath = 23, 	//Redirected path
        UnknownDevice = 24, 	//Unknown device or directory
        NetNameNotFound = 25 	//Net name not found
    }
}