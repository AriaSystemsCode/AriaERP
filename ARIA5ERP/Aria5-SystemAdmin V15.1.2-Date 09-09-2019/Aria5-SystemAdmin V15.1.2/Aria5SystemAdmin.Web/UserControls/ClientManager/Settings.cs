using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Xml;

namespace Aria5SystemAdmin.Web.UserControls.ClientManager
{
    public class Settings
    {
        public Core.Utilites.SQLInfo Aria4 { get; set; }

        public Core.Utilites.SQLInfo SystemMaster { get; set; }

        public Core.Utilites.SQLInfo ClientMaster { get; set; }

        public string AriaSourcePath { get; set; }

        public string AriaMasterPath { get; set; }

        public string AriaSharedPath { get; set; }

        public string AriaSharedServerName { get; set; }

        public string AriaSharedLocalPath { get; set; }

        public string AriaSQLDBPath { get; set; }

        public string AriaMappedPath { get; set; }

        public string DomainName { get; set; }

        public string DomainAdminUserName { get; set; }

        public string DomainAdminUserPassword { get; set; }

        public void Save()
        {
            string path = System.IO.Path.GetDirectoryName(System.Web.HttpContext.Current.Server.MapPath("~/UserControls/ClientManager/"));
            path += "\\Settings.xml";
            XmlDocument xml = new XmlDocument();
            xml.Load(path);
            var root = xml.SelectSingleNode("//Settings");

            SaveNode(root, "AriaSourcePath", AriaSourcePath);
            SaveNode(root, "AriaMasterPath", AriaMasterPath);
            SaveNode(root, "AriaSharedPath", AriaSharedPath);
            SaveNode(root, "AriaSharedServerName", AriaSharedServerName);
            SaveNode(root, "AriaSharedLocalPath", AriaSharedLocalPath);
            SaveNode(root, "AriaSQLDBPath", AriaSQLDBPath);
            SaveNode(root, "AriaMappedPath", AriaMappedPath);
            SaveNode(root, "DomainName", DomainName);
            SaveNode(root, "DomainAdminUserName", DomainAdminUserName);
            SaveNode(root, "DomainAdminUserPassword", DomainAdminUserPassword);

            root = SaveNode(root, "Aria4", null);
            SaveNode(root, "ServerName", Aria4.ServerName);
            SaveNode(root, "UserName", Aria4.Admin.UserName);
            SaveNode(root, "Password", Aria4.Admin.Password);

            root = xml.SelectSingleNode("//Settings");
            root = SaveNode(root, "SystemMaster", null);
            SaveNode(root, "ServerName", SystemMaster.ServerName);
            SaveNode(root, "DataBaseName", SystemMaster.DataBaseName);
            SaveNode(root, "UserName", SystemMaster.Admin.UserName);
            SaveNode(root, "Password", SystemMaster.Admin.Password);

            root = xml.SelectSingleNode("//Settings");
            root = SaveNode(root, "ClientMaster", null);
            SaveNode(root, "ServerName", ClientMaster.ServerName);
            SaveNode(root, "UserName", ClientMaster.Admin.UserName);
            SaveNode(root, "Password", ClientMaster.Admin.Password);

            xml.Save(path);
        }

        private XmlNode SaveNode(XmlNode root, string nodePath, string NodeText)
        {
            var node = root.SelectSingleNode(nodePath);
            if (node == null)
            {
                node = root.OwnerDocument.CreateElement(nodePath);
                root.AppendChild(node);
            }
            if (NodeText != null)
                node.InnerText = NodeText;
            return node;
        }

        private static string LoadNode(XmlNode root, string nodePath)
        {
            var node = root.SelectSingleNode(nodePath);
            if (node != null)
                return node.InnerText;
            return null;
        }

        public static Settings Load()
        {
            string path = System.IO.Path.GetDirectoryName(System.Web.HttpContext.Current.Server.MapPath("~/UserControls/ClientManager/"));
            path += "\\Settings.xml";
            XmlDocument xml = new XmlDocument();
            xml.Load(path);
            var root = xml.SelectSingleNode("//Settings");

            Settings settings = new Settings();
            settings.AriaMappedPath = LoadNode(root, "AriaMappedPath");
            settings.AriaSourcePath = LoadNode(root, "AriaSourcePath");
            settings.AriaMasterPath = LoadNode(root, "AriaMasterPath");
            settings.AriaSharedPath = LoadNode(root, "AriaSharedPath");
            settings.AriaSharedServerName = LoadNode(root, "AriaSharedServerName");
            settings.AriaSharedLocalPath = LoadNode(root, "AriaSharedLocalPath");
            settings.AriaSQLDBPath = LoadNode(root, "AriaSQLDBPath");
            settings.DomainName = LoadNode(root, "DomainName");
            settings.DomainAdminUserName = LoadNode(root, "DomainAdminUserName");
            settings.DomainAdminUserPassword = LoadNode(root, "DomainAdminUserPassword");

            settings.Aria4 = new Core.Utilites.SQLInfo();
            root = root.SelectSingleNode("Aria4");
            if (root != null)
            {
                settings.Aria4.ServerName = LoadNode(root, "ServerName");
                string user = LoadNode(root, "UserName");
                string pass = LoadNode(root, "Password");
                settings.Aria4.Admin = new Core.Utilites.Credential(user, pass);
            }

            settings.SystemMaster = new Core.Utilites.SQLInfo();
            root = xml.SelectSingleNode("//Settings");
            root = root.SelectSingleNode("SystemMaster");
            if (root != null)
            {
                settings.SystemMaster.ServerName = LoadNode(root, "ServerName");
                settings.SystemMaster.DataBaseName = LoadNode(root, "DataBaseName");
                string user = LoadNode(root, "UserName");
                string pass = LoadNode(root, "Password");
                settings.SystemMaster.Admin = new Core.Utilites.Credential(user, pass);
            }

            settings.ClientMaster = new Core.Utilites.SQLInfo();
            root = xml.SelectSingleNode("//Settings");
            root = root.SelectSingleNode("ClientMaster");
            if (root != null)
            {
                settings.ClientMaster.ServerName = LoadNode(root, "ServerName");
                string user = LoadNode(root, "UserName");
                string pass = LoadNode(root, "Password");
                settings.ClientMaster.Admin = new Core.Utilites.Credential(user, pass);
            }
            return settings;
        }
    }
}