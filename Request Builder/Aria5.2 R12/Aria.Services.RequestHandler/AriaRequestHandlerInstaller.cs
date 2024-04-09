using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Configuration.Install;
using System.Xml;
using System.ServiceProcess;
using System.Diagnostics;

namespace Aria.Services.RequestHandler
{
    [RunInstaller(true)]
    public partial class AriaRequestHandlerInstaller : Installer
    {
        private string _serviceName = null;

        private string _displayName = null;

        private string _description = null;

        private ServiceStartMode _startType;

        private ServiceAccount _account;

        private string _password = null;

        private string _userName = null;
        
        public AriaRequestHandlerInstaller()
        {
            Init();

            InitializeComponent();        
        }

        private void Init()
        {
            try
            {
                XmlDocument xmlDocument = new XmlDocument();

                xmlDocument.Load(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));

                XmlElement documentElement = xmlDocument.DocumentElement;

                for (int index = 0; index < documentElement.ChildNodes.Count; index++)
                {
                    if (documentElement.ChildNodes[index].Name == "RequestHandlerService")
                    {
                        XmlNode xmlNode = null;

                        for (int childIndex = 0; childIndex < documentElement.ChildNodes[index].ChildNodes.Count; childIndex++)
                        {
                            xmlNode = documentElement.ChildNodes[index].ChildNodes[childIndex];

                            if (xmlNode.Name == "ServiceName")
                            {
                                _serviceName = xmlNode.InnerText;
                            }
                            else if (xmlNode.Name == "DisplayName")
                            {
                                _displayName = xmlNode.InnerText;
                            }
                            else if (xmlNode.Name == "description")
                            {
                                _description = xmlNode.InnerText;
                            }
                            else if (xmlNode.Name == "StartType")
                            {
                                _startType = (ServiceStartMode)Enum.Parse(typeof(ServiceStartMode), xmlNode.InnerText);
                            }
                            else if (xmlNode.Name == "ServiceAccount")
                            {
                                for (int accountIndex = 0; accountIndex < xmlNode.ChildNodes.Count; accountIndex++)
                                {
                                    if (xmlNode.ChildNodes[accountIndex].Name == "type")
                                    {
                                        _account = (ServiceAccount)Enum.Parse(typeof(ServiceAccount), xmlNode.ChildNodes[accountIndex].InnerText);
                                    }
                                    else if (xmlNode.ChildNodes[accountIndex].Name == "UserName")
                                    {
                                        _userName = xmlNode.ChildNodes[accountIndex].InnerText;
                                    }
                                    else if (xmlNode.ChildNodes[accountIndex].Name == "password")
                                    {
                                        _password = xmlNode.ChildNodes[accountIndex].InnerText;
                                    }
                                }
                            }
                        }

                        break;
                    }
                }

                xmlDocument.Save(System.Environment.GetEnvironmentVariable("ARIA_SERVER_CONFIGURATION_PATH", EnvironmentVariableTarget.Machine));
            }
            catch (Exception ex)
            {
                //EventLog.WriteEntry("Aria.Services.RequestHandler", ex.Message, EventLogEntryType.Error);
            }
        }
    }
}