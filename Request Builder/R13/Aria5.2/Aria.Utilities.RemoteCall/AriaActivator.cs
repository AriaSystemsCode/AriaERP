using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using Aria.EnterpriseServices.ObjectDictionary;
using System.Runtime.Remoting.Channels;
using System.Collections;
using System.Runtime.Serialization.Formatters;
using System.Runtime.Remoting.Channels.Tcp;
using System.Windows.Forms;
using System.Diagnostics;

namespace Aria.Utilities.RemoteCall
{
    public class AriaActivator
    {
        private bool _init = false;

        public void Init()
        {
            BinaryClientFormatterSinkProvider clientProvider = new BinaryClientFormatterSinkProvider();
            BinaryServerFormatterSinkProvider serverProvider = new BinaryServerFormatterSinkProvider();
            serverProvider.TypeFilterLevel = System.Runtime.Serialization.Formatters.TypeFilterLevel.Full;

            IDictionary props = new Hashtable();
            props["port"] = 0;
            string s = System.Guid.NewGuid().ToString();
            props["name"] = s;
            props["typeFilterLevel"] = TypeFilterLevel.Full;
            TcpChannel chan = new TcpChannel(props, clientProvider, serverProvider);

            ChannelServices.RegisterChannel(chan);

            _init = true;
        }

        public object GetRemoteObject(string className, string computerName, int portNumber)
        {
            EventLog.WriteEntry("AriaActivator", "1", EventLogEntryType.Information);
            if (!_init) Init();

            if (computerName == null) computerName = "";
            if (className == null) className = "";


            string result = "";

            result = "";
            for (int i = 0; i < className.Length; i++)
            {
                if (Convert.ToByte(Convert.ToChar(className.Substring(i, 1))) != 0)
                {
                    result += Convert.ToChar(className.Substring(i, 1)).ToString();
                }
                else
                {
                    break;
                }
            }
            className = result;
            EventLog.WriteEntry("AriaActivator", "2", EventLogEntryType.Information);

            result = "";
            for (int i = 0; i < computerName.Length; i++)
            {
                if (Convert.ToByte(Convert.ToChar(computerName.Substring(i, 1))) != 0)
                {
                    result += Convert.ToChar(computerName.Substring(i, 1)).ToString();
                }
                else
                {
                    break;
                }
            }
            computerName = result;
            EventLog.WriteEntry("AriaActivator", "3", EventLogEntryType.Information);

            if (computerName == null || computerName.Trim() == "")
            {
                computerName = SystemInformation.ComputerName.ToUpper();
            }

            Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric obj1 = new AriaObjectDictionaryDBCentric();
            Aria.EnterpriseServices.RequestHandler.AriaRequestAgent obj2 = new Aria.EnterpriseServices.RequestHandler.AriaRequestAgent();
            Aria.Environment.AriaEnviromentVariables obj3 = new Aria.Environment.AriaEnviromentVariables();
            Aria.EnterpriseServices.Messaging.AriaMessagingManager obj4 = new Aria.EnterpriseServices.Messaging.AriaMessagingManager();
            Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy obj5 = new Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy();
            Aria.Utilities.ParameterSubstitution.AriaParameterSubstituter obj6 = new Aria.Utilities.ParameterSubstitution.AriaParameterSubstituter();

            string url = string.Format("tcp://{0}:{1}/{2}", computerName, 1500, className);
            EventLog.WriteEntry("AriaActivator", "4", EventLogEntryType.Information);
            foreach(AssemblyName assemblyName in Assembly.GetAssembly(typeof(AriaActivator)).GetReferencedAssemblies())
            {
                Assembly assembly = Assembly.Load(assemblyName);

                foreach (Type type in assembly.GetTypes())
                {
                    //SAB 05-19-2013 [Start]
                    //if (type.FullName == className || type.FullName + "Testing" == className)
                    if (type.FullName == className || type.FullName + "Test" == className)
                    //SAB 05-19-2013 [End]
                    {
                        return Activator.GetObject(type, url);
                        EventLog.WriteEntry("AriaActivator", "5", EventLogEntryType.Information);
                    }
                }
            }
            EventLog.WriteEntry("AriaActivator", "6", EventLogEntryType.Information);
            return null;
        }
    }
}
