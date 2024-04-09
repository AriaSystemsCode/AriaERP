using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using Aria.EnterpriseServices.ObjectDictionary;
using System.Runtime.Remoting.Channels;
using System.Collections;
using System.Runtime.Serialization.Formatters;
using System.Runtime.Remoting.Channels.Tcp;

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
            if (!_init) Init();

            Aria.EnterpriseServices.ObjectDictionary.AriaObjectDictionaryDBCentric obj1 = new AriaObjectDictionaryDBCentric();
            Aria.EnterpriseServices.RequestHandler.AriaRequestAgent obj2 = new Aria.EnterpriseServices.RequestHandler.AriaRequestAgent();
            Aria.Environment.AriaEnviromentVariables obj3 = new Aria.Environment.AriaEnviromentVariables();
            Aria.EnterpriseServices.Messaging.AriaMessagingManager obj4 = new Aria.EnterpriseServices.Messaging.AriaMessagingManager();
            Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy obj5 = new Aria.EnterpriseServices.RequestHandler.Proxy.AriaRequestProxy();
            Aria.Utilities.ParameterSubstitution.AriaParameterSubstituter obj6 = new Aria.Utilities.ParameterSubstitution.AriaParameterSubstituter();

            string url = string.Format("tcp://{0}:{1}/{2}", computerName, portNumber, className);

            foreach(AssemblyName assemblyName in Assembly.GetAssembly(typeof(AriaActivator)).GetReferencedAssemblies())
            {
                Assembly assembly = Assembly.Load(assemblyName);

                foreach (Type type in assembly.GetTypes())
                {
                    if (type.FullName == className)
                    {
                        return Activator.GetObject(type, url);
                    }
                }
            }
            return null;
        }
    }
}
