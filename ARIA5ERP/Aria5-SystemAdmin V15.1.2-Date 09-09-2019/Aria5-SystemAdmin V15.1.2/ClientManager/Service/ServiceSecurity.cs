using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.ServiceModel;
using System.ServiceModel.Channels;
using System.Web;

namespace Service
{
    public class ServiceSecurity
    {
        string AllowedIPListFilePath = System.Web.HttpContext.Current.Server.MapPath("~\\AllowedIPs.txt");
        public bool IsValid()
        {
            if (File.Exists(AllowedIPListFilePath))
            {
                var IpList = File.ReadAllLines(AllowedIPListFilePath).Select(x => x.Trim()).ToList();
                OperationContext context = OperationContext.Current;
                MessageProperties messageProperties = context.IncomingMessageProperties;
                RemoteEndpointMessageProperty endpointProperty = messageProperties[RemoteEndpointMessageProperty.Name] as RemoteEndpointMessageProperty;
                string clientIp = endpointProperty.Address;

                if (IpList.Contains("*.*.*.*"))
                    return true;
                else if (IpList.Contains(clientIp))
                    return true;
                else
                    return false;
            }
            return false;
        }
    }
}