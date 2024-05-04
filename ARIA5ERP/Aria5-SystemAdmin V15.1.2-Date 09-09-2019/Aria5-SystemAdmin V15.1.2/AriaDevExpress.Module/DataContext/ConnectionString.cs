using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Configuration;
using System.Reflection;
using System.IO;

namespace AriaDevExpress.Module.DataContext
{
    static public class ConnectionString
    {
        public static string connectionString { get; set; }
        public static string Getvalue()
        {

            string path = Path.GetDirectoryName(Assembly.GetExecutingAssembly().CodeBase.Replace("file:///", ""));
            path += "\\Connection.xml";
            System.Xml.XmlDocument doc = new System.Xml.XmlDocument();
            try
            {
                doc.Load(path);
            }
            catch (Exception)
            { }
            string connection = "";

            try
            {
#if RELEASE
            connection = doc.SelectSingleNode("//Release").InnerText.Trim();
#else
                connection = doc.SelectSingleNode("//Debug").InnerText.Trim();

                return connection;
#endif
            }
            catch (Exception)
            { return null; }
            
        }
    }
}
