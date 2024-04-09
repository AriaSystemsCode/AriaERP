using System.Text;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Soap;

namespace Aria.Xml
{
    /// <summary>
    /// This is class responsible on serializing any object
    /// </summary>
    public class AriaXmlSerializer
    {
        public string ConvertToXml(object source)
        {
            IFormatter formatter = new SoapFormatter();
            MemoryStream memoryStream = new MemoryStream();
            formatter.Serialize(memoryStream, source);
            
            string objectString = Encoding.ASCII.GetString(memoryStream.GetBuffer()); 
			
            int index = objectString.IndexOf("\0");  //Check for the null terminator character
            if (index > 0)
            {
                objectString = objectString.Substring(0, index);
            }

            return objectString;
        }

        public object ConvertFromXml(string xml)
        {
            byte[] bytes = new byte[xml.Length];

            Encoding.ASCII.GetBytes(xml, 0, xml.Length, bytes, 0);

            MemoryStream memoryStream = new MemoryStream(bytes);
            IFormatter formatter = new SoapFormatter();

            return formatter.Deserialize(memoryStream);
        }
    }
}
