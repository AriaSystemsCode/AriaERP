using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization;
using System.ServiceModel;
using System.Text;

namespace Service
{
    [ServiceContract]
    public interface ILogService
    {
        [OperationContract]
        Response QueryRequest(string Name);
    }

    public class LogService : ILogService
    {
        string LogPath
        {
            get
            {
                string path = System.Reflection.Assembly.GetExecutingAssembly().CodeBase.Replace("file:///", "");// System.Web.HttpContext.Current.Server.MapPath("~\\Log\\");
                path = System.IO.Path.GetDirectoryName(System.IO.Path.GetDirectoryName(path));
                path += "\\Log\\";
                return path;
            }
        }

        string logPathFormat = "{0}{1}";

        public string NewRequest(string key)
        {
            string Format = "{0}{1}_{2:D3}";
            int index = 1;

            while (File.Exists(string.Format(Format, LogPath, key, index)))
                index++;
            string content = string.Format("***** Created {0} *****", DateTime.Now.ToString("F")) + Environment.NewLine;
            File.WriteAllText(string.Format(Format, LogPath, key, index), content);
            return string.Format(Format, "", key, index);
        }

        public List<string> Query(string Name)
        {
            string file = string.Format(logPathFormat, LogPath, Name);
            if (File.Exists(file))
            {
                return File.ReadAllLines(file).ToList();
            }
            return null;
        }

        public void Add(string Name, string line)
        {
            string file = string.Format(logPathFormat, LogPath, Name);
            if (File.Exists(file))
            {
                line = string.Format("{0:F} *** {1} {2}", DateTime.Now, line, Environment.NewLine);
                File.AppendAllText(file, line);
            }
        }

        public Response QueryRequest(string Name)
        {
            Response response = new Response();
            bool validRequest = new ServiceSecurity().IsValid();
            if (!validRequest)
            {
                response.Status = ResponseStatus.RequestProcessingError;
                response.Content = new string[] { "Invalid Client Ip Address" }.ToList();
                return response;
            }

            var log = Query(Name);
            if (log.Last().ToLower().Contains("Request Processing Completed".ToLower()))
            {
                if (log.Any(x => x.ToLower().Contains("error")))
                    response.Status = ResponseStatus.RequestCompletedWithErrors;
                else
                    response.Status = ResponseStatus.RequestCompletedSuccessfully;
            }
            else
                response.Status = ResponseStatus.RequestInWork;
            response.Content = log;
            return response;
        }
    }

}
