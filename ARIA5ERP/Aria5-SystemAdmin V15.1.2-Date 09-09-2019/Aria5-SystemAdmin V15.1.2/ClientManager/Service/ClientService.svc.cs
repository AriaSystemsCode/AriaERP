using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
using System.ServiceModel;
using System.Text;
using System.Threading.Tasks;

namespace Service
{
    [ServiceContract]

    public interface IClientService
    {
        [OperationContract()]
        Response CreateClient(Core.Utilites.Settings options);
    }

    public class ClientService : IClientService
    {
        public Response CreateClient(Core.Utilites.Settings options)
        {
            Response response = new Response();
            bool validRequest = new ServiceSecurity().IsValid();
            if (!validRequest)
            {
                response.Status = ResponseStatus.RequestProcessingError;
                response.Content = new string[] { "Invalid Client Ip Address" }.ToList();
                return response;
            }
            try
            {
                LogService logger = new LogService();
                string logKey = logger.NewRequest(options.ClientCode);
                logger.Add(logKey, "Create Client Request Receieved");

                Core.Business.Main main = new Core.Business.Main();
                main.RequestKey = logKey;
                main.LogProgress = new Core.Business.Main.progress(logger.Add);
                Action<Core.Utilites.Settings> CreateClientAction = new Action<Core.Utilites.Settings>(main.CreateClient);
                AsyncCallback CreateClientCallBack = new AsyncCallback(EndCreateClient);
                CreateClientAction.BeginInvoke(options, CreateClientCallBack, logKey);

                //main.CreateClientAsync(options);

                response.Status = ResponseStatus.RequestRecieved;
                response.Content = new string[] { logKey }.ToList();
            }
            catch
            {
            }
            return response;
        }

        public void EndCreateClient(IAsyncResult asyncResult)
        {
            LogService logger = new LogService();
            string logKey = Convert.ToString(asyncResult.AsyncState);
            logger.Add(logKey, "Request Processing Completed");
        }
    }
}
