using Core.Utilites;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
using System.ServiceModel;
using System.Text;

namespace Service
{

    [ServiceContract]
    public interface IDataService
    {
        [OperationContract]
        List<KeyValuePair<string, string>> GetSydApp(string AriaSourcePath);
         
        [OperationContract]
        List<KeyValuePair<string, string>> GetRoles(SQLInfo SystemMasterSqlInfo);
    }


    // NOTE: You can use the "Rename" command on the "Refactor" menu to change the class name "DataService" in code, svc and config file together.
    // NOTE: In order to launch WCF Test Client for testing this service, please select DataService.svc or DataService.svc.cs at the Solution Explorer and start debugging.
    public class DataService : IDataService
    {
        public List<KeyValuePair<string, string>> GetSydApp(string AriaSourcePath)
        {
            return DataHelper.GetSydApp(AriaSourcePath);
        }

        public List<KeyValuePair<string, string>> GetRoles(SQLInfo SystemMasterSqlInfo)
        {
            return DataHelper.GetAllSystemMasterRoles(SystemMasterSqlInfo);
        }
    }
}
