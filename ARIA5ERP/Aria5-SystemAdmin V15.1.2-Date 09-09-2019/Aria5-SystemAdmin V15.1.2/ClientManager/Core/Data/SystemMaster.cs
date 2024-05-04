using Core.Utilites;
using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Data.EntityClient;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Core.Data
{
    public partial class SystemMaster : DbContext
    {
        public SystemMaster(SQLInfo sqlInfo)
            : base(GetEFConnectionString(sqlInfo))
        {



        }
        private static string GetEFConnectionString(SQLInfo SqlInfo)
        {
            string con = SqlInfo.GetConnection().ConnectionString;
            System.Data.SqlClient.SqlConnectionStringBuilder scsb = new System.Data.SqlClient.SqlConnectionStringBuilder(con);

            EntityConnectionStringBuilder ecb = new EntityConnectionStringBuilder();
            ecb.Metadata = "res://*/Data.SystemMasterModel.csdl|res://*/Data.SystemMasterModel.ssdl|res://*/Data.SystemMasterModel.msl";
            ecb.Provider = "System.Data.SqlClient";
            ecb.ProviderConnectionString = scsb.ConnectionString;

            return ecb.ConnectionString;
        }
    }
}
