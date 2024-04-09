using Aria.DataTypes;
using Aria.DataTypes.Settings;
using System.EnterpriseServices;


namespace Aria.Data.DataTypes
{
    public abstract class AriaDataTypeAdapter 
    {
        protected AriaDbConnection _connection;
        public AriaDbConnection Connection
        {
            get { return _connection; }
            set { _connection = value; }
        }

        protected AriaDataTypeSettings _settings;
        public AriaDataTypeSettings Settings
        {
            get { return _settings; }
            set { _settings = value; }
        }

        protected AriaDataType _value;
        public AriaDataType Value
        {
          get { return _value; }
          set { _value = value; }
        }


        public AriaDataTypeAdapter(AriaDbConnection connection, AriaDataTypeSettings settings, AriaDataType value)
        {
            _connection = connection;
            _settings = settings;
            _value = value;
        }
        
        public abstract object GetData(string dataPath,string clientId);
        
        public abstract void SetData(string dataPath, object value);
    }
}
