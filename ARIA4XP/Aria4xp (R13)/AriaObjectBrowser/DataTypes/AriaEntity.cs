using System;
using System.Collections.Generic;
using System.Text;

namespace AriaObjectBrowser.DataTypes
{
    public class AriaEntity
    {
        private static string _entityType;
        public static string EntityType
        {
            get { return AriaEntity._entityType; }
            set { AriaEntity._entityType = value; }
        }

        private static string _entityId;
        public static string EntityId
        {
            get { return AriaEntity._entityId; }
            set { AriaEntity._entityId = value; }
        }
    }
}
