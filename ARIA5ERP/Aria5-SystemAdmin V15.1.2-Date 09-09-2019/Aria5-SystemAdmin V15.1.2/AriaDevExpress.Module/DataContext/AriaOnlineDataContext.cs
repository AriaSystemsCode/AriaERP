using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace AriaDevExpress.Module.DataContext
{
    public partial class AriaOnlineDataContext
    {
        public AriaOnlineDataContext()
            : base(ConnectionString.Getvalue())
        {
            OnCreated();
        }
    }
}
