using System;
using System.Collections.Generic;
using System.Text;

namespace Aria.Utilities.QueryBuilder.TableFieldValue
{
    public class AriaQueryTableRangeField : IAriaQueryTableFieldValue
    {
        private object _start;
        public object Start
        {
            get { return _start; }
            set { _start = value; }
        }

        private object _end;
        public object End
        {
            get { return _end; }
            set { _end = value; }
        }

        #region IAriaQueryTableFieldValue Members

        public string getValue(DatabaseType dataBaseType, Operator fieldOperator)
        {
            return getValue(dataBaseType);
        }

        public string getValue(DatabaseType dataBaseType)
        {
            string range = " BETWEEN ";

            switch (dataBaseType)
            {
                case DatabaseType.FOX:
                    range += Start.ToString();
                    range += " AND ";
                    range += End.ToString();
                    break;

                case DatabaseType.SQL:
                    range += Start.ToString();
                    range += " AND ";
                    range += End.ToString();
                    break;
            }

            return range;
        }

        #endregion
    }
}
