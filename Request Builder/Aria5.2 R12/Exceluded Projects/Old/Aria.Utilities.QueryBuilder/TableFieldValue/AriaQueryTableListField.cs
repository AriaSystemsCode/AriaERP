using System;
using System.Collections.Generic;
using System.Text;
using System.Collections;

namespace Aria.Utilities.QueryBuilder.TableFieldValue
{
    public class AriaQueryTableListField : IAriaQueryTableFieldValue
    {
        private ArrayList _list = new ArrayList();
        public ArrayList List
        {
            get { return _list; }
            set { _list = value; }
        }

        #region IAriaQueryTableFieldValue Members

        public string getValue(DatabaseType dataBaseType, Operator fieldOperator)
        {
            return getValue(dataBaseType);
        }

        public string getValue(DatabaseType dataBaseType)
        {
            string list = " IN (";

            bool flag = false;
            foreach (object element in List)
            {
                if( flag )
                    list += ", ";

                list += element.ToString();
                flag = true;
            }

            list += ")";

            return list;
        }

        #endregion
    }
}
