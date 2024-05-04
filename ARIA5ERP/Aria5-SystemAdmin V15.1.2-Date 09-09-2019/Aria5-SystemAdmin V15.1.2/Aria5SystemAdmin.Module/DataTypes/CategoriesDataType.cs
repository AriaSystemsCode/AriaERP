using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Aria5SystemAdmin.Module.DataTypes
{
    public class CategoriesDataType
    {


        private System.String _categoryId;


        public System.String CategoryId
        {
            get
            {
                return _categoryId;
            }
            set
            {
                _categoryId = value;
            }
        }


        private System.String _categoryName;


        public System.String CategoryName
        {
            get
            {
                return _categoryName;
            }
            set
            {
                _categoryName = value;
            }
        }

        private System.Guid _categoryOid;


        public System.Guid CategoryOid
        {
            get
            {
                return _categoryOid;
            }
            set
            {
                _categoryOid = value;
            }
        }

    }
}
