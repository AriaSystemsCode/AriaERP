using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.ExpressApp.ConditionalAppearance;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    public class Enable_Client_For_Custom_LevelOfUpgrade_Only : AppearanceAttribute
    {
        public Enable_Client_For_Custom_LevelOfUpgrade_Only()
            : base("DisableClient")
        {
            this.Criteria = "ToStr(LevelOfUpgrade) != 'U'";
            this.TargetItems = "Client";
            this.Enabled = false;
        }
    }

    public class Enable_Decimal_Places_For_Numeric_DataType_Only : AppearanceAttribute
    {
        public Enable_Decimal_Places_For_Numeric_DataType_Only()
            : base("DecimalPlacesDisableRule")
        {
            this.Criteria = "ToStr(DataType) != 'N'";
            this.TargetItems = "DecimalPlaces";
            this.Enabled = false;
        }
    }

    public class Disable_Width_For_Date_And_Logical_DataTypes : AppearanceAttribute
    {
        public Disable_Width_For_Date_And_Logical_DataTypes()
            : base("FieldWidthDisableRule")
        {
            this.Criteria = "ToStr(DataType) = 'D' || ToStr(DataType) = 'L'";
            this.TargetItems = "Width";
            this.Enabled = false;
        }
    }

    public class Disable_Database_For_A27 : AppearanceAttribute
    {
        public Disable_Database_For_A27()
            : base("Disable DataBase for A27")
        {
            this.Criteria = "ToStr(Product) = 'ARIA27'";
            this.TargetItems = "Database";
            this.Enabled = false;
        }
    }
}

