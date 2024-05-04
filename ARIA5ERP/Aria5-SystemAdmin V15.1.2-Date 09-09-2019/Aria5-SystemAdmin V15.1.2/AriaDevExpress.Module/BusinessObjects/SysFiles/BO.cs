using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo.Metadata;
using DevExpress.Persistent.Validation;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    public class BO
    {
        public static string GetEnumDisplayName(object value)
        {
            string description = null;
            var type = value.GetType();
            var memInfo = type.GetMember(value.ToString());
            var attributes = memInfo[0].GetCustomAttributes(typeof(DevExpress.Xpo.DisplayNameAttribute), false);
            if (attributes != null)
                description = ((DevExpress.Xpo.DisplayNameAttribute)attributes[0]).DisplayName;
            return description;
        }

        public static bool IsRankProperty(object value)
        {
            bool Rank = false;
            var type = value.GetType();
            var memInfo = type.GetMember(value.ToString());
            var attributes = memInfo[0].GetCustomAttributes(typeof(RankAttribute), false);
            if (attributes != null)
                Rank = ((RankAttribute)attributes[0]).RankProperty;
            return Rank;
        }

        public class RankAttribute : Attribute
        {
            public bool RankProperty { get; set; }

            public RankAttribute(bool RankProperty)
            {
                this.RankProperty = RankProperty;
            }
        }

        public class SelectorAttribute : Attribute
        {
            public bool SelectionProperty { get; set; }

            public SelectorAttribute(bool SelectionProperty)
            {
                this.SelectionProperty = SelectionProperty;
            }
        }

        public static bool ValidateWithoutTicket(object target, ContextIdentifiers contextIDs)
        {
            // To Be Check By Mahmoud
            RuleSetValidationResult results =  Validator.RuleSet.ValidateTarget(null, target, contextIDs);

            foreach (RuleSetValidationResultItem resultItem in results.Results)
            {
                if (resultItem.State == ValidationState.Invalid)
                {
                    if (resultItem.Rule.UsedProperties != null && resultItem.Rule.UsedProperties.Count > 0 && resultItem.Rule.UsedProperties.Contains("Ticket"))
                        continue;
                    else
                        return false;
                }

            }
            return true;
        }
    }

    public class BoolToYNValueConverter : ValueConverter
    {
        public override object ConvertFromStorageType(object value)
        {
            bool? result = null;
            if (!string.IsNullOrWhiteSpace((string)value))
            {
                if (((string)value) == "Y")
                    result = true;
                else if (((string)value) == "N")
                    result = false;
            }
            return result;
        }

        public override object ConvertToStorageType(object value)
        {
            string result = "";
            if (((bool?)value) != null && ((bool?)value).HasValue)
                result = ((bool?)value).Value ? "Y" : "N";
            return result;
        }

        public override Type StorageType
        {
            get
            {
                return typeof(string);
            }
        }
    }

    public class BoolToPAValueConverter : ValueConverter
    {
        public override object ConvertFromStorageType(object value)
        {
            bool result = false;
            if (!string.IsNullOrWhiteSpace((string)value))
            {
                if (((string)value) == "P")
                    result = true;
                else if (((string)value) == "A")
                    result = false;
            }
            return result;
        }

        public override object ConvertToStorageType(object value)
        {
            string result = (bool)value ? "P" : "A";
            return result;
        }

        public override Type StorageType
        {
            get
            {
                return typeof(string);
            }
        }


    }

    public class IntToStringPaddedConverter : ValueConverter
    {
        public override object ConvertFromStorageType(object value)
        {
            int result = 0;
            if (!string.IsNullOrWhiteSpace((string)value))
            {
                int.TryParse((string)value, out result);
            }
            return result;
        }

        public override object ConvertToStorageType(object value)
        {
            string result = ((int)value).ToString().PadLeft(2, '0');
            return result;
        }

        public override Type StorageType
        {
            get
            {
                return typeof(string);
            }
        }
    }

    public class FilterConverter : ValueConverter
    {
        public override object ConvertFromStorageType(object value)
        {
            List<Filter> filterList = new List<Filter>();
            if (value != null && ((string)value).Length > 38)
                filterList = Filter.Convertor((string)value);
            return filterList;
        }

        public override object ConvertToStorageType(object value)
        {
            string result = null;
            if (value != null && value is List<Filter> && ((List<Filter>)value).Count > 0)
                result = Filter.Convertor(((List<Filter>)value));
            return result;
        }

        public override Type StorageType
        {
            get
            {
                return typeof(string);
            }
        }
    }

    //ATA add new files filter tahat enable us to work with files array 7/30/2017 [start]
    public class FilesConverter : ValueConverter
    {
        public override object ConvertFromStorageType(object value)
        {
            List<Files> filterList = new List<Files>();
            if (value != null && ((string)value).Length > 38)
                filterList = Files.Convertor((string)value);
            return filterList;
        }

        public override object ConvertToStorageType(object value)
        {
            string result = null;
            if (value != null && value is List<Files> && ((List<Files>)value).Count > 0)
                result = Files.Convertor(((List<Files>)value));
            return result;
        }

        public override Type StorageType
        {
            get
            {
                return typeof(string);
            }
        }
    }




    public class FieldsConverter : ValueConverter
    {
        public override object ConvertFromStorageType(object value)
        {
            List<Fields> filterList = new List<Fields>();
            if (value != null && ((string)value).Length > 38)
                filterList = Fields.Convertor((string)value);
            return filterList;
        }

        public override object ConvertToStorageType(object value)
        {
            string result = null;
            if (value != null && value is List<Fields> && ((List<Fields>)value).Count > 0)
                result = Fields.Convertor(((List<Fields>)value));
            return result;
        }

        public override Type StorageType
        {
            get
            {
                return typeof(string);
            }
        }
    }
    //ATA add new files filter tahat enable us to work with files array 7/30/2017 [End]


    //public class ObjectFiltersConverter : ValueConverter
    //{
    //    public override object ConvertFromStorageType(object value)
    //    {
    //        List<Filter> filterList = new List<Filter>();
    //        if (value != null && ((string)value).Length > 38)
    //            filterList = Filter.Convertor((string)value);
    //        List<ObjectFilters> ObjectFilterList = new List<ObjectFilters>();
    //        filterList.ForEach(filter => ObjectFilterList.Add(new ObjectFilters() { Filter = filter }));
    //        return ObjectFilterList;
    //    }
    //    public override object ConvertToStorageType(object value)
    //    {
    //        string result = null;
    //        if (value != null && value is List<ObjectFilters> && ((List<ObjectFilters>)value).Count > 0)
    //        {
    //            List<Filter> filterList = new List<Filter>();
    //            ((List<ObjectFilters>)value).ForEach(objectFilter => filterList.Add(objectFilter.Filter));
    //            result = Filter.Convertor(filterList);
    //        }
    //        return result;
    //    }
    //    public override Type StorageType
    //    {
    //        get
    //        {
    //            return typeof(string);
    //        }
    //    }
    //}

    public interface IAuditable
    {

    }
}