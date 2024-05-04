using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
//using DevExpress.Xpo.Metadata;


namespace WebApplication1
{

    

    public class FilterConverter //: ValueConverter
    {
        public object ConvertFromStorageType(object value)
        {
            List<Filter> filterList = new List<Filter>();
             if (value != null && ((string)value).Length > 38)
             filterList = Filter.Convertor((string)value);
            return filterList;
        }

        public object ConvertToStorageType(object value)
        {
            string result = null;
            if (value != null && value is List<Filter> && ((List<Filter>)value).Count > 0)
                result = Filter.Convertor(((List<Filter>)value));
            return result;
        }

        public Type StorageType
        {
            get
            {
                return typeof(string);
            }
        }
    }

    //ATA add new files filter tahat enable us to work with files array 7/30/2017 [start]
    public class FilesConverter //: ValueConverter
    {
        public  object ConvertFromStorageType(object value)
        {
            List<Files> filterList = new List<Files>();
            if (value != null && ((string)value).Length > 38)
                filterList = Files.Convertor((string)value);
            return filterList;
        }

        public  object ConvertToStorageType(object value)
        {
            string result = null;
            if (value != null && value is List<Files> && ((List<Files>)value).Count > 0)
                result = Files.Convertor(((List<Files>)value));
            return result;
        }

        public  Type StorageType
        {
            get
            {
                return typeof(string);
            }
        }
    }




    public class FieldsConverter //: ValueConverter
    {
        public  object ConvertFromStorageType(object value)
        {
            List<Fields> filterList = new List<Fields>();
            if (value != null && ((string)value).Length > 38)
                filterList = Fields.Convertor((string)value);
            return filterList;
        }

        public  object ConvertToStorageType(object value)
        {
            string result = null;
            if (value != null && value is List<Fields> && ((List<Fields>)value).Count > 0)
                result = Fields.Convertor(((List<Fields>)value));
            return result;
        }

        public  Type StorageType
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