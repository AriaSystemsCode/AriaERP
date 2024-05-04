using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using DevExpress.Xpo.Metadata;
using DevExpress.Xpo;
using DevExpress.ExpressApp.ConditionalAppearance;

namespace AriaDevExpress.Module.BusinessObjects.SysFiles
{
    public enum Products
    {
        ARIA27,
        ARIA4XP
    }

    public enum DataBaseTypes
    {
        FOX,
        SQL
    }

    public enum UpgradeLevelType
    {
        [DisplayName("Application")]
        A,
        [DisplayName("System")]
        S,
        [DisplayName("User")]
        U
    }

    public enum SystemDataType
    {
        [DisplayName("Character")]
        C,
        [DisplayName("Number")]
        N,
        [DisplayName("Date")]
        D,
        [DisplayName("Logic")]
        L,
        [DisplayName("Memo")]
        M,
        [DisplayName("General")]
        G
    }

    public enum sydReprtObjectType
    {
        [DisplayName("Report")]
        R,
        [DisplayName("Program")]
        P
    }



    public enum ValidEntryType
    {
        [DisplayName("Manual")]
        M,
        [DisplayName("Array")]
        A,
        [DisplayName("Code")]
        C,
        [DisplayName("Field")]
        F
    }

    public enum OrderDirection
    {
        Ascending,
        Descending
    }

    public enum FoxMathFunctions
    {
        [DisplayName("")]
        Math = 0,
        [DisplayName("*")]
        Multiply,
        [DisplayName("/")]
        Division,
        [DisplayName("+")]
        Addition,
        [DisplayName("-")]
        Subtract,
        [DisplayName("ABS(expN)")]
        ABS,
        [DisplayName("MAX(,)")]
        MAX,
        [DisplayName("^")]
        Power,
        [DisplayName("MIN(,)")]
        MIN,
        [DisplayName("MOD(,)")]
        MOD,
        [DisplayName("PAYMENT(,,)")]
        PAYMENT,
        [DisplayName("PI()")]
        PI,
        [DisplayName("PV(,,)")]
        PV,
        [DisplayName("RAND()")]
        RAND,
        [DisplayName("ACOS(expN)")]
        ACOS,
        [DisplayName("ASIN(expN)")]
        ASIN,
        [DisplayName("ATAN(expN)")]
        ATAN,
        [DisplayName("ATAN2(expN)")]
        ATAN2,
        [DisplayName("CEILING(expN)")]
        CEILING,
        [DisplayName("EXP(expN)")]
        EXP,
        [DisplayName("FV(,,)")]
        FV,
        [DisplayName("INT(expN)")]
        INT,
        [DisplayName("LOG(expN)")]
        LOG,
        [DisplayName("LOG10(expN)")]
        LOG10,
        [DisplayName("ROUND(expN)")]
        ROUND,
        [DisplayName("RTOD(expN)")]
        RTOD,
        [DisplayName("SIGN(expN)")]
        SIGN,
        [DisplayName("SIN(expN)")]
        SIN,
        [DisplayName("SQRT(expN)")]
        SQRT,
        [DisplayName("TAN(expN)")]
        TAN,
        [DisplayName("VAL(expC)")]
        VAL,
    }

    public enum FoxStringFunctions
    {
        [DisplayName("    ")]
        String = 0,
        [DisplayName("\"\"")]
        DoubleQuotes,
        [DisplayName("+")]
        Addition,
        [DisplayName("-")]
        Subtract,
        [DisplayName("ASC(expC)")]
        ASC,
        [DisplayName("ALLTRIM(expC)")]
        ALLTRIM,
        [DisplayName("LEFT(expC,)")]
        LEFT,
        [DisplayName("LEN(expC)")]
        LEN,
        [DisplayName("LOWER(expC)")]
        LOWER,
        [DisplayName("UPPER(expC)")]
        UPPER,
        [DisplayName("LTRIM(expC)")]
        LTRIM,
        [DisplayName("RTRIM(expC)")]
        RTRIM,
        [DisplayName("PADC(expC,)")]
        PADC,
        [DisplayName("PADL(expC,)")]
        PADL,
        [DisplayName("PADR(expC,)")]
        PADR,
        [DisplayName("RIGHT(expC,)")]
        RIGHT,
        [DisplayName("TYPE(expC)")]
        TYPE,
        [DisplayName("PROPER(expC)")]
        PROPER,
        [DisplayName("SOUNDEX(expC)")]
        SOUNDEX,
        [DisplayName("CHR(expN)")]
        CHR,
        [DisplayName("SPACE(expN)")]
        SPACE,
        [DisplayName("STR(expN,,)")]
        STR,
        [DisplayName("AT(,,)")]
        AT,
        [DisplayName("ATC(,,)")]
        ATC,
        [DisplayName("MAX(,)")]
        MAX,
        [DisplayName("MIN(,)")]
        MIN,
        [DisplayName("OCCURS(,)")]
        OCCURS,
        [DisplayName("RAT(,,)")]
        RAT,
        [DisplayName("REPLICATE(,)")]
        REPLICATE,
        [DisplayName("CHRTRAN(,,)")]
        CHRTRAN,
        [DisplayName("STRTRAN(,,)")]
        STRTRAN,
        [DisplayName("STUFF()")]
        STUFF,
        [DisplayName("SUBSTR()")]
        SUBSTR,
        [DisplayName("TRANSFORM(,)")]
        TRANSFORM
    }

    public enum FoxLogicalFunctions
    {
        [DisplayName("    ")]
        Logical = 0,
        [DisplayName("==")]
        DoubleEqual,
        [DisplayName("()")]
        Braces,
        [DisplayName("<")]
        GreaterThan,
        [DisplayName(">")]
        LowerThan,
        [DisplayName("=")]
        Equal,
        [DisplayName("<>")]
        NotEqual,
        [DisplayName("<=")]
        GreaterThanOrEqual,
        [DisplayName(">=")]
        LowerThanOrEqual,
        [DisplayName(".T.")]
        True,
        [DisplayName(".F.")]
        False,
        [DisplayName("NOT")]
        NOT,
        [DisplayName("AND")]
        AND,
        [DisplayName("OR")]
        OR,
        [DisplayName("BETWEEN(,,)")]
        BETWEEN,
        [DisplayName("EMPTY(expR)")]
        EMPTY,
        [DisplayName("IIF(,,)")]
        IIF,
        [DisplayName("INLIST(,,)")]
        INLIST


    }

    public enum FoxDateFunctions
    {
        [DisplayName("    ")]
        Date = 0,
        [DisplayName("CDOW(expD)")]
        CDOW,
        [DisplayName("CMONTH(expD)")]
        CMONTH,
        [DisplayName("CTOD(expC)")]
        CTOD,
        [DisplayName("DATE(expD)")]
        DATE,
        [DisplayName("DAY(expD)")]
        DAY,
        [DisplayName("DMY(expD)")]
        DMY,
        [DisplayName("DOW(expD)")]
        DOW,
        [DisplayName("DTOS(expD)")]
        DTOS,
        [DisplayName("GOMONTH(expD)")]
        GOMONTH,
        [DisplayName("MDY(expD)")]
        MDY,
        [DisplayName("MONTH(expD)")]
        MONTH,
        [DisplayName("YEAR(expD)")]
        YEAR,
        [DisplayName("MAX(,)")]
        MAX,
        [DisplayName("MIN(,)")]
        MIN,
        [DisplayName("SECONDS()")]
        SECONDS,
        [DisplayName("TIME()")]
        TIME
    }



    //public static class EnumUtils
    //{
    //    public static T ParseEnum<T>(string value, T defaultValue) where T : struct, IConvertible
    //    {
    //        if (!typeof(T).IsEnum) throw new ArgumentException("T must be an enumerated type");
    //        if (string.IsNullOrEmpty(value)) return defaultValue;

    //        foreach (T item in Enum.GetValues(typeof(T)))
    //        {
    //            if (item.ToString().ToLower().Equals(value.Trim().ToLower())) return item;
    //        }
    //        return defaultValue;
    //    }
    //}

    public enum MenuSubCategory
    {
        A,
        I,
        S
    }

    public enum MenuSubType
    {
        [DisplayName("Bar")]
        B,
        [DisplayName("Calling")]
        C,
        [DisplayName("Disabled")]
        P,
        [DisplayName("Branch")]
        S
    }

    public enum MenuProcType
    {
        [DisplayName(" ")]
        [Persistent(" ")]
        None,
        [DisplayName("Custom Program")]
        C,
        [DisplayName("External Program")]
        E,
        [DisplayName("Global Function")]
        G,
        [DisplayName("Module")]
        M,
        [DisplayName("Program")]
        P,
        [DisplayName("Report")]
        R,
        [DisplayName("System")]
        S
    }

    public enum MenuPopLevel
    {
        [DisplayName(" ")]
        [Persistent(" ")]
        None,
        [DisplayName("01")]
        [Persistent("01")]
        first,
        [DisplayName("02")]
        [Persistent("02")]
        second,
        [DisplayName("03")]
        [Persistent("03")]
        third
    }

    public class EnumValueConverter<T> : ValueConverter where T : struct, IConvertible
    {
        public T DefaultValue { get; set; }

        public override object ConvertFromStorageType(object value)
        {
            if (!typeof(T).IsEnum) throw new ArgumentException("T must be an enumerated type");
            if (string.IsNullOrEmpty((string)value)) return DefaultValue;

            foreach (T item in Enum.GetValues(typeof(T)))
            {
                object[] attributes = item.GetType().GetMember(item.ToString())[0].GetCustomAttributes(typeof(PersistentAttribute), false);
                if (item.ToString().ToLower().Equals(((string)value).Trim().ToLower()))
                    return item;
                else if (attributes.Length > 0 && ((PersistentAttribute)attributes[0]).MapTo.Equals((string)value))
                    return item;
            }
            return DefaultValue;
        }

        public override object ConvertToStorageType(object value)
        {
            object[] attributes = value.GetType().GetMember(value.ToString())[0].GetCustomAttributes(typeof(PersistentAttribute), false);
            if (attributes.Length > 0)
                value = ((PersistentAttribute)attributes[0]).MapTo;
            return value.ToString();
        }

        public override Type StorageType
        {
            get
            {
                return typeof(string);
            }
        }
    }
}

