﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Aria5SystemAdmin.Module.BusinessObjects;
using Aria5SystemAdmin.Module.Managers;
using DevExpress.Xpo;
using Aria5SystemAdmin.Module;
using DevExpress.Data.Filtering;
using System.Data.SqlClient;
using Excel = Microsoft.Office.Interop.Excel;
using DevExpress.Spreadsheet;
using DevExpress.Web.ASPxSpreadsheet;
using System.Data;
using System.Data.OleDb;
using AriaDevExpress.Module.BusinessObjects.SysFiles;
using System.Text.RegularExpressions;

namespace GenerateMemoFiles
{
    class Program
    {
        /// <summary>
        /// Main Function
        /// </summary>
        /// <param name="args"></param>
        static void Main(string[] args)
        {
            string DBPathe = @"D:\Aria4xp\Aria4xp (R13)\sysfiles\\";
            string TableName = @"SYDOBJCT";
            string ColumnName = @"mprgfiles";
            string CompareColumnName = @"mprgfiles";
            string CompareValue = @"";
            var x = GetDbaseOrFoxproRawValue(DBPathe, TableName, ColumnName, CompareColumnName, CompareValue);

        }


        public static object GetDbaseOrFoxproRawValue(string DBPath, string TableName, string ColumnName, string CompareColumnName, string CompareValue)
        {
            #region OLD
            //using (BinaryReader read = new BinaryReader(System.IO.File.Open(Path.Combine(DBPath, TableName + ".dbf"), FileMode.Open, FileAccess.Read, FileShare.ReadWrite)))
            //{


            //    // Is it a type of file that I can handle?
            //    if (new byte[] { 0x02, 0x03, 0x30, 0x43, 0x63, 0x83, 0x8b, 0xcb, 0xf5, 0xfb }.Contains(read.ReadByte()))
            //    {
            //        // Skip date.
            //        read.BaseStream.Seek(3, SeekOrigin.Current);

            //        // Read useful datas...
            //        uint RecordCount = read.ReadUInt32();
            //        ushort FirstRecord = read.ReadUInt16();
            //        ushort RecordLength = read.ReadUInt16();
            //        int FieldCount = FirstRecord - 296 / 32;

            //        // Make sure things aren't stupid.
            //        ColumnName = ColumnName.ToLower();
            //        CompareColumnName = CompareColumnName.ToLower();

            //        // Find target column (field)
            //        string temp;
            //        UInt32 CompareFieldOffset = uint.MaxValue, FieldOffset = uint.MaxValue;
            //        byte CompareFieldLength = 0, FieldLength = 0;
            //        char FieldType = ' ';
            //        for (int i = 0; i < FieldCount; i++)
            //        {
            //            read.BaseStream.Seek(32 + (i * 32), SeekOrigin.Begin);
            //            temp = Encoding.ASCII.GetString(read.ReadBytes(11)).Replace("\0", "").ToLower();
            //            if (temp == CompareColumnName)
            //            {
            //                read.ReadChar();
            //                CompareFieldOffset = read.ReadUInt32();
            //                CompareFieldLength = read.ReadByte();
            //            }
            //            if (temp == ColumnName)
            //            {
            //                FieldType = read.ReadChar();
            //                FieldOffset = read.ReadUInt32();
            //                FieldLength = read.ReadByte();
            //            }

            //            if (CompareFieldOffset != uint.MaxValue && FieldOffset != uint.MaxValue)
            //                break;
            //        }

            //        // Make sure we can continue.
            //        if (CompareFieldOffset == uint.MaxValue ||
            //            FieldOffset == uint.MaxValue) return null;

            //        // Iterate through each record to find the one we want.
            //        for (int index = 0; index < RecordCount; index++)
            //        {
            //            var c=read.BaseStream.Seek(FirstRecord + (index * RecordLength) + CompareFieldOffset, SeekOrigin.Begin);
            //            temp = Encoding.Default.GetString(read.ReadBytes(CompareFieldLength)).Replace("\0", "");
            //            //if (temp == CompareValue)
            //            //{
            //            //read.BaseStream.Seek(FirstRecord + (index * RecordLength) + FieldOffset, SeekOrigin.Begin);
            //            //switch (FieldType)
            //            //{
            //            //    case 'M':
            //            //    case 'I': return read.ReadUInt32();
            //            //    case 'C':
            //            //    default: return Encoding.Default.GetString(read.ReadBytes(FieldLength)).Replace("\0", "");
            //            //}
            //            //}
            //        }
            //    }
            //    else
            //    {
            //        return null;
            //    }
            //}

            //return null; 
            #endregion
            return null;
        }
    }
}
