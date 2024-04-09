using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Security.Cryptography;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;


namespace Aria.Utilities.Aria40Converter.Helpers
{
    static class StringHelper
    {


        public static string ReplaceSpecialWords(this string value)
        {
            value = value.Replace("(_Code)", "Code");
            value = value.Replace("(Add_User)", "Add User");
            value = value.Replace("(Edit_User)", "Edit User");

            return value;
        }

        public static string RemoveSpecialChar(this string value)
        {
            bool isCode = false;
            if (value.Contains("(_Code)"))
            {
                value = value.Replace("(_Code)", "");
                isCode = true;
            }

            bool isAddUser = false;
            if (value.Contains("(Add_User)"))
            {
                value = value.Replace("(Add_User)", "");
                isAddUser = true;
            }

            bool isEditUser = false;
            if (value.Contains("(Edit_User)"))
            {
                value = value.Replace("(Edit_User)", "");
                isEditUser = true;
            }
            
            string result = "";

            foreach (char c in value.ToCharArray())
            {
                if (!char.IsLetterOrDigit(c))
                    continue;

                result += c;
            }

            if (isCode)
            {
                result += "_Code";
            }

            if (isAddUser)
            {
                result += "Add_User";
            }

            if (isEditUser)
            {
                result += "Edit_User";
            }

            return result;
        }

        public static string GetMd5Hash(MD5 md5Hash, string input)
        {

            // Convert the input string to a byte array and compute the hash.
            byte[] data = md5Hash.ComputeHash(Encoding.UTF8.GetBytes(input));

            // Create a new Stringbuilder to collect the bytes
            // and create a string.
            StringBuilder sBuilder = new StringBuilder();

            // Loop through each byte of the hashed data 
            // and format each one as a hexadecimal string.
            for (int i = 0; i < data.Length; i++)
            {
                sBuilder.Append(data[i].ToString("x2"));
            }

            // Return the hexadecimal string.
            return sBuilder.ToString();
        }

        public static bool VerifyMd5Hash(MD5 md5Hash, string input, string hash)
        {
            // Hash the input.
            string hashOfInput = GetMd5Hash(md5Hash, input);

            // Create a StringComparer an compare the hashes.
            StringComparer comparer = StringComparer.OrdinalIgnoreCase;

            if (0 == comparer.Compare(hashOfInput, hash))
            {
                return true;
            }
            else
            {
                return false;
            }
        }



        public static bool CompareRecursiveTree(this TreeNode tn1, TreeNode tn2)
        {   
                if (tn1 == null || tn2 == null)
                {
                    return tn1 == tn2;
                }

                if ((tn1.Text != tn2.Text) || (tn1.Nodes.Count != tn2.Nodes.Count))
                {
                    return false;
                }

                for (int i = 0; i < tn1.Nodes.Count; i++)
                {
                    if (!CompareRecursiveTree(tn1.Nodes[i], tn2.Nodes[i]))
                    {
                        return false;
                    }
                }

                return true;
        }


        public static T DeepClone<T>(T obj)
        {
            using (var ms = new MemoryStream())
            {
                var formatter = new BinaryFormatter();
                formatter.Serialize(ms, obj);
                ms.Position = 0;

                return (T)formatter.Deserialize(ms);
            }
        }

    }
}

