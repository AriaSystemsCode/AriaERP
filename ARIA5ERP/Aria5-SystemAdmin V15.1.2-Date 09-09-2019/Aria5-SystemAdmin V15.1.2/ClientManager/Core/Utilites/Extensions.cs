using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Text;
using System.Threading.Tasks;

namespace Core.Utilites
{
    public static class Extensions
    {
        public static string EndsWithBackSlash(this string input)
        {
            if (input.EndsWith("\\"))
                return input;
            else
                return input + "\\";
        }

        public static void DirectoryCopy(string sourceDirName, string destDirName, bool copySubDirs = true)
        {
            // Get the subdirectories for the specified directory.
            DirectoryInfo dir = new DirectoryInfo(sourceDirName);
            DirectoryInfo[] dirs = dir.GetDirectories();

            if (!dir.Exists)
            {
                throw new DirectoryNotFoundException(
                    "Source directory does not exist or could not be found: "
                    + sourceDirName);
            }

            // If the destination directory doesn't exist, create it. 
            if (!Directory.Exists(destDirName))
            {
                Directory.CreateDirectory(destDirName);
            }

            // Get the files in the directory and copy them to the new location.
            FileInfo[] files = dir.GetFiles();
            foreach (FileInfo file in files)
            {
                string temppath = Path.Combine(destDirName, file.Name);
                if (!File.Exists(temppath))
                    file.CopyTo(temppath, false);
            }

            // If copying subdirectories, copy them and their contents to new location. 
            if (copySubDirs)
            {
                foreach (DirectoryInfo subdir in dirs)
                {
                    string temppath = Path.Combine(destDirName, subdir.Name);
                    DirectoryCopy(subdir.FullName, temppath, copySubDirs);
                }
            }
        }

        public static string GetExecptionMessage(this Exception ex)
        {
            string message = "";
            if (ex is System.Net.WebException)
            {
                System.Net.WebException ex2 = ex as WebException;
                message = "There are problems with your Intenet Connection :" + ex2.Message;
            }
            else if (ex is System.Data.SqlClient.SqlException)
            {
                System.Data.SqlClient.SqlException ex3 = ex as System.Data.SqlClient.SqlException;
                message = "There are problems executing sql script :" + ex3.Message;
            }
            else if (ex is Microsoft.SqlServer.Management.Common.ExecutionFailureException)
            {
                Microsoft.SqlServer.Management.Common.ExecutionFailureException ex3 = ex as Microsoft.SqlServer.Management.Common.ExecutionFailureException;
                message = "There are problems executing sql script :" + ex3.Message;
            }

            else
                message = ex.Message + " ";
            System.Diagnostics.StackTrace trace = new System.Diagnostics.StackTrace(ex, true);
            if (trace.FrameCount > 0)
            {
                message += "Method:" + trace.GetFrame(0).GetMethod().Name + " ";
                message += "Line No:" + trace.GetFrame(0).GetFileLineNumber() + " ";
                message += "File Name:" + trace.GetFrame(0).GetFileName();
            }
            if (ex.InnerException != null)
                message += Environment.NewLine + GetExecptionMessage(ex.InnerException);

            return message;
        }
    }
}
