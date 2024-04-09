using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;

namespace Aria.RequestHandler.Troubleshooting
{
    public class CheckAssemblyExistsInGac : Check
    {
        private string _assemblyName;
        public string AssemblyName
        {
            get { return _assemblyName; }
            set { _assemblyName = value; }
        }        

        public CheckAssemblyExistsInGac(string AssemblyName)
        {
            _assemblyName = AssemblyName;            
            CheckAction = "Check Assembly Exists in GAC for " + AssemblyName + " (Assembly mustn't be registered in GAC))";
            CheckResult = CheckResult.Failed;
        }

        public override void GetCheckResult()
        {
            if (ParentCheck != null && ParentCheck.CheckResult == CheckResult.Failed)
            {
                CheckResult = CheckResult.Failed;
                CheckError = "Parent check failed";
                return;
            }

            bool result = GacUtil.IsAssemblyInGAC(AssemblyName);
            if (result)
            {
                CheckResult = CheckResult.Failed;
                CheckError = "Assembly " + AssemblyName + " is registered in GAC";
            }
            else
            {
                CheckResult = CheckResult.Succeed;
            }

        }
    }

    #region Class to check assembly exists in GAC
    public static class GacUtil
    {
        [DllImport("fusion.dll")]
        private static extern IntPtr CreateAssemblyCache(out IAssemblyCache ppAsmCache, int reserved);

        [ComImport]
        [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
        [Guid("e707dcde-d1cd-11d2-bab9-00c04f8eceae")]
        private interface IAssemblyCache
        {
            int Dummy1();

            [PreserveSig()]
            IntPtr QueryAssemblyInfo(int flags, [MarshalAs(UnmanagedType.LPWStr)] string assemblyName, ref AssemblyInfo assemblyInfo);

            int Dummy2();
            int Dummy3();
            int Dummy4();
        }

        [StructLayout(LayoutKind.Sequential)]
        private struct AssemblyInfo
        {
            public int cbAssemblyInfo;
            public int assemblyFlags;
            public long assemblySizeInKB;

            [MarshalAs(UnmanagedType.LPWStr)]
            public string currentAssemblyPath;

            public int cchBuf;
        }

        public static bool IsAssemblyInGAC(string assemblyName)
        {
            var assembyInfo = new AssemblyInfo { cchBuf = 512 };
            assembyInfo.currentAssemblyPath = new string('\0', assembyInfo.cchBuf);

            IAssemblyCache assemblyCache;

            var hr = CreateAssemblyCache(out assemblyCache, 0);

            if (hr == IntPtr.Zero)
            {
                hr = assemblyCache.QueryAssemblyInfo(
                    1,
                    assemblyName,
                    ref assembyInfo);

                if (hr != IntPtr.Zero)
                {
                    return false;
                }

                return true;
            }

            Marshal.ThrowExceptionForHR(hr.ToInt32());
            return false;
        }
    }
    #endregion
}
