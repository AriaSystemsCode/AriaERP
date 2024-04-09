using System;
using System.Collections;
using System.EnterpriseServices;
using System.Reflection;
using System.Threading;
using Aria.Reflection;

namespace Aria.ThreadManager
{
    public interface IThreadManager
    {
        void StartThread(string GUID, string classID, string MethodName, object[] Params);
        bool StopThread(string GUID);
    }

    class DataWrapper
    {
        private string _methodName;
        private string _classID;
        private object[] _params;

        public string MethodName
        {
            get { return _methodName; }
            set { _methodName = value; }
        }

        public string ClassID
        {
            get { return _classID; }
            set { _classID = value; }
        }

        public object[] Params
        {
            get { return _params; }
            set { _params = value; }
        }
    }

    [ObjectPooling(true, 1, 1), JustInTimeActivation(true)]
    public class AriaThreadManager : ServicedComponent,IThreadManager
    {
        private Hashtable _threadHandle;
        public AriaThreadManager()
        {
            _threadHandle = new Hashtable();
        }

        [AutoComplete]
        public void StartThread(string GUID, string classID, string MethodName, object[] Params)
        {
            DataWrapper data = new DataWrapper();
            data.ClassID = classID;
            data.MethodName = MethodName;
            data.Params = Params;
            Thread thread = new Thread(new ParameterizedThreadStart(Run));
            _threadHandle[GUID]= thread;
            thread.Start(data);
            System.Windows.Forms.MessageBox.Show("SartThread");
            System.Windows.Forms.MessageBox.Show(_threadHandle.Count.ToString());
        }

        [AutoComplete]
        private void Run(object Data)
        {
            System.Windows.Forms.MessageBox.Show("RUNNN");
            AriaReflector reflector = new AriaReflector();
            DataWrapper data =null;
            try
            {
                data = (DataWrapper)Data;    
            }catch(Exception e)
            {
                System.Windows.Forms.MessageBox.Show(e.StackTrace);
            }
            
            System.Windows.Forms.MessageBox.Show("RUNNN11");
            if(data == null)
                System.Windows.Forms.MessageBox.Show("nulll");
            //string th = data.ClassID.Split(new char[1] {'.'})[0];
           // string cl = data.ClassID.Split(new char[1] {'.'})[1];
            //System.Windows.Forms.MessageBox.Show("split");
            //System.Windows.Forms.MessageBox.Show(th+" "+cl);
            //Assembly asm = Assembly.Load(th+".dll");
            //Type type = asm.GetType(cl);
            //System.Windows.Forms.MessageBox.Show(type.ToString());
            //object obj = asm.CreateInstance(cl);
            //object[] args = new object[0];
            ///object ret = type.InvokeMember(data.MethodName,BindingFlags.Default | BindingFlags.InvokeMethod,null,obj,data.Params);
            reflector.ExecuteMethod(data.ClassID,data.MethodName,data.Params);
            System.Windows.Forms.MessageBox.Show("11RUNNN");
        }

        [AutoComplete]
        public bool StopThread(string GUID)
        {
            Thread thread = (Thread)_threadHandle[GUID];
            System.Windows.Forms.MessageBox.Show(_threadHandle.Count.ToString());
            if(thread==null)
            {
                System.Windows.Forms.MessageBox.Show("nullllll");
                return false;
            }
            thread.Abort();
            bool result = thread.Join(60000);
            if(result)
                _threadHandle.Remove(GUID);
            return result;
        }        
    }
}

