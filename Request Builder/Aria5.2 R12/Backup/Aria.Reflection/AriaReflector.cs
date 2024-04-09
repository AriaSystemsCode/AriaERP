using System;
using System.Reflection;

namespace Aria.Reflection
{
    /// <summary>
    /// Used to Execute Method, get object, get type of object.
    /// </summary>
    public class AriaReflector
    {
        public Type GetType(AppDomain domain, string type)
        {
            Type returnType = null;

            Assembly[] assemblies = domain.GetAssemblies();

            for (int index = 0; index < assemblies.Length; index++)
            {
                returnType = assemblies[index].GetType(type);
                if (returnType != null)
                {
                    return returnType;
                }
            }

            return returnType;
        }

        public object GetObject(AppDomain domain, string type, object[] arguments)
        {
            Type returnObject = null;

            Assembly[] assemblies = domain.GetAssemblies();

            for (int index = 0; index < assemblies.Length; index++)
            {
                returnObject = assemblies[index].GetType(type);
                if (returnObject != null)
                {
                    return returnObject.Assembly.CreateInstance(type, false, 
                                BindingFlags.CreateInstance, null, arguments, null, null);
                }
            }

            return null;
        }

        public void ExecuteMethod(string className, string methodName, object[] arguments)
        {
            Type type = System.Type.GetTypeFromProgID (className);
            object Ref = Activator.CreateInstance(type);
            type.InvokeMember(methodName, BindingFlags.InvokeMethod,
                null, Ref, arguments);
            Ref = null;
            GC.Collect();
        }
    }
}
