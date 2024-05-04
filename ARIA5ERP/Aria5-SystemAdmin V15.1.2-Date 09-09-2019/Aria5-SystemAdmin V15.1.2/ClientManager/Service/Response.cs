using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
using System.Threading;
using System.Web;

namespace Service
{
    [DataContract]
    public class Response
    {
        [DataMember]
        public ResponseStatus Status { get; set; }

        [DataMember]
        public List<string> Content { get; set; }
    }

    public enum ResponseStatus
    {
        RequestCompletedSuccessfully,
        RequestCompletedWithErrors,
        RequestProcessingError,
        RequestRecieved,
        RequestInWork
    }
}