using System;

namespace Aria.DataTypes.RequestHandler
{
    public partial class AriaRequest
    {
        public void SetOneTimeOnlyRequestSettings(DateTime requestTime)
        {
            _recurrenceType = AriaRequestRecurrenceTypes.Once;
            _requestStartTime = requestTime;
        }
    }
}