namespace Aria.DataTypes.RequestHandler
{
    public partial class AriaRequest
    {
        public void SetImmediateRequestSettings()
        {
            _recurrenceType = AriaRequestRecurrenceTypes.Immediate;
        }
    }
}