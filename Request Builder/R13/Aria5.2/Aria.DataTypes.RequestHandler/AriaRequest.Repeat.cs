using System;

namespace Aria.DataTypes.RequestHandler
{
    public partial class AriaRequest
    {
        private bool _repeatTask = false;
        public bool RepeatTask
        {
            get { return _repeatTask; }
            set { _repeatTask = value; }
        }

        private int _repeatTaskEvery = 0;
        public int RepeatTaskEvery
        {
            get { return _repeatTaskEvery; }
            set { _repeatTaskEvery = value; }
        }

        private AriaRequestRepeatUntilTypes _repeatTaskUntilType = AriaRequestRepeatUntilTypes.NotSet;
        public AriaRequestRepeatUntilTypes RepeatTaskUntilType
        {
            get { return _repeatTaskUntilType; }
            set { _repeatTaskUntilType = value; }
        }

        private DateTime _repeatTaskUntillTime = new DateTime(1900,01,01);
        public DateTime RepeatTaskUntillTime
        {
            get { return _repeatTaskUntillTime; }
            set { _repeatTaskUntillTime = value; }
        }

        private int _repeatTaskUntilDuration = 0;
        public int RepeatTaskUntilDuration
        {
            get { return _repeatTaskUntilDuration; }
            set { _repeatTaskUntilDuration = value; }
        }

        private bool _holdTaskIfStillRunning = false;
        public bool HoldTaskIfStillRunning
        {
            get { return _holdTaskIfStillRunning; }
            set { _holdTaskIfStillRunning = value; }
        }

        public void UndoRepeatTask()
        {
            _repeatTask = false;
            _repeatTaskEvery = 0;
            _repeatTaskUntillTime = new DateTime(1900, 01, 01);
            _holdTaskIfStillRunning = false;
        }

        public void SetRepeatTaskUntilTimeSettings(int repeatTaskEvery, DateTime repeatTaskUntillTime, bool holdTaskIfStillRunning)
        {
            _repeatTask = true;
            _repeatTaskUntilType = AriaRequestRepeatUntilTypes.Time;
            _repeatTaskEvery = repeatTaskEvery;
            _repeatTaskUntillTime = repeatTaskUntillTime;
            _holdTaskIfStillRunning = holdTaskIfStillRunning;
        }

        public void SetRepeatTaskUntilDurationSettings(int repeatTaskEvery, int repeatTaskUntilDuration, bool holdTaskIfStillRunning)
        {
            _repeatTask = true;
            _repeatTaskUntilType = AriaRequestRepeatUntilTypes.Duration;
            _repeatTaskEvery = repeatTaskEvery;
            _repeatTaskUntilDuration = repeatTaskUntilDuration;
            _holdTaskIfStillRunning = holdTaskIfStillRunning;
        }
    }
}