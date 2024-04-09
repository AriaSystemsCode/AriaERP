using System;
using System.Data;
using System.Collections;
using Aria.Xml;


namespace Aria.DataTypes
{
    /// <remarks>
    /// Open Issues:
    /// 1-We need to take in consideration the case of between, contains and inList for the RightHandSide onject.
    /// </remarks>
    [Serializable, AriaSerializableAttribute]
    public class AriaCondition : AriaDataType
    {
        private AriaStandardDataType _leftHandSide;
        /// <summary>
        /// Left hand side formula formated as below.
        /// </summary>
        /// <remarks>
        /// Formats:
        /// Expression:
        /// Field:
        /// ParameterName:
        /// Value:
        /// </remarks>
        public AriaStandardDataType LeftHandSide
        {
            get { return _leftHandSide; }
            set { _leftHandSide = value; }
        }

        private bool _isOperator;
        public bool IsOperator
        {
            get { return _isOperator; }
            set { _isOperator = value; }
        }

        private AriaConditionOperators _operator;
        public AriaConditionOperators Operator
        {
            get { return _operator; }
            set { _operator = value; }
        }

        private AriaDataType _rightHandSide;
        public AriaDataType RightHandSide
        {
            get { return _rightHandSide; }
            set { _rightHandSide = value; }
        }

        private AriaConditionParameterizedData _leftSideParameterizedData = new AriaConditionParameterizedData();
        public AriaConditionParameterizedData LeftSideParameterizedData
        {
            get { return _leftSideParameterizedData; }
            set { _leftSideParameterizedData = value; }
        }

        private AriaConditionParameterizedData _rightSideParameterizedData = new AriaConditionParameterizedData();
        public AriaConditionParameterizedData RightSideParameterizedData
        {
            get { return _rightSideParameterizedData; }
            set { _rightSideParameterizedData = value; }
        }

        private bool CompareStrings()
        {
            bool comparator = true;
            AriaConditionOperators conditionOperator = Operator;
            string leftHandSide = LeftHandSide.Value.ToString();
            string rightHandSide = null;
            int stringCompare = 0;
            int fromCompare = 0;
            int toCompare = 0;

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    fromCompare = leftHandSide.CompareTo(((AriaStandardDataType)((AriaRange)RightHandSide).From).Value.ToString());
                    toCompare = leftHandSide.CompareTo(((AriaStandardDataType)((AriaRange)RightHandSide).To).Value.ToString());
                    break;
                     
                case AriaConditionOperators.InList:
                    for (int index = 0; index < ((AriaList)RightHandSide).Items.Count; index++)
                    {
                        if (leftHandSide.Equals(((AriaList)RightHandSide).Items[index].ToString()))
                            stringCompare++;
                    }
                    break;

                default:
                    rightHandSide = ((AriaStandardDataType)RightHandSide).Value.ToString();
                    stringCompare = leftHandSide.CompareTo(rightHandSide);
                    break;
            }

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    comparator = (fromCompare >= 0 && toCompare <= 0);
                    break;

                case AriaConditionOperators.Contains:
                    comparator = leftHandSide.Contains(rightHandSide);
                    break;

                case AriaConditionOperators.GreaterOrEqual:
                    comparator = (stringCompare >= 0);
                    break;

                case AriaConditionOperators.GreaterThan:
                    comparator = (stringCompare > 0);
                    break;

                case AriaConditionOperators.InList:
                    comparator = (stringCompare > 0);
                    break;

                case AriaConditionOperators.LessOrEqual:
                    comparator = (stringCompare <= 0);
                    break;

                case AriaConditionOperators.LessThan:
                    comparator = (stringCompare < 0);
                    break;

                case AriaConditionOperators.Like:
                    comparator = (stringCompare == 0);
                    break;
            }

            if (!IsOperator)
                comparator = !comparator;

            return comparator;
        }

        private bool CompareInts()
        {
            bool comparator = true;
            AriaConditionOperators conditionOperator = Operator;
            int leftHandSide = int.Parse(LeftHandSide.Value.ToString());
            int rightHandSide = 0;
            int intCompare = 0;
            int fromCompare = 0;
            int toCompare = 0;

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    fromCompare = leftHandSide.CompareTo(int.Parse(((AriaStandardDataType)((AriaRange)RightHandSide).From).Value.ToString()));
                    toCompare = leftHandSide.CompareTo(int.Parse(((AriaStandardDataType)((AriaRange)RightHandSide).To).Value.ToString()));
                    break;

                case AriaConditionOperators.InList:
                    for (int index = 0; index < ((AriaList)RightHandSide).Items.Count; index++)
                    {
                        if (leftHandSide.Equals(int.Parse(((AriaList)RightHandSide).Items[index].ToString())))
                            intCompare++;
                    }
                    break;

                default:
                    rightHandSide = int.Parse(((AriaStandardDataType)RightHandSide).Value.ToString());
                    intCompare = leftHandSide.CompareTo(rightHandSide);
                    break;
            }

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    comparator = (fromCompare >= 0 && toCompare <= 0);
                    break;

                case AriaConditionOperators.Contains:
                    break;

                case AriaConditionOperators.GreaterOrEqual:
                    comparator = (intCompare >= 0);
                    break;

                case AriaConditionOperators.GreaterThan:
                    comparator = (intCompare > 0);
                    break;

                case AriaConditionOperators.InList:
                    comparator = (intCompare > 0);
                    break;

                case AriaConditionOperators.LessOrEqual:
                    comparator = (intCompare <= 0);
                    break;

                case AriaConditionOperators.LessThan:
                    comparator = (intCompare < 0);
                    break;

                case AriaConditionOperators.Like:
                    comparator = (intCompare == 0);
                    break;
            }

            if (!IsOperator)
                comparator = !comparator;

            return comparator;
        }

        private bool CompareLogicals()
        {
            bool comparator = true;
            AriaConditionOperators conditionOperator = Operator;
            bool leftHandSide = bool.Parse(LeftHandSide.Value.ToString());
            bool rightHandSide = true;
            int boolCompare = 0;
            int fromCompare = 0;
            int toCompare = 0;

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    fromCompare = leftHandSide.CompareTo(bool.Parse(((AriaStandardDataType)((AriaRange)RightHandSide).From).Value.ToString()));
                    toCompare = leftHandSide.CompareTo(bool.Parse(((AriaStandardDataType)((AriaRange)RightHandSide).To).Value.ToString()));
                    break;

                case AriaConditionOperators.InList:
                    for (int index = 0; index < ((AriaList)RightHandSide).Items.Count; index++)
                    {
                        if (leftHandSide.Equals(bool.Parse(((AriaList)RightHandSide).Items[index].ToString())))
                            boolCompare++;
                    }
                    break;

                default:
                    rightHandSide = bool.Parse(((AriaStandardDataType)RightHandSide).Value.ToString());
                    boolCompare = leftHandSide.CompareTo(rightHandSide);
                    break;
            }

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    comparator = (fromCompare >= 0 && toCompare <= 0);
                    break;

                case AriaConditionOperators.Contains:
                    break;

                case AriaConditionOperators.GreaterOrEqual:
                    comparator = (boolCompare >= 0);
                    break;

                case AriaConditionOperators.GreaterThan:
                    comparator = (boolCompare > 0);
                    break;

                case AriaConditionOperators.InList:
                    comparator = (boolCompare > 0);
                    break;

                case AriaConditionOperators.LessOrEqual:
                    comparator = (boolCompare <= 0);
                    break;

                case AriaConditionOperators.LessThan:
                    comparator = (boolCompare < 0);
                    break;

                case AriaConditionOperators.Like:
                    comparator = (boolCompare == 0);
                    break;
            }

            if (!IsOperator)
                comparator = !comparator;

            return comparator;
        }

        private bool CompareNumerics()
        {
            bool comparator = true;
            AriaConditionOperators conditionOperator = Operator;
            double leftHandSide = double.Parse(LeftHandSide.Value.ToString());
            double rightHandSide = 0.0;
            int numericCompare = 0;
            int fromCompare = 0;
            int toCompare = 0;

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    fromCompare = leftHandSide.CompareTo(double.Parse(((AriaStandardDataType)((AriaRange)RightHandSide).From).Value.ToString()));
                    toCompare = leftHandSide.CompareTo(double.Parse(((AriaStandardDataType)((AriaRange)RightHandSide).To).Value.ToString()));
                    break;

                case AriaConditionOperators.InList:
                    for (int index = 0; index < ((AriaList)RightHandSide).Items.Count; index++)
                    {
                        if (leftHandSide.Equals(double.Parse(((AriaList)RightHandSide).Items[index].ToString())))
                            numericCompare++;
                    }
                    break;

                default:
                    rightHandSide = double.Parse(((AriaStandardDataType)RightHandSide).Value.ToString());
                    numericCompare = leftHandSide.CompareTo(rightHandSide);
                    break;
            }

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    comparator = (fromCompare >= 0 && toCompare <= 0);
                    break;

                case AriaConditionOperators.Contains:
                    break;

                case AriaConditionOperators.GreaterOrEqual:
                    comparator = (numericCompare >= 0);
                    break;

                case AriaConditionOperators.GreaterThan:
                    comparator = (numericCompare > 0);
                    break;

                case AriaConditionOperators.InList:
                    comparator = (numericCompare > 0);
                    break;

                case AriaConditionOperators.LessOrEqual:
                    comparator = (numericCompare <= 0);
                    break;

                case AriaConditionOperators.LessThan:
                    comparator = (numericCompare < 0);
                    break;

                case AriaConditionOperators.Like:
                    comparator = (numericCompare == 0);
                    break;
            }

            if (!IsOperator)
                comparator = !comparator;

            return comparator;
        }

        private bool CompareDates()
        {
            bool comparator = true;
            AriaConditionOperators conditionOperator = Operator;
            DateTime leftHandSide = (DateTime)LeftHandSide.Value;
            DateTime rightHandSide = new DateTime();
            int dateCompare = 0;
            int fromCompare = 0;
            int toCompare = 0;

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    fromCompare = leftHandSide.CompareTo((DateTime)((AriaStandardDataType)((AriaRange)RightHandSide).From).Value);
                    toCompare = leftHandSide.CompareTo((DateTime)((AriaStandardDataType)((AriaRange)RightHandSide).To).Value);
                    break;

                case AriaConditionOperators.InList:
                    for (int index = 0; index < ((AriaList)RightHandSide).Items.Count; index++)
                    {
                        if (leftHandSide.Equals((DateTime)(object)((AriaList)RightHandSide).Items[index]))
                            dateCompare++;
                    }
                    break;

                default:
                    rightHandSide = (DateTime)((AriaStandardDataType)RightHandSide).Value;
                    dateCompare = leftHandSide.CompareTo(rightHandSide);
                    break;
            }

            switch (conditionOperator)
            {
                case AriaConditionOperators.Between:
                    comparator = (fromCompare >= 0 && toCompare <= 0);
                    break;

                case AriaConditionOperators.Contains:
                    break;

                case AriaConditionOperators.GreaterOrEqual:
                    comparator = (dateCompare >= 0);
                    break;

                case AriaConditionOperators.GreaterThan:
                    comparator = (dateCompare > 0);
                    break;

                case AriaConditionOperators.InList:
                    comparator = (dateCompare > 0);
                    break;

                case AriaConditionOperators.LessOrEqual:
                    comparator = (dateCompare <= 0);
                    break;

                case AriaConditionOperators.LessThan:
                    comparator = (dateCompare < 0);
                    break;

                case AriaConditionOperators.Like:
                    comparator = (dateCompare == 0);
                    break;
            }

            if (!IsOperator)
                comparator = !comparator;

            return comparator;
        }

        public bool IsConditionValid()
        {
            bool comparator = true;

            switch (LeftHandSide.Value.GetType().ToString())
            { 
                case "System.String":
                    comparator = CompareStrings();
                    break;

                case "System.Int32":
                    comparator = CompareInts();
                    break;

                case "System.Boolean":
                    comparator = CompareLogicals();
                    break;

                case "System.Double":
                    comparator = CompareNumerics();
                    break;

                case "System.DateTime":
                    comparator = CompareDates();
                    break;
            }

            if (!comparator)
                return comparator;

            return true;
        }
    }
}