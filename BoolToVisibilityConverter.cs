using System;
using System.Globalization;
using System.Windows.Data;

//Вспомогательный класс для окна ввода пароля (текст подсказки)

namespace AES_Crypt_GUI
{
    public class BoolToVisibilityConverter : IMultiValueConverter

    {
        public object Convert(object[] values, Type targetType, object parameter, CultureInfo culture)
        {
            bool hasText = !(bool)values[0];
            bool hasFocus = (bool)values[1];
            if (hasText || hasFocus)
                return System.Windows.Visibility.Collapsed;
            return System.Windows.Visibility.Visible;
        }
        public object[] ConvertBack(object value, Type[] targetTypes, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}
