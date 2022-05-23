using System.Drawing;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Interop;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using Microsoft.Win32;
using static AES_Crypt_CLI; // Подключаем логику

namespace AES_Crypt_GUI
{

    // Графическая часть C#
    public static class JunkClass
    {
        public static byte[] IconToBytes(Icon icon)
        {
            using (MemoryStream ms = new MemoryStream())
            {
                icon.Save(ms);
                return ms.ToArray();
            }
        }

        public static Icon BytesToIcon(byte[] bytes)
        {
            using (MemoryStream ms = new MemoryStream(bytes))
            {
                return new Icon(ms);
            }
        }
        public static ImageSource ToImageSource(this Icon icon)
        {
            ImageSource imageSource = Imaging.CreateBitmapSourceFromHIcon(
                icon.Handle,
                Int32Rect.Empty,
                BitmapSizeOptions.FromEmptyOptions());

            return imageSource;
        }
    }
    public partial class MainWindow : Window
    {

        string file = "";
        string pass = "";
        bool isCrypting = true;

        public MainWindow()
        {
            InitializeComponent();
        }

        private void TextBox_TextChanged(object sender, TextChangedEventArgs e)
        {
            pass = this.passTextBox.Text;
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            OpenFileDialog openFileDialog = new OpenFileDialog();
            openFileDialog.InitialDirectory = Path.GetFullPath(Path.Combine(Path.GetDirectoryName(System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName), @"..\..\..\data"));
            openFileDialog.ShowDialog();
            file = openFileDialog.FileName;
            this.Title = "AES Crypt GUI: " + file;
            this.goButton.IsEnabled = true;
        }

        private void GoButton_Click(object sender, RoutedEventArgs e)
        {
            string newNamePart = "";
            if (isCrypting)
            {
                newNamePart = "_enc.txt";
            }
            else
            {
                newNamePart = "_dec.txt";
            }
            aes(isCrypting, file, file.Remove(file.Length - 4) + newNamePart, pass);
            this.Title += " - Done!";
            System.Threading.Thread.Sleep(3000);
            this.Title = this.Title.Remove(this.Title.Length - 8);
        }

        private void EncRadio_Checked(object sender, RoutedEventArgs e)
        {
            isCrypting = true;
            Icon icon = JunkClass.BytesToIcon(AES_Crypt_GUI.Properties.Resources._lock);
            this.Icon = JunkClass.ToImageSource(icon);

        }

        private void DecRadio_Checked(object sender, RoutedEventArgs e)
        {
            isCrypting = false;
            Icon icon = JunkClass.BytesToIcon(AES_Crypt_GUI.Properties.Resources.unlock);
            this.Icon = JunkClass.ToImageSource(icon);
        }
    }
}
