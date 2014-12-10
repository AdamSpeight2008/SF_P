Imports SFD. StringFormat
Imports SFD. StringFormat. Exts
Imports SFD.StringFormat.StringFormat



Module Module1

  Sub Main()
    Dim s = "{100,1:C2}{1}"
    Dim r0 = ""
    Try
      r0 = String.Format(s, 1.2345, 2.45)
    Catch ex As Exception
      Console.WriteLine(ex)
    End Try
    Dim r1 = StringFormat.Parse(s)
    Dim i = 0
    Dim r2 = r1.MadeOf(0)(1).GetValue(i)
  End Sub

End Module




