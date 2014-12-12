Imports SFD. StringFormat
Imports SFD. StringFormat. Exts
Imports SFD.StringFormat.StringFormat



Module Module1

  Sub Main()
    '0123456789012

    Dim s = " {8 ,1:C2} {}}"
    'Dim r0 = ""
    'Try
    '  r0 = String.Format(s, 1.2345, 2.45)
    'Catch ex As Exception
    '  Console.WriteLine(ex)
    'End Try
    Dim r1 = Type2_Parser.Parse(SourceText.Create(s))
    'Dim r1 = StringFormat.Parse(s)
    'Dim i = 0
    'Dim r2 = r1.MadeOf(0)(1).GetValue(i)
  End Sub

End Module




