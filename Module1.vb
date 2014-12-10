Imports StringFormat


Module Module1

  Sub Main()
    Dim s = "{0, -1 : C2"
    Dim r0 = ""
    Try
      r0 = String.Format(s, 1.2345, 2.45)
    Catch ex As Exception
      Console.WriteLine(ex)
    End Try
    Dim r1 = StringFormat.StringFormat.Parse(s)

  End Sub

End Module




