Imports SFD. StringFormat
Imports SFD. StringFormat. Exts
Imports SFD.StringFormat.StringFormat



Module Module1

  Sub Main()
    '0123456789012

    Dim ss = {
      "",
      "A",
      "}",
      "{",
      "{{",
      "}}",
      "{0}",
      "{A}",
      "{0A}",
      "{ 0}",
      "{0 }",
      "{0,}",
      "{0,1}",
      "{0,-1}",
      "{0, 1}",
      "{0, -1}",
      "{0,A}",
      "{0:{",
      "{0:{{",
      "{0:{{{",
      "{0:{{}",
      "{0:}}",
      "{0:}}}",
      "{0:}",
      "{0,1:C2",
      "{0,1:C2}",
      "} X::= {0,9:C1}{0h}"
      }
    '    Dim s = " {9j}"
    'Dim r0 = ""
    'Try
    '  r0 = String.Format(s, 1.2345, 2.45)
    'Catch ex As Exception
    '  Console.WriteLine(ex)
    'End Try
    For Each s In ss
      Dim r As SpanKind = Type2_Parser.Parse(SourceText.Create(s))
      Console.ForegroundColor = ConsoleColor.Red
      Console.Write("{0}", s)
      Console.ForegroundColor = ConsoleColor.White
      Console.WriteLine(" {1}", s, r)
      For Each p In r.MadeOf
        Console.ForegroundColor = ConsoleColor.Green
        Console.WriteLine("  {0}", p)
        For Each pp In p.MadeOf
          Console.ForegroundColor = ConsoleColor.Blue
          Console.WriteLine("    {0}", pp)

        Next
      Next
      Console.WriteLine()
      Console.ReadKey()
    Next
    'Dim r1 = StringFormat.Parse(s)
    'Dim i = 0
    'Dim r2 = r1.MadeOf(0)(1).GetValue(i)
  End Sub

End Module




