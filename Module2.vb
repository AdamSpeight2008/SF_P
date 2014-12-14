Imports SFD.StringFormat
Imports SFD.StringFormat.Exts
Imports SFD.StringFormat.StringFormat
Module Module2
  Sub Main()
    Dim args As Object() = {1, 1.23, "Apple"}
    Dim fs = "A:= {} B:= {1}  C:= {3}"
    Dim om = Parse(SourceText.Create(fs))
  End Sub
End Module
