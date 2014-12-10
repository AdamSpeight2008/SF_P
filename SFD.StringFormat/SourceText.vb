
Namespace Global.SFD.StringFormat

  Public Class SourceText
    Property _S As String = ""
    Private Sub New(s As String)
      _S = If(s, "")
    End Sub

    Default ReadOnly Property [Char](index As Integer) As Char?
      Get
        If index < 0 Then Return New Char?()
        If index >= _S.Length Then Return New Char?()
        Return New Char?(_S(index))
      End Get
    End Property

    Public ReadOnly Property Length() As Integer
      Get
        Return _S.Length
      End Get
    End Property

    Public Shared Function Create(s As String) As SourceText
      Return New SourceText(s)
    End Function

    Public Function GetText(s As Span) As String
      Dim l = s.Size
      If l = 0 Then Return String.Empty
      If s.Start >= Length Then Return String.Empty
      If s.Start < 0 Then Return String.Empty
      Return _S.Substring(s.Start, l)
    End Function

  End Class

End Namespace