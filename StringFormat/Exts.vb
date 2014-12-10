Imports System.Runtime.CompilerServices
Imports StringFormat.StringFormat
Namespace Global.StringFormat

  Public Module Exts

    <Extension>
    Public Function IsNone(sk As SpanKind) As Boolean
      Return sk = Kinds.None
    End Function
    <Extension>
    Public Function IsNotNone(sk As SpanKind) As Boolean
      Return sk <> Kinds.None
    End Function
    <Extension>
    Public Function HasError(sk As SpanKind) As Boolean
      Select Case sk.Kind
        Case Kinds.Err_EOT,
               Kinds.Err_Malformed_ArgAlign,
               Kinds.Err_Malformed_ArgFormat,
               Kinds.Err_Malformed_ArgHole,
               Kinds.Err_Malformed_ArgIndex,
               Kinds.Err_Malformed_Text,
               Kinds.Err_UC
          Return True
      End Select
      Return False
    End Function

    <Extension>
    Public Function GetText(sk As SpanKind) As String
      Return sk.Span.Source.GetText(sk.Span)
    End Function
    <Extension>
    Public Function GetSourceText(sk As SpanKind) As SourceText
      Return sk.Span.Source 
    End Function
    <Extension>
    Public Function GetValue(sk As SpanKind, ByRef value As integer) As Boolean
      If sk = Kinds.Arg_Index Then
        Dim Digits = sk.MadeOf.FirstOrDefault(Function(_sk) _sk = Kinds.Digits)
        Dim rawText = Digits.GetText()
        Return Integer.TryParse(rawText, value)
      ElseIf sk = Kinds.Arg_Format Then
        Dim Minus = sk.MadeOf.FirstOrDefault(Function(_sk) _sk = Kinds.Minus)
        Dim Digits = sk.MadeOf.FirstOrDefault(Function(_sk) _sk = Kinds.Digits)
        If Digits Is Nothing Then Return False
        Dim rawText = If(Minus IsNot Nothing, "-", "") & Digits.GetText()
        Return Integer.TryParse(rawText, value)
      Else
        value = 0
        Return False

      End If
    End Function

  End Module
End Namespace