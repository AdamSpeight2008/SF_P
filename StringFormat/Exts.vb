Imports System.Runtime.CompilerServices
Imports StringFormat.StringFormat
Namespace Global.StringFormat

  Public Module Exts

    <Extension>
    Public Function IsNone(sk As SpanKind) As Boolean
      Return sk = Kinds.None
    End Function
    <Extension>
    Public Function IsNotNone(sk As StringFormat.SpanKind) As Boolean
      Return sk <> Kinds.None
    End Function
    <Extension>
    Public Function HasError(sk As StringFormat.SpanKind) As Boolean
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
  End Module
End Namespace