Imports SFD.StringFormat.Exts


Namespace Global.SFD.StringFormat

  Partial Public Class StringFormat




    Public Enum Kinds As Integer
      Empty        '
      None         '
      Digits       ' ::- ( '0' - '9' )+
      BL           ' ::= '{'
      BR           ' ::= '}'
      QBL          ' ::= BL BL
      QBR          ' ::= BR BR
      Quoted       ' ::= QBL | QBR
      Spaces       ' ::= ' '*
      Comma        ' ::= ','
      Colon        ' ::= ':'
      ArgHole      ' ::= BL Arg_Index Arg_Align? Arg_Format? BR 
      Arg_Index    ' ::= Digits Spaces
      Arg_Align    ' ::= Comma Spaces Minus? Digits Spaces
      Arg_Format   ' ::= Colon Tezt
      Minus        ' ::= '-'
      TextChars    ' ::=
      Text         ' ::= Qouted | TextChars
      FormatString ' ::= ( Text | ArgHole )*
      Err_EOT          ' 
      Err_UC
      Err_Malformed_ArgHole
      Err_Malformed_ArgIndex
      Err_Malformed_ArgAlign
      Err_Malformed_ArgFormat
      Err_Malformed_Text
      Err_Malformed_FormatString
    End Enum

    Public Class Constants
      Public Shared ReadOnly Comma As Char = ","c
      Public Shared ReadOnly Colon As Char = ":"c
      Public Shared ReadOnly Space As Char = " "c
      Public Shared ReadOnly Brace_L As Char = "{"c
      Public Shared ReadOnly Brace_R As Char = "}"c
      Public Shared ReadOnly Minus As Char = "-"c
      Friend Shared ReadOnly Digit0 As Char = "0"c
      Friend Shared ReadOnly Digit9 As Char = "9"c
    End Class




    'Private Shared Function Digits(source As SourceText, i As Integer) As SpanKind
    '  ' Digit  ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
    '  ' Digits ::= Digit+ 
    '  Dim si = i
    '  While (i < source.Length)
    '    Dim c = source(i)
    '    If c.HasValue Then
    '      If (c.Value >= Constants.Digit0 AndAlso c.Value <= Constants.Digit9) Then
    '        i += 1
    '      Else
    '        Exit While
    '      End If
    '    Else
    '      Exit While
    '    End If
    '  End While
    '  Dim s = New Span(source, si, i)
    '  If s.Size = 0 Then Return SpanKind.MakeFrom(Kinds.None, source, si, i)
    '  Return SpanKind.MakeFrom(Kinds.Digits, source, si, i)
    'End Function

    'Private Shared Function Spaces(source As SourceText, i As Integer) As SpanKind
    '  ' Spaces ::= ' '*
    '  Dim si = i
    '  While (i < source.Length)
    '    Dim c = source(i)
    '    If c.HasValue = False Then Exit While
    '    If c.Value <> Constants.Space Then Exit While
    '    i += 1
    '  End While
    '  If (si - i) = 0 Then Return SpanKind.MakeFrom(Kinds.None, source, si, i)
    '  Return SpanKind.MakeFrom(Kinds.Spaces, source, si, i)
    'End Function

    'Private Shared Function Comma(Source As SourceText, i As Integer) As SpanKind
    '  ' Comma ::= ','
    '  Dim si = i
    '  Dim c = Source(i)
    '  If c.HasValue AndAlso c.Value = Constants.Comma Then Return SpanKind.MakeFrom(Kinds.Comma, Source, si, i + 1)

    '  Return SpanKind.MakeFrom(Kinds.None, Source, si, i)
    'End Function

    'Private Shared Function Colon(Source As SourceText, i As Integer) As SpanKind
    '  ' Colon ::= ':'
    '  Dim si = i
    '  Dim c = Source(i)
    '  If c.HasValue AndAlso c.Value = Constants.Colon Then Return SpanKind.MakeFrom(Kinds.Colon, Source, si, i + 1)
    '  Return SpanKind.MakeFrom(Kinds.None, Source, si, i)
    'End Function

    'Private Shared Function Minus(Source As SourceText, i As Integer) As SpanKind
    '  ' Minus ::= '-'
    '  Dim si = i
    '  Dim c = Source(i)
    '  If c.HasValue AndAlso c.Value = Constants.Minus Then Return SpanKind.MakeFrom(Kinds.Minus, Source, si, i + 1)
    '  Return SpanKind.MakeFrom(Kinds.None, Source, si, i)
    'End Function

    Private Shared Function BL(Source As SourceText, i As Integer) As SpanKind
      ' BL ::= '{'
      Dim si = i
      Dim c = Source(i)
      If c.HasValue AndAlso c.Value = Constants.Brace_L Then Return SpanKind.MakeFrom(Kinds.BL, Source, si, i + 1)
      Return SpanKind.MakeFrom(Kinds.None, Source, si, i)
    End Function

    Private Shared Function BR(Source As SourceText, i As Integer) As SpanKind
      ' BR ::= '}'
      Dim si = i
      Dim c = Source(i)
      If c.HasValue AndAlso c.Value = Constants.Brace_R Then Return SpanKind.MakeFrom(Kinds.BR, Source, si, i + 1)
      Return SpanKind.MakeFrom(Kinds.None, Source, si, i)
    End Function

    Private Shared Function QBL(source As SourceText, i As Integer) As SpanKind
      '  QBL ::= BL BL
      Dim si = i
      Dim c0 = BL(source, i)
      If c0.Span.Size > 0 Then
        Dim c1 = BL(source, c0.Span.Finish)
        If c1.Span.Size > 0 Then Return SpanKind.MakeFrom(Kinds.QBL, c0, c1)
        Return SpanKind.MakeFrom(Kinds.None, c0, c1)
      End If
      Return SpanKind.MakeFrom(Kinds.None, c0)
    End Function

    Private Shared Function QBR(source As SourceText, i As Integer) As SpanKind
      '  QBR ::= '{' '{'
      Dim si = i
      Dim c0 = BR(source, i)
      If c0.Span.Size > 0 Then
        Dim c1 = BR(source, c0.Span.Finish)
        If c1.Span.Size > 0 Then Return SpanKind.MakeFrom(Kinds.QBR, c0, c1)
        Return SpanKind.MakeFrom(Kinds.None, c0, c1)
      End If
      Return SpanKind.MakeFrom(Kinds.None, c0)
    End Function

    Private Shared Function Quoted(source As SourceText, i As Integer) As SpanKind
      ' Qouted ::= QBL | QBR
      Dim _QL = QBL(source, i) : If _QL = Kinds.QBL Then Return SpanKind.MakeFrom(Kinds.Quoted, _QL, _QL, {_QL})
      Dim _QR = QBR(source, i) : If _QR = Kinds.QBR Then Return SpanKind.MakeFrom(Kinds.Quoted, _QR, _QR, {_QR})
      Return SpanKind.MakeFrom(Kinds.None, _QR)
    End Function

  End Class

End Namespace
