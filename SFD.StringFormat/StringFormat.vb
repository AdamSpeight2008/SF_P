Imports SFD.StringFormat.Exts


Namespace Global.SFD.StringFormat

  Public Class StringFormat




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




    Private Shared Function Digits(source As SourceText, i As Integer) As SpanKind
      ' Digit  ::= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
      ' Digits ::= Digit+ 
      Dim si = i
      While (i < source.Length)
        Dim c = source(i)
        If c.HasValue Then
          If (c.Value >= Constants.Digit0 AndAlso c.Value <= Constants.Digit9) Then
            i += 1
          Else
            Exit While
          End If
        Else
          Exit While
        End If
      End While
      Dim s = New Span(source, si, i)
      If s.Size = 0 Then Return SpanKind.MakeFrom(Kinds.None, source, si, i)
      Return SpanKind.MakeFrom(Kinds.Digits, source, si, i)
    End Function

    Private Shared Function Spaces(source As SourceText, i As Integer) As SpanKind
      ' Spaces ::= ' '*
      Dim si = i
      While (i < source.Length)
        Dim c = source(i)
        If c.HasValue = False Then Exit While
        If c.Value <> Constants.Space Then Exit While
        i += 1
      End While
      If (si - i) = 0 Then Return SpanKind.MakeFrom(Kinds.None, source, si, i)
      Return SpanKind.MakeFrom(Kinds.Spaces, source, si, i)
    End Function

    Private Shared Function Comma(Source As SourceText, i As Integer) As SpanKind
      ' Comma ::= ','
      Dim si = i
      Dim c = Source(i)
      If c.HasValue AndAlso c.Value = Constants.Comma Then Return SpanKind.MakeFrom(Kinds.Comma, Source, si, i + 1)

      Return SpanKind.MakeFrom(Kinds.None, Source, si, i)
    End Function

    Private Shared Function Colon(Source As SourceText, i As Integer) As SpanKind
      ' Colon ::= ':'
      Dim si = i
      Dim c = Source(i)
      If c.HasValue AndAlso c.Value = Constants.Colon Then Return SpanKind.MakeFrom(Kinds.Colon, Source, si, i + 1)
      Return SpanKind.MakeFrom(Kinds.None, Source, si, i)
    End Function

    Private Shared Function Minus(Source As SourceText, i As Integer) As SpanKind
      ' Minus ::= '-'
      Dim si = i
      Dim c = Source(i)
      If c.HasValue AndAlso c.Value = Constants.Minus Then Return SpanKind.MakeFrom(Kinds.Minus, Source, si, i + 1)
      Return SpanKind.MakeFrom(Kinds.None, Source, si, i)
    End Function

    Private Shared Function Arg_Index(Source As SourceText, i As Integer) As SpanKind
      ' Arg_Index ::= Digits Spaces
      Dim parts As New List(Of SpanKind)
      Dim _Digits = Digits(Source, i) : If _Digits.IsNotNone Then parts.Add(_Digits)
      Dim _Spaces = Spaces(Source, _Digits.Span.Finish) : If _Spaces.IsNotNone Then parts.Add(_Spaces)
      If _Digits.IsNotNone Then

        Dim c = Source(_Spaces.Finish)
        If c.HasValue Then
          If (c.Value = Constants.Comma OrElse c.Value = Constants.Colon OrElse c.Value = Constants.Brace_R) Then
            Return SpanKind.MakeFrom(Kinds.Arg_Index, _Digits, _Spaces, parts)
          Else
            parts.Add(SpanKind.MakeFrom(Kinds.Err_UC, Source, _Spaces.Finish, _Spaces.Finish + 1))
            Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgIndex, _Digits, _Spaces, parts)


          End If
        Else
          parts.Add(SpanKind.MakeFrom(Kinds.Err_EOT, Source, _Spaces.Finish, _Spaces.Finish))
          Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgIndex, _Digits, _Spaces, parts)

        End If
      Else
        Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgIndex, _Digits, _Spaces, parts)

        Return SpanKind.MakeFrom(Kinds.None, _Digits, _Spaces)

      End If

    End Function

    Private Shared Function Arg_Align(Source As SourceText, i As Integer) As SpanKind
      ' Arg_Align ::= Comma Spaces Minus? Digits Spaces
      Dim parts As New List(Of SpanKind)
      Dim _Comma = Comma(Source, i) : If _Comma.IsNotNone Then parts.Add(_Comma)
      Dim _SpaceA = Spaces(Source, _Comma.Span.Finish) : If _SpaceA.IsNotNone Then parts.Add(_SpaceA)
      Dim _Minus = Minus(Source, _SpaceA.Span.Finish) : If _Minus.IsNotNone Then parts.Add(_Minus)
      Dim _Digit = Digits(Source, _Minus.Span.Finish) : If _Digit.IsNotNone Then parts.Add(_Digit)
      Dim _SpaceB = Spaces(Source, _Digit.Span.Finish) : If _SpaceB.IsNotNone Then parts.Add(_SpaceB)

      If _Comma.IsNone Then Return SpanKind.MakeFrom(Kinds.None, _Comma)
      If _Minus.IsNone AndAlso _Comma.IsNotNone AndAlso _Digit.IsNotNone Then Return SpanKind.MakeFrom(Kinds.Arg_Align, _Comma, _SpaceB, parts)
      If _Minus.IsNotNone AndAlso _Comma.IsNotNone AndAlso _Digit.IsNone Then Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgAlign, _Comma, _SpaceB, parts)
      If _Minus.IsNotNone AndAlso _Comma.IsNotNone AndAlso _Digit.IsNotNone Then Return SpanKind.MakeFrom(Kinds.Arg_Align, _Comma, _SpaceB, parts)
      Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgAlign, _Comma, _SpaceB, parts)
    End Function

    Private Shared Function Arg_Format(Source As SourceText, i As Integer) As SpanKind
      Dim parts As New List(Of SpanKind)
      Dim ti=0
      Dim _Colon = Colon(Source, i)
      If _Colon = Kinds.Colon Then
        parts.Add(_Colon)
        i = _Colon.Finish
        ti=i
        While i < Source.Length
          Dim c = Source(i)
          If c.HasValue = False Then Exit While
          Dim _Quoted = Quoted(Source, i)
          If _Quoted = Kinds.Quoted Then
            'Dim tp = SpanKind.MakeFrom(Kinds.TextChars, Source, ti, _Quoted.Start)
            'If tp.Span.Size > 0 Then parts.Add(tp)
            'parts.Add(_Quoted)
            i = _Quoted.Finish
            ti = i
          ElseIf c.Value = Constants.Brace_R Then
            Dim tp = SpanKind.MakeFrom(Kinds.TextChars, Source, ti, i)
            If tp.Span.Size > 0 Then parts.Add(tp)
            Return SpanKind.MakeFrom(Kinds.Arg_Format, Source, parts.First.Start, i, parts)

          ElseIf c.Value = Constants.Brace_L Then
            Dim _Error1_ = SpanKind.MakeFrom(Kinds.Err_UC, Source, i, i + 1)
            parts.Add(_Error1_)
            i = _Error1_.Finish
          Else
            i += 1
          End If

        End While
        Dim _Error0_ = SpanKind.MakeFrom(Kinds.Err_EOT, Source, i, i)
        parts.Add(_Error0_)
        Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgFormat, parts.First, parts.Last, parts)
      Else
        Return SpanKind.MakeFrom(Kinds.None, _Colon)
      End If
    End Function

    Private Shared Function Arg_Hole(source As SourceText, i As Integer) As SpanKind
      ' Arg_Hole ::= BL Arg_Index Arg_Align? Arg_Format? BR
      Dim s As New List(Of SpanKind)
      Dim _BL = BL(source, i)
      Dim _ArgIndex = Arg_Index(source, _BL.Span.Finish)
      Dim _ArgAlign = Arg_Align(source, _ArgIndex.Finish)
      Dim _ArgFormat = Arg_Format(source, _ArgAlign.Finish)
      Dim _BR = BR(source, _ArgFormat.Finish)

      If _BL = Kinds.BL Then s.Add(_BL)
      If _ArgIndex = Kinds.Arg_Index Then s.Add(_ArgIndex)
      If _ArgAlign = Kinds.Arg_Align Then s.Add(_ArgAlign)
      If _ArgFormat = Kinds.Arg_Format Then s.Add(_ArgFormat)
      If _BR = Kinds.BR Then s.Add(_BR)

      If _BL.IsNone Then Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgHole, _BL, _BR, s)
      If _BR.IsNone Then Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgHole, _BL, _BR, s)
      If _ArgIndex.IsNotNone AndAlso _ArgIndex <> Kinds.Err_Malformed_ArgIndex Then
        Return SpanKind.MakeFrom(Kinds.ArgHole, _BL, _BR, s)
      End If


      Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgHole, _BL, _BR, s)
    End Function

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
      Dim _QL = QBL(source, i) : If _QL = Kinds.QBL Then Return SpanKind.MakeFrom(Kinds.Quoted, _QL,_QL, {_QL})
      Dim _QR = QBR(source, i) : If _QR = Kinds.QBR Then Return SpanKind.MakeFrom(Kinds.Quoted, _QR, _QR, {_QR})
      Return SpanKind.MakeFrom(Kinds.None, _QR)
    End Function

    Private Shared Function Text(source As SourceText, i As Integer) As SpanKind
      ' Text ::= ( TextChar | Qouted )*
      Dim si = i
      Dim parts As New List(Of SpanKind)
      While i < source.Length
        Dim c = source(i)
        If c.HasValue = False Then Exit While
        Dim q = Quoted(source, i)
        If q = Kinds.Quoted Then
          parts.Add(q)
          i = q.Finish
        ElseIf c.Value = Constants.Brace_L Then
          Dim ti = i
          Dim _ARG_ = Arg_Hole(source,i)
          If _ARG_ = Kinds.ArgHole Then Exit While
          If _ARG_ = Kinds.Err_Malformed_ArgHole Then Exit While 
          Return SpanKind.MakeFrom(Kinds.Err_Malformed_Text,source,si,i+1,parts)
          Exit While
        ElseIf c.Value = Constants.Brace_R Then
          Exit While
        Else
          i += 1
        End If
      End While
      Return SpanKind.MakeFrom(Kinds.Text, source, si, i, parts)
    End Function

    Private Shared Function FormatString(source As SourceText, i As Integer) As SpanKind
      ' FormatString ::= ( ArgHole|Text   )+
      Dim si = i
      Dim parts As New List(Of SpanKind)
      While i < source.Length
        Dim _Text = Text(source, i)
        Dim _ArgHole = Arg_Hole(source, i)

        If _ArgHole = Kinds.ArgHole Then
          parts.Add(_ArgHole)
          i = _ArgHole.Finish
        ElseIf _Text = Kinds.Text AndAlso _Text.IsEmpty = False Then
          parts.Add(_Text )
          i = _Text.Finish
        Else If _ArgHole = Kinds.Err_Malformed_ArgHole Then
          parts.Add(_ArgHole)
          i = _ArgHole.Finish
        ElseIf _Text = Kinds.Err_Malformed_Text Then
          parts.Add(_Text)
          i = _Text.Finish
          Exit While
        End If

      End While
      If parts.Count > 0 Then
        Dim HasErrors = parts.Any(Function(sk) sk.HasError)
        If HasErrors Then Return SpanKind.MakeFrom(Kinds.Err_Malformed_FormatString, parts.First, parts.Last, parts)
        Return SpanKind.MakeFrom(Kinds.FormatString, parts.First, parts.Last, parts)
      End If
      Return SpanKind.MakeFrom(Kinds.None, source, si, si)
    End Function

    Public Shared Function Parse(s As String) As SpanKind
      Return FormatString(SourceText.Create(s), 0)
    End Function

  End Class

End Namespace
