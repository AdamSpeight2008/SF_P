Namespace Global.StringFormat

Public Class StringFormat


  Public Enum Kinds As Integer
    None         '
      Err_EOT          ' 
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
      Err_UC
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


    Public Class SpanKind

      Public ReadOnly Property Span As Span
      Public ReadOnly Property Kind As Kinds
      Public ReadOnly Property MadeOf As New List(Of SpanKind)

      Private Sub New(kind As Kinds, span As Span, ParamArray MadeOf() As SpanKind)
        _Span = span
        _Kind = kind
        _MadeOf.AddRange(MadeOf)
      End Sub

      Shared Operator IsTrue(s As SpanKind) As Boolean
        Return s.Kind <> Kinds.None
      End Operator

      Shared Operator IsFalse(s As SpanKind) As Boolean
        Return s.Kind = Kinds.None
      End Operator

      Public Shared Function MakeFrom(kind As Kinds, Start As SpanKind, Finish As SpanKind, ParamArray MadeOf() As SpanKind) As SpanKind
        Dim span = New Span(Start.Span.Start, Finish.Span.Finish)
        Return New SpanKind(kind, span, MadeOf)
      End Function

      Public Shared Function MakeFrom(kind As Kinds, Start As Integer, Finish As Integer, ParamArray MadeOf() As SpanKind) As SpanKind
        Dim span = New Span(Start, Finish)
        Return New SpanKind(kind, span, MadeOf)
      End Function

      Public Shared Function MakeFrom(kind As Kinds, StartAndFinish As SpanKind) As SpanKind
        Dim span = New Span(StartAndFinish.Span.Start, StartAndFinish.Span.Finish)
        Return New SpanKind(kind, span)
      End Function

      Public ReadOnly Property Start() As Integer
        Get
          Return Span.Start
        End Get
      End Property

      Public ReadOnly Property Finish() As Integer
        Get
          Return Span.Finish
        End Get
      End Property

      Public Overrides Function ToString() As String
        Return String.Format("{0}  {1}", Span, Kind.ToString("F"))
      End Function

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
      Dim s = New Span(si, i)
      If s.Size = 0 Then Return SpanKind.MakeFrom(Kinds.None, si, i)
      Return SpanKind.MakeFrom(Kinds.Digits, si, i)
    End Function

    Private Shared Function Spaces(source As SourceText, i As Integer) As SpanKind
      ' Spaces ::= ' '*
      Dim si = i
      While (i < source.Length)
        Dim c = source(i)
        If c.HasValue AndAlso (c.Value = Constants.Space) Then
          i += 1
        Else
          Exit While
        End If
      End While
      Return SpanKind.MakeFrom(Kinds.Spaces, si, i)
    End Function

    Private Shared Function Comma(Source As SourceText, i As Integer) As SpanKind
      ' Comma ::= ','
      Dim si = i
      Dim c = Source(i)
      If c.HasValue AndAlso c.Value = Constants.Comma Then Return SpanKind.MakeFrom(Kinds.Comma, si, i + 1)

      Return SpanKind.MakeFrom(Kinds.None, si, i)
    End Function

    Private Shared Function Colon(Source As SourceText, i As Integer) As SpanKind
      ' Colon ::= ':'
      Dim si = i
      Dim c = Source(i)
      If c.HasValue AndAlso c.Value = Constants.Colon Then Return SpanKind.MakeFrom(Kinds.Colon, si, i + 1)
      Return SpanKind.MakeFrom(Kinds.None, si, i)
    End Function

    Private Shared Function Minus(Source As SourceText, i As Integer) As SpanKind
      ' Minus ::= '-'
      Dim si = i
      Dim c = Source(i)
      If c.HasValue AndAlso c.Value = Constants.Minus Then Return SpanKind.MakeFrom(Kinds.Minus, si, i + 1)
      Return SpanKind.MakeFrom(Kinds.None, si, i)
    End Function

    Private Shared Function Arg_Index(Source As SourceText, i As Integer) As SpanKind
      ' Arg_Index ::= Digits Spaces
      Dim _Digits = Digits(Source, i)
      Dim _Spaces = Spaces(Source, _Digits.Span.Finish)
      Dim r = SpanKind.MakeFrom(Kinds.Arg_Index, _Digits, _Spaces, _Digits, _Spaces)
      If r.Span.Size > 0 Then Return r
      Return SpanKind.MakeFrom(Kinds.None, _Digits, _Spaces)
    End Function

    Private Shared Function Arg_Align(Source As SourceText, i As Integer) As SpanKind
      ' Arg_Align ::= Comma Spaces Minus? Digits Spaces
      Dim _Comma = Comma(Source, i)
      Dim _SpaceA = Spaces(Source, _Comma.Span.Finish)
      Dim _Minus = Minus(Source, _SpaceA.Span.Finish)
      Dim _Digit = Digits(Source, _Minus.Span.Finish)
      Dim _SpaceB = Spaces(Source, _Digit.Span.Finish)
      Dim s = SpanKind.MakeFrom(Kinds.Arg_Align, _Comma, _SpaceB, _Comma, _SpaceA, _Minus, _Digit, _SpaceB)
      If s.Span.Size > 0 Then Return s
      Return SpanKind.MakeFrom(Kinds.None, _Comma, _SpaceB)
    End Function

    Private Shared Function Arg_Format(Source As SourceText, i As Integer) As SpanKind
      'Throw New System.NotImplementedException
      Dim parts As New List(Of SpanKind)

      Dim _Colon = Colon(Source, i)
      If _Colon.Kind = Kinds.Colon Then
        parts.Add(_Colon)
        i = _Colon.Finish
        While i < Source.Length
          Dim c = Source(i)
          If c.HasValue = False Then Exit While
          Dim _Quoted = Quoted(Source, i)
          If _Quoted.Kind = Kinds.Quoted Then
            parts.Add(_Quoted)
            i = _Quoted.Finish
          ElseIf c.Value = Constants.Brace_R Then
            Return SpanKind.MakeFrom(Kinds.Arg_Format, parts.First.Start, i, parts.ToArray)

          ElseIf c.Value = Constants.Brace_L Then
            Dim _Error1_ = SpanKind.MakeFrom(Kinds.Err_UC, i, i + 1)
            parts.Add(_Error1_)
            i = _Error1_.Finish
          Else
            i += 1
          End If

        End While
        Dim _Error0_ = SpanKind.MakeFrom(Kinds.Err_EOT, i, i)
        parts.Add(_Error0_)
        Return SpanKind.MakeFrom(Kinds.Arg_Format, parts.First, parts.Last, parts.ToArray)
    Else
        Return SpanKind.MakeFrom(Kinds.None,_Colon)
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

    If _BL.Kind = Kinds.BL Then s.Add(_BL)
    If _ArgIndex.Kind = Kinds.Arg_Index Then s.Add(_ArgIndex)
    If _ArgAlign.Kind = Kinds.Arg_Align Then s.Add(_ArgAlign)
    If _ArgFormat.Kind = Kinds.Arg_Format Then s.Add(_ArgFormat)
    If _BR.Kind = Kinds.BR Then s.Add(_BR)

    If (_BL.Kind = Kinds.None) OrElse (_BR.Kind = Kinds.None) OrElse (_ArgIndex.Kind = Kinds.None) Then
      Return SpanKind.MakeFrom(Kinds.None, _BL, _BR, s.ToArray)
    End If

    Dim p = SpanKind.MakeFrom(Kinds.ArgHole, _BL, _BR, s.ToArray)
    Return p
  End Function

  Private Shared Function BL(Source As SourceText, i As Integer) As SpanKind
    ' BL ::= '{'
    Dim si = i
    Dim c = Source(i)
    If c.HasValue AndAlso c.Value = Constants.Brace_L Then Return SpanKind.MakeFrom(Kinds.BL, si, i + 1)
    Return SpanKind.MakeFrom(Kinds.None, si, i)
  End Function

  Private Shared Function BR(Source As SourceText, i As Integer) As SpanKind
    ' BR ::= '}'
    Dim si = i
    Dim c = Source(i)
    If c.HasValue AndAlso c.Value = Constants.Brace_R Then Return SpanKind.MakeFrom(Kinds.BR, si, i + 1)
    Return SpanKind.MakeFrom(Kinds.None, si, i)
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
    Dim _QL = QBL(source, i)
    If _QL.Kind = Kinds.QBL Then Return SpanKind.MakeFrom(Kinds.Quoted, _QL, _QL, _QL)
    Dim _QR = QBR(source, i)
    If _QR.Kind = Kinds.QBR Then Return SpanKind.MakeFrom(Kinds.Quoted, _QR, _QR, _QR)
    Return SpanKind.MakeFrom(Kinds.None, _QR)
  End Function

  Private Shared Function Text(source As SourceText, i As Integer) As SpanKind
    ' Text ::= ( TextChar | Qouted )*
    Dim si = i
    Dim parts As New List(Of SpanKind)
    While i < source.Length
      Dim c = source(i)
      Dim q = Quoted(source, i)
      If q.Kind = Kinds.QBL OrElse q.Kind = Kinds.QBR Then
        parts.Add(q)
        i = q.Finish
      Else
        'Dim _BR = BR(source, i)
        'If _BR.Kind = Kinds.BR Then
        '  Exit While
        'Else
        i += 1
        'End If
      End If
    End While
    Return SpanKind.MakeFrom(Kinds.Text, si, i, parts.ToArray)
  End Function

  Private Shared Function FormatString(source As SourceText, i As Integer) As SpanKind
    ' FormatString ::= ( Text | ArgHole )+
    Dim si = i
    Dim parts As New List(Of SpanKind)
    While i < source.Length
      Dim _Text = Text(source, i)
      Dim _ArgHole = Arg_Hole(source, i)

      If _ArgHole.Kind = Kinds.ArgHole Then
        parts.Add(_ArgHole)
        i = _ArgHole.Finish
      ElseIf _Text.Kind = Kinds.Text Then
        parts.Add(_Text)
        i = _Text.Finish
      Else
        Debugger.Break()
      End If
    End While
    If parts.Any Then Return SpanKind.MakeFrom(Kinds.FormatString, parts.First, parts.Last, parts.ToArray)
    Return SpanKind.MakeFrom(Kinds.None, si, si)
  End Function

  Public Shared Function Parse(s As String) As SpanKind
    Return FormatString(SourceText.Create(s), 0)
  End Function

End Class

End Namespace 
