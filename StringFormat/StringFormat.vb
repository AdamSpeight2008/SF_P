Imports StringFormat.Exts

Namespace Global.StringFormat

Public Class StringFormat




    Public Enum Kinds As Integer
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

      Public Shared Operator =(sk As SpanKind, k As Kinds ) As Boolean
        If sk Is Nothing Then Return false
        Return sk.Kind = k
      End Operator
      Public Shared Operator <>(sk As SpanKind, k As Kinds) As Boolean
        If sk Is Nothing Then Return False
        Return sk.Kind <> k
      End Operator
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
        If c.HasValue = False Then Exit While
        If c.Value <> Constants.Space Then Exit While
        i+=1
      End While
      If (si-i)=0 Then Return SpanKind.MakeFrom(Kinds.None,si,i)
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
      Dim parts As New List(Of SpanKind)
      Dim _Digits = Digits(Source, i) : If _Digits.IsNotNone Then parts.Add(_Digits)
      Dim _Spaces = Spaces(Source, _Digits.Span.Finish) : If _Spaces.IsNotNone Then parts.Add(_Spaces)
      If _Digits.IsNotNone Then

        Dim c = Source(_Spaces.Finish)
        If c.HasValue Then
          If (c.Value = Constants.Comma OrElse c.Value = Constants.Colon OrElse c.Value = Constants.Brace_R) Then
            Return SpanKind.MakeFrom(Kinds.Arg_Index, _Digits, _Spaces, parts.ToArray)
          Else
            parts.Add(SpanKind.MakeFrom(Kinds.Err_UC, _Spaces.Finish, _Spaces.Finish + 1))
            Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgIndex, _Digits, _Spaces, parts.ToArray)


          End If
        Else
          parts.Add(SpanKind.MakeFrom(Kinds.Err_EOT, _Spaces.Finish, _Spaces.Finish))
          Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgIndex, _Digits, _Spaces, parts.ToArray)

        End If
      Else
        Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgIndex, _Digits, _Spaces, parts.ToArray)

        Return SpanKind.MakeFrom(Kinds.None, _Digits, _Spaces)

      End If

    End Function

    Private Shared Function Arg_Align(Source As SourceText, i As Integer) As SpanKind
      ' Arg_Align ::= Comma Spaces Minus? Digits Spaces
      Dim parts As New List(Of SpanKind)
      Dim _Comma = Comma(Source, i) : If _Comma.IsNotNone  Then parts.Add(_Comma)
      Dim _SpaceA = Spaces(Source, _Comma.Span.Finish) : If _SpaceA.IsNotNone Then parts.Add(_SpaceA)
      Dim _Minus  =  Minus(Source, _SpaceA.Span.Finish) : If _Minus.IsNotNone Then parts.Add(_Minus)
      Dim _Digit  = Digits(Source, _Minus.Span.Finish)  : If _Digit.IsNotNone Then parts.Add(_Digit)
      Dim _SpaceB = Spaces(Source, _Digit.Span.Finish)  : If _SpaceB.IsNotNone Then parts.Add(_SpaceB)

      If _Comma.IsNone Then Return SpanKind.MakeFrom(Kinds.None,_Comma)
      If _Minus.IsNone AndAlso _Comma.IsNotNone AndAlso _Digit.IsNotNone Then Return SpanKind.MakeFrom(Kinds.Arg_Align, _Comma, _SpaceB, parts.ToArray)
      If _Minus.IsNotNone AndAlso _Comma.IsNotNone AndAlso _Digit.IsNotNone Then Return SpanKind.MakeFrom(Kinds.Arg_Align, _Comma, _SpaceB, parts.ToArray)
      Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgAlign, _Comma, _SpaceB, parts.ToArray)
    End Function

    Private Shared Function Arg_Format(Source As SourceText, i As Integer) As SpanKind
      Dim parts As New List(Of SpanKind)

      Dim _Colon = Colon(Source, i)
      If _Colon = Kinds.Colon Then
        parts.Add(_Colon)
        i = _Colon.Finish
        While i < Source.Length
          Dim c = Source(i)
          If c.HasValue = False Then Exit While
          Dim _Quoted = Quoted(Source, i)
          If _Quoted = Kinds.Quoted Then
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
        Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgFormat, parts.First, parts.Last, parts.ToArray)
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

    If _BL = Kinds.BL Then s.Add(_BL)
    If _ArgIndex = Kinds.Arg_Index Then s.Add(_ArgIndex)
    If _ArgAlign = Kinds.Arg_Align Then s.Add(_ArgAlign)
    If _ArgFormat = Kinds.Arg_Format Then s.Add(_ArgFormat)
    If _BR = Kinds.BR Then s.Add(_BR)

    If _BL.IsNone Then  Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgHole, _BL, _BR, s.ToArray)
      If _BR.IsNone Then Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgHole, _BL, _BR, s.ToArray)
      If _ArgIndex.IsNotNone AndAlso _ArgIndex <> Kinds.Err_Malformed_ArgIndex Then
        Return SpanKind.MakeFrom(Kinds.ArgHole, _BL, _BR, s.ToArray)
      End If


      Return SpanKind.MakeFrom(Kinds.Err_Malformed_ArgHole, _BL, _BR, s.ToArray)
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
    Dim _QL = QBL(source, i) : If _QL = Kinds.QBL Then Return SpanKind.MakeFrom(Kinds.Quoted, _QL, _QL, _QL)
    Dim _QR = QBR(source, i) : If _QR = Kinds.QBR Then Return SpanKind.MakeFrom(Kinds.Quoted, _QR, _QR, _QR)
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

          Exit While
        ElseIf c.Value = Constants.Brace_R Then
          Exit While
        Else
          i += 1
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

        If _ArgHole = Kinds.ArgHole Then
          parts.Add(_ArgHole)
          i = _ArgHole.Finish
        ElseIf _ArgHole = Kinds.Err_Malformed_ArgHole Then
          parts.Add(_ArgHole)
          i = _ArgHole.Finish
        ElseIf _Text = Kinds.Text Then
        parts.Add(_Text)
        i = _Text.Finish
      Else
        i += 1
      End If
    End While
    If parts.Count >0 Then
       Dim HasErrors = parts.Any(Function(sk) sk.HasError)
       If HasErrors Then Return SpanKind.MakeFrom(Kinds.Err_Malformed_FormatString, parts.First, parts.Last, parts.ToArray)
Return SpanKind.MakeFrom(Kinds.FormatString, parts.First, parts.Last, parts.ToArray)
    End If
      Return SpanKind.MakeFrom(Kinds.None, si, si)
    End Function

  Public Shared Function Parse(s As String) As SpanKind
    Return FormatString(SourceText.Create(s), 0)
  End Function

End Class

End Namespace 
