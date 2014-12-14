Namespace Global.SFD.StringFormat

  Public Class StringFormat


    Public Shared Function Parse(sr As SourceText) As SpanKind
      Dim spans As New List(Of SpanKind)
      Dim InHole As Boolean = False
      Dim InErrorState = False
      Dim IsQuoted = False
      Dim i = 0
      While i < sr.Length
        Select Case True
          Case sr(i) = Constants.Brace_L AndAlso sr(i + 1) = Constants.Brace_L
            IsQuoted = True
            spans.Add(SpanKind.MakeFrom(StringFormat.Kinds.QBL, sr, i, i + 2))
          Case (sr(i) = Constants.Brace_L) AndAlso Not InHole
            Dim si = i
            InHole = True
            Dim arg_Hole = Parse_ArgHole(sr, i)
            spans.Add(arg_Hole)
            If arg_Hole.HasError Then
              InErrorState = True
              'spans.Add(SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgHole, sr, si, i, {arg_Hole}))
            Else
              'spans.Add(arg_Hole)

            End If

            If Not InErrorState Then InHole = False
          Case (sr(i) = Constants.Brace_L) AndAlso InHole ' Parsing Error: Recursize hole not allowed
            spans.Add(SpanKind.MakeFrom(StringFormat.Kinds.Err_UC, sr, i, i + 1))
          Case (sr(i) = Constants.Brace_R ) AndAlso (sr(i + 1) = Constants.Brace_R )
            IsQuoted = True
            spans.Add(SpanKind.MakeFrom(StringFormat.Kinds.QBR, sr, i, i + 2))

          Case (sr(i) =Constants.Brace_R ) AndAlso Not InHole
            ' Parsing Error: Mismatched 
            spans.Add(SpanKind.MakeFrom(StringFormat.Kinds.Err_UC, sr, i, i + 1))
          Case (sr(i) = Constants.Brace_R ) AndAlso InHole
            Dim item = spans.RemoveLast()
            Dim mfh = SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgHole, sr, item.Start, i + 1, item.MadeOf)
            spans.Add(mfh)
            InHole = False
            InErrorState = False
        End Select
        If IsQuoted Then
          i += 1
          IsQuoted = False
        End If
        i += 1
      End While
      Return SpanKind.MakeFrom(StringFormat.Kinds.FormatString, sr, 0, i, spans)
    End Function

 

    Private  Shared Function Parse_Arg_Index(sr As SourceText,
                                ByRef i As Integer) As SpanKind
      Dim si = i
      Dim parts As New List(Of SpanKind)
      If sr(i).IsDigit Then
        Dim Digits = sr.Parse_Digits(i)
        parts.Add(Digits)
        If i >= sr.Length Then Return UnepectedEOT(StringFormat.Kinds.Err_Malformed_ArgIndex, sr, si, Digits.Finish + 1, parts)

        Dim RawText = Digits.GetText
        Dim value = 0
        If RawText.Length > 0 AndAlso Integer.TryParse(RawText, value) Then
          Return SpanKind.MakeFrom(StringFormat.Kinds.Arg_Index, sr, si, Digits.Finish, parts)
        Else
        End If
      End If
      Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgIndex, sr, si, i + 1, parts)
    End Function

    Private Shared Function Parse_Arg_Alignment(sr As SourceText, ByRef i As Integer) As SpanKind
      Dim si = i
      Dim spaces = Consume_Spaces(sr, i)
      'Dim Raw = ""
      Dim parts As New List(Of SpanKind)
      Dim minus As SpanKind = Nothing
      If sr(i) = Constants.Minus Then
        minus = SpanKind.MakeFrom(StringFormat.Kinds.Minus, sr, i, i + 1)
        parts.Add(minus)
        i += 1
      End If
      If sr(i).IsDigit Then
        Dim digits = sr.Parse_Digits(i)
        parts.Add(digits)
        Dim trailingSpaces = sr.Consume_Spaces(i)
        If trailingSpaces.Span.Size > 0 Then parts.Add(trailingSpaces)
        Return SpanKind.MakeFrom(StringFormat.Kinds.Arg_Align, sr, si, i, parts)
      Else
        i+=1
        Dim trailingSpaces = sr.Consume_Spaces(i)
        If trailingSpaces.Span.Size > 0 Then parts.Add(trailingSpaces)
        If i >= sr.Length Then Return UnepectedEOT(StringFormat.Kinds.Err_Malformed_ArgAlign, sr, si, i, parts)
        'Dim uec = SpanKind.MakeFrom(StringFormat.Kinds.Err_UC, sr, i, i + 1)
        'parts(uec)
        ' Return UnepectedEOT(StringFormat.Kinds.Err_Malformed_ArgAlign,sr,si,i, parts)
        Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgAlign, sr, si, i + 1, parts)
      End If
    End Function


    Private  Shared Function Parse_Arg_Format(sr As SourceText, ByRef i As Integer) As SpanKind
      Dim si = i
      Dim parts As New List(Of SpanKind)
      While (i < sr.Length)
        Dim c = sr(i)
        Dim q = Quoted(sr, i)
        If (c = Constants.Brace_L) Then
          If q Then
            i += 1
          Else
            Return UnepectedEOT(StringFormat.Kinds.Err_Malformed_ArgFormat, sr, si, i + 1, parts)
            'Dim uc = SpanKind.MakeFrom(StringFormat.Kinds.BL, sr, i, i + 1)
            ' Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgFormat, sr, si, i, {uc})
          End If
        ElseIf (c = Constants.Brace_R ) Then
          If q Then
            i += 1
          Else
            Exit While
            'Dim uc = SpanKind.MakeFrom(StringFormat.Kinds.BR, sr, i, i + 1)
            'Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgFormat, sr, si, i, {uc})

          End If
        End If
        i += 1
      End While
      If i >= sr.Length Then Return UnepectedEOT(StringFormat.Kinds.Err_Malformed_ArgFormat, sr, si, i + 1, parts)

      Return SpanKind.MakeFrom(StringFormat.Kinds.Arg_Format, sr, si, i)
    End Function



    Private Shared Function Parse_ArgHole(sr As SourceText,
                                ByRef i As Integer) As SpanKind
      Dim LeftEdgeOfHole = i
      Dim parts As New List(Of SpanKind)
      parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.BR, sr, i, i + 1))
      i += 1
      If i >= sr.Length Then Return UnepectedEOT(sr, LeftEdgeOfHole, i, parts)

      Dim Index_Value As SpanKind = Nothing
      Dim Align_Value As SpanKind = Nothing
      Dim Format_Value As SpanKind = Nothing
      If sr(i).IsDigit Then
        Index_Value = Parse_Arg_Index(sr, i)
        parts.Add(Index_Value)
        If Index_Value.HasError Then Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgHole, sr, LeftEdgeOfHole, i + 1, parts)
      Else
        ' Add parser for identifers here
      End If
      Dim si = i
      sr.Consume_Spaces(i)
      If (si - i) > 0 Then parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.Spaces, sr, si, i + 1))
      ' Alignment 
      If sr(i) = Constants.Comma Then
        parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.Comma, sr, i, i + 1))
        i += 1
        If i >= sr.Length Then Return UnepectedEOT(sr, LeftEdgeOfHole, i, parts)
        Align_Value = Parse_Arg_Alignment(sr, i)
        parts.Add(Align_Value)
        If Align_Value.HasError Then Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgHole, sr, LeftEdgeOfHole, i + 1, parts)
      End If
      ' Formatting
      If sr(i) = Constants.Colon  Then
        parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.Colon, sr, i, i + 1))
        i += 1
        Format_Value = Parse_Arg_Format(sr, i)
        parts.Add(Format_Value)
        If Format_Value.HasError Then Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgHole, sr, LeftEdgeOfHole, i + 1, parts)
      ElseIf sr(i) = Constants.Brace_R  Then

      Else
        If i >= sr.Length Then Return UnepectedEOT(sr, LeftEdgeOfHole, i, parts)
        Return UnexpectedChar(sr, LeftEdgeOfHole, i, parts)
      End If
      parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.BR, sr, i, i + 1))
      If parts.Count <= 2 Then Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgHole, sr, LeftEdgeOfHole, i + 1, parts)
      If Index_Value Is Nothing Then Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgHole, sr, LeftEdgeOfHole, i + 1, parts)


      Return SpanKind.MakeFrom(StringFormat.Kinds.ArgHole, sr, LeftEdgeOfHole, i + 1, parts)
    End Function

    Private Shared Function UnexpectedChar(sr As SourceText, si As Integer, i As Integer, ByRef parts As List(Of SpanKind)) As SpanKind
      Return UnexpectedChar(StringFormat.Kinds.Err_Malformed_ArgHole, sr, si, i, parts)
    End Function

    Private Shared Function UnepectedEOT(sr As SourceText, si As Integer, i As Integer, ByRef parts As List(Of SpanKind)) As SpanKind
      Return UnepectedEOT(StringFormat.Kinds.Err_Malformed_ArgHole, sr, si, i, parts)
    End Function

    Private Shared Function UnexpectedChar(sk As StringFormat.Kinds, sr As SourceText, si As Integer, i As Integer, ByRef parts As List(Of SpanKind)) As SpanKind
      'If si > i Then Throw New Exception
      parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.Err_UC, sr, si, i + 1))
      Return SpanKind.MakeFrom(sk, sr, si, i + 2, parts)
    End Function

    Private Shared Function UnepectedEOT(sk As StringFormat.Kinds, sr As SourceText, si As Integer, i As Integer, ByRef parts As List(Of SpanKind)) As SpanKind
      'If si>i Then Throw New Exception 
      parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.Err_EOT, sr, i - 1, i))
      Return SpanKind.MakeFrom(sk, sr, si, i + 1, parts)
    End Function
    End Class
   
    Module _Exts

    <Extension>
    Public Function IsDigit(c As Char?) As Boolean
      Return c.HasValue AndAlso (("0"c <= c.Value) AndAlso (c.Value <= "9"c))
    End Function

    <Extension>
    Public Function Parse_Digits(sr As SourceText, ByRef i As Integer) As SpanKind
      Dim si = i
      While i < sr.Length AndAlso sr(i).IsDigit
        i += 1
      End While
      Return SpanKind.MakeFrom(If((i - si) = 0, StringFormat.Kinds.Empty, StringFormat.Kinds.Digits), sr, si, i)
    End Function

    <Extension>
    Function Consume_Spaces(sr As SourceText, ByRef i As Integer) As SpanKind
      Dim si = i
      While si < sr.Length AndAlso (sr(i) = " "c)
        i += 1
      End While
      Return SpanKind.MakeFrom(If((i - si) = 0, StringFormat.Kinds.Empty, StringFormat.Kinds.Spaces), sr, si, i)
    End Function

    <Extension>
    Function RemoveLast(Of T As Class)(ByRef l As List(Of T)) As T
      If l Is Nothing Then Return Nothing
      Dim lastIndex = l.Count - 1
      Dim lastItem = l(lastIndex)
      l.RemoveAt(lastIndex)
      Return lastItem
    End Function

    <Extension>
    Function Quoted(fs As SourceText, i As Integer) As Boolean
      Return fs(i).Equals(fs(i + 1))
    End Function

  End Module
End Namespace

