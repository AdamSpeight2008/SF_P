Public Module Type2_Parser



  Function Quoted(fs As SourceText, i As Integer) As Boolean
    Return fs(i).Equals( fs(i + 1))
  End Function
  Public Function Parse(sr As SourceText) As SpanKind
    Dim spans As New List(Of SpanKind)
    Dim InHole As Boolean = False
    Dim InErrorState = False
    Dim IsQuoted = False
    Dim i = 0
    While i < sr.Length
      Select Case True
        Case sr(i) = "{"c AndAlso sr(i + 1) = "{"c
          IsQuoted = True
          spans.Add(SpanKind.MakeFrom(StringFormat.Kinds.QBL, sr, i, i + 2))
        Case (sr(i) = "{"c) AndAlso Not InHole
          Dim si = i
          InHole = True
          Dim arg_Hole = Parse_ArgHole(sr, i)
          If arg_Hole.HasError Then
              InErrorState = True
            spans.Add( SpanKind.MakeFrom( StringFormat.Kinds.Err_Malformed_ArgHole,sr,si,i  ) )
            Else
          spans.Add(arg_Hole)
          End If

          If Not InErrorState Then InHole = False
        Case (sr(i) = "{"c) AndAlso InHole ' Parsing Error: Recursize hole not allowed
          spans.Add(SpanKind.MakeFrom(StringFormat.Kinds.Err_UC, sr, i, i + 1))
        Case (sr(i) = "}"c) AndAlso (sr(i + 1) = "}"c)
          IsQuoted = True
          spans.Add(SpanKind.MakeFrom(StringFormat.Kinds.QBR, sr, i, i + 2))

        Case (sr(i) = "}"c)
          If Not InHole Then
            ' Parsing Error: Mismatched brace.
            spans.Add(SpanKind.MakeFrom(StringFormat.Kinds.Err_UC, sr, i, i + 1))
          Else
            InHole = False
            InErrorState = False
          End If
      End Select
      If IsQuoted Then
        i += 1
        IsQuoted = False
      End If
      i += 1
    End While
    Return SpanKind.MakeFrom(StringFormat.Kinds.FormatString, sr, 0, i, spans)
  End Function

  Private Function Parse_Arg_Index(sr As SourceText,
                              ByRef i As Integer) As SpanKind
    Dim si=i
 Dim Digits As SpanKind
    If sr(i).IsDigit Then
      'If sr(i).IsDigit = False Then
      '  Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgIndex, sr, i, i + 1, {SpanKind.MakeFrom(StringFormat.Kinds.Err_UC, sr, i, i + 1)})
      'End If
      Digits = sr.Parse_Digits(i)
      Dim RawText = Digits.GetText
      Dim value = 0
      If Integer.TryParse(RawText, value) Then Return SpanKind.MakeFrom(StringFormat.Kinds.Arg_Index, sr, si, si, {Digits})
    End If
    Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgIndex,sr, si, i, {Digits})
  End Function

  Private Function Parse_Arg_Alignment(sr As SourceText,
                              ByRef i As Integer) As SpanKind
    Dim Raw = ""
    Dim si = i
    Dim minus As SpanKind = Nothing
    If sr(i) = "-"c Then
      minus = SpanKind.MakeFrom(StringFormat.Kinds.Minus,sr , i, i + 1)
      i += 1
    End If
    If sr(i).IsDigit Then
      Dim digits = sr.Parse_Digits(i)
      Return SpanKind.MakeFrom(StringFormat.Kinds.Arg_Align, sr, si, i, {minus, digits})
    Else
      Return SpanKind.MakeFrom(StringFormat.Kinds.Err_UC,sr , i, i + 1)
    End If
  End Function


  Private Function Parse_Arg_Format(sr As SourceText,
                              ByRef i As Integer) As SpanKind
    Dim si = i
    Dim EoAF = False 
    While (i < sr.Length)
      Dim c = sr(i) 
      Dim q = Quoted(sr,i)
      If (c = "{"c) Then
        If q Then
          i += 1
        Else
          Dim uc = SpanKind.MakeFrom(StringFormat.Kinds.BL,sr,i,i+1)
          Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgFormat, sr, si, i, {uc})
        End If
      ElseIf (c="}"c) Then
        If q Then
          i+=1
        Else
          Dim uc = SpanKind.MakeFrom(StringFormat.Kinds.BR, sr, i, i + 1)
          Return SpanKind.MakeFrom(StringFormat.Kinds.Err_Malformed_ArgFormat, sr, si, i, {uc})

        End If
      End If
      i += 1
    End While
    Return SpanKind.MakeFrom(StringFormat.Kinds.Arg_Format, sr, si, i)
  End Function



  Private Function Parse_ArgHole(sr As SourceText,
                              ByRef i As Integer) As SpanKind
    Dim LeftEdgeOfHole = i
    Dim parts As New List(Of SpanKind)
    parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.BR,sr,i,i+1))
    i += 1
    If i >= sr.Length Then Return SpanKind.MakeFrom(StringFormat.Kinds.Err_EOT, sr, i - 1, i)

    Dim Index_Value As SpanKind = Nothing
    Dim Align_Value As SpanKind = Nothing
    Dim Format_Value As SpanKind = Nothing
    If sr(i).IsDigit Then
      Index_Value = Parse_Arg_Index(sr, i)
      'If Index_Value.HasError Then Return res
      parts.Add(Index_Value)
    Else
      ' Add parser for identifers here

    End If
    Dim si=i
    sr.Consume_Spaces(i)
    If (si-i) >0 Then parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.Spaces,sr,si,i+1))
    ' Alignment 
    If sr(i) = ","c Then
      parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.Comma,sr,i,i+1))
      i += 1
      If (i >= sr.Length) Then Return SpanKind.MakeFrom(StringFormat.Kinds.Err_EOT,sr , i - 1, i)
       Align_Value = Parse_Arg_Alignment(sr, i)
       parts.Add(Align_Value)
      'If Align_Value.HasError Then
      'Else
      '  Align_Value = res1
      'End If
    End If
    ' Formatting
    If sr(i) = ":"c Then
      parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.Colon,sr,i,i+1))
      i += 1
      Format_Value = Parse_Arg_Format(sr, i)
      parts.Add(Format_Value)
    ElseIf sr(i) = "}"c Then

    Else
      Return SpanKind.MakeFrom(StringFormat.Kinds.Err_UC,sr , i, i + 1)
    End If
    parts.Add(SpanKind.MakeFrom(StringFormat.Kinds.BR, sr, i, i + 1))
    Return SpanKind.MakeFrom(StringFormat.Kinds.ArgHole, sr, LeftEdgeOfHole, i + 1, parts)
  End Function

  <Runtime.CompilerServices.Extension>
  Public Function IsDigit(c As Char?) As Boolean
    Return c.HasValue AndAlso (("0"c <= c.Value) AndAlso (c.Value <= "9"c))
  End Function

  <Runtime.CompilerServices.Extension>
  Public Function Parse_Digits(sr As SourceText, ByRef i As Integer) As SpanKind
    Dim si = i
    While i < sr.Length AndAlso sr(i).IsDigit
      i += 1
    End While
    Return SpanKind.MakeFrom(If((i - si) = 0, StringFormat.Kinds.Empty, StringFormat.Kinds.Digits), sr,si, i )
  End Function

  <Runtime.CompilerServices.Extension>
  Function Consume_Spaces(sr As SourceText, ByRef i As Integer) As SpanKind
    Dim si = i
    While si < sr.Length AndAlso (sr(i) = " "c)
      i += 1
    End While
    Return SpanKind.MakeFrom(If((i - si) = 0, StringFormat.Kinds.Empty, StringFormat.Kinds.Spaces), sr, si, i)

  End Function

End Module

