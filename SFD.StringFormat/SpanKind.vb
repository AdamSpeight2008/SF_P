﻿Imports SFD.StringFormat. StringFormat
Imports SFD.StringFormat

Namespace Global.SFD.StringFormat

  Public Class SpanKind

    Public ReadOnly Property Span As Span
    Public ReadOnly Property Kind As Kinds
    Public ReadOnly Property MadeOf As New List(Of SpanKind)

    Private Sub New(kind As Kinds, span As Span,
                    Optional MadeOf As IEnumerable(Of SpanKind) = Nothing)
      _Span = span
      _Kind = kind
      If MadeOf IsNot Nothing Then _MadeOf.AddRange(MadeOf)
    End Sub

    Default Public ReadOnly Property MadeOfItem(ByVal Index As Integer) As SpanKind
      Get
        If Index < 0 Then Return Nothing
        If Index >= MadeOf.Count Then Return Nothing
        Return MadeOf(Index)
      End Get
    End Property


    Shared Operator IsTrue(s As SpanKind) As Boolean
      Return s.Kind <> Kinds.None
    End Operator

    Shared Operator IsFalse(s As SpanKind) As Boolean
      Return s.Kind = Kinds.None
    End Operator

    Public Function Offset(sp As Span, k As Kinds) As SpanKind 
      Dim s = Me.Start + sp.Start
      Dim f = Me.Start + sp.Finish 
      Return New SpanKind(k,New Span(Me.GetSourceText,s,f))
    End Function
    Public Function Offset(start As integer, finish As Integer, k As Kinds) As SpanKind
      Dim s = Me.Start + start
      Dim f = Me.Start + Finish
      Return New SpanKind(k, New Span(Me.GetSourceText, s, f))
    End Function
    Public Shared Function MakeFrom(kind As Kinds, Start As SpanKind, Finish As SpanKind,
                                    Optional MadeOf As IEnumerable(Of SpanKind)=Nothing) As SpanKind
      Dim ref = Start.Span.Source
      Dim span = New Span(ref, Start.Span.Start, Finish.Span.Finish)
      Return New SpanKind(kind, span, MadeOf)
    End Function

    Public Shared Function MakeFrom(kind As Kinds, source As SourceText, Start As Integer, Finish As Integer,
                                    Optional MadeOf As IEnumerable(Of SpanKind) = Nothing) As SpanKind
      Dim span = New Span(source, Start, Finish)
      Return New SpanKind(kind, span, MadeOf)
    End Function

    Public Shared Function MakeFrom(kind As Kinds, StartAndFinish As SpanKind) As SpanKind
      Dim ref = StartAndFinish.Span.Source
      Dim span = New Span(ref, StartAndFinish.Span.Start, StartAndFinish.Span.Finish)
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

    Public Shared Operator =(sk As SpanKind, k As Kinds) As Boolean
      If sk Is Nothing Then Return False
      Return sk.Kind = k
    End Operator
    Public Shared Operator <>(sk As SpanKind, k As Kinds) As Boolean
      If sk Is Nothing Then Return False
      Return sk.Kind <> k
    End Operator
  End Class
End Namespace
