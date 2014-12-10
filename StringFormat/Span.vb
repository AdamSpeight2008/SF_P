﻿Namespace Global.StringFormat
  Public Structure Span
    Public ReadOnly Property Start As Integer
    Public ReadOnly Property Finish As Integer

    Public Sub New(Start As Integer, Finish As Integer)
      _Start = Start
      _Finish = Finish
    End Sub
    Public ReadOnly Property Size() As Integer
      Get
        Return (Finish - Start)
      End Get
    End Property

    Public Overrides Function ToString() As String
      Return String.Format("({0,6} , {1,-6})", Start, Finish)

    End Function

  End Structure

End Namespace