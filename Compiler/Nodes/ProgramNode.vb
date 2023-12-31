﻿Namespace Global.JsBasic.Compiler.Nodes

  ''' <summary>
  ''' The root node of a BASIC program.
  ''' </summary>
  Friend Class ProgramNode
    Inherits GenericJsBasicNode
    Implements IJsBasicNode

    Public ReadOnly Property Lines As IEnumerable(Of LineNode)
      Get
        Return ChildNodes.Cast(Of LineNode)()
      End Get
    End Property

    Public Sub New(args As Irony.Compiler.AstNodeArgs)
      MyBase.New(args)
    End Sub

    Public Overrides Sub GenerateJavaScript(context As JsContext, textWriter1 As IO.TextWriter)
      textWriter1.WriteLine($"/* Generated by the JsBasic compiler at {Now:HH:mm:ss MMM dd yyyy} */")
      For Each line In Lines
        line.GenerateJavaScript(context, textWriter1)
      Next
    End Sub

  End Class

End Namespace