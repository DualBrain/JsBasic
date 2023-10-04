Imports Irony.Parsing
Imports Irony.Interpreter

Module Program

  Sub Main(args As String())
    Console.WriteLine("Hello World!")
    Console.Clear()
    Dim grammar = New GWBasicGrammar
    Dim language = New LanguageData(grammar)

    'Dim runtime = New LanguageRuntime(language)
    'Dim commandLine = New CommandLine(runtime)
    'CommandLine.Run()

    Dim bas = <bas>
10 PRINT "Hello World!"
20 GOTO 10
              </bas>.Value

    Dim parser = New Parser(grammar)
    'TestHelper.CheckGrammarErrors(parser)
    Dim parseTree = parser.Parse(bas)
    'TestHelper.CheckParseErrors(parseTree)

    Dim root = parseTree.Root
    WriteNodes(root, 0)

    'Dim ast = parseTree.Root.AstNode

  End Sub

  Private Sub WriteNodes(root As ParseTreeNode, depth As Integer)
    For Each child In root.ChildNodes
      Console.WriteLine($"{Space(depth)}{child} `{child.Term}`")
      If child.ChildNodes.Any Then
        WriteNodes(child, depth + 2)
      End If
    Next
  End Sub

End Module
