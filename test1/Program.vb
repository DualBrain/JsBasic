Module Program

  Sub Main(args As String())

    ' variables can now optionally contain a postfix.
    ' added & and # as a valid postfix
    ' modified "s_" + name to be "str_" + name
    ' modified next so that variable is optional
    ' line numbers are now optional

    Dim bas = <bas>
10 for x = 1 to 5
20  print x
30 next
              </bas>.Value?.Trim

    Dim result = JsBasic.Compiler.Generate(bas)
    If result?.IsSuccessful Then
      Console.WriteLine($"function runProgram() {{
  run('console', 22, 40, {result.StartFunction});
}}
{result.JavaScript?.Trim}")
    Else
      Console.WriteLine($"COMPILE ERROR{Environment.NewLine}{Environment.NewLine}{result.ResultMessage}")
    End If

  End Sub

End Module