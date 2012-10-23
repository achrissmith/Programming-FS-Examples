Please download the F# PowerPack first, located at:
http://fsharppowerpack.codeplex.com/

The following snippet has been added to the "Query" project to run FSLex and FSYacc
along with the build.

<!-- These are order dependent. Include Microsoft.FSharp.Targets BEFORE FSharp.PowerPack.Targets. -->
  <Import Project="$(MSBuildExtensionsPath32)\..\FSharpPowerPack-2.0.0.0\bin\FSharp.PowerPack.targets" />
  <ItemGroup>
    <FsLex Include="Lexer.fsl">
      <Link>QueryLexer.fsl</Link>
    </FsLex>
    <FsYacc Include="parser.fsy">
      <Module>Query.Core.Parser</Module>
      <Link>QueryParser.fsy</Link>
    </FsYacc>
  </ItemGroup>