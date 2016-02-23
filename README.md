#Programming F# Source Code#
This repro contains the source code for all things _Programming F#_, available from O'Reilly Media.

Forgive me. But as a beginner, I am unable to get 'breakText' unresoved - from FsLexYacc extention, which I have installed in my project.

Any comment would be much appreciated.

Here is the Packages.config:

<?xml version="1.0" encoding="utf-8"?>
<packages>
  <package id="FsLexYacc" version="6.1.0" targetFramework="net452" />
  <package id="FsLexYacc.Runtime" version="6.1.0" targetFramework="net452" />
</packages>

Here is the App.config - which I was told to manually add the lines to import FsLexYacc:

<?xml version="1.0" encoding="utf-8" ?>

<configuration>
  <Import Project="c:\FsLexYacc.6.0.1\bin\FsLexYacc.targets" />
  
  <FsYacc Include="..\LexAndYaccMiniProject\Parser.fsy">
  <OtherFlags>--module Parser</OtherFlags>
</FsYacc>
<FsLex Include="..\LexAndYaccMiniProject\Lexer.fsl">
  <OtherFlags>--unicode</OtherFlags>
</FsLex>
  
    <startup> 
        <supportedRuntime version="v4.0" sku=".NETFramework,Version=v4.5.2" />
    
    </startup>
</configuration>

Where have I gone wrong, please?
