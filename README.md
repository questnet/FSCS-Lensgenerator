# FSCS-LensGenerator #

Generate [Lenses][steckermeier2015lenses] for an FSharp project, currently only [Aether][AetherGithub] is supported.

| branch | status |
| -------- | ------ |
| master | [![Build status](https://ci.appveyor.com/api/projects/status/qe15qgdby7ctp6mi/branch/master?svg=true)](https://ci.appveyor.com/project/embix/fscs-lensgenerator/branch/master) |
| [Samples][samplerepo]: master | [![Build status](https://ci.appveyor.com/api/projects/status/woaqf96rgy152co9/branch/master?svg=true)](https://ci.appveyor.com/project/embix/fscs-lensgeneratorsample/branch/master) |
| [Samples][samplerepo]: [issue9_workaround](https://github.com/questnet/FSCS-Lensgenerator/issues/9)  | [![Build status](https://ci.appveyor.com/api/projects/status/woaqf96rgy152co9/branch/issue9_workaround?svg=true)](https://ci.appveyor.com/project/embix/fscs-lensgeneratorsample/branch/issue9_workaround) |

# Introduction

TODO: Give a short introduction of your project. Let this section explain the objectives or the motivation behind this project. 

# Getting Started
TODO: Guide users through getting your code up and running on their own system. In this section you can talk about:
1.	Installation process
2.	Software dependencies
3.	Latest releases
4.	API references

Sample Usage:

    FSCSLensGenerator.exe
    --projectname SampleProject
    --projectdir C:\git\SampleSolution\SampleProject
    --projectfilename SampleProject.fsproj
    --outfilename C:\temp\SampleProject.Lenses\lenses.fs
    --outputnamespace SampleProject.Test.Lenses
    --syslib System.Numerics
    --projlib ..\packages\FParsec\lib\net40-client\FParsec.dll
    --projlib ..\packages\FParsec\lib\net40-client\FParsecCS.dll
    --libdir C:\git\SampleSolution\bin
    --binlib FunToolbox.dll

#Build and Test
TODO: Describe and show how to build your code and run the tests. 

#Contribute
TODO: Explain how other users and developers can contribute to make your code better. 

If you want to learn more about creating good readme files then refer the following [guidelines](https://www.visualstudio.com/en-us/docs/git/create-a-readme). You can also seek inspiration from the below readme files:
- [ASP.NET Core](https://github.com/aspnet/Home)
- [Visual Studio Code](https://github.com/Microsoft/vscode)
- [Chakra Core](https://github.com/Microsoft/ChakraCore)

 [AetherGithub]: https://github.com/xyncro/aether
 [steckermeier2015lenses]: http://www21.in.tum.de/teaching/fp/SS15/papers/17.pdf
 [samplerepo]: https://github.com/questnet/FSCS-LensgeneratorSample
