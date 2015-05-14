# JPL Dynamic Scripts Generic DSL

This project defines a tool-neutral, generic DSL for specifying "dynamic scripts".
A dynamic script specifies the location of a modeling function compiled for the JVM
that can be invoked for modeling elements matching the applicability criteria specified in the script.

The genericity of this DSL is that the applicability criteria can be specified in terms of
references to tool-neutral identifiers of metaclasses, stereotypes and library types and
the context where scripts are to be made available to users is specified in terms of generic
notions of model browser and diagrams; user-interface notions that are widely common to graphical modeling tools.

## Build procedure (local)

```
% sbt -DJPL_MBEE_LOCAL_REPOSITORY=<directory path for a local Ivy2 repository (will be created if necessary)> publish
```

## Build procedure (Jenkins CI)

The Jenkins job must set the environment variable JPL_MBEE_LOCAL_REPOSITORY
			
See ```build.xml```