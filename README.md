# JPL Dynamic Scripts Generic DSL
 
[![Build Status](https://travis-ci.org/JPL-IMCE/imce.dynamic_scripts.generic_dsl.svg?branch=IMCEI-283)](https://travis-ci.org/JPL-IMCE/imce.dynamic_scripts.generic_dsl)
[ ![Download](https://api.bintray.com/packages/jpl-imce/gov.nasa.jpl.imce/imce.dynamic_scripts.generic_dsl/images/download.svg) ](https://bintray.com/jpl-imce/gov.nasa.jpl.imce/imce.dynamic_scripts.generic_dsl/_latestVersion)

A textual DSL for specifying browser, diagram & contextualized actions in a modeling tool.

This project defines a tool-neutral, generic DSL for specifying "dynamic scripts".
A dynamic script specifies the location of a modeling function compiled for the JVM
that can be invoked for modeling elements matching the applicability criteria specified in the script.

The genericity of this DSL is that the applicability criteria can be specified in terms of
references to tool-neutral identifiers of metaclasses, stereotypes and library types and
the context where scripts are to be made available to users is specified in terms of generic
notions of model browser and diagrams; user-interface notions that are widely common to graphical modeling tools.
