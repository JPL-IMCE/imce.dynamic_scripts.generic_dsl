/**
 * License Terms
 *
 * Copyright (c) 2014, California
 * Institute of Technology ("Caltech").  U.S. Government sponsorship
 * acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 *
 *  *   Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *  *   Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the
 *      distribution.
 *
 *  *   Neither the name of Caltech nor its operating division, the Jet
 *      Propulsion Laboratory, nor the names of its contributors may be
 *      used to endorse or promote products derived from this software
 *      without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.dynamicScripts

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
object DynamicScriptsTypes {

  val TAB_INDENT = "    "

  object ScopeKind extends Enumeration {
    type ScopeKind = Value
    val STEREOTYPE, METACLASS, CLASSIFIER = Value
  }
  import ScopeKind._

  object BinaryDerivationRefresh extends Enumeration {
    type BinaryDerivationRefresh = Value
    val EAGER_COMPUTATION_AS_NEEDED, DELAYED_COMPUTATION_UNTIL_INVOKED = Value
  }
  import BinaryDerivationRefresh._

  case class QName( qname: String ) {
    def prettyPrint(): String = s"'${qname}'"
  }
  case class JName( jname: String )
  case class SName( sname: String )
  case class HName( hname: String )
  case class FName( path: String )

  sealed abstract class BundleContext {
    def prettyPrint(indentation: String): String
    def sortKey(): String
  }
  
  case class ProjectContext(project: JName, dependencies: Seq[JName]) extends BundleContext {
    def prettyPrint(indentation: String): String = {
      val tab0 = indentation
      val tab1 = tab0 + TAB_INDENT
      val deps =
        if ( dependencies.isEmpty )
          ""
        else
          ( for ( d <- dependencies ) yield d.jname ) mkString (
            s"\n${tab0}dependencies: {\n${tab1}", s"\n${tab1}", s"\n${tab0}}" )
    
      s"${indentation}project: ${project.jname}${deps}"
    }
    def sortKey(): String = s"project:${project.jname}"
  }
  
  case class PluginContext(pluginID: HName) extends BundleContext {
    def prettyPrint(indentation: String): String = s"${indentation}plugin.id: '${pluginID.hname}'"
    def sortKey(): String = s"plugin:${pluginID.hname}"
  }
  
  sealed abstract class DynamicScriptInfo {
    val name: HName
    val icon: Option[FName]
    val context: BundleContext
    val className: JName
    val methodName: SName
    def prettyPrintInfo( indentation: String ): String = 
      s"""|
          |${indentation}name: '${name.hname}'${if ( icon.isDefined ) s"\n${indentation}icon: '${icon.get.path}'" else ""}
          |${context.prettyPrint(indentation)}
          |${indentation}class: ${className.jname}
          |${indentation}method: ${methodName.sname}""".stripMargin

    def prettyPrint( indentation: String ): String
  }

  sealed abstract class ComputedDerivedFeature extends DynamicScriptInfo {
    val refresh: BinaryDerivationRefresh

    def prettyPrintRefresh(): String =
      refresh match {
        case EAGER_COMPUTATION_AS_NEEDED       => "Early"
        case DELAYED_COMPUTATION_UNTIL_INVOKED => "Delayed"
      }

    def prettyPrintComputed( indentation: String, kind: String ) =
      s"""|${indentation}${prettyPrintRefresh()}${kind}(${prettyPrintInfo( indentation + TAB_INDENT )}
          |\n${indentation})""".stripMargin
  }

  case class ComputedDerivedProperty(
    name: HName,
    icon: Option[FName],
    context: BundleContext,
    className: JName,
    methodName: SName,
    refresh: BinaryDerivationRefresh ) extends ComputedDerivedFeature {

    override def prettyPrint( indentation: String ): String = prettyPrintComputed( indentation, "DerivedProperty" )
  }

  case class ComputedDerivedTable(
    name: HName,
    icon: Option[FName],
    context: BundleContext,
    className: JName,
    methodName: SName,
    refresh: BinaryDerivationRefresh ) extends ComputedDerivedFeature {

    override def prettyPrint( indentation: String ): String = prettyPrintComputed( indentation, "DerivedTable" )
  }

  sealed abstract class DynamicActionScript extends DynamicScriptInfo {
    def sortKey(): String = s"${name.hname}|${context.sortKey()}|${className.jname}|${methodName.sname}"
  }

  val DynamicActionScriptOrdering = new Ordering[DynamicActionScript] {
    override def compare( a1: DynamicActionScript, a2: DynamicActionScript ): Int = a1.sortKey().compareTo( a2.sortKey() )
  }

  sealed trait DynamicMenuActionScript {}

  case class BrowserContextMenuAction(
    name: HName,
    icon: Option[FName],
    context: BundleContext,
    className: JName,
    methodName: SName ) extends DynamicActionScript with DynamicMenuActionScript {

    override def prettyPrint( indentation: String ): String =
      indentation + s"BrowserContextMenuAction(${prettyPrintInfo( indentation + TAB_INDENT )}\n${indentation})"
  }

  sealed abstract class DynamicContextDiagramActionScript extends DynamicActionScript {
    val diagramTypes: Seq[SName]
    val diagramStereotypes: Seq[QName]

    override def prettyPrintInfo( indentation: String ): String = {
      val tab0 = indentation
      val tab1 = tab0 + TAB_INDENT
      val dTypes =
        if ( diagramTypes.isEmpty )
          ""
        else
          ( for ( dt <- diagramTypes ) yield dt.sname ) mkString ( s"\n${tab0}diagramTypes: {", ", ", "}" )
      val dSTypes =
        if ( diagramStereotypes.isEmpty )
          ""
        else
          ( for ( ds <- diagramStereotypes ) yield s"'${ds.qname}'" ) mkString (
            s"\n${tab0}diagramStereotypes: {\n${tab1}", s",\n${tab1}", s"\n${tab0}}" )
      s"""|
          |${tab0}name: '${name.hname}'${if ( icon.isDefined ) s"\n${tab0}icon: '${icon.get.path}'" else ""}
          |${context.prettyPrint(tab0)}
          |${tab0}class: ${className.jname}
          |${tab0}method: ${methodName.sname}""".stripMargin
    }

    def prettyPrintKind( indentation: String, kind: String, aux: => String ): String =
      indentation + kind + s"(${prettyPrintInfo( indentation + TAB_INDENT )}${aux}\n${indentation})"
  }

  case class DynamicContextDiagramActionScriptInfo(
    name: HName,
    icon: Option[FName],
    diagramTypes: Seq[SName],
    diagramStereotypes: Seq[QName],
    context: BundleContext,
    className: JName,
    methodName: SName ) extends DynamicContextDiagramActionScript {
    override def prettyPrint( indentation: String ): String = ???
  }

  sealed abstract class DynamicContextMenuActionScript extends DynamicContextDiagramActionScript with DynamicMenuActionScript {}

  case class DiagramContextMenuAction(
    info: DynamicContextDiagramActionScriptInfo ) extends DynamicContextMenuActionScript {

    val name = info.name
    val icon = info.icon
    val context = info.context
    val className = info.className
    val methodName = info.methodName
    val diagramTypes = info.diagramTypes
    val diagramStereotypes = info.diagramStereotypes

    override def prettyPrint( indentation: String ): String =
      info.prettyPrintKind( indentation, "DiagramContextMenuAction", "" )
  }
  
  sealed abstract class ElementKindDesignation {
    def prettyPrint( indentation: String ): String
  }

  case class MetaclassDesignation( metaclass: SName ) extends ElementKindDesignation {
    override def prettyPrint( indentation: String ): String = s"${indentation}[ m: ${metaclass.sname} ]"
  }

  case class StereotypedMetaclassDesignation( metaclass: SName, profile: QName, stereotype: QName ) extends ElementKindDesignation {
    override def prettyPrint( indentation: String ): String = s"${indentation}[ m: ${metaclass.sname}, p: ${profile.prettyPrint()}, s: ${stereotype.prettyPrint()}]"
  }

  case class ClassifiedInstanceDesignation( metaclass: SName, classifier: QName ) extends ElementKindDesignation {
    override def prettyPrint( indentation: String ): String = s"${indentation}[ m: ${metaclass.sname}, c: ${classifier.prettyPrint()}]"
  }

  case class StereotypedClassifiedInstanceDesignation( metaclass: SName, classifier: QName, profile: QName, stereotype: QName ) extends ElementKindDesignation {
    override def prettyPrint( indentation: String ): String = s"${indentation}[ m: ${metaclass.sname}, c: ${classifier.prettyPrint()}, p: ${profile.prettyPrint()}, s: ${stereotype.prettyPrint()}]"
  }
  
  sealed abstract class DynamicContextShapeCreationActionScript extends DynamicContextDiagramActionScript {}

  case class ToplevelShapeInstanceCreator(
    info: DynamicContextDiagramActionScriptInfo,
    elementShape: ElementKindDesignation ) extends DynamicContextShapeCreationActionScript {

    val name = info.name
    val icon = info.icon
    val context = info.context
    val className = info.className
    val methodName = info.methodName
    val diagramTypes = info.diagramTypes
    val diagramStereotypes = info.diagramStereotypes

    override def prettyPrint( indentation: String ): String =
      info.prettyPrintKind( indentation, "ToplevelElementShape", 
          elementShape.prettyPrint(s"\n${indentation}elementShape: ") )
  }

  sealed abstract class DynamicContextPathCreationActionScript extends DynamicContextDiagramActionScript {}

  case class ToplevelPathInstanceCreator(
    info: DynamicContextDiagramActionScriptInfo,
    elementPath: ElementKindDesignation,
    pathFrom: ElementKindDesignation,
    pathTo: ElementKindDesignation ) extends DynamicContextPathCreationActionScript {

    val name = info.name
    val icon = info.icon
    val context = info.context
    val className = info.className
    val methodName = info.methodName
    val diagramTypes = info.diagramTypes
    val diagramStereotypes = info.diagramStereotypes

    override def prettyPrint( indentation: String ): String =
      info.prettyPrintKind( indentation, "ToplevelRelationPath", 
          elementPath.prettyPrint(s"\n${indentation}elementPath: ") +
          pathFrom.prettyPrint(s"\n${indentation}pathFrom: ") +
          pathTo.prettyPrint(s"\n${indentation}pathTo: "))
  }

  sealed abstract class DynamicScript {
    val name: HName
    def prettyPrint( indentation: String ): String
  }

  case class DynamicScriptOrdering[T <: DynamicScript]() extends Ordering[T] {
    def compare( c1: T, c2: T ): Int = c1.name.hname.compareTo( c2.name.hname )
  }

  case class ComputedCharacterization(
    name: HName,
    characterizesInstancesOf: ElementKindDesignation,
    computedDerivedFeatures: Seq[ComputedDerivedFeature] ) extends DynamicScript {

    override def prettyPrint( indentation: String ): String = {
      val indent1 = indentation + TAB_INDENT
      val indent2 = indent1 + TAB_INDENT
      s"${indentation}characterization(\n${indent1}name='${name.hname}'\n${characterizesInstancesOf.prettyPrint(indent1)}\n${indent1}features {\n${( for ( df <- computedDerivedFeatures ) yield df.prettyPrint( indent2 ) ) mkString ( "\n" )}\n${indent1}})"
    }

  }

  case class DynamicScriptsForInstancesOfKind(
    name: HName,
    applicableTo: ElementKindDesignation,
    scripts: Seq[DynamicActionScript] ) extends DynamicScript {

    override def prettyPrint( indentation: String ): String = {
      val indent1 = indentation + TAB_INDENT
      val indent2 = indent1 + TAB_INDENT
      s"${indentation}dynamicScripts(\n${indent1}name='${name.hname}'\n${applicableTo.prettyPrint(indent1)}\n${indent1}scripts {\n${( for ( s <- scripts ) yield s.prettyPrint( indent2 ) ) mkString ( "\n" )}\n${indent1}})"
    }
  }

}