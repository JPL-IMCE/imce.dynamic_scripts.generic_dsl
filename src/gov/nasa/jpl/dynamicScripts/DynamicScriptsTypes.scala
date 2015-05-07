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

  object ScopeAccess extends Enumeration {
    type ScopeAccess = Value
    val READ_ONLY, READ_WRITE = Value

    def prettyPrint( access: ScopeAccess ): String = access match {
      case READ_ONLY  => "r/o"
      case READ_WRITE => "r/w"
    }
  }
  import ScopeAccess._

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
  case class HName( hname: String ) {
    def prettyPrint(): String = s"'${hname}'"
  }
  case class FName( path: String )

  sealed abstract class BundleContext {
    def prettyPrint( indentation: String ): String
    def sortKey(): String
  }

  case class ProjectContext( project: JName, dependencies: Seq[JName], requiresPlugin: Option[HName] ) extends BundleContext {
    def prettyPrint( indentation: String ): String = {
      val tab0 = indentation
      val tab1 = tab0 + TAB_INDENT
      val deps =
        if ( dependencies.isEmpty )
          ""
        else
          ( for ( d <- dependencies ) yield d.jname ) mkString (
            s"\n${tab0}dependencies: {\n${tab1}", s"\n${tab1}", s"\n${tab0}}" )

      val rp = requiresPlugin match {
        case None       => ""
        case Some( hn ) => s"\n${tab0}requires.plugin.id: '${hn.hname}'"
      }
      s"${indentation}project: ${project.jname}${deps}${rp}"
    }
    def sortKey(): String = s"project:${project.jname}"
  }

  case class PluginContext( pluginID: HName ) extends BundleContext {
    def prettyPrint( indentation: String ): String = s"${indentation}plugin.id: ${pluginID.prettyPrint()}"
    def sortKey(): String = s"plugin:${pluginID.hname}"
  }

  sealed abstract class DynamicScriptInfo {
    val name: HName
    val icon: Option[FName]
    val context: BundleContext
    val access: ScopeAccess
    val className: JName
    val methodName: SName
    def prettyPrintInfo( indentation: String ): String =
      s"""|
          |${indentation}name: ${name.prettyPrint()}${if ( icon.isDefined ) s"\n${indentation}icon: '${icon.get.path}'" else ""}
          |${context.prettyPrint( indentation )}
          |${indentation}access: ${ScopeAccess.prettyPrint( access )}
          |${indentation}class: ${className.jname}
          |${indentation}method: ${methodName.sname}""".stripMargin

    def prettyPrint( indentation: String ): String

    def sortKey(): String = s"${name.hname}|${context.sortKey()}|${className.jname}|${methodName.sname}"
  }

  val DynamicScriptOrdering = new Ordering[DynamicScriptInfo] {

    override def compare( s1: DynamicScriptInfo, s2: DynamicScriptInfo ): Int =
      s1.sortKey().compareTo( s2.sortKey() )

  }

  sealed abstract class ValueTypeDesignation

  sealed abstract class PrimitiveTypeDesignation extends ValueTypeDesignation

  case class IntegerTypeDesignation() extends PrimitiveTypeDesignation
  case class RationalTypeDesignation() extends PrimitiveTypeDesignation
  case class RealTypeDesignation() extends PrimitiveTypeDesignation
  case class StringTypeDesignation() extends PrimitiveTypeDesignation

  case class CustomTypeDesignation( typeDescriptor: HName ) extends ValueTypeDesignation

  case class DerivedFeatureValueType(
    key: SName,
    typeName: HName,
    typeInfo: ValueTypeDesignation )

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

  case class ComputedDerivedWidget(
    name: HName,
    icon: Option[FName],
    context: BundleContext,
    access: ScopeAccess,
    className: JName,
    methodName: SName,
    refresh: BinaryDerivationRefresh ) extends ComputedDerivedFeature {

    override def prettyPrint( indentation: String ): String = prettyPrintComputed( indentation, "DerivedProperty" )
  }

  case class ComputedDerivedProperty(
    name: HName,
    icon: Option[FName],
    context: BundleContext,
    access: ScopeAccess,
    className: JName,
    methodName: SName,
    refresh: BinaryDerivationRefresh,
    valueType: Option[DerivedFeatureValueType] ) extends ComputedDerivedFeature {

    override def prettyPrint( indentation: String ): String = prettyPrintComputed( indentation, "DerivedProperty" )
  }

  case class ComputedDerivedTable(
    name: HName,
    icon: Option[FName],
    context: BundleContext,
    access: ScopeAccess,
    className: JName,
    methodName: SName,
    refresh: BinaryDerivationRefresh,
    columnValueTypes: Option[Seq[DerivedFeatureValueType]] ) extends ComputedDerivedFeature {

    override def prettyPrint( indentation: String ): String = prettyPrintComputed( indentation, "DerivedTable" )
  }

  case class ComputedDerivedTree(
    name: HName,
    icon: Option[FName],
    context: BundleContext,
    access: ScopeAccess,
    className: JName,
    methodName: SName,
    refresh: BinaryDerivationRefresh,
    columnValueTypes: Option[Seq[DerivedFeatureValueType]] ) extends ComputedDerivedFeature {

    override def prettyPrint( indentation: String ): String = prettyPrintComputed( indentation, "DerivedTree" )
  }

  sealed abstract class DynamicActionScript extends DynamicScriptInfo

  sealed trait DynamicMenuActionScript {}

  case class MainToolbarMenuAction(
    toolbarMenuPath: Seq[HName],
    name: HName,
    icon: Option[FName],
    context: BundleContext,
    access: ScopeAccess,
    className: JName,
    methodName: SName ) extends DynamicActionScript with DynamicMenuActionScript {

    override def prettyPrint( indentation: String ): String = {
      val tpath = ( for ( p <- toolbarMenuPath ) yield p.prettyPrint() ) mkString (
        s"\n${indentation}toolbarMenuPath: { ", " > ", " }" )
      indentation + s"MainToolbarMenuAction(${tpath}${prettyPrintInfo( indentation + TAB_INDENT )}\n${indentation})"
    }
  }

  case class BrowserContextMenuAction(
    name: HName,
    icon: Option[FName],
    context: BundleContext,
    access: ScopeAccess,
    className: JName,
    methodName: SName ) extends DynamicActionScript with DynamicMenuActionScript {

    override def prettyPrint( indentation: String ): String =
      indentation + s"BrowserContextMenuAction(${prettyPrintInfo( indentation + TAB_INDENT )}\n${indentation})"
  }

  sealed abstract class DynamicContextDiagramActionScript extends DynamicActionScript {
    val diagramTypes: Seq[HName]
    val diagramStereotypes: Seq[QName]

    override def prettyPrintInfo( indentation: String ): String = {
      val tab0 = indentation
      val tab1 = tab0 + TAB_INDENT
      val dTypes =
        if ( diagramTypes.isEmpty )
          ""
        else
          ( for ( dt <- diagramTypes ) yield s"'${dt.hname}'" ) mkString ( s"\n${tab0}diagramTypes: {", ", ", "}" )
      val dSTypes =
        if ( diagramStereotypes.isEmpty )
          ""
        else
          ( for ( ds <- diagramStereotypes ) yield s"'${ds.qname}'" ) mkString (
            s"\n${tab0}diagramStereotypes: {\n${tab1}", s",\n${tab1}", s"\n${tab0}}" )
      s"""|
          |${tab0}name: ${name.prettyPrint()}${if ( icon.isDefined ) s"\n${tab0}icon: '${icon.get.path}'" else ""}${dTypes}${dSTypes}
          |${context.prettyPrint( tab0 )}
          |${tab0}class: ${className.jname}
          |${tab0}method: ${methodName.sname}""".stripMargin
    }

    def prettyPrintKind( indentation: String, kind: String, aux: => String ): String =
      indentation + kind + s"(${prettyPrintInfo( indentation + TAB_INDENT )}${aux}\n${indentation})"
  }

  case class DynamicContextDiagramActionScriptInfo(
    name: HName,
    icon: Option[FName],
    diagramTypes: Seq[HName],
    diagramStereotypes: Seq[QName],
    context: BundleContext,
    access: ScopeAccess,
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
    val access = info.access
    val className = info.className
    val methodName = info.methodName
    val diagramTypes = info.diagramTypes
    val diagramStereotypes = info.diagramStereotypes

    override def prettyPrint( indentation: String ): String =
      info.prettyPrintKind( indentation, "DiagramContextMenuAction", "" )
  }

  sealed abstract class ElementKindDesignation extends ValueTypeDesignation {
    val metaclass: SName
    def prettyPrint( indentation: String ): String
    def compareTo( other: ElementKindDesignation ): Int
  }

  case class MetaclassDesignation( metaclass: SName ) extends ElementKindDesignation {
    override def prettyPrint( indentation: String ): String = s"${indentation}[ m: ${metaclass.sname} ]"
    def compareTo( other: ElementKindDesignation ): Int = other match {
      case md: MetaclassDesignation => metaclass.sname.compareTo( md.metaclass.sname )
      case _                        => -1
    }
  }

  case class StereotypedMetaclassDesignation( metaclass: SName, profile: QName, stereotype: QName ) extends ElementKindDesignation {
    override def prettyPrint( indentation: String ): String = s"${indentation}[ m: ${metaclass.sname}, p: ${profile.prettyPrint()}, s: ${stereotype.prettyPrint()}]"
    def compareTo( other: ElementKindDesignation ): Int = other match {
      case _: MetaclassDesignation => 1
      case smd: StereotypedMetaclassDesignation =>
        metaclass.sname.compareTo( smd.metaclass.sname ) match {
          case 0 => profile.qname.compareTo( smd.profile.qname ) match {
            case 0 => stereotype.qname.compareTo( smd.stereotype.qname )
            case c => c
          }
          case c => c
        }
      case _ => -1
    }
  }

  case class ClassifiedInstanceDesignation( metaclass: SName, classifier: QName ) extends ElementKindDesignation {
    override def prettyPrint( indentation: String ): String = s"${indentation}[ m: ${metaclass.sname}, c: ${classifier.prettyPrint()}]"
    def compareTo( other: ElementKindDesignation ): Int = other match {
      case _@ ( _: MetaclassDesignation | _: StereotypedMetaclassDesignation ) => 1
      case cdi: ClassifiedInstanceDesignation => metaclass.sname.compareTo( cdi.metaclass.sname ) match {
        case 0 => classifier.qname.compareTo( cdi.classifier.qname )
        case c => c
      }
      case _ => -1
    }
  }

  case class StereotypedClassifiedInstanceDesignation( metaclass: SName, classifier: QName, profile: QName, stereotype: QName ) extends ElementKindDesignation {
    override def prettyPrint( indentation: String ): String = s"${indentation}[ m: ${metaclass.sname}, c: ${classifier.prettyPrint()}, p: ${profile.prettyPrint()}, s: ${stereotype.prettyPrint()}]"
    def compareTo( other: ElementKindDesignation ): Int = other match {
      case _@ ( _: MetaclassDesignation | _: StereotypedMetaclassDesignation | _: ClassifiedInstanceDesignation ) => 1
      case scid: StereotypedClassifiedInstanceDesignation => metaclass.sname.compareTo( scid.metaclass.sname ) match {
        case 0 => classifier.qname.compareTo( scid.classifier.qname ) match {
          case 0 => profile.qname.compareTo( scid.profile.qname ) match {
            case 0 => stereotype.qname.compareTo( scid.stereotype.qname )
            case c => c
          }
          case c => c
        }
        case c => c
      }
    }
  }

  sealed abstract class DynamicContextShapeCreationActionScript extends DynamicContextDiagramActionScript {}

  case class ToplevelShapeInstanceCreator(
    info: DynamicContextDiagramActionScriptInfo,
    elementShape: ElementKindDesignation ) extends DynamicContextShapeCreationActionScript {

    val name = info.name
    val icon = info.icon
    val context = info.context
    val access = info.access
    val className = info.className
    val methodName = info.methodName
    val diagramTypes = info.diagramTypes
    val diagramStereotypes = info.diagramStereotypes

    override def prettyPrint( indentation: String ): String =
      info.prettyPrintKind( indentation, "ToplevelElementShape",
        elementShape.prettyPrint( s"\n${indentation}elementShape: " ) )
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
    val access = info.access
    val className = info.className
    val methodName = info.methodName
    val diagramTypes = info.diagramTypes
    val diagramStereotypes = info.diagramStereotypes

    override def prettyPrint( indentation: String ): String =
      info.prettyPrintKind( indentation, "ToplevelRelationPath",
        elementPath.prettyPrint( s"\n${indentation}elementPath: " ) +
          pathFrom.prettyPrint( s"\n${indentation}pathFrom: " ) +
          pathTo.prettyPrint( s"\n${indentation}pathTo: " ) )
  }

  sealed abstract class DynamicScript {
    val name: HName
    def prettyPrint( indentation: String ): String
  }

  case class ComputedCharacterization(
    name: HName,
    characterizesInstancesOf: ElementKindDesignation,
    computedDerivedFeatures: Seq[ComputedDerivedFeature] ) extends DynamicScript {

    override def prettyPrint( indentation: String ): String = {
      val indent1 = indentation + TAB_INDENT
      val indent2 = indent1 + TAB_INDENT
      val prettyFeatures = s"${indent1}features {\n${( for ( df <- computedDerivedFeatures ) yield df.prettyPrint( indent2 ) ) mkString ( "\n" )}\n${indent1}})"
      s"${indentation}characterization(\n${indent1}name=${name.prettyPrint()}\n${characterizesInstancesOf.prettyPrint( indent1 )}\n${prettyFeatures}"
    }

  }

  case class ComputedCharacterizationOrdering() extends Ordering[ComputedCharacterization] {
    def compare( c1: ComputedCharacterization, c2: ComputedCharacterization ): Int =
      c1.name.hname.compareTo( c2.name.hname ) match {
        case 0 => c1.characterizesInstancesOf.compareTo( c2.characterizesInstancesOf )
        case c => c
      }
  }

  def merge( c1: ComputedCharacterization, c2: ComputedCharacterization ): ComputedCharacterization =
    if ( ComputedCharacterizationOrdering().compare( c1, c2 ) != 0 ) c1
    else c1.copy( computedDerivedFeatures = c1.computedDerivedFeatures ++ c2.computedDerivedFeatures )

  case class DynamicScriptsForInstancesOfKind(
    name: HName,
    applicableTo: ElementKindDesignation,
    scripts: Seq[DynamicActionScript] ) extends DynamicScript {

    override def prettyPrint( indentation: String ): String = {
      val indent1 = indentation + TAB_INDENT
      val indent2 = indent1 + TAB_INDENT
      val prettyScripts = s"${indent1}scripts {\n${( for ( s <- scripts ) yield s.prettyPrint( indent2 ) ) mkString ( "\n" )}\n${indent1}})"
      s"${indentation}dynamicScripts(\n${indent1}name=${name.prettyPrint()}\n${applicableTo.prettyPrint( indent1 )}\n${prettyScripts}"
    }
  }

  case class DynamicScriptsForInstancesOfKindOrdering() extends Ordering[DynamicScriptsForInstancesOfKind] {
    def compare( c1: DynamicScriptsForInstancesOfKind, c2: DynamicScriptsForInstancesOfKind ): Int =
      c1.name.hname.compareTo( c2.name.hname ) match {
        case 0 => c1.applicableTo.compareTo( c2.applicableTo )
        case c => c
      }
  }

  def merge( c1: DynamicScriptsForInstancesOfKind, c2: DynamicScriptsForInstancesOfKind ): DynamicScriptsForInstancesOfKind =
    if ( DynamicScriptsForInstancesOfKindOrdering().compare( c1, c2 ) != 0 ) c1
    else c1.copy( scripts = c1.scripts ++ c2.scripts )

  case class DynamicScriptsForMainToolbarMenus(
    name: HName,
    scripts: Seq[MainToolbarMenuAction] ) extends DynamicScript {

    override def prettyPrint( indentation: String ): String = {
      val indent1 = indentation + TAB_INDENT
      val indent2 = indent1 + TAB_INDENT
      val prettyScripts = s"${indent1}scripts {\n${( for ( s <- scripts ) yield s.prettyPrint( indent2 ) ) mkString ( "\n" )}\n${indent1}})"
      s"${indentation}toolbarMenuScripts(\n${indent1}name=${name.prettyPrint()}\n${prettyScripts}"
    }
  }

  case class DynamicScriptsForMainToolbarMenusOrdering() extends Ordering[DynamicScriptsForMainToolbarMenus] {
    def compare( c1: DynamicScriptsForMainToolbarMenus, c2: DynamicScriptsForMainToolbarMenus ): Int =
      c1.name.hname.compareTo( c2.name.hname )
  }

  def merge( c1: DynamicScriptsForMainToolbarMenus, c2: DynamicScriptsForMainToolbarMenus ): DynamicScriptsForMainToolbarMenus =
    if ( DynamicScriptsForMainToolbarMenusOrdering().compare( c1, c2 ) != 0 ) c1
    else c1.copy( scripts = c1.scripts ++ c2.scripts )

}