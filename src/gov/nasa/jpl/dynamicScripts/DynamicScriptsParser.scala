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

import scala.language.implicitConversions
import scala.util.Try

import org.parboiled2._

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
class DynamicScriptsParser( val input: ParserInput ) extends Parser with StringBuilding {

  import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes._
  import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes.BinaryDerivationRefresh._
  import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes.ScopeKind._

  val WhiteSpaceChar = CharPredicate( " \n\r\t\f" )
  val CommentChar = CharPredicate.Visible ++ ' ' ++ '\t' -- '\n' -- '\r'
  val CommentEnd = CharPredicate( "\n\r" )
  val AnyChar = CharPredicate.Visible -- '\'' -- WhiteSpaceChar
  val FileChar = CharPredicate.Alpha ++ '/' ++ '.' ++ '-' ++ '_'

  def CommentString: Rule0 = rule { ch('#') ~ zeroOrMore( CommentChar ) ~ CommentEnd }
  implicit def VS( c: Char ): Rule0 = rule { ch( c ) ~ zeroOrMore( WhiteSpaceChar | CommentString ) }
  implicit def VS( s: String ): Rule0 = rule { str( s ) ~ zeroOrMore( WhiteSpaceChar | CommentString ) }

  def VS = rule { zeroOrMore( WhiteSpaceChar | CommentString ) }
  def WS = rule { oneOrMore( WhiteSpaceChar | CommentString ) }

  def InputLine = rule { VS ~ oneOrMore( Expression ~ VS ) ~ EOI }

  /**
   * Terminals & Handling Whitespace
   * 
   * Following the parboiled2 recommendation, the rules for constructing terminals handle, as applicable:
   * - non-whitespace keyword prefix
   * - whitespace suffix
   * 
   * What constitutes a whitespace suffix includes characters classified as whitespace (' ', '\t', '\n', ....) 
   * but also punctuation marks such as colons, commas, etc..
   * 
   * @see https://github.com/sirthias/parboiled2#id24
   */
  
  def StereotypeKind = rule { VS( "s:" ) ~> ( () => ScopeKind.STEREOTYPE ) }
  def MetaclassKind = rule { VS( "m:" ) ~> ( () => ScopeKind.METACLASS ) }
  def ClassifierKind = rule { VS( "c:" ) ~> ( () => ScopeKind.CLASSIFIER ) }
  def scopeKind: Rule1[ScopeKind] = rule { StereotypeKind | MetaclassKind | ClassifierKind }

  def Filepath = rule { ch( '\'' ) ~ capture( CharPredicate.Alpha ~ zeroOrMore( ch( ' ' ) ~ FileChar | FileChar ) ) ~ '\'' ~> ( FName( _ ) ) }

  def HumanName = rule { ch( '\'' ) ~ capture( CharPredicate.Alpha ~ zeroOrMore( ch( ' ' ) ~ AnyChar | AnyChar ) ) ~ '\'' ~> ( HName( _ ) ) }

  def SimpleName = rule { capture( CharPredicate.Alpha ~ zeroOrMore( CharPredicate.AlphaNum ) ) ~ ( WS | !CharPredicate.AlphaNum )  ~> ( SName( _ ) ) }
  
  def SimpleNameComma = rule { capture( CharPredicate.Alpha ~ zeroOrMore( CharPredicate.AlphaNum ) ) ~ VS ~ ',' ~> ( SName( _ ) ) }

  def AlphaAlphaNum = rule { CharPredicate.Alpha ~ zeroOrMore( CharPredicate.AlphaNum ) }

  def JavaName = rule { capture( AlphaAlphaNum ~ zeroOrMore( ch( '.' ) ~ AlphaAlphaNum ) ) ~ VS ~> ( JName( _ ) ) }

  def AlphaAnyChar = rule { CharPredicate.Alpha ~ zeroOrMore( ch( ' ' ) ~ AnyChar | AnyChar ) }
  
  def QualifiedName = rule { ch( '\'' ) ~ capture( AlphaAnyChar ~ zeroOrMore( str( "::" ) ~ AlphaAnyChar ) ) ~ '\'' ~> ( QName( _ ) ) }
 
  def QualifiedNameComma = rule { ch( '\'' ) ~ capture( AlphaAnyChar ~ zeroOrMore( str( "::" ) ~ AlphaAnyChar ) ) ~ '\'' ~ ',' ~> ( QName( _ ) ) }
  
  def SimpleNames = rule { '{' ~ SimpleName ~ zeroOrMore( ',' ~ SimpleName ) ~ '}' ~> { ( name: SName, names: Seq[SName] ) => Seq( name ) ++ names } }

  def QualifiedNames = rule { '{' ~ QualifiedName ~ zeroOrMore( ',' ~ QualifiedName ) ~ '}' ~> { ( name: QName, names: Seq[QName] ) => Seq( name ) ++ names } }

  def HNamePath = rule { '{' ~ HumanName ~ zeroOrMore( '>' ~ HumanName ) ~ '}' ~> { ( name: HName, names: Seq[HName] ) => Seq( name ) ++ names } }

  /**
   * Prefixed Terminals
   * 
   * The prefix indicates the type of the terminal.
   */
  
  def DiagramTypes = rule { optional( str( "diagramTypes" ) ~ ':' ~ SimpleNames ) ~> (_.getOrElse( Seq() )) }
  def DiagramStereotypes = rule { optional( str( "diagramStereotypes" ) ~ ':' ~ QualifiedNames ) ~> (_.getOrElse( Seq() )) }
  
  def name_HumanName = rule { str( "name" ) ~ ':' ~ HumanName }
  
  def icon_Filepath = rule { optional( str( "icon" ) ~ ':' ~ Filepath ) }
  
  def project_JavaName = rule { str( "project" ) ~ ':' ~ JavaName }
  def dependencies_JavaName = rule { optional( str( "dependencies" ) ~ '{' ~ oneOrMore( JavaName ) ~ '}' ) ~> (_.getOrElse( Seq() )) }
  
  def plugin_HumanName = rule { str( "plugin.id" ) ~ ':' ~ HumanName }
  def requiresPlugin_HumanName = rule { optional( str( "requires.plugin.id" ) ~ ':' ~ HumanName ) }
  
  def class_JavaName = rule { str( "class" ) ~ ':' ~ JavaName }
  def method_SimpleName = rule { str( "method" ) ~ ':' ~ SimpleName }

  def metaclass_SimpleName = rule { '[' ~ str( "m" ) ~ ':' ~ SimpleName ~ ']' }
  def metaclass_SimpleNameComma = rule { str( "m" ) ~ ':' ~ SimpleNameComma }
  def profile_QualifiedNameComma = rule { str( "p" ) ~ ':' ~ QualifiedNameComma }
  def stereotype_QualifiedName = rule { str( "s" ) ~ ':' ~ QualifiedName }
  def classifier_QualifiedName = rule { str( "c" ) ~ ':' ~ QualifiedName }

  def elementShape = rule { str( "elementShape" ) ~ ':' ~ elementTypeDesignation }

  def elementPath = rule { str( "elementPath" ) ~ ':' ~ elementTypeDesignation }
  def pathFrom = rule { str( "pathFrom" ) ~ ':' ~ elementTypeDesignation }
  def pathTo = rule { str( "pathTo" ) ~ ':' ~ elementTypeDesignation }

  def toolbarMenuPath_HNames = rule { str( "toolbarMenuPath" ) ~ ':' ~ HNamePath }
  
  /**
   * Non-terminal rules.
   */
  
  def projectContext = rule { project_JavaName ~ dependencies_JavaName ~ requiresPlugin_HumanName ~> ProjectContext }
  def pluginContext = rule { plugin_HumanName ~> PluginContext }
  def bundleContext = rule { projectContext | pluginContext }

  def dynamicScriptInfo = rule {
    name_HumanName ~ icon_Filepath ~ bundleContext ~ class_JavaName ~ method_SimpleName
  }

  def DelayedDerivedProperty = rule {
    "DelayedDerivedProperty" ~ '(' ~ dynamicScriptInfo ~ ')' ~> {
      ( name: HName, icon: Option[FName], context: BundleContext, className: JName, method: SName ) =>
        ComputedDerivedProperty( name, icon, context, className, method, DELAYED_COMPUTATION_UNTIL_INVOKED )
    }
  }

  def EarlyDerivedProperty = rule {
    "EarlyDerivedProperty" ~ '(' ~ dynamicScriptInfo ~ ')' ~> {
      ( name: HName, icon: Option[FName], context: BundleContext, className: JName, method: SName ) =>
        ComputedDerivedProperty( name, icon, context, className, method, EAGER_COMPUTATION_AS_NEEDED )
    }
  }

  def DelayedDerivedTable = rule {
    "DelayedDerivedTable" ~ '(' ~ dynamicScriptInfo ~ ')' ~> {
      ( name: HName, icon: Option[FName], context: BundleContext, className: JName, method: SName ) =>
        ComputedDerivedTable( name, icon, context, className, method, DELAYED_COMPUTATION_UNTIL_INVOKED )
    }
  }

  def EarlyDerivedTable = rule {
    "EarlyDerivedTable" ~ '(' ~ dynamicScriptInfo ~ ')' ~> {
      ( name: HName, icon: Option[FName], context: BundleContext, className: JName, method: SName ) =>
        ComputedDerivedTable( name, icon, context, className, method, EAGER_COMPUTATION_AS_NEEDED )
    }
  }

  def DerivedFeature = rule { DelayedDerivedProperty | EarlyDerivedProperty | DelayedDerivedTable | EarlyDerivedTable }

  def toolbarMenuAction = rule {
    toolbarMenuPath_HNames ~ name_HumanName ~ icon_Filepath ~ bundleContext ~ class_JavaName ~ method_SimpleName
  }
  
  def ToolbarMenuAction = rule { 
    "MainToolbarMenuAction"  ~ '(' ~ toolbarMenuAction ~ ')' ~> MainToolbarMenuAction
  }
  
  def BrowserMenuAction = rule {
    "BrowserContextMenuAction" ~ '(' ~ dynamicScriptInfo ~ ')' ~> BrowserContextMenuAction
  }
  
  def dynamicContextDiagramScriptInfo = rule {
    name_HumanName ~ icon_Filepath ~ DiagramTypes ~ DiagramStereotypes ~ bundleContext ~ class_JavaName ~ method_SimpleName ~> DynamicContextDiagramActionScriptInfo
  }

  def DiagramMenuAction = rule {
    "DiagramContextMenuAction" ~ '(' ~ dynamicContextDiagramScriptInfo ~ ')' ~> DiagramContextMenuAction
  }
  
  def metaclassDesignation = rule {
    metaclass_SimpleName ~> MetaclassDesignation
  }

  def stereotypedMetaclassesignation = rule {
    '[' ~ metaclass_SimpleNameComma ~ profile_QualifiedNameComma ~ stereotype_QualifiedName ~ ']' ~> StereotypedMetaclassDesignation
  }
  
  def classifiedInstanceDesignation = rule {
    '[' ~ metaclass_SimpleNameComma ~ classifier_QualifiedName ~ ']' ~> ClassifiedInstanceDesignation
  }
  
  def stereotypedClassifiedInstanceDesignation = rule {
    '[' ~ metaclass_SimpleNameComma ~ classifier_QualifiedName ~ profile_QualifiedNameComma ~ stereotype_QualifiedName ~ ']' ~> StereotypedClassifiedInstanceDesignation
  }

  def elementTypeDesignation: Rule1[ElementKindDesignation] = rule { metaclassDesignation | stereotypedMetaclassesignation | classifiedInstanceDesignation | stereotypedClassifiedInstanceDesignation }

  def ToplevelShapeCreator = rule {
    "ToplevelElementShape" ~ '(' ~ dynamicContextDiagramScriptInfo ~ elementShape ~ ')' ~> ToplevelShapeInstanceCreator
  }

  def ToplevelPathCreator = rule {
    "ToplevelRelationPath" ~ '(' ~ dynamicContextDiagramScriptInfo ~ elementPath ~ pathFrom ~ pathTo ~ ')' ~> ToplevelPathInstanceCreator
  }

  def DynamicActionScripts = rule { ToplevelShapeCreator | ToplevelPathCreator | BrowserMenuAction | DiagramMenuAction }

  def scripts = rule { "scripts" ~ '{' ~ zeroOrMore( DynamicActionScripts ) ~ '}' }
  def features = rule { "features" ~ '{' ~ zeroOrMore( DerivedFeature ) ~ '}' }
  def menuScripts = rule { "scripts" ~ '{' ~ zeroOrMore( ToolbarMenuAction ) ~ '}' }
  
  def DynamicScripts: Rule1[DynamicScriptsForInstancesOfKind] = rule {
    "dynamicScripts" ~ '(' ~ str( "name" ) ~ '=' ~ HumanName ~ elementTypeDesignation ~ scripts ~ ')' ~> DynamicScriptsForInstancesOfKind
  }

  def DynamicCharacterization: Rule1[ComputedCharacterization] = rule {
    "characterization" ~ '(' ~ str( "name" ) ~ '=' ~ HumanName ~ elementTypeDesignation ~ features ~ ')' ~> ComputedCharacterization
  }

  def ToolbarMenuScripts: Rule1[DynamicScriptsForMainToolbarMenus] = rule {
    "toolbarMenuScripts" ~ '(' ~ str( "name" ) ~ '=' ~ HumanName ~ menuScripts ~ ')' ~> DynamicScriptsForMainToolbarMenus
  }
  
  def Expression: Rule1[DynamicScript] = rule { DynamicScripts | DynamicCharacterization | ToolbarMenuScripts }
}

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
object DynamicScriptsParser {

  import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes.DynamicScript

  def parse( concreteSyntax: String ): ( DynamicScriptsParser, Try[Seq[DynamicScript]] ) = {
    val parser = new DynamicScriptsParser( concreteSyntax )
    ( parser, parser.InputLine.run() )
  }

}
