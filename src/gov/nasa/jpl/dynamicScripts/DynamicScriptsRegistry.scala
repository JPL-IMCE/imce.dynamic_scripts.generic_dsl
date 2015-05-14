/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package gov.nasa.jpl.dynamicScripts

import java.io.File
import scala.io.Source
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes._
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes.BinaryDerivationRefresh._
import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes.ScopeKind._
import scala.util.Success
import scala.util.Failure
import org.parboiled2.ParseError
import java.nio.file.Paths

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
case class DynamicScriptsRegistry(
  metaclassCharacterizations: Map[String, scala.collection.immutable.SortedSet[ComputedCharacterization]],
  stereotypedMetaclassCharacterizations: Map[String, scala.collection.immutable.SortedSet[ComputedCharacterization]],
  classifierCharacterizations: Map[String, scala.collection.immutable.SortedSet[ComputedCharacterization]],
  stereotypedClassifierCharacterizations: Map[String, scala.collection.immutable.SortedSet[ComputedCharacterization]],

  metaclassActions: Map[String, scala.collection.immutable.SortedSet[DynamicScriptsForInstancesOfKind]],
  stereotypedMetaclassActions: Map[String, scala.collection.immutable.SortedSet[DynamicScriptsForInstancesOfKind]],
  classifierActions: Map[String, scala.collection.immutable.SortedSet[DynamicScriptsForInstancesOfKind]],
  stereotypedClassifierActions: Map[String, scala.collection.immutable.SortedSet[DynamicScriptsForInstancesOfKind]],

  toolbarMenuPathActions: Map[String, scala.collection.immutable.SortedSet[DynamicScriptsForMainToolbarMenus]] ) {

  override def toString(): String = {

    def mapToString[V <: DynamicScript]( map: Map[String, scala.collection.immutable.SortedSet[V]] ): String =
      ( for ( ( k, cs ) <- map ) yield s"    '${k}' -> ${( for ( c <- cs ) yield c.prettyPrint( "      " ) ).mkString( "{\n", "\n", "\n    }\n" )}" ).mkString( "{\n", "\n", "  }" )

    val buff = new StringBuilder()
    buff ++= "DynamicScriptsRegistry("
    buff ++= s"\n  metaclassCharacterizations=${mapToString( metaclassCharacterizations )},"
    buff ++= s"\n  stereotypedMetaclassCharacterizations=${mapToString( stereotypedMetaclassCharacterizations )},"
    buff ++= s"\n  classifierCharacterizations=${mapToString( classifierCharacterizations )},"
    buff ++= s"\n  stereotypedClassifierCharacterizations=${mapToString( stereotypedClassifierCharacterizations )},"
    buff ++= s"\n  metaclassActions=${mapToString( metaclassActions )},"
    buff ++= s"\n  stereotypedMetaclassActions=${mapToString( stereotypedMetaclassActions )},"
    buff ++= s"\n  classifierActions=${mapToString( classifierActions )},"
    buff ++= s"\n  stereotypedClassifierActions=${mapToString( stereotypedClassifierActions )}"
    buff ++= s"\n  toolbarMenuPathActions=${mapToString( toolbarMenuPathActions )}"
    buff ++= "\n)"

    buff.toString()
  }
}

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
object DynamicScriptsRegistry {

  type SMap[K, V] = Map[K, scala.collection.immutable.SortedSet[V]]

  def updatedSMap[V](
    map: SMap[String, V], key: String, value: V,
    default: => scala.collection.immutable.SortedSet[V],
    merge: ( ( V, V ) => V ) ): SMap[String, V] = {
    val currentValues: scala.collection.immutable.SortedSet[V] = map.getOrElse( key, default )
    val newValues: scala.collection.immutable.SortedSet[V] =
      if ( currentValues.contains( value ) ) ( default /: currentValues ) { case ( ( s, v ) ) => s + merge( v, value ) }
      else currentValues + value
    map.updated( key, newValues )
  }

  def emptyComputedCharacterizationSet() = scala.collection.immutable.TreeSet[ComputedCharacterization]()( ComputedCharacterizationOrdering() )

  def updatedSMap( map: SMap[String, ComputedCharacterization], k: String, c: ComputedCharacterization ): SMap[String, ComputedCharacterization] =
    updatedSMap( map, k, c, emptyComputedCharacterizationSet, DynamicScriptsTypes.merge )

  def emptyDynamicActionScriptSet() = scala.collection.immutable.TreeSet[DynamicScriptsForInstancesOfKind]()( DynamicScriptsForInstancesOfKindOrdering() )

  def updatedSMap( map: SMap[String, DynamicScriptsForInstancesOfKind], k: String, a: DynamicScriptsForInstancesOfKind ): SMap[String, DynamicScriptsForInstancesOfKind] =
    updatedSMap( map, k, a, emptyDynamicActionScriptSet, DynamicScriptsTypes.merge )

  def emptyDynamicScriptsForMainToolbarMenusSet() = scala.collection.immutable.TreeSet[DynamicScriptsForMainToolbarMenus]()( DynamicScriptsForMainToolbarMenusOrdering() )

  def updatedSMap( map: SMap[String, DynamicScriptsForMainToolbarMenus], k: String, a: DynamicScriptsForMainToolbarMenus ): SMap[String, DynamicScriptsForMainToolbarMenus] =
    updatedSMap( map, k, a, emptyDynamicScriptsForMainToolbarMenusSet, DynamicScriptsTypes.merge )

  def init() = DynamicScriptsRegistry(
    metaclassCharacterizations = Map(),
    stereotypedMetaclassCharacterizations = Map(),
    classifierCharacterizations = Map(),
    stereotypedClassifierCharacterizations = Map(),

    metaclassActions = Map(),
    stereotypedMetaclassActions = Map(),
    classifierActions = Map(),
    stereotypedClassifierActions = Map(),

    toolbarMenuPathActions = Map() )

  def merge( r: DynamicScriptsRegistry, dynamicScript: DynamicScript ): DynamicScriptsRegistry = dynamicScript match {

    case c: ComputedCharacterization => c.characterizesInstancesOf match {
      case _: MetaclassDesignation                     => r.copy( metaclassCharacterizations = DynamicScriptsRegistry.updatedSMap( r.metaclassCharacterizations, c.name.hname, c ) )
      case _: StereotypedMetaclassDesignation          => r.copy( stereotypedMetaclassCharacterizations = DynamicScriptsRegistry.updatedSMap( r.stereotypedMetaclassCharacterizations, c.name.hname, c ) )
      case _: ClassifiedInstanceDesignation            => r.copy( classifierCharacterizations = DynamicScriptsRegistry.updatedSMap( r.classifierCharacterizations, c.name.hname, c ) )
      case _: StereotypedClassifiedInstanceDesignation => r.copy( stereotypedClassifierCharacterizations = DynamicScriptsRegistry.updatedSMap( r.stereotypedClassifierCharacterizations, c.name.hname, c ) )
    }

    case s: DynamicScriptsForInstancesOfKind => s.applicableTo match {
      case _: MetaclassDesignation                     => r.copy( metaclassActions = DynamicScriptsRegistry.updatedSMap( r.metaclassActions, s.name.hname, s ) )
      case _: StereotypedMetaclassDesignation          => r.copy( stereotypedMetaclassActions = DynamicScriptsRegistry.updatedSMap( r.stereotypedMetaclassActions, s.name.hname, s ) )
      case _: ClassifiedInstanceDesignation            => r.copy( classifierActions = DynamicScriptsRegistry.updatedSMap( r.classifierActions, s.name.hname, s ) )
      case _: StereotypedClassifiedInstanceDesignation => r.copy( stereotypedClassifierActions = DynamicScriptsRegistry.updatedSMap( r.stereotypedClassifierActions, s.name.hname, s ) )
    }

    case t: DynamicScriptsForMainToolbarMenus => r.copy( toolbarMenuPathActions = DynamicScriptsRegistry.updatedSMap( r.toolbarMenuPathActions, t.name.hname, t ) )
  }

  protected def parseDynamicScript( file: File ): Either[List[DynamicScript], String] =
    if ( !( file.exists() && file.canRead() ) )
      Right( s"Dynamic Script file '${file}' does not exist or is not readable" )
    else
      DynamicScriptsParser.parse( Source.fromFile( file ).mkString ) match {
        case ( _, Success( scripts: Seq[DynamicScript] ) ) =>
          Left( scripts.toList )
        case ( parser, Failure( e: ParseError ) ) =>
          Right( s"Dynamic Script file '${file}' has syntax errors\n${parser.formatError( e )}" )
        case ( _, Failure( e ) ) =>
          Right( s"Dynamic Script file '${file}' has unexpected parsing errors\n${e}" )
      }

  def mergeDynamicScripts( r: DynamicScriptsRegistry, filepaths: List[String] ): ( DynamicScriptsRegistry, List[String] ) =
    ( ( r, List[String]() ) /: filepaths ) {
      case ( ( r: DynamicScriptsRegistry, errors: List[String] ), filepath: String ) =>
        try {
          val path = Paths.get( filepath ).toRealPath()
          parseDynamicScript( path.toFile ) match {
            case Left( scripts: List[DynamicScript] ) => ( ( r /: scripts ) { ( ri, script ) => merge( ri, script ) }, errors )
            case Right( error: String )               => ( r, errors :+ error )
          }

        } catch {
          case t: Throwable =>
            ( r, errors :+ t.getClass.getName+" for dynamic script filepath: '"+filepath+"': "+t.getMessage )
        }
    }

}