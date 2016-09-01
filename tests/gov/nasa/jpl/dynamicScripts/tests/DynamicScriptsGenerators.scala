/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
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
package gov.nasa.jpl.dynamicScripts.tests

import org.scalacheck._

import gov.nasa.jpl.dynamicScripts.DynamicScriptsTypes._

import scala.Predef.augmentString
import scala.StringContext
import scala.collection.immutable._

/**
 * @author Nicolas.F.Rouquette@jpl.nasa.gov
 */
object DynamicScriptsGenerators {

  val path1Names = for ( i1 <- Gen.choose(1, 9) ) yield HName(s"MyMenu${i1}")

  val path2Names = for ( i2 <- Gen.choose(1, 9) ) yield HName(s"MySubMenu${i2}")

  val toolbarMenuPathGen = for ( (p1, p2) <- Gen.zip(path1Names, path2Names) ) yield Seq( p1, p2 )
  
  val menuActionNameGen = for ( i3 <- Gen.choose(1, 9) ) yield HName(s"MyAction${i3}")
  
  val metaclassNames = Gen.oneOf( "Association", "Class", "Datatype", "Element", "Property", "Package" )

  val metaclassGen = for ( m <- metaclassNames ) yield MetaclassDesignation( SName( m ) )

  val sysmlStereotypesGen = for (
    s <- Gen.oneOf(
      StereotypedMetaclassDesignation( SName( "Class" ), QName( "SysML" ), QName( "SysML::Block" ) ),
      StereotypedMetaclassDesignation( SName( "AssociationClass" ), QName( "SysML" ), QName( "SysML::Block" ) ),
      StereotypedMetaclassDesignation( SName( "Datatype" ), QName( "SysML" ), QName( "SysML::ValueType" ) ) )
  ) yield s

  val foundationComponentStereotypesGen = for ( s <- Gen.oneOf( "mission:Component", "mission:Function", "analysis:Characterization", "behavior:PropertyGroup" ) )
    yield StereotypedMetaclassDesignation(
    SName( "Component" ),
    QName( "IMCE::IMCE Bundle Profile::imce.jpl.nasa.gov::foundation::project::project-bundle" ),
    QName( s"IMCE::IMCE Bundle Profile::imce.jpl.nasa.gov::foundation::project::project-bundle::${s.stripPrefix( ":" )}::${s}" ) )

  val qudvShapeClassifiersGen = for ( c <- Gen.oneOf( "SimpleQuantityKind", "DerivedQuantityKind", "SimpleUnit", "DerivedUnit" ) )
    yield ClassifiedInstanceDesignation( SName( "InstanceSpecification" ), QName( s"IMCE.ISO-80000::IMCE.QUDV::${c}" ) )

  val qudvPathClassifiersGen = for ( c <- Gen.oneOf( "A_systemOfQuantities_quantityKind", "A_systemOfUnits_unit" ) )
    yield ClassifiedInstanceDesignation( SName( "InstanceSpecification" ), QName( s"IMCE.ISO-80000::IMCE.QUDV::${c}" ) )

  val elementKindDesignationGen: Gen[ElementKindDesignation] = Gen.oneOf( 
      metaclassGen, 
      sysmlStereotypesGen, 
      foundationComponentStereotypesGen, 
      qudvShapeClassifiersGen, 
      qudvPathClassifiersGen )

  val characterizationNameGen = for ( i <- Gen.choose(10, 20) ) yield HName( s"characterizationScript${i}" )
  
  /**
   * TODO: add generators for ComputedDerivedFeature
   */
  val computedCharacterizationGen = for ( (n, k) <- Gen.zip( characterizationNameGen, elementKindDesignationGen ) ) 
    yield ComputedCharacterization( name=n, characterizesInstancesOf=k, computedDerivedFeatures=Seq() )
  
  val dynamicScriptNameGen = for ( i <- Gen.choose(10, 20) ) yield HName( s"dynamicScript${i}" )
  
  /**
   * TODO: add generators for DynamicActionScript
   */
  val dynamicScriptsForInstancesOfKindGen = for ( (n, k) <- Gen.zip( dynamicScriptNameGen, elementKindDesignationGen ) )
    yield DynamicScriptsForInstancesOfKind( name=n, applicableTo=k, scripts=Seq() )
  
  val dynamicScriptGen: Gen[DynamicScript] = Gen.oneOf( computedCharacterizationGen, dynamicScriptsForInstancesOfKindGen )
}