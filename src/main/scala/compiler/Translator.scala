/**
  * Hipster cellular automaton language.
  * Hipster to Java translator.
  *
  * © 2017, Dominic Verity, Macquarie University, All rights reserved.
  * 
  * This Source Code Form is subject to the terms of the Mozilla Public
  * License, v. 2.0. If a copy of the MPL was not distributed with this
  * file, You can obtain one at http://mozilla.org/MPL/2.0/.
  */

package compiler

import org.bitbucket.inkytonik.kiama._

import rewriting.Rewriter
import HipsterTree.HipsterTree
import attribution.Attribution

class Translator (val tree : HipsterTree)
    extends Attribution with TranslationAttributes with StructuralAnalysis
    with NameAnalysis with TypeAnalysis with Rewriter {

  import compiler.HipsterTree._
  import compiler.{MiniJavaTree => J}
  import SymbolTable._

  /**
    * Identifier names of neighbours declared in source 
    * program. Given in order declared, because `collectl` does a 
    * top-down, left-to-right traversal of the tree. 
    */
  val neighbours : Vector[String] =
    (collectl {
      case NeighbourDecl(IdnDef(i), _) => i
    } (tree.root)).toVector

  /**
    * List of names of initialisers declared in the source program.
    */
  val initialisers : Vector[IdnDef] =
    (collectl {
      case InitialiserDecl(i, _) => i
    } (tree.root)).toVector

  /**
    * Default values for each type of variable.
    */
  def defaultVal (tipe : Type) : J.Expression =
    tipe match {
      case IntType() => J.IntExp(0)
      case FloatType() => J.DoubleExp(0)
      case BoolType() => J.FalseExp()
      case NeighbourType() => J.ThisExp()
      case UnknownType() => J.FalseExp()
    }

  /**
    * List of state field identifier definition nodes in source program.
    * Each one is paired with the translation of the initialisation 
    * expression given in its declaration. If that expression is missing an
    * appropriate default is used.
    */
  val stateFields : Vector[(IdnDef, J.Expression)] =
    (collectl {
      case d @ VarDecl(t, i, e)
          if parentIsOfType[StateDecl](d) =>
            (i, e.map(translateExpr).getOrElse(defaultVal(t)))
    } (tree.root)).toVector

  /**
    * Translate Hipster types into corresponding Java types.
    */
  def translateType(tipe : Type) : J.Type =
    tipe match {
      case IntType() => J.IntType()
      case BoolType() => J.BooleanType()
      case FloatType() => J.DoubleType()
      // neighbour variables become references to `Cell` objects.
      case NeighbourType() => J.ClassType(J.IdnUse("Cell"))
      // Shouldn't get here because UnknownType() is a pseudo-type
      // which never actually occurs in a Hipster source tree.
      case UnknownType() => J.ClassType(J.IdnUse("Error"))
    }

  /**
    * Translate a Hipster statement.
    */
  def translateStmt(stmt : Statement) : J.Statement =
    stmt match {
      // block statements translate directly to Java blocks.
      case Block(stmts) => J.Block(stmts.map(translateStmt))
      case VarDecl(t, i, e) =>
        // The obvious translation: translate the type, get the
        // target name for the identifier being assigned to,
        // translate the initialiser expression or obtain a
        // default value translation, and build the corresponding
        // Java assignment using these translations.
        J.Var(translateType(t), J.IdnDef(getTargetName(i)),
          Some(e.map(translateExpr).getOrElse(defaultVal(t))))
      case AssignStmt(lval, exp) =>
        // Must take care when translating assignments to state fields
        // of type neighbour, because we are assigning to a field of
        // a cell in the next generation we must make sure that the
        // reference stored is to a cell in that generation.

        // Translate expression to assign.
        val et = translateExpr(exp)
        // Make an identifier node referencing the `_other` field
        val other = J.IdnUse((getTargetName(tree.root, "_other")))
        // Extract the identifier of the field being assigned to
        // (if we are assigning to a field).
        val idn =
          lval match {
            case NeighbourExpr(_, i) => i
            case IdnExpr(i) => i
          }
        // Add an `_other` dotted suffix to the translation of
        // the expression on the right if necessary...
        val ett =
          // check to see if the extracted identifier....
          entity(idn) match {
            // if it is a state field of `neighbour` type then...
            case StateField(NeighbourType(),_) =>
              // add the required suffix...
              et match {
                // but if the translated expression is just `this`
                // emit a direct field reference....
                case J.ThisExp() => J.IdnExp(other)
                // otherwise just add dot `_other`. 
                case _ => J.FieldExp(et,other)
              }
           // in all other case add nothing to the expression
           case _ => et
          }
        // finally form the target assignment statement.
        J.Assign(translateLValue(lval), ett)
      case IfStmt(exp, thn, els) =>
        // Translate directly into corresponding Java if statement.
        J.If(translateExpr(exp), translateStmt(thn),
          els.map(translateStmt).getOrElse(J.Empty()))
      case ForStmt(i, s, e, st, b) =>
        // FIXME Add your code to translate `for` statements here.
        val i_var = J.Var(J.IntType(), J.IdnDef(getTargetName(i)), Some(translateExpr(s)))
        val e_var = J.Var(J.IntType(), J.IdnDef(getTargetName(b, "_end")), Some(translateExpr(e)))
        val st_var = J.Var(J.IntType(), J.IdnDef(getTargetName(b, "_step")),
          st.map(translateExpr).orElse(Some(J.IntExp(1))))
        val d_var = J.Var(J.BooleanType(), J.IdnDef(getTargetName(b, "_dirn")),
          Some(J.GreaterEqExp(J.IdnExp(J.IdnUse(st_var.name.idn)), J.IntExp(0))))
        J.Block(Vector(
          i_var, e_var, st_var, d_var,
          J.If(
            J.NotEqualsExp(J.IdnExp(J.IdnUse(st_var.name.idn)), J.IntExp(0)),
            J.While(
              J.OrExp(
                J.AndExp(
                  J.IdnExp(J.IdnUse(d_var.name.idn)),
                  J.GreaterEqExp(
                    J.IdnExp(J.IdnUse(e_var.name.idn)),
                    J.IdnExp(J.IdnUse(i_var.name.idn))
                  )
                ),
                J.AndExp(
                  J.NotExp(J.IdnExp(J.IdnUse(d_var.name.idn))),
                  J.LessEqExp(
                    J.IdnExp(J.IdnUse(e_var.name.idn)),
                    J.IdnExp(J.IdnUse(i_var.name.idn))
                  )
                )
              ),
              J.Block(Vector(
                translateStmt(b),
                J.Assign(
                  J.IdnExp(J.IdnUse(i_var.name.idn)),
                  J.PlusExp(
                    J.IdnExp(J.IdnUse(i_var.name.idn)),
                    J.IdnExp(J.IdnUse(st_var.name.idn))
                  )
                )
              ))
            ),
            J.Empty()
          )
        )

        )
      case s @ IterateOverStmt(i, ns, b) =>
        // FIXME Add your code to translate `iterate...over` statements here.

        val i_var = J.Var(
          J.ClassType(J.IdnUse("Cell")),
          J.IdnDef(getTargetName(i)),
          None
        )

        val ns_var = translateNeighbourSet(ns)

        J.Block(
          i_var+:

          ns_var.flatMap (
            (n : J.LValue) =>
              Vector(
                J.Assign(
                  J.IdnExp(J.IdnUse(i_var.name.idn)),
                  n
                ),
                translateStmt(b)
              )
          )
        )

      case s @ CellStmt(CoordExpr(os), b) =>
        // Enclose translation in a block.
        J.Block(
          Vector(
            J.Var(
              // Declare a new variable of type `Cell`...
              J.ClassType(J.IdnUse("Cell")),
              // Get a uniquified name for that variable...
              J.IdnDef(getTargetName(b, "_cell")),
              Some(
                // Initialise it to a cell reference obtained by...
                J.CallExp(
                  None,
                  // Calling the provided method for getting a cell
                  // at a specified absolute location.
                  J.IdnUse("getCellAbs"),
                  // Passing it ordinates translated from the expressions
                  // specified in the coordinate expression provided.
                  os.map(translateExpr)))),
            // Finally add the translation of the body.
            translateStmt(b)))
      case FunCallStmt(FunCallExpr(i, as)) =>
        // See translation of `FunCallExpr` below for commentary on
        // this translation.
        J.Call(None, J.IdnUse(getTargetName(i)), as.map(translateExpr))
      case ReturnStmt(v) => J.Return(v.map(translateExpr))
      case EmptyStmt() => J.Empty()
    }

  /**
    * Translate a Hipster expression.
    */
  def translateExpr(exp : Expression) : J.Expression =
    exp match {
      // Various identifier and neighbour field expression are
      // tricky to translate, indeed this is probably to most
      // difficult thing to get right when 
      case lv : LValue => translateLValue(lv)

      // To translate function calls we:
      //
      // * get the uniquified target name for the function identifier,
      // * translate each entry in the list of argument expressions and
      // * build the corresponding java method call.
      //
      // All methods are called on `this`, and the Hipster semantic rules
      // regarding function types and usage ensure that this is always
      // the right thing to do.
      case s @ FunCallExpr(i, as) =>
        J.CallExp(None, J.IdnUse(getTargetName(i)), as.map(translateExpr))

      // The rest of these clauses are essentially just direct translations
      // from Hipster operations and constants to their corresponding Java
      // counterparts.
      case IntExpr(n) => J.IntExp(n)
      case FloatExpr(n) => J.DoubleExp(n)
      case TrueExpr() => J.TrueExp()
      case FalseExpr() => J.FalseExp()
      case PlusExpr(l, r) =>
        J.PlusExp(translateExpr(l), translateExpr(r))
      case MinusExpr(l, r) =>
        J.MinusExp(translateExpr(l), translateExpr(r))
      case MultExpr(l, r) =>
        J.StarExp(translateExpr(l), translateExpr(r))
      case DivExpr(l, r) =>
        J.DivExp(translateExpr(l), translateExpr(r))
      case ModExpr(l, r) =>
        J.ModExp(translateExpr(l), translateExpr(r))
      case NegExpr(e) =>
        J.NegExp(translateExpr(e))
      case AndExpr(l, r) =>
        J.AndExp(translateExpr(l), translateExpr(r))
      case OrExpr(l, r) =>
        J.OrExp(translateExpr(l), translateExpr(r))
      case NotExpr(e) =>
        J.NotExp(translateExpr(e))
      case EqualExpr(l, r) =>
        J.EqualsExp(translateExpr(l), translateExpr(r))
      case LessExpr(l, r) =>
        J.LessExp(translateExpr(l), translateExpr(r))
      case LessEqExpr(l, r) =>
        J.LessEqExp(translateExpr(l), translateExpr(r))
      case GreaterExpr(l, r) =>
        J.GreaterExp(translateExpr(l), translateExpr(r))
      case GreaterEqExpr(l, r) =>
        J.GreaterEqExp(translateExpr(l), translateExpr(r))
    }

  /**
    * Conditionally construct a singleton vector. Returns a vector
    * containing only the value `v` unless the parameter `b` is `false`,
    * in which case it returns the empty vector.
    */
  def conditional[T] (v : => T, b : Boolean) : Vector[T] =
    if (b) Vector(v) else Vector()

  /**
    * Translate a vector of identifier names into an iterated field
    * expression. Converts the emtpy vector to `this`.
    */
  def pathToLValue (ns : Vector[String]) : J.LValue =
    ns match {
      case id +: ids =>
        ((J.IdnExp(J.IdnUse(id)) : J.LValue) /: ids) (
          (e, s) => J.FieldExp(e, J.IdnUse(s)))
      case _ => J.ThisExp()
    }

  /**
    * Translate a neighbour symbol or variable. This translation
    * takes into account whether the identifier being translated
    * is in the body of a cell statement or on the left of an 
    * assignment statement.
    */
  def translateNbrRef (i : IdnUse) : Vector[String] = 
    (entity(i) match {
      case _ : Neighbour =>
        conditional(getTargetName(i, "_cell"), isInCellBody(i)) ++
        conditional(getTargetName(i), i != IdnUse("me"))
      case e : TypedEntity =>
        conditional(getTargetName(i), e.tipe == NeighbourType())
      case _ => Vector()
    }) ++
      conditional(getTargetName(tree.root, "_other"), isOnTheLeft(i))

  /**
    * Translate a state field identifier. Adds a prefix which takes 
    * into account whether the identifier being translated is in the 
    * body of a cell statement or on the left of an assignment statement.
    */
  def translateStateFld (i : IdnUse) : Vector[String] = 
    conditional(getTargetName(i, "_cell"), isInCellBody(i)) ++
      conditional(getTargetName(tree.root, "_other"), isOnTheLeft(i)) :+
      getTargetName(i)

  /**
    * Translate a Hipster lvalue expression.
    */
  def translateLValue (lv : LValue) : J.LValue =
    pathToLValue(
      lv match {
        case NeighbourExpr(IdnExpr(i),j) =>
          translateNbrRef(i) :+ getTargetName(j)
        case IdnExpr(i) =>
          entity(i) match {
            case _ : StateField => translateStateFld(i)
            case _ : Neighbour => translateNbrRef(i)
            case _ => Vector(getTargetName(i))
          }
      })

  /**
    * Translate a neighbour set into a list of target code
    * cell refernces.
    */
  def translateNeighbourSet(s : NeighbourSet) : Vector[J.LValue] = {
    val cell = J.IdnUse(getTargetName(s, "_cell"))
    (s match {
      case Others() => neighbours
      case All() => neighbours :+ "me"
      case Subset(nbrs) => nbrs map {case IdnExpr(IdnUse(i)) => i }
    }) map ((n : String) => {
      val idn = J.IdnUse(getTargetName(s, n))
      n match {
        case "me" if isInCellBody(s) => J.IdnExp(cell)
        case "me" => J.ThisExp()
        case _ if isInCellBody(s) => J.FieldExp(J.IdnExp(cell), idn)
        case _ => J.IdnExp(idn)
      }
    })}

  /**
    * Mark function definitions that will translate to
    * methods of the outer class.
    */
  def translatesToInnerMethod(n : FunctionDecl) : Boolean =
    insideCellFunction(n)

  /**
    * Mark function definitiosn that will translate to
    * methods of the inner class.
    */
  def translatesToOuterMethod(n : FunctionDecl) : Boolean =
    insideGeneralFunction(n) || insideInitialiserFunction(n)

  /**
    * Collect together all of the method declarations for the inner
    * `Cell` class.
    */ 
  val innerMethods : Vector[J.ClassMember] =
    (collectl {
      case d @ UpdaterDecl(bs) =>
        J.Method(
          // We synchronize the updater because it could be called
          // from one thread while the colour mapper is being called
          // from another one.
          Vector(J.Synchronized()),
          // The updater doesn't return values, so return type is `void`.
          J.Void(),
          // The simulator is hardcoded to expect the updater method to
          // have the un-mangled name "updater".
          J.IdnDef("updater"),
          // No arguments.
          Vector(),
          // The first thing that an updater must do is to copy the current
          // state field values from this cell in the current generation to
          // the corresponding cell in the new generation. Construct that
          // copying code by mapping over the list of state field identifiers
          // collected above.
          stateFields.map{
            case (i, _) =>
              J.Assign(
                J.FieldExp(
                  // We assign to the field of the "_other" reference, that is
                  // to say to the corresponding field of the partner object
                  // in the next generation.
                  J.IdnExp(J.IdnUse(getTargetName(tree.root, "_other"))),
                  J.IdnUse(getTargetName(i))),
                J.IdnExp(J.IdnUse(getTargetName(i))))
          } ++ bs.map(translateStmt))
      case ColourMapperDecl(bs) =>
        J.Method(
          // We synchronize the colour mapper because it could be called
          // from one thread while the updater is being called from
          // another one.
          Vector(J.Synchronized()),
          // The colour mapper method returns a colour values
          // as an integer.
          J.ReturnType(J.IntType()),
          // The simulator is hardcoded to expect the colour mapper method to
          // have the un-mangled name "mapper".
          J.IdnDef("mapper"),
          // No arguments.
          Vector(),
          // Construct the list of translated statements for the body of this
          // method by mapping the statement translation over list of statements
          // in the original declaration.
          bs map translateStmt)
      case d : FunctionDecl if (translatesToInnerMethod(d)) =>
        // For function declarations just call the translator defined below.
        translateFunctionDecl(d)
      case d @ NeighbourhoodDecl(nbrs) =>
        // Generate a method for initialising neighbour references.
        // This is only called once for each `Cell` object  during the
        // construction of the enclosing `CellularAutomaton` object. 
        J.Method(
          // No modifiers. There is no need to syncronize things because
          // this is called well before the automaton is displayed or
          // updated for the first time.
          Vector(),
          // No return value.
          J.Void(),
          // The simulator is hardcoded to expect this method to
          // have the un-mangled name `initNeighbours`.
          J.IdnDef("initNeighbours"),
          // No parameters.
          Vector(),
          J.Assign(
            // Initialise the `_theother` field
            J.IdnExp(J.IdnUse(getTargetName(tree.root, "_theother"))),
            // It will always contain a reference to the corresponding
            // cell in the other generation. That reference is obtained
            // by calling the `getOther` method provided by the simulator.
            J.CallExp(None, J.IdnUse("getOther"), Vector())) +:
            // Map over the list of neighbour declarations..
            nbrs.map {
              case NeighbourDecl(i, CoordExpr(os)) =>
                J.Assign(
                  // ...for each one make an assignment to the corresponding
                  // field of the `Cell` class
                  J.IdnExp(J.IdnUse(getTargetName(i))),
                  // ... of the cell in this generation obtained by calling
                  // the simulator provided `getCellRel` method..
                  J.CallExp(None, J.IdnUse("getCellRel"),
                    // ... passing offsets translated by mapping the
                    // expression translator over the entries of the
                    // associated coordinate expression.
                    os.map(translateExpr)))
            })
    } (tree.root)).toVector

  /**
    * Collect together all of the method declarations for the outer
    * `CellularAutomaton` class.
    */ 
  val outerMethods : Vector[J.ClassMember] =
    (collectl {
      case InitialiserDecl(i, bs) =>
        J.Method(
          // No modifiers.
          Vector(),
          // Initialisers don't return values, so return type is `void`
          J.Void(),
          // Get the translated name for this initialiser.
          J.IdnDef(getTargetName(i)),
          // No arguments.
          Vector(),
          // Construct the list of translated statements for the body of this
          // method by mapping the statement translation over list of statements
          // in the original declaration.
          bs map translateStmt)
      case d : FunctionDecl if (translatesToOuterMethod(d)) =>
        // For function declarations just call the translator defined below.
        translateFunctionDecl(d)
      case DimDecl(dms) =>
        // Generate constructor for outer CellularAutomaton class.
        J.Method(
          // No access modifiers
          Vector(),
          // Constructor with a call to the default super-class constructor
          J.ConstructWithSuper(Vector()),
          // This is a constructor for the CellularAutomaton class.
          J.IdnDef("CellularAutomaton"),
          // Default constructor, no parameters.
          Vector(),
          // Now for the body code.
          Vector(
            // Initialise dimensions.
            J.Call(None, J.IdnUse("initDims"),
              dms.map(d => translateExpr(d.exp))),
            // Initialise cyclicity flags.
            J.Call(None, J.IdnUse("initCycles"),
              dms map {
                case _ : BoundedDim => J.FalseExp()
                case _ : CyclicDim => J.TrueExp()
              }),
            // Create the cells objects owned by this automaton.
            J.Call(None, J.IdnUse("initWorld"), Vector())
          ) ++
            // Register each initialiser.
            initialisers.map {
              case i @ IdnDef(s) => 
              J.Call(None, J.IdnUse("addInitialiser"),
                Vector(
                  J.StringExp(s),
                  J.LambdaExp(
                    Vector(),
                    J.Call(None, J.IdnUse(getTargetName(i)), Vector()))))})
    } (tree.root)).toVector

  /**
    * All functions are translated in the same way, but the translated code 
    * for some will end up in the outer `CellularAutomaton` class and
    * for others will end up in the inner `Cell` class. So we provide a
    * separate translator for function declarations which can be called 
    * in both of the translations for inner and outer classes.
    */
  def translateFunctionDecl(f : FunctionDecl) : J.Method =
    f match {
      case FunctionDecl(i, ps, r, bs) =>
        J.Method(
          // Private modifier, there is no reason for methods generated from
          // Hipster functions to be visible from the outside.
          Vector(J.Private()),
          // Translate return type, or `void` if the functions doesn't
          // have a return value.
          r.map(t => J.ReturnType(translateType(t))).getOrElse(J.Void()),
          // Get the translated name for this function.
          J.IdnDef(getTargetName(i)),
          // Map the type and target name translations over the argument list.
          // to form the translated argument list.
          (ps map {case ParamDecl(t, i) =>
            J.Argument(translateType(t), J.IdnDef(getTargetName(i)))}),
          // Construct the list of translated statements for the body of this
          // method by mapping the statement translation over list of statements
          // in the original declaration.
          bs map translateStmt)
    }


  /**
    * Collect together all of the field declarations for the inner
    * `Cell` class
    */ 
  val innerFields : Vector[J.Field] =
    (collectl {
      case d : NeighbourhoodDecl =>
        Vector(
          J.Field(Vector(), J.ClassType(J.IdnUse("Cell")),
            J.IdnDef(getTargetName(tree.root, "_other")), None),
          J.Field(Vector(), J.ClassType(J.IdnUse("Cell")),
            J.IdnDef(getTargetName(tree.root, "_theother")), None))
      case d @ VarDecl(t, i, e) if parentIsOfType[StateDecl](d) =>
        Vector(
          J.Field(Vector(), translateType(t),
            J.IdnDef(getTargetName(i)), None))
      case NeighbourDecl(i, _) =>
        Vector(
          J.Field(Vector(), J.ClassType(J.IdnUse("Cell")),
            J.IdnDef(getTargetName(i)), None))
    } (tree.root)).toVector.flatten

  /**
    * Collect together all of the field declarations for the outer
    * `Cell` class
    */ 
  val outerFields : Vector[J.Field] =
    (collectl {
      case ConstantDecl(t, i, e) =>
        J.Field(Vector(J.Private(),J.Static(),J.Final()),
          translateType(t), J.IdnDef(getTargetName(i)),
          Some(translateExpr(e)))
    } (tree.root)).toVector

  // Boiler plate code.

  /**
    * Standard one parameter constructor, simply delegates to 
    * the super-class constructor.
    */
  val innerConstructor : J.Method =
    J.Method(
      Vector(),
      J.ConstructWithSuper(Vector(
        J.IdnExp(J.IdnUse(getTargetName(tree.root, "_loc"))))),
      J.IdnDef("Cell"),
      Vector(
        J.Argument(
          J.ClassType(J.IdnUse("Point")),
          J.IdnDef(getTargetName(tree.root, "_loc")))),
      Vector(
        J.Assign(
          J.IdnExp(J.IdnUse(getTargetName(tree.root, "_other"))),
          J.ThisExp()),
        J.Call(None, J.IdnUse("resetCellState"), Vector())))

  /**
    * Inner class method to reset the state of a cell object.
    */
  val resetMethod : J.Method =
    J.Method(
      Vector(J.Synchronized()), J.Void(), J.IdnDef("resetCellState"),
      Vector(),
      stateFields map {
        case (i,e) => J.Assign(J.IdnExp(J.IdnUse(getTargetName(i))),e)}
    )

  /**
    * Boilerplate code for the getCellAbs and makeCell methods of the
    * outer class.
    */
  val outerBoilerplate : Vector[J.Method] =
    Vector(
      J.Method(
        Vector(),
        J.ReturnType(J.ClassType(J.IdnUse("BaseCell"))),
        J.IdnDef("makeCell"),
        Vector(
          J.Argument(
            J.ClassType(J.IdnUse("Point")),
            J.IdnDef(getTargetName(tree.root, "_loc")))),
        Vector(
          J.Return(Some(
            J.NewExp(
              J.IdnUse("Cell"),
              J.ConstructorArgs(Vector(
                J.IdnExp(J.IdnUse(getTargetName(tree.root, "_loc")))))))))),
      J.Method(
        Vector(),
        J.ReturnType(J.ClassType(J.IdnUse("Cell"))),
        J.IdnDef("getCellAbs"),
        Vector(
          J.Varargs(
            J.IntType(),
            J.IdnDef(getTargetName(tree.root, "_loc")))),
        Vector(
          J.Return(Some(
            J.CastExp(
              J.ClassType(J.IdnUse("Cell")),
              J.CallExp(
                None, J.IdnUse("getCell"),
                Vector(J.IdnExp(J.IdnUse(getTargetName(tree.root, "_loc"))))
              )))))))

  /**
    * Boilerplate code for the getCellRel and makeCell methods of the
    * inner class.
    */
  val innerBoilerplate : Vector[J.Method] =
    Vector(
      J.Method(
        Vector(),
        J.ReturnType(J.ClassType(J.IdnUse("Cell"))),
        J.IdnDef("getCellRel"),
        Vector(
          J.Varargs(
            J.IntType(),
            J.IdnDef(getTargetName(tree.root, "_off")))),
        Vector(
          J.Return(Some(
            J.CastExp(
              J.ClassType(J.IdnUse("Cell")),
              J.CallExp(
                None, J.IdnUse("getCell"),
                Vector(
                  J.IdnExp(J.IdnUse(getTargetName(tree.root, "_off")))))
            ))))),
      J.Method(
        Vector(),
        J.ReturnType(J.ClassType(J.IdnUse("Cell"))),
        J.IdnDef("getOther"),
        Vector(),
        Vector(
          J.Return(Some(
            J.CastExp(
              J.ClassType(J.IdnUse("Cell")),
              J.CallExp(
                None, J.IdnUse("getNextGen"),
                Vector())))))),
      J.Method(
        Vector(), J.Void(), J.IdnDef("otherToSelf"), Vector(),
        Vector(
          J.Assign(
            J.IdnExp(J.IdnUse(getTargetName(tree.root, "_other"))),
            J.ThisExp()))),
      J.Method(
        Vector(), J.Void(), J.IdnDef("otherToOther"), Vector(),
        Vector(
          J.Assign(
            J.IdnExp(J.IdnUse(getTargetName(tree.root, "_other"))),
            J.IdnExp(J.IdnUse(getTargetName(tree.root, "_theother")))))))

  /**
    * Java tree defining the inner `Cell` class.
    */
  val innerClass : J.Class =
    J.Class(
      Vector(),
      J.IdnDef("Cell"),
      Some(J.IdnUse("BaseCell")),
      (innerFields :+ innerConstructor) ++ (innerMethods :+
        resetMethod) ++ innerBoilerplate
    )

  /**
    *  Java tree defining the outer `CellularAutomaton` class.
    */
  val translated : J.Class =
    J.Class(
      Vector(),
      J.IdnDef("CellularAutomaton"),
      Some(J.IdnUse("BaseAutomaton")),
      (outerFields :+ innerClass) ++ outerMethods ++ outerBoilerplate
    )
}
