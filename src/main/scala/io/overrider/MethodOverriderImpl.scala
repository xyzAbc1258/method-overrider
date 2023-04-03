package io.overrider

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.whitebox

class MethodOverriderImpl(val c: whitebox.Context) {

  import c.universe._

  private case class Target[A](member: A, memberType: Tree, impl: Tree) {
    def withMember[B](member: B): Target[B] = copy(member = member)
  }
  private case class Context(aType: Type, actualType: Type, valueIdent: Tree) {
    def inContext(typ: Type): Type = {
      val res1 = typ.finalResultType.asSeenFrom(actualType, aType.typeSymbol).dealias
      val res2 = res1.map {
        case TypeRef(ThisType(n), s, a) if n == aType.typeSymbol =>
          c.internal.typeRef(actualType, s, a)
        case x => x
      }
      res2
    }
  }

  private implicit class TypeUtils(val typ: Type) {
    def inContext(implicit ctx: Context): Type = {
      val res = ctx.inContext(typ)
      res
    }
  }

  private def error(msg: String): Nothing = {
    c.error(c.enclosingPosition, msg)
    sys.error(msg) //Not necessary since c.error doesn't return
  }

  private def info(msg: Any): Unit = c.info(c.enclosingPosition, s"$msg", force = true)

  def print(a: Expr[Any]): c.Expr[Unit] = {
    info(a.tree)
    info(c.universe.showRaw(a.tree, BooleanFlag.booleanToBooleanFlag(true)))
    c.Expr(q"()")
  }

  def withOverrides[A: c.WeakTypeTag](original: Expr[A])(overrides: Expr[Overrides[A]]): Expr[A] = {
    val aTag = implicitly[WeakTypeTag[A]].tpe.dealias
    ensureSupportsType(aTag.typeSymbol)
    val methodsImpls = analyzeOverrides(overrides.tree)
    val stableType = getStableType(original)
    implicit val ctx: Context = Context(
      aTag,
      stableType,
      q"this.__proxy"
    )
    val allImpls = buildMethodImpls(methodsImpls)
    val tree =
      q"""
          new $aTag with io.overrider.Proxy[$aTag] {
            val __proxy: ${original.actualType} = $original
            ..$allImpls
          }
     """
    //info(show(tree) + "\n\n\n" + showRaw(tree))
    c.Expr[A](tree)
  }

  private def getStableType(original: c.universe.Expr[_]) = {
    original.actualType match {
      case x @ SingleType(_, _) => x
      case _ =>
        val fullProxyType = buildProxyType(original)
        val proxyField = fullProxyType.member(TermName("__proxy"))
        c.internal.singleType(
          c.internal.thisType(
            c.internal.refinedType(List(fullProxyType, original.staticType), NoSymbol).typeSymbol
          ),
          proxyField
        )
    }
  }

  private def ensureSupportsType(aTagSymbol: Symbol): Unit = {
    if (!(aTagSymbol.isClass && aTagSymbol.asClass.isTrait) || aTagSymbol.asClass.isSealed) {
      error("MethodOverrider supports only non-sealed traits")
    }
  }

  private def buildProxyType(original: Expr[_]) = {
    val abstractProxyType = typeOf[Proxy[_]].typeConstructor
    appliedType(abstractProxyType, original.actualType)
  }

  private def buildMethodImpls(impls: List[Target[Name]])(implicit context: Context) = {
    for {
      member <- context.aType.members if member.isAbstract || impls.exists(_.member == member.name)
    } yield {
      impls.find(_.member == member.name) match {
        case Some(target) => buildMemberImpl(target.withMember(member))
        case None =>
          buildMemberImpl(
            Target(member, EmptyTree, Select(context.valueIdent, member))
          )
      }
    }
  }

  private def buildMemberImpl(target: Target[Symbol])(implicit context: Context): Tree = {
    if (target.member.isMethod) {
      val as = target.member.asMethod
      val returnTT = TypeTree(as.returnType.inContext)
      if (as.isVal) {
        q"""lazy val ${as.name}: $returnTT = ${target.impl}"""
      } else if (as.isVar) {
        q"""var ${as.name}: $returnTT = ${target.impl}"""
      } else {
        buildMethodImpl(target.withMember(as))
      }
    } else if (target.member.isType) {
      val asType = target.member.asType
      val typeParams = asType.typeParams
      val typeParamsDefs = typeParams.map(symbolToTypeDef)
      val typeParamsIdents = typeParamsDefs.map(_.name)
      q"final type ${target.member.name.toTypeName}[..$typeParamsDefs] = ${target.impl}[..$typeParamsIdents]"
    } else {
      error(s"Don't know how to handle member: ${target.member.fullName}")
    }
  }

  private def paramSymbolToValDef(s: Symbol, typeTreeBuilder: Type => Tree)(implicit ctx: Context) = {
    ValDef(
      if (s.isImplicit) Modifiers(Flag.IMPLICIT) else NoMods,
      s.name.toTermName,
      typeTreeBuilder(s.typeSignature.inContext),
      EmptyTree
    )
  }

  private def buildMethodImpl(target: Target[MethodSymbol])(implicit context: Context): Tree = {
    if (target.member.typeParams.nonEmpty) {
      buildGenericMethodImpl(target)
    } else {
      val returnTT = TypeTree(target.member.returnType.inContext)
      val withParams =
        target.member.paramLists.map(_.map(paramSymbolToValDef(_, TypeTree(_))))
      val paramIdents = withParams.map(_.map(x => Ident(x.name)))
      val tree = simplify(q"""${target.impl}(...$paramIdents)""")._1
      q"""def ${target.member.name}(...$withParams): $returnTT = $tree"""
    }
  }

  private def symbolToTypeDef(s: Symbol): TypeDef = {
    val typeParams = s.asType.typeParams.map(symbolToTypeDef)
    TypeDef(
      Modifiers(Flag.PARAM),
      s.name.toTypeName,
      typeParams,
      TypeBoundsTree(TypeTree(typeOf[Nothing]), TypeTree(typeOf[Any]))
    )
  }

  private def typeToTypeTree(baseType: Type, toReplace: mutable.AnyRefMap[AnyRef, Tree]): Tree = {
    val res = baseType match {
      case TypeRef(NoPrefix, h, args) if args.nonEmpty =>
        val nh = symbolToTypeTree(h, toReplace)
        val na = args.map(typeToTypeTree(_, toReplace))
        AppliedTypeTree(nh, na)
      case TypeRef(NoPrefix, h, _) =>
        symbolToTypeTree(h, toReplace)
      case x =>
        symbolToTypeTree(x.typeSymbol, toReplace)
    }
    res
  }

  private def symbolToTypeTree(baseType: Symbol, toReplace: mutable.AnyRefMap[AnyRef, Tree]): Tree = {
    baseType match {
      case m if toReplace.contains(m) => toReplace(m)
      case x: TypeSymbol              => TypeTree(x.toType)
      case x                          => error(s"Don't know what to do with ${showRaw(x)}")
    }
  }

  private def buildGenericMethodImpl(target: Target[MethodSymbol])(implicit context: Context): Tree = {
    val method = target.member
    val typeParamDefs = method.typeParams.map(symbolToTypeDef)
    def transformType(x: Type) = typeToTypeTree(
      x,
      mutable.AnyRefMap.from[AnyRef, Tree](method.typeParams.zip(typeParamDefs.map(_.name).map(Ident(_))))
    )
    val params = method.paramLists.map(_.map(paramSymbolToValDef(_, transformType)))
    val returnTT = transformType(method.returnType.inContext)
    val typeParamNames = typeParamDefs.map(_.name.toTypeName)
    val paramIdents = params.map(_.map(_.name))
    q"""
       def ${method.name}[..$typeParamDefs](...$params): $returnTT = ${target.impl}[..$typeParamNames](...$paramIdents)
     """
  }

  @tailrec
  private final def analyzeOverrides(overrides: Tree, list: List[Target[Name]] = Nil): List[Target[Name]] = {
    overrides match {
      case Apply( //head.define[rTypeParam](selectorArg => selectorBody)(implementation)
            Apply(
              TypeApply(Select(head, TermName("define")), List(rTypeParam)),
              List(Function(List(ValDef(_, selectorArg, _, _)), selectorBody))
            ),
            List(implementation)
          ) =>
        val methods = selectorBody.collect {
          case Select(Ident(h), method) if h == selectorArg => method
        }
        methods match {
          case method :: Nil => analyzeOverrides(head, Target(method, rTypeParam, implementation) :: list)
          case Nil           => error(s"No invocations found in: $selectorBody")
          case methods =>
            error(
              s"Multiple invocations. Don't know which method to replace. Candidates: ${methods.mkString(", ")}"
            )
        }
      case TypeApply(Select(_, TermName("apply")), _) => list.reverse //Overrides.apply[T]
      case x                                          => error(s"Unrecognized body: ${showRaw(x)}")
    }
  }

  private type SimplifyCtx = Map[TermName, Tree]

  private def simplify(tree: Tree, ctx: SimplifyCtx = Map.empty): (Tree, SimplifyCtx) = {
    def pure(x: Tree) = x -> ctx
    def rec_(x: Tree) = simplify(x, ctx)._1
    def recReset(x: Tree) = rec_(x) -> ctx
    tree match {
      case Apply(Function(defs, body), args) if defs.length == args.length =>
        val (x, _) = simplify(body, ctx ++ defs.map(_.name).zip(args))
        pure(x)
      case Block(l, t) =>
        val (v, nc) = l.foldLeft(Vector.empty[Tree] -> ctx) { case ((l, c), t) =>
          val (st, nctx) = simplify(t, c)
          (l :+ st) -> nctx
        }
        val nt = simplify(t, nc)._1
        pure(Block(v.toList, nt))
      case ValDef(m, n, tpe, v) =>
        val (sv, nc) = simplify(v, ctx)
        ValDef(m, n, tpe, sv) -> (nc - n) //nc or ctx... ?
      case Apply(body, args) => //(x => y => z)(1)(2) -> (y => z[x/1])(2) -> z[x/1][y/2]
        val newBody = rec_(body)
        if (!(newBody equalsStructure body)) recReset(Apply(newBody, args.map(rec_)))
        else pure(Apply(newBody, args.map(rec_)))
      case Ident(name: TermName) if ctx.contains(name) => pure(ctx(name))
      case i: Ident                                    => pure(i)
      case Select(t, n)                                => pure(Select(rec_(t), n))
      case Function(v, t)                              => pure(Function(v, simplify(t, v.map(_.name).foldLeft(ctx)(_ - _))._1))
      case l: Literal                                  => pure(l)
      case EmptyTree                                   => pure(EmptyTree)
      case TypeApply(tree, types)                      => pure(TypeApply(rec_(tree), types))
      case o @ This(_)                                 => pure(o)
      case other =>
        info("Other to simplify: " + c.universe.showRaw(other))
        pure(other)
    }
  }
}
