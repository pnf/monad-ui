package monadui

object LiftOutputTuples {
  import scala.language.experimental.macros
  import scala.reflect.macros.blackbox.Context

  def apply[M[_],T](expr: M[T]): M[T] =  macro liftTuplesImpl[M[T],T]

  def liftTuplesImpl[MT : c.WeakTypeTag, T : c.WeakTypeTag](c: Context)(expr: c.Tree) = {

    import c.universe._

    val flatMapName = TermName("flatMap")
    val mapName = TermName("map")

    object OpsUnwrap {
      val tc = weakTypeOf[MT].typeConstructor
      def unapply(v: c.Tree) = v match {
        case v if v.tpe.typeConstructor == tc ⇒ Some(v)
        case q"$ops($inner)" if inner.tpe.typeConstructor == tc ⇒ Some(inner)
        case _ ⇒ None
      }
    }

    class TupleLiftingTransformer extends Transformer {
      // Perhaps Nested flatMap
      private object PN {
        def unapply(tree: Tree): Option[Tree] = tree match {
          case Apply(TypeApply(Select(wr@OpsUnwrap(vm), TermName("flatMap")), tOuter),
                     (oldClosure@Function(vValDef :: Nil,
                     PN(Apply(TypeApply(Select(OpsUnwrap(wm), inner : TermName), tInner),
                     (oldInnerClosure@Function(wValDef :: Nil, expr)) :: Nil)))) :: Nil)
            if (inner == flatMapName || inner == mapName) ⇒

            val ap2 = q"${wr.tpe.typeSymbol.companion}.ap2"

            // Crucial part: ensure that v is never used as the qualifier
            val vUsed = wm.find(vValDef.symbol == _.symbol)
            if (vUsed.isDefined) {
              c.info(vUsed.get.pos, s"Not lifting, because ${vValDef.symbol} is used on rhs", true)
              Some(TupleLiftingTransformer.super.transform(tree))
            }

            else {
              val xexpr = transform(expr)
              val qual = q"$ap2($vm, $wm)"

              // Function parameter of type (V,W) for new closure
              val vt = oldClosure.vparams.head.tpt.tpe
              val wt = oldInnerClosure.vparams.head.tpt.tpe
              val tupleOfXYtt: Tree = tq"($vt,$wt)"

              // Assemble and type new combinator application:
              val vwArgName = internal.reificationSupport.freshTermName("x$")
              val vvd = c.internal.valDef(vValDef.symbol, q"$vwArgName._1")
              val wvd = c.internal.valDef(wValDef.symbol, q"$vwArgName._2")
              val ret = q"$qual.$inner[..$tInner]({$vwArgName : $tupleOfXYtt => {$vvd; $wvd; ..$xexpr}})"
              val rett = c.typecheck(ret)

              // Extract the new closure after typing, so we can fix ownership problems.
              val Apply(_, newClosureTyped :: Nil) = rett
              // The new closure belongs to whoever owned the old closure.
              c.internal.setOwner(newClosureTyped.symbol, oldClosure.symbol.owner)
              // The new parameters belong to the new closure.
              c.internal.changeOwner(rett, vValDef.symbol.owner, newClosureTyped.symbol)
              c.internal.changeOwner(rett, wValDef.symbol.owner, newClosureTyped.symbol)
              // c.info(tree.pos, s"Lifting to $rett", true)
              Some(rett)
            }

          // Boring map.  Possibly the inner of a nested map.  Actually, we don't bother to insist that
          // it's a map/flatMap, since that will be done in the previous case.
          case Apply(TypeApply(Select(wm, comb), _),
          Function(_ :: Nil, _) :: Nil) ⇒
            Some(TupleLiftingTransformer.super.transform(tree))

          case t ⇒
            None
        }
      }

      override def transform(tree: Tree): Tree = tree match {
        case PN(xformed) ⇒ xformed

        case _ ⇒
          super.transform(tree)
      }

    }

    val ret =  (new TupleLiftingTransformer).transform(expr)

    c.info(c.enclosingPosition, s"Converted to ${showCode(ret, printTypes=Some(false))}", true)

    ret

  }

}

