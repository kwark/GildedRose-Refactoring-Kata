package com.gildedrose

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.boolean.Or
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.Interval.Closed
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.auto._

/**
 * Once the goblin has been slain, you can safely refactor the Item class:
 * - Make it an immutable case class with smart constructors to make it impossible to construct invalid Items.
 * - Use refinement types (http://www.github.com/fthomas/refined) to guard the quality and make sure the name is never empty
 */
package object refined {

  /**
   * Quality is an Int constrained from 0 to 50 (or 80 in the special case of Sulfuras)
   */
  type Quality = Refined[Int, Or[Closed[0, 50], Equal[80]]]

  object Quality extends RefinedTypeOps[Quality, Int] {
    def add(q: Quality, amount: PosInt): Quality =
      Quality.unapply(q.value + amount.value).fold(maxQuality)(identity)

    def minus(q: Quality, amount: PosInt): Quality =
      Quality.unapply(q.value - amount.value).fold(minQuality)(identity)
  }

  val minQuality: Quality = 0
  val maxQuality: Quality = 50
  val magicQuality: Quality = 80

  // smart constructor trick, see https://gist.github.com/tpolecat/a5cb0dc9adeacc93f846835ed21c92d2
  sealed abstract case class Item(name: NonEmptyString,
                                  quality: Quality,
                                  sellIn: Int) {

    def updated(q: Quality, sellIn: Int): Item = new Item(this.name, q, sellIn)  {}
  }

  /**
   * smart constructors
   */
  object Item {

    def sulfuras(sellIn: Int): Item = new Item(NonEmptyString.unsafeFrom(ItemType.SULFURAS), magicQuality, sellIn) {}

    def agedBrie(quality: Int, sellIn: Int): Option[Item] =
      Quality.unapply(quality).flatMap { q =>
        if (q != magicQuality) Some(new Item(NonEmptyString.unsafeFrom(ItemType.AGED_BRIE), q, sellIn) {})
        else None
      }

    def backstagePass(concert: String, quality: Int, sellIn: Int): Option[Item] =
      Quality.unapply(quality).flatMap { q =>
        if (q != magicQuality) Some(new Item(NonEmptyString.unsafeFrom(s"${ItemType.BACKSTAGE_PASS} to a $concert concert"), q, sellIn) {})
        else None
      }

    def conjured(name: String, quality: Int, sellIn: Int): Option[Item] =
      Quality.unapply(quality).flatMap { q =>
        if (q != magicQuality) Some(new Item(NonEmptyString.unsafeFrom(s"${ItemType.Conjured} $name"), q, sellIn) {})
        else None
      }

    def other(name: NonEmptyString, quality: Int, sellIn: Int): Option[Item] =
      Quality.unapply(quality).flatMap { q =>
        if (q != magicQuality) Some(new Item(name, q, sellIn) {})
        else None
      }
  }

}
