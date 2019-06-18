package com.gildedrose.refined

import com.gildedrose.ItemType
import com.gildedrose.ItemType.{AgedBrie, BackstagePass, Conjured, Sulfuras}
import eu.timepit.refined.types.string.NonEmptyString
import org.scalacheck.Gen

object Generators {

  val agedBrieGen: Gen[Item] = for {
    sellIn <- Gen.chooseNum[Int](0, 999)
    quality <- Gen.chooseNum[Int](0, 50)
  } yield Item.agedBrie(quality, sellIn).get


  val sulfurasGen: Gen[Item] =
    Gen.chooseNum[Int](Int.MinValue, Int.MaxValue).map(Item.sulfuras)

  val backstagePassGen: Gen[Item] = for {
    concert <- Gen.alphaStr.filter(_.nonEmpty)
    sellIn <- Gen.chooseNum[Int](0, 999)
    quality <- Gen.chooseNum(0, 50)
  } yield Item.backstagePass(concert, quality, sellIn).get

  val conjuredItemGen: Gen[Item] = for {
    name <- Gen.alphaStr
    sellIn <- Gen.chooseNum[Int](0, 999)
    quality <- Gen.chooseNum(0, 50)
  } yield Item.conjured(name, quality, sellIn).get

  val otherItemGen: Gen[Item] = for {
    name <- Gen.alphaStr.filter(_.nonEmpty).filterNot(n => Set(AgedBrie, Sulfuras, BackstagePass, Conjured).contains(ItemType.itemType(n)))
    sellIn <- Gen.chooseNum[Int](0, 999)
    quality <- Gen.chooseNum(0, 50)
  } yield Item.other(NonEmptyString.unsafeFrom(name), quality, sellIn).get

}
