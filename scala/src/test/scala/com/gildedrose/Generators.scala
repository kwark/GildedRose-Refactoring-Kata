package com.gildedrose

import org.scalacheck.Gen
import ItemType._

object Generators {

  val agedBrieGen: Gen[Item] = for {
    sellIn <- Gen.chooseNum[Int](0, 999)
    quality <- Gen.chooseNum(0, 50)
  } yield new Item(AGED_BRIE, sellIn, quality)

  val sulfurasGen: Gen[Item] = for {
    sellin <- Gen.chooseNum[Int](Int.MinValue, Int.MaxValue)
  } yield new Item(SULFURAS, sellin, 80)

  val backstagePassGen: Gen[Item] = for {
    sellIn <- Gen.chooseNum[Int](0, 999)
    quality <- Gen.chooseNum(0, 50)
  } yield new Item(s"$BACKSTAGE_PASS to a TAFKAL80ETC concert", sellIn, quality)

  val conjuredGen: Gen[Item] = for {
    name <- Gen.alphaStr
    sellIn <- Gen.chooseNum[Int](0, 999)
    quality <- Gen.chooseNum(0, 50)
  } yield new Item(s"$CONJURED $name", sellIn, quality)

  val otherItemGen: Gen[Item] = for {
    name <- Gen.alphaStr.filter(_.nonEmpty).filterNot(n => Set(AgedBrie, Sulfuras, BackstagePass, Conjured).contains(ItemType.itemType(n)))
    sellIn <- Gen.chooseNum[Int](0, 999)
    quality <- Gen.chooseNum(0, 50)
  } yield new Item(name, sellIn, quality)

}
