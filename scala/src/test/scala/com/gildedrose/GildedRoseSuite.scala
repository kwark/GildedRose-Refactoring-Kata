package com.gildedrose

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import Generators._
import org.scalacheck.Gen

class GildedRoseSuite extends FunSuite with ScalaCheckDrivenPropertyChecks with Matchers {

  test("sulfuras => quality and sellIn never change") {
    forAll(sulfurasGen, Gen.chooseNum[Int](0, 999)) { case (item, days) =>
      val originalSellIn = item.sellIn
      val updated: Item = updateRose(item, days)
      updated.name shouldBe item.name
      updated.quality shouldBe 80
      updated.sellIn shouldBe originalSellIn
    }
  }

  test("aged brie => quality increases by 1 as long as sellIn > 0 with max of 50, sellIn decreases") {
    forAll(agedBrieGen) { item =>
      val originalSellIn = item.sellIn
      val originalQuality = item.quality
      forAll(Gen.chooseNum(0, originalSellIn)) { days =>
        val copy = copyItem(item)
        val updated: Item = updateRose(copy, days)
        updated.name shouldBe item.name
        updated.quality shouldBe Math.min(50, originalQuality + days)
        updated.sellIn shouldBe originalSellIn - days
      }
    }
  }

  test("aged brie => quality increases by 2 as soon as sellIn < 0 with max of 50, sellIn decreases") {
    forAll(agedBrieGen) { item =>
      val originalSellIn = item.sellIn
      val originalQuality = item.quality
      forAll(Gen.chooseNum(originalSellIn + 1, originalSellIn + 100)) { days =>
        val copy = copyItem(item)
        val updated: Item = updateRose(copy, days)
        updated.name shouldBe item.name
        updated.quality shouldBe Math.min(50, originalQuality + originalSellIn + 2*(days-originalSellIn))
        updated.sellIn shouldBe originalSellIn - days
      }
    }
  }

  test("backstage pass => quality = 0 when concert date has passed") {
    forAll(backstagePassGen) { item =>
      val originalSellIn = item.sellIn
      forAll(Gen.chooseNum(originalSellIn + 1, originalSellIn + 100)) { days =>
        val copy = copyItem(item)
        val updated: Item = updateRose(copy, days)
        updated.name shouldBe item.name
        updated.quality shouldBe 0
        updated.sellIn shouldBe originalSellIn - days
      }
    }
  }

  test("conjured items => quality and sellIn decreases by 2 when sellIn > 0") {
    forAll(conjuredGen, Gen.chooseNum[Int](0, 999)) { case (item, days) =>
      val originalSellIn = item.sellIn
      val originalQuality = item.quality
      forAll(Gen.chooseNum(0, originalSellIn)) { days =>
        val copy = copyItem(item)
        val updated: Item = updateRose(copy, days)
        updated.name shouldBe item.name
        updated.quality shouldBe Math.max(0, originalQuality - 2 * days)
        updated.sellIn shouldBe originalSellIn - days
      }
    }
  }

  test("conjure items => quality decreases by 4 as soon as sellIn < 0") {
    forAll(conjuredGen) { item =>
      val originalSellIn = item.sellIn
      val originalQuality = item.quality
      forAll(Gen.chooseNum(originalSellIn + 1, originalSellIn + 100)) { days =>
        val copy = copyItem(item)
        val updated: Item = updateRose(copy, days)
        updated.name shouldBe item.name
        updated.quality shouldBe Math.max(0, originalQuality - 2 * originalSellIn - 4*(days-originalSellIn))
        updated.sellIn shouldBe originalSellIn - days
      }
    }
  }

  test("other items => quality and sellIn decreases by 1 when sellIn > 0") {
    forAll(otherItemGen, Gen.chooseNum[Int](0, 999)) { case (item, days) =>
      val originalSellIn = item.sellIn
      val originalQuality = item.quality
      forAll(Gen.chooseNum(0, originalSellIn)) { days =>
        val copy = copyItem(item)
        val updated: Item = updateRose(copy, days)
        updated.name shouldBe item.name
        updated.quality shouldBe Math.max(0, originalQuality - days)
        updated.sellIn shouldBe originalSellIn - days
      }
    }
  }

  test("other items => quality decreases by 2 as soon as sellIn < 0") {
    forAll(otherItemGen) { item =>
      val originalSellIn = item.sellIn
      val originalQuality = item.quality
      forAll(Gen.chooseNum(originalSellIn + 1, originalSellIn + 100)) { days =>
        val copy = copyItem(item)
        val updated: Item = updateRose(copy, days)
        updated.name shouldBe item.name
        updated.quality shouldBe Math.max(0, originalQuality - originalSellIn - 2*(days-originalSellIn))
        updated.sellIn shouldBe originalSellIn - days
      }
    }
  }


  private def copyItem(item: Item): Item = new Item(item.name, item.sellIn, item.quality)

  private def updateRose(item: Item, days: Int): Item = {
    val rose = new GildedRose(Array[Item](item))
    (0 until days).foreach(_ => rose.updateQuality())
    rose.items(0)
  }

  private def show(item: Item): String = s"Item '${item.name}' s=${item.sellIn}, q=${item.quality}"
}