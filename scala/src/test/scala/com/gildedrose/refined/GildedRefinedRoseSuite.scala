package com.gildedrose.refined

import com.gildedrose.refined.Generators._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.annotation.tailrec

class GildedRefinedRoseSuite extends FunSuite with ScalaCheckDrivenPropertyChecks with Matchers {

  test("sulfuras => quality and sellIn never change") {
    forAll(sulfurasGen, Gen.chooseNum[Int](0, 999)) { case (item, days) =>
      val updated: Item = updateRose(item, days)
      updated.name shouldBe item.name
      updated.quality.value shouldBe 80
      updated.sellIn shouldBe item.sellIn
    }
  }

  test("aged brie => quality increases by 1 as long as sellIn > 0 with max of 50, sellIn decreases") {
    forAll(agedBrieGen) { item =>
      forAll(Gen.chooseNum(0, item.sellIn)) { days =>
        val updated: Item = updateRose(item, days)
        updated.name shouldBe item.name
        updated.quality.value shouldBe Math.min(50, item.quality.value + days)
        updated.sellIn shouldBe item.sellIn - days
      }
    }
  }

  test("aged brie => quality increases by 2 as soon as sellIn < 0 with max of 50, sellIn decreases") {
    forAll(agedBrieGen) { item =>
      forAll(Gen.chooseNum(item.sellIn + 1, item.sellIn + 100)) { days =>
        val updated: Item = updateRose(item, days)
        updated.name shouldBe item.name
        updated.quality.value shouldBe Math.min(50, item.quality.value + item.sellIn + 2*(days-item.sellIn))
        updated.sellIn shouldBe item.sellIn - days
      }
    }
  }

  test("backstage pass => quality = 0 when concert date has passed") {
    forAll(backstagePassGen) { item =>
      forAll(Gen.chooseNum(item.sellIn + 1, item.sellIn + 100)) { days =>
        val updated: Item = updateRose(item, days)
        updated.name shouldBe item.name
        updated.quality.value shouldBe 0
        updated.sellIn shouldBe item.sellIn - days
      }
    }
  }

  test("conjured items => quality and sellIn decreases by 2 when sellIn > 0") {
    forAll(conjuredItemGen) { item =>
      forAll(Gen.chooseNum(0, item.sellIn)) { days =>
        val updated: Item = updateRose(item, days)
        updated.name shouldBe item.name
        updated.quality.value shouldBe Math.max(0, item.quality.value - 2*days)
        updated.sellIn shouldBe item.sellIn - days
      }
    }
  }

  test("conjured items => quality decreases by 4 as soon as sellIn < 0") {
    forAll(conjuredItemGen) { item =>
      forAll(Gen.chooseNum(item.sellIn + 1, item.sellIn + 100)) { days =>
        val updated: Item = updateRose(item, days)
        updated.name shouldBe item.name
        updated.quality.value shouldBe Math.max(0, item.quality.value - 2*item.sellIn - 4*(days-item.sellIn))
        updated.sellIn shouldBe item.sellIn - days
      }
    }
  }

  test("other items => quality and sellIn decreases by 1 when sellIn > 0") {
    forAll(otherItemGen) { item =>
      forAll(Gen.chooseNum(0, item.sellIn)) { days =>
        val updated: Item = updateRose(item, days)
        updated.name shouldBe item.name
        updated.quality.value shouldBe Math.max(0, item.quality.value - days)
        updated.sellIn shouldBe item.sellIn - days
      }
    }
  }


  test("other items => quality decreases by 2 as soon as sellIn < 0") {
    forAll(otherItemGen) { item =>
      forAll(Gen.chooseNum(item.sellIn + 1, item.sellIn + 100)) { days =>
        val updated: Item = updateRose(item, days)
        updated.name shouldBe item.name
        updated.quality.value shouldBe Math.max(0, item.quality.value - item.sellIn - 2*(days-item.sellIn))
        updated.sellIn shouldBe item.sellIn - days
      }
    }
  }

  @tailrec
  private def updateRose(item: Item, days: Int): Item = {
    if (days <= 0) item
    else updateRose(RefinedRose.updateQuality(item), days - 1)
  }

  def debug(item: Item): String = s"Item '${item.name}' s=${item.sellIn}, q=${item.quality}"
}