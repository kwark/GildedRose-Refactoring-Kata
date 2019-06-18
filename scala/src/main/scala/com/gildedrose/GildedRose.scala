package com.gildedrose

import com.gildedrose.ItemType.{AgedBrie, BackstagePass, Conjured, Other, Sulfuras}

object GildedRose {
  def increaseQuality(item: Item, amount: Int): Unit = item.quality = Math.min(50, item.quality + amount)
  def decreaseQuality(item: Item, amount: Int): Unit = item.quality = Math.max(0, item.quality - amount)
}

class GildedRose(val items: Array[Item]) {

    def updateQuality(): Unit =
      items.foreach { item =>
        val itemType = ItemType.itemType(item.name)
        itemType match {
          case AgedBrie => GildedRose.increaseQuality(item, if (item.sellIn <= 0) 2 else 1)
          case Sulfuras => // don't change quality
          case BackstagePass if item.sellIn <= 0 => item.quality = 0
          case BackstagePass if item.sellIn > 10 => GildedRose.increaseQuality(item, 1)
          case BackstagePass if item.sellIn <= 10 => GildedRose.increaseQuality(item, 2)
          case BackstagePass if item.sellIn <= 5 => GildedRose.increaseQuality(item, 3)
          case Conjured => GildedRose.decreaseQuality(item, if (item.sellIn <= 0) 4 else 2)
          case Other => GildedRose.decreaseQuality(item, if (item.sellIn <= 0) 2 else 1)
        }
        if (itemType != Sulfuras) item.sellIn = item.sellIn - 1
      }

}