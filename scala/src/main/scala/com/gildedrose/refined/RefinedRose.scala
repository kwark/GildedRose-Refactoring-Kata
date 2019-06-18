package com.gildedrose.refined

import com.gildedrose.ItemType
import com.gildedrose.ItemType.{AgedBrie, BackstagePass, Conjured, Other, Sulfuras}
import eu.timepit.refined.types.numeric.PosInt

object RefinedRose {

  private def amount(item: Item, factor: Int = 1): PosInt = if (item.sellIn <= 0) PosInt.unsafeFrom(2*factor) else PosInt.unsafeFrom(1*factor)

  def updateQuality(item: Item): Item = {
    val itemType = ItemType.itemType(item.name.value)
    val quality = itemType match {
        case Sulfuras                           => item.quality // don't change quality
        case AgedBrie                           => Quality.add(item.quality, amount(item))
        case BackstagePass if item.sellIn <= 0  => Quality(0)
        case BackstagePass if item.sellIn > 10  => Quality.add(item.quality, PosInt(1))
        case BackstagePass if item.sellIn <= 10 => Quality.add(item.quality, PosInt(2))
        case BackstagePass if item.sellIn <= 5  => Quality.add(item.quality, PosInt(3))
        case Conjured                           => Quality.minus(item.quality, amount(item, factor = 2))
        case Other                              => Quality.minus(item.quality, amount(item))
      }
    if (itemType != Sulfuras)
      item.updated(quality, item.sellIn - 1)
    else
      item

  }

}