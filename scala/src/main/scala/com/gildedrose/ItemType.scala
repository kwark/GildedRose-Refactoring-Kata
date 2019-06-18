package com.gildedrose

sealed trait ItemType extends Product with Serializable

object ItemType {

  val SULFURAS = "Sulfuras, Hand of Ragnaros"
  val AGED_BRIE= "Aged Brie"
  val BACKSTAGE_PASS = "Backstage passes"
  val CONJURED = "Conjured"

  final case object AgedBrie extends ItemType
  final case object Sulfuras extends ItemType
  final case object BackstagePass extends ItemType
  final case object Conjured extends ItemType
  final case object Other extends ItemType

  def itemType(name: String): ItemType =
    if (name.startsWith(AGED_BRIE)) AgedBrie
    else if (name.startsWith(SULFURAS)) Sulfuras
    else if (name.startsWith(BACKSTAGE_PASS)) BackstagePass
    else if (name.startsWith(CONJURED)) Conjured
    else Other

}

