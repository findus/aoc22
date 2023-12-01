package aoc2022

import io.io

object day3 extends App {

  def getAllCharsPresentInAll(set: List[Set[Char]]): Set[Char] = { set.foldLeft(set.head)((prev,next) => prev.intersect(next) ) }
  def getPosition(character: Char) = { character.toInt - (if (character.isUpper) { 38 } else { 96 }) }

  io.load("aoc2022/day3") { lines =>
    val items = lines.map(line => line.grouped(line.length / 2).map(_.sorted.distinct.toSet).toList)
    println(items.flatMap(set => getAllCharsPresentInAll(set).map(getPosition)).sum)
  }

  io.load("aoc2022/day3") { lines =>
    val items = lines.map(_.sorted.distinct.toSet).sliding(3,3).toList
    println(items.flatMap(setOfThree => getAllCharsPresentInAll(setOfThree)).map(getPosition).sum)
  }

}