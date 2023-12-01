package aoc2022

import scala.collection.mutable
import io.io

object  day19 extends App {

  val regex = "Blueprint (\\d+): Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.".r

  val ORE = "ore"
  val CLAY = "clay"
  val OBSIDIAN = "obsidian"
  val GEODE = "geode"

  case class RobotState(amount: Int)
  case class State(blueprint: Blueprint, ore_robots: RobotState, clay_robots: RobotState, obsidian_robots: RobotState, geode_robots: RobotState, ore: Int, clay: Int, obsidian: Int, geode: Int, passedMinutes: Int, queue: Map[String, List[Int]]) {
    def everythingBuildable(): Boolean = {
      val e = blueprint.geode_robot_costs.buildable(this).isDefined &&
        blueprint.ore_robot_costs.buildable(this).isDefined &&
        blueprint.obsidian_robot_costs.buildable(this).isDefined &&
        blueprint.clay_robot_costs.buildable(this).isDefined
      e
    }
  }

  case class Costs(name: String, ore: Int, clay: Int, obsidian: Int) {
    def buildable(state: State):  Option[State]  = {
      val newEntry = state.queue.get(name).map(e => e.appended(0)).getOrElse(List(0))
      val newMap = state.queue + ((name,newEntry))
      val buildable = state.ore >= ore && state.clay >= clay && state.obsidian >= obsidian

      val e = name match {
        case ORE      if state.ore_robots.amount >= state.blueprint.clay_robot_costs.ore.max(state.blueprint.obsidian_robot_costs.ore).max(state.blueprint.geode_robot_costs.ore) =>  None
        case CLAY     if state.ore < state.blueprint.clay_robot_costs.clay &&  state.clay_robots.amount >= state.blueprint.obsidian_robot_costs.clay =>  None
        case OBSIDIAN if state.ore < state.blueprint.obsidian_robot_costs.ore && state.clay < state.blueprint.obsidian_robot_costs.clay && state.obsidian_robots.amount >= state.blueprint.geode_robot_costs.obsidian =>  None
        case GEODE    if state.ore < state.blueprint.geode_robot_costs.ore && state.obsidian < state.blueprint.geode_robot_costs.obsidian =>  None
        case _ => Some("a")
      }

      if (e.isEmpty) {
        return None
      }

      val newState = if (buildable) Some(state.copy(ore = state.ore - ore, clay = state.clay - clay, obsidian = state.obsidian - obsidian, queue = newMap)) else None
      newState
    }
  }
  case class Blueprint(
                      id: Int,
                      ore_robot_costs: Costs,
                      clay_robot_costs: Costs,
                      obsidian_robot_costs: Costs,
                      geode_robot_costs: Costs
                      )

  val firstGeode: mutable.Stack[Int] = new mutable.Stack()

  def simulate(state: State): Option[State] = {
    val newTime = state.passedMinutes + 1

    val stateAfterTimeUpdate = state.copy(passedMinutes = newTime)

    val clayBuildable = stateAfterTimeUpdate.blueprint.clay_robot_costs.buildable(stateAfterTimeUpdate)
    val oreBuildable = stateAfterTimeUpdate.blueprint.ore_robot_costs.buildable(stateAfterTimeUpdate)
    val obsidianBuildable = stateAfterTimeUpdate.blueprint.obsidian_robot_costs.buildable(stateAfterTimeUpdate)
    val geodeBuildable = stateAfterTimeUpdate.blueprint.geode_robot_costs.buildable(stateAfterTimeUpdate)
    val doNothing = stateAfterTimeUpdate
    val states = List(geodeBuildable, obsidianBuildable, oreBuildable, clayBuildable)
      .flatten
      .appended(doNothing)

    val meem =  if (geodeBuildable.isDefined) {
      List(geodeBuildable.get)
    } else {
      states
    }

    val results = meem.map(newState => {

//      if (newState.geode_robots.amount > 0 && !firstGeode.contains(newTime)) {
//        firstGeode.push(newTime)
//      }
//
//      if (firstGeode.minOption.isDefined && (firstGeode.min < newTime) && newState.geode_robots.amount == 0 ) {
//        return Some(newState)
//      }

      val ready = newState.queue.map { case (k,v) => (k,v.map(n => n - 1)) }.map { case (k,v) => (k, v.partition(n => n == -1)) }
      val remaining = ready.map { case (k,v) => (k, v._2) }


      val newOre = newState.ore + newState.ore_robots.amount
      val newClay = newState.clay + newState.clay_robots.amount
      val newObsidian = newState.obsidian + newState.obsidian_robots.amount
      val newGeode = newState.geode + newState.geode_robots.amount

      val newBots = ready.map { case (k,v) => (k, v._1) }
      val newestState = newBots.foldLeft(newState)((prev, action) => {
        action._1 match {
          case "ore" => prev.copy(ore_robots = prev.ore_robots.copy(prev.ore_robots.amount + action._2.size))
          case "clay" => prev.copy(clay_robots = prev.clay_robots.copy(prev.clay_robots.amount + action._2.size))
          case "obsidian" => prev.copy(obsidian_robots = prev.obsidian_robots.copy(prev.obsidian_robots.amount + action._2.size))
          case "geode" => prev.copy(geode_robots = prev.geode_robots.copy(prev.geode_robots.amount + action._2.size))
        }
      })

      val newestNewestState = newestState.copy(ore = newOre, clay = newClay, obsidian = newObsidian, geode = newGeode, queue = remaining)
      if (newTime == 24) {
        return Some(newestNewestState)
      }
      simulate(newestNewestState)
    })
    val geode = results.flatten.maxByOption(_.geode)
    geode
  }

  io.load("aoc2022/day19") { lines =>
    val parsedInput = lines.map { case regex(
    id,
    orerobotcosts,
    clayrobotcosts,
    obsidianrobotcosts,
    obsidianrobotcostsclay,
    geoderobotcostsore,
    geoderobotcostsobsidian
    ) => Blueprint(
      Integer.parseInt(id),
      Costs(ORE, Integer.parseInt(orerobotcosts), 0 ,0),
      Costs(CLAY, Integer.parseInt(clayrobotcosts), 0 ,0),
      Costs(OBSIDIAN, Integer.parseInt(obsidianrobotcosts), Integer.parseInt(obsidianrobotcostsclay) ,0),
      Costs(GEODE, Integer.parseInt(geoderobotcostsore), 0 , Integer.parseInt(geoderobotcostsobsidian)),
    ) }

    val result = parsedInput.flatMap(line => {
      val initialState = State(line, RobotState(1), RobotState(0), RobotState(0), RobotState(0), 0, 0, 0, 0, 0, Map.empty)
      firstGeode.clear()
      val result = simulate(initialState)
      println(result.map(_.geode * line.id), "line id:", line.id,"result", result.get.geode)
      result.map(_.geode * line.id)
    }).sum
    println(result)


  }

}
