import scala.annotation.tailrec

object  day19 extends App {

  val regex = "Blueprint [1-9999]: Each ore robot costs (\\d+) ore. Each clay robot costs (\\d+) ore. Each obsidian robot costs (\\d+) ore and (\\d+) clay. Each geode robot costs (\\d+) ore and (\\d+) obsidian.".r

  case class RobotState(amount: Int)
  case class State(blueprint: Blueprint, ore_robots: RobotState, clay_robots: RobotState, obsidian_robots: RobotState, geode_robots: RobotState, ore: Int, clay: Int, obsidian: Int, geode: Int, passedMinutes: Int, queue: Map[String, List[Int]])
  case class Costs(name: String, ore: Int, clay: Int, obsidian: Int) {
    def buildable(state: State) = {
      val newEntry = state.queue.get(name).map(e => e.appended(1)).getOrElse(List(1))
      val newMap = state.queue + ((name,newEntry))
      val buildable = state.ore >= ore && state.clay >= clay && state.obsidian >= obsidian
      val newState = if (buildable) Some(state.copy(ore = state.ore - ore, clay = state.clay - clay, obsidian = state.obsidian - obsidian, queue = newMap)) else None
      newState
    }
  }
  case class Blueprint(
                      ore_robot_costs: Costs,
                      clay_robot_costs: Costs,
                      obsidian_robot_costs: Costs,
                      geode_robot_costs: Costs
                      )

  def simulate(state: State): State = {
    val newTime = state.passedMinutes + 1
    if (newTime == 24) return state.copy(passedMinutes = newTime)

    val ready = state.queue.map { case (k,v) => (k,v.map(n => n - 1)) }.map { case (k,v) => (k, v.partition(n => n == 0)) }
    val remaining = ready.map { case (k,v) => (k, v._2) }

    val stateAfterBotUpdate = state.copy(queue = remaining, passedMinutes = newTime)

    val clayBuildable = stateAfterBotUpdate.blueprint.clay_robot_costs.buildable(stateAfterBotUpdate)
    val oreBuildable = stateAfterBotUpdate.blueprint.ore_robot_costs.buildable(stateAfterBotUpdate)
    val obsidianBuildable = stateAfterBotUpdate.blueprint.obsidian_robot_costs.buildable(stateAfterBotUpdate)
    val geodeBuildable = stateAfterBotUpdate.blueprint.geode_robot_costs.buildable(stateAfterBotUpdate)
    val doNothing = stateAfterBotUpdate
    val states = List(geodeBuildable, obsidianBuildable, oreBuildable, clayBuildable).flatten.appended(doNothing)

    val results = states.map(newState => {
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

      val newestNewestState = newestState.copy(ore = newOre, clay = newClay, obsidian = newObsidian, geode = newGeode)
      simulate(newestNewestState)
    })
    val geode = results.maxBy(_.geode)
    geode
  }

  io.load("day19") { lines =>
    val parsedInput = lines.map { case regex(
    orerobotcosts,
    clayrobotcosts,
    obsidianrobotcosts,
    obsidianrobotcostsclay,
    geoderobotcostsore,
    geoderobotcostsobsidian
    ) => Blueprint(
      Costs("ore", Integer.parseInt(orerobotcosts), 0 ,0),
      Costs("clay", Integer.parseInt(clayrobotcosts), 0 ,0),
      Costs("obsidian", Integer.parseInt(obsidianrobotcosts), Integer.parseInt(obsidianrobotcostsclay) ,0),
      Costs("geode", Integer.parseInt(geoderobotcostsore), 0 , Integer.parseInt(geoderobotcostsobsidian)),
    ) }

    val initialState = State(parsedInput.head, RobotState(1), RobotState(0), RobotState(0), RobotState(0),0 ,0 ,0 ,0 ,0, Map.empty)
    val result = simulate(initialState)
    println(result)

    println(parsedInput)
  }

}
