#ifndef _SIMULATIONLIST_H_
#define _SIMULATIONLIST_H_

#include <vector>
#include "Simulation.hpp"

/*
 * A simple wrapper around a standard vector of simulations?
 */
class SimulationList : public std::vector<Simulation>
{
 public:
  /*
   * Construct a simulation list from a given source URL
   */
  SimulationList(const wchar_t* url);
 private:
};


#endif // defined _SIMULATIONLIST_H_
