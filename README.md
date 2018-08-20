# Simulation-of-farmer-technology-adoption
Monte Carlo simulation:

The code in this repository simulates the decisions of Iowa hog farmers on whether and which ammonia control technology to invest in based on costs, environmental values, and perceptions of risk.  Farmers perceive the costs of each technology and the impact of technology implementation on the cost of running the farm, the mass of ammonia not released to the atmosphere, and the risk associated with each technology based on its impact to farm operations and additional risk associated with implementing an unfamiliar technology based on how many farmers in each county have previously implemented the technology.  Farmers' decisions are determined by maximizing benefits/minimizing costs across options that fall within each farmer's threshold for willingness-to-accept risk.

The user sets parameters for each model run to determine potential scenarios of environmental values and financial or technical assistance available to the farmer, and the simulation chooses from a distribution of willingness-to-accept risk for each farmer in each iteration of each scenario. The model outputs the farmers' decisions among eight technologies or no technology over five periods of learning about technologies through in-county farm neighbors' previous technology decisions.  The model runs at either the upper or lower confidence bounds on technology cost and performance.

Here is the purpose of each script in this repository, in the order that they run:
1. Simulation cost eff data HIGH/LOW: Creates upper and lower bounds on data for technology cost and performance at each farm in the sample.
2. Simulation farmer decisions sample: User sets parameters for scenarios to run through Monte Carlo simulation.  This script drives the simulation.  User can set Monte Carlo n and define as many scenarios as desired.
    A. Simulation farmer decisions: Draws from distribution of farmer willingness-to-accept risk and simulates decisions under the scenario parameters
    B. Farmer decision simulation reset: This script resets the simulation to wipe data from previous scenario run so that the new scenario parameters will take effect
