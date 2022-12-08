# biodivercity (development version)

- Build and use models to make spatial predictions for community (_Beta_) diversity, i.e., variety of species
- Calculate and benchmark total (_Gamma_) diversity, i.e., total quantity of species for a given area

<br>

# biodivercity 0.1.0 (pre-release)

- Finished development of main functions and vignettes, documented on website https://ecological-cities.github.io/biodivercity
- Accompanying datasets yet to be released, so package still cannot be built/used
- Features include the ability to build and apply predictive models, but currently available only for local (_Alpha_) diversity, i.e., predict the number of species
- Assessment of community (_Beta_) diversity to be released in the future, i.e., predict the variety of species


Key features include:

- Step-by-step protocols for random point sampling of an animal group within areas of interest
- Generate data summaries of the animals surveyed, at multiple levels of granularity (e.g., areas, periods, animal groups, species)
- Download and process landscape data within areas of interest (e.g., remotely sensed data, OpenStreetMap data, manually mapped data)
- Build predictive models to assess local (_Alpha_), community (_Beta_) and total (_Gamma_) diversity for an animal group (currently available only for _Alpha_ diversity); respectively, these three types of diversity represent the local quantity of species, variety of species, and total quantity of species within an area of interest
- Use models to make pixel-based spatial predictions across new areas and time periods
- Convert vector data generated from future design scenarios into formats suitable for model predictions
- Summarise spatial predictions to benchmark and compare biodiversity 'performance' between planning units across the city
