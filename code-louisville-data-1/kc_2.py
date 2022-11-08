import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt

dataframe = pd.read_csv("assets/deadbees.csv")

dead = dataframe["dead"]
dead = pd.to_datetime(dataframe["Dead_Year"], errors = "coerce")


plt.plot(dead, "Dead_Year", marker="^")
plt.show()