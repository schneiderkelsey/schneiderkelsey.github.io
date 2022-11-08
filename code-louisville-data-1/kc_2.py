import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt

dataframe = pd.read_csv("assets/deadbees.csv")

dead = dataframe["dead"]
dead = pd.to_datetime(dataframe["dead"], errors = "coerce")


plt.plot(dead, dead, marker="^")
plt.show()