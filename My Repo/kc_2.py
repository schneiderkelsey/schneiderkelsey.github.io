import pandas as pd
import matplotlib as mpl
import matplotlib.pyplot as plt

dataframe = pd.read_csv("assets/deadbees.csv")

Total_DeadBees = dataframe["Total_DeadBees"]
Dead_Year = pd.to_datetime(dataframe["Dead_Year"], errors = "coerce")


plt.plot(Dead_Year, Total_DeadBees, marker="^")
plt.show()