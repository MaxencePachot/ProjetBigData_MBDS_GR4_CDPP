import pandas
import matplotlib.pyplot as plt


class Analysis:

    def __init__(self, file):
        self.file = file
        self.desc = file.df.describe()

    def plot(self, value):
        self.file.df[value].value_counts().sort_index().plot(kind='bar', width=1, edgecolor = 'black')
        plt.show()

    def scatter(self, value):
        pass


