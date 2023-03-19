import sys

import pandas
from PyQt5.QtCore import QStringListModel
from PyQt5.QtGui import QPixmap
from PyQt5.QtWidgets import *
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
import matplotlib.pyplot as plt
import numpy as np
from Analysis import Analysis
from File import File
from PandasModel import PandasModel


class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("DataViz")

        # Menubar 1
        menu1 = self.menuBar().addMenu('Fichier')
        action1 = QAction('Nouveau projet', self)
        action2 = QAction('Ouvrir', self)
        action3 = QAction('Quitter', self)
        menu1.addAction(action1)
        menu1.addAction(action2)
        menu1.addAction(action3)
        action2.triggered.connect(self.on_ouvrir)
        action3.triggered.connect(self.on_quitter)

        # Menubar 2
        menu1 = self.menuBar().addMenu('Data Analyse')
        action1 = QAction('Joindre table', self)
        action2 = QAction('Ouvrir', self)
        action3 = QAction('Quitter', self)
        menu1.addAction(action1)
        menu1.addAction(action2)
        menu1.addAction(action3)
        action2.triggered.connect(self.on_ouvrir)
        action3.triggered.connect(self.on_quitter)

        self.centralWidget = MainWidget(self)
        self.centralWidget.tracer()
        self.centralWidget.tracer2()
        self.centralWidget.tracer3()
        self.centralWidget.tracer4()
        self.setCentralWidget(self.centralWidget)

    def on_ouvrir(self):
        fileName = QFileDialog.getOpenFileName(self, "Ouvrir", "", "Csv Files (*.csv)")
        if fileName == ('', ''):
            return
        i = 0
        x = 0
        while x != "/":
            i += 1
            x = str(fileName[0])[-i]
        file = File(fileName[0])
        self.centralWidget.add(fileName[0])
        self.centralWidget.combois(file.df.columns.values.tolist())
        self.centralWidget.table.setModel(PandasModel(file.df))

    def on_quitter(self):
        self.quit()


class MainWidget(QWidget):
    def __init__(self, parent):
        super().__init__(parent)
        self.test = File("Clients_4.csv")
        self.test.cleaning()
        self.anal = Analysis(self.test)
        label1 = QLabel('Choix de la data')
        button = QPushButton('Ajouter au dashboard')
        label2 = QLabel('Choix du rendu')
        cbox2 = QComboBox()
        cbox2.addItem('Effectif')
        label3 = QLabel('')

        self.modelcb = QStringListModel()
        # Création de la combobox
        self.cbox3 = QComboBox()
        self.cbox3.setModel(self.modelcb)

        label4 = QLabel('Choix du canvas')
        cbox4 = QComboBox()
        cbox4.addItem('1')
        cbox4.addItem('2')
        cbox4.addItem('3')
        cbox4.addItem('4')
        self.figure1 = plt.figure(1)
        self.figure2 = plt.figure(2)
        self.figure3 = plt.figure(3)
        self.figure4 = plt.figure(4)
        self.canvas = FigureCanvas(self.figure1)
        self.canvas2 = FigureCanvas(self.figure2)
        self.canvas3 = FigureCanvas(self.figure3)
        self.canvas4 = FigureCanvas(self.figure4)
        self.table = QTableView()
        self.list = QListView()
        self.model = QStringListModel()
        self.list.setModel(self.model)
        layout = QGridLayout()
        sublayout = QGridLayout()
        sublayout.addWidget(label1, 0, 0)
        sublayout.addWidget(label2, 2, 0)
        sublayout.addWidget(cbox2, 3, 0)
        sublayout.addWidget(self.list, 0, 1)
        sublayout.addWidget(self.cbox3, 1, 1)
        sublayout.addWidget(label4, 2, 1)
        sublayout.addWidget(cbox4, 3, 1)
        sublayout.addWidget(button, 4, 1)
        group = QGroupBox("Option d'ajout")
        group.setLayout(sublayout)
        layout.addWidget(group, 0, 0)
        layout.addWidget(self.table, 1, 0)
        layout.addWidget(self.canvas, 0, 1)
        layout.addWidget(self.canvas2, 1, 1)
        layout.addWidget(self.canvas3, 0, 2)
        layout.addWidget(self.canvas4, 1, 2)
        self.setLayout(layout)

    def add(self, truc):
        self.model.setStringList(self.model.stringList() + [truc])

    def combois(self, truc):
        self.modelcb.setStringList(truc)


    def tracer(self):
        plt.figure(1)
        self.test.df['nbEnfantsAcharge'].value_counts().sort_index().plot(kind='bar', width=1, edgecolor='black')
        self.canvas.draw()

    def tracer2(self):
        plt.figure(2)
        self.test.df['situationFamiliale'].value_counts().sort_index().plot(kind='bar', width=1, edgecolor='black')
        self.canvas2.draw()

    def tracer3(self):
        plt.figure(3)
        self.test.df['sexe'].value_counts().sort_index().plot(kind='bar', width=1, edgecolor='black')
        self.canvas3.draw()

    def tracer4(self):
        plt.figure(4)
        self.test.df['age'].value_counts().sort_index().plot(kind='bar', width=1, edgecolor='black')
        self.canvas4.draw()


def main():
    app = QApplication(sys.argv)
    window = MainWindow()
    window.show()
    app.exec()


if __name__ == '__main__':
    main()
