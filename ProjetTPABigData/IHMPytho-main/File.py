import pandas


class File:

    marques = ['Audi', 'BMW', 'Dacia', 'Daihatsu', 'Fiat', 'Ford', 'Honda', 'Hyundaï',
               'Jaguar', 'Kia', 'Lancia', 'Mercedes', 'Mini', 'Nissan', 'Peugeot', 'Renault',
               'Saab', 'Seat', 'Skoda', 'Volkswagen', 'Volvo']
    longueurs = ['courte', 'moyenne', 'longue', 'très longue']
    couleurs = ['blanc', 'bleu', 'gris', 'noir', 'rouge']

    def __init__(self, file):
        self.df = pandas.read_csv(file, encoding="ISO-8859-1")

    def cleaning(self):
        self.df.dropna(inplace=True)
        for col in self.df.columns:
            if col == 'age':
                self.df['age'] = pandas.to_numeric(self.df['age'], errors="coerce", downcast="integer")
                self.df = self.df[(self.df['age'] >= 18) & (self.df['age'] <= 84)]
            elif col == 'sexe':
                self.df = self.df[self.df['sexe'].isin(['M', 'F'])]
            elif col == 'situationFamiliale':
                self.df = self.df[self.df['situationFamiliale'].isin(['Célibataire', 'En Couple'])]
            elif col == 'taux':
                self.df['taux'] = pandas.to_numeric(self.df['taux'], errors="coerce", downcast="integer")
                self.df = self.df[(self.df['taux'] >= 544) & (self.df['taux'] <= 74185)]
            elif col == 'nbEnfantsAcharge':
                self.df['nbEnfantsAcharge'] = pandas.to_numeric(self.df['nbEnfantsAcharge'], errors="coerce", downcast="integer")
                self.df = self.df[(self.df['nbEnfantsAcharge'] >= 0) & (self.df['nbEnfantsAcharge'] <= 4)]
            elif col == 'Immatriculation':
                self.df['Immatriculation'] = self.df['Immatriculation'].apply(lambda x: x.strip())
                self.df = self.df[self.df['Immatriculation'].str.match(r'^\d{4} [A-Z]{2} \d{2}$')]
            elif col == 'marque':
                self.df = self.df[self.df['marque'].isin(self.marques)]
            elif col == 'puissance':
                self.df['puissance'] = pandas.to_numeric(self.df['puissance'], errors="coerce", downcast="integer")
                self.df = self.df[(self.df['puissance'] >= 55) & (self.df['puissance'] <= 507)]
            elif col == 'longueur':
                self.df = self.df[self.df['longueur'].isin(self.longueurs)]
            elif col == 'nbPlaces':
                self.df['nbPlaces'] = pandas.to_numeric(self.df['nbPlaces'], errors="coerce", downcast="integer")
                self.df = self.df[(self.df['nbPlaces'] >= 5) & (self.df['nbPlaces'] <= 7)]
            elif col == 'nbPortes':
                self.df['nbPortes'] = pandas.to_numeric(self.df['nbPortes'], errors="coerce", downcast="integer")
                self.df = self.df[(self.df['nbPortes'] >= 3) & (self.df['nbPortes'] <= 5)]
            elif col == 'couleur':
                self.df = self.df[self.df['couleur'].isin(self.couleurs)]
            elif col == 'prix':
                self.df['prix'] = pandas.to_numeric(self.df['prix'], errors="coerce", downcast="integer")
                self.df = self.df[(self.df['prix'] >= 7500) & (self.df['prix'] <= 101300)]
            self.df = self.df.reset_index(drop=True)


