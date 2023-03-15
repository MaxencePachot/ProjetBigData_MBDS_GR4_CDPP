package org.mbds.hadoop.projetBigData;

import java.util.Iterator;
import java.io.IOException;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.util.GenericOptionsParser;

// Notre classe Driver (contient le main du programme Hadoop).
public class CO2
{	
    // La classe MAP
    public static class CO2Map extends Mapper<Object, Text, Text, Text> {
        // la fonction map
        protected void map(Object key, Text value, Context context) throws IOException, InterruptedException {
            // Ne pas prendre en compte la ligne 1
            if (value.toString().contains("Marque")) {
                return;
            }

            // On passe la valeur en String pour pouvoir manipuler plus simplement les valeurs
            String valueString = value.toString();

            // On remplace les espaces insécables par de vrais espaces et on enlève les guillemets
            valueString = valueString.replaceAll("\\u00a0"," ");
            //Gestion exception Volkswagen
            valueString = valueString.replaceAll("\"","");

            // On sépare Marque/Modele de Bonus/Malus et ainsi de suite
            String[] tabValue = valueString.split(",");

            // POUR LA COLONNE MARQUE
            String marque;
            String[] tabMarqueModeleSep = tabValue[1].split(" ");
            marque = tabMarqueModeleSep[0]; //Notre marque est le premier élément du sous tableau Marque Modele

            // Gestion des cas où la marque de voiture n'existe pas dans le catalogue du concessionnaire
            if (marque.contains("CITROEN") || marque.contains("DS") || marque.contains("SMART") || marque.contains("TESLA") || marque.contains("TOYOTA") || marque.contains("MITSUBISHI") || marque.contains("PORSCHE") || marque.contains("BENTLEY") || marque.contains("LAND")) {
                return;
            }

            // POUR LA COLONNE BONUS/MALUS
            String bonusMalus = tabValue[2];

            // Gestion cas particulier si "," dans la marque
            if((bonusMalus.equals(" 150kW (204ch)") || bonusMalus.equals(" 100kW (136ch)"))) {
                bonusMalus = tabValue[3];
            }

            bonusMalus = bonusMalus.replaceAll(" ","").replaceAll("€1","").replaceAll("€","");

            // Gestion exception Volkswagen
            tabValue[2] = tabValue[2].replaceAll("\"","");
            // Gestion des valeurs manquantes
            if(bonusMalus.equals("-")){
                bonusMalus = "0";
            }

            // POUR LA COLONNE REJET CO2
            String CO2 = tabValue[3];

            // Gestion cas particulier si "," dans la marque
            if((tabValue[2].equals(" 150kW (204ch)") || tabValue[2].equals(" 100kW (136ch)"))) {
                CO2 = tabValue[4];
            }

            // POUR LA COLONNE COUT EN ENERGIE
            String coutEnergie = tabValue[4];
            // Gestion cas particulier si "," dans la marque
            if((tabValue[2].equals(" 150kW (204ch)") || tabValue[2].equals(" 100kW (136ch)"))) {
                coutEnergie = tabValue[5];
            }
            coutEnergie = coutEnergie.replaceAll(" ","").replaceAll("€","");

            // Pour obtenir nos couples clés / valeurs de la fonction MAP
            String newValue = bonusMalus + "|" + CO2 + "|" + coutEnergie;
            context.write(new Text(marque), new Text(newValue));
        }
    }

    // La classe REDUCE
    public static class CO2Reduce extends Reducer<Text, Text, Text, Text> {
        // la fonction reduce
        int nbValueOther = 0;
        int avgBonusMalusOther = 0;
        int avgCO2Other = 0;
        int avgCoutEnergieOther = 0;
        protected void reduce(Text key, Iterable<Text> values, Context context) throws IOException, InterruptedException {

            // On initialise les variables dont on va avoir besoin
            String bonusMalus;
            String CO2;
            String coutEnergie;

            int nbValue = 0;

            int sumBonusMalus = 0;
            int sumCO2 = 0;
            int sumCoutEnergie =0;

            int avgBonusMalus = 0;
            int avgCO2 = 0;
            int avgCoutEnergie =0;

            nbValueOther++;

            // On parcourt les valeurs associées à chaque clé
            Iterator<Text> i = values.iterator();
			if(nbValueOther<14){
				while(i.hasNext()){
					nbValue++;
					// On récupère la valeur
					String valuesString = i.next().toString();
					// On affiche dans la console les couples clés / valeurs
					System.err.println("Key : " + key + " | Values : " + valuesString);
					// On sépare les valeurs
					String[] tabValues = valuesString.split("\\|");
					// On associe nos variables à leurs valeurs
					bonusMalus = tabValues[0];
					CO2 = tabValues[1];
					coutEnergie = tabValues[2];
					// On calcule les sommes
					sumBonusMalus += Integer.parseInt(bonusMalus);
					sumCO2 += Integer.parseInt(CO2);
					sumCoutEnergie += Integer.parseInt(coutEnergie);
				}
				// Calcul des moyennes de BonusMalus, CO2 et coutEnergie par marque
				avgBonusMalus = sumBonusMalus/nbValue;
				avgCO2 = sumCO2/nbValue;
				avgCoutEnergie = sumCoutEnergie/nbValue;
			}
            
			
			// On implémente la moyenne globale
			avgBonusMalusOther = avgBonusMalusOther + avgBonusMalus;
			avgCO2Other = avgCO2Other + avgCO2;
			avgCoutEnergieOther = avgCoutEnergieOther + avgCoutEnergie;
			

            // On envoie le résultat (selon la key)
            if(nbValueOther<14){
                context.write(key, new Text(avgBonusMalus + ";" + avgCO2 + ";" + avgCoutEnergie));
            }
			// Si la key est la dernière (ZZOTHER) correspondant à la variable qui sera attribuée aux véhicules du catalogue n'étant pas dans CO2.csv
            else{
				if(nbValueOther == 14){
					avgBonusMalusOther = (avgBonusMalusOther+avgBonusMalus)/(nbValueOther-1);
					avgCO2Other = (avgCO2Other+avgCO2)/(nbValueOther-1);
					avgCoutEnergieOther = (avgCoutEnergieOther+avgCoutEnergie)/(nbValueOther-1);
				}
                context.write(key, new Text(avgBonusMalusOther + ";" + avgCO2Other + ";" + avgCoutEnergieOther));
            }
        }
		
    }

    // Le main du programme
    // Le main du programme.
    public static void main(String[] args) throws Exception
    {
        // Créer un object de configuration Hadoop.
        Configuration conf=new Configuration();
        // Permet à Hadoop de lire ses arguments génériques, récupérer les arguments restants dans ourArgs.
        String[] ourArgs=new GenericOptionsParser(conf, args).getRemainingArgs();
        // Obtient un nouvel objet Job: une tâche Hadoop. On fourni la configuration Hadoop ainsi qu'une description
        // textuelle de la tâche.
		conf.set("mapreduce.output.textoutputformat.separator", ";");
        Job job=Job.getInstance(conf, "correction de CO2.csv");

        // Défini les classes driver, map et reduce.
        job.setJarByClass(CO2.class);
        job.setMapperClass(CO2Map.class);
        job.setReducerClass(CO2Reduce.class);

        // Défini types clefs/valeurs de notre programme Hadoop.
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(Text.class);

        // Défini les fichiers d'entrée du programme et le répertoire des résultats.
        // On se sert du premier et du deuxième argument restants pour permettre à l'utilisateur de les spécifier
        // lors de l'exécution.
        FileInputFormat.addInputPath(job, new Path(ourArgs[0]));
        FileOutputFormat.setOutputPath(job, new Path(ourArgs[1]));

        // On lance la tâche Hadoop. Si elle s'est effectuée correctement, on renvoie 0. Sinon, on renvoie -1.
        if(job.waitForCompletion(true))
            System.exit(0);
        System.exit(-1);
    }
}