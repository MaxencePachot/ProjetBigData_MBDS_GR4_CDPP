package concessionnaire;

import oracle.kv.KVStore;
import java.util.List;
import java.util.Iterator;
import oracle.kv.KVStoreConfig;
import oracle.kv.KVStoreFactory;
import oracle.kv.FaultException;
import oracle.kv.StatementResult;
import oracle.kv.table.TableAPI;
import oracle.kv.table.Table;
import oracle.kv.table.Row;
import oracle.kv.table.PrimaryKey;
import oracle.kv.ConsistencyException;
import oracle.kv.RequestTimeoutException;
import java.lang.Integer;
import oracle.kv.table.TableIterator;
import oracle.kv.table.EnumValue;
import java.io.File;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;

import java.util.StringTokenizer;
import java.util.ArrayList;
import java.util.List;
/**
 * Cette classe fournit les fonctions nécessaires pour gérer les tables.
 * La fonction void executeDDL(String statement) reçoit en paramètre
 * une commande ddl et l'applique dans la base nosql.
 * La displayResult affiche l'état de l'exécution de la commande
 * la fonction createTableCritere permet de créer une table critère>.
 */

public class Concessionnaire {
    private final KVStore store;
    private final String tabImmatriculations="IMMATRICULATIONS_ESTIA2223_PACHOT";

    /**
     * Runs the DDL command line program.
     */
    public static void main(String args[]) {
        try {
            Concessionnaire ccn= new Concessionnaire(args);
            ccn.initConcessionnaireTablesAndData(ccn);

        } catch (RuntimeException e) {
            e.printStackTrace();
        }
    }
    /**
     * Parses command line args and opens the KVStore.
     */
    Concessionnaire(String[] argv) {

        String storeName = "kvstore";
        String hostName = "localhost";
        String hostPort = "5000";

        final int nArgs = argv.length;
        int argc = 0;
        store = KVStoreFactory.getStore
                (new KVStoreConfig(storeName, hostName + ":" + hostPort));
    }


    /**
     * Affichage du résultat pour les commandes DDL (CREATE, ALTER, DROP)
     */

    private void displayResult(StatementResult result, String statement) {
        System.out.println("===========================");
        if (result.isSuccessful()) {
            System.out.println("Statement was successful:\n\t" +
                    statement);
            System.out.println("Results:\n\t" + result.getInfo());
        } else if (result.isCancelled()) {
            System.out.println("Statement was cancelled:\n\t" +
                    statement);
        } else {
            /*
             * statement was not successful: may be in error, or may still
             * be in progress.
             */
            if (result.isDone()) {
                System.out.println("Statement failed:\n\t" + statement);
                System.out.println("Problem:\n\t" +
                        result.getErrorMessage());
            }
            else {

                System.out.println("Statement in progress:\n\t" +
                        statement);
                System.out.println("Status:\n\t" + result.getInfo());
            }
        }
    }

	/*
		La méthode initAirbaseTablesAndData permet :
		- de supprimer les tables si elles existent
		- de créer des tables
		- Insérer des critères
		- et charger les clients et les appréciations
	**/

    public void initConcessionnaireTablesAndData(Concessionnaire ccn) {
        ccn.dropTableImmatriculations();
        ccn.createTableImmatriculations();
        ccn.loadImmatriculationsDataFromFile("Immatriculations.csv");
    }

    /**
     public void dropTableCriteres()
     M&thode de suppression de la table criteres.
     */
    public void dropTableImmatriculations() {
        String statement = null;

        statement ="drop table "+tabImmatriculations;
        executeDDL(statement);
    }

    /**
     public void createTableClient()
     M&thode de création de la table client.
     +"longueur ENUM (courte,moyenne,longue,treslongue),"
     +"couleur ENUM (blanc, bleu, gris, noir, rouge),"
     */

    public void createTableImmatriculations() {
        String statement = null;
        statement="create table "+tabImmatriculations+" ("
                +"immatriculation STRING,"
                +"marque STRING,"
                +"nom STRING,"
                +"puissance INTEGER,"
                +"longueur STRING,"
                +"nbplaces INTEGER,"
                +"nbportes INTEGER,"
                +"couleur STRING,"
                +"occasion BOOLEAN,"
                +"prix INTEGER,"
                +"PRIMARY KEY(immatriculation))";
        executeDDL(statement);

    }
    /**
     public void executeDDL(String statement)
     méthode générique pour executer les commandes DDL
     */
    public void executeDDL(String statement) {
        TableAPI tableAPI = store.getTableAPI();
        StatementResult result = null;

        System.out.println("****** Dans : executeDDL ********" );
        try {
            /*
             * Add a table to the database.
             * Execute this statement asynchronously.
             */

            result = store.executeSync(statement);
            displayResult(result, statement);

        } catch (IllegalArgumentException e) {
            System.out.println("Invalid statement:\n" + e.getMessage());
        } catch (FaultException e) {
            System.out.println("Statement couldn't be executed, please retry: " + e);
        }
    }

    /**
     private void insertAClientRow(String immatriculation, String marque, String nom, int puissance, String longueur, int nbplaces,
     int nbportes, String couleur, boolean occasion, int prix)

     Cette méthode insère une nouvelle ligne dans la table CLIENT
     */

    private void insertImmatriculationsRow(String immatriculation, String marque, String nom, int puissance, String longueur, int nbplaces,
                                  int nbportes, String couleur, boolean occasion, int prix){
        //TableAPI tableAPI = store.getTableAPI();
        StatementResult result = null;
        String statement = null;
        System.out.println("********************************** Dans : insertImmatriculationsRow *********************************" );

        try {
            TableAPI tableH = store.getTableAPI();
            // The name you give to getTable() must be identical
            // to the name that you gave the table when you created
            // the table using the CREATE TABLE DDL statement.
            Table tableImmatriculations = tableH.getTable(tabImmatriculations);

            // Get a Row instance
            Row immatriculationsRow = tableImmatriculations.createRow();
            // Now put all of the cells in the row.
            // This does NOT actually write the data to
            // the store.

            // Create one row
            immatriculationsRow.put("immatriculation", immatriculation);
            immatriculationsRow.put("marque", marque);
            immatriculationsRow.put("nom", nom);
            immatriculationsRow.put("puissance", puissance);
            immatriculationsRow.put("longueur", longueur);
            immatriculationsRow.put("nbplaces", nbplaces);
            immatriculationsRow.put("nbportes", nbportes);
            immatriculationsRow.put("couleur", couleur);
            immatriculationsRow.put("occasion", occasion);
            immatriculationsRow.put("prix", prix);

            // Now write the table to the store.
            // "item" is the row's primary key. If we had not set that value,
            // this operation will throw an IllegalArgumentException.
            tableH.put(immatriculationsRow, null, null);

        }
        catch (IllegalArgumentException e) {
            System.out.println("Invalid statement:\n" + e.getMessage());
        }
        catch (FaultException e) {
            System.out.println("Statement couldn't be executed, please retry: " + e);
        }

    }

    /**
     void loadImmatriculationsDataFromFile(String immatriculationsDataFileName)
     cette methodes permet de charger les immatriculations depuis le fichier
     appelé Immatriculations.csv.
     Pour chaque immatriculation chargée, la
     méthode insertImmatriculationsRow sera appelée
     */
    void loadImmatriculationsDataFromFile(String immatriculationsDataFileName){
        InputStreamReader 	ipsr;
        BufferedReader 		br=null;
        InputStream 		ips;

        // Variables pour stocker les données les d'un fichier.
        String 		ligne;
        System.out.println("********************************** Dans : loadImmatriculationsDataFromFile *********************************" );

        /* parcourir les lignes du fichier texte et découper chaque ligne */
        try {
            ips  = new FileInputStream(immatriculationsDataFileName);
            ipsr = new InputStreamReader(ips);
            br   = new BufferedReader(ipsr);

            /* open csv file to read data */

            //parcourir le fichier ligne par ligne et découper chaque ligne en
            //morceau séparés par le symbole ,
            br.readLine(); //consume first line and ignore
            while ((ligne = br.readLine()) != null) {

                ArrayList<String> immatriculationsRecord= new ArrayList<String>();
                StringTokenizer val = new StringTokenizer(ligne,",");

                while(val.hasMoreTokens()) {
                    immatriculationsRecord.add(val.nextToken().toString());
                }
                String immatriculation	= immatriculationsRecord.get(0);
				immatriculation = immatriculation.replaceAll("\\s+","");
                String marque		    = immatriculationsRecord.get(1);
                String nom		        = immatriculationsRecord.get(2);
                int puissance	        = Integer.parseInt(immatriculationsRecord.get(3));
                String longueur		    = immatriculationsRecord.get(4);
                int nbplaces		    = Integer.parseInt(immatriculationsRecord.get(5));
                int nbportes	        = Integer.parseInt(immatriculationsRecord.get(6));
                String couleur	        = immatriculationsRecord.get(7);
                boolean occasion	    = Boolean.parseBoolean(immatriculationsRecord.get(8));
                int prix	            = Integer.parseInt(immatriculationsRecord.get(9));

                System.out.println("immatriculation="+immatriculation
                        +" marque="+marque
                        +" nom="+nom
                        +" puissance="+puissance
                        +" longueur="+longueur
                        +" nbplaces="+nbplaces
                        +" nbportes="+nbportes
                        +" couleur="+couleur
                        +" occasion=" +occasion
                        +" prix="+prix);
                // Add the client in the KVStore
                this.insertImmatriculationsRow(immatriculation, marque, nom, puissance, longueur, nbplaces, nbportes, couleur, occasion, prix);
            }
        }
        catch(Exception e){
            e.printStackTrace();
        }
    }
    /**
     private void displayClientRow (Row clientRow)
     Cette méthode d'afficher une ligne de la table client.
     */
    private void displayImmatriculationsRow (Row immatriculationRow) {
        System.out.println("========== DANS : displayClientRow =================");
        //System.out.println("===========================");
        String immatriculation = immatriculationRow.get("immatriculation").asString().get();
        String marque = immatriculationRow.get("marque").asString().get();
        String nom = immatriculationRow.get("nom").asString().get();
        int puissance = immatriculationRow.get("puissance").asInteger().get();
        String longueur = immatriculationRow.get("longueur").asString().get();
        int nbplaces = immatriculationRow.get("nbplaces").asInteger().get();
        int nbportes = immatriculationRow.get("nbportes").asInteger().get();
        String couleur = immatriculationRow.get("couleur").asString().get();
        String occasion = immatriculationRow.get("occasion").asString().get();
        int prix = immatriculationRow.get("prix").asInteger().get();

        System.out.println(" Immatriculations row :{ immatriculation=" + immatriculation +
                " marque="+marque +" nom="+nom+" puissance="+puissance+" longueur="+longueur+" nbplaces="+nbplaces
                +" nbportes="+nbportes+" couleur="+couleur+" occasion="+occasion+" prix="+prix+"}");
    }
}

