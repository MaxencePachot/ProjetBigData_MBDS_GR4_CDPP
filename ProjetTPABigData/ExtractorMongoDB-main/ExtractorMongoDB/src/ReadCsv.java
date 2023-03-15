import java.io.*;

public class ReadCsv {
    public void readClient(String fileName) {
        String line = "";
        String splitBy = ",";
        MongoAdd MA = new MongoAdd();
        Client client = new Client(); 
        Transcription transcript = new Transcription();
        try {
            FileWriter myWriter = new FileWriter("rejet.txt");
            myWriter.write("");
            myWriter.close();
        } catch (IOException e) {            
            e.printStackTrace();
        }
        
        transcript.mapping();        
        try {
            BufferedReader br = new BufferedReader(new FileReader(fileName+".csv"));
            while ((line = br.readLine()) != null) 
            {
                String[] listClient = line.split(splitBy); 
                try {
                    client.age = Integer.parseInt(listClient[0]);
                    client.sexe = transcript.transcr(listClient[1]);
                    client.taux = Integer.parseInt(listClient[2]);
                    client.fam = transcript.transcr(listClient[3]);
                    client.nbenfant = Integer.parseInt(listClient[4]);
                    client.ndvoiture = Boolean.parseBoolean(listClient[5]);
                    client.immatriculation = listClient[6].replaceAll(" ", "");                        
                    MA.addClient(client, fileName);
                } catch (Exception e) {
                    PrintWriter pw = new PrintWriter(new FileOutputStream("Log"));
                    e.printStackTrace(pw);
                }                          
                       
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
 
    public void readMarketing(String fileName) {
        String line = "";
        String splitBy = ",";
        MongoAdd MA = new MongoAdd();
        Marketing marketing = new Marketing();
        Transcription transcript = new Transcription();
        transcript.mapping(); 
        try {
            BufferedReader br = new BufferedReader(new FileReader(fileName+".csv"));
            while ((line = br.readLine()) != null) 
            {
                String[] listClient = line.split(splitBy); 
                try {
                    marketing.age = Integer.parseInt(listClient[0]);
                    marketing.sexe = transcript.transcr(listClient[1]);
                    marketing.taux = Integer.parseInt(listClient[2]);
                    marketing.fam = transcript.transcr(listClient[3]);
                    marketing.nbenfant = Integer.parseInt(listClient[4]);
                    marketing.ndvoiture = Boolean.parseBoolean(listClient[5]);                       
                    MA.addMarketing(marketing, fileName);
                } catch (Exception e) {
                    PrintWriter pw = new PrintWriter(new FileOutputStream("Log"));
                    e.printStackTrace(pw);
                }                          
                       
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void readCatalogue(String fileName) {
        String line = "";
        String splitBy = ",";
        MongoAdd MA = new MongoAdd();
        Catalogue catalogue = new Catalogue();
        try {
            BufferedReader br = new BufferedReader(new FileReader(fileName+".csv"));
            while ((line = br.readLine()) != null) 
            {
                String[] listClient = line.split(splitBy); 
                try {
                    catalogue.marque = listClient[0];
                    catalogue.nom = listClient[1];
                    catalogue.puissance = Integer.parseInt(listClient[2]);
                    catalogue.longueur = listClient[3];
                    catalogue.nbPlaces = Integer.parseInt(listClient[4]);
                    catalogue.nbPortes = Integer.parseInt(listClient[5]);
                    catalogue.couleur = listClient[6];  
                    catalogue.occasion = Boolean.parseBoolean(listClient[7]);
                    catalogue.prix = Integer.parseInt(listClient[8]);                     
                    MA.addCatalogue(catalogue, fileName);
                } catch (Exception e) {
                    PrintWriter pw = new PrintWriter(new FileOutputStream("Log"));
                    e.printStackTrace(pw);
                }                          
                       
            }
            br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
