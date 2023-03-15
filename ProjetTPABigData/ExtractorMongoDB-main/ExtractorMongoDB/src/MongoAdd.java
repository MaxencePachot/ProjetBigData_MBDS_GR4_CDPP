import org.bson.Document;
import com.mongodb.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;

public class MongoAdd {
    public void addClient(Client clientToAdd, String fileName) {
        MongoClient mongoClient = new MongoClient("localhost", 27017);
        MongoDatabase client = mongoClient.getDatabase("Client");        
        MongoCollection<Document> client12 = client.getCollection(fileName);
        client12.insertOne(new Document()                
                .append("Age", clientToAdd.age)
                .append("Sexe", clientToAdd.sexe)
                .append("Taux", clientToAdd.taux)
                .append("Situation Familliale", clientToAdd.fam)
                .append("Nombre d'enfant", clientToAdd.nbenfant)                
                .append("2nd voiture", clientToAdd.ndvoiture)
                .append("immatriculation", clientToAdd.immatriculation));
        mongoClient.close();
    }

    public void addMarketing(Marketing marketingToAdd, String fileName) {
        MongoClient mongoClient = new MongoClient("localhost", 27017);
        MongoDatabase client = mongoClient.getDatabase("Marketing");
        MongoCollection<Document> marketing = client.getCollection(fileName);
        marketing.insertOne(new Document()                
                .append("Age", marketingToAdd.age)
                .append("Sexe", marketingToAdd.sexe)
                .append("Taux", marketingToAdd.taux)
                .append("Situation Familliale", marketingToAdd.fam)
                .append("Nombre d'enfant", marketingToAdd.nbenfant)                
                .append("2nd voiture", marketingToAdd.ndvoiture));
        mongoClient.close();
    }

    public void addCatalogue(Catalogue catalogueToAdd, String fileName) {
        MongoClient mongoClient = new MongoClient("localhost", 27017);
        MongoDatabase client = mongoClient.getDatabase("Catalogue");        
        MongoCollection<Document> catalogue = client.getCollection(fileName);
        catalogue.insertOne(new Document()                
                .append("Marque", catalogueToAdd.marque)
                .append("Nom", catalogueToAdd.nom)
                .append("Puissance", catalogueToAdd.puissance)
                .append("Longueur", catalogueToAdd.longueur)
                .append("Nb Places", catalogueToAdd.nbPlaces)                
                .append("Nb Portes", catalogueToAdd.nbPortes)
                .append("Couleur", catalogueToAdd.couleur)
                .append("Occasion", catalogueToAdd.occasion)
                .append("Prix", catalogueToAdd.prix));
        mongoClient.close();
    }
}
