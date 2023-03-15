import com.mongodb.MongoClient;
import com.mongodb.client.MongoDatabase;

public class App {
    public static void main(String[] args) throws Exception {
        MongoClient mongoClient = new MongoClient("localhost", 27017);
        MongoDatabase client = mongoClient.getDatabase("Client");
        MongoDatabase marketing = mongoClient.getDatabase("Marketing");
        MongoDatabase catalogue = mongoClient.getDatabase("Catalogue");
        client.drop();
        marketing.drop();
        catalogue.drop();
        client.createCollection("Clients_4");
        client.createCollection("Clients_12");
        marketing.createCollection("Marketing");
        catalogue.createCollection("Catalogue");
        mongoClient.close();        
        ReadCsv reader = new ReadCsv();
        reader.readClient("Clients_4");
        reader.readClient("Clients_12");
        reader.readMarketing("Marketing");
        reader.readCatalogue("Catalogue");
    }
}
