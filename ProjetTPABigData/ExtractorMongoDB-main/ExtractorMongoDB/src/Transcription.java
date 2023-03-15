import java.io.*;
import java.text.Normalizer;
import java.util.HashMap;
import java.util.Map;

public class Transcription {
    File file = new File("transcription.txt");
    String line = "";    
    Map<String, String[]> trancriptionTable = new HashMap<>(); 

    public void mapping(){
        try{
            BufferedReader br = new BufferedReader(new FileReader(file));
        while ((line = br.readLine()) != null){
            String[] tab1 = line.split(":");            
            String[] tab2 = tab1[1].split(",");
            trancriptionTable.put(tab1[0], tab2);
        }
        br.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String transcr(String input){       
        input = Normalizer.normalize(input, Normalizer.Form.NFD);
        input = input.replaceAll("[\\p{InCombiningDiacriticalMarks}]", "");        
        input = input.toLowerCase();        
        for (String key : trancriptionTable.keySet()) {
            for (String element : trancriptionTable.get(key)) {
                if (element.equals(input)) {
                    return key;
                }
            }
        }
        return input;
    }
}

