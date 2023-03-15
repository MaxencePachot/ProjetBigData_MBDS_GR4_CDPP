public class Client {
    int age;
    String sexe;
    double taux;
    String fam;
    int nbenfant;
    boolean ndvoiture;
    String immatriculation;

    public int getAge() {
        return age;
    }

    public String getSexe() {
        return sexe;
    }

    public void setSexe(String sexe) {
        this.sexe = sexe;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public double getTaux() {
        return taux;
    }

    public void setTaux(double taux) {
        this.taux = taux;
    }

    public String getFam() {
        return fam;
    }

    public void setFam(String fam) {
        this.fam = fam;
    }

    public int getNbenfant() {
        return nbenfant;
    }

    public void setNbenfant(int nbenfant) {
        this.nbenfant = nbenfant;
    }

    public boolean isNdvoiture() {
        return ndvoiture;
    }

    public void setNdvoiture(boolean ndvoiture) {
        this.ndvoiture = ndvoiture;
    }

    public String getImmatriculation() {
        return immatriculation;
    }

    public void setImmatriculation(String immatriculation) {
        this.immatriculation = immatriculation;
    }

    public boolean check(){
        if(this.age>0 && 
          ("f".equals(this.sexe) ||
           "m".equals(this.sexe))&&
          ("Celibataire".equals(this.fam) ||
          "En Couple".equals(this.fam))&&
           this.taux>=0 &&
           this.nbenfant>=0){
           return true;
        }
        else{
            return false;
        }
    }
}
