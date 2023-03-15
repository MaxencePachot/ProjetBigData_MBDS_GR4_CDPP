public class Marketing {
    int age;
    String sexe;
    double taux;
    String fam;
    int nbenfant;
    boolean ndvoiture;

    public int getAge() {
        return this.age;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public String getSexe() {
        return this.sexe;
    }

    public void setSexe(String sexe) {
        this.sexe = sexe;
    }

    public double getTaux() {
        return this.taux;
    }

    public void setTaux(double taux) {
        this.taux = taux;
    }

    public String getFam() {
        return this.fam;
    }

    public void setFam(String fam) {
        this.fam = fam;
    }

    public int getNbenfant() {
        return this.nbenfant;
    }

    public void setNbenfant(int nbenfant) {
        this.nbenfant = nbenfant;
    }

    public boolean isNdvoiture() {
        return this.ndvoiture;
    }

    public void setNdvoiture(boolean ndvoiture) {
        this.ndvoiture = ndvoiture;
    }    

    public boolean check(){
        if(this.age>0 && 
          (this.sexe=="f" ||
           this.sexe=="m")&&
          (this.fam=="celibataire" ||
           this.fam=="en couple")&&
           this.nbenfant>=0){
            return true;
        }
        else{
            return false;
        }
    }

}
