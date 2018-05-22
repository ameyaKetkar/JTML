package scenario1;

public class Manage {

    public void printAllNames(){
        Staffer s = new Staffer();
        s.getAllStafferNames().forEach(System.out::println);
    }


}
