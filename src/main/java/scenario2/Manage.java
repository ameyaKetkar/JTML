package scenario2;

public class Manage {
    Staffer s = new Staffer();
    public void printAllNames(){
        s.getAllStafferNames().forEach(System.out::println);
    }

    public void printName(long id){
      System.out.print(s.getNameOfStaff(id));
    }

    public void printStaffWithName(String s1){
       String name = s.getStaffWithName(s1);
        System.out.println(name);
    }

}
