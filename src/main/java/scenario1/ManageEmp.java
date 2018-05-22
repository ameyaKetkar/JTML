package scenario1;

public class ManageEmp {

    public void printAllNames(){
        Employee s = new Employee();
        s.getAllEmployeeNames().forEach(System.out::println);
    }


}
