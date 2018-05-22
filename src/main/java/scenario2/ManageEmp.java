package scenario2;

import java.util.Optional;

public class ManageEmp {
    Employee s = new Employee();
    public void printAllNames(){

        s.getAllEmployeeNames().forEach(System.out::println);
    }

    public void printName(int id){
        System.out.print(s.getNameOfEmp(id));
    }
    public void printName(long id){
        System.out.print(s.getNameOfEmp((int)id));
    }

    public void printStaffWithName(String s1){
        Optional<String>name = s.getEmpWithName(s1);
        System.out.println(name.orElse(null));
    }

}
