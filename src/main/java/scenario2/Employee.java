package scenario2;

import java.util.List;
import java.util.Optional;

public class Employee {
    public List<String> getAllEmployeeNames(){return null;}

    public String getNameOfEmp(int id){ return someFunc(id);}

    private String someFunc(int id) {
        return null;
    }

    public Optional<String> getEmpWithName(String s){
        return getAllEmployeeNames().stream().filter(x->x.equals(s)).findFirst();
    }
}
