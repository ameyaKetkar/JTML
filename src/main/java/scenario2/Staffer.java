package scenario2;

import java.util.List;

public class Staffer {
    public List<String> getAllStafferNames(){return null;}

    public String getNameOfStaff(long id){ return someFunc(id);}

    private String someFunc(long id) {
        return null;
    }

    public String getStaffWithName(String s){
        for(String s1 : getAllStafferNames())
            if(s1.equals(s))
                return s1;
        return null;
    }
}
