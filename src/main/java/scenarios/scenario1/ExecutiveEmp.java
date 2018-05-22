package scenarios.scenario1;

import java.util.List;

/**
 * Created by ameya on 5/21/18.
 */
public class ExecutiveEmp extends Employee {

    @Override
    public List<String> getAllEmployeeNames() {
        return getAllExecutiveStafferNames();
    }

    public List<String> getAllExecutiveStafferNames() {
        return null;
    }
}
