package scenario2;

import java.util.List;

/**
 * Created by ameya on 5/21/18.
 */
public class ExecutiveStaff extends Staffer {

    @Override
    public List<String> getAllStafferNames() {
        return getAllExecutiveStafferNames();
    }

    public List<String> getAllExecutiveStafferNames() {
        return null;
    }
}
