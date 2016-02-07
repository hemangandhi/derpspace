package general;

import java.util.Set;

public interface Validator<T> {

	public Set<T> filter(Set<T> moves, int x, int y, T[][] mat);
	public boolean validate(T[][] mat);
	public Set<T> getStates();
}
