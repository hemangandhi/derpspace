package players;

public interface LaserHandler {

	boolean canStart();
	LaserDirection emit(LaserDirection in);
}
