package othello_1;

import java.awt.*;

public interface UIHandler<T> {

	public Color getDefaultColor();
	public Color getTurn();
	public void updateTurn(int layer);
	public T [][] getGrid();
}
