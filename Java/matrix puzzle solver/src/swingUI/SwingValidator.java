package swingUI;

import java.awt.event.MouseListener;
import general.*;
import javax.swing.*;

public interface SwingValidator<T> extends Validator<T>, MouseListener{

	public void render(JLabel lbl, T state);
	public String title();
	public boolean isValidDim(int x, int y);
	//public boolean isValidState(T state);
	public T labelToState(JLabel lbl);
}
