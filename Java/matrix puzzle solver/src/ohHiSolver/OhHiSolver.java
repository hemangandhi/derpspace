package ohHiSolver;

import java.awt.Color;
import java.awt.event.MouseEvent;
import java.util.HashSet;
import java.util.Set;
import javax.swing.JLabel;
import swingUI.SwingValidator;
import general.MatUtils;

public class OhHiSolver implements SwingValidator<CellState> {

	
	@Override
	public void mouseClicked(MouseEvent arg0) {
		// TODO Auto-generated method stub
		CellState s = labelToState((JLabel)arg0.getSource());
		if(s == CellState.RED)
			render((JLabel)arg0.getSource(), CellState.BLUE);
		else if(s == CellState.BLUE)
			render((JLabel)arg0.getSource(), null);
		else
			render((JLabel)arg0.getSource(), CellState.RED);
	}

	@Override
	public void mouseEntered(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	

	@Override
	public String title() {
		// TODO Auto-generated method stub
		return "0h h1 solver!";
	}

	@Override
	public boolean isValidDim(int x, int y) {
		// TODO Auto-generated method stub
		return x % 2 == 0 && y % 2 == 0;
	}

	@Override
	public Set<CellState> filter(Set<CellState> moves, int x, int y,
			CellState[][] mat) {
		// TODO Auto-generated method stub
		HashSet<CellState> r = new HashSet<CellState>();
		for(CellState cs: moves){
			mat[x][y] = cs;
			if(validate(mat))
				r.add(cs);
		}
		mat[x][y] = null;
		return r;
	}
	
	private boolean allRowsAndColsEven(CellState[][] mat){
		for(int i = 0; i < mat.length; i++){
			int r = MatUtils.countInRow(mat, CellState.RED, i);
			int b = MatUtils.countInRow(mat, CellState.BLUE, i);
			if(r > mat[0].length/2 || b > mat[0].length/2)
				return false;
		}
		
		for(int i = 0; i < mat[0].length; i++){
			int r = MatUtils.countInCol(mat, CellState.RED, i);
			int b = MatUtils.countInCol(mat, CellState.BLUE, i);
			if(r > mat[0].length/2 || b > mat[0].length/2)
				return false;
		}
		
		return true;
	}

	@Override
	public boolean validate(CellState[][] mat) {
		// TODO Auto-generated method stub
		return MatUtils.noColMatch(mat) && MatUtils.noRowMatch(mat)
				&& MatUtils.notNAdj(mat, 3, false)
				&& allRowsAndColsEven(mat);
	}

	@Override
	public void render(JLabel lbl, CellState state) {
		lbl.setOpaque(true);
		if (state == CellState.RED)
			lbl.setBackground(Color.RED);
		else if (state == CellState.BLUE)
			lbl.setBackground(Color.BLUE);
		else
			lbl.setBackground(Color.WHITE);
	}

	@Override
	public CellState labelToState(JLabel lbl) {
		// TODO Auto-generated method stub
		if(lbl.getBackground().equals(Color.RED))
			return CellState.RED;
		else if(lbl.getBackground().equals(Color.BLUE))
			return CellState.BLUE;
		else
			return null;
	}

	@Override
	public Set<CellState> getStates() {
		// TODO Auto-generated method stub
		HashSet<CellState> s = new HashSet<>();
		s.add(CellState.RED);
		s.add(CellState.BLUE);
		return s;
	}

}
