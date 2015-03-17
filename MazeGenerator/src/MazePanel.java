import javax.swing.*;
import java.awt.*;

public class MazePanel extends JPanel {

	public void paintComponent(Graphics g){
		final int X_OFFSET = 100;
		final int Y_OFFSET = 100;
		MazeNode[][] maze = (new MazeGrid(3,3)).getMaze();
		for(MazeNode[] row: maze){
			for(MazeNode node: row){
				for(MazeNode other: node.connecteds()){
					g.drawLine((node.getX() + 1)*X_OFFSET,
							(node.getY() + 1)*Y_OFFSET,
							(other.getX() + 1)*X_OFFSET,(other.getY() + 1)*Y_OFFSET);
				}
			}
		}
	}
	
	public static void main(String [] args){
		JFrame window = new JFrame("Maze!");
		window.setSize(750, 750);
		window.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		window.add(new MazePanel());
		window.setVisible(true);
	}
}
