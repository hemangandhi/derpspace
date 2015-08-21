package canvas;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;

public class Canvas extends JPanel implements MouseListener, Runnable, MouseMotionListener{

	private ArrayList<Point> points;
	private boolean isPressed;
	
	public Canvas(){
		super();
		isPressed = false;
		points = new ArrayList<>();
		setOpaque(true);
	}
	
	public void paintComponent(Graphics g){
		super.paintComponent(g);
		setBackground(Color.WHITE);
		g.setColor(Color.BLACK);
		for(int i = 0; i < points.size(); i++)
			g.fillOval(points.get(i).x, points.get(i).y, 5, 5);
	}
	
	public void run(){
		while(true){
			if(isPressed)
				points.add(MouseInfo.getPointerInfo().getLocation());
			repaint();
			try{
				Thread.sleep(1);
			}catch(InterruptedException ise){
				
			}
		}
	}

	@Override
	public void mouseClicked(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
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
		isPressed = true;
	}

	@Override
	public void mouseReleased(MouseEvent arg0) {
		isPressed = false;
		
	}

	@Override
	public void mouseDragged(MouseEvent arg0) {
		points.add(new Point(arg0.getX(),arg0.getY()));
		
	}

	@Override
	public void mouseMoved(MouseEvent arg0) {
		points.add(new Point(arg0.getX(),arg0.getY()));
		
	}
	
	
}
