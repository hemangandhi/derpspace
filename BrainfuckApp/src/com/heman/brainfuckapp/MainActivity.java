package com.heman.brainfuckapp;

import java.util.HashMap;

import android.support.v7.app.ActionBarActivity;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.EditText;

public class MainActivity extends ActionBarActivity implements BfkUIHandler{

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.main, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		// Handle action bar item clicks here. The action bar will
		// automatically handle clicks on the Home/Up button, so long
		// as you specify a parent activity in AndroidManifest.xml.
		int id = item.getItemId();
		if (id == R.id.action_settings) {
			return true;
		}
		return super.onOptionsItemSelected(item);
	}

	@Override
	public void output(int val) {
		// TODO Auto-generated method stub
		EditText output = (EditText) findViewById(R.id.output_field);
		if(((MenuItem)findViewById(R.id.IO_mode)).isChecked())
			output.setText(output.getText() + (val + ""));
		else
			output.setText(output.getText() + ((char)val + ""));
	}

	@Override
	public int getInput() {
		// TODO Auto-generated method stub
		return 0;
	}

	@Override
	public boolean debug(HashMap<Integer, Integer> tape, int pointer,
			int readIndex) {
		// TODO Auto-generated method stub
		return true;
	}

	@Override
	public void showError() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void endOfCode() {
		// TODO Auto-generated method stub
		
	}
}
