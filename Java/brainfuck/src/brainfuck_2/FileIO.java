package brainfuck_2;

import java.io.*;

public interface FileIO {

	String readFile() throws IOException;
	
	void setFile(String path);
	
	void writeFile( String code) throws IOException;
}
