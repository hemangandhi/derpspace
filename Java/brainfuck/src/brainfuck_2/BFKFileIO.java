package brainfuck_2;

import java.io.*;

public class BFKFileIO implements FileIO {

	private File f;
	
	@Override
	public String readFile() throws IOException {
		String toRet = "";
		DataInputStream in = new DataInputStream(new FileInputStream(f));
		while(in.available() > 0)
			toRet += in.readChar();
		in.close();
		return toRet;
	}
	
	public void setFile(String path){
		f = new File(path);
	}

	@Override
	public void writeFile(String code) throws IOException {
		DataOutputStream out = new DataOutputStream(new FileOutputStream(f));
		out.writeChars(code);
		out.close();
	}

}
