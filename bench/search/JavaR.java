import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.CharBuffer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


@SuppressWarnings("unused")
public class JavaR {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		Pattern patt = Pattern.compile(args[0]);

		File file = new File(args[1]);
		char[] data = new char[(int)file.length()];
		int cnt=0;
		try(BufferedReader fin = new BufferedReader(new FileReader(file))){
			fin.read(data,0, (int)file.length());
			long start = System.nanoTime();
			Matcher matcher = patt.matcher(CharBuffer.wrap(data));
			while(matcher.find())
			{
				cnt++;
			}
			double total = (System.nanoTime()-start)/1000000000.0;
			System.out.println("Total matches: "+cnt);
			System.out.println("Time elapsced: "+total);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	
	}

}

