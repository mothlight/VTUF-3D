import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Comparator;
import java.util.TreeMap;
import java.util.List;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.*;



import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;


public class Repage
{

  final static Charset ENCODING = StandardCharsets.UTF_8;

  public static void main(String[] args)
  {
    Repage repage = new Repage();
    String[] file = repage.process();
    for (String line : file)
    {
      if (line.equals("DELETEME"))
      {
      }
      else
      {
	System.out.println(line);
      }
    }
	  
  }

  public String[] process()
  {    
    String[] file=null;
    //String file = readLargerTextFileAlternate("TUFreg3D.f90");
    //System.out.println(file);

    try
    {
      file = readLines("TUFreg3D.f90");
    }
    catch (Exception e)
    {
      e.printStackTrace();
    }
    int count = 0;
    for (String line : file)
    {
      line = line.trim();
      if (line.startsWith("&") || line.startsWith("$") || line.startsWith("."))
      {
          String prevLine = file[count-1].trim();
	  String currLine = file[count].trim();
	  int strLength = currLine.length();
          currLine = currLine.substring(1,strLength);
	  currLine = currLine.trim();
	  file[count-1] = "DELETEME";
	  file[count] = "      " + prevLine + currLine;
	//System.out.println(line);
      }else if (line.length()>0 && Character.isDigit(line.charAt(0)) )
      {
	  file[count]=line;
      }
      //System.out.println(line);
      count++;
    }

    return file;
    
  }

   public String readLargerTextFileAlternate(String aFileName) 
   {	   
	   StringBuffer sb = new StringBuffer();
	    Path path = Paths.get(aFileName);
	    try (BufferedReader reader = Files.newBufferedReader(path, ENCODING))
	    {
	      String line = null;
	      while ((line = reader.readLine()) != null) 
	      {
	    	  sb.append(line + '\n');
	      }      
	    }
		catch (IOException e)
		{			
			e.printStackTrace();
		}
	    return sb.toString();
	  }


  public String[] readLines(String filename) throws IOException {
        FileReader fileReader = new FileReader(filename);
        BufferedReader bufferedReader = new BufferedReader(fileReader);
        List<String> lines = new ArrayList<String>();
        String line = null;
        while ((line = bufferedReader.readLine()) != null) {
            lines.add(line);
        }
        bufferedReader.close();
        return lines.toArray(new String[lines.size()]);
    }


}