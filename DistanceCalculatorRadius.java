import java.util.*;
import java.lang.*;
import java.io.*;


public class DistanceCalculatorRadius {

	public static void main (String[] args) throws java.lang.Exception {
		long startTime = System.nanoTime();
		File houseInfo = new File("javahomes.csv");
		List<Integer> neighbors = new ArrayList<Integer>();
		List<Float> aveSize = new ArrayList<Float>();
		List<Home> Minneapolis = new ArrayList<Home>();
		Double aveNeigh;
		Scanner fileInput;
		int ave;
		String[] homeArray = new String[5];
		Home houseData;
		Home neighborData;
		
		try{
			fileInput = new Scanner(houseInfo);
			fileInput.nextLine();
			while(fileInput.hasNextLine()){
				homeArray = fileInput.nextLine().split(",");
				Home curHouse = new Home(Integer.parseInt(homeArray[0]),Double.parseDouble(homeArray[2]),
					Double.parseDouble(homeArray[3]),Double.parseDouble(homeArray[1]),
					Boolean.parseBoolean(homeArray[4]));
				Minneapolis.add(curHouse);				
			}
		}
		catch(FileNotFoundException e){
			System.out.println("Something happened");
		}
		for(int curHouse=0;curHouse<Minneapolis.size();curHouse++){
			houseData = Minneapolis.get(curHouse);
			if(houseData.isValid()){
				int totalSize = 0;
				int numNeighbors = 0;
				for(int curNeighbor=0; curNeighbor<Minneapolis.size();curNeighbor++){
					neighborData = Minneapolis.get(curNeighbor);
					if(distance(houseData.getLat(),houseData.getLon(), neighborData.getLat(),
						neighborData.getLon())<.5){
						totalSize += neighborData.getSqrFeet();
						numNeighbors += 1;
				  }
				}
				aveNeigh=(totalSize-houseData.getSqrFeet())/numNeighbors;
				houseData.setNeighSize(aveNeigh);
			}
		}
		PrintWriter writer = new PrintWriter("deviations_half_mile_all.csv", "UTF-8"); //file writer
		for(Home house: Minneapolis){
			if(house.isValid()){
				writer.println(house.getID() + "," + house.getNeighSize());
		    }
		}
		writer.close();
		long endTime = System.nanoTime();
		System.out.println((endTime - startTime)/1000000000);
	}

	private static double distance(double lat1, double lon1, double lat2, double lon2) {
		double theta = lon1 - lon2;
		double dist = Math.sin(deg2rad(lat1)) * Math.sin(deg2rad(lat2)) + Math.cos(deg2rad(lat1)) * Math.cos(deg2rad(lat2)) * Math.cos(deg2rad(theta));
		dist = Math.acos(dist);
		dist = rad2deg(dist);
		dist = dist * 60 * 1.1515;


		return (dist);
	}

	/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/*::	This function converts decimal degrees to radians						 :*/
	/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	private static double deg2rad(double deg) {
		return (deg * Math.PI / 180.0);
	}

	/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	/*::	This function converts radians to decimal degrees						 :*/
	/*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
	private static double rad2deg(double rad) {
		return (rad * 180 / Math.PI);
	}
}