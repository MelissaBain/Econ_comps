import java.util.*;
import java.lang.*;
import java.io.*;


class DistanceCalculatorKNeighbors{

	public static void main (String[] args) throws java.lang.Exception {
		long startTime = System.nanoTime();
		File houseInfo = new File("quarter_mile_buffer_javahomes.csv");
		List<Integer> neighbors = new ArrayList<Integer>();
		List<Float> aveSize = new ArrayList<Float>();
		List<Home> Minneapolis = new ArrayList<Home>();
		Double aveNeigh;
		Scanner fileInput;
		int ave;
		String[] homeArray = new String[5];
		Home houseData;
		Home neighborData;
		LinkedListKNeigh kInfo;
		int k = 1;
		Double neighDistance;
		KHomes curNeigh;
		int validHomes = 0;
		Double totalMaxDistance = 0.0;
		
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
				validHomes ++;
				kInfo = new LinkedListKNeigh(k+1);
				for(int curNeighbor=0; curNeighbor<Minneapolis.size();curNeighbor++){
					neighborData = Minneapolis.get(curNeighbor);
					neighDistance =distance(houseData.getLat(),houseData.getLon(), neighborData.getLat(),
						neighborData.getLon());
					curNeigh = new KHomes(neighDistance, neighborData.getSqrFeet());
					kInfo.add(curNeigh);	
				}
				totalMaxDistance += kInfo.getDist();
				houseData.setNeighSize(kInfo.getAve());
			}
		}
		PrintWriter writer = new PrintWriter("deviations_k_1.csv", "UTF-8"); //file writer
		for(Home house: Minneapolis){
			if(house.isValid()){
				writer.println(house.getID() + "," + house.getNeighSize());
		    }
		}
		writer.println("Ave max distance" + "," + (totalMaxDistance/validHomes));
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