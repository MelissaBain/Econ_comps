/**
*The home class stores information about each house in the dataset. 
*/

class Home{
	private int ID;
	private Double lat;
	private Double lon; 
	private Double sqrFeet;
	private Double neighSize = 0.0;
	private boolean soldHalfMile;
	private int neighbors;

	Home(int ID, Double x, Double y, Double sqrFeet,boolean soldHalfMile){
		this.ID = ID;
		lat = y;
		lon = x;
		this.sqrFeet=sqrFeet;
		this.soldHalfMile = soldHalfMile;
	}
	
	public boolean isValid(){
		return soldHalfMile;
	}
	
	public Double getLat(){
		return lat;
	}
	
	public Double getLon(){
		return lon;
	}
	
	public void setNeighSize(Double size){
		neighSize = size;
	}
	
	public Double getSqrFeet(){
		return sqrFeet;
	}
	
	public Double getNeighSize(){
		return neighSize;
	}
	
	public int getID(){
		return ID;
	}
	
	public void setNeighbors(int neighbors){
		this.neighbors = neighbors;
	}
	
	public int getNeighbors(){
		return neighbors;
	}
}