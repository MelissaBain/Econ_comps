class KHomes implements Comparable<KHomes>{
	private Double distance;
	private Double sqrFeet;
	private KHomes nextHouse = null;
	
	public KHomes(Double distance, Double sqrFeet){
		this.distance=distance;
		this.sqrFeet = sqrFeet;
	}
	
	public Double getSqrFeet(){
		return sqrFeet;
	}
	
	public Double getDistance(){
		return distance;
	}
	
	public int compareTo(KHomes house) {
      return (this.distance).compareTo(house.distance);
    }
    
    public void setNext(KHomes nextHouse){
    	this.nextHouse = nextHouse;
    }
    
    public KHomes getNext(){
    	return nextHouse;
    }
    
    public static void main(String[] args){
    	KHomes house1 = new KHomes(.4,2000.0);
    	KHomes house2 = new KHomes(.8,3000.0);
    	System.out.println(house2.compareTo(house1));
    }

}