class LinkedListKNeigh{
	int k;
	KHomes header;
	int size = 0;
	public LinkedListKNeigh(int k){
		this.k = k;
	}
	
	public void add(KHomes houseToAdd){
		KHomes curHouse;
		KHomes pastHouse;
		if(size==0){
			header = houseToAdd;
			size++;
		}
		else if (size<k){
			size++;
			curHouse = header;
			pastHouse=null;
			int counter = 0;
			while(curHouse!=null && houseToAdd.compareTo(curHouse)<0){
				counter++;
				pastHouse = curHouse;
				curHouse = curHouse.getNext();
			}
			houseToAdd.setNext(curHouse);
			if(counter ==0){
				header=houseToAdd;
			}else{
				pastHouse.setNext(houseToAdd);
			}
			
		}
		else if (houseToAdd.compareTo(header)<0) {
			if(houseToAdd.compareTo(header.getNext())<0){
				header=header.getNext();
				curHouse = header.getNext();
				pastHouse = header;
				while(curHouse!=null && houseToAdd.compareTo(curHouse)<0){
					pastHouse = curHouse;
					curHouse = curHouse.getNext();
				}
				houseToAdd.setNext(curHouse);
				pastHouse.setNext(houseToAdd);

			}
			else{
				houseToAdd.setNext(header.getNext());
				header=houseToAdd;
			}
		}
	}
	
	public void printList(){
		KHomes curHouse = header;
		while(curHouse!=null){
			System.out.println(curHouse.getDistance());
			curHouse = curHouse.getNext();
		}
		System.out.println();
	}
	    
	public Double getAve(){
    	Double totalSqrFt = 0.0;
    	KHomes curHouse = header;
    	for(int i=1; i<k; i++){
    		totalSqrFt+=curHouse.getSqrFeet();
			curHouse = curHouse.getNext();
    	} 
    	return totalSqrFt/(k-1);
    }
    
    public Double getDist(){
    	return header.getDistance();
    }
    
    public Double getRingAve(int R){
    	Double totalSqrFt = 0.0;
    	KHomes curHouse = header;
    	for(int i=R+1; i<k; i++){
    		totalSqrFt+=curHouse.getSqrFeet();
			curHouse = curHouse.getNext();
    	} 
    	return totalSqrFt/(k-1-R);
    }
    
	public static void main(String[] args){
		KHomes house1 = new KHomes(.4,2000.0);
		LinkedListKNeigh testList = new LinkedListKNeigh(4);
		testList.printList();
		testList.add(house1);
		testList.printList();
		KHomes house2 = new KHomes(.8,2000.0);
		testList.add(house2);
		testList.printList();
		KHomes house3 = new KHomes(.2,2000.0);
		testList.add(house3);
		testList.printList();
		KHomes house4 = new KHomes(.1,2000.0);
		testList.add(house4);
		testList.printList();	
		KHomes house5 = new KHomes(.5,2000.0);
		testList.add(house5);
		testList.printList();	
		KHomes house6 = new KHomes(.001,2000.0);
		testList.add(house6);
		testList.printList();	
		KHomes house7 = new KHomes(.9,2000.0);
		testList.add(house7);
		testList.printList();
	}
}