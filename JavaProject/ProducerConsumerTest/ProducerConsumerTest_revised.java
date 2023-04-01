
package HW8;

import java.awt.event.*;
import java.util.Random;
import java.util.Scanner;

public class ProducerConsumerTest {
	
	public static void main(String[] args) {
		Scanner sc = new Scanner(System.in);
		 
		MBuffer buffer = new MBuffer();
		Thread producer = (new Thread(new MProducer(buffer))); 
		Thread worker1 =  (new Thread(new MConsumer(buffer,"C1", 1000))); 
		Thread worker2 =  (new Thread(new MConsumer(buffer,"C2", 2000))); 

		producer.setDaemon(true);
		worker1.setDaemon(true);
		worker2.setDaemon(true);
		
		producer.start();
		worker1.start();
		worker2.start();
		

		//종료 문자는 한가지만 입력한다고 가정.

		while(true) {
			if(sc.next().charAt(0)=='q') {
				System.out.println("[Admin] 종료하겠습니다.");
				producer.interrupt();
				worker1.interrupt();
				worker2.interrupt();
				break;
			}
			else System.out.println("[Admin] 알지 못하는 명령어 입니다");
		}
		try {
			producer.join();
			worker1.join();
			worker2.join();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("[Admin] producer와 consumer 모두 종료하였습니다. 미처리 작업 개수:"+buffer.getCnt());
	}
}



class MBuffer {
	private int[] data= new int[5];
	private int cnt = 0; //현재 케이크 수
	private int total_cnt = 0; // 총 케이크 수
	private int head = 0; //buffer내 가져갈 위치
	private int flag = 0; //buffer내 현 생산 위치
	

	public synchronized int get() {
		while (cantGet()) {
			try {
				wait();
			} catch (InterruptedException e) {
			}
		}
		int cake = data[head];
		head = (head+1)%5;
		cnt--;
		notifyAll();
		return cake;
	}
	
	public boolean cantGet(){
		return cnt<=0;
	}
	
	public int getTotalCnt() {
		return total_cnt;
	}
	
	public int getCnt() {
		return cnt;
	}
	
	public synchronized void put(int data) {
		//System.out.println(flag);
		while (4<flag){ //케잌이 버퍼가 꽉차있으면 생산되지 않는다.
			try {
				wait();
			} catch (InterruptedException e) {
			}
		}
		this.data[flag] = data;
		flag = (flag+1)%5;
		cnt++;
		total_cnt++;
		notifyAll();
	}
}

class MProducer implements Runnable{
	private MBuffer buffer;

	public MProducer(MBuffer buffer) {
		this.buffer = buffer;
	}

	public void getResult() {
		System.out.println( "생산자 종료. 총 생산 케익 개수="+buffer.getTotalCnt() );
	}

	public void run() {
		int i=0;
			try {
				while (!Thread.interrupted()) {
					buffer.put(i);
					System.out.println("생산자: " + i + "번 케익을 생산하였습니다.");
					i+=1;

					Thread.sleep((int) (Math.random() * 1000)); // 0~1초 사이 휴식
				}
			} catch (InterruptedException ee) {getResult();}
	}
	
}

class MConsumer implements Runnable {
	private MBuffer buffer;
	private int WorkLimitTime;
	private int HowManyWorks;
	private int TotalWorkTimes;
	private String name;
	
	public MConsumer(MBuffer drop, String _name, int _WorkLimitTime) {
		this.buffer = drop;
		this.name = _name;
		this.WorkLimitTime = _WorkLimitTime;
		this.HowManyWorks = 0;
	}

	public String getName() {
		return this.name;
	}
	
	public void getResult() {
		System.out.println( "소비자["+name+"]: 종료. HowManyWorks="+HowManyWorks+", TotalWorkTimes:"+TotalWorkTimes );
	}
	
	public void run() {
//		/synchronized 메소드 써서 2개 동시에 되도 문제없게하기
			try {
			
				while (!Thread.interrupted()) {
					int data = buffer.get();
					int currentWorkTime = (int) (Math.random() * WorkLimitTime);
					HowManyWorks++;
					TotalWorkTimes += currentWorkTime;

					if(buffer.cantGet()) {
						System.out.println("소비자["+name+"]: 버퍼에 일이 없어서 대기 중입니다");
					}
					System.out.println("소비자["+name+"]: " + data + "번 케익을 소비하였습니다.소요시간:"+currentWorkTime);
				

					Thread.sleep(currentWorkTime);
				}
			} catch (InterruptedException e) {  getResult();}
		
		
	}
}

