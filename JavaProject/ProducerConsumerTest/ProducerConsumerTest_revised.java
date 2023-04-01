
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
		

		//���� ���ڴ� �Ѱ����� �Է��Ѵٰ� ����.

		while(true) {
			if(sc.next().charAt(0)=='q') {
				System.out.println("[Admin] �����ϰڽ��ϴ�.");
				producer.interrupt();
				worker1.interrupt();
				worker2.interrupt();
				break;
			}
			else System.out.println("[Admin] ���� ���ϴ� ��ɾ� �Դϴ�");
		}
		try {
			producer.join();
			worker1.join();
			worker2.join();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("[Admin] producer�� consumer ��� �����Ͽ����ϴ�. ��ó�� �۾� ����:"+buffer.getCnt());
	}
}



class MBuffer {
	private int[] data= new int[5];
	private int cnt = 0; //���� ����ũ ��
	private int total_cnt = 0; // �� ����ũ ��
	private int head = 0; //buffer�� ������ ��ġ
	private int flag = 0; //buffer�� �� ���� ��ġ
	

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
		while (4<flag){ //�ɟ��� ���۰� ���������� ������� �ʴ´�.
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
		System.out.println( "������ ����. �� ���� ���� ����="+buffer.getTotalCnt() );
	}

	public void run() {
		int i=0;
			try {
				while (!Thread.interrupted()) {
					buffer.put(i);
					System.out.println("������: " + i + "�� ������ �����Ͽ����ϴ�.");
					i+=1;

					Thread.sleep((int) (Math.random() * 1000)); // 0~1�� ���� �޽�
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
		System.out.println( "�Һ���["+name+"]: ����. HowManyWorks="+HowManyWorks+", TotalWorkTimes:"+TotalWorkTimes );
	}
	
	public void run() {
//		/synchronized �޼ҵ� �Ἥ 2�� ���ÿ� �ǵ� ���������ϱ�
			try {
			
				while (!Thread.interrupted()) {
					int data = buffer.get();
					int currentWorkTime = (int) (Math.random() * WorkLimitTime);
					HowManyWorks++;
					TotalWorkTimes += currentWorkTime;

					if(buffer.cantGet()) {
						System.out.println("�Һ���["+name+"]: ���ۿ� ���� ��� ��� ���Դϴ�");
					}
					System.out.println("�Һ���["+name+"]: " + data + "�� ������ �Һ��Ͽ����ϴ�.�ҿ�ð�:"+currentWorkTime);
				

					Thread.sleep(currentWorkTime);
				}
			} catch (InterruptedException e) {  getResult();}
		
		
	}
}

