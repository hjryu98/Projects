
package HW8;
import java.util.*;

public class ProducerConsumerTest {
        public static void main(String[] args) throws InterruptedException{
        Scanner sc = new Scanner(System.in);
        
        Buffer buffer = new Buffer();
        
        //instantiate.
        Thread producer = (new Thread(new MProducer(buffer)));
        Thread worker1 =  (new Thread(new MConsumer(buffer,"C1", 1000)));
        Thread worker2 =  (new Thread(new MConsumer(buffer,"C2", 2000)));
        
        producer.setDaemon(true);        
	worker1.setDaemon(true);
	worker2.setDaemon(true);
        
        //start the sub threads
        producer.start();
        worker1.start();
	worker2.start();
         
       
        //while loop that stands for, if "q" is entered in system, terminates all jobs, otherwise keep going with error message.
        while(true){
            char st = sc.next().charAt(0);
            if(st == 'q'){
                System.out.println("[Admin] �����ϰڽ��ϴ�.");
                producer.interrupt(); 
                worker1.interrupt();
                worker2.interrupt();
                break;
            }
            else{
                System.out.println("[Admin] ���� ���ϴ� ��ɾ��Դϴ�:");
            }
        }try{
            producer.join();
            worker1.join();
            worker2.join();
        }catch(InterruptedException e){//By this error exception, we can terminate threads.
            e.printStackTrace();
        }
        System.out.println("[Admin] producer �� consumer ��� �����Ͽ����ϴ�.��ó�� �۾� ����:" + buffer.getnumber());
                

    }
}

    class Buffer { //Buffer class that stores inventories, and this buffer's capacity is 5.
        private final int[] data = new int[5]; //capacity
        private static int cnt = 0; //number of inventories in buffer
        private static int f = 0; 
        private static int r = 0; //f, r is the index that stands for front and rear pointer.(Implementation of circular queue.)
    
        /**
         * 
         * @return how many inventories in buffer currently. 
         */
        public int getnumber(){
            return cnt;
        }
        
        /**
         * make this method synchronized.
         */
        public synchronized void get(){

            while(cnt <= 0){ //If there is no inventories, Consumer waiting for input.
                System.out.println("[" + Thread.currentThread().getName() + "]" + "���ۿ� ���� ��� ��� ���Դϴ�");
                try{
                    wait();
                }catch(InterruptedException e){

                }
            }
            int currentdata = data[f];
            data[f++] = 0;
            System.out.format("�Һ���[%s]: %d �� ������ �Һ��Ͽ����ϴ�.",Thread.currentThread().getName(), currentdata - 1);
            cnt--;
            if(f >= 5){
                f = 0;
            }

            notifyAll();
        }
        
         /**
         * make this method synchronized.
         */
        public synchronized void put(int inputdata){

            while(cnt >= data.length){ //If the buffer full, Producer waiting for manufacturing.
                try{
                    wait();
                } catch(InterruptedException e){

                }
            }
            data[r++] = inputdata;
            System.out.format("������: %d �� ������ �����Ͽ����ϴ�.\n", inputdata );
            if(r >= 5){
                r = 0;
            }
            cnt++;
            notifyAll();
        }
    }

class MConsumer implements Runnable {
    private Buffer buffer;
    private int WorkLimitTime;
    private int HowManyWorks;
    private int TotalWorkTimes;
    private String name;

    public MConsumer(Buffer drop, String _name, int _WorkLimitTime) {
	this.buffer = drop;
	this.name = _name;
	this.WorkLimitTime = _WorkLimitTime;
	this.HowManyWorks = 0;
        
    }

    public void run() {
            Thread.currentThread().setName(name);

                try {
                    while(!Thread.interrupted()){ //By this while loop, after main method interrupt the thread, thread will terminate this job in catch.
                    buffer.get();
                    int currentWorkTime = (int) (Math.random() * WorkLimitTime);
                    System.out.println("�ҿ�ð�:" + currentWorkTime);
                    HowManyWorks++;
                    TotalWorkTimes += currentWorkTime;
                    Thread.sleep(currentWorkTime);
                } 
                }catch (InterruptedException e) { //When interrupted, print this.
                     System.out.format("�Һ���[%s]:����. HowManyWorks=%d, TotalWorkTimes=%d\n",name, HowManyWorks, TotalWorkTimes);
                }  
          
    }
}

class MProducer implements Runnable {
    private Buffer buffer;

    public MProducer(Buffer buffer) {
	this.buffer = buffer;
    }

    public void run() {
        int i = 0;
        try{
            while(!Thread.interrupted()){//By this while loop, after main method interrupt the thread, thread will terminate this job in catch.
                buffer.put(++i);
                
                Thread.sleep((int)(Math.random() * 1000));
            }
        }catch(InterruptedException e){//When interrupted, print this.
             System.out.format("������ ����. �� ���� ���� ���� = %d\n", i - 1);
        }
      }

  }
