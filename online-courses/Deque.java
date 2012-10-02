/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author simon
 */
public class Deque<Item> implements Iterable<Item> {
//   public Deque() {                     // construct an empty deque
//   public boolean isEmpty()           // is the deque empty?
//   public int size()                  // return the number of items on the deque
//   public void addFirst(Item item)    // insert the item at the front
//   public void addLast(Item item)     // insert the item at the end
//   public Item removeFirst()          // delete and return the item at the front
//   public Item removeLast()           // delete and return the item at the end
//   public Iterator<Item> iterator()   // return an iterator over items in order from front to end
    final private int MAX_CNT=100; 
    private Vector<Item> items = null; 
    public Deque(){
        items = new Item[MAX_CNT];
    }
    
    public boolean isEmpty() {
        return true; 
    }
    
    public int size() {
        return 0;
    }
    
    public void addFirst(Item item) {
    }
    
   public void addLast(Item item) {
       
   }
   public Item removeFirst() {
       return null;
   }
   public Item removeLast() {
       return null;       
   }
   public Iterator<Item> iterator() {
       return null;
   }
}
