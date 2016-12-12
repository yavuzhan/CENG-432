class IyteHashTable {
  var size = 16
  
  private val map: Array[ImmutableList] = new Array[ImmutableList](size)
  for (i <- 0 until map.length){
  map(i)=Nil
  }
  
  def get(searchKey: String): String = map(hash(searchKey)).get(searchKey)
  
  def set(newKey: String, newValue: String): IyteHashTable = { 
    map(hash(newKey)) = map(hash(newKey)).add(newKey, newValue)
    this
   }
  
  private def hash(string: String): Int = string.hashCode
  
}

sealed trait ImmutableList{
  def add(newKey: String, newValue: String): ImmutableList = this match {
    case Nil => new NonEmptyNode(newKey, newValue, Nil)
    case th @ NonEmptyNode(k, v, t) =>
    if(newKey < k) new NonEmptyNode(newKey, newValue, th)
    else if (newKey > k) new NonEmptyNode(k, v, t.add(newKey, newValue))
    else new NonEmptyNode(newKey, newValue, t)
 }

  def get(searchKey: String): String = this match {
    case Nil => throw new Exception("not found")
    case NonEmptyNode(k,v,t) =>
    if (searchKey == k) v
    else if (searchKey < k) t.get(searchKey)
    else null
  }
}

case class NonEmptyNode(key: String, value :String, tail: ImmutableList) extends ImmutableList
case object Nil extends ImmutableList

object IyteHashTable {
  def apply() = new IyteHashTable()
}