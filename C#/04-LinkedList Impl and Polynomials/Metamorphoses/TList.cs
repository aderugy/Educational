using System;

namespace Metamorphoses;

public class TList<T>
{
    public class OutOfRangeIndexException : Exception
    {
    }

    public class ElementNotFoundException : Exception
    {
    }

    public class Element<T>
    {
        public T Data { get; set; }

        public Element<T>? Next { get; set; }

        public Element(T data)
        {
            Data = data;
            Next = null;
        }
    }

    private Element<T>? _head;
    private uint _size;

    public Element<T> Head
    {
        get => _head!;
        set => _head = value;
    }

    public uint Size
    {
        get => _size;
        set => _size = value;
    }

    //Instantiates an empty list, head should be null and size should be 0
    public TList()
    {
        _head = null;
        _size = 0;
    }

    //Instantiates a new TList from an element, size should be 1
    public TList(T e)
    {
        _head = new Element<T>(e);
        _size = 1;
    }

    //Add the element e at the end of the list, size should be updated
    public void Add(T e)
    {
        Element<T> nextElement = _head;
        if (nextElement == null)
            _head = new Element<T>(e);
        else
        {
            while (nextElement.Next != null)
            {
                nextElement = nextElement.Next;
            }

            nextElement.Next = new Element<T>(e);
        }
        _size++;
    }

    //Insert the element e at the position n in our list then return the list thus created
    public void Insert(T e, int n)
    {
        if ((_head == null && n > 0) || n > _size || n < 0)
            throw new OutOfRangeIndexException();
        
        Element<T> previous = _head;
        Element<T> toInsert = new(e);
        
        if (n == 0)
        {
            toInsert.Next = previous;
            _head = toInsert;
            _size++;
            return;
        }
        
        if (n == _size)
        {
            Add(e);
            return;
        }
        
        
        for (int i = 1; i < n; i++)
        {
            if (previous == null)
                throw new OutOfRangeIndexException();
            previous = previous.Next;
        }

        if (previous is null) return;
        toInsert.Next = previous.Next;
        previous.Next = toInsert;
        _size++;
    }

    //Returns the string representing the TList graphically
    public override string ToString()
    {
        string result = "{";

        Element<T> currentElement = _head;

        while (currentElement != null)
        {
            result += ' ' + currentElement.Data.ToString();
            currentElement = currentElement.Next;
            if (currentElement != null)
                result += ',';
        }

        return result + " }";
    }

    //Returns true if both lists are equals
    public static bool operator ==(TList<T> l1, TList<T> l2)
    {
        if (l1 is null || l2 is null)
            return l1 is null && l2 is null;
        
        if (l1._size != l2._size) return false;

        Element<T> currentElementL1 = l1._head;
        Element<T> currentElementL2 = l2._head;

        while (currentElementL1 != null && currentElementL2 != null)
        {
            if (!currentElementL1.Data.Equals(currentElementL2.Data))
                return false;
            currentElementL1 = currentElementL1.Next;
            currentElementL2 = currentElementL2.Next;
        }

        return true;
    }

    //Returns false if both lists are equals
    public static bool operator !=(TList<T> l1, TList<T> l2)
    {
        return !(l1 == l2);
    }

    //Creates a new list that is the result of the concat of l1 l2
    public static TList<T> operator +(TList<T> l1, TList<T> l2)
    {
        TList<T> concatenatedList = new TList<T>();

        Element<T> currentElementL1 = l1._head;
        Element<T> lastElementNewList = null;

        if (currentElementL1 != null)
        {
            concatenatedList._head = new Element<T>(currentElementL1.Data);
            lastElementNewList = concatenatedList._head;
            
            while (currentElementL1.Next != null)
            {
                currentElementL1 = currentElementL1.Next;
                Element<T> toAdd = new Element<T>(currentElementL1.Data);
                lastElementNewList.Next = toAdd;
                lastElementNewList = lastElementNewList.Next;
            }
        }

        Element<T> currentElementL2 = l2._head;

        if (currentElementL2 != null)
        {
            if (lastElementNewList == null)
            {
                concatenatedList._head = new Element<T>(currentElementL2.Data);
                lastElementNewList = concatenatedList._head;
            }
            else
            {
                lastElementNewList.Next = new Element<T>(currentElementL2.Data);
                lastElementNewList = lastElementNewList.Next;
            }

            while (currentElementL2.Next != null)
            {
                currentElementL2 = currentElementL2.Next;
                Element<T> toAdd = new Element<T>(currentElementL2.Data);
                lastElementNewList.Next = toAdd;
                lastElementNewList = lastElementNewList.Next;
            }
        }

        concatenatedList._size = l1._size + l2._size;
        return concatenatedList;
    }

    //Removes the first occurence of element from l1 if it exits
    public static TList<T> operator -(TList<T> l1, T e)
    {
        Element<T> currentElement = l1._head;
        if (currentElement is null) return l1;
        if (currentElement.Data.Equals(e))
        {
            l1._head = currentElement.Next;
            l1._size--;
            return l1;
        }

        while (currentElement.Next != null)
        {
            Element<T> nextElement = currentElement.Next;
            if (nextElement.Data.Equals(e))
            {
                currentElement.Next = nextElement.Next;
                l1._size--;
                return l1;
            }

            currentElement = currentElement.Next;
        }

        return l1;
    }

    //Reverse a TList<T>
    public void Reverse()
    {
        Element<T> previous = _head;
        if (previous == null || previous.Next == null) return;
        Element<T> current = previous.Next;
        previous.Next = null;

        while (current.Next != null)
        {
            Element<T> nextElement = current.Next;
            current.Next = previous;
            previous = current;
            current = nextElement;
        }

        current.Next = previous;
        _head = current;
    }
}