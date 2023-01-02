using System;

namespace Metamorphoses;

public class Polynomial
{
    public class Monomial
    {
        public uint Degree { get; set; }

        public int Coefficient { get; set; }

        //Instantiates the zero Monomial
        public Monomial()
        {
            Degree = 0;
            Coefficient = 0;
        }

        //Instantiates a Monomial with set coefficient and degree
        public Monomial(int coefficient, uint degree)
        {
            Coefficient = coefficient;
            Degree = degree;
        }

        //Returns a string representing a monomial
        public override string ToString()
        {
            if (Coefficient == 0) return "0";
            if (Coefficient == 1 && Degree == 0) return "1";
            
            const string degrees = "⁰¹²³⁴⁵⁶⁷⁸⁹";
            const string digits = "0123456789";
            uint degree = Degree;
            int coefficient = Coefficient;

            bool isCoeffNegative = Coefficient < 0;
            string degreeToString = ""; 
            coefficient = isCoeffNegative ? -coefficient : coefficient;
            
            while (Degree != 1 && degree > 0)
            {
                degreeToString = degrees[(int) degree % 10] + degreeToString;
                degree /= 10;
            }

            string coeffToString = "";

            while (Coefficient != 1 && coefficient > 0)
            {
                coeffToString = digits[coefficient % 10] + coeffToString;
                coefficient /= 10;
            }

            if (Degree == 0) return coeffToString;
            return (isCoeffNegative ? "-" : "") + coeffToString + "X" + degreeToString;
        }

        //Returns true if both Monomials are equals
        public static bool operator ==(Monomial m1, Monomial m2)
        {
            if (m1 is null && m2 is null) return true;
            if (m1 is null || m2 is null) return false;
            return m1.Degree.Equals(m2.Degree) && m1.Coefficient.Equals(m2.Coefficient);
        }

        //Returns false if both Monomials are equals
        public static bool operator !=(Monomial m1, Monomial m2)
        {
            return !(m1 == m2);
        }

        //Sum two Monomials when possible
        public static Monomial operator +(Monomial m1, Monomial m2)
        {
            if (m1.Degree != m2.Degree) throw new ArgumentException();

            return new Monomial(m1.Coefficient + m2.Coefficient, m1.Degree);
        }

        //Subtract a Monomial by another when possible
        public static Monomial operator -(Monomial m1, Monomial m2)
        {
            if (m1.Degree != m2.Degree) throw new ArgumentException();

            return new Monomial(m1.Coefficient - m2.Coefficient, m1.Degree);
        }

        //Multiply a Monomial by an integer
        public static Monomial operator *(Monomial m1, int n)
        {
            return new Monomial(m1.Coefficient * n, m1.Degree);
        }

        //Multiply two Monomials
        public static Monomial operator *(Monomial m1, Monomial m2)
        {
            return new Monomial(m1.Coefficient * m2.Coefficient,
                m1.Degree + m2.Degree);
        }
    }

    private TList<Monomial> _monomials { get; set; }

    public TList<Monomial> Get_Monomials()
    {
        return _monomials;
    }

    //Instantiates zero Polynomial
    public Polynomial()
    {
        _monomials = new TList<Monomial>();
        _monomials.Add(new Monomial());
    }

    //Instantiates a Polynomial from a single Monomial
    public Polynomial(Monomial m)
    {
       _monomials = new TList<Monomial>();
       _monomials.Add(m);
    }

    //Returns the Monomial of degree n of a Polynomial
    public Monomial GetMonomial(int degree)
    {
        TList<Monomial>.Element<Monomial> currentElement = _monomials.Head;

        while (currentElement != null)
        {
            if (currentElement.Data.Degree == degree) return currentElement.Data;
            currentElement = currentElement.Next;
        }
        return null;
    }

    public bool Contains(Monomial m)
    {
        var currentElement = _monomials.Head;

        while (currentElement is not null)
        {
            if (currentElement.Data.Equals(m)) return true;
            currentElement = currentElement.Next;
        }

        return false;
    }

    public bool Contains(uint degree)
    {
        var currentElement = _monomials.Head;

        while (currentElement is not null)
        {
            if (currentElement.Data.Degree.Equals(degree)) return true;
            currentElement = currentElement.Next;
        }

        return false;
    }
    
    //Returns the string representing a Polynomial
    public override string ToString()
    {
        TList<Monomial>.Element<Monomial> currentElement = _monomials.Head;
        if (currentElement is null) return "0";
        
        Monomial[] monomials = new Monomial[_monomials.Size];
        monomials[0] = currentElement.Data;

        int i = 1;
        while (currentElement.Next is not null && i < monomials.Length)
        {
            currentElement = currentElement.Next;
            monomials[i] = currentElement.Data;
            i++;
        }

        string toReturn = "";
        int max = -1;

        for (int j = 0; j < monomials.Length; j++)
        {
            if (monomials[j] is null) continue;

            if (max == -1)
                max = j;

            if (monomials[max].Degree < monomials[j].Degree)
                max = j;

            if (j != monomials.Length - 1 || max < 0) continue;

            string monoToString = monomials[max].ToString();
            monomials[max] = null;
            j = -1;
            max = -1;
            
            if (monoToString.Equals("") || monoToString.Equals("0")) continue;    
            
            
            bool isNegative = monoToString.Contains('-');
            string sign = isNegative ? "- " : "+ ";
            
            if (toReturn.Equals(""))
                if (isNegative)
                    toReturn = sign + monoToString[1..];
                else
                    toReturn = monoToString;
            else
                toReturn += " " + sign +
                            (isNegative ? monoToString[1..] : monoToString);
        }

        return toReturn;
    }

    //Returns true if both Polynomials are equals
    public static bool operator ==(Polynomial p1, Polynomial p2)
    {
        if (p1 is null && p2 is null) return true;
        if (p1 is null || p2 is null) return false;
        
        var currentElementP1 = p1.Get_Monomials().Head;
        var currentElementP2 = p2.Get_Monomials().Head;

        while (currentElementP1 is not null && currentElementP2 is not null)
        {
            if (currentElementP1.Data != currentElementP2.Data) return false;
            currentElementP1 = currentElementP1.Next;
            currentElementP2 = currentElementP2.Next;
        }

        return false;
    }

    //Returns false if both Polynomials are equals
    public static bool operator !=(Polynomial p1, Polynomial p2)
    {
        return !(p1 == p2);
    }

    //Add a Monomial m to a Polynomial p
    public static Polynomial operator +(Polynomial p, Monomial m)
    {
        Polynomial toReturn = new (m);

        var currentElement = p._monomials.Head;
        while (currentElement is not null)
        {
            Monomial currentMonomial = currentElement.Data;

            if (toReturn.Contains(currentMonomial.Degree))
            {
                toReturn.GetMonomial((int)currentMonomial.Degree).Degree += currentElement.Data.Degree;
            }
            
            currentElement = currentElement.Next;
        }

        return toReturn;
    }

    //Sum two Polynomials
    public static Polynomial operator +(Polynomial p1, Polynomial p2)
    {
        Polynomial toReturn = new ();

        var currentElement = p1._monomials.Head;
        while (currentElement is not null)
        {
            toReturn += currentElement.Data;
            currentElement = currentElement.Next;
        }

        var currentElement2 = p2._monomials.Head;
        while (currentElement2 is not null)
        {
            toReturn += currentElement2.Data;
            currentElement2 = currentElement2.Next;
        }
        return toReturn;
    }

    //Subtract a Monomial m to a Polynomial p
    public static Polynomial operator -(Polynomial p, Monomial m)
    {
        return p + new Monomial(-m.Coefficient, m.Degree);
    }

    //Subtract a Polynomial by another
    public static Polynomial operator -(Polynomial p1, Polynomial p2)
    {
        Polynomial toReturn = new ();

        var currentElement = p1._monomials.Head;
        while (currentElement is not null)
        {
            toReturn._monomials.Add(currentElement.Data);
            currentElement = currentElement.Next;
        }

        var currentElement2 = p2._monomials.Head;
        while (currentElement2 is not null)
        {
            toReturn -= currentElement2.Data;
            currentElement2 = currentElement2.Next;
        }
        return toReturn;
    }

    //Multiply a Polynomial p by a Monomial m
    public static Polynomial operator *(Polynomial p, Monomial m)
    {
        throw new NotImplementedException();
    }

    //Multiply two Polynomials
    public static Polynomial operator *(Polynomial p1, Polynomial p2)
    {
        throw new NotImplementedException();
    }

    //Derives a Polynomial
    public Polynomial Derivative()
    {
        throw new NotImplementedException();
    }

    //Primitives a Polynomial
    public Polynomial Primitive()
    {
        throw new NotImplementedException();
    }

    //Returns the integral of a Polynomial p in [a,b]
    public long Integral(long a, long b)
    {
        throw new NotImplementedException();
    }

    //Compare two Polynomials and return the one with the greater growth rate
    public static Polynomial ComparePolynomials(Polynomial p1, Polynomial p2)
    {
        throw new NotImplementedException();
    }

    //Returns p(n)
    public long PolynomialFunctionLong(long n)
    {
        throw new NotImplementedException();
    }

    //Apply a polynomial function p to all elements of a TMatrix of int m
    public TMatrix<long> ApplyPolynomialToTMatrix(TMatrix<long> m)
    {
        throw new NotImplementedException();
    }

    //Given a polynomial P and a TMatrix of int M, returns P(M)
    public TMatrix<long> PolynomialOfMatrix(TMatrix<long> m)
    {
        throw new NotImplementedException();
    }
}