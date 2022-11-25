#ifndef _DYNAMICS_HPP_
#define _DYNAMICS_HPP_

#ifndef ERROR
#define _STRING(x) #x
#define _STRING2(x) _STRING(x)
#define ERROR() (std::cerr << ("ERROR " __FILE__ " : " _STRING2(__LINE__)) << std::endl)
#endif

#include <iostream>

namespace Dynamics
{
	class Vector
	{
	private:
		double x, y, z;
	public:
		Vector(double x, double y, double z);
		Vector(const Vector& vector);
		~Vector();
		double get_x()const;
		double get_y()const;
		double get_z()const;
		Vector operator+()const;
		Vector operator-()const;
		Vector operator+(const Vector &vector)const;
		Vector operator-(const Vector &vector)const;
		Vector operator*(const Vector &vector)const; // cross product
		double operator,(const Vector &vector)const; // inner product
		double operator^(const Vector &vector)const; // angle between vectors
		double operator!()const; // length
		double operator*()const; // length ^ 2
	};
	using Coordinates = Vector;
}

std::ostream& operator<<(std::ostream &ostream, const Dynamics::Vector &vector);

#endif

