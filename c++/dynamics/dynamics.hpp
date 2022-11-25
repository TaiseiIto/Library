#ifndef _DYNAMICS_HPP_
#define _DYNAMICS_HPP_

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
		Vector operator+(const Vector &vector)const;
		Vector operator-(const Vector &vector)const;
		double operator,(const Vector &vector)const; // inner product
		Vector operator*(const Vector &vector)const; // cross product
	};
}

std::ostream& operator<<(std::ostream &ostream, const Dynamics::Vector &vector);

#endif

