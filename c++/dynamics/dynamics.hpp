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
	class Plane;
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
		Vector operator*(double a)const; // scalar multiplication of vector
		Vector operator*(const Vector &vector)const; // cross product
		double operator,(const Vector &vector)const; // inner product
		double operator/(const Vector &vector)const; // angle between vectors
		double operator/(const Dynamics::Plane& plane)const; // angle between vector and plane
		double operator!()const; // length
		double operator*()const; // length ^ 2
	};
	using Coordinates = Vector;
	class Plane
	{
	private:
		Coordinates point; // The plane contains the point.
		Vector normal; // The plane and the normal form a right angle.
	public:
		Plane(const Coordinates& point, const Vector& normal);
		Plane(const Coordinates& a, const Coordinates& b, const Coordinates& c); // A plane containing given 3 points.
		Plane(const Plane& plane);
		~Plane();
		Coordinates get_point()const;
		Vector get_normal()const;
		Coordinates operator-(const Coordinates& point)const; // normal from point to plane
		double operator/(const Plane &plane)const; // angle between vectors
		double operator/(const Dynamics::Vector& vector)const; // angle between plane and vector
	};
	const unsigned int print_length = 10;
}

Dynamics::Vector operator*(double a, const Dynamics::Vector& vector); // scalar multiplication of vector
Dynamics::Coordinates operator-(const Dynamics::Coordinates& point, const Dynamics::Plane& plane); // normal from plane to point
Dynamics::Coordinates operator>(const Dynamics::Coordinates& point, const Dynamics::Plane& plane); // projection of point onto plane
std::ostream& operator<<(std::ostream &ostream, const Dynamics::Vector &vector);
std::ostream& operator<<(std::ostream &ostream, const Dynamics::Plane &plane);

#endif

