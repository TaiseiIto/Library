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
		Vector operator*(double a)const; // Scalar multiplication of vector
		Vector operator/(double a)const; // Scalar division of vector
		Vector operator*(const Vector &vector)const; // Cross product
		double operator,(const Vector &vector)const; // Inner product
		double operator/(const Vector &vector)const; // Angle between vectors
		double operator/(const Dynamics::Plane& plane)const; // Angle between vector and plane
		double operator!()const; // length
		double operator*()const; // length ^ 2
		Vector operator>>(const Plane& plane)const; // Projection of vector onto plane
		// Vector rotation
		// If the axis direction is forward and the angle is positive, rotate clockwise.
		// If the axis direction is forward and the angle is negative, rotate counterclockwise.
		Vector rotate(const Vector& axis, double angle/* radian */)const;
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
		Coordinates operator-(const Coordinates& point)const; // Normal from point to plane
		double operator/(const Plane &plane)const; // Angle between vectors
		double operator/(const Dynamics::Vector& vector)const; // Angle between plane and vector
	};
	class Posture
	{
	private:
		double roll, pitch, yaw; // radian
		// -M_PI / 2 <= roll < M_PI / 2
		// -M_PI <= pitch < M_PI
		// 0 <= yaw < 2 * M_PI
		Vector front, left, up;
		void set_front_up(const Vector& front, const Vector& up);
	public:
		Posture(double roll, double pitch, double yaw);
		Posture(const Vector& front, const Vector& up);
		Posture(const Posture& posture);
		~Posture();
		double get_roll()const;
		double get_pitch()const;
		double get_yaw()const;
		Vector get_front()const;
		Vector get_back()const;
		Vector get_left()const;
		Vector get_right()const;
		Vector get_up()const;
		Vector get_down()const;
		Posture operator-()const; // Reverse rotation
		Posture operator+(const Posture& posture)const; // Synthesize rotations
	};
	const double angle_error_limit = 2 * M_PI / 360;
	const unsigned int print_length = 10;
}

Dynamics::Vector operator*(double a, const Dynamics::Vector& vector); // Scalar multiplication of vector
Dynamics::Coordinates operator-(const Dynamics::Coordinates& point, const Dynamics::Plane& plane); // Normal from plane to point
Dynamics::Coordinates operator>(const Dynamics::Coordinates& point, const Dynamics::Plane& plane); // Projection of point onto plane
std::ostream& operator<<(std::ostream &ostream, const Dynamics::Vector &vector);
std::ostream& operator<<(std::ostream &ostream, const Dynamics::Plane &plane);
std::ostream& operator<<(std::ostream &ostream, const Dynamics::Posture &posture);

#endif

