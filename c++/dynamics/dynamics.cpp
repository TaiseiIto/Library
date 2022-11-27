#include <cmath>
#include <iomanip>
#include "dynamics.hpp"

Dynamics::Vector::Vector(double x, double y, double z):x(x), y(y), z(z)
{
}

Dynamics::Vector::Vector(const Dynamics::Vector& vector):x(vector.x), y(vector.y), z(vector.z)
{
}

Dynamics::Vector::~Vector()
{
}

double Dynamics::Vector::get_x()const
{
	return this->x;
}

double Dynamics::Vector::get_y()const
{
	return this->y;
}

double Dynamics::Vector::get_z()const
{
	return this->z;
}

Dynamics::Vector Dynamics::Vector::operator+()const
{
	return *this;
}

Dynamics::Vector Dynamics::Vector::operator-()const
{
	return Dynamics::Vector(-this->x, -this->y, -this->z);
}

Dynamics::Vector Dynamics::Vector::operator+(const Dynamics::Vector &vector)const
{
	return Dynamics::Vector(x + vector.get_x(), y + vector.get_y(), z + vector.get_z());
}

Dynamics::Vector Dynamics::Vector::operator-(const Dynamics::Vector &vector)const
{
	return Dynamics::Vector(x - vector.get_x(), y - vector.get_y(), z - vector.get_z());
}

Dynamics::Vector Dynamics::Vector::operator*(double a)const // scalar multiplication of vector
{
	return Dynamics::Vector(a * this->x, a * this->y, a * this->z);
}

Dynamics::Vector Dynamics::Vector::operator/(double a)const // scalar division of vector
{
	return *this * (1 / a);
}

Dynamics::Vector Dynamics::Vector::operator*(const Dynamics::Vector &vector)const // cross product
{
	return Dynamics::Vector(this->y * vector.get_z() - this->z * vector.get_y(), this->z * vector.get_x() - this->x * vector.get_z(), this->x * vector.get_y() - this->y * vector.get_x());
}

double Dynamics::Vector::operator,(const Dynamics::Vector &vector)const // inner product
{
	return this->x * vector.get_x() + this->y * vector.get_y() + this->z * vector.get_z();
}

double Dynamics::Vector::operator/(const Dynamics::Vector &vector)const // angle between vectors
{
	double denominator = !*this * !vector;
	if(denominator == 0)
	{
		ERROR();
		return 0;
	}
	return std::acos((*this, vector) / denominator);
}

double Dynamics::Vector::operator/(const Dynamics::Plane& plane)const // angle between vector and plane
{
	return std::abs(M_PI / 2 - (*this / plane.get_normal()));
}

double Dynamics::Vector::operator!()const // length
{
	return std::sqrt(**this);
}

double Dynamics::Vector::operator*()const // length ^ 2
{
	return (*this, *this);
}

Dynamics::Vector Dynamics::Vector::operator>>(const Dynamics::Plane& plane)const // projection of vector onto plane
{
	return (*this > plane) - (Dynamics::Vector(0, 0, 0) > plane);
}

// Vector rotation
// If the axis direction is forward and the angle is positive, rotate clockwise.
// If the axis direction is forward and the angle is negative, rotate counterclockwise.
Dynamics::Vector Dynamics::Vector::rotate(const Dynamics::Vector& axis, double angle)const
{
	if(*axis == 0)
	{
		ERROR();
		return Dynamics::Vector(0, 0, 0);
	}
	if(**this == 0)return *this;
	Dynamics::Plane p(Dynamics::Vector(0, 0, 0), axis);
	Dynamics::Vector v = *this >> p;
	Dynamics::Vector w = *this - v;
	Dynamics::Vector x = axis * v / !axis;
	Dynamics::Vector y = std::cos(angle) * v + std::sin(angle) * x;
	return w + y;
}

Dynamics::Plane::Plane(const Dynamics::Coordinates& point, const Dynamics::Vector& normal):point(point), normal(normal)
{
	if(*normal == 0)ERROR();
}

Dynamics::Plane::Plane(const Dynamics::Plane& plane):Dynamics::Plane::Plane(plane.point, plane.normal)
{
}

Dynamics::Plane::Plane(const Dynamics::Coordinates& a, const Dynamics::Coordinates& b, const Dynamics::Coordinates& c):Dynamics::Plane::Plane(a, (b - a) * (c - a)) // A plane containing given 3 points.
{
}

Dynamics::Plane::~Plane()
{
}

Dynamics::Coordinates Dynamics::Plane::get_point()const
{
	return this->point;
}

Dynamics::Vector Dynamics::Plane::get_normal()const
{
	return this->normal;
}

Dynamics::Coordinates Dynamics::Plane::operator-(const Coordinates& point)const // normal from point to plane
{
	return point - *this;
}

double Dynamics::Plane::operator/(const Plane &plane)const // angle between vectors
{
	return M_PI - (this->normal / plane.normal);
}

double Dynamics::Plane::operator/(const Dynamics::Vector& vector)const // angle between plane and vector
{
	return vector / *this;
}

Dynamics::Vector operator*(double a, const Dynamics::Vector& vector) // scalar multiplication of vector
{
	return vector * a;
}

Dynamics::Coordinates operator-(const Dynamics::Coordinates& point, const Dynamics::Plane& plane) // normal from plane to point
{
	Dynamics::Vector n = plane.get_normal();
	Dynamics::Vector qp = point - plane.get_point();
	return ((qp , n) / *n) * n;
}

Dynamics::Coordinates operator>(const Dynamics::Coordinates& point, const Dynamics::Plane& plane) // projection of point onto plane
{
	return point - (point - plane);
}

std::ostream& operator<<(std::ostream &ostream, const Dynamics::Vector &vector)
{
	ostream << "(" << std::setw(Dynamics::print_length) << vector.get_x() << "," << std::setw(Dynamics::print_length) << vector.get_y() << "," << std::setw(Dynamics::print_length) << vector.get_z() << ")";
	return ostream;
}

std::ostream& operator<<(std::ostream &ostream, const Dynamics::Plane &plane)
{
	ostream << "(point = " << plane.get_point() << ", normal = " << plane.get_normal() << ")";
	return ostream;
}

