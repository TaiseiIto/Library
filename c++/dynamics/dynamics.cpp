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

Dynamics::Vector Dynamics::Vector::operator*(const Dynamics::Vector &vector)const // cross product
{
	return Dynamics::Vector(this->y * vector.get_z() - this->z * vector.get_y(), this->z * vector.get_x() - this->x * vector.get_z(), this->x * vector.get_y() - this->y * vector.get_x());
}

double Dynamics::Vector::operator,(const Dynamics::Vector &vector)const // inner product
{
	return this->x * vector.get_x() + this->y * vector.get_y() + this->z * vector.get_z();
}

double Dynamics::Vector::operator^(const Dynamics::Vector &vector)const // angle between vectors
{
	double denominator = !*this * !vector;
	if(denominator == 0)
	{
		ERROR();
		return 0;
	}
	return std::acos((*this, vector) / denominator);
}

double Dynamics::Vector::operator!()const // length
{
	return std::sqrt(**this);
}

double Dynamics::Vector::operator*()const // length ^ 2
{
	return (*this, *this);
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
	return point;
}

Dynamics::Vector Dynamics::Plane::get_normal()const
{
	return normal;
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

