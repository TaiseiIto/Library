#include "dynamics.hpp"

Dynamics::Vector::Vector(double x, double y, double z):x(x), y(y), z(z)
{
}

Dynamics::Vector::Vector(const Vector& vector):x(vector.x), y(vector.y), z(vector.z)
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

std::ostream& operator<<(std::ostream &ostream, const Dynamics::Vector &vector)
{
	ostream << "(" << vector.get_x() << "," << vector.get_y() << "," << vector.get_z() << ")";
	return ostream;
}

