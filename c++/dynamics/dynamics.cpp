#include <cmath>
#include <iomanip>
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

double Dynamics::Vector::operator,(const Vector &vector)const // inner product
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
	return (*this, vector) / denominator;
}

double Dynamics::Vector::operator!()const // length
{
	return std::sqrt(**this);
}

double Dynamics::Vector::operator*()const // length ^ 2
{
	return (*this, *this);
}

std::ostream& operator<<(std::ostream &ostream, const Dynamics::Vector &vector)
{
	const unsigned int length = 10;
	ostream << "(" << std::setw(length) << vector.get_x() << "," << std::setw(length) << vector.get_y() << "," << std::setw(length) << vector.get_z() << ")";
	return ostream;
}

