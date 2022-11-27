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

Dynamics::Vector Dynamics::Vector::operator*(double a)const // Scalar multiplication of vector
{
	return Dynamics::Vector(a * this->x, a * this->y, a * this->z);
}

Dynamics::Vector Dynamics::Vector::operator/(double a)const // Scalar division of vector
{
	return *this * (1 / a);
}

Dynamics::Vector Dynamics::Vector::operator*(const Dynamics::Vector &vector)const // Cross product
{
	return Dynamics::Vector(this->y * vector.get_z() - this->z * vector.get_y(), this->z * vector.get_x() - this->x * vector.get_z(), this->x * vector.get_y() - this->y * vector.get_x());
}

double Dynamics::Vector::operator,(const Dynamics::Vector &vector)const // Inner product
{
	return this->x * vector.get_x() + this->y * vector.get_y() + this->z * vector.get_z();
}

double Dynamics::Vector::operator/(const Dynamics::Vector &vector)const // Angle between vectors
{
	double denominator = !*this * !vector;
	if(denominator == 0)
	{
		ERROR();
		return 0;
	}
	return std::acos((*this, vector) / denominator);
}

double Dynamics::Vector::operator/(const Dynamics::Plane& plane)const // Angle between vector and plane
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

Dynamics::Vector Dynamics::Vector::operator>>(const Dynamics::Plane& plane)const // Projection of vector onto plane
{
	return (*this > plane) - (Dynamics::Vector(0, 0, 0) > plane);
}

// Vector rotation
// If the axis direction is forward and the angle is positive, rotate clockwise.
// If the axis direction is forward and the angle is negative, rotate counterclockwise.
Dynamics::Vector Dynamics::Vector::rotate(const Dynamics::Vector& axis, double angle/* radian */)const
{
	if(*axis == 0)
	{
		ERROR();
		return Dynamics::Vector(0, 0, 0);
	}
	if(**this == 0)return *this;
	if(*this / axis == 0)return *this;
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

Dynamics::Coordinates Dynamics::Plane::operator-(const Coordinates& point)const // Normal from point to plane
{
	return point - *this;
}

double Dynamics::Plane::operator/(const Plane &plane)const // Angle between vectors
{
	return M_PI - (this->normal / plane.normal);
}

double Dynamics::Plane::operator/(const Dynamics::Vector& vector)const // Angle between plane and vector
{
	return vector / *this;
}

Dynamics::Posture::Posture(double roll, double pitch, double yaw): roll(roll), pitch(pitch), yaw(yaw), front(Dynamics::Vector(1, 0, 0)), left(Dynamics::Vector(0, 1, 0)), up(Dynamics::Vector(0, 0, 1))
{
	// Roll rotation
	this->left = this->left.rotate(this->front, this->roll);
	this->up = this->up.rotate(this->front, this->roll);
	// Pitch rotation
	this->front = this->front.rotate(this->left, this->pitch);
	this->up = this->up.rotate(this->left, this->pitch);
	// Yaw rotation
	this->front = this->front.rotate(this->up, this->yaw);
	this->left = this->left.rotate(this->up, this->yaw);
	// Adjust roll
	Dynamics::Coordinates o(0, 0, 0);
	Dynamics::Coordinates x(1, 0, 0);
	Dynamics::Coordinates y(0, 1, 0);
	Dynamics::Coordinates z(0, 0, 1);
	Dynamics::Plane front_back_separator(o, y, z);
	Dynamics::Plane left_right_separator(x, o, z);
	Dynamics::Vector up_projection = this->up >> front_back_separator;
	this->roll = *up_projection ? up_projection / left_right_separator : 0;
}

Dynamics::Posture::Posture(const Posture& posture): roll(posture.roll), pitch(posture.pitch), yaw(posture.yaw), front(posture.front), left(posture.left), up(posture.up)
{
}

Dynamics::Posture::~Posture()
{
}

double Dynamics::Posture::get_roll()const
{
	return this->roll;
}

double Dynamics::Posture::get_pitch()const
{
	return this->pitch;
}

double Dynamics::Posture::get_yaw()const
{
	return this->yaw;
}

Dynamics::Vector Dynamics::Posture::get_front()const
{
	return this->front;
}

Dynamics::Vector Dynamics::Posture::get_back()const
{
	return -this->front;
}

Dynamics::Vector Dynamics::Posture::get_left()const
{
	return this->left;
}

Dynamics::Vector Dynamics::Posture::get_right()const
{
	return -this->left;
}

Dynamics::Vector Dynamics::Posture::get_up()const
{
	return this->up;
}

Dynamics::Vector Dynamics::Posture::get_down()const
{
	return -this->up;
}

Dynamics::Vector operator*(double a, const Dynamics::Vector& vector) // Scalar multiplication of vector
{
	return vector * a;
}

Dynamics::Coordinates operator-(const Dynamics::Coordinates& point, const Dynamics::Plane& plane) // Normal from plane to point
{
	Dynamics::Vector n = plane.get_normal();
	Dynamics::Vector qp = point - plane.get_point();
	return ((qp , n) / *n) * n;
}

Dynamics::Coordinates operator>(const Dynamics::Coordinates& point, const Dynamics::Plane& plane) // Projection of point onto plane
{
	return point - (point - plane);
}

std::ostream& operator<<(std::ostream &ostream, const Dynamics::Vector &vector)
{
	ostream << "(x = " << std::setw(Dynamics::print_length) << vector.get_x() << ", y = " << std::setw(Dynamics::print_length) << vector.get_y() << ", z = " << std::setw(Dynamics::print_length) << vector.get_z() << ")";
	return ostream;
}

std::ostream& operator<<(std::ostream &ostream, const Dynamics::Plane &plane)
{
	ostream << "(point = " << plane.get_point() << ", normal = " << plane.get_normal() << ")";
	return ostream;
}

std::ostream& operator<<(std::ostream &ostream, const Dynamics::Posture &posture)
{
	ostream << "(roll = " << posture.get_roll() << ", pitch = " << posture.get_pitch() << ", yaw = " << posture.get_yaw() << ")";
	return ostream;
}

