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

double Dynamics::Plane::operator/(const Plane &plane)const // Angle between planes
{
	return M_PI - (this->normal / plane.normal);
}

double Dynamics::Plane::operator/(const Dynamics::Vector& vector)const // Angle between plane and vector
{
	return vector / *this;
}

void Dynamics::Posture::set_front_up(const Vector& front, const Vector& up)
{
	if(*front == 0)
	{
		ERROR();
		return;
	}
	if(*up == 0)
	{
		ERROR();
		return;
	}
	if(Dynamics::angle_error_limit <= std::abs(M_PI / 2 - (front / up)))
	{
		ERROR();
		return;
	}
	this->front = front / !front;
	this->up = up / !up;
	this->left = up * front;
	// Adjust roll
	Dynamics::Coordinates o(0, 0, 0);
	Dynamics::Coordinates x(1, 0, 0);
	Dynamics::Coordinates y(0, 1, 0);
	Dynamics::Coordinates z(0, 0, 1);
	Dynamics::Plane front_back_separator(o, y, z);
	Dynamics::Plane left_right_separator(x, o, z);
	Dynamics::Vector up_projection = this->up >> front_back_separator;
	if(up_projection.get_y() == 0)this->roll = 0;
	else if(up_projection.get_y() * up_projection.get_z() < 0)this->roll = up_projection / left_right_separator;
	else this->roll = -(up_projection / left_right_separator);
	// Adjust pitch
	Dynamics::Vector rolled_up = z.rotate(x, this->roll);
	if(0 < this->up.get_x())this->pitch = rolled_up / this->up;
	else this->pitch = -(rolled_up / this->up);
	// Adjust yaw
	Dynamics::Vector rolled_left = y.rotate(x, this->roll);
	Dynamics::Vector pitched_front = x.rotate(rolled_left, this->pitch);
	if(rolled_left / this->front <= M_PI / 2)this->yaw = pitched_front / this->front;
	else this->yaw = 2 * M_PI - (pitched_front / this->front);
}

Dynamics::Posture::Posture(double roll, double pitch, double yaw): roll(0), pitch(0), yaw(0), front(0, 0, 0), left(0, 0, 0), up(0, 0, 0)
{
	Dynamics::Coordinates front(1, 0, 0);
	Dynamics::Coordinates left(0, 1, 0);
	Dynamics::Coordinates up(0, 0, 1);
	// Roll rotation
	front = front.rotate(front, roll);
	left = left.rotate(front, roll);
	up = up.rotate(front, roll);
	// Pitch rotation
	front = front.rotate(left, pitch);
	left = left.rotate(left, pitch);
	up = up.rotate(left, pitch);
	// Yaw rotation
	front = front.rotate(up, yaw);
	left = left.rotate(up, yaw);
	up = up.rotate(up, yaw);
	this->set_front_up(front, up);
}

Dynamics::Posture::Posture(const Vector& front, const Vector& up): roll(0), pitch(0), yaw(0), front(0, 0, 0), left(0, 0, 0), up(0, 0, 0)
{
	set_front_up(front, up);
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

Dynamics::Posture Dynamics::Posture::operator+()const // Identity map
{
	return *this;
}

Dynamics::Posture Dynamics::Posture::operator-()const // Reverse rotation
{
	Dynamics::Vector this_front(this->front);
	Dynamics::Vector this_left(this->left);
	Dynamics::Vector this_up(this->up);
	Dynamics::Vector reverse_front(1, 0, 0);
	Dynamics::Vector reverse_left(0, 1, 0);
	Dynamics::Vector reverse_up(0, 0, 1);
	// Release yaw rotation
	this_front = this_front.rotate(this_up, -this->yaw);
	this_left = this_left.rotate(this_up, -this->yaw);
	this_up = this_up.rotate(this_up, -this->yaw);
	reverse_front = reverse_front.rotate(this_up, -this->yaw);
	reverse_left = reverse_left.rotate(this_up, -this->yaw);
	reverse_up = reverse_up.rotate(this_up, -this->yaw);
	// Release pitch rotation
	this_front = this_front.rotate(this_left, -this->pitch);
	this_left = this_left.rotate(this_left, -this->pitch);
	this_up = this_up.rotate(this_left, -this->pitch);
	reverse_front = reverse_front.rotate(this_left, -this->pitch);
	reverse_left = reverse_left.rotate(this_left, -this->pitch);
	reverse_up = reverse_up.rotate(this_left, -this->pitch);
	// Release roll rotation
	this_front = this_front.rotate(this_front, -this->roll);
	this_left = this_left.rotate(this_front, -this->roll);
	this_up = this_up.rotate(this_front, -this->roll);
	reverse_front = reverse_front.rotate(this_front, -this->roll);
	reverse_left = reverse_left.rotate(this_front, -this->roll);
	reverse_up = reverse_up.rotate(this_front, -this->roll);
	return Dynamics::Posture(reverse_front, reverse_up);
}

Dynamics::Posture Dynamics::Posture::operator+(const Dynamics::Posture& posture)const // Synthesize rotations
{
	Dynamics::Vector base_front(1, 0, 0);
	Dynamics::Vector base_left(0, 1, 0);
	Dynamics::Vector base_up(0, 0, 1);
	Dynamics::Vector synthetic_front(this->front);
	Dynamics::Vector synthetic_left(this->left);
	Dynamics::Vector synthetic_up(this->up);
	// Apply roll rotation
	base_front = base_front.rotate(base_front, posture.roll);
	base_left = base_left.rotate(base_front, posture.roll);
	base_up = base_up.rotate(base_front, posture.roll);
	synthetic_front = synthetic_front.rotate(base_front, posture.roll);
	synthetic_left = synthetic_left.rotate(base_front, posture.roll);
	synthetic_up = synthetic_up.rotate(base_front, posture.roll);
	// Apply pitch rotation
	base_front = base_front.rotate(base_left, posture.pitch);
	base_left = base_left.rotate(base_left, posture.pitch);
	base_up = base_up.rotate(base_left, posture.pitch);
	synthetic_front = synthetic_front.rotate(base_left, posture.pitch);
	synthetic_left = synthetic_left.rotate(base_left, posture.pitch);
	synthetic_up = synthetic_up.rotate(base_left, posture.pitch);
	// Apply yaw rotation
	base_front = base_front.rotate(base_up, posture.yaw);
	base_left = base_left.rotate(base_up, posture.yaw);
	base_up = base_up.rotate(base_up, posture.yaw);
	synthetic_front = synthetic_front.rotate(base_up, posture.yaw);
	synthetic_left = synthetic_left.rotate(base_up, posture.yaw);
	synthetic_up = synthetic_up.rotate(base_up, posture.yaw);
	return Dynamics::Posture(synthetic_front, synthetic_up);
}

Dynamics::Posture Dynamics::Posture::operator-(const Dynamics::Posture& posture)const // Synthesize reverse rotation
{
	return *this + -posture;
}

Dynamics::Vector Dynamics::Posture::operator*(const Dynamics::Vector& vector)const // Apply rotation to vector
{
	Dynamics::Vector base_front(1, 0, 0);
	Dynamics::Vector base_left(0, 1, 0);
	Dynamics::Vector base_up(0, 0, 1);
	Dynamics::Vector rotated_vector = vector;
	// Apply roll rotation
	base_front = base_front.rotate(base_front, this->roll);
	base_left = base_left.rotate(base_front, this->roll);
	base_up = base_up.rotate(base_front, this->roll);
	rotated_vector = rotated_vector.rotate(base_front, this->roll);
	// Apply pitch rotation
	base_front = base_front.rotate(base_left, this->pitch);
	base_left = base_left.rotate(base_left, this->pitch);
	base_up = base_up.rotate(base_left, this->pitch);
	rotated_vector = rotated_vector.rotate(base_left, this->pitch);
	// Apply yaw rotation
	base_front = base_front.rotate(base_up, this->yaw);
	base_left = base_left.rotate(base_up, this->yaw);
	base_up = base_up.rotate(base_up, this->yaw);
	rotated_vector = rotated_vector.rotate(base_up, this->yaw);
	return vector;
}

Dynamics::State::State(const Dynamics::Coordinates& coordinates, const Dynamics::Posture& posture):coordinates(coordinates), posture(posture)
{
}

Dynamics::State::State(double x, double y, double z, double roll, double pitch, double yaw):coordinates(x, y, z), posture(roll, pitch, yaw)
{
}

Dynamics::State::State(const State& state):coordinates(state.coordinates), posture(state.posture)
{
}

Dynamics::State::~State()
{
}

Dynamics::Coordinates Dynamics::State::get_coordinates()const
{
	return coordinates;
}

Dynamics::Posture Dynamics::State::get_posture()const
{
	return posture;
}

Dynamics::State Dynamics::State::operator+()const // Identity map
{
	return *this;
}

Dynamics::State Dynamics::State::operator-()const // Reverse state
{
	return Dynamics::State((-this->posture) * (-this->coordinates), -this->posture);
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

std::ostream& operator<<(std::ostream &ostream, const Dynamics::State &state)
{
	ostream << "(coordinates = " << state.get_coordinates() << ", posture = " << state.get_posture() << ")";
	return ostream;
}

