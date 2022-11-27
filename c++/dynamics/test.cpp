#include <cmath>
#include <iostream>
#include "dynamics.hpp"

int main(void)
{
	Dynamics::Vector vector_a(1, 2, 3);
	Dynamics::Vector vector_b(3, 2, 1);
	Dynamics::Vector vector_c(1, 0, 0);
	Dynamics::Vector vector_d = vector_a * vector_b;
	Dynamics::Plane plane_a(Dynamics::Coordinates(1, 0, 0), Dynamics::Coordinates(0, 1, 0), Dynamics::Coordinates(0, 0, 1));
	Dynamics::Plane plane_b(Dynamics::Coordinates(1, 2, 3), Dynamics::Coordinates(4, 5, 6), Dynamics::Coordinates(9, 8, 7));
	Dynamics::Posture posture_a(4, 5, 6);
	Dynamics::Posture posture_b(vector_a, vector_d);
	std::cout << "vector_a = " << vector_a << std::endl;
	std::cout << "vector_b = " << vector_b << std::endl;
	std::cout << "vector_c = " << vector_c << std::endl;
	std::cout << "vector_d = " << vector_d << std::endl;
	std::cout << "+vector_a = " << +vector_a << std::endl;
	std::cout << "-vector_a = " << -vector_a << std::endl;
	std::cout << "vector_a + vector_b = " << (vector_a + vector_b) << std::endl;
	std::cout << "vector_a - vector_b = " << (vector_a - vector_b) << std::endl;
	std::cout << "vector_a, vector_b = " << (vector_a, vector_b) << std::endl;
	std::cout << "vector_a * vector_b = " << (vector_a * vector_b) << std::endl;
	std::cout << "vector_a / vector_b = " << (vector_a / vector_b) << std::endl;
	std::cout << "!vector_a = " << !vector_a << std::endl;
	std::cout << "*vector_a = " << *vector_a << std::endl;
	std::cout << "plane_a = " << plane_a << std::endl;
	std::cout << "vector_a / plane_a = " << (vector_a / plane_a) << std::endl;
	std::cout << "plane_a / vector_a = " << (plane_a / vector_a) << std::endl;
	std::cout << "plane_a / plane_b = " << (plane_a / plane_b) << std::endl;
	std::cout << "vector_a > plane_a = " << (vector_a > plane_a) << std::endl;
	std::cout << "!((vector_a > plane_a) - plane_a) = " << !((vector_a > plane_a) - plane_a) << std::endl;
	std::cout << "(vector_a - (vector_a > plane_a)) / plane_a.get_normal() = " << ((vector_a - (vector_a > plane_a)) / plane_a.get_normal()) << std::endl;
	std::cout << "vector_a >> plane_a = " << (vector_a >> plane_a) << std::endl;
	std::cout << "(vector_a >> plane_a) / plane_a = " << ((vector_a >> plane_a) / plane_a) << std::endl;
	std::cout << "vector_c.rotate(plane_a.get_normal(), 2 * M_PI / 3) = " << vector_c.rotate(plane_a.get_normal(), 2 * M_PI / 3) << std::endl;
	std::cout << "posture_a = " << posture_a << std::endl;
	std::cout << "posture_b = " << posture_b << std::endl;
	std::cout << "-posture_a = " << -posture_a << std::endl;
	std::cout << "-posture_a + posture_a = " << (-posture_a + posture_a) << std::endl;
	return EXIT_SUCCESS;
}

