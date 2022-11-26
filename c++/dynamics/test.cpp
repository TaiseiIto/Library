#include <iostream>
#include "dynamics.hpp"

int main(void)
{
	Dynamics::Vector vector_a(1, 2, 3);
	Dynamics::Vector vector_b(3, 2, 1);
	Dynamics::Plane plane_a(Dynamics::Coordinates(1, 0, 0), Dynamics::Coordinates(0, 1, 0), Dynamics::Coordinates(0, 0, 1));
	Dynamics::Plane plane_b(Dynamics::Coordinates(1, 2, 3), Dynamics::Coordinates(4, 5, 6), Dynamics::Coordinates(9, 8, 7));
	std::cout << "vector_a = " << vector_a << std::endl;
	std::cout << "vector_b = " << vector_b << std::endl;
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
	return EXIT_SUCCESS;
}

