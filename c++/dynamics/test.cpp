#include <iostream>
#include "dynamics.hpp"

int main(void)
{
	Dynamics::Vector vector_a(1, 2, 3);
	Dynamics::Vector vector_b(3, 2, 1);
	std::cout << "vector_a = " << vector_a << std::endl;
	std::cout << "vector_b = " << vector_b << std::endl;
	std::cout << "vector_a + vector_b = " << (vector_a + vector_b) << std::endl;
	std::cout << "vector_a - vector_b = " << (vector_a - vector_b) << std::endl;
	std::cout << "vector_a , vector_b = " << (vector_a , vector_b) << std::endl;
	std::cout << "vector_a * vector_b = " << (vector_a * vector_b) << std::endl;
	return EXIT_SUCCESS;
}

