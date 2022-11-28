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
	Dynamics::State state_a(vector_a, posture_a);
	Dynamics::State state_b(6, 5, 4, 3, 2, 1);
	Dynamics::State state_c = state_b.to_absolute(state_a);
	Dynamics::State state_d = state_c.to_relative(state_a);
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
	std::cout << "posture_a - posture_a = " << (posture_a - posture_a) << std::endl;
	std::cout << "posture_a - posture_b = " << (posture_a - posture_b) << std::endl;
	std::cout << "posture_a - posture_b + posture_b = " << (posture_a - posture_b + posture_b) << std::endl;
	std::cout << "posture_a - posture_b + posture_b - posture_a = " << (posture_a - posture_b + posture_b - posture_a) << std::endl;
	std::cout << "state_a = " << state_a << std::endl;
	std::cout << "state_b = " << state_b << std::endl;
	std::cout << "state_c = " << state_c << std::endl;
	std::cout << "state_d = " << state_d << std::endl;
	std::cout << "-state_a = " << -state_a << std::endl;
	std::cout << "state_a - state_a = " << state_a - state_a << std::endl;
	std::cout << "state_a + state_b = " << state_a + state_b << std::endl;
	std::cout << "state_a - state_b = " << state_a - state_b << std::endl;
	// Face each other test
	Dynamics::Coordinates a_position(0, 0, 0);
	Dynamics::Vector a_front(1, 1, 1);
	Dynamics::Vector a_left(-1, 1, 0);
	Dynamics::Vector a_up = a_front * a_left;
	Dynamics::Posture a_posture(a_front, a_up);
	Dynamics::State a_state(a_position, a_posture);
	Dynamics::Coordinates b_position(1, 1, 1);
	Dynamics::Vector b_front(-1, -1, -1);
	Dynamics::Vector b_left(1, -1, 0);
	Dynamics::Vector b_up = b_front * b_left;
	Dynamics::Posture b_posture(b_front, b_up);
	Dynamics::State b_state(b_position, b_posture);
	std::cout << "a_state = " << a_state << std::endl;
	std::cout << "b_state = " << b_state << std::endl;
	std::cout << "a_state.to_relative(a_state) = " << a_state.to_relative(a_state) << std::endl;
	std::cout << "b_state.to_relative(a_state) = " << b_state.to_relative(a_state) << std::endl;
	return EXIT_SUCCESS;
}

