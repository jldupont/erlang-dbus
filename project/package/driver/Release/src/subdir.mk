################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CC_SRCS += \
../src/egress.cc \
../src/erlang_dbus_driver.cc \
../src/ingress.cc \
../src/logger.cc 

OBJS += \
./src/egress.o \
./src/erlang_dbus_driver.o \
./src/ingress.o \
./src/logger.o 

CC_DEPS += \
./src/egress.d \
./src/erlang_dbus_driver.d \
./src/ingress.d \
./src/logger.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.cc
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	g++ -I../include -I/usr/include/dbus-1.0 -I/usr/lib/dbus-1.0/include -O3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


