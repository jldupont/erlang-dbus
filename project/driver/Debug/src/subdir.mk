################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../src/egress.c \
../src/erlang_dbus_driver.c \
../src/ingress.c \
../src/queue.c 

OBJS += \
./src/egress.o \
./src/erlang_dbus_driver.o \
./src/ingress.o \
./src/queue.o 

C_DEPS += \
./src/egress.d \
./src/erlang_dbus_driver.d \
./src/ingress.d \
./src/queue.d 


# Each subdirectory must supply rules for building sources it contributes
src/%.o: ../src/%.c
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C Compiler'
	gcc -I/usr/include/dbus-1.0 -I/usr/lib/dbus-1.0/include -O0 -g3 -Wall -pthread -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


