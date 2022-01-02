#
#Question 13.2

#In this problem you, can simulate a simplified airport security system at a busy airport.
# #Passengers arrive according to a Poisson distribution with λ1 = 5 per minute (i.e., mean interarrival rate 1 = 0.2 minutes) to the ID/boarding-pass check queue,
# #where there are several servers who each have exponential service time with mean rate 2 = 0.75 minutes.
# #[Hint: model them as one block that has more than one resource.]
# #After that, the passengers are assigned to the shortest of the several personal-check queues,
# #where they go through the personal scanner (time is uniformly distributed between 0.5 minutes and 1 minute).

#Use the Arena software (PC users) or Python with SimPy (PC or Mac users) to build a simulation of the system,
# #and then vary the number of ID/boarding-pass checkers and personal-check queues to determine
# #how many are needed to keep average wait times below 15 minutes.
# #[If you’re using SimPy, or if you have access to a non-student version of Arena, you can use λ1 = 50 to simulate a busier airport.]

# Import Libray

# import SimPy module
import simpy

# Import random module
import random # probability distributions

# Set constants based on requirement

boardcheck = 20  # number of boarding-pass checkers
Scanners = 20  # number of scanners

arrival = 45  # arrival rate (passengers per minute)
checkRate = 0.75  # boarding-pass check rate (minutes per passenger)
minScan = 0.5  # scanner minimum time for uniform distribution
maxScan = 1.0  # scanner maximum time for uniform distribution
runTime = 500  # run time (minutes) per simulation
replications = 30  # number of replications

#Initialize  variables

avgCheckTime = []  # average boarding-pass check time (for each replication)
avgScanTime = []  # average scan time (for each replication)
avgWaitTime = []  # average total wait time (for each replication)
avgSystemTime = []  # average total time in system (for each replication)


# Create model
# Setup Entities

class Entity(object):
    def __init__(self, env):
        self.env = env
        self.checker = simpy.Resource(env, boardcheck)  # define number of boarding-pass checkers
        self.scanner = []  # define a set of scanners with 1 each; needed because each has its own queue
        for i in range(Scanners):
            self.scanner.append(simpy.Resource(env, 1))

    # define boarding-pass check time (exponential)
    def check(self, passenger):
        # expovariate actually uses 1 over the mean, like Poisson
        yield self.env.timeout(random.expovariate(1.0 / checkRate))# using yield as simpy is a generator

    # define scan time (uniform)
    def scan(self, passenger):
        yield self.env.timeout(random.uniform(minScan, maxScan))


# Passenger process through system

def passenger(env, name, s):
    # access  variables to be able to modify them
    global checkWait
    global scanWait
    global sysTime
    global totThrough

    timeArrive = env.now  # note arrival time of passenger(in sec)

    # print('%s arrives at time %.2f' % (name,timeArrive))

    # Go through boarding-pass check queue
    with s.checker.request() as request:
        # print('check queue length = %d' % len(s.checker.queue))
        yield request  # request a checker
        tIn = env.now  # note when passenger starts being checked
        yield env.process(s.check(name))  # call check process
        tOut = env.now  # note when passenger ends being checked
        checkTime.append(tOut - tIn)  # calculate total time for passenger to be checked

    # Find the shortest scanner queue (note: scanners are numbered 0 through Scanners-1)
    minq = 0
    for i in range(1, Scanners):
        if (len(s.scanner[i].queue) < len(s.scanner[minq].queue)):
            minq = i

    # print('scanner queue %d lengths = %d' % (minq,len(s.scanner[minq].queue)))

    # Go through scanner queue
    with s.scanner[minq].request() as request:  # use scanner number minq (the shortest, from above)
        yield request  # request the scanner
        tIn = env.now  # note when passenger starts being scanned
        yield env.process(s.scan(name))  # call scan process
        tOut = env.now  # note when passenger ends being scanned
        scanTime.append(tOut - tIn)  # calculate total time for passenger to be scanned

    timeLeave = env.now  # note time passenger finishes
    sysTime.append(timeLeave - timeArrive)  # calculate total time in system for passenger
    totThrough += 1  # count another passenger who got through the system


# Passenger arrival process

def setup(env):
    i = 0
    s = Entity(env)
    while True:  # keep doing it (until simulation ends)
        yield env.timeout(random.expovariate(arrival))  # find time until next passenger is created
        i += 1  # count one more passenger

        # send the passenger through its process
        env.process(passenger(env, 'Passenger %d' % i, s))  # name the passenger "Passenger i"


# steps to run

# for each replication
for i in range(replications):
    # choose random seed
    random.seed(i)

    # create environment (all processes are run within the environment
    env = simpy.Environment()

    # initialize global variables
    totThrough = 0
    checkTime = []
    scanTime = []
    sysTime = []

    # run the simulation
    env.process(setup(env))  # start passenger arrival process
    env.run(until=runTime)  # run for runTime simulated minutes (720)

    # Calculate average times for this replication

    avgSystemTime.append(sum(sysTime[1:totThrough]) / totThrough)
    avgCheckTime.append(sum(checkTime[1:totThrough]) / totThrough)
    avgScanTime.append(sum(scanTime[1:totThrough]) / totThrough)
    avgWaitTime.append(avgSystemTime[i] - avgCheckTime[i] - avgScanTime[i])

    print('%d : Replication %d times %.2f %.2f %.2f %.2f' % (
    totThrough, i + 1, avgSystemTime[i], avgCheckTime[i], avgScanTime[i], avgWaitTime[i]))

# Calculate overall averages across all replications

print('-----')
print('Average system time = %.2f' % (sum(avgSystemTime) / replications))
print('Average check time = %.2f' % (sum(avgCheckTime) / replications))
print('Average scan time = %.2f' % (sum(avgScanTime) / replications))
print('Average wait time = %.2f' % (sum(avgWaitTime) / replications))