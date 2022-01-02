#sample#workign great
import simpy

def main():
    env = simpy.Environment()  # to define a environment (all process takes place in one environment)
    env.process(traffic_light(env)) #definining a process/setup the model
    env.run(until=120) #run for 120 sec (2 min) - as while=set to true and will runindefineetly
    print("simulation complete")
    print(simpy.Resource(env,1))#function object

def traffic_light(env):
    while True:
        print("light turned Green at ", str(env.now))
        yield env.timeout(30) #processes waiting for 30 sec(software doesnt actually wait. it jumps to the next events)
        print("light turned yellow at ", str(env.now))
        yield env.timeout(5) # light stays in yellow for 5 sec
        print("light turned red at ", str(env.now))
        yield env.timeout(10)  # light stays in red for 10 sec

if __name__=='__main__':
    main()

#from results the simulation stops at 90 as 90+30 more sec from green to yellow is over the limit of run time of 120 set