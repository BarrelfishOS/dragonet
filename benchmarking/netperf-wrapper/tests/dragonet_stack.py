## -*- mode: python; coding: utf-8 -*-

# This file contains all the information regarding running the dragonet stack
# It assumes access to GLOBAL variables for some information
#   Currently used global variables
#   -

onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"

dragonet_container_old = {
                "llvmSF":   ["./scripts/pravin/deployDragonetGen.sh", "stack-sf" , "sf"],
                "llvmE10k": ["./scripts/pravin/deployDragonetGen.sh", "stack-e10k", "e10k"],
                #"dpdk": ["./scripts/pravin/deployDragonetGen.sh", "stack-dpdk", "dpdk"],
                "dpdk2": ["./scripts/pravin/deployDragonetGen.sh", "stack-dpdk2", "dpdk"],
                }


gen_deploy = "sudo ./scripts/pravin/deployDragonetGen.sh"
gen_is_app_ready = "./scripts/pravin/wait_for_dn_app.sh"
dragonet_dir = "dragonet/Dragonet/"

#app_ready_event_count = len(FLOWS)
app_ready_event_count = 1

def dragonet_container_gen(sname):
    return {
            'base_dir'              : dragonet_dir,
            'deploy_stack'          : ("%s bg %s" % (gen_deploy, sname)),
            'deploy_stack_cmd'      : ("%s bg %s %d %s" % (
                                            gen_deploy,
                                            sname,
                                            HWQUEUES,
                                            #"priority"
                                            "balance"
                                            )),
            'stack_process_name'    : ("stack-%s" % (sname)),
            'is_stack_ready'        : ("%s stack-%s" % (gen_is_app_ready, sname)),
            'is_stack_ready_cmd'    : ("%s stack-%s %d" % (
                                            gen_is_app_ready,
                                            sname,
                                            HWQUEUES
                                            # this command still needs app-endpoints and app-name
                                            # Which will be provided by the caller
                                            )),
            'is_ready_wait_events'  :  app_ready_event_count,
           }

dragonet_container = {
                "llvmSF": dragonet_container_gen('sf'),
                "llvmE10k": dragonet_container_gen('e10k'),
                "dpdk2": dragonet_container_gen('dpdk2')
                }

def get_stack_cmd(name):
    # Ugly hack to overload SERVER_CORESHIFT function to decide
    #   if we should start the stack
    if SERVER_CORESHIFT == 5:
        return ""

    # run stack
    cmd = ("cd %s ; " % (dragonet_container[name]['base_dir'])
            + "sudo %s " % (get_isolation_container(is_server=False))
            + " %s %d %s " % (
                dragonet_container[name]['deploy_stack'],
                HWQUEUES, COSTFN)
            + " %s %d %d " % (ORACLE, CONCURRENCY,
                len(client_names)))
    return cmd


