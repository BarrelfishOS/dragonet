## -*- mode: python; coding: utf-8 -*-

# This file contains all the information regarding running the dragonet stack
# It assumes access to GLOBAL variables for some information
#   Currently used global variables
#   -

onload_prefix="onload --profile=latency --preload=/usr/lib64/libonload.so"

dragonet_container_old = {
                "llvmSF":   ["./scripts/pravin/deployDragonetGen.sh", "stack-sf" , "sf"],
                "llvmE10k": ["./scripts/pravin/deployDragonetGen.sh", "stack-e10k", "e10k"],
                "dpdk": ["./scripts/pravin/deployDragonetGen.sh", "stack-dpdk", "dpdk"],
                }


gen_deploy = "sudo ./scripts/pravin/deployDragonetGen.sh"
gen_is_app_ready = "./scripts/pravin/wait_for_dn_app.sh"
dragonet_dir = "dragonet/Dragonet/"

def dragonet_container_gen(sname):
    return {
            'base_dir'              : dragonet_dir,
            'deploy_stack'          : ("%s bg %s" % (gen_deploy, sname)),
            'deploy_stack_cmd'      : ("%s bg %s %d %s" % (
                                            gen_deploy,
                                            sname,
                                            HWQUEUES,
                                            "priority"
                                            #"balance"
                                            )),
            'stack_process_name'    : ("stack-%s" % (sname)),
            'is_stack_ready'        : ("%s stack-%s" % (gen_is_app_ready, sname)),
            'is_stack_ready_cmd'    : ("%s stack-%s %d" % (
                                            gen_is_app_ready,
                                            sname,
                                            HWQUEUES
                                            # this command still needs app-endpoints and app-name
                                            # Which will be provided by the caller
                                            ))
           }

dragonet_container = {
                "llvmSF": dragonet_container_gen('sf'),
                "llvmE10k": dragonet_container_gen('e10k'),
                "dpdk": dragonet_container_gen('dpdk')
                }


