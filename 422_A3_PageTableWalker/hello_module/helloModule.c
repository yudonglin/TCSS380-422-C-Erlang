#include<linux/module.h>
#include<linux/sched/signal.h>

int proc_count(void) {
    int i = 0;
    struct task_struct *thechild;
    for_each_process(thechild)
    i++;
    return i;
}

int proc_init(void) {
    printk(KERN_INFO
    "There are %d running processes.\n", proc_count());
    return 0;
}

void proc_cleanup(void) {
    printk(KERN_INFO
    "helloModule: performing cleanup of module\n");
}

MODULE_LICENSE("GPL");
module_init(proc_init);
module_exit(proc_cleanup);

