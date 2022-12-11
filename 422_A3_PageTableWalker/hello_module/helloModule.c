#include<linux/module.h>
#include<linux/sched/signal.h>
#include<linux/pid_namespace.h>
#include<asm/io.h>


#define MIN_PID 650

unsigned long virt2phys(struct mm_struct *mm, unsigned long vpage) {
    pgd_t *pgd;
    p4d_t *p4d;
    pud_t *pud;
    pmd_t *pmd;
    pte_t *pte;
    struct page *page;
    unsigned long int physical_page_addr;

    pgd = pgd_offset(mm, vpage);

    if (pgd_none(*pgd) || pgd_bad(*pgd)) {
        return 0;
    }
    p4d = p4d_offset(pgd, vpage);

    if (p4d_none(*p4d) || p4d_bad(*p4d)) {
        return 0;
    }
    pud = pud_offset(p4d, vpage);
    if (pud_none(*pud) || pud_bad(*pud)) {
        return 0;
    }
    pmd = pmd_offset(pud, vpage);

    if (pmd_none(*pmd) || pmd_bad(*pmd)) {
        return 0;
    }
    if (!(pte = pte_offset_map(pmd, vpage))) {
        return 0;
    }

    if (!(page = pte_page(*pte))) {
        return 0;
    }

    physical_page_addr = page_to_phys(page);

    pte_unmap(pte);

    // handle unmapped page
    if (physical_page_addr == 70368744173568) {
        return 0;
    }
    return physical_page_addr;
}

int proc_count(void) {
    int i = 0;
    struct task_struct *task;
    for_each_process(task)
    {
        struct vm_area_struct *vma;
        unsigned long int vpage;
        unsigned long int total_pages = 0;
        if (task->pid > MIN_PID) {
            i++;
            if (task->mm && task->mm->mmap) {
                for (vma = task->mm->mmap; vma; vma = vma->vm_next) {
                    for (vpage = vma->vm_start; vpage < vma->vm_end; vpage += PAGE_SIZE) {
                        unsigned long int physical_page_addr = virt2phys(task->mm, vpage);
                        if (physical_page_addr != 0) {
                            total_pages += PAGE_SIZE;
                        }
                    }
                }
            }
            printk("%d, %s, %ld\n", task->pid, task->comm, total_pages);
        }
    }
    return i;
}

int proc_init(void) {
    printk("PROCESS REPORT:\n proc_id,proc_name,total_pages");
    printk("There are %d running processes that has a PID >%d.\n", proc_count(), MIN_PID);
    return 0;
}

void proc_cleanup(void) {
    printk(KERN_INFO
    "helloModule: performing cleanup of module\n");
}

MODULE_LICENSE("GPL");
module_init(proc_init);
module_exit(proc_cleanup);

// useful commands
//sudo rmmod ./helloModule.ko
// sudo insmod ./helloModule.ko
// tail -n 10 /var/log/syslog
