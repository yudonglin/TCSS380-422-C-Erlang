#include<linux/module.h>
#include<linux/sched/signal.h>
#include<linux/pid_namespace.h>
#include<asm/io.h>

/*
 *
 * commands:
 * sudo rmmod ./procReport.ko
 * sudo insmod ./procReport.ko
 * tail -n 10 /var/log/syslog
 *
 */

unsigned long virt2phys(struct mm_struct *mm, unsigned long vpage);

int proc_init(void);

// Initialize the proc
// pre: NULL
// post: int
int proc_init(void) {

    /* A counter for total continue pages */
    unsigned long int totalContig = 0;

    /* A counter for total non-continue pages */
    unsigned long int totalNoncon = 0;

    /* A counter for total pages */
    unsigned long int totalPages = 0;

    /* A counter for current total continue pages */
    unsigned long int curContig = 0;

    /* A counter for current total non-continue pages */
    unsigned long int curNoncon = 0;

    /* A counter for current total pages */
    unsigned long int curTotal = 0;

    struct task_struct *task;
    struct vm_area_struct *vma;
    unsigned long int vpage;
    unsigned long int tmp;

    printk(KERN_INFO
    "PROCESS REPORT:");
    printk(KERN_INFO
    "proc_id,proc_name,contig_pages,noncontig_pages,total_pages");

    for_each_process(task)
    {
        /* Asked by the instructor */
        if (task->pid > 650) {
            if (task->mm && task->mm->mmap) {
                curContig = 0;
                curNoncon = 0;
                curTotal = 0;
                for (vma = task->mm->mmap; vma; vma = vma->vm_next) {
                    /* Reset the tmp */
                    tmp = 0;
                    for (vpage = vma->vm_start; vpage < vma->vm_end; vpage += PAGE_SIZE) {
                        unsigned long phys = virt2phys(task->mm, vpage);

                        if (phys != 0) {
                            /* Update tmp */
                            tmp += PAGE_SIZE;
                            /* It's a continue page */
                            if (phys == tmp) {
                                curContig += PAGE_SIZE;
                            }
                                /* It's a non-continue page */
                            else {
                                curNoncon += PAGE_SIZE;
                                tmp = phys;
                            }
                            curTotal += PAGE_SIZE;
                        }
                    }
                }
                printk(KERN_INFO
                "%d,%s,%ld,%ld,%ld\n", task->pid, task->comm, curContig, curNoncon, curTotal);
                /* Update total continue / non-continue pages */
                totalContig += curContig;
                totalNoncon += curNoncon;
                totalPages += curTotal;
            }
        }
    }

    printk(KERN_INFO
    "TOTAL,,%ld,%ld,%ld\n", totalContig, totalNoncon, totalPages);

    return 0;
}

// Virtual address to physical address
// pre: struct mm_struct*, unsigned long
// post: unsigned long
unsigned long virt2phys(struct mm_struct *mm, unsigned long vpage) {
    /* Store the physical address */
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

// Cleanup proc
// pre: NULL
// post: NULL
void proc_cleanup(void) {
    printk(KERN_INFO
    "procReport: performing cleanup of module\n");
}

MODULE_LICENSE("GPL");
module_init(proc_init);
module_exit(proc_cleanup);