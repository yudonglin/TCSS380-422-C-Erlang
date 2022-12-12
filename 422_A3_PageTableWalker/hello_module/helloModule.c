#include<linux/string.h>
#include<linux/module.h>
#include<linux/kernel.h>
#include<linux/sched.h>
#include<linux/pid.h>
#include<linux/pid_namespace.h>
#include<asm/uaccess.h>
#include<linux/fs.h> 
#include<linux/cdev.h>
#include<linux/proc_fs.h>
#include<linux/init.h>
#include<linux/list.h>
#include<linux/seq_file.h>
#include<linux/slab.h>

/** A struct data for proc report output */
typedef struct __data
{
	int pid;
	char* name;
	struct list_head list;
    int contig;
    int noncon;
} data;

/** A data pointer for proc report */
static data* children;

/** A counter for total continue pages */
static int totalContig;

/** A counter for total non-continue pages */
static int totalNoncon;

/** Declare function prototypes */
int proc_init(void);
int iterateList(data* theData);
int showConsole(data* procList);
int writeReport(struct seq_file* seqFile, void* theVoid);
int openProc(struct inode* inode, struct file* file);
static const struct file_operations procOperations;
void proc_cleanup(void);

// Initialize the proc
// pre: NULL
// post: int
int proc_init(void)
{
	/** kmalloc list and initialize data struct */
	data* list;	
	list = kmalloc(sizeof(data), GFP_KERNEL);
    list->contig = 0;
    list->noncon = 0;
	INIT_LIST_HEAD(&list->list);

	iterateList(list);
	showConsole(list);
	children = list;
	proc_create("proc_report", 0, NULL, &procOperations);
	return 0;
}

// Virtual address to physical address
// pre: struct mm_struct*, unsigned long
// post: unsigned long
unsigned long virt2phys(const struct mm_struct *mm, unsigned long virt) 
{
	/** Store the physical address */
  	unsigned long phys = 0;
	pgd_t *pgd;
	pud_t *pud;
	pmd_t *pmd;
	pte_t *pte;
	struct page* page;
	pgd = pgd_offset(mm, virt);

	if (pgd_none(*pgd) || pgd_bad(*pgd))
		return 0;

	pud = pud_offset(pgd, virt);

	if (pud_none(*pud) || pud_bad(*pud))
		return 0;

	pmd = pmd_offset(pud, virt);

	if (pmd_none(*pmd) || pmd_bad(*pmd))
		return 0;

	if (!(pte = pte_offset_map(pmd, virt)))
		return 0;

	if (!(page = pte_page(*pte)))
		return 0;

	phys = page_to_phys(page);
	pte_unmap(pte);
	return phys;
}

// Iterate entire list to count all children and find out 1st child
// pre: data*
// post: int
int iterateList(data* theData)
{
	struct task_struct* task;
	struct vm_area_struct *vma = 0;
	/** a temp to store address for next iteration comparison */
	unsigned long vpage, tmp = 0;

	for_each_process(task)
	{
		data* curData;
		curData = kmalloc(sizeof(data), GFP_KERNEL);
		list_add_tail(&curData->list, &theData->list);
		curData->pid = task->pid;
		curData->name = task->comm;
		curData->contig = 0;
		curData->noncon = 0;
		/** Asked by the instructor */
		if (curData->pid > 650) 
		{
			if (task->mm && task->mm->mmap)
			{
				for (vma = task->mm->mmap; vma; vma = vma->vm_next)
				{
					/** Reset the tmp */
					tmp = 0;
					for (vpage = vma->vm_start; vpage < vma->vm_end; vpage += PAGE_SIZE)
					{
					 	unsigned long phys = virt2phys(task->mm, vpage);
					
						if (phys != 0)
						{
							/** It's a continue page */
							if (phys == tmp + PAGE_SIZE) curData->contig++;
						   	/** It's a non-continue page */
							else curData->noncon++;
							/** Update tmp */
							tmp = phys;
						}
					}
				}
				/** Update total continue / non-continue pages */
				totalContig += curData->contig;
				totalNoncon += curData->noncon;
			}
		}
	}
	return 0;
}

// Show result on console
// pre: data*
// post: int
int showConsole(data* procList)
{
	data* cur, *next;
	printk(KERN_INFO "PROCESS REPORT:");
	printk(KERN_INFO "proc_id,proc_name,contig_pages,noncontig_pages,total_pages");

	list_for_each_entry_safe(cur, next, &procList->list, list)
	{
		/** Asked by the instructor */
		if (cur->pid > 650)
		{
			printk(KERN_INFO "%d,%s,%d,%d,%d\n", cur->pid, cur->name, cur->contig, cur->noncon, cur->contig+cur->noncon);
		}		
	}
	printk(KERN_INFO ",,%d,%d,%d\n", totalContig, totalNoncon, totalContig+totalNoncon);
	return 0;
}

// Write report to proc
// pre: seq_file*, void*
// post: int
int writeReport(struct seq_file* seqFile, void* theVoid)
{
	data* cur, *next;
	data* procList = children;
	seq_printf(seqFile, "PROCESS REPORT: \n");
	seq_printf(seqFile, "proc_id,proc_name,contig_pages,noncontig_pages,total_pages\n");
    
	list_for_each_entry_safe(cur, next, &procList->list, list)
	{
		/** Asked by the instructor */
		if (cur->pid > 650)
		{
			seq_printf(seqFile, "%d,%s,%d,%d,%d\n", cur->pid, cur->name, cur->contig, cur->noncon, cur->contig+cur->noncon);
		}
	}
	seq_printf(seqFile, ",,%d,%d,%d\n", totalContig, totalNoncon, totalContig+totalNoncon);
    return 0;
}

// Open proc file
// pre: inode*, file*
// post: int
int openProc(struct inode* inode, struct file* file)
{
    return single_open(file, writeReport, NULL);
}

// struct file operations
static const struct file_operations procOperations = {
    .open       = openProc,  // open proc
    .read       = seq_read,
    .llseek     = seq_lseek,
    .release    = single_release,
    .owner      = THIS_MODULE,
};

// Cleanup proc
// pre: NULL
// post: NULL
void proc_cleanup(void)
{
    // kfree for kmalloc  
  	data* cur, *next;
    // remove proc entry
	remove_proc_entry("proc_report",NULL); 

	list_for_each_entry_safe(cur, next, &children->list, list)
	{				
		list_del(&cur->list);
		kfree(cur);
	}
}

MODULE_LICENSE("GPL");
module_init(proc_init);
module_exit(proc_cleanup);