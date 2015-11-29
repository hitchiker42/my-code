#/usr/bin/local/python3
"""Section of sort.py, just used for renaming files, as bash really is better
at sorting things, but this still works for renaming"""
#Tucker DiNapoli, Created: 8/11/12; Last Modified 9/3/13
import itertools,os,argparse,re,unittest#try to only import needed fxns
from sys import argv,stdin,stdout
from subprocess import call,check_output

def pick(choice,n=3,message=None):
    """ask user for input, if a value from choice return true or false, else ask for input again up to n times"""
    y=choice[0];n=choice[1]
    while(n>0):
        ans=stdin.read(1)
        if ans==y:
            return(True)
        elif ans==n:
            return(False)
        else:
            if message!=None:
                print(message)
            n-=1
    return(False)

def rename_fxn(args):
    path=args.dir
    std=args.std
    ex=args.extra
    depth=args.depth
    """rename files according to a standard set of rules and/or user specified rules"""
    accept=['-','_','.','[',']']#allowed non alphanumeric chars
    if std==False and ex==None:#if no rules to rename files do nothing
        return
#define what constitutes a good filename
    def good(name):
        """if name is an allowable file name return true, else return false"""
        name=name.strip('~#')#ok if names starts or ends with ~ or #
        for i in accept:
            name.replace('',i)
        if name.isalnum():
            return(True)
        else:
            return(False)
#define standard rules
    def std_rules(name):
        """Take a name and return a cleaned up version of it"""
        accept=['-','_','.','[',']']
        ws_del=accept[:]
        ws_del.extend([')','(','{','}','<','>'])
        if good(name):
            return(name)
        name=list(name.strip())
        good_chr=[]
        for i,x in enumerate(name):
            accept=['-','_','.','[',']']
            if x in accept or x.isalnum():
                good_chr.append(x)
            elif x in ('(',"{",'<'):
                good_chr.append('[')
            elif x in (')',"}",'>'):
                good_chr.append(']')
            elif x is ' ':
                if name[i-1] in ws_del or name[i+1] in ws_del:
                    continue
                else:
                    good_chr.append('_')
            else:
                continue
        name_new=''.join(iter(good_chr));
        return(name_new)
#back to main
    if not (ex==None):
        for i,rule in enumerate(ex):
            #run through the rules
            if std==True:
                pass
            #run std rules too
    ls=check_output(['ls',path]).decode().splitlines()#get files
    lsn=[]
    for n in ls:
        lsn.append(std_rules(n))#get new names, haven't actually changed anything yes
    if lsn==ls:#if nothing would be renamed then exit
        print("no files need to be renamed, goodbye")
        return
    pairs=list(zip(ls,lsn))
    for i in pairs:
        print(i[0],'\t',i[1])
    print('above are the old names and proposed new names.\nwould you like to replace the old names with the new ones(y/n)')
    x=stdin.read(1)
    if x is  'y':
        for i in pairs:
            if i[0]==i[1]:
                continue
            call(['mv','-T',"{}".format('/'.join([path,i[0]])),"{}".format('/'.join([path,i[1]]))])
    else:
        return
    #Seems I wrote a test...i think its ok
    try:
        for i in lsn:
            assert(os.access('/'.join([path,i]),os.F_OK))
        print('\nFiles successfully renamed\nGoodbye')
    except AssertionError:
        try:
            for i in ls:
                assert(os.access('/'.join([path,i]),os.F_OK))
            print("Renaming has failed, all files exist with their original names")
        except AssertionError:
            print("Somthing has gone horribly wrong if you get this message")
    finally:
        return

#for the record I could make this work using md5 sums
#def rm_dupes(path):
#    os.mkdir("copies")
#    ls=check_output(['ls',path]).decode().splitlines()
#    test=ls.copy()
#    copies=[]
#    while not (test==[]):
#        x=test.pop()
#        if x in test:
#            copies.append(x)
#check if (#) in x that should be a copy
#class tests(unittest.TestCase):
    #make a temp dir to test in
    #os.mkdir('/'.join([str(pwd),'temp_sort'])
    #fill it with some random stuff
    #names='temp file names'
    #ext='some random extensions'
    #for i in temp:
    #    for j in ext:
    #        call(['touch','{}.{}'.format(i,j)
    #run each fxn in temp, repopulate if needed insure that what i say is correct actually is

#parse command line arguments
pwd=os.getcwd()
parser=argparse.ArgumentParser(description='mass rename files so they have no syaces or other special characters')
subparsers=parser.add_subparsers()
rename=subparsers.add_parser('rename',aliases=['r'],description="rename files in given directory according to a set of predefined rules or user given rules")
rename.add_argument('dir',help="directory of files to rename")
rename.add_argument('-s','--std',action='store_false',help="do not use standard rules")
rename.add_argument('--extra','-e',help='extra rules, given as a list of rules each enclosed in quotes. the rules must take a character as an arguement and return a true/false value')
rename.add_argument('--depth','-d',type=int,help='number of subdirectorys to recurse into when renaming files default is to just rename files in the current directory')
rename.set_defaults(func=rename_fxn)
args=parser.parse_args()

if __name__=='__main__':
    args.func(args)
