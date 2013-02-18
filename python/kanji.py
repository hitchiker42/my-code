#!/usr/bin/python3
"""Doc String"""
#Kanji Quiz, Tucker DiNapoli, Created 10/23/12, Last Modified 10/28/12
#Two seperate functions are in the program, on is a kanji quiz
#the other allows the user to add to/build a kanji dictionary
#if the argumend add is given the program will run the
#dictionary building funcion otherwize the test will be run
#the dictionary used is stored in a seperate file as a python shelve object
#There is also a simple dictionary search function
#and the ability to print the dictionary entry by entry

import shelve,argparse
from random import choice
from sys import stdin
kanji=shelve.open('Kanji')
test=len(kanji)

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

def Kanji_Test():
    """Prompts user with a random kanji from the dictionary and expects a responce that is in the values for that kanji.
       Gives up to 5 wrong guesses, if ans is blank or after too many wrong guesses the values are printed.
       The program is exited upon being given the word exit as an answer"""
    guess=''
    ans=choice(list(iter(kanji)))
    while(True):
        cnt=0
        print('\nIdentify This kanji\n\n',ans)
        guess=input()
        while(cnt>=0):
            if guess in ('exit','e'):
                return()
            elif guess=='' and cnt<5:
                cnt=5
            elif guess in kanji[ans]:
                print('\nCorrect')
                cnt=-1
            elif cnt<5:
                print('\nWrong,guess again')
                guess=input()
                cnt+=1
            else:
                print('\nSorry,try again\nThe correct answer was\n\t',ans)
                cnt=-1
    kanji.close
    return()

def Kanji_Add():
    """Function to add characters to the kanji dictionary and/or add meanings to a new or old character
       Mostly self explanitory when run, can append to an old entry or erase old entry and start anew"""
    loop=True
    while (loop):
        temp=[]
        print('\nEnter in the kanji')
        char=input()
        if char in kanji:
            print("{} is in the dictionary with meanings {} would you like to change this entry".format(char,kanji[char]))
            x=stdin.read(1)
            if x is not 'y':
                continue
            else:
                print("would you like to append meanings or enter in completely new meanings?(a/n)")
                if not pick(('a','n'),message="select a or n"):
                    kanji[char]=[]
        print('\nEnter in On readings in katakana,seperate multiple readings with commas')
        temp.append(input.split(','))
        print('\nEnter in Kun readings in hiragana with multiple readings seperated by commas')
        temp.append(input.split(','))
        kanji[char]=temp
        print("Enter in More characters?(y/n)")
        if not pick(('y','n')):
            loop=False
    print("run the test")
    if pick(('y','n')):
        Kanji_Test
    else:
        kanji.close
    return()

#Some generic bookeping functions
#Search for specific keys or specific values in a key
#Also print out dictionary entries one by one prompting after each one
#proably won't work as is

def bookeeping():
    print('\nSearch dictionary for values or keys or print out entries one by one\nTo select enter values,keys or entries, any other value will exit')
    loop=True
    x=True
    choice=input()
    while x==True:
        if choice in ('keys','k'):
            keys=kanji.keys()
            while loop==True:
                print('\nEnter value to search')
                key=input()
                if key in keys:
                    print('value for',key,'is',kanji[key])
                else:
                    print('\nContinue searching?')
                ans=input()
                if ans in ('y','yes'):
                    pass
                else:
                    break
        elif choice in ('values','v'):
            values=kanji.values()
            while loop==True:
                print('\nEnter value to search in key')
                entry=input()
                value=entry[0]
                key=entry[1]
                if value in kanji[key]:
                    print(value,'is in the key',key)
                else:
                    print(value,'is not in the key',key)
            print('\nContinue searching?')
            ans=input()
            if ans in('y','yes'):
                pass
            else:break
        elif choice in ('entries','e'):
            entries=list(iter(kanji.values()))
            i=-1
            while loop==True:
                i+=1
                print(entries[1])
                if input()!='':
                    loop=False
        else:
            kanji.close
            return()
        print('\nEnter edit to edit dictionary,enter another option or anything else to exit')
        choice=input()
        if choice in ('edit','e'):
            Kanji_Add()

parser=argparse.ArgumentParser(description='Kanji dictionary and a test to identfy characters from that dictionary')
funct=parser.add_mutually_exclusive_group(required=True)
funct.add_argument('--func',default=list, help='function to run, just a seperate value for logistics')
funct.add_argument('--test','-t',help='Run the kanji test',action='store_const',const=Kanji_Test,dest='--func')
funct.add_argument('--add','-a',help='Add or modify dictionary entires',action='store_const',const=Kanji_Add,dest='--func')
funct.add_argument('--book','-b',help='get information about the dictionary',action='store_const',const=bookeeping,dest='--func')
args=parser.parse_args()
if __name__=='__main__':
    #this should close the shelf even if program randomly terminates
    try:
    args.func()
    finally:
        try:
            kanji.close
        except:
            ValueError
        finally:
        print('\nGoodbye!\n')
