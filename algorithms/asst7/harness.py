#!/usr/bin/env python2

import os
import sys
import argparse

import subprocess
import re

#argument parsing
a = argparse.ArgumentParser(description='validate parses for parsing assignment')
a.add_argument("--binary", help='binary to use', default = './parser', dest="bin")
a.add_argument("--ref", help='reference solution to use', default = './parser_reference', dest="ref")
a.add_argument("--sentence", help='sentence to try and parse', default = 'sentence', dest="sen")
a.add_argument("--grammar", help='grammar to use', default = 'fish.cnf', dest="grammar")
a.add_argument("--grad", help='whether or not to use graduate student mode', default = False, action="store_true", dest="grad")

arguments = a.parse_args()

#running student solution
command = [arguments.bin, arguments.grammar, "cyk"]
#make the input from a file
sentence = open(arguments.sen, "r")
student_p = subprocess.Popen(command, stdin=sentence, stdout=subprocess.PIPE)
sentence.close();
student_p.wait()
student_output = student_p.communicate()[0]

if not arguments.grad:
    student_output = re.sub("\.|\d+", "", student_output)

#running reference solution
student_output = student_output.split('\n')
command = [arguments.ref, arguments.grammar, "cyk"]
sentence = open(arguments.sen, "r")
reference_p = subprocess.Popen(command, stdin=sentence, stdout=subprocess.PIPE)
reference_p.wait()
sentence.close();
reference_output = reference_p.communicate()[0]
#for undergraduates, the probability should be stripped from the solution.
if not arguments.grad:
    reference_output = re.sub("\.|\d+", "", reference_output)
reference_output = reference_output.split('\n')


if not arguments.grad:
    #checking for lines in the student output that arent in the reference output
    for student_parse in student_output :
        if not student_parse in reference_output:
            print("error: student output contained this parse that reference lacked")
            print(student_parse)

    #checking for lines in the reference output that aren't in the reference output
    for reference_parse in reference_output :
        if not reference_parse in student_output:
            print("error: reference output contained this parse that student lacked")
            print(reference_parse)
#done undergraduate validation
else:
    new_reference = []
    #identify the most probable parses (plural because there might be a tie)
    for reference_parse in reference_output:
        probability = (re.findall("\d+.\d+", reference_parse))
        if probability:
            reference_parse = re.sub("\.|\d+","", reference_parse)
            new_reference.append((reference_parse, float(probability[0])))
    new_reference = sorted(new_reference, key=lambda o:o[1])
    new_reference.reverse()
    probability = new_reference[0]
    probability = probability[1]
    #find the parse in the list of parses
    student_parse = re.sub("\.|\d","", student_output[0])
    matches = [i for i, v in enumerate(new_reference) if v[0] == student_parse]
    matches = matches[0]
    student_prob = (new_reference[matches])[1]
    print student_prob
    if not student_prob == probability:
        print student_parse
        print "Error - incorrect parse selected"
        for p in new_reference:
            if p[1] == probability:
                print p[0] + " with probability " + str(p[1])

#    print(new_reference)

#done everything
print("Done")
