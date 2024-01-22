import json

# file1 = open("A_people.json")
# A_people = file1

# file2 = open("B_people.json")
# B_people = file2

# file3 = open("C_people.json")
# C_people = file3

# file4 = open("D_people.json")
# D_people = file4

# file5 = open("E_people.json")
# E_people = file5

# file6 = open("F_people.json")
# F_people = file6

# file7 = open("G_people.json")
# G_people = file7

# file8 = open("H_people.json")
# H_people = file8

# file9 = open("I_people.json")
# I_people = file9

# file10 = open("J_people.json")
# J_people = file10

# file11 = open("K_people.json")
# K_people = file11

# file12 = open("L_people.json")
# L_people = file12

# file13 = open("M_people.json")
# M_people = file13

# file14 = open("N_people.json")
# N_people = file14

# file15 = open("O_people.json")
# O_people = file15

# file16 = open("P_people.json")
# P_people = file16

# file17 = open("Q_people.json")
# Q_people = file17

# file18 = open("R_people.json")
# R_people = file18

# file19 = open("S_people.json")
# S_people = file19

# file20 = open("T_people.json")
# T_people = file20

# file21 = open("U_people.json")
# U_people = file21

# file22 = open("V_people.json")
# V_people = file22

# file23 = open("W_people.json")
# W_people = file23

# file24 = open("X_people.json")
# X_people = file24

# file25 = open("Y_people.json")
# Y_people = file25

# file26 = open("Z_people.json")
# Z_people = file26


letters = ("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")

for letter in letters:
    file = open(f"{letter}_people.json")
    people = file

    for i in range(len(people)):
        if people[i]["ontology/birthPlace_label"] 