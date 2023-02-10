setwd("...")

mylist <- list(quants = c(0,0,0), # vector with 3 entries
               loc = 0,           # one number
               spread = 0,        # one number
               stud_no = c(2999999,3000000)) # vector of your (and your partner's) student number
                # If you don't have a partner for some reason, set the second entry of the vector to 0.
save(mylist, file="myfile1.RData")

# To check the correct format of mylist:
# (Note: this does not say anything about the correctness of the entries!)
if(length(mylist$quants)==3) print("quants has the right length.") else print("Error in quants.")
if(length(mylist$loc) == 1) print("loc has the right length.") else print("Error in loc")
if(length(mylist$spread) == 1) print("spread has the right length.") else print("Error in spread")
if(length(mylist$stud_no) == 2) print("stud_no has the right length.") else print("Error in stud_no")

