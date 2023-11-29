# Calculate restuarant bill

basic_meal = input("Basic meal price: ")

try:
    basic_meal = int(basic_meal)
except:
    print("Input is incorrect.\nPlease try again.")
    exit 

sale_tax = 0.07 * basic_meal # off basic meal
tip = 0.2 * (basic_meal + sale_tax) # off sum of basic meal and sale tax

bill = basic_meal + sale_tax + tip 

print("Thanks for dinning with us @ SuperbMeals\nMeal: {0}\nTax: {1}\nTip: {2}\nTotal: {3}\nWe hope to see you soon.".format(basic_meal, round(sale_tax, 2), round(tip, 2), bill))

