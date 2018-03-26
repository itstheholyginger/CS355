# Programmer: Amariah Del Mar
# Date: 3/7/18
# Class: CptS 355
# Assignment: HW3


from functools import reduce
from collections import Counter
from inspect import getfullargspec
import math

# 1. Dictionaries
# a. addDict(d) - 10%
"""
This function takes a nested dictionary containing the amount of hours the user studied for individual classes
each day over a week and returns a dictionary of the total number of hours the user worked on each class
per week.
"""


def addDict(d):
	if d is None:  # do nothing if dict is empty
		return
	else:
		newDict = {}  # created a new dictionary which is to be returned with the result
		for day in d:  # iterates though each day in the dictionary
			other = d[day]
			for c in other:  # iterates through each class
				if c in newDict.keys():  # if that class is already in the new dict
					newDict[c] += other[c]  # adds current day's hours to the new dict
				else:
					newDict[c] = other[c]  # adds day and current day's hours to new dict
	return newDict


"""
testing addDict function
"""
def testaddDict():
	print("\nTesting addDict()")
	d = {'Mon': {'355': 2, '451': 1, '360': 2}, 'Tue': {'451': 2, '360': 3},
	     'Thu': {'355': 3, '451': 2, '360': 3}, 'Fri': {'355': 2},
	     'Sun': {'355': 1, '451': 3, '360': 1}}
	if addDict(d) != {'355': 8, '451': 8, '360': 9}:
		return False
	d = {'Mon': {'355': 1, '451': 1, '360': 1}, 'Tue': {'451': 1, '360': 1},
	     'Thu': {'355': 1, '451': 1, '360': 1}, 'Fri': {'355': 1},
	     'Sun': {'355': 1, '451': 1, '360': 1}}
	if addDict(d) != {'355': 4, '451': 4, '360': 4}:
		return False
	d = None
	if addDict(d) is not None:
		return False
	return True


# b. addDictN(L) - 10%

def add_helper(x, y):
	return x + Counter(y)


"""
This function performs the addDict function defined above on multiple weeks and returns a dictionary
with the total number of hours worked on each individual class
"""


def addDictN(L):
	if not L:
		return []
	temp = list(map(addDict, L))
	result = reduce(add_helper, temp, Counter())
	return result

"""
Tests addDictN function
"""
def testaddDictN():
	print("\nTesting addDictN()")
	d = [{'Mon': {'355': 2, '360': 2}, 'Tue': {'451': 2, '360': 3}, 'Thu': {'360': 3},
	      'Fri': {'355': 2}, 'Sun': {'355': 1}},
	     {'Tue': {'360': 2}, 'Wed': {'355': 2}, 'Fri': {'360': 3, '355': 1}},
	     {'Mon': {'360': 5}, 'Wed': {'451': 4}, 'Thu': {'355': 3}, 'Fri': {'360': 6},
	      'Sun': {'355': 5}}]
	if addDictN(d) != {'355': 16, '360': 24, '451': 6}:
		return False
	d = [{'Mon': {'355': 21, '360': 5}, 'Tue': {'451': 2, '360': 3}, 'Thu': {'360': 1},
	      'Fri': {'355': 2}, 'Sun': {'355': 1}},
	     {'Tue': {'360': 5}, 'Wed': {'355': 2}, 'Fri': {'360': 3, '355': 1}},
	     {'Mon': {'360': 3}, 'Wed': {'451': 4}, 'Thu': {'355': 3}, 'Fri': {'360': 6},
	      'Sun': {'355': 6}}]
	if addDictN(d) != {'355': 36, '360': 26, '451': 6}:
		return False
	d = [{'Mon': {'355': 1, '360': 5}, 'Tue': {'451': 1, '360': 1}, 'Thu': {'360': 1},
	      'Fri': {'355': 2}},
	     {'Tue': {'360': 5}, 'Wed': {'355': 2}, 'Fri': {'355': 1}},
	     {'Mon': {'360': 1}, 'Wed': {'451': 4}, 'Thu': {'355': 3}, 'Fri': {'360': 1},
	      'Sun': {'355': 6}}]
	if addDictN(d) != {'355': 15, '360': 14, '451': 5}:
		return False
	return True


# 2. Dictionaries nd List Comprehension
# a. charCount(s) - 5%
"""
This function takes a string and returns a dictionary containing each letter in the string
and how many times it occurred in the string
"""
def charCount(s):
	if s == '':    # returns nothing if string is empty
		return
	else:
		newDict = {}
		for char in s:      # put all vals into dictionary first
			if char in newDict.keys() and char != ' ':      # +1 if char is already in dict and makes sure to ignore spaces
				newDict[char] += 1
			else:
				if char != ' ':                             # adds char to dict with an occurrence of 1
					newDict[char] = 1
		L = []
		for item in newDict:                                # moves all items in dictionary to tuples in a list
			L.append((item, newDict[item]))
		L = sorted(L)                                       # sorts list by num of occurences, then alphabetically
		return sorted(L, key=lambda x: x[1])

"""
Testing charCount
"""
def testcharCount():
	print("\nTesting charCount()")
	if charCount('Cpts355 --- Assign1') != [('1', 1), ('3', 1), ('A', 1), ('C', 1), ('g', 1), ('i', 1), ('n', 1),
	                                        ('p', 1), ('t', 1), ('5', 2), ('-', 3), ('s', 3)]:
		return False
	if charCount("Hello world") != [('H', 1), ('d', 1), ('e', 1), ('r', 1), ('w', 1), ('o', 2), ('l', 3)]:
		return False
	if charCount("Frick this is going to be late") != [('F', 1), ('a', 1), ('b', 1), ('c', 1), ('h', 1), ('k', 1), ('l', 1), ('n', 1), ('r', 1), ('e', 2), ('g', 2), ('o', 2), ('s', 2), ('t', 3), ('i', 4)]:
		return False

	return True


# print(charCount('Cpts355 --- Assign1'))

# b. charCount2(s) - 5%
"""
Does same thing as charCount, except this function uses list comprehension
"""
def charCount2(s):
	L = [(char, s.count(char)) for char in s if char is not ' ']
	L = sorted(list(set(L)))

	return sorted(L, key=lambda x: x[1])

"""
Testing charCount2
"""
def testcharCount2():
	print("\nTesting charCount2")
	if charCount2('Cpts355 --- Assign1') != [('1', 1), ('3', 1), ('A', 1), ('C', 1), ('g', 1), ('i', 1), ('n', 1),
	                                         ('p', 1), ('t', 1), ('5', 2), ('-', 3), ('s', 3)]:
		return False
	if charCount2("Hello world") != [('H', 1), ('d', 1), ('e', 1), ('r', 1), ('w', 1), ('o', 2), ('l', 3)]:
		return False
	if charCount2("Frick this is going to be late") != [('F', 1), ('a', 1), ('b', 1), ('c', 1), ('h', 1), ('k', 1), ('l', 1), ('n', 1), ('r', 1), ('e', 2), ('g', 2), ('o', 2), ('s', 2), ('t', 3), ('i', 4)]:
		return False
	return True


# 3. List and Dictionary
# a. lookupVal(L, k) - 5%

"""
This function takes a list of dicts and a value. It searches the list to determine if the
given value occurrs in the list as a dict key. If so, it returns the key's value.
"""
def lookupVal(L, k):
	x = None
	for item in L:
		if k in item:
			x = item[k]
	return x

"""
Testing lookupVal
"""
def testlookupVal():
	print("\nTesting lookupVal()")
	L1 = [{"x": 1, "y": True, "z": "found"}, {"x": 2}, {"y": False}]
	if lookupVal(L1, "x") != 2:
		return False
	if lookupVal(L1, "y"):
		return False
	if lookupVal(L1, "z") != "found":
		return False
	if lookupVal(L1, "t") is not None:
		return False
	return True


# b. lookupVal2(tL, k) - 10%
"""
This function take a list of tuples and a key k is an input. It recursively searches for the key in the dictionaries
and if the key is found, it returns the key's value
"""
def lookupVal2(tL, k):
	if not tL:
		return None
	val = tL[-1][0]
	d = tL[-1][1]
	i = val + 1 - len(tL)
	for key, value in d.items():
		if key == k:
			return value
	return lookupVal2(tL[:i], k)

"""
Testing lookupVal2
"""
def testlookupVal2():
	print("\nTesting lookupVal2()")
	L2 = [(0, {"x": 0, "y": True, "z": "zero"}),
	      (0, {"x": 1}),
	      (1, {"y": False}),
	      (1, {"x": 3, "z": "three"}),
	      (2, {})]

	if lookupVal2(L2, "x") != 1:
		return False
	if lookupVal2(L2, "y"):
		return False
	if lookupVal2(L2, "z") != "zero":
		return False
	if lookupVal2(L2, "t") is not None:
		return False
	return True


# 4. Higher order functions
# funRun(d, name, args) - 10%
# def funRun(d, name, args):

"""
This function takes a dictionary, a function name, and a list of arguments as inputs. The dictionary
contains the function names and the function values. funRun calls the functions using the arguments provided.
"""
def funRun(d, name, args):
	if d.get(name) is not None:
		if len(getfullargspec(d[name]).args) is not len(args):
			print("The number of arguments provided does not match the number of arguments required for this function")
			return
		else:
			return d[name](*args)
	else:
		return

"""
Testing funRun
"""
def testfunRun():
	print("\nTesting funRun()")
	d = {"add": lambda x, y: (x + y), "concat3": lambda a, b, c:
	(a + "," + b + "," + c), "mod2": lambda n: (n % 2)}
	if funRun(d, "concat3", ["one", "two", "three"]) != "one,two,three":
		return False
	if funRun(d, "mod2", [40]) != 0:
		return False
	if funRun(d, "add", [5, 2]) != 7:
		return False
	return True


# 5. Recursion
# numPaths(m, n) - 10%
"""
This function takes ints m and n as inputs and uses recursion to returns the number of possible routes that can be taken in a
box of size m x n to get from the top left corner to the bottom right corner. 
"""
def numPaths(m, n):
	if m == 1 or n == 1:
		return 1
	return numPaths(m - 1, n) + numPaths(m, n - 1)

"""
Testing numPaths
"""
def testnumPaths():
	print("\nTesting numPaths()")

	if numPaths(2, 2) != 2:
		return False
	if numPaths(3, 3) != 6:
		return False
	if numPaths(3, 10) != 55:
		return False
	return True


# 6. Iterators
# a. iterSquares() - 10%
"""
This class creates an iterator that iterates through perfect squares
"""
class iterSquares():
	def __init__(self):
		self.current = 1

	def __next__(self):
		result = self.current ** 2
		self.current += 1
		return result

	def __iter__(self):
		return self


# b. numbersToSum(iNumbers, sum) - 10%
"""
This function takes an iterator (of iterSquares), and an int value s, and returns a list of perfect squares whose
sum is less than the given int value s
"""
def numbersToSum(iNumbers, s):
	L = []
	while sum(L + [(iNumbers.current + 1) ** 2]) < s:
		L.append(iNumbers.__next__())
	return L

"""
Testing numbersToSum
"""
def testnumbersToSum():
	print("\nTesting numbersToSum()")
	squares = iterSquares()

	if numbersToSum(squares, 55) != [1, 4, 9, 16]:
		return False
	if numbersToSum(squares, 100) != [25, 36]:
		return False
	if numbersToSum(squares, 180) != [49, 64]:
		return False
	return True


# 7. Streams
# a. streamSquares(k) - 5%

"""
Creates a stream object
"""
class Stream(object):
	def __init__(self, first, compute_rest, empty=False):
		self.first = first
		self._compute_rest = compute_rest
		self.empty = empty
		self._rest = None
		self._computed = False

	@property
	def rest(self):
		assert not self.empty, 'Empty streams have no rest.'

		if not self._computed:
			self._rest = self._compute_rest()
			self._computed = True
		return self._rest

"""
This function creates a stream of perfect squares the begin at the value k
"""
def streamSquares(k):
	def compute_rest():
		return streamSquares((k + 1) ** 2)

	k = math.sqrt(k)        # finds sqrt of k
	k = int(k)              # converts to int for a whole number
	return Stream((k ** 2), compute_rest)

"""
Testing streamSquares
"""
def teststreamSquares():
	print('\nTesting streamSquares()')
	sqStream = streamSquares(25)
	L = []
	while sqStream.first < 225:
		L.append(sqStream.first)
		sqStream = sqStream.rest
	if L != [25, 36, 49, 64, 81, 100, 121, 144, 169, 196]:
		return False

	sqStream = streamSquares(87)
	L = []
	while sqStream.first < 150:
		L.append(sqStream.first)
		sqStream = sqStream.rest
	if L != [81, 100, 121, 144]:
		return False

	sqStream = streamSquares(341)
	L = []
	while sqStream.first < 600:
		L.append(sqStream.first)
		sqStream = sqStream.rest
	if L != [324, 361, 400, 441, 484, 529, 576]:
		return False
	return True


# b. evenStream(stream) - 10%
"""
This function takes a stream as an argument, then creates a list of the even values of that stream
"""
def evenStream(stream):
	def compute_rest_1():
		return evenStream(stream.rest)

	def compute_rest_2():
		return evenStream(stream.rest.rest)

	if stream.first % 2 == 0:  # if val is even
		return Stream(stream.first, compute_rest_1)
	else:
		return Stream(stream.rest.first, compute_rest_2)

"""
Testing evenStream
"""
def testevenStream():
	print("\nTesting evenStream()")
	evenS = evenStream(streamSquares(9))
	L = []
	while evenS.first < 225:
		L.append(evenS.first)
		evenS = evenS.rest
	if L != [16, 36, 64, 100, 144, 196]:
		return False

	evenS = evenStream(streamSquares(87))
	L = []
	while evenS.first < 150:
		L.append(evenS.first)
		evenS = evenS.rest
	if L != [100, 144]:
		return False

	evenS = evenStream(streamSquares(341))
	L = []
	while evenS.first < 600:
		L.append(evenS.first)
		evenS = evenS.rest
	if L != [324, 400, 484, 576]:
		return False
	return True

"""
Runs all the tests in the main function
"""
if __name__ == '__main__':
	passedMsg = "%s passed"
	failedMsg = "%s failed"

	if testaddDict():
		print(passedMsg % 'addDict')
	else:
		print(failedMsg % 'addDict')

	if testaddDictN():
		print(passedMsg % 'addDictN')
	else:
		print(failedMsg % 'addDictN')

	if testcharCount():
		print(passedMsg % 'charCount')
	else:
		print(failedMsg % 'charCount')

	if testcharCount2():
		print(passedMsg % 'charCount2')
	else:
		print(failedMsg % 'charCount2')

	if testlookupVal():
		print(passedMsg % 'lookupVal')
	else:
		print(failedMsg % 'lookupVal')

	if testlookupVal2():
		print(passedMsg % 'lookupVal2')
	else:
		print(failedMsg % 'lookupVal2')

	if testfunRun():
		print(passedMsg % 'funRun')
	else:
		print(failedMsg % 'funRun')

	if testnumPaths():
		print(passedMsg % 'numPaths')
	else:
		print(failedMsg % 'numPaths')

	if testnumbersToSum():
		print(passedMsg % 'numbersToSum')
	else:
		print(failedMsg % 'numbersToSum')

	if teststreamSquares():
		print(passedMsg % 'streamSquares')
	else:
		print(failedMsg % 'streamSquares')

	if testevenStream():
		print(passedMsg % 'evenStream')
	else:
		print(failedMsg % 'evenStream')
