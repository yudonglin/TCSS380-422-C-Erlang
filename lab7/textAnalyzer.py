"""
Author: Yudong Lin & Wei Wei Chien & Michael Theisen
Date: 5/29/2022
Description:
The program is to analyze a text file (a part of Gettysburg Address) and find the following information:
a. for the word entered by the end-user, it is to find its concordance, in this it means case all the lines on which the word occurs
b. the grade level of the text, which is calculated using a formula that counts total number of sentences, words, and syllables
c. the vocabulary of words that appear in the file along with their frequencies
Requirement: python >= 3.10
"""
dontCount = ["the", "and", "of", "in", "on", "at", "to", "an", "this", "that"]

# Receives a word and calculates and returns a count of the number of syllables in this word
# syllable is defined as the count of vowels unless the vowel appears in the ending es, e, or ed
# words up to 3 chars count as one syllable regardless
def syllableCount(word: str) -> int:
    count: int = 0
    if len(word) <= 3:
        count = 1
    else:
        # 1.  count the number of vowels (aeiou) in the word
        # subtract if the vowel was counted because a word ends in es, e, or ed (use endswith built-in method)
        count += sum([word.count(l) for l in "aeiou"])
        if word.endswith(("es", "e", "ed")):
            count -= 1
    return count


# Determines if a word consists of letters and digits only
def isalnum(word):
    for i in word:
        if i not in "0123456789" and not i.isalpha():
            return False
    return True


# Takes a line and produces a list of words; word is defined as:
# any sequence of alphanumeric characters (length >= 1) with the beginning and ending non-alphanumeric character removed
def cleanline(lineList):
    ## clean words procedure
    lineCleaned = []

    for word in lineList:
        if len(word) > 0:
            if isalnum(word):
                lineCleaned.append(word)
            else:
                sub = ""
                for char in word:
                    if not isalnum(char):
                        char = "*"
                    sub += char
                sublist = sub.split("*")
                for w in sublist:
                    if len(w) > 0:
                        lineCleaned.append(w)

    return lineCleaned


# Receives a list of strings/lines and prints all the strings/lines that match the input word
def concordance(fileList: list[str]):
    word: str = input("Enter the word for all the lines it appears in: ").lower()
    ## 2. print all the lines in fileList that contain the word
    for eachLine in fileList:
        if word in cleanline(eachLine.lower().split()):
            print(eachLine.removesuffix("\n"))


# Receives a list of strings/lines and returns the number of sentences and a list of words
def sentences(fileList):
    sentenceCount = 0
    wordList = []
    for line in fileList:
        line = line.lower()
        sentenceCount += line.count(".") + line.count("?") + line.count("!") + line.count(";")
        lineList = line.split()
        words = cleanline(lineList)
        if len(words) > 0:
            wordList = wordList + words
    # 3. return sentenceCount and wordList
    return sentenceCount, wordList


# Receives a list of strings/lines and prints grade level
# 4. provide function header for this function that matches the call
def gradelevel(fileList: list[str]):
    sCount, wList = sentences(fileList)
    wordCount = len(wList)
    totalSyllables = 0
    for item in wList:
        totalSyllables += syllableCount(item)
    gradeLevel = 0.39 * (wordCount / sCount) + 11.8 * (totalSyllables / wordCount) - 15.59
    # 5. print sentence, word, syllable counts and grade level
    print(
        "Sentence count: {0}\nWord count: {1}\nSyllables count: {2}\nGrade Level: {3}".format(
            sCount, wordCount, totalSyllables, round(gradeLevel)
        )
    )


# Receives a list of strings/lines and sets up a dictionary, prints the dictionary in sorted order
# by word
def vocabulary(fileList: list[str]):
    sCount, wList = sentences(fileList)
    # 6. build a dictionary from wList that counts the frequency of words in wList
    #  sort the dictionary by word and print it
    #  exclude dontCount list words from the dictionary
    #  exclude words of length < 1 (e.g. 'a')
    frequencyT: dict[str, int] = {
        wordKey: sum([cleanline(eachLine.lower().split()).count(wordKey) for eachLine in fileList])
        for wordKey in filter(lambda x: (x not in dontCount and len(x) > 1), wList)
    }
    # print frequency table
    print("WORD FREQUENCY")
    for key in sorted(list(frequencyT.keys())):
        print(key, frequencyT[key])
    # print statistics
    printStatistics(frequencyT)

# 10. once you are done, add your own function that you would call from the end of vocabulary function
# this function should accept the dictionary you just built, find and print the mean of frequencies, the lowest and highest word count
# for the word with the highest count, it should show what the word/s with that count (may be more than one word with highest count if run on a different file than gettysburg.txt
# this definition belongs in between vocabulary and main
def printStatistics(frequencyT: dict[str, int]):
    value_list: list[int] = list(frequencyT.values())
    print("average word frequency: ", sum(value_list) / len(value_list))
    print("lowest word frequency: ", min(value_list))
    maximumV: int = max(value_list)
    print("highest word frequency: ", maximumV)
    print("words with highest frequency: ", [key for key in frequencyT if frequencyT[key] >= maximumV])


def main():

    # the file our program will process
    infile1 = open("gettysburg.txt", "r")
    # reads an entire file into a list, where a list element
    # is one line from the file treated as a string,
    # i.e. we end up with a list of strings
    fileList: list[str] = infile1.readlines()
    infile1.close()

    while True:

        # 7. while statement that is entered, if choice is not suitable for an int
        # or int_choice is not within the range [1, 3]
        while True:
            print("Which text processing output are you interested in? ")
            choice = input("Enter 1 for concordance, 2 for grade level, 3 for vocabulary: ")
            if choice.isdigit():
                int_choice = int(choice)
                if 1 <= int_choice <= 3:
                    break
            print("Invalid input - try again")

        # 8. store concordance, gradelevel and vocabulary into a list
        functionList: list = [concordance, gradelevel, vocabulary]
        # 9. based on the user's choice (1, 2 or 3)  call appropriate  function
        # from the list of functions (do not use the if statement) and pass fileList to it
        functionList[int_choice - 1](fileList)

        # 11. once you are done, add a loop that allows the user to repeat the program
        # until 'q' is entered; the loop should start after the file is closed, so that
        # the user does not need to reopen the file
        if input("Repeat? Enter any char. Enter q to quit: ") == "q":
            break

# if ran directly, call main.
if __name__ == "__main__":
    main()
