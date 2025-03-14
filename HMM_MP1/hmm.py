import numpy as np
import time
import matplotlib.pyplot as plt

states = ['P','K', 'N']

states_win = {
        'P': 'K', #key beats value
        'K': 'N',
        'N': 'P'
        }
games=np.array([
    #P K N
    [1,1,1],  #P
    [1,1,1],  #K
    [1,1,1]]) #N

opp_strategy = np.array([1, #P    <-- we dont know that``
                         0, #K
                         0])#N



profit_each_step =[]
profit=0

n = 30
state = 'P'
opp_move = ''
move = ''
prod = ''

for i in range(n):

    opp_index = states.index(state)

    move = np.random.choice(states, p=games[opp_index]/sum(games[opp_index]))
    my_index = states.index(move)

    opp_move = np.random.choice(states, p=opp_strategy)


    if states_win[move] == opp_move:
        profit = profit +1
        games[opp_index, my_index] += 1
    elif states_win[opp_move] == move:
        profit = profit - 1
        games[opp_index, my_index] -= 1

    profit_each_step.append(profit)

    state = opp_move


print("For ", n, " matches - " ,"Profit: ", profit)
print("Macierz przejść po nauce:")
print(games / games.sum(axis=1, keepdims=True))

plt.plot(profit_each_step)
plt.show()
