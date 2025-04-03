import numpy as np
import time
import matplotlib.pyplot as plt

states = ['P','K', 'N']

states_win = {
        'P': 'K', #key beats value
        'K': 'N',
        'N': 'P'
        }

get_win_move = {
        'P': 'N',
        'N': 'K',
        'K': 'P',

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

n = 100
state = 'K'
opp_move = 'K'
move = ''
prod = ''
prev_opp = state;
prev_opp_index = states.index(prev_opp)

for i in range(n):

    opp_index = states.index(state)

    sum_row = sum(games[opp_index])
    if sum_row == 0:
        move = np.random.choice(states)
    else:
        move = np.random.choice(states, p= (games[opp_index]/sum_row)) # predicted opponent move


    opp_move = np.random.choice(states, p=opp_strategy) # opponent move

    opp_index = states.index(opp_move)


    my_move = get_win_move[move]

    print("Opp MOVE: ->", opp_move)
    print("Opp PREDICTED MOVE: ->", move)
    print("My move based on opp predicted move: ->",my_move)



    prev_opp_index = states.index(prev_opp)
    games[prev_opp_index, opp_index] += 1

    if states_win[my_move] == opp_move:
        profit = profit +1
    elif states_win[opp_move] == my_move:
        profit = profit-1

    profit_each_step.append(profit)


    prev_opp = move


print("For ", n, " matches - " ,"Profit: ", profit)
print("Macierz przejść po nauce:")
print(games / games.sum(axis=1, keepdims=True))

plt.plot(profit_each_step)
plt.show()
