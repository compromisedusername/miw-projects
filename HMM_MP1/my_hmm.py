import numpy as np
import matplotlib.pyplot as plt

import numpy as np
import matplotlib.pyplot as plt

# Możliwe ruchy
states = ['P', 'K', 'N']

# Mapa wygranych: klucz pokonuje wartość
states_win = {
    'P': 'K',  # Papier bije Kamień
    'K': 'N',  # Kamień bije Nożyce
    'N': 'P'   # Nożyce biją Papier
}

# Macierz przejść (model uczenia)
games = np.array([
    [1, 1, 1],  # P -> P, K, N
    [1, 1, 1],  # K -> P, K, N
    [1, 1, 1]   # N -> P, K, N
], dtype=float)

# Strategia przeciwnika (stała)
opp_strategy = np.array([3/3, 0/3, 0/3])

# Parametry symulacji
n = 5555
profit_each_step = []
profit = 0
state = 'P'  # Zaczynamy od papieru

for i in range(n):
    # Wybór ruchu na podstawie macierzy przejść
    state_index = states.index(state)
    move = np.random.choice(states, p=games[state_index] / sum(games[state_index]))

    # Ruch przeciwnika (losowy, zgodnie ze strategią)
    opp_move = np.random.choice(states, p=opp_strategy)

    # Sprawdzamy wynik gry
    if states_win[move] == opp_move:
        profit += 1  # Wygrana
    elif states_win[opp_move] == move:
        profit -= 1  # Przegrana

    # Aktualizacja macierzy przejść (uczenie się)
    move_index = states.index(move)
    games[state_index, move_index] += 1  # Wzmocnienie wybranego ruchu

    # Historia profitu
    profit_each_step.append(profit)

    # Przeciwnik ustala nowy stan
    state = opp_move

# Wykres zmiany profitu
plt.plot(profit_each_step)
plt.xlabel("Liczba gier")
plt.ylabel("Stan kasy")
plt.title("Zmiana profitu w czasie")
plt.show()

print("Ostateczny profit po", n, "rundach:", profit)

