import random

def random_game():
    game = random.random()
    if game < 0.1: return 'draw'
    elif game < 0.4: return 'b'
    else: return 'a'

def finite_tournament():
    outcomes = []
    wins = ['drawa', 'drawb', 'aa', 'bb', 'adraw', 'bdraw']
    for g in range(3):
        outcomes.append(random_game())
        if ''.join(outcomes) in wins:
            break
    return outcomes
    
def game_ends_in_two(tries):
    two_rounds_games = 0
    for i in xrange(tries):
        game = finite_tournament()
        if len(game) == 2:
            two_rounds_games += 1
    return float(two_rounds_games) / tries

def a_wins_three_rounds_game(tries):
    games = 0
    a_wins = 0
    for i in xrange(tries):
        game = finite_tournament()
        if len(game) > 2 and game[2] == 'draw':
            games += 1
            if game[0] == 'a': a_wins += 1
    return float(a_wins) / games

def at_least_three_games_played(times):
    good = 0
    games = 0
    def a_wins(game):
        return game[-1] == 'a' or ''.join(game) == 'adraw'
    for i in xrange(times):
        game = finite_tournament()
        if a_wins(game):
            games += 1
            if len(game) > 2:
                good += 1
    return float(good) / games, float(good) / times, float(games) / times

print '''Game ends in two turns: $\\num{%f}$
A wins first round: $\\num{%f}$
Three games were played: $\\num{%f}$
''' % (game_ends_in_two(100000),
       a_wins_three_rounds_game(100000),
       at_least_three_games_played(100000))

def select_coin():
    tails = 0
    chance = random.random() * 3
    if chance <= 1: tails = 1.0 / 3
    elif chance <= 2: tails = 0.5
    else: tails = 2.0 / 3
    return tails

def chance_of_tails(times):
    return sum(select_coin() for x in xrange(times)) / times
        
def chance_two_tails(times):
    return sum(select_coin() ** 2
               for x in xrange(times)) / times

def test_coins(times):
    return sum(select_coin() for x in xrange(times)) / times

def chance_of_fair_two_tails(times):
    fair, total = 0.0, 0.0
    for x in xrange(times):
        coin = select_coin() ** 2
        if coin == 0.25: fair += coin
        total += coin
    return fair / total

def chance_third_tails(times):
    two_thirds, half, third, total = 0.0, 0.0, 0.0, 0.0
    for x in xrange(times):
        coin = select_coin() ** 2
        if coin == 0.25: half += coin
        elif coin < 0.25: third += coin
        else: two_thirds += coin
        total += coin
    two_thirds /= total
    half /= total
    third /= total
    return two_thirds * 2 / 3 + half / 2 + third / 3
    
print '''
      + Probability of tossing tails: $\\num{%s}
      + Probability of tossing two tails: $\\num{%s}
      + Chance of fair coin given two tails: $\\num{%s}
      + Chance third toss will be tails: $\\num{%s}
      ''' % tuple(f(100000) for f in [chance_of_tails,
                                      chance_two_tails,
                                      chance_of_fair_two_tails,
                                      chance_third_tails])
