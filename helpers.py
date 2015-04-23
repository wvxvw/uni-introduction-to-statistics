import random

def random_game():
    game = random.random()
    if game < 0.1: return 'draw'
    elif game < 0.4: return 'b'
    else: return 'a'

def tournament():
    winner = None
    previous = None
    turns = 0
    game = []
    while not winner:
        candidate = random_game()
        turns += 1
        if candidate == 'a' and previous in ['a', 'draw']:
            winner = 'a'
        elif candidate == 'b' and previous in ['b', 'draw']:
            winner = 'b'
        previous = candidate
        game.append(candidate)
    return winner, turns, game
    
def game_ends_in_two(tries):
    two_rounds_games = 0
    for i in xrange(tries):
        winner, turns, game = tournament()
        if turns == 2:
            two_rounds_games += 1
    return float(two_rounds_games) / tries

def a_wins_three_rounds_game(tries):
    games = 0
    a_wins = 0
    for i in xrange(tries):
        winner, turns, game = tournament()
        if len(game) > 2 and game[2] == 'draw':
            games += 1
            if game[0] == 'a': a_wins += 1
    return float(a_wins) / games
            
def chance_player_wins(player, times):
    wins = 0
    for i in xrange(times):
        winner, turns, game = tournament()
        if winner == player:
            wins += 1
    return float(wins) / times

def chance_game_won_by(player, times):
    wins = 0
    for i in xrange(times):
        if player == random_game():
            wins += 1
    return float(wins) / times

def at_least_three_games_played(times):
    good = 0
    games = 0
    for i in xrange(times):
        winner, turns, game = tournament()
        if winner == 'a':
            games += 1
            if len(game) > 2:
                good += 1
    return float(good) / games
