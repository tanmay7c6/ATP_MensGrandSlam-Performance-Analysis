# ATP Dataset Variable Definitions
# File 1 : Match.csv
match_id: Unique ID for each game, it is made up of four parts
          eg: m_2018_A_2
              m-> Mens 
              2018 -> Year
              A -> Type of tournament (A- Australian Open, F- French Open, W - Wimbledon. U - US Open)
              2-> Game number in that particular year and championship for mens
round: gives which round did tha match belong to eg: 1st Round, 2nd Round, Quarterfinal, Semifinal etc
avg_minutes_game: Average time to win a game(in minutes)
avg_seconds_points: Average time to win a point(in seconds)
avg_minutes_set: Average length of a set(in minutes)
tournament: Name of the Tournament
year: Year of the Tournament
match_minutes: Length of match(in minutes)

# File 2: Player.csv

player_id: Unique ID assigned to a player
name: Name of the Player
hand: Right hand or Left hand
country: Country represented by the player
birthday: Date of birth of the player

# File 3: Stats.csv
match_id: Unique ID for each game, it is made up of four parts
          eg: m_2018_A_2
              m-> Mens 
              2018 -> Year
              A -> Type of tournament (A- Australian Open, F- French Open, W - Wimbledon. U - US Open)
              2-> Game number in that particular year and championship for mens
player_id: Unique ID assigned to a player, represents ID of player playing the match
pts: Total points won by the player in the tournament
rank: ATP Rank of the player who playing the match
winner: True or False to represent whether a player won or loss the game
sets: Number of sets won by the player in the particular match
1: total games won in set 1
2: total games won in set 2
3: total games won in set 3
4: total games won in set 4(0 represents best of 5 game won in 3 sets)
5: total games won in set 5(0 represents best of 5 game won in 3 or 4 sets)  
avg_odds: Average odds of winning the game for the player
max_odds: Maximum odds of winning the game for the player 
total_pts: Total points won by the player in the particular match
service_pts: Total service points won by the player in the match
return_pts: Total return points won by the player in the match
aces: Total number of aces served by the player in the match
bp_saved: Total number of breakpoints saved by the player in the match
bp_faced: Total number of breakpoints faced by the player in the match
first_serve_rtn_won: Total number of first serve return wins by the player in the match
