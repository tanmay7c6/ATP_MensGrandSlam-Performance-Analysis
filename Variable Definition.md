# **ATP Dataset Variable Definitions**
## File 1 : Match.csv
* __match_id:__ Unique ID for each game, it is made up of four parts
	 * __Example:__ in match_id **m_2018_A_2**
		* __m__-> Mens 
		* __2018__ -> Year of the tournament
		* __A__ -> Type of tournament *(A- Australian Open, F- French Open, W - Wimbledon. U - US Open)
		* __2__-> Game number in that particular year and championship for mens
* __round:__ gives which round did tha match belong to *eg: 1st Round, 2nd Round, Quarterfinal, Semifinal etc
* __avg_minutes_game:__ Average time to win a game(*in minutes*)
* __avg_seconds_points:__ Average time to win a point(*in seconds*)
* __avg_minutes_set:__ Average length of a set(*in minutes*)
* __tournament:__ Name of the Tournament
* __year:__ Year of the Tournament
* __match_minutes:__ Length of match(*in minutes*)

## File 2: Player.csv

* __player_id:__ Unique ID assigned to a player
* __name:__ Name of the Player
* __hand:__ Right hand or Left hand
* __country:__ Country represented by the player
* __birthday:__ Date of birth of the player

## File 3: Stats.csv
* __match_id:__ Unique ID for each game, it is made up of four parts
	* __eg:__ in match_id **m_2018_A_2**
		* __m__-> Mens 
		* __2018__ -> Year of the tournament
		* __A__ -> Type of tournament *(A- Australian Open, F- French Open, W - Wimbledon. U - US Open)
		* __2__-> Game number in that particular year and championship for mens
* __player_id:__ Unique ID assigned to a player, represents ID of player playing the match
* __pts:__ Total points won by the player in the tournament
* __rank:__ ATP Rank of the pla* yer who playing the match
* __winner:__ True or False to represent whether a player won or loss the game
* __sets:__ Number of sets won by the player in the particular match
* __1:__ total games won in set 1
* __2:__ total games won in set 2
* __3:__ total games won in set 3
* __4:__ total games won in set 4(*0 represents best of 5 game won in 3 sets*)
* __5:__ total games won in set 5(*0 represents best of 5 game won in 3 or 4 sets*)  
* __avg_odds:__ Average odds of winning the game for the player
* __max_odds:__ Maximum odds of winning the game for the player 
* __total_pts:__ Total points won by the player in the particular match
* __service_pts:__ Total service points won by the player in the match
* __return_pts:__ Total return points won by the player in the match
* __aces:__ Total number of aces served by the player in the match
* __bp_saved:__ Total number of breakpoints saved by the player in the match
* __bp_faced:__ Total number of breakpoints faced by the player in the match
* __first_serve_rtn_won:__ Total number of first serve return wins by the player in the match
