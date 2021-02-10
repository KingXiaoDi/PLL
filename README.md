# PLL
A repository to track stats and dashboards for the Premier Lacrosse League (PLL).

### Plan
Flesh out the current statistical offerings from the PLL (examples [here](https://stats.premierlacrosseleague.com/)). Lean heavily on hockey as an example. Possibilities range from simple scoring and penalty summaries (example [here](https://www.hockey-reference.com/boxscores/202102080CBJ.html); I've been unable to find this sort of summary anywhere from the PLL) to more advanced displays (see [Moneypuck](http://moneypuck.com/g.htm?id=2020020203)) and statistical analyses (see [@IneffectiveMath's](https://twitter.com/IneffectiveMath/) website [HockeyViz](https://hockeyviz.com/)).

The goal is to have this work complete in time for the PLL's 2021 season, tentatively scheduled to start on June 4th. It would be nice (but likely difficult) to have a website with the ability to share this information by then. An extra step could include having the site update in roughyl real-time (likely incredibly difficult due to the lack of statistical sources outside of manual charting).

#### Current data sets include information for every goal scored in the 2020 PLL Championship series (**20** games)\*, including:
- Game
- Quarter and Time
- Goal Scorer
- Assist (if any)
- Goalie
- Goal Value (**1** or **2** points)
- Handedness of the shot
- Manpower advantage/disadvantage (man up, man down, 9v9, 10v10, etc.)
- Fast break (including advantage numbers, e.g. 4v3, 5v4, etc.) or settled
- Current score (not directly available, but calculable from the rest of the data)

#### Future charting could include further information about the shot itself (some of these are already partially charted):
- Time remaining on shot clock
- Was the shot a bounce shot and/or a jump shot?
- Did the player enter the crease after scoring?
- Was the shot off a rebound?
- What release did the player use? (overhand, 3/4, sidearm, **between the legs!!**, etc.)

#### Some information will likely only be available to the league itself, if at all, including:
- Location where the shooter releases the shot (could be estimated, but would be inaccurate; plus we know the league tracks this information somehow)
- Shot velocity
- Player velocity at release
- Location where the ball crosses the goal line (very difficult to estimate, likely would need technological assistance which may not currently exist)
- Further information about how the ball passed the goalie (possible to chart manually). Some things I've semi-tracked:
  - Was the shot to the goalie's stick or off side?
  - Was the shot to the player's near or far side?
  - Was the shot to the goalie's left or right side? (Given if you have the first and know the handedness of the goalie)
  - Potentially numbering goalie holes (like hockey, but mostly only relevant for **5**-, **6**-, and **7**-holes)
  
Eventually, all this information could be used to create an [xG formula](https://jetsnation.ca/2020/01/07/expected-goals-xg-models-explained/).

\* - Data currently available for 19 of the 20 games. Charting is incomplete for the 2nd semi-final game between the Whipsnakes and the Redwoods.

### Future Work
After completing the work above, here are some other extensions I'm considering.

#### Possessions
Track the game on a possession level (a team gains possession whenever a new shot clock starts if they weren't already in possession). This information could lead to data on possession lengths, shots per possession, and other similar statistics. Given some statistics shown during broadcasts, it's likely that the PLL already tracks this to some degree.

#### Corsi/Fenwick
A big push in hockey relatively recently has involved expanding what is counted as a shot. Instead of tracking only shots on goal (which excludes shots that miss entirely, hit the post, or are blocked), Fenwick and Corsi measure combinations of all shot attempts. A side benefit: we could track who blocks the most shots (even though we already know that it's Glicini). This addition would be a boon for the sport which is sorely lacking in defensive statistics (caused turnovers and ground balls?).

#### Clears/Rides
Similar to possessions, but specific to clears/rides. How often does a team fail to clear the ball (reach the offensive zone and "set up possession").

#### Full Play-By-Play
The holy grail. While much easier in a sport like football and baseball which have clearly defined plays, it's also possible in open sports like basketball and [hockey](https://www.nhl.com/gamecenter/car-vs-cbj/2021/02/08/2020020197#game=2020020197,game_state=final,game_tab=plays), so it is also possible in lacrosse (although it is unlikely to have a perfect description of the entire game). This step would work best with intricate tracking data, which could allow for more intricate details than is cost-effective for manually charting (imagine a ball with tracking information that could be used to determine when it is passed, who the passer is, and who received the pass or was the intended target. Then forget the idea when because you look at the millions the NHL has spent on designing a puck with sensors and yet they had to stop using it because the manufacturing changed the way the puck moved).

Still, even without details to the lowest level, something similar to what the NHL provides would be amazing and limit the need for manual charting.
