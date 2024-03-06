# Zellix-Combo
This project aims to simulate the combo between the Magic the Gathering cards Zellix, Sanity Flayer and Altar of the Brood. Zellix putting a creature token into play will always trigger Altar of the Brood, but Altar of the Brood will only sometimes trigger Zellix. This is determined from the number of creatures in your opponets deck and the number of opponets you have. 

This code simulates up to three players with each unique combination of creatures in a deck (between 0 and 40) and up to 4 triggers (additional copies of Zellix or Altar will act as an additional trigger) 10000 times and takes an average of the number of cards milled and the percentage of simulations that entirely milled out the opponets. The way this code is written takes into account the hypergeometric distribution of the deck of cards.
