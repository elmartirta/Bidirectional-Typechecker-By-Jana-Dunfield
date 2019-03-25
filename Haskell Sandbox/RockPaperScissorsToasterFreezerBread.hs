data Move = Rock | Paper | Scissors | Toaster | Freezer | Bread

score :: Move -> Move -> Int
score Rock Paper = -1
score Rock Scissors = 1
score Rock Toaster = 1

score Paper Rock = 1
score Paper Scissors = -1
score Paper Toaster = -1

score Scissors Rock = -1
score Scissors Paper = 1

score Toaster Rock = -1
score Toaster Paper = 1
score Toaster Freezer = 1
score Toaster Bread = 1

score Freezer Toaster = -1

score Bread Toaster = -1

score _ _ = 0

