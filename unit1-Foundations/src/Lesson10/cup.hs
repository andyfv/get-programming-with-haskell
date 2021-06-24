module Cup where

-- Create a cup object
cup flOz = \message -> message flOz


-- Access an object
getOz aCup = aCup (\flOz -> flOz)


-- Change an object state
drink aCup ozDrank = if ozDiff >= 0
                     then cup ozDiff
                     else cup 0
    where flOz   = getOz aCup
          ozDiff = flOz - ozDrank


isEmpty aCup = getOz aCup == 0


coffeeCup = cup 12

afterManySips = foldl drink coffeeCup [1,1,1,1,1]


-- robot object
robot (name, attack, hp) = \message -> message (name, attack, hp)

-- robot instance
killerRobot = robot ("Kill3r", 25, 200)

-- helper functions to access 
name   (n, _, _) = n
attack (_, a, _) = a
hp     (_, _, h) = h

-- getters
getName   aRobot = aRobot name
getAttack aRobot = aRobot attack
getHP     aRobot = aRobot hp


-- setters
setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))
setHP aRobot newHP = aRobot (\(n, a, h) -> robot (n, a, newHP))


-- Prototype-based OOP
nicerRobot   = setName killerRobot "kitty"
gentlerRobot = setAttack killerRobot 5
softRobot    = setHP killerRobot 50



-- print
printRobot aRobot = 
    aRobot (\(n,a,h) -> 
        n 
        ++ " attack: " 
        ++ (show a) 
        ++ " hp: " 
        ++ (show h)
        )



-- sending messages between objects
damage aRobot attackDamage = 
    aRobot (\(n,a,h) -> robot (n, a, h - attackDamage))

fight aRobot defender = damage defender attack
    where attack = if getHP aRobot > 10
                   then getAttack aRobot
                   else 0

-- contender robot
gentleRobot = robot ("Mr. Friendly", 10, 300)


-- fight
gentleRobotRound1 = fight killerRobot gentleRobot
killerRobotRound1 = fight gentleRobot killerRobot

gentleRobotRound2 = fight killerRobotRound1 gentleRobotRound1
killerRobotRound2 = fight gentleRobotRound1 killerRobotRound1

gentleRobotRound3 = fight killerRobotRound2 gentleRobotRound2
killerRobotRound3 = fight gentleRobotRound2 killerRobotRound2


                

-- Exercises

-- 1. getLife
getLife robots = map (getHP) robots 


-- 2. threeRoundFight


-- 3. list of robots
robots = [killerRobot, gentlerRobot, softRobot]

fightWithAll robot oponents = (\fight -> map (fight robot) oponents)
-- remainingLife = map (getHP) 
