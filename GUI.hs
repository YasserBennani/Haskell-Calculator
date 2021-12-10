-- / 
-- Author : Yasser Bennani 
-- CW 2 Part 2 - Exam 
-- /

module Main where
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified ParsingExt as E
import Calculator

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 1308
        , jsStatic     = Just "static"
        } windowSetup

windowSetup :: Window -> UI ()
windowSetup win = do
    -- Set window params
                return win # set UI.title "Haskell Calculator"
                UI.addStyleSheet win "calc.css"

    -- create necessary buttons
                numPad <- mapM createButton numbers
                ops <- mapM createButton operators
                prts <- mapM createButton parentheses
                point <- createButton dot
                eq <- createButton "=" -- Calculate expression
                clear <- createButton "C" -- Clear calculator
                clearEntry <- createButton "CE" -- Clear last entry
                sign <- createButton "+/-" -- Change sign
                recAns <- createButton "MR" -- Memory Recall
                ms <- createButton "MS" -- Store calculation in memory
                mp <- createButton "M+" -- Add calculation to value in memory
                mc <- createButton "MC" -- Clear memory
                mm <- createButton "M-" -- Subtract calculation from memory

    -- link on click events
                let numOnClick = zipWith onClickEvent numPad numbers
                let opOnClick  = zipWith onClickEvent ops operators
                let prtsOnClick= zipWith onClickEvent prts parentheses 
                let eqOnClick  = displayAnswer eq
                let dotOnClick = onClickEvent point dot
                let clearOnClick = clearEvent clear
                let clearEntryOnClick = clearEntryEvent clearEntry
                let signOnClick = signEvent sign
                let recMemOnClick = recallMemEvent recAns
                let storeMemOnClick = storeMemEvent ms
                let addMemOnClick = addMemEvent mp
                let clearMemory = clearMemEvent mc
                let subMemory = subMemEvent mm

    -- gather all events under one event list
                let events = numOnClick 
                            ++ opOnClick 
                            ++ prtsOnClick
                            ++ [eqOnClick,dotOnClick,clearOnClick,signOnClick,recMemOnClick,
                            clearEntryOnClick,clearMemory,addMemOnClick,storeMemOnClick,subMemory] 

    -- Accumulate results of all events to form the expression to be calculated (evaluated)
                calcState <- accumB initialize $ foldl1 (unionWith const) events

    -- Set up DOM elements to be displayed to the user
                calcModel <- UI.label #."calcModel" #set UI.text model
                -- The main calculator display
                calcDisplay <- UI.div
                            #. "calcDisplay"  
                            #+([UI.label # sink UI.text (fmap showCurrAns calcState)])
                -- The memory display
                memoryDisplay <- UI.div
                            #. "memoryDisplay"  
                            #+([UI.label # sink UI.text (fmap showMem calcState)])
                 -- Div encompassing the graphical elements of the calculator
                calcDiv <- UI.div
                            #. "calcDiv" 
                            # set UI.align "center"
                            #+ ((map element 
                            ([calcModel,calcDisplay, memoryDisplay,ms,mc,mp,mm,
                            recAns,clearEntry,clear,sign]
                            ++ ops
                            ++ numPad
                            ++ [point,eq] 
                            ++ prts
                            ))) 
    -- Add DOM elements to the window under a container div
                getBody win #+ [    
                                UI.div 
                                #. "container" 
                                #+ [element calcDiv] 
                                ]
                return ()

-- Constants
numbers:: [String]
numbers = map show ([1..9]++[0])

operators :: [String]
operators = ["+","-","*","/"] 

parentheses :: [String]
parentheses = ["(", ")"]

dot :: String
dot = "."

model :: String
model = "HASKALC ________ psx-YB7"

-- Button creator
createButton :: String -> UI Element
createButton label = UI.button #. "button" #set UI.text label 

-- Event handlers
onClickEvent :: Element -> String -> Event (Calculator -> Calculator)
onClickEvent button inp = (\calc -> concatInput calc inp) <$ UI.click button

clearEvent :: Element -> Event (Calculator-> Calculator)
clearEvent button = reset <$ UI.click button

clearEntryEvent :: Element -> Event (Calculator -> Calculator)
clearEntryEvent button = clearEntry <$ UI.click button

signEvent :: Element -> Event (Calculator -> Calculator)
signEvent button = changeSign <$ UI.click button

storeMemEvent :: Element -> Event (Calculator -> Calculator)
storeMemEvent button = storeMem <$ UI.click button

recallMemEvent :: Element -> Event (Calculator -> Calculator)
recallMemEvent button = recallMem <$ UI.click button

addMemEvent :: Element -> Event (Calculator -> Calculator)
addMemEvent button = addMem <$ UI.click button

subMemEvent :: Element -> Event (Calculator -> Calculator)
subMemEvent button = subMem <$ UI.click button

displayAnswer :: Element -> Event (Calculator -> Calculator)
displayAnswer button = evalCalc <$ UI.click button

clearMemEvent :: Element -> Event (Calculator -> Calculator)
clearMemEvent button = clearMem <$ UI.click button
-- 