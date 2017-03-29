module Rfc2822DateTime exposing (DateTime, Date, Time, WeekDay(..), Month(..), Zone(..), parse)

{-| This implementation follows section 3.3 of the [RFC2822](https://tools.ietf.org/html/rfc2822#section-3.3) specification.


## Types

@docs DateTime, Date, Time, WeekDay, Month, Zone

## Parsing

@docs parse

-}

import Char
import Combine exposing (..)
import Combine.Char exposing (digit)


{-| Record containing all the date time information in the [RFC2822](https://tools.ietf.org/html/rfc2822) standard.
-}
type alias DateTime =
    { dayOfWeek : Maybe WeekDay
    , date : Date
    , time : Time
    }


{-| Record containing all the date information in the [RFC2822](https://tools.ietf.org/html/rfc2822) standard.
-}
type alias Date =
    { year : Int
    , month : Month
    , day : Int
    }


{-| Record containing all the time information in the [RFC2822](https://tools.ietf.org/html/rfc2822) standard.
-}
type alias Time =
    { hour : Int
    , minute : Int
    , second : Maybe Int
    , zone : Zone
    }


{-| Day of weeks as specified in [RFC2822](https://tools.ietf.org/html/rfc2822)
-}
type WeekDay
    = Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
    | Sun


{-| Months as specified in [RFC2822](https://tools.ietf.org/html/rfc2822)
-}
type Month
    = Jan
    | Feb
    | Mar
    | Apr
    | May
    | Jun
    | Jul
    | Aug
    | Sep
    | Oct
    | Nov
    | Dec


{-| Different time zones as specified in [RFC2822](https://tools.ietf.org/html/rfc2822)
-}
type Zone
    = UT
    | GMT
    | EST
    | CST
    | MST
    | PST
    | EDT
    | CDT
    | MDT
    | PDT
    | Offset Int
    | Military Char


{-| Parse a raw string to DateTime.

    parse "Mon, 06 Mar 2017 21:22:23 +0000" == Ok { date = { year = 2017, month = Mar, day = 6 }, time = { hour = 21, minute = 22, second = Just 23, zone = Offset 0 }}
    parse "foo" == Err
-}
parse : String -> Result String DateTime
parse input =
    case Combine.parse dateTime input of
        Ok ( _, _, s ) ->
            Ok s

        Err _ ->
            Err "Could not parse input"


dateTime : Parser s DateTime
dateTime =
    succeed DateTime
        <*> (maybe (dayOfWeek <* maybe (string ",")))
        <*> date
        <*> (fws *> time <* maybe cfws)


dayOfWeek : Parser s WeekDay
dayOfWeek =
    choice
        [ maybe fws *> dayName
        , obsDayOfWeek
        ]


dayName : Parser s WeekDay
dayName =
    choice
        [ string "Mon" $> Mon
        , string "Tue" $> Tue
        , string "Wed" $> Wed
        , string "Thu" $> Thu
        , string "Fri" $> Fri
        , string "Sat" $> Sat
        , string "Sun" $> Sun
        ]


date : Parser s Date
date =
    succeed (\d m y -> Date y m d)
        <*> day
        <*> month
        <*> year


year : Parser s Int
year =
    choice
        [ parseStringToInt fourOrMoreDigits
        , obsYear
        ]


month : Parser s Month
month =
    choice
        [ fws *> monthName <* fws
        , obsMonth
        ]


monthName : Parser x Month
monthName =
    choice
        [ string "Jan" $> Jan
        , string "Feb" $> Feb
        , string "Mar" $> Mar
        , string "Apr" $> Apr
        , string "May" $> May
        , string "Jun" $> Jun
        , string "Jul" $> Jul
        , string "Aug" $> Aug
        , string "Sep" $> Sep
        , string "Oct" $> Oct
        , string "Nov" $> Nov
        , string "Dec" $> Dec
        ]


day : Parser x Int
day =
    choice
        [ (maybe fws) *> parseStringToInt oneOrTwoDigits
        , obsDay
        ]


time : Parser a Time
time =
    succeed (\( h, m, s ) z -> Time h m s z)
        <*> timeOfDay
        <*> (fws *> zone)


timeOfDay : Parser x ( Int, Int, Maybe Int )
timeOfDay =
    succeed (,,)
        <*> hour
        <*> (string ":" *> minute)
        <*> maybe (string ":" *> second)


hour : Parser x Int
hour =
    choice
        [ parseStringToInt twoDigits
        , obsHour
        ]


minute : Parser x Int
minute =
    choice
        [ parseStringToInt twoDigits
        , obsMinute
        ]


second : Parser x Int
second =
    choice
        [ parseStringToInt twoDigits
        , obsSecond
        ]


zone : Parser x Zone
zone =
    choice
        [ Offset <$> (zoneModifier <*> parseStringToInt fourDigits)
        , obsZone
        ]


parseStringToInt : Parser s String -> Parser s Int
parseStringToInt x =
    x
        >>= (\v ->
                case String.toInt v of
                    Ok s ->
                        succeed s

                    Err e ->
                        fail e
            )


zoneModifier : Parser s (Int -> Int)
zoneModifier =
    or (string "+" $> identity)
        (string "-" $> ((*) -1))


oneOrTwoDigits : Parser s String
oneOrTwoDigits =
    regex "[0-9]{1,2}"


twoOrMoreDigits : Parser s String
twoOrMoreDigits =
    regex "[0-9]{2,}"


twoDigits : Parser s String
twoDigits =
    regex "[0-9]{2}"


fourOrMoreDigits : Parser s String
fourOrMoreDigits =
    regex "[0-9]{4,}"


fourDigits : Parser s String
fourDigits =
    regex "[0-9]{4}"


obsYear : Parser x Int
obsYear =
    maybe cfws
        *> parseStringToInt twoOrMoreDigits
        <* maybe cfws
        |> map patchTwoDigitYear


obsDayOfWeek : Parser s WeekDay
obsDayOfWeek =
    maybe cfws *> dayName <* maybe cfws


obsMonth : Parser x Month
obsMonth =
    cfws *> monthName <* cfws


obsDay : Parser x Int
obsDay =
    maybe cfws *> parseStringToInt oneOrTwoDigits <* maybe cfws


obsHour : Parser x Int
obsHour =
    maybe cfws *> parseStringToInt twoDigits <* maybe cfws


obsMinute : Parser x Int
obsMinute =
    maybe cfws *> parseStringToInt twoDigits <* maybe cfws


obsSecond : Parser x Int
obsSecond =
    maybe cfws *> parseStringToInt twoDigits <* maybe cfws


obsZone : Parser x Zone
obsZone =
    choice
        [ string "UT" $> UT
        , string "GMT" $> GMT
        , string "EST" $> EST
        , string "EDT" $> EDT
        , string "CST" $> CST
        , string "CDT" $> CDT
        , string "MST" $> MST
        , string "MDT" $> MDT
        , string "PST" $> PST
        , string "PDT" $> PDT
        , (Combine.Char.satisfy (charInRange 65 73)) |> map Military
        , (Combine.Char.satisfy (charInRange 75 90)) |> map Military
        , (Combine.Char.satisfy (charInRange 97 105)) |> map Military
        , (Combine.Char.satisfy (charInRange 107 122)) |> map Military
        ]


patchTwoDigitYear : Int -> Int
patchTwoDigitYear x =
    if x <= 49 then
        x + 2000
    else
        x + 1900


cfws : Parser s ()
cfws =
    many (maybe fws *> comment) $> ()


comment : Parser s ()
comment =
    lazy <|
        \() ->
            parens (many (maybe fws *> ccontent) *> maybe fws) $> ()


ccontent : Parser s ()
ccontent =
    choice [ ctext, quotedPair, comment ] $> ()


quotedPair : Parser s ()
quotedPair =
    choice
        [ string "\\" *> text
        , obsQuotedPair
        ]
        $> ()


text : Parser s Char
text =
    Combine.Char.satisfy
        (\c -> charInRange 0 9 c || charInRange 11 12 c || charInRange 14 127 c)


obsQuotedPair : Parser s Char
obsQuotedPair =
    string "\\" *> (Combine.Char.satisfy (charInRange 0 127))


charInRange : Int -> Int -> Char -> Bool
charInRange low high =
    Char.toCode >> keyInRange low high


keyInRange : Int -> Int -> Int -> Bool
keyInRange low high x =
    low <= x && x <= high


ctext : Parser s ()
ctext =
    choice
        [ noWsCtl
        , Combine.Char.satisfy (charInRange 33 39)
        , Combine.Char.satisfy (charInRange 42 91)
        , Combine.Char.satisfy (charInRange 93 126)
        ]
        $> ()


noWsCtl : Parser s Char
noWsCtl =
    choice
        [ Combine.Char.satisfy (charInRange 1 8)
        , Combine.Char.satisfy (charInRange 11 12)
        , Combine.Char.satisfy (charInRange 14 31)
        , Combine.Char.satisfy (charInRange 127 127)
        ]


nonWhitespaceControls : List (Parser s a) -> Parser s a
nonWhitespaceControls =
    choice


fws : Parser s ()
fws =
    choice
        [ maybe (many wsp *> crlf) *> many1 wsp
        ]
        $> ()


wsp : Parser s String
wsp =
    or (string " ") (string "\t")


crlf : Parser s String
crlf =
    regex "\x0D?\n"
