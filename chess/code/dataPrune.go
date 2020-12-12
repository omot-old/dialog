package main

import (
	"bufio"
	"encoding/csv"
	"fmt"
	"log"
	"os"
	"strings"
	"time"
)

var header = []string{"Event", "Site", "White", "Black", "Result", "UTCDate", "WhiteElo", "BlackElo", "WhiteRatingDiff", "BlackRatingDiff", "ECO", "Opening", "TimeControl", "Termination", "game"}
var defMap = map[string]string{
	"Event":           "",
	"Site":            "",
	"White":           "",
	"Black":           "",
	"Result":          "",
	"UTCDate":         "",
	"WhiteElo":        "",
	"BlackElo":        "",
	"WhiteRatingDiff": "",
	"BlackRatingDiff": "",
	"ECO":             "",
	"Opening":         "",
	"TimeControl":     "",
	"Termination":     "",
	"game":            "",
}

func mapToList(x map[string]string) []string {
	var a []string
	i := 0
	for _, element := range header {
		a = append(a, x[element])
		i++
	}
	return a

}
func multRep(str string, rep []string) string {
	var ret string
	ret = str
	for _, elt := range rep {
		ret = strings.ReplaceAll(ret, elt, "")
	}
	return ret
}

var name = "long"

func main() {

	file, _ := os.Open("data/" + name + ".pgn")
	ofile, _ := os.Create("data/" + name + ".csv")

	scanner := bufio.NewScanner(file)
	ocsv := csv.NewWriter(ofile)
	ocsv.Write(header)

	realMap := defMap
	count, writ := 0, 0
	totStart := time.Now()
	now := time.Now()

	for scanner.Scan() {
		text := strings.ReplaceAll(scanner.Text(), "\n", "")
		if text == "" || len(text) <= 1 {
			continue
		} else if text[0] == '1' || text[0] == ' ' {

			if text[0] == '1' {
				realMap["game"] = text
			} else {
				realMap["game"] = ""
			}

			if strings.Contains(realMap["Event"], "Classical") {
				writ++
				ocsv.Write(mapToList(realMap))
			}
			realMap = defMap
			count++
			if count%100000 == 0 {
				fmt.Println("P:", fmt.Sprintf("%.1fm", float64(count)/1000000.0),
					"\tW:", writ,
					"\tL:", time.Now().Sub(now),
					"\tE:", time.Now().Sub(totStart),
					"\tR:", time.Now().Sub(now)*time.Duration(680-(count/100000)))
				now = time.Now()
			}
		} else if text[0] == '[' {
			var key, elt string
			key = text[1:strings.Index(text, " ")]
			elt = text[strings.Index(text, "\"")+1 : strings.LastIndex(text, "\"")]
			realMap[key] = elt
		} else {
			fmt.Println("malformed:", text)
		}
	}

	if err := scanner.Err(); err != nil {
		log.Fatal(err)
	}

}
