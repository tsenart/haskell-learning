package main

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"io"
	"math/rand"
	"os"
	"sort"
)

func main() {
	seed := flag.Int64("seed", 0, "Random number generator seed")
	aliens := flag.Int64("aliens", 10, "Initial alien population count")

	flag.Parse()

	if err := run(os.Stdin, *seed, *aliens); err != nil {
		fmt.Fprintf(os.Stderr, "error: %v", err)
		os.Exit(1)
	}
}

func run(in io.Reader, seed, aliens int64) error {
	var graph CityGraph
	if err := graph.DecodeFrom(in); err != nil {
		return err
	} else if err = Simulate(seed, aliens, 10000, graph); err != nil {
		return err
	} else {
		return graph.EncodeTo(os.Stdout)
	}
}

func Simulate(seed, alienCount, maxMoves int64, g CityGraph) error {
	rng := rand.New(rand.NewSource(seed))

	cities := g.Sorted()
	rng.Shuffle(len(cities), func(i, j int) {
		cities[i], cities[j] = cities[j], cities[i]
	})

	for i, city := range cities {
		alien := &Alien{ID: int64(i)}
		city.Aliens[alien.ID] = alien
		if alien.ID == alienCount {
			break
		}
	}

	for destroyed := 0; alienCount > 0 && destroyed != len(cities); {
		for _, city := range cities {
			if len(city.Aliens) == 0 || city.Destroyed {
				continue
			}

			neighbours := make([]*City, 0, len(city.Neighbours))
			for _, neighbour := range city.Neighbours {
				if city := g[neighbour]; city != nil && !city.Destroyed {
					neighbours = append(neighbours, city)
				}
			}

			sort.SliceStable(neighbours, func(i, j int) bool {
				return neighbours[i].Name <= neighbours[j].Name
			})

			if len(neighbours) == 0 {
				continue
			}

			neighbour := neighbours[rng.Intn(len(neighbours))]
			aliens := make([]*Alien, 0, len(city.Aliens))
			for _, alien := range city.Aliens {
				aliens = append(aliens, alien)
			}

			sort.SliceStable(aliens, func(i, j int) bool {
				return aliens[i].ID <= aliens[j].ID
			})

			alien := aliens[0]
			if _, ok := neighbour.Aliens[alien.ID]; ok {
				return fmt.Errorf("Alien %d already in %q", alien.ID, neighbour.Name)
			}

			delete(city.Aliens, alien.ID)
			if neighbour.Aliens[alien.ID] = alien; len(neighbour.Aliens) > 1 {
				neighbour.Destroyed = true
				destroyed++
				alienCount -= int64(len(neighbour.Aliens))
				fmt.Fprintf(os.Stderr, "%s has been destroyed by aliens: %v\n", neighbour.Name, neighbour.AlienIDs())
			} else if alien.Moves++; alien.Moves > maxMoves {
				return nil
			}
		}
	}

	return nil
}

type Alien struct {
	ID, Moves int64
}

type City struct {
	Name       string
	Aliens     map[int64]*Alien
	Neighbours map[string]string
	Destroyed  bool
}

func (c City) AlienIDs() []int64 {
	ids := make([]int64, 0, len(c.Aliens))
	for id := range c.Aliens {
		ids = append(ids, id)
	}
	sort.Slice(ids, func(i, j int) bool { return ids[i] <= ids[j] })
	return ids
}

type CityGraph map[string]*City

func (g *CityGraph) Sorted() []*City {
	cities := make([]*City, 0, len(*g))
	for _, city := range *g {
		cities = append(cities, city)
	}

	sort.SliceStable(cities, func(i, j int) bool {
		return cities[i].Name <= cities[j].Name
	})

	return cities
}

func (g CityGraph) EncodeTo(w io.Writer) error {
	for _, city := range g.Sorted() {
		if city.Destroyed {
			continue
		}

		_, err := fmt.Fprint(w, city.Name)
		if err != nil {
			return err
		}

		for _, direction := range []string{"north", "east", "south", "west"} {
			if neighbour, ok := city.Neighbours[direction]; ok {
				if _, err = fmt.Fprintf(w, " %s=%s", direction, neighbour); err != nil {
					return err
				}
			}
		}

		if _, err = fmt.Fprintln(w); err != nil {
			return err
		}
	}

	return nil
}

func (g *CityGraph) DecodeFrom(r io.Reader) error {
	if *g == nil {
		*g = map[string]*City{}
	}

	sc := bufio.NewScanner(r)

	for sc.Scan() {
		if line := bytes.TrimSpace(sc.Bytes()); len(line) == 0 {
			continue
		} else if err := g.decodeCity(line); err != nil {
			return err
		}
	}

	return sc.Err()
}

func (g *CityGraph) decodeCity(line []byte) error {
	fs := bytes.Fields(line)
	if len(fs) < 3 {
		return fmt.Errorf("city must have at least two neighbouring cities: %q", line)
	}

	c := City{
		Name:       string(fs[0]),
		Aliens:     make(map[int64]*Alien),
		Neighbours: make(map[string]string, len(fs)-1),
	}

	for _, neighbour := range fs[1:] {
		if tokens := bytes.SplitN(neighbour, []byte("="), 2); len(tokens) != 2 {
			return fmt.Errorf("invalid city neighbourhood format: %q", line)
		} else {
			direction, name := bytes.ToLower(tokens[0]), tokens[1]
			c.Neighbours[string(direction)] = string(name)
		}
	}

	(*g)[c.Name] = &c

	return nil
}
