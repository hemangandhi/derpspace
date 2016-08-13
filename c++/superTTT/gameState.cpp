#include "gameState.h"
#include "Range.h"
#include <algorithm>
#include <stdexcept>

template <class Iter, class Pred>
Iter first(Iter f, Iter l, Pred p){
        for(; f != l && !p(*f); f++);
        return f;
}

CellState winByRow(InnerBoard in){
        auto hR = first(in.begin(), in.end(), 
                        [](auto v){
                                return std::all_of(v.begin(), v.end(),
                                                [](CellState cs){
                                                        return cs != CellState::empty && cs == v[0];
                                                });
                        });
        if(hR != in.end())
                return (*hR)[0];
        else
                return CellState::empty;
}

CellState wonBoard(InnerBoard in){
        CellState rw = winByRow(in);
        if(rw != CellState::empty) return rw;

        InnerBoard transpose;
        std::transform(Range(3), Range(3).end(),
                        [](int col){
                                std::transform(in.begin(), in.end(), transpose[col].begin(), 
                                                [&](auto row){
                                                        return row[col];
                                                })
                        });
        CellState cw = winByRow(transpose);
        if(cw != CellState::empty) return cw;

        if(std::all_of(Range(3), Range(3).end(),
                                [](int v){
                                        return in[v][v] != CellState::empty && in[v][v] == in[0][0];
                                })){
                return in[0][0];
        }

        if(std::all_of(Range(2, 7, 2), Range(2, 7, 2).end(),
                                [](int ind){
                                        return in[ind / 3][ind % 3] != CellState::empty && in[ind / 3][ind % 3] == in[0][2];
                                })){
                return in[0][2];
        }

        return CellState::empty;
}

GameState::GameState(){
        this->moves = MoveSet();
        Range inds = Range(3);
        std::for_each(inds.begin(), inds.end(),
                        [](int i){
                                std::for_each(inds.begin(), inds.end(),
                                                [&](int j){
                                                        this->moves.insert({{1, 1}, {i, j}});
                                                });
                        });
        this->board = Board_t();
        std::for_each(Range(81), Range(81).end(),
                        [](int i){
                                this->board[i/27][(i/9) % 3][(i/3) % 3][i % 3] = CellState::empty;
                        });
        this->turn = CellState::circle;
}

void GameState::move(Move m){
        if(this->moves.find(m) == this->moves.end())
                throw std::invalid_argument("Invalid move!");

        this->board[std::get<0>(std::get<0>(m))][std::get<1>(std::get<0>(m))][std::get<0>(std::get<1>(m))][std::get<1>(std::get<1>(m))] = this->turn;

        
}
