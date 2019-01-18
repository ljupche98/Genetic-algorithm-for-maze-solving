printMaze <- function(maze, rows, cols) {
  for (x in seq(1, rows)) {
    print(maze[((x-1)*cols +1) : (x*cols)])
  }
}

printMazeStep <- function(maze, rows, cols, pos, ch) {
  maze[pos] <- ch
  printMaze(maze, rows, cols)
  cat("\n")
}

printSolution <- function(maze, rows, cols, solution) {
  currentPosition <- grep('s', maze)
  ci <- (currentPosition - 1) %/% cols
  cj <- (currentPosition - 1) %% cols
  
  endPosition <- grep('e', maze)
  ei <- (endPosition - 1) %/% cols
  ej <- (endPosition - 1) %% cols
  
  it <- 1; n <- length(solution)
  add <- abs(ci - ei) + abs(cj - ej); invalid <- 0; coins <- 0
  printMazeStep(maze, rows, cols, currentPosition, 'x')
  for (move in solution) {
    oldPosition <- currentPosition
    
    if (move == 'U') {
      currentPosition <- moveUp(currentPosition, rows, cols)
    } else if (move == 'D') {
      currentPosition <- moveDown(currentPosition, rows, cols)       
    } else if (move == 'L') {
      currentPosition <- moveLeft(currentPosition, rows, cols)            
    } else if (move == 'R') {
      currentPosition <- moveRight(currentPosition, rows, cols)
    } else {
      print('Error: Incorrect solution format')
      return(-1)
    }
    
    if (maze[currentPosition] == '#') {
      invalid <- invalid + 1
      currentPosition <- oldPosition
    }
    
    if (maze[currentPosition] == 'c') {
      coins <- coins + 1
      maze[currentPosition] <- ' '
    }
    
    it <- it + 1
    ci <- (currentPosition - 1) %/% cols
    cj <- (currentPosition - 1) %% cols
    add <- min(add, abs(ci - ei) + abs(cj - ej))
    printMazeStep(maze, rows, cols, currentPosition, 'x');
    
    if (maze[currentPosition] == 'e') {
      print(c("Solution found in ", it, " steps and ", invalid, " invalid moves and took ", coins, " coins"))
      return(0)
    }
  }
  
  print(c("Solution wasn't found, but the minimum distance to the solution was ", add, " with ", invalid, " invalid moves and took ", coins, " coins"))
  return(0)
}

moveUp <- function(position, rows, cols) {
  newPosition <- position - cols
  
  if (newPosition < 1) {
    return (position)
  } else {
    return (newPosition)
  }
}

moveDown <- function(position, rows, cols) {
  newPosition <- position + cols
  
  if (newPosition > rows*cols) {
    return (position)
  } else { 
    return (position + cols)
  }
}

moveLeft <- function(position, rows, cols) {
  newPosition <- position - 1
  
  if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
    return (position)
  } else {
    return (position - 1)
  }
}

moveRight <- function(position, rows, cols) {
  newPosition <- position + 1
  
  if ((position - 1) %/% cols != (newPosition - 1) %/% cols) {
    return (position)
  } else { 
    return (position + 1)
  }
}

simulate <- function(maze, solution, rows, cols) {
  currentPosition <- grep('s', maze)
  ci <- (currentPosition - 1) %/% cols
  cj <- (currentPosition - 1) %% cols
  
  endPosition <- grep('e', maze)
  ei <- (endPosition - 1) %/% cols
  ej <- (endPosition - 1) %% cols
  
  add <- abs(ci - ei) + abs(cj - ej)
  it <- 1; n <- length(solution);
  for (move in solution) {
    oldPosition <- currentPosition
    
    if (move == 'U') {
      currentPosition <- moveUp(currentPosition, rows, cols)
    } else if (move == 'D') {
      currentPosition <- moveDown(currentPosition, rows, cols)       
    } else if (move == 'L') {
      currentPosition <- moveLeft(currentPosition, rows, cols)            
    } else if (move == 'R') {
      currentPosition <- moveRight(currentPosition, rows, cols)
    } else {
      print('Error: Incorrect solution format')
      return (-1)
    }
    
    if (maze[currentPosition] == '#') {
      currentPosition <- oldPosition
    }
    
    if (maze[currentPosition] == 'e') {
      return (-it)
    }
    
    it <- it + 1
    ci <- (currentPosition - 1) %/% cols
    cj <- (currentPosition - 1) %% cols
    add <- min(add, abs(ci - ei) + abs(cj - ej))
  }
  
  return (- rows * cols - add)
}

simulateInvalid <- function(maze, solution, rows, cols) {
  currentPosition <- grep('s', maze)
  ci <- (currentPosition - 1) %/% cols
  cj <- (currentPosition - 1) %% cols
  
  endPosition <- grep('e', maze)
  ei <- (endPosition - 1) %/% cols
  ej <- (endPosition - 1) %% cols
  
  add <- 0; mnd <- 1e9; invalid <- 0
  it <- 1; n <- length(solution);
  for (move in solution) {
    oldPosition <- currentPosition
    
    if (move == 'U') {
      currentPosition <- moveUp(currentPosition, rows, cols)
    } else if (move == 'D') {
      currentPosition <- moveDown(currentPosition, rows, cols)       
    } else if (move == 'L') {
      currentPosition <- moveLeft(currentPosition, rows, cols)            
    } else if (move == 'R') {
      currentPosition <- moveRight(currentPosition, rows, cols)
    } else {
      print('Error: Incorrect solution format')
      return (-1)
    }
    
    if (maze[currentPosition] == '#') {
      invalid <- invalid + 1
      currentPosition <- oldPosition
    }
    
    it <- it + 1
    ci <- (currentPosition - 1) %/% cols
    cj <- (currentPosition - 1) %% cols
    
    crd <- abs(ci - ei) + abs(cj - ej)
    
    if (crd < mnd) {
      mnd <- crd
      add <- invalid
    }
    
    if (maze[currentPosition] == 'e') {
      return (- mnd * (rows * cols) ^ 2 - it * (rows * cols) - add)
    }
  }
  
  return (- mnd * (rows * cols) ^ 2 - it * (rows * cols) - add)
}

simulateCoins <- function(maze, solution, rows, cols, coins) {
  currentPosition <- grep('s', maze)
  ci <- (currentPosition - 1) %/% cols
  cj <- (currentPosition - 1) %% cols
  
  endPosition <- grep('e', maze)
  ei <- (endPosition - 1) %/% cols
  ej <- (endPosition - 1) %% cols
  
  add <- abs(ci - ei) + abs(cj - ej)
  it <- 1; n <- length(solution); cur_coins <- 0
  for (move in solution) {
    oldPosition <- currentPosition
    
    if (move == 'U') {
      currentPosition <- moveUp(currentPosition, rows, cols)
    } else if (move == 'D') {
      currentPosition <- moveDown(currentPosition, rows, cols)       
    } else if (move == 'L') {
      currentPosition <- moveLeft(currentPosition, rows, cols)            
    } else if (move == 'R') {
      currentPosition <- moveRight(currentPosition, rows, cols)
    } else {
      print('Error: Incorrect solution format')
      return(-1)
    }
    
    if (maze[currentPosition] == '#') {
      currentPosition <- oldPosition
    }
    
    if (maze[currentPosition] == 'c') {
      cur_coins <- cur_coins + 1
      maze[currentPosition] = '.'
    }
    
    if (maze[currentPosition] == 'e') {
      return(- (coins - cur_coins) * (1 + !!coins) * rows * cols - it)
    }
    
    it <- it + 1
    ci <- (currentPosition - 1) %/% cols
    cj <- (currentPosition - 1) %% cols
    add <- min(add, abs(ci - ei) + abs(cj - ej))
  }
  
  return (- (1 + coins) * (1 + !!coins) * rows * cols - (coins - cur_coins) * (rows - 1) * (cols - 1) - add)
}

simulateCoinsMinDist <- function(maze, solution, rows, cols, coins) {
  currentPosition <- grep('s', maze)
  ci <- (currentPosition - 1) %/% cols
  cj <- (currentPosition - 1) %% cols
  
  endPosition <- grep('e', maze)
  ei <- (endPosition - 1) %/% cols
  ej <- (endPosition - 1) %% cols
  
  cv <- c(); sz <- 0; md <- 1e9
  for (i in 1 : length(maze)) {
    if (maze[i] == 'c') {
      cv[coins - sz] <- i
      sz <- sz + 1
    }
  }
  
  for (i in 1 : sz) {
    xi <- (cv[i] - 1) %/% cols
    xj <- (cv[i] - 1) %% cols
    md <- min(md, abs(ci - xi) + abs(cj - xj))
  }
  
  add <- abs(ci - ei) + abs(cj - ej)
  it <- 1; n <- length(solution); cur_coins <- 0
  for (move in solution) {
    oldPosition <- currentPosition
    
    if (move == 'U') {
      currentPosition <- moveUp(currentPosition, rows, cols)
    } else if (move == 'D') {
      currentPosition <- moveDown(currentPosition, rows, cols)       
    } else if (move == 'L') {
      currentPosition <- moveLeft(currentPosition, rows, cols)            
    } else if (move == 'R') {
      currentPosition <- moveRight(currentPosition, rows, cols)
    } else {
      print('Error: Incorrect solution format')
      return(-1)
    }
    
    if (maze[currentPosition] == '#') {
      currentPosition <- oldPosition
    }
    
    if (maze[currentPosition] == 'c') {
      cur_coins <- cur_coins + 1
      maze[currentPosition] = '.'
    }
    
    for (i in 1 : sz) {
      xi <- (cv[i] - 1) %/% cols
      xj <- (cv[i] - 1) %% cols
      md <- min(md, abs(ci - xi) + abs(cj - xj))
    }
    
    if (maze[currentPosition] == 'e') {
      return(- (coins - cur_coins) * (1 + !!coins) * rows * cols * (rows + cols) - md * (rows + cols) - it)
    }
    
    it <- it + 1
    ci <- (currentPosition - 1) %/% cols
    cj <- (currentPosition - 1) %% cols
    add <- min(add, abs(ci - ei) + abs(cj - ej))
  }
  
  return (- (1 + coins) * (1 + !!coins) * rows * cols * (rows + cols) -
            (coins - cur_coins) * rows * cols * (rows + cols) - md * (rows + cols) - add)
}

simulateSolution <- function(maze, solution, rows, cols, coins) {
  if (!coins) {
    #return(simulate(maze, solution, rows, cols))
    return(simulateInvalid(maze, solution, rows, cols))
  } else {
    #return(simulateCoins(maze, solution, rows, cols, coins))
    return(simulateCoinsMinDist(maze, solution, rows, cols, coins))
  }
}

BFS <- function(maze, rows, cols) {
  comi <- c(0, 1, 0, -1); comj <- c(1, 0, -1, 0)
  q <- c(grep('e', maze)); q[rows * cols] <- 0; l <- 1; r <- 2
  dist <- integer(rows * cols) - 1; dist[grep('e', maze)] <- 0
  
  while (l < r) {
    x <- q[l]; l <- l + 1
    i <- (x - 1) %/% cols; j <- (x - 1) %% cols
    
    for (k in 1 : 4) {
      if (i + comi[k] >= 0 && i + comi[k] < rows && j + comj[k] >= 0 && j + comj[k] < cols) {
        if (dist[(i + comi[k]) * cols + (j + comj[k]) + 1] == -1) {
          if (maze[(i + comi[k]) * cols + (j + comj[k]) + 1] != '#') {
            dist[(i + comi[k]) * cols + (j + comj[k]) + 1] <- dist[x] + 1
            q[r] <- (i + comi[k]) * cols + (j + comj[k]) + 1; r <- r + 1
          }
        }
      }
    }
  }
  
  return(dist)
}

initSolution <- function(rows, cols, coins) {
  v <- c('L', 'R', 'D', 'U')
  
  ret <- c()
  it <- 0; maxit <- (1 + !!coins) * rows * cols
  while (it < maxit) {
    ret[maxit - it] <- v[sample(1 : 4, 1)]
    it <- it + 1
  }
  
  return(ret)
}

initSolutions <- function(rows, cols, maxit, coins) {
  it <- 0; ret <- list()
  
  while (it < maxit) {
    ret[[maxit - it]] <- initSolution(rows, cols, coins)
    it <- it + 1
  }
  
  return(ret)
}

eval <- function(maze, rows, cols, solutions, coins) {
  it <- 0; ret <- c()
  
  for (sol in solutions) {
    it <- it + 1
    ret[[it]] <- simulateSolution(maze, sol, rows, cols, coins)
  }
  
  return(ret)
}

randomSelection <- function(fn) {
  return(sample(1 : length(fn), 2))
}

rankProportional <- function(fn) {
  rank <- c(); n <- length(fn)
  
  for (i in 1 : n) {
    u <- which.min(fn)
    fn[u] <- 1e9; rank[u] <- i
  }
  
  cum <- c(0);
  for (i in 1 : n) {
    cum[i + 1] <- cum[i] + rank[i]
  }
  
  ret <- c(); size <- 0
  while (size < 2) {
    rn <- sample(0 : cum[length(cum) - 1] - 1, 1)
    
    for (i in 1 : n) {
      if (cum[i] <= rn && rn < cum[i + 1]) {
        ret[size + 1] <- i; size <- size + 1
        break
      }
    }
    
    ret <- unique(ret); size <- length(ret)
  }
  
  return(ret)
}

fnBest <- function(fn) {
  u <- which.max(fn); fn[u] <- -1e9
  return(c(u, which.max(fn)))
}

select <- function(fn) {
  #return(fnBest(fn))
  return(rankProportional(fn))
  return(randomSelection(fn))
}

findBest <- function(fn, k) {
  ret <- c()
  
  for (i in 1 : k) {
    u <- which.max(fn)
    ret[i] <- u; fn[u] <- -2 ** 30
  }
  
  return(ret)
}

selectElite <- function(solutions, fn, elitism) {
  elite <- findBest(fn, elitism); ret <- list()
  
  for (i in 1 : elitism) {
    ret[[i]] = solutions[[elite[i]]]
  }
  
  return(ret)
}

crossover <- function(solutions, sel) {
  u <- solutions[[sel[1]]]; v <- solutions[[sel[2]]]
  n <- length(u);
  
  #print(u)
  #print(v)
  
  #rn <- sample(1 : (n - 1), 1)
  
  #print(list(c(u[1 : rn], v[(rn + 1) : n]),
  #           c(v[1 : rn], u[(rn + 1) : n])))
  #return(list(c(u[1 : rn], v[(rn + 1) : n]),
  #            c(v[1 : rn], u[(rn + 1) : n])))
  
  it <- 1;
  while (it <= n && u[it] == v[it]) {
    it <- it + 1
  }
  
  # u == v
  if (it >= n) {
    return(list(u, v))
  }
  
  # u = Axy
  # v = Azw
  if (it == n - 1) {
    return(list(c(u[1 : (it - 1)], u[it], v[it + 1]), c(v[1 : (it - 1)], v[it], u[it + 1])))
  }
  
  if (it == 1) {
    rn <- sample(2 : (n - 1), 1)
    return(list(c(u[1 : rn], v[(rn + 1) : n]), c(v[1 : rn], u[(rn + 1) : n])))
  }
  
  rn <- sample(1 : (n - it), 1)
  return(list(c(u[1 : (it - 1)], u[it : (it + rn - 1)], v[(it + rn) : n]),
              c(v[1 : (it - 1)], v[it : (it + rn - 1)], u[(it + rn) : n])))
}

mutate <- function(x, p) {
  v = c('L', 'R', 'D', 'U')
  
  n <- length(x)
  for (i in 1 : n) {  
    y <- sample(1 : 100, 1)
    
    if (y <= p) {
      y <- sample(1 : 4, 1)
      x[i] <- v[y]
    }
  }
  
  return(x)
}

geneticAlgorithm <- function(maze, rows, cols) {
  N <- 51; elitism <- 1; pm <- 17; maxit <- 100; coins <- length(maze[maze == "c"])
  print_sol <- 1
  
  solutions <- initSolutions(rows, cols, N, coins)
  print(eval(maze, rows, cols, solutions, coins))
  
  for (i in 1 : maxit) {
    fn <- eval(maze, rows, cols, solutions, coins)
    new_solutions <- selectElite(solutions, fn, elitism)
    
    size <- length(new_solutions)
    while (size < N) {
      sel <- select(fn)
      
      combs <- crossover(solutions, sel)
      
      fm <- mutate(combs[[1]], pm)
      sm <- mutate(combs[[2]], pm)
      
      new_solutions[[size + 1]] <- fm
      new_solutions[[size + 2]] <- sm
      
      size = size + 2
    }
    
    solutions = new_solutions
  }
  
  fn <- eval(maze, rows, cols, solutions, coins)
  print(fn)
  
  sol = solutions[[which.max(fn)]]
  if (print_sol == 1) {
    printSolution(maze, rows, cols, sol)
  }
  
  print(sol)
  print(c("Fitness = ", fn[which.max(fn)]))
}

maze1 <- c(' ', ' ', ' ', ' ', 'e',
           ' ', '#', '#', '#', '#',
           ' ', ' ', 's', ' ', ' ',
           '#', '#', '#', '#', ' ',
           ' ', ' ', ' ', ' ', ' ')
rows1 <- 5
cols1 <- 5
solution1 <- c('L', 'L','U', 'U', 'R', 'R', 'R', 'R', 'R')
geneticAlgorithm(maze1, rows1, cols1)

maze3 <- c(' ', ' ', ' ', ' ', 'e',
           ' ', '#', '#', '#', '#',
           ' ', ' ', 's', ' ', ' ',
           '#', '#', '#', '#', ' ',
           ' ', 'c', ' ', 'c', ' ')
rows3 <- 5
cols3 <- 5
geneticAlgorithm(maze3, rows3, cols3)

maze4 <- c(' ', ' ', ' ', '#', ' ', ' ', 'c',
           'c', 'c', 's', ' ', ' ', '#', 'c',
           'c', '#', '#', '#', '#', '#', ' ',
           ' ', '#', ' ', ' ', ' ', '#', ' ',
           ' ', '#', '#', ' ', '#', ' ', '#',
           ' ', '#', 'c', '#', ' ', 'c', ' ',
           ' ', '#', ' ', ' ', 'c', '#', 'c',
           ' ', '#', 'c', '#', '#', '#', ' ',
           ' ', '#', ' ', '#', '#', 'e', 'c',
           'c', ' ', ' ', '#', ' ', '#', '#')

rows4 <- 10
cols4 <- 7
geneticAlgorithm(maze4, rows4, cols4)
  
maze2 <- c('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', ' ', ' ', '#', ' ', '#', '#',
           '#', '#', 'e', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', ' ', ' ', ' ', '#', '#',
           '#', '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', '#', ' ', ' ', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', ' ',
           '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', '#', '#', ' ', '#', '#', ' ', ' ', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ',
           '#', ' ', '#', ' ', '#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#', ' ', '#', ' ',
           '#', ' ', ' ', ' ', '#', ' ', '#', ' ', '#', '#', '#', '#', ' ', '#', ' ', '#', ' ',
           '#', '#', ' ', '#', '#', '#', '#', ' ', '#', '#', ' ', ' ', ' ', ' ', ' ', '#', 's',
           '#', '#', ' ', ' ', '#', ' ', ' ', ' ', '#', '#', ' ', '#', '#', '#', ' ', '#', ' ',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', ' ', '#', '#',
           '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#')

solution2 <- c('U', 'U', 'U', 'U', 'U', 'U', 'L', 'L', 'D', 'L', 'L', 'L', 'L', 'L', 'D', 'D', 'D', 'L', 'L', 'L', 'L', 'U', 'U', 'U', 'U', 'L', 'U', 'U', 'U', 'U', 'L', 'L', 'U', 'U')
cols2 <- 17
rows2 <- 18
geneticAlgorithm(maze2, rows2, cols2)
