/**
 * The engine for playing Baroque/Ultima/that absolute nutter Chess variant that
 * captivated me. The rules are detailed below:
 * https://en.wikipedia.org/wiki/Baroque_chess
 * I'm however implementing the "pure" rules at:
 * http://www.inference.org.uk/mackay/ultima/ultima.html
 * 
 * To play, create a blank Board using Board::new_board(), then
 * feed it moves via make_move.
 * This is very WIP. Ugly-ass code below.
 * TODO: Only the engine is partially completed, no winning condition (capturing
 * the king will keep the game going until it crashes when you try to move a
 * Coordinator)
 */
pub mod baroque {
    use std::char;
    use std::cmp;
    use std::fmt;
    use std::collections::HashMap;

    const BOARD_WIDTH: usize = 8;
    const BOARD_HEIGHT: usize = 8;

    // TODO: Switch to a more general, -1/0/+1 system.
    #[derive(Copy, Clone, PartialEq)]
    enum NS { N, S, O }

    #[derive(Copy, Clone, PartialEq)]
    enum EW { E, W, O }

    #[derive(Copy, Clone)]
    struct Direction {
        horizontal: EW,
        vertical: NS,
    }

    impl Direction {
        fn is_valid(&self) -> bool {
            self.horizontal != EW::O || self.vertical != NS::O
        }

        fn cardinal_directions() -> Vec<Direction> {
            vec![
                Direction { vertical: NS::N, horizontal: EW::O },
                Direction { vertical: NS::S, horizontal: EW::O },
                Direction { vertical: NS::O, horizontal: EW::E },
                Direction { vertical: NS::O, horizontal: EW::W },
            ]
        }
    }

    /**
     * A basic x and y coordinate system with helpers.
     * Will check for border cases to prevent overflowing the board 
     * automatically, and is totally safe to pass to other classes.
     */
    #[derive(Copy, Clone, PartialEq, Hash, Eq)]
    pub struct Coord {
        x: usize,
        y: usize,
    }

    impl fmt::Display for Coord {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let y = self.get_y() + 1;
            // x is guaranteed to be safe
            let x = char::from_u32('a' as u32 + self.get_x() as u32).unwrap();
            write!(f, "{}{}", x, y)
        }
    }

    impl Coord {
        pub fn new(x: usize, y: usize) -> Coord {
            // Safe guards to prevent overflow.
            // usize type will prevent negative values.
            Coord {
                x: cmp::min(x, BOARD_WIDTH - 1),
                y: cmp::min(y, BOARD_HEIGHT - 1),
            }
        }

        fn get_x(&self) -> usize {
            self.x
        }

        fn get_y(&self) -> usize {
            self.y
        }

        fn sub_x(&self) -> usize {
            self.x.checked_sub(1).unwrap_or(BOARD_WIDTH)
        }

        fn sub_y(&self) -> usize {
            self.y.checked_sub(1).unwrap_or(BOARD_HEIGHT)
        }

        fn horizontally_aligned(&self, c: Coord) -> bool {
            self.x != c.x && self.y == c.y
        }

        fn vertically_aligned(&self, c: Coord) -> bool {
            self.x == c.x && self.y != c.y
        }

        fn orthogonally_aligned(&self, c: Coord) -> bool {
            self.vertically_aligned(c) || self.horizontally_aligned(c)
        }

        fn diagonally_aligned(&self, c: Coord) -> bool {
            *self != c &&
                (self.x as isize - c.x as isize).abs() == (self.y as isize - c.y as isize).abs()
        }

        fn aligned(&self, c: Coord) -> bool {
            self.orthogonally_aligned(c) || self.diagonally_aligned(c)
        }

        fn adjacent_coord(&self, dir: Direction) -> Option<Coord> {
            let x = match dir.horizontal {
                EW::E => self.x + 1,
                EW::W => self.sub_x(),
                EW::O => self.x,
            };
            let y = match dir.vertical {
                NS::N => self.y + 1,
                NS::S => self.sub_y(),
                NS::O => self.y,
            };
            if x == BOARD_WIDTH || y == BOARD_HEIGHT {
                None
            } else {
                Some(Coord::new(x, y))
            }
        }

        fn relative_direction(&self, c: Coord) -> Direction {
            let mut horizontal = EW::O;
            let mut vertical = NS::O;
            if self.orthogonally_aligned(c) || self.diagonally_aligned(c) {
                if c.x < self.x {
                    horizontal = EW::W; 
                }
                if c.x > self.x {
                    horizontal = EW::E;
                }
                if c.y < self.y {
                    vertical = NS::S;
                }
                if c.y > self.y {
                    vertical = NS::N;
                }
            }
            Direction {horizontal, vertical}
        }

        fn get_neighboring_coords(&self) -> Vec<Coord> {
            let mut list: Vec<Coord> = Vec::new();
            let x_coords = [self.sub_x(), self.x, self.x + 1];
            let y_coords = [self.sub_y(), self.y, self.y + 1];
            for i in x_coords.iter().cloned() {
                for j in y_coords.iter().cloned() {
                    if (i != self.x || j != self.y) && i != BOARD_WIDTH && j != BOARD_HEIGHT {
                            list.push(Coord::new(i, j));
                        }
                }
            }
            list
        }
    }

    struct CoordWalk {
        current: Coord,
        end: Coord,
    }

    impl CoordWalk {
        fn new(start: Coord, end: Coord) -> CoordWalk {
            CoordWalk { current: start, end }
        }
    }

    impl Iterator for CoordWalk {
        type Item = Coord;

        fn next(&mut self) -> Option<Self::Item> {
            let direction = self.current.relative_direction(self.end);
            // Don't spin in place or go nowhere.
            if !direction.is_valid() {
                return None;
            }
            match self.current.adjacent_coord(direction) {
                Some(c) if self.current != self.end => {
                    self.current = c;
                    Some(c)
                },
                _ => None,
            }
        }
    }

    enum GameState {
        Turn(Side),
        Victory(Side),
    }

    pub struct Board {
        squares: HashMap<Coord, Piece>,
        state: GameState
    }

    impl Board {
        pub fn new_blank_board() -> Board {
            Board {
                squares: HashMap::new(),
                state: GameState::Turn(Side::White),
            }
        }

        pub fn new_board() -> Board {
            let mut squares = HashMap::new();
            // put the pincers
            for i in 0..8 {
                squares.insert(Coord::new(i, 1), Piece::new(Side::White, PieceType::Pincer));
                squares.insert(Coord::new(i, 6), Piece::new(Side::Black, PieceType::Pincer));
            }

            squares.insert(Coord::new(0, 0), Piece::new(Side::White, PieceType::Coordinator));
            squares.insert(Coord::new(0, 7), Piece::new(Side::Black, PieceType::Coordinator));

            squares.insert(Coord::new(7, 0), Piece::new(Side::White, PieceType::Immobilizer));
            squares.insert(Coord::new(7, 7), Piece::new(Side::Black, PieceType::Immobilizer));

            squares.insert(Coord::new(1, 0), Piece::new(Side::White, PieceType::LongLeaper));
            squares.insert(Coord::new(1, 7), Piece::new(Side::Black, PieceType::LongLeaper));
            squares.insert(Coord::new(6, 0), Piece::new(Side::White, PieceType::LongLeaper));
            squares.insert(Coord::new(6, 7), Piece::new(Side::Black, PieceType::LongLeaper));

            squares.insert(Coord::new(2, 0), Piece::new(Side::White, PieceType::Chameleon));
            squares.insert(Coord::new(2, 7), Piece::new(Side::Black, PieceType::Chameleon));
            squares.insert(Coord::new(5, 0), Piece::new(Side::White, PieceType::Chameleon));
            squares.insert(Coord::new(5, 7), Piece::new(Side::Black, PieceType::Chameleon));

            squares.insert(Coord::new(3, 0), Piece::new(Side::White, PieceType::Withdrawer));
            squares.insert(Coord::new(4, 7), Piece::new(Side::Black, PieceType::Withdrawer));

            squares.insert(Coord::new(4, 0), Piece::new(Side::White, PieceType::King));
            squares.insert(Coord::new(3, 7), Piece::new(Side::Black, PieceType::King));

            Board {
                squares,
                state: GameState::Turn(Side::White),
            }
        }

        pub fn display(&self) {
            println!("");
            for y in (0..BOARD_HEIGHT).rev() {
                print!("{}  ", y);
                for x in 0..BOARD_WIDTH {
                    let character = match self.get_piece(Coord::new(x, y)) {
                        None => '.',
                        Some((piece, _)) => match (piece.get_side(), piece.get_type()) {
                            (Side::White, PieceType::King) => '♔',
                            (Side::White, PieceType::Withdrawer) => '♕',
                            (Side::White, PieceType::Coordinator) => '♖',
                            (Side::White, PieceType::Chameleon) => '♗',
                            (Side::White, PieceType::LongLeaper) => '♘',
                            (Side::White, PieceType::Pincer) => '♙',
                            (Side::White, PieceType::Immobilizer) => '⧖',
                            (Side::Black, PieceType::King) => '♚',
                            (Side::Black, PieceType::Withdrawer) => '♛',
                            (Side::Black, PieceType::Coordinator) => '♜',
                            (Side::Black, PieceType::Chameleon) => '♝',
                            (Side::Black, PieceType::LongLeaper) => '♞',
                            (Side::Black, PieceType::Pincer) => '♟',
                            (Side::Black, PieceType::Immobilizer) => '⧗',
                        }
                    };
                    print!(" {} ", character);
                }
                println!("\n");
            }
            print!("    ");
            for x in 0..BOARD_WIDTH {
                print!("{}  ", x);
            }
            println!("\n");
        }

        pub fn get_squares(&self) -> &HashMap<Coord, Piece> {
            &self.squares
        }

        #[cfg(test)]
        fn put_piece(&mut self, p: Piece, c: Coord) {
            self.squares.insert(c, p);
        }
        
        /** 
         * Get the piece at the coordinate. Returns an Option encapsulating
         * both the piece and the provided coordinate, since in some cases that
         * this function is called you may not get the coordinate back (like in
         * the other helper functions)
         */
        fn get_piece(&self, c: Coord) -> Option<(&Piece, Coord)> {
            self.squares.get(&c).map(|p| (p, c))
        }

        fn get_pieces(&self, list: &[Coord]) -> Vec<(&Piece, Coord)> {
            list.iter()
                .filter_map(|c| self.get_piece(*c))
                .collect()
        }

        fn get_neighboring_pieces(&self, c: Coord) -> Vec<(&Piece, Coord)> {
            self.get_pieces(&c.get_neighboring_coords())
        }

        // Below are helper functions that does not really care about individual
        // piece behaviors, like how they move, but only their allegiance.

        /**
         * Check that a piece can move straight from start to end without any
         * blocking piece in between. Useful for Rook/Bishop/Queen movement.
         */
        fn no_obstacles(&self, start: Coord, end: Coord) -> bool {
            if !start.relative_direction(end).is_valid() {
                return false;
            }
            let walker = CoordWalk::new(start, end);
            self.get_pieces(&walker.collect::<Vec<Coord>>()).is_empty()
        }

        /**
         * Get the position of the King.
         */
        fn get_king_position(&self, side: Side) -> Coord {
            for (c, p) in self.squares.iter() {
                if p.get_type() == PieceType::King &&
                    p.get_side() == side {
                        return *c;
                    }
            }
            panic!("Cannot find King???");
        }

        // Now the game logic functions
        pub fn make_move(&mut self, start: Coord, end: Coord) -> Result<Vec<String>, String> {
            let removed_coords_list;
            // Borrows self.squares immutably to check whether a move is valid ot not.
            if let Some((piece, _)) = self.get_piece(start) {
                match self.state {
                    GameState::Turn(side) if side != piece.get_side() => {
                        return
                            Err(format!("Cannot move {}, it is currently {}'s turn", piece, side));
                    },
                    GameState::Victory(side) => {
                        return Err(format!("Game has ended; {} won", side));
                    },
                    _ => (),
                }
                if let Some(captured_coords) = piece.check_valid_move(self, start, end) {
                    removed_coords_list = captured_coords;
                    self.state = match piece.get_side() {
                        Side::Black => GameState::Turn(Side::White),
                        Side::White => GameState::Turn(Side::Black),
                    };
                } else {
                    return Err(format!("{} from {} to {} is an invalid move", piece, start, end));
                }

            } else {
                return Err(format!("No piece detected at {}.", start));
            }
            // We mutate self.squares here, after returning all immutable
            // references to it taken above to ensure memory safety.
            // unwrap()'s are safe here because we have checked for piece
            // existence above. And if something went wrong then we want it to
            // panic anyway.
            let mut messages = Vec::new();
            let piece = self.squares.remove(&start).unwrap();
            messages.push(format!("Moving {} from {} to {}", piece, start, end));
            for c in removed_coords_list {
                let removed_piece = self.squares.remove(&c).unwrap();
                messages.push(format!("Captured {} at {}", removed_piece, c));
            }
            self.squares.insert(end, piece);
            Ok(messages)
        }

        pub fn get_possible_moves(&self, side: Side) -> Vec<(Coord, Coord)> {
            let mut result = Vec::new();
            for (c, p) in self.squares.iter() {
                if p.get_side() == side {
                    for x in 0..BOARD_WIDTH {
                        for y in 0..BOARD_HEIGHT {
                            let end = Coord::new(x, y);
                            if p.check_valid_move(self, *c, end).is_some() {
                                result.push((*c, end));
                            }
                        }
                    }
                }
            }
            result
        }

        // For testing.
        #[cfg(test)]
        fn is_immobilized(&self, coord: Coord) -> Option<bool> {
            self.get_piece(coord).map(|(p, c)| p.is_immobilized(self, c))
        }
    }

    #[derive(Copy, Clone, PartialEq)]
    pub enum Side {
        Black,
        White,
    }

    impl fmt::Display for Side {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let name = match self {
                Side::Black => "Black",
                Side::White => "White",
            };
            write!(f, "{}", name)
        }
    }

    #[derive(Copy, Clone, PartialEq)]
    pub enum PieceType {
        King,
        Pincer,
        Withdrawer,
        LongLeaper,
        Coordinator,
        Immobilizer,
        Chameleon,
    }

    impl fmt::Display for PieceType {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let name = match self  {
                PieceType::King => "King",
                PieceType::Pincer => "Pincer",
                PieceType::Withdrawer => "Withdrawer",
                PieceType::LongLeaper => "LongLeaper",
                PieceType::Coordinator => "Coordinator",
                PieceType::Immobilizer => "Immobilizer",
                PieceType::Chameleon => "Chameleon",
            };
            write!(f, "{}", name)
        }
    }

    impl PieceType {
        fn non_chameleon_types() -> Vec<PieceType> {
            vec![
                PieceType::King,
                PieceType::Pincer,
                PieceType::Withdrawer,
                PieceType::LongLeaper,
                PieceType::Coordinator,
                PieceType::Immobilizer,
            ]
        }
    }

    /**
     * Struct representing a piece in the game.
     * The struct holds the behavior code of the piece. It could live in the
     * Board struct since many pieces need to know the entire board, but is put
     * here for readability.
     * List of pieces in Baroque/Ultima put here for quick reference:
     * - King: Identical to the standard chess King. Moves and captures 1 square
     * in any direction, and must be protected at all cost - checkmating the 
     * enemy King wins the game.
     * - Pincers: Replacing the Pawns of regular chess, the Pincers move like
     * standard Rooks (through any number of unblocked squares orthogonally),
     * but captures by pincing an enemy unit between themselves and another
     * allied piece so there's no space between them. They can only pince
     * enemy units that are adjacent orthogonally.
     * All remaining pieces move like chess Queens (orthogonally and
     * diagonally), but captures differently.
     * - Coordinator: Captures any enemy pieces that share the Coordinator's
     * rank and the allied King's file, or its file and the King's rank. In
     * other words, if the Coordinator and the King are placed in two opposing
     * corners of a rectangle that aligns with the board, then any enemy pieces
     * in the remaining corners are captured.
     * - Long Leapers: Captures by jumping over enemy pieces on its path. Can
     * make multiple captures in one move as long as there's at least an empty
     * square behind each piece for the Long Leaper to land. Cannot jump over
     * adjacent pieces (e.g. 2 pieces at once) or allied pieces.
     * - Withdrawer: Captures the adjacent enemy piece that it moved away from
     * at the beginning of the move. The captured piece must be positioned
     * directly opposite of the move direction, so the Withdrawer cannot capture
     * multiple adjacent enemy pieces.
     * - Immobilizer: Cannot capture, but any enemy pieces adjacent to it is
     * immobilized and unable to move. For a full list of rules determining when
     * a piece is frozen when multiple Immobilizers/Chameleons are involved, see
     * the `is_immobilized` function.
     * - Chameleon: Captures by imitating the capturing method of the piece it's
     * trying to capture. So it captures enemy pincers by pincing them, enemy
     * Coordinators by coordinating them with the Chameleon's King, etc. A
     * Chameleon cannot capture the enemy Immobilizer, but will freeze that
     * Immobilizer (and no other pieces), and it cannot capture Chameleons.
     * Note: when capturing, it must imitate the target piece's method of moving
     * as well, so it cannot capture Pincers if it moved diagonally even if it
     * pinces them at the end of the move. Similarly, it can only capture (or
     * more accurately check) the King if it's adjacent to the King at the start
     * of the move.  And finally if the Chameleon captured any Long Leapers by 
     * leaping over them it cannot attempt to capture any other pieces since no
     * other pieces in the game can leap.
     */
    pub struct Piece {
        side: Side,
        piece_type: PieceType,
    }

    impl fmt::Display for Piece {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{} {}", self.side, self.piece_type)
        }
    }

    impl Piece {
        fn new(side: Side, piece_type: PieceType) -> Piece {
            Piece {side, piece_type}
        }

        fn get_type(&self) -> PieceType {
            self.piece_type
        }

        fn get_side(&self) -> Side {
            self.side
        }

        fn is_allied(&self, p: &Piece) -> bool {
            self.get_side() == p.get_side()
        }

        /**
         * Check if the piece is immobilized. Uses Cambridge rules, in which a
         * piece X is frozen if either of the below complete conditions apply:
         * 1. It is an Immobilizer and is near a hostile Chameleon.
         * 2. It is near a hostile Immobilizer that is not next to a friendly
         * Immobilizer or Chameleon other than X.
         * These rules prevent mass freezing situations (where two Immobilizers
         * freeze each other and all surrounding pieces) that can lead to an
         * unfun game.
         * NOTE: Consider the following situation:
         * Any White - Black Immobilizer - White Immobilizer - Black Chameleon
         * According to the rules, the Black Immobilizer is free to move, but it
         * won't freeze any White pieces either. You would think that an
         * Immobilizer's ability to freeze is tied with its ability to move, but
         * this is not the case.
         */
        fn is_immobilized(&self, board: &Board, position: Coord) -> bool {
            self.is_immobilized_internal(board, position, None)
        }

        // If excluded_coord is provided then this is the second immobilized
        // check (checking if an Immobilizer can freeze said excluded coord);
        // we don't need to go further.
        fn is_immobilized_internal(&self, board: &Board, position: Coord,
                                   excluded_coord: Option<Coord>) -> bool {
            let is_second_loop = excluded_coord.is_some();
            let excluded_coord = excluded_coord.unwrap_or_else(|| position);
            for (piece, coord) in board.get_neighboring_pieces(position) {
                if excluded_coord != coord && !self.is_allied(piece) {
                    if piece.get_type() == PieceType::Chameleon &&
                        self.get_type() == PieceType::Immobilizer {
                            return true;
                        }
                    if piece.get_type() == PieceType::Immobilizer &&
                        (is_second_loop ||
                         !piece.is_immobilized_internal(board, coord, Some(position))) {
                           return true;
                       }
                }
            }
            false
        }

        /**
         * Check if there's an enemy piece in the target direction, and an ally
         * piece beyond that. If that's the case then return the coord in the
         * target direction.
         * For the Pincers.
         */
        fn is_pincing(&self, board: &Board, start: Coord, direction: Direction) -> Option<Coord> {
            // Get the Option containing the coord adjacent to this coord in the specified
            // direction, then use get_piece on it if any.
            let get_piece_from_next =
                |coord: Coord| coord.adjacent_coord(direction).and_then(|c| board.get_piece(c));
            if let Some((enemy_piece, enemy_coord)) = get_piece_from_next(start) {
                if let Some((ally_piece, _)) = get_piece_from_next(enemy_coord){
                    if !self.is_allied(enemy_piece) && self.is_allied(ally_piece) {
                        return Some(enemy_coord);
                    }
                }
            }
            None
        }

        /**
         * For the Leaper. Leapers cannot jump over adjacent enemy.
         */
        fn is_valid_leaper_path(&self, board: &Board, start: Coord, end: Coord) -> bool {
            if !start.relative_direction(end).is_valid() {
                return false;
            }
            let mut previous_coord_has_piece = false;
            for c in CoordWalk::new(start, end) {
                if let Some((piece, _)) = board.get_piece(c) {
                    // stop if we're not expecting a piece or is an ally
                    if previous_coord_has_piece || self.is_allied(piece) {
                        return false;
                    } else {
                        previous_coord_has_piece = true;
                    }
                } else {
                    previous_coord_has_piece = false;
                }
            }
            // if the end square is occupied, then the path is not valid.
            return !previous_coord_has_piece;
        }

        fn get_coordinated_pieces(&self, board: &Board, position: Coord) -> Vec<Coord> {
            let king_position = board.get_king_position(self.get_side());
            vec![
                Coord::new(position.get_x(), king_position.get_y()),
                Coord::new(king_position.get_x(), position.get_y()),
            ].into_iter().filter(|c| board.get_piece(*c)
                                 .map_or(false, |(piece, _)| !self.is_allied(piece)))
                .collect()
        }

        fn get_withdrawn_piece(&self, board: &Board, begin: Coord, end: Coord) -> Vec<Coord> {
            match begin.adjacent_coord(end.relative_direction(begin))
                .and_then(|c| board.get_piece(c)) {
                    Some((piece, coord)) if !self.is_allied(piece) => vec![coord],
                    _ => Vec::new(),
                }
        }

        /**
         * Check if the piece can move to from the start position to the end
         * position. If the move is valid, return a Some containing a vector
         * of coordinates that the move captured, since in Baroque no piece
         * other than the King actually capture by substitution.
         */
        fn check_valid_move(&self, board: &Board, begin: Coord, end: Coord) -> Option<Vec<Coord>> {
            if self.is_immobilized(board, begin) {
                return None;
            }
            // Chameleon handling code - aka I can't find a good way to fit this
            // asshat into my existing code; it gets special handling all to
            // itself. Or I just suck at Rust and a better pattern is possible.
            let mut chameleon_captures = Vec::new();
            if let PieceType::Chameleon = self.piece_type {
                for piece_type in PieceType::non_chameleon_types().into_iter() {
                    let transformed_piece = Piece::new(self.side, piece_type);
                    // The Long Leaper case gets special handling because of its
                    // non-standard movement rules; any piece can pin enemies
                    // but only Pincers will capture, and piece can withdraw
                    // from enemies but only Withdrawers will capture, etc.
                    // Long Leapers however is the only piece that can leap over
                    // enemies, and must capture while doing so.
                    // What this means is that if the Chameleon pinced some enemies
                    // and happened to pin some Pincers, then those Pincers are
                    // captured, not other pinced enemies; this applies to other
                    // piece types that can capture multiple enemies.
                    // If the Chameleon leaped over some enemy pieces however,
                    // ALL of them must be enemy Leapers for the move to be valid,
                    // and ALL of those Leapers will be captured.
                    if let Some(captures) =
                        transformed_piece.check_valid_move(board, begin, end) {
                            let same_type = |(p, _): &(&Piece, Coord)| p.get_type() == piece_type;
                            let pieces = board.get_pieces(&captures);
                            match piece_type {
                                PieceType::LongLeaper => {
                                    if pieces.iter().all(same_type) {
                                        chameleon_captures.extend(captures);
                                    }
                                },
                                _ => {
                                    if pieces.iter().any(same_type) {
                                        let captures: Vec<Coord> = pieces.iter()
                                            .filter(|pc| same_type(&pc))
                                            .map(|&(_, c)| c)
                                            .collect();
                                        chameleon_captures.extend(captures);
                                    }
                                },
                            }
                        }
                }
            }

            // Now for the remaining pieces
            let is_valid = match self.get_type() {
                PieceType::Pincer =>
                    begin.orthogonally_aligned(end) && board.no_obstacles(begin, end),
                PieceType::LongLeaper =>
                    self.is_valid_leaper_path(board, begin, end),
                PieceType::King =>
                    begin.get_neighboring_coords().contains(&end) &&
                    board.get_piece(end).map_or(true, |(p, _)| !self.is_allied(p)),
                // Chameleon moves like Queens, unless a capture allows otherwise
                PieceType::Chameleon =>
                    !chameleon_captures.is_empty() ||
                    (begin.aligned(end) && board.no_obstacles(begin, end)),
                // All other pieces move like Queens that can't capture.
                _ =>
                    begin.aligned(end) && board.no_obstacles(begin, end),
             };
            // TODO: Move all the sublogic to their own functions like Coordinator.
            if is_valid {
                let captured_coords = match self.get_type() {
                    // We already check above that the target piece, if any, is hostile.
                    PieceType::King => board.get_piece(end)
                        .map_or(Vec::new(), |(_, c)| vec![c]),

                    PieceType::Pincer =>
                        Direction::cardinal_directions().into_iter()
                        .filter_map(|dir| self.is_pincing(board, end, dir))
                        .collect(),

                    PieceType::LongLeaper =>
                        // We already check above that all pieces in the path is hostile.
                        CoordWalk::new(begin, end).filter(|c| board.get_piece(*c).is_some())
                        .collect(),

                    PieceType::Withdrawer =>
                        self.get_withdrawn_piece(board, begin, end),

                    PieceType::Coordinator =>
                        self.get_coordinated_pieces(board, end),

                    PieceType::Immobilizer => Vec::new(),

                    PieceType::Chameleon => chameleon_captures,
                };
                Some(captured_coords)
            } else {
                None
            }
        }
    }


    #[cfg(test)]
    mod tests {
        use super::*;

        fn print_messages(messages: Vec<String>) {
            for s in messages {
                println!("{}", s);
            }
        }

        #[test]
        fn test_horizontally_aligned() {
            let coord1 = Coord::new(0, 1);
            let coord2 = Coord::new(3, 1);
            let coord3 = Coord::new(0, 6);
            assert!(!coord1.horizontally_aligned(coord1));
            assert!(coord1.horizontally_aligned(coord2));
            assert!(!coord1.horizontally_aligned(coord3));
        }

        #[test]
        fn test_vertically_aligned() {
            let coord1 = Coord::new(1, 3);
            let coord2 = Coord::new(1, 1);
            let coord3 = Coord::new(0, 3);
            assert!(!coord1.vertically_aligned(coord1));
            assert!(coord1.vertically_aligned(coord2));
            assert!(!coord1.vertically_aligned(coord3));
        }

        #[test]
        fn test_diagonally_aligned() {
            let coord1 = Coord::new(4, 3);
            let coord2 = Coord::new(5, 4);
            let coord3 = Coord::new(0, 7);
            let coord4 = Coord::new(1, 0);
            let coord5 = Coord::new(7, 6);
            let coord6 = Coord::new(7, 2);
            assert!(!coord1.diagonally_aligned(coord1));
            assert!(coord1.diagonally_aligned(coord2));
            assert!(coord1.diagonally_aligned(coord3));
            assert!(coord1.diagonally_aligned(coord4));
            assert!(coord1.diagonally_aligned(coord5));
            assert!(!coord1.diagonally_aligned(coord6));
        }

        // See https://en.wikipedia.org/wiki/Baroque_chess for these exact test scenarios
        // TODO:
        // - The type of the pieces being tested is not important aside from the
        // Chameleon tests; maybe add a generic type?
        // - Write a macro for putting pieces, with optional support for algebraic
        // notation
        #[test]
        fn test_pincer() {
            let mut board = Board::new_blank_board();
            board.put_piece(Piece::new(Side::White, PieceType::Pincer), Coord::new(6, 3));

            board.put_piece(Piece::new(Side::White, PieceType::Withdrawer), Coord::new(3, 0));
            board.put_piece(Piece::new(Side::White, PieceType::Pincer), Coord::new(6, 1));
            board.put_piece(Piece::new(Side::White, PieceType::King), Coord::new(1, 3));
            board.put_piece(Piece::new(Side::White, PieceType::Pincer), Coord::new(3, 5));
            board.put_piece(Piece::new(Side::White, PieceType::Coordinator), Coord::new(5, 5));
            board.put_piece(Piece::new(Side::Black, PieceType::Chameleon), Coord::new(3, 2));
            board.put_piece(Piece::new(Side::Black, PieceType::LongLeaper), Coord::new(6, 2));
            board.put_piece(Piece::new(Side::Black, PieceType::Immobilizer), Coord::new(2, 3));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(3, 4));
            board.put_piece(Piece::new(Side::Black, PieceType::Withdrawer), Coord::new(4, 4));
            board.put_piece(Piece::new(Side::Black, PieceType::King), Coord::new(3, 6));
            assert!(board.make_move(Coord::new(6, 3), Coord::new(3, 3)).is_ok());
            assert!(board.get_piece(Coord::new(2, 3)).is_none());
            assert!(board.get_piece(Coord::new(3, 4)).is_none());
            assert!(board.get_piece(Coord::new(3, 2)).is_some());
            assert!(board.get_piece(Coord::new(4, 4)).is_some());
            assert!(board.get_piece(Coord::new(6, 2)).is_some());
        }

        #[test]
        fn test_withdrawer() {
            let mut board = Board::new_blank_board();
            board.put_piece(Piece::new(Side::White, PieceType::Withdrawer), Coord::new(6, 5));

            board.put_piece(Piece::new(Side::White, PieceType::King), Coord::new(5, 1));
            board.put_piece(Piece::new(Side::White, PieceType::LongLeaper), Coord::new(4, 1));
            board.put_piece(Piece::new(Side::Black, PieceType::King), Coord::new(3, 3));
            board.put_piece(Piece::new(Side::Black, PieceType::Chameleon), Coord::new(7, 5));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(6, 6));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(7, 6));
            assert!(board.make_move(Coord::new(6, 5), Coord::new(3, 2)).is_ok());
            assert!(board.get_piece(Coord::new(7, 6)).is_none());
            assert!(board.get_piece(Coord::new(6, 6)).is_some());
            assert!(board.get_piece(Coord::new(7, 5)).is_some());
        }

        #[test]
        fn test_long_leaper() {
            let mut board = Board::new_blank_board();
            board.put_piece(Piece::new(Side::White, PieceType::LongLeaper), Coord::new(3, 1));

            board.put_piece(Piece::new(Side::White, PieceType::Immobilizer), Coord::new(0, 1));
            board.put_piece(Piece::new(Side::White, PieceType::Pincer), Coord::new(2, 2));
            board.put_piece(Piece::new(Side::Black, PieceType::Chameleon), Coord::new(3, 0));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(1, 1));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(5, 1));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(6, 1));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(3, 2));
            board.put_piece(Piece::new(Side::Black, PieceType::LongLeaper), Coord::new(3, 4));
            board.put_piece(Piece::new(Side::Black, PieceType::Coordinator), Coord::new(3, 6));
            board.put_piece(Piece::new(Side::Black, PieceType::Withdrawer), Coord::new(5, 3));
            board.put_piece(Piece::new(Side::Black, PieceType::King), Coord::new(6, 2));
            assert!(board.make_move(Coord::new(3, 1), Coord::new(1, 3)).is_err());
            assert!(board.make_move(Coord::new(3, 1), Coord::new(7, 1)).is_err());
            assert!(board.make_move(Coord::new(3, 1), Coord::new(3, 7)).is_ok());
            assert!(board.get_piece(Coord::new(3, 2)).is_none());
            assert!(board.get_piece(Coord::new(3, 4)).is_none());
            assert!(board.get_piece(Coord::new(3, 6)).is_none());
        }

        #[test]
        fn test_coordinator() {
            let mut board = Board::new_blank_board();
            board.put_piece(Piece::new(Side::White, PieceType::Coordinator), Coord::new(3, 3));

            board.put_piece(Piece::new(Side::White, PieceType::King), Coord::new(2, 1));
            board.put_piece(Piece::new(Side::White, PieceType::Pincer), Coord::new(5, 2));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(3, 1));
            board.put_piece(Piece::new(Side::Black, PieceType::Immobilizer), Coord::new(5, 1));
            board.put_piece(Piece::new(Side::Black, PieceType::King), Coord::new(6, 4));
            board.put_piece(Piece::new(Side::Black, PieceType::LongLeaper), Coord::new(2, 5));
            board.put_piece(Piece::new(Side::Black, PieceType::Chameleon), Coord::new(4, 5));
            assert!(board.make_move(Coord::new(3, 3), Coord::new(5, 5)).is_ok());
            assert!(board.get_piece(Coord::new(5, 1)).is_none());
            assert!(board.get_piece(Coord::new(2, 5)).is_none());
        }

        // Slightly different from Wikipedia, here the Black Chameleon is
        // replaced with a Black Immobilizer that doesn't get near the white
        // Immobilizer until later. This is due to this game implementing the
        // Cambridge rules (Immobilizers get neutralized if they're near each
        // other or a Chameleon), and that we want to test the Chameleon later.
        #[test]
        fn test_immobilizer() {
            let mut board = Board::new_blank_board();
            board.put_piece(Piece::new(Side::White, PieceType::Immobilizer), Coord::new(5, 2));

            board.put_piece(Piece::new(Side::White, PieceType::King), Coord::new(2, 3));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(3, 3));
            board.put_piece(Piece::new(Side::Black, PieceType::LongLeaper), Coord::new(2, 4));
            board.put_piece(Piece::new(Side::Black, PieceType::King), Coord::new(1, 5));
            board.put_piece(Piece::new(Side::Black, PieceType::Coordinator), Coord::new(3, 5));
            board.put_piece(Piece::new(Side::Black, PieceType::Withdrawer), Coord::new(4, 5));
            board.put_piece(Piece::new(Side::Black, PieceType::LongLeaper), Coord::new(6, 3));
            board.put_piece(Piece::new(Side::Black, PieceType::Immobilizer), Coord::new(2, 7));
            // the initial black Leaper that's near the white Immobilizer.
            assert_eq!(board.is_immobilized(Coord::new(6, 3)), Some(true));
            // the black King that's nowhere near the action.
            assert_eq!(board.is_immobilized(Coord::new(1, 5)), Some(false));
            // the tightly-clumped squad of black pieces that's not frozen yet.
            assert_eq!(board.is_immobilized(Coord::new(2, 4)), Some(false));
            assert_eq!(board.is_immobilized(Coord::new(3, 3)), Some(false));
            assert_eq!(board.is_immobilized(Coord::new(4, 5)), Some(false));
            assert_eq!(board.is_immobilized(Coord::new(3, 5)), Some(false));

            assert!(board.make_move(Coord::new(5, 2), Coord::new(3, 4)).is_ok());
            // The black Leaper is no longer frozen as the Immobilizer is away.
            assert_eq!(board.is_immobilized(Coord::new(6, 3)), Some(false));
            // The black King is fine, as expected.
            assert_eq!(board.is_immobilized(Coord::new(1, 5)), Some(false));
            // The rest of the Black pieces though are frozen.
            assert_eq!(board.is_immobilized(Coord::new(2, 4)), Some(true));
            assert_eq!(board.is_immobilized(Coord::new(3, 3)), Some(true));
            assert_eq!(board.is_immobilized(Coord::new(4, 5)), Some(true));
            assert_eq!(board.is_immobilized(Coord::new(3, 5)), Some(true));
            // Now we move the Black Immobilizer in. Per the Cambridge rules,
            // the two opposing Immobilizers will freeze each other while the
            // other pieces are free.
            assert!(board.make_move(Coord::new(2, 7), Coord::new(2, 5)).is_ok());
            assert_eq!(board.is_immobilized(Coord::new(2, 4)), Some(false));
            assert_eq!(board.is_immobilized(Coord::new(3, 3)), Some(false));
            assert_eq!(board.is_immobilized(Coord::new(4, 5)), Some(false));
            assert_eq!(board.is_immobilized(Coord::new(3, 5)), Some(false));
            // The two immobilizers though are locked in with each other.
            assert_eq!(board.is_immobilized(Coord::new(3, 4)), Some(true));
            assert_eq!(board.is_immobilized(Coord::new(2, 5)), Some(true));
        }

        // The Chameleon test is also slightly different from Wikipedia:
        // 1. According to Cambridge rules the move is still valid, but would
        // only capture the Long Leapers, as the move would not be valid had
        // the White Chameleon be a non-Leaper. If the Leapers are not present
        // then yes, the Black Pincers, Withdrawer and Coordinator bite the dust.
        // We test both cases.
        // 2. A Black Immobilizer is added to b5 to test that the Chameleon
        // neutralizes it.
        fn test_chameleon_common_board() -> Board {
            let mut board = Board::new_blank_board();
            board.put_piece(Piece::new(Side::White, PieceType::Chameleon), Coord::new(6, 5));

            board.put_piece(Piece::new(Side::White, PieceType::King), Coord::new(0, 0));
            board.put_piece(Piece::new(Side::White, PieceType::LongLeaper), Coord::new(2, 3));
            board.put_piece(Piece::new(Side::White, PieceType::Pincer), Coord::new(0, 5));
            board.put_piece(Piece::new(Side::White, PieceType::Coordinator), Coord::new(2, 7));
            board.put_piece(Piece::new(Side::Black, PieceType::Withdrawer), Coord::new(7, 5));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(2, 4));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(1, 5));
            board.put_piece(Piece::new(Side::Black, PieceType::Pincer), Coord::new(2, 6));
            board.put_piece(Piece::new(Side::Black, PieceType::Coordinator), Coord::new(2, 0)); 
            board.put_piece(Piece::new(Side::Black, PieceType::Immobilizer), Coord::new(1, 4)); 
            board
        }

        #[test]
        fn test_chameleon_no_leaper() {
            let mut board = test_chameleon_common_board();
            assert_eq!(board.is_immobilized(Coord::new(2, 3)), Some(true));
            assert!(board.make_move(Coord::new(6, 5), Coord::new(2, 5)).is_ok());
            assert!(board.get_piece(Coord::new(7, 5)).is_none());
            assert!(board.get_piece(Coord::new(2, 4)).is_none());
            assert!(board.get_piece(Coord::new(1, 5)).is_none());
            assert!(board.get_piece(Coord::new(2, 6)).is_none());
            assert!(board.get_piece(Coord::new(2, 0)).is_none());
            assert_eq!(board.is_immobilized(Coord::new(2, 3)), Some(false));
        }

        #[test]
        fn test_chameleon_with_leaper() {
            let mut board = test_chameleon_common_board();
            board.put_piece(Piece::new(Side::Black, PieceType::LongLeaper), Coord::new(5, 5));
            board.put_piece(Piece::new(Side::Black, PieceType::LongLeaper), Coord::new(3, 5));
            assert_eq!(board.is_immobilized(Coord::new(2, 3)), Some(true));
            assert!(board.make_move(Coord::new(6, 5), Coord::new(2, 5)).is_ok());
            assert!(board.get_piece(Coord::new(5, 5)).is_none());
            assert!(board.get_piece(Coord::new(3, 4)).is_none());
            assert!(board.get_piece(Coord::new(7, 5)).is_some());
            assert!(board.get_piece(Coord::new(2, 4)).is_some());
            assert!(board.get_piece(Coord::new(1, 5)).is_some());
            assert!(board.get_piece(Coord::new(2, 6)).is_some());
            assert!(board.get_piece(Coord::new(2, 0)).is_some());
            assert_eq!(board.is_immobilized(Coord::new(2, 3)), Some(false));
        }
    }
}

pub mod players {
    use super::baroque::*;
    use std::io;
    use rand::seq::SliceRandom;

    fn read_line() -> io::Result<Vec<usize>> {
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;
        let input_coords: Vec<usize> = input.split(' ').filter_map(|s| s.trim().parse().ok())
            .collect();
        if input_coords.len() != 4 {
            Err(io::Error::new(io::ErrorKind::InvalidInput, "Four valid numbers were not provided"))
        } else {
            Ok(input_coords)
        }
    }

    pub trait Player {
        fn get_side(&self) -> Side;
        fn play(&self, board: &Board) -> Option<(Coord, Coord)>;
    }

    pub struct Human {
        side: Side,
    }

    impl Human {
        pub fn new(side: Side) -> Human {
            Human {side}
        }
    }

    impl Player for Human {
        fn get_side(&self) -> Side { self.side }
        fn play(&self, _: &Board) -> Option<(Coord, Coord)> {
            loop {
                match read_line() {
                    Ok(input) => {
                        return Some((Coord::new(input[0], input[1]),
                        Coord::new(input[2], input[3])));
                    },
                    Err(e) => {
                        println!("{}", e);
                        continue;
                    },
                }
            }
        }
    }

    pub struct AI {
        side: Side,
    }

    impl AI {
        pub fn new(side: Side) -> AI {
            AI {side}
        }
    }

    impl Player for AI {
        fn get_side(&self) -> Side { self.side }
        fn play(&self, board: &Board) -> Option<(Coord, Coord)> {
            let mut rng = rand::thread_rng();
            board.get_possible_moves(self.side).choose(&mut rng).cloned()
        }
    }
}
