use self::FluffyDir::*;
use self::Tile::*;
use bevy::app::DefaultTaskPoolOptions;
use bevy::asset::{HandleId, LoadState};
use bevy::input::{keyboard::KeyCode, Input};
use bevy::prelude::*;
use bevy::sprite::TextureAtlasBuilder;
use shrinkwraprs::Shrinkwrap;
use std::io::{BufRead, BufReader};
use std::ops::{Add, AddAssign};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

const TILE_SIZE: u32 = 24;
const TILES_X: u32 = 20;
const TILES_Y: u32 = 15;
const LEVELS: &'static [u8] = include_bytes!("../levels");

#[derive(Default, Eq, PartialEq, Clone, Copy, Debug)]
struct Coords(i8, i8);

impl Coords {
    fn is_valid(&self) -> bool {
        self.0 >= 0 && self.0 < TILES_X as i8 && self.1 >= 0 && self.1 < TILES_Y as i8
    }
    fn x(&self) -> usize {
        self.0 as usize
    }
    fn y(&self) -> usize {
        self.1 as usize
    }
}

impl From<&Coords> for Vec3 {
    fn from(coords: &Coords) -> Self {
        Self::new(
            (coords.0 as u32 * TILE_SIZE) as f32 - (TILES_X * TILE_SIZE) as f32 / 2.0
                + TILE_SIZE as f32 / 2.0,
            (TILES_Y * TILE_SIZE) as f32 / 2.0
                - (coords.1 as u32 * TILE_SIZE) as f32
                - TILE_SIZE as f32 / 2.0,
            0.0,
        )
    }
}

impl Add for Coords {
    type Output = Self;
    fn add(self, other: Self) -> Self {
        Self(self.0 + other.0, self.1 + other.1)
    }
}

impl AddAssign for Coords {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl Add<FluffyDir> for Coords {
    type Output = Self;
    fn add(self, dir: FluffyDir) -> Self {
        self + Self::from(dir)
    }
}
impl AddAssign<FluffyDir> for Coords {
    fn add_assign(&mut self, dir: FluffyDir) {
        *self = *self + dir;
    }
}

impl From<FluffyDir> for Coords {
    fn from(dir: FluffyDir) -> Self {
        match dir {
            Up => Self(0, -1),
            Down => Self(0, 1),
            Left => Self(-1, 0),
            Right => Self(1, 0),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Change {
    Move(Coords, Coords),
    Remove(Coords),
    Add(Tile, Coords),
}

type Changes = Vec<Change>;

fn main() {
    App::build()
        .add_resource(WindowDescriptor {
            width: TILES_X * TILE_SIZE,
            height: TILES_Y * TILE_SIZE,
            resizable: false,
            title: "Budge".into(),
            ..Default::default()
        })
        .add_resource(DefaultTaskPoolOptions::with_num_threads(1))
        .add_resource(ClearColor(Color::WHITE))
        .init_resource::<Board>()
        .init_resource::<EntityBoard>()
        .init_resource::<PlayerCoords>()
        .init_resource::<SpikyCoords>()
        .init_resource::<FluffyCoords>()
        .init_resource::<Status>()
        .init_resource::<FluffyDir>()
        .init_resource::<Changes>()
        .add_default_plugins()
        .add_resource(TickTimer(Timer::from_seconds(0.25, true)))
        .add_resource(Status::Win(0))
        .init_resource::<LevelNum>()
        .init_resource::<Lives>()
        .init_resource::<SpriteHandles>()
        .add_startup_system(setup.system())
        .add_system(load_atlas.system())
        .add_system(time_system.system())
        .add_system(move_monsters.system())
        .add_system(move_tiles.system())
        .add_system(start_level.system())
        .add_system(user_input.system())
        .add_system(reduce_timers.system())
        .run();
}

type Board = [[Tile; TILES_Y as usize]; TILES_X as usize + 1];
type EntityBoard = [[Option<Entity>; TILES_Y as usize]; TILES_X as usize + 1];

#[derive(Eq, PartialEq, Debug, Default, Shrinkwrap)]
struct PlayerCoords(Coords);
#[derive(Eq, PartialEq, Debug, Default, Shrinkwrap)]
struct SpikyCoords(Coords);
#[derive(Eq, PartialEq, Debug, Default, Shrinkwrap)]
struct FluffyCoords(Coords);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum FluffyDir {
    Right,
    Up,
    Left,
    Down,
}

impl Default for FluffyDir {
    fn default() -> Self {
        Right
    }
}

impl FluffyDir {
    fn next(&self) -> Self {
        match *self {
            Right => Up,
            Up => Left,
            Left => Down,
            Down => Right,
        }
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, EnumIter)]
enum Tile {
    Player,
    Spiky,
    Fluffy,
    Heart,
    Gate,
    Disk,
    Killer,
    Freeze,
    Dead,
    Block(Neighbors),
    Empty,
    Invisible,
}

impl Default for Tile {
    fn default() -> Self {
        Empty
    }
}

impl Tile {
    fn from_char(c: char) -> Option<Self> {
        Some(match c {
            ' ' => Empty,
            '-' => Invisible,
            '+' => Gate,
            'o' => Disk,
            'x' => Killer,
            'f' => Freeze,
            'P' => Player,
            '*' => Spiky,
            '@' => Fluffy,
            '&' => Heart,
            '!' => Dead,
            '#' => Block(Neighbors(0)),
            _ => {
                return None;
            }
        })
    }

    fn is_block(&self) -> bool {
        if let Block(_) = *self {
            true
        } else {
            false
        }
    }

    fn sprite(&self) -> Option<String> {
        Some(format!(
            "sprites/{}.png",
            match *self {
                Empty | Invisible => {
                    return None;
                }
                Block(neighbors) => neighbors.sprite(),
                other => {
                    let mut name = format!("{:?}", other);
                    name.make_ascii_lowercase();
                    name
                }
            }
        ))
    }

    const fn sprite_idx(&self) -> Option<u8> {
        Some(match *self {
            Empty | Invisible => {
                return None;
            }
            Player => 0,
            Spiky => 1,
            Fluffy => 2,
            Heart => 3,
            Gate => 4,
            Disk => 5,
            Killer => 6,
            Freeze => 7,
            Dead => 8,
            Block(neighbors) => 9 + neighbors.0,
        })
    }

    fn all() -> impl Iterator<Item = Tile> {
        Self::iter()
            .filter(|t| t.sprite_idx() != Block(Default::default()).sprite_idx())
            .chain(Neighbors::all().map(|n| Block(n)))
    }
}

const TILE_SPRITE_COUNT: usize = count_tile_sprites();

const fn count_tile_sprites() -> usize {
    if let Some(idx) = Block(Neighbors(Neighbors::ALL)).sprite_idx() {
        idx as usize + 1
    } else {
        0 // unreachable!()
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Default, Debug)]
struct Neighbors(u8);

impl Neighbors {
    const UP: u8 = 1 << 0;
    const DOWN: u8 = 1 << 1;
    const LEFT: u8 = 1 << 2;
    const RIGHT: u8 = 1 << 3;
    const ALL: u8 = 0b00001111;

    fn new(up: bool, down: bool, left: bool, right: bool) -> Self {
        Neighbors(
            ((up as u8) << 0) | ((down as u8) << 1) | ((left as u8) << 2) | ((right as u8) << 3),
        )
    }

    fn sprite(&self) -> String {
        format!(
            "block{}{}{}{}{}",
            self.ifstr(Self::ALL, "_"),
            self.ifstr(Self::UP, "u"),
            self.ifstr(Self::DOWN, "d"),
            self.ifstr(Self::LEFT, "l"),
            self.ifstr(Self::RIGHT, "r"),
        )
    }

    fn has_neighbor(&self, dir: u8) -> bool {
        (self.0 & dir) != 0
    }

    fn ifstr(&self, dir: u8, true_val: &'static str) -> &'static str {
        if self.has_neighbor(dir) {
            true_val
        } else {
            ""
        }
    }

    fn all() -> impl Iterator<Item = Neighbors> {
        (0..1 << 4).map(|v| Neighbors(v))
    }
}

#[derive(Default)]
pub struct SpriteHandles {
    handles: Vec<HandleId>,
    atlas_loaded: bool,
    atlas_handle: Handle<TextureAtlas>,
    sprite_indices: [u32; TILE_SPRITE_COUNT],
}

fn setup(
    mut commands: Commands,
    mut sprite_handles: ResMut<SpriteHandles>,
    asset_server: Res<AssetServer>,
) {
    sprite_handles.handles = asset_server.load_asset_folder("sprites").unwrap();
    commands.spawn(Camera2dComponents::default());
}

fn load_atlas(
    mut sprite_handles: ResMut<SpriteHandles>,
    asset_server: Res<AssetServer>,
    mut texture_atlases: ResMut<Assets<TextureAtlas>>,
    mut textures: ResMut<Assets<Texture>>,
) {
    if sprite_handles.atlas_loaded {
        return;
    }
    let mut texture_atlas_builder =
        TextureAtlasBuilder::new(Vec2::new(144.0, 144.0), Vec2::new(144.0, 144.0));
    if let Some(LoadState::Loaded(_)) = asset_server.get_group_load_state(&sprite_handles.handles) {
        for texture_id in sprite_handles.handles.iter() {
            let handle = Handle::from_id(*texture_id);
            let texture = textures.get(&handle).unwrap();
            texture_atlas_builder.add_texture(handle, &texture);
        }
        let texture_atlas = texture_atlas_builder.finish(&mut textures).unwrap();

        for tile in Tile::all() {
            if let Some(idx) = tile.sprite_idx() {
                let idx = idx as usize;
                let filename = tile.sprite().unwrap();
                sprite_handles.sprite_indices[idx] = texture_atlas
                    .get_texture_index(asset_server.get_handle(&filename).unwrap())
                    .unwrap() as u32;
            }
        }
        sprite_handles.atlas_handle = texture_atlases.add(texture_atlas);
        sprite_handles.atlas_loaded = true;
    }
}

fn start_level(
    mut commands: Commands,
    llsf: (
        ResMut<LevelNum>,
        Res<Lives>,
        ResMut<Status>,
        ResMut<FluffyDir>,
    ),
    mut window: ResMut<Windows>,
    mut board: ResMut<Board>,
    mut entity_board: ResMut<EntityBoard>,
    mut changes: ResMut<Changes>,
    mut player_coords: ResMut<PlayerCoords>,
    mut spiky_coords: ResMut<SpikyCoords>,
    mut fluffy_coords: ResMut<FluffyCoords>,
    sprite_handles: Res<SpriteHandles>,
) {
    if !sprite_handles.atlas_loaded {
        return;
    }
    let (mut cur_level, lives, mut status, mut fluffy_dir) = llsf;
    match *status {
        Status::Win(0) => {}
        Status::Dead(0) => {
            if lives.0 == 0 {
                return;
            }
            cur_level.0 -= 1;
        }
        _ => {
            return;
        }
    }
    *fluffy_dir = Default::default();
    let data_reader = BufReader::new(LEVELS);
    let name = data_reader
        .lines()
        .skip(((TILES_Y + 1) * cur_level.0 as u32) as usize)
        .next()
        .unwrap()
        .unwrap();
    let window_id = window.get_primary().unwrap().id;
    window.get_mut(window_id).unwrap().title = name; // TODO: Fix this so it actually works
    let data_reader = BufReader::new(LEVELS);
    *board = Default::default();
    for entity in entity_board
        .iter()
        .flat_map(|r| r.iter())
        .filter_map(|&e| e)
    {
        commands.despawn(entity);
    }
    *entity_board = Default::default();
    let rows = data_reader
        .lines()
        .skip(((TILES_Y + 1) * cur_level.0 as u32) as usize + 1);
    for (y, cols) in rows.enumerate().take(TILES_Y as usize) {
        for (x, cell) in cols.unwrap().chars().enumerate() {
            let tile = Tile::from_char(cell).unwrap();
            board[x][y] = tile;
            if let Some(dst) = match tile {
                Player => Some(&mut player_coords.0),
                Spiky => Some(&mut spiky_coords.0),
                Fluffy => Some(&mut fluffy_coords.0),
                _ => None,
            } {
                *dst = Coords(x as i8, y as i8);
            }
        }
    }
    for x in 0..TILES_X as usize + 1 {
        for y in 0..TILES_Y as usize {
            let mut tile = board[x][y];
            if tile.is_block() {
                let up = y > 0 && board[x][y - 1].is_block();
                let down = y < TILES_Y as usize - 1 && board[x][y + 1].is_block();
                let left = x > 0 && board[x - 1][y].is_block();
                let right = x < TILES_X as usize - 1 && board[x + 1][y].is_block();
                tile = Block(Neighbors::new(up, down, left, right));
                board[x][y] = tile;
            }
            if tile.sprite_idx().is_some() {
                changes.push(Change::Add(tile, Coords(x as i8, y as i8)));
            }
        }
    }
    cur_level.0 += 1;
    *status = Status::Play;
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum Status {
    Welcome,
    Win(u8),
    Play,
    Pause,
    Dead(u8),
}

impl Default for Status {
    fn default() -> Self {
        Self::Welcome
    }
}

#[derive(Shrinkwrap)]
struct TickTimer(Timer);
#[derive(Copy, Clone, Default, Shrinkwrap)]
struct LevelNum(u8);
#[derive(Copy, Clone, Shrinkwrap)]
struct Lives(u8);

impl Default for Lives {
    fn default() -> Self {
        Lives(3)
    }
}

fn time_system(time: Res<Time>, mut timer: ResMut<TickTimer>) {
    timer.0.tick(time.delta_seconds);
}

fn reduce_timers(tick: Res<TickTimer>, mut status: ResMut<Status>, lives: Res<Lives>) {
    if !tick.finished {
        return;
    }
    match *status {
        Status::Win(ref mut n) => {
            *n -= 1;
        }
        Status::Dead(0) => {
            if lives.0 == 0 {
                *status = Status::Welcome;
            }
        }
        Status::Dead(ref mut n) => {
            *n -= 1;
        }
        _ => {}
    }
}

fn move_monsters(
    tick: Res<TickTimer>,
    mut board: ResMut<Board>,
    mut spiky_coords: ResMut<SpikyCoords>,
    mut fluffy_coords: ResMut<FluffyCoords>,
    player_coords: Res<PlayerCoords>,
    mut fluffy_dir: ResMut<FluffyDir>,
    mut changes: ResMut<Changes>,
    mut status: ResMut<Status>,
    mut lives: ResMut<Lives>,
) {
    if !tick.0.finished || *status != Status::Play {
        return;
    }
    let fluffy_target = **fluffy_coords + *fluffy_dir;
    match if fluffy_target.is_valid() {
        board[fluffy_target.x()][fluffy_target.y()]
    } else {
        Invisible
    } {
        Spiky => {
            *status = Status::Win(6);
            changes.push(Change::Remove(**spiky_coords));
            changes.push(Change::Remove(**fluffy_coords));
            changes.push(Change::Add(Heart, fluffy_target));
            return;
        }
        Player => {
            *status = Status::Dead(6);
            changes.push(Change::Remove(**player_coords));
            changes.push(Change::Add(Dead, **player_coords));
            lives.0 -= 1;
            return;
        }
        Empty => {
            changes.push(Change::Move(fluffy_coords.0, fluffy_target));
            board[fluffy_coords.x()][fluffy_coords.y()] = Empty;
            fluffy_coords.0 = fluffy_target;
            board[fluffy_target.x()][fluffy_target.y()] = Fluffy;
        }
        _ => {
            *fluffy_dir = fluffy_dir.next();
        }
    }
    let spiky_target = spiky_coords.0
        + Coords(
            ((player_coords.0).0 - (spiky_coords.0).0).signum(),
            ((player_coords.0).1 - (spiky_coords.0).1).signum(),
        );
    match if spiky_target.is_valid() {
        board[spiky_target.x()][spiky_target.y()]
    } else {
        Invisible
    } {
        Fluffy => {
            *status = Status::Win(6);
            changes.push(Change::Remove(**spiky_coords));
            changes.push(Change::Remove(**fluffy_coords));
            changes.push(Change::Add(Heart, spiky_target));
            return;
        }
        Player => {
            *status = Status::Dead(6);
            changes.push(Change::Remove(**player_coords));
            changes.push(Change::Add(Dead, **player_coords));
            lives.0 -= 1;
            return;
        }
        Empty => {
            changes.push(Change::Move(spiky_coords.0, spiky_target));
            board[spiky_coords.x()][spiky_coords.y()] = Empty;
            spiky_coords.0 = spiky_target;
            board[spiky_target.x()][spiky_target.y()] = Spiky;
        }
        _ => {}
    }
}

fn move_tiles(
    mut commands: Commands,
    sprite_handles: Res<SpriteHandles>,
    mut entity_board: ResMut<EntityBoard>,
    mut changes: ResMut<Changes>, // TODO: Use events or ChangedRes for this
    query: Query<&mut Transform>,
) {
    for change in changes.iter() {
        match change {
            Change::Add(tile, coords) => {
                let sprite_idx = tile.sprite_idx().unwrap();
                let entity = commands
                    .spawn(SpriteSheetComponents {
                        sprite: TextureAtlasSprite::new(
                            sprite_handles.sprite_indices[sprite_idx as usize],
                        ),
                        texture_atlas: sprite_handles.atlas_handle,
                        transform: Transform::from_translation(coords.into()),
                        ..Default::default()
                    })
                    .current_entity()
                    .unwrap();
                entity_board[coords.x()][coords.y()] = Some(entity);
            }
            Change::Move(from, to) => {
                let entity = entity_board[from.x()][from.y()].take().unwrap();
                // TODO: verify that destination is empty
                entity_board[to.x()][to.y()] = Some(entity);
                let transform: &mut Transform = &mut query.get_mut(entity).unwrap();
                transform.set_translation(to.into());
            }
            Change::Remove(at) => {
                let entity = entity_board[at.x()][at.y()].take().unwrap();
                commands.despawn(entity);
            }
        }
    }
    changes.clear();
}

fn user_input(
    keyboard_input: Res<Input<KeyCode>>,
    mut status: ResMut<Status>,
    mut player_coords: ResMut<PlayerCoords>,
    mut board: ResMut<Board>,
    mut changes: ResMut<Changes>,
    mut lives: ResMut<Lives>,
) {
    let mut dir = Coords::default();
    if keyboard_input.just_pressed(KeyCode::P) {
        match *status {
            Status::Pause => *status = Status::Play,
            Status::Play => {
                *status = Status::Pause;
            }
            _ => {}
        }
    }
    if *status != Status::Play {
        return;
    }
    if keyboard_input.just_pressed(KeyCode::Left) {
        dir += Left;
    }
    if keyboard_input.just_pressed(KeyCode::Right) {
        dir += Right;
    }
    if keyboard_input.just_pressed(KeyCode::Up) {
        dir += Up;
    }
    if keyboard_input.just_pressed(KeyCode::Down) {
        dir += Down;
    }
    // TODO: Don't allow cheating with diagonals
    if dir != Default::default() {
        let target = **player_coords + dir;
        if target.is_valid() {
            match board[target.x()][target.y()] {
                Empty => {
                    board[player_coords.x()][player_coords.y()] = Empty;
                    board[target.x()][target.y()] = Player;
                    changes.push(Change::Move(player_coords.0, target));
                    player_coords.0 = target;
                }
                Gate => {
                    board[player_coords.x()][player_coords.y()] = Empty;
                    board[target.x()][target.y()] = Player;
                    changes.push(Change::Remove(target));
                    changes.push(Change::Move(player_coords.0, target));
                    player_coords.0 = target;
                }
                Spiky | Fluffy => {
                    *status = Status::Dead(6);
                    changes.push(Change::Remove(**player_coords));
                    changes.push(Change::Add(Dead, **player_coords));
                    lives.0 -= 1;
                    return;
                }
                Disk => {
                    let disk_target = target + dir;
                    if disk_target.is_valid() && board[disk_target.x()][disk_target.y()] == Empty {
                        board[disk_target.x()][disk_target.y()] = Disk;
                        changes.push(Change::Move(target, disk_target));
                        board[player_coords.x()][player_coords.y()] = Empty;
                        board[target.x()][target.y()] = Player;
                        changes.push(Change::Move(player_coords.0, target));
                        player_coords.0 = target;
                    }
                }
                _ => {}
            }
        }
    }
}
