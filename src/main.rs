use self::FluffyDir::*;
use self::Tile::*;
use bevy::input::{keyboard::KeyCode, Input};
use bevy::prelude::*;
use bevy::sprite::TextureAtlasBuilder;
use bevy::window::WindowResized;
use shrinkwraprs::Shrinkwrap;
use std::io::{BufRead, BufReader};
use std::ops::{Add, AddAssign};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

const TILE_SIZE: u32 = 24;
const TILES_X: u32 = 20;
const TILES_Y: u32 = 15;
const LEVELS: &'static [u8] = include_bytes!("../levels");

const SPRITES: [(&str, &[u8]); 28] = [
    (
        "assets/block_dl.png",
        include_bytes!("../assets/block_dl.png"),
    ),
    (
        "assets/block_dlr.png",
        include_bytes!("../assets/block_dlr.png"),
    ),
    (
        "assets/block_d.png",
        include_bytes!("../assets/block_d.png"),
    ),
    (
        "assets/block_dr.png",
        include_bytes!("../assets/block_dr.png"),
    ),
    (
        "assets/block_l.png",
        include_bytes!("../assets/block_l.png"),
    ),
    (
        "assets/block_lr.png",
        include_bytes!("../assets/block_lr.png"),
    ),
    ("assets/block.png", include_bytes!("../assets/block.png")),
    (
        "assets/block_r.png",
        include_bytes!("../assets/block_r.png"),
    ),
    (
        "assets/block_udl.png",
        include_bytes!("../assets/block_udl.png"),
    ),
    (
        "assets/block_udlr.png",
        include_bytes!("../assets/block_udlr.png"),
    ),
    (
        "assets/block_ud.png",
        include_bytes!("../assets/block_ud.png"),
    ),
    (
        "assets/block_udr.png",
        include_bytes!("../assets/block_udr.png"),
    ),
    (
        "assets/block_ul.png",
        include_bytes!("../assets/block_ul.png"),
    ),
    (
        "assets/block_ulr.png",
        include_bytes!("../assets/block_ulr.png"),
    ),
    (
        "assets/block_u.png",
        include_bytes!("../assets/block_u.png"),
    ),
    (
        "assets/block_ur.png",
        include_bytes!("../assets/block_ur.png"),
    ),
    ("assets/dead.png", include_bytes!("../assets/dead.png")),
    ("assets/disk.png", include_bytes!("../assets/disk.png")),
    ("assets/empty.png", include_bytes!("../assets/empty.png")),
    ("assets/fluffy.png", include_bytes!("../assets/fluffy.png")),
    ("assets/freeze.png", include_bytes!("../assets/freeze.png")),
    ("assets/gate.png", include_bytes!("../assets/gate.png")),
    ("assets/heart.png", include_bytes!("../assets/heart.png")),
    ("assets/icon.png", include_bytes!("../assets/icon.png")),
    (
        "assets/invisible.png",
        include_bytes!("../assets/invisible.png"),
    ),
    ("assets/killer.png", include_bytes!("../assets/killer.png")),
    ("assets/player.png", include_bytes!("../assets/player.png")),
    ("assets/spiky.png", include_bytes!("../assets/spiky.png")),
];

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
            title: "Budgers".into(),
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
        .init_resource::<Events<WindowResized>>()
        .init_resource::<EventReader<WindowResized>>()
        .add_default_plugins()
        .add_resource(TickTimer(Timer::from_seconds(0.25, true)))
        .init_resource::<LevelNum>()
        .init_resource::<Lives>()
        .init_resource::<SpriteHandles>()
        .init_resource::<UiHandles>()
        .add_startup_system(setup.system())
        .add_system(draw_ui.system())
        .add_system(time_system.system())
        .add_system(move_monsters.system())
        .add_system(move_tiles.system())
        .add_system(start_level.system())
        .add_system(user_input.system())
        .add_system(reduce_timers.system())
        .add_system(window_resized.system())
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
            "assets/{}.png",
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
    atlas_handle: Handle<TextureAtlas>,
    sprite_indices: [u32; TILE_SPRITE_COUNT],
}

struct UiHandles {
    board: Entity,
}

impl Default for UiHandles {
    fn default() -> Self {
        Self {
            board: Entity::new(0),
        }
    }
}

fn setup(
    mut commands: Commands,
    mut textures: ResMut<Assets<Texture>>,
    mut texture_atlases: ResMut<Assets<TextureAtlas>>,
    mut sprite_handles: ResMut<SpriteHandles>,
    mut ui_handles: ResMut<UiHandles>,
    mut fonts: ResMut<Assets<Font>>,
    asset_server: Res<AssetServer>,
) {
    commands.spawn(Camera2dComponents::default());
    commands.spawn(UiCameraComponents::default());
    let mut texture_atlas_builder =
        TextureAtlasBuilder::new(Vec2::new(144.0, 144.0), Vec2::new(144.0, 144.0));
    for tile in Tile::all() {
        if let Some(path) = tile.sprite() {
            let bytes = SPRITES
                .iter()
                .filter_map(|(ref bytes_path, bytes)| {
                    if &path == bytes_path {
                        Some(bytes)
                    } else {
                        None
                    }
                })
                .cloned()
                .next()
                .unwrap();
            let handle = asset_server
                .load_inline(&mut textures, path, bytes.into())
                .unwrap();
            let texture = textures.get(&handle).unwrap();
            texture_atlas_builder.add_texture(handle, &texture);
        }
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

    let font = asset_server
        .load_inline(
            &mut fonts,
            "font.ttf",
            include_bytes!("../assets/Xolonium-Regular.ttf")
                .as_ref()
                .into(),
        )
        .unwrap();

    ui_handles.board = commands
        .spawn(NodeComponents {
            style: Style {
                size: Size::new(
                    Val::Px((TILE_SIZE * TILES_X) as f32),
                    Val::Px((TILE_SIZE * TILES_Y) as f32),
                ),
                position_type: PositionType::Absolute,
                ..Default::default()
            },
            draw: Draw {
                is_visible: false,
                ..Default::default()
            },
            ..Default::default()
        })
        .with(Container::Board)
        .current_entity()
        .unwrap();
    commands
        .spawn(NodeComponents {
            style: Style {
                flex_direction: FlexDirection::ColumnReverse,
                align_items: AlignItems::Center,
                position_type: PositionType::Absolute,
                size: Size::new(Val::Percent(100.0), Val::Percent(100.0)),
                ..Default::default()
            },
            transform: Transform::from_scale(2.0),
            ..Default::default()
        })
        .with(Container::Welcome)
        .with_children(|children| {
            children.spawn(TextComponents {
                text: Text {
                    value: "Budgers".into(),
                    font: font,
                    style: TextStyle {
                        font_size: 32.0,
                        color: Color::BLACK,
                    },
                },
                ..Default::default()
            });
        });
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum Container {
    Welcome,
    Board,
}

fn start_level(
    mut commands: Commands,
    cur_level: Res<LevelNum>,
    mut status: ResMut<Status>,
    mut fluffy_dir: ResMut<FluffyDir>,
    mut window: ResMut<Windows>,
    mut board: ResMut<Board>,
    mut entity_board: ResMut<EntityBoard>,
    mut changes: ResMut<Changes>,
    coords: (
        ResMut<PlayerCoords>,
        ResMut<SpikyCoords>,
        ResMut<FluffyCoords>,
    ),
) {
    let (mut player_coords, mut spiky_coords, mut fluffy_coords) = coords;
    if *status != Status::SetupLevel {
        return;
    }
    *fluffy_dir = Default::default();
    let data_reader = BufReader::new(LEVELS);
    let name = data_reader
        .lines()
        .skip(((TILES_Y + 1) * cur_level.0 as u32) as usize)
        .next()
        .unwrap()
        .unwrap();
    let window_id = window.get_primary().unwrap().id();
    window.get_mut(window_id).unwrap().set_title(name);
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
    *status = Status::Play;
}

#[derive(Eq, PartialEq, Copy, Clone)]
enum Status {
    Welcome,
    SetupLevel,
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

fn reduce_timers(
    tick: Res<TickTimer>,
    mut status: ResMut<Status>,
    lives: Res<Lives>,
    mut level: ResMut<LevelNum>,
) {
    if !tick.finished {
        return;
    }
    match *status {
        Status::Win(0) => {
            level.0 += 1;
            *status = Status::SetupLevel;
        }
        Status::Win(ref mut n) => {
            *n -= 1;
        }
        Status::Dead(0) => {
            if lives.0 == 0 {
                *status = Status::Welcome;
            } else {
                *status = Status::SetupLevel;
            }
        }
        Status::Dead(ref mut n) => {
            *n -= 1;
        }
        _ => {}
    }
}

fn draw_ui(status: Res<Status>, container: &Container, mut draw: Mut<Draw>) {
    let visible_container = match *status {
        Status::Welcome => Container::Welcome,
        _ => Container::Board,
    };
    let should_be_visible = *container == visible_container;
    if draw.is_visible != should_be_visible {
        println!(
            "Setting {:?} visibility from {} to {}",
            container, draw.is_visible, should_be_visible
        );
        draw.is_visible = should_be_visible;
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
    status: Res<Status>,
    sprite_handles: Res<SpriteHandles>,
    mut entity_board: ResMut<EntityBoard>,
    mut changes: ResMut<Changes>, // TODO: Use events or ChangedRes for this
    ui_handles: Res<UiHandles>,
    query: Query<&mut Transform>,
) {
    match *status {
        Status::Welcome | Status::SetupLevel => {
            return;
        }
        _ => {}
    }
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
                    .with(Parent(ui_handles.board))
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

fn window_resized(
    events: Res<Events<WindowResized>>,
    // mut window: ResMut<Windows>,
    mut event_reader: ResMut<EventReader<WindowResized>>,
    mut container_query: Query<(&Container, &mut Transform)>,
) {
    if let Some(event) = event_reader.latest(&events) {
        let scale = (event.width as f32 / (TILES_X * TILE_SIZE) as f32)
            .min(event.height as f32 / (TILES_Y * TILE_SIZE) as f32);
        for (_, mut transform) in container_query.iter().iter() {
            transform.set_scale(scale);
        }
        /* TODO: Debounce this so it doesn't crash the game
        let window_id = window.get_primary().unwrap().id();
        window.get_mut(window_id).unwrap().set_resolution(
            scale as u32 * TILES_X * TILE_SIZE,
            scale as u32 * TILES_Y * TILE_SIZE,
        );
        */
    }
}

fn user_input(
    keyboard_input: Res<Input<KeyCode>>,
    mut status: ResMut<Status>,
    mut player_coords: ResMut<PlayerCoords>,
    mut board: ResMut<Board>,
    mut changes: ResMut<Changes>,
    mut lives: ResMut<Lives>,
    mut level: ResMut<LevelNum>,
) {
    if keyboard_input.just_pressed(KeyCode::P) {
        match *status {
            Status::Pause => *status = Status::Play,
            Status::Play => {
                *status = Status::Pause;
            }
            _ => {}
        }
    }
    if *status == Status::Welcome && keyboard_input.just_pressed(KeyCode::Space) {
        *lives = Lives::default();
        *level = LevelNum::default();
        *status = Status::SetupLevel;
    }
    if *status != Status::Play {
        return;
    }
    let mut dir = Coords::default();
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
