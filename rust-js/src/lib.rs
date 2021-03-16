mod utils;

use std::rc::Rc;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;

extern crate fixedbitset;
use fixedbitset::FixedBitSet;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

// This imports the "alert()" JS function into Rust, so that we can call it from
// Rust.
#[wasm_bindgen]
extern "C" {
    // console.log(). We can name the rust function anything we want (and even
    // give it various type signatures in the argument list), but it will
    // ultimately be called with JS's console.log() function.
    #[wasm_bindgen(js_namespace = console, js_name = log)]
    fn js_log(s: &str);
}

macro_rules! js_log {
    ($($t:tt)*) => (js_log(&format_args!($($t)*).to_string()))
}

// Called when the wasm module is instantiated. The body of this code is based
// on
// https://rustwasm.github.io/docs/wasm-bindgen/examples/without-a-bundler.html.
// The original function uses web_sys to manipulate the DOM at runtime, but here
// we do nothing because we don't want to execute any code for simply
// initializing the wasm module.
#[wasm_bindgen(start)]
pub fn entrypoint() -> Result<(), JsValue> {
    //    // Use `web_sys`'s global `window` function to get a handle on the global
    //    // window object.
    //    let window = web_sys::window().expect("no global `window` exists");
    //    let document = window.document().expect("should have a document on window");
    //    let body = document.body().expect("document should have a body");

    //    // Manufacture the element we're gonna append. This is basically client-side
    //    // dynamic HTML.
    //    let val = document.create_element("p")?;
    //    val.set_inner_html("Hello from Rust!");

    //    // Append element dynamically to the document, at runtime!
    //    body.append_child(&val)?;

    js_log!("rust-js wasm module initialized");

    draw_all_circles()
}

// This is the internal representation of a bitmap picture. We have the width,
// height, and a bunch of bits in a flat array.
pub struct ScreenBuffer {
    width: u32,
    height: u32,
    pixels: FixedBitSet,
}

// This is a Cartesian coordinate. We use this when using Cartesian coordinates
// against the ScreenBuffer.
#[derive(Debug)]
pub struct Point {
    x: i32,
    y: i32,
}

// This is our notion of an HTML Canvas with grid lines, and is basically a
// virtual representation of a physical screen's array of pixels.
pub struct Graph {
    canvas_ctx: Box<web_sys::CanvasRenderingContext2d>,
    pixel_size: u8,
}

// Find all <canvas> tags in the document, and for each one, draw a circle based
// on the parameters passed in via the HTML "name" attribute. This way we can
// just define things declaratively in the HTML, instead of invoking JS
// directly.
pub fn draw_all_circles() -> Result<(), JsValue> {
    let document = web_sys::window().unwrap().document().unwrap();
    let canvases = document.get_elements_by_tag_name("canvas");
    js_log!("{} canvases found", canvases.length());
    let mut canvases_to_work_on = Vec::new();
    for i in 0..canvases.length() {
        let canvas = canvases.item(i).unwrap();
        let canvas_id = canvas.id();
        let split = canvas_id.split("__");
        let args: Vec<&str> = split.collect();
        if args.len() != 3 {
            js_log!(
                "unrecognized canvas calling convention {:?}; skipping",
                args
            );
            continue;
        }
        canvases_to_work_on.push(canvas_id);
    }

    let mut i = 0;
    for canvas_id in canvases_to_work_on {
        let split = canvas_id.split("__");
        let args: Vec<&str> = split.collect();
        let canvas = document.get_element_by_id(canvas_id.as_str()).unwrap();

        // Uniquify the canvas's id.
        let canvas_id_new = format!("{}-id{}", canvas_id, i);
        canvas.set_id(&canvas_id_new);
        js_log!(
            "canvas id \"{}\": drawing circle with algorithm \"{}\", radius {}, pixel size {}",
            canvas_id_new,
            args[0],
            args[1],
            args[2]
        );
        draw_circle(
            &canvas_id_new,
            args[0],
            args[1].parse().unwrap(),
            args[2].parse().unwrap(),
        );
        i += 1;
    }

    Ok(())
}

// Draws a circle. Takes a string argument and uses that to figure out which
// circle-drawing algorithm to use.
#[wasm_bindgen]
pub fn draw_circle(canvas_id: &str, algo: &str, size: i32, pixel_size: u8) {
    // Make the pixel grid a little bigger than the desired circle size.
    let width = size as u32 * 2 + 1 + 2;
    let height = size as u32 * 2 + 1 + 2;
    let graph = init_graph(canvas_id, width, height, pixel_size).unwrap();

    // Internal screen buffer. We draw into the screen buffer, then read it back
    // out to draw into the given HTML canvas element.
    let mut screen = ScreenBuffer::new(width, height);

    // Draw a set of "crosshair" pixels to act as a reference.
    screen.draw_crosshair(&graph);

    let f = match algo {
        "blank" => move |_| Vec::new(),
        "naive_4" => get_circle_points_naive_4,
        "naive_8" => get_circle_points_naive_8,
        "naive_8_faster" => get_circle_points_naive_8_faster,
        "naive_8_faster_tweaked_radius" => get_circle_points_naive_8_faster_tweaked_radius,
        "bresenham_float_ese" => get_circle_points_bresenham_float_ese,
        "bresenham_integer_ese" => get_circle_points_bresenham_integer_ese,
        "bresenham_integer_ese_2order" => get_circle_points_bresenham_integer_ese_2order,
        "bresenham_integer_ene" => get_circle_points_bresenham_integer_ene,
        "bresenham_integer_ene_2order" => get_circle_points_bresenham_integer_ene_2order,
        "bresenham_integer_ene_2order_leq" => get_circle_points_bresenham_integer_ene_2order_leq,
        _ => get_circle_points_naive_8,
    };

    let points = f(size);
    screen.set_points(points);

    // Draw black pixels.
    screen.draw_points(&graph, &"rgba(0,0,0,1.0)".into());

    // Overlay a gray grid to delineate between each pixel.
    screen.draw_grid(&graph);
}

// Duplicates points by exploiting 4-way symmetry.
pub fn mirror_points_4(p: Point) -> Vec<Point> {
    let mut points = Vec::new();
    points.push(Point { x: p.x, y: p.y });
    points.push(Point { x: -p.x, y: p.y });
    points.push(Point { x: p.x, y: -p.y });
    points.push(Point { x: -p.x, y: -p.y });
    points
}

// Duplicates points by exploiting 8-way symmetry.
pub fn mirror_points_8(p: Point) -> Vec<Point> {
    let mut points = Vec::new();
    points.push(Point { x: p.x, y: p.y });
    points.push(Point { x: p.y, y: p.x });
    points.push(Point { x: -p.x, y: p.y });
    points.push(Point { x: -p.y, y: p.x });
    points.push(Point { x: p.x, y: -p.y });
    points.push(Point { x: p.y, y: -p.x });
    points.push(Point { x: -p.x, y: -p.y });
    points.push(Point { x: -p.y, y: -p.x });
    points
}

// Draw a circle by pairing up each Y value with an X value that lie on a circle
// with radius 'r'. This has a bug because some Y values get skipped. Can you
// see why?
fn get_circle_points_naive_4(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    for x in 0..(r + 1) {
        let y = (((r * r) - (x * x)) as f64).sqrt() as i32;
        points.extend(mirror_points_4(Point { x: x, y: y }));
    }
    points
}

// Better than get_circle_points_naive_4, but wastes CPU cycles because the
// 8-way symmetry overcorrects and we draw some pixels more than once.
fn get_circle_points_naive_8(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    for x in 0..(r + 1) {
        let y = (((r * r) - (x * x)) as f64).sqrt() as i32;
        points.extend(mirror_points_8(Point { x: x, y: y }));
    }
    points
}

// Slightly faster than get_circle_points_naive_8, because of the break
// condition at the middle of the arc. However this is still inefficient due to
// the square root calculation.
fn get_circle_points_naive_8_faster(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    for x in 0..(r + 1) {
        let y = (((r * r) - (x * x)) as f64).sqrt() as i32;
        if x > y {
            break;
        }
        points.extend(mirror_points_8(Point { x: x, y: y }));
    }
    points
}

// This is much closer to Bresenham's algorithm aesthetically, by simply using
// 'r + 0.5' for the square root calculation instead of 'r' directly.
fn get_circle_points_naive_8_faster_tweaked_radius(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    let r_tweaked = r as f64 + 0.5;
    for x in 0..(r + 1) {
        let y_tweaked = (((r_tweaked * r_tweaked) - ((x * x) as f64)) as f64).sqrt();
        let y = y_tweaked.floor() as i32;
        if x > y {
            break;
        }
        points.extend(mirror_points_8(Point { x: x, y: y }));
    }
    points
}

// Draw a circle using a floating point determinant, f_m. Draw by moving E or SE.
fn get_circle_points_bresenham_float_ese(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    let mut x = 0;
    let mut y = r;
    let mut f_m: f64 = 5.0 / 4.0 - (r as f64);
    points.extend(mirror_points_8(Point { x: x, y: y }));
    while x < y {
        if f_m < 0.0 {
            f_m += 2.0 * (x as f64) + 3.0;
        } else {
            f_m += 2.0 * ((x - y) as f64) + 5.0;
            y -= 1;
        }
        x += 1;
        points.extend(mirror_points_8(Point { x: x, y: y }));
    }
    points
}

// Like draw_circle_bresenham_float_ese, but f_m is an integer variable.
fn get_circle_points_bresenham_integer_ese(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    let mut x = 0;
    let mut y = r;
    let mut f_m = 1 - r;
    points.extend(mirror_points_8(Point { x: x, y: y }));
    while x < y {
        if f_m < 0 {
            f_m += (x << 1) + 3;
        } else {
            f_m += ((x - y) << 1) + 5;
            y -= 1;
        }
        x += 1;
        points.extend(mirror_points_8(Point { x: x, y: y }));
    }
    points
}

// Like draw_circle_bresenham_integer_ese, but use 2nd-order differences to
// remove multiplication from the inner loop.
fn get_circle_points_bresenham_integer_ese_2order(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    let mut x = 0;
    let mut y = r;
    let mut f_m = 1 - r;
    let mut d_e = 3;
    let mut d_se = -(r << 1) + 5;
    points.extend(mirror_points_8(Point { x: x, y: y }));
    while x < y {
        if f_m < 0 {
            f_m += d_e;
        } else {
            f_m += d_se;
            d_se += 2;
            y -= 1;
        }
        d_e += 2;
        d_se += 2;
        x += 1;
        points.extend(mirror_points_8(Point { x: x, y: y }));
    }
    points
}

// Like draw_circle_bresenham_integer_ese, but we start drawing from (0, -r) and
// work our way up and to the right (that is, move E or NE). The difference is
// that we increment, instead of decrementing, y; this feels more uniform
// because we increment x as well.
fn get_circle_points_bresenham_integer_ene(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    let mut x = 0;
    let mut y = -r;
    let mut f_m = 1 - r;
    points.extend(mirror_points_8(Point { x: x, y: y }));
    while x < -y {
        if f_m < 0 {
            f_m += (x << 1) + 3;
        } else {
            f_m += ((x + y) << 1) + 5;
            y += 1;
        }
        x += 1;
        points.extend(mirror_points_8(Point { x: x, y: y }));
    }
    points
}

// Like draw_circle_bresenham_integer_ese_2order, but start from (0, -r) and
// move E or NE.
fn get_circle_points_bresenham_integer_ene_2order(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    let mut x = 0;
    let mut y = -r;
    let mut f_m = 1 - r;
    let mut d_e = 3;
    let mut d_ne = -(r << 1) + 5;
    points.extend(mirror_points_8(Point { x: x, y: y }));
    while x < -y {
        if f_m < 0 {
            f_m += d_e;
        } else {
            f_m += d_ne;
            d_ne += 2;
            y += 1;
        }
        d_e += 2;
        d_ne += 2;
        x += 1;
        points.extend(mirror_points_8(Point { x: x, y: y }));
    }
    points
}

// Like draw_circle_bresenham_integer_ene_2order, but use 'f_m <= 0' instead of
// 'f_m < 0'.
fn get_circle_points_bresenham_integer_ene_2order_leq(r: i32) -> Vec<Point> {
    let mut points = Vec::new();
    let mut x = 0;
    let mut y = -r;
    let mut f_m = 1 - r;
    let mut d_e = 3;
    let mut d_ne = -(r << 1) + 5;
    points.extend(mirror_points_8(Point { x: x, y: y }));
    while x < -y {
        if f_m <= 0 {
            f_m += d_e;
        } else {
            f_m += d_ne;
            d_ne += 2;
            y += 1;
        }
        d_e += 2;
        d_ne += 2;
        x += 1;
        points.extend(mirror_points_8(Point { x: x, y: y }));
    }
    points
}

// Convert canvas coordinates to cartesian coordinates.
fn get_cartesian_coord(x: u32, y: u32, width: u32, height: u32, pixel_size: u8) -> Point {
    let ps = pixel_size as u32;
    // 1 extra pixel to account for the grid lines.
    let pg = ps + 1;
    let mid_width = (width / pg / 2) as i32;
    let mid_height = (height / pg / 2) as i32;
    Point {
        x: (x / pg) as i32 - mid_width,
        y: -((y / pg) as i32 - mid_height),
    }
}

// Convert cartesian coordinates to the top-left corner of a pixel on the canvas
// (canvas coordinates). This is the opposite of get_cartesian_coord().
fn get_canvas_coord(p: &Point, width: u32, height: u32, pixel_size: u8) -> (u32, u32) {
    let ps = pixel_size as u32;
    // 1 extra pixel to account for the grid lines.
    let pg = ps + 1;
    let mid_width = (width / pg / 2) as i32;
    let mid_height = (height / pg / 2) as i32;
    (
        // Translate by 1 extra pixel to account for the initial 1-pixel thick
        // grid lines at the top and left of the canvas.
        (p.x + mid_width) as u32 * pg + 1,
        (-p.y + mid_height) as u32 * pg + 1,
    )
}

// These functions allow rendering of the internal ScreenBuffer (not meant to be
// used by clients directly) to an HTML canvas by modifying a Graph (our
// "virtual" pixel array).
impl ScreenBuffer {
    // Retrieve the index (in a flat array) of a particular (row, column)
    // coordinate pair. Note that this can be any rectangular shape (row and
    // column size do not have to be the same).
    fn get_index(&self, row: u32, column: u32) -> usize {
        (column * self.height + row) as usize
    }

    pub fn new(width: u32, height: u32) -> ScreenBuffer {
        let size = (width * height) as usize;
        let mut pixels = FixedBitSet::with_capacity(size);

        // Turn off all pixels.
        for i in 0..(width * height) {
            pixels.set(i as usize, false);
        }

        ScreenBuffer {
            width,
            height,
            pixels,
        }
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn pixels(&self) -> *const u32 {
        self.pixels.as_slice().as_ptr()
    }

    // Turns "on" given points in terms of (x, y) coordinates, using an offset,
    // into the ScreenBuffer. This way, we can treat the centered pixel in the
    // middle of the canvas as the origin (0, 0) in a traditional Cartesian
    // plane. This is unlike the native HTML canvas element which uses (0,0) at
    // the top left.
    pub fn set_points(&mut self, points: Vec<Point>) {
        let offset = self.width >> 1;

        for p in points {
            // Skip undrawable points (points that lie out of bounds of the
            // drawable ScreenBuffer).
            if p.x.abs() > offset as i32 || p.y.abs() > offset as i32 {
                continue;
            }
            let idx = self.get_index((p.x + offset as i32) as u32, (-p.y + offset as i32) as u32);
            self.pixels.set(idx, true);
        }
    }

    // Draw onto the HTML canvas by using the information in the ScreenBuffer.
    // Note that the ScreenBuffer is essentially a bitmap as it only stores 1
    // bit per pixel.
    fn draw_points(&self, g: &Graph, color: &JsValue) {
        // Color in black pixels.
        g.canvas_ctx.set_fill_style(color);
        let gp = g.pixel_size as f64;
        // Now draw each bit that is turned "on" in the ScreenBuffer
        // (self.pixels) to the HTML canvas.
        for y in 0..self.height {
            for x in 0..self.width {
                let idx = self.get_index(x, y);
                let pixel = self.pixels[idx];
                // Only draw points that fall inside the dimensions of the
                // ScreenBuffer.
                if !pixel {
                    continue;
                }
                let x_p = 1.0 + (x as f64 * (gp + 1.0));
                let y_p = 1.0 + (y as f64 * (gp + 1.0));
                g.canvas_ctx.fill_rect(x_p, y_p, gp, gp);
            }
        }
    }

    fn draw_crosshair(&self, g: &Graph) {
        g.canvas_ctx.set_fill_style(&"rgba(0,255,0,0.1)".into());
        let offset: i32 = (self.width >> 1) as i32;
        let gp = g.pixel_size as f64;
        for y in -offset..=offset {
            for x in -offset..=offset {
                let x_p = (x + offset) as f64 * (gp + 1.0) + 1.0;
                let y_p = (-y + offset) as f64 * (gp + 1.0) + 1.0;

                // Skip the origin.
                if x == 0 && y == 0 {
                    continue;
                }

                if x.abs() == y.abs() || x == 0 || y == 0 {
                    g.canvas_ctx.fill_rect(x_p, y_p, gp, gp);
                }
            }
        }
    }

    // Draw a grid of vertical and horizontal lines "between" each "pixel" to
    // the canvas. This is actually not that accurate as the "pen" we're drawing
    // with is > 1 pixel thick, and the canvas size does not account for these
    // grid lines. But, it's good enough for our purposes.
    fn draw_grid(&self, g: &Graph) {
        // We use 1-pixel-thick rectangles to avoid funny business with
        // 0.5-pixel adjustments, as documented here:
        // https://stackoverflow.com/a/7531540/437583. As a result the code is
        // shorter (a single call to fill_rect() replaces the move_to() and
        // line_to() pair). Compare the line-based version for vertical lines
        // below:
        //
        //   for i in 0..=self.width {
        //       let x: f64 = (i * (pixel_size as u32)) as f64 + 0.5;
        //       ctx.move_to(x as f64, 0.0);
        //       ctx.line_to(x as f64, ((pixel_size as u32) * self.height) as f64);
        //   }

        let gp = g.pixel_size as f64;
        g.canvas_ctx.set_fill_style(&"rgba(192,192,192,1.0)".into());
        // Vertical lines.
        for i in 0..=self.width {
            g.canvas_ctx.fill_rect(
                i as f64 * (gp + 1.0),
                0.0,
                1.0,
                (self.height as f64 * (gp + 1.0)) + 1.0,
            );
        }

        // Horizontal lines.
        for j in 0..=self.height {
            g.canvas_ctx.fill_rect(
                0.0,
                j as f64 * (gp + 1.0),
                (self.width as f64 * (gp + 1.0)) + 1.0,
                1.0,
            );
        }
    }
}

// Initializes a canvas element to have grid lines, etc to look like a pixelated
// screen.
pub fn init_graph(
    canvas_id: &str,
    width: u32,
    height: u32,
    pixel_size: u8,
) -> Result<Graph, JsValue> {
    let document = web_sys::window().unwrap().document().unwrap();
    let canvas_body = document.get_element_by_id(canvas_id).unwrap();
    let canvas: web_sys::HtmlCanvasElement = canvas_body
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

    // Set canvas width and height. The extra 1 pixel is to account for the 0.5
    // pixel coordinates in the drawing of grid lines.
    let canvas_width = (pixel_size as u32 + 1) * width;
    let canvas_height = (pixel_size as u32 + 1) * height;
    canvas.set_width(canvas_width + 1);
    canvas.set_height(canvas_height + 1);

    let canvas_ctx = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    init_tooltip_highlight(canvas_id, pixel_size).unwrap();

    let g = Graph {
        // Allocate the "ctx" part on the heap. This way, when this function
        // goes out of scope, the assignment to "ctx" here is still valid
        // because even though the local variable "ctx" is dropped, the
        // assignment occurred on the heap and holds a valid memory address on
        // the heap.
        canvas_ctx: Box::new(canvas_ctx),
        pixel_size: pixel_size,
    };

    Ok(g)
}

// Initializes the hovering tooltip that follows the mouse to show the Cartesian
// coordinate of the active (hovered-over) pixel in the Graph.
fn init_tooltip_highlight(canvas_id: &str, pixel_size: u8) -> Result<(), JsValue> {
    let document = web_sys::window().unwrap().document().unwrap();

    let active_pixel_canvas_element = document.create_element("canvas").unwrap();
    active_pixel_canvas_element.set_id(&format!("{}-active", canvas_id));
    active_pixel_canvas_element.set_class_name(&"active-pixel-canvas layer");
    let active_pixel_canvas: web_sys::HtmlCanvasElement = active_pixel_canvas_element
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

    let center_div_element = document.create_element("div").unwrap();
    center_div_element.set_class_name(&"canvas-centered");
    let center_div: web_sys::HtmlDivElement = center_div_element
        .dyn_into::<web_sys::HtmlDivElement>()
        .map_err(|_| ())
        .unwrap();

    let tooltip_element = document.create_element("div").unwrap();
    tooltip_element.set_id(&format!("{}-tooltip", canvas_id));
    let tooltip_div: web_sys::HtmlDivElement = tooltip_element
        .dyn_into::<web_sys::HtmlDivElement>()
        .map_err(|_| ())
        .unwrap();
    tooltip_div.set_class_name("canvas-tooltip");
    // TODO: Make this DRY.
    let canvas_body = document.get_element_by_id(canvas_id).unwrap();
    canvas_body.set_class_name(&"layer");
    let canvas: web_sys::HtmlCanvasElement = canvas_body
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

    // Make the active pixel canvas the same size as the underlying canvas
    // element.
    active_pixel_canvas.set_width(canvas.width());
    active_pixel_canvas.set_height(canvas.height());

    // TODO: Make this DRY.
    let canvas_body = document.get_element_by_id(canvas_id).unwrap();
    canvas_body
        .parent_node()
        .unwrap()
        .append_child(&tooltip_div)?;
    // "Wrap" trick to enclose a wrapper (here "center_div") around some element
    // (here "canvas_body"). See
    // https://plainjs.com/javascript/manipulation/wrap-an-html-structure-around-an-element-28/.
    canvas_body
        .parent_node()
        .unwrap()
        .insert_before(&center_div, Some(&canvas_body))?;
    center_div.append_child(&canvas_body)?;

    center_div.append_child(&active_pixel_canvas)?;

    let active_pixel_canvas_ctx = active_pixel_canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();
    let active_pixel_canvas_element2 = document
        .get_element_by_id(&format!("{}-active", canvas_id))
        .unwrap();
    let active_pixel_canvas2: web_sys::HtmlCanvasElement = active_pixel_canvas_element2
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();
    let active_pixel_canvas_ctx2 = active_pixel_canvas2
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    let window = web_sys::window().unwrap();
    let tooltip_div = Rc::new(tooltip_div);
    // This portion of attaching listeners is based off of
    // https://rustwasm.github.io/wasm-bindgen/examples/paint.html.
    {
        // When the mouse is moving inside the canvas, make the tooltip follow
        // it around, but with a 20px X offset. We need to take into
        // consideration the event.offset_*() values to get the value inside of
        // the canvas, but also the location of the canvas itself (grid_*) and
        // the browser window's scrollbars (window_offset_*).
        let tooltip_div = tooltip_div.clone();
        let closure = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            let window_offset_x = window.scroll_x().unwrap();
            let window_offset_y = window.scroll_y().unwrap();
            let grid_x = canvas_body.get_bounding_client_rect().x();
            let grid_y = canvas_body.get_bounding_client_rect().y();
            let offset_x = window_offset_x + grid_x;
            let offset_y = window_offset_y + grid_y;
            let p = get_cartesian_coord(
                event.offset_x() as u32,
                event.offset_y() as u32,
                canvas.width(),
                canvas.height(),
                pixel_size,
            );

            if event.offset_x() % ((pixel_size + 1) as i32) <= 1 {
                return;
            }
            if event.offset_y() % ((pixel_size + 1) as i32) <= 1 {
                return;
            }
            // Clear the pixel (well, the entire canvas actually). This way we
            // don't leave behind a "trail" of old pixels.
            active_pixel_canvas_ctx.clear_rect(
                0.0,
                0.0,
                active_pixel_canvas.width() as f64,
                active_pixel_canvas.height() as f64,
            );
            active_pixel_canvas_ctx.set_fill_style(&"rgba(255,0,0,0.5)".into());
            let pair = get_canvas_coord(&p, canvas.width(), canvas.height(), pixel_size);
            // Highlight the pixel.
            active_pixel_canvas_ctx.fill_rect(
                pair.0 as f64 - 1.0,
                pair.1 as f64 - 1.0,
                pixel_size as f64 + 2.0,
                pixel_size as f64 + 2.0,
            );
            tooltip_div.set_inner_html(&format!("({}, {})", p.x, p.y));
            tooltip_div
                .style()
                .set_property(
                    "left",
                    &format!(
                        "{}px",
                        (pair.0 as f64 + offset_x + (pixel_size as f64 * 1.5))
                    ),
                )
                .unwrap();
            tooltip_div
                .style()
                .set_property(
                    "top",
                    &format!("{}px", pair.1 as f64 + offset_y + (pixel_size as f64 * 0.5)),
                )
                .unwrap();
            tooltip_div.style().set_property("opacity", "1").unwrap();
        }) as Box<dyn FnMut(_)>);
        // The FnMut... stuff here is explained in
        // https://doc.rust-lang.org/rust-by-example/fn/closures/output_parameters.html.
        document
            .get_element_by_id(&format!("{}-active", canvas_id))
            .unwrap()
            .add_event_listener_with_callback("mousemove", closure.as_ref().unchecked_ref())?;
        closure.forget();
    }
    {
        // When the mouse leaves the canvas region, make the tooltip invisible.
        let tooltip_div = tooltip_div.clone();
        let closure = Closure::wrap(Box::new(move |_: web_sys::MouseEvent| {
            tooltip_div.style().set_property("opacity", "0").unwrap();
            active_pixel_canvas_ctx2.clear_rect(
                0.0,
                0.0,
                active_pixel_canvas2.width() as f64,
                active_pixel_canvas2.height() as f64,
            );
        }) as Box<dyn FnMut(_)>);
        document
            .get_element_by_id(&format!("{}-active", canvas_id))
            .unwrap()
            .add_event_listener_with_callback("mouseout", closure.as_ref().unchecked_ref())?;
        closure.forget();
    }
    Ok(())
}
