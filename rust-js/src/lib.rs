mod utils;

use std::cell::Cell;
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

    js_log("rust-js wasm module initialized");

    Ok(())
}

pub struct ScreenBuffer {
    width: u32,
    height: u32,
    pixels: FixedBitSet,
}

pub struct Point {
    x: i32,
    y: i32,
}

pub struct Graph {
    canvas_ctx: Box<web_sys::CanvasRenderingContext2d>,
    pixel_size: u8,
}

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
    {
        // When the mouse enters the canvas region, make the tooltip visible.
        let tooltip_div = tooltip_div.clone();
        let closure = Closure::wrap(Box::new(move |_: web_sys::MouseEvent| {
            tooltip_div.style().set_property("opacity", "1").unwrap();
        }) as Box<dyn FnMut(_)>);
        document
            .get_element_by_id(&format!("{}-active", canvas_id))
            .unwrap()
            .add_event_listener_with_callback("mouseover", closure.as_ref().unchecked_ref())?;
        closure.forget();
    }
    Ok(())
}

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

#[wasm_bindgen]
pub fn draw_circle(canvas_id: &str, size: u32, pixel_size: u8) {
    let width = size;
    let height = size;
    let graph = init_graph(canvas_id, width, height, pixel_size).unwrap();

    // Internal screen buffer. We draw into the screen buffer, then read it back
    // out to draw into the given HTML canvas element.
    let mut screen = ScreenBuffer::new(width, height);

    // Draw a set of "crosshair" pixels to act as a reference.
    screen.draw_crosshair(&graph);

    // TODO: Manipulate internal screen buffer to draw a circle.
    let mut points = Vec::new();
    //    points.push(Point { x: 15, y: -15 });
    //    points.push(Point { x: 10, y: -10 });
    //    points.push(Point { x: -15, y: 15 });
    points.push(Point { x: 5, y: 3 });
    screen.set_points(points);

    // Draw black pixels.
    screen.draw_points(&graph, &"rgba(0,0,0,1.0)".into());

    // Overlay a gray grid to delineate between each pixel.
    screen.draw_grid(&graph);
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

// This is our internal representation of a "screen", and is not exposed to JS.
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
