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

pub struct Mouse {
    x: u32,
    y: u32,
    down: bool,
}

pub struct Graph {
    canvas_ctx: Box<web_sys::CanvasRenderingContext2d>,
    pixel_size: u8,
}

fn setup_tooltip(canvas_id: &str, pixel_size: u8) -> Result<(), JsValue> {
    let document = web_sys::window().unwrap().document().unwrap();
    let tooltip_element = document.create_element("div").unwrap();
    tooltip_element.set_id(&format!("{}-tooltip", canvas_id));
    let tooltip_div: web_sys::HtmlDivElement = tooltip_element
        .dyn_into::<web_sys::HtmlDivElement>()
        .map_err(|_| ())
        .unwrap();
    //    tooltip_div.set_width(100);
    //    tooltip_div.set_height(50);
    tooltip_div.set_class_name("canvas-tooltip");
    // TODO: Make this DRY.
    let canvas_body = document.get_element_by_id(canvas_id).unwrap();
    let canvas: web_sys::HtmlCanvasElement = canvas_body
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

    // TODO: Make this DRY.
    let canvas_body = document.get_element_by_id(canvas_id).unwrap();
    canvas_body
        .parent_node()
        .unwrap()
        .append_child(&tooltip_div)?;

    // The tooltip size should be big enough to show the Cartesian coordinates.
    //    let context = tooltip_canvas
    //        .get_context("2d")
    //        .unwrap()
    //        .unwrap()
    //        .dyn_into::<web_sys::CanvasRenderingContext2d>()
    //        .unwrap();

    //    context.set_fill_style(&"rgba(255,0,0,0.1)".into());
    //    let window = web_sys::window().unwrap();

    let tooltip_div = Rc::new(tooltip_div);
    {
        let tooltip_div = tooltip_div.clone();
        let closure = Closure::wrap(Box::new(move |event: web_sys::MouseEvent| {
            //        context.fill_rect(0.0, 0.0, 10.0, 10.0);
            //            let window_offset_x = window.scroll_x().unwrap();
            //            let window_offset_y = window.scroll_y().unwrap();
            let grid_x = canvas_body.get_bounding_client_rect().x();
            let grid_y = canvas_body.get_bounding_client_rect().y();
            //            let offset_x = window_offset_x + grid_x;
            //            let offset_y = window_offset_y + grid_y;
            let p = get_cartesian(
                event.offset_x() as u32,
                event.offset_y() as u32,
                canvas.width(),
                canvas.height(),
                pixel_size,
            );
            tooltip_div.set_inner_html(&format!("({}, {})", p.x, p.y));
            tooltip_div
                .style()
                .set_property(
                    "left",
                    &format!("{}px", (event.offset_x() + 20) as f64 + grid_x),
                )
                .unwrap();
            tooltip_div
                .style()
                .set_property("top", &format!("{}px", event.offset_y() as f64 + grid_y))
                .unwrap();
        }) as Box<dyn FnMut(_)>);
        document
            .get_element_by_id(canvas_id)
            .unwrap()
            .add_event_listener_with_callback("mousemove", closure.as_ref().unchecked_ref())?;
        closure.forget();
    }
    {
        let tooltip_div = tooltip_div.clone();
        let closure = Closure::wrap(Box::new(move |_: web_sys::MouseEvent| {
            tooltip_div.style().set_property("opacity", "0").unwrap();
        }) as Box<dyn FnMut(_)>);
        document
            .get_element_by_id(canvas_id)
            .unwrap()
            .add_event_listener_with_callback("mouseout", closure.as_ref().unchecked_ref())?;
        closure.forget();
    }
    {
        let tooltip_div = tooltip_div.clone();
        let closure = Closure::wrap(Box::new(move |_: web_sys::MouseEvent| {
            tooltip_div.style().set_property("opacity", "1").unwrap();
        }) as Box<dyn FnMut(_)>);
        document
            .get_element_by_id(canvas_id)
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
    let canvas_width = pixel_size as u32 * width;
    let canvas_height = pixel_size as u32 * height;
    canvas.set_width(canvas_width + 1);
    canvas.set_height(canvas_height + 1);

    let canvas_ctx = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    setup_tooltip(canvas_id, pixel_size).unwrap();

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
pub fn draw_circle(canvas_id: &str, width: u32, height: u32, pixel_size: u8) {
    let graph = init_graph(canvas_id, width, height, pixel_size).unwrap();

    // Internal screen buffer. We draw into the screen buffer, then read it back
    // out to draw into the given HTML canvas element.
    let mut screen = ScreenBuffer::new(width, height);

    // Draw a set of "crosshair" pixels to act as a reference.
    screen.draw_crosshair(&graph);

    // TODO: Manipulate internal screen buffer to draw a circle.
    let mut points = Vec::new();
    points.push(Point { x: 15, y: -15 });
    screen.set_points(points);

    // Draw black pixels.
    screen.draw_points(&graph, &"rgba(0,0,0,1.0)".into());

    // Overlay a gray grid to delineate between each pixel.
    screen.draw_grid(&graph);
}

fn get_cartesian(x: u32, y: u32, width: u32, height: u32, pixel_size: u8) -> Point {
    let mid_width = (width / pixel_size as u32 / 2) as i32;
    let mid_height = (height / pixel_size as u32 / 2) as i32;
    Point {
        x: (x / (pixel_size as u32)) as i32 - mid_width,
        y: -((y / (pixel_size as u32)) as i32 - mid_height),
    }
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
                let x_p = x as f64 * gp;
                let y_p = y as f64 * gp;
                g.canvas_ctx.fill_rect(x_p, y_p, gp, gp);
            }
        }
    }

    fn draw_crosshair(&self, g: &Graph) {
        // X and Y axes. Also, y = x and y = -x diagonals.
        g.canvas_ctx.set_fill_style(&"rgba(0,255,0,0.1)".into());
        let gp = g.pixel_size as f64;
        let x_o = (self.width - 1) as f64 * gp / 2.0;
        let y_o = (self.height - 1) as f64 * gp / 2.0;
        // X axis. Draw 2 rectangles to skip over the origin pixel.
        g.canvas_ctx
            .fill_rect(0.0, y_o, ((self.width >> 1) as f64) * gp, gp);
        g.canvas_ctx.fill_rect(
            ((self.width >> 1) + 1) as f64 * gp,
            y_o,
            ((self.width >> 1) as f64) * gp,
            gp,
        );
        // Y axis. Again, draw 2 rectangles.
        g.canvas_ctx
            .fill_rect(x_o, 0.0, gp, (self.height >> 1) as f64 * gp);
        g.canvas_ctx.fill_rect(
            x_o,
            ((self.height >> 1) + 1) as f64 * gp,
            gp,
            (self.height >> 1) as f64 * gp,
        );

        // y = x and y = -x diagonals.
        let offset: i32 = (self.width >> 1) as i32;
        for y in -offset..=offset {
            for x in -offset..=offset {
                // Skip the origin, as well as any instance where y != x.
                if y == 0 || y.abs() != x.abs() {
                    continue;
                }
                let x_p = (x + offset) as f64 * gp;
                let y_p = (-y + offset) as f64 * gp;
                // TODO: Create a "fill_pixel" and call that using the
                // cartesian coordinates instead of canvas coordinates.
                g.canvas_ctx.fill_rect(x_p, y_p, gp, gp);
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
            g.canvas_ctx
                .fill_rect(i as f64 * gp, 0.0, 1.0, self.height as f64 * gp);
        }

        // Horizontal lines.
        for j in 0..=self.height {
            g.canvas_ctx
                .fill_rect(0.0, j as f64 * gp, self.width as f64 * gp, 1.0);
        }

        // Draw bottom-right corner pixel. This is a residue of the grid system
        // above, which has a small amount of error (more precisely, the error
        // of not drawing the 1-pixel bottom-right corner of the grid).
        g.canvas_ctx
            .fill_rect(self.width as f64 * gp, self.height as f64 * gp, 1.0, 1.0);
    }
}
