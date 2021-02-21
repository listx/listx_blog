mod utils;

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

#[derive(Clone, Copy)]
pub struct Canvas<'a> {
    ctx: &'a web_sys::CanvasRenderingContext2d,
    pixel_size: u8,
}

// TODO: Make this an internal "backend" function. Expose dedicated "client"
// APIs, such as draw_circle_naive(), draw_circle_bresenham(), etc. that reuse
// this function.
#[wasm_bindgen]
pub fn draw_circle(
    canvas_id: &str,
    width: u32,
    height: u32,
    pixel_size: u8,
) -> Result<(), JsValue> {
    let document = web_sys::window().unwrap().document().unwrap();
    let canvas = document.get_element_by_id(canvas_id).unwrap();
    let canvas: web_sys::HtmlCanvasElement = canvas
        .dyn_into::<web_sys::HtmlCanvasElement>()
        .map_err(|_| ())
        .unwrap();

    // Set canvas width and height. The extra 1 pixel is to account for the 0.5
    // pixel coordinates in the drawing of grid lines.
    let canvas_width = pixel_size as u32 * width;
    let canvas_height = pixel_size as u32 * height;
    canvas.set_width(canvas_width + 1);
    canvas.set_height(canvas_height + 1);

    let ctx = canvas
        .get_context("2d")
        .unwrap()
        .unwrap()
        .dyn_into::<web_sys::CanvasRenderingContext2d>()
        .unwrap();

    let c = Canvas {
        ctx: &ctx,
        pixel_size: pixel_size,
    };

    // Internal screen buffer. We draw into the screen buffer, then read it back
    // out to draw into the given HTML canvas element.
    let mut screen = ScreenBuffer::new(width, height);

    // TODO: Manipulate internal screen buffer to draw a circle.
    let mut points = Vec::new();
    //    points.push(Point { x: 0, y: 0 });
    points.push(Point { x: 15, y: -15 });
    screen.set_points(points);

    screen.draw_canvas(c, &"rgba(0,0,0,1.0)".into());

    // Overlay a grid on top.
    screen.draw_canvas_grid(&c)
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
            js_log!("HELLO");
            // Skip undrawable points (points that lie out of bounds of the
            // drawable ScreenBuffer).
            if p.x.abs() > offset as i32 || p.y.abs() > offset as i32 {
                js_log!("p.x {}, p.y {} (skipping)", p.x, p.y);
                continue;
            }
            let idx = self.get_index((p.x + offset as i32) as u32, (-p.y + offset as i32) as u32);
            js_log!("idx {} (drawing)", idx);
            self.pixels.set(idx, true);
        }
    }

    // Draw onto the HTML canvas by using the information in the ScreenBuffer.
    // Note that the ScreenBuffer is essentially a bitmap as it only stores 1
    // bit per pixel.
    fn draw_canvas(&self, c: Canvas, color: &JsValue) {
        // Color in black pixels.
        c.ctx.set_fill_style(color);
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
                let x_p = x as f64 * c.pixel_size as f64;
                let y_p = y as f64 * c.pixel_size as f64;
                c.ctx
                    .fill_rect(x_p, y_p, c.pixel_size as f64, c.pixel_size as f64);
            }
        }
    }

    // Draw a grid of vertical and horizontal lines "between" each "pixel" to
    // the canvas. This is actually not that accurate as the "pen" we're drawing
    // with is > 1 pixel thick, and the canvas size does not account for these
    // grid lines. But, it's good enough for our purposes.
    fn draw_canvas_grid(&self, c: &Canvas) -> Result<(), JsValue> {
        // "Crosshair" pixel at the origin.
        //        ctx.set_fill_style(&"rgba(255,0,0,0.5)".into());
        //        let x_o = (self.width - 1) as f64 * pixel_size as f64 / 2.0;
        //        let y_o = (self.height - 1) as f64 * pixel_size as f64 / 2.0;
        //        ctx.fill_rect(x_o, y_o, pixel_size as f64, pixel_size as f64);

        // X and Y axes. Also, y = x and y = -x diagonals.
        c.ctx.set_fill_style(&"rgba(0,255,0,0.1)".into());
        let x_o = (self.width - 1) as f64 * c.pixel_size as f64 / 2.0;
        let y_o = (self.height - 1) as f64 * c.pixel_size as f64 / 2.0;
        // X axis.
        c.ctx.fill_rect(
            0.0,
            y_o,
            self.width as f64 * c.pixel_size as f64,
            c.pixel_size as f64,
        );
        // Y axis.
        c.ctx.fill_rect(
            x_o,
            0.0,
            c.pixel_size as f64,
            self.height as f64 * c.pixel_size as f64,
        );

        // y = x and y = -x diagonals.
        let offset: i32 = (self.width >> 1) as i32;
        for y in -offset..=offset {
            for x in -offset..=offset {
                if y == x || y == -x {
                    let x_p = (x + offset) as f64 * c.pixel_size as f64;
                    let y_p = (-y + offset) as f64 * c.pixel_size as f64;
                    // TODO: Create a "fill_pixel" and call that using the
                    // cartesian coordinates instead of canvas coordinates.
                    c.ctx
                        .fill_rect(x_p, y_p, c.pixel_size as f64, c.pixel_size as f64);
                }
            }
        }

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

        c.ctx.set_fill_style(&"rgba(192,192,192,1.0)".into());
        // Vertical lines.
        for i in 0..=self.width {
            c.ctx.fill_rect(
                (i * (c.pixel_size as u32)) as f64,
                0.0,
                1.0,
                (self.height * c.pixel_size as u32) as f64,
            );
        }

        // Horizontal lines.
        for j in 0..=self.height {
            c.ctx.fill_rect(
                0.0,
                (j * (c.pixel_size as u32)) as f64,
                (self.width * c.pixel_size as u32) as f64,
                1.0,
            );
        }

        // Draw bottom-right corner pixel. This is a residue of the grid system
        // above, which has a small amount of error (more precisely, the error
        // of not drawing the 1-pixel bottom-right corner of the grid).
        c.ctx.fill_rect(
            (self.width * c.pixel_size as u32) as f64,
            (self.height * c.pixel_size as u32) as f64,
            1.0,
            1.0,
        );

        Ok(())
    }
}
