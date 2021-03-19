#[cfg(test)]

mod tests {
    extern crate rust_js;
    use rust_js::*;
    use std::env;

    use serde::{Deserialize, Serialize};

    #[derive(Debug, PartialEq, Serialize, Deserialize)]
    pub struct Golden {
        algo: String,
        radius: i32,
        points: Vec<Point>,
    }

    fn read_yaml(fpath: String) -> Result<Vec<Golden>, Box<dyn std::error::Error>> {
        let f = std::fs::File::open(fpath)?;
        let goldens: Vec<Golden> = serde_yaml::from_reader(f)?;
        Ok(goldens)
    }

    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn golden() -> Result<(), Box<dyn std::error::Error>> {
        let fpath = env::var("BRESENHAM_GOLDEN").unwrap_or_default();
        let goldens: Vec<Golden> = read_yaml(fpath).unwrap();

        for golden in goldens {
            let algo: &str = &golden.algo;
            let f = match algo {
                "naive_4" => get_circle_points_naive_4,
                "naive_8" => get_circle_points_naive_8,
                "naive_8_faster" => get_circle_points_naive_8_faster,
                "naive_8_faster_tweaked_radius" => get_circle_points_naive_8_faster_tweaked_radius,
                "bresenham_float_ese" => get_circle_points_bresenham_float_ese,
                "bresenham_integer_ese" => get_circle_points_bresenham_integer_ese,
                "bresenham_integer_ese_2order" => get_circle_points_bresenham_integer_ese_2order,
                "bresenham_integer_ene" => get_circle_points_bresenham_integer_ene,
                "bresenham_integer_ene_2order" => get_circle_points_bresenham_integer_ene_2order,
                "bresenham_integer_ene_2order_leq" => {
                    get_circle_points_bresenham_integer_ene_2order_leq
                }
                _ => unreachable!(),
            };
            let mut points = f(golden.radius);
            points.sort();
            for i in 0..golden.points.len() {
                assert_eq!(points[i], golden.points[i]);
            }
        }
        Ok(())
    }
}
