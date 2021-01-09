use std::ops::Mul;

pub trait Intersectable {}

pub type Vector3 = [f32; 3];

pub trait VecMath {
    fn dot(&self, v: Self) -> f32;
    fn cross(&self, v: Self) -> Self;
    fn sub(&self, v: Self) -> Self;
    fn add(&self, v: Self) -> Self;
    fn mult(&self, v: f32) -> Self;
}

impl VecMath for Vector3 {
    fn dot(&self, v: Self) -> f32 {
        // i = j x k, j = i x k, k = i x j
        self[0] * v[0] + self[1] * v[1] + self[2] * v[2]
    }

    fn cross(&self, v: Self) -> Self {
        // i = j x k, j = i x k, k = i x j
        [
            self[1] * v[2] - self[2] * v[1],
            self[2] * v[0] - self[0] * v[2],
            self[0] * v[1] - self[1] * v[0],
        ]
    }

    fn sub(&self, v: Self) -> Self {
        [self[0] - v[0], self[1] - v[1], self[2] - v[2]]
    }

    fn add(&self, v: Self) -> Self {
        [self[0] + v[0], self[1] + v[1], self[2] + v[2]]
    }

    fn mult(&self, v: f32) -> Self {
        [self[0] * v, self[1] * v, self[2] * v]
    }
}

/// {vec_origin} + {vec_direction} * length = vec_point on ray
#[derive(Debug)]
pub struct Ray {
    origin: [f32; 3],
    dir: [f32; 3],
}

impl Ray {
    pub fn eval_at(&self, t : f32) -> Vector3 {
        self.origin.add(self.dir.mult(t))
    }
}

/// vec_point on sphere - {vec_origin} = radius
#[derive(Debug)]
pub struct Sphere {
    origin: [f32; 3],
    radius: f32,
}
impl Intersectable for Sphere {}

/// ( vec_point on sphere - {center} ) dot {normal} = 0
#[derive(Debug)]
pub struct Surface {
    center: [f32; 3],
    normal: [f32; 3],
}
impl Intersectable for Surface {}

pub trait RayIntersect<T: Intersectable> {
    fn intersect(&self, _o: T) -> Option<f32>;
}

impl RayIntersect<Surface> for Ray {
    fn intersect(&self, _s: Surface) -> Option<f32> {
        todo!(" WIP ");
    }
}

impl RayIntersect<Sphere> for Ray {
    fn intersect(&self, sphere: Sphere) -> Option<f32> {
        let oc = self.origin.sub(sphere.origin);
        let a = self.dir.dot(self.dir);
        let b = self.dir.dot(oc) * 2.0;
        let c = oc.dot(oc) - sphere.radius * sphere.radius;
        let d = b * b - 4.0 * a * c;
        if d < 0f32 {
            None
        } else {
            let t = (-b - d.sqrt()) / (2.0 * a);
            if t < 0f32 {
                let t = (-b + d.sqrt()) / (2.0 * a);
                if t < 0f32 {
                    // both t are -ve
                    None
                } else {
                    Some(t)
                }
            } else {
                let t1 = (-b + d.sqrt()) / (2.0 * a);
                Some(if t1 < 0f32 || t < t1 { t } else { t1 })
            }
        }
    }
}

