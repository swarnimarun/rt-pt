use crate::raytrace::{ Vector3, Sphere };

pub type Color = [f32; 3];

#[derive(Debug)]
enum Material {
    Diffuse(Color),
    Specular,
    Refractive
}

#[derive(Debug)]
pub struct SceneData {
    sphere_list : Vec<(Sphere, Material)>,
    // surface_list : Vec<Surface>,
}

impl SceneData {
    pub fn create_default() -> SceneData {
        SceneData {
            sphere_list : Vec::new(),
            // surface_list : Vec::new()
        }
    }
}

#[derive(Debug)]
pub struct Scene {
    pub data : SceneData,
    pub camera : Camera
}

#[derive(Debug)]
enum Tag {
    Sphere(usize),
    Surface(usize)
}

/// For optimization
/// Takes fixed Camera Position to create a ordered list of items to allow faster search
#[derive(Debug)]
pub struct OrderedScene<'a> {
    order_list : Vec<Tag>,
    scene : &'a Scene
}

#[derive(Debug)]
pub struct Camera {
    pub origin: Vector3,
    pub aspect_ratio: f32,
    pub focal_length: f32,
    pub viewport_height: f32,
    pub out_buffer_width: i32,
    pub out_buffer_height: i32,
}

impl Camera {
    pub fn get_color(&self, _scn_data : &SceneData) -> [f32; 3] {
        [0f32; 3]
    }
    pub fn create_default() -> Camera {
        Camera {
            origin : [0f32; 3],
            aspect_ratio : (16f32 / 9f32),
            focal_length : 1f32,
            viewport_height : 1f32,
            out_buffer_height : 135,
            out_buffer_width : 240
        }
    }
}
