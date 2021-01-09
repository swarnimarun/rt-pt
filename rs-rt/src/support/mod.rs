use glutin::{self, PossiblyCurrent};

use std::ffi::CStr;

pub mod gl {
    pub use self::Gles2 as Gl;
    include!(concat!(env!("OUT_DIR"), "/gl_bindings.rs"));
}

pub struct Gl {
    pub gl: gl::Gl,
    fb : u32,
    tex : u32
}

pub fn load(gl_context: &glutin::Context<PossiblyCurrent>) -> Gl {
    let gl = gl::Gl::load_with(|ptr| gl_context.get_proc_address(ptr) as *const _);

    let version = unsafe {
        let data = CStr::from_ptr(gl.GetString(gl::VERSION) as *const _)
            .to_bytes()
            .to_vec();
        String::from_utf8(data).unwrap()
    };

    println!("OpenGL version {}", version);

    let mut fb = 0;
    let mut tex = 0;
    unsafe {
        gl.GenFramebuffers(1, &mut fb);
        gl.BindFramebuffer(gl::FRAMEBUFFER, fb);
        gl.GenTextures(1, &mut tex);
        gl.BindTexture(gl::TEXTURE, tex);

    }

    Gl { gl, fb, tex }
}

impl Gl {
    pub fn draw_frame(&self, _frame: *const u8, width: i32, height: i32) {
        // if let Some(color) = frame.get(0) {
        //     unsafe {
        //         self.gl.ClearColor(color[0], color[1], color[2], color[3]);
        //         self.gl.Clear(gl::COLOR_BUFFER_BIT);
        //         self.gl.DrawArrays(gl::TRIANGLES, 0, 3);
        //     }
        // }
        unsafe {
            self.gl.BlitFramebuffer(
                0,
                0,
                width,
                height,
                0,
                0,
                width,
                height,
                gl::COLOR_BUFFER_BIT,
                gl::NEAREST,
            )
        }
    }
}
