mod support;
mod raytrace;
mod scene;

use glutin::{
    // self,
    event::{ 
        Event,
        WindowEvent
    },
    event_loop::{
        ControlFlow,
        EventLoop
    },
    window::WindowBuilder,
    ContextBuilder,
};

use crate::scene::*;

pub fn scene_render(_scn : crate::scene::Scene) -> Vec<u8> {
    let mut frame = Vec::new();
    frame.reserve((_scn.camera.out_buffer_height * _scn.camera.out_buffer_width * 3) as usize);
    for i in 0.._scn.camera.out_buffer_height {
        for j in 0.._scn.camera.out_buffer_width {
            let st = (i * _scn.camera.out_buffer_width * 3 + j * 3) as usize;
            let v = _scn.camera.get_color(&_scn.data);
            frame[st + 0] = (v[0].abs() * 255f32) as u8;
            frame[st + 1] = (v[1].abs() * 255f32) as u8;
            frame[st + 2] = (v[2].abs() * 255f32) as u8;
        }
    }
    frame
}

fn main() {
    let el = EventLoop::new();
    let wb = WindowBuilder::new().with_title("Raytrace");
    let glc = ContextBuilder::new()
        .build_windowed(wb, &el)
        .expect("Couldn't build Context");
    let window_context = unsafe { glc.make_current().unwrap() };
    
    println!(
        "Pixel format of the window's GL context: {:?}",
        window_context.get_pixel_format()
    );
    
    let gl = support::load(&window_context.context());

    let scene_obj = Scene { data: SceneData::create_default(), camera: Camera::create_default() };
    let width = scene_obj.camera.out_buffer_width;
    let height = scene_obj.camera.out_buffer_height;
    let frame = scene_render(scene_obj);
    el.run(move |event, _, control_flow| {
        println!("{:?}", event);
        *control_flow = ControlFlow::Wait;

        match event {
            Event::LoopDestroyed => return,
            Event::WindowEvent { event, .. } => match event {
                WindowEvent::Resized(physical_size) => {
                    window_context.resize(physical_size)
                }
                WindowEvent::CloseRequested => {
                    *control_flow = ControlFlow::Exit
                }
                _ => (),
            },
            Event::RedrawRequested(_) => {
                gl.draw_frame(frame.as_ptr(), width, height);
                window_context.swap_buffers().unwrap();
            }
            _ => (),
        }
    });
}
