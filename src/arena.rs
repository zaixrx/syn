use crate::vm::{Object, Classifier};

pub type Pointer = usize;

pub struct Arena {
    data: Vec<Classifier<Object>>,
}

impl Arena {
    pub fn new() -> Arena {
        Arena {
            data: Vec::new()
        }
    }

    pub fn get_obj(&self, ptr: Pointer) -> Option<&Object> {
        if self.data.len() <= ptr {
            return None;
        }
        match self.data[ptr] {
            Classifier::Runtime(ref obj) => Some(obj),
            Classifier::Readonly(ref obj) => Some(obj),
        }
    }

    pub fn get_obj_mut(&mut self, ptr: Pointer) -> Option<&mut Object> {
        if self.data.len() <= ptr {
            return None;
        }
        if let Classifier::Runtime(ref mut obj) = self.data[ptr] {
            return Some(obj);
        }
        return None;
    }

    pub fn push_obj(&mut self, obj: Classifier<Object>) -> Pointer {
        let ptr = self.data.len();
        self.data.push(obj);
        return ptr;
    }

    pub fn push_objs(&mut self, objs: Vec<Classifier<Object>>) -> Pointer {
        let ptr = self.data.len();
        objs.iter().for_each(|obj| self.data.push(obj.clone()));
        return ptr;
    }
}
